unit uSMTPd;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  RSR,
  RSR.DNS,
  App,
  App.Consts,
  App.Build,

  hReceive,

  Core.Database,
  Core.Database.Types,
  Core.Database.Timer,

  Core.Timer,
  Core.Streams,
  Core.Logging,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.Bytes,
  Core.Arrays.Pointers,

  Core.Strings,
  Core.Utils.Sockets,
  Core.Utils.Time,

  Core.XML,

  Storage,
  Storage.Main,
  Storage.Domains,
  Storage.Security,
  Storage.DNS,
  Storage.UserStorage,
  Storage.KeepAlive,
  Storage.Certs,
  Storage.AuraDisks,
  Storage.Intrusion,
  Storage.MatrixNodes,
  Storage.MatrixQueue,
  Storage.MatrixServices,
  Storage.UserAccounts,

  Encryption.Base64,

  SysUtils,
  MD5,
  XmlReader,
  DOM,
  XMLRead;

Type
  TReceiveMailServer=class;
  TMailManager=class;
  {$i uSMTP.Transport.Types.inc}
  {$i uSMTP.Transport.Receive.Dec.inc}
  {$i uSMTP.TransportRelay.Type.inc}

  TReceiveMailServer=Class(TRSRServer)
  private
    {$i uSMTP.ReceiveMailServer.Private.Decs.inc}
  protected
    {$i uSMTP.ReceiveMailServer.Protected.Decs.inc}
  public
    Constructor Create(aDomain:Core.Strings.VarString; Const aKind,aNodeID,aIP:QWord; aPort,aScale:WORD; aSecure:boolean); ReIntroduce;
    Destructor  Destroy; override;
  public
    procedure   OnQueueItemReceived(qTask:Core.Database.Types.TTask; var Item:Storage.MatrixQueue.Items.Item);
  end;

  TMailManager=Class(TRSRManager)
  private
    {$i uSMTP.MailManager.Private.Decs.inc}
  protected
    {$i uSMTP.MailManager.Protected.Decs.inc}
  public
    Constructor Create(AOwner:TReceiveMailServer); reintroduce;
    Destructor  Destroy; override;
  end;

implementation
uses StrUtils,DateUtils;

{$i uSMTP.Implements.Consts.inc}

{$i uSMTP.Code.ExtractUser.inc}

Constructor TReceiveMailServer.Create(aDomain:Core.Strings.VarString; const aKind,aNodeID,aIP:QWord; aPort,aScale:WORD; aSecure:boolean);
const
  S_SSL:Array[boolean] of Core.Strings.VarString=(' ',' SSL ');
  S_SVC:Array[boolean] of Core.Strings.PVarString=(@Service_SMTP,@Service_SMTP_SSL);
  I_SVC:Array[boolean] of LongInt=(Storage.MatrixServices.Items.mkSMTP,Storage.MatrixServices.Items.mkSMTPSO);
var
  iLcv:LongInt;
begin
  Inherited Create(@FManagers,tTCP,aIP,APort,aScale,aSecure,CREATE_SUSPENDED);

  FService:=S_SVC[aSecure]^;
  FKind:=aKind;

  InitCriticalSection(FAntiSpamLock);

  TI_RootUser.Priority:=tpNormal;
  TI_RootUser.Expires:=IncSecond(Core.Timer.dtNow,60);
  TI_RootUser.Event:=@OnTimer_FillRootUserData;
  TI_RootUser.Location:='uSMTPd.OnTimer_FillRootUserData';

  Core.Database.Timer.Background.RegisterEvent(TI_RootUser,LoadNoUpdate);

  TI_Filters.Priority:=tpNormal;
  TI_Filters.Expires:=IncSecond(Core.Timer.dtNow,5);
  TI_Filters.Event:=@OnTimer_FillAntiSpamData;
  TI_Filters.Location:='uSMTPd.OnTimer_FillAntiSpamData';
  Core.Database.Timer.Background.RegisterEvent(TI_Filters,LoadNoUpdate);

  RelayLcv:=0;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Server'));

  Storage.MatrixNodes.Node.DB.Fill(Task,aNodeID,FMatrixNode);

  RootDomain.Name:=ADomain;
  Storage.Domains.Items.DB.Fill(Task,RootDomain);
  RootUA.User:=RootDomain.Root;
  RootUA.DomainID:=RootDomain.ID;
  Storage.UserAccounts.Items.DB.Fill(Task,RootDomain.Root,RootUA);
  {$ifdef RSR_DEBUG}
  OnException('uSMTPd.TReceiveMailServer.Create','Load Managers',Concat('Loading (',IntToStr(aRelayScale) ,').'));
  {$endif}

  if RootDomain.CertID<>0 then
    Storage.Certs.Items.DB.Read(Task,RootDomain.ID,RootDomain.CertID,Cert);

  FSecure:=aSecure;
  FSSLInfo.keyLen:=Core.Arrays.Bytes.Copy(Cert.DerKey,FSSLInfo.keyData);
  Load(Cert,FSSLInfo.Manifest);

  FXMLParser:=TDOMParser.Create();
  FXMLParser.Options.Validate:=False;
  FXMLParser.Options.ConformanceLevel:=clFragment;
  FXMLDocument:=nil;
  FXMLSource:=nil;

  iLcv:=0;
  While iLcv<aScale do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TMailManager.Create(Self);
    Except
      iLcv:=aScale;
    end;
    Inc(iLcv);
  end;
end;

Destructor  TReceiveMailServer.Destroy;
begin

  Core.Database.Timer.Background.UnloadEvent(TI_RootUser,UnloadNoExecute);
  Core.Database.Timer.Background.UnloadEvent(TI_Filters,UnloadNoExecute);

  FreeAndNil(FXMLParser);
  DoneCriticalSection(FAntiSpamLock);
  Storage.Security.Filter.Done(FFilters);
  Inherited Destroy;
end;

procedure   TReceiveMailServer.OnTimer_FillRootUserData(TimerP:Core.Timer.PItem);
begin
  Storage.UserAccounts.Items.DB.Fill(Core.Database.Timer.Background.Task,RootUA.ID,RootUA);
  TimerP^.Expires:=IncSecond(Core.Timer.dtNow,60);
end;

procedure   TReceiveMailServer.OnTimer_FillAntiSpamData(TimerP:Core.Timer.PItem);
begin
  EnterCriticalSection(FAntiSpamLock);
  Try
    Storage.Security.Filter.DB.Live(Core.Database.Timer.Background.Task,secWhiteList,FFilters[secWhiteList]);
    Storage.Security.Filter.DB.Live(Core.Database.Timer.Background.Task,secBlackList,FFilters[secBlackList]);
    Storage.Security.Filter.DB.Live(Core.Database.Timer.Background.Task,secBLService,FFilters[secBLService]);
    Storage.Security.Filter.DB.Live(Core.Database.Timer.Background.Task,secContentFilter,FFilters[secContentFilter]);
    Storage.Security.Filter.DB.Live(Core.Database.Timer.Background.Task,secContentProfiles,FFilters[secContentProfiles]);

    Storage.Security.Filter.toString(FFilters[secBLService],saDNSBlackLists);


    Storage.Security.Filter.setData(FFilters[secContentProfiles]);

  Finally
    LeaveCriticalSection(FAntiSpamLock);
  end;
  TimerP^.Expires:=IncSecond(Core.Timer.dtNow,60);
end;

procedure   TReceiveMailServer.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure   TReceiveMailServer.OnError(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure   TReceiveMailServer.OnQueueItemReceived(qTask:Core.Database.Types.TTask; var Item:Storage.MatrixQueue.Items.Item);
begin
  FManager:=GetNextManager() as TMailManager;
  FDisposePtr:=true;
  new(FRelayMailP);
  Try
    Storage.UserStorage.Items.SMTP.Init(FRelayMailP^);
    if Storage.MatrixNodes.Node.DB.Fill(qTask,Item.DiskID,FRelayMailP^.Disk) then begin
      Try
        FXMLSource:=TXMLInputSource.Create(Item.Meta);
        Try
          Try
            Try
              FXMLParser.Parse(FXMLSource,FXMLDocument);
              If Storage.UserStorage.Items.SMTP.fromXML(FXMLDocument,FRelayMailP^) then begin
                if (Item.Trys<=FRelayMailP^.iTryMax) then begin
                  if Storage.AuraDisks.Files.Read(FRelayMailP^.Disk,Domain.Global,Use.Global,Folder.Queue,Item.ID,Kinds.NOSQL,FRelayMailP^.Data) then begin
                    FRelayMailP^.QueueID:=Item.ID;
                    FRelayMailP^.Helo:=Concat(FManager.Owner.FMatrixNode.Alias,'.',FManager.Owner.RootDomain.Name);
                    FRelayRSRP:=FManager.Allocate(rsrClient,rsrsTCP);
                    FRelayRSRP^.Info.DataP:=FRelayMailP;
                    FRelayRSRP^.Transport:=FManager.FTransportRelay;
                    FDisposePtr:=false;
                  end else begin
                    FRelayMailP^.Error:=Concat(FRelayMailP^.Error,'AuraDisk was unable to read message',#13#10);
                    Item.Meta:=Storage.UserStorage.Items.SMTP.toXML(FRelayMailP^,XML_HEADER_ON);
                    Item.TTL:=DateUtils.IncSecond(Core.Timer.dtUT,15);
                    Storage.MatrixQueue.Items.DB.Fail(qTask,Item.ID,Item.NodeID,Item.TTL,Item.Meta);
                  end;
                end else begin
                  Item.TTL:=DateUtils.IncSecond(Core.Timer.dtUT,15);
                  Storage.MatrixQueue.Items.DB.Delete(Task,FRelayMailP^.Disk,Item.ID);
                  FManager.FTransportRelay.Bounce(Task,FRefactor,FRelayMailP,'Too many delivery attempts.');
                end;
              end;
            Finally
              FreeAndNil(FXMLDocument);
            end;
          Except
           On E:Exception do begin
             OnError('OnQueueItemReceived','uSMTPd.pas',Concat('XML parse error for ',IntToSTr(Item.ID),' ',E.Message));
             Storage.MatrixQueue.Items.DB.Delete(Task,FRelayMailP^.Disk,Item.ID);
           end;
          end;
        Finally
          FreeAndNil(FXMLSource);
        end;
      Except
        On E:Exception do begin
          OnError('OnQueueItemReceived','uSMTPd.pas',Concat('XML source error for ',IntToSTr(Item.ID),' ',E.Message));
          Storage.MatrixQueue.Items.DB.Delete(Task,FRelayMailP^.Disk,Item.ID);
        end;
      end;
    end else begin
      Item.TTL:=DateUtils.IncSecond(Core.Timer.dtUT,15);
      OnError('OnQueueItemReceived','uSMTPd.pas',Concat('Unable to retrieve disk ID ',IntToStr(Item.DiskID),' for ',IntToSTr(Item.ID)));
      Storage.MatrixQueue.Items.DB.Fail(qTask,Item.ID,Item.NodeID,Item.TTL,Item.Meta);
    end;
  Finally
    if (FDisposePtr=true) then begin
      Storage.UserStorage.Items.SMTP.Done(FRelayMailP^);
      Dispose(FRelayMailP);
    end;
  end;
end;

Constructor TMailManager.Create(AOwner:TReceiveMailServer);
const
  S_SVC:Array[boolean] of Core.Strings.PVarString=(@Service_SMTP,@Service_SMTP_SSL);
begin
  FKind:=AOwner.FKind;
  FService:=S_SVC[AOwner.FSecure]^;

  FXMLParser:=TDOMParser.Create();
  FXMLParser.Options.Validate:=False;

  dtNextAntiSpamUpdate:=0;
  Empty(saNoFilters);
  Empty(smNoFilters);


  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Manager'));

  FUserAccounts:=Storage.UserAccounts.Items.TList.Create(@AOwner.RootDomain,FService);

  Owner:=AOwner;

  FTransportRecv:=TTransportReceive.Create(Self);
  FTransportRelay:=TTransportRelay.Create(Self);

  Inherited Create(aOwner,@AOwner.FSSLInfo,AOwner.FSecure,THREAD_METHODS_OFF,MAIL_STACK_SIZE);

  TimeOut:=Storage.MatrixServices.Items.DefaultTimeOut[Storage.MatrixServices.Items.mkSMTP];
end;

Destructor  TMailManager.Destroy;
begin
  FreeAndNil(FXMLParser);
  Core.Arrays.VarString.Done(saDNSBlackLists);
  Core.Arrays.VarString.Done(saNoFilters);
  Core.Arrays.VarString.Done(smNoFilters);

  Storage.Security.Filter.Done(NoProfiles);
  Storage.Security.Filter.Done(NoPhrases);
  Storage.Security.Filter.Done(ContentProfiles);
  Storage.Security.Filter.Done(ContentPhrases);
  Storage.Security.Filter.Done(WhiteList);
  Storage.Security.Filter.Done(BlackList);

  Storage.UserStorage.Items.SMTP.Done(FSummary);
  FreeAndNil(FUserAccounts);

  FreeAndNil(FTransportRecv);
  FreeAndNil(FTransportRelay);

  Inherited Destroy;
end;

{$i uSMTP.MailManager.Methods.inc}
{$i uSMTP.TransportBase.Methods.inc}
{$i uSMTP.TransportReceive.Methods.inc}
{$i uSMTP.TransportRelay.Methods.inc}

end.

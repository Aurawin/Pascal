{
 Copyright Aurawin LLC 2014-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit uAuDiskd;

interface

uses
  Classes,
  RSR,
  RSR.DNS,

  App.Consts,
  App.Build,

  hReceive,

  Storage,
  Storage.Main,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Core.Logging,
  Core.Timer,
  Core.Strings,
  Core.Streams,

  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.LargeInt,
  Core.Arrays.VarString,
  Core.Arrays.Bytes,
  Core.Arrays.Pointers,
  Core.Utils.Sockets,
  Core.Utils.Time,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.Security,
  Storage.DNS,
  Storage.UserStorage,
  Encryption.Base64,
  Storage.MatrixNodes,
  Storage.KeepAlive,
  Storage.Certs,
  Storage.AuraDisks,
  Storage.Intrusion,

  SysUtils,
  MD5;

Type
  TAuDiskServer=class;
  TAuDiskManager=class;

  TAuDiskServer=Class(TRSRServer)
  private
    TI_RootUser                  : Core.Timer.Item;
    FManagers                    : TRSRManagers;
    FMatrixNode                  : Storage.MatrixNodes.Node.Item;
  protected
    procedure   OnTimer_FillRootUserData(TimerP:Core.Timer.PItem);

    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    RootDomain                   : Storage.Domains.Items.TDomain;
    RootUA                       : Storage.UserAccounts.Items.Item;
  public
    Constructor Create(aDomain:Core.Strings.VarString; Const aNodeID,aIP:QWord; aPort,aScale:WORD; aSecure:boolean); ReIntroduce;
    Destructor  Destroy; override;
  end;

  TAuDiskManager=Class(TRSRManager)
  private
    FUserAccounts                : Storage.UserAccounts.Items.TList;
    Owner                        : TAuDiskServer;
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;

    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; Var Handled:Boolean); override;
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  public
    Constructor Create(AOwner:TAuDiskServer); reintroduce;
    Destructor  Destroy; override;
  end;

implementation
uses
  StrUtils,
  DateUtils;

Constructor TAuDiskServer.Create(aDomain:Core.Strings.VarString; Const aNodeID,aIP:QWord; aPort,aScale:WORD; aSecure:boolean);
const
  S_SSL:Array[boolean] of Core.Strings.VarString=(' ',' SSL ');
  S_SVC:Array[boolean] of Core.Strings.PVarString=(@SERVICE_AUDISK,@SERVICE_AUDISK_SSL);
var
  iLcv:LongInt;
begin
  Inherited Create(@FManagers,tTCP,aIP,APort,aScale,aSecure,CREATE_SUSPENDED);


  FService:=S_SVC[aSecure]^;
  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat('Aura Disk ',FService,' Server') );

  TI_RootUser.Priority:=tpNormal;
  TI_RootUser.Expires:=IncSecond(Core.Timer.dtNow,60);
  TI_RootUser.Event:=@OnTimer_FillRootUserData;
  TI_RootUser.Location:='uAuDiskd.OnTimer_FillRootUserData';
  Core.Database.Timer.Background.RegisterEvent(TI_RootUser,LoadNoUpdate);

  Storage.MatrixNodes.Node.DB.Fill(Task,aNodeID,FMatrixNode);

  RootDomain.Name:=ADomain;
  Storage.Domains.Items.DB.Fill(Task,RootDomain);
  RootUA.User:=RootDomain.Root;
  RootUA.DomainID:=RootDomain.ID;
  Storage.UserAccounts.Items.DB.Fill(Task,RootDomain.Root,RootUA);

  {$ifdef RSR_DEBUG}
  OnException('uAuDiskd.TAuDiskServer.Create','Load Managers',Concat('Loading (',IntToStr(aRelayScale) ,').'));
  {$endif}

  if RootDomain.CertID<>0 then
    Storage.Certs.Items.DB.Read(Task,RootDomain.ID,RootDomain.CertID,Cert);

  FSecure:=aSecure;
  FSSLInfo.keyLen:=Core.Arrays.Bytes.Copy(Cert.DerKey,FSSLInfo.keyData);
  Load(Cert,FSSLInfo.Manifest);

  iLcv:=0;
  While iLcv<aScale do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TAuDiskManager.Create(Self);
    Except
      iLcv:=aScale;
    end;
    Inc(iLcv);
  end;
end;

Destructor  TAuDiskServer.Destroy;
begin
  Core.Database.Timer.Background.UnloadEvent(TI_RootUser,UnloadNoExecute);
  Inherited Destroy;
end;

procedure   TAuDiskServer.OnTimer_FillRootUserData(TimerP:Core.Timer.PItem);
begin
  Storage.UserAccounts.Items.DB.Fill(Core.Database.Timer.Background.Task,RootUA.ID,RootUA);
  TimerP^.Expires:=IncSecond(Core.Timer.dtNow,60);
end;

procedure   TAuDiskServer.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

procedure   TAuDiskServer.OnError(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

Constructor TAuDiskManager.Create(AOwner:TAuDiskServer);
const
   S_SVC:Array[boolean] of Core.Strings.PVarString=(@SERVICE_AUDISK,@SERVICE_AUDISK_SSL);
begin
  FService:=S_SVC[AOwner.FSecure]^;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header, Concat('AuraDisk ',FService,' Manager'));

  FUserAccounts:=Storage.UserAccounts.Items.TList.Create(@AOwner.RootDomain,FService);

  Owner:=AOwner;

  Inherited Create(aOwner,@AOwner.FSSLInfo,AOwner.FSecure,THREAD_METHODS_OFF,MAIL_STACK_SIZE);
  TimeOut:=0;
end;

Destructor  TAuDiskManager.Destroy;
begin
  FreeAndNil(FUserAccounts);
  Inherited Destroy;
end;

procedure   TAuDiskManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

procedure   TAuDiskManager.OnError(RSRP:PRSR);
begin

end;

procedure   TAuDiskManager.OnDisconnect(RSRP:PRSR);
begin

end;

procedure   TAuDiskManager.OnConnect(RSRP:PRSR);
begin

end;

procedure   TAuDiskManager.OnDataReceived(RSRP:PRSR; Var Handled:Boolean);
begin

end;

procedure   TAuDiskManager.OnQueue(RSRP:PRSR);
begin

end;

procedure   TAuDiskManager.OnDNSResult(RSRP:PRSR);
begin

end;

procedure   TAuDiskManager.OnInitialize(RSRP:PRSR);
begin

end;

procedure   TAuDiskManager.OnFinalize(RSRP:PRSR);
begin

end;

end.

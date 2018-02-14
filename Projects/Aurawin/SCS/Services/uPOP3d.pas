{
 Copyright Aurawin LLC 2003-2013
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit uPOP3d;

interface
uses
  Types,
  classes,

  App,
  App.Build,
  App.Consts,

  RSR,
  RSR.DNS,
  RSR.POP3,

  Core.Database,
  Core.Database.Types,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Utils.Sockets,

  Core.Logging,
  Core.Strings,
  Core.Timer,

  Storage,
  Storage.Main,
  Storage.Certs,
  Storage.UserAccounts,
  Storage.UserStorage,
  Storage.Domains,
  Storage.CoreObjects,
  Storage.KeepAlive,
  Storage.Intrusion,

  SysUtils;
Type
  TPOPMailServer=Class(TRSRServer)
  private
    FManagers            : TRSRManagers;
    RootDomain           : Storage.Domains.Items.TDomain;
  protected
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    Constructor Create(aDomain:Core.Strings.VarString; Const aIP:Int64; aPort, aScale:WORD; aSecure:boolean); ReIntroduce;
    Destructor  Destroy; override;
  end;

  TPOPManager=Class(TRSRManager)
  private
    POP3P        : PPOP3;
    Owner        : TPOPMailServer;
    FResponse    : Core.Strings.VarString;
    UserAccounts : Storage.UserAccounts.Items.TList;
  private
    procedure   Process(RSRP:PRSR; var InputN:Core.Strings.VarString);
    procedure   ClipBuffer(Const CRLFCount:LongInt; Var Buffer:Core.Strings.VarString);
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; Var Handled:Boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  public
    Constructor Create(AOwner:TPOPMailServer); reintroduce;
    Destructor  Destroy; override;
  end;

implementation
uses DateUtils;

const
  S_SSL:Array[boolean] of Core.Strings.VarString=(' ',' SSL ');
  S_SVC:Array[boolean] of Core.Strings.PVarString=(@Service_POP3,@Service_POP3_SSL);

Constructor TPOPMailServer.Create(aDomain:Core.Strings.VarString; Const aIP:Int64; aPort,aScale:Word; aSecure:boolean);
var
  iLcv:LongInt;
begin
  Inherited Create(@FManagers,tTCP,aIP,aPort,aScale,aSecure,CREATE_SUSPENDED);

  FService:=S_SVC[aSecure]^;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Server'));

  RootDomain.Name:=aDomain;

  Storage.Domains.Items.DB.Fill(Storage.Main.Task,RootDomain);

  if (RootDomain.CertID<>0) then
    Storage.Certs.Items.DB.Read(Task,RootDomain.ID,RootDomain.CertID,Cert);

  FSecure:=aSecure;
  FSSLInfo.keyLen:=Core.Arrays.Bytes.Copy(Cert.DerKey,FSSLInfo.keyData);
  Load(Cert,FSSLInfo.Manifest);

  iLcv:=0;
  While iLcv<aScale do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TPOPManager.Create(Self);
    Except
      iLcv:=aScale;
    end;
    Inc(iLcv);
  end;

end;

Destructor  TPOPMailServer.Destroy;
begin
  Inherited Destroy;
end;

{$i uPOP3Server.Callbacks.inc}

Constructor TPOPManager.Create(AOwner:TPOPMailServer);
begin
  FService:=S_SVC[AOwner.FSecure]^;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Manager'));

  Owner:=AOwner;
  UserAccounts:=Storage.UserAccounts.Items.TList.Create(@AOwner.RootDomain,FService);

  Inherited Create(aOwner,@AOwner.FSSLInfo,AOwner.FSecure,THREAD_METHODS_OFF,POP_STACK_SIZE);

  TimeOut:=120000;
end;

Destructor  TPOPManager.Destroy;
begin
  FreeAndNil(UserAccounts);
  Inherited Destroy;
end;

{$i uPOP3Manager.ClipBuffer.inc}

procedure  TPOPManager.OnQueue(RSRP:PRSR);
begin
  POP3P:=RSRP^.Info.DataP;
  If POP3P<>Nil then begin
    if Storage.Intrusion.Intruder.DB.Count(Task,Owner.RootDomain.ID,RSRP^.Address.sin_addr.s_addr,Core.Timer.dtUT)=0 then begin
      POP3P:=RSRP^.Info.DataP;
      POP3P^.State:=RS_NONE;
      POP3P^.ErrorCount:=0;
      POP3P^.DeleteIndex:=-1;
      FResponse:=Concat('+OK ',Owner.RootDomain.Name,' running ',App.Build.Title,' ',App.Build.Edition,' Build (',App.Build.Version,') RSR Build (',App.Build.RSR,'). AURA POP3 Service Ready.',#13#10);
      Send(RSRP,FResponse);
    end else begin
      FResponse:=Concat('+ERR ',Owner.RootDomain.Name,' blocked by ',App.Build.Title,' ',App.Build.Edition,' Build (',App.Build.Version,') RSR Build (',App.Build.RSR,'). AURA POP3 Service denied.',#13#10);
      Send(RSRP,FResponse);
      Close(RSRP);
    end;
  end else begin
    FResponse:=Concat('-ERR ',Owner.RootDomain.Name,' Aura POP3 Service Not Ready.',#13#10);
    Send(RSRP,FResponse);
    Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TPOPManager.OnQueue Error:','Null pointer to POP3 Data'));
    Close(RSRP);
  end;
end;

procedure  TPOPManager.OnConnect(RSRP:PRSR);
begin
end;

procedure  TPOPManager.OnDisconnect(RSRP:PRSR);
begin
end;

procedure  TPOPManager.OnInitialize(RSRP:PRSR);
begin
  New(POP3P);
  RSR.POP3.Init(POP3P^);
  RSRP^.Info.DataP:=POP3P;
end;

procedure  TPOPManager.OnFinalize(RSRP:PRSR);
begin
  POP3P:=RSRP^.Info.DataP;
  If POP3P<>Nil then begin
    Try
      EntryPoint:='TPOPManager.OnFinalize.Done';
      RSR.POP3.Done(POP3P^);
    Finally
      RSRP^.Info.DataP:=Nil;
      Dispose(POP3P);
    end;
  end;
end;

{$i uPOP3Manager.OnDataReceived.inc}
{$i uPOP3Manager.Process.inc}
{$i uPOP3Manager.Exceptions.inc}
{$i uPOP3Manager.Errors.inc}

end.

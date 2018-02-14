unit uService;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, SvcMgr, Dialogs,
  UReceive,UPop3, OCL,hCCApp,uStorage,Core.Timer,
  ccWinSock,uHttpd,uXMPPD,hXMPPD,ccUtils,hDatabase;

type
  TASCMS = class(TService)
    procedure ServiceDestroy(Sender: TObject);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServicePause(Sender: TService; var Paused: Boolean);
    procedure ServiceContinue(Sender: TService; var Continued: Boolean);
  private
    { Private declarations }
    // FUserUpdateTimer:Core.Timer.Item;
    FServers:TList;
    FStartupDebug:String;
    // procedure _OnUserUpdate(Sender:TObject);
    Procedure OnDBMSExeception(sModule,sLocation,sMessage:String);
  public
    Function  AddService(Var MS:TMatrixService):Boolean;
    Function  StopService(Var MS:TMatrixService):Boolean;
    Function  StartService(Var MS:TMatrixService):Boolean;
    Function  ContinueService(Var MS:TMatrixService):Boolean;
    Function  PauseService(Var MS:TMatrixService):Boolean;

    function  GetServiceController: TServiceController; override;
    { Public declarations }
  end;

var
  ASCMS: TASCMS;

implementation
uses DateUtils,hSRConsts,IniFiles, uDatabase, uProviders;

{$R *.DFM}

Procedure OnDBMSConnected(SessionID:TSessionHandle); stdcall;
begin

end;

procedure OnDBMSDisconnected(SessionID:TSessionHandle); stdcall;
begin

end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ASCMS.Controller(CtrlCode);
end;
{
procedure TSRMail._OnUserUpdate(Sender:TObject);
begin
  FUserUpdateTimer.Expires:=IncSecond(Now,IniFile.ReadInteger('Settings','Auto Update Users',60));
  uStorage.UpdateServers;
end;
}

Function TASCMS.GetServiceController: TServiceController;
begin
  Result := ServiceController;
end;

Function  TASCMS.StopService(Var MS:TMatrixService):Boolean;
begin

end;

Function  TASCMS.StartService(Var MS:TMatrixService):Boolean;
var
  iLcv: LongInt;
begin
  Result:=False;
  Try
    AddService(MS);
    Result:=True;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,Service_RSR,Concat('StartService:',E.Message));
  end;
end;

Function  TASCMS.PauseService(Var MS:TMatrixService):Boolean;
begin
  case MS.Kind of
    mkPOP3:TPOPMailServer(MS.Service).Active:=False;
    mkSMTP:TReceiveMailServer(MS.Service).Active:=False;
    mkHTTP:THTTPServer(MS.Service).Active:=False;
    mkXMPPSToS:TXMPPServerToServerServer(MS.Service).Active:=False;
    mkXMPPCToS:TXMPPClientSessionServer(MS.Service).Active:=False;
  end;
end;

Function  TASCMS.ContinueService(Var MS:TMatrixService):Boolean;
begin
  case MS.Kind of
    mkPOP3:TPOPMailServer(MS.Service).Active:=True;
    mkSMTP:TReceiveMailServer(MS.Service).Active:=True;
    mkHTTP:THTTPServer(MS.Service).Active:=True;
    mkXMPPSToS:TXMPPServerToServerServer(MS.Service).Active:=True;
    mkXMPPCToS:TXMPPClientSessionServer(MS.Service).Active:=True;
  end;
end;

Function  TASCMS.AddService(Var MS:TMatrixService):Boolean;
var
  Domain : TDomain;

  procedure PushSMTP;
  begin
    Try
      MS.Service:=TReceiveMailServer.Create(VarStringToString(Domain.Domain),MS.IP,MS.Port);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(VarStringToString(Domain.Domain),Service_SMTP,Concat('AddService:',E.Message));
    end;
  end;
  procedure PushPOP3;
  begin
    Try
      MS.Service:=TPOPMailServer.Create(VarStringToString(Domain.Domain),MS.IP,MS.Port);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(VarStringToString(Domain.Domain),Service_POP3,Concat('AddService:',E.Message));
    end;
  end;
  Procedure PushXMPPCToS;
  begin
    Try
      MS.Service:=TXMPPClientSessionServer.Create(VarStringToString(Domain.Domain),MS.IP,MS.Port);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(VarStringToString(Domain.Domain),Service_XMPP,Concat('AddService:',E.Message));
    end;
  end;
  Procedure PushXMPPSToS;
  begin
    Try
      MS.Service:=TXMPPServerToServerServer.Create(VarStringToString(Domain.Domain),MS.IP,MS.Port);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(VarStringToString(Domain.Domain),Service_XMPP,Concat('AddService:',E.Message));
    end;
  end;
  procedure PushHTTP;
  begin
    Try
      MS.Service:=THTTPServer.Create(VarStringToString(Domain.Domain),MS.IP,MS.Port);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(VarStringToString(Domain.Domain),Service_HTTP,Concat('AddService:',E.Message));
    end;
  end;

begin
  Result:=False;
  MS.Service:=Nil;
  uStorage.Domain_Fill(Module,MS.DomainID,Domain);
  case MS.Kind of
    mkPOP3:PushPOP3;
    mkSMTP:PushSMTP;
    mkHTTP:PushHTTP;
    mkXMPPSToS:PushXMPPSToS;
    mkXMPPCToS:PushXMPPCToS;
    else begin
      Core.Logging.Native.WriteLogEntry(VarStringToString(Domain.Domain),'Unknown','Could not add service');
    end;
  end;
  Result:=True;
end;

procedure TASCMS.ServiceDestroy(Sender: TObject);
var
  iProviderLcv,iManLcv:LongInt;
begin
  For iProviderLcv:=0 to High(uStorage.SearchProviders) do begin
    for iManLcv:=0 to High(uStorage.SearchProviders[iProviderLcv].Managers) do
      TProviderManager(uStorage.SearchProviders[iProviderLcv].Managers[iManLcv]).Free;
  end;
  Empty(uStorage.SearchProviders);

  if uDatabase.Connected(uStorage.Module.SessionID) then
    uDatabase.Disconnect(uStorage.Module.SessionID);
  uDatabase.FinalizeDBMS(uStorage.Module.SessionID);
  ShutdownStorage;
end;

procedure TASCMS.ServiceCreate(Sender: TObject);
var
  bstarted:Boolean;
  iLcv,iManLcv:LongInt;
begin
  StartupStorage;
  FStartupDebug:='';
  Try
    uStorage.Header.Mode:=dbmODBC;
    uStorage.Header.DataStorageName:=uStorage.DB_Name;
    uStorage.Header.Username:=uStorage.DB_User;
    uStorage.Header.Password:=uStorage.DB_Password;
    uStorage.Header.OnConnected:=OnDBMSConnected;
    uStorage.Header.OnDisconnected:=OnDBMSDisconnected;
    uStorage.Module.OnException:=OnDBMSExecption;

    uStorage.Module.SessionID:=uDatabase.InitializeDBMS(@uStorage.Header,@uStorage.Module);
    If uStorage.Module.SessionID<>-1 then begin
      If uDatabase.Connect(uStorage.Module.SessionID) then begin
        uHTTPd.StartBackgroundTimers();
        uStorage.Config_DNS_Fill(uStorage.Module,uStorage.Regular_DNS,dnskRegular);
        // Use IP Helper to find this node and cluster...
        uStorage.Providers_List(uStorage.Module,uStorage.SearchProviders);
        For iLcv:=0 to High(uStorage.SearchProviders) do begin
          SetLength(SearchProviders[iLcv].Managers,SearchProviders[iLcv].Scale);
          For iManLcv:=0 to High(SearchProviders[iLcv].Managers) do begin
            SearchProviders[iLcv].Managers[iManLcv]:=uProviders.TProviderManager.Create(@SearchProviders[iLcv]);
          end;
        end;
        uStorage.Matrix_Services.Cluster.ID:=IniFile.ReadInteger('Matrix','Cluster ID',0);
        uStorage.Matrix_Services.Node.ID:=IniFile.ReadInteger('Matrix','Node ID',0);
        // Or use Something Else
        If uStorage.Cluster_Fill(uStorage.Matrix_Services.Cluster.ID,uStorage.Matrix_Services.Cluster) then begin
          uStorage.Matrix_Services.Node.ClusterID:=uStorage.Matrix_Services.Cluster.ID;
          If uStorage.MatrixNode_Fill(uStorage.Matrix_Services.Node.ID,uStorage.Matrix_Services.Node) then begin
            If Not uStorage.MatrixServices_Fill(uStorage.Matrix_Services) then begin
              Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,'ServiceCreate','MatrixServices_Fill Failed');
            end;
          end else begin
            Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,'ServiceCreate','MatrixNode_Fill Failed');
          end;
        end else begin
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,'ServiceCreate','Cluster_Fill Failed');
        end;
      end else begin
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,'ServiceCreate','Connect Failed');
      end;
    end else begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,'ServiceCreate','InitializeDBMS Failed');
    end;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,Service_RSR,Concat('ServiceCreate:',E.Message));
  end;
end;

procedure TASCMS.ServiceStop(Sender: TService; var Stopped: Boolean);
var
  Lcv:LongInt;
begin
  For Lcv:=0 to High(uStorage.Matrix_Services.List) do
    StopService(uStorage.Matrix_Services.List[Lcv]);
  Stopped:=True;
end;

procedure TASCMS.ServiceStart(Sender: TService; var Started: Boolean);
var
  iLcv: LongInt;
begin
  Started:=False;
  Try
    for iLcv := 0 to High(Matrix_Services.List) do
      StartService(Matrix_Services.List[iLcv]);
    for iLcv := 0 to High(Matrix_Services.List) do
      ContinueService(Matrix_Services.List[iLcv]);
    Started:=True;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry('Service Start Fatal Error','',E.Message);
  end;
end;

procedure TASCMS.ServicePause(Sender: TService; var Paused: Boolean);
var
  iLcv:LongInt;
begin
  Paused:=False;
  for iLcv := 0 to High(Matrix_Services.List) do
    PauseService(Matrix_Services.List[iLcv]);
  Paused:=True;
end;

procedure TASCMS.ServiceContinue(Sender: TService;
  var Continued: Boolean);
var
  iLcv:LongInt;
begin
  Continued:=False;
  Core.Logging.Native.WriteLogEntry('Continuing Services...','',Concat(IntToStr(Length(Matrix_Services.List)),' total'));
  for iLcv := 0 to High(Matrix_Services.List) do
    ContinueService(Matrix_Services.List[iLcv]);
  Continued:=True;
end;

Procedure  TASCMS.OnDBMSExeception(sModule,sLocation,sMessage:String);
begin
  Core.Logging.Native.WriteLogEntry(sModule,sLocation,sMessage);
end;

end.

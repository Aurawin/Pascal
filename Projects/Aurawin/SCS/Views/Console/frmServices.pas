unit frmServices;

interface

uses
  Classes,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  StdCtrls,
  ComCtrls,

  RSR,
  uPOP3d,
  hRTSPd,
  uRTSPd,
  uSMTPd,
  uHTTPd,

  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,

  Storage,
  Storage.Main,
  Storage.UserStorage,
  Storage.MatrixNodes,
  Storage.Domains,
  Storage.UserAccounts,
  Core.Utils.Sockets,

  Core.Utils.Forms,

  Core.Timer,
  FileUtil,
  SysUtils;

type

  { TfrmServices }
  TServiceItem=record
      POP3Server    : TPOPMailServer;
      SMTPServer    : TReceiveMailServer;
      HTTPServer    : THTTPServer;
      HTTPSServer   : THTTPServer;
      RTSPServer    : TRTSPServer;
  end;
  TServiceItems=Array of TServiceItem;

  TfrmServices = class(TForm)
    cbHTTPS: TCheckBox;
    cbPOP3: TCheckBox;
    cbHTTP: TCheckBox;
    cbDomains: TComboBox;
    cbRTSP: TCheckBox;
    cbSMTP: TCheckBox;
    gbServices: TGroupBox;
    gbDomains: TGroupBox;
    sbStatus: TStatusBar;
    procedure cbDomainsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FDomains    : Core.Arrays.Types.VarString;
    DebugItems  : TServiceItems;
    FInfoP      : PFormInfo;
    tiRefresh   : Core.Timer.Item;
    FNodeIP     : QWord;
  private
    procedure onRefreshTimer(TimerP:Core.Timer.PItem);
  public
    { public declarations }
  end;


var
  ServicesForm: TfrmServices;

implementation
uses {$ifdef Unix}BaseUnix,{$endif}
     DateUtils;
{ TfrmServices }
Const
  CheckBoxText                   : Array[boolean] of Core.Strings.VarString=('%s (Not Bound)','%s Server scaled to (%s) Active On Port %s');
  FLoading                       : Boolean = false;
  LOOPBACK_IP                    = 0;
  PORT_POP3                      = 110;
  PORT_SMTP                      = 25;
  PORT_HTTP                      = 80;
  PORT_HTTPS                     = 443;
  DEBUG_DOMAIN:Core.Strings.VarString            ='aurawin.com';

procedure TfrmServices.onRefreshTimer(TimerP:Core.Timer.PItem);
begin
  cbDomainsChange(cbDomains);
end;

procedure TfrmServices.FormCreate(Sender: TObject);
var
  iLcv:integer;
  iCount:integer;
begin
  FInfoP:=Core.Utils.Forms.List.Load(Self,Self,nil,nil);
  FLoading:=True;
  Try
    tiRefresh.Mode:=temSynchronize;
    tiRefresh.Location:='frmDebug.onRefreshTimer';
    tiRefresh.Event:=@onRefreshTimer;
    Core.Timer.Background.RegisterEvent(tiRefresh,LoadNoUpdate);

    if (Storage.NodeID=0) or (Storage.ResourceID=0) or (Storage.ClusterID=0) then exit;

    Storage.MatrixNodes.Node.DB.GetIP(Storage.Main.Task,Storage.NodeID,FNodeIP);
    sbStatus.SimpleText:=Concat('Loading services on [',Core.Utils.Sockets.InAddrToStr(FNodeIP),']');
    {$ifdef Unix}
      if BaseUnix.FpSetgid(0)=0 then begin
        if BaseUnix.FpSetuid(0)=0 then begin
          {$i frmServices.LoadServices.inc}
        end else begin
          sbStatus.SimpleText:=Concat('Unable to debug services on [',Core.Utils.Sockets.InAddrToStr(FNodeIP),']');
        end;
      end else begin
          sbStatus.SimpleText:=Concat('Unable to debug services on [',Core.Utils.Sockets.InAddrToStr(FNodeIP),']');
      end;
    {$else}
      {$i frmServices.LoadServices.inc}
    {$endif}
  finally
    FLoading:=false;
  end;
end;

procedure TfrmServices.FormDestroy(Sender: TObject);
var
  iLcv:integer;
begin
  Core.Timer.Background.UnloadEvent(tiRefresh,UnloadNoExecute);
  for iLcv:=0 to High(DebugItems) do begin
    if DebugItems[iLcv].POP3Server<>nil then
      DebugItems[iLcv].POP3Server.Terminate;
    if DebugItems[iLcv].SMTPServer<>nil then
      DebugItems[iLcv].SMTPServer.Terminate;
    if DebugItems[iLcv].HTTPServer<>nil then
      DebugItems[iLcv].HTTPServer.Terminate;
    if DebugItems[iLcv].HTTPSServer<>nil then
      DebugItems[iLcv].HTTPSServer.Terminate;
    if DebugItems[iLcv].RTSPServer<>nil then
      DebugItems[iLcv].RTSPServer.Terminate;
  end;
  Finalize(DebugItems);
end;

procedure TfrmServices.cbDomainsChange(Sender: TObject);
var
  iIndex:integer;
begin
  iIndex:=cbDomains.ItemIndex;
  if (iIndex<0) or (iIndex>=Length(DebugItems)) then begin
    cbPOP3.Enabled:=false;
    cbPOP3.Checked:=false;
    cbSMTP.Enabled:=false;
    cbSMTP.Checked:=false;
    cbHTTP.Enabled:=false;
    cbHTTP.Checked:=false;
    cbHTTPS.Enabled:=false;
    cbHTTPS.Checked:=false;
    cbRTSP.Enabled:=false;
    cbRTSP.Checked:=false;
  end else begin
    cbPOP3.Enabled:=(DebugItems[iIndex].POP3Server<>nil) and DebugItems[iIndex].POP3Server.Bound;
    cbPOP3.Checked:=(DebugItems[iIndex].POP3Server<>nil) and DebugItems[iIndex].POP3Server.Bound;
    if (DebugItems[iIndex].POP3Server<>nil) then
      cbPOP3.Caption:=Format(CheckBoxText[DebugItems[iIndex].POP3Server.Bound],['POP3',IntToStr(DebugItems[iIndex].POP3Server.Scale),IntToStr(DebugItems[iIndex].POP3Server.Port)])
    else
      cbPOP3.Caption:='POP3 Not installed';

    cbSMTP.Enabled:=(DebugItems[iIndex].SMTPServer<>nil) and DebugItems[iIndex].SMTPServer.Bound;
    cbSMTP.Checked:=(DebugItems[iIndex].SMTPServer<>nil) and DebugItems[iIndex].SMTPServer.Bound;
    if (DebugItems[iIndex].SMTPServer<>nil) then
      cbSMTP.Caption:=Format(CheckBoxText[DebugItems[iIndex].SMTPServer.Bound],['SMTP',IntToStr(DebugItems[iIndex].SMTPServer.Scale),IntToStr(DebugItems[iIndex].SMTPServer.Port)])
    else
      cbSMTP.Caption:='SMTP Not installed';
    cbHTTP.Enabled:=(DebugItems[iIndex].HTTPServer<>nil) and DebugItems[iIndex].HTTPServer.Bound;
    cbHTTP.Checked:=(DebugItems[iIndex].HTTPServer<>nil) and DebugItems[iIndex].HTTPServer.Bound;
    if (DebugItems[iIndex].HTTPServer<>nil) then
      cbHTTP.Caption:=Format(CheckBoxText[DebugItems[iIndex].HTTPServer.Bound],['HTTP',IntToStr(DebugItems[iIndex].HTTPServer.Scale),IntToStr(DebugItems[iIndex].HTTPServer.Port)])
    else
      cbHTTP.Caption:='HTTP Not installed';
    cbHTTPS.Enabled:=(DebugItems[iIndex].HTTPSServer<>nil) and DebugItems[iIndex].HTTPSServer.Bound;
    cbHTTPS.Checked:=(DebugItems[iIndex].HTTPSServer<>nil) and DebugItems[iIndex].HTTPSServer.Bound;
    if (DebugItems[iIndex].HTTPSServer<>nil) then
      cbHTTPS.Caption:=Format(CheckBoxText[DebugItems[iIndex].HTTPSServer.Bound],['HTTPS',IntToStr(DebugItems[iIndex].HTTPSServer.Scale),IntToStr(DebugItems[iIndex].HTTPSServer.Port)])
    else
      cbHTTPS.Caption:='HTTPS Not installed';

    cbRTSP.Enabled:=(DebugItems[iIndex].RTSPServer<>nil) and DebugItems[iIndex].RTSPServer.Bound;
    cbRTSP.Checked:=(DebugItems[iIndex].RTSPServer<>nil) and DebugItems[iIndex].RTSPServer.Bound;
    if (DebugItems[iIndex].RTSPServer<>nil) then
      cbRTSP.Caption:=Format(CheckBoxText[DebugItems[iIndex].RTSPServer.Bound],['RTSP',IntToStr(DebugItems[iIndex].RTSPServer.Scale),IntToStr(DebugItems[iIndex].RTSPServer.Port)])
    else
      cbRTSP.Caption:='RTSP Not installed';
  end;
  tiRefresh.Expires:=DateUtils.incMillisecond(Core.Timer.dtNow,1500);
end;

procedure TfrmServices.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
  Core.Utils.Forms.List.UnLoad(FInfoP^);
end;

initialization
  {$I frmServices.lrs}

end.


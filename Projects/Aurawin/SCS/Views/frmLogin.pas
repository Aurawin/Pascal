unit frmLogin;

{$mode objfpc}{$H+}

interface

uses
  Types,
  Classes,
  Interfaces,

  FileUtil,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Buttons,
  SynEdit,

  Core.Timer,
  Core.Strings,
  Core.Database,
  Core.Database.Types,
  Core.Database.Timer,

  App,
  App.Build,
  App.Consts,
  App.IniFile,

  frmProvider,
  frmKeyword,
  frmEdit,
  uRTSPd,

  Core.Utils.Forms,
  Core.Logging,

  Storage,
  Storage.Main,
  Storage.KeepAlive,

  Core.Arrays.LargeWord,

  SysUtils;
type
  { TLoginForm }
  TLoginForm = class(TForm)
    btnCancel: TBitBtn;
    btnLogin: TBitBtn;
    cbLoadServices: TCheckBox;
    cbLoadServices1: TCheckBox;
    cbLoadDisks: TCheckBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Image1: TImage;
    Label24: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    lblEncoding: TLabel;
    Panel1: TPanel;
    pnlButtons: TGroupBox;
    pnlNodeID1: TPanel;
    pnlEncoding: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    txtAuxNodes: TEdit;
    cmboEncoding: TComboBox;
    txtResourceID: TEdit;
    txtNodeID: TEdit;
    txtDataSource: TEdit;
    txtHostName: TEdit;
    txtClusterID: TEdit;
    txtPassword: TEdit;
    txtUserName: TEdit;
    GroupBox1: TGroupBox;
    Label25: TLabel;
    Label26: TLabel;
    pcPages: TPageControl;
    pnlLogo: TPanel;
    rgDrivers: TRadioGroup;
    tsCredentials: TTabSheet;
    tsDrivers: TTabSheet;
    pnlResourceID:TPanel;
    pnlClusterID:TPanel;
    pnlNodeID:TPanel;
    pnlHostName:TPanel;
    pnlSchema:TPanel;
    pnlUser:TPanel;
    pnlPassword:TPanel;

    procedure btnCancelClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure cbLoadDisksClick(Sender: TObject);
    procedure cbLoadServices1Click(Sender: TObject);
    procedure cmboEncodingChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure rgDriversClick(Sender: TObject);
    procedure txtAuxNodesChange(Sender: TObject);
    procedure txtClusterIDChange(Sender: TObject);
    procedure txtClusterIDKeyPress(Sender: TObject; var Key: char);
    procedure txtDataSourceChange(Sender: TObject);
    procedure txtHostNameChange(Sender: TObject);
    procedure txtNodeIDChange(Sender: TObject);
    procedure txtPasswordChange(Sender: TObject);
    procedure txtResourceIDChange(Sender: TObject);
    procedure txtUserNameChange(Sender: TObject);
  private
    FUpdateTimer : Core.Timer.Item;
    FInfoP       : PFormInfo;
  private
    procedure OnUpdateHeader(ItemP:Core.Timer.PItem);
    procedure OnDBException(sModule,sLocation,sTable,sTask,sMessage:Core.Strings.VarString);
    procedure OnDBConnected(Task:Core.Database.Types.TTask);
    procedure OnDBDisconnected(Task:Core.Database.Types.TTask);
  public
    { public declarations }
  end;

var
  LoginForm : TLoginForm;
 const
   EntryHeight=(
      {$ifdef Unix}
         {$ifdef Darwin}
            32
         {$else}
            34
         {$endif}
      {$else}
        30
      {$endif}
   );

implementation
uses
  uHTTPd,
  uXMPPd,
  frmConsole,
  Form.DBException,

  DateUtils;

{ TLoginForm }

procedure TLoginForm.OnUpdateHeader(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  Storage.SaveINIFile;
end;

procedure TLoginForm.OnDBConnected(Task:Core.Database.Types.TTask);
var
  P:Pointer;
begin
  If frmLogin.LoginForm.Visible then begin

    uHTTPd.StartBackgroundTimers();
    uRTSPd.StartBackgroundTimers();

    Core.Database.Timer.Init(Storage.Main.Header);

    frmConsole.ConsoleForm.pcPages.ActivePage:=frmConsole.ConsoleForm.tsWelcome;
    frmConsole.ConsoleForm.lbConnection.Caption:='Connected';
    frmConsole.ConsoleForm.ProcessStartup;

    P:=@Application.Mainform;
    Pointer(P^):=frmConsole.ConsoleForm;

  end;
  Screen.Cursor:=crDefault;
  frmLogin.LoginForm.Visible:=false;
end;

procedure TLoginForm.OnDBDisconnected(Task:Core.Database.Types.TTask);
begin
  If frmConsole.ConsoleForm.Visible then begin
    frmConsole.ConsoleForm.lbConnection.Caption:='Disconnected';
    frmConsole.ConsoleForm.lbStatus.Caption:='You have been disconnected.';
    frmConsole.ConsoleForm.Display_Welcome;
    frmConsole.ConsoleForm.Enabled:=false;
    frmLogin.LoginForm.Visible:=true;
    frmLogin.LoginForm.BringToFront;
  end;
  Screen.Cursor:=crDefault;
end;

procedure TLoginForm.txtDataSourceChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.Main.Header.Schema:=txtDataSource.Text;
  FUpdateTimer.Expires:=IncMilliSecond(Core.Timer.dtNow,TIMER_DEF_DELAY_MS_SMALL);
end;

procedure TLoginForm.txtHostNameChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.Main.Header.HostName:=txtHostName.Text;
  FUpdateTimer.Expires:=IncMilliSecond(Core.Timer.dtNow,TIMER_DEF_DELAY_MS_TYPE);
end;

procedure TLoginForm.txtNodeIDChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.NodeID:=StrToIntDef(txtNodeID.Text,0);
  App.IniFile.Native.WriteInteger(INI_DB_SECT_MATRIX,INI_DB_VALUE_NODEID,Storage.NodeID);
end;

procedure TLoginForm.txtPasswordChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.Main.Header.Password:=txtPassword.Text;
  FUpdateTimer.Expires:=IncMilliSecond(Core.Timer.dtNow,TIMER_DEF_DELAY_MS_TYPE);
end;

procedure TLoginForm.txtResourceIDChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.ResourceID:=StrToIntDef(txtResourceID.Text,0);
  App.IniFile.Native.WriteInteger(INI_DB_SECT_MATRIX,INI_DB_VALUE_RESOURCEID,Storage.ResourceID);
end;

procedure TLoginForm.txtUserNameChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.Main.Header.Username:=txtUserName.Text;
  FUpdateTimer.Expires:=IncMilliSecond(Core.Timer.dtNow,TIMER_DEF_DELAY_MS_TYPE);
end;

procedure TLoginForm.btnCancelClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TLoginForm.btnLoginClick(Sender: TObject);
begin
  Screen.Cursor:=crHourglass;
  Application.ProcessMessages;
  Storage.Main.Task.Connection.Connected:=true;
  if (Storage.Main.Task.Connection.Connected=true) then
    Screen.Cursor:=crDefault;
end;

procedure TLoginForm.cbLoadDisksClick(Sender: TObject);
begin
  if not Visible then exit;
  Storage.auDisks:=cbLoadDisks.Checked;
  App.IniFile.Native.WriteBool(INI_DB_SECT_MATRIX,INI_LOAD_DISKS,Storage.auDisks);
end;

procedure TLoginForm.cbLoadServices1Click(Sender: TObject);
begin
  if not Visible then exit;
  Storage.guiServices:=cbLoadServices.Checked;
  App.IniFile.Native.WriteBool(INI_DB_SECT_MATRIX,INI_GUI_LOAD_SERVICES,Storage.guiServices);
end;

procedure TLoginForm.cmboEncodingChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.Main.Header.Encoding:=cmboEncoding.Text;
  FUpdateTimer.Expires:=IncMilliSecond(Core.Timer.dtNow,TIMER_DEF_DELAY_MS_SMALL);
end;

procedure TLoginForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
  Application.Terminate;
end;

procedure TLoginForm.FormCreate(Sender: TObject);
var
  iLcv:Core.Database.Types.Driver;
begin
  for iLcv:=Low(Core.Database.Types.Driver) to High(Core.Database.Types.Driver) do
    rgDrivers.Items.Add(DB_Modes[iLcv]);
  FInfoP:=Core.Utils.Forms.List.Load(Self,Self,nil,nil);
  {$ifdef Windows}
    btnCancel.BorderSpacing.Top:=0;
    btnLogin.BorderSpacing.Top:=0;
  {$endif}
  cmboEncoding.Items.StrictDelimiter:=true;
  cmboEncoding.Items.Delimiter:=',';
  cmboEncoding.Items.DelimitedText:=Core.Database.Types.CHAR_ENCODING;

  App.IniFile.Init(App.Files.IniName());
  Core.Logging.Start(App.Files.LogName());



  Storage.ReadINIFile();
  Storage.Main.Init();

  Storage.Main.Task.OnConnected:=@OnDBConnected;
  Storage.Main.Task.OnDisconnected:=@OnDBDisconnected;

  pcPages.PageIndex:=0;

  cmboEncoding.Text:=Storage.Main.Header.Encoding;

  txtUserName.Text:=Storage.Main.Header.Username;
  txtPassword.Text:=Storage.Main.Header.Password;
  txtHostName.Text:=Storage.Main.Header.HostName;
  txtDataSource.Text:=Storage.Main.Header.Schema;

  rgDrivers.ItemIndex:=Integer(Storage.Main.Header.Mode);

  txtClusterID.Text:=IntToStr(Storage.ClusterID);
  txtResourceID.Text:=IntToStr(Storage.ResourceID);
  txtNodeID.Text:=IntToStr(Storage.NodeID);
  txtAuxNodes.Text:=Core.Arrays.LargeWord.toString(Storage.AuxNodes,',');
  cbLoadServices.Checked:=Storage.guiServices;
  cbLoadDisks.Checked:=Storage.auDisks;

  FUpdateTimer.Expires:=0;
  FUpdateTimer.Event:=@OnUpdateHeader;
  FUpdateTimer.Location:='frmLogin.OnUpdateHeader';
  FUpdateTimer.Mode:=temNormal;
  FUpdateTimer.Priority:=tpIdle;

  Core.Timer.Background.RegisterEvent(FUpdateTimer,AddToStack);

  pnlResourceID.Height:=EntryHeight;
  pnlClusterID.Height:=EntryHeight;
  pnlNodeID.Height:=EntryHeight;

  pnlHostName.Height:=EntryHeight;
  pnlSchema.Height:=EntryHeight;

  pnlUser.Height:=EntryHeight;
  pnlPassword.Height:=EntryHeight;
end;

procedure TLoginForm.FormDestroy(Sender: TObject);
begin
  Core.Timer.Background.UnloadEvent(FUpdateTimer,UnloadNoExecute);
  Core.Utils.Forms.List.UnLoad(FInfoP^);
  Core.Timer.Done(FUpdateTimer);
end;

procedure TLoginForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#27 then  btnCancelClick(Sender);
end;

procedure TLoginForm.OnDBException(sModule,sLocation,sTable,sTask,sMessage:Core.Strings.VarString);
begin
  Form.DBException.ShowException(sModule,sLocation,sTable,sTask,sMessage);
end;

procedure TLoginForm.rgDriversClick(Sender: TObject);
begin
  if not Visible then exit;
  Storage.Main.Header.Mode:=Core.Database.Types.Driver(rgDrivers.ItemIndex);
  FUpdateTimer.Expires:=IncMilliSecond(Core.Timer.dtNow,TIMER_DEF_DELAY_MS_SMALL);
  // What to do?  Restart ?
end;

procedure TLoginForm.txtAuxNodesChange(Sender: TObject);
begin
  if not Visible then exit;
  Core.Arrays.LargeWord.fromString(txtAuxNodes.Text,Storage.AuxNodes,',');
  App.IniFile.Native.WriteString(INI_DB_SECT_MATRIX,INI_DB_VALUE_AUXILIARY_NODES,Core.Arrays.LargeWord.toString(AuxNodes,','));
end;

procedure TLoginForm.txtClusterIDChange(Sender: TObject);
begin
  if not Visible then exit;
  Storage.ClusterID:=StrToIntDef(txtClusterID.Text,0);
  App.IniFile.Native.WriteInteger(INI_DB_SECT_MATRIX,INI_DB_VALUE_CLUSTERID,Storage.ClusterID);
end;

procedure TLoginForm.txtClusterIDKeyPress(Sender: TObject; var Key: char);
begin
  if (Sender=txtAuxNodes) and (Key=#44) then Exit;
  {$i Input.KeyPress.OnlyNumbers.inc}
end;

initialization
{$I frmLogin.lrs}
end.


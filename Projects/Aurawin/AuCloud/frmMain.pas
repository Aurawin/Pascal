unit frmMain;

interface

uses
  Types,
  Classes,
  SysUtils,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,

  Buttons,
  Menus,

  auSettings,
  auLang,

  RSR,
  RSR.HTTP,
  RSR.DNS,

  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.Arrays.LargeInt,
  Core.Arrays.Bytes,
  Core.Streams,
  Core.Utils.Files,
  Core.Utils.Time,
  Core.Timer,
  Core.XML,
  Core.Logging,
  Core.Database.Types,

  Encryption.SSL,

  Storage,
  Storage.Main,
  Storage.UserStorage,
  Storage.VDM,
  Storage.Social,
  Storage.Social.Network,
  Storage.Social.Sync,
  Storage.Social.Sync.Pipes,
  Storage.Social.Sync.Queue,
  Storage.UserAccounts,

  App,
  App.Consts,
  App.Build,

  Core.Utils.TreeView,
  Core.Utils.ListView,

  uEngine,


  DOM,
  XMLRead,
  StdCtrls,
  ComCtrls, Grids;

type
  { TAurawin }
  TPathEntry=class;

  TAurawin = class(TForm)
    btnRefreshConnections: TButton;
    btnResources: TPanel;
    btnSettings: TPanel;
    btnResourceNew: TBitBtn;
    btnResourceDelete: TBitBtn;
    btnConnect: TBitBtn;
    btnStatus: TPanel;
    btnRefreshNetworks: TButton;
    cbOtherNetworks: TComboBox;
    cbRememberCookies: TCheckBox;
    cbMyNetworks: TComboBox;
    lbSize: TLabel;
    pnlLocation: TPanel;
    pnlConnection1: TPanel;
    sgQueue: TStringGrid;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    hdrMyNetwork: THeaderControl;
    hdrOtherNetwork: THeaderControl;
    lblVersion: TLabel;
    lblDomain: TLabel;
    miSep: TMenuItem;
    pnlConnect: TPanel;
    pnlCredBtns: TPanel;
    pcPipes: TPageControl;
    pnlCredDomain: TPanel;
    imgResources: TImage;
    imgSettings: TImage;
    imgStats: TImage;
    lblResources: TLabel;
    lblSettings: TLabel;
    lblStatus: TLabel;
    lblSplash: TLabel;
    lblPassword: TLabel;
    lblName: TLabel;
    MenuItem1: TMenuItem;
    miResume: TMenuItem;
    miPause: TMenuItem;
    miExit: TMenuItem;
    miResourcesCredsOn: TMenuItem;
    miResourcesCredsOff: TMenuItem;
    MenuItem4: TMenuItem;
    miResourceDelete: TMenuItem;
    MenuItem6: TMenuItem;
    miResourceNew: TMenuItem;
    miResourceEdit: TMenuItem;
    pnlNav: TPanel;
    pnlCredOptions: TPanel;
    pnlCredName: TPanel;
    pnlCredPassword: TPanel;
    puTray: TPopupMenu;
    puResources: TPopupMenu;
    scMyPaths: TScrollBox;
    scOtherPaths: TScrollBox;
    sConnect: TShape;
    tbActive: TToggleBox;
    tglbHome: TToggleBox;
    tsConnections: TTabSheet;
    tsMyNetworks: TTabSheet;
    tsStatus: TTabSheet;
    tiSystem: TTrayIcon;
    txtPassword: TEdit;
    txtName: TEdit;
    lvResources: TListView;
    pnlResourceCaption: TPanel;
    pnlResourceNew: TPanel;
    pnlResource: TPanel;
    pnlCreds: TPanel;
    oDirectory: TSelectDirectoryDialog;
    pnlResources: TPanel;
    tsResources: TTabSheet;
    tsCredentials: TTabSheet;
    lbStatus: TLabel;
    pcMain: TPageControl;
    pnlConnection: TPanel;
    pnlStatus: TPanel;
    pnlText: TPanel;
    tsSettings: TTabSheet;
    txtDomain: TEdit;
    procedure btnRefreshNetworksClick(Sender: TObject);
    procedure btnResourceNewClick(Sender: TObject);
    procedure btnResourceDeleteClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure cbOtherNetworksChange(Sender: TObject);
    procedure cbMyNetworksChange(Sender: TObject);
    procedure cbRememberCookiesChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure imgStatsClick(Sender: TObject);
    procedure imgSettingsClick(Sender: TObject);
    procedure imgResourcesClick(Sender: TObject);
    procedure lbStatusClick(Sender: TObject);
    procedure lvResourcesDblClick(Sender: TObject);
    procedure lvResourcesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miExitClick(Sender: TObject);
    procedure miResourceEditClick(Sender: TObject);
    procedure miResourcesCredsOffClick(Sender: TObject);
    procedure miResourcesCredsOnClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pcMainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure puResourcesPopup(Sender: TObject);
    procedure tiSystemClick(Sender: TObject);
    procedure tbActiveChange(Sender: TObject);
    procedure tglbHomeChange(Sender: TObject);
    procedure tsConnectionsResize(Sender: TObject);
    procedure tsMyNetworksResize(Sender: TObject);
    procedure tsStatusResize(Sender: TObject);
    procedure txtDomainEditingDone(Sender: TObject);
    procedure txtNameEditingDone(Sender: TObject);
    procedure txtPasswordEditingDone(Sender: TObject);
  private
    { private declarations }
    FNameWidth                   : LongInt;
    FPathWidth                   : LongInt;
    FButtonWidth                 : LongInt;
    FDirectionWidth              : LongInt;
    FModeWidth                   : LongInt;
    FLoading                     : Boolean;
    FSwitching                   : Boolean;
    FSetAsDefaultResource        : Boolean;

    tiSocialAutoSave             : Core.Timer.Item;
    tiSelectRc                   : Core.Timer.Item;
    tiSize                       : Core.Timer.Item;
    tiProcessSyncItems           : Core.Timer.Item;

    FBtnLabel                    : TLabel;
  private
    FXMLParser                   : TDOMParser;
    FManifestFactor              : TMemoryStream;
  private
    procedure ResetLoginMessage();

    procedure NetworksRetrieved(var Items:Storage.Social.Network.TNetworks);
    procedure ConnectionsRetrieved(var Items:Storage.Social.Connection.TConnections; var Networks:Storage.Social.Network.TNetworks);
    procedure UserAccountRetrieved(var Item:Storage.UserAccounts.Items.Item);
    procedure SocialSyncRetrieved(Var Item:Storage.Social.Sync.TSync);

    procedure SyncItemQueued(Item:Storage.Social.Sync.Queue.TItem);
    procedure SyncItemRemoved(Item:Storage.Social.Sync.Queue.TItem);

    procedure SocialSyncModified(Var Item:Storage.Social.Sync.TSync);

    procedure ResourcesListed(Var Resources:Storage.VDM.Resources.TResources);
    procedure ResourceAdded(Var Resource:Storage.VDM.Resources.TResource);
    procedure ResourceDeleted(Var Resource:Storage.VDM.Resources.TResource);
    procedure ResourceWritten(Var Resource:Storage.VDM.Resources.TResource);

    procedure CoreError(Var SR:TRSR; cObject,cCommand:Core.Strings.VarString; Code:Word);
  private
    procedure SocketConnected(RSRP:PRSR);
    procedure SocketDisconnected(RSRP:PRSR);
    procedure SocketFinalized(RSRP:PRSR);
    procedure SocketError(RSRP:PRSR);
    procedure ResetButtons();
    procedure ResetButton(btnText:TLabel);
    procedure SetButton(btnText:TLabel);
  private
    procedure AllocateSocket();
    procedure Load();
    procedure ClearMyNetworkEntries();
    procedure SetSyncEnginePipes(var Item:Storage.Social.Sync.TSync);
    procedure ClearOtherNetworkEntries();
  private
    procedure TI_Size(ItemP:Core.Timer.PItem);
    procedure TI_ProcessSyncItems(ItemP:Core.Timer.PItem);
    procedure TI_SocialAutoSave(ItemP:Core.Timer.PItem);
    procedure TI_SelectRc(ItemP:Core.Timer.PItem);

  protected
    FDataRSRP             : PRSR;
  public
    procedure NavButtonChange(Expanded:boolean);
    procedure NavDisplayChange(Expanded:boolean);
  end;

  TPathEntry=class(TPanel)
  private
    FLoading              : Boolean;

  private
    procedure  OnBrowseMyNetworkClick(Sender:TObject);
    procedure  OnBrowseOtherNetworkClick(Sender:TObject);
    procedure  OnMyNetworkValueChanged(Sender:TObject);
    procedure  OnOtherNetworkValueChanged(Sender:TObject);
  protected
    procedure  Resize; override;
  public
    pnlName    : TPanel;
    pnlValue   : TPanel;
    pnlCredOptions : TPanel;
    txtValue   : TEdit;
    tgbSync    : TToggleBox;
    tgbMode    : TToggleBox;
    btnBrowse  : TBitBtn;
  public
    Constructor CreateAsMyNetwork(PipeP:Storage.Social.Sync.Pipes.PItem);
    Constructor CreateAsOtherNetwork(PipeP:Storage.Social.Sync.Pipes.PItem);
  end;

var
  Aurawin   : TAurawin;
  AuSockets : TAuSocketMan;

implementation
uses DateUtils, Math, frmResources;
{$R *.lfm}

{ TAurawin }

constructor TPathEntry.CreateAsMyNetwork(PipeP:Storage.Social.Sync.Pipes.PItem);
begin
  FLoading:=True;

  inherited Create(Aurawin.scMyPaths);

  Parent:=Aurawin.scMyPaths;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  Align:=alTop;
  AutoSize:=true;
  Caption:='';
  Height:=40;
  Top:=40*Parent.ControlCount;

  pnlName:=TPanel.Create(Self);
  pnlName.Parent:=Self;
  pnlName.Align:=alLeft;

  pnlValue:=TPanel.Create(Self);
  pnlValue.Parent:=Self;
  pnlValue.Align:=alClient;

  pnlCredOptions:=TPanel.Create(Self);
  pnlCredOptions.Parent:=Self;
  pnlCredOptions.Align:=alRight;

  pnlName.Caption:=PipeP^.Name;
  pnlName.BevelInner:=bvNone;
  pnlName.BevelOuter:=bvNone;
  pnlName.AutoSize:=false;
  pnlName.Alignment:=taRightJustify;
  pnlName.Width:=80;

  pnlCredOptions.Caption:='';
  pnlCredOptions.BevelInner:=bvNone;
  pnlCredOptions.BevelOuter:=bvNone;
  pnlCredOptions.Align:=alRight;
  pnlCredOptions.AutoSize:=false;
  pnlCredOptions.Width:=145;
  pnlCredOptions.BorderSpacing.Left:=2;
  pnlCredOptions.BorderSpacing.Right:=4;
  pnlCredOptions.BorderSpacing.Top:=2;

  pnlValue.Caption:='';
  pnlValue.BevelInner:=bvNone;
  pnlValue.BevelOuter:=bvNone;
  pnlValue.Align:=alClient;
  pnlValue.AutoSize:=true;

  txtValue:=TEdit.Create(pnlValue);
  txtValue.ReadOnly:=true;
  txtValue.Parent:=pnlValue;
  txtValue.Align:=alClient;
  txtValue.BorderSpacing.Top:=4;
  txtValue.BorderSpacing.Bottom:=4;
  txtValue.BorderSpacing.Left:=4;
  txtValue.BorderSpacing.Right:=2;
  txtValue.Text:=PipeP^.Path;

  tgbSync:=TToggleBox.Create(pnlCredOptions);
  tgbSync.Parent:=pnlCredOptions;
  tgbSync.Autosize:=false;
  tgbSync.Align:=alLeft;
  tgbSync.Checked:=(PipeP^.Direction<>Direction.Off);
  tgbSync.Caption:=auLang.Table.Options.PipeFlow[PipeP^.Direction];
  tgbSync.Width:=82;
  tgbSync.OnChange:=@OnMyNetworkValueChanged;

  btnBrowse:=TBitBtn.Create(pnlCredOptions);
  btnBrowse.Parent:=pnlCredOptions;
  btnBrowse.Align:=alLeft;
  btnBrowse.Caption:=auLang.Table.Labels.Browse;
  btnBrowse.AutoSize:=true;
  btnBrowse.Left:=0;
  btnBrowse.OnClick:=@OnBrowseMyNetworkClick;

  AutoSize:=true;
  FLoading:=false;
end;

constructor TPathEntry.CreateAsOtherNetwork(PipeP:Storage.Social.Sync.Pipes.PItem);
begin
  FLoading:=True;

  inherited Create(Aurawin.scOtherPaths);

  Parent:=Aurawin.scOtherPaths;
  BevelInner:=bvNone;
  BevelOuter:=bvNone;
  Align:=alTop;
  AutoSize:=true;
  Caption:='';
  Height:=40;
  Top:=40*Parent.ControlCount;

  pnlName:=TPanel.Create(Self);
  pnlName.Parent:=Self;
  pnlName.Align:=alLeft;

  pnlValue:=TPanel.Create(Self);
  pnlValue.Parent:=Self;
  pnlValue.Align:=alClient;

  pnlCredOptions:=TPanel.Create(Self);
  pnlCredOptions.Parent:=Self;
  pnlCredOptions.Align:=alRight;

  pnlName.Caption:=PipeP^.Name;
  pnlName.BevelInner:=bvNone;
  pnlName.BevelOuter:=bvNone;
  pnlName.AutoSize:=false;
  pnlName.Alignment:=taRightJustify;
  pnlName.Width:=80;

  pnlCredOptions.Caption:='';
  pnlCredOptions.BevelInner:=bvNone;
  pnlCredOptions.BevelOuter:=bvNone;
  pnlCredOptions.Align:=alRight;
  pnlCredOptions.AutoSize:=false;
  pnlCredOptions.Width:=145;
  pnlCredOptions.BorderSpacing.Left:=2;
  pnlCredOptions.BorderSpacing.Right:=4;
  pnlCredOptions.BorderSpacing.Top:=2;

  pnlValue.Caption:='';
  pnlValue.BevelInner:=bvNone;
  pnlValue.BevelOuter:=bvNone;
  pnlValue.Align:=alClient;
  pnlValue.AutoSize:=true;

  txtValue:=TEdit.Create(pnlValue);
  txtValue.ReadOnly:=true;
  txtValue.Parent:=pnlValue;
  txtValue.Align:=alClient;
  txtValue.BorderSpacing.Top:=4;
  txtValue.BorderSpacing.Bottom:=4;
  txtValue.BorderSpacing.Left:=4;
  txtValue.BorderSpacing.Right:=2;
  txtValue.Text:=PipeP^.Path;

  tgbSync:=TToggleBox.Create(pnlCredOptions);
  tgbSync.Parent:=pnlCredOptions;
  tgbSync.Autosize:=false;
  tgbSync.Align:=alLeft;
  tgbSync.Checked:=(PipeP^.Direction<>Direction.Off);
  tgbSync.Caption:=auLang.Table.Options.PipeFlow[PipeP^.Direction];
  tgbSync.Width:=82;
  tgbSync.OnChange:=@OnOtherNetworkValueChanged;

  btnBrowse:=TBitBtn.Create(pnlCredOptions);
  btnBrowse.Parent:=pnlCredOptions;
  btnBrowse.Align:=alLeft;
  btnBrowse.Caption:=auLang.Table.Labels.Browse;
  btnBrowse.AutoSize:=true;
  btnBrowse.Left:=0;
  btnBrowse.OnClick:=@OnBrowseOtherNetworkClick;

  AutoSize:=true;
  FLoading:=false;
end;

procedure TPathEntry.OnBrowseMyNetworkClick(Sender:TObject);
var
  pipeP:Storage.Social.Sync.Pipes.PItem;
  hdrP:Storage.Social.Sync.PHeader;
  sncP:Storage.Social.Sync.PSync;
  idx:integer;
  netP:Storage.Social.Network.PNetwork;
begin
  txtValue.SetFocus();
  idx:=Aurawin.cbMyNetworks.ItemIndex;
  if idx<>-1 then begin
    netP:=AuSockets.Networks[idx];
    hdrP:=AuSockets.getSync(netP^);
    sncP:=hdrP^.ItemP;
    pipeP:=AuSockets.getPipe(pnlName.Caption,sncP^);
    FLoading:=true;
    sncP^.Modified:=true;
    Aurawin.oDirectory.FileName:=UTF8ToAnsi(pipeP^.Path);
    if Aurawin.oDirectory.Execute then begin
      pipeP^.Path:=Aurawin.oDirectory.FileName;
      txtValue.Text:=Aurawin.oDirectory.FileName;
      Aurawin.tiSocialAutoSave.Expires:=IncMillisecond(Core.Timer.dtUT,AUTO_SAVE_DELAY);
    end;
    FLoading:=false;
  end;
end;

procedure TPathEntry.OnBrowseOtherNetworkClick(Sender:TObject);
var
  pipeP:Storage.Social.Sync.Pipes.PItem;
  sncP:Storage.Social.Sync.PSync;
  hdrP:Storage.Social.Sync.PHeader;
  idx:integer;
  netP:Storage.Social.Network.PNetwork;
begin
  txtValue.SetFocus();
  idx:=Aurawin.cbOtherNetworks.ItemIndex;
  if idx<>-1 then begin
    netP:=AuSockets.OtherNetworks[idx];
    hdrP:=AuSockets.getSync(netP^);
    sncP:=hdrP^.ItemP;
    pipeP:=AuSockets.getPipe(pnlName.Caption,sncP^);
    FLoading:=true;
    sncP^.Modified:=true;
    Aurawin.oDirectory.FileName:=UTF8ToAnsi(pipeP^.Path);
    if Aurawin.oDirectory.Execute then begin
      pipeP^.Path:=AnsiToUTF8(Aurawin.oDirectory.FileName);
      txtValue.Text:=Aurawin.oDirectory.FileName;
      Aurawin.tiSocialAutoSave.Expires:=IncMillisecond(Core.Timer.dtUT,AUTO_SAVE_DELAY);
    end;
    FLoading:=false;
  end;
end;

procedure TPathEntry.OnMyNetworkValueChanged(Sender:TObject);
var
  pipeP:Storage.Social.Sync.Pipes.PItem;
  sncP:Storage.Social.Sync.PSync;
  hdrP:Storage.Social.Sync.PHeader;
  idx:integer;
  netP:Storage.Social.Network.PNetwork;
begin
  if FLoading then Exit;

  idx:=Aurawin.cbMyNetworks.ItemIndex;
  if idx<>-1 then begin
    netP:=AuSockets.Networks[idx];
    hdrP:=AuSockets.getSync(netP^);
    sncP:=hdrP^.ItemP;
    pipeP:=AuSockets.getPipe(pnlName.Caption,sncP^);
    FLoading:=true;
    inc(pipeP^.Direction,1);
    if pipeP^.Direction>Storage.Social.Sync.Pipes.Direction.Both then begin
      pipeP^.Direction:=Storage.Social.Sync.Pipes.Direction.Off;
      tgbSync.Checked:=false;
    end else
      tgbSync.Checked:=true;
    sncP^.Modified:=true;
    tgbSync.Caption:=auLang.Table.Options.PipeFlow[pipeP^.Direction];
    Aurawin.tiSocialAutoSave.Expires:=IncMillisecond(Core.Timer.dtUT,AUTO_SAVE_DELAY);
    FLoading:=false;
  end;
end;

procedure TPathEntry.OnOtherNetworkValueChanged(Sender:TObject);
var
  pipeP:Storage.Social.Sync.Pipes.PItem;
  sncP:Storage.Social.Sync.PSync;
  hdrP:Storage.Social.Sync.PHeader;
  idx:integer;
  netP:Storage.Social.Network.PNetwork;
begin
  if FLoading then Exit;
  idx:=Aurawin.cbOtherNetworks.ItemIndex;
  if idx<>-1 then begin
    netP:=AuSockets.OtherNetworks[idx];
    hdrP:=AuSockets.getSync(netP^);
    sncP:=hdrP^.ItemP;


    pipeP:=AuSockets.getPipe(pnlName.Caption,sncP^);
    FLoading:=true;
    inc(pipeP^.Direction,1);
    if pipeP^.Direction>Storage.Social.Sync.Pipes.Direction.Both then begin
      pipeP^.Direction:=Storage.Social.Sync.Pipes.Direction.Off;
      tgbSync.Checked:=false;
    end else
      tgbSync.Checked:=true;

    sncP^.Modified:=true;
    tgbSync.Caption:=auLang.Table.Options.PipeFlow[pipeP^.Direction];
    Aurawin.tiSocialAutoSave.Expires:=IncMillisecond(Core.Timer.dtUT,AUTO_SAVE_DELAY);
    FLoading:=false;
  end;
end;

procedure TPathEntry.Resize();
begin
  if FLoading then exit;
  Aurawin.FNameWidth:=pnlName.Width+4;
  Aurawin.FPathWidth:=txtValue.Width+4;
  Aurawin.FButtonWidth:=btnBrowse.Width+4;
  Aurawin.FDirectionWidth:=tgbSync.Width;
  if tgbMode<>nil then
    Aurawin.FModeWidth:=tgbMode.Width;
end;

procedure TAurawin.Load();
begin
  tbActive.Checked:=false;
  with Settings do begin
    Load();
    txtName.Text:=Username;
    txtPassword.Text:=Password;
    txtDomain.Text:=Domain;
    cbRememberCookies.Checked:=SaveLogin;
  end;
end;

procedure TAurawin.NavButtonChange(Expanded:boolean);
begin
  if Expanded=true then begin
    lblResources.Visible:=true;
    lblStatus.Visible:=true;
    lblSettings.Visible:=true;

    btnResources.Visible:=true;
    btnSettings.Visible:=true;
    btnStatus.Visible:=true;

  end else begin
    if pcMain.ActivePage=tsResources then begin
       lblResources.Visible:=false;
       lblStatus.Visible:=true;
       lblSettings.Visible:=true;
       btnResources.Visible:=true;
       btnSettings.Visible:=false;
       btnStatus.Visible:=false;
    end else if pcMain.ActivePage=tsSettings then begin
       lblSettings.Visible:=false;
       lblStatus.Visible:=true;
       lblResources.Visible:=true;

       btnResources.Visible:=false;
       btnSettings.Visible:=true;
       btnStatus.Visible:=false;
    end else if pcMain.ActivePage=tsStatus then begin
       lblStatus.Visible:=false;
       lblSettings.Visible:=true;
       lblResources.Visible:=true;

       btnResources.Visible:=false;
       btnSettings.Visible:=false;
       btnStatus.Visible:=true;
    end;
  end;
end;

procedure TAurawin.NavDisplayChange(Expanded:boolean);
begin
  if Expanded=true then begin
    tglbHome.Caption:=auLang.Table.Symbol.Chevron.Left;
  end else begin
    tglbHome.Caption:=auLang.Table.Symbol.Chevron.Right;
  end;
  NavButtonChange(Expanded);

  if pcMain.ActivePage=tsResources then begin
    pnlLocation.Caption:=auLang.Table.Labels.Resources;
  end else if pcMain.ActivePage=tsSettings then begin
    pnlLocation.Caption:=auLang.Table.Labels.Settings;
  end else if pcMain.ActivePage=tsStatus then begin
    pnlLocation.Caption:=auLang.Table.Labels.Status;
  end;
end;

procedure TAurawin.ClearMyNetworkEntries();
begin
  scMyPaths.DisableAlign();
  Try
    While scMyPaths.ControlCount>0 do
      scMyPaths.Controls[0].Free();
  Finally
    scMyPaths.EnableAlign();
  end;
end;

procedure TAurawin.ClearOtherNetworkEntries();
begin
  scOtherPaths.DisableAlign();
  Try
    While scOtherPaths.ControlCount>0 do
      scOtherPaths.Controls[0].Free();
  Finally
    scOtherPaths.EnableAlign();
  end;
end;

procedure TAurawin.SocialSyncRetrieved(var Item:Storage.Social.Sync.TSync);
begin
  if Item.Network.OwnerID=Settings.UserID then begin
    ClearMyNetworkEntries();
  end else begin
    ClearOtherNetworkEntries();
  end;
  SetSyncEnginePipes(Item);
end;

procedure TAurawin.SyncItemQueued(Item:Storage.Social.Sync.Queue.TItem);
begin
  sgQueue.InsertRowWithValues(
    sgQueue.RowCount,
    [
      Item.LocalizedFile,
      Core.Strings.toString(Item.FileP^.Size),
      Item.SyncP^.Network.Title,
      Storage.Social.Sync.Queue.Status.Kind[Item.Kind]^,
      auLang.Table.Engine.Status.Sync.Scheduled
    ]
  );
  sgQueue.Objects[0,sgQueue.RowCount-1]:=Item;
  if (tiSize.Expires=0) then
    tiSize.Expires:=IncMillisecond(Core.Timer.dtUT,auSettings.SYNC_CALCULATE_SIZE);
end;

procedure TAurawin.SyncItemRemoved(Item:Storage.Social.Sync.Queue.TItem);
var
  iLcv:integer;
begin
  for iLcv:=0 to sgQueue.RowCount-1 do begin
    if sgQueue.Objects[0,iLcv]=Item then begin
       sgQueue.DeleteRow(iLcv);
       break;
    end;
  end;
  if (tiSize.Expires=0) then
    tiSize.Expires:=IncMillisecond(Core.Timer.dtUT,auSettings.SYNC_CALCULATE_SIZE);
end;

procedure TAurawin.SocialSyncModified(var Item:Storage.Social.Sync.TSync);
begin
  tiSocialAutoSave.Expires:=IncMillisecond(Now,AUTO_SAVE_DELAY);
end;

procedure TAurawin.NetworksRetrieved(var Items:Storage.Social.Network.TNetworks);
var
  iLcv:integer;
  netP:PNetwork;
begin

  cbMyNetworks.Items.Clear();
  for iLcv:=0 to High(Items) do begin
    netP:=Items[iLcv];
    cbMyNetworks.Items.Add(netP^.Title);
  end;
  AuSockets.ListConnections(FDataRSRP^);

end;

procedure TAurawin.ConnectionsRetrieved(var Items:Storage.Social.Connection.TConnections; var Networks:Storage.Social.Network.TNetworks);
var
  iLcv:integer;
  netP:PNetwork;
begin
  cbOtherNetworks.Items.Clear();
  for iLcv:=0 to High(Networks) do begin
    netP:=Networks[iLcv];
    cbOtherNetworks.Items.Add(netP^.Title);
  end;
end;

procedure TAurawin.UserAccountRetrieved(var Item:Storage.UserAccounts.Items.Item);
begin
  // The user was authenticated propperly.
  Settings.Throttle:=Item.Throttle;
  FDataRSRP^.Throttle.Limit:=Item.Throttle;
  Settings.UserID:=Item.ID;
  pnlNav.Visible:=True;
  pcMain.ActivePage:=tsSettings;
  pnlNav.Enabled:=true;
  pnlStatus.Visible:=true;
  AuSockets.ListResources(FDataRSRP^);

  tbActive.Checked:=true;
  {$ifdef Buffer_Debug}
  Core.Logging.Native.WriteLogEntry(
    SYSTEM_LOG,
    'TAurawin.UserAccountRetrieved',
    'Listing Resources'
  );
  {$endif}
end;

procedure TAurawin.CoreError(Var SR: TRSR; cObject, cCommand: Core.Strings.VarString; Code: Word);
begin
  if (
       (SameText(cObject,NS_CORE_OBJ_VDM) and SameText(cCommand,NS_CORE_CMD_RC_LIST)) or
       (SameText(cObject,NS_CORE_OBJ_VDM_ACCOUNT) and SameText(cCommand,NS_CORE_CMD_ACCT_READ))
    )
  then begin
    pcMain.ActivePage:=tsCredentials;
    lblSplash.Caption:=auLang.Table.Splash.AccessDenied;
    lblSplash.Font.Color:=clRed;
    lblSplash.Font.Style:=[fsBold];
    Application.ProcessMessages;
    txtPassword.SetFocus();
    btnConnect.Enabled:=true;
  end;
end;

procedure TAurawin.ResourcesListed(var Resources: Storage.VDM.Resources.TResources);
var
  iLcv:integer;
  liRc:TListItem;
  rcP:Storage.VDM.Resources.PResource;
  mrResult:Word;
begin
  frmResource.lvResources.Items.Clear;
  lvResources.Items.Clear;
  for iLcv:=0 to High(Resources) do begin
    rcP:=Resources[iLcv];
    liRc:=lvResources.Items.Add;
    liRC.Caption:=IntToStr(rcP^.ID);
    liRC.SubItems.Add(auLang.Table.Options.YesNo[rcP^.Flags or Storage.VDM.Resources.Flags.SaveSession=rcp^.Flags]);
    liRC.SubItems.Add(Storage.VDM.Resources.getDirection(rcP^,auLang.Table.Labels.Download,auLang.Table.Labels.Upload));
    liRC.SubItems.Add(rcP^.Name);
    liRC.SubItems.Add(rcP^.Description);

    liRc:=frmResource.lvResources.Items.Add;
    liRC.Caption:=IntToStr(rcP^.ID);
    liRC.SubItems.Add(rcP^.Name);
    liRC.SubItems.Add(rcP^.Description);
  end;
  rcP:=AuSockets.getResource(Settings.ResourceID);
  if (rcP=nil) then begin

    frmResource.SetModes([rmList,rmNew]);
    mrResult:=frmResource.ShowModal();
    if mrResult=mrOK then begin
      FSetAsDefaultResource:=true;
      AuSockets.CreateResource(
        FDataRSRP^,
        frmREsource.txtName.Text,
        frmResource.txtDescription.Text,
        frmResource.cbRememberCookies.Checked,
        frmResource.tgDownload.Checked,
        frmResource.tgUpload.Checked
      );
    end else if mrResult=mrSelect then begin
      Settings.ResourceID:=StrToIntDef(frmResource.lvResources.Selected.Caption,0);
      Settings.ResourceName:=frmResource.lvResources.Selected.SubItems[0];
      Settings.ResourceDescription:=frmResource.lvResources.Selected.SubItems[1];
      rcP:=AuSockets.getResource(Settings.ResourceID);
    end else
      Application.Terminate;
  end;
  if (rcP<>nil) then begin
    pnlResourceCaption.Caption:=auLang.Table.Splash.getResource(rcP^.Name,rcP^.Description);
    AuSockets.setResource(rcP^);
  end;
  AuSockets.ListNetworks(FDataRSRP^);
end;

procedure TAurawin.ResourceAdded(var Resource: Storage.VDM.Resources.TResource);
var
  liRC:TListItem;
begin

  liRC:=lvResources.Items.Add;
  liRC.Caption:=IntToStr(Resource.ID);
  liRC.SubItems.Add(auLang.Table.Options.YesNo[Resource.Flags or Storage.VDM.Resources.Flags.SaveSession=Resource.Flags]);
  liRC.SubItems.Add(Storage.VDM.Resources.getDirection(Resource,auLang.Table.Labels.Download,auLang.Table.Labels.Upload));
  liRC.SubItems.Add(Resource.Name);
  liRC.SubItems.Add(Resource.Description);

  liRc:=frmResource.lvResources.Items.Add;
  liRC.Caption:=IntToStr(Resource.ID);
  liRC.SubItems.Add(Resource.Name);
  liRC.SubItems.Add(Resource.Description);

  if FSetAsDefaultResource then begin
    FSetAsDefaultResource:=false;
    Settings.ResourceID:=Resource.ID;
    Settings.ResourceName:=Resource.Name;
    Settings.ResourceDescription:=Resource.Description;
    pnlResourceCaption.Caption:=auLang.Table.Splash.getResource(Resource.Name,Resource.Description);
    AuSockets.SetResource(Resource);
  end;
end;

procedure TAurawin.ResourceWritten(var Resource: Storage.VDM.Resources.TResource);
var
  liRC:TListItem;
begin
  if Resource.ID=Settings.ResourceID then
    pnlResourceCaption.Caption:=auLang.Table.Splash.getResource(Resource.Name,Resource.Description);
  liRC:=Core.Utils.ListView.getItemByCaption(lvResources,IntToStr(Resource.ID));
  liRC.SubItems[0]:=auLang.Table.Options.YesNo[Resource.Flags or Storage.VDM.Resources.Flags.SaveSession=Resource.Flags];
  liRC.SubItems[1]:=Storage.VDM.Resources.getDirection(Resource,auLang.Table.Labels.Download,auLang.Table.Labels.Upload);
  liRC.SubItems[2]:=Resource.Name;
  liRC.SubItems[3]:=Resource.Description;
end;


procedure TAurawin.ResourceDeleted(var Resource: Storage.VDM.Resources.TResource);
var
  liRC:TListItem;
begin
  liRC:=Core.Utils.ListView.getItemByCaption(lvResources,IntToStr(Resource.ID));
  if liRC<>nil then
    liRC.Delete;
end;

procedure TAurawin.SocketConnected(RSRP:PRSR);
begin
  if RSRP=FDataRSRP then begin
    btnConnect.Enabled:=false;
    sConnect.Brush.Color:=clGreen;
    AuSockets.ReadAccount(FDataRSRP^);
  end;
end;

procedure TAurawin.SocketFinalized(RSRP: PRSR);
begin
  if RSRP=FDataRSRP then begin
    FDataRSRP:=nil;
    sConnect.Brush.Color:=clRed;
    pnlNav.Enabled:=false;
    pcMain.ActivePage:=tsCredentials;
  end;
end;

procedure TAurawin.SocketError(RSRP: PRSR);
begin
  if RSRP=FDataRSRP then begin

    AllocateSocket();

  end;
end;

procedure TAurawin.AllocateSocket();
begin

  if FDataRSRP<>nil then begin
    AuSockets.Close(FDataRSRP);
    FDataRSRP:=nil;
  end;

  AuSockets.Reset();
  FSwitching:=true;
  pcMain.ActivePage:=tsCredentials;
  tsCredentials.Show();
  pnlNav.Enabled:=false;
  btnConnect.Enabled:=true;
  FSwitching:=false;

  FDataRSRP:=AuSockets.Allocate(rsrClient,rsrsTCP);
  FDataRSRP^.Finite:=false;
end;

procedure TAurawin.SocketDisconnected(RSRP: PRSR);
begin
  // figure out if we are to re-create socket
end;

procedure TAurawin.ResetButtons();
begin
  ResetButton(lblResources);
  ResetButton(lblSettings);
end;

procedure TAurawin.ResetButton(btnText:TLabel);
begin
  btnText.Font.Color:=clBtnText;
  btnText.Color:=clBtnFace;
end;

procedure TAurawin.SetButton(btnText:TLabel);
begin
  btnText.Font.Color:=clHighlightText;
  btnText.Color:=clHighlight;
end;

procedure TAurawin.TI_SelectRc(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
end;

procedure TAurawin.TI_Size(ItemP:Core.Timer.PItem);
var
  sLabel:Core.Strings.VarString;
  size : float;
  qSize:QWord;
begin
  qSize:=AuSockets.getSyncFileSize();
  if (qSize>=auLang.Table.Size.Exabyte) then begin
    sLabel:=auLang.Table.Labels.Exabytes;
    size:=qSize/auLang.Table.Size.Exabyte;
  end else if (qSize>=auLang.Table.Size.Petabyte) then begin
    sLabel:=auLang.Table.Labels.Petabytes;
    size:=qSize/auLang.Table.Size.Petabyte;
  end else if (qSize>=auLang.Table.Size.Terabyte) then begin
    sLabel:=auLang.Table.Labels.Terabytes;
    size:=qSize/auLang.Table.Size.Terabyte;
  end else if (qSize>=auLang.Table.Size.Gigabyte) then begin
    sLabel:=auLang.Table.Labels.GigaBytes;
    size:=qSize/auLang.Table.Size.GigaByte;
  end else if (qSize>=auLang.Table.Size.Megabyte) then begin
    sLabel:=auLang.Table.Labels.Megabytes;
    size:=qSize/auLang.Table.Size.Megabyte;
  end else if (qSize>=auLang.Table.Size.Kilobyte) then begin
    sLabel:=auLang.Table.Labels.Kilobytes;
    size:=qSize/auLang.Table.Size.Kilobyte;
  end else begin
    sLabel:=auLang.Table.Labels.Bytes;
    size:=qSize;
  end;
  lbSize.Caption:=Format(AuLang.Table.Format.SizeCalculation,[size,sLabel]);
  ItemP^.Expires:=0;
end;

procedure TAurawin.SetSyncEnginePipes(var Item:Storage.Social.Sync.TSync);
var
  iPipesLen    : LongInt;
  iPipesLcv    : LongInt;

  procedure PushProcessSocialSync(PipeP:Storage.Social.Sync.Pipes.PItem);
  var
    bSync        : boolean;
  begin
    bSync:=(
      ( (PipeP^.Direction or Storage.Social.Sync.Pipes.Direction.Upload) = PipeP^.Direction) or
      ( (PipeP^.Direction or Storage.Social.Sync.Pipes.Direction.Download) = PipeP^.Direction)
    );


    if (
         (Item.statePipes=rqsReceived) and
         (AuSockets.Mode in [EngineMode.emWorking,EngineMode.emNone]) and
         DirectoryExists(PipeP^.Path)
       )
    then begin
      if (
        (PipeP^.Direction or Storage.Social.Sync.Pipes.Direction.Upload = PipeP^.Direction) and
        (PipeP^.scannerUpload=nil)
      ) then
        PipeP^.scannerUpload:=TScannerUploadThread.Create(
          AuSockets,
          PipeP,
          @Item,
          FDataRSRP
        );
    end;
    if (
         (Item.statePipes=rqsReceived) and
         (AuSockets.Mode in [EngineMode.emWorking,EngineMode.emNone]) and
         DirectoryExists(PipeP^.Path)
       )
    then begin
      if (
        (PipeP^.Direction or Storage.Social.Sync.Pipes.Direction.Download = PipeP^.Direction) and
        (PipeP^.scannerDownload=nil)
      ) then
        PipeP^.scannerDownload:=TScannerDownloadThread.Create(
          AuSockets,
          PipeP,
          @Item,
          FDataRSRP
        );
    end;

  end;
begin
  FDataRSRP^.Throttle.Consumption:=FDataRSRP^.SendBuffer.posWrite+FDataRSRP^.RecvBuffer.posWrite;
  for iPipesLcv:=0 to High(Item.Pipes) do begin
    PushProcessSocialSync(Item.Pipes[iPipesLcv]);
  end;
end;

procedure TAurawin.TI_ProcessSyncItems(ItemP:Core.Timer.PItem);
begin
  if (AuSockets.Status=esStart) then begin
    AuSockets.AddMethod(cmProcessSyncItems.Create(AuSockets,FDataRSRP));
    ItemP^.Expires:=IncMillisecond(Core.Timer.dtUT,auSettings.SYNC_PROCESS_ITEMS_DELAY);
  end else
    ItemP^.Expires:=0;
end;

procedure TAurawin.TI_SocialAutoSave(ItemP:Core.Timer.PItem);
var
  rcP:Storage.VDM.Resources.PResource;
  listP:Storage.Social.Sync.PHeaderList;
  hdrP:Storage.Social.Sync.PHeader;
  itmP:Storage.Social.Sync.PSync;
  iLcv:integer;
begin
  ItemP^.Expires:=0;
  rcP:=AuSockets.getResource(Settings.ResourceID);
  listP:=AuSockets.getSyncList();
  if ( AuSockets.Connected and AuSockets.Authenticated and (rcP<>nil) and (listP<>nil) ) then begin
    for iLcv:=0 to High(listP^) do begin
      hdrP:=listP^[iLcv];
      if (hdrP<>nil) then begin
        itmP:=hdrP^.ItemP;
        if (itmP^.Modified=true) then begin
          AuSockets.WriteSyncSocial(FDataRSRP^,itmP^);
          itmP^.Modified:=false;
        end;
      end;
    end;
  end;
end;

procedure TAurawin.FormCreate(Sender: TObject);
begin
  Caption:=App.Build.Caption;

  lblVersion.Caption:=Concat(App.Build.Title,' built on ',App.Build.Version,' for ',App.Build.Edition,' (',IntToStr(RSR.RSR_Build_Number),')');
  FManifestFactor:=TMemoryStream.Create();

  FNameWidth:=50;
  FPathWidth:=100;
  FButtonWidth:=50;
  FDirectionWidth:=100;
  FModeWidth:=100;


  tbActive.Caption:=uEngine.Engine_Status[AuSockets.Status]^;
  lblStatus.Caption:=auLang.Table.Labels.Status;
  lblSettings.Caption:=auLang.Table.Labels.Settings;
  lblResources.Caption:=auLang.Table.Labels.Resources;
  btnRefreshNetworks.Caption:=auLang.Table.Symbol.Reload;
  btnRefreshConnections.Caption:=auLang.Table.Symbol.Reload;

  FBtnLabel:=nil;

  FXMLParser:=TDOMParser.Create();


  FSetAsDefaultResource:=false;
  FSwitching:=false;
  FLoading:=true;


  AuSockets.OnSyncItemQueued:=@SyncItemQueued;
  AuSockets.OnSyncItemRemoved:=@SyncItemRemoved;

  AuSockets.OnSocSyncRetrieved:=@SocialSyncRetrieved;
  AuSockets.OnSocSyncModified:=@SocialSyncModified;
  AuSockets.OnNetworksListed:=@NetworksRetrieved;
  AuSockets.OnConnectionsListed:=@ConnectionsRetrieved;
  AuSockets.OnConnected:=@SocketConnected;
  AuSockets.OnDisconnected:=@SocketDisconnected;
  AuSockets.OnFinalized:=@SocketFinalized;
  AuSockets.OnErrored:=@SocketError;
  AuSockets.OnResourcesListed:=@ResourcesListed;
  AuSockets.OnResourceAdded:=@ResourceAdded;
  AuSockets.OnResourceWritten:=@ResourceWritten;
  AuSockets.OnResourceDeleted:=@ResourceDeleted;
  AuSockets.OnAccountRetrieved:=@UserAccountRetrieved;
  AuSockets.OnCoreError:=@CoreError;

  tiSelectRc.Mode:=temSynchronize;
  tiSelectRc.Expires:=0;
  tiSelectRc.Event:=@TI_SelectRc;
  tiSelectRc.Location:='frmMain.TAurawin.TI_SelectRc';
  Core.Timer.Background.RegisterEvent(tiSelectRc,Core.Timer.LoadNoUpdate);


  tiSocialAutoSave.Mode:=temNormal;
  tiSocialAutoSave.Expires:=0;
  tiSocialAutoSave.Event:=@TI_SocialAutoSave;
  tiSocialAutoSave.Location:='frmMain.TAurawin.TI_SocialAutoSave';
  Core.Timer.Background.RegisterEvent(tiSocialAutoSave,Core.Timer.LoadNoUpdate);

  tiSize.Mode:=temSynchronize;
  tiSize.Expires:=0;
  tiSize.Event:=@TI_Size;
  tiSize.Location:='frmMain.TAurawin.TI_Size';
  Core.Timer.Background.RegisterEvent(tiSize,Core.Timer.LoadNoUpdate);

  tiProcessSyncItems.Mode:=temNormal;
  tiProcessSyncItems.Expires:=0;
  tiProcessSyncItems.Event:=@TI_ProcessSyncItems;
  tiProcessSyncItems.Location:='frmMain.TAurawin.TI_ProcessSyncItems';
  Core.Timer.Background.RegisterEvent(tiProcessSyncItems,Core.Timer.LoadNoUpdate);

  Load();

  pcMain.ShowTabs:=false;
  pcMain.ActivePage:=tsCredentials;

  lblSplash.Caption:=auLang.Table.Splash.ProvideCredentials;
  lblName.Caption:=auLang.Table.Labels.Account;
  lblPassword.Caption:=auLang.Table.Labels.Password;
  cbRememberCookies.Caption:=auLang.Table.Options.SaveLoginInfo;


  hdrMyNetwork.Sections[0].Text:=auLang.Table.Labels.Name;
  hdrOtherNetwork.Sections[0].Text:=auLang.Table.Labels.Name;


  hdrMyNetwork.Sections[1].Text:=auLang.Table.Labels.Path;
  hdrOtherNetwork.Sections[1].Text:=auLang.Table.Labels.Path;


  hdrMyNetwork.Sections[3].Text:=auLang.Table.Labels.Direction;
  hdrOtherNetwork.Sections[3].Text:=auLang.Table.Labels.Direction;



  pnlCredName.Height:=App.Consts.EntryHeight;
  pnlCredPassword.Height:=App.Consts.EntryHeight;

  pnlCredDomain.Height:=App.Consts.EntryHeight;
  pnlCredOptions.Height:=App.Consts.EntryHeight;
  pnlCredBtns.Height:=App.Consts.ButtonsHeight;
  pnlResource.Height:=App.Consts.EntryHeight;


  hdrMyNetwork.Height:=App.Consts.HeaderHeight;
  hdrOtherNetwork.Height:=App.Consts.HeaderHeight;
  pnlStatus.Height:=App.Consts.StatusHeight;


  FLoading:=False;

  AllocateSocket();

  tiSize.Expires:=IncMillisecond(Core.Timer.dtUT,auSettings.SYNC_CALCULATE_SIZE);

  pnlStatus.Align:=alBottom;
end;

procedure TAurawin.FormDestroy(Sender: TObject);
begin
  FDataRSRP:=nil;

  Application.Terminate();

  FreeAndNil(FManifestFactor);
  FreeAndNil(FXMLParser);
end;

procedure TAurawin.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Core.Timer.Background.UnloadEvent(tiProcessSyncItems,Core.Timer.UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(tiSelectRc,Core.Timer.UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(tiSocialAutoSave,Core.Timer.UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(tiSize,Core.Timer.UnloadNoExecute);

  AuSockets.Terminate();
  AuSockets:=nil;
  Application.Terminate;
end;

procedure TAurawin.btnConnectClick(Sender: TObject);
begin
  AllocateSocket();

  if (AuSockets.Connected=false) then begin
    AuSockets.Connect(FDataRSRP,Settings.Domain,80,CONNECT_RESOLVE);
  end else begin
    AuSockets.ListResources(FDataRSRP^);
  end;
end;

procedure TAurawin.cbOtherNetworksChange(Sender: TObject);
var
  idx:integer;
  iLcv:integer;
  netP:Storage.Social.Network.PNetwork;
  sncP:Storage.Social.Sync.PSync;
  hdrP:Storage.Social.Sync.PHeader;
  pipeP:Storage.Social.Sync.Pipes.PItem;

begin
  scOtherPaths.DisableAlign();
  Try
    ClearOtherNetworkEntries();
    idx:=cbOtherNetworks.ItemIndex;
    if idx<>-1 then begin
      netP:=AuSockets.OtherNetworks[idx];
      hdrP:=AuSockets.getSync(netP^);
      if hdrP<>nil then begin
        sncP:=hdrP^.ItemP;
        for iLcv:=0 to High(sncP^.Pipes) do begin
          pipeP:=sncP^.Pipes[iLcv];
          TPathEntry.CreateAsOtherNetwork(pipeP);
        end;
      end;
    end;
    tsConnectionsResize(Sender);
  finally
    scOtherPaths.EnableAlign();
  end;
end;

procedure TAurawin.cbMyNetworksChange(Sender: TObject);
var
  idx:integer;
  iLcv:integer;
  netP:Storage.Social.Network.PNetwork;
  sncP:Storage.Social.Sync.PSync;
  hdrP:Storage.Social.Sync.PHeader;
  pipeP:Storage.Social.Sync.Pipes.PItem;

begin
  scMyPaths.DisableAlign();
  Try
    ClearMyNetworkEntries();
    idx:=cbMyNetworks.ItemIndex;
    if idx<>-1 then begin
      NetP:=AuSockets.Networks[idx];
      hdrP:=AuSockets.getSync(netP^);
      if (hdrP<>nil) then begin
        sncP:=hdrP^.ItemP;
        for iLcv:=0 to High(sncP^.Pipes) do begin
          pipeP:=sncP^.Pipes[iLcv];
          TPathEntry.CreateAsMyNetwork(pipeP);
        end;
      end;
    end;
    tsMyNetworksResize(Sender);
  finally
    scMyPaths.EnableAlign();
  end;
end;

procedure TAurawin.cbRememberCookiesChange(Sender: TObject);
begin
  If FLoading then exit;
  Settings.SaveLogin:=cbRememberCookies.Checked;
end;

procedure TAurawin.btnResourceNewClick(Sender: TObject);
begin
  frmResource.SetModes([rmNew]);
  if frmResource.ShowModal()=mrOK then
    AuSockets.CreateResource(
      FDataRSRP^,
      frmResource.txtName.Text,
      frmResource.txtDescription.Text,
      frmResource.cbRememberCookies.Checked,
      frmResource.tgUpload.Checked,
      frmResource.tgDownload.Checked
    );
end;

procedure TAurawin.btnRefreshNetworksClick(Sender: TObject);
begin
  AuSockets.ListNetworks(FDataRSRP^);
end;

procedure TAurawin.btnResourceDeleteClick(Sender: TObject);
var
  iID:int64;
begin
  if (lvResources.Selected<>nil) and (MessageDlg(
    auLang.Table.Messages.Delete.Resource.Caption,
    auLang.Table.Messages.Delete.Resource.getMessage(lvResources.Selected.SubItems[2]),
    mtConfirmation,
    [mbYes, mbNo],
    0
  )=mrYes) then begin
    iID:=StrToIntDef(lvResources.Selected.Caption,0);
    if iID<>0 then
      AuSockets.DeleteResource(FDataRSRP^,iID);
  end;
end;

procedure TAurawin.imgStatsClick(Sender: TObject);
begin
  if FLoading then exit;
  pcMain.ActivePage:=tsStatus;
  NavDisplayChange(tglbHome.Checked);
end;

procedure TAurawin.imgSettingsClick(Sender: TObject);
begin
  if FLoading then exit;
  pcMain.ActivePage:=tsSettings;
  NavDisplayChange(tglbHome.Checked);
end;

procedure TAurawin.imgResourcesClick(Sender: TObject);
begin
  if FLoading then exit;
  pcMain.ActivePage:=tsResources;
  NavDisplayChange(tglbHome.Checked);
end;

procedure TAurawin.lbStatusClick(Sender: TObject);
const
  Switcher:integer=0;
begin
  if (Switcher=0) then begin
    lbStatus.Caption:=Concat('Engine: ',AuSockets.EntryPoint);
    Switcher:=1;
  end else begin
    lbStatus.Caption:=Concat('Command: ',AuSockets.CommandThread.EntryPoint);
    Switcher:=0;
  end;
end;

procedure TAurawin.lvResourcesDblClick(Sender: TObject);
begin
  miResourceEditClick(Sender);
end;

procedure TAurawin.lvResourcesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  tiSelectRc.Expires:=IncMillisecond(Now,400);
end;

procedure TAurawin.miExitClick(Sender: TObject);
begin
  Application.Terminate();
end;

procedure TAurawin.miResourceEditClick(Sender: TObject);
var
  iID:Int64;
  itmP:Storage.VDM.Resources.PResource;
begin
  iID:=StrToIntDef(lvResources.Selected.Caption,0);
  itmP:=auSockets.getResource(iID);
  if (itmP<>nil) then begin
    frmResource.SetModes([rmEdit]);
    frmResource.txtEditName.Text:=itmP^.Name;
    frmResource.txtEditDescription.Text:=itmP^.Description;
    frmResource.cbEditRememberCookies.Checked:=itmP^.Flags or Storage.VDM.Resources.Flags.SaveSession = itmP^.Flags;
    frmResource.tgEditDownload.Checked:=itmP^.Flags or Storage.VDM.Resources.Flags.SyncDownload = itmP^.Flags;
    frmResource.tgEditUpload.Checked:=itmP^.Flags or Storage.VDM.Resources.Flags.SyncUpload=itmP^.Flags;
    if frmResource.ShowModal()=mrOK then begin
      itmP^.Flags:=0;
      itmP^.Name:=frmResource.txtEditName.Text;
      itmP^.Description:=frmResource.txtEditDescription.Text;
      if frmResource.tgEditDownload.Checked then
        itmP^.Flags:=itmP^.Flags or Storage.VDM.Resources.Flags.SyncDownload;
      if frmResource.tgEditUpload.Checked then
        itmP^.Flags:=itmP^.Flags or Storage.VDM.Resources.Flags.SyncUpload;
      if frmResource.cbEditRememberCookies.Checked then
        itmP^.Flags:=itmP^.Flags or Storage.VDM.Resources.Flags.SaveSession;
      AuSockets.WriteResource(FDataRSRP^,itmP^);
    end;
  end;
end;

procedure TAurawin.miResourcesCredsOffClick(Sender: TObject);
var
  iID:Int64;
  itmP:Storage.VDM.Resources.PResource;
begin
  if FLoading then exit;
  if lvResources.Selected<>nil then begin
    iID:=StrToIntDef(lvResources.Selected.Caption,0);
    itmP:=auSockets.getResource(iID);
    if (itmP<>nil) then begin
      itmP^.Flags:=itmP^.Flags and not Storage.VDM.Resources.Flags.SaveSession;
      AuSockets.WriteResource(FDataRSRP^,itmP^);
    end;
  end;
end;

procedure TAurawin.miResourcesCredsOnClick(Sender: TObject);
var
  iID:Int64;
  itmP:Storage.VDM.Resources.PResource;
begin
  if FLoading then exit;
  if lvResources.Selected<>nil then begin
    iID:=StrToIntDef(lvResources.Selected.Caption,0);
    itmP:=auSockets.getResource(iID);
    if (itmP<>nil) then begin
      itmP^.Flags:=itmP^.Flags or Storage.VDM.Resources.Flags.SaveSession;
      AuSockets.WriteResource(FDataRSRP^,itmP^);
    end;
  end;
end;

procedure TAurawin.pcMainChange(Sender: TObject);
begin
  if FBtnLabel<>nil then
    ResetButton(FBtnLabel);
  FBtnLabel:=nil;
  if (pcMain.ActivePage=tsResources) then
    FBtnLabel:=lblResources
  else if (pcMain.ActivePage=tsSettings) then
    FBtnLabel:=lblSettings
  else if (pcMain.ActivePage=tsStatus) then
    FBtnLabel:=lblStatus;
  if FBtnLabel<>nil then
    SetButton(FBtnLabel);
end;

procedure TAurawin.pcMainChanging(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange:=FSwitching or ( (FLoading=false) and AuSockets.Authenticated and AuSockets.Connected);
end;

procedure TAurawin.puResourcesPopup(Sender: TObject);
var
  rcP:Storage.VDM.Resources.PResource;
  iID:int64;
begin
  FLoading:=true;
  if lvResources.Selected<>nil then begin
    iID:=StrToIntDef(lvResources.Selected.Caption,0);
    rcP:=AuSockets.getResource(iID);
    if (rcP^.Flags or Storage.VDM.Resources.Flags.SaveSession=rcP^.Flags) then
      miResourcesCredsOn.Checked:=true
    else
      miResourcesCredsOff.Checked:=true;
  end;
  FLoading:=false;
end;

procedure TAurawin.tiSystemClick(Sender: TObject);
begin
  Application.Restore();
  Self.WindowState:=wsNormal;
end;

procedure TAurawin.tbActiveChange(Sender: TObject);
begin
  if FLoading then exit;
  if (tbActive.Checked=true) then begin
    AuSockets.Status:=esStart;
    tbActive.Caption:=uEngine.Engine_Status[esStart]^;
    tiProcessSyncItems.Expires:=IncMillisecond(Core.Timer.dtUT,auSettings.SYNC_PROCESS_ITEMS_DELAY);
  end else begin
    AuSockets.Status:=esPause;
    tbActive.Caption:=uEngine.Engine_Status[esPause]^;
    tiProcessSyncItems.Expires:=0;
  end;
end;

procedure TAurawin.tglbHomeChange(Sender: TObject);
begin
  NavDisplayChange(tglbHome.Checked);
end;

procedure TAurawin.tsConnectionsResize(Sender: TObject);
begin
  hdrOtherNetwork.Sections[0].Width:=FNameWidth;
  hdrOtherNetwork.Sections[1].Width:=FPathWidth;
  hdrOtherNetwork.Sections[2].Width:=FButtonWidth;
  hdrOtherNetwork.Sections[3].Width:=FDirectionWidth;
end;

procedure TAurawin.tsMyNetworksResize(Sender: TObject);
begin
  hdrMyNetwork.Sections[0].Width:=FNameWidth;
  hdrMyNetwork.Sections[1].Width:=FPathWidth;
  hdrMyNetwork.Sections[2].Width:=FButtonWidth;
  hdrMyNetwork.Sections[3].Width:=FDirectionWidth;
end;

procedure TAurawin.tsStatusResize(Sender: TObject);
begin
   sgQueue.Columns[0].Width:= sgQueue.ClientWidth - ( ((App.Consts.StatusHeaderWidth*4) + StatusHeaderSizeBias) );
   sgQueue.Columns[1].Width:=App.Consts.StatusHeaderWidth+App.Consts.StatusHeaderSizeBias;
   sgQueue.Columns[2].Width:=App.Consts.StatusHeaderWidth;
   sgQueue.Columns[3].Width:=App.Consts.StatusHeaderWidth;
   sgQueue.Columns[4].Width:=App.Consts.StatusHeaderWidth;

end;

procedure TAurawin.txtDomainEditingDone(Sender: TObject);
begin
  if FLoading then exit;
  Settings.Domain:=txtDomain.Text;
  ResetLoginMessage();
end;

procedure TAurawin.ResetLoginMessage();
begin
  lblSplash.Caption:=auLang.Table.Splash.ProvideCredentials;
  lblSplash.Font.Color:=clBtnText;
  lblSplash.Font.Style:=[];
end;

procedure TAurawin.txtNameEditingDone(Sender: TObject);
begin
  if FLoading then exit;
  Settings.Username:=txtName.Text;
  ResetLoginMessage();
end;

procedure TAurawin.txtPasswordEditingDone(Sender: TObject);
begin
  if FLoading then exit;
  Settings.Password:=txtPassword.Text;
  ResetLoginMessage();
end;

initialization
  AuSockets:=TAuSocketMan.Create();
end.


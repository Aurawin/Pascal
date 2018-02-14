unit frmFileMan;

interface

uses
  Classes,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  PairSplitter,
  ComCtrls,
  Menus,
  ExtCtrls,
  StdCtrls,
  ActnList,
  Buttons,
  ExtDlgs,
  Spin,


  App.Consts,

  RSR,
  RSR.HTTP,

  Core.Timer,
  Core.Strings,

  Core.Database,
  Core.Database.Types,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.Boolean,
  Core.Utils.Time,
  Core.Utils.Forms,
  Core.Utils.Files,
  Core.Utils.ListView,
  Core.Utils.TreeView,

  Storage,
  Storage.Main,
  Storage.AuraDisks,
  Storage.ContentTypes,
  Storage.FAT,
  Storage.Domains,
  Storage.MatrixServices,
  Storage.RTSP,
  Storage.UserAccounts,
  Storage.KeepAlive,


  SysUtils,
  FileUtil;


const ComboHeight=(
      {$ifdef Unix}
         {$ifdef Darwin}
            34
         {$else}
            38
         {$endif}
      {$else}
        34
      {$endif}
   );
type

  { TFileManager }

  TFileManager = class(TForm)
    alCompressOff: TAction;
    alCompressOn: TAction;
    alCompressToggle: TAction;
    aFileNewStream: TAction;
    alCacheOff: TAction;
    alCacheOn: TAction;
    alCacheToggle: TAction;
    alEditRename: TAction;
    alEditRenameFile: TAction;
    alKeywordsToggle: TAction;
    alKeywordsOff: TAction;
    alKeywordsOn: TAction;
    alEditRenameFolder: TAction;
    alEditDelete: TAction;
    alEditPaste: TAction;
    alEditCopy: TAction;
    alEditCut: TAction;
    alViewRefresh: TAction;
    alFileExport: TAction;
    alFileImport: TAction;
    btnContent: TBitBtn;
    btnFileRename: TBitBtn;
    btnFileRenameCancel: TBitBtn;
    btnFind: TBitBtn;
    btnStreamRename: TBitBtn;
    gbFileNameEditor: TGroupBox;
    gbManagerResults: TGroupBox;
    gbStreamEditor: TGroupBox;
    GroupBox1: TGroupBox;
    Keywords: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbPath: TLabel;
    lbFiles: TLabel;
    lbDomain: TLabel;
    lbStatus: TLabel;
    lvManagerSearchResults: TListView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    miFileNewStream: TMenuItem;
    miSep1: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    miNewStream: TMenuItem;
    miSep: TMenuItem;
    miDomainFileCache: TMenuItem;
    miDomainFileCacheOn: TMenuItem;
    miDomainFileCacheOff: TMenuItem;
    miWindow: TMenuItem;
    mipuFolderRename: TMenuItem;
    mipuFileNewFolder: TMenuItem;
    mipuFileNewFile: TMenuItem;
    miDomainFileKeywordsOff: TMenuItem;
    miDomainFileKeywordsOn: TMenuItem;
    mipuFolderNewFile: TMenuItem;
    mipuFolderNewFolder: TMenuItem;
    miEditSeparator2: TMenuItem;
    miEditRename: TMenuItem;
    mipuDelete: TMenuItem;
    mipuSeparator2: TMenuItem;
    mipuSeparator1: TMenuItem;
    mipuExport: TMenuItem;
    mipuImport: TMenuItem;
    mipuFolderRefresh: TMenuItem;
    miViewRefresh: TMenuItem;
    miView: TMenuItem;
    mipuNew: TMenuItem;
    miFileSeperator4: TMenuItem;
    miFileImport: TMenuItem;
    miFileExport: TMenuItem;
    alFileRestore: TAction;
    alFileBackup: TAction;
    alFileDelete: TAction;
    alFileNewFile: TAction;
    alFileNewFolder: TAction;
    alMain: TActionList;
    cbPath: TComboBox;
    lvFiles: TListView;
    MainMenu: TMainMenu;
    miNewFolder: TMenuItem;
    miNewFile: TMenuItem;
    miFileSeperator3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    miFileSeperator: TMenuItem;
    miFileDelete: TMenuItem;
    miFileSeperator1: TMenuItem;
    miFileNewFolder: TMenuItem;
    miFileNew: TMenuItem;
    miFileNewFile: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miOpen: TMenuItem;
    miRestore: TMenuItem;
    miBackup: TMenuItem;
    miFileExit: TMenuItem;
    miFile: TMenuItem;
    MI_DOMAIN_FILE_DELETE: TMenuItem;
    mipuFilesNew: TMenuItem;
    MI_DOMAIN_FILE_RENAME: TMenuItem;
    odFile: TOpenDialog;
    odFolder: TSelectDirectoryDialog;
    odStream: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    gbFind: TGroupBox;
    pnlDomain: TPanel;
    pnlProgress: TPanel;
    pbProgress: TProgressBar;
    puFolders: TPopupMenu;
    puFiles: TPopupMenu;
    pnlFolders: TPanel;
    pnlPath: TPanel;
    puNew: TPopupMenu;
    pnlFiles: TPanel;
    pnlStatus: TPanel;
    sdFile: TSaveDialog;
    seCacheTTL: TSpinEdit;
    StatusBar: TPanel;
    tbbCache: TToolButton;
    tbbCache1: TToolButton;
    tbPath: TToolBar;
    tbMain: TToolBar;
    tbbNew: TToolButton;
    tbbOpen: TToolButton;
    tbbDelete: TToolButton;
    tbs1: TToolButton;
    ToolButton1: TToolButton;
    tbbRefresh: TToolButton;
    tbbKeywords: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tvFolders: TTreeView;
    txtFileName: TEdit;
    txtFilesFind: TEdit;
    txtStreamName: TEdit;
    procedure alCacheOffExecute(Sender: TObject);
    procedure alCacheOnExecute(Sender: TObject);
    procedure alCacheToggleExecute(Sender: TObject);
    procedure alCompressOffExecute(Sender: TObject);
    procedure alCompressOnExecute(Sender: TObject);
    procedure alCompressToggleExecute(Sender: TObject);
    procedure alEditRenameExecute(Sender: TObject);
    procedure alEditRenameFileExecute(Sender: TObject);
    procedure alFileBackupExecute(Sender: TObject);
    procedure alFileDeleteExecute(Sender: TObject);
    procedure alFileExportExecute(Sender: TObject);
    procedure alFileImportExecute(Sender: TObject);
    procedure alFileNewFileExecute(Sender: TObject);
    procedure alFileNewFolderExecute(Sender: TObject);
    procedure alKeywordsOffExecute(Sender: TObject);
    procedure alKeywordsOnExecute(Sender: TObject);
    procedure alKeywordsToggleExecute(Sender: TObject);
    procedure alViewRefreshExecute(Sender: TObject);
    procedure btnContentClick(Sender: TObject);
    procedure btnFileRenameCancelClick(Sender: TObject);
    procedure btnFileRenameClick(Sender: TObject);
    procedure btnFindClick(Sender: TObject);
    procedure btnStreamRenameClick(Sender: TObject);
    procedure cbPathSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvFilesResize(Sender: TObject);
    procedure alFileRestoreExecute(Sender: TObject);
    procedure lvFilesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure miFileExitClick(Sender: TObject);
    procedure miFileImportClick(Sender: TObject);
    procedure miOpenClick(Sender: TObject);
    procedure seCacheTTLChange(Sender: TObject);
    procedure tvFoldersEdited(Sender: TObject; Node: TTreeNode; var S: Core.Strings.VarString);
    procedure tvFoldersEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure tvFoldersSelectionChanged(Sender: TObject);
    procedure txtFileNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure txtFilesFindKeyPress(Sender: TObject; var Key: char);
    procedure txtFileNameKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    FChecking   : Boolean;
    FFat        : TDSFAT;
    FDomainP    : Storage.Domains.Items.PDomain;
    FRootP      : Storage.UserAccounts.Items.PItem;
    FPath       : Core.Strings.VarString;
    FFolder     : TDSFolder;
    FFileEdit   : TDSFile;
    FStreamEdit : TDSFile;
    FRefactor   : TMemoryStream;
    FInfoP      : PFormInfo;
    FTask       : Core.Database.Types.TTask;
    FLList      : TList;  // for lock lists
    tiFileSelect: Core.Timer.Item;
    tiFolderSelect: Core.Timer.Item;
  private
    procedure OnFileSelect(ItemP:Core.Timer.PItem);
    procedure OnFolderSelect(ItemP:Core.Timer.PItem);
  private
    procedure SetFolder(Folder:TDSFolder);
    procedure OnStatusUpdate(pgPosition,pgMax:QWORD; Status:Core.Strings.VarString);
    procedure OnDBException(sModule,sLocation,sTable,sTask,sMessage:Core.Strings.VarString);
  public
    procedure LoadDomain;
    procedure Clear;
    procedure Show(DomainP:Storage.Domains.Items.PDomain; RootP:Storage.UserAccounts.Items.PItem); overload;
    procedure Backup(sDomain:Core.Strings.VarString);
  end;

implementation
uses
  DateUtils,frmEdit,frmImage,
  Form.DBException,

  Math;

const
  FLoading:boolean=False;

  IDX_FILE_CACHE     = 0;
  IDX_FILE_COMPRESS  = 1;
  IDX_FILE_KEYWORDS  = 2;
  IDX_FILE_SIZE      = 3;
  IDX_FILE_TTL       = 4;
  IDX_FILE_MODIFIED  = 5;
  IDX_FILE_CREATED   = 6;

{ TFileManager }

procedure TFileManager.OnDBException(sModule,sLocation,sTable,sTask,sMessage:Core.Strings.VarString);
begin
  Form.DBException.ShowException(sModule,sLocation,sTable,sTask,sMessage);
end;

procedure TFileManager.OnStatusUpdate(pgPosition,pgMax:QWORD; Status:Core.Strings.VarString);
begin
  pbProgress.Max:=pgMax;
  pbProgress.Position:=pgPosition;
  lbStatus.Caption:=Status;
  Application.ProcessMessages;
end;

procedure TFileManager.FormCreate(Sender: TObject);
begin
  FTask:=Core.Database.Types.TTask.Create(Storage.Main.Header,'FileManager');

  tiFileSelect.Event:=@OnFileSelect;
  tiFileSelect.Location:='frmFileMan.OnFileSelect';
  tiFileSelect.Expires:=0;
  tiFileSelect.Mode:=temSynchronize;
  Core.Timer.Background.RegisterEvent(tiFileSelect,LoadNoUpdate);

  tiFolderSelect.Event:=@OnFolderSelect;
  tiFolderSelect.Location:='frmFileMan.OnFolderSelect';
  tiFolderSelect.Expires:=0;
  tiFolderSelect.Mode:=temSynchronize;
  Core.Timer.Background.RegisterEvent(tiFolderSelect,LoadNoUpdate);

  FFileEdit:=nil;
  FRefactor:=TMemoryStream.Create;
  FFat:=TDSFAT.Create();
  FFat.OnStatus:=@OnStatusUpdate;

  tbPath.Height:=ComboHeight;
end;

procedure TFileManager.FormDestroy(Sender: TObject);
begin
  Core.Timer.Background.UnloadEvent(tiFileSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(tiFolderSelect,UnloadNoExecute);

  FreeAndNil(FTask);
  FreeAndNil(FFat);
  FreeAndNil(FRefactor);
end;

procedure TFileManager.alFileBackupExecute(Sender: TObject);
begin
  if sdFile.Execute then
    FFat.Backup(sdFile.FileName);
end;

procedure TFileManager.alEditRenameFileExecute(Sender: TObject);
var
  dsFile:TDSFile;
begin
  if lvFiles.Selected<>nil then begin
    dsFile:=TDSFile(lvFiles.Selected.Data);
    If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
      FFileEdit:=dsFile;
      txtFileName.Text:=FFileEdit.Name;
      gbFileNameEditor.Visible:=True;
      txtFileName.SetFocus;
    end;
  end;
end;

procedure TFileManager.alEditRenameExecute(Sender: TObject);
begin
  if tvFolders.Focused then
    alEditRenameFolder.Execute
  else
    alEditRenameFile.Execute;
end;

procedure TFileManager.alCacheToggleExecute(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if (lvFiles.Selected<>nil) and (FChecking=false) then begin
    FChecking:=true;
    alCacheToggle.Checked:=not alCacheToggle.Checked;
    FChecking:=false;
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(li.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Cache:=alCacheToggle.Checked;
          dsFile.Save(FTask,dsFileSaveCache);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_CACHE]:=YES_NO[dsFile.Cache];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alCompressOffExecute(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if lvFiles.Selected<>nil then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Cache:=false;
          dsFile.Save(FTask,dsFileSaveDeflate);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_COMPRESS]:=YES_NO[dsFile.Deflate];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alCompressOnExecute(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if lvFiles.Selected<>nil then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Deflate:=true;
          dsFile.Save(FTask,dsFileSaveDeflate);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_COMPRESS]:=YES_NO[dsFile.Deflate];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alCompressToggleExecute(Sender: TObject);
var
  dsFile:TDSFile;
  li:TListItem;
  iLcv:integer;
begin
  if (lvFiles.Selected<>nil) and (FChecking=false) then begin
    FChecking:=true;
    alCompressToggle.Checked:=not alCompressToggle.Checked;
    FChecking:=false;
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      dsFile:=TDSFile(li.Data);
      if li.Selected then begin
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Deflate:=alCompressToggle.Checked;
          dsFile.Save(FTask,dsFileSaveDeflate);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_COMPRESS]:=YES_NO[dsFile.Deflate];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alCacheOnExecute(Sender: TObject);
var
  dsFile:TDSFile;
  li:TListItem;
  iLcv:integer;
begin
  if lvFiles.Selected<>nil then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Cache:=true;
          dsFile.Save(FTask,dsFileSaveCache);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_CACHE]:=YES_NO[dsFile.Cache];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alCacheOffExecute(Sender: TObject);
var
  dsFile:TDSFile;
  li:TListItem;
  iLcv:integer;
begin
  if lvFiles.Selected<>nil then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Cache:=false;
          dsFile.Save(FTask,dsFileSaveCache);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_CACHE]:=YES_NO[dsFile.Cache];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alFileDeleteExecute(Sender: TObject);
var
  dsFile:TDSFile;
  dsFolder:TDSFolder;
  Node:TTreeNode;
begin
  if (lvFiles.Selected<>nil) then begin
    if lvFiles.Focused then begin
      if not gbFileNameEditor.visible then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.Delete;
          if gbFileNameEditor.Visible then begin
            txtFileName.Clear;
            gbFileNameEditor.Visible:=false;
          end;
          lvFiles.Selected.Delete;
        end;
      end;
    end;
  end else begin
    Node:=tvFolders.Selected;
    if Node<>nil then begin
      dsFolder:=TDSFolder(Node.Data);
      if (dsFolder<>nil) and ((dsFolder.Attributes or FS_ATTR_SYSTEM)<>dsFolder.Attributes) then begin
        if Dialogs.MessageDlg(FMT_CONFORM_FOLDER_DELETE_TITLE,Format(FMT_CONFIRM_FOLDER_DELETE_MSG,[Node.Text]),mtConfirmation,[mbYes,mbNo],0)=mrYes then begin
          dsFolder.Delete;
          Node.Delete;
          lvFiles.BeginUpdate;
          Try
            lvFiles.Items.Clear;
          finally
            lvFiles.EndUpdate;
          end;
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alFileExportExecute(Sender: TObject);
var
  expFolder:TDSFolder;
begin
  if tvFolders.Selected=nil then exit;
  expFolder:=TDSFolder(tvFolders.Selected.Data);
  if odFolder.Execute then begin
    Screen.Cursor:=crHourGlass;
    Try
      pbProgress.Position:=0;
      pnlProgress.Visible:=True;
      Application.ProcessMessages;
      FFat.Export(expFolder,odFolder.FileName);
      pnlProgress.Visible:=False;
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TFileManager.alFileImportExecute(Sender: TObject);
var
  sPath:Core.Strings.VarString;
  Folder:TDSFolder;
  {$i frmFileManager.Process.Folders.inc}
begin
  if (tvFolders.Selected=nil) or (tvFolders.Selected.Data=nil) then exit;
  if odFolder.Execute then begin
    sPath:=odFolder.FileName;
    Folder:=TDSFolder(tvFolders.Selected.Data);
    Screen.Cursor:=crHourGlass;
    Try
      pbProgress.Position:=0;
      pnlProgress.Visible:=True;
      Application.ProcessMessages;
      FFat.Import(Folder,sPath);
      PushProcessFolder(Folder);
      SetFolder(TDSFolder(tvFolders.Selected.Data));
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TFileManager.alFileNewFileExecute(Sender: TObject);
var
  dsFile:TDSFile;
  liNew:TListItem;
  dsFolder:TDSFolder;
begin
  if (tvFolders.Selected=nil) or (tvFolders.Selected.Data=nil) then exit;
  dsFolder:=TDSFolder(tvFolders.Selected.Data);
  if dsFolder<>nil then begin
    dsFile:=dsFolder.Files.New(FS_ATTR_NONE,FAT_KEYWORDS_ON,NO_DEFLATE,NO_CACHE,'New File','');
    FFileEdit:=dsFile;
    while lvFiles.Selected<>nil do
      lvFiles.Selected.Selected:=false;
    liNew:=lvFiles.Items.Add;
    liNew.Caption:=dsFile.Name;
    liNew.SubItems.Add(YES_NO[dsFile.Cache]);
    liNew.SubItems.Add(YES_NO[dsFile.Deflate]);
    liNew.SubItems.Add(YES_NO[dsFile.HasKeywords]);
    liNew.SubItems.Add('0');
    liNew.SubItems.Add(IntToStr(dsFile.CacheTTL));
    liNew.SubItems.Add(Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF));
    liNew.SubItems.Add(Core.Utils.Time.DateTimeToString(dsFile.Created,dtpoShort,BIAS_STAMP_OFF));
    liNew.Data:=dsFile;
    liNew.Selected:=True;
    lvFiles.Selected:=liNew;
    liNew.MakeVisible(False);
    txtFileName.Text:=liNew.Caption;
    gbFileNameEditor.Visible:=True;
    txtFileName.SetFocus;
  end;
end;

procedure TFileManager.alFileNewFolderExecute(Sender: TObject);
var
  tvFolder:TTreeNode;
  tvNew:TTreeNode;
  dsFolder:TDSFolder;
  dsNew:TDSFolder;
  sPath:Core.Strings.VarString;
begin
  tvFolder:=tvFolders.Selected;
  if tvFolder=nil then exit;
  dsFolder:=TDSFolder(tvFolders.Selected.Data);
  if dsFolder<>nil then begin
    sPath:=Concat(dsFolder.Path,'/New Folder');
    dsNew:=FFat.Folders.New(sPath,FS_ATTR_NONE);

    tvNew:=tvFolders.Items.AddChild(tvFolder,'New Folder');
    tvNew.Data:=dsNew;
    tvNew.EditText;
  end;
end;

procedure TFileManager.alKeywordsOffExecute(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if lvFiles.Selected<>nil then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.HasKeywords:=false;
          dsFile.Save(FTask, dsFileSaveKeywords);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_KEYWORDS]:=YES_NO[dsFile.HasKeywords];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alKeywordsOnExecute(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if lvFiles.Selected<>nil then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(lvFiles.Selected.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.HasKeywords:=true;
          dsFile.Save(FTask,dsFileSaveKeywords);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_KEYWORDS]:=YES_NO[dsFile.HasKeywords];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alKeywordsToggleExecute(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if (lvFiles.Selected<>nil) and (FChecking=false) then begin
    FChecking:=true;
    alKeywordsToggle.Checked:=not alKeywordsToggle.Checked;
    FChecking:=false;
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(li.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.HasKeywords:=alKeywordsToggle.Checked;
          dsFile.Save(FTask,dsFileSaveKeywords);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_KEYWORDS]:=YES_NO[dsFile.HasKeywords];
        end;
      end;
    end;
  end;
end;

procedure TFileManager.alViewRefreshExecute(Sender: TObject);
begin
  if FLoading or (tvFolders.Selected=nil) then exit;
  SetFolder(TDSFolder(tvFolders.Selected.Data));
end;

procedure TFileManager.btnContentClick(Sender: TObject);
var
  fsStream:TFileStream;
  mItem:Storage.RTSP.Manifest.TItem;
begin
  if odStream.Execute() then begin
    fsStream:=TFileStream.Create(odStream.FileName,fmOpenRead);

    // Chop into bins and
    Storage.RTSP.Manifest.Init(mItem);
    Try
      Storage.RTSP.Manifest.DB.createStream(FTask,FDomainP^.ID,FRootP^.ID,FFolder.ID,FStreamEdit.ID,Storage.AuraDisks.Kinds.Domain,fsStream.Size,mItem);
    Finally
      Storage.RTSP.Manifest.Done(mItem);
    end;
    //createStream(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID:QWord; Space:Integer; Size:QWord; out Item:TItem):boolean;
  end;
end;

procedure TFileManager.btnFileRenameCancelClick(Sender: TObject);
begin
  gbFileNameEditor.Visible:=false;
  txtFileName.Clear;
  FFileEdit:=nil;
end;

procedure TFileManager.btnFileRenameClick(Sender: TObject);
var
  li:TListItem;

  procedure PushEndEdit(Canceled:Boolean);
  begin
    {$i frmFileMan.EndEdit.inc}
  end;
begin
  if (FFileEdit=nil) or (txtFileName.GetTextLen=0)  then begin
    PushEndEdit(true);
    exit;
  end else begin
    PushEndEdit(false);
  end;
end;

procedure TFileManager.btnFindClick(Sender: TObject);
const
  sQuery:Core.Strings.VarString='';
var
  iLcv:integer;
  li:TListItem;
  List:TList;
  dsFolder:TDSFolder;
  dsFile:TDSFile;
begin
  if (gbManagerResults.Visible) and (sQuery=txtFilesFind.Text) then begin
    lvManagerSearchResults.BeginUpdate;
    lvManagerSearchResults.Clear;
    lvManagerSearchResults.EndUpdate;
    gbManagerResults.Visible:=false;
  end else begin
    sQuery:=txtFilesFind.Text;
    lvManagerSearchResults.Clear;
    gbManagerResults.Visible:=True;
    lvManagerSearchResults.Cursor:=crHourglass;
    lbStatus.Caption:='Searching...';
    Application.ProcessMessages;
    List:=TList.Create;
    Try
      FFat.Search(List,sQuery);
      lbStatus.Caption:='Loading...';
      Application.ProcessMessages;
      lvManagerSearchResults.Hint:=Format('Found %.0N results.',[Double(List.Count)]);
      for iLcv:=0 to List.Count-1 do begin
        if TObject(List[iLcv]) is TDSFile then begin
          dsFile:=TDSFile(List[iLcv]);
          li:=lvManagerSearchResults.Items.Add;
          li.Caption:=dsFile.Name;
          li.SubItems.Add(dsFile.Parent.Path);
          li.Data:=dsFile;
        end else begin
          dsFolder:=TDSFolder(List[iLcv]);
          li:=lvManagerSearchResults.Items.Add;
          li.Caption:=Core.Utils.Files.Extract(dsFolder.Path,epoName);
          li.SubItems.Add(dsFolder.Path);
          li.Data:=dsFolder;
        end;
      end;
    Finally
      FreeAndNil(List);
      lvManagerSearchResults.Cursor:=crDefault;
      lbStatus.Caption:='';
    end;
  end;
end;


procedure TFileManager.btnStreamRenameClick(Sender: TObject);
  procedure PushEndEdit(Canceled:Boolean);
  begin
    {$i frmFileMan.Stream.EndEdit.inc}
  end;
begin
  if (FStreamEdit=nil) or (txtStreamName.GetTextLen=0)  then begin
    PushEndEdit(true);
    exit;
  end else begin
    PushEndEdit(false);
  end;
end;

procedure TFileManager.cbPathSelect(Sender: TObject);
var
  dsFolder:TDSFolder;
begin
  if FFat.Find(dsFolder,cbPath.Text,FRefactor) then
    SetFolder(dsFolder);
end;

procedure TFileManager.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  Core.Utils.Forms.List.UnLoad(FInfoP^);
end;

procedure TFileManager.Clear;
begin
  FLoading:=True;
  Try
    tvFolders.BeginUpdate;
    Try
      tvFolders.Items.Clear;
    finally
      tvFolders.EndUpdate;
    end;
    lvFiles.BeginUpdate;
    Try
      lvFiles.Clear;
    finally
      lvFiles.EndUpdate;
    end;
    FFat.Clear;
    FPath:='';
    FFolder:=nil;
  finally
    FLoading:=False;
  end;
end;

procedure TFileManager.LoadDomain;
  procedure PushProcessFolder(Folder:TDSFolder);
  var
    iLcv:integer;
    List:TList;
  begin
    Core.Utils.TreeView.Add(Folder.Path,tvFolders,Folder);
    List:=Folder.Folders.LockList();
    Try
      for iLcv:=0 to List.Count-1 do
        PushProcessFolder(TDSFolder(List[iLcv]));
    Finally
      Folder.Folders.UnlockList();
    end;
  end;

  procedure PushProcessFolders(Folders:TDSFolders);
  var
    iLcv:integer;
    List:TList;
  begin
    List:=Folders.Locklist();
    Try
      for iLcv:=0 to List.Count-1 do
        PushProcessFolder(TDSFolder(List[iLcv]));
    finally
      Folders.UnlockList();
    end;
  end;
begin
  FLoading:=true;
  Try
    Screen.Cursor:=crHourglass;
    Try
      tvFolders.BeginUpdate;
      Try
        tvFolders.Items.Clear;
        lvFiles.Items.Clear;
        cbPath.Items.Clear;
        cbPath.Text:='';
        Application.ProcessMessages;
        FFat.Load(FDomainP^.ID,Storage.MatrixServices.Default.Cluster,Storage.MatrixServices.Default.Resource,Storage.MatrixServices.Default.Node,dsfatloBoth);
        Application.ProcessMessages;
        PushProcessFolders(FFat.Folders);
      finally
        tvFolders.EndUpdate;
      end;
    finally
      Screen.Cursor:=crDefault;
    end;
  finally
    FLoading:=false;
  end;
end;

procedure TFileManager.Backup(sDomain:Core.Strings.VarString);
var
  Archive:TDSFat;
  BackupID:QWord;
begin
  FLoading:=True;
  Try
    Archive:=TDSFat.Create();
    Try
      Storage.Domains.Items.DB.GetID(FTask,sDomain,BackupID);
      Application.ProcessMessages;
      Archive.Load(BackupID,Storage.MatrixServices.Default.Cluster,Storage.MatrixServices.Default.Resource,Storage.MatrixServices.Default.Node,dsfatloBoth);
      if sdFile.Execute then
        Archive.Backup(sdFile.FileName);
    finally
      FreeAndNil(Archive);
    end;
  finally
    FLoading:=False;
  end;
end;

procedure TFileManager.Show(DomainP:Storage.Domains.Items.PDomain; RootP:Storage.UserAccounts.Items.PItem);
begin
  FDomainP:=DomainP;
  FRootP:=RootP;
  lbDomain.Caption:=FDomainP^.Name;
  FInfoP:=Core.Utils.Forms.List.Load(Self,FDomainP,MainMenu,miWindow);
  Inherited Show;
  LoadDomain;
end;

procedure TFileManager.SetFolder(Folder:TDSFolder);
const Canceled:boolean=false;
var
  iLcv:integer;
  li:TListItem;
begin
  Screen.Cursor:=crHourGlass;
  Try
    if gbFileNameEditor.Visible then begin
      {$i frmFileMan.EndEdit.inc}
    end;
    if (FFolder<>nil) and FFolder.Loading then begin
      Canceled:=true;
      repeat
        Application.ProcessMessages;
      until (FFolder.Loading=false);
      Canceled:=False;
    end;
    FFolder:=Folder;
    if (FFolder<>nil) then begin
      {$i frmFileMan.AddFile.inc}
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;


procedure TFileManager.lvFilesResize(Sender: TObject);
var
  iBias:Integer;
begin
  iBias:=lvFiles.Columns[1].MaxWidth+lvFiles.Columns[2].MaxWidth+lvFiles.Columns[3].MaxWidth+lvFiles.Columns[4].MaxWidth+4;
  lvFiles.Columns[0].Width:=Math.Max(0,lvFiles.ClientWidth-iBias);
end;

procedure TFileManager.alFileRestoreExecute(Sender: TObject);
begin
  if odFolder.Execute then
    FFat.Restore(odFolder.FileName);
end;

procedure TFileManager.OnFileSelect(ItemP:Core.Timer.PItem);
const
  Canceled:boolean=true;
var
  dsSelect:TDSFile;
  li:TListItem;

  procedure DisableButtons;
  begin
    FChecking:=true;

    alKeywordsOn.Checked:=false;
    alKeywordsOff.Checked:=true;
    alKeywordsToggle.Checked:=false;

    alCacheOn.Checked:=false;
    alCacheOff.Checked:=true;
    alCacheToggle.Checked:=false;

    alCompressOn.Checked:=false;
    alCompressOff.Checked:=true;
    alCompressToggle.Checked:=false;

    FChecking:=false;
  end;

  procedure SetButtons;
  begin
    FChecking:=true;

    alKeywordsOn.Checked:=dsSelect.HasKeywords;
    alKeywordsOff.Checked:=(dsSelect.HasKeywords=false);
    alKeywordsToggle.Checked:=dsSelect.HasKeywords;

    alCacheOn.Checked:=dsSelect.Cache;
    alCacheOff.Checked:=(dsSelect.Cache=false);
    alCacheToggle.Checked:=dsSelect.Cache;

    alCompressOn.Checked:=dsSelect.Deflate;
    alCompressOff.Checked:=(dsSelect.Deflate=false);
    alCompressToggle.Checked:=dsSelect.Deflate;

    seCacheTTL.Value:=dsSelect.CacheTTL;

    FChecking:=false;
  end;

begin
  if gbFileNameEditor.Visible then begin
    {$i frmFileMan.EndEdit.inc}
  end;
  li:=lvFiles.Selected;
  if li<>nil then begin
    dsSelect:=TDSFile(li.Data);
    SetButtons();
  end else
    DisableButtons();

  ItemP^.Expires:=0;
end;

procedure TFileManager.onFolderSelect(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  SetFolder(TDSFolder(tvFolders.Selected.Data));
end;

procedure TFileManager.lvFilesSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  liItem:TListItem;
begin
  if FFileEdit=nil then begin
    tiFileSelect.Expires:=IncMillisecond(Core.Timer.dtNow,300);
  end else if (Core.Utils.ListView.IndexOf(lvFiles,FFileEdit,liItem)<>-1) and (Item<>liItem) then begin
    liItem.Selected:=true;
  end;
end;

procedure TFileManager.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFileManager.miFileImportClick(Sender: TObject);
var
  sPath:Core.Strings.VarString;
  Folder:TDSFolder;

  procedure PushProcessFolder(Folder:TDSFolder);
  var
    iLcv:integer;
    List:TList;
  begin
    Core.Utils.TreeView.Add(Folder.Path,tvFolders,Folder);
    List:=Folder.Folders.LockList();
    Try
      for iLcv:=0 to List.Count-1 do
        PushProcessFolder(TDSFolder(List[iLcv]));
    Finally
      Folder.Folders.UnlockList();
    end;
  end;

  procedure PushProcessFolders(Folders:TDSFolders);
  var
    iLcv:integer;
    List:TList;
  begin
    List:=Folders.LockList();
    Try
      for iLcv:=0 to List.Count-1 do
        PushProcessFolder(TDSFolder(List[iLcv]));
    finally
      Folders.UnlockList();
    end;
  end;
begin
  if (tvFolders.Selected=nil) or (tvFolders.Selected.Data=nil) then exit;
  if odFolder.Execute then begin
    sPath:=odFolder.FileName;
    Folder:=TDSFolder(tvFolders.Selected.Data);
    Screen.Cursor:=crHourGlass;
    Try
      Application.ProcessMessages;
      lbStatus.Caption:='Importing...';
      Application.ProcessMessages;
      FFat.Import(Folder,sPath);
      PushProcessFolder(Folder);
      lbStatus.Caption:='';
      SetFolder(TDSFolder(tvFolders.Selected.Data));
    finally
      Screen.Cursor:=crDefault;
    end;
  end;
end;

procedure TFileManager.miOpenClick(Sender: TObject);
var
  dsFile:TDSFile;
  frmEdit:TSCSEditorForm;
  frmImage:TImageViewerForm;
begin
  if lvFiles.Selected<>nil then begin
    dsFile:=TDSFile(lvFiles.Selected.Data);
    if dsFile<>nil then begin
      if Core.Utils.Forms.List.Show(dsFile)=false then begin
        if (
             Pos('image',
             Lowercase(RSR.HTTP.ContentTypeFromFile(Storage.ContentTypes.List,ExtractFileExt(dsFile.Name))))
             >0
           )
        then begin
          frmImage:=TImageViewerForm.Create(nil);
          frmImage.Show(dsFile);
        end else begin
          frmEdit:=TSCSEditorForm.Create(nil);
          frmEdit.Show(dsFile);
        end;
      end;
    end;
  end;
end;

procedure TFileManager.seCacheTTLChange(Sender: TObject);
var
  dsFile:TDSFile;
  iLcv:integer;
  li:TListItem;
begin
  if (lvFiles.Selected<>nil) and (FChecking=false) then begin
    for iLcv:=0 to lvFiles.Items.Count-1 do begin
      li:=lvFiles.Items[iLcv];
      if li.Selected then begin
        dsFile:=TDSFile(li.Data);
        If ((dsFile.Attributes or FS_ATTR_SYSTEM)<>dsFile.Attributes) then begin
          dsFile.CacheTTL:=seCacheTTL.Value;
          dsFile.Save(FTask,dsFileSaveCache);
          li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(dsFile.Modified,dtpoShort,BIAS_STAMP_OFF);
          li.SubItems[IDX_FILE_TTL]:=IntToStr(dsFile.CacheTTL);
        end;
      end;
    end;
  end;
end;

procedure TFileManager.tvFoldersEdited(Sender: TObject; Node: TTreeNode; var S: Core.Strings.VarString);
var
  dsFolder:TDSFolder;
begin
  if Node=nil then exit;
  dsFolder:=TDSFolder(Node.Data);
  if (dsFolder<>nil) and (Node.Text<>S) then begin
    dsFolder.Rename(S);
    Node.Text:=S;
  end;
end;

procedure TFileManager.tvFoldersEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  FFolder:=TDSFolder(Node.Data);
  AllowEdit:=(
     (FFolder.Attributes or FS_ATTR_SYSTEM <> FFolder.Attributes) and
     (FFolder.Attributes or FS_ATTR_READONLY <> FFolder.Attributes) and
     (FFolder.Attributes or FS_ATTR_COREOBJECT <> FFolder.Attributes)
  );
end;


procedure TFileManager.tvFoldersSelectionChanged(Sender: TObject);
begin
  if FLoading or (tvFolders.Selected=nil) then exit;
  tiFolderSelect.Expires:=IncMillisecond(Core.Timer.dtNow,300);
end;

procedure TFileManager.txtFileNameKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=27 then begin
    key:=0;
  end;
end;

procedure TFileManager.txtFilesFindKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    btnFind.Click;
end;

procedure TFileManager.txtFileNameKeyPress(Sender: TObject; var Key: char);
var
  Canceled:boolean=true;
  li:TListItem;
begin
  if Key in [#13,#27] then begin
    Canceled:=(Key=#27);
    Key:=#0;
    {$i frmFileMan.EndEdit.inc}
  end;
end;

initialization
  {$I frmFileMan.lrs}

end.


unit uEngine;

interface

uses
  Classes,
  SysUtils,
  auLang,

  App,
  App.Consts,
  App.Build,

  RSR,
  RSR.HTTP,
  RSR.DNS,
  RSR.Core,

  Core.Timer,
  Core.Threads,
  Core.Generics,
  Core.Strings,
  Core.Streams,
  Core.XML,
  Core.Logging,
  Core.Interlocked,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.Arrays.LargeInt,
  Core.Arrays.Bytes,

  Core.Utils.Time,
  Core.Utils.Files,
  Core.Utils.Sockets,

  Storage,
  Storage.Main,
  Storage.VDM,
  Storage.UserAccounts,
  Storage.Social,
  Storage.Social.Files,
  Storage.Social.Folders,
  Storage.Social.Network,
  Storage.Social.Sync,
  Storage.Social.Sync.Queue,
  Storage.Social.Sync.Pipes,


  DOM,
  XMLRead,
  MD5;

Const
  URI_SOC                                 : Core.Strings.VarString = '/core/soc/';
  URI_VDM                                 : Core.Strings.VarString = '/core/vdm/';
  URI_VDM_ACCOUNT                         : Core.Strings.VarString = '/core/vdm/account/';
  NS_CORE_OBJ_VDM                         : Core.Strings.VarString = '/core/vdm';
  NS_CORE_OBJ_SOCIAL                      : Core.Strings.VarString = '/core/soc';
  NS_CORE_OBJ_VDM_ACCOUNT                 : Core.Strings.VarString = '/core/vdm/account';

  NS_CORE_CMD_SOC_FLDRS_ADD               : Core.Strings.VarString = '/fldrs/a';  // Add a folder
  NS_CORE_CMD_SOC_FLDRS_LIST              : Core.Strings.VarString = '/fldrs/l';  // List folders
  NS_CORE_CMD_SOC_NETWORK_LIST            : Core.Strings.VarString = '/n/l';      // List own network connections
  NS_CORE_CMD_SOC_CONNECTION_LIST         : Core.Strings.VarString = '/c/l';      // List other network connnections
  NS_CORE_CMD_SOC_FILE_LIST               : Core.Strings.VarString = '/fls/l';    // List files in a folder
  NS_CORE_CMD_SOC_FILE_LIST_ALL           : Core.Strings.VarString = '/fls/la';   // List all files
  NS_CORE_CMD_SOC_FILE_ADD                : Core.Strings.VarString = '/fls/a';    // Add a file
  NS_CORE_CMD_SOC_FILE_SETDATA            : Core.Strings.VarString = '/fls/sed';  // Sets raw data
  NS_CORE_CMD_SOC_FILE_SETDATA_PARAM      : Core.Strings.VarString = 'fls/sed';   // Sets raw data
  NS_CORE_CMD_SOC_FILE_GETDATA            : Core.Strings.VarString = '/fls/ged';  // Gets raw data
  NS_CORE_CMD_SOC_FILE_GETDATA_PARAM      : Core.Strings.VarString = 'fls/ged';   // Gets raw data

  NS_CORE_CMD_SOC_FILE_DOWNLOAD           : Core.Strings.VarString = '/fls/dl';   // Download a file
  NS_CORE_CMD_SOC_FILE_GET                : Core.Strings.VarString = '/fls/get';  // Get a file over http
  NS_CORE_CMD_SOC_FILE_READ               : Core.Strings.VarString = '/fls/r';    // Read a file
  NS_CORE_CMD_SOC_FILE_WRITE              : Core.Strings.VarString = '/fls/w';    // Write a file
  NS_CORE_CMD_SOC_SYNC_READ               : Core.Strings.VarString = '/sync/r';   // Read a Sync Item based on ResourceID
  NS_CORE_CMD_SOC_SYNC_WRITE              : Core.Strings.VarString = '/sync/w';   // Write a Sync Item with ResourceID

  NS_CORE_CMD_FLDRS_LIST                  : Core.Strings.VarString = '/fldrs/l';  // List Folders;
  NS_CORE_CMD_FILES_LIST                  : Core.Strings.VarString = '/fls/l';    // List Files in a folder
  NS_CORE_CMD_FILES_LIST_ALL              : Core.Strings.VarString = '/fls/la';   // List All Files
  NS_CORE_CMD_FILES_ADD                   : Core.Strings.VarString = '/fls/a';    // Add File
  NS_CORE_CMD_FILES_WRITE                 : Core.Strings.VarString = '/fls/w';    // Save a File
  NS_CORE_CMD_FILES_SETDATA               : Core.Strings.VarString = '/fls/sed';  // Sets raw data
  NS_CORE_CMD_FILES_SETDATA_PARAM         : Core.Strings.VarString = 'fls/sed';   // Sets raw data
  NS_CORE_CMD_FILES_GETDATA               : Core.Strings.VarString = '/fls/ged';  // Gets raw data
  NS_CORE_CMD_FILES_GETDATA_PARAM         : Core.Strings.VarString = 'fls/ged';   // Gets raw data



  NS_CORE_CMD_FILES_READ                  : Core.Strings.VarString = '/fls/r';    // Read at least 1 File
  NS_CORE_CMD_FLDR_DEL                    : Core.Strings.VarString = '/fldrs/d';  // Delete Folder
  NS_CORE_CMD_FLDR_ADD                    : Core.Strings.VarString = '/fldrs/a';  // Create Folder

  NS_CORE_CMD_RC_ADD                      : Core.Strings.VarString = '/res/a';    // Create a Resource
  NS_CORE_CMD_RC_DEL                      : Core.Strings.VarString = '/res/d';    // Delete a Resource
  NS_CORE_CMD_RC_LIST                     : Core.Strings.VarString = '/res/l';    // List all Resources for this user
  NS_CORE_CMD_RC_REFRESH                  : Core.Strings.VarString = '/res/h';    // Refresh a Resource
  NS_CORE_CMD_RC_WRITE                    : Core.Strings.VarString = '/res/w';    // Save a Resource
  NS_CORE_CMD_SYNC_READ                   : Core.Strings.VarString = '/sync/r';   // Read a Sync Item based on ResourceID
  NS_CORE_CMD_SYNC_WRITE                  : Core.Strings.VarString = '/sync/w';   // Write a Sync Item with ResourceID

  NS_CORE_CMD_SYNC_OPEN_CHANNEL           : Core.Strings.VarString = '/sync/oc';  // Open socket sync channel
  NS_CORE_CMD_SYNC_CLOSE_CHANNEL          : Core.Strings.VarString = '/sync/cc';  // Close socket sync channel

  NS_CORE_CMD_ACCT_READ                   : Core.Strings.VarString = '/ar';  // Read User Account Information

  fieldCoreObject                         : Core.Strings.VarString = 'co-ns';
  fieldCoreCommand                        : Core.Strings.VarString = 'cc-ns';
  fieldAuth                               : Core.Strings.VarString = 'auth';
  fieldResource                           : Core.Strings.VarString = 'resource';
  fieldCode                               : Core.Strings.VarString = 'code';



Type
  TAuSocketMan=class;

  StatusEvent=procedure(pgValue,pgMax:QWORD; Status:Core.Strings.VarString) of object;
  ErrorEvent=procedure(Var SR:TRSR; cObject,cCommand:Core.Strings.VarString; Code:WORD) of object;

  EngineStatus=(esStop,esStart,esPause);
  EngineMode=(emNone,emWorking,emBuffersFull);

  {$i uEngine.ScanThread.Type.inc}

  {$i uEngine.cmProcessSyncItems.Type.inc}
  {$i uEngine.cmProcessAllFilesSocial.Type.inc}

  {$i uEngine.cmWriteResource.Type.inc}
  {$i uEngine.cmWriteSyncsSocial.Type.inc}
  {$i uEngine.cmReadAccount.Type.inc}

  {$i uEngine.cmReadSocialSync.Type.inc}
  {$i uEngine.cmListResources.Type.inc}
  {$i uEngine.cmCreateResource.Type.inc}
  {$i uEngine.cmCreateFolderSocial.Type.inc}
  {$i uEngine.cmListFoldersSocial.Type.inc}
  {$i uEngine.cmDeleteResource.Type.inc}
  {$i uEngine.cmListAllSocialFiles.Type.inc}
  {$i uEngine.cmListNetworks.Type.inc}
  {$i uEngine.cmListConnections.Type.inc}
  {$i uEngine.cmProcessListFoldersSocial.Type.inc}

  {$i uEngine.cmSyncReadSocial.Type.inc}

  TAuSocketMan=class(TRSRManager)
  private
    FMode                        : EngineMode;
    FStatus                      : EngineStatus;
    FResponse                    : THTTPResponse;
    FXMLParser                   : TDOMParser;
    FXMLDocument                 : TXMLDocument;
    FAccount                     : Storage.UserAccounts.Items.Item;
    FStatusLock                  : TRTLCriticalSection;

  private
    // User based Sync events
    FOnAccountRetrieved          : Storage.UserAccounts.Items.Event;
  private
    FOnResourcesListed           : Resources.TResourcesEvent;
    FOnResourceAdded             : Resources.TResourceEvent;
    FOnResourceWritten           : Resources.TResourceEvent;
    FOnResourceDeleted           : Resources.TResourceEvent;
  private
    FOnConnected                 : TRSREvent;
    FOnDisconnected              : TRSREvent;
    FOnError                     : TRSREvent;
    FOnFinalized                 : TRSREvent;
  private
    FOnStatusChanged             : StatusEvent;

    FOnCoreError                 : ErrorEvent;
    FOnConnectionsListed         : Storage.Social.Connection.TConnectionsEvent;
    FOnNetworksListed            : Storage.Social.Network.TNetworksEvent;
  private
    FRSRP                        : PRSR;
  private
    FCoreCommand                 : Core.Strings.VarString;
    FCoreObject                  : Core.Strings.VarString;
    FEngineStatus                : Core.Strings.VarString;
    FCoreCode                    : WORD;
  private
    FResourceID                  : QWord;
  private
    // User based Sync

    FNetworksLoaded              : boolean;
    FLocalized                   : Core.Strings.VarString;
    FSyncQueue                   : Storage.Social.Sync.Queue.TItems;
  private
    // Social based Sync
    FOnSocSyncModified           : Storage.Social.Sync.TSyncEvent;
    FOnSocSyncRetrieved          : Storage.Social.Sync.TSyncEvent;

    FOnSyncItemQueued            : Storage.Social.Sync.Queue.ItemEvent;
    FOnSyncItemRemoved           : Storage.Social.Sync.Queue.ItemEvent;


    FSocSyncList                 : Storage.Social.Sync.THeaderList;

    FSocHdrP                     : Storage.Social.Sync.PHeader;
    FSocSyncP                    : Storage.Social.Sync.PSync;

    FConnections                 : Storage.Social.Connection.TConnections;
    FConNetworks                 : Storage.Social.Network.TNetworks;
    FNetworks                    : Storage.Social.Network.TNetworks;

    FNetworkP                    : Storage.Social.Network.PNetwork;

    FSocFolder                   : Storage.Social.Folders.TSFolder;
    FSocFolderP                  : Storage.Social.Folders.PSFolder;



    FSocFile                     : Storage.Social.Files.TSFile;
    FSocFileP                    : Storage.Social.Files.PSFile;
    FSocFileReads                : Storage.Social.Files.TSFiles;
  private
    FAuthenticated               : boolean;
    FConnected                   : boolean;
    FResources                   : Resources.TResources;
    FResource                    : Resources.TResource;
    FResourceP                   : Resources.PResource;


  private
    function    ParseXML(var Data:Core.Strings.VarString):boolean; overload;
    function    ParseXML(Data:TMemoryStream; ClearAfter:Boolean):boolean; overload;
    function    ParseXML(var Data:Core.Strings.VarString; xHeader:boolean):boolean; overload;
  private
    procedure   DoCoreError(RSRP:PRSR; Code:Word);
  private
    procedure   FileQueueItemCreate(RSRP:PRSR; Request:THTTPRequest; Item:Storage.Social.Sync.Queue.TItem);
    procedure   FileQueueItemRead(RSRP:PRSR; Request:THTTPRequest; Item:Storage.Social.Sync.Queue.TItem);
    procedure   FileQueueItemWrite(RSRP:PRSR; Request:THTTPRequest; Item:Storage.Social.Sync.Queue.TItem);
  private
    procedure   SyncItemQueued();
    procedure   SyncItemRemoved();


    procedure   SyncSocSyncRetrieved();
    procedure   SyncAccountRetrieved();

    procedure   SyncConnected();
    procedure   SyncDisconnected();
    procedure   SyncError();
    procedure   SyncFinalized();
    procedure   SyncResourcesListed();
    procedure   SyncResourceAdded();
    procedure   SyncResourceWritten();
    procedure   SyncResourceDeleted();
    procedure   SyncNetworksListed();
    procedure   SyncConnectionsListed();
    procedure   SyncCoreError();
  protected
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:Boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
  protected
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    constructor Create(); reIntroduce;
    destructor  Destroy(); override;
  public
    function    UploadAllowed(Name:Core.Strings.VarString):boolean;
    procedure   DNSLookup(RSRP:PRSR; Value:Core.Strings.VarString; Types:TDNSStyles); reIntroduce;
  public
    procedure   SyncFileUpdated(FileP:Storage.Social.Files.PSFile; Data:TStream); overload;
  public
    procedure   ListAllFiles(Var SR:TRSR; var Sync:Storage.Social.Sync.TSync); overload;
  public
    procedure   ListFolders(Var SR:TRSR; var Sync:Storage.Social.Sync.TSync); overload;
  public
    function    ReadFile(Var SR:TRSR; Request:THTTPRequest; var Item:Storage.Social.Files.TSFile):Int64; overload;
    function    WriteFile(Var SR:TRSR; Request:THTTPRequest; var Item:Storage.Social.Files.TSFile; Data:TFileStream):Int64; overload;
    function    CreateFile(Var SR:TRSR; Request:THTTPRequest; var Item:Storage.Social.Files.TSFile):Int64; overload;
  public
    procedure   ListResources(Var SR:TRSR);
    procedure   ListNetworks(Var SR:TRSR);
    procedure   ListConnections(Var SR:TRSR);
  public
    procedure   ReadAccount(Var SR:TRSR);
  public
    procedure   ReadSync(Var SR:TRSR; var Sync:Storage.Social.Sync.TSync); overload;
  public
    procedure   CreateFolder(Var SR:TRSR; var Folder:Storage.Social.Folders.TSFolder); overload;
  public
    procedure   CreateResource(Var SR:TRSR; Name,Description:Core.Strings.VarString; aRemember,aSyncUpload,aSyncDownload:boolean);
    procedure   DeleteResource(Var SR:TRSR; iID:Int64);
  public
    procedure   WriteResource(Var SR:TRSR; var Item:Storage.VDM.Resources.TResource);
    procedure   WriteSyncSocial(Var SR:TRSR; var aSync:Storage.Social.Sync.TSync); overload;
    procedure   SetResource(var Item:Storage.VDM.Resources.TResource);
  public
    function    getSyncStatus:Core.Strings.VarString;
    procedure   setSyncStatus(Value:Core.Strings.VarString);
    function    getSyncFileSize():QWord;

    function    getSyncList():Storage.Social.Sync.PHeaderList;
    function    getSync(var Network:Storage.Social.Network.TNetwork):Storage.Social.Sync.PHeader; overload;
    function    getNetwork(iID:QWord):Storage.Social.Network.PNetwork;
    function    getResource(iID:QWord):Resources.PResource;
    function    getPipe(Name:Core.Strings.VarString; var Sync:Storage.Social.Sync.TSync):Storage.Social.Sync.Pipes.PItem; overload;

    procedure   Reset();
  public
    DoNotUploadByNames          : Core.Arrays.Types.VarString;
    DoNotUploadByStartOfName    : Core.Arrays.Types.VarString;

  public
    property    Resources:Resources.TResources read FResources;
    property    Resource:Resources.TResource read FResource;
    property    Networks:Storage.Social.Network.TNetworks read FNetworks;
    property    Connections:Storage.Social.Connection.TConnections read FConnections;
    property    OtherNetworks:Storage.Social.Network.TNetworks read FConNetworks;
  public
    property    Authenticated:boolean read FAuthenticated;
  public
    property    OnSyncItemQueued : Storage.Social.Sync.Queue.ItemEvent read FOnSyncItemQueued write FOnSyncItemQueued;
    property    OnSyncItemRemoved : Storage.Social.Sync.Queue.ItemEvent read FOnSyncItemRemoved write FOnSyncItemRemoved;

    property    OnAccountRetrieved: Storage.UserAccounts.Items.Event read FOnAccountRetrieved write FOnAccountRetrieved;
    property    OnSocSyncModified:Storage.Social.Sync.TSyncEvent read FOnSocSyncModified write FOnSocSyncModified;

    property    OnSocSyncRetrieved: Storage.Social.Sync.TSyncEvent read FOnSocSyncRetrieved write FOnSocSyncRetrieved;

    property    OnResourcesListed:Storage.VDM.Resources.TResourcesEvent read FOnResourcesListed write FOnResourcesListed;
    property    OnResourceAdded:Storage.VDM.Resources.TResourceEvent read FOnResourceAdded write FOnResourceAdded;
    property    OnResourceWritten:Storage.VDM.Resources.TResourceEvent read FOnResourceWritten write FOnResourceWritten;
    property    OnResourceDeleted:Storage.VDM.Resources.TResourceEvent read FOnResourceDeleted write FOnResourceDeleted;
    property    OnCoreError:ErrorEvent read FOnCoreError write FOnCoreError;
    property    OnConnected:TRSREvent read FOnConnected write FOnConnected;
    property    OnDisconnected:TRSREvent read FOnDisconnected write FOnDisconnected;
    property    OnFinalized:TRSREvent read FOnFinalized write FOnFinalized;
    property    OnErrored:TRSREvent read FOnFinalized write FOnError;
    property    OnStatusChanged:StatusEvent read FOnStatusChanged write FOnStatusChanged;
    property    OnNetworksListed:Storage.Social.Network.TNetworksEvent read FOnNetworksListed write FOnNetworksListed;
    property    OnConnectionsListed:Storage.Social.Connection.TConnectionsEvent read FOnConnectionsListed write FOnConnectionsListed;

    property    Connected:boolean read FConnected;
    property    ResourceID: QWord read FResourceID write FResourceID;

    property    NetworksLoaded : boolean read FNetworksLoaded;

    property    Mode:EngineMode read FMode write FMode;
    property    Status: EngineStatus read FStatus write FStatus;
  end;
const
  Engine_Status:Array[EngineStatus] of Core.Strings.PVarString = (
    @auLang.Table.Engine.Status.Stopped,
    @auLang.Table.Engine.Status.Running,
    @auLang.Table.Engine.Status.Paused
  );

implementation
uses
  auSettings,
  DateUtils;

constructor TAuSocketMan.Create();
begin
  Core.Arrays.VarString.Init(DoNotUploadByNames);
  Core.Arrays.VarString.Init(DoNotUploadByStartOfName);
  Core.Arrays.VarString.Add(@DoNotUploadByStartOfName,'._');
  Core.Arrays.VarString.Add(@DoNotUploadByStartOfName,'.~lock.');
  Core.Arrays.VarString.Add(@DoNotUploadByNames,'.ds_store');
  Core.Arrays.VarString.Add(@DoNotUploadByNames,'thumbs.db');
  Core.Arrays.VarString.Add(@DoNotUploadByNames,'desktop.ini');
  Storage.UserAccounts.Items.Init(FAccount);

  InitCriticalSection(FStatusLock);
  FMode:=emNone;

  FOnError:=nil;
  FOnSocSyncModified:=nil;

  FNetworksLoaded:=false;
  FSyncQueue:=Storage.Social.Sync.Queue.TItems.Create(Self);
  FSyncQueue.OnFileCreate:=@FileQueueItemCreate;
  FSyncQueue.OnFileRead:=@FileQueueItemRead;
  FSyncQueue.OnFileWrite:=@FileQueueItemWrite;
  FSyncQueue.OnSyncItemQueued:=@SyncItemQueued;
  FSyncQueue.OnSyncItemRemoved:=@SyncItemRemoved;


  FXMLParser:=TDOMParser.Create();
  FXMLParser.Options.Validate:=False;
  FResponse:=THTTPResponse.Create();
  Inherited Create(nil,NO_SSL,SSL_OFF,THREAD_METHODS_ON,HTTP_STACK_SIZE);
end;

destructor  TAuSocketMan.Destroy();
begin
  Core.Arrays.VarString.Done(DoNotUploadByNames);
  Core.Arrays.VarString.Done(DoNotUploadByStartOfName);
  Storage.UserAccounts.Items.Done(FAccount);
  FSyncQueue.Free();
  FXMLParser.Free();
  FResponse.Free();
  DoneCriticalSection(FStatusLock);
  Inherited Destroy();
end;

procedure   TAuSocketMan.OnQueue(RSRP:PRSR);
begin
  RSRP^.Finite:=false;
end;

function    TAuSocketMan.UploadAllowed(Name:Core.Strings.VarString):boolean;
begin
  Result:=true;
  if Core.Arrays.VarString.Find(DoNotUploadByNames,Name) then
    Result:=false
  else
    Result:=Core.Utils.Files.Search(DoNotUploadByStartOfName,Name)=-1;
end;

procedure   TAuSocketMan.DNSLookup(RSRP:PRSR; Value:Core.Strings.VarString; Types:TDNSStyles);
begin

end;

function    TAuSocketMan.getSync(var Network:Storage.Social.Network.TNetwork):Storage.Social.Sync.PHeader;
begin
  Result:=Storage.Social.Sync.Get(Network,FSocSyncList);
end;

function    TAuSocketMan.getNetwork(iID:QWord):Storage.Social.Network.PNetwork;
begin
  Result:=Storage.Social.Network.Get(iID,FNetworks);
end;

function    TAuSocketMan.getResource(iID:QWord):Storage.VDM.Resources.PResource;
begin
  Result:=Storage.VDM.Resources.getItem(iID,FResources);
end;

function    TAuSocketMan.getPipe(Name:Core.Strings.VarString; var Sync:Storage.Social.Sync.TSync):Storage.Social.Sync.Pipes.PItem;
begin
  Result:=Storage.Social.Sync.Pipes.getItem(Name,Sync.Pipes);
end;

function    TAuSocketMan.getSyncList():Storage.Social.Sync.PHeaderList;
begin
  Result:=@FSocSyncList;
end;

procedure   TAuSocketMan.Reset();
begin
  FStatus:=esStop;
  FConnected:=false;
  FAuthenticated:=false;
  FRSRP:=nil;
end;

procedure   TAuSocketMan.OnError(RSRP:PRSR);
begin
  FRSRP:=RSRP;
  if Assigned(FOnError) then
    Synchronize(@SyncError);
end;

procedure   TAuSocketMan.OnDisconnect(RSRP:PRSR);
begin
  FAuthenticated:=false;
  FRSRP:=RSRP;
  if Assigned(FOnDisconnected) then
    Synchronize(@SyncDisconnected);
end;

procedure   TAuSocketMan.OnConnect(RSRP:PRSR);
begin
  FRSRP:=RSRP;
  FConnected:=true;
  if Assigned(FOnConnected) then
    Synchronize(@SyncConnected);
  FRSRP:=nil;
end;

procedure   TAuSocketMan.OnDNSResult(RSRP:PRSR);
begin

end;

procedure   TAuSocketMan.OnInitialize(RSRP:PRSR);
begin
  RSRP^.Finite:=false;
end;

procedure   TAuSocketMan.OnFinalize(RSRP:PRSR);
begin
  FRSRP:=RSRP;
  if Assigned(FOnFinalized) then
    Synchronize(@SyncFinalized);
end;

procedure   TAuSocketMan.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(
    SYSTEM_LOG,
    Concat(sProcedure,'.',sLocation),
    sError
  );
end;

procedure   TAuSocketMan.FileQueueItemCreate(RSRP:PRSR; Request:THTTPRequest; Item:Storage.Social.Sync.Queue.TItem);
begin
  CreateFile(RSRP^,Request,Item.FileP^);
  Item.Status:=Storage.Social.Sync.Queue.Status.Operation.soRequested;
end;

procedure   TAuSocketMan.FileQueueItemRead(RSRP:PRSR; Request:THTTPRequest; Item:Storage.Social.Sync.Queue.TItem);
begin
  ReadFile(RSRP^,Request,Item.FileP^);
  Item.Status:=Storage.Social.Sync.Queue.Status.Operation.soRequested;
end;

procedure   TAuSocketMan.FileQueueItemWrite(RSRP:PRSR; Request:THTTPRequest; Item:Storage.Social.Sync.Queue.TItem);
var
  fsData:TFileStream;
begin
  fsData:=TFileStream.Create(Item.LocalizedFile,fmOpenRead or fmShareDenyNone);
  Try
    WriteFile(RSRP^,Request,Item.FileP^,fsData);
  finally
    fsData.Free();
  end;
  Item.Status:=Storage.Social.Sync.Queue.Status.Operation.soRequested;
end;

{$i uEngine.ReadFile.Social.inc}
{$i uEngine.WriteFile.Social.inc}

{$i uEngine.Methods.inc}

{$i uEngine.Synchronize.Callbacks.inc}

{$i uEngine.GetFileProfile.inc}

{$i uEngine.SyncFileUpdated.Social.inc}

{$i uEngine.ParseXML.inc}
{$i uEngine.OnDataReceived.inc}

{$i uEngine.cmWriteResource.Code.inc}

{$i uEngine.cmWriteSyncsSocial.Code.inc}
{$i uEngine.cmProcessSyncItems.Code.inc}
{$i uEngine.cmReadAccount.Code.inc}
{$i uEngine.cmReadSocialSync.Code.inc}
{$i uEngine.cmCreateResource.Code.inc}
{$i uEngine.cmListResources.Code.inc}
{$i uEngine.cmDeleteResource.Code.inc}
{$i uEngine.cmCreateFolderSocial.Code.inc}
{$i uEngine.cmListFoldersSocial.Code.inc}
{$i uEngine.cmListAllSocialFiles.Code.inc}
{$i uEngine.cmProcessListFoldersSocial.Code.inc}
{$i uEngine.cmListNetworks.Code.inc}
{$i uEngine.cmListConnections.Code.inc}
{$i uEngine.cmProcessAllFilesSocial.Code.inc}
{$i uEngine.cmSyncReadSocial.Code.inc}
{$i uEngine.CreateFile.Social.inc}

{$i uEngine.ScanThread.Code.inc}

{$i uEngine.SetResource.inc}

end.


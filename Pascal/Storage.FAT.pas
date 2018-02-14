unit Storage.FAT;


interface

{
  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

  Storage.FAT.pas is the place where all domain related files and folders are
  materialized.  It is where we virtualize files / folder storage for domains

  Domains can store Files and Folders

  Based on Two Tables

  Storage Folder Table  : Stores Directory Structure
  Storage File Table    : Stores Items with Ptrs back to Folders.

  Requires Access Control Unit


  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}


uses
  Classes, SysUtils,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.Boolean,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.Strings,
  Core.Generics,
  Core.Keywords,
  Core.Streams,
  Core.XML,
  Core.Timer,
  Core.Logging,
  Core.Utils.Files,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Timer,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  App,
  App.Consts,

  Encryption.Zip,

  Storage.Main,
  Storage.MatrixNodes,

  IniFiles,
  MD5,
  XMLRead,
  DOM;
Const

  DS_ANY_DOMAIN                   = 0;

  TOL_MODIFIED_MS                 = 100;

  NO_DEFLATE                      = false;
  NO_CACHE                        = false;
  YES_DEFLATE                     = true;
  YES_CACHE                       = true;

  RESCAN_WAIT                     = 40;
  FS_ATTR_NONE                    = 0;
  FS_ATTR_SYSTEM                  = 1 shl 0;                               // System File/Folder
  FS_ATTR_RESERVED                = 1 shl 1;                               // <<-- Use This first
  FS_ATTR_READONLY                = 1 shl 2;                               // Read Only File/Folder (no renaming)
  FS_ATTR_COREOBJECT              = 1 shl 3;                               // Core Object Flag

  FS_ATTR_REQUIRED_FOLDER         = FS_ATTR_SYSTEM or FS_ATTR_READONLY;

  FAT_RECURSE_OFF                 = false;
  FAT_RECURSE_ON                  = true;

  FAT_KEYWORDS_ON                 = true;
  FAT_KEYWORDS_OFF                = false;

  STORAGE_KIND_FOLDER             = 1 shl 0;
  STORAGE_KIND_FILE               = 1 shl 1;

  RFI_CORE                        = 0;
  RFI_FTP                         = 1;
  RFI_WEB                         = 2;
  RFI_COUNT                       = 3;

  RequiredFolders                 : Array[0..RFI_COUNT-1] of Core.Strings.VarString=('core','ftp','http');


const
  INI_SEC_TEMPLATE:Core.Strings.VarString='Template';
  INI_SEC_META:Core.Strings.VarString='Meta Tags';
  INI_SEC_SCRIPTS:Core.Strings.VarString='JavaScripts';
  INI_SEC_STYLES:Core.Strings.VarString='StyleSheets';
  INI_SEC_LINKS:Core.Strings.VarString='Links';
  INI_SEC_SEO_FOLLOW:Core.Strings.VarString='SEO Follow';
  INI_SEC_WRAPPER:Core.Strings.VarString='Component Wrapper';

  INI_NAME:Core.Strings.VarString='template.inf';
  DEFAULT_FILE:Core.Strings.VarString='index.html';

  INI_VAL_OUTPUT:Core.Strings.VarString='Output';
  INI_VAL_TITLE:Core.Strings.VarString='Title';
  INI_VAL_CACHE_MANIFEST : Core.Strings.VarString ='Cache Manifest';
  INI_VAL_DESCRIPTION:Core.Strings.VarString='Description';
  INI_VAL_KEYWORDS:Core.Strings.VarString='Keywords';
  INI_VAL_COMPONENTS:Core.Strings.VarString='Components';
  INI_VAL_NAME:Core.Strings.VarString='Name';
  INI_VAL_ENABLED:Core.Strings.VarString='Enabled';
  INI_VAL_CONTENT:Core.Strings.VarString='Content';
  INI_VAL_CACHE:Core.Strings.VarString='Cache';
  INI_VAL_TTL:Core.Strings.VarString='TTL';
  INI_VAL_Kind:Core.Strings.VarString='Kind';
  INI_VAL_DOCTYPE:Core.Strings.VarString='DocType';
  INI_VAL_CLASS:Core.Strings.VarString='Class';
  INI_VAL_CODE:Core.Strings.VarString='Code';
  INI_VAL_COUNT:Core.Strings.VarString='Count';
  INI_VAL_COMPRESS:Core.Strings.VarString='Compress';
  INI_VAL_STYLE:Core.Strings.VarString='Style';
  INI_VAL_ID:Core.Strings.VarString='ID';
  INI_VAL_EMBEDDED:Core.Strings.VarString='Embedded';
  INI_VAL_ONLOAD:Core.Strings.VarString='OnLoad';
  INI_VAL_ONRESIZE:Core.Strings.VarString='OnResize';

  //INI_VAL_HTTP_EQUIV='http-equiv';
  INI_VAL_CHARSET='Charset';
  INI_VAL_VIEWPORT:Core.Strings.VarString='ViewPort';
  INI_VAL_APPLE_WEB_APP:Core.Strings.VarString='AppleWebApp';
  INI_VAL_APPLE_WEB_STATUSBAR:Core.Strings.VarString='AppleWebStatusBar';
  INI_VAL_APPLE_TOUCH_FULLSCREEN:Core.Strings.VarString='AppleTouchFullScreen';
  //HTML PUBLIC "-//IETF//DTD HTML 5.0 Strict//EN
  FMT_SEO_FOLLOW:Core.Strings.VarString='<link rel="search" title="%0:s" href="%1:s" />';
  FMT_LINK:Core.Strings.VarString='<link %0:s/>';
  FMT_PAGE:Core.Strings.VarString='<!DOCTYPE %17:s><html%16:s><head>'#13#10+
    '<title>%5:s</title>'#13#10+
    '<meta name="viewport" content="%7:s" >'#13#10+
    '<meta name="apple-mobile-web-app-capable" content="%9:s" >'#13#10+
    '<meta name="apple-mobile-web-app-status-bar-style" content="%10:s" >'#13#10+
    '<meta name="apple-touch-fullscreen" content="%11:s" >'#13#10+
    '<meta name="description" content="%1:s" >'#13#10+
    '<meta name="keywords" content="%2:s" >'#13#10+
    '<meta charset="%18:s" >'#13#10+
    '<style type="text/css">'#13#10+'%3:s'#13#10'</style>'#13#10+
    '%20:s'+
    '%14:s'+
    '%15:s'+
    '%19:s'+
    '<script type="text/javascript">'#13#10'%4:s'#13#10'</script>'#13#10+
    '</head><%8:s>'#13#10+
    '%12:s<div id="%0:s_Template" class="TemplateComponents">%6:s</div>%13:s'#13#10+
    '</body></html>';

  FMT_WRAPPER_POST:Core.Strings.VarString='</div>';
  FMT_JAVASCRIPT:Core.Strings.VarString='<script src="%s" type="text/javascript"></script>'#13#10;
  FMT_JAVASCRIPT_EMBEDDED_START:Core.Strings.VarString=#13#10'<script type="text/javascript">'#13#10;
  FMT_JAVASCRIPT_EMBEDDED_START_LEN=35;
  FMT_JAVASCRIPT_EMBEDDED_END:Core.Strings.VarString=#13#10'</script>'#13#10;
  FMT_JAVASCRIPT_EMBEDDED_END_LEN=11;
  FMT_STYLESHEET:Core.Strings.VarString='<link rel="stylesheet" href="%s" type="text/css">'#13#10;
  FMT_WRAPPER_PRE_CLASS:Array[boolean] of Core.Strings.VarString = ('<div class="%0:s">','<div class="%0:s" id="%1:s">');

Type
  TPageFormat=(
    {0}  pfName,
    {1}  pfMetaDescription,
    {2}  pfMetaKeywords,
    {3}  pfStyles,
    {4}  pfScripts,
    {5}  pfTitle,
    {6}  pfComponents,
    {7}  pfMetaViewport,
    {8}  pfBodyDec,
    {9}  pfMetaAppleWebApp,
    {10} pfMetaAppleStatusBar,
    {11} pfMetaAppleFullscreen,
    {12} pfWrapperPre,
    {13} pfWrapperPost,
    {14} pfExternalStyles,
    {15} pfExternalScripts,
    {16} pfCacheManifest,
    {17} pfDocType,
    {18} pfCharset,
    {19} pfLinks,
    {20} pfSEOFollow
  );
  TDSFolder=Class;
  TDSFile=Class;
  TDSFolders=Class;
  TDSFiles=Class;
  TDSFAT=Class;
  TDSFileList=Array of TDSFile;

  TTemplate=class;

  TTemplateKind=(tkPage,tkJavaScript,tkCombine,tkComponents); // Page is entire page, Components is set of components put together inside a DIV content
  TTemplateResourceKind=(trkNone,trkCombination,trkJavaScript,trkCSS);
  PTemplateComponent=^TTemplateComponent;
  TTemplateResource=record
    Kind     : TTemplateResourceKind;
    Name     : Core.Strings.VarString;
    Storage  : TDSFile;
    Modified : double;
  end;
  TTemplateComponent=record
    Valid       : boolean;
    Stale       : boolean;
    Name        : Core.Strings.VarString;
    Content     : TTemplateResource;
    Code        : TTemplateResource;
    Style       : TTemplateResource;
    Combination : TTemplateResource;
  end;
  GTemplateList=specialize GObjectList<TTemplate>;

  TTemplateList=class(GTemplateList)
  public
    procedure Add(Template:TTemplate);
    procedure Invalidate;
  end;
  TDSJavsScriptFiles = Array of TDSFile;
  GComponentList=specialize GStructList<TTemplateComponent>;
  TComponentList=class(GComponentList)
  public
    procedure Empty(var Item:TTemplateComponent); override; overload;
    procedure Empty(var Item:TTemplateResource); overload;
    procedure Done(var Item:TTemplateComponent); override; overload;
    procedure Done(var Item:TTemplateResource); overload;
    procedure Init(var Item:TTemplateComponent); override; overload;
    procedure Init(var Item:TTemplateResource); overload;
  public
    function  Find(Name:Core.Strings.VarString; out Item:PTemplateComponent):boolean;
    procedure Clear; override;
    procedure Invalidate;
    procedure Assign(Source:TComponentList);
    procedure Purge;
  private
    function BuildStyles(Folder:TDSFolder; Refactor:TMemoryStream):Core.Strings.VarString;
    function BuildContent(Folder:TDSFolder; Refactor:TMemoryStream):Core.Strings.VarString;
    function BuildJavaScript(Folder:TDSFolder; Refactor:TMemoryStream):Core.Strings.VarString;
  public
    destructor Destroy; override;
  end;

  TTemplate=class
  private
    FModified        : double;
    FKWFModified     : double;
    FScriptCount     : LongInt;
    FScriptsEmbedded : boolean;
    FStale           : boolean;

    FKind            : TTemplateKind;

    FStyleCount      : LongInt;
    FLinkCount       : LongInt;

    FManifest        : TIniFile;
    FTemplate        : TDSFile;
    FOutput          : TDSFile;
    FLines           : TStringList;

    FEmbeddedScripts : TDSJavsScriptFiles;

    // Wrapper Components
    FWrapped         : boolean;
    FWrapperClass    : Core.Strings.VarString;
    FWrapperID       : Core.Strings.VarString;
    // ------Meta Data------
    FSEOFollow       : Core.Strings.VarString;
    FLinks           : Core.Strings.VarString;
    FTitle           : Core.Strings.VarString;
    FCacheManifest   : Core.Strings.VarString;
    FName            : Core.Strings.VarString;
    FDescription     : Core.Strings.VarString;
    FKeywords        : Core.Strings.VarString;
    FOutputFileName  : Core.Strings.VarString;
    // ------Meta Data------
    FLock            : TRTLCriticalSection;
    FComponents      : TComponentList;
    FRefactor        : TMemoryStream;
    // Includes
    FJavaScripts     : Core.Arrays.Types.VarString;
    FStyleSheets     : Core.Arrays.Types.VarString;
  private
    function  BuildJavaScripts(Refactor:TMemoryStream):Core.Strings.VarString; overload;
    procedure BuildJavaScripts(Dest:TMemoryStream; Refactor:TMemoryStream); overload;
    function  BuildStyleSheets(Refactor:TMemoryStream):Core.Strings.VarString;
    function  GetKind(sValue:Core.Strings.VarString):TTemplateKind;
    function  GetResourceKind(sValue:Core.Strings.VarString):TTemplateResourceKind;
    procedure Reset();
  public
    constructor Create(aModified:double); reIntroduce;
    destructor  Destroy; override;
  public
    property Modified:double read FModified;
    property Stale:Boolean read FStale;
    property Components:TComponentList read FComponents;
  public
    procedure Invalidate;
    procedure Load(Task:Core.Database.Types.TTask; Folder:TDSFolder; Item:TDSFile); overload;
  end;

  TDSStatusEvent=procedure(pgValue,pgMax:QWORD; Status:Core.Strings.VarString) of object;
  TDSFindOption=(dsFindByName,dsFindByPath);
  TDSFATLoadOption=(dsfatloAll,dsfatloFolders,dsfatloBoth);

  TDSFileSaveOption=(dsfileSaveAll,dsFileSaveInfo,dsFileSaveKeywords,dsFileSaveCache,dsFileSaveDeflate,dsFileSaveAttribs,dsFileSaveDigest);

  TFATCallbackEvent=function : boolean of object;


  PDSFATRefactor=^TDSFATRefactor;
  TDSFATRefactor=record
    FAT                          : TDSFAT;
    Refactor                     : TStream;
    FreeRefactor                 : Boolean;
  end;

  TFATScanValue=class
  private
    FLock                        : TRTLCriticalSection;
    FList                        : TDSFileList;
    FIndex                       : Cardinal;
    FCount                       : LongInt;
  public
    procedure   Reset;
    function    GetNextFile(Var Flush:Boolean):TDSFile;
    procedure   Add(Item:TDSFile);
    procedure   Remove(Item:TDSFile);
    function    IndexOf(Item:TDSFile): LongInt;
  public
    constructor Create;
    destructor  Destroy; override;
  end;


  PFileDetails=^TFileDetails;
  TFileDetails=Record
    ID                          : QWord;
    ACL                         : QWord;
    Size                        : QWord;
    Imported                    : LongInt;
    Created                     : Double;
    Modified                    : Double;
    ContentType                 : LongInt;
    CacheTTL                    : LongInt;
    HasKeywords                 : Boolean;
    Cache                       : Boolean;
    Deflate                     : Boolean;
    Digest                      : TMD5Digest;
    Name                        : Core.Strings.VarString;
  End;

  TDSFile=Class
  private
    FScanP                      : PKWSFile;
    FOwner                      : TDSFiles;
    FParent                     : TDSFolder;
    FMemory                     : TMemoryStream;
    FHasKeywords                : Boolean;
    FID                         : QWord;
    FFolderID                   : QWord;
    FTemplates                  : TTemplateList;
    FVolatile                   : Boolean;
  private
    function  GetReadOnly:Boolean;
    function  GetStale:Boolean;
    procedure Archive(Manifest:TStream; ZipFile:TZipper);
    procedure SetHasKeywords(Value:Boolean);
  public
    Attributes                  : QWord;
    Size                        : QWord;
    ContentType                 : LongInt;
    Imported                    : LongInt;
    Cache                       : Boolean;
    Deflate                     : Boolean;
    Refresh                     : Boolean;
    Downloaded                  : Double;
    Validated                   : Boolean;
    Created                     : Double;
    Modified                    : Double;
    Lock                        : TRTLCriticalSection;
    CacheTTL                    : LongInt;
    Digest                      : TMD5Digest;
    Name                        : Core.Strings.VarString;
  public
    constructor Create(aParent:TDSFolder; aID,aAttributes:QWord; aImported,aContentType:LongInt; aHasKeywords,aCache:Boolean; aCreated,aModified:Double; aName:Core.Strings.VarString); virtual;
    destructor Destroy; override;
  public
    function  toXML():string;
    function  fromXML(xDoc:TXMLDocument):boolean;
    procedure Export(Path:Core.Strings.VarString);
    procedure Assign(Item:TDSFile);
    function  AcquireData:TFileStream;
    procedure Save(Task:Core.Database.Types.TTask; const Option:TDSFileSaveOption);
    function  toString(Task:Core.Database.Types.TTask):Core.Strings.VarString; reIntroduce;
    procedure toStream(Output:TStream);
    procedure BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
    procedure Search(Results:TList; Term:Core.Strings.VarString);
    procedure Load(Task:Core.Database.Types.TTask);
    procedure Delete();
    procedure Empty();
    procedure AddToTemplates(Template:TTemplate);
  public
    property Parent:TDSFolder read FParent;
    property ID:QWord read FID;
    property FolderID:QWord read FFolderID;
    property ReadOnly:Boolean read GetReadOnly;
    property HasKeywords:Boolean read FHasKeywords write SetHasKeywords;
    property Volatile:boolean read FVolatile;
    property Stale:boolean read GetStale;
  end;

  TDSFiles=Class(TThreadList)
  private
    FParent                     : TDSFolder;
    FFAT                        : TDSFAT;
  private
    function  Get(Index:LongInt):TDSFile; reIntroduce;
    procedure Put(Index:LongInt; Value:TDSFile); reIntroduce;
    procedure Archive(Manifest:TStream; ZipFile:TZipper);
  public
    constructor Create(aParent:TDSFolder; aFAT:TDSFAT); virtual;
    destructor  Destroy; override;
  public
    function  First:TDSFile; reIntroduce;
    function  Last:TDSFile; reIntroduce;
    procedure Clear;
    procedure DeleteAll;
  public
    procedure Sort();
    procedure toXML(Output:TStream; Stamp:Boolean);
    procedure Search(Results:TList; Term:Core.Strings.VarString);
    procedure BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
    procedure Export(Path:Core.Strings.VarString);
    function  Find(var Item:TDSFile; Name:Core.Strings.VarString):boolean; overload;
    function  Find(var Item:TDSFile; ID:QWord):boolean; overload;
    function  Add(Item:TDSFile): LongInt; overload;
    function  Add(aID,aAttributes:QWord; HasKeywords:Boolean; const aName:Core.Strings.VarString=''): LongInt; overload;
    function  New(aAttributes:QWord; aHasKeywords,aDeflate,aCache:Boolean; aName,aContent:Core.Strings.VarString):TDSFile; overload;
    function  New(Task:Core.Database.Types.TTask; aAttributes:QWord; aHasKeywords,aDeflate,aCache:Boolean; aName,aContent:Core.Strings.VarString):TDSFile; overload;
    procedure Remove(Item:TDSFile); overload;
    procedure Remove(aID:QWord); overload;
    procedure Invalidate();
    function  IndexOf(aID:QWord): LongInt; overload;
    function  IndexOf(Name:Core.Strings.VarString): LongInt; overload;
    procedure Load(Task:Core.Database.Types.TTask);
  public
    property Items[Index: Integer]: TDSFile read Get write Put;
  end;
  TTemplatetizeEvent=function(Folder:TDSFolder; Item:TDSFile):TTemplate of Object;
  TDSFolder=Class
  private
    FOwner                      : TDSFolders;
    FFolders                    : TDSFolders;
    FFiles                      : TDSFiles;
    FName                       : Core.Strings.VarString;
    FPath                       : Core.Strings.VarString;
    FID                         : QWord;
    FAttributes                 : Byte;
    FLoading                    : Boolean;
    FTemplate                   : TTemplate;
  private
    procedure Archive(Manifest:TStream; ZipFile:TZipper);
  public
    constructor Create(aOwner:TDSFolders; aID:QWord; aPath:Core.Strings.VarString; aAttributes:byte); virtual;
    destructor  Destroy; override;
  public
    procedure Rename(Var sName:Core.Strings.VarString);
    function  Find(ID:QWord; Out Item:TDSFile):Boolean;
    function  FileCount(Recursive:Boolean):QWORD;
    procedure PathChanged(Parent:TDSFolder);
    procedure Load(Task:Core.Database.Types.TTask; const Recursive:boolean);
    procedure BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
    procedure Search(Results:TList; Term:Core.Strings.VarString);
    procedure Export(Path:Core.Strings.VarString; const IncludeName:Boolean=true);
    procedure SetAttributes(Value:Byte);
    procedure Delete;
    procedure Flush();
    procedure Templatetize(var FileName:Core.Strings.VarString; Callback:TTemplatetizeEvent);
  public
    property Folders:TDSFolders read FFolders;
    property Files:TDSFiles read FFiles;
    property Owner:TDSFolders read FOwner;
    property Path:Core.Strings.VarString read FPath;
    property Name:Core.Strings.VarString read FName;
    property Attributes:Byte read FAttributes write SetAttributes;
    property ID:QWord read FID;
    property Template:TTemplate read FTemplate;
    property Loading:boolean read FLoading write FLoading;
  end;

  TDSFolders=Class(TThreadList)
  private
    FParent                     : TDSFolders;
    FFAT                        : TDSFAT;
  private
    function  Get(Index:LongInt):TDSFolder; reIntroduce;
    procedure Put(Index:LongInt; Value:TDSFolder); reIntroduce;
    procedure Archive(Manifest:TStream; ZipFile:TZipper);
  public
    function Last: TDSFolder; reIntroduce;
    function First: TDSFolder; reIntroduce;
  public
    procedure Clear;
    function  FileCount(Recursive:Boolean):QWORD;
    function  IndexOf(aID:QWord): LongInt; reIntroduce;
    function  Add(Item: TDSFolder): LongInt; overload;
    procedure PathChanged(Parent:TDSFolder);
    function  Add(aID:QWord; aPath:Core.Strings.VarString; aAttributes:byte): LongInt; overload;
    procedure Remove(Item:TDSFolder); overload;
    procedure Remove(aID:QWord); overload;
    procedure BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
    procedure Search(Results:TList; Term:Core.Strings.VarString); virtual;
    procedure Templatetize(var FileName:Core.Strings.VarString; Callback:TTemplatetizeEvent);
    procedure Export(Path:Core.Strings.VarString);
    function  Find(ID:QWord; Out Item:TDSFile):Boolean; overload;
    function  Find(var Folder:TDSFolder; Const FolderID:QWord):boolean; overload;
    function  Find(var Folder:TDSFolder; Value:Core.Strings.VarString; Option:TDSFindOption):boolean; overload;
    function  New(aPath:Core.Strings.VarString; aAttributes:byte):TDSFolder;
    procedure Load(Task:Core.Database.Types.TTask; Recursive:Boolean);
    procedure DeleteAll;
    procedure Flush();
  public
    property Items[Index: Integer]: TDSFolder read Get write Put;
    property FAT:TDSFat read FFAT;
  public
    constructor Create(aParent:TDSFolders; aFAT:TDSFat); reIntroduce;
    destructor  Destroy; override;
  end;

  TDSFAT=Class
  private
    FProgressValue              : QWORD;
    FProgressMax                : QWORD;
    FScanInfo                   : TFATScanValue;
    FDomainID                   : QWord;
    FNodeID                     : QWord;
    FResourceID                 : QWord;
    FClusterID                  : QWord;
    FLoading                    : Boolean;
    FFolders                    : TDSFolders;
    FTask                       : Core.Database.Types.TTask;
    FKWEngine                   : TKWSEngine;
    FDomain                     : Core.Strings.VarString;
    FOnStatus                   : TDSStatusEvent;
    FFATTimerItem               : Core.Timer.Item;
    FFatDwell                   : Double;
  private
    procedure   SetDomainID(Value:QWord);
    function    OnGetNextFile(CommandP:PKWSECommand; var FileP:PKWSFile; var Resource:Core.Strings.VarString; var Length:Int64):Boolean;
    procedure   OnScanComplete(CommandP:PKWSECommand);
    function    OnCBKeywordDefault(ItemP:PKeyword):Core.Strings.VarString;
    procedure   OnFATTimerItem(ItemP:Core.Timer.PItem);
  public
    constructor Create(); reIntroduce;
    destructor  Destroy; Override;
  public
    procedure   Load(aDomainID,aClusterID,aResourceID,aNodeID:QWord; const Options:TDSFATLoadOption);
    function    Refresh(Task:Core.Database.Types.TTask; out Refreshed:QWord):boolean; overload;
    procedure   Refresh(Delay:Word); overload;
    procedure   Import(Start:TDSFolder; Path:Core.Strings.VarString);
    procedure   Export(Start:TDSFolder; Path:Core.Strings.VarString);
    function    FileCount(Recursive:Boolean):QWORD;
    Function    Find(var Folder:TDSFolder; Path:Core.Strings.VarString; Refactor:TStream):boolean; overload;
    procedure   Clear; virtual;
    procedure   BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
    procedure   Search(Results:TList; Term:Core.Strings.VarString; Level:LongInt; Refactor:TMemoryStream); overload;
    procedure   Search(var Results:TList; Term:Core.Strings.VarString);overload;
    procedure   Scan(Task:Core.Database.Types.TTask; KeywordsP:PKeywords);
    procedure   Flush;

  public
    function    Acquire(var Path:Core.Strings.VarString; Refactor:TStream):TDSFolder; overload;
    function    Acquire(URI:Core.Strings.VarString; Refactor:TStream; Var dsFile:TDSFile):boolean; overload;
    function    Acquire(Folder:TDSFolder; FileName:Core.Strings.VarString):TDSFile; overload;
    function    Force(Parent:TDSFolders; var Folder:TDSFolder; FolderID:QWord; Path:Core.Strings.VarString; Attributes:Byte; Refactor:TStream):boolean; overload;
    function    Force(Parent:TDSFolders; var Folder:TDSFolder; Path:Core.Strings.VarString; Attributes:Byte):boolean; overload;
    procedure   Templatetize(FileName:Core.Strings.VarString; Callback:TTemplatetizeEvent);
  public
    procedure   Backup(FileName:Core.Strings.VarString);
    procedure   Restore(FileName:Core.Strings.VarString);
  public
    property Folders:TDSFolders read FFolders;
    property DomainID:QWord read FDomainID write SetDomainID;
    property Domain:Core.Strings.VarString read FDomain;
    property Task:Core.Database.Types.TTask read FTask;
  public
    AuraDiskNode                 : Storage.MatrixNodes.Node.Item;
  public
    property OnStatus:TDSStatusEvent read FOnStatus write FOnStatus;
  end;
  Folders=class
  type
    XML=class
    type
      Stanza=class
      const
        Items                    : Core.Strings.VarString = 'fdrs';
        Item                     : Core.Strings.VarString = 'fdr';
      end;
      Fields=class
      const
        ID                       : Core.Strings.VarString = 'id';
        Path                     : Core.Strings.VarString = 'pth';
        Attributes               : Core.Strings.VarString = 'atr';
      end;
    end;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        Attributes               : Core.Database.Types.Integer = 3;
        Path                     : Core.Database.Types.Integer = 4;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        Attributes               : Core.Database.Types.VarString = 'ATTR';
        Path                     : Core.Database.Types.VarString = 'VPTH';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Storage';
        Name                     : 'Folders';
        Value                    : 'scs_dfldrs';
        Hint                     : 'Storage of domain folder structures';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..4] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Attributes; KeyP: @Keys.Attributes; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Path; KeyP: @Keys.Path; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNotNull;  )
      );
      class Function  SetAttributes(Task:Core.Database.Types.TTask; FolderID:QWord; Attributes:Byte):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,ID:QWord):Boolean;
      class Function  Verify(Task:Core.Database.Types.TTask; DomainID:QWord; Var Path:Core.Strings.VarString; Var ID:QWord):Boolean; overload;
      class Function  List(Task:Core.Database.Types.TTask; FAT:TDSFAT):Boolean;
      class Function  Exists(Task:Core.Database.Types.TTask; DomainID:QWord; Var Path:Core.Strings.VarString):Boolean;
      class Function  Create(Task:Core.Database.Types.TTask; Folders:TDSFolders; DomainID:QWord; Path:Core.Strings.VarString; Var FolderID:QWord; Attributes:Byte):Boolean;
      class Function  Rename(Task:Core.Database.Types.TTask; ID:QWord; Var Path:Core.Strings.VarString):Boolean; overload;
    end;
    class procedure toXML(List:TList; Output:TStream; Stamp:Boolean);
  end;
  Files=class
  type
     Extensions=class
     type
       Media=class
       const
         BMP                     : Core.Strings.VarString = 'bmp';
         GIF                     : Core.Strings.VarString = 'gif';
         PNG                     : Core.Strings.VarString = 'png';
         JPG                     : Core.Strings.VarString = 'jpg';
         MPG                     : Core.Strings.VarString = 'mpg';
         MPEG                    : Core.Strings.VarString = 'mpeg';
         MOV                     : Core.Strings.VarString = 'mov';
         AVI                     : Core.Strings.VarString = 'avi';
       end;
       Compressed=class
       const
         ZIP                     : Core.Strings.VarString = 'zip';
         EXE                     : Core.Strings.VarString = 'exe';
         BIN                     : Core.Strings.VarString = 'bin';
         JAR                     : Core.Strings.VarString = 'jar';
         GZ                      : Core.Strings.VarString = 'gz';
         GZIP                    : Core.Strings.VarString = 'gzip';
         cab                     : Core.Strings.VarString = 'cab';
       end;
    end;
    XML=class
    type
      Stanza=class
      const
        Items                    : Core.Strings.VarString = 'fls';
        Item                     : Core.Strings.VarString = 'fl';
      end;
      Fields=class
      const
        ID                       : Core.Strings.VarString = 'id';
        FolderID                 : Core.Strings.VarString = 'fid';
        Path                     : Core.Strings.VarString = 'pth';
        Name                     : Core.Strings.VarString = 'n';
        Digest                   : Core.Strings.VarString = 'd';
        Created                  : Core.Strings.VarString = 'ctd';
        Modified                 : Core.Strings.VarString = 'mtd';
        Attributes               : Core.Strings.VarString = 'atr';
        Size                     : Core.Strings.VarString = 'z';
        Compress                 : Core.Strings.VarString = 'cmp';
        Cache                    : Core.Strings.VarString = 'cac';
        CacheTTL                 : Core.Strings.VarString = 'ttl';
        Keywords                 : Core.Strings.VarString = 'kwds';
      end;
    end;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        PathID                   : Core.Database.Types.Integer = 3;
        Name                     : Core.Database.Types.Integer = 4;
        Size                     : Core.Database.Types.Integer = 5;
        Attributes               : Core.Database.Types.Integer = 6;
        ContentType              : Core.Database.Types.Integer = 7;
        HasKeywords              : Core.Database.Types.Integer = 8;
        Cache                    : Core.Database.Types.Integer = 9;
        CacheTTL                 : Core.Database.Types.Integer = 10;
        Digest                   : Core.Database.Types.Integer = 11;
        Deflate                  : Core.Database.Types.Integer = 12;
        Created                  : Core.Database.Types.Integer = 13;
        Modified                 : Core.Database.Types.Integer = 14;
        Imported                 : Core.Database.Types.Integer = 15;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        PathID                   : Core.Database.Types.VarString = 'PTHID';
        Name                     : Core.Database.Types.VarString = 'NAME';
        Size                     : Core.Database.Types.VarString = 'SZE';
        Attributes               : Core.Database.Types.VarString = 'ATTR';
        ContentType              : Core.Database.Types.VarString = 'CTYPE';
        HasKeywords              : Core.Database.Types.VarString = 'KYWDP';
        Cache                    : Core.Database.Types.VarString = 'CACHE';
        CacheTTL                 : Core.Database.Types.VarString = 'CATTL';
        Digest                   : Core.Database.Types.VarString = 'IDGT';
        Deflate                  : Core.Database.Types.VarString = 'DEFLT';
        Created                  : Core.Database.Types.VarString = 'CDT';
        Modified                 : Core.Database.Types.VarString = 'MDT';
        Imported                 : Core.Database.Types.VarString = 'IMPA';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Storage';
        Name                     : 'Files';
        Value                    : 'scs_dfls';
        Hint                     : 'Storage of domain files';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..15] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.PathID; KeyP: @Keys.PathID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNotNull; ),
        (IDP: @IDs.Size; KeyP: @Keys.Size; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Attributes; KeyP: @Keys.Attributes; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ContentType; KeyP: @Keys.ContentType; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.HasKeywords; KeyP: @Keys.HasKeywords; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Cache; KeyP: @Keys.Cache; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.CacheTTL; KeyP: @Keys.CacheTTL; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Digest; KeyP: @Keys.Digest; DataType: dftMD5Digest; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Deflate; KeyP: @Keys.Deflate; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Imported; KeyP: @Keys.Imported; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
      class Function  Read(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Item:TDSFile):Boolean; overload;
      class Function  Read(var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; Var Data:Core.Arrays.Types.Bytes):Boolean; overload;
      class Function  Read(var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; var Stream:TFileStream):Boolean; overload;
      class Function  Read(Task:Core.Database.Types.TTask; Var Details:TFileDetails):Boolean; overload;
      class Function  Write(Task:Core.Database.Types.TTask; Var Details:TFileDetails):Boolean; overload;
      class Function  Write(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; Var Data:Core.Arrays.Types.Bytes):Boolean; overload;

      class Function  Write(dsFile:TDSFile; const Option:TDSFileSaveOption):Boolean; overload;
      class Function  Write(Task:Core.Database.Types.TTask; dsFile:TDSFile; const Option:TDSFileSaveOption):Boolean; overload;

      class Function  List(Task:Core.Database.Types.TTask; var FolderID:QWord; var Entries:Core.Arrays.Types.LargeWord):Boolean; overload;


      class Function  Basic(Task:Core.Database.Types.TTask; FAT:TDSFAT):Boolean;
      class Function  Refresh(Task:Core.Database.Types.TTask; FAT:TDSFAT; out Refreshed:QWord; var ReScan:boolean):Boolean;
      class Function  Exists(Task:Core.Database.Types.TTask; FolderID:QWord; Var Path:Core.Strings.VarString):Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord):Boolean;

      class Function  Edit(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; Var Digest:TMD5Digest; Var Data:Core.Arrays.Types.Bytes):Boolean; overload;
      class Function  Rename(Task:Core.Database.Types.TTask; ID:QWord; Var Name:Core.Strings.VarString):Boolean; overload;
      class Function  Create(Task:Core.Database.Types.TTask; Folder:TDSFolder; DomainID:QWord; Var FileID:QWord; iFileAge:LongInt; Attributes:QWord; HasKeywords,Deflate,Cache:Boolean; TTL:LongInt; FileName:Core.Strings.VarString; Var Data:Core.Arrays.Types.Bytes; var Digest:TMD5Digest; out FileIndex:LongInt):Boolean; overload;
      class Function  PrepareCopyFileName(Task:Core.Database.Types.TTask; FolderID:QWord; Var Name:Core.Strings.VarString):Boolean;
      class Function  Verify(Task:Core.Database.Types.TTask; DomainID:QWord; Var Path:Core.Strings.VarString; Var FolderID,FileID:QWord):Boolean; overload;
    end;
    class procedure toXML(List:TList; Output:TStream; Stamp:Boolean); overload;
    class procedure toXML(Item:TDSFile; Output:TStream; Stamp:Boolean); overload;
  end;

  procedure Empty(Var Item:TFileDetails); overload;

  procedure Init(var Item:TDSFATRefactor; FAT:TDSFat; Refactor:TStream); overload;
  procedure Done(Var Item:TDSFATRefactor); overload;

  Function  GetFAT(Task:Core.Database.Types.TTask; FAT:TDSFAT):Boolean;



  procedure StartBackgroundTimers();
  procedure StopBackgroundTimers();



implementation
uses
  DateUtils,
  db,
  sqldb,

  Storage,
  Storage.ContentTypes,
  Storage.Domains,
  Storage.MatrixServices,
  Storage.Keywords,
  Storage.AuraDisks,
  Encryption.Base64,
  StrUtils;
const
  TemplateKind:Array[TTemplateKind] of Core.Strings.VarString = ('page','javascript','combine','components');
  ResourceKind:Array[TTemplateResourceKind] of Core.Strings.VarString = ('none','combination','javascript','css');
Var
  FATTimer                        : Core.Database.Types.TTimer;

Const
  SURPRESS_FIRST_FOLDER:boolean=false;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP=Folders.DB.MonitorP then begin
      Try
        With Folders.DB do begin
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
          // we have to lookup each folder and purge from AuraDisks
        end;
      Finally
        SetLength(Commands,0);
      End;
    end else if ItemP=Files.DB.MonitorP then begin
      Try
        With Files.DB do begin
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
          // we have to lookup each file and purge from AuraDisks
        end;
      Finally
        SetLength(Commands,0);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
  end;
end;

procedure cbDestroyTableFolders(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Folders.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyTableFiles(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Files.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Folders.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTableFolders, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Files.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTableFiles, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

procedure Empty(Var Item:TFileDetails);
begin
  Item.ID:=0;
  Item.Size:=0;
  Item.Created:=0;
  Item.Modified:=0;
  Item.Imported:=0;
  Item.ContentType:=0;
  Item.HasKeywords:=False;
  Item.Cache:=False;
  Item.CacheTTL:=0;
  Item.Deflate:=false;
  SetLength(Item.Name,0);
end;

procedure Init(var Item:TDSFATRefactor; FAT:TDSFat; Refactor:TStream);
begin
  Item.FAT:=FAT;
  Item.Refactor:=Refactor;
  Item.FreeRefactor:=Refactor=nil;
  If Item.FreeRefactor then
    Item.Refactor:=TMemoryStream.Create();
end;

procedure Done(Var Item:TDSFATRefactor);
begin
  If Item.FreeRefactor then
    FreeAndNil(Item.Refactor);
end;

constructor TFATScanValue.Create;
begin
  InitCriticalSection(FLock);
  SetLength(FList,0);
  FIndex:=0;
  FCount:=0;
  Inherited Create;
end;

destructor  TFATScanValue.Destroy;
begin
  DoneCriticalSection(FLock);
  Finalize(FList,0);
  FIndex:=0;
  FCount:=0;
  Inherited Destroy;
end;

procedure   TFATScanValue.Reset;
begin
  SetLength(FList,0);
  FIndex:=0;
  FCount:=0;
end;

function    TFATScanValue.GetNextFile(var Flush:boolean):TDSFile;
begin
  EnterCriticalSection(FLock);
  Try
    Result:=nil;
    While (FIndex<FCount) and (Result=nil) do begin
      if (Flush) or
         (FList[FIndex].FScanP=nil) or
         (FList[FIndex].FScanP^.dtFile<>FList[FIndex].Modified) or
         (FList[FIndex].FScanP^.dtDownloaded<>FList[FIndex].Downloaded)
      then
        Result:=FList[FIndex];
      InterlockedIncrement(FIndex);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure   TFATScanValue.Add(Item:TDSFile);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(Item);
  if iIndex=-1 then begin
    EnterCriticalSection(FLocK);
    Try
      SetLength(FList,FCount+1);
      FList[FCount]:=Item;
      InterlockedIncrement(FCount);
    finally
      LeaveCriticalSection(FLock);
    end;
  end;
end;

procedure   TFATScanValue.Remove(Item:TDSFile);
var
  iLcv:LongInt;
  iLcvNext:LongInt;
begin
  EnterCriticalSection(FLocK);
  Try
    for iLcv:=0 to FCount-1 do begin
      if FList[iLcv]=Item then begin
        for iLcvNext:=iLcv to FCount-2 do
          FList[iLcvNext]:=FList[iLcvNext+1];
        InterlockedExchangeAdd(FCount,-1);
        break;
      end;
    end;
    SetLength(FList,FCount);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function    TFATScanValue.IndexOf(Item:TDSFile): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  EnterCriticalSection(FLocK);
  Try
    for iLcv:=0 to FCount-1 do begin
      if FList[iLcv]=Item then begin
        Result:=iLcv;
        break;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TDSFile.Create(aParent:TDSFolder; aID,aAttributes:QWord; aImported,aContentType:LongInt; aHasKeywords,aCache:Boolean; aCreated,aModified:Double; aName:Core.Strings.VarString);
begin
  Inherited Create;
  FScanP:=nil;
  FParent:=aParent;
  if (FParent<>nil) then begin
    FOwner:=aParent.FFiles;
    FTemplates:=TTemplateList.Create();
    FVolatile:=false;
    FFolderID:=aParent.FID;
  end else begin
    FOwner:=nil;
    FTemplates:=nil;
    FVolatile:=true;
    FFolderID:=0;
  end;
  InitCriticalSection(Lock);
  FMemory:=nil;
  FID:=aID;
  CacheTTL:=0;
  Attributes:=aAttributes;
  Imported:=aImported;
  ContentType:=aContentType;
  HasKeywords:=aHasKeywords;
  Cache:=aCache;
  Created:=aCreated;
  Modified:=aModified;
  Deflate:=false;
  Downloaded:=0;
  Refresh:=False;
  Name:=aName;
  System.FillChar(Digest[0],SizeOf(Digest),0);
end;


destructor TDSFile.Destroy;
var
  skwP:PKWSFile;
  List:TList;
begin
  if (FOwner<>nil) then begin
    List:=FOwner.LockList();
    Try
      List.Remove(Self);
    finally
      FOwner.UnlockList();
    end;
    if (FOwner.FFAT.FScanInfo)<>nil then
      FOwner.FFAT.FScanInfo.Remove(Self);

    FOwner:=nil;
  end;
  if (FScanP<>nil) then begin
    skwP:=FScanP;
    InterLockedExchange(FScanP,nil);
    Core.Keywords.Done(skwP^);
    Dispose(skwP);
  end;
  DoneCriticalSection(Lock);
  FreeAndNil(FTemplates);
  FreeAndNil(FMemory);
  FOwner:=nil;
  FParent:=nil;
  Inherited Destroy;
end;

procedure  TDSFile.Assign(Item:TDSFile);
begin
  Name:=Item.Name;
  Copy(Item.Digest,Digest);
  Cache:=Item.Cache;
  Deflate:=Item.Deflate;
  FHasKeywords:=Item.FHasKeywords;
  CacheTTL:=Item.CacheTTL;
end;

function   TDSFile.GetStale:Boolean;
begin
  Result:=(FScanP<>nil) and (FScanP^.dtFile<>Modified);
end;

function   TDSFile.GetReadOnly:Boolean;
begin
  Result:=((Attributes or FS_ATTR_READONLY) = Attributes);
end;

procedure  TDSFile.Archive(Manifest:TStream; ZipFile:TZipper);
var
  sManifest:Core.Strings.VarString;
  FS:TFileStream;
  FAT:TDSFat;
begin
  FAT:=FParent.FOwner.FFAT;
  FS:=nil;
  Files.DB.Read(FAT.AuraDiskNode,FAT.FDomainID,FParent.FID,FID,FS);
  Try
    sManifest:=Concat(
      '<file>',
        '<id>',IntToStr(FID),'</id>',
        '<aclid>',IntToStr(FID),'</aclid>',
        '<name>',Name,'</name>',
        '<created>',FloatToStrF(Created,ffFixed,8,0),'</created>',
        '<modified>',FloatToStrF(Modified,ffFixed,8,0),'</modified>',
        '<content-type>',IntToStr(ContentType),'</content-type>',
        '<keywords>',YES_NO[HasKeywords],'</keywords>',
        '<cache>',YES_NO[Cache],'</cache>',
        '<deflate>',YES_NO[Deflate],'</deflate>',
      '</file>',#13#10
    );
    FS.Position:=0;
    ZipFile.Entries.AddFileEntry(FS,Join(FParent.Path,Name));
    Core.Streams.Write(sManifest,Manifest);
  finally
    FreeAndNil(FS);
  end;
end;

procedure  TDSFile.SetHasKeywords(Value:Boolean);
begin
  if FHasKeywords<>Value then begin
    FHasKeywords:=Value;
    If (FParent.FFolders.FFAT.FScanInfo<>Nil) then begin
      If FHasKeywords then
        FParent.FFolders.FFAT.FScanInfo.Add(Self)
      else
        FParent.FFolders.FFAT.FScanInfo.Remove(Self);
    end;
  end;
end;

function   TDSFile.toXML():string;
begin
  Result:=Concat(
    '<',Files.XML.Stanza.Item,'>',
    Core.XML.DB.Print(Files.XML.Fields.ID,FID),
    Core.XML.DB.Print(Files.XML.Fields.FolderID,FFolderID),
    Core.XML.DB.Print(Files.XML.Fields.Name,Name),
    Core.XML.DB.Print(Files.XML.Fields.Digest,Digest),
    Core.XML.DB.Print(Files.XML.Fields.Cache,Cache),
    Core.XML.DB.Print(Files.XML.Fields.Compress,Deflate),
    Core.XML.DB.Print(Files.XML.Fields.Keywords,FHasKeywords),
    Core.XML.DB.Print(Files.XML.Fields.Size,Size),
    Core.XML.DB.Print(Files.XML.Fields.Attributes,Attributes),
    Core.XML.DB.Print(Files.XML.Fields.CacheTTL,CacheTTL),
    Core.XML.DB.Print(Files.XML.Fields.Created,Created),
    Core.XML.DB.Print(Files.XML.Fields.Modified,Modified),
    '</',Files.XML.Stanza.Item,'>'
  );
end;

function  TDSFile.fromXML(xDoc:TXMLDocument):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Empty();
  xItem:=Core.XML.DB.getNode(xDoc,Files.XML.Stanza.Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      FID:=toQword(xItem,Files.XML.Fields.ID);
      FFolderID:=toQword(xItem,Files.XML.Fields.FolderID);
      Name:=toString(xItem,Files.XML.Fields.Name);
      toMD5Digest(xItem,Files.XML.Fields.Digest,Digest);
      Cache:=toBoolean(xItem,Files.XML.Fields.Cache);
      Deflate:=toBoolean(xItem,Files.XML.Fields.Compress);
      FHasKeywords:=toBoolean(xItem,Files.XML.Fields.Keywords);
      Size:=toQword(xItem,Files.XML.Fields.Size);
      Attributes:=toQword(xItem,Files.XML.Fields.Attributes);
      CacheTTL:=toInteger(xItem,Files.XML.Fields.CacheTTL);
      Created:=toDouble(xItem,Files.XML.Fields.Created);
      Modified:=toDouble(xItem,Files.XML.Fields.Modified);
      Result:=True;
    end;
  end;
end;


procedure  TDSFile.Export(Path:Core.Strings.VarString);
var
  fsDest:TFileStream;
  fsSource:TFileStream;
  FAT:TDSFat;
begin
  FAT:=FParent.FOwner.FFAT;
  fsSource:=nil; fsDest:=nil;
  Path:=Concat(Path,Name);
  If Assigned(FParent.FOwner.FFat.FOnStatus) then begin
    Inc(FParent.FOwner.FFat.FProgressValue);
    FParent.FOwner.FFat.FOnStatus(FParent.FOwner.FFat.FProgressValue,FParent.FOwner.FFat.FProgressMax,Format(FMT_PROGRESS_USR_STATUS,['Exporting',Append(Path,Name)]));
  end;
  fsDest:=TFileStream.Create(Path,fmCreate);
  Try
    fsSource:=nil;
    Try
      Files.DB.Read(FAT.AuraDiskNode,FAT.FDomainID,FParent.FID,FID,fsSource);
      Core.Streams.Copy(fsSource,fsDest);
    finally
      FreeAndNil(fsSource);
    end;
  Finally
    FreeAndNil(fsDest);
  end;
end;

function   TDSFile.AcquireData:TFileStream;
var
  FS:TFileStream;
  FAT:TDSFat;
begin
  FAT:=FParent.FOwner.FFAT;
  FS:=nil;
  Files.DB.Read(FAT.AuraDiskNode,FAT.FDomainID,FParent.FID,FID,FS);
  Result:=FS;
end;

procedure  TDSFile.Save(Task:Core.Database.Types.TTask; const Option:TDSFileSaveOption);
begin
  Files.DB.Write(Task,Self,Option);
end;

function   TDSFile.toString(Task:Core.Database.Types.TTask):Core.Strings.VarString;
var
  FS:TFileStream;
begin
  If (HasKeywords) and (FScanP<>nil) and (FScanP^.Read^.Count>0) then begin
    Result:=Core.Keywords.toString(FScanP^.Read^)
  end  else begin
    FS:=AcquireData();
    Try
      Result:=Core.Streams.toString(FS);
    finally
      FreeAndNil(FS);
    end;
  end;
end;

procedure  TDSFile.toStream(Output:TStream);
var
  FS:TFileStream;
begin
  If (FHasKeywords=true) and (FScanP<>nil) and (FScanP^.Read^.Count>0) then begin
    Core.Keywords.toStream(FScanP^.Read^,Output)
  end else if (FVolatile=false) then begin
    FS:=AcquireData();
    Try
      Core.Streams.Copy(FS,Output);
    finally
      FreeAndNil(FS);
    end;
  end else begin
    Core.Streams.Copy(FMemory,Output);
  end;
end;

procedure  TDSFile.Search(Results:TList; Term:Core.Strings.VarString);
begin
  If System.Pos(Term,Name)>0 then
    Results.Add(Self);
end;

procedure  TDSFile.BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
begin
  If HasKeywords then begin
    if Downloaded=0 then
      Load(Task);
    Item.Add(Self);

  end;
end;

procedure  TDSFile.Load(Task:Core.Database.Types.TTask);
begin
  Files.DB.Read(Task,FParent.FOwner.FFAT.AuraDiskNode,Self);
end;

procedure TDSFile.AddToTemplates(Template:TTemplate);
begin
  EnterCriticalSection(Lock);
  Try
    FTemplates.Add(Template);
  finally
    LeaveCriticalSection(Lock);
  end;
end;

procedure TDSFile.Delete();
begin
  Storage.FAT.Files.DB.Delete(
    FOwner.FFAT.FTask,
    FParent.FOwner.FFAT.AuraDiskNode,
    FParent.FOwner.FFAT.DomainID,
    FFolderID,
    FID
  );
  FOwner.Remove(Self);
end;

procedure TDSFile.Empty();
begin
  FID:=0;
  FFolderID:=0;
  Cache:=false;
  Deflate:=false;
  CacheTTL:=0;
  System.FillChar(Digest[0],SizeOf(Digest),0);
end;

constructor TDSFiles.Create(aParent:TDSFolder; aFAT:TDSFAT);
begin
  FParent:=aParent;
  FFAT:=aFAT;
  Inherited Create;
end;

destructor TDSFiles.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

function TDSFiles.Get(Index:LongInt):TDSFile;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=TDSFile(List[Index]);
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Put(Index:LongInt; Value:TDSFile);
var
  List:TList;
begin
  List:=LockList();
  Try
    List[Index]:=Value;
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Archive(Manifest:TStream; ZipFile:TZipper);
var
  sManifest:Core.Strings.VarString;
  iLcv:LongInt;
  List:TList;
begin
  sManifest:='<files>';
  Core.Streams.Write(sManifest,Manifest);
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFile(List[iLcv]).Archive(Manifest,ZipFile);
  finally
    UnlockList();
  end;
  sManifest:='</files>';
  Core.Streams.Write(sManifest,Manifest);
end;

function TDSFiles.First:TDSFile;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=TDSFile(List.First);
  finally
    UnlockList();
  end;
end;

function TDSFiles.Last:TDSFile;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=TDSFile(List.Last);
  finally
    UnlockList();
  end;
end;

function TDSFiles.Add(Item:TDSFile): LongInt;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=List.Add(Item);
  finally
    UnlockList();
  end;
end;

function TDSFiles.Add(aID,aAttributes:QWord; HasKeywords:Boolean; const aName:Core.Strings.VarString=''): LongInt;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=List.Add(TDSFile.Create(FParent,aID,aAttributes,0,0,HasKeywords,true,0,0,aName));
    If (FParent.FOwner.FFAT.FScanInfo<>nil) then
      FParent.FOwner.FFAT.FScanInfo.Add(TDSFile(List[Result]));
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Remove(Item:TDSFile);
var
  List:TList;
begin
  List:=LockList();
  Try
    List.Remove(Item);
    Item.FOwner:=nil;
    Item.FParent:=nil;
    Item.Free;
  finally
    UnlockList();
  end;

end;

function  TDSFiles.New(aAttributes:QWord; aHasKeywords,aDeflate,aCache:Boolean; aName,aContent:Core.Strings.VarString):TDSFile;
var
  iFileID:QWord;
  iFileIndex:LongInt;
  baData:Core.Arrays.Types.Bytes;
  dgFile:TMD5Digest;
begin
  Result:=FFAT.Acquire(FParent,aName);
  if Result=nil then begin
    Core.Arrays.Bytes.fromString(aContent,baData);
    Try
      Core.Arrays.Bytes.CheckSum(baData,dgFile);
      Storage.FAT.Files.DB.Create(FFat.FTask,FParent,FFat.DomainID,iFileID,0,aAttributes,aHasKeywords,aDeflate,aCache,0,aName,baData,dgFile,iFileIndex);
      if iFileIndex<>-1 then
        Result:=Items[iFileIndex];
    finally
      Core.Arrays.Bytes.Done(baData);
    end;
  end;
end;

function  TDSFiles.New(Task:Core.Database.Types.TTask; aAttributes:QWord; aHasKeywords,aDeflate,aCache:Boolean; aName,aContent:Core.Strings.VarString):TDSFile;
var
  iFileID:QWord;
  iFileIndex:LongInt;
  baData:Core.Arrays.Types.Bytes;
  dgFile:TMD5Digest;
begin
  Result:=FFAT.Acquire(FParent,aName);
  if Result=nil then begin
    Core.Arrays.Bytes.fromString(aContent,baData);
    Try
      Core.Arrays.Bytes.CheckSum(baData,dgFile);
      Storage.FAT.Files.DB.Create(Task,FParent,FFat.DomainID,iFileID,aAttributes,0,aHasKeywords,aDeflate,aCache,0,aName,baData,dgFile,iFileIndex);
      if iFileIndex<>-1 then
        Result:=Items[iFileIndex];
    finally
      Core.Arrays.Bytes.Done(baData);
    end;
  end;
end;

procedure TDSFiles.Remove(aID:QWord);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(aID);
  if iIndex<>-1 then
    Remove(Items[iIndex]);
end;

procedure TDSFiles.Invalidate();
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do begin
      TDSFile(List[iLcv]).Refresh:=false;
      TDSFile(List[iLcv]).Validated:=false;
    end;
  finally
    UnlockList();
  end;
end;

function  TDSFiles.IndexOf(aID:QWord): LongInt;
var
  iLcv:LongInt;
  List:TList;
begin
  Result:=-1;
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do begin
      If TDSFile(Items[iLcv]).ID=aID then begin
        Result:=iLcv;
        Break;
      end;
    end;
  finally
    UnlockList();
  end;
end;

function  TDSFiles.IndexOf(Name:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
  List:TList;
begin
  Result:=-1;
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do begin
      If SameText(TDSFile(List[iLcv]).Name,Name) then begin
        Result:=iLcv;
        Break;
      end;
    end;
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Load(Task:Core.Database.Types.TTask);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFile(List[iLcv]).Load(Task);
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Clear;
var
  List:TList;
begin
  List:=LockList();
  Try
    While (List.Count>0) do
      TDSFile(List[0]).Free();
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.DeleteAll;
var
  List:TList;
begin
  List:=LockList();
  Try
    While (List.Count>0) do
      TDSFile(List[0]).Delete();
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Search(Results:TList; Term:Core.Strings.VarString);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFile(List[iLcv]).Search(Results,Term);
  finally
    UnlockList();
  end;
end;

function cbFileSort(Item1,Item2:Pointer): LongInt;
begin
  Result:=SysUtils.CompareText(TDSFile(Item1).Name,TDSFile(Item2).Name);
end;

procedure TDSFiles.Sort();
var
  List:TList;
begin
  List:=LockList();
  Try
    List.Sort(@cbFileSort);
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.toXML(Output:TStream; Stamp:Boolean);
var
  sData:Core.Strings.VarString;
  iLcv:LongInt;
  List:TList;
begin
  if Stamp then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  sData:=Concat('<',Files.XML.Stanza.Items,'>');
  Core.Streams.Write(sData,Output);
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do begin
      sData:=TDSFile(List[iLcv]).toXML();
      Core.Streams.Write(sData,Output);
    end;
  finally
    UnlockList();
  end;
  sData:=Concat('</',Files.XML.Stanza.Items,'>');
  Core.Streams.Write(sData,Output);
end;

procedure TDSFiles.BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFile(List[iLcv]).BuildScanList(Task,Item);
  finally
    UnlockList();
  end;
end;

procedure TDSFiles.Export(Path:Core.Strings.VarString);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFile(List[iLcv]).Export(Path);
  finally
    UnlockList();
  end;
end;

function  TDSFiles.Find(var Item:TDSFile; Name:Core.Strings.VarString):boolean;
var
  iIndex:LongInt;
begin
  Result:=False; Item:=nil; iIndex:=IndexOf(Name);
  if iIndex<>-1 then begin
    Item:=Items[iIndex];
    Result:=True;
  end;
end;

function  TDSFiles.Find(var Item:TDSFile; ID:QWord):boolean;
var
  iIndex:LongInt;
begin
  Result:=False; Item:=nil; iIndex:=IndexOf(ID);
  if iIndex<>-1 then begin
    Item:=Items[iIndex];
    Result:=True;
  end;
end;

constructor TDSFolder.Create(aOwner:TDSFolders; aID:QWord; aPath:Core.Strings.VarString; aAttributes:Byte);
begin
  Inherited Create;
  FLoading:=False;
  FID:=aID;
  FPath:=aPath;
  FOwner:=aOwner;
  FAttributes:=aAttributes;
  FFolders:=TDSFolders.Create(aOwner,aOwner.FFAT);
  FFiles:=TDSFiles.Create(Self,aOwner.FFAT);
  FName:=Core.Utils.Files.Extract(FPath,epoName);
end;

destructor  TDSFolder.Destroy;
var
  List:TList;
begin
  if (FOwner<>nil) then begin
    List:=FOwner.LockList();
    Try
      List.Remove(Self);
    finally
      FOwner.UnlockList();
    end;
  end;
  FreeAndNil(FFolders);
  FreeAndNil(FFiles);
  FreeAndNil(FTemplate);
  FOwner:=nil;
  Inherited Destroy;
end;

procedure TDSFolder.Archive(Manifest:TStream; ZipFile:TZipper);
var
  sManifest:Core.Strings.VarString;
begin
  sManifest:=Concat(
    '<folder>',
      '<id>',IntToStr(FID),'</id>',
      '<path>',Path,'</path>'
  );
  Core.Streams.Write(sManifest,Manifest);
  Empty(sManifest);
  Folders.Archive(Manifest,ZipFile);
  Files.Archive(Manifest,ZipFile);
  sManifest:='</folder>';
  Core.Streams.Write(sManifest,Manifest);
end;

procedure TDSFolder.Load(Task:Core.Database.Types.TTask; const Recursive:boolean);
begin
  Files.Load(Task);
  if Recursive then
    Folders.Load(Task,Recursive);
end;

procedure TDSFolder.SetAttributes(Value:Byte);
begin
  if FAttributes<>Value then begin
    FAttributes:=Value;
    Storage.FAT.Folders.DB.SetAttributes(FOwner.FFAT.FTask,FID,Value);
  end;
end;

procedure TDSFolder.Rename(Var sName:Core.Strings.VarString);
var
  sPath:Core.Strings.VarString;
begin
  sPath:=Core.Utils.Files.Extract(FPath,epoAllButName);
  Core.Utils.Files.Validate(sName);
  FPath:=Core.Utils.Files.Append(sPath,sName,SCS_PATH_DELIM);
  Storage.FAT.Folders.DB.Rename(FOwner.FFat.FTask,FID,FPath);
  FName:=Core.Utils.Files.Extract(FPath,epoName);
  Folders.PathChanged(Self);
end;

function  TDSFolder.Find(ID:QWord; Out Item:TDSFile):Boolean;
begin
  Result:=Files.Find(Item,ID);
  if (Result=false) then
     Result:=Folders.Find(ID,Item);
end;

function  TDSFolder.FileCount(Recursive:Boolean):QWORD;
var
  List:TList;
begin
  List:=Files.LockList();
  Try
    Result:=List.Count;
    if Recursive then
      Inc(Result,FFolders.FileCount(Recursive));
  finally
    Files.UnlockList();
  end;
end;


procedure TDSFolder.PathChanged(Parent:TDSFolder);
begin
  FPath:=Core.Utils.Files.Append(Parent.Path,Core.Utils.Files.Extract(FPath,epoName),SCS_PATH_DELIM);
  Storage.FAT.Folders.DB.Rename(FOwner.FFat.FTask,FID,FPath);
  Folders.PathChanged(Self);
end;

procedure TDSFolder.Search(Results:TList; Term:Core.Strings.VarString);
begin
  Files.Search(Results,Term);
  Folders.Search(Results,Term);
  if System.Pos(Term,Path)>0 then
    Results.Add(Self);
end;

procedure TDSFolder.Export(Path:Core.Strings.VarString; const IncludeName:Boolean=true);
begin
  if IncludeName then
    Path:=EndWithSeparator(Concat(Path,FName));
  SysUtils.ForceDirectories(Path);
  FFiles.Export(Path);
  FFolders.Export(Path);
end;

procedure TDSFolder.BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
begin
  Files.BuildScanList(Task,Item);
  Folders.BuildScanList(Task,Item);
end;

procedure TDSFolder.Delete;
begin
  Storage.FAT.Folders.DB.Delete(FOwner.FFat.FTask,FOwner.FFat.AuraDiskNode,FOwner.FFat.FDomainID,FID);
  Files.DeleteAll();
  Folders.DeleteAll();
  FOwner.Remove(Self);
end;

procedure TDSFolder.Flush;
begin
  If FTemplate<>nil then
    FTemplate.Invalidate;
end;

procedure TDSFolder.Templatetize(var FileName:Core.Strings.VarString; Callback:TTemplatetizeEvent);
var
  dsItem:TDSFile;
begin
  if Files.Find(dsItem,FileName) then
    FTemplate:=Callback(Self,dsItem);
  Folders.Templatetize(FileName,Callback);
end;

constructor TDSFolders.Create(aParent:TDSFolders; aFAT:TDSFat);
begin
  Inherited Create;
  FParent:=aParent;
  FFAT:=aFAT;
end;

destructor  TDSFolders.Destroy;
begin
  Clear;
  Inherited Destroy;
end;

function TDSFolders.Get(Index:LongInt):TDSFolder;
var
  List:TList;
begin
  List:=LockList();
  Try
    if (Index>-1) and (Index<List.Count) then begin
      Result:=TDSFolder(List[Index]);
    end else begin
      Result:=nil;
    end;
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Put(Index:LongInt; Value:TDSFolder);
var
  List:TList;
begin
  List:=LockList();
  Try
    List[Index]:=Value;
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Load(Task:Core.Database.Types.TTask; Recursive:Boolean);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    For iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).Load(Task,Recursive);
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.DeleteAll;
var
  List:TList;
begin
  List:=LockList();
  Try
    While List.Count>0 do
      TDSFolder(List[0]).Delete();
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Flush();
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    For iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).Flush();
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Archive(Manifest:TStream; ZipFile:TZipper);
var
  sManifest:Core.Strings.VarString;
  iLcv:LongInt;
  List:TList;
begin
  sManifest:='<folders>';
  Core.Streams.Write(sManifest,Manifest);
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).Archive(Manifest,ZipFile);
  finally
    UnlockList();
  end;
  sManifest:='</folders>';
  Core.Streams.Write(sManifest,Manifest);
end;

procedure TDSFolders.Export(Path:Core.Strings.VarString);
var
  iLcv:LongInt;
  List:TList;
begin
  Path:=EndWithSeparator(Path);
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).Export(Path);
  finally
    UnlockList();
  end;
end;

function TDSFolders.Last: TDSFolder;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=TDSFolder(List.Last);
  finally
    UnlockList();
  end;
end;

function TDSFolders.First: TDSFolder;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=TDSFolder(List.First);
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Clear;
var
  List:TList;
begin
  List:=LockList();
  Try
    While (List.Count>0) do
      TDSFolder(List[0]).Free();
  finally
    UnlockList();
  end;
end;

function TDSFolders.FileCount(Recursive:Boolean):QWORD;
var
  iLcv:LongInt;
  List:TList;
begin
  Result:=0;
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      Inc(Result,TDSFolder(List[iLcv]).FileCount(Recursive));
  finally
    UnlockList();
  end;
end;

function TDSFolders.IndexOf(aID:QWord): LongInt;
var
  iLcv:LongInt;
  List:TList;
begin
  Result:=-1;
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do begin
      if TDSFolder(List[iLcv]).ID=aID then begin
        Result:=iLcv;
        Break;
      end;
    end;
  finally
    UnlockList();
  end;
end;

function TDSFolders.Add(Item: TDSFolder): LongInt;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=List.Add(Item);
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.PathChanged(Parent:TDSFolder);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).PathChanged(Parent);
  finally
    UnlockList();
  end;
end;

function TDSFolders.Add(aID:QWord; aPath:Core.Strings.VarString; aAttributes:byte): LongInt;
var
  List:TList;
begin
  List:=LockList();
  Try
    Result:=List.Add(TDSFolder.Create(Self,aID,aPath,aAttributes));
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Remove(Item:TDSFolder);
begin
  Inherited Remove(Item);
  Item.FOwner:=nil;
  Item.Free;
end;

procedure TDSFolders.Remove(aID:QWord);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(aID);
  if iIndex<>-1 then
    Remove(Items[iIndex]);
end;

procedure TDSFolders.Search(Results:TList; Term:Core.Strings.VarString);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).Search(Results,Term);
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.Templatetize(var FileName:Core.Strings.VarString; Callback:TTemplatetizeEvent);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      TDSFolder(List[iLcv]).Templatetize(FileName,Callback);
  finally
    UnlockList();
  end;
end;

procedure TDSFolders.BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
var
  iLcv:LongInt;
  List:TList;
begin
  List:=LockList();
  Try
  for iLcv:=0 to List.Count-1 do
    TDSFolder(Items[iLcv]).BuildScanList(Task,Item);
  finally
    UnlockList();
  end;
end;

Function  TDSFolders.Find(var Folder:TDSFolder; Const FolderID:QWord):boolean;

  procedure ParseSubFolders(Pivot:TDSFolders);
  var
    iLcv:LongInt;
    iCount:LongInt;
    Scan:TDSFolder;
    List:TList;
  begin
    List:=Pivot.LockList();
    Try
      iLcv:=0; iCount:=List.Count;
      While (iLcv<iCount) and (Folder=nil) do begin
        Scan:=TDSFolder(List[iLcv]);
        If (Scan.ID=FolderID) then begin
          Folder:=Scan;
          Result:=True;
          Break;
        end else
          ParseSubFolders(Scan.Folders);
        Inc(iLcv);
      end;
    finally
      Pivot.UnlockList();
    end;
  end;

begin
  Result:=False;
  ParseSubFolders(Self);
end;

function  TDSFolders.Find(ID:QWord; Out Item:TDSFile):Boolean;

  procedure ParseSubFolders(Pivot:TDSFolders);
  var
    iLcv:LongInt;
    Scan:TDSFolder;
    List:TList;
  begin
    iLcv:=0;
    List:=Pivot.LockList();
    Try
      While (iLcv<List.Count) and (Result=false) do begin
        Scan:=TDSFolder(List[iLcv]);
        Result:=Scan.Find(ID,Item);
        If (Result=false) then
          ParseSubFolders(Scan.Folders);
        Inc(iLcv);
      end;
    finally
      Pivot.UnlockList();
    end;
  end;
begin
  Result:=False;
  ParseSubFolders(Self);
end;

Function  TDSFolders.Find(var Folder:TDSFolder; Value:Core.Strings.VarString; Option:TDSFindOption):boolean;

  procedure PushFindByName();
  var
    iLcv:LongInt;
    List:TList;
  begin
    List:=LockList();
    Try
      for iLcv:=0 to List.Count-1 do begin
        if Core.Strings.SameText(TDSFolder(List[iLcv]).FName,Value) then begin
          Result:=True;
          Folder:=TDSFolder(List[iLcv]);
          Break;
        end;
      end;
    finally
      UnlockList();
    end;
  end;

  procedure PushFindByPath;
  var
    iLcv:LongInt;
    List:TList;
  begin
    List:=LockList();
    Try
      for iLcv:=0 to List.Count-1 do begin
        if SameText(TDSFolder(List[iLcv]).FPath,Value) then begin
          Result:=True;
          Folder:=TDSFolder(List[iLcv]);
          Break;
        end;
      end;
    finally
      UnlockList();
    end;
  end;

begin
  Result:=false;
  Case Option of
    dsFindByPath  : PushFindByPath;
    dsFindByName  : PushFindByName;
  end;
end;

function  TDSFolders.New(aPath:Core.Strings.VarString; aAttributes:Byte):TDSFolder;
var
  iFolderID   : QWord;
  iPathCount  : LongInt;
  iPathLcv    : LongInt;
  saPath      : Core.Arrays.Types.VarString;
  Pivot       : TDSFolders;
  Folder      : TDSFolder;
  sNewPath    : Core.Strings.VarString;
begin
  Result:=nil;
  // we need to force each folder along the way..

  Core.Arrays.VarString.fromString(saPath,aPath,'/',[soClearList,soIgnoreDelimAtStart]);

  try
    iPathCount:=System.Length(saPath);
    iPathLcv:=0;
    Pivot:=Self;
    Folder:=nil;
    for iPathLcv:=0 to iPathCount-1 do begin
      if Pivot.Find(Folder,saPath[iPathLcv],dsFindByName) then begin
        Pivot:=Folder.Folders;
      end else begin
        sNewPath:=Core.Arrays.VarString.toString(saPath,0,iPathLcv+1,'/',nil,TRAILING_DELIM_OFF,LEADING_DELIM_ON);
        if Storage.FAT.Folders.DB.Create(FFAT.FTask,Self,FFAT.FDomainID,sNewPath,iFolderID,aAttributes) then begin
          Folder:=Get(IndexOf(iFolderID));
          Pivot:=Folder.Folders;
        end else begin
          Pivot:=nil;
          Folder:=nil;
          Break;
        end;
      end;
    end;
    Result:=Folder;
  finally
    Core.Arrays.VarString.Done(saPath);
  end;
end;

constructor  TDSFAT.Create();
begin
  Storage.MatrixNodes.Node.Init(AuraDiskNode);
  StartBackgroundTimers();

  Inherited Create;
  FFatDwell:=0;
  FProgressValue:=0;
  FProgressMax:=0;
  FScanInfo:=nil;
  FDomainID:=0;
  FFolders:=TDSFolders.Create(nil,Self);
  FTask:=Core.Database.Types.TTask.Create(Storage.Main.Header,'Storage.FAT');


  FKWEngine:=TKWSEngine.Create;
  FKWEngine.OnGetNextItem:=@OnGetNextFile;
  FKWEngine.OnScanComplete:=@OnScanComplete;


  Core.Timer.Init(FFATTimerItem);
  FFATTimerItem.Event:=@OnFATTimerItem;
  FFATTimerItem.Location:='Storage.FAT.OnFATTimerItem';
  FFATTimerItem.Mode:=temNormal;
  FFATTimerItem.Expires:=IncMinute(Core.Timer.dtNow,1);

  FATTimer.RegisterEvent(FFATTimerItem,LoadNoUpdate);

end;

Destructor  TDSFAT.Destroy;
begin
  FKWEngine.Terminate;
  FKWEngine:=nil;
  FLoading:=true;
  Try
    FreeAndNil(FFolders);
  Finally
    FLoading:=False;
  end;

  FATTimer.UnloadEvent(FFATTimerItem,Core.Timer.UnloadNoExecute);

  Storage.MatrixNodes.Node.Done(AuraDiskNode);

  Inherited Destroy;
end;

procedure  TDSFAT.SetDomainID(Value:QWord);
begin
  if FDomainID<>Value then begin
    Clear();
    FDomainID:=Value;
    Node.Empty(AuraDiskNode);
    Storage.Domains.Items.DB.GetName(FTask,FDomainID,FDomain);
    Storage.AuraDisks.Router.Verify(FTask,FDomainID,Use.Global,Kinds.Domain,AuraDiskNode);
  end;
end;

procedure  TDSFAT.OnScanComplete(CommandP:PKWSECommand);
var
  svScan:TFATScanValue;
begin
  if CommandP^.Interval=0 then begin
    svScan:=TFATScanValue(toPointer(CommandP^.Value));
    svScan.Free;
    Empty(CommandP^.Value);
  end else begin
    svScan:=TFATScanValue(toPointer(CommandP^.Value));
    svScan.FIndex:=0;
  end;
end;

function   TDSFAT.OnCBKeywordDefault(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=ItemP^.Value;
end;

function   TDSFAT.OnGetNextFile(CommandP:PKWSECommand; var FileP:PKWSFile; var Resource:Core.Strings.VarString; var Length:Int64):Boolean;
var
  dsFile:TDSFile;
  FS:TFileStream;
  svScan:TFATScanValue;
begin
  svScan:=TFATScanValue(toPointer(CommandP^.Value));
  dsFile:=svScan.GetNextFile(CommandP^.Flush);
  Result:=(dsFile<>nil);
  if Result then begin
    if (dsFile.FScanP=nil) then begin
      new(dsFile.FScanP);
      Core.Keywords.Init(dsFile.FScanP^);
    end;
    FileP:=dsFile.FScanP;
    FileP^.dtFile:=dsFile.Modified;
    FileP^.dtDownloaded:=dsFile.Downloaded;
    if (dsFile.FVolatile=true) then begin
      Resource:=Core.Streams.toString(dsFile.FMemory);
    end else begin
      FS:=dsFile.AcquireData();
      Try
        Resource:=Core.Streams.toString(FS);
      finally
        FreeAndNil(FS);
      end;
    end;
  end;
  Length:=System.Length(Resource);
end;

function  TDSFAT.Refresh(Task:Core.Database.Types.TTask; out Refreshed:QWord):boolean;
var
  bRescan:Boolean;
begin
  Result:=Files.DB.Refresh(Task,Self,Refreshed,bRescan);
  if Result and bRescan then
    FKWEngine.Awake(FScanInfo);
end;

procedure  TDSFAT.Refresh(Delay:Word);
begin
  FFATTimerItem.Expires:=IncMillisecond(Core.Timer.dtNow,Delay);
end;

procedure  TDSFAT.Load(aDomainID,aClusterID,aResourceID,aNodeID:QWord; const Options:TDSFATLoadOption);
var
  iLcv:LongInt;
  Required:TDSFolder;
  iID:QWord;
  Refactor:TMemoryStream;
begin
  // Assume Main Thread here.
  if aDomainID=0 then exit;
  if  FDomainID<>aDomainID then
    Clear;
  FDomainID:=aDomainID;
  FClusterID:=aClusterID;
  FResourceID:=aResourceID;
  FNodeID:=aNodeID;
  Storage.MatrixNodes.Node.Empty(AuraDiskNode);
  Storage.AuraDisks.Router.Verify(FTask,aDomainID,Use.Global,Kinds.Domain,AuraDiskNode);
  Storage.MatrixNodes.Node.DB.Fill(FTask,AuraDiskNode);

  Storage.Domains.Items.DB.GetName(Task,FDomainID,FDomain);
  case Options of
    dsfatloAll     : GetFAT(Task,Self); // Will Load even data
    dsfatloFolders : Storage.FAT.Folders.DB.List(Task,Self);
    dsfatloBoth    : Begin
      Storage.FAT.Folders.DB.List(Task,Self);
      Storage.FAT.Files.DB.Basic(Task,Self);
    end;
  end;
  Refactor:=TMemoryStream.Create;
  Try
    for iLcv:=0 to High(RequiredFolders) do begin
      if Find(Required,RequiredFolders[iLcv],Refactor)=false then
        Storage.FAT.Folders.DB.Create(Task,FFolders,Self.FDomainID,Concat('/',RequiredFolders[iLcv]),iID,FS_ATTR_REQUIRED_FOLDER);
    end;
  finally
    FreeAndNil(Refactor)
  end;
end;

procedure  TDSFat.Import(Start:TDSFolder; Path:Core.Strings.VarString);
// Recursively scans folders for folders and files... Adding them to the system
// If not present...  Resetting data and attributes of files as needed
var
  bbData:Core.Arrays.Types.Bytes;
  dsFile:TDSFile;
  dgFile:TMD5Digest;

  function SearchFileCount(PrePath:Core.Strings.VarString):QWORD;
  var
    SR:TSearchRec;
    iResult:LongInt;
  begin
    Result:=0;
    iResult:=FindFirst(Append(PrePath,'*'),faDirectory or faAnyFile,SR);
    Try
      While (iResult=0) do begin
        if isDirectory(SR.Attr) then begin
          // Any folders get added as folders and parsed
          if (SR.Name<>'..') and (SR.Name<>'.') then
            Inc(Result,SearchFileCount(Append(PrePath,SR.Name)));
        end else if (not SameText(SR.Name,'thumbs.db')) then
          Inc(Result);
        iResult:=FindNext(SR);
      end;
    finally
      FindClose(SR);
    end;
  end;

  procedure ScanFolder(Folder:TDSFolder; PrePath:Core.Strings.VarString);
  var
    SR:TSearchRec;
    Sub:TDSFolder;
    iID:QWord;
    iIndex:LongInt;
    iResult:LongInt;
    bException:Boolean;
    bKeywords:boolean;
    bImage:boolean;
    bCompressed:boolean;
    bDeflate:boolean;
    bCache:boolean;
    sExt:Core.Strings.VarString;
    fsAura:TFileStream;
  begin
    iResult:=FindFirst(Append(PrePath,'*'),faDirectory or faAnyFile,SR);
    Try
      While (iResult=0) do begin
        if isDirectory(SR.Attr) then begin
          // Any folders get added as folders and parsed
          if (
            (SR.Name<>'..') and
            (SR.Name<>'.') and
            (SR.Name<>'.svn') and
            (SR.Name<>'.backup')
          )
          then begin
            Sub:=nil;
            if Folder.Folders.Find(Sub,SR.Name,dsFindByName)=False then begin
              if Storage.FAT.Folders.DB.Create(Task,Folder.Folders,FDomainID,Join(Folder.Path,SR.Name,'/'),iID,FS_ATTR_NONE) then begin
                iIndex:=Folder.Folders.IndexOf(iID);
                if iIndex<>-1 then
                  Sub:=Folder.Folders.Get(iIndex);
              end;
            end;
            if Sub<>nil then
              ScanFolder(Sub,Append(PrePath,SR.Name));
          end;
        end else if (not SameText(SR.Name,'thumbs.db')) then begin
          Inc(FProgressValue);
          if Assigned(FOnStatus) then
            FOnStatus(FProgressValue,FProgressMax,Format(FMT_PROGRESS_USR_STATUS,['Importing',Append(PrePath,SR.Name)]));
          // Any files get added as files
          if Folder.Files.Find(dsFile,SR.Name)=false then begin
            sExt:=Core.Utils.Files.Extract(SR.Name,efeoNone);
            bImage:=(
              Core.Strings.SameText(sExt,Files.Extensions.Media.BMP) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.GIF) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.PNG) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.JPG) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.MPG) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.MPEG) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.MOV) or
              Core.Strings.SameText(sExt,Files.Extensions.Media.AVI)
            );
            bCompressed:=(
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.ZIP) or
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.EXE) or
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.BIN) or
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.JAR) or
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.GZ) or
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.GZIP) or
              Core.Strings.SameText(sExt,Files.Extensions.Compressed.CAB)
            );
            bKeywords:=(
              (bImage=false) and
              (bCompressed=false) and
              (Core.Arrays.Bytes.Pos(bbData,'{$i ')<>-1)
            );
            try
              bException:=false;
              Core.Arrays.Bytes.fromFile(Append(PrePath,SR.Name),bbData);
            except
              On E:Exception do begin
                // todo add logging for bad import
                bException:=True;
              end;
            end;
            Try
              if bException=false then begin
                bDeflate:=(bImage=false) and (bCompressed=false);
                bCache:=true;
                Core.Arrays.Bytes.CheckSum(bbData,dgFile);
                Storage.FAT.Files.DB.Create(Task,Folder,FDomainID,iID,SR.Time,FS_ATTR_NONE,bKeywords,bDeflate,bCache,0,SR.Name,bbData,dgFile,iIndex);
                if iIndex<>-1 then
                  dsFile:=Folder.Files.Items[iIndex];
              end;
            finally
              Empty(bbData);
            end;
          end else if (dsFile.Imported<>SR.Time) then begin
            Core.Arrays.Bytes.fromFile(Append(PrePath,SR.Name),bbData);
            try
              Core.Arrays.Bytes.CheckSum(bbData,dgFile);
              If Same(dgFile,dsFile.Digest)=false then begin
                dsFile.Imported:=SR.Time;
                fsAura:=dsFile.AcquireData();
                Try
                  Core.Streams.fromData(bbData,fsAura);
                  dsFile.Size:=Length(bbData);
                  Core.Arrays.Bytes.Copy(dgFile,dsFile.Digest);
                finally
                  FreeAndNil(fsAura);
                end;
                dsFile.Downloaded:=Core.Timer.dtUT;
                dsFile.Save(Task,dsfileSaveAll);
              end else begin
                dsFile.Imported:=SR.Time;
              end;
            finally
              Empty(bbData);
            end;
          end;
        end;
        iResult:=FindNext(SR);
      end;
    finally
      FindClose(SR);
    end;
  end;

begin
  FProgressValue:=0;
  FProgressMax:=SearchFileCount(Path);
  Try
    ScanFolder(Start,Path);
  finally
    Core.Arrays.Bytes.Done(bbData);
  end;
  FProgressMax:=0; FProgressValue:=0;
  If Assigned(FOnStatus) then
    FOnStatus(0,0,'');
end;

function   TDSFAT.FileCount(Recursive:Boolean):QWORD;
begin
  Result:=FFolders.FileCount(Recursive);
end;

procedure  TDSFAT.Export(Start:TDSFolder; Path:Core.Strings.VarString);
begin
  // Path is the target folder to base all sub folders on.
  Path:=EndWithSeparator(Path);
  FProgressValue:=0;
  if (Start=nil) then begin
    FProgressMax:=FileCount(FAT_RECURSE_ON);
    FFolders.Export(Path);
  end else begin
    FProgressMax:=Start.FileCount(FAT_RECURSE_ON);
    Start.Export(Path,SURPRESS_FIRST_FOLDER);
  end;
  FProgressMax:=0; FProgressValue:=0;
  If Assigned(FOnStatus) then
    FOnStatus(0,0,'');
end;

function   TDSFAT.Acquire(var Path:Core.Strings.VarString; Refactor:TStream):TDSFolder;
begin
  Result:=Nil;
  Find(Result,Path,Refactor);
end;

Function   TDSFAT.Acquire(Folder:TDSFolder; FileName:Core.Strings.VarString):TDSFile;
begin
  Result:=nil;
  Folder.FFiles.Find(Result,FileName);
end;

Function   TDSFAT.Acquire(URI:Core.Strings.VarString; Refactor:TStream; Var dsFile:TDSFile):boolean;
var
  saPath:Core.Arrays.Types.VarString;
  iPathLen:LongInt;
  iPathCount:LongInt;
  iPathLcv:LongInt;
  sCoreCheck:Core.Strings.VarString;
  sCoreFolder:Core.Strings.VarString;
  Pivot:TDSFolders;
  Folder:TDSFolder;
begin
  Result:=false; Folder:=nil; Pivot:=FFolders; dsFile:=nil;
  iPathLen:=System.Length(URI);
  if iPathLen>0 then begin
    if (URI[1]='/') then begin
      //Core Check
      iPathLen:=System.Length(RequiredFolders[RFI_CORE]);
      sCoreFolder:=Concat('/',RequiredFolders[RFI_CORE],'/');
      sCoreCheck:=System.Copy(URI,1,iPathLen+2);
      if SameText(sCoreFolder,sCoreCheck) then begin
        // Leave as is
      end else begin
        URI:=Concat(RequiredFolders[RFI_WEB],URI);
      end;
    end else begin
      //Core Check
      iPathLen:=System.Length(RequiredFolders[RFI_CORE]);
      sCoreFolder:=Concat('/',RequiredFolders[RFI_CORE],'/');
      sCoreCheck:=System.Copy(URI,1,iPathLen+2);
      if SameText(sCoreFolder,sCoreCheck) then begin
        // Leave as is
      end else begin
        URI:=Concat(RequiredFolders[RFI_WEB],'/',URI);
      end;
    end;
    Core.Arrays.VarString.fromString(saPath,URI,'/',[soClearList,soIgnoreDelimAtStart]);
    try
      iPathCount:=System.Length(saPath);
      iPathLcv:=0;
      for iPathLcv:=0 to iPathCount-2 do begin
        if Pivot.Find(Folder,saPath[iPathLcv],dsFindByName) then begin
          Pivot:=Folder.Folders;
        end else begin
          Pivot:=nil;
          Folder:=nil;
          break;
        end;
      end;
      if (Folder<>nil) and (Pivot<>nil) then begin
        Result:=True;
        Result:=Folder.FFiles.Find(dsFile,saPath[iPathCount-1]);
      end;
    finally
      Core.Arrays.VarString.Done(saPath);
    end;
  end;
end;


Function   TDSFat.Force(Parent:TDSFolders; var Folder:TDSFolder; FolderID:QWord; Path:Core.Strings.VarString; Attributes:Byte; Refactor:TStream):boolean;
var
  iPathCount:LongInt;
  iPathLcv:LongInt;
  saPath:Core.Arrays.Types.VarString;
  sDirectory:Core.Strings.VarString;
  Pivot:TDSFolders;
begin
  Result:=false; Folder:=nil;
  Core.Arrays.VarString.fromString(saPath,Path,'/',[soClearList,soIgnoreDelimAtStart]);
  try
    iPathCount:=System.Length(saPath);
    iPathLcv:=0; Pivot:=Parent;
    for iPathLcv:=0 to iPathCount-1 do begin
      Folder:=nil;
      sDirectory:=Core.Arrays.VarString.toString(saPath,0,iPathLcv+1,'/',Refactor,TRAILING_DELIM_OFF,LEADING_DELIM_ON);
      if Force(Pivot,Folder,sDirectory,Attributes) then
        Pivot:=Folder.Folders;
    end;
    if Folder<>nil then begin
      if Folder.ID=0 then
        Folder.FID:=FolderID;
      Result:=True;
    end;
  finally
    Core.Arrays.VarString.Done(saPath);
  end;
end;

Function   TDSFat.Force(Parent:TDSFolders; var Folder:TDSFolder; Path:Core.Strings.VarString; Attributes:Byte):boolean;
var
  iLcv:LongInt;
  List:TList;
 begin
  Folder:=nil;
  List:=Parent.LockList();
  Try
    For iLcv:=0 to List.Count-1 do begin
      if Core.Strings.SameText(TDSFolder(List[iLcv]).FPath,Path) then begin
        Folder:=TDSFolder(List[iLcv]);
        Result:=True;
        Break;
      end;
    end;
  finally
    Parent.UnlockList();
  end;
  if Folder=nil then begin
    iLcv:=Parent.Add(0,Path,Attributes);
    Folder:=Parent.Items[iLcv];
    Result:=True;
  end;
end;

procedure   TDSFAT.Templatetize(FileName:Core.Strings.VarString; Callback:TTemplatetizeEvent);
begin
  FFolders.Templatetize(FileName,Callback);
end;

procedure   TDSFAT.OnFATTimerItem(ItemP:Core.Timer.PItem);
Const                                           // 120 sec                  // 15 sec
  preSetDelay: Array[Boolean] of Integer=(DOMAIN_FAT_REFRESH_INACTIVE,DOMAIN_FAT_REFRESH_ACTIVE);
var
  Refreshed:QWord;
  Delay:LongInt;
begin
  Try
    Refreshed:=0;
    {$if defined(RSR_DEBUG)}
      Core.Logging.Native.WriteLogEntry(FDomain,'FAT','OnFATTimer.Refreshing');
    {$endif}
    Try
      if not Refresh(FATTimer.Task,Refreshed) then
        Core.Logging.Native.WriteLogEntry(FDomain,'FAT','OnFATTimerItem Error: Refresh command failed.');
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain,'FAT',Concat('OnFATTimerItem Exception: ',E.Message));
    End;
    {$if defined(RSR_DEBUG)}
      Core.Logging.Native.WriteLogEntry(FDomain,'FAT','OnFATTimer.Refreshed');
    {$endif}
  Finally
    if (Refreshed=0) then begin
      // no new changes detected
      Delay:=preSetDelay[(Core.Timer.dtNow<FFatDwell)]; // delay expires so we can pause
    end else begin
      // Some files were changed... Increase scans until Refreshed=0
      FFatDwell:=IncSecond(Core.Timer.dtNow,DOMAIN_FAT_REFRESH_DWELL);
      Delay:=DOMAIN_FAT_REFRESH_ACTIVE;
    end;
    ItemP^.Expires:=IncSecond(Core.Timer.dtNow,Delay);
  end;
end;

procedure   TDSFAT.Backup(FileName:Core.Strings.VarString);
var
  Archive                        : TZipper;
  msManifest                     : TMemoryStream;
begin
  Archive:=TZipper.Create();
  Try
    FileName:=Core.Utils.Files.Prepare(FileName,'.scs');
    //Archive.SaveToFile(odSKWFile.FileName);
    if FileExists(FileName) then
      SysUtils.DeleteFile(FileName);
    Archive.FileName:=FileName;
    msManifest:=TMemoryStream.Create;
    Try
      Core.Streams.Write('<xml><archive>',msManifest);
      FFolders.Archive(msManifest,Archive);
      Core.Streams.Write('</archive></xml>',msManifest);
      msManifest.Position:=0;
      Archive.Entries.AddFileEntry(msManifest,'/Manifest.xml');
    finally
      FreeAndNil(msManifest);
    end;
  Finally
    FreeAndNil(Archive);
  end;
end;

procedure   TDSFAT.Restore(FileName:Core.Strings.VarString);
begin

end;

procedure   TDSFAT.Search(var Results:TList; Term:Core.Strings.VarString);
begin
  if Results=nil then Results:=TList.Create;
  Folders.Search(Results,Term);
end;

procedure   TDSFAT.BuildScanList(Task:Core.Database.Types.TTask; Item:TFATScanValue);
begin
 Folders.BuildScanList(Task,Item);
end;

procedure   TDSFAT.Flush;
begin
  FKWEngine.Flush(FScanInfo);
  FFolders.Flush;
end;

procedure   TDSFAT.Scan(Task:Core.Database.Types.TTask; KeywordsP:PKeywords);
var
  MS                             : Storage.MatrixServices.Items.Item;
  Scale                          : Byte;
begin
  If FKWEngine.Status = ssNone then begin
    Scale:=0;
    Storage.MatrixServices.Items.Init(MS);
    Try
      // Load Defaults
      MS.Kind:=Storage.MatrixServices.Items.mkKeywordScanner;
      MS.NodeID:=Storage.MatrixServices.Current.NodeID;
      MS.ClusterID:=Storage.MatrixServices.Current.ClusterID;
      MS.DomainID:=Storage.MatrixServices.Current.DomainID;
      MS.ResourceID:=Storage.MatrixServices.Current.ResourceID;
      if Storage.MatrixServices.Items.DB.Fill(Task,MS,Storage.MatrixServices.Items.FillOption.foUseCRNDK) and (MS.ID<>0) then
        Scale:=MS.Scale;
      If Scale=0 then
        raise Exception.Create(Format(FMT_SCAN_ERROR_DISABLED,[MS.ClusterID,MS.ResourceID,MS.NodeID,MS.DomainID]));
      FKWEngine.Execute(kwsEngineSetThreads,Scale);
    finally
      Storage.MatrixServices.Items.Done(MS);
    end;
  end;
  if FScanInfo=nil then begin
    FScanInfo:=TFATScanValue.Create();
    BuildScanList(Task,FScanInfo);
    FKWEngine.Execute(kwsEngineStart,RESCAN_WAIT,KeywordsP,FScanInfo);
  end;
end;

procedure   TDSFAT.Clear;
begin
  FDomainID:=0;
  FLoading:=True;
  Try
    FFolders.Clear();
    Storage.MatrixNodes.Node.Empty(AuraDiskNode);
  finally
    FLoading:=False;
  end;
end;

procedure TDSFAT.Search(Results:TList; Term:Core.Strings.VarString; Level:LongInt; Refactor:TMemoryStream);
var
  Root:TDSFolder;
  iLcv:LongInt;
  List:TList;

  procedure Pivot(Sub:TDSFolder; Level:LongInt);
  var
    iLcv:LongInt;
    List:TList;
  begin
    Results.Add(Sub);
    Dec(Level);
    if Level>=0 then begin
      List:=Sub.Folders.LockList();
      Try
        For iLcv:=0 to List.Count-1 do begin
          Pivot(TDSFolder(List[iLcv]),Level);
        end;
      finally
        Sub.Folders.UnlockList();
      end;
    end;
  end;

begin
  if Find(Root,Term,Refactor) then begin
    Results.Add(Root);
    Dec(Level);
    if Level>=0 then begin
      List:=Root.Folders.LockList();
      Try
        For iLcv:=0 to List.Count-1 do begin
          Pivot(TDSFolder(List[iLcv]),Level);
        end;
      finally
        Root.Folders.UnlockList();
      end;
    end;
  end else begin
    List:=Folders.LockList();
    Try
      For iLcv:=0 to List.Count-1 do Pivot(TDSFolder(List[iLcv]),Level);
    finally
      Folders.UnlockList();
    end;
  end;
end;

Function    TDSFAT.Find(var Folder:TDSFolder; Path:Core.Strings.VarString; Refactor:TStream):boolean;
var
  iPathCount:LongInt;
  iPathLcv:LongInt;
  saPath:Core.Arrays.Types.VarString;
  Pivot:TDSFolders;
begin
  Result:=False;
  Core.Arrays.VarString.fromString(saPath,Path,'/',[soClearList,soIgnoreDelimAtStart]);
  Try
    iPathCount:=System.Length(saPath);
    iPathLcv:=0;
    Pivot:=Folders;
    Folder:=nil;
    for iPathLcv:=0 to iPathCount-1 do begin
      if Pivot.Find(Folder,saPath[iPathLcv],dsFindByName) then begin
        Pivot:=Folder.Folders;
      end else begin
        Folder:=nil;
        Break;
      end;
    end;
    Result:=(Folder<>nil);
  finally
    Core.Arrays.VarString.Done(saPath);
  end;
end;

procedure CB_GetFile_Object(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  dsFile     : TDSFile;
begin
  dsFile:=TDSFile(DataP);
  {$i Storage.FAT.CB_GetFAT_Files.inc}
  dsFile.Downloaded:=Core.Timer.dtUT;
end;

procedure CB_GetFile_Details_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  with PFileDetails(DataP)^ do begin;
    Cache:=Fields.FieldByName(Files.DB.Keys.Cache).AsBoolean;
    HasKeywords:=Fields.FieldByName(Files.DB.Keys.HasKeywords).AsBoolean;
    Deflate:=Fields.FieldByName(Files.DB.Keys.Deflate).AsBoolean;
    Imported:=Fields.FieldByName(Files.DB.Keys.Imported).AsInteger;
    Size:=Fields.FieldByName(Files.DB.Keys.Size).AsLargeInt;
    ContentType:=Fields.FieldByName(Files.DB.Keys.ContentType).AsInteger;
    Created:=Fields.FieldByName(Files.DB.Keys.Created).AsDateTime;
    Modified:=Fields.FieldByName(Files.DB.Keys.Modified).AsDateTime;
    Name:=Fields.FieldByName(Files.DB.Keys.Name).AsString;
    Encryption.Base64.Decode(Fields.FieldByName(Files.DB.Keys.Digest).AsString,Digest);
  end;
end;

class Function  Files.DB.Read(Task:Core.Database.Types.TTask; Var Details:TFileDetails):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Details.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.HasKeywords,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Imported,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Size,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Cache,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.CacheTTL,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Digest,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Deflate,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ContentType,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Created,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GetFile_Details_ID,@Details);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Files.DB.Write(Task:Core.Database.Types.TTask; dsFile:TDSFile; const Option:TDSFileSaveOption):Boolean;

  procedure PushSaveAll;
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Imported,poNone,oNone,dsFile.Imported,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Size,poNone,oNone,dsFile.Size,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,dsFile.Name,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Digest,poNone,oNone,dsFile.Digest,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Deflate,poNone,oNone,dsFile.Deflate,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cache,poNone,oNone,dsFile.Cache,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CacheTTL,poNone,oNone,dsFile.CacheTTL,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Attributes,poNone,oNone,dsFile.Attributes,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.HasKeywords,poNone,oNone,dsFile.FHasKeywords,Commands);
      Result:=Core.Database.SQL.Update(Task,@Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

  procedure PushSaveInfo;
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,dsFile.Name,Commands);
      Result:=Core.Database.SQL.Update(Task,@Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

  procedure PushSaveKeywords;
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.HasKeywords,poNone,oNone,dsFile.FHasKeywords,Commands);
      Result:=Core.Database.SQL.Update(Task,@Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

  procedure PushSaveCache;
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cache,poNone,oNone,dsFile.Cache,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CacheTTL,poNone,oNone,dsFile.CacheTTL,Commands);
      Result:=Core.Database.SQL.Update(Task,@Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

  procedure PushSaveAttributes;
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);

      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Attributes,poNone,oNone,dsFile.Attributes,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Deflate,poNone,oNone,dsFile.Deflate,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cache,poNone,oNone,dsFile.Cache,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CacheTTL,poNone,oNone,dsFile.CacheTTL,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.HasKeywords,poNone,oNone,dsFile.FHasKeywords,Commands);

      Result:=Core.Database.SQL.Update(Task,@Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

  procedure PushSaveDeflate();
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Deflate,poNone,oNone,dsFile.Deflate,Commands);
      Result:=Core.Database.SQL.Update(Task,@Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

  procedure PushSaveDigest();
  var
    iCount:LongInt;
    Commands:Core.Database.Types.Commands;
  begin
    iCount:=0;
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dsFile.Modified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Size,poNone,oNone,dsFile.Size,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Digest,poNone,oNone,dsFile.Digest,Commands);
      Result:=Core.Database.SQL.Update(Task, @Commands);
    finally
      Core.Database.Done(Commands);
    end;
  end;

begin
  Result:=False;

  dsFile.Modified:=Core.Timer.dtUT;
  Case Option of
    dsfileSaveAll      : PushSaveAll();
    dsfileSaveInfo     : PushSaveInfo();
    dsFileSaveKeywords : PushSaveKeywords();
    dsFileSaveCache    : PushSaveCache();
    dsFileSaveDeflate  : PushSaveDeflate();
    dsFileSaveAttribs  : PushSaveAttributes();
    dsFileSaveDigest   : PushSaveDigest();
  end;
end;

class Function  Files.DB.Write(dsFile:TDSFile; const Option:TDSFileSaveOption):Boolean;
begin
  Result:=Write(dsFile.FParent.FFolders.FFAT.FTask,dsFile,Option);
end;

class Function  Files.DB.Write(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; Var Data:Core.Arrays.Types.Bytes):Boolean;
var
  iCount:LongInt;
  iLength:LongInt;
  Commands:Core.Database.Types.Commands;
  dtModified:Double;
  dgDigest:TMD5Digest;

  FS:TFileStream;
begin
  Result:=False;
  Try
    iCount:=0; iLength:=Length(Data); dtModified:=Core.Timer.dtUT;

    Core.Arrays.Bytes.CheckSum(Data,dgDigest);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dtModified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Size,poNone,oNone,iLength,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Digest,poNone,oNone,dgDigest,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);

    if Result then begin
      FS:=nil;
      Try
        Result:=Storage.AuraDisks.Files.Acquire(Node,DomainID,Use.Global,FolderID,ID,Kinds.Domain,FS);
        if (Result=true) then begin
          FS.Size:=0;
          Core.Streams.Write(Data,FS);
        end;
      finally
        FreeAndNil(FS);
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Files.DB.Write(Task:Core.Database.Types.TTask; Var Details:TFileDetails):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Details.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,Details.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ContentType,poNone,oNone,Details.ContentType,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.HasKeywords,poNone,oNone,Details.HasKeywords,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Imported,poNone,oNone,Details.Imported,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cache,poNone,oNone,Details.Cache,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CacheTTL,poNone,oNone,Details.CacheTTL,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Deflate,poNone,oNone,Details.Deflate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Digest,poNone,oNone,Details.Digest,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Created,poNone,oNone,Details.Created,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Details.Modified,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function Files.DB.Read(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Item:TDSFile):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;

begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Item.FID,Commands);
    EnterCriticalSection(Item.Lock);
    Try

        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Imported,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Attributes,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ContentType,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.HasKeywords,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Size,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Cache,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.CacheTTL,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Digest,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Deflate,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Created,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);

        Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GetFile_Object,Item);
    Finally
      LeaveCriticalSection(Item.Lock);
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function Files.DB.Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; Var Data:Core.Arrays.Types.Bytes):Boolean; overload;
var
  FS:TFileStream;
begin
  Result:=false;
  FS:=nil;
  Try
    Result:=Storage.AuraDisks.Files.Acquire(Node,DomainID,Use.Global,FolderID,ID,Kinds.Domain,FS);
    if (Result=true) then begin
      System.SetLength(Data,FS.Size);
      Core.Arrays.Bytes.fromStream(@Data,FS.Size,FS,0);
    end;
  Finally
    FreeAndNil(FS);
  end;
end;

class Function Files.DB.Read(var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; var Stream:TFileStream):Boolean; overload;
begin
  Result:=False;
  FreeAndNil(Stream);
  Result:=Storage.AuraDisks.Files.Acquire(Node,DomainID,Use.Global,FolderID,ID,Kinds.Domain,Stream);
end;

procedure CB_FAT_Folders(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  RequestP:PDSFATRefactor;
  Root:TDSFolders;
  Folder:TDSFolder;
  iID:QWord;
  Attribs:Byte;
  sPath:Core.Strings.VarString;
begin
  RequestP:=DataP;
  Folder:=nil;
  Root:=RequestP^.FAT.Folders;
  iID:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
  sPath:=Fields.FieldByName(Folders.DB.Keys.Path).AsString;
  Attribs:=Fields.FieldByName(Folders.DB.Keys.Attributes).AsInteger;
  Folder:=RequestP^.FAT.Acquire(sPath,RequestP^.Refactor);
  if Folder=nil then
    RequestP^.FAT.Force(Root,Folder,iID,sPath,Attribs,RequestP^.Refactor);
  if Folder<>nil then begin
    Folder.FName:=Core.Utils.Files.Extract(Folder.FPath,epoName);
    Folder.FID:=iID;
    Folder.FAttributes:=Attribs;
  end;
end;

procedure CB_GetFAT_Files_Refresh_Validate(Commands:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  Folder       : TDSFolder;
  dsFile       : TDSFile;
  iID          : QWord;
  iIndex       : LongInt;
  dtModified   : Double;
begin
  Folder:=TDSFolder(DataP);
  iID:=Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt;
  dtModified:=Fields.FieldByName(Files.DB.Keys.Modified).AsFloat;
  iIndex:=Folder.Files.IndexOf(iID);
  If iIndex=-1 then begin
    iIndex:=Folder.Files.Add(iID,0,false);
    dsFile:=Folder.Files.Items[iIndex];
    dsFile.Refresh:=true;
    //dsFile.Stale:=false;
    dsFile.Validated:=true;
  end else begin
    dsFile:=Folder.Files.Items[iIndex];
    dsFile.Refresh:=DateUtils.MilliSecondsBetween(dsFile.Modified,dtModified)>TOL_MODIFIED_MS;
    if dsFile.Refresh then begin
      dsFile.Downloaded:=0; // Should trigger another download when used again
      dsFile.FTemplates.Invalidate();
    end;
    dsFile.Modified:=dtModified;
    dsFile.Validated:=true;
  end;
end;

procedure CB_GetFAT_Files(Commands:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  Folder       : TDSFolder;
  dsFile       : TDSFile;
  iID          : QWord;
  iAttributes  : QWord;
  iIndex       : LongInt;
  bKeywords    : Boolean;
begin
  Folder:=TDSFolder(DataP);
  iID:=Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt;
  bKeywords:=Fields.FieldByName(Files.DB.Keys.HasKeywords).AsBoolean;
  iAttributes:=Fields.FieldByName(Files.DB.Keys.Attributes).AsLargeInt;
  iIndex:=Folder.Files.IndexOf(iID);
  If iIndex=-1 then
    iIndex:=Folder.Files.Add(iID,iAttributes,bKeywords);
  dsFile:=Folder.Files.Items[iIndex];
  {$i Storage.FAT.CB_GetFAT_Files.inc}
end;

procedure CB_GetFAT_File(Commands:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  dsFile       : TDSFile;
begin
  dsFile:=TDSFile(DataP);
  {$i Storage.FAT.CB_GetFAT_Files.inc}
  dsFile.Downloaded:=Core.Timer.dtUT;
end;

procedure CB_FAT_Files_List(Commands:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.LargeWord.Add(Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt,Core.Arrays.Types.PLargeWord(DataP)^);
end;

class Function  Files.DB.Refresh(Task:Core.Database.Types.TTask; FAT:TDSFAT; out Refreshed:QWord; var ReScan:boolean):Boolean;

  procedure PushProcessFiles(Folders:TDSFolders);
  var
    Folder                       : TDSFolder;
    dsFile                       : TDSFile;
    iFolderLcv                   : LongInt;
    iFileLcv                     : LongInt;
    iCount                       : LongInt;
    bHasKeywords                 : Boolean;
    RStart                       : QWord;
    Commands                     : Core.Database.Types.Commands;
    lckFolders                   : TList;
    lckFiles                     : TList;
  begin
    iFolderLcv:=0;
    lckFolders:=Folders.LockList();
    Try
      while iFolderLcv<lckFolders.Count do begin
        RStart:=Refreshed;
        Folder:=TDSFolder(lckFolders[iFolderLcv]);
        Folder.Files.Invalidate();
        Empty(Commands);
        iCount:=0;
        Try
          With Files.DB do begin
            Core.Database.AddCommand(iCount,TableP,@Commands);
            Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.PathID,poNone,oEqual,Folder.FID,Commands);
            Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
            Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);
          end;
          Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GetFAT_Files_Refresh_Validate,Folder);
        Finally
          Empty(Commands);
        end;
        lckFiles:=Folder.Files.LockList();
        Try
          iFileLcv:=0;
          while (iFileLcv<lckFiles.Count) do begin
            dsFile:=TDSFile(lckFiles[iFileLcv]);
            if (dsFile.Refresh) then begin
              iCount:=0;
              Inc(Refreshed);
              bHasKeywords:=dsFile.HasKeywords;
              Try
                Core.Database.AddCommand(iCount,TableP,@Commands);
                Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,dsFile.FID,Commands);
                {$i Storage.FAT.Select.File.inc}
                Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GetFAT_File,dsFile);
                if Result and (bHasKeywords<>dsFile.HasKeywords) then
                  Rescan:=True;
              finally
                Empty(Commands);
              end;
              Inc(iFileLcv);
            end else if (dsFile.Validated=false) and (dsFile.Volatile=false) then begin
              dsFile.Free();
            end else
              Inc(iFileLcv);
          end;
          if (Refreshed>RStart) then
            Folder.Files.Sort();
          PushProcessFiles(Folder.Folders);
        finally
          Folder.Files.UnlockList();
        end;
        Inc(iFolderLcv);
      end;
    finally
      Folders.UnlockList();
    end;
  end;
begin
  ReScan:=False; Refreshed:=0;
  Folders.DB.List(Task,FAT);
  PushProcessFiles(FAT.Folders);
end;


class Function  Files.DB.Basic(Task:Core.Database.Types.TTask; FAT:TDSFAT):Boolean;
  procedure PushProcessFolders(Folders:TDSFolders);
  var
    Folder     : TDSFolder;
    iLcv       : LongInt;
    iCount     : LongInt;
    Commands   : Core.Database.Types.Commands;
    List       : TList;
  begin
    iLcv:=0;
    Try
      List:=Folders.LockList();
      Try
        For iLcv:=0 to List.Count-1 do begin
          Folder:=TDSFolder(List[iLcv]);
          Empty(Commands);
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.PathID,poNone,oEqual,Folder.FID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Name,poNone,oAscending,Commands);
          {$i Storage.FAT.Select.File.inc}
          Core.Database.SQL.Select(Task,@Commands,@CB_GetFAT_Files,Folder);
          Empty(Commands);
          PushProcessFolders(Folder.Folders);
        end;
      finally
        Folders.UnlockList();
      end;
    finally
      Core.Database.Done(Commands);
    end;
  end;
begin
  Result:=False;
  PushProcessFolders(FAT.Folders);
  Result:=True;
end;

class Function  Files.DB.List(Task:Core.Database.Types.TTask; var FolderID:QWord; var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount     : LongInt;
  Commands   : Core.Database.Types.Commands;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.PathID,poNone,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.ID,poNone,oAscending,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FAT_Files_List,@Entries);

  finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Folders.DB.List(Task:Core.Database.Types.TTask; FAT:TDSFAT):Boolean;
var
  iCount     : LongInt;
  Commands   : Core.Database.Types.Commands;
  dsRequest  : TDSFATRefactor;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,FAT.FDomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Path,poNone,oAscending,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Attributes,poNone,oNone,Commands);
    Init(dsRequest,FAT,nil);
    Try
      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FAT_Folders,@dsRequest);
    finally
      Done(dsRequest);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

Function  GetFAT(Task:Core.Database.Types.TTask; FAT:TDSFAT):Boolean;
begin
  Result:=False;
end;

class Function  Files.DB.Exists(Task:Core.Database.Types.TTask; FolderID:QWord; Var Path:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Count:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poNone,oEqual,Path,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.PathID,poAnd,oEqual,FolderID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Files.DB.PrepareCopyFileName(Task:Core.Database.Types.TTask; FolderID:QWord; Var Name:Core.Strings.VarString):Boolean;
Const
  FrmtFileStr='Copy (%s) of %s';
var
  iLcv,iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  sStartName:Core.Strings.VarString;
  Count:QWord;
  // Copy (1) of FileName
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poNone,oEqual,Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.PathID,poAnd,oEqual,FolderID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count=0);
    If Not Result then begin
      iLcv:=1; sStartName:=Name;
      Repeat
        Name:=Format(FrmtFileStr,[IntToStr(iLcv),sStartName]);
        Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count=0);
        Inc(iLcv);
      Until Result;
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;


class Function  Folders.DB.Exists(Task:Core.Database.Types.TTask; DomainID:QWord; Var Path:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Count:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poAnd,oEqual,Path,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Files.DB.Create(Task:Core.Database.Types.TTask; Folder:TDSFolder; DomainID:QWord; Var FileID:QWord; iFileAge:LongInt; Attributes:QWord; HasKeywords,Deflate,Cache:Boolean; TTL:LongInt; FileName:Core.Strings.VarString; Var Data:Core.Arrays.Types.Bytes; var Digest:TMD5Digest; out FileIndex:LongInt):Boolean;
var
  iCount     : LongInt;
  iSize      : Qword;
  dtStamp    : Double;
  iInsertID  : QWord;
  iReset     : QWord;
  Commands   : Core.Database.Types.Commands;
  dsFile     : TDSFile;
  FS         : TFileStream;
begin
  Result:=False;
  Try
    FileIndex:=-1; iCount:=0; iReset:=0; FS:=nil;
    iInsertID:=Random(High(Integer)); FileID:=0; dtStamp:=Core.Timer.dtUT;
    iSize:=Length(Data);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,FileID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.PathID,poNone,oNone,Folder.FID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Imported,poNone,oNone,iFileAge,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,dtStamp,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,dtStamp,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cache,poNone,oNone,Cache,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.CacheTTL,poNone,oNone,TTL,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Digest,poNone,oNone,Digest,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Size,poNone,oNone,iSize,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Attributes,poNone,oNone,Attributes,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.HasKeywords,poNone,oNone,HasKeywords,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Deflate,poNone,oNone,Deflate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Name,poNone,oNone,FileName,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    If Result then begin
      FileIndex:=Folder.Files.Add(FileID,Attributes,HasKeywords,FileName);
      dsFile:=Folder.Files.Items[FileIndex];
      dsFile.Cache:=Cache;
      dsFile.Deflate:=Deflate;
      dsFile.HasKeywords:=HasKeywords;
      dsFile.Created:=dtStamp;
      dsFile.Modified:=dtStamp;
      dsFile.Size:=iSize;
      dsFile.Downloaded:=dtStamp;
      Try
        Storage.AuraDisks.Files.Acquire(Folder.FOwner.FFAT.AuraDiskNode,DomainID,Use.Global,Folder.FID,dsFile.ID,Kinds.Domain,FS);
        if (FS<>nil) then
          Core.Streams.fromData(Data,FS);
      finally
        FreeAndNil(FS);
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Verify_FOLDER_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Database.Types.PLargeWord(DataP)^:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
end;

procedure CB_Verify_FILE_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Database.Types.PLargeWord(DataP)^:=Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt;
end;
class Function  Folders.DB.Verify(Task:Core.Database.Types.TTask; DomainID:QWord; Var Path:Core.Strings.VarString; Var ID:QWord):Boolean;
var
  iCount            : LongInt;
  Commands          : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    ID:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poAnd,oEqual,Path,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Verify_FOLDER_ID,@ID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Files.DB.Verify(Task:Core.Database.Types.TTask; DomainID:QWord; Var Path:Core.Strings.VarString; Var FolderID,FileID:QWord):Boolean;
var
  iCount            : LongInt;
  Commands          : Core.Database.Types.Commands;
  FolderName        : Core.Strings.VarString;
  FileName          : Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0;
    FolderID:=0;
    FileID:=0;
    FolderName:=Core.Utils.Files.Extract(Path,epoAllButName);
    FileName:=Core.Utils.Files.Extract(Path,epoName);
    Core.Database.AddCommand(iCount,Folders.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Folders.DB.TableP,useForFields,Folders.DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Folders.DB.TableP,useForCriteria,Folders.DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,Folders.DB.TableP,useForCriteria,Folders.DB.IDs.Path,poAnd,oEqual,FolderName,Commands);

    Core.Database.SQL.Select(Task,@Commands,@CB_Verify_FOLDER_ID,@FolderID);

    if (FolderID>0) then begin
      iCount:=0;
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poNone,oEqual,FileName,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.PathID,poAnd,oEqual,FolderID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Verify_FILE_ID,@FileID);
    end;

  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Folders.DB.Create(Task:Core.Database.Types.TTask; Folders:TDSFolders; DomainID:QWord; Path:Core.Strings.VarString; Var FolderID:QWord; Attributes:Byte):Boolean;
var
  iCount            : LongInt;
  iInsertID,iReset  : QWord;
  Commands          : Core.Database.Types.Commands;
begin
  Result:=False;
  FolderID:=0;
  Result:=Verify(Task,DomainID,Path,FolderID);
  if (FolderID=0) then begin
    Try
      iCount:=0;
      iReset:=0;
      iInsertID:=Random(High(Integer));
      // Setup Primary ID
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,FolderID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Path,poNone,oNone,Path,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Attributes,poNone,oNone,Attributes,Commands);

      Result:=Core.Database.SQL.Insert(Task,@Commands);
      If Result then
        Folders.Add(FolderID,Path,Attributes);
    Finally
      Core.Database.Done(Commands);
    End;
  end else if Folders.IndexOf(FolderId)=-1 then begin
    Folders.Add(FolderID,Path,Attributes);
  end;
end;

class Function  Folders.DB.Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,ID:QWord):Boolean;
var
  iCount:LongInt;
  iLcv:LongInt;
  fIDs:Core.Arrays.Types.LargeWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ID,Commands);

      Result:=Core.Database.SQL.Delete(Task,@Commands);

    Finally
      Core.Database.Empty(Commands);
      iCount:=0;
    End;
    // Now we have to list all files in teh folder and delete all AuDisk Files

    Try
      Storage.FAT.Files.DB.List(Task,ID,fIDs);

      for iLcv:=0 to High(fIDs) do
        Storage.AuraDisks.Files.Delete(Node,DomainID,Use.Global,ID,fIDs[iLcv],Kinds.Domain);
    Finally
      Core.Arrays.LargeWord.Done(fIDs);
    end;
    // Delete All SQL Entries
    Try
      iCount:=0;
      Core.Database.AddCommand(iCount,Files.DB.TableP,@Commands);
      Core.Database.AddCommand(iCount,Files.DB.TableP,useForCriteria,Files.DB.IDs.PathID,poNone,oEqual,ID,Commands);
      Result:=Core.Database.SQL.Delete(Task,@Commands);
    finally
      Core.Database.Empty(Commands);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Files.DB.Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);

    Storage.AuraDisks.Files.Delete(Node,DomainID,Use.Global,FolderID,ID,Kinds.Domain);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Files.DB.Edit(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,FolderID,ID:QWord; Var Digest:TMD5Digest; Var Data:Core.Arrays.Types.Bytes):Boolean; overload;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  dtModified:Double;
  FS:TFileStream;
begin
  Result:=False;
  Try
    iCount:=0; dtModified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Digest,poNone,oEqual,Digest,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dtModified,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);

    FS:=nil;
    Try
      Storage.AuraDisks.Files.Acquire(Node,DomainID,Use.Global,FolderID,ID,Kinds.Domain,FS);
      if FS<>nil then
        Core.Streams.Copy(Data,FS);
    finally
      FreeAndNil(FS);
    end;

  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Files.DB.Rename(Task:Core.Database.Types.TTask; ID:QWord; Var Name:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,Name,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Folders.DB.Rename(Task:Core.Database.Types.TTask; ID:QWord; Var Path:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Path,poNone,oNone,Path,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Folders.DB.SetAttributes(Task:Core.Database.Types.TTask; FolderID:QWord; Attributes:Byte):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Attributes,poNone,oNone,Attributes,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure TComponentList.Clear;
var
  iLcv:LongInt;
  itmP:PTemplateComponent;
begin
  for iLcv:=0 to Count-1 do begin
    itmP:=Items[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  Inherited Clear;
end;

procedure TComponentList.Invalidate;
var
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do
    Items[iLcv]^.Valid:=False;
end;

procedure TComponentList.Assign(Source:TComponentList);
var
  iLcv:LongInt;
begin
  inherited Clear();
  for iLcv:=0 to Source.Count-1 do begin
    Add(Source[iLcv]^);
    Source[iLcv]:=nil;
  end;
  Source.Pack();
end;

procedure TComponentList.Purge;
var
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do begin
    if not Items[iLcv]^.Valid then begin
      Done(Items[iLcv]^);
      Dispose(Items[iLcv]);
      Items[iLcv]:=nil;
    end;
  end;
  Pack();
end;

destructor TComponentList.Destroy;
begin
  Clear();
  inherited Destroy();
end;

function TComponentList.Find(Name:Core.Strings.VarString; out Item:PTemplateComponent):boolean;
var
  iLcv:LongInt;
begin
  Result:=false; Item:=nil;
  for iLcv:=0 to Count-1 do begin
    if SameText(Items[iLcv]^.Name,Name) then begin
      Result:=True;
      Item:=Items[iLcv];
      Break;
    end;
  end;
end;

function TComponentList.BuildStyles(Folder:TDSFolder; Refactor:TMemoryStream):Core.Strings.VarString;
var
  iLcv:LongInt;
  FS:TFileStream;
begin
  Refactor.Clear;
  for iLcv:=0 to Count-1 do begin
    if (Items[iLcv]^.Style.Storage<>nil) then begin
      FS:=Items[iLcv]^.Style.Storage.AcquireData;
      Try
        FS.Position:=0;
        Refactor.CopyFrom(FS,FS.Size);
      finally
        FreeAndNil(FS);
      end;
    end;
  end;
  Result:=Core.Streams.toString(Refactor);
  Refactor.Clear;
end;

function TComponentList.BuildContent(Folder:TDSFolder; Refactor:TMemoryStream):Core.Strings.VarString;
var
  iLcv:LongInt;
  FS:TFileStream;
begin
  Refactor.Clear;
  for iLcv:=0 to Count-1 do begin
    if (Items[iLcv]^.Content.Storage<>nil) then begin
      FS:=Items[iLcv]^.Content.Storage.AcquireData;
      Try
        FS.Position:=0;
        Refactor.CopyFrom(FS,FS.Size);
      finally
        FreeAndNil(FS);
      end;
    end;
  end;
  Result:=Core.Streams.toString(Refactor);
  Refactor.Clear;
end;

function TComponentList.BuildJavaScript(Folder:TDSFolder; Refactor:TMemoryStream):Core.Strings.VarString;
var
  iLcv:LongInt;
  FS:TFileStream;
begin
  Refactor.Clear;
  for iLcv:=0 to Count-1 do begin
    if (Items[iLcv]^.Code.Storage<>nil) then begin
      FS:=Items[iLcv]^.Code.Storage.AcquireData;
      Try
        FS.Position:=0;
        Refactor.CopyFrom(FS,FS.Size);
      finally
        FreeAndNil(FS);
      end;
    end;
  end;
  Result:=Core.Streams.toString(Refactor);
  Refactor.Clear;
end;

procedure TTemplateList.Add(Template:TTemplate);
var
  idx:LongInt;
begin
  idx:=IndexOf(Template);
  if idx=-1 then
    Inherited Add(Template);
end;

procedure TTemplateList.Invalidate;
var
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do
    Items[iLcv].FStale:=true;
end;

procedure TComponentList.Empty(var Item:TTemplateComponent);
begin
  Item.Valid:=False;
  Item.Stale:=False;
  SetLength(Item.Name,0);
  Empty(Item.Combination);
  Empty(Item.Content);
  Empty(Item.Code);
  Empty(Item.Style);
end;

procedure TComponentList.Empty(var Item:TTemplateResource);
begin
  SetLength(Item.Name,0);
  Item.Storage:=nil;
  Item.Modified:=0;
  Item.Kind:=trkNone;
end;

procedure TComponentList.Init(var Item:TTemplateComponent);
begin
  Item.Valid:=False;
  SetLength(Item.Name,0);
  Init(Item.Content);
  Init(Item.Code);
  Init(Item.Style);
end;

procedure TComponentList.Init(var Item:TTemplateResource);
begin
  SetLength(Item.Name,0);
  Item.Storage:=nil;
  Item.Modified:=0;
  Item.Kind:=trkNone;
end;

procedure TComponentList.Done(var Item:TTemplateComponent);
begin
  Finalize(Item.Name);
  Done(Item.Content);
  Done(Item.Code);
  Done(Item.Style);
  Finalize(Item);
end;

procedure TComponentList.Done(var Item:TTemplateResource);
begin
  Finalize(Item.Name);
  Item.Storage:=nil;
  Item.Modified:=0;
  Finalize(Item);
end;

constructor TTemplate.Create(aModified:double);
begin
  FKWFModified:=0;
  FStale:=false;
  FModified:=aModified;
  FLines:=TStringList.Create;
  FRefactor:=TMemoryStream.Create();
  FComponents:=TComponentList.Create();

  InitCriticalSection(FLock);
  Inherited Create;
end;

destructor TTemplate.Destroy();
begin
  FreeAndNil(FLines);
  Finalize(FEmbeddedScripts);
  Core.Arrays.VarString.Done(FJavaScripts);
  Core.Arrays.VarString.Done(FStyleSheets);
  DoneCriticalSection(FLock);
  FreeAndNil(FComponents);
  FreeAndNil(FRefactor);
  Inherited Destroy;
end;

procedure TTemplate.Invalidate();
begin
  FStale:=True;
  FModified:=0;
end;

function  TTemplate.BuildJavaScripts(Refactor:TMemoryStream):Core.Strings.VarString;
var
  iLcv:LongInt;
  sWrite:Core.Strings.VarString;
  dsFile:TDSFile;
  FS:TFileStream;
begin
  Refactor.Clear;
  Try
    if FScriptsEmbedded then begin
      for iLcv:=0 to High(FEmbeddedScripts) do begin
        Refactor.Write(FMT_JAVASCRIPT_EMBEDDED_START[1],FMT_JAVASCRIPT_EMBEDDED_START_LEN);
        dsFile:=FEmbeddedScripts[iLcv];
        if (dsFile<>nil) then begin
          FS:=dsFile.AcquireData();
          Try
            FS.Position:=0;
            Refactor.CopyFrom(FS,FS.Size);
          finally
            FreeAndNil(FS);
          end;
        end;
        Refactor.Write(FMT_JAVASCRIPT_EMBEDDED_END[1],FMT_JAVASCRIPT_EMBEDDED_END_LEN);
      end;
    end else begin
      for iLcv:=0 to High(FJavaScripts) do begin
        sWrite:=Format(FMT_JAVASCRIPT,[FJavaScripts[iLcv]]);
        Refactor.Write(sWrite[1],Length(sWrite));
      end;
    end;
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Clear;
  end;
end;


procedure  TTemplate.BuildJavaScripts(Dest:TMemoryStream; Refactor:TMemoryStream);
var
  iLcv:LongInt;
  dsFile:TDSFile;
  FS:TFileStream;
begin
  Dest.Clear();
  Refactor.Clear();
  for iLcv:=0 to High(FEmbeddedScripts) do begin
    dsFile:=FEmbeddedScripts[iLcv];
    if (dsFile<>nil) then begin
      if (dsFile.Volatile=true) then begin
        dsFile.FMemory.Position:=0;
        Dest.CopyFrom(dsFile.FMemory,dsFile.FMemory.Size);
      end else begin
        FS:=dsFile.AcquireData();
        Try
          FS.Position:=0;
          Dest.CopyFrom(FS,FS.Size);
        finally
          FreeAndNil(FS);
        end;
      end;
      Dest.Write(#13#10,2);
    end;
  end;
end;

procedure TTemplate.Reset();
begin
  SetLength(FWrapperClass,0);
  SetLength(FWrapperID,0);
  SetLength(FSEOFollow,0);

  SetLength(FLinks,0);
  SetLength(FTitle,0);
  SetLength(FCacheManifest,0);
  SetLength(FName,0);
  SetLength(FDescription,0);
  SetLength(FKeywords,0);
  SetLength(FOutputFileName,0);

  Core.Arrays.VarString.Empty(FJavaScripts);
  Core.Arrays.VarString.Empty(FStyleSheets);
end;

function  TTemplate.GetKind(sValue:Core.Strings.VarString):TTemplateKind;
var
  iLcv:TTemplateKind;
begin
   Result:=TTemplateKind.tkPage;
   for iLcv:=Low(TTemplateKind) to High(TTemplateKind) do begin
     if SameText(sValue,TemplateKind[iLcv]) then begin
       Result:=iLcv;
       Exit;
     end;
   end;
end;

function  TTemplate.GetResourceKind(sValue:Core.Strings.VarString):TTemplateResourceKind;
var
  iLcv:TTemplateResourceKind;
begin
   Result:=TTemplateResourceKind.trkNone;
   for iLcv:=Low(TTemplateResourceKind) to High(TTemplateResourceKind) do begin
     if SameText(sValue,ResourceKind[iLcv]) then begin
       Result:=iLcv;
       Exit;
     end;
   end;
end;

function  TTemplate.BuildStyleSheets(Refactor:TMemoryStream):Core.Strings.VarString;
var
  iLcv:LongInt;
  sWrite:Core.Strings.VarString;
begin
  Refactor.Clear;
  Try
    for iLcv:=0 to High(FStyleSheets) do begin
      sWrite:=Format(FMT_STYLESHEET,[FStyleSheets[iLcv]]);
      Refactor.Write(sWrite[1],Length(sWrite));
    end;
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Clear;
  end;
end;

procedure TTemplate.Load(Task:Core.Database.Types.TTask; Folder:TDSFolder; Item:TDSFile);
var
  FAT          : TDSFAT;
  ForceWrite   : boolean;
  fsTemplate   : TFileStream;
  msTemplate   : TMemoryStream;

  procedure PushGenerateContent(Dest:TMemoryStream);
  var
    iLcv:LongInt;
    dsFile:TDSFile;
    FS:TFileStream;
  begin
    Dest.Clear();
    FRefactor.Clear();
    for iLcv:=0 to High(FEmbeddedScripts) do begin
      dsFile:=FEmbeddedScripts[iLcv];
      if (dsFile<>nil) then begin
        if (dsFile.Volatile=true) then begin
          dsFile.FMemory.Position:=0;
          Dest.CopyFrom(dsFile.FMemory,dsFile.FMemory.Size);
        end else begin
          FS:=dsFile.AcquireData();
          Try
            FS.Position:=0;
            Dest.CopyFrom(FS,FS.Size);
          finally
            FreeAndNil(FS);
          end;
        end;
        Dest.Write(#13#10,2);
      end;
    end;
  end;

  procedure PushGenerateJavaScript();
  begin
    BuildJavaScripts(FOutput.FMemory,FRefactor);
    System.SetLength(FEmbeddedScripts,0);
    EnterCriticalSection(FOutput.Lock);
    Try
      Core.Streams.CheckSum(FOutput.FMemory,FOutput.Digest);
      FOutput.Size:=FOutput.FMemory.Size;
      FOutput.Modified:=Core.Timer.dtUT;
    Finally
      LeaveCriticalSection(FOutput.Lock);
    end;
  end;

  procedure PushProcessJavaScript();
  var
    iLcv:LongInt;
    sValue:Core.Strings.VarString;
    saList:Core.Arrays.Types.VarString;
  begin
    FOutputFileName:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_OUTPUT,DEFAULT_FILE);
    if not Folder.Files.Find(FOutput,FOutputFileName) then begin
      ForceWrite:=true;
      FOutput:=TDSFile.Create(Folder,0,FS_ATTR_NONE,0,0,FAT_KEYWORDS_ON,NO_CACHE,Core.Timer.dtUT,Core.Timer.dtUT,FOutputFileName);
      Folder.Files.Add(FOutput);
    end;
    if (FOutput.FMemory=nil) then begin
      ForceWrite:=true;
      FOutput.FMemory:=TMemoryStream.Create();
      FOutput.FVolatile:=true;
    end;

    FOutput.Cache:=FManifest.ReadBool(INI_SEC_TEMPLATE,INI_VAL_CACHE,false);
    FOutput.CacheTTL:=FManifest.ReadInteger(INI_SEC_TEMPLATE,INI_VAL_TTL,0);
    FOutput.Deflate:=FManifest.ReadBool(INI_SEC_TEMPLATE,INI_VAL_COMPRESS,true);
    FTitle:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_TITLE,'');
    FDescription:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_DESCRIPTION,'');

    FName:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_NAME,'');
    FOutput.FHasKeywords:=FManifest.ReadBool(INI_SEC_TEMPLATE,INI_VAL_KEYWORDS,false);
    FScriptCount:=FManifest.ReadInteger(INI_SEC_SCRIPTS,INI_VAL_COUNT,0);

    Core.Arrays.VarString.Empty(saList);
    Try
      for iLcv:=1 to FScriptCount do begin
        sValue:=FManifest.ReadString(INI_SEC_SCRIPTS,IntToStr(iLcv),'');
        if Length(sValue)>0 then
          Core.Arrays.VarString.Add(saList,sValue,[aoOverwriteDuplicate]);
      end;
      Core.Arrays.VarString.Copy(saList,FJavaScripts);
      FScriptCount:=System.Length(FJavaScripts);
      System.SetLength(FEmbeddedScripts,FScriptCount);
      for iLcv:=0 to FScriptCount-1 do begin
        FEmbeddedScripts[iLcv]:=nil;
        if FAT.Acquire(FJavaScripts[iLcv],FRefactor,FEmbeddedScripts[iLcv]) then begin
          FEmbeddedScripts[iLcv].AddToTemplates(Self); // will no add if already present;
          if (FEmbeddedScripts[iLcv].Downloaded=0) then begin
            FEmbeddedScripts[iLcv].Load(Task);
            ForceWrite:=true;
          end;
        end;
      end;

    finally
      Core.Arrays.VarString.Empty(saList);
    end;
    if (ForceWrite=true) then
      PushGenerateJavaScript();
  end;

  procedure PushGeneratePage();
  var
    sOnLoad   : Core.Strings.VarString;
    sOnResize : Core.Strings.VarString;
    sOutput   : Array[TPageFormat] of Core.Strings.VarString;
    sWrite    : Core.Strings.VarString;
    pfLcv     : TPageFormat;
  begin
    sOutput[pfName]:=FName;
    sOutput[pfMetaDescription]:=FDescription;
    sOutput[pfMetaKeywords]:=FKeywords;
    sOutput[pfStyles]:=FComponents.BuildStyles(Folder,FRefactor);
    sOutput[pfLinks]:=FLinks;
    sOutput[pfScripts]:=FComponents.BuildJavaScript(Folder,FRefactor);
    sOutput[pfTitle]:=FTitle;
    sOutput[pfCacheManifest]:=FCacheManifest;
    sOutput[pfComponents]:=FComponents.BuildContent(Folder,FRefactor);
    sOutput[pfMetaViewport]:=FManifest.ReadString(INI_SEC_META,INI_VAL_VIEWPORT,'');
    sOutput[pfBodyDec]:='body';
    sOnLoad:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_ONLOAD,'');
    sOnResize:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_ONRESIZE,'');
    sOutput[pfMetaAppleWebApp]:=FManifest.ReadString(INI_SEC_META,INI_VAL_APPLE_WEB_APP,'');
    sOutput[pfMetaAppleStatusBar]:=FManifest.ReadString(INI_SEC_META,INI_VAL_APPLE_WEB_STATUSBAR,'');
    sOutput[pfMetaAppleFullscreen]:=FManifest.ReadString(INI_SEC_META,INI_VAL_APPLE_TOUCH_FULLSCREEN,'');
    sOutput[pfDocType]:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_DOCTYPE,'');
    sOutput[pfCharset]:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_CHARSET,'UTF-8');
    sOutput[pfExternalScripts]:=BuildJavaScripts(FRefactor);
    sOutput[pfExternalStyles]:=BuildStyleSheets(FRefactor);
    sOutput[pfSEOFollow]:=FSEOFollow;
    if FWrapped then begin
      sOutput[pfWrapperPre]:=Format(FMT_WRAPPER_PRE_CLASS[Length(FWrapperID)>0],[FWrapperClass,FWrapperID]);
      sOutput[pfWrapperPost]:=FMT_WRAPPER_POST;
    end else begin
      sOutput[pfWrapperPre]:='';
      sOutput[pfWrapperPost]:='';
    end;
    if Length(sOnLoad)>0 then
      sOutput[pfBodyDec]:=Concat(sOutput[pfBodyDec],' onload="',sOnLoad,'"');
    if Length(sOnResize)>0 then
      sOutput[pfBodyDec]:=Concat(sOutput[pfBodyDec],' onresize="',sOnResize,'"');
    sWrite:=Format(
      FMT_PAGE,[
        {0} sOutput[pfName],
        {1} sOutput[pfMetaDescription],
        {2} sOutput[pfMetaKeywords],
        {3} sOutput[pfStyles],
        {4} sOutput[pfScripts],
        {5} sOutput[pfTitle],
        {6} sOutput[pfComponents],
        {7} sOutput[pfMetaViewport],
        {8} sOutput[pfBodyDec],
        {9} sOutput[pfMetaAppleWebApp],
        {10} sOutput[pfMetaAppleStatusBar],
        {11} sOutput[pfMetaAppleFullscreen],
        {12} sOutput[pfWrapperPre],
        {13} sOutput[pfWrapperPost],
        {14} sOutput[pfExternalScripts],
        {15} sOutput[pfExternalStyles],
        {16} sOutput[pfCacheManifest],
        {17} sOutput[pfDocType],
        {18} sOutput[pfCharset],
        {19} sOutput[pfLinks],
        {20} sOutput[pfSEOFollow]
      ]
    );

    System.SetLength(FEmbeddedScripts,0);

    EnterCriticalSection(FOutput.Lock);
    Try
      Core.Streams.ReWrite(sWrite,FOutput.FMemory);
      FOutput.Size:=FOutput.FMemory.Size;
      Core.Streams.CheckSum(FOutput.FMemory,FOutput.Digest);
      FOutput.Modified:=Core.Timer.dtUT;
    Finally
      LeaveCriticalSection(FOutput.Lock);
    end;
    for pfLcv:=Low(TPageFormat) to High(TPageFormat) do
      SetLength(sOutput[pfLcv],0);
  end;

  function ForceComponent(sName:Core.Strings.VarString):PTemplateComponent;
  begin
    Result:=nil;
    // Also must enforce order
    if not FComponents.Find(sName,Result) then begin
      New(Result);
      FComponents.Init(Result^);
      Result^.Name:=sName;
      FComponents.Add(Result^);
    end;
    Result^.Valid:=true;
  end;

  procedure ForceCombination(var tcItem : TTemplateComponent; const Kind:TTemplateResourceKind=trkNone);
  var
    iLcv         : LongInt;
    iCt          : LongInt;
    itmP         : PTemplateComponent;
    sFileName    : Core.Strings.VarString;
    dsFile       : TDSFile;
  begin
    iCt:=FManifest.ReadInteger(tcItem.Name,INI_VAL_COUNT,0);
    for iLcv:=1 to iCt do begin
      sFileName:=FManifest.ReadString(tcItem.Name,IntToStr(iLcv),'');
      if FAT.Acquire(sFileName,FRefactor,dsFile) then begin
        itmP:=ForceComponent(sFileName);
        itmP^.Content.Kind:=Kind;
        itmP^.Content.Storage:=dsFile;
        itmP^.Valid:=true;
        dsFile.AddToTemplates(Self);
        if (itmP^.Content.Modified<>dsFile.Modified) then begin
          tcItem.Stale:=true;
          itmP^.Stale:=true;
          itmP^.Content.Modified:=dsFile.Modified;
        end;
      end;
    end;
  end;

  procedure OutputCombination(var tcItem:TTemplateComponent);
  var
    iLcv:LongInt;
    iCt:LongInt;
    itmP:PTemplateComponent;
    sFileName:Core.Strings.VarString;
  begin
    iCt:=FManifest.ReadInteger(tcItem.Name,INI_VAL_COUNT,0);
    EnterCriticalSection(FOutput.Lock);
    Try
      FOutput.FMemory.Size:=0;
      for iLcv:=1 to iCt do begin
        sFileName:=FManifest.ReadString(tcItem.Name,IntToStr(iLcv),'');
        itmP:=ForceComponent(sFileName);
        if (itmP<>nil) and (itmP^.Content.Storage<>nil) then begin
          FRefactor.Size:=0;
          itmP^.Content.Storage.toStream(FRefactor);
          itmP^.Stale:=false;
          FRefactor.Position:=0;
          FOutput.FMemory.CopyFrom(FRefactor,FRefactor.Size);
          FRefactor.Size:=0;
          FOutput.FMemory.Write(#13#10,2);
        end;
      end;
      FOutput.Size:=FOutput.FMemory.Size;
      FOutput.Modified:=Core.Timer.dtUT;
      Core.Streams.CheckSum(FOutput.FMemory,FOutput.Digest);
    finally
      LeaveCriticalSection(FOutput.Lock);
    end;
  end;

  procedure ProcessCombination(var tcItem:TTemplateComponent);
  begin
    FOutputFileName:=FManifest.ReadString(tcItem.Name,INI_VAL_OUTPUT,DEFAULT_FILE);
    if not Folder.Files.Find(FOutput,FOutputFileName) then begin
      tcItem.Stale:=true;
      FOutput:=TDSFile.Create(Folder,0,FS_ATTR_NONE,0,0,FAT_KEYWORDS_ON,NO_CACHE,Core.Timer.dtUT,Core.Timer.dtUT,FOutputFileName);
      Folder.Files.Add(FOutput);
    end;
    if (FOutput.FMemory=nil) then begin
      FOutput.FMemory:=TMemoryStream.Create();
      FOutput.FVolatile:=true;
      tcItem.Stale:=true;
    end;
    if (tcItem.Stale=true) or (ForceWrite=true) then begin
      FOutput.Cache:=FManifest.ReadBool(tcItem.Name,INI_VAL_CACHE,false);
      FOutput.CacheTTL:=FManifest.ReadInteger(tcItem.Name,INI_VAL_TTL,0);
      FOutput.Deflate:=FManifest.ReadBool(tcItem.Name,INI_VAL_COMPRESS,true);
      OutputCombination(tcItem);
    end;

  end;

  procedure PushProcessCombinations();
  var
    saList       : Core.Arrays.Types.VarString;
    iLcv         : LongInt;
    tcItemP      : PTemplateComponent;
    rcKind       : TTemplateResourceKind;
  begin
    Core.Arrays.VarString.fromString(@saList,FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_COMPONENTS,''),',');
    try
      FComponents.Invalidate();
      for iLcv:=0 to System.High(saList) do begin
        tcItemP:=ForceComponent(saList[iLcv]);
        tcItemP^.Combination.Kind:=trkCombination;
        rcKind:=GetResourceKind(FManifest.ReadString(tcItemP^.Name,INI_VAL_KIND,ResourceKind[trkNone]));
        ForceCombination(tcItemP^,rcKind);
      end;
      FComponents.Purge();
      for iLcv:=0 to FComponents.Count-1 do begin
        tcItemP:=FComponents.Items[iLcv];
        if (tcItemP^.Combination.Kind=trkCombination) then
          ProcessCombination(tcItemP^);
        tcItemP^.Stale:=false;
      end;
    finally
      Core.Arrays.VarString.Done(saList);
    end;
  end;

  procedure PushProcessPage();
  var
    saList       : Core.Arrays.Types.VarString;
    iLcv         : LongInt;
    sValue       : Core.Strings.VarString;
    tcItemP      : PTemplateComponent;
  begin
    Core.Arrays.VarString.fromString(@saList,FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_COMPONENTS,''),',');
    try
      FComponents.Invalidate();

      for iLcv:=0 to High(saList) do begin
        tcItemP:=ForceComponent(saList[iLcv]);
        // check content item
        tcItemP^.Content.Name:=FManifest.ReadString(tcItemP^.Name,INI_VAL_CONTENT,Concat(tcItemP^.Name,'.html'));
        if Folder.Files.Find(tcItemP^.Content.Storage,tcItemP^.Content.Name) then begin
          tcItemP^.Content.Storage.AddToTemplates(Self); // will no add if already present
          if tcItemP^.Content.Storage.Downloaded=0 then
            tcItemP^.Content.Storage.Load(Task);
          if (tcItemP^.Content.Storage.Modified<>tcItemP^.Content.Modified) then begin
            tcItemP^.Content.Modified:=tcItemP^.Content.Storage.Modified;
            tcItemP^.Valid:=true;
            ForceWrite:=true;
          end;
        end;
        // check code item
        tcItemP^.Code.Name:=FManifest.ReadString(tcItemP^.Name,INI_VAL_CODE,Concat(tcItemP^.Name,'.js'));
        if Folder.Files.Find(tcItemP^.Code.Storage,tcItemP^.Code.Name) then begin
          tcItemP^.Code.Storage.AddToTemplates(Self); // will no add if already present
          if tcItemP^.Code.Storage.Downloaded=0 then
            tcItemP^.Code.Storage.Load(Task);
          if (tcItemP^.Code.Storage.Modified<>tcItemP^.Code.Modified) then begin
            tcItemP^.Code.Modified:=tcItemP^.Code.Storage.Modified;
            tcItemP^.Valid:=true;
            ForceWrite:=true;
          end;
        end;
        // check style item
        tcItemP^.Style.Name:=FManifest.ReadString(tcItemP^.Name,INI_VAL_STYLE,Concat(tcItemP^.Name,'.css'));
        if Folder.Files.Find(tcItemP^.Style.Storage,tcItemP^.Style.Name) then begin
          tcItemP^.Style.Storage.AddToTemplates(Self); // will no add if already present
          if tcItemP^.Style.Storage.Downloaded=0 then
            tcItemP^.Style.Storage.Load(Task);
          if (tcItemP^.Style.Storage.Modified<>tcItemP^.Style.Modified) then begin
            tcItemP^.Style.Modified:=tcItemP^.Style.Storage.Modified;
            tcItemP^.Valid:=true;
            ForceWrite:=true;
          end;
        end;
      end;
    finally
      Core.Arrays.VarString.Empty(saList);
    end;
    FComponents.Purge();
    FOutputFileName:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_OUTPUT,DEFAULT_FILE);
    if not Folder.Files.Find(FOutput,FOutputFileName) then begin
      ForceWrite:=true;
      FOutput:=TDSFile.Create(Folder,0,FS_ATTR_NONE,0,0,FAT_KEYWORDS_ON,NO_CACHE,Core.Timer.dtUT,Core.Timer.dtUT,FOutputFileName);
      Folder.Files.Add(FOutput);
    end;
    if (FOutput.FMemory=nil) then begin
      FOutput.FMemory:=TMemoryStream.Create();
      FOutput.FVolatile:=true;
      ForceWrite:=True;
    end;

    FOutput.Cache:=FManifest.ReadBool(INI_SEC_TEMPLATE,INI_VAL_CACHE,false);
    FOutput.CacheTTL:=FManifest.ReadInteger(INI_SEC_TEMPLATE,INI_VAL_TTL,0);
    FOutput.Deflate:=FManifest.ReadBool(INI_SEC_TEMPLATE,INI_VAL_COMPRESS,true);
    FTitle:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_TITLE,'');
    FName:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_NAME,'');
    FDescription:=FManifest.ReadString(INI_SEC_META,INI_VAL_DESCRIPTION,'');
    FKeywords:=FManifest.ReadString(INI_SEC_META,INI_VAL_KEYWORDS,'');
    FWrapped:=FManifest.ReadBool(INI_SEC_WRAPPER,INI_VAL_ENABLED,false);
    FWrapperClass:=FManifest.ReadString(INI_SEC_WRAPPER,INI_VAL_CLASS,'');
    FWrapperID:=FManifest.ReadString(INI_SEC_WRAPPER,INI_VAL_ID,'');
    FScriptCount:=FManifest.ReadInteger(INI_SEC_SCRIPTS,INI_VAL_COUNT,0);
    FCacheManifest:=FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_CACHE_MANIFEST,'');
    if System.Length(FCacheManifest)>0 then
      FCacheManifest:=Concat(' manifest="',FCacheManifest,'"');

    FScriptsEmbedded:=FManifest.ReadBool(INI_SEC_SCRIPTS,INI_VAL_EMBEDDED,false);
    Try
      for iLcv:=1 to FScriptCount do begin
        sValue:=FManifest.ReadString(INI_SEC_SCRIPTS,IntToStr(iLcv),'');
        if Length(sValue)>0 then
          Core.Arrays.VarString.Add(saList,sValue,[aoOverwriteDuplicate]);
      end;
      Core.Arrays.VarString.Copy(saList,FJavaScripts);
      FScriptCount:=System.Length(FJavaScripts);
      if FScriptsEmbedded then begin
        System.SetLength(FEmbeddedScripts,FScriptCount);
        for iLcv:=0 to FScriptCount-1 do begin
          if FAT.Acquire(FJavaScripts[iLcv],FRefactor,FEmbeddedScripts[iLcv]) then begin
            FEmbeddedScripts[iLcv].AddToTemplates(Self); // will no add if already present;
            if FEmbeddedScripts[iLcv].Downloaded=0 then
              FEmbeddedScripts[iLcv].Load(Task);
          end;
        end;
      end;
    finally
      Core.Arrays.VarString.Empty(saList);
    end;
    FStyleCount:=FManifest.ReadInteger(INI_SEC_STYLES,INI_VAL_COUNT,0);
    Try
      for iLcv:=1 to FStyleCount do begin
        sValue:=FManifest.ReadString(INI_SEC_STYLES,IntToStr(iLcv),'');
        if Length(sValue)>0 then
          Core.Arrays.VarString.Add(saList,sValue,[aoOverwriteDuplicate]);
      end;
      Core.Arrays.VarString.Copy(saList,FStyleSheets);
    finally
      Core.Arrays.VarString.Empty(saList);
    end;
    FLinkCount:=FManifest.ReadInteger(INI_SEC_LINKS,INI_VAL_COUNT,0);
    Try
      for iLcv:=1 to FLinkCount do begin
        sValue:=Format(FMT_LINK,[FManifest.ReadString(INI_SEC_LINKS,IntToStr(iLcv),'')]);
        if Length(sValue)>0 then
          Core.Arrays.VarString.Add(saList,sValue,[aoOverwriteDuplicate]);
      end;
      FLinks:=Core.Arrays.VarString.toString(saList,#13#10,FRefactor,true);
    finally
      Core.Arrays.VarString.Empty(saList);
    end;
    FManifest.ReadSection(INI_SEC_SEO_FOLLOW,FLines);
    Try
      for iLcv:=0 to FLines.Count-1 do begin
        sValue:=FManifest.ReadString(INI_SEC_SEO_FOLLOW,FLines[iLcv],'');
        sValue:=Format(FMT_SEO_FOLLOW,[FLines[iLcv],sValue]);
        Core.Arrays.VarString.Add(saList,sValue,[aoOverwriteDuplicate]);
      end;
      FSEOFollow:=Core.Arrays.VarString.toString(saList,#13#10,FRefactor,true);
    finally
      Core.Arrays.VarString.Empty(saList);
    end;
    if (FModified=0) then
      ForceWrite:=true;
    if ForceWrite then
       PushGeneratePage();
  end;

begin
  Reset();
  EnterCriticalSection(FLock);
  Try
    FAT:=Folder.FOwner.FFAT;
    FTemplate:=Item;
    FManifest:=nil;
    fsTemplate:=nil;
    msTemplate:=nil;
    If (Item.HasKeywords) then begin
      if (Item.FScanP<>nil) and (Item.FScanP^.Read^.Count>0) then begin
        ForceWrite:=(Item.Downloaded=0) or (FStale=true) or (Item.Modified<>FModified) or (FKWFModified<>Item.FScanP^.dtFile);
        msTemplate:=TMemoryStream.Create();
        Core.Streams.Write(Core.Keywords.toString(Item.FScanP^.Read^),msTemplate);
        msTemplate.Position:=0;
        FManifest:=TIniFile.Create(msTemplate);
        FKWFModified:=Item.FScanP^.dtFile;
        FModified:=Item.Modified;
      end else begin
        FModified:=0;
        FKWFModified:=0;
      end;
    end else begin
      fsTemplate:=Item.AcquireData();
      FManifest:=TIniFile.Create(fsTemplate);
      ForceWrite:=(Item.Downloaded=0) or (FStale=true) or (Item.Modified<>FModified);
      FModified:=Item.Modified;
    end;
    Try
      if FManifest<>nil then begin
        Try
          FManifest.CacheUpdates:=True;
          FKind:=GetKind(FManifest.ReadString(INI_SEC_TEMPLATE,INI_VAL_KIND,TemplateKind[tkPage]));
          Case FKind of
            tkPage       : PushProcessPage();
            tkJavaScript : PushProcessJavaScript();
            tkCombine    : PushProcessCombinations();
          end;
          FStale:=False;
        finally
          FreeAndNil(FManifest);
        end;
      end;
    finally
      FreeAndNil(fsTemplate);
      FreeAndNil(msTemplate);
    end;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

class procedure Folders.toXML(List:TList; Output:TStream; Stamp:Boolean);
var
  sData:Core.Strings.VarString;
  iLcv:LongInt;
begin
  if Stamp then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  sData:=Concat('<',Folders.XML.Stanza.Items,'>');
  Core.Streams.Write(sData,Output);
  for iLcv:=0 to List.Count-1 do begin
    sData:=Concat(
      '<',XML.Stanza.Item,'>',
      Core.XML.DB.Print(XML.Fields.ID,TDSFolder(List[iLcv]).FID),
      Core.XML.DB.Print(XML.Fields.Path,TDSFolder(List[iLcv]).FPath),
      '</',XML.Stanza.Item,'>'
    );
    Core.Streams.Write(sData,Output);
  end;
  sData:=Concat('</',XML.Stanza.Items,'>');
  Core.Streams.Write(sData,Output);
end;

class procedure Files.toXML(Item:TDSFile; Output:TStream; Stamp:Boolean);
begin
  if Stamp then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Core.Streams.Write(Item.toXML(),Output);
end;

class procedure Files.toXML(List:TList; Output:TStream; Stamp:Boolean);
var
  sData:Core.Strings.VarString;
  iLcv:LongInt;
  Item:TDSFile;
begin
  if Stamp then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  sData:=Concat('<',XML.Stanza.Items,'>');
  Core.Streams.Write(sData,Output);
  for iLcv:=0 to List.Count-1 do begin
    Item:=TDSFile(List[iLcv]);
    if (Item.Volatile=false) then
      sData:=TDSFile(List[iLcv]).toXML();
    Core.Streams.Write(sData,Output);
  end;
  sData:=Concat('</',XML.Stanza.Items,'>');
  Core.Streams.Write(sData,Output);
end;

procedure StartBackgroundTimers();
begin
  if FATTimer=nil then begin
    FATTimer:=Core.Database.Timer.Create(Storage.Main.Header);
    FATTimer.Task.Name:='Storage.FAT.FATTimer';
  end;
end;

procedure StopBackgroundTimers();
begin
  if (FATTimer<>nil) then  FATTimer.Terminate();
  FATTimer:=nil;
end;


initialization
  RegisterDB();

finalization
  StopBackgroundTimers();
end.


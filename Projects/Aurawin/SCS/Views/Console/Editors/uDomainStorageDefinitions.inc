{
  DomainStorage is where we virtualize files / folder storage for domains

  Domains can store Files and Folders

  Based on Two Tables

  Storage Folder Table  : Stores Directory Structure
  Storage Item Table    : Stores Items with Ptrs back to Folders.

  Requires Access Control Unit
}

Var
  DB_Table_Domain_Storage_Folders        : String;
  DB_Table_Domain_Storage_Files          : String;
  DB_TI_DomainStorage_Folders            : LongInt;
  DB_TI_DomainStorage_Files              : LongInt;

Const

  STORAGE_KIND_FOLDER             = 1 shl 0;
  STORAGE_KIND_FILE               = 1 shl 1;
  WM_SCANFILES                    = WM_USER+1001;

  DS_FN_ID                        = GN_FN_ID;                              // Record Number
  DS_FN_INSERTID                  = GN_FN_INSERT_ID;
  DS_FN_DOMAIN_ID                 = GN_FN_DOMAIN_ID;                       // Domain of Item's Owner
  DS_FN_PATH_ID                   = 'PATH_ID';                             // ID of Path
  DS_FN_KEYWORDS                  = 'CONTENT_KEYWORDS';                    // Content Has Keywords.. Requires Parsing before delivery
  DS_FN_CREATE_DT                 = 'Created_DT';                          // Date Created
  DS_FN_MODIFIED_DT               = 'Modified_DT';                         // Last Modified DT
  DS_FN_PATH                      = 'VIRTUAL_PATH';                                // Location  /images/ or /test/data.png
  DS_FN_DATA                      = GN_FN_DATA;                            // Actual Binary Data
  DS_FN_ACL                       = 'ACL_ID';                              // ACL ID
  DS_FN_NAME                      = 'ITEM_NAME';                           // File name... index.html
  DS_FN_CT                        = 'CONTENT_TYPE';                        // Content Type for pushing out (WORD)
  DS_FN_CACHE                     = 'CACHEABLE';                           // Can Cache Item Contents Typically True unless File is too large

Type

  TDomainStorageItemProperties=(
    domainSPIId,domainSPIInsertID,domainSPIDomainID,domainSPIPathID,domainSPIName,domainSPIACLID,
    domainSPIContentType,domainSPIHasKeywords,domainSPICache,domainSPICreated,domainSPIModified,domainSPIData
  );
  TDomainStorageFolderProperties=(
    domainSPFId,domainSPFInsertID,domainSPFDomainID,domainSPFPath
  );
  TDFile=record
     ID                         : Int64;
     Name                       : Core.Strings.VarString;
  end;
  TDFiles=Array of TDFile;
  TDFolder=record
     ID                         : Int64;
     Path                       : Core.Strings.VarString;
     Files                      : TDFiles;
  end;
  TDDirectory=Array of TDFolder;
  PDDirectory=^TDDirectory;
  PDFolder=^TDFolder;
  PFileDetails=^TFileDetails;
  TFileDetails=Record
    ID                          : Int64;
    Size                        : LongInt;
    Created                     : Double;
    Modified                    : Double;
    ContentType                 : LongInt;
    HasKeywords                 : Boolean;
    Cache                       : Boolean;
    Name                        : Core.Strings.VarString;
  End;
  TDSFile=Record
    ID                          : Int64;
    ACLID                       : Int64;
    LockCount                   : LongInt;
    ContentType                 : LongInt;
    HasKeywords                 : Boolean;
    Cache                       : Boolean;
    DownloadNeeded              : Boolean;
    Created                     : Double;
    Modified                    : Double;
    Name                        : Core.Strings.VarString;
    Data                        : TByteBuffer;
  end;
  PDSFile=^TDSFile;
  TDSFiles=Array of TDSFile;
  TDSFolder=record
    ID                          : Int64;
    Path                        : Core.Strings.VarString;
    Files                       : TDSFiles;
  end;
  PDSFolder=^TDSFolder;
  TDSFolders=Array of TDSFolder;
  PDSFolders=^TDSFolders;
  TDSFAT=Record
    DomainID                    : Int64;
    Folders                     : TDSFolders;
  end;
  TDomainFat=class(TObject)
  private
    FFolderCount                : LongInt;
    FFileCount                  : LongInt;
    FWNDHandle                  : THandle;
    FLock                       : TRTLCriticalSection;
    //FFileIndexes                : TFileBasedIndex;
    //FFolderIndexes              : TFileBasedIndex;

    FFileSystem                 : TDSFAT;

    //procedure    WinProc(Var Msg:Messages.TMessage);
  public
    Constructor  Create; ReIntroduce;
    Destructor   Destroy; Override;

    procedure    Lock;
    procedure    Unlock;

    procedure    LoadFiles(Var Module:TDBMSModule; DomainID:Int64);

    Function     FindFolderID(Var Path:String; Var FolderID:Int64):Boolean;
    Function     FindFileID(Var Path,FileName:String; Var FolderID:Int64; Var dtCreated,dtModified:Double; Var HasKeywords,Cacheable:Boolean):Int64; overload;

    Function     AcquireFile(var FolderID,FileID:Int64):PByteBuffer; overload;
    procedure    ReleaseFile(var FolderID,FileID:Int64);

    property     FileSystem: TDSFat read FFileSystem;
  end;

  procedure Copy(Var Source,Destination:TDFile); overload;
  procedure Copy(Var Source,Destination:TDFiles); overload;
  procedure Copy(Var Source,Destination:TDFolder); overload;
  procedure Copy(Var Source,Destination:TDDirectory); overload;

  procedure Empty(Var Item:TFileDetails); overload;
  procedure Empty(Var Item:TDSFolders); overload;
  procedure Empty(Var Item:TDSFolder); overload;
  procedure Empty(Var Item:TDSFiles); overload;
  procedure Empty(Var Item:TDSFile); overload;
  procedure Empty(Var Item:TDSFAT); overload;
  procedure Empty(Var Item:TDFile); overload;
  procedure Empty(Var Item:TDFiles); overload;
  procedure Empty(Var Item:TDFolder); overload;
  procedure Empty(Var Item:TDDirectory); overload;

  Function  IndexOf(Var Item:TDDirectory; FolderID:Int64):Integer; overload;
  Function  IndexOf(Var Item:TDSFolder; FileID:Int64):Integer; overload;
  Function  IndexOf(Var Item:TDSFolders; FolderID:Int64):Integer; overload;

  Function  GetFile(Var Module:TDBMSModule; DomainID:Int64; Var Folder:TDSFolder; Var Item:TDSFile):Boolean; overload;
  Function  GetFile(Var Module:TDBMSModule; ID:Int64; Var Data:TByteBuffer):Boolean; overload;
  Function  GetFile(Var Module:TDBMSModule; Var Details:TFileDetails):Boolean; overload;
  Function  SetFile(Var Module:TDBMSModule; Var Details:TFileDetails):Boolean; overload;
  Function  SetFile(Var Module:TDBMSModule; ID:Int64; Var Data:TByteBuffer):Boolean; overload;

  Function  CreateFile(Var Module:TDBMSModule; Var Directory:TDDirectory; DomainID,FolderID:Int64; Var FileID:Int64; Var FileName:Core.Strings.VarString; Var Data:TByteBuffer):Boolean;
  Function  CreateFolder(Var Module:TDBMSModule; Var Directory:TDDirectory; DomainID:Int64; Var Path:Core.Strings.VarString; Var FolderID:Int64):Boolean;

  Function  FolderExists(Var Module:TDBMSModule; DomainID:Int64; Var Path:Core.Strings.VarString):Boolean; overload;
  Function  FileExists(Var Module:TDBMSModule; FolderID:Int64; Var Path:Core.Strings.VarString):Boolean; overload;

  Function  PrepareCopyFileName(Var Module:TDBMSModule; FolderID:Int64; Var Name:Core.Strings.VarString):Boolean;

  Function  GetDirectory(Var Module:TDBMSModule; DomainID:Int64; Var Directory:TDDirectory):Boolean;
  Function  GetFAT(Var Module:TDBMSModule; DomainID:Int64; FAT:TDomainFat):Boolean;

  Function  DeleteFolder(Var Module:TDBMSModule; ID:Int64):Boolean;
  Function  DeleteFile(Var Module:TDBMSModule; ID:Int64):Boolean;

  Function  RenameFile(Var Module:TDBMSModule; ID:Int64; Var Name:Core.Strings.VarString):Boolean; overload;
  Function  RenameFolder(Var Module:TDBMSModule; ID:Int64; Var Path:Core.Strings.VarString):Boolean; overload;

  Function  EditFile(Var Module:TDBMSModule; ID:Int64; Var Data:TByteBuffer):Boolean; overload;


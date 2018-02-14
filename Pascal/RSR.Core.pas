unit RSR.Core;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}


interface
  uses
    Classes,SysUtils,

    RSR,
    RSR.HTTP,

    Core.Generics,
    Core.Strings,
    Core.Keywords,
    Core.Arrays,
    Core.Arrays.Types,
    Core.Arrays.VarString,
    Core.Arrays.KeyString,
    Core.Arrays.Bytes,
    Core.Arrays.LargeInt,
    Core.Arrays.LargeWord,
    Core.Utils.Files,
    Core.Utils.Time,
    Core.Database,
    Core.Database.Types,
    Core.Database.SQL,


    Storage.Types,

    App,
    App.Consts,

    Storage.CoreObjects,
    Storage.FAT,
    Storage.Domains,
    Storage.Main,
    Storage.UserAccounts,
    Storage.Search,
    Storage.KeepAlive,
    Storage.MatrixNodes,

    XMLRead,
    DOM;
const
  {$i RSR.Core.StatusCodes.inc}
type
  NameSpace=class
  type
    Field=class
    type
      co=class
      type
        cmd=class
        const
          NameSpace                        = 'cc-ns';
        end;
      const
        NameSpace                          = 'co-ns';
      end;
    const
      Auth                                 = 'auth';
      Resource                             = 'resource';
    end;
  end;
  TCoreObjects=class;
  TCOIFields=class;
  TCOItems=class;

  TTransportResource=record
    NameSpace                    : Core.Strings.VarString;
    Path                         : Core.Strings.VarString;
    Name                         : Core.Strings.VarString;
    Ext                          : Core.Strings.VarString;
  end;
  TTransportBase=class
  protected
    FCoreObjects                 : TCoreObjects;
    FManager                     : TRSRManager;
    FNameSpace                   : Core.Strings.VarString;

    FCacheRequest                : Double;
    FCacheResponse               : Double;

    FOutput                      : TMemoryStream;
    FInput                       : TMemoryStream;
    FCoreObject                  : TTransportResource;
    FCoreCommand                 : TTransportResource;
    FResource                    : TTransportResource;

    FKeywordsP                   : PKeywords;
    FFAT                         : TDSFAT;
    FDomainP                     : Storage.Domains.Items.PDomain;
    FMedia                       : Pointer;
    FNodeP                       : Storage.MatrixNodes.Node.PItem;

    FCache                       : Boolean;
    FCacheTTL                    : LongInt;
    FCacheDate                   : Double;
    FCacheExposure               : Core.Strings.VarString;
    FContentType                 : Core.Strings.VarString;
    FCacheTag                    : Core.Strings.VarString;
    FCacheValidation             : Core.Strings.VarString;
    FETagRequest                 : Core.Strings.VarString;
    FETagRequested               : boolean;
  protected
    function    ResolvePath(var URI: Core.Strings.VarString; var Folder:TDSFolder; var Resource:TDSFile; var Path, FileName,Ext: Core.Strings.VarString; Refactor:TStream): TResolveResult; virtual; abstract;
  public
    procedure   OnCoreObjectRequestAuth(CommandP:PCoreCommand; var SR:TRSR; sTitle,sPrompt:Core.Strings.VarString); virtual; abstract;
    procedure   OnCoreObjectError(CommandP:PCoreCommand; var SR:TRSR; Error:Word); virtual; abstract;
    procedure   OnCoreObjectSuccess(CommandP:PCoreCommand; var SR:TRSR; Code:Word); virtual; abstract;
    function    OnCoreObjectCheckCredentials(CommandP:PCoreCommand; var SR:TRSR; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD; virtual; abstract;
    procedure   OnCoreObjectRedirect(CommandP:PCoreCommand; var SR:TRSR; URL:Core.Strings.VarString); virtual; abstract;
    procedure   OnCoreObjectNotFound(var SR:TRSR; NameSpace:Core.Strings.VarString); virtual; abstract;
    procedure   OnCoreObjectCommandNotFound(var SR:TRSR; nsObject,nsCommand:Core.Strings.VarString); virtual; abstract;
    procedure   OnBadRequest(Var RSR:TRSR); virtual; abstract;
    procedure   OnInvalidMethod(Var RSR:TRSR); virtual; abstract;
    procedure   OnNotFound(var SR:TRSR; Title,Message:Core.Strings.VarString); virtual; abstract;
    procedure   OnDataReceived(var SR:TRSR; var Handled:boolean); virtual; abstract;
  public
    procedure   SetResource(NameSpace,Path,Name,Ext:Core.Strings.VarString);
    procedure   SetCoreObject(NameSpace,Path,Name,Ext:Core.Strings.VarString); overload;
    procedure   SetCoreCommand(NameSpace,Path,Name,Ext:Core.Strings.VarString); overload;
    procedure   SetCoreObject(var NameSpace:Core.Strings.VarString); overload;
    procedure   SetCoreCommand(var NameSpace:Core.Strings.VarString); overload;
  public
    procedure   SendContent(var SR:TRSR; Content:Core.Strings.VarString); virtual; abstract;
    procedure   SendFile(var SR:TRSR; Folder:TDSFolder; Resource:TDSFile; Var sExt:Core.Strings.VarString); virtual; abstract;
    procedure   Redirect(var SR:TRSR; URL:Core.Strings.VarString); virtual; abstract;
    procedure   RequestPassword(var SR:TRSR; Title,Prompt:Core.Strings.VarString); virtual; abstract;
    procedure   GetResponse(Var RSR:TRSR); virtual; abstract;
    procedure   Reset(); virtual;
  public
    property    Output:TMemoryStream read FOutput;
    property    Input:TMemoryStream read FInput;
    property    Media: pointer read FMedia write FMedia;
  public
    property    CacheRequest  : TDateTime read FCacheRequest write FCacheRequest;
    property    CacheResponse : TDateTime read FCacheResponse  write FCacheResponse;
    property    Cache : boolean read FCache write FCache;
    property    CacheTTL : LongInt read FCacheTTL write FCacheTTL;
    property    CacheDate : Double read FCacheDate write FCacheDate;
    property    CacheTag : Core.Strings.VarString read FCacheTag write FCacheTag;
    property    CacheExposure :Core.Strings.VarString read FCacheExposure write FCacheExposure;
    property    CacheValidation :Core.Strings.VarString read FCacheValidation write FCacheValidation;
    property    ContentType :Core.Strings.VarString read FContentType write FContentType;
    property    ETagRequest:Core.Strings.VarString read FETagRequest write FETagRequest;
    property    ETagRequested:boolean read FETagRequested write FETagRequested;
    property    KeywordsP:PKeywords read FKeyWordsP;
  public
    Constructor Create(aNameSpace:Core.Strings.VarString; aDomainP:Storage.Domains.Items.PDomain; aFAT:TDSFAT; aManager:TRSRManager; aNodeP:Storage.MatrixNodes.Node.PItem; aInput:TMemoryStream; aCoreObjects:TCoreObjects; aKeywordsP:PKeywords); reIntroduce;
    Destructor  Destroy; override;
  end;

  NS_COI_FIELD_FLAGS=class
  const
    IDENTITY                     = 1 shl 0;  // Primary Key or Identifier Field (No writing allowed)
    READONLY                     = 1 shl 1;  // Read Only
    READ                         = 1 shl 2;  // Retrieval Requested
    WRITE                        = 1 shl 3;  // Write Requested
  end;
  PCOIField=^TCOIField;
  TCOIField=record
    InfoP                        : Core.Database.Types.PField;
    Flags                        : BYTE;
    Value                        : Pointer;
  end;
  GCOIFields=specialize GStructList<TCOIField>;
  TCOIFields=class(GCOIFields)
  private
    FIDP                         : Core.Database.Types.PField;
    FDefined                     : Boolean;
  public
    function getField(Name:Core.Strings.VarString):PCOIField;
    function getValue(FieldP:Core.Database.Types.PField):pointer;
    function setValue(FieldP:Core.Database.Types.PField; Value:pointer):boolean;
  public
    procedure Empty(Var Item:TCOIField); override;
    procedure Done(Var Item:TCOIField); override;
    procedure Init(Var Item:TCOIField); override;
  public
    constructor Create(aID:Core.Database.Types.PField); reIntroduce;
    procedure   Define(var Manifest:Core.Arrays.Types.KeyStrings);
  end;

  TCOItem=class
  private
    Owner                        : TCOItems;
    FFields                      : TCOIFields;
  public
    function  Delete:Boolean;
    function  Write:Boolean;
    function  Load(var Criteria:Core.Arrays.Types.KeyStrings):Boolean;
    function  Refresh:Boolean;
  public
    Constructor Create(aOwner:TCOItems; aID:Core.Database.Types.PField); reIntroduce;
  end;

  GCOItems=specialize GObjectList<TCOItem>;
  TCOItems=class(GCOItems)
  private
    CoreObjects                  : TCoreObjects;
    FOnDBException               : Core.Database.Types.ExceptionEvent;
    FTask                        : Core.Database.Types.TTask;
  private
    procedure _OnDBException(sModule,sLocation,sTable,sTask,sMessage:Core.Strings.VarString);
  public
    TableP                       : Core.Database.Types.PTable;
  public
    Constructor Create(aOwner:TCoreObjects; aTable:Core.Database.Types.PTable); reIntroduce;
    Destructor  Destroy; override;
  public
    function  Load(sSQL:Core.Strings.VarString):Boolean;
    function  Refresh(sSQL:Core.Strings.VarString):Boolean;
  public
    property OnDBException:Core.Database.Types.ExceptionEvent read FOnDBException write FOnDBException;
  published
    property Task : Core.Database.Types.TTask read FTask;
  end;

  TCoreObject=Class (TPersistent)
  private
    FOwner                 : TCoreObjects;
  protected
    FTask                  : Core.Database.Types.TTask;
    FRefactor              : TMemoryStream;
    FXMLDocument           : TXMLDocument;
    FXMLSource             : TXMLInputSource;
    FCommands              : TCoreCommands;
  private
    procedure Reset;
    procedure VerifyFAT;
    procedure VerifyIDs(Task:Core.Database.Types.TTask);
  protected
    class procedure Install(Task:Core.Database.Types.TTask); Virtual; abstract;
    class procedure UnInstall; Virtual; abstract;
    procedure Initialize; virtual; abstract;
    procedure Finalize; virtual; abstract;
    procedure Started; virtual;
  protected
    function  BeforeExecute(CommandP:PCoreCommand; var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; Var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; virtual; abstract;
  protected
    function  IndexOf(var Resource:TDSFile): LongInt; overload;
    function  UAP(Var Socket:TRSR):Storage.UserAccounts.Items.PItem;
  protected
    OwnerP                       : PCoreListInfo;
  public
    constructor Create(AOwner:TCoreObjects); reIntroduce;
    destructor  Destroy; override;
  public
    Folder                       : TDSFolder;
    Header                       : TCoreObjectInfo;
  public
    function  Transport(Var Socket:TRSR):TTransportBase;
    function  Manager(Var Socket:TRSR):TRSRManager;
  public
    function  IndexOf(var NameSpace:Core.Strings.VarString): LongInt; overload;
    function  Find(var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Provider:Provider.TItem; var Query:Query.TItem; var Results:Cache.TItem):WORD; virtual;
  public
    property  CoreObjects:TCoreObjects read FOwner;
    property  Commands:TCoreCommands read FCommands;
  End;
  TCoreObjectArray = Array of TCoreObject;
  CCoreObject=class of TCoreObject;
  GCoreObjects=specialize GObjectList<TCoreObject>;
  TCoreObjects=class (GCoreObjects)
  private
    FInfo                        : TCoreListInfo;
    FResult                      : LongInt;
    FHandled                     : Boolean;
    FXMLParser                   : TDOMParser;
    FNestCount                   : LongInt;
    FTask                        : Core.Database.Types.TTask;
  private
    procedure Add(Item:TCoreObject; var coaList:TCoreObjectArray); overload;
    function  GetLastError:Cardinal;
  public
    procedure Execute(var SR:TRSR; Transport:TTransportBase; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream);
    procedure NestedExecute(var SR:TRSR; Transport:TTransportBase; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream);
  public
    function  Find(var NameSpace:Core.Strings.VarString):TCoreObject;
    procedure Reset(aDomainP:Storage.Domains.Items.PDomain; aRootP:Storage.UserAccounts.Items.PItem; aDefaultP:Storage.UserAccounts.Items.PItem; aKeywordsP:PKeywords; aFat:TDSFat; aAccounts:Storage.UserAccounts.Items.TList);
  public
    procedure VerifyFAT;
    procedure Started;
    procedure VerifyIDs(Task:Core.Database.Types.TTask);
    function  IndexOf(var NameSpace:Core.Strings.VarString): LongInt; overload;
    function  IndexOf(Var Folder:TDSFolder): LongInt; overload;
    function  IndexOf(ProviderID:Int64): LongInt; overload;
  public
  public
    property LastError:Cardinal read GetLastError;
    property Result:LongInt read FResult;
    property Task:Core.Database.Types.TTask read FTask;
  public
    constructor Create(aHeader:Core.Database.Types.THeader; aServer:TRSRServer; aManager:TRSRManager; aNodeP:Storage.MatrixNodes.Node.PItem; aDomainP:Storage.Domains.Items.PDomain; aRootP:Storage.UserAccounts.Items.PItem; aDefaultP:Storage.UserAccounts.Items.PItem; aKeywordsP:PKeywords; aFat:TDSFat; aAccounts:Storage.UserAccounts.Items.TList); reIntroduce;
    destructor Destroy; override;
  public
    procedure Load;
  end;

  procedure Empty(var Item:TTransportResource); overload;

  function Validate(NameSpace:Core.Strings.VarString):Core.Strings.VarString;
  var
    CoreObjectItems:TCoreObjectItems;

implementation


procedure Empty(var Item:TTransportResource);
begin
  SetLength(Item.Ext,0);
  SetLength(Item.Name,0);
  SetLength(Item.NameSpace,0);
  SetLength(Item.Path,0);
end;

Constructor TTransportBase.Create(aNameSpace:Core.Strings.VarString; aDomainP:Storage.Domains.Items.PDomain; aFAT:TDSFAT; aManager:TRSRManager; aNodeP:Storage.MatrixNodes.Node.PItem; aInput:TMemoryStream; aCoreObjects:TCoreObjects; aKeywordsP:PKeywords);
begin
  FDomainP:=aDomainP;
  FFAT:=aFAT;
  FNameSpace:=aNameSpace;
  FManager:=aManager;
  FNodeP:=aNodeP;
  FCoreObjects:=aCoreObjects;
  FKeywordsP:=aKeywordsP;
  FOutput:=TMemoryStream.Create;
  FInput:=aInput;
  Inherited Create;
end;

Destructor  TTransportBase.Destroy;
begin
  FreeAndNil(FOutput);
  Inherited Destroy;
end;

procedure   TTransportBase.SetResource(NameSpace,Path,Name,Ext:Core.Strings.VarString);
begin
  FResource.NameSpace:=NameSpace;
  FResource.Path:=Path;
  FResource.Name:=Name;
  FResource.Ext:=Ext;
end;

procedure   TTransportBase.SetCoreObject(NameSpace,Path,Name,Ext:Core.Strings.VarString);
begin
  FCoreObject.NameSpace:=NameSpace;
  FCoreObject.Path:=Path;
  FCoreObject.Name:=Name;
  FCoreObject.Ext:=Ext;
end;

procedure   TTransportBase.SetCoreObject(var NameSpace:Core.Strings.VarString);
begin
  FCoreObject.NameSPace:=NameSpace;
end;

procedure   TTransportBase.SetCoreCommand(var NameSpace:Core.Strings.VarString);
begin
  FCoreCommand.NameSpace:=NameSpace;
end;

procedure   TTransportBase.SetCoreCommand(NameSpace,Path,Name,Ext:Core.Strings.VarString);
begin
  FCoreCommand.NameSpace:=NameSpace;
  FCoreCommand.Path:=Path;
  FCoreCommand.Name:=Name;
  FCoreCommand.Ext:=Ext;
end;

procedure  TTransportBase.Reset();
begin
  FOutput.Size:=0;
  FMedia:=nil;
  FCacheRequest:=0;
  FCacheResponse:=0;

  FCache:=false;
  FCacheTTL:=0;
  FCacheDate:=0;
  FCacheExposure:=PUBLIC_CACHE;
  SetLength(FCacheValidation,0);

  FETagRequested:=false;
  SetLength(FETagRequest,0);
  SetLength(FContentType,0);
  SetLength(FCacheTag,0);

  Empty(FCoreCommand);
  Empty(FCoreObject);
  Empty(FResource);
end;


Constructor TCOItems.Create(aOwner:TCoreObjects; aTable:Core.Database.Types.PTable);
begin
  CoreObjects:=aOwner;

  FTask:=Core.Database.Types.TTask.Create(Storage.Main.Header,'RSR.Core.TCOItems');

  Inherited Create;
end;

Destructor TCOItems.Destroy;
begin
  FTask.Free();

  Inherited Destroy;
end;

function  TCOItems.Load(sSQL:Core.Strings.VarString):Boolean;
begin
  Result:=false;
end;

function  TCOItems.Refresh(sSQL:Core.Strings.VarString):Boolean;
begin
  Result:=false;
end;

procedure   TCOItems._OnDBException(sModule,sLocation,sTable,sTask,sMessage:Core.Strings.VarString);
begin
  If Assigned(FOnDBException) then FOnDBException(sModule,sLocation,sTable,sTask,sMessage);
end;


Constructor TCOItem.Create(aOwner:TCOItems; aID:Core.Database.Types.PField);
begin
  Owner:=aOwner;
  FFields:=TCOIFields.Create(aID);
  Inherited Create;
end;

function TCOItem.Delete:Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.AddCommand(iCount,Owner.TableP,@Commands);
    Core.Database.AddCommand(iCount,FFields.FIDP^.KeyP^,useForCriteria,FFields.FIDP^.DataType,FFields.FIDP^.IDP^,FFields.getValue(FFields.FIDP),poNone,oEqual,@Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    // Future Version Needs to make sure Temp Result Store Is Cleared Of All Their Data
  Finally
    Core.Database.Done(Commands);
  end;
end;

function TCOItem.Write:Boolean;
var
  iCount:LongInt;
  iLcv:LongInt;
  ItemP:PCOIField;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Owner.TableP,@Commands);
    Core.Database.AddCommand(iCount,FFields.FIDP^.KeyP^,useForCriteria,FFields.FIDP^.DataType,FFields.FIDP^.IDP^,FFields.getValue(FFields.FIDP),poNone,oEqual,@Commands);
    for iLcv:=0 to FFields.Count-1 do begin
      ItemP:=FFields.Items[iLcv];
      If (ItemP<>Nil) and ((ItemP^.Flags or NS_COI_FIELD_FLAGS.IDENTITY) <> ItemP^.Flags) and ((ItemP^.Flags or NS_COI_FIELD_FLAGS.READONLY) <> ItemP^.Flags) and ((ItemP^.Flags or NS_COI_FIELD_FLAGS.WRITE) = ItemP^.Flags) then
        Core.Database.AddCommand(iCount,ItemP^.InfoP^.KeyP^,useForUpdates,ItemP^.InfoP^.DataType,ItemP^.InfoP^.IDP^,FFields.getValue(ItemP^.InfoP),poNone,oNone,@Commands);
    end;
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

function TCOItem.Load(Var Criteria:Core.Arrays.Types.KeyStrings):Boolean;
var
  iCCount:LongInt;
  iLcv:LongInt;
  ItemP:PCOIField;
begin
  Result:=false;
  iCCount:=Length(Criteria);
  for iLcv:=0 to iCCount-1 do begin
    ItemP:=FFields.getField(Criteria[iLcv]^.Key)
  end;
  Result:=true;
end;

function TCOItem.Refresh:Boolean;
begin
  Result:=false;
end;

constructor TCOIFields.Create(aID:Core.Database.Types.PField);
begin
  FIDP:=aID;
  FDefined:=false;
  Inherited Create;
end;

procedure TCOIFields.Define(var Manifest:Core.Arrays.Types.KeyStrings);
begin
  If (FDefined=false) then begin

  end;
end;

function TCOIFields.getField(Name:Core.Strings.VarString):PCOIField;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to Count-1 do begin
    if (Items[iLcv]^.InfoP^.KeyP^=Name) then begin
      Result:=Items[iLcv];
      Break;
    end;
  end;
end;

function TCOIFields.getValue(FieldP:Core.Database.Types.PField):pointer;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to Count-1 do begin
    if (Items[iLcv]^.InfoP=FieldP) then begin
      Result:=Items[iLcv]^.Value;
      Break;
    end;
  end;
end;

function TCOIFields.setValue(FieldP:Core.Database.Types.PField; Value:pointer):boolean;
var
  iLcv:LongInt;
  ItemP:PCOIField;
begin
  Result:=false;
  for iLcv:=0 to Count-1 do begin
    ItemP:=Items[iLcv];
    if (ItemP^.InfoP=FieldP) then begin
      case ItemP^.InfoP^.DataType of
        dftBoolean     : Core.Database.Types.PBool(ItemP^.Value)^:=Core.Database.Types.PBool(Value)^;
        dftByte        : Core.Database.Types.PByte(ItemP^.Value)^:=Core.Database.Types.PByte(Value)^;
        dftInteger     : Core.Database.Types.PInteger(ItemP^.Value)^:=Core.Database.Types.PInteger(Value)^;
        dftInt64       : Core.Database.Types.PLargeInt(ItemP^.Value)^:=Core.Database.Types.PLargeInt(Value)^;
        dftQWord       : Core.Database.Types.PLargeWord(ItemP^.Value)^:=Core.Database.Types.PLargeWord(Value)^;
        dftInt64Array  : Core.Arrays.LargeInt.Copy(Core.Database.Types.PLargeIntArray(Value)^,Core.Database.Types.PLargeIntArray(ItemP^.Value)^);
        dftQWordArray  : Core.Arrays.LargeWord.Copy(Core.Database.Types.PLargeWordArray(Value)^,Core.Database.Types.PLargeWordArray(ItemP^.Value)^);
        dftDateTime    : Core.Database.Types.PDateTime(ItemP^.Value)^:=Core.Database.Types.PDateTime(Value)^;
        dftFloat       : Core.Database.Types.PFloat(ItemP^.Value)^:=Core.Database.Types.PFloat(Value)^;
        dftDouble      : Core.Database.Types.PDouble(ItemP^.Value)^:=Core.Database.Types.PDouble(Value)^;
        dftSmallString : Core.Database.Types.PSmallString(ItemP^.Value)^:=Core.Database.Types.PSmallString(Value)^;
        dftString      : Core.Database.Types.PVarString(ItemP^.Value)^:=Core.Database.Types.PVarString(Value)^;
        dftMemo        : Core.Database.Types.PMemo(ItemP^.Value)^:=Core.Database.Types.PMemo(Value)^;
        dftByteBuffer  : Core.Arrays.Bytes.Copy(Core.Database.Types.PBuffer(Value)^,Core.Database.Types.PBuffer(ItemP^.Value)^);
      end;
      Result:=True;
      Break;
    end;
  end;
end;

procedure TCOIFields.Empty(Var Item:TCOIField);
begin
  case Item.InfoP^.DataType of
    dftBoolean     : Core.Database.Types.PBool(Item.Value)^:=false;
    dftByte        : Core.Database.Types.PByte(Item.Value)^:=0;
    dftInteger     : Core.Database.Types.PInteger(Item.Value)^:=0;
    dftInt64       : Core.Database.Types.PLargeInt(Item.Value)^:=0;
    dftQWord       : Core.Database.Types.PLargeWord(Item.Value)^:=0;
    dftInt64Array  : Core.Arrays.LargeInt.Empty(Core.Database.Types.PLargeIntArray(Item.Value)^);
    dftQWordArray  : Core.Arrays.LargeWord.Empty(Core.Database.Types.PLargeWordArray(Item.Value)^);
    dftDateTime    : Core.Database.Types.PDateTime(Item.Value)^:=0;
    dftFloat       : Core.Database.Types.PFloat(Item.Value)^:=0;
    dftDouble      : Core.Database.Types.PDouble(Item.Value)^:=0;
    dftSmallString : SetLength(Core.Database.Types.PSmallString(Item.Value)^,0);
    dftString      : SetLength(Core.Database.Types.PVarString(Item.Value)^,0);
    dftMemo        : SetLength(Core.Database.Types.PMemo(Item.Value)^,0);
    dftByteBuffer  : Core.Arrays.Bytes.Empty(Core.Database.Types.PBuffer(Item.Value)^);
  end;
end;

procedure TCOIFields.Done(Var Item:TCOIField);
begin
  case Item.InfoP^.DataType of
    dftBoolean     : Dispose(Core.Database.Types.PBool(Item.Value));
    dftByte        : Dispose(Core.Database.Types.PByte(Item.Value));
    dftInteger     : Dispose(Core.Database.Types.PInteger(Item.Value));
    dftInt64       : Dispose(Core.Database.Types.PLargeInt(Item.Value));
    dftQWord       : Dispose(Core.Database.Types.PLargeWord(Item.Value));
    dftInt64Array  : begin
        Core.Arrays.LargeInt.Done(Core.Database.Types.PLargeIntArray(Item.Value)^);
        Dispose(Core.Database.Types.PLargeIntArray(Item.Value));
    end;
    dftQWordArray  : begin
        Core.Arrays.LargeWord.Done(Core.Database.Types.PLargeWordArray(Item.Value)^);
        Dispose(Core.Database.Types.PLargeWordArray(Item.Value));
    end;
    dftDateTime    : Core.Database.Types.PDateTime(Item.Value)^:=0;
    dftFloat       : Core.Database.Types.PFloat(Item.Value)^:=0;
    dftDouble      : Core.Database.Types.PDouble(Item.Value)^:=0;
    dftSmallString : begin
      Finalize(Core.Database.Types.PSmallString(Item.Value)^);
      Dispose(Core.Database.Types.PSmallString(Item.Value));
    end;
    dftString      : begin
      Finalize(Core.Database.Types.PVarString(Item.Value)^);
      Dispose(Core.Database.Types.PVarString(Item.Value));
    end;
    dftMemo        : begin
      Finalize(Core.Database.Types.PMemo(Item.Value)^);
      Dispose(Core.Database.Types.PMemo(Item.Value));
    end;
    dftByteBuffer  : begin
      Finalize(Core.Database.Types.PBuffer(Item.Value)^);
      Dispose(Core.Database.Types.PBuffer(Item.Value));
    end;
  end;
end;

procedure TCOIFields.Init(Var Item:TCOIField);
begin
  case Item.InfoP^.DataType of
    dftBoolean     : Core.Database.Types.PBool(Item.Value)^:=false;
    dftByte        : Core.Database.Types.PByte(Item.Value)^:=0;
    dftInteger     : Core.Database.Types.PInteger(Item.Value)^:=0;
    dftInt64       : Core.Database.Types.PLargeInt(Item.Value)^:=0;
    dftQWord       : Core.Database.Types.PLargeWord(Item.Value)^:=0;
    dftInt64Array  : Core.Arrays.LargeInt.Init(Core.Database.Types.PLargeIntArray(Item.Value)^);
    dftQWordArray  : Core.Arrays.LargeWord.Init(Core.Database.Types.PLargeWordArray(Item.Value)^);
    dftDateTime    : Core.Database.Types.PDateTime(Item.Value)^:=0;
    dftFloat       : Core.Database.Types.PFloat(Item.Value)^:=0;
    dftDouble      : Core.Database.Types.PDouble(Item.Value)^:=0;
    dftSmallString : SetLength(Core.Database.Types.PSmallString(Item.Value)^,0);
    dftString      : SetLength(Core.Database.Types.PVarString(Item.Value)^,0);
    dftMemo        : SetLength(Core.Database.Types.PMemo(Item.Value)^,0);
    dftByteBuffer  : Core.Arrays.Bytes.Init(Core.Database.Types.PBuffer(Item.Value)^);
  end;
end;

function  Validate(NameSpace:Core.Strings.VarString):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  Result:=NameSpace; iLen:=System.Length(Result);
  If (iLen>0) and (Result[1]='/') then begin
    System.Delete(Result,1,1);
    iLen-=1;
  end;
  if (iLen>0) and (Result[iLen]='/') then begin
    iLen-=1;
    System.SetLength(Result,iLen);
  end;
end;

constructor TCoreObject.Create(AOwner:TCoreObjects);
begin
  FOwner:=AOwner;
  FRefactor:=TMemoryStream.Create;
  FTask:=AOwner.Task;
  Inherited Create;
end;

destructor  TCoreObject.Destroy;
begin
  OwnerP:=nil;

  FreeAndNil(FXMLSource);
  FreeAndNil(FXMLDocument);
  FreeAndNil(FRefactor);

  Inherited Destroy;

  if GetClass(Header.CLSInfo^.Name)<>nil then
    UnInstall;
end;

procedure TCoreObject.Reset;
var
  iLcv:LongInt;
begin
  Folder:=nil;
  For iLcv:=0 to High(FCommands) do
    FCommands[iLcv]^.Resource:=nil;
end;

procedure TCoreObject.VerifyFAT;
var
  iLcv:LongInt;
  fldCMD:TDSFolder;
  flCMD:TDSFile;
  sCMDPath:Core.Strings.VarString;
  sCMDName:Core.Strings.VarString;
begin
  If Folder=nil then begin
    Folder:=OwnerP^.Fat.Acquire(Header.ACLInfo^.NameSpace,FRefactor);
    if Folder=nil then
      Folder:=OwnerP^.Fat.Folders.New(Header.ACLInfo^.NameSpace,FS_ATTR_COREOBJECT);
    if Folder.Attributes or FS_ATTR_COREOBJECT<>Folder.Attributes then
      Folder.Attributes:=(Folder.Attributes or FS_ATTR_COREOBJECT);
  end;

  For iLcv:=0 to High(FCommands) do begin
    if FCommands[iLcv]^.Resource=nil then begin
      sCMDName:=Core.Utils.Files.Extract(FCommands[iLcv]^.ACLInfo^.NameSpace,epoName,App.Consts.PathDelim);
      sCMDPath:=Core.Utils.Files.Extract(FCommands[iLcv]^.ACLInfo^.NameSpace,epoAllButName,App.Consts.PathDelim);
      sCMDPath:=Core.Utils.Files.Append(Header.ACLInfo^.NameSpace,sCMDPath,App.Consts.PathDelim);
      fldCMD:=OwnerP^.Fat.Acquire(sCMDPath,FRefactor);
      if fldCMD=nil then
        fldCMD:=OwnerP^.Fat.Folders.New(sCMDPath,FS_ATTR_COREOBJECT);

      flCMD:=OwnerP^.Fat.Acquire(fldCMD,sCMDName);
      if flCMD=nil then begin
        flCMD:=fldCMD.Files.New(FS_ATTR_COREOBJECT,FAT_KEYWORDS_ON,NO_DEFLATE,FCommands[iLcv]^.Cache,sCMDName,CO_DEFAULT_PAGE);
      end;
      if flCMD.Attributes or FS_ATTR_COREOBJECT<>flCMD.Attributes then begin
        flCMD.Attributes:=flCMD.Attributes or FS_ATTR_COREOBJECT;
        flCMD.Save(FTask,dsFileSaveAttribs);
      end;
      FCommands[iLcv]^.Resource:=flCMD;
    end;
  end;
end;

procedure TCoreObject.VerifyIDs(Task:Core.Database.Types.TTask);
var
  iLcv:LongInt;
  sPath:Core.Strings.VarString;
  sName:Core.Strings.VarString;
  fldCMD:TDSFolder;
begin
  If Header.ID=0 then begin
    if Storage.CoreObjects.COREOBJECT_ID(Task,Header.ACLInfo^.NameSpace,Header.ID) then begin
      if Header.ID=0 then begin // Add Header to Database and Obtain new ID
        Storage.CoreObjects.COREOBJECT_Add(Task,Header);
        Header.ProviderID:=Storage.Search.Provider.DB.Identify(Task,Header.ACLInfo^.NameSpace,Provider.Defaults.NoNameSpace);
      end;
    end;
  end;
  for iLcv:=0 to High(FCommands) do begin
    if FCommands[iLcv]^.ID=0 then begin
      if Storage.CoreObjects.COREOBJECT_ID(Task,Header.ID,FCommands[iLcv]^.ACLInfo^.NameSpace,FCommands[iLcv]^.ID) then begin
        if FCommands[iLcv]^.ID=0 then
          Storage.CoreObjects.COREOBJECT_Add(Task,FCommands[iLcv]^);
      end;
    end;
  end;
  If Folder=nil then begin
    Folder:=OwnerP^.Fat.Acquire(Header.ACLInfo^.NameSpace,FRefactor);
    if Folder=nil then
      Folder:=OwnerP^.Fat.Folders.New(Header.ACLInfo^.NameSpace,FS_ATTR_COREOBJECT);
  end;
  For iLcv:=0 to High(FCommands) do begin
    if FCommands[iLcv]^.Resource=nil then begin
      // Should force directories
      sName:=Core.Utils.Files.Extract(FCommands[iLcv]^.ACLInfo^.NameSpace,epoName,App.Consts.PathDelim);
      sPath:=Core.Utils.Files.Append(Header.ACLInfo^.NameSpace,FCommands[iLcv]^.ACLInfo^.NameSpace,App.Consts.PathDelim);
      fldCMD:=OwnerP^.Fat.Acquire(sPath,FRefactor);
      if fldCMD=nil then
        fldCMD:=OwnerP^.Fat.Folders.New(sPath,FS_ATTR_COREOBJECT);
      FCommands[iLcv]^.Resource:=OwnerP^.Fat.Acquire(fldCMD,sName);
      if FCommands[iLcv]^.Resource=nil then begin
        FCommands[iLcv]^.Resource:=fldCMD.Files.New(FS_ATTR_COREOBJECT,FAT_KEYWORDS_ON,FCommands[iLcv]^.Compress,FCommands[iLcv]^.Cache,sName,CO_DEFAULT_PAGE);
      end;
    end;
  end;
end;

function  TCoreObject.IndexOf(var Resource:TDSFile): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(FCommands) do begin
    if FCommands[iLcv]^.Resource=Resource then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function  TCoreObject.UAP(Var Socket:TRSR):Storage.UserAccounts.Items.PItem;
begin
  Result:=Socket.Credentials;
end;

function  TCoreObject.Transport(Var Socket:TRSR):TTransportBase;
begin
  Result:=TTransportBase(Socket.Transport);
end;

function  TCoreObject.Manager(Var Socket:TRSR):TRSRManager;
begin
  Result:=MAN_MAP[Socket.Info.Socket];
end;

function  TCoreObject.IndexOf(var NameSpace:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(FCommands) do begin
    if FCommands[iLcv]^.ACLInfo^.NameSpace=NameSpace then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;


procedure  TCoreObject.Started;
begin

end;

function   TCoreObject.Find(var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Provider:Provider.TItem; var Query:Query.TItem; var Results:Cache.TItem):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

constructor TCoreObjects.Create(aHeader:Core.Database.Types.THeader; aServer:TRSRServer; aManager:TRSRManager; aNodeP:Storage.MatrixNodes.Node.PItem; aDomainP:Storage.Domains.Items.PDomain; aRootP:Storage.UserAccounts.Items.PItem; aDefaultP:Storage.UserAccounts.Items.PItem; aKeywordsP:PKeywords; aFat:TDSFat; aAccounts:Storage.UserAccounts.Items.TList);
begin
  Inherited Create;
  FNestCount:=0;
  FXMLParser:=TDOMParser.Create;
  FXMLParser.Options.Validate:=False;
  Storage.CoreObjects.Init(FInfo);
  With FInfo do begin
    DomainP:=aDomainP;
    KeywordsP:=aKeywordsP;
    Server:=aServer;
    Manager:=aManager;
    NodeP:=aNodeP;
    Fat:=aFat;
    RootP:=aRootP;
    DefaultP:=aDefaultP;
    Accounts:=aAccounts;
  end;
  FTask:=Core.Database.Types.TTask.Create(aHeader,'RSR.Core');
end;

destructor TCoreObjects.Destroy;
begin
  FXMLParser.Free();
  FTask.Free();
  Storage.CoreObjects.Done(FInfo);
  inherited Destroy;
end;

procedure TCoreObjects.Load;
var
  iLcv:LongInt;
  coGeneric:CCoreObject;
  coInfoP:PCoreObjectInfo;
  coItem:TCoreObject;
begin
  if FInfo.Loaded then exit;
  for iLcv:=0 to High(CoreObjectItems) do begin
    coInfoP:=CoreObjectItems[iLcv];
    coGeneric:=CCoreObject(GetClass(coInfoP^.CLSInfo^.Name));
    if coGeneric.InheritsFrom(TCoreObject) then begin
      coItem:=coGeneric.Create(Self);
      Add(coItem);
      Copy(coInfoP^,coItem.Header);
      coItem.OwnerP:=@FInfo;
      coItem.Initialize;
    end;
  end;
  FInfo.Loaded:=True;
end;

procedure TCoreObjects.VerifyFAT;
var
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do
    TCoreObject(Items[iLcv]).VerifyFAT;
end;

procedure TCoreObjects.Started;
var
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do
    TCoreObject(Items[iLcv]).Started;
end;

procedure TCoreObjects.Reset(aDomainP:Storage.Domains.Items.PDomain; aRootP:Storage.UserAccounts.Items.PItem; aDefaultP:Storage.UserAccounts.Items.PItem; aKeywordsP:PKeywords; aFat:TDSFat; aAccounts:Storage.UserAccounts.Items.TList);
var
  iLcv:LongInt;
begin
  FInfo.DomainP:=aDomainP;
  FInfo.Accounts:=aAccounts;
  FInfo.DefaultP:=aDefaultP;
  FInfo.RootP:=aRootP;
  FInfo.KeywordsP:=aKeywordsP;
  FInfo.Fat:=aFat;

  for iLcv:=0 to Count-1 do
    TCoreObject(Items[iLcv]).Reset;
end;

procedure TCoreObjects.VerifyIDs(Task:Core.Database.Types.TTask);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do
    TCoreObject(Items[iLcv]).VerifyIDs(Task);
end;

function TCoreObjects.IndexOf(var NameSpace:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to Count-1 do begin
    If SameText(NameSpace,TCoreObject(Items[iLcv]).Header.ACLInfo^.NameSpace) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function  TCoreObjects.Find(var NameSpace:Core.Strings.VarString):TCoreObject;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to Count-1 do begin
    If SameText(NameSpace,TCoreObject(Items[iLcv]).Header.ACLInfo^.NameSpace) then begin
      Result:=Items[iLcv];
      Break;
    end;
  end;
end;

function TCoreObjects.IndexOf(ProviderID:Int64): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to Count-1 do begin
    If ProviderID=TCoreObject(Items[iLcv]).Header.ProviderID then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function TCoreObjects.IndexOf(Var Folder:TDSFolder): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to Count-1 do begin
    If Folder=TCoreObject(Items[iLcv]).Folder then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function TCoreObjects.GetLastError:Cardinal;
begin
  Result:=FInfo.LastError;
end;

procedure TCoreObjects.Add(Item:TCoreObject; var coaList:TCoreObjectArray);
var
  iCt:LongInt;
begin
  iCt:=System.Length(coaList);
  System.SetLength(coaList,iCt+1);
  coaList[iCt]:=Item;
end;

procedure TCoreObjects.NestedExecute(var SR:TRSR; Transport:TTransportBase; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream);
var
  iCOIndex:LongInt;
  iCCIndex:LongInt;
  idxSrch:LongInt;

  procedure PushCommandExecution;
  begin
    if FResult=CO_STATUS_OK then begin
      FHandled:=False;

      Transport.Cache:=Items[iCOIndex].FCommands[iCCIndex]^.Cache or Items[iCOIndex].FCommands[iCCIndex]^.Resource.Cache;
      Transport.CacheExposure:=PUBLIC_CACHE;
      Transport.CacheDate:=0;
      Transport.CacheTTL:=Items[iCOIndex].FCommands[iCCIndex]^.Resource.CacheTTL;

      FResult:=Items[iCOIndex].FCommands[iCCIndex]^.Method(Items[iCOIndex].FCommands[iCCIndex],SR,Parameters,srcHeaders,respHeaders,Data,FHandled);
      FInfo.LastError:=FResult;
      if (FHandled=false) and (FNestCount=1) then begin
        Core.Arrays.KeyString.Update(respHeaders,fieldCode,IntToStr(FResult));
        idxSrch:=Core.Arrays.KeyString.IndexOf(srcHeaders,fieldSearch);
        if idxSrch>-1 then
          Core.Arrays.KeyString.Update(respHeaders,fieldSearch,srcHeaders[idxSrch]^.Value);
        if ( (FResult=CO_STATUS_OK) or (Transport.FOutput.Size>0) ) then
          Transport.OnCoreObjectSuccess(Items[iCOIndex].FCommands[iCCIndex],SR,FResult)
        else if (FResult=CO_STATUS_DEFERRED) then begin
          // The system is pending an RSRMethod
          // Do not send a response.  External system will do so.
        end else
          Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,FResult);
      end;
    end else begin
      FInfo.LastError:=CO_STATUS_ERR_CO_CMD_INITIALIZATION;
      Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,CO_STATUS_ERR_CO_CMD_INITIALIZATION);
    end;
  end;

begin
  Inc(FNestCount);
  Try
    Transport.Output.Size:=0;
    Core.Arrays.KeyString.Update(respHeaders,fieldRemoteIP,IntToStr(SR.Address.sin_addr.S_addr));
    iCOIndex:=IndexOf(Transport.FCoreObject.NameSpace);
    if iCOIndex<>-1 then begin
      iCCIndex:=Items[iCOIndex].IndexOf(Transport.FCoreCommand.NameSpace);
      if iCCIndex<>-1 then begin
        FHandled:=False;
        FInfo.LastError:=0;
        Transport.OnCoreObjectCheckCredentials(Items[iCOIndex].FCommands[iCCIndex],SR,srcHeaders,respHeaders,Data);
        if (
          Storage.CoreObjects.Granted(Items[iCOIndex].Header,FInfo.DefaultP) or
          Storage.CoreObjects.Granted(Items[iCOIndex].Header,SR.Credentials)
        ) then begin
          if (
            Storage.CoreObjects.Granted(Items[iCOIndex].FCommands[iCCIndex],FInfo.DefaultP) or
            Storage.CoreObjects.Granted(Items[iCOIndex].FCommands[iCCIndex],SR.Credentials)
          ) then begin
            FResult:=CO_STATUS_OK;
            If (
              (Items[iCOIndex].FCommands[iCCIndex]^.Secure) and
              ( (SR.STATE or RSR_STATE_SECURE) <> SR.STATE)
            ) then
              FResult:=CO_STATUS_ERR_CO_CMD_REQUIRES_SECURITY;
            if FResult=CO_STATUS_OK then begin
              FResult:=Items[iCOIndex].BeforeExecute(Items[iCOIndex].FCommands[iCCIndex],SR,Parameters,srcHeaders,respHeaders,Data,FHandled);
              if FResult=CO_STATUS_OK then begin
                If (Items[iCOIndex].FCommands[iCCIndex]^.Anonymous=false) and (SR.Credentials=nil) then begin
                  FResult:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
                  FInfo.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
                  Transport.OnCoreObjectRequestAuth(Items[iCOIndex].FCommands[iCCIndex],SR,format(PWD_REQUEST_TITLE,[Items[iCOIndex].FCommands[iCCIndex]^.ACLInfo^.Caption]),format(PWD_REQUEST_MESSAGE,[Items[iCOIndex].FCommands[iCCIndex]^.ACLInfo^.Caption]));
                end else begin
                  If (Items[iCOIndex].FCommands[iCCIndex]^.XMLInfo^.Enabled) then begin
                    Transport.ContentType:=RSR.HTTP.ctXML;
                    if (Data.Size>0) then begin
                      FreeAndNil(Items[iCOIndex].FXMLDocument);
                      if not Assigned(Items[iCOIndex].FXMLSource) then
                        Items[iCOIndex].FXMLSource:=TXMLInputSource.Create(Data);
                      try
                        Data.Position:=0;
                        FXMLParser.Parse(Items[iCOIndex].FXMLSource,Items[iCOIndex].FXMLDocument);
                      except
                        FResult:=CO_STATUS_ERR_CO_CMD_INVALID_XML;
                      end;
                      Data.Size:=0;
                      if FResult=CO_STATUS_OK then begin
                        PushCommandExecution;
                      end else begin
                        FInfo.LastError:=FResult;
                        Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,FResult);
                      end;
                    end else
                      PushCommandExecution;
                  end else
                    PushCommandExecution;
                  Transport.FOutput.Size:=0;
                  FreeAndNil(Items[iCOIndex].FXMLDocument);
                  FreeAndNil(Items[iCOIndex].FXMLSource);
                end;
              end else begin
                FInfo.LastError:=FResult;
                Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,CO_STATUS_ERR_CO_BEFORE_EXECUTE);
              end;
            end else begin
              FResult:=CO_STATUS_ERR_CO_CMD_REQUIRES_SECURITY;
              FInfo.LastError:=CO_STATUS_ERR_CO_CMD_REQUIRES_SECURITY;
              Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,CO_STATUS_ERR_CO_CMD_REQUIRES_SECURITY);
            end;
          end else begin
            FResult:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
            FInfo.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
            Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
          end;
        end else begin
          FResult:=CO_STATUS_ERR_CO_ACCESS_DENIED;
          FInfo.LastError:=CO_STATUS_ERR_CO_ACCESS_DENIED;
          Transport.OnCoreObjectError(Items[iCOIndex].FCommands[iCCIndex],SR,CO_STATUS_ERR_CO_ACCESS_DENIED);
        end;
      end else begin
        FResult:=CO_STATUS_ERR_CO_CMD_NOT_FOUND;
        FInfo.LastError:=CO_STATUS_ERR_CO_CMD_NOT_FOUND;
        Transport.OnCoreObjectCommandNotFound(SR,Transport.FResource.Path,Transport.FResource.Name);
      end;
    end else begin
      FResult:=CO_STATUS_ERR_CO_NOT_FOUND;
      FInfo.LastError:=CO_STATUS_ERR_CO_NOT_FOUND;
      Transport.OnCoreObjectNotFound(SR,Transport.FResource.Path);
    end;
    Transport.Output.Size:=0;
  finally
    Dec(FNestCount);
  end;
end;

procedure TCoreObjects.Execute(var SR:TRSR; Transport:TTransportBase; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream);
begin
  if FNestCount>0 then
    Classes.EComponentError.Create('Cannot call execute recursively');
  Try
    NestedExecute(SR,Transport,Parameters,srcHeaders,respHeaders,Data);
  finally
    FNestCount:=0;
  end;
end;

end.

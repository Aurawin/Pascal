unit Storage.Social.Files;

interface

uses
  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.Timer,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,
  Core.Utils.Files,
  Core.Strings,
  Core.Streams,
  Core.XML,

  Storage,
  Storage.Main,
  Storage.Social.Sync.Pipes,
  Storage.MatrixNodes,
  Storage.AuraDisks,

  Encryption.Base64,

  DOM,
  MD5,
  XMLRead,
  Classes,
  SysUtils;

type
  Kind=class
  const
    BIN                        : LongInt = 0;
    SMTP                       : LongInt = 1;
    XMPP                       : LongInt = 2;
    Image                      : LongInt = 3;
    Music                      : LongInt = 4;
    Video                      : LongInt = 5;
    CalcSheet                  : LongInt = 6;
    Document                   : LongInt = 7;
    Text                       : LongInt = 8;
    Presentation               : LongInt = 9;
    PlayList                   : LongInt = 10;
    PDF                        : LongInt = 11;
  end;
  Music=class
  Type
    XML=class
    const
      Stanza                 = 's';
      Tags                   = 'tags';
      Tag                    = 'tag';
    Type
      Fields=class
      const
        Inspected            = 'ip';
        Kind                 = 'k';
        Name                 = 'n';
        Value                = 'v';
        // New Values
        Album                = 'al';
        Song                 = 's';
        Details              = 'd';
        Artist               = 'a';
        Accompaniment        = 'ac';
        Composer             = 'co';
        TrackNumber          = 'tn';
        Year                 = 'yr';
        Genre                = 'ge';
        Group                = 'gp';
      end;
    end;
    TItem=record
      Kind                   : LongInt;
      Name                   : Core.Strings.VarString;
      Value                  : Core.Strings.VarString;
    end;
    PItem=^TItem;
    TItems=packed record
      Inspected              : boolean;
      Tags                   : Array of PItem;

      Album                  : Core.Strings.VarString;
      Song                   : Core.Strings.VarString;
      Details                : Core.Strings.VarString;
      Artist                 : Core.Strings.VarString;
      Accompaniment          : Core.Strings.VarString;
      Composer               : Core.Strings.VarString;
      TrackNumber            : Core.Strings.VarString;
      Year                   : Core.Strings.VarString;
      Genre                  : Core.Strings.VarString;
      Group                  : Core.Strings.VarString;
    end;
    class procedure Copy(var Source,Dest:TItem);

    class procedure Init(var Items:TItems); overload;
    class procedure Init(var Item:TItem); overload;

    class procedure Empty(Var Items:TItems); overload;
    class procedure Empty(var Item:TItem); overload;

    class procedure Done(var Item:TItem); overload;
    class procedure Done(var Items:TItems); overload;

    class function  Add(itmP:PItem; var Items:TItems): LongInt; overload;
    class function  Add(var Item:TItem; var Items:TItems): LongInt; overload;

    class function toXML(var Items:TItems; Refactor:TStream):Core.Strings.VarString; overload;
    class function toXML(var Item:TItem; Output:TStream):boolean; overload;
    class function fromXML(xNode:TDOMNode; var Items:TItems):boolean; overload;
    class function fromXML(xNode:TDOMNode; var Item:TItem):boolean; overload;
  end;
  DB=class
  Type
    XML=class
    type
      Stanzas=class
      const
        Items                  : Core.Database.Types.VarString = 'files';
        Item                   : Core.Database.Types.VarString = 'file';
      end;
    const
      ID                       : Core.Database.Types.VarString = 'id';
      FolderID                 : Core.Database.Types.VarString = 'fid';
      OwnerID                  : Core.Database.Types.VarString = 'oid';
      NetworkID                : Core.Database.Types.VarString = 'nid';
      Created                  : Core.Database.Types.VarString = 'ctd';
      Modified                 : Core.Database.Types.VarString = 'mtd';
      Allocated                : Core.Database.Types.VarString = 'atd';
      Kind                     : Core.Database.Types.VarString = 'k';
      Name                     : Core.Database.Types.VarString = 'n';
      Size                     : Core.Database.Types.VarString = 'z';
      Digest                   : Core.Database.Types.VarString = 'd';
      Summary                  : Core.Database.Types.VarString = 's';
    end;
    Keys=class
    const
      ID                       : Core.Database.Types.VarString = 'ITMID';
      InsertID                 : Core.Database.Types.VarString = 'ITMIID';
      DomainID                 : Core.Database.Types.VarString = 'ITMDID';
      NetworkID                : Core.Database.Types.VarString = 'ITMNID';
      OwnerID                  : Core.Database.Types.VarString = 'ITMOID';
      FolderID                 : Core.Database.Types.VarString = 'ITMFID';
      Created                  : Core.Database.Types.VarString = 'ITMDTC';
      Modified                 : Core.Database.Types.VarString = 'ITMDTM';
      Allocated                : Core.Database.Types.VarString = 'ITMALO';
      Digest                   : Core.Database.Types.VarString = 'ITMMD5';
      Kind                     : Core.Database.Types.VarString = 'ITMKND';
      Path                     : Core.Database.Types.VarString = 'ITMPTH';
      Name                     : Core.Database.Types.VarString = 'ITMNME';
      Summary                  : Core.Database.Types.VarString = 'ITMSMY';
      Size                     : Core.Database.Types.VarString = 'ITMSZE';
      InFolders                : Core.Database.Types.VarString = 'ITMFID';
    end;
    IDs=class
    const
      ID                       : Core.Database.Types.Integer = 0;
      InsertID                 : Core.Database.Types.Integer = 1;
      DomainID                 : Core.Database.Types.Integer = 2;
      NetworkID                : Core.Database.Types.Integer = 3;
      OwnerID                  : Core.Database.Types.Integer = 4;
      FolderID                 : Core.Database.Types.Integer = 5;
      Created                  : Core.Database.Types.Integer = 6;
      Modified                 : Core.Database.Types.Integer = 7;
      Allocated                : Core.Database.Types.Integer = 8;
      Digest                   : Core.Database.Types.Integer = 9;
      Kind                     : Core.Database.Types.Integer = 10;
      Size                     : Core.Database.Types.Integer = 11;
      Name                     : Core.Database.Types.Integer = 12;
      Summary                  : Core.Database.Types.Integer = 13;
      InFolders                : Core.Database.Types.Integer = 14;
    end;
  const
    TableP: Core.Database.Types.PTable = nil;
    MonitorP: Core.Database.Monitor.Types.PItem = nil;
    Startup: Core.Database.Types.TableIni = (
      AutoCreate               : True;
      AutoCommit               : True;
      Group                    : 'System/Applications/Social/Storage';
      Name                     : 'Files';
      Value                    : 'scs_soc_fls';
      Hint                     : 'Storage of files for social networks';
      PrimaryKeyP              : @Keys.ID;
      );
    Fields: array [0..14] of Core.Database.Types.Field = (
      (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
      (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.FolderID; KeyP: @Keys.FolderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Allocated; KeyP: @Keys.Allocated; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Digest; KeyP: @Keys.Digest; DataType: dftMD5Digest; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Size; KeyP: @Keys.Size; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
      (IDP: @IDs.Summary; KeyP: @Keys.Summary; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.InFolders; KeyP: @Keys.InFolders; DataType: dftQWordArray; AutoCreate: False; Verified: True; Precision: 0; Flags: cfNone;)
    );

  end;
const
  Allocate_Creating         = 1;
  Allocate_Waiting          = 2;
  Allocate_Writing          = 3;
  Allocate_Base             = 4;
  Allocate_RetryThreshold   = 2;
  Allocate_Window           = 5; // minutes;
type
  PSFile=^TSFile;
  PSFiles=^TSFiles;
  TSFiles=Array of PSFile;

  TSFile=record
    ID                       : QWord;
    OwnerID                  : QWord;
    FolderID                 : QWord;
    NetworkID                : QWord;
    Size                     : QWord;
    Kind                     : LongInt;
    Created                  : Core.Database.Types.DateTime;
    Modified                 : Core.Database.Types.DateTime;
    Allocated                : Core.Database.Types.DateTime;
    Digest                   : TMD5Digest;
    Name                     : Core.Strings.VarString;
    Summary                  : Core.Strings.VarString;
    Verified                 : boolean;
    PipeP                    : Storage.Social.Sync.Pipes.PItem;
  end;

  TFilesEvent=procedure(var Items:TSFiles) of object;
  TFileEvent=procedure(var Item:TSFile) of object;

  function  toXML(var Item:TSFile; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean; overload;
  function  toXML(var Item:TSFiles; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean; overload;

  function  fromXML(xItem:TDOMNode; var Item:TSFile):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Item:TSFile):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Items:TSFiles):boolean; overload;

  function  Get(ID:QWord; var Items:TSFiles):PSFile; overload;
  function  Get(Name:Core.Strings.VarString; Var Items:TSFiles):PSFile;
  function  Get(FolderID:QWord; Name:Core.Strings.VarString; var Items:TSFiles):PSFile; overload;
  function  Get(FolderID,ID:QWord; var Items:TSFiles):PSFile; overload;

  function  Create(var Items:TSFiles):PSFile;
  procedure Invalidate(var Items:TSFiles);

  function  Consumption(Task:Core.Database.Types.TTask; Var DomainID,UserID,Value:QWord):boolean;



  function  Add(var Item:TSFile; var Items:TSFiles): LongInt; overload;
  function  Add(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID:QWord; Var Item:TSFile; Data:TStream):Boolean; overload;
  function  Add(Name:Core.Strings.VarString; OwnerID,NetworkID,FolderID,Size:QWord; aKind:LongInt; var Digest:TMD5Digest; Var Items:TSFiles):PSFile; overload;

  function  Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID,FolderID:QWord; var ItemID:QWord; Name:Core.Strings.VarString; dtCreated:Double=0; iKind:LongInt=0): Boolean; overload;
  function  Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID,FolderID:QWord; var ItemID:QWord; var Name,Data:Core.Strings.VarString): Boolean; overload;

  function  SetDigest(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; var Size:QWord; var Digest:TMD5Digest): Boolean;
  function  SetCreatedStamp(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; Stamp:Double): Boolean;
  function  SetModifiedStamp(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; Stamp:Double): Boolean;
  function  SetAllocatedStamp(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; Stamp:Double): Boolean;

  function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,ItemID:QWord): Boolean;

  function  Remove(var Item:TSFile; var Items:TSFiles): LongInt; overload;
  function  Remove(Index:LongInt; var Items:TSFiles): LongInt; overload;

  function  Clear(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,NetworkID,FolderID:QWord): Boolean;
  function  Fill (Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,ItemID:QWord; Var Item:TSFile; var Data:TFileStream):Boolean; overload;
  function  Fill (Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; Var Item:TSFile):Boolean; overload;
  function  Fill (Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Value:Core.Strings.VarString; Var Item:TSFile):Boolean; overload;
  function  Force(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,OwnerID:QWord; iKind:LongInt; Created:double; Name:Core.Strings.VarString; Var Item:TSFile; var Data:TFileStream):Boolean;
  function  Data(var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,ItemID:QWord; var Stream:TFileStream):Boolean;
  function  Extension(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID,ItemID:QWord; var Value:Core.Strings.VarString):Boolean;
  function  Exists(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Value:Core.Strings.VarString):Boolean;

  function  IndexOf(ID:QWord; Var Items:TSFiles): LongInt; overload;
  function  IndexOf(Name:Core.Strings.VarString; Var Items:TSFiles): LongInt; overload;
  function  IndexOf(var Item:TSFile; Var Items:TSFiles): LongInt; overload;

  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Items:Core.Arrays.Types.LargeWord; SortField:Byte=0):Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Items:TSFiles; SortField:Byte=0):Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TSFiles; SortField:Byte=0):Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TSFiles; var Pattern:Core.Arrays.Types.VarString; SortField:Byte=0):Boolean; overload;

  function  ListAll(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Var Items:TSFiles):Boolean; overload;
  function  ListAll(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Var Items:Core.Arrays.Types.LargeWord):Boolean; overload;

  function  Write(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Var DomainID,NetworkID:QWord; var Item:TSFile; Const UpdateStamp:Boolean):Boolean;
  function  Refresh(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Var DomainID,NetworkID:QWord; var Item:TSFile):Boolean;

  function  Rename(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TSFile):Boolean;
  function  SetSummary(Task:Core.Database.Types.TTask; var DomainID,NetworkID:QWord; var Item:TSFile):Boolean;
  function  Move(Task:Core.Database.Types.TTask; Var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,ItemID,FolderID,NewFolderID:QWord): Boolean;


  function  Copy(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID,FolderID,ItemID:QWord; var NewItemID:QWord): Boolean; overload;
  procedure Copy(Var Source,Destination:TSFile); overload;
  function  Compare(var Val1,Val2:TMD5Digest):boolean;


  procedure Clear(var Items:TSFiles);

  procedure Empty(Var Item:TSFile); overload;
  procedure Empty(Var Items:TSFiles); overload;

  procedure Done(Var Item:TSFile); overload;
  procedure Done(Var Items:TSFiles); overload;

  procedure Init(var Item:TSFile); overload;
  procedure Init(var Items:TSFiles); overload;

implementation

procedure cbDestroyFiles(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP=DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  
  procedure PushUserDeleted;
  begin
     if (ItemP=DB.MonitorP) then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.OwnerID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;
begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED      : PushDomainDeleted;
    Core.Database.Monitor.Notify.USER_DELETED        : PushUserDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then  begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFiles, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;


class function Music.toXML(var Item:TItem; Output:TStream):boolean;
begin
  with Core.XML.DB do begin
    Core.Streams.Write(
      Concat(
        '<',XML.Tag,'>',
        Print(XML.Fields.Kind,Item.Kind),
        Print(XML.Fields.Name,Item.Name,CDATA_OFF),
        Print(XML.Fields.Value,Item.Value,CDATA_ON),
        '</',XML.Tag,'>'
      ),
      Output
    );
  end;
  Result:=true;
end;

class function Music.toXML(var Items:TItems; Refactor:TStream):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  Refactor.Size:=0;
  Try
    with Core.XML.DB do begin
      Core.Streams.Write(Print(XML.Fields.Inspected,Items.Inspected),Refactor);
      Core.Streams.Write(Print(XML.Fields.Album,Items.Album,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Song,Items.Song,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Details,Items.Details,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Artist,Items.Artist,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Accompaniment,Items.Accompaniment,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Composer,Items.Composer,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.TrackNumber,Items.TrackNumber,CDATA_OFF),Refactor);
      Core.Streams.Write(Print(XML.Fields.Year,Items.Year,CDATA_OFF),Refactor);
      Core.Streams.Write(Print(XML.Fields.Genre,Items.Genre,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Group,Items.Group,CDATA_ON),Refactor);
      Core.Streams.Write(Concat('<',XML.Tags,'>'),Refactor);
      for iLcv:=0 to High(Items.Tags) do
        Music.toXML(Items.Tags[iLcv]^,Refactor);
      Core.Streams.Write(Concat('</',XML.Tags,'>'),Refactor);
    end;
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Size:=0;
  end;
end;

class function Music.fromXML(xNode:TDOMNode; var Item:TItem):boolean;
begin
  with Core.XML.DB do begin
    Item.Kind:=toInteger(xNode,XML.Fields.Kind);
    Item.Name:=toString(xNode,XML.Fields.Name);
    Item.Value:=toString(xNode,XML.Fields.Value);
  end;
  Result:=True;
end;

class function Music.fromXML(xNode:TDOMNode; var Items:TItems):boolean;
var
  xTags:TDOMNode;
  xTag:TDOMNode;
  itmP:PItem;
  iLcv:LongInt;
begin
  Result:=false; Empty(Items);
  Items.Inspected:=Core.XML.DB.toBoolean(xNode,XML.Fields.Inspected);
  Items.Album:=Core.XML.DB.toString(xNode,XML.Fields.Album);
  Items.Song:=Core.XML.DB.toString(xNode,XML.Fields.Song);
  Items.Details:=Core.XML.DB.toString(xNode,XML.Fields.Details);
  Items.Artist:=Core.XML.DB.toString(xNode,XML.Fields.Artist);
  Items.Accompaniment:=Core.XML.DB.toString(xNode,XML.Fields.Accompaniment);
  Items.Composer:=Core.XML.DB.toString(xNode,XML.Fields.Composer);
  Items.TrackNumber:=Core.XML.DB.toString(xNode,XML.Fields.TrackNumber);
  Items.Year:=Core.XML.DB.toString(xNode,XML.Fields.Year);
  Items.Genre:=Core.XML.DB.toString(xNode,XML.Fields.Genre);
  Items.Group:=Core.XML.DB.toString(xNode,XML.Fields.Group);

  xTags:=Core.XML.DB.getChildNode(xNode,XML.Tags);
  if xTags<>nil then begin
    for iLcv:=0 To xTags.ChildNodes.Count-1 do begin
      xTag:=xTags.ChildNodes[iLcv];
      if Sysutils.SameText(xTag.NodeName,XML.Tag) then begin
        new(itmP);
        Init(itmP^);
        Add(itmP,Items);
        fromXML(xTag,itmP^);
      end;
    end;
    Result:=True;
  end;
end;

class procedure Music.Init(var Items:TItems);
var
  iLcv:LongInt;
begin
  Items.Inspected:=False;
  for iLcv:=0 to High(Items.Tags) do begin
    Done(Items.Tags[iLcv]^);
    Dispose(Items.Tags[iLcv]);
  end;
  SetLength(Items.Tags,0);

  SetLength(Items.Album,0);
  SetLength(Items.Song,0);
  SetLength(Items.Artist,0);
  SetLength(Items.Details,0);
  SetLength(Items.Accompaniment,0);
  SetLength(Items.Composer,0);
  SetLength(Items.TrackNumber,0);
  SetLength(Items.Year,0);
  SetLength(Items.Genre,0);
  SetLength(Items.Group,0);
end;

class procedure Music.Init(var Item:TItem);
begin
  Item.Kind:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
end;

class procedure Music.Empty(Var Items:TItems);
var
  iLcv:LongInt;
begin
  Items.Inspected:=false;
  for iLcv:=0 to High(Items.Tags) do begin
    Done(Items.Tags[iLcv]^);
    Dispose(Items.Tags[iLcv]);
  end;
  SetLength(Items.Tags,0);

  SetLength(Items.Album,0);
  SetLength(Items.Song,0);
  SetLength(Items.Details,0);
  SetLength(Items.Artist,0);
  SetLength(Items.Accompaniment,0);
  SetLength(Items.Composer,0);
  SetLength(Items.TrackNumber,0);
  SetLength(Items.Year,0);
  SetLength(Items.Genre,0);
  SetLength(Items.Group,0);
end;

class procedure Music.Empty(var Item:TItem);
begin
  Item.Kind:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
end;

class procedure Music.Done(var Item:TItem);
begin
  Finalize(Item.Name);
  Finalize(Item.Value);
  Finalize(Item);
end;

class procedure Music.Done(var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items.Tags) do begin
    Done(Items.Tags[iLcv]^);
    Dispose(Items.Tags[iLcv]);
  end;
  Finalize(Items.Album);
  Finalize(Items.Song);
  Finalize(Items.Details);
  Finalize(Items.Artist);
  Finalize(Items.Accompaniment);
  Finalize(Items.Composer);
  Finalize(Items.TrackNumber);
  Finalize(Items.Year);
  Finalize(Items.Genre);
  Finalize(Items.Group);

  Finalize(Items);
end;

class procedure Music.Copy(var Source,Dest:TItem);
begin
  Dest.Kind:=Source.Kind;
  Dest.Name:=Source.Name;
  Dest.Value:=Source.Value;
end;

class function  Music.Add(itmP:PItem; var Items:TItems): LongInt;
begin
  Result:=System.Length(Items.Tags);
  System.SetLength(Items.Tags,Result+1);
  Items.Tags[Result]:=itmP;
end;


class function  Music.Add(var Item:TItem; var Items:TItems): LongInt;
var
  itmP:PItem;
begin
  new(itmP);
  Init(itmP^);
  Copy(Item,itmP^);
  Result:=Add(itmP,Items);
end;

procedure Init(var Item:TSFile);
begin
  Item.ID:=0;
  Item.OwnerID:=0;
  Item.FolderID:=0;
  Item.NetworkID:=0;
  Item.Size:=0;
  Item.Kind:=0;
  Item.Created:=0.0;
  Item.Modified:=0.0;
  Item.Allocated:=0.0;
  Item.Verified:=false;
  System.SetLength(Item.Name,0);
  System.SetLength(Item.Summary,0);
  FillByte(Item.Digest,SizeOf(Item.Digest),0);
end;

procedure Empty(Var Item:TSFile);
begin
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.FolderID:=0;
  Item.OwnerID:=0;
  Item.Size:=0;
  Item.Kind:=0;
  Item.Created:=0.0;
  Item.Modified:=0.0;
  Item.Allocated:=0.0;
  Item.Verified:=false;
  System.SetLength(Item.Summary,0);
  SetLength(Item.Name,0);
end;

procedure Clear(var Items:TSFiles);
begin
  SetLength(Items,0);
end;

procedure Init(Var Items:TSFiles);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    Done(Items[iLcv]^);
    Dispose(Items[iLcv]);
  end;
  SetLength(Items,0);
end;

procedure Empty(Var Items:TSFiles);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    Done(Items[iLcv]^);
    Dispose(Items[iLcv]);
  end;
  SetLength(Items,0);
end;

procedure Done(Var Item:TSFile);
begin
  Finalize(Item.Name);
  Finalize(Item.Summary);
  Finalize(Item);
end;

procedure Done(Var Items:TSFiles);
var
  iLcv:LongInt;
  itmP:PSFile;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  System.SetLength(Items,0);
  Finalize(Items);
end;

function  Get(ID:QWord; var Items:TSFiles):PSFile;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]^.ID=ID then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

function  Get(FolderID:QWord; Name:Core.Strings.VarString; var Items:TSFiles):PSFile;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Items) do begin
    if ( (Items[iLcv]^.FolderID=FolderID) and SameText(Items[iLcv]^.Name,Name) ) then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

function  Get(FolderID,ID:QWord; var Items:TSFiles):PSFile;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Items) do begin
    if ( (Items[iLcv]^.FolderID=FolderID) and (Items[iLcv]^.ID=ID) ) then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

function  Create(var Items:TSFiles):PSFile;
var
  itmP:PSFile;
  ct:LongInt;
begin
  New(itmP);
  Init(itmP^);
  Result:=itmP;
  ct:=System.Length(Items);
  SetLength(Items,ct+1);
  Items[ct]:=itmP;
end;

function  Remove(var Item:TSFile; var Items:TSFiles): LongInt;
var
  iCt:LongInt;
  iLcv:LongInt;
begin
  Result:=IndexOf(Item,Items);
  if (Result<>-1) then begin
    iCt:=System.Length(Items);
    for iLcv:=Result to iCt-2 do
      Items[iLcv]:=Items[iLcv+1];
    System.SetLength(Items,iCt-1);
  end;
end;


function  Remove(Index:LongInt; var Items:TSFiles): LongInt;
var
  iCt:LongInt;
  iLcv:LongInt;
begin
  Result:=Index;
  if (Index<>-1) then begin
    iCt:=System.Length(Items);
    for iLcv:=Index to iCt-2 do
      Items[iLcv]:=Items[iLcv+1];
    System.SetLength(Items,iCt-1);
  end;
end;

function  Get(Name:Core.Strings.VarString; Var Items:TSFiles):PSFile;
var
  iLcv:LongInt;
begin
  Result:=nil;;
  for iLcv:=0 to High(Items) do begin
    if SameText(Items[iLcv]^.Name,Name) then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

function  Add(var Item:TSFile; var Items:TSFiles): LongInt;
var
  iCt:LongInt;
begin
  Result:=-1;
  if Item.ID<>0 then
    Result:=IndexOf(Item.ID,Items);
  if (Result=-1)then begin
    iCt:=System.Length(Items);
    System.SetLength(Items,iCt+1);
    Items[iCt]:=@Item;
    Result:=iCt;
  end;
end;

function Add(Name:Core.Strings.VarString; OwnerID,NetworkID,FolderID,Size:QWord; aKind:LongInt; var Digest:TMD5Digest; Var Items:TSFiles):PSFile;
begin
  Result:=Create(Items);
  Result^.OwnerID:=OwnerID;
  Result^.NetworkID:=NetworkID;
  Result^.FolderID:=FolderID;

  Result^.Size:=Size;
  Result^.Kind:=aKind;
  Result^.Name:=Name;
  Core.Arrays.Bytes.Copy(Digest,Result^.Digest);
end;

function  Add(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID:QWord; var Item:TSFile; Data:TStream):Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  FSData                         : TFileStream;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(Integer));
    Item.ID:=0;
    if (Item.Created=0) then
      Item.Created:=Core.Timer.dtUT;
    if (Item.Modified=0) then
      Item.Modified:=Core.Timer.dtUT;
    if (Data<>nil) then begin
      Item.Size:=Data.Size;
      Item.Allocated:=Core.Timer.dtUT;
      Core.Streams.CheckSum(Data,Item.Digest);
    end else begin
      Item.Allocated:=Allocate_Waiting;
      Item.Size:=0;
      Core.Arrays.Bytes.Empty(Item.Digest);
    end;
    Item.NetworkID:=NetworkID;
    Item.OwnerID:=OwnerID;

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);
    {$i Storage.Social.File.Add.Fields.inc}
    Result:=Core.Database.SQL.Insert(Task,@Commands);
    if Item.ID<>0 then begin
      Storage.AuraDisks.Files.Create(Node,DomainID,NetworkID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.Social,FSData);
      Try
        if Data<>nil then
          Core.Streams.Copy(Data,FSData);
      finally
        FSData.Free();
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cb_Files_Fill_Name(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PString(DataP)^:=Fields.FieldByName(DB.Keys.Name).AsString;
end;

procedure cb_Files_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmP:PSFile;
begin
  itmP:=DataP;
  {$i Storage.Social.File.Fill.inc}
  itmP^.Summary:=Fields.FieldByName(DB.Keys.Summary).AsString;
end;

procedure cb_Files_Fill_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmsP:PSFiles;
  itmP:PSFile;
  iLen:LongInt;
begin
  itmsP:=DataP;
  iLen:=System.Length(itmsP^);
  System.SetLength(itmsP^,iLen+1);
  System.New(itmP);
  Init(itmP^);
  {$i Storage.Social.File.Fill.inc}
  itmP^.Summary:=Fields.FieldByName(DB.Keys.Summary).AsString;
  itmsP^[iLen]:=itmP;
end;

procedure cb_Files_Fill_List_All(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmsP:PSFiles;
  itmP:PSFile;
  iLen:LongInt;
begin
  itmsP:=DataP;
  iLen:=System.Length(itmsP^);
  System.SetLength(itmsP^,iLen+1);
  System.New(itmP);
  Init(itmP^);
  {$i Storage.Social.File.Fill.inc}
  itmsP^[iLen]:=itmP;
end;

procedure cb_Files_Fill_List_IDs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.LargeWord.Add(Fields.FieldByName(DB.Keys.ID).AsLargeInt,PLargeWordArray(DataP)^);
end;

function Data(var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,ItemID:QWord; var Stream:TFileStream):Boolean;
begin
  Result:=Storage.AuraDisks.Files.Read(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social,Stream);
end;

function Fill(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,ItemID:QWord; Var Item:TSFile; var Data:TFileStream):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill,@Item) and (Item.ID<>0);

    if Result then
      Result:=Storage.AuraDisks.Files.Acquire(Node,DomainID,NetworkID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.Social,Data);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Fill(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; Var Item:TSFile):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill,@Item) and (Item.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  Fill (Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Value:Core.Strings.VarString; Var Item:TSFile):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poAnd,oEqual,Value,Commands);
    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill,@Item) and (Item.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Extension(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID,ItemID:QWord; var Value:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    SetLength(Value,0);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_Name,@Value);
    Value:=Core.Utils.Files.Extract(Value,efeoNone);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Exists(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; var Value:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  fCount                         : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poAnd,oEqual,Value,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,fCount) and (fCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Force(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,OwnerID:QWord; iKind:LongInt; Created:double; Name:Core.Strings.VarString; Var Item:TSFile; var Data:TFileStream):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Empty(Item);
    iCount:=0;

    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poAnd,oEqual,Name,Commands);

    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill,@Item) and (Item.ID<>0);
    Item.Kind:=iKind;
    Item.Created:=Created;
    if (Item.ID=0) then begin
      Item.FolderID:=FolderID;
      Item.NetworkID:=NetworkID;
      Item.OwnerID:=OwnerID;
      Item.Name:=Name;
      Create(Task,Node,DomainID,NetworkID,OwnerID,FolderID,Item.ID,Item.Name,Item.Created,iKind);
    end;
    if (Item.ID<>0) then begin
      Result:=Storage.AuraDisks.Files.Acquire(Node,DomainID,NetworkID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.Social,Data);
    end else
      Result:=False;
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  Clear(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,NetworkID,FolderID:QWord): Boolean;
var
  iCount                         : LongInt;
  iLcv                           : LongInt;
  IDs                            : Core.Arrays.Types.LargeWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  List(Task,DomainID,NetworkID,FolderID,IDs,DB.IDs.ID);
  Try
    for iLcv:=0 to High(IDs) do
      Storage.AuraDisks.Files.Delete(Node,DomainID,NetworkID,FolderID,IDs[iLcv],Storage.AuraDisks.Kinds.Social);
  finally
    Core.Arrays.LargeWord.Done(IDs);
  end;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);

    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  IndexOf(ID:QWord; Var Items:TSFiles): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]^.ID=ID then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

function  IndexOf(Name:Core.Strings.VarString; Var Items:TSFiles): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if SameText(Name,Items[iLcv]^.Name) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

function  IndexOf(Var Item:TSFile; Var Items:TSFiles): LongInt;
var
  iLcv:LongInt;
  itmP:PSFile;
begin
  itmP:=@Item;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]=itmP then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

procedure  Invalidate(Var Items:TSFiles);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]<>nil then
      Items[iLcv]^.Verified:=false;
  end;
end;


function List(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Items:Core.Arrays.Types.LargeWord; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Arrays.LargeWord.Empty(Items);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);


    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_List_IDs,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function List(Task:Core.Database.Types.TTask; DomainID,NetworkID,FolderID:QWord; Var Items:TSFiles; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Empty(Items);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);


    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_List,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TSFiles; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Items);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InFolders,poAnd,oIn,Folders,Commands);


    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_List,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TSFiles; var Pattern:Core.Arrays.Types.VarString; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  iPatternCount                  : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Items);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InFolders,poAnd,oIn,Folders,Commands);


    iPatternCount:=System.Length(Pattern);
    if iPatternCount>0 then begin
      if iPatternCount>1 then begin
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poNone,oLike,Pattern[0],Commands);
        for iLcv:=1 to iPatternCount-2 do
          Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poOr,oLike,Pattern[iLcv],Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poOr,oLike,Pattern[iPatternCount-1],Commands);

        Core.Database.AddCommand(iCount,DB.TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);
      end else begin
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Name,poAnd,oLike,Pattern[0],Commands);
      end;
    end;

    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_List,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function ListAll(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Var Items:TSFiles):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Items);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    {$i Storage.Social.Files.Fields.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_List_All,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function ListAll(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Var Items:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Arrays.LargeWord.Empty(Items);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill_List_IDS,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  toXML(var Item:TSFiles; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(DB.XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,Refactor,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(DB.XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  Result:=true;
end;

function  toXML(var Item:TSFile; Output:TMemoryStream; Refactor:TStream; Header:Boolean):boolean;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(DB.XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(DB.XML.ID,Item.ID),Output);
    Core.Streams.Write(Print(DB.XML.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(DB.XML.OwnerID,Item.OwnerID),Output);
    Core.Streams.Write(Print(DB.XML.FolderID,Item.FolderID),Output);
    Core.Streams.Write(Print(DB.XML.Size,Item.Size),Output);
    Core.Streams.Write(Print(DB.XML.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(DB.XML.Created,Item.Created),Output);
    Core.Streams.Write(Print(DB.XML.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(DB.XML.Allocated,Item.Allocated),Output);
    Core.Streams.Write(Print(DB.XML.Digest,Item.Digest),Output);
    Core.Streams.Write(Print(DB.XML.Name,Item.Name),Output);
    Core.Streams.Write(Print(DB.XML.Summary,Item.Summary,CDATA_OFF),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(DB.XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=true;
end;

function  fromXML(xItem:TDOMNode; var Item:TSFile):boolean;
var
  xSmry:TDOMNode;
begin
  Result:=False;
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      xSmry:=getChildNode(xItem,DB.XML.Summary);
      Item.ID:=toQWord(xItem,DB.XML.ID);
      Item.NetworkID:=toQWord(xItem,DB.XML.NetworkID);
      Item.OwnerID:=toQWord(xItem,DB.XML.OwnerID);
      Item.FolderID:=toQWord(xItem,DB.XML.FolderID);
      Item.Size:=toQword(xItem,DB.XML.Size);
      Item.Kind:=toInteger(xItem,DB.XML.Kind);
      Item.Created:=toDouble(xItem,DB.XML.Created);
      Item.Modified:=toDouble(xItem,DB.XML.Modified);
      Item.Allocated:=toDouble(xItem,DB.XML.Allocated);
      Item.Name:=toString(xItem,DB.XML.Name);
      Item.Summary:=getNodeText(xSmry);
      toMD5Digest(xItem,DB.XML.Digest,Item.Digest);
      Item.Verified:=true;
      Result:=True;
    end;
  end;
end;

function  fromXML(xDoc:TXMLDocument; var Item:TSFile):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  xItem:=Core.XML.DB.getNode(xDoc,DB.XML.Stanzas.Item);
  Result:=fromXML(xItem,Item);
end;

function  fromXML(xDoc:TXMLDocument; var Items:TSFiles):boolean;
var
  xItems    : TDOMNode;
  xItem     : TDOMNode;
  xSmry     : TDOMNode;
  iCount    : LongInt;
  iLcv      : LongInt;
  itmP      : PSFile;
  iID       : QWord;
begin
  Result:=False;
  xItems:=Core.XML.DB.getNode(xDoc,DB.XML.Stanzas.Items);
  Invalidate(Items);
  if (xItems<>nil) then begin
    iCount:=0;
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      xItem:=xItems.ChildNodes[iLcv];
      with Core.XML.DB do begin
        xSmry:=getChildNode(xItem,DB.XML.Summary);
        iID:=toQWord(xItem,DB.XML.ID);

        itmP:=Get(iID,Items);
        if (itmP=nil) then begin
          new(itmP);
          Init(itmP^);
        end;

        itmP^.ID:=iID;
        itmP^.NetworkID:=toQWord(xItem,DB.XML.NetworkID);
        itmP^.OwnerID:=toQWord(xItem,DB.XML.OwnerID);
        itmP^.FolderID:=toQWord(xItem,DB.XML.FolderID);
        itmP^.Size:=toQWord(xItem,DB.XML.Size);
        itmP^.Kind:=toInteger(xItem,DB.XML.Kind);
        itmP^.Created:=toDouble(xItem,DB.XML.Created);
        itmP^.Modified:=toDouble(xItem,DB.XML.Modified);
        itmP^.Allocated:=toDouble(xItem,DB.XML.Allocated);
        itmP^.Name:=toString(xItem,DB.XML.Name);
        itmP^.Summary:=getNodeText(xSmry);

        toMD5Digest(xItem,DB.XML.Digest,itmP^.Digest);

        itmP^.Verified:=true;
        Result:=True;
      end;
      SetLength(Items,iCount+1);
      Items[iCount]:=itmP;
      Inc(iCount);
    end;
  end;
end;

function  Move(Task:Core.Database.Types.TTask; Var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,ItemID,FolderID,NewFolderID:QWord): Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;

  FSSource:TFileStream;
  FSDest:TFileStream;
begin
  Result:=False; iCount:=0;
  Try
    if Storage.AuraDisks.Files.Exists(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social) then begin
      Storage.AuraDisks.Files.Read(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social,FSSource);
      Storage.AuraDisks.Files.Create(Node,DomainID,NetworkID,NewFolderID,ItemID,Storage.AuraDisks.Kinds.Social,FSDest);
      Core.Streams.Copy(FSSource,FSDest);
      FSSource.Size:=0;
      FSSource.Free();
      FSDest.Free();
      Storage.AuraDisks.Files.Delete(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social);
    end;

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,ItemID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.FolderID,poNone,oNone,NewFolderID,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure Copy(Var Source,Destination:TSFile);
begin
  With Destination do begin
    ID:=Source.ID;
    NetworkID:=Source.NetworkID;
    OwnerID:=Source.OwnerID;
    FolderID:=Source.FolderID;
    Size:=Source.Size;
    Kind:=Source.Kind;
    Created:=Source.Created;
    Modified:=Source.Modified;
    Allocated:=Source.Allocated;
    Core.Arrays.Bytes.Copy(Source.Digest,Digest);
    Name:=Source.Name;
    Summary:=Source.Summary;
  end;
end;

function  Compare(var Val1,Val2:TMD5Digest):boolean;
begin
  Result:=SysUtils.CompareMem(@Val1[0],@Val2[0],SizeOf(TMD5Digest));
end;

function  Copy(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID,FolderID,ItemID:QWord; var NewItemID:QWord): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Item                           : TSFile;
  FSSource                       : TFileStream;
  FSDest                         : TFileStream;
begin
  iCount:=0;
  Result:=False;
  Empty(Item);
  Try
    Item.ID:=ItemID;
    Item.NetworkID:=NetworkID;
    if Fill(Task,Node,DomainID,NetworkID,FolderID,ItemID,Item,FSSource) then begin
      Try
        Try
          Item.OwnerID:=OwnerID;
          Item.ID:=0;
          Core.Database.AddCommand(iCount,DB.TableP,@Commands);
          // Setup Primary ID
          Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
          Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
          Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
          Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);

          {$i Storage.Social.File.Add.Fields.inc}
          Result:=Core.Database.SQL.Insert(Task,@Commands);
          NewItemID:=Item.ID;
          if Item.ID<>0 then begin
            if Storage.AuraDisks.Files.Create(Node,DomainID,NetworkID,FolderID,Item.ID,Storage.AuraDisks.Kinds.Social,FSDest) then begin
              Core.Streams.Copy(FSSource,FSDest);
              FSDest.Free();
            end;
          end;
        Finally
          Core.Database.Done(Commands);
        End;
      finally
        FSSource.Free();
      end;
    end;
  finally
    Done(Item);
  end;
end;

function Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID,FolderID:QWord; var ItemID:QWord; Name:Core.Strings.VarString; dtCreated:Double=0; iKind:LongInt=0): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Allocated                      : Double;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(Integer));
    if dtCreated=0 then
      dtCreated:=Core.Timer.dtUT;
    Allocated:=Allocate_Waiting;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.OwnerID,poNone,oNone,OwnerID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.NetworkID,poNone,oNone,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.FolderID,poNone,oNone,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Kind,poNone,oNone,iKind,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Created,poNone,oNone,dtCreated,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Modified,poNone,oNone,dtCreated,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Allocated,poNone,oNone,Allocated,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Name,poNone,oNone,Name,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

    if ItemID<>0 then
      Storage.AuraDisks.Files.Create(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,OwnerID,FolderID:QWord; var ItemID:QWord; var Name,Data:Core.Strings.VarString): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Size                           : QWord;
  Commands                       : Core.Database.Types.Commands;
  dtNow                          : Double;
  Digest                         : TMD5Digest;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(Integer));
    dtNow:=Core.Timer.dtUT;
    Size:=System.Length(Data);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.NetworkID,poNone,oNone,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.OwnerID,poNone,oNone,OwnerID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.FolderID,poNone,oNone,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Digest,poNone,oNone,Digest,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Kind,poNone,oNone,Kind.Bin,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Size,poNone,oNone,Size,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Created,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Modified,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Allocated,poNone,oNone,dtNow,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Name,poNone,oNone,Name,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    if ItemID<>0 then
      Storage.AuraDisks.Files.Create(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social,Data);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  SetCreatedStamp(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; Stamp:Double): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Created,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  SetDigest(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; var Size:QWord; var Digest:TMD5Digest): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Stamp                          : Double;
begin
  Result:=False;
  Try
    iCount:=0;
    Stamp:=Core.Timer.dtUT;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Modified,poNone,oNone,Stamp,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Digest,poNone,oNone,Digest,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Size,poNone,oNone,Size,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  SetModifiedStamp(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; Stamp:Double): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Modified,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  SetAllocatedStamp(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord; Stamp:Double): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Allocated,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  Write(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,NetworkID:QWord; Var Item:TSFile; Const UpdateStamp:Boolean):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    if UpdateStamp=true then begin
      Item.Modified:=Core.Timer.dtUT;
      Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Modified,poNone,oNone,Item.Modified,Commands);
    end;
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Allocated,poNone,oNone,Item.Allocated,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Size,poNone,oNone,Item.Size,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Digest,poNone,oNone,Item.Digest,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Summary,poNone,oNone,Item.Summary,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  SetSummary(Task:Core.Database.Types.TTask; var DomainID,NetworkID:QWord; var Item:TSFile):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Modified,poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Summary,poNone,oNone,Item.Summary,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


function  Refresh(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Var DomainID,NetworkID:QWord; var Item:TSFile):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Modified,poAnd,oNotEqual,Item.Modified,Commands);

    {$i Storage.Social.Files.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Files_Fill,@Item);
  finally
    Core.Database.Done(Commands);
  End;
end;


function  Rename(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Var Item:TSFile):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Modified,poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Name,poNone,oNone,Item.Name,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,NetworkID,FolderID,ItemID:QWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Storage.AuraDisks.Files.Delete(Node,DomainID,NetworkID,FolderID,ItemID,Storage.AuraDisks.Kinds.Social);

    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

function  Consumption(Task:Core.Database.Types.TTask; Var DomainID,UserID,Value:QWord):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    with Files do begin
      Core.Database.AddCommand(iCount,DB.TableP,@Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.OwnerID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Size,poNone,oNone,Commands);
    end;
    Result:=Core.Database.SQL.Sum(Task,@Commands,Value);
  finally
    Core.Database.Done(Commands);
  end;
end;


initialization
  RegisterDB;
end.


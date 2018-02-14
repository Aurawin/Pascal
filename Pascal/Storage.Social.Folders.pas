unit Storage.Social.Folders;

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
  Core.Generics,
  Core.Strings,
  Core.Streams,
  Core.XML,

  Storage,
  Storage.Main,
  Storage.MatrixNodes,
  Storage.Social.Folders.Defaults,
  Storage.Social.Files,
  Storage.AuraDisks,

  DOM,
  MD5,
  XMLRead,
  Classes,
  SysUtils;
Const
  Defaults : Storage.Social.Folders.Defaults.Home=(
      Documents: 'Documents';
      Music    : 'Music';
      Pictures : 'Pictures';
      Videos   : 'Videos';
      Trash    : 'Trash';
  );

type
  DB=class
  type
    XML=class
    type
      Stanzas=class
      const
        Items                  : Core.Database.Types.VarString = 'folders';
        Item                   : Core.Database.Types.VarString = 'folder';
      end;
    const
      ID                       : Core.Database.Types.VarString = 'id';
      NetworkID                : Core.Database.Types.VarString = 'nid';
      OwnerID                  : Core.Database.Types.VarString = 'oid';
      Path                     : Core.Database.Types.VarString = 'path';
    end;
    Keys=class
    const
      ID                       : Core.Database.Types.VarString = 'ITMID';
      InsertID                 : Core.Database.Types.VarString = 'ITMIID';
      DomainID                 : Core.Database.Types.VarString = 'ITMDID';
      NetworkID                : Core.Database.Types.VarString = 'ITMNID';
      OwnerID                  : Core.Database.Types.VarString = 'ITMOID';
      Path                     : Core.Database.Types.VarString = 'ITMPTH';
    end;
    IDs=class
    const
      ID                       : Core.Database.Types.Integer = 0;
      InsertID                 : Core.Database.Types.Integer = 1;
      DomainID                 : Core.Database.Types.Integer = 2;
      NetworkID                : Core.Database.Types.Integer = 3;
      OwnerID                  : Core.Database.Types.Integer = 4;
      Path                     : Core.Database.Types.Integer = 5;
    end;
  const
    TableP: Core.Database.Types.PTable = nil;
    MonitorP: Core.Database.Monitor.Types.PItem = nil;
    Startup: Core.Database.Types.TableIni = (
      AutoCreate               : True;
      AutoCommit               : True;
      Group                    : 'System/Applications/Social/Storage';
      Name                     : 'Folders';
      Value                    : 'scs_soc_flds';
      Hint                     : 'Storage of folders structures for social networks';
      PrimaryKeyP              : @Keys.ID;
    );
    Fields: array [0..5] of Core.Database.Types.Field = (
      (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
      (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.DomainID; KeyP: @Keys.DomainID;  DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
      (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Path; KeyP: @Keys.Path; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
    );
  end;
Const
  FREE_FILES                 = true;
  CLEAR_FILES                = false;
type
  PSFolder=^TSFolder;
  TSFolders=Array of PSFolder;
  PSFolders=^TSFolders;
  TSFolder=record
    ID                            : QWord;
    NetworkID                     : QWord;
    OwnerID                       : QWord;
    Path                          : Core.Strings.VarString;
    // Runtime Fields
    Parent                        : PSFolder;
    Name                          : Core.Strings.VarString;
    Location                      : Core.Strings.VarString;
    Files                         : TSFiles;
    Folders                       : TSFolders;
    Verified                      : Boolean;
  end;
  GMasterList=Specialize GStructList<TSFolder>;

  TMasterList=class(GMasterList)

  public
    function Find(ID:QWord):PSFolder;
    function FindByPath(Path:Core.Strings.VarString):PSFolder;
    function fromXML(xDoc:TXMLDocument):boolean;
    procedure Invalidate();
    procedure Purge();
  public
    procedure Init(var Item:TSFolder; ParentP:PSFolder); reIntroduce;
    procedure Empty(var Item:TSFolder; const FreeFiles:boolean); reIntroduce;
    procedure Done(var Item:TSFolder; const FreeFiles:boolean); reIntroduce;
  end;

  TFoldersEvent=procedure(var Folders:TSFolders) of object;
  TFolderEvent=procedure(var Folders:TSFolder) of object;
  function  toXML(var Item:TSFolder; Output:TMemoryStream; Header:boolean):boolean;
  function  toXML(var Item:TSFolders; Output:TMemoryStream; Header:Boolean):boolean;
  function  fromXML(xDoc:TXMLDocument; var Item:TSFolder):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Items:TSFolders):boolean; overload;

  procedure Invalidate(Var Item:TSFolders);
  procedure Purge(Var Item:TSFolders); overload;
  procedure Purge(Var Item:TSFolders; Depth:LongInt); overload;

  procedure Empty(Var Item:TSFolder; const FreeFiles:boolean); overload;
  procedure Empty(Var Item:TSFolders; const FreeFiles:boolean); overload;
  procedure Init(Var Item:TSFolder; ParentP:PSFolder); overload;
  procedure Init(Var Items:TSFolders); overload;
  procedure Done(Var Item:TSFolder; const FreeFiles:boolean); overload;
  procedure Done(Var Items:TSFolders; const FreeFiles:boolean); overload;

  function  getPath(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord):Core.Strings.VarString;
  function  getFolder(iID:QWord; var List:TSFolders):PSFolder; overload;
  function  getFolder(Name:Core.Strings.VarString; var List:TSFolders):PSFolder; overload;
  function  getFolderByPath(Path:Core.Strings.VarString; var List:TSFolders):PSFolder; overload;
  function  getFolderByPath(Path:Core.Strings.VarString; var List:TMasterList):PSFolder; overload;

  function  getRoot(var Folder:TSFolder):PSFolder;
  function  getFile(Name:Core.Strings.VarString; var Folder:TSFolder):PSFile; overload;
  function  getFile(ID:QWord; var Folder:TSFolder):PSFile; overload;

  function  Force(Path:Core.Strings.VarString; NetworkID,ID:QWord; var Root:TSFolders):PSFolder; overload;

  function  Create(var Folders:TSFolders; var ParentP:PSFolder):PSFolder; overload;
  function  Create(NetworkID:QWord; var Items:TSFolders):PSFolder;

  function  Remove(FolderP:PSFolder; var Folders:TSFolders):boolean;

  function  Add(iNetworkID,iID:QWord; Path,Name:Core.Strings.VarString; var Folders:TSFolders; ParentP:PSFolder):PSFolder; overload;
  function  Add(FolderP:PSFolder; var Folders:TSFolders):PSFolder; overload;

  function  Add(Name:Core.Strings.VarString; var Digest:TMD5Digest; var Folder:TSFolder):PSFile; overload;

  procedure Organize(Master:TMasterList; var saPath:Core.Arrays.Types.VarString;  var Parent:TSFolder); overload;
  procedure Organize(Master:TMasterList; var Root:TSFolder); overload;


  function  Create(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var ItemID:QWord; Path:Core.Strings.VarString): Boolean;
  function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,NetworkID,ItemID:QWord): Boolean;
  function  Fill(Task:Core.Database.Types.TTask; DomainID,FolderID:QWord; var Folder:TSFolder):Boolean; overload;
  function  Fill(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Path:Core.Strings.VarString; var Folder:TSFolder):Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:TSFolders): Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Path:Core.Strings.VarString; var Items:TSFolders): Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Path:Core.Strings.VarString; var Items:Core.Arrays.Types.LargeWord): Boolean; overload;
  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord): Boolean; overload;

  function  Rename(Task:Core.Database.Types.TTask; var DomainID,NetworkID:QWord; var Item:TSFolder): Boolean;
  function  Rename(Task:Core.Database.Types.TTask; var DomainID,NetworkID:QWord; var Items:TSFolders): Boolean; overload;

  function  Verify(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID,ItemID:QWord; Path:Core.Strings.VarString): Boolean;
  function  CreateDefaults(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var Trash,Documents,Music,Pictures,Videos:QWord): Boolean;


implementation

procedure cbDestroyFolders(ItemP:Core.Database.Monitor.Types.PItem);
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
        Result:=False;
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
    if ItemP=DB.MonitorP then begin
      Result:=False;
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
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFolders, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

function  toXML(var Item:TSFolders; Output:TMemoryStream; Header:Boolean):boolean;
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
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(DB.XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  Result:=true;
end;

function  toXML(var Item:TSFolder; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=true;
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
    Core.Streams.Write(Print(DB.XML.Path,Item.Path),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(DB.XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
end;

function  fromXML(xDoc:TXMLDocument; var Item:TSFolder):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  xItem:=Core.XML.DB.getNode(xDoc,DB.XML.Stanzas.Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,DB.XML.ID);
      Item.OwnerID:=toQWord(xItem,DB.XML.OwnerID);
      Item.NetworkID:=toQWord(xItem,DB.XML.NetworkID);
      Item.Path:=toString(xItem,DB.XML.Path);
      Item.Verified:=true;
      Result:=True;
    end;
  end;
end;

function  fromXML(xDoc:TXMLDocument; var Items:TSFolders):boolean;
var
  xItems:TDOMNode;
  xItem:TDOMNode;
  iCount,iLcv:LongInt;
  itmP:PSFolder;
  iID:QWord;
begin
  Result:=False;
  xItems:=Core.XML.DB.getNode(xDoc,DB.XML.Stanzas.Items);
  Invalidate(Items);
  if (xItems<>nil) then begin
    iCount:=System.Length(Items);
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      xItem:=xItems.ChildNodes[iLcv];
      with Core.XML.DB do begin
        iID:=toQWord(xItem,DB.XML.ID);
        itmP:=getFolder(iID,Items);
        if (itmP=nil) then begin
          new(itmP);
          itmP^.ID:=iID;
          SetLength(Items,iCount+1);
          Items[iCount]:=itmP;
          Inc(iCount);
        end;
        itmP^.NetworkID:=toQWord(xItem,DB.XML.NetworkID);
        itmP^.OwnerID:=toQWord(xItem,DB.XML.OwnerID);
        itmP^.Path:=toString(xItem,DB.XML.Path);
        itmP^.Verified:=true;
        Result:=True;
      end;
    end;
  end;
  Purge(Items);
end;

procedure Empty(Var Item:TSFolder; const FreeFiles:boolean);
begin
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.OwnerID:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Path,0);
  SetLength(Item.Location,0);
  Storage.Social.Files.Empty(Item.Files);
  Empty(Item.Folders,FreeFiles);
end;

procedure Empty(Var Item:TSFolders; const FreeFiles:boolean);
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^,FreeFiles);
    Dispose(itmP);
  end;
  SetLength(Item,0);
end;

procedure Invalidate(Var Item:TSFolders);
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    itmP^.Verified:=false;
  end;
end;

procedure Purge(Var Item:TSFolders);
var
  iLcv:LongInt;
  itmP:PSFolder;
  jLcv:LongInt;
  firstCount:LongInt;
  Count:LongInt;

begin
  firstCount:=Length(Item);
  Count:=firstCount;
  iLcv:=0;
  While iLcv<Count do begin
    itmP:=Item[iLcv];
    if (itmP^.Verified=false) then begin
      Done(itmP^,FREE_FILES);
      Dispose(itmP);
      for jLcv:=iLcv to Count-2 do
        Item[jLcv]:=Item[jLcv+1];
      Dec(Count);
    end else
      Inc(iLcv);
  end;
  if Count<>firstCount then
    SetLength(Item,Count);
end;

procedure Purge(Var Item:TSFolders; Depth:LongInt);
var
  saPath:Core.Arrays.Types.VarString;
  fldrP:PSFolder;
  iCount:LongInt;
  iLcvDepth:LongInt;
  iBase:LongInt;
  iLcv:LongInt;
begin
  iCount:=Length(Item);
  if iCount>0 then begin
    fldrP:=Item[0]; // first one determines level
    fldrP^.Verified:=True;
    iBase:=Core.Arrays.VarString.fromString(saPath,fldrP^.Path,'/');
    for iLcv:=1 to iCount-1 do begin
      fldrP:=Item[iLcv];
      iLcvDepth:=Core.Arrays.VarString.fromString(saPath,fldrP^.Path,'/');
      fldrP^.Verified:=((iLcvDepth-iBase)<=Depth);
    end;
  end;
  Core.Arrays.VarString.Done(saPath);
  Purge(Item);
end;

procedure Init(Var Items:TSFolders);
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    Done(itmP^,FREE_FILES);
    Dispose(itmP);
  end;
  SetLength(Items,0);
end;

procedure Init(Var Item:TSFolder; ParentP:PSFolder);
begin
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.OwnerID:=0;
  Item.Parent:=ParentP;
  SetLength(Item.Name,0);
  SetLength(Item.Path,0);
  SetLength(Item.Location,0);
  Init(Item.Files);
  Init(Item.Folders);
end;

procedure Done(Var Item:TSFolder; const FreeFiles:boolean);
begin
  if (Item.Parent<>nil) then
    Remove(@Item,Item.Parent^.Folders);

  Finalize(Item.Name);
  Finalize(Item.Path);
  Finalize(Item.Location);
  if (FreeFiles=true) then begin
    Done(Item.Files);
    Done(Item.Folders,FreeFiles);
  end  else begin
    Clear(Item.Files);
    Finalize(Item.Files);
    Done(Item.Folders,false);
  end;
  Finalize(Item);
end;

procedure Done(Var Items:TSFolders; const FreeFiles:boolean);
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    Done(itmP^,FreeFiles);
    Dispose(itmP);
  end;
  System.SetLength(Items,0);
  Finalize(Items);
end;

procedure cb_List_Folders(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemsP:PSFolders;
  ItemP:PSFolder;
  iCount:LongInt;
begin
  ItemsP:=DataP;
  iCount:=System.Length(ItemsP^);
  New(ItemP);
  Init(ItemP^,nil);
  System.SetLength(ItemsP^,iCount+1);
  Itemsp^[iCount]:=ItemP;

  ItemP^.ID:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
  ItemP^.NetworkID:=Fields.FieldByName(DB.Keys.NetworkID).AsLargeInt;
  ItemP^.OwnerID:=Fields.FieldByName(DB.Keys.OwnerID).AsLargeInt;
  ItemP^.Path:=Fields.FieldByName(DB.Keys.Path).AsString;
end;

procedure cb_List_Folder_IDs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iCount:LongInt;
  ListP:PLargeWordArray;
begin
  ListP:=DataP;
  iCount:=System.Length(ListP^);
  System.SetLength(ListP^,iCount+1);
  ListP^[iCount]:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
end;

function  getFolder(iID:QWord; var List:TSFolders):PSFolder;
var
  iLcv:LongInt;
  fldr:PSFolder;
begin
  Result:=nil; iLcv:=0;
  for iLcv:=0 to High(List) do begin
     fldr:=List[iLcv];
     if (fldr^.ID=iID) then begin
       Result:=fldr;
       Exit;
     end else begin
       Result:=getFolder(iID,fldr^.Folders);
       if Result<>nil then
         Exit;
     end;
  end;
end;

function  getRoot(var Folder:TSFolder):PSFolder;
var
  lcvP:PSFolder;
begin
  Result:=nil;
  lcvP:=@Folder;
  Repeat
     if (lcvP^.Parent=nil) then
       Result:=lcvP;
     lcvP:=lcvP^.Parent;
  Until (lcvP=nil) or (Result<>nil);
end;

function  getFolder(Name:Core.Strings.VarString; var List:TSFolders):PSFolder;
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  Result:=nil;
  iLcv:=0;
  While iLcv<Length(List) do begin
    itmP:=List[iLcv];
    if SameText(itmP^.Name,Name) then begin
      Result:=itmP;
      break;
    end;
    Inc(iLcv);
  end;
end;

function  getFolderByPath(Path:Core.Strings.VarString; var List:TSFolders):PSFolder;
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  Result:=nil;
  iLcv:=0;
  While iLcv<Length(List) do begin
    itmP:=List[iLcv];
    if SameText(itmP^.Path,Path) then begin
      Result:=List[iLcv];
      break;
    end;
    Inc(iLcv);
  end;
end;

function  getFolderByPath(Path:Core.Strings.VarString; var List:TMasterList):PSFolder;
var
  iLcv:LongInt;
  itmP:PSFolder;
begin
  Result:=nil;
  iLcv:=0;
  for iLcv:=0 to List.Count-1 do begin
    itmP:=List[iLcv];
    if SameText(itmP^.Path,Path) then begin
      Result:=List[iLcv];
      break;
    end;
  end;
end;


function  getFile(Name:Core.Strings.VarString; var Folder:TSFolder):PSFile;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Folder.Files) do begin
    if SameText(Folder.Files[iLcv]^.Name,Name) then begin
      Result:=Folder.Files[iLcv];
      break;
    end;
  end;
end;

function  getFile(ID:QWord; var Folder:TSFolder):PSFile;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Folder.Files) do begin
    if (Folder.Files[iLcv]^.ID=ID) then begin
      Result:=Folder.Files[iLcv];
      break;
    end;
  end;
end;


function Force(Path:Core.Strings.VarString; NetworkID,ID:QWord; var Root:TSFolders):PSFolder;
var
  iLcv,iCt:LongInt;
  itmP:PSFolder;
  ParentP:PSFolder;
  saPath:Core.Arrays.Types.VarString;
  FoldersP:PSFolders;
  sPath:Core.Strings.VarString;
begin
  Result:=nil;
  itmP:=nil;
  ParentP:=nil;
  FoldersP:=@Root;
  SetLength(sPath,0);
  Core.Arrays.VarString.fromString(saPath,Path,'/');
  Try
    iCt:=Length(saPath); iLcv:=0;
    While (iLcv<iCt) do begin
      if iLcv=0 then
        sPath:=saPath[iLcv]
      else
        sPath:=Concat(sPath,'/',saPath[iLcv]);
      itmP:=getFolder(saPath[iLcv],FoldersP^);
      if (itmP=nil) then
        itmP:=Add(NetworkID,0,sPath,saPath[iLcv],FoldersP^,ParentP);
      itmP^.NetworkID:=NetworkID;
      FoldersP:=@itmP^.Folders;
      ParentP:=itmP;
      inc(iLcv);
    end;
    if (itmP<>nil) then itmP^.ID:=ID;
  finally
    Core.Arrays.VarString.Done(saPath);
  end;
  Result:=itmP;
end;

function  Add(iNetworkID,iID:QWord; Path,Name:Core.Strings.VarString; Var Folders:TSFolders; ParentP:PSFolder):PSFolder;
var
  iCt:LongInt;
  itmP:PSFolder;
begin
  new(itmP);
  Init(itmP^,ParentP);
  itmP^.ID:=iID;
  itmP^.NetworkID:=iNetworkID;
  itmP^.Name:=Name;
  itmP^.Path:=Path;
  iCt:=System.Length(Folders);
  SetLength(Folders,iCt+1);
  Folders[iCt]:=itmP;

  Result:=itmP;

  itmP:=nil;
end;

function  Remove(FolderP:PSFolder; var Folders:TSFolders):boolean;
var
  iLcv,jLcv:integer;
  iCount:integer;
begin
  Result:=false; iCount:=Length(Folders);
  for iLcv:=0 to iCount-1 do begin
    if (Folders[iLcv]=FolderP) then begin
      for jLcv:=iLcv to iCount-2 do
        Folders[jLcv]:=Folders[jLcv+1];
      Result:=true;
      break;
    end;
  end;
  if (Result=true) then
    SetLength(Folders,iCount-1);
end;

procedure Organize(Master:TMasterList; var saPath:Core.Arrays.Types.VarString; var Parent:TSFolder);
var
  iLcv:Integer;
  fldrP:PSFolder;
  saPathP:Core.Arrays.Types.PVarString;
begin
  for iLcv:=1 to High(saPath) do begin
    fldrP:=getFolder(saPath[iLcv],Parent.Folders);
    if (fldrP=nil) then begin
      saPathP:=Sub(saPath,0,iLcv)

    end;

  end;
end;

procedure Organize(Master:TMasterList; var Root:TSFolder);
var
  iLcv:Integer;
  fldrP:PSFolder;
  fldrRP:PSFolder;
  saPath:Core.Arrays.Types.VarString;

begin
  for iLcv:=0 to Master.Count-1 do begin
    fldrP:=Master[iLcv];
    Core.Arrays.VarString.fromString(saPath,fldrP^.Path,'/');
    Try
      if (Length(saPath)>1) then begin
        // has children
        fldrRP:=getFolderByPath(saPath[0], Master);
        if (fldrRP<>nil) then begin
          Organize(Master,saPath,fldrRP^);
        end;
      end else begin
        // this is where the folder belongs in the master list
      end;
    finally
      Core.Arrays.VarString.Empty(saPath);
    end;
  end;
end;

function  Add(FolderP:PSFolder; Var Folders:TSFolders):PSFolder;
var
  iLcv,iCt:LongInt;
begin
  Result:=nil;
  iCt:=System.Length(Folders);
  for iLcv:=0 to iCt-1 do begin
    if (Folders[iLcv]=FolderP) then begin
      Result:=FolderP;
      break;
    end;
  end;
  if (Result=nil) then begin
    SetLength(Folders,iCt+1);
    Folders[iCt]:=FolderP;
    Result:=FolderP;
  end;
end;

function  Create(Var Folders:TSFolders; var ParentP:PSFolder):PSFolder;
var
  iCt:LongInt;
  itmP:PSFolder;
begin
  new(itmP);
  Init(itmP^,ParentP);

  iCt:=System.Length(Folders);
  SetLength(Folders,iCt+1);
  Folders[iCt]:=itmP;

  Result:=itmP;
end;

function Create(NetworkID:QWord; var Items:TSFolders):PSFolder;
var
  iCt:LongInt;
  fldr:PSFolder;
begin
  iCt:=System.Length(Items);
  new(fldr);
  Init(fldr^,nil);
  fldr^.NetworkID:=NetworkID;
  SetLength(Items,iCt+1);
  Items[iCt]:=fldr;
  Result:=fldr
end;

function  Add(Name:Core.Strings.VarString; var Digest:TMD5Digest; var Folder:TSFolder):PSFile;
var
  iCt:LongInt;
  itmP:PSFile;
begin
  new(itmP);
  Init(itmP^);
  itmP^.FolderID:=Folder.ID;

  itmP^.Name:=Name;
  Core.Arrays.Bytes.Copy(Digest,itmP^.Digest);


  iCt:=System.Length(Folder.Files);
  SetLength(Folder.Files,iCt+1);
  Folder.Files[iCt]:=itmP;

  Result:=itmP;
end;

procedure cb_Path(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PString(DataP)^:=Fields.FieldByName(DB.Keys.Path).AsString;
end;


function  getPath(Task:Core.Database.Types.TTask; var DomainID,NetworkID,ItemID:QWord):Core.Strings.VarString;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sPath                          : Core.Strings.VarString;
begin
  SetLength(sPath,0);
  iCount:=0;
  Core.Database.AddCommand(iCount,DB.TableP,@Commands);
  Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
  Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
  Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,ItemID,Commands);
  Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Path,poNone,oNone,Commands);

  Core.Database.SQL.Select(Task,@Commands,@cb_Path,@sPath);

  Result:=sPath;
end;

function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:TSFolders): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Items,FREE_FILES); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,DB.IDs.Path,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.NetworkID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.OwnerID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_List_Folders,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Path:Core.Strings.VarString; var Items:TSFolders): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sSubFolders                    : Core.Strings.VarString;
begin
  Result:=False;
  sSubFolders:=Concat(Path,'/%');
  Empty(Items,FREE_FILES); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Path,poNone,oLike,sSubFolders,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Path,poOr,oEqual,Path,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,DB.IDs.Path,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.NetworkID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.OwnerID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_List_Folders,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Path:Core.Strings.VarString; var Items:Core.Arrays.Types.LargeWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sSubFolders                    : Core.Strings.VarString;
begin
  Result:=False;
  sSubFolders:=Concat(Path,'/%');
  Core.Arrays.LargeWord.Empty(Items); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Path,poNone,oLike,sSubFolders,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Path,poOr,oEqual,Path,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,DB.IDs.Path,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_List_Folder_IDs,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Core.Arrays.LargeWord.Empty(Items); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_List_Folder_IDs,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  Rename(Task:Core.Database.Types.TTask; var DomainID,NetworkID:QWord; var Items:TSFolders): Boolean;
var
  iLcv                           : LongInt;
begin
  Result:=True;
  For iLcv:=0 to High(Items) do
    Rename(Task,DomainID,NetworkID,Items[iLcv]^);
  Result:=True;
end;

function  Rename(Task:Core.Database.Types.TTask; var DomainID,NetworkID:QWord; var Item:TSFolder): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Path,poNone,oNone,Item.Path,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  Create(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var ItemID:QWord; Path:Core.Strings.VarString): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.NetworkID,poNone,oNone,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.OwnerID,poNone,oNone,OwnerID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Path,poNone,oNone,Path,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,NetworkID,ItemID:QWord): Boolean;
var
  Commands                       : Core.Database.Types.Commands;
  IDs                            : Core.Arrays.Types.LargeWord;
  iLcv                           : LongInt;
  Folders                        : Core.Arrays.Types.LargeWord;
  sPath                          : Core.Strings.VarString;

  procedure PushDelete(iID:QWord);
  var
    iCount                       : LongInt;
  begin
    Clear(Task,Node,DomainID,NetworkID,iID);

    iCount:=0;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,iID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);

    Storage.AuraDisks.Files.Delete(Node,DomainID,NetworkID,iID,Storage.AuraDisks.Kinds.Social);
  end;

begin
  Result:=False; Core.Arrays.LargeWord.Empty(IDs);
  Try
    sPath:=getPath(Task,DomainID,NetworkID,ItemID);
    if (ItemID<>0) then begin
      PushDelete(ItemID);
      if Length(sPath)>0 then begin
        Try
          Result:=List(Task,DomainID,NetworkID,sPath,Folders);
          try
            for iLcv:=0 to High(Folders) do
              PushDelete(Folders[iLcv]);
          finally
            Core.Arrays.LargeWord.Empty(Folders);
          end;
        Finally
          Core.Database.Done(Commands);
        End;
      end;
    end;

  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cb_Read_Folder(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemP:PSFolder;
begin
  ItemP:=DataP;
  ItemP^.ID:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
  ItemP^.NetworkID:=Fields.FieldByName(DB.Keys.NetworkID).AsLargeInt;
  ItemP^.OwnerID:=Fields.FieldByName(DB.Keys.OwnerID).AsLargeInt;
  ItemP^.Path:=Fields.FieldByName(DB.Keys.Path).AsString;
end;

function  Fill(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Path:Core.Strings.VarString; var Folder:TSFolder):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Folder.ID:=0; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Path,poAnd,oEqual,Path,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.NetworkID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.OwnerID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Read_Folder,@Folder) and (Folder.ID<>0);
  finally
    Core.Database.Done(Commands);
  end;
end;
function  Fill(Task:Core.Database.Types.TTask; DomainID,FolderID:QWord; var Folder:TSFolder):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Folder,FREE_FILES); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,FolderID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.NetworkID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.OwnerID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Read_Folder,@Folder);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  CreateDefaults(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var Trash,Documents,Music,Pictures,Videos:QWord): Boolean;
var
  sFolder   : Core.Strings.VarString;
begin
  sFolder:=Defaults.Documents;
  Result:=Create(Task,DomainID,NetworkID,OwnerID,Documents,sFolder);
  sFolder:=Defaults.Music;
  Result:=Create(Task,DomainID,NetworkID,OwnerID,Music,sFolder);
  sFolder:=Defaults.Pictures;
  Result:=Create(Task,DomainID,NetworkID,OwnerID,Pictures,sFolder);
  sFolder:=Defaults.Videos;
  Result:=Create(Task,DomainID,NetworkID,OwnerID,Videos,sFolder);
  sFolder:=Defaults.Trash;
  Result:=Create(Task,DomainID,NetworkID,OwnerID,Trash,sFolder);
end;

function  Verify(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID,ItemID:QWord; Path:Core.Strings.VarString): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Folder                         : TSFolder;
begin
  Result:=False;
  Init(Folder,nil); iCount:=0;
  Try
    Try
      Core.Database.AddCommand(iCount,DB.TableP,@Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.Path,poAnd,oEqual,Path,Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForOrderBy,DB.IDs.Path,poNone,oNone,Commands);

      Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Path,poNone,oNone,Commands);

      Result:=Core.Database.SQL.Select(Task,@Commands,@cb_List_Folders,@Folder);
      ItemID:=Folder.ID;
      if (ItemID=0) then
        Result:=Create(Task,DomainID,NetworkID,OwnerID,ItemID,Path);
    finally
      Core.Database.Done(Commands);
    end;
  Finally
    Done(Folder,FREE_FILES);
  end;
end;

function TMasterList.Find(ID:QWord):PSFolder;
var
  iLcv:integer;
  itmP:PSFolder;
begin
  Result:=nil;
  for iLcv:=0 to Count-1 do begin
    itmP:=Items[iLcv];
    if (itmP^.Id=Id) then begin
      Result:=itmP;
      break;
    end;
  end;
end;

function TMasterList.FindByPath(Path:Core.Strings.VarString):PSFolder;
var
  iLcv:integer;
  itmP:PSFolder;
begin
  Result:=nil;
  for iLcv:=0 to Count-1 do begin
    itmP:=Items[iLcv];
    if Core.Strings.SameText(Path,itmP^.Path) then begin
      Result:=itmP;
      break;
    end;
  end;
end;

function TMasterList.fromXML(xDoc:TXMLDocument):boolean;
var
  xItems:TDOMNode;
  xItem:TDOMNode;
  iLcv:LongInt;
  itmP:PSFolder;
  itmParentP:PSFolder;
  sParent:Core.Strings.VarString;
  iID:QWord;
begin
  Result:=False;
  xItems:=Core.XML.DB.getNode(xDoc,DB.XML.Stanzas.Items);
  Invalidate();
  if (xItems<>nil) then begin
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      xItem:=xItems.ChildNodes[iLcv];
      with Core.XML.DB do begin
        iID:=toQWord(xItem,DB.XML.ID);
        itmP:=Find(iID);
        if (itmP=nil) then begin
          new(itmP);
          Init(itmP^,nil);
          itmP^.ID:=iID;
          Add(itmP^);
        end;
        itmP^.NetworkID:=toQWord(xItem,DB.XML.NetworkID);
        itmP^.OwnerID:=toQWord(xItem,DB.XML.OwnerID);
        itmP^.Path:=toString(xItem,DB.XML.Path);
        itmP^.Name:=Core.Utils.Files.Extract(itmP^.Path,epoName,'/');
        itmP^.Verified:=true;
        Result:=True;
      end;
    end;
  end;
  for iLcv:=0 to Count-1 do begin
    itmP:=Items[iLcv];
    if (itmP^.Name<>itmP^.Path) then begin
      sParent:=Core.Utils.Files.Extract(itmP^.Path, epoAllButName,'/');
      itmParentP:=FindByPath(sParent);
      if (itmP^.Parent<>itmParentP) and (itmP^.Parent<>nil) then begin
        Storage.Social.Folders.Remove(itmP,itmP^.Parent^.Folders);
      end;
      itmP^.Parent:=itmParentP;
      Storage.Social.Folders.Add(itmP,itmParentP^.Folders);
    end;
  end;


  Purge();
end;

procedure TMasterList.Invalidate();
var
  iLcv:integer;
  fldrP:PSFolder;
begin
  for iLcv:=0 to Count-1 do begin
    fldrP:=Items[iLcv];
    fldrP^.Verified:=false;
    Storage.Social.Files.Invalidate(fldrP^.Files);
  end;
end;

procedure TMasterList.Purge();
var
  iLcv:LongInt;
  itmP:PSFolder;
  jLcv:LongInt;
begin
  iLcv:=0;
  While iLcv<Count do begin
    itmP:=Items[iLcv];
    if (itmP^.Verified=false) and (itmP^.ID<>0) then begin
      Remove(itmP);
      Done(itmP^,FREE_FILES);
      Dispose(itmP);
    end else
      Inc(iLcv);
  end;
end;

procedure TMasterList.Init(var Item:TSFolder; ParentP:PSFolder);
begin
  Storage.Social.Folders.Init(Item,ParentP);
end;

procedure TMasterList.Empty(var Item:TSFolder; const FreeFiles:boolean);
begin
  Storage.Social.Folders.Empty(Item,FreeFiles);
end;

procedure TMasterList.Done(var Item:TSFolder; const FreeFiles:boolean);
begin
  Storage.Social.Folders.Done(Item,FreeFiles);
end;

initialization
  RegisterDB;

end.




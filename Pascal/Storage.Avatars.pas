unit Storage.Avatars;

{
  unit Storage.Avatars.pas

  Avatar Storage Retrieval Database Module

  DBMS facilities to handle Avatars

  Copyright Aurawin LLC 2003-2012
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
   RSR.HTTP,
   Core.XML,
   Core.Arrays.Types,
   Core.Arrays.VarString,
   Core.Arrays.LargeWord,
   Core.Arrays.Bytes,
   Core.Streams,
   Core.Strings,
   Core.Timer,

   Core.Database,
   Core.Database.Types,
   Core.Database.SQL,
   Core.Database.Monitor,
   Core.Database.Monitor.Notify,
   Core.Database.Monitor.Types,

   Storage.Main,
   Storage.ContentTypes,

   Encryption.Base64,
   Multimedia.Image,

   DOM,
   XMLRead,
   Classes,
   SysUtils;

type
  Items = class
  type
    Defaults=class
    const
      MaxSize                  : LongInt = 256;
    end;
    Kinds=class
    Const
      User                     : Qword = 0;
      Network                  : Qword = 1;
    end;
    Lands=class
    Const
      User                     = 0;
      Network                  = 1;
    end;
    TItem=record
      ID                       : Qword;
      OwnerID                  : Qword;
      Kind                     : Qword;
      Created                  : Double;
      Modified                 : Double;
      Width                    : LongInt;
      Height                   : LongInt;
      Size                     : LongInt;
      Extension                : Core.Strings.VarString;
      Digest                   : Core.Strings.VarString;
      Data                     : Core.Arrays.Types.Bytes;
    end;
    PItem=^TItem;
    TItems=Array of PItem;
    PItems=^TItems;
    XML=class
    type
      Stanzas=class
      const
        Item                     : Core.Database.Types.VarString = 'avatar';
        Items                    : Core.Database.Types.VarString = 'avatars';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        OwnerID                  : Core.Database.Types.VarString = 'oid';
        Kind                     : Core.Database.Types.VarString = 'kind';
        Created                  : Core.Database.Types.VarString = 'ctd';
        Modified                 : Core.Database.Types.VarString = 'mdf';
        Width                    : Core.Database.Types.VarString = 'width';
        Height                   : Core.Database.Types.VarString = 'height';
        Size                     : Core.Database.Types.VarString = 'size';
        Extension                : Core.Database.Types.VarString = 'ext';
        Data                     : Core.Database.Types.VarString = 'data';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        OwnerID                  : Core.Database.Types.Integer = 3;
        Kind                     : Core.Database.Types.Integer = 4;
        Created                  : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        Width                    : Core.Database.Types.Integer = 7;
        Height                   : Core.Database.Types.Integer = 8;
        Size                     : Core.Database.Types.Integer = 9;
        Extension                : Core.Database.Types.Integer = 10;
        Data                     : Core.Database.Types.Integer = 11;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        OwnerID                  : Core.Database.Types.VarString = 'IOID';

        Kind                     : Core.Database.Types.VarString = 'IKND';
        Created                  : Core.Database.Types.VarString = 'ICTD';
        Modified                 : Core.Database.Types.VarString = 'IMTD';

        Width                    : Core.Database.Types.VarString = 'IWID';
        Height                   : Core.Database.Types.VarString = 'IHGT';

        Size                     : Core.Database.Types.VarString = 'ISIZ';
        Extension                : Core.Database.Types.VarString = 'IEXT';
        Data                     : Core.Database.Types.VarString = 'IDAT';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit                   : True;
        Group        : 'System/Applications/Social';
        Name         : 'Avatars';
        Value        : 'scs_soc_avtrs';
        Hint         : 'Avatar image storage';
        PrimaryKeyP  : @Keys.ID;
      );
      Fields: array [0..11] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.OwnerID;  KeyP: @Keys.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.Kind;  KeyP: @Keys.Kind; DataType: dftQword; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),

        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified;  KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),

        (IDP: @IDs.Width; KeyP: @Keys.Width; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Height; KeyP: @Keys.Height; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),

        (IDP: @IDs.Size;  KeyP: @Keys.Size; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Extension; KeyP: @Keys.Extension; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function Add(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; Kind:LongInt; Extension:Core.Strings.VarString; Data:TMemoryStream; var Entry:TItem):boolean;
      class function Update(Task:Core.Database.Types.TTask; DomainID:QWord; Extension:Core.Strings.VarString; Data:TMemoryStream; var Entry:TItem):boolean;
      class function Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Entry:TItem):boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
    end;


    class function  toXML(var Entry:TItem; Output:TMemoryStream; Header:boolean):boolean;
    class function  fromXML(xDoc:TXMLDocument; var Entry:TItem):boolean; overload;
    class function  asDataString(var Entry:TItem; out ContentType:Core.Strings.VarString):Core.Strings.VarString;

    class procedure Empty(Var Entry:TItem); overload;
    class procedure Empty(Var Entries:TItems); overload;

    class procedure Init(Var Entry:TItem); overload;
    class procedure Init(Var Entries:TItems); overload;

    class procedure Done(Var Entry:TItem); overload;
    class procedure Done(Var Entries:TItems); overload;
  end;

implementation
uses
   DB,
   sqldb;

procedure cbDestroyAvatar(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Items.DB do begin
    Core.Database.Done(TableP^);
    Dispose(TableP);
    TableP := nil;
    Core.Database.Monitor.Done(MonitorP^);
    Dispose(MonitorP);
    MonitorP := nil;
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Items.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;
  procedure PushUserDeleted;
  begin
    if ItemP = Items.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.OwnerID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;
begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyAvatar, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

class function  Items.fromXML(xDoc:TXMLDocument; var Entry:TItem):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Empty(Entry);
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Entry.ID:=toQword(xItem,XML.Fields.ID);
      Entry.OwnerID:=toQword(xItem,XML.Fields.OwnerID);
      Entry.Kind:=toQword(xItem,XML.Fields.Kind);
      Entry.Created:=toDouble(xItem,XML.Fields.Created);
      Entry.Modified:=toDouble(xItem,XML.Fields.Modified);
      Entry.Width:=toInteger(xItem,XML.Fields.Width);
      Entry.Height:=toInteger(xItem,XML.Fields.Height);
      Entry.Size:=toInteger(xItem,XML.Fields.Size);
      Entry.Extension:=toString(xItem,XML.Fields.Extension);
      Result:=True;
    end;
  end;
end;

class function  Items.asDataString(var Entry:TItem; out ContentType:Core.Strings.VarString):Core.Strings.VarString;
begin
  ContentType:=ContentTypeFromFile(Storage.ContentTypes.List,Entry.Extension);
  Result:=Concat(
    'data:',
    ContentType,';base64,',
    Encryption.Base64.Encode(Entry.Data)
  );
end;

class function  Items.toXML(var Entry:TItem; Output:TMemoryStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.OwnerID,Entry.OwnerID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Entry.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Entry.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Kind,Entry.Kind),Output);
    Core.Streams.Write(Print(XML.Fields.Width,Entry.Width),Output);
    Core.Streams.Write(Print(XML.Fields.Height,Entry.Height),Output);
    Core.Streams.Write(Print(XML.Fields.Size,Entry.Size),Output);
    Core.Streams.Write(Print(XML.Fields.Extension,Entry.Extension,CDATA_OFF),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class procedure Items.Empty(Var Entry:TItem);
begin
  Entry.ID:=0;
  Entry.OwnerID:=0;
  Entry.Kind:=0;
  Entry.Created:=0.0;
  Entry.Modified:=0.0;
  Entry.Width:=0;
  Entry.Height:=0;
  Entry.Size:=0;
  SetLength(Entry.Extension,0);
  SetLength(Entry.Data,0);
end;

class procedure Items.Empty(Var Entries:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Init(Var Entry:TItem);
begin
  Entry.ID:=0;
  Entry.OwnerID:=0;
  Entry.Kind:=0;
  Entry.Created:=0.0;
  Entry.Modified:=0.0;
  Entry.Width:=0;
  Entry.Height:=0;
  Entry.Size:=0;
  SetLength(Entry.Extension,0);
  SetLength(Entry.Data,0);
end;

class procedure Items.Init(Var Entries:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Done(Var Entry:TItem);
begin
  Finalize(Entry.Extension);
  Finalize(Entry.Data);
  Finalize(Entry);
end;

class procedure Items.Done(Var Entries:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;

class function Items.DB.Add(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; Kind:LongInt; Extension:Core.Strings.VarString; Data:TMemoryStream; var Entry:TItem):boolean;
var
  Commands:Core.Database.Types.Commands;
  iInsertID:QWord;
  iCount:LongInt;
  iReset:QWord;
  sKind:Core.Strings.VarString;
begin
  iInsertID:=Random(High(Integer)); iReset:=0; iCount:=0;
  Data.Position:=0;
  Empty(Entry);

  Entry.Height:=Defaults.MaxSize;
  Entry.Width:=Defaults.MaxSize;

  sKind:=Multimedia.Image.Tool.Kind.fromString(Extension);
  Multimedia.Image.Tool.Transform(Data,Entry.Width,Entry.Height);
  Entry.Size:=Data.Size;

  Entry.OwnerID:=OwnerID;
  Entry.Kind:=Kind;
  Entry.Created:=Core.Timer.dtUT;
  Entry.Modified:=Core.Timer.dtUT;
  Entry.Extension:=Extension;

  Core.Arrays.Bytes.fromStream(@Entry.Data,Data,0);
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);


    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.OwnerID,poNone,oNone,Entry.OwnerID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Kind,poNone,oNone,Entry.Kind,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Created,poNone,oNone,Entry.Created,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Width,poNone,oNone,Entry.Width,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Height,poNone,oNone,Entry.Height,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Size,poNone,oNone,Entry.Size,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Extension,poNone,oNone,Entry.Extension,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Data,poNone,oNone,Entry.Data,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Update(Task:Core.Database.Types.TTask; DomainID:QWord; Extension:Core.Strings.VarString; Data:TMemoryStream; var Entry:TItem):boolean;
var
  Commands:Core.Database.Types.Commands;
  iCount:LongInt;
  Kind:Core.Strings.VarString;
begin
  Data.Position:=0;
  iCount:=0;

  Entry.Height:=Defaults.MaxSize;
  Entry.Width:=Defaults.MaxSize;
  Entry.Extension:=Extension;
  Kind:=Multimedia.Image.Tool.Kind.fromString(Extension);

  Multimedia.Image.Tool.Transform(Data,Entry.Width,Entry.Height);

  Entry.Modified:=Core.Timer.dtUT;
  Entry.Size:=Data.Size;

  Core.Arrays.Bytes.fromStream(@Entry.Data,Data,0);
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount,DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Entry.ID, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Width,poNone,oNone,Entry.Width,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Height,poNone,oNone,Entry.Height,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Size,poNone,oNone,Entry.Size,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Extension,poNone,oNone,Entry.Extension,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Data,poNone,oNone,Entry.Data,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbReadAvatar(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Items.PItem;
begin
  ItmP:=DataP;
  With Items.DB.Keys do begin
    ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
    ItmP^.OwnerID:=Fields.FieldByName(OwnerID).AsLargeInt;
    ItmP^.Kind:=Fields.FieldByName(Kind).AsInteger;
    ItmP^.Created:=Fields.FieldByName(Created).AsFloat;
    ItmP^.Modified:=Fields.FieldByName(Modified).AsFloat;
    ItmP^.Width:=Fields.FieldByName(Width).AsInteger;
    ItmP^.Height:=Fields.FieldByName(Height).AsInteger;
    ItmP^.Size:=Fields.FieldByName(Size).AsInteger;
    ItmP^.Extension:=Fields.FieldByName(Extension).AsString;
    Core.Arrays.Bytes.fromString(Fields.FieldByName(Data).AsString,@ItmP^.Data);
  end;
end;

class function Items.DB.Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Entry:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Entry);

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.OwnerID,poNone,oNone,Entry.OwnerID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Entry.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Created,poNone,oNone,Entry.Created,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Width,poNone,oNone,Entry.Width,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Height,poNone,oNone,Entry.Height,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Size,poNone,oNone,Entry.Size,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Extension,poNone,oNone,Entry.Extension,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Data,poNone,oNone,Entry.Data,Commands);

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadAvatar, @Entry) and  (Entry.ID<>0) );

    Entry.Digest:=Core.Arrays.Bytes.CheckSum(Entry.Data);

  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDB;
end.


unit Storage.Signatures;


{
  unit Storage.Signatures.pas

  Signatures Database Module

  DBMS facilities to handle Signatures for Spectrum

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

interface

uses
  Classes,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  RSR.Core,

  Core.Strings,
  Core.Streams,
  Core.Timer,
  Core.XML,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,


  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.CoreObjects,


  DOM,
  SysUtils;


type
  Items = class
  type
    Item = record
      ID                       : QWord;
      UserID                   : QWord;
      Created                  : double;
      Modified                 : double;
      Title                    : Core.Strings.VarString;
      Data                     : Core.Strings.VarString;
    end;
    PItem = ^Item;
    List=Array of PItem;
    PList = ^List;
    XML=class
    type
      Stanzas=class
      const
        Signatures               = 'sigs';
        Signature                = 'sig';
      end;
      Fields=class
      const
        ID                       = 'id';
        Created                  = 'created';
        Modified                 = 'modified';
        Title                    = 'title';
        Data                     = 'data';
      end;
    end;
    DB = class
      type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        Created                  : Core.Database.Types.Integer = 4;
        Modified                 : Core.Database.Types.Integer = 5;
        Title                    : Core.Database.Types.Integer = 6;
        Data                     : Core.Database.Types.Integer = 7;
      end;
      Keys = class
      const
        ID                       : Core.Database.Types.VarString = 'TID';
        InsertID                 : Core.Database.Types.VarString = 'TIID';
        DomainID                 : Core.Database.Types.VarString = 'TDID';
        UserID                   : Core.Database.Types.VarString = 'TUID';
        Created                  : Core.Database.Types.VarString = 'TCTD';
        Modified                 : Core.Database.Types.VarString = 'MDFD';
        Title                    : Core.Database.Types.VarString = 'ITIT';
        Data                     : Core.Database.Types.VarString = 'ISIG';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Signatures';
        Value                    : 'scs_sigs';
        Hint                     : 'Signature storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields                     : array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID;  DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
      class function Add(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
      class function Read(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
      class function Write(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID, UserID, ItemID:QWord):boolean;
      class function List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entries:List):boolean;
      class function Refresh(Task: Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item): boolean;
    end;
    class procedure Init(var Entry:Item);
    class procedure Empty(var Entry:Item); overload;
    class procedure Empty(var Entries:List); overload;
    class procedure Done(var Entry:Item); overload;
    class procedure Done(var Entries:List); overload;

    class function IndexOf(var Entries:List; ID:QWord): LongInt;
    class function fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
    class function toXML(var Entries:List; Output:TMemoryStream):boolean; overload;
    class function toXML(var Entry:Item; Output:TMemoryStream):boolean; overload;
end;

implementation

uses DB;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Items.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Items.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;


begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted();
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Init(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    UserID:=0;
    Created:=0;
    Modified:=0;
    SetLength(Title,0);
    SetLength(Data,0);
  end;
end;

class procedure Items.Empty(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    UserID:=0;
    Created:=0;
    Modified:=0;
    SetLength(Title,0);
    SetLength(Data,0);
  end;
end;

class procedure Items.Empty(var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Done(var Entry:Item);
begin
  With Entry do begin
    Finalize(Title);
    Finalize(Data);
  end;
  Finalize(Entry);
end;

class procedure Items.Done(var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;

class function Items.IndexOf(var Entries:List; ID:QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class function Items.DB.Add(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iReset: QWord;
  iInsertID: QWord;
begin
  Result := False;
  iCount := 0;
  iReset := 0;
  iInsertID := Random(High(LongInt));
  Entry.Created:=Core.Timer.dtUT;
  Entry.Modified:=Entry.Created;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, IDs.ID, poNone, oNone, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, IDs.InsertID, poNone, oNone, iReset, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.UserID, poNone, oNone, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Created, poNone, oNone, Entry.Created, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Title, poNone, oNone, Entry.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Data, poNone, oNone, Entry.Data, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadSignature(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Items.PItem(DataP)^ do begin
    ID          := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    Created     := Fields.FieldByName(Items.DB.Keys.Created).AsFloat;
    Modified    := Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
    Title       := Fields.FieldByName(Items.DB.Keys.Title).AsString;
    Data        := Fields.FieldByName(Items.DB.Keys.Data).AsString;
  end;
end;

class function Items.DB.Read(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    {$i Storage.Signatures.Fields.inc}
    Result:=Core.Database.SQL.Select(Task, @Commands, @cbReadSignature, @Entry);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Refresh(Task: Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Modified, poAnd, oNotEqual, Entry.Modified, Commands);

    {$i Storage.Signatures.Fields.inc}

    Result:=Core.Database.SQL.Select(Task, @Commands, @cbReadSignature, @Entry) ;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbSignatureList(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Items.PList;
  iID:QWord;
  iDX:LongInt;
begin
  ListP:=DataP;
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iDX:=Items.IndexOf(ListP^,iID);
  if iDX=-1 then begin
    iDX:=Length(ListP^);
    SetLength(ListP^,iDX+1);
    New(ListP^[iDX]);
    Items.Init(ListP^[iDX]^);
  end;
  cbReadSignature(CommandsP,Fields,ListP^[iDX]);
end;

class function  Items.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Entries:List):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Entries);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    {$i Storage.Signatures.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbSignatureList, @Entries);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID, UserID, ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.Write(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Title, poNone, oNone, Entry.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Data, poNone, oNone, Entry.Data, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  with Core.XML.DB do begin
    xItem:=getNode(xDoc,XML.Stanzas.Signature);
    if xItem<>nil then begin
      Entry.ID:=toQWord(xItem,XML.Fields.ID);
      Entry.Created:=toDouble(xItem,XML.Fields.Created);
      Entry.Modified:=toDouble(xItem,XML.Fields.Modified);
      Entry.Title:=toString(xItem,XML.Fields.Title);
      Entry.Data:=toString(xItem,XML.Fields.Data);
      Result:=True;
    end;
  end;
end;

class Function  Items.toXML(var Entries:List; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  Output.Position:=Output.Size;

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Signatures,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to High(Entries) do
    toXML(Entries[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Signatures,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class Function  Items.toXML(var Entry:Item; Output:TMemoryStream):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Signature,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Entry.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Entry.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Entry.Title,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Data,Entry.Data,CDATA_ON),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Signature,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

initialization
  RegisterDB;
end.


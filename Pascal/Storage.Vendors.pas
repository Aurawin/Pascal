unit Storage.Vendors;
{
  unit Storage.Vendors.pas

  Vendor Database Module

  DBMS facilities to handle scalable client-server applications over Core Objects

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

interface

uses
  RSR.Core,

  Core.Strings,
  Core.Timer,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.CoreObjects,

  Classes,
  SysUtils;


type
  Items=class
  Type
    Item = record
      ID                         : Core.Database.Types.LargeWord;
      Founded                    : Core.Database.Types.Double;
      Joined                     : Core.Database.Types.Double;
      Modified                   : Core.Database.Types.Double;
      Size                       : Core.Database.Types.Integer;
      State                      : Core.Database.Types.Integer;
      Auth                       : Core.Database.Types.SmallString;
      Name                       : Core.Database.Types.SmallString;
      Email                      : Core.Database.Types.SmallString;
      Website                    : Core.Database.Types.SmallString;
      Phone                      : Core.Database.Types.SmallString;
      Street1                    : Core.Database.Types.SmallString;
      Street2                    : Core.Database.Types.SmallString;
      City                       : Core.Database.Types.SmallString;
      Province                   : Core.Database.Types.SmallString;
      Postal                     : Core.Database.Types.SmallString;
      Country                    : Core.Database.Types.SmallString;
      Status                     : Core.Database.Types.SmallString;
    end;
    PItem = ^Item;
    State = class
    const
      Unknown   = -1;
      Pending   = 0;
      Approved  = 1;
      Blocked   = 2;
      Revoked   = 3;
    end;
    DB = class
    type
      IDs = class
      const
        ID                         : Core.Database.Types.Integer = 0;
        InsertID                   : Core.Database.Types.Integer = 1;
        DomainID                   : Core.Database.Types.Integer = 2;
        UserID                     : Core.Database.Types.Integer = 3;
        Founded                    : Core.Database.Types.Integer = 4;
        Joined                     : Core.Database.Types.Integer = 5;
        Modified                   : Core.Database.Types.Integer = 6;
        Size                       : Core.Database.Types.Integer = 7;
        State                      : Core.Database.Types.Integer = 8;
        Auth                       : Core.Database.Types.Integer = 9;
        Name                       : Core.Database.Types.Integer = 10;
        Email                      : Core.Database.Types.Integer = 11;
        Website                    : Core.Database.Types.Integer = 12;
        Phone                      : Core.Database.Types.Integer = 13;
        Street1                    : Core.Database.Types.Integer = 14;
        Street2                    : Core.Database.Types.Integer = 15;
        City                       : Core.Database.Types.Integer = 16;
        Province                   : Core.Database.Types.Integer = 17;
        Postal                     : Core.Database.Types.Integer = 18;
        Country                    : Core.Database.Types.Integer = 19;
        Status                     : Core.Database.Types.Integer = 20;
      end;

      Keys = class
      const
        ID                         : Core.Database.Types.VarString = 'VID';
        InsertID                   : Core.Database.Types.VarString = 'VIID';
        DomainID                   : Core.Database.Types.VarString = 'VDID';
        UserID                     : Core.Database.Types.VarString = 'VUID';
        Founded                    : Core.Database.Types.VarString = 'VFND';
        Joined                     : Core.Database.Types.VarString = 'JYND';
        Modified                   : Core.Database.Types.VarString = 'MDFD';
        Size                       : Core.Database.Types.VarString = 'VSZE';
        State                      : Core.Database.Types.VarString = 'VSTE';
        Name                       : Core.Database.Types.VarString = 'VNME';
        Email                      : Core.Database.Types.VarString = 'VEML';
        Website                    : Core.Database.Types.VarString = 'VWS';
        Phone                      : Core.Database.Types.VarString = 'VPHN';
        Street1                    : Core.Database.Types.VarString = 'STR1';
        Street2                    : Core.Database.Types.VarString = 'STR2';
        City                       : Core.Database.Types.VarString = 'CTY';
        Province                   : Core.Database.Types.VarString = 'PRVC';
        Postal                     : Core.Database.Types.VarString = 'PSTL';
        Country                    : Core.Database.Types.VarString = 'CTRY';
        Status                     : Core.Database.Types.VarString = 'VST';
        Auth                       : Core.Database.Types.VarString = 'AUTH';
      end;

      Defaults = class
      const
        State = State.Pending;
      end;

    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Vendors';
        Value                    : 'scs_vndrs';
        Hint                     : 'Vendor information storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..20] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;        ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Founded; KeyP: @Keys.Founded; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Joined; KeyP: @Keys.Joined; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Size;  KeyP: @Keys.Size; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.State;  KeyP: @Keys.State; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Auth; KeyP: @Keys.Auth; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Email;  KeyP: @Keys.Email; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.Website; KeyP: @Keys.Website; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Phone;  KeyP: @Keys.Phone; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Street1; KeyP: @Keys.Street1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Street2; KeyP: @Keys.Street2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.City;  KeyP: @Keys.City; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.Province;  KeyP: @Keys.Province; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Postal; KeyP: @Keys.Postal; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Country; KeyP: @Keys.Country; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Status;  KeyP: @Keys.Status; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
        );
      class function Add(Task:Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry: Item): boolean;
      class function Read(Task:Core.Database.Types.TTask; VendorID: QWord; var Entry: Item): boolean; overload;
      class function Read(Task:Core.Database.Types.TTask; Auth: Core.Strings.VarString; var Entry: Item): boolean; overload;
      class function Write(Task:Core.Database.Types.TTask; var Entry: Item): boolean;
      class function getState(Task:Core.Database.Types.TTask; VendorID: QWord; var Value:LongInt): boolean;
      class function setState(Task:Core.Database.Types.TTask; VendorID: QWord; var Value:LongInt): boolean;
      class function getStatus(Task:Core.Database.Types.TTask; VendorID: QWord; var Value: Core.Strings.VarString): boolean; overload;
      class function getStatus(Task:Core.Database.Types.TTask; DomainID: QWord; var sAuth: Core.Strings.VarString; var Value: Core.Strings.VarString): boolean;  overload;
      class function setStatus(Task:Core.Database.Types.TTask; VendorID: QWord; var Value: Core.Strings.VarString): boolean;
      class function Find(Task:Core.Database.Types.TTask; DomainID: QWord; var sAuth: Core.Strings.VarString; Out ID: QWord): boolean;
    end;

    class procedure Init(var Entry: Item);
    class procedure Empty(var Entry: Item);
    class procedure Done(var Entry: Item);
end;


implementation

uses
  DB,
  sqldb;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Items.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria,Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED: PushDomainDeleted;
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
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Init(var Entry: Item);
begin
  Entry.ID := 0;
  Entry.Founded := 0;
  Entry.Joined := 0;
  Entry.Modified := 0;
  Entry.Size := 0;
  Entry.State := DB.Defaults.State;
  SetLength(Entry.Auth, 0);
  SetLength(Entry.Name, 0);
  SetLength(Entry.Email, 0);
  SetLength(Entry.Website, 0);
  SetLength(Entry.Phone, 0);
  SetLength(Entry.Street1, 0);
  SetLength(Entry.Street2, 0);
  SetLength(Entry.City, 0);
  SetLength(Entry.Province, 0);
  SetLength(Entry.Postal, 0);
  SetLength(Entry.Country, 0);

  SetLength(Entry.Status, 0);
end;

class procedure Items.Empty(var Entry: Item);
begin
  Entry.ID := 0;
  Entry.Founded := 0;
  Entry.Joined := 0;
  Entry.Modified := 0;
  Entry.Size := 0;
  Entry.State := DB.Defaults.State;
  SetLength(Entry.Auth, 0);
  SetLength(Entry.Name, 0);
  SetLength(Entry.Email, 0);
  SetLength(Entry.Website, 0);
  SetLength(Entry.Phone, 0);
  SetLength(Entry.Street1, 0);
  SetLength(Entry.Street2, 0);
  SetLength(Entry.City, 0);
  SetLength(Entry.Province, 0);
  SetLength(Entry.Postal, 0);
  SetLength(Entry.Country, 0);

  SetLength(Entry.Status, 0);
end;

class procedure Items.Done(var Entry: Item);
begin
  Finalize(Entry.Auth);
  Finalize(Entry.Name);
  Finalize(Entry.Email);
  Finalize(Entry.Website);
  Finalize(Entry.Phone);
  Finalize(Entry.Street1);
  Finalize(Entry.Street2);
  Finalize(Entry.City);
  Finalize(Entry.Province);
  Finalize(Entry.Postal);
  Finalize(Entry.Country);

  Finalize(Entry.Status);

  Finalize(Entry);
end;

class function Items.DB.Add(Task:Core.Database.Types.TTask;  DomainID, UserID: QWord; var Entry: Item): boolean;
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
  try
    With Items.DB do begin
      Core.Database.AddCommand(iCount, TableP,@Commands);
      // Setup Primary ID
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.InsertID, poNone, oNone, iInsertID, Commands);
      Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.InsertID, poNone, oEqual, iInsertID, Commands);
      Core.Database.AddCommand(iCount, TableP, useForPrimaryID,IDs.ID, poNone, oNone, Entry.ID, Commands);
      Core.Database.AddCommand(iCount, TableP, useForResetInsertID,IDs.InsertID, poNone, oNone, iReset, Commands);

      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.DomainID, poNone, oNone, DomainID, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.UserID, poNone, oNone, UserID, Commands);

      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Founded, poNone, oNone, Entry.Founded, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Joined, poNone, oNone, Entry.Joined, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Modified, poNone, oNone, Entry.Modified, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Size, poNone, oNone, Entry.Size, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.State, poNone, oNone, Entry.State, Commands);

      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Auth, poNone, oNone, Entry.Auth, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Name, poNone, oNone, Entry.Name, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Email, poNone, oNone, Entry.Email, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Website, poNone, oNone, Entry.Website, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Phone, poNone, oNone, Entry.Phone, Commands);

      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Street1, poNone, oNone, Entry.Street1, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Street2, poNone, oNone, Entry.Street2, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.City, poNone, oNone, Entry.City, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Province, poNone, oNone, Entry.Province, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Postal, poNone, oNone, Entry.Postal, Commands);
      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Country, poNone, oNone, Entry.Country, Commands);

      Core.Database.AddCommand(iCount, TableP, useForInsert,IDs.Status, poNone, oNone, Entry.Status, Commands);

      Result := Core.Database.SQL.Insert(Task, @Commands);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbRead(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Items.PItem(DataP)^ do
  begin
    ID := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    Founded := Fields.FieldByName(Items.DB.Keys.Founded).AsFloat;
    Joined := Fields.FieldByName(Items.DB.Keys.Joined).AsFloat;
    Modified := Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
    Size := Fields.FieldByName(Items.DB.Keys.Size).AsInteger;
    Auth := Fields.FieldByName(Items.DB.Keys.Auth).AsString;
    Name := Fields.FieldByName(Items.DB.Keys.Name).AsString;
    Email := Fields.FieldByName(Items.DB.Keys.Email).AsString;
    Website := Fields.FieldByName(Items.DB.Keys.Website).AsString;
    Phone := Fields.FieldByName(Items.DB.Keys.Phone).AsString;
    Street1 := Fields.FieldByName(Items.DB.Keys.Street1).AsString;
    Street2 := Fields.FieldByName(Items.DB.Keys.Street2).AsString;
    City := Fields.FieldByName(Items.DB.Keys.City).AsString;
    Province := Fields.FieldByName(Items.DB.Keys.Province).AsString;
    Postal := Fields.FieldByName(Items.DB.Keys.Postal).AsString;
    Country := Fields.FieldByName(Items.DB.Keys.Country).AsString;
    Status := Fields.FieldByName(Items.DB.Keys.Status).AsString;
  end;
end;

class function Items.DB.Read(Task:Core.Database.Types.TTask;  VendorID: QWord; var Entry: Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.ID := 0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, Items.DB.IDs.ID, poNone, oEqual, VendorID, Commands);
    {$i Storage.Vendors.Read.Fields.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbRead, @Entry) and (Entry.ID <> 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Read(Task:Core.Database.Types.TTask;  Auth: Core.Strings.VarString; var Entry: Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.ID := 0;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Auth, poNone, oEqual, Auth, Commands);
    {$i Storage.Vendors.Read.Fields.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands,  @cbRead, @Entry) and (Entry.ID <> 0));
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Items.DB.Write(Task:Core.Database.Types.TTask;  var Entry: Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, Entry.ID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Founded, poNone, oNone, Entry.Founded, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Joined, poNone, oNone, Entry.Joined, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Size, poNone, oNone, Entry.Size, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Auth, poNone, oNone, Entry.Auth, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Name, poNone, oNone, Entry.Name, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Email, poNone, oNone, Entry.Email, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Website, poNone, oNone, Entry.Website, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Phone, poNone, oNone, Entry.Phone, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Street1, poNone, oNone, Entry.Street1, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Street2, poNone, oNone, Entry.Street2, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.City, poNone, oNone, Entry.City, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Province, poNone, oNone, Entry.Province, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Postal, poNone, oNone, Entry.Postal, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Country, poNone, oNone, Entry.Country, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Status, poNone, oNone, Entry.Status, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetStatus(CommandsP: Core.Database.Types.PCommands; Fields: TFields;
  const DataP: Pointer);
begin
  Core.Database.Types.PSmallString(DataP)^ := Fields.FieldByName(Items.DB.Keys.Status).AsString;
end;

class function Items.DB.getStatus(Task:Core.Database.Types.TTask; VendorID: QWord; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Value, 0);
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, VendorID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Status, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetStatus, @Value) and (Length(Value) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.getStatus(Task:Core.Database.Types.TTask; DomainID: QWord; var sAuth: Core.Strings.VarString; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Value, 0);
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.Auth, poAnd, oEqual, sAuth, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields,IDs.Status, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetStatus, @Value) and (Length(Value) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Items.DB.setStatus(Task:Core.Database.Types.TTask; VendorID: QWord; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtModified: double;
begin
  Result := False;
  try
    iCount := 0;
    dtModified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, VendorID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, dtModified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Status, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetState(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Database.Types.PInteger(DataP)^ := Fields.FieldByName(Items.DB.Keys.State).AsInteger;
end;
class function Items.DB.getState(Task:Core.Database.Types.TTask; VendorID: QWord; var Value:LongInt): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;   Value:=State.Unknown;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, VendorID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields,IDs.State, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbGetState, @Value) and (Value<>State.Unknown));
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Items.DB.setState(Task:Core.Database.Types.TTask; VendorID: QWord; var Value:LongInt): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtModified: double;
begin
  Result := False;
  try
    iCount := 0;
    dtModified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, VendorID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.Modified, poNone, oNone, dtModified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues,IDs.State, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbFindID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Database.Types.PLargeWord(DataP)^ := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

class function Items.DB.Find(Task:Core.Database.Types.TTask; DomainID: QWord; var sAuth: Core.Strings.VarString; Out ID: QWord): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    ID := 0;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Auth, poAnd, oEqual, sAuth, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ID, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbFindID, @ID) and (ID <> 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDB;
end.


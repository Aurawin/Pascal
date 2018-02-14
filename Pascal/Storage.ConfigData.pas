unit Storage.ConfigData;
{
 unit Storage.ConfigData.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Public Release License
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

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Strings,

  SysUtils;

type
  Defaults=class
  const
    AllDomains                   : QWord = 0;
  end;
  Items=class
  type
    PItem=^Item;
    cbEvent=procedure (cfgP:PItem) of object;
    Item=Record
      ID                         : QWord;
      DomainID                   : QWord;
      OnEnforce                  : cbEvent;  // runtime
      Name                       : Core.Strings.VarString;
      Value                      : Core.Strings.VarString;
    end;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        Name                     : Core.Database.Types.Integer = 3;
        Value                    : Core.Database.Types.Integer = 4;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        Name                     : Core.Database.Types.VarString = 'ITMNME';
        Value                    : Core.Database.Types.VarString = 'ITMVAL';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System';
        Name                     : 'Configuration Data';
        Value                    : 'scs_cfg_data';
        Hint                     : 'Storage for domain based configuration data';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..4] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType:dftString; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Value; KeyP: @Keys.Value; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; )
      );
      class Function  Add(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Write(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Exists(Task:Core.Database.Types.TTask; Var DomainID:QWord; Name:Core.Strings.VarString):Boolean;
      class Function  List(Task:Core.Database.Types.TTask; Var DomainID:QWord; Var Entries:Core.Arrays.Types.VarString):Boolean;
    end;
    class procedure Empty(var Entry:Item); overload;
    class procedure Init(var Entry:Item); overload;
    class procedure Done(Var Entry:Item); overload;

  end;

Const
  CFG_NS_OS_GROUP_NAME           = 'os_group_name';
  CFG_NS_OS_GROUP_ID             = 'os_group_id';
  CFG_NS_OS_USER_NAME            = 'os_user_name';
  CFG_NS_OS_USER_ID              = 'os_user_id';
  CFG_NS_OS_RAID_GROUP_NAME      = 'os_raid_group_name';
  CFG_NS_OS_RAID_GROUP_ID        = 'os_raid_group_id';
  CFG_NS_OS_RAID_USER_NAME       = 'os_raid_user_name';
  CFG_NS_OS_RAID_USER_ID         = 'os_raid_user_id';

implementation
uses db;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,Items.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
  end;
end;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDBM;
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
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Empty(var Entry:Item);
begin
  Entry.DomainID:=0;
  Entry.ID:=0;
  Entry.OnEnforce:=nil;
  SetLength(Entry.Name,0);
  SetLength(Entry.Value,0);
end;

class procedure Items.Init(var Entry:Item);
begin
  Entry.OnEnforce:=nil;
  Entry.DomainID:=0;
  Entry.ID:=0;
  SetLength(Entry.Name,0);
  SetLength(Entry.Value,0);
end;

class procedure Items.Done(Var Entry:Item);
begin
  Finalize(Entry.Name);
  Finalize(Entry.Value);
  Finalize(Entry);
end;


class Function  Items.DB.Add(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    iReset:=0;
    iInsertID:=Random(High(Int64));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Name,poNone,oNone,Entry.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Value,poNone,oNone,Entry.Value,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Write(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount,iIndex                  : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,Entry.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Value,poNone,oNone,Entry.Value,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_CFG_DAT_Read(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Items.PItem(DataP)^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  Items.PItem(DataP)^.Value:=Fields.FieldByName(Items.DB.Keys.Value).AsString;
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Entry.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Value,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_CFG_DAT_Read,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_CFG_DAT_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.VarString.Add(Core.Arrays.Types.PVarString(DataP),Fields.FieldByName(Items.DB.Keys.Name).AsString);
end;


class Function  Items.DB.List(Task:Core.Database.Types.TTask; Var DomainID:QWord; Var Entries:Core.Arrays.Types.VarString):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Core.Arrays.VarString.Empty(Entries);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_CFG_DAT_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Exists(Task:Core.Database.Types.TTask; Var DomainID:QWord; Name:Core.Strings.VarString):Boolean;
var
  iCount,iLcv:LongInt;
  Count:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Name,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDBM;
end.


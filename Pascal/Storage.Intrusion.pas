unit Storage.Intrusion;

{
  unit Storage.Intrusion.pas

  Intrusion Database Module

  DBMS facilities to handle Intrustion Detection and blocks


  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,
  SysUtils,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,
  Core.Timer,
  Core.Strings,
  Core.Arrays.VarString,

  Core.Arrays.Bytes,
  Core.Streams,

  Storage,
  Storage.MatrixNodes,
  Core.Utils.Files,

  XMLRead,
  DOM;

type
  Account=class
  Type
    PItem=^TItem;
    TItem=record
      User                     : Core.Strings.VarString;
      Password                 : Core.Strings.VarString;
      IP                       : Int64;
      Stamp                    : double;
    end;
    TItems=Array of PItem;
    PItems=^TItems;
    DB = class
    type
      IDs = class
      const
        DomainID                 : Core.Database.Types.Integer = 0;
        User                     : Core.Database.Types.Integer = 1;
        Password                 : Core.Database.Types.Integer = 2;
        IP                       : Core.Database.Types.Integer = 3;
        Stamp                    : Core.Database.Types.Integer = 4;
      end;
      Keys=class
      const
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        User                     : Core.Database.Types.VarString = 'IUID';
        Password                 : Core.Database.Types.VarString = 'IPWD';
        IP                       : Core.Database.Types.VarString = 'IIP';
        Stamp                    : Core.Database.Types.VarString = 'STMP';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Intrusion';
        Name                     : 'Logs';
        Value                    : 'scs_intrn_lgs';
        Hint                     : 'Intrusion log item storage';
        PrimaryKeyP              : Core.Database.Types.NoKey;
      );
      Fields: array [0..4] of Core.Database.Types.Field = (
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.User; KeyP: @Keys.User; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Password; KeyP: @Keys.Password; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.IP; KeyP: @Keys.IP; DataType: dftInt64; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Stamp; KeyP: @Keys.Stamp; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
      class function Add(Task:Core.Database.Types.TTask; DomainID:QWord; User,Pass:Core.Strings.VarString; IP:Int64 ):boolean;
      class function Recent(Task:Core.Database.Types.TTask; DomainID:QWord; User:Core.Strings.VarString; var Item:TItems):boolean; overload;
      class function Recent(Task:Core.Database.Types.TTask; DomainID:QWord; User:Core.Strings.VarString; out Count:QWord):boolean; overload;
      class function Recent(Task:Core.Database.Types.TTask; DomainID: QWord; IP:Int64):QWord; overload;

      class function Delete(Task:Core.Database.Types.TTask; DomainID:QWord; User:Core.Strings.VarString):boolean; overload;
      class function Delete(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):boolean; overload;

    end;
    Defaults=class
    const
      SecondsRecent            : LongInt = 20;  // seconds
      MaxRecent                : LongInt = 10;  // number of recent entries
      MaxRecentBeforeBlocked   : LongInt = 20;  // number of auth failures within a few minutes
    end;


    class procedure Copy(Var Source,Dest:TItem); overload;

    class procedure Empty(Var Item:TItem); overload;
    class procedure Empty(Var Item:TItems); overload;

    class procedure Init(Var Item:TItem); overload;
    class procedure Init(Var Item:TItems); overload;

    class procedure Done(Var Item:TItem); overload;
    class procedure Done(Var Item:TItems); overload;
  end;

  Intruder = class
  type
    DB = class
    type
      IDs = class
      const
        DomainID                 : Core.Database.Types.Integer = 0;
        IP                       : Core.Database.Types.Integer = 1;
        Stamp                    : Core.Database.Types.Integer = 2;
      end;
      Keys=class
      const
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        IP                       : Core.Database.Types.VarString = 'IIP';
        Stamp                    : Core.Database.Types.VarString = 'STMP';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Intrusion';
        Name                     : 'Intruders';
        Value                    : 'scs_intrn_idr';
        Hint                     : 'Intruder storage';
        PrimaryKeyP              : Core.Database.Types.NoKey;
      );
      Fields: array [0..2] of Core.Database.Types.Field = (
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.IP; KeyP: @Keys.IP; DataType: dftInt64; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Stamp; KeyP: @Keys.Stamp; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function Add(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64; Stamp:Double):boolean;
      class function Count(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64; Stamp:Double):QWord; overload;
      class function Recent(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):QWord;
      class function Delete(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):boolean;
    end;
    Defaults=class
    const
      Threshold                : LongInt = 9;  // number of recent entries
      SinceWindow              : LongInt = 2;  // number of minutes used for counting
      SecondsSinceLastRecord   : LongInt = 30; // number of days used for blocking
    end;

    PItem=^TItem;
    TItem=record
      ID                       : QWord;
      IP                       : Int64;
      Stamp                    : Double;
    end;
    TItems=Array of PItem;
    PItems=^TItems;


    class procedure Copy(Var Source,Dest:TItem); overload;

    class procedure Empty(Var Item:TItem); overload;
    class procedure Empty(Var Item:TItems); overload;

    class procedure Init(Var Item:TItem); overload;
    class procedure Init(Var Item:TItems); overload;

    class procedure Done(Var Item:TItem); overload;
    class procedure Done(Var Item:TItems); overload;
  end;

implementation
uses
  DB,
  sqldb,
  DateUtils;

procedure cbDestroyLog(ItemP: Core.Database.Monitor.Types.PItem);
begin
  Done(Account.DB.TableP^);
  Account.DB.TableP := nil;
  Account.DB.MonitorP := nil;
end;

procedure cbDestroyIntruder(ItemP: Core.Database.Monitor.Types.PItem);
begin
  Done(Intruder.DB.TableP^);
  Intruder.DB.TableP := nil;
  Intruder.DB.MonitorP := nil;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Account.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Account.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Account.DB.TableP, useForCriteria, Account.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Intruder.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Intruder.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Intruder.DB.TableP, useForCriteria, Intruder.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
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
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Account.DB do begin
    if TableP = nil then begin
      New(TableP);
      Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Init(MonitorP^, TableP^, @cbDestroyLog, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Intruder.DB do begin
    if TableP = nil then begin
      New(TableP);
      Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Init(MonitorP^, TableP^, @cbDestroyIntruder, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;


class procedure Account.Copy(Var Source,Dest:TItem);
begin
  Dest.IP:=Source.IP;
  Dest.User:=Source.User;
  Dest.Password:=Source.Password;
  Dest.Stamp:=Source.Stamp;
end;

class procedure Account.Empty(Var Item:TItem);
begin
  SetLength(Item.User,0);
  SetLength(Item.Password,0);
  Item.IP:=0;
  Item.Stamp:=0;
end;

class procedure Account.Empty(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Account.Init(Var Item:TItem);
begin
  SetLength(Item.User,0);
  SetLength(Item.Password,0);
  Item.IP:=0;
  Item.Stamp:=0;
end;

class procedure Account.Init(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Account.Done(Var Item:TItem);
begin
  Finalize(Item.User);
  Finalize(Item.Password);
  Finalize(Item);
end;

class procedure Account.Done(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class function Account.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; User,Pass:Core.Strings.VarString; IP:Int64):boolean;
var
  Commands:Core.Database.Types.Commands;
  iCount:LongInt;
  Stamp:Double;
  LogCount:QWord;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Stamp:=Core.Timer.dtUT;
    User:=UTF8Encode(User);
    Pass:=UTF8Encode(Pass);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.User,poNone,oNone,User,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Password,poNone,oNone,Pass,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.IP,poNone,oNone,IP,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Stamp,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

    (*
    // now check to see if we need to lock the user out
    if Log.DB.Recent(Task,DomainID,User,LogCount) then begin
      if LogCount>=Intruder.Defaults.Threshold then begin
        if Intruder.DB.Count(Task,DomainID,IP,Stamp)=0 then
          Intruder.DB.Add(Task,DomainID,IP);
      end;
    end;
    *)

  finally
    Core.Database.Done(Commands);
  end;
end;

class function Account.DB.Delete(Task:Core.Database.Types.TTask; DomainID:QWord; User:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.User, poNone, oEqual, User, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Account.DB.Delete(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.IP, poNone, oEqual, IP, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbRecentLogs(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Account.PItems;
  itmP:Account.PItem;
  iCount:LongInt;
begin
  ListP:=DataP;
  iCount:=System.Length(ListP^);
  if iCount<Account.Defaults.MaxRecent then begin
    New(itmP);
    Account.Init(itmP^);
    System.SetLength(ListP^,iCount+1);
    ListP^[iCount]:=itmP;
    With Account.DB.Keys do begin
      ItmP^.User:=Fields.FieldByName(User).AsString;
      ItmP^.Password:=Fields.FieldByName(Password).AsString;
      ItmP^.IP:=Fields.FieldByName(IP).AsLargeInt;
      ItmP^.Stamp:=Fields.FieldByName(Stamp).AsFloat;
    end;
  end;
end;

class function Account.DB.Recent(Task:Core.Database.Types.TTask; DomainID:QWord; User:Core.Strings.VarString; var Item:TItems):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Stamp:Double;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);
    Stamp:=DateUtils.IncSecond(Core.Timer.dtUT,-Defaults.SecondsRecent);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.User, poAnd, oEqual, User, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Stamp, poAnd, oGreaterThanEqualTo, Stamp, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.User,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Password,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.IP,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Stamp,poNone,oNone,Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbRecentLogs, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Account.DB.Recent(Task:Core.Database.Types.TTask; DomainID:QWord; User:Core.Strings.VarString; out Count:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Stamp:Double;
begin
  Result := False;
  try
    iCount := 0;
    Count:=0;
    Stamp:=DateUtils.IncSecond(Core.Timer.dtUT,-Defaults.SecondsRecent);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.User, poAnd, oEqual, User, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Stamp, poAnd, oGreaterThanEqualTo, Stamp, Commands);

    Result:=Core.Database.SQL.Count(Task, @Commands,Count);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Account.DB.Recent(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Stamp:Double;
begin
  Result := 0;
  try
    iCount := 0; Result:=0;
    Stamp:=DateUtils.IncSecond(Core.Timer.dtUT,-Defaults.SecondsRecent);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.IP, poAnd, oEqual, IP, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Stamp, poAnd, oGreaterThanEqualTo, Stamp, Commands);

    Core.Database.SQL.Count(Task, @Commands,Result);
  finally
    Core.Database.Done(Commands);
  end;
end;


class procedure Intruder.Copy(Var Source,Dest:TItem);
begin
  Dest.IP:=Source.IP;
  Dest.ID:=Source.ID;
  Dest.Stamp:=Source.Stamp;
end;

class procedure Intruder.Empty(Var Item:TItem);
begin
  Item.ID:=0;
  Item.IP:=0;
  Item.Stamp:=0;
end;

class procedure Intruder.Empty(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Intruder.Init(Var Item:TItem);
begin
  Item.ID:=0;
  Item.IP:=0;
  Item.Stamp:=0;
end;

class procedure Intruder.Init(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Intruder.Done(Var Item:TItem);
begin
  Finalize(Item);
end;

class procedure Intruder.Done(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class function Intruder.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64; Stamp:Double):boolean;
var
  Commands:Core.Database.Types.Commands;
  iCount:LongInt;
begin
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.IP,poNone,oNone,IP,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Stamp,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Intruder.DB.Count(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64; Stamp:Double):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtCheck : Core.Database.Types.Double;
begin
  Result:=0;
  try
    iCount := 0;
    dtCheck:=DateUtils.IncMinute(Stamp,-Defaults.SinceWindow);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.IP, poAnd, oEqual, IP, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Stamp, poAnd, oGreaterThanEqualTo, dtCheck, Commands);


    Core.Database.SQL.Count(Task, @Commands,Result);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Intruder.DB.Recent(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtCheck : Core.Database.Types.Double;
begin
  Result:=0;
  try
    iCount := 0;
    dtCheck:=DateUtils.IncSecond(Core.Timer.dtUT,-Defaults.SecondsSinceLastRecord);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.IP, poAnd, oEqual, IP, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Stamp, poAnd, oGreaterThanEqualTo, dtCheck, Commands);

    Core.Database.SQL.Count(Task, @Commands,Result);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Intruder.DB.Delete(Task:Core.Database.Types.TTask; DomainID:QWord; IP:Int64):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.IP, poAnd, oEqual, IP, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


initialization
  RegisterDB;
end.


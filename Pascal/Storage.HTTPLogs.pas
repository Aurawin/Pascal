{
 unit Storage.HTTPLogs.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
}

unit Storage.HTTPLogs;

interface

uses
  Classes,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Core.Strings,

  Storage,
  Storage.Main,

  SysUtils;

Type
  Items=class
  Type
    Item=record
      ID                         : QWord;
      RemoteAddress              : QWord;
      StatusCode                 : LongInt;
      TimeStamp                  : Double;
      URI                        : Core.Strings.VarString;
      Method                     : Core.Strings.VarString;
      UserAgent                  : Core.Strings.VarString;
      Referer                    : Core.Strings.VarString;
    end;
    PItem=^Item;
    List=Array of Item;
    PList=^List;
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        DomainID                 : Core.Database.Types.Integer = 1;
        TimeStamp                : Core.Database.Types.Integer = 2;
        Method                   : Core.Database.Types.Integer = 3;
        StatusCode               : Core.Database.Types.Integer = 4;
        RemoteAddress            : Core.Database.Types.Integer = 5;
        UserAgent                : Core.Database.Types.Integer = 6;
        Referer                  : Core.Database.Types.Integer = 7;
        URI                      : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        TimeStamp                : Core.Database.Types.VarString = 'ITMDTS';
        Method                   : Core.Database.Types.VarString = 'ITMETH';
        StatusCode               : Core.Database.Types.VarString = 'ITMSC';
        RemoteAddress            : Core.Database.Types.VarString = 'ITMRA';
        UserAgent                : Core.Database.Types.VarString = 'ITMUA';
        Referer                  : Core.Database.Types.VarString = 'ITMREF';
        URI                      : Core.Database.Types.VarString = 'ITMURI';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Logging';
        Name                     : 'HTTP Logs';
        Value                    : 'scs_lgshttp';
        Hint                     : 'Domain based logging for web services';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.TimeStamp; KeyP: @Keys.TimeStamp; DataType: dftDateTime; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Method; KeyP: @Keys.Method; DataType: dftString; AutoCreate: True; Verified: False; Precision: 55; Flags: cfNone; ),
        (IDP: @IDs.StatusCode; KeyP: @Keys.StatusCode; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.RemoteAddress; KeyP: @Keys.RemoteAddress; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserAgent; KeyP: @Keys.UserAgent; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Referer; KeyP: @Keys.Referer; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;),
        (IDP: @IDs.URI; KeyP: @Keys.URI; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );

      class Function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entry:Item):Boolean;
      class Function  Fill(Task:Core.Database.Types.TTask; DomainID:QWord; StartDate,EndDate:Double; Var Entries:List):Boolean;
    end;
    class procedure Init(Var Entry:Item); overload;
    class procedure Init(var Entries:List); overload;

    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(var Entries:List); overload;

    class procedure Done(Var Entry:Item); overload;
    class procedure Done(var Entries:List); overload;
  end;

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
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted();
  end;
end;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDBM;
var
  iLcv                           : LongInt;
begin
  With Items.DB do begin
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

class procedure Items.Empty(Var Entry:Item);
begin
  Entry.ID:=0;
  Entry.StatusCode:=0;
  Entry.TimeStamp:=0;
  Entry.RemoteAddress:=0;
  SetLength(Entry.URI,0);
  SetLength(Entry.Method,0);
  SetLength(Entry.UserAgent,0);
  SetLength(Entry.Referer,0);
end;

class procedure Items.Init(Var Entry:Item);
begin
  Entry.ID:=0;
  Entry.StatusCode:=0;
  Entry.TimeStamp:=0;
  Entry.RemoteAddress:=0;
  SetLength(Entry.URI,0);
  SetLength(Entry.Method,0);
  SetLength(Entry.UserAgent,0);
  SetLength(Entry.Referer,0);
end;

class procedure Items.Done(Var Entry:Item);
begin
  Finalize(Entry.URI);
  Finalize(Entry.Method);
  Finalize(Entry.UserAgent);
  Finalize(Entry.Referer);
  Finalize(Entry);
end;

class procedure Items.Init(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Empty(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Done(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

procedure CB_HTTP_RetrieveLogs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
var
  iCount:LongInt;
  ListP:Items.PList;
begin
  ListP:=DataP;
  iCount:=Length(ListP^);
  SetLength(ListP^,iCount+1);
  With ListP^[iCount] do begin
    ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    TimeStamp:=Fields.FieldByName(Items.DB.Keys.TimeStamp).AsDateTime;
    StatusCode:=Fields.FieldByName(Items.DB.Keys.StatusCode).AsInteger;
    RemoteAddress:=Fields.FieldByName(Items.DB.Keys.RemoteAddress).AsLargeInt;
    Method:=Fields.FieldByName(Items.DB.Keys.Method).AsString;
    UserAgent:=Fields.FieldByName(Items.DB.Keys.UserAgent).AsString;
    Referer:=Fields.FieldByName(Items.DB.Keys.Referer).AsString;
    URI:=Fields.FieldByName(Items.DB.Keys.URI).AsString;
  end;
end;

class Function Items.DB.Fill(Task:Core.Database.Types.TTask; DomainID:QWord; StartDate,EndDate:Double; Var Entries:List):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.TimeStamp,poAnd,oGreaterThanEqualTo,StartDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.TimeStamp,poAnd,oLessThanEqualTo,EndDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.TimeStamp,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Method,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.StatusCode,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.RemoteAddress,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.UserAgent,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Referer,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.URI,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_HTTP_RetrieveLogs,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;


class Function Items.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.TimeStamp,poNone,oNone,Entry.TimeStamp,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Method,poNone,oNone,Entry.Method,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.StatusCode,poNone,oNone,Entry.StatusCode,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.RemoteAddress,poNone,oNone,Entry.RemoteAddress,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserAgent,poNone,oNone,Entry.UserAgent,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Referer,poNone,oNone,Entry.Referer,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.URI,poNone,oNone,Entry.URI,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDBM;
end.


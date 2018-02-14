{
unit Storage.TmpAccounts.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

Storage for Temporary User Accounts for Search System
i.e.) Cookies over HTTP

}
unit Storage.TmpAccounts;


interface

uses
  Classes,

  Core.Strings,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,

  Core.Timer,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,

  SysUtils;


Type
  Items=Class
  Type
    Item=record
      ID                           : QWord;
      LastIP                       : QWord;
      Created                      : Double;
      LastUse                      : Double;
      Email                        : Core.Strings.VarString;
      Password                     : Core.Strings.VarString;
      Auth                         : Core.Strings.VarString;
    end;
    List=Array of Item;
    PItem=^Item;
    PList=^List;

    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        LastIP                   : Core.Database.Types.Integer = 3;
        Created                  : Core.Database.Types.Integer = 4;
        LastUse                  : Core.Database.Types.Integer = 5;
        Auth                     : Core.Database.Types.Integer = 6;
        Email                    : Core.Database.Types.Integer = 7;
        Password                 : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        LastIP                   : Core.Database.Types.VarString = 'ITMLIP';
        Created                  : Core.Database.Types.VarString = 'ITMCDT';
        LastUse                  : Core.Database.Types.VarString = 'ITMLDT';
        Auth                     : Core.Database.Types.VarString = 'ITAUTH';
        Email                    : Core.Database.Types.VarString = 'ITMEML';
        Password                 : Core.Database.Types.VarString = 'ITMPWD';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Users/Accounts';
        Name                     : 'Anonymous';
        Value                    : 'scs_tmpaccts';
        Hint                     : 'Temporary User Accounts for anonymous access';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.LastIP; KeyP: @Keys.LastIP; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.LastUse; KeyP: @Keys.LastUse; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.Auth; KeyP: @Keys.Auth; DataType: dftString; AutoCreate: True; Verified: False; Precision: 16; Flags: cfNone ),
        (IDP: @IDs.Email; KeyP: @Keys.Email; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone ),
        (IDP: @IDs.Password; KeyP: @Keys.Password; DataType: dftString; AutoCreate: True; Verified: False; Precision: 25; Flags: cfNone )
      );
      class Function  Add(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; LastIP:QWord; Var Auth:Core.Strings.VarString):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
      class Function  SetPassword(Task:Core.Database.Types.TTask; ID:QWord; Var Password:Core.Strings.VarString):Boolean;
      class Function  SetLastIP(Task:Core.Database.Types.TTask; ID,LastIP:QWord):Boolean;
      class Function  Register(Task:Core.Database.Types.TTask; ID,LastIP:QWord; Var Email,Password:Core.Strings.VarString):Boolean;
      class Function  List(Task:Core.Database.Types.TTask; Var Entries:List):Boolean;
      class Function  Find(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; Var Auth,Email,Password:Core.Strings.VarString):Boolean; overload;
      class Function  Find(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; Var Auth:Core.Strings.VarString):Boolean; overload;
    end;
    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:List); overload;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:List); overload;
    class procedure Copy(Var Source,Destination:Item);
  end;

implementation
uses db;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
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

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP=nil then begin
      New(TableP);
      Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Init(MonitorP^,TableP^,@cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.Created:=Source.Created;
  Destination.LastUse:=Source.LastUse;
  Destination.LastIP:=Source.LastIP;
  Destination.Email:=Source.Email;
  Destination.Password:=Source.Password;
  Destination.Auth:=Source.Auth;
end;

class procedure Items.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    Created:=0;
    LastUse:=0;
    LastIP:=0;
    Core.Strings.Empty(Email);
    Core.Strings.Empty(Password);
    Core.Strings.Empty(Auth);
  end;
end;

class procedure Items.Done(Var Entry:Item);
begin
  with Entry do begin
    Finalize(Email);
    Finalize(Password);
    Finalize(Auth);
  end;
  Finalize(Entry);
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

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; LastIP:QWord; Var Auth:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  iInsertID,iReset:QWord;
  dtNow:Double;
begin
  Result:=False;
  Try
    iCount:=0;
    iInsertID:=System.Random(High(Integer));
    iReset:=0;
    ID:=0;
    dtNow:=Core.Timer.dtUT;

    Auth:=Core.Arrays.VarString.GenerateRandomString(16,16);

    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastIP,poNone,oNone,LastIP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,dtUT,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastUse,poNone,oNone,dtUT,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Auth,poNone,oNone,Auth,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (ID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    // Future Version Needs to make sure Temp Result Store Is Cleared Of All Their Data
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_TempUser_Find(CommandP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  UserP:Items.PItem;
begin
  UserP:=DataP;
  UserP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  UserP^.Email:=Fields.FieldByName(Items.DB.Keys.Email).AsString;
  UserP^.Password:=Fields.FieldByName(Items.DB.Keys.Password).AsString;
end;

class Function  Items.DB.Find(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; Var Auth,Email,Password:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  Data:Item;
begin
  Result:=False;
  Try
    iCount:=0; ID:=0;
    Empty(Data);
    Core.Strings.Empty(Email);
    Core.Strings.Empty(Password);
    Try
      Data.Auth:=Auth;
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Auth,poNone,oEqual,Data.Auth,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Email,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Password,poNone,oNone,Commands);
      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_TempUser_Find,@Data);
      If Result then begin
        ID:=Data.ID;
        Email:=Data.Email;
        Password:=Data.Password;
        Result:=ID>0;
      end;
    Finally
      Done(Data);
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure CB_TempUser_Find_Auth(CommandP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

class Function  Items.DB.Find(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; Var Auth:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; ID:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Auth,poNone,oEqual,Auth,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_TempUser_Find_Auth,@ID) and (ID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.SetPassword(Task:Core.Database.Types.TTask; ID:QWord; Var Password:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Password,poNone,oNone,Password,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.SetLastIP(Task:Core.Database.Types.TTask; ID,LastIP:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  dtNow:Double;
begin
  Result:=False;
  Try
    iCount:=0; dtNow:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastIP,poNone,oNone,LastIP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastUse,poNone,oNone,dtNow,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;


class Function  Items.DB.Register(Task:Core.Database.Types.TTask; ID,LastIP:QWord; Var Email,Password:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastIP,poNone,oNone,LastIP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Password,poNone,oNone,Password,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Email,poNone,oNone,Email,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_TempUser_List(CommandP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iIndex:LongInt;
  ListP:Items.PList;
begin
  ListP:=DataP;
  iIndex:=Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex].ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  ListP^[iIndex].Created:=Fields.FieldByName(Items.DB.Keys.Created).AsDateTime;
  ListP^[iIndex].LastUse:=Fields.FieldByName(Items.DB.Keys.LastUse).AsDateTime;
  ListP^[iIndex].LastIP:=Fields.FieldByName(Items.DB.Keys.LastIP).AsLargeInt;
end;

class Function  Items.DB.List(Task:Core.Database.Types.TTask; Var Entries:List):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Created,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastUse,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastIP,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_TempUser_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDBM;
end.


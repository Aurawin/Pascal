{
unit Storage.SrchUserLayer.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

// The User Store Stores End User Interactions...

}
unit Storage.SrchUserLayer;



interface

uses
  Classes,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Strings,

  SysUtils;

Type
  User=class
  Type
    Commands=class
    const
      None                       = 0;
      Delete                     = 1;
      Move                       = 2;
      Copy                       = 3;
      Rank                       = 4;
      Rate                       = 5;
      Vote                       = 6;
    end;
    Item=record
      ID                         : QWord;
      ProviderID                 : QWord;
      UserID                     : QWord;
      ResultID                   : QWord;
      Command                    : QWord;
      Data                       : Core.Strings.VarString;
    end;
    PItem=^Item;
    Items=Array of Item;
    PItems=^Items;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        ProviderID               : Core.Database.Types.Integer = 2;
        DomainID                 : Core.Database.Types.Integer = 3;
        UserID                   : Core.Database.Types.Integer = 4;
        ResultID                 : Core.Database.Types.Integer = 5;
        Command                  : Core.Database.Types.Integer = 6;
        Data                     : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITIID';
        ProviderID               : Core.Database.Types.VarString = 'ITMPD';
        DomainID                 : Core.Database.Types.VarString = 'IMDID';
        UserID                   : Core.Database.Types.VarString = 'IMUID';
        ResultID                 : Core.Database.Types.VarString = 'IMRID';
        Command                  : Core.Database.Types.VarString = 'IMCMD';
        Data                     : Core.Database.Types.VarString = 'IMCDT';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Users/Interactions';
        Name                     : 'Registered';
        Value                    : 'scs_srch_ui';
        Hint                     : 'Interactions for registered domain users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..7] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ResultID; KeyP: @Keys.ResultID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Command; KeyP: @Keys.Command; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType:dftString; AutoCreate:True; Verified:False; Precision:1024; Flags: cfNone; )
      );

      class Function  ListOfCommands(Task:Core.Database.Types.TTask; UserID,ResultID: QWord; Var Entries:Items):Boolean;
      class Function  DeleteUserData(Task:Core.Database.Types.TTask; UserID:QWord):Boolean;
      class Function  DeleteDomainData(Task:Core.Database.Types.TTask; DomainID:QWord):Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; ProviderID,DomainID,UserID,ResultID,Command:QWord; Var Data:Core.Strings.VarString):Boolean;
    end;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:Items) overload;
    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:Items) overload;
  end;


implementation
uses db;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With User.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushProviderDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=User.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,User.DB.IDs.ProviderID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=User.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,User.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,User.DB.TableP,useForCriteria,User.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushUserDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=User.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,User.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,User.DB.TableP,useForCriteria,User.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.PROVIDER_DELETED : PushProviderDeleted;
    Core.Database.Monitor.Notify.DOMAIN_DELETED   : PushDomainDeleted;
    Core.Database.Monitor.Notify.USER_DELETED     : PushUserDeleted;
  end;
end;


procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with User.DB do begin
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


class procedure User.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    ProviderID:=0;
    UserID:=0;
    ResultID:=0;
    Command:=0;
    Core.Strings.Empty(Data);
  end;
end;

class procedure User.Done(Var Entry:Item);
begin
  Core.Strings.Done(Entry.Data);
  Finalize(Entry);
end;

class procedure User.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure User.Done(Var Entries:Items) ;
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

procedure CB_UserLayer_List_Of_Commands(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iCount:LongInt;
  ListP:User.PItems;
begin
  ListP:=DataP;
  iCount:=Length(ListP^);
  SetLength(ListP^,iCount+1);
  ListP^[iCount].ID:=Fields.FieldByName(User.DB.Keys.ID).AsLargeInt;
  ListP^[iCount].ProviderID:=Fields.FieldByName(User.DB.Keys.ProviderID).AsLargeInt;
  ListP^[iCount].UserID:=Fields.FieldByName(User.DB.Keys.UserID).AsLargeInt;
  ListP^[iCount].ResultID:=Fields.FieldByName(User.DB.Keys.ResultID).AsLargeInt;
  ListP^[iCount].Command:=Fields.FieldByName(User.DB.Keys.Command).AsLargeInt;
  ListP^[iCount].Data:=Fields.FieldByName(User.DB.Keys.Data).AsString;
end;

class Function  User.DB.ListOfCommands(Task:Core.Database.Types.TTask; UserID,ResultID: QWord;  Var Entries:Items):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResultID,poNone,oEqual,ResultID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ProviderID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.UserID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ResultID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Command,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Data,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_UserLayer_List_Of_Commands,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  User.DB.DeleteUserData(Task:Core.Database.Types.TTask; UserID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  User.DB.DeleteDomainData(Task:Core.Database.Types.TTask; DomainID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;


class Function  User.DB.Add(Task:Core.Database.Types.TTask; ProviderID,DomainID,UserID,ResultID,Command:QWord; Var Data:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ResultID,poNone,oNone,ResultID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Command,poNone,oNone,Command,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Data,poNone,oNone,Data,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;
initialization
  RegisterDBM;
end.


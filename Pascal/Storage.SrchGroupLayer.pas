unit Storage.SrchGroupLayer;

{
unit Storage.SrchGroupLayer.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html


This Database Module is for Enabling Interactive Search for groups

}


interface

uses
  Classes,

  Core.Database,
  Core.Database.SQL,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,

  Core.Strings,

  SysUtils;

Const

  // Group Store Access Control List Elements

  ICL_NONE                       = 1 shl 0;
  ICL_DELETE                     = 1 shl 1;
  ICL_RANK                       = 1 shl 2;
  ICL_SAVE                       = 1 shl 3;
  ICL_TAG                        = 1 shl 4;
  // etc.
  IACL_ALL                       = 1 shl 63;

  ACL_NONE                       = 1 shl 0;
  ACL_CREATE_SESSION             = 1 shl 1;
  ACL_JOIN_SESSION               = 1 shl 2;
  ACL_DELETE_SESSION             = 1 shl 3;
  ACL_ADD_QUERY                  = 1 shl 4;
  // etc.
  ACL_ALL                        = 1 shl 63;

Type
  TGroupData=record
    ID                           : QWord;
    OwnerID                      : QWord;
    Default_IACLPs               : QWord;
    Default_ACLPs                : QWord;
    Name                         : Core.Strings.VarString;
    Description                  : Core.Strings.VarString;
  end;
  TGroupLayerPrivileges=record
    GroupID                      : QWord;
    UserID                       : QWord;
    AccessPrivileges             : QWord;
    InteractionPrivileges        : QWord;
  end;
  TGroupInteraction=record
    ID                           : QWord;
    GroupID                      : QWord;
    ProviderID                   : QWord;
    UserID                       : QWord;
    ResultID                     : QWord;
    Command                      : QWord;
    CommandData                  : Core.Strings.VarString;
  end;

  PGroupData=^TGroupData;
  TGroupDataList=Array of TGroupData;
  PGroupDataList=^TGroupDataList;

  Groups=class
  Type
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        OwnerID                  : Core.Database.Types.Integer = 4;
        DefaultACLP              : Core.Database.Types.Integer = 5;
        DefaultICLP              : Core.Database.Types.Integer = 6;
        Name                     : Core.Database.Types.Integer = 7;
        Description              : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        OwnerID                  : Core.Database.Types.VarString = 'ITMOID';
        DefaultACLP              : Core.Database.Types.VarString = 'DACLP';
        DefaultICLP              : Core.Database.Types.VarString = 'DICL';
        Name                     : Core.Database.Types.VarString = 'ITMNME';
        Description              : Core.Database.Types.VarString = 'ITMDES';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Groups';
        Name                     : 'Structures';
        Value                    : 'scs_srch_gps';
        Hint                     : 'Group structure storage for domain users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..8] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DefaultACLP; KeyP: @Keys.DefaultACLP; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DefaultICLP; KeyP: @Keys.DefaultICLP; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType:dftString; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:1024*1; Flags: cfNone; )
      );
      class Function  Create(Task:Core.Database.Types.TTask; DomainID,UserID: QWord; Var ItemID:QWord):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; ItemID:QWord):Boolean;
      class Function  Edit(Task:Core.Database.Types.TTask; Var Data:TGroupData):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; Var Data:TGroupData):Boolean;
    end;
  end;
  ACL=class
  Type
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        GroupID                  : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ACLP                     : Core.Database.Types.Integer = 4;
        ICLP                     : Core.Database.Types.Integer = 5;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        GroupID                  : Core.Database.Types.VarString = 'GPID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        ACLP                     : Core.Database.Types.VarString = 'ACLP';
        ICLP                     : Core.Database.Types.VarString = 'IICLP';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Groups';
        Name                     : 'Control Lists';
        Value                    : 'scs_srch_gcls';
        Hint                     : 'Group access/interaction control list storage for domain users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..5] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ACLP; KeyP: @Keys.ACLP; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ICLP; KeyP: @Keys.ICLP; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; )
      );
    end;
  end;
  Interactions=class
  Type
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        ProviderID               : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        GroupID                  : Core.Database.Types.Integer = 4;
        ResultID                 : Core.Database.Types.Integer = 5;
        Command                  : Core.Database.Types.Integer = 6;
        CommandData              : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        ProviderID               : Core.Database.Types.VarString = 'PVID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        GroupID                  : Core.Database.Types.VarString = 'GPID';
        ResultID                 : Core.Database.Types.VarString = 'RID';
        Command                  : Core.Database.Types.VarString = 'IACMD';
        CommandData              : Core.Database.Types.VarString = 'IACDD';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Groups';
        Name                     : 'Interactions';
        Value                    : 'scs_srch_gia';
        Hint                     : 'Group interactions storage for domain users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..7] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ResultID; KeyP: @Keys.ResultID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Command; KeyP: @Keys.Command; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP: @IDs.CommandData; KeyP: @Keys.CommandData; DataType:dftString; AutoCreate:True; Verified:False; Precision:1024; Flags: cfNone; )
      );
    end;
  end;

  procedure Empty(Var Item:TGroupLayerPrivileges); overload;
  procedure Empty(Var Item:TGroupData); overload;
  procedure Empty(Var Item:TGroupInteraction); overload;


implementation
uses db;

procedure cbDestroyGroups(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Groups.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyControlLists(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With ACL.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyInteractions(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Interactions.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Groups.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Groups.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Groups.DB.TableP,useForCriteria,Groups.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=ACL.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,ACL.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,ACL.DB.TableP,useForCriteria,ACL.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Interactions.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,useForCriteria,Interactions.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
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
    if ItemP=Groups.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Groups.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Groups.DB.TableP,useForCriteria,Groups.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=ACL.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,ACL.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,ACL.DB.TableP,useForCriteria,ACL.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Interactions.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,useForCriteria,Interactions.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;
  procedure PushProviderDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Interactions.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,useForCriteria,Interactions.DB.IDs.ProviderID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED   : PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED     : PushUserDeleted();
    Core.Database.Monitor.Notify.PROVIDER_DELETED : PushProviderDeleted();
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Groups.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyGroups,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with ACL.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyControlLists,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Interactions.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyInteractions,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


procedure Empty(Var Item:TGroupData);
begin
  Item.ID:=0;
  Item.OwnerID:=0;
  Item.Default_ACLPs:=ACL_NONE;
  Item.Default_IACLPs:=ICL_NONE;
  Empty(Item.Name);
  Empty(Item.Description);
end;

procedure Empty(Var Item:TGroupInteraction);
begin
  Item.ID:=0;
  Item.GroupID:=0;
  Item.ProviderID:=0;
  Item.UserID:=0;
  Item.ResultID:=0;
  Item.Command:=0;
  SetLength(Item.CommandData,0);
end;

procedure Empty(Var Item:TGroupLayerPrivileges);
begin
  Item.GroupID:=0;
  Item.UserID:=0;
  Item.AccessPrivileges:=ACL_NONE;
  Item.InteractionPrivileges:=ICL_NONE;
end;

class Function  Groups.DB.Create(Task:Core.Database.Types.TTask; DomainID,UserID: QWord; Var ItemID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  iInsertID                      : QWord;
  iReset                         : QWord;
begin
  Result:=False;
  Try
    iCount:=0; iInsertID:=Random(High(Integer)); iReset:=0; ItemID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.OwnerID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands) and (ItemID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Groups.DB.Delete(Task:Core.Database.Types.TTask; ItemID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure CB_GroupLayer_Read_Group(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iCount                         : LongInt;
  GroupLayerDataP                : PGroupData;
begin
  GroupLayerDataP:=DataP;
  GroupLayerDataP^.Name:=Fields.FieldByName(Groups.DB.Keys.Name).AsString;
  GroupLayerDataP^.Description:=Fields.FieldByName(Groups.DB.Keys.Description).AsString;
end;

class Function  Groups.DB.Read(Task:Core.Database.Types.TTask; Var Data:TGroupData):Boolean;  // ID Is Already Present
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Data.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GroupLayer_Read_Group,@Data);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Groups.DB.Edit (Task:Core.Database.Types.TTask; Var Data:TGroupData):Boolean;  // ID Is Already Present
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Data.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,Data.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Description,poNone,oNone,Data.Description,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDBM;
end.


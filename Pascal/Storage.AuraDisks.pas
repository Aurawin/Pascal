unit Storage.AuraDisks;

{
  unit Storage.AuraDisks.pas

  AuraDisks Database Module

  DBMS facilities to handle Cloud Storage for

       Users
       Social Networks
       .
       .
       .
       And future capabilities.

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Core.Strings,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.XML,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Core.Arrays.Bytes,
  Core.Interlocked,
  Core.Streams,
  Core.Utils.Files,

  Storage.Main,
  Storage.MatrixNodes,
  Storage.UserAccounts,

  App,
  App.Consts,

  XMLRead,
  DOM,
  Classes,
  SysUtils;

type
  Codes=class
  const
    SUCCESS                  : LongInt = 0;
    ERROR_DISK_NOT_MOUNTED   : LongInt = 1;
    ERROR_DISK_FULL          : LongInt = 2;
    ERROR_DISK_QUOTA         : LongInt = 3;
  end;
  Kinds=class
  Const
    User                     : LongInt = 0;  // NEVER MODIFY
    Social                   : LongInt = 1;  // NEVER MODIFY
    CoreData                 : LongInt = 2;  // NEVER MODIFY
    AppData                  : LongInt = 3;  // NEVER MODIFY
    Domain                   : LongInt = 4;  // NEVER MODIFY
    NOSQL                    : LongInt = 5;  // NEVER MODIFY
  end;
  Kind=class
  Const
    User                     = 0;
    Social                   = 1;
    CoreData                 = 2;
    AppData                  = 3;
    Domain                   = 4;
    NOSQL                    = 5;
  end;
  Use=class
  const
    Global                   : LongInt = 0;  // NEVER MODIFY
  end;
  Domain=class
  const
    Global                   : LongInt = 0; // NEVER MODIFIY
  end;
  Folder=class
  const
    Global                   : LongInt = 0; // NEVER MODIFY
    Queue                    : LongInt = 1; // System Queue
  end;
  Process=class
  Const
    UserID                   : QWord   = 0; // root;
    GroupID                  : QWord   = 0; // root
    FileMode                 : LongInt = &664; // ug+rw o+r
    FolderMode               : LongInt = &774; // ug+rwx o+r
  end;

  Router = class
  type
    XML=class
    type
      Stanzas=class
      const
        Item                   = 'disk';
        Items                  = 'disks';
      end;
      Fields=class
      const
        ID                     = 'id';
        DomainID               = 'did';
        UseID                  = 'uid';
        NodeID                 = 'nid';
        PrimaryID              = 'pid';
        SecondaryID            = 'sid';
        Kind                   = 'kind';
        Enabled                = 'enab';
        Live                   = 'live';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UseID                    : Core.Database.Types.Integer = 3;
        NodeID                   : Core.Database.Types.Integer = 4;
        Kind                     : Core.Database.Types.Integer = 5;
        Enabled                  : Core.Database.Types.Integer = 6;
        Live                     : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UseID                    : Core.Database.Types.VarString = 'IUID';
        NodeID                   : Core.Database.Types.VarString = 'INID';
        Kind                     : Core.Database.Types.VarString = 'IKND';
        Locked                   : Core.Database.Types.VarString = 'ILCK';
        Enabled                  : Core.Database.Types.VarString = 'IEBD';
        Live                     : Core.Database.Types.VarString = 'ILIV';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit                   : True;
        Group        : 'Clustering/Nodes';
        Name         : 'Disks';
        Value        : 'scs_mtx_dsks';
        Hint         : 'Cloud disk storage';
        PrimaryKeyP  : @Keys.ID;
      );
      Fields: array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UseID; KeyP: @Keys.UseID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NodeID; KeyP: @Keys.NodeID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Enabled; KeyP: @Keys.Enabled; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Live; KeyP: @Keys.Live; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PItem=^TItem;
    TItem=record
      ID                       : QWord;
      UseID                    : QWord;
      NodeID                   : QWord;
      Kind                     : LongInt;
      Enabled                  : boolean;
      Live                     : boolean;
    end;
    TItems=Array of PItem;
    PItems=^TItems;

    const AuDisks:Core.Strings.VarString = 'AuDisks';
    const FMT_ROUTE_FILE:Core.Strings.VarString='%0:s%1:s%0:s%2:s%0:s%3:s%0:s%4:s%0:s%5:s%0:s%6:s%0:s%7:s%0:s%8:s%0:s%9:s';
    const FMT_ROUTE_PATH:Core.Strings.VarString='%0:s%1:s%0:s%2:s%0:s%3:s%0:s%4:s%0:s%5:s%0:s%6:s%0:s%7:s%0:s%8:s';
    // 0 = '/' on unix
    // 1 = AuDisks
    // 2 = clusterID
    // 3 = resourceID
    // 4 = nodeID
    // 5 = kind
    // 6 = domainid
    // 7 = userid
    // 8 = folderID
    // 9 = fileID
    class function Add(Task:Core.Database.Types.TTask; DomainID,UseID,NodeID:QWord; Kind:LongInt; var Item:TItem):boolean;
    class function Verify(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Node:Storage.MatrixNodes.Node.Item; out Item:TItem):boolean; overload;
    class function Verify(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Node:Storage.MatrixNodes.Node.Item):boolean; overload;
    class function Verify(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out NodeID:QWord):boolean; overload;

    class function Allocate(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Node:Storage.MatrixNodes.Node.Item; out Item:TItem):boolean; overload;
    class function Allocate(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out NodeID:QWord):boolean; overload;

    class function Read(Task:Core.Database.Types.TTask; ItemID:QWord; var Item:TItem):boolean;
    class function Delete(Task:Core.Database.Types.TTask; ItemID:QWord):boolean;

    class function  toXML(var Item:TItem; Output:TMemoryStream; Header:boolean):boolean;
    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;

    class procedure Copy(Var Source,Dest:TItem); overload;

    class procedure Empty(Var Item:TItem); overload;
    class procedure Empty(Var Item:TItems); overload;

    class procedure Init(Var Item:TItem); overload;
    class procedure Init(Var Item:TItems); overload;

    class procedure Done(Var Item:TItem); overload;
    class procedure Done(Var Item:TItems); overload;
  end;
  Files=class
  public
    class function Create(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt):boolean; overload;
    class function Create(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:Core.Strings.VarString):boolean; overload;
    class function Create(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Var Data:Core.Arrays.Types.Bytes):boolean; overload;
    class function Create(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Var Data:TFileStream):boolean; overload;

    class function Copy(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Var NewID:QWord):boolean; overload;
    class function Copy(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID,NewFolderID:QWord; Kind:LongInt; Var NewID:QWord):boolean; overload;

    class function Read(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.Bytes):boolean; overload;
    class function Read(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Strings.VarString):boolean; overload;
    class function Read(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.VarString):boolean; overload;

    class function Exists(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt):boolean;

    class function Read(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Strings.VarString):boolean; overload;
    class function Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.Bytes):boolean; overload;
    class function Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.VarString):boolean; overload;
    class function Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:TFileStream):boolean; overload;

    class function Acquire(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:TFileStream):boolean; overload;

    class function Write(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.Bytes):boolean; overload;

    class function Write(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:Core.Strings.VarString):boolean; overload;
    class function Write(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:Core.Arrays.Types.Bytes):boolean; overload;
    class function Write(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Data:TStream):boolean; overload;

    class function Delete(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt):boolean; overload;
    class function Delete(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID:QWord; Kind:LongInt):boolean; overload;
  end;
  Quota=class
  type
    XML=class
    type
      Stanzas=class
      const
        Item                   = 'quota';
        Items                  = 'quotas';
      end;
      Fields=class
      const
        DomainID               = 'did';
        UseID                  = 'uid';
        Kind                   = 'kind';
        Consumption            = 'cnsp';
        Value                  = 'v';
        Limit                  = 'l';
        Price                  = 'c';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        DomainID                 : Core.Database.Types.Integer = 0;
        UseID                    : Core.Database.Types.Integer = 1;
        Kind                     : Core.Database.Types.Integer = 2;
        Limit                    : Core.Database.Types.Integer = 3;
        Consumption              : Core.Database.Types.Integer = 4;
      end;
      Keys=class
      const
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UseID                    : Core.Database.Types.VarString = 'IUID';
        Kind                     : Core.Database.Types.VarString = 'IKND';
        Limit                    : Core.Database.Types.VarString = 'ILIM';
        Consumption              : Core.Database.Types.VarString = 'ICTN';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit   : True;
        Group        : 'Clustering/Nodes';
        Name         : 'Quota';
        Value        : 'scs_mtx_quta';
        Hint         : 'Cloud disk quota system';
        PrimaryKeyP  : nil;
      );
      Fields: array [0..4] of Core.Database.Types.Field = (
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQword; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UseID; KeyP: @Keys.UseID; DataType: dftQword; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Limit; KeyP: @Keys.Limit; DataType: dftQword; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Consumption; KeyP: @Keys.Consumption; DataType: dftQword; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PItem=^TItem;
    TItem=record
      UseID                    : Qword;
      Kind                     : LongInt;
      Limit                    : QWord;
      Consumption              : QWord;
    end;
    Type
      Strata=class
      const
        Level1  : Storage.UserAccounts.Items.Cost=(Limit : 4*1000*1000*1000; Price: 0.0;);
        Level2  : Storage.UserAccounts.Items.Cost=(Limit : 8*1000*1000*1000; Price: 2.60;);
        Level3  : Storage.UserAccounts.Items.Cost=(Limit : 10*1000*1000*1000; Price: 3.25;);
        Level4  : Storage.UserAccounts.Items.Cost=(Limit : 12*1000*1000*1000; Price: 4.0;);
        Level5  : Storage.UserAccounts.Items.Cost=(Limit : 16*1000*1000*1000; Price: 5.0;);
        Level6  : Storage.UserAccounts.Items.Cost=(Limit : 32*1000*1000*1000; Price: 10.0;);
        Level7  : Storage.UserAccounts.Items.Cost=(Limit : 64*1000*1000*1000; Price: 17.0;);
        Level8  : Storage.UserAccounts.Items.Cost=(Limit : 128*1000*1000*1000; Price: 25.0;);
        Level9  : Storage.UserAccounts.Items.Cost=(Limit : 256*1000*1000*1000; Price: 50.0;);
        Level10 : Storage.UserAccounts.Items.Cost=(Limit : 512*1000*1000*1000; Price: 100.0;);
        Level11 : Storage.UserAccounts.Items.Cost=(Limit : 750*1000*1000*1000; Price: 110.0;);
        Level12 : Storage.UserAccounts.Items.Cost=(Limit : 1 * 1000*1000*1000*1000; Price: 150.0;);
        Level13 : Storage.UserAccounts.Items.Cost=(Limit : 2 * 1000*1000*1000*1000; Price: 300.0;);
        Level14 : Storage.UserAccounts.Items.Cost=(Limit : 3 * 1000*1000*1000*1000; Price: 450.0;);
    end;

    TItems=Array of PItem;
    PItems=^TItems;

    class function getLimit(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Value:QWord):boolean;
    class function setLimit(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; Value:QWord):boolean;

    class function Allowed(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; diskQuota,Value:QWord):boolean;

    class function incConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; diskQuota,Value:QWord):boolean;
    class function decConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; Value:QWord):boolean;

    class function getConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Value:QWord):boolean;
    class function getConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; out Value:QWord):boolean;
  end;


implementation
uses DB, sqldb;

procedure cbDestroyRouter(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Router.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Router.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Router.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Router.DB.TableP, useForCriteria, Router.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task,@Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;
  procedure PushUserDeleted;
  begin
    if ItemP = Router.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Router.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Router.DB.TableP, useForCriteria, Router.DB.IDs.UseID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task,@Commands);
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
  with Router.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyRouter, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class function  Router.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Empty(Item);
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.UseID:=toQWord(xItem,XML.Fields.UseID);
      Item.NodeID:=toQWord(xItem,XML.Fields.NodeID);
      Item.Kind:=toInteger(xItem,XML.Fields.Kind);
      Result:=True;
    end;
  end;
end;

class function  Router.toXML(var Item:TItem; Output:TMemoryStream; Header:boolean):boolean;
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
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.UseID,Item.UseID),Output);
    Core.Streams.Write(Print(XML.Fields.NodeID,Item.NodeID),Output);
    Core.Streams.Write(Print(XML.Fields.Kind,Item.Kind),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class procedure Router.Copy(Var Source,Dest:TItem);
begin
  Dest.ID:=Source.ID;
  Dest.UseID:=Source.UseID;
  Dest.NodeID:=Source.NodeID;
  Dest.Kind:=Source.Kind;
  Dest.Enabled:=Source.Enabled;
  Dest.Live:=Source.Live;
end;

class procedure Router.Empty(Var Item:TItem);
begin
  Item.ID:=0;
  Item.UseID:=0;
  Item.NodeID:=0;
  Item.Kind:=0;
  Item.Enabled:=False;
  Item.Live:=False;
end;

class procedure Router.Empty(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Router.Init(Var Item:TItem);
begin
  Item.ID:=0;
  Item.UseID:=0;
  Item.NodeID:=0;
  Item.Kind:=0;
  Item.Enabled:=False;
  Item.Live:=false;
end;

class procedure Router.Init(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Router.Done(Var Item:TItem);
begin
  Finalize(Item);
end;

class procedure Router.Done(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure cbReadRouter(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Router.PItem;
begin
  ItmP:=DataP;
  With Router.DB.Keys do begin
    ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
    ItmP^.UseID:=Fields.FieldByName(UseID).AsLargeInt;
    ItmP^.NodeID:=Fields.FieldByName(NodeID).AsLargeInt;
    ItmP^.Kind:=Fields.FieldByName(Kind).AsInteger;
    ItmP^.Enabled:=Fields.FieldByName(Enabled).AsBoolean;
    ItmP^.Live:=Fields.FieldByName(Live).AsBoolean;
  end;
end;


class function Router.Add(Task:Core.Database.Types.TTask; DomainID,UseID,NodeID:QWord; Kind:LongInt; var Item:TItem):boolean;
var
  Commands:Core.Database.Types.Commands;
  iInsertID:QWord;
  iCount:LongInt;
  iReset:QWord;
begin
  iInsertID:=Random(High(Integer)); iReset:=0; iCount:=0;
  Empty(Item);
  Item.UseID:=UseID;
  Item.NodeID:=NodeID;
  Item.Kind:=Kind;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.UseID,poNone,oNone,Item.UseID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.NodeID,poNone,oNone,Item.NodeID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Kind,poNone,oNone,Item.Kind,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Enabled,poNone,oNone,Item.Enabled,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Live,poNone,oNone,Item.Live,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Router.Allocate(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Node:Storage.MatrixNodes.Node.Item; out Item:TItem):boolean;
var
  Nodes:Storage.MatrixNodes.Node.Items;
  Length,idx:LongInt;
begin
  Result:=false;
  Storage.MatrixNodes.Node.Empty(Node);
  Storage.MatrixNodes.Node.DB.Disks(Task,Nodes);
  try
    Length:=System.Length(Nodes);
    if Length>0 then begin
      // todo predictive model based on usage and quota
      Result:=True;
      idx:=Random(Length);
      Storage.MatrixNodes.Node.Copy(Nodes[idx]^,Node);
    end;
  finally
    Storage.MatrixNodes.Node.Done(Nodes);
  end;
  Result:=Router.Add(Task,DomainID,UseID,Node.ID,Kind,Item);
end;

class function Router.Allocate(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out NodeID:QWord):boolean;
var
  Node:Storage.MatrixNodes.Node.Item;
  Item:TItem;
begin
  Result:=false;
  NodeID:=0;
  Storage.MatrixNodes.Node.Init(Node);
  Init(Item);
  Try
    Result:=Allocate(Task,DomainID,UseID,Kind,Node,Item);
    if Result then
      NodeID:=Node.ID;
  finally
    Storage.MatrixNodes.Node.Done(Node);
    Done(Item);
  end;
end;

class function Router.Verify(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Node:Storage.MatrixNodes.Node.Item; out Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Item.Kind:=Kind;
    Item.UseID:=UseID;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    {$i Storage.AuraDisks.Router.Read.Fields.inc}

    if Core.Database.SQL.Select(Task, @Commands, @cbReadRouter, @Item) then begin
      if Item.ID=0 then begin
        // Allocate which node the user will be tied to for storage
        Router.Allocate(Task,DomainID,UseID,Kind,Node,Item);
      end else
        Storage.MatrixNodes.Node.DB.Fill(Task,Item.NodeID,Node);
      Result:=Item.ID<>0;
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Router.Verify(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Node:Storage.MatrixNodes.Node.Item):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Route:TItem;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Route);
    Route.Kind:=Kind;
    Route.UseID:=UseID;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    {$i Storage.AuraDisks.Router.Read.Fields.inc}

    if Core.Database.SQL.Select(Task, @Commands, @cbReadRouter, @Route) then begin
      if Route.ID=0 then begin
        // Allocate which node the user will be tied to for storage
        Router.Allocate(Task,DomainID,UseID,Kind,Node,Route);
      end else
        Storage.MatrixNodes.Node.DB.Fill(Task,Route.NodeID,Node);
      Result:=Route.ID<>0;
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Router.Verify(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out NodeID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Route:TItem;
  Node:Storage.MatrixNodes.Node.Item;
begin
  Result:=False;  NodeID:=0;
  Storage.MatrixNodes.Node.Init(Node);
  Init(Route);
  Try
    Result:=Verify(Task,DomainID,UseID,Kind,Node);
    if Result then
      NodeID:=Node.ID;
  finally
    Storage.MatrixNodes.Node.Done(Node);
    Done(Route);
  end;
end;

class function Router.Read(Task:Core.Database.Types.TTask; ItemID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);

    {$i Storage.AuraDisks.Router.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadRouter, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Router.Delete(Task:Core.Database.Types.TTask; ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);
    Result := Core.Database.SQL.Delete(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Files.Create(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,            // 0
      Router.AuDisks,                // 1
      IntToStr(Node.ClusterID),      // 2
      IntToStr(Node.ResourceID),     // 3
      IntToStr(Node.ID),             // 4
      IntToStr(Kind),                // 5
      IntToStr(DomainID),            // 6
      IntToStr(UseID),               // 7
      IntToStr(FolderID)             // 8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  FS:=TFileStream.Create(sPath,fmCreate or fmShareDenyNone);
  try
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Create(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:Core.Strings.VarString):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID)         // 8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  FS:=TFileStream.Create(sPath,fmCreate or fmShareDenyNone);
  try
    Core.Streams.Copy(Data,FS);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Create(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Var Data:Core.Arrays.Types.Bytes):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID)         // 8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  FS:=TFileStream.Create(sPath,fmCreate or fmShareDenyNone);
  try
    iLength:=System.Length(Data);
    if iLength>0 then
      FS.Write(Data[0],iLength);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Create(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Var Data:TFileStream):boolean;
var
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        //0
      Router.AuDisks,            //1
      IntToStr(Node.ClusterID),  //2
      IntToStr(Node.ResourceID), //3
      IntToStr(Node.ID),         //4
      IntToStr(Kind),            //5
      IntToStr(DomainID),        //6
      IntToStr(UseID),           //7
      IntToStr(FolderID)         //8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  Data:=TFileStream.Create(sPath,fmCreate or fmShareDenyNone);
  Result:=True;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Copy(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Var NewID:QWord):boolean;
var
  Source:TFileStream;
  Dest:TFileStream;
begin
  Source:=nil; Dest:=nil;
  Result:=Acquire(Node,DomainID,UseID,FolderID,FileID,Kind,Source);
  if Result then begin
    Result:=Create(Node,DomainID,UseID,FolderID,NewID,Kind,Dest);
    if Result then
      Core.Streams.Copy(Source,Dest);
  end;
  FreeAndNil(Source);
  FreeAndNil(Dest);
end;

class function Files.Copy(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID,NewFolderID:QWord; Kind:LongInt; Var NewID:QWord):boolean;
var
  Source:TFileStream;
  Dest:TFileStream;
begin
  Source:=nil; Dest:=nil;
  Result:=Acquire(Node,DomainID,UseID,FolderID,FileID,Kind,Source);
  if Result then begin
    Result:=Create(Node,DomainID,UseID,NewFolderID,NewID,Kind,Dest);
    if Result then
      Core.Streams.Copy(Source,Dest);
  end;
  FreeAndNil(Source);
  FreeAndNil(Dest);
end;

class function Files.Read(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.Bytes):boolean;
var
  Route:Router.TItem;
  Node:Storage.MatrixNodes.Node.Item;
begin
  Result:=Router.Verify(Task,DomainID,UseID,Kind,Node,Route);
  if Result then begin
    Storage.MatrixNodes.Node.DB.Fill(Task,Node.ID,Node);
    Result:=Read(Node,DomainID,UseID,FolderID,FileID,Kind,Data);
  end;
end;

class function Files.Read(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.VarString):boolean;
var
  Route:Router.TItem;
  Node:Storage.MatrixNodes.Node.Item;
begin
  Result:=Router.Verify(Task,DomainID,UseID,Kind,Node,Route);
  if Result then begin
    Storage.MatrixNodes.Node.DB.Fill(Task,Node.ID,Node);
    Result:=Read(Node,DomainID,UseID,FolderID,FileID,Kind,Data);
  end;
end;


class function Files.Read(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Strings.VarString):boolean;
var
  Route:Router.TItem;
  Node:Storage.MatrixNodes.Node.Item;
begin
  Result:=Router.Verify(Task,DomainID,UseID,Kind,Node,Route);
  if Result then begin
    Storage.MatrixNodes.Node.DB.Fill(Task,Node.ID,Node);
    Result:=Read(Node,DomainID,UseID,FolderID,FileID,Kind,Data);
  end;
end;

class function Files.Exists(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt):boolean;
var
  sPath:Core.Strings.VarString;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_FILE,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID),        // 8
      IntToSTr(FileID)           // 9
    ]
  );
  Result:=FileExists(sPath);
end;


class function Files.Read(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Strings.VarString):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_FILE,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID),        // 8
      IntToSTr(FileID)           // 9
    ]
  );
  FS:=TFileStream.Create(sPath,fmOpenRead or fmShareDenyNone);
  try
    Result:=true;
    iLength:=FS.Size;
    SetLength(Data,iLength);
    if (iLength>0) then
      FS.Read(Data[1],iLength);
  finally
    FreeAndNil(FS);
  end;
end;

class function Files.Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.Bytes):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_FILE,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID),        // 8
      IntToSTr(FileID)           // 9
    ]
  );

  FS:=TFileStream.Create(sPath,fmOpenRead or fmShareDenyNone);
  try
    iLength:=FS.Size;
    SetLength(Data,iLength);
    if iLength>0 then
      FS.Read(Data[0],iLength);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
end;

class function Files.Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.VarString):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_FILE,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID),        // 8
      IntToSTr(FileID)           // 9
    ]
  );
  FS:=TFileStream.Create(sPath,fmOpenRead or fmShareDenyNone);
  try
    Core.Arrays.VarString.fromStream(Data,FS);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
end;

class function Files.Read(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:TFileStream):boolean;
var
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_FILE,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID),        // 8
      IntToSTr(FileID)           // 9
    ]
  );
  Data:=TFileStream.Create(sPath,fmOpenRead or fmShareDenyNone);
  Data.Position:=0;
  Result:=True;
end;

class function Files.Acquire(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:TFileStream):boolean;
var
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,            // 0
      Router.AuDisks,                // 1
      IntToStr(Node.ClusterID),      // 2
      IntToStr(Node.ResourceID),     // 3
      IntToStr(Node.ID),             // 4
      IntToStr(Kind),                // 5
      IntToStr(DomainID),            // 6
      IntToStr(UseID),               // 7
      IntToStr(FolderID)             // 8
    ]
  );
  if (Node.ID = 0 ) or (Node.ResourceID=0) or (Node.ClusterID=0) then
    raise Exception.Create(Format(FMT_FORCE_PATH_NULL_NODE,[sPath]));

  if Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode) then begin
    sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
    if FileExists(sPath) then begin
      Data:=TFileStream.Create(sPath,fmOpenReadWrite or fmShareDenyNone);
      Data.Position:=0;
      Result:=True;
    end else begin
      Data:=TFileStream.Create(sPath,fmCreate or fmShareDenyNone);
      Result:=True;
    end;
  end else begin
     raise Exception.Create(Format(FMT_FORCE_PATH_FAILURE,[sPath]));
  end;
end;

class function Files.Write(Task:Core.Database.Types.TTask; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; out Data:Core.Arrays.Types.Bytes):boolean;
var
  Route:Router.TItem;
  Node:Storage.MatrixNodes.Node.Item;
begin
  Result:=Router.Verify(Task,DomainID,UseID,Kind,Node,Route);
  if Result then begin
    Storage.MatrixNodes.Node.DB.Fill(Task,Node.ID,Node);
    Result:=Write(Node,DomainID,UseID,FolderID,FileID,Kind,Data);
  end;
end;

class function Files.Write(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:Core.Strings.VarString):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID)         // 8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  FS:=TFileStream.Create(sPath,fmOpenRead or fmOpenWrite or fmShareDenyNone);
  try
    iLength:=System.Length(Data);
    FS.Size:=0;
    if iLength>0 then
      FS.Write(Data[1],iLength);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Write(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; var Data:Core.Arrays.Types.Bytes):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID)         // 8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  FS:=TFileStream.Create(sPath,fmOpenRead or fmOpenWrite or fmShareDenyNone);
  try
    iLength:=System.Length(Data);
    FS.Size:=0;
    if iLength>0 then
      FS.Write(Data[0],iLength);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Write(Var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord; Kind:LongInt; Data:TStream):boolean;
var
  FS:TFileStream;
  sPath:Core.Strings.VarString;
  iLength:QWord;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID)         // 8
    ]
  );
  Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.FolderMode);
  sPath:=Concat(sPath,SysUtils.PathDelim,IntToStr(FileID));
  FS:=TFileStream.Create(sPath,fmOpenRead or fmOpenWrite or fmShareDenyNone);
  try
    Data.Position:=0;
    FS.Size:=Data.Size;
    FS.CopyFrom(Data,Data.Size);
    Result:=True;
  finally
    FreeAndNil(FS);
  end;
  Core.Utils.Files.SetPermissions(sPath,Process.UserID,Process.GroupID,Process.FileMode);
end;

class function Files.Delete(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID,FileID:QWord;Kind:LongInt):boolean;
var
  hIO:THandle;
  sPath:Core.Strings.VarString;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_FILE,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID),        // 8
      IntToSTr(FileID)           // 9
    ]
  );
  SysUtils.DeleteFile(sPath);
  Result:=True;
end;

class function Files.Delete(var Node:Storage.MatrixNodes.Node.Item; DomainID,UseID,FolderID:QWord; Kind:LongInt):boolean;
var
  sPath:Core.Strings.VarString;
begin
  Result:=false;
  sPath:=Format(
    Router.FMT_ROUTE_PATH,
    [
      SysUtils.PathDelim,        // 0
      Router.AuDisks,            // 1
      IntToStr(Node.ClusterID),  // 2
      IntToStr(Node.ResourceID), // 3
      IntToStr(Node.ID),         // 4
      IntToStr(Kind),            // 5
      IntToStr(DomainID),        // 6
      IntToStr(UseID),           // 7
      IntToStr(FolderID)         // 8
    ]
  );
  SysUtils.RemoveDir(sPath);
  Result:=True;
end;

procedure cbGetLimit(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Quota.DB.Keys.Limit).AsLargeInt;
end;

class function Quota.getLimit(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Value:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:=0;
    Value:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Limit,poNone,oNone,Commands);

    Result := Core.Database.SQL.Sum(Task,@Commands, Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Quota.setLimit(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; Value:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:= 0;
    Value:=0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForIncrement,DB.IDs.Limit,poNone,oNone,Value,Commands);

    Result := Core.Database.SQL.Update(Task,@Commands);

  finally
    Core.Database.Done(Commands);
  end;
end;


class function Quota.Allowed(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; diskQuota,Value:QWord):boolean;
var
  qFut:QWord;
  qValue:QWord;
  qLimit:QWord;
begin
  Result:=false;
  qValue:=0;
  qLimit:=0;
  if Quota.getConsumption(Task,DomainID,UseID,Kind,qValue) then begin
    if Quota.getLimit(Task,DomainID,UseID,Kind,qLimit) then begin
      qFut:=qValue+Value;
      Result:=qFut<diskQuota;
      if Result and (qLimit>0) then
        Result:=(qFut<qLimit);
    end;
  end;
end;

class function Quota.incConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; diskQuota,Value:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:= 0;
    Value:=0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForIncrement,DB.IDs.Consumption,poNone,oNone,Value,Commands);

    Result := Core.Database.SQL.Update(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Quota.decConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; Value:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:= 0;
    Value:=0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForDecrement,DB.IDs.Consumption,poNone,oNone,Value,Commands);

    Result := Core.Database.SQL.Update(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetConsumption(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Quota.DB.Keys.Consumption).AsLargeInt;
end;

class function Quota.getConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; Kind:LongInt; out Value:Qword):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:= 0;
    Value:=0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Consumption,poNone,oNone,Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbGetConsumption, @Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Quota.getConsumption(Task:Core.Database.Types.TTask; DomainID,UseID:QWord; out Value:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:=0;
    Value:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UseID, poAnd, oEqual, UseID, Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Consumption,poNone,oNone,Commands);

    Result := Core.Database.SQL.Sum(Task,@Commands, Value);
  finally
    Core.Database.Done(Commands);
  end;
end;


initialization
  RegisterDB;
end.


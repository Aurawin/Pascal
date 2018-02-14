unit Storage.MatrixQueue;
{
  unit Storage.MatrixQueue.pas

  Matrix Queue Database Module

  DBMS facilities for Queuing of Operations
  for RSR Servers

  Copyright Aurawin LLC 2014
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

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.Strings,
  Core.Streams,
  Core.Streams.Types,
  Core.Timer,
  Core.XML,

  RSR.Core,

  Storage.Types,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.MatrixNodes,
  Storage.CoreObjects,

  DOM,
  XMLRead;

Type
  Items = class
  type
    Kind=class
    const
      None                       = 0; // DO NOT MODIFY
      SMTP                       = 1; // DO NOT MODIFY
    end;
    Defaults=class
    const
      Trys                       : LongInt = 0;
      Inc                        : LongInt = 1;
      Limit                      : LongInt = 1;
      Unlocked                   : boolean = false;
      Locked                     : boolean = true;
    end;
    Item=record
      ID                         : QWord;
      NodeID                     : QWord;
      DiskID                     : QWord;
      Kind                       : QWord;
      TTL                        : Double;
      Trys                       : LongInt;
      Locked                     : Boolean;
      Verified                   : Boolean;
      Meta                       : Core.Strings.VarString;
    end;
    PItem=^Item;
    DB=class
    Type
      IDs= class
      const
        ID                         : Core.Database.Types.Integer = 0;
        InsertID                   : Core.Database.Types.Integer = 1;
        NodeID                     : Core.Database.Types.Integer = 2;
        DiskID                     : Core.Database.Types.Integer = 3;
        Kind                       : Core.Database.Types.Integer = 4;
        TTL                        : Core.Database.Types.Integer = 5;
        Trys                       : Core.Database.Types.Integer = 6;
        Locked                     : Core.Database.Types.Integer = 7;
        Meta                       : Core.Database.Types.Integer = 8;
        Limit                      : Core.Database.Types.Integer = 9;
      end;
      Keys=class
      const
        ID                         : Core.Database.Types.VarString = 'ITMID';
        InsertID                   : Core.Database.Types.VarString = 'ITMIID';
        NodeID                     : Core.Database.Types.VarString = 'ITMNID';
        DiskID                     : Core.Database.Types.VarString = 'ITMDID';
        Kind                       : Core.Database.Types.VarString = 'ITMK';
        TTL                        : Core.Database.Types.VarString = 'ITMTTL';
        Trys                       : Core.Database.Types.VarString = 'ITMTRY';
        Locked                     : Core.Database.Types.VarString = 'ITMLKD';
        Meta                       : Core.Database.Types.VarString = 'ITMMD';
        Limit                      : Core.Database.Types.VarString = 'ITMIT';
      end;
      const
        TableP                     : Core.Database.Types.PTable = nil;
        MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
        Startup                    : Core.Database.Types.TableIni = (
          AutoCreate               : True;
          AutoCommit               : True;
          Group                    : 'Clustering';
          Name                     : 'Queue';
          Value                    : 'scs_mtx_que';
          Hint                     : 'Storage for Matrix based queue items';
          PrimaryKeyP              : @Keys.ID;
        );
        Fields: array [0..9] of Core.Database.Types.Field = (
          (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
          (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.NodeID; KeyP: @Keys.NodeID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.DiskID; KeyP: @Keys.DiskID;  DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
          (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.TTL; KeyP: @Keys.TTL;  DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
          (IDP: @IDs.Trys; KeyP: @Keys.Trys;  DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Locked; KeyP: @Keys.Locked; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.Meta; KeyP: @Keys.Meta; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Limit;  KeyP: @Keys.Limit; DataType: dftInteger; AutoCreate: false; Verified: true; Precision: 0; Flags: cfNone; )
        );

      class function Add(Task:Core.Database.Types.TTask; var Disk:Storage.MatrixNodes.Node.Item; NodeID,aKind:Qword; var TTL:Double; var Meta,Data:Core.Strings.VarString; out ID:QWord):boolean;
      class function Unlock(Task:Core.Database.Types.TTask; var ID:QWord):boolean;
      class function IncTry(Task:Core.Database.Types.TTask; var ID:QWord; Count:LongInt=1):boolean;
      class function Fail(Task:Core.Database.Types.TTask; var ID,NodeID:QWord; var TTL:Double; var Meta:Core.Strings.VarString):boolean;
      class function Delete(Task:Core.Database.Types.TTask; var Disk:Storage.MatrixNodes.Node.Item; var ID:QWord):boolean;
      class function Get(Task:Core.Database.Types.TTask; var NodeID,aKind:QWord; var Entry:Item):boolean;
    end;
    class procedure Empty(var Entry:Item);
    class procedure Init(var Entry:Item);
    class procedure Done(var Entry:Item);
 end;

implementation

uses
  DB,
  sqldb,
  Storage.AuraDisks;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
begin
  Result := False;
  // ToDo.
  // 1 Delete  if User/Domain is Deleted.
  // 2 Reroute if Cluster/Resource/Node is/are deleted
end;

procedure RegisterDB;
var
  iLcv:LongInt;
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

class function Items.DB.Add(Task:Core.Database.Types.TTask; var Disk:Storage.MatrixNodes.Node.Item; NodeID,aKind:Qword; var TTL:Double; var Meta,Data:Core.Strings.VarString; out ID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iLength,iReset: QWord;
  iInsertID: QWord;
  FS:TFileStream;
begin
  Result := False;
  iCount := 0;
  iReset := 0;
  ID:=0;
  iLength:=Length(Data);
  iInsertID := Random(High(LongInt));
  Try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, IDs.ID, poNone, oNone, ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, IDs.InsertID, poNone, oNone, iReset, Commands);
    {$i Storage.MatrixQueue.Insert.Fields.inc}
    Result := Core.Database.SQL.Insert(Task, @Commands);
    if (ID<>0) then begin
      if Storage.AuraDisks.Files.Create(Disk,Domain.Global,Use.Global,Folder.Queue,ID,Kinds.NOSQL,FS) then begin
        Try
          Core.Streams.Append(Data,iLength,FS);

          iCount := 0;
          Core.Database.AddCommand(iCount, TableP,@Commands);
          Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ID, Commands);
          Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Locked, poNone, oNone, Defaults.Unlocked, Commands);
          Result := Core.Database.SQL.Update(Task, @Commands);

          Result:=True;

        finally
          FS.Free();
        end;
      end;
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Unlock(Task:Core.Database.Types.TTask; var ID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Locked, poNone, oNone, Defaults.Unlocked, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; var Disk:Storage.MatrixNodes.Node.Item; var ID:QWord):boolean;
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

    Storage.AuraDisks.Files.Delete(Disk,Domain.Global,Use.Global,Folder.Queue,ID,Kinds.NOSQL);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.IncTry(Task:Core.Database.Types.TTask; var ID:QWord; Count:LongInt=1):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount:= 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForIncrement, IDs.Trys, poNone, oNone, Count, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Fail(Task:Core.Database.Types.TTask; var ID,NodeID:QWord; var TTL:Double; var Meta:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  //  IncTry(Module,Task,ID);
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForIncrement, IDs.Trys, poNone, oNone, Defaults.Inc, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Locked, poNone, oNone, Defaults.Unlocked, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.TTL, poNone, oNone, TTL, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.NodeID, poNone, oNone, NodeID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Meta, poNone, oNone, Meta, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadItem(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  itmP:Items.PItem;
begin
  itmP:=DataP;
  itmP^.Verified    := true;
  itmP^.ID          := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  itmP^.NodeID      := Fields.FieldByName(Items.DB.Keys.NodeID).AsLargeInt;
  itmP^.DiskID      := Fields.FieldByName(Items.DB.Keys.DiskID).AsLargeInt;
  itmP^.Kind        := Fields.FieldByName(Items.DB.Keys.Kind).AsInteger;
  itmP^.Trys        := Fields.FieldByName(Items.DB.Keys.Trys).AsInteger;
  itmP^.TTL         := Fields.FieldByName(Items.DB.Keys.TTL).AsFloat;
  itmP^.Locked      := Fields.FieldByName(Items.DB.Keys.Locked).AsBoolean;
  itmP^.Meta        := Fields.FieldByName(Items.DB.Keys.Meta).AsString;
end;

class function Items.DB.Get(Task:Core.Database.Types.TTask; var NodeID,aKind:QWord; var Entry:Item):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=false;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NodeID, poNone, oEqual, NodeID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Kind, poAnd, oEqual, aKind, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Locked, poAnd, oEqual, Defaults.Unlocked, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.TTL, poAnd, oLessThan, Core.Timer.dtUT, Commands);

    Core.Database.AddCommand(iCount, TableP, useForLimit, IDs.Limit, poNone, oNone, Defaults.Limit, Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NodeID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DiskID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Trys,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.TTL,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Locked,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Meta,poNone,oNone,Commands);

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadItem, @Entry) and Entry.Verified);
    if (Entry.Verified) then begin
      iCount:= 0;
      Core.Database.AddCommand(iCount, TableP,@Commands);
      Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
      Core.Database.AddCommand(iCount, TableP, useForIncrement, IDs.Trys, poNone, oNone, Defaults.Inc, Commands);
      Core.Database.AddCommand(iCount, TableP, useForUpdates, IDs.Locked, poNone, oNone, Defaults.Locked, Commands);
      Result := Core.Database.SQL.Update(Task, @Commands);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Items.Empty(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    NodeID:=0;
    Kind:=0;
    TTL:=0;
    Trys:=0;
    Locked:=Defaults.Unlocked;
    Verified:=false;
    SetLength(Meta,0);
  end;
end;

class procedure Items.Init(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    NodeID:=0;
    Kind:=0;
    TTL:=0;
    Trys:=0;
    Locked:=Defaults.Unlocked;
    Verified:=false;
    SetLength(Meta,0);
  end;
end;

class procedure Items.Done(var Entry:Item);
begin
  Finalize(Entry.Meta,0);
  Finalize(Entry);
end;

initialization
  RegisterDB;
end.


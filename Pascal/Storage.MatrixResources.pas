{
unit Storage.MatrixResources.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html


Matrix Resources Table

Matrix Resources Table for describing computer assets either
virtual or hardware based

}
unit Storage.MatrixResources;



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
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Strings
;



Type
  Resource=class
  type
    Item=record
      ID                         : QWord;
      ClusterID                  : QWord;
      Verified                   : Boolean;
      Name                       : Core.Strings.VarString;
    end;
    PItem=^Item;
    Items=Array of PItem;
    PItems=^Items;
    RemoveOptions=(roUnverified);
    DB=class
    Type
      IDS=class
      const
        ID                       : LongInt= 0;
        InsertID                 : LongInt= 1;
        ClusterID                : LongInt= 2;
        Name                     : LongInt= 3;
      end;
      Keys=class
      const
        ID                       : Core.Strings.VarString = 'MRID';
        InsertID                 : Core.Strings.VarString = 'MRIID';
        ClusterID                : Core.Strings.VarString = 'MRCID';
        Name                     : Core.Strings.VarString = 'MRN';
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Clustering';
        Name                 : 'Resources';
        Value                : 'scs_mtx_res';
        Hint                 : 'Storage for matrix based resource information for groups of nodes.';
        PrimaryKeyP          : @Keys.ID;
      );
      Fields: array [0..3] of Core.Database.Types.Field= (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.ClusterID; KeyP: @Keys.ClusterID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone)
      );
      class Function Exists(Task:Core.Database.Types.TTask; ClusterID:System.QWord; sName:Core.Strings.VarString):System.Boolean; overload;
      class Function Exists(Task:Core.Database.Types.TTask; ResourceID:System.QWord):System.Boolean; overload;
      class Function Create(Task:Core.Database.Types.TTask; Var Entry:Resource.Item):Boolean;
      class Function Delete(Task:Core.Database.Types.TTask; Var Entry:Resource.Item):Boolean;
      class Function Fill(Task:Core.Database.Types.TTask; ID:System.QWord; out Entry:Resource.Item):System.Boolean;
      class Function Fill(Task:Core.Database.Types.TTask; ClusterID:System.QWord; sAlias:Core.Strings.VarString; out Entry:Resource.Item):System.Boolean;
      class Function Write(Task:Core.Database.Types.TTask; Var Entry:Resource.Item):System.Boolean;

      class Function List(Task:Core.Database.Types.TTask; ClusterID:System.QWord; var Entries:Resource.Items):System.Boolean; overload;
      class Function List(Task:Core.Database.Types.TTask; ClusterID:System.QWord; var Entries:Core.Arrays.Types.VarString):System.Boolean; overload;
      class Function List(Task:Core.Database.Types.TTask; ClusterID:System.QWord; var Entries:Core.Arrays.Types.KeyStrings):System.Boolean; overload;
    end;



    class procedure Copy(Var Source,Destination:Resource.Item); overload;

    class procedure Empty(Var Entry:Resource.Item); overload;
    class procedure Empty(Var Entry:Resource.Items); overload;

    class procedure Init(Var Entry:Resource.Item); overload;
    class procedure Init(Var Entry:Resource.Items); overload;
    class procedure Verify(Var Entry:Resource.Items; Value:Boolean); overload;

    class procedure Remove(Var Entry:Resource.Items; Const Options:RemoveOptions=roUnverified); overload;
    class procedure Remove(var Entry:PItem;Var Entries:Resource.Items); overload;

    class procedure Done(Var Entry:Resource.Item); overload;
    class procedure Done(Var Entry:Resource.Items); overload;

    class function  IndexOf(Var Entries:Resource.Items; var ID:System.QWord): LongInt; overload;

  end;

implementation
uses db;


procedure cbDestroyResource(ItemP: Core.Database.Monitor.Types.PItem);
begin
  Done(Resource.DB.TableP^);
  Resource.DB.TableP:=nil;
  Resource.DB.MonitorP:=nil;
end;

function cbDBMonitorNotifyied(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:System.Cardinal):System.Boolean;

  procedure PushClusterDeleted;
  var
    iCount                         : LongInt;
    Commands                       : Core.Database.Types.Commands;
  begin
    if ItemP=Resource.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Resource.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Resource.DB.TableP,useForCriteria,Resource.DB.IDS.ClusterID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushResourceDeleted;
  var
    iCount                         : LongInt;
    Commands                       : Core.Database.Types.Commands;
  begin
    if ItemP=Resource.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Resource.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Resource.DB.TableP,useForCriteria,Resource.DB.IDS.ID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.CLUSTER_DELETED           : PushClusterDeleted();
    Core.Database.Monitor.Notify.CLUSTER_RESOURCE_DELETED  : PushResourceDeleted();
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  if Resource.DB.TableP = nil then begin
    New(Resource.DB.TableP);
    Init(Resource.DB.TableP^, Resource.DB.Startup);
    for iLcv := 0 to High(Resource.DB.Fields) do
      Core.Database.AddField(@Resource.DB.Fields[iLcv], Resource.DB.TableP);
    if Resource.DB.MonitorP = nil then begin
      New(Resource.DB.MonitorP);
      Init(Resource.DB.MonitorP^, Resource.DB.TableP^, @cbDestroyResource, @cbDBMonitorNotifyied);
      Core.Database.Monitor.Add(Resource.DB.MonitorP);
    end;
  end;
end;


procedure CB_MatrixResource_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
 ItemP:Resource.PItem;
begin
  ItemP:=Resource.PItem(DataP);
  {$i Storage.MatrixResources.Resource.Fill.inc}
end;

class Function  Resource.DB.Fill(Task:Core.Database.Types.TTask; ClusterID:System.QWord; sAlias:Core.Strings.VarString; out Entry:Resource.Item):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0; Empty(Entry);
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.Name,poNone,oEqual,sAlias,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ClusterID,poAnd,oEqual,ClusterID,Commands);
    {$i Storage.MatrixResources.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_MatrixResource_Fill,@Entry) and (Entry.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Resource.DB.Fill(Task:Core.Database.Types.TTask; ID:System.QWord; out Entry:Resource.Item):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0; Empty(Entry);
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ID,poNone,oEqual,ID,Commands);
    {$i Storage.MatrixResources.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_MatrixResource_Fill,@Entry) and (Entry.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Resource.DB.Exists(Task:Core.Database.Types.TTask; ClusterID:System.QWord; sName:Core.Strings.VarString):System.Boolean;
var
  iCount                         : LongInt;
  Count                          : Int64;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.Name,poNone,oEqual,sName,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ClusterID,poAnd,oEqual,ClusterID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  finally
    Core.Database.Done(Commands);
  end;
end;

class Function Resource.DB.Exists(Task:Core.Database.Types.TTask; ResourceID:System.QWord):System.Boolean;
var
  iCount                         : LongInt;
  Count                          : int64;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ID,poNone,oEqual,ResourceID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_MatrixResource_List_KPL(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.KeyString.Add(DataP,Fields.FieldByName(Resource.DB.Keys.ID).AsString,Fields.FieldByName(Resource.DB.Keys.Name).AsString);
end;

class Function  Resource.DB.List(Task:Core.Database.Types.TTask; ClusterID:System.QWord; Var Entries:Core.Arrays.Types.KeyStrings):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Arrays.KeyString.Empty(Entries);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDS.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDS.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_MatrixResource_List_KPL,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_MatrixResource_List_SA(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.VarString.Add(DataP,Fields.FieldByName(Resource.DB.Keys.Name).AsString);
end;

class Function  Resource.DB.List(Task:Core.Database.Types.TTask; ClusterID:System.QWord; Var Entries:Core.Arrays.Types.VarString):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Arrays.VarString.Empty(Entries);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDS.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_MatrixResource_List_SA,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_MatrixResource_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP:Resource.PItems;
  ItemP:Resource.PItem;
  iID:System.QWord;
  iIndex:LongInt;
begin
  ListP:=DataP;
  iID:=Fields.FieldByName(Resource.DB.Keys.ID).AsLargeInt;
  iIndex:=Resource.IndexOf(ListP^,iID);
  if iIndex=-1 then begin
    iIndex:=Length(ListP^);
    SetLength(ListP^,iIndex+1);
    New(ItemP);
    Resource.Init(ItemP^);
    ItemP^.ID:=iID;
    ListP^[iIndex]:=ItemP;
  end else
    ItemP:=@ListP^[iIndex];
  {$i Storage.MatrixResources.Resource.Fill.inc}
end;

class Function  Resource.DB.List(Task:Core.Database.Types.TTask; ClusterID:System.QWord; Var Entries:Resource.Items):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Resource.Verify(Entries,false);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ClusterID,poNone,oEqual,ClusterID,Commands);
    {$i Storage.MatrixResources.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_MatrixResource_List,@Entries);
    Resource.Remove(Entries,roUnVerified);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Resource.DB.Create(Task:Core.Database.Types.TTask; Var Entry:Resource.Item):System.Boolean;
var
  iCount                         : LongInt;
  InsertID,iReset                : System.QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; InsertID:=Random(High(Integer)); Entry.ID:=0;
    // Set Table
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDS.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDS.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDS.InsertID,poNone,oNone,iReset,Commands);
    // Add attributes
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDS.ClusterID,poNone,oNone,Entry.ClusterID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDS.Name,poNone,oNone,Entry.Name,Commands);
    // Insert Into Database
    Result:=(Core.Database.SQL.Insert(Task,@Commands)) and (Entry.ID<>0);
  finally
    Core.Database.Done(Commands);
  end;

end;

class Function  Resource.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Resource.Item): System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Resource.DB.Write(Task:Core.Database.Types.TTask; Var Entry:Resource.Item):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDS.Name,poNone,oNone,Entry.Name,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Resource.Copy(Var Source,Destination:Resource.Item);
begin
  Destination.ClusterID:=Source.ClusterID;
  Destination.ID:=Source.ID;
  Destination.Name:=Source.Name;
  Destination.Verified:=Source.Verified;
end;

class procedure Resource.Empty(Var Entry:Resource.Item);
begin
  Entry.ClusterID:=0;
  Entry.ID:=0;
  SetLength(Entry.Name,0);
end;

class procedure Resource.Empty(Var Entry:Resource.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entry) do begin
    if Entry[iLcv]<>nil then begin
      Done(Entry[iLcv]^);
      Dispose(Entry[iLcv]);
    end;
  end;
  SetLength(Entry,0);
end;

class procedure Resource.Init(Var Entry:Resource.Item);
begin
  Entry.ID:=0;
  Entry.ClusterID:=0;
  Entry.Verified:=false;
  SetLength(Entry.Name,0);
end;

class procedure Resource.Init(Var Entry:Resource.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entry) do begin
    if Entry[iLcv]<>nil then begin
      Done(Entry[iLcv]^);
      Dispose(Entry[iLcv]);
    end;
  end;
  SetLength(Entry,0);
end;

class procedure Resource.Verify(Var Entry:Resource.Items; Value:Boolean);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entry) do
    if Entry[iLcv]<>nil then
      Entry[iLcv]^.Verified:=Value;
end;

class procedure Resource.Remove(Var Entry:Resource.Items; Const Options:RemoveOptions=roUnVerified);
var
  iCount:LongInt;

  procedure PushRemoveUnVerified(iStart:LongInt);
  var
    iLcv,jLcv:LongInt;
  begin
    for iLcv:=iStart to iCount-1 do begin
      if (Entry[iLcv]=nil) then begin
        for jLcv:=iLcv to iCount-2 do
          Entry[iLcv]:=Entry[iLcv+1];
        Dec(iCount);
        SetLength(Entry,iCount);
        PushRemoveUnVerified(iLcv);
        Break;
      end else if (Entry[iLcv]^.Verified=false) then begin
        Done(Entry[iLcv]^);
        Dispose(Entry[iLcv]);
        for jLcv:=iLcv to iCount-2 do
          Entry[iLcv]:=Entry[iLcv+1];
        Dec(iCount);
        SetLength(Entry,iCount);
        PushRemoveUnVerified(iLcv);
        Break;
      end;
    end;
  end;

begin
  iCount:=Length(Entry);
  Case Options of
    roUnverified: PushRemoveUnverified(0);
  end;
end;

class procedure Resource.Remove(var Entry:Resource.PItem; Var Entries:Resource.Items);
var
  iCount,iLcv,jLcv:LongInt;
begin
  iCount:=Length(Entries);
  for iLcv:=0 to iCount-1 do begin
    if (Entries[iLcv]=Entry) then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
      for jLcv:=iLcv to iCount-2 do
        Entries[iLcv]:=Entries[iLcv+1];
      Dec(iCount);
      SetLength(Entries,iCount);
      Break;
    end;
  end;
end;

class procedure Resource.Done(Var Entry:Resource.Item);
begin
  Finalize(Entry.Name);
  Finalize(Entry);
end;

class procedure Resource.Done(Var Entry:Resource.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entry) do begin
    if Entry[iLcv]<>nil then begin
      Done(Entry[iLcv]^);
      Dispose(Entry[iLcv]);
    end;
  end;
  Finalize(Entry);
end;

class function  Resource.IndexOf(Var Entries:Resource.Items; var ID:System.QWord): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1; iCount:=Length(Entries); iLcv:=0;
  While (iLcv<iCount) and (Result=-1) do begin
    if (Entries[iLcv]<>nil) and (Entries[iLcv]^.ID=ID) then
      Result:=iLcv;
    inc(iLcv);
  end;
end;

initialization
  RegisterDBM;
end.


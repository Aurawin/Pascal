{
unit Storage.SrchQueries.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

Storage for Search Queries
i.e.) Cookies over HTTP

}
unit Storage.SrchQueries;

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

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,
  Core.Arrays.Pointers,

  SysUtils;

Type
  Items=class
  type
    Item=record
      ID                         : QWord;
      ProviderID                 : QWord;
      QueryString                : Core.Strings.VarString;
    end;
    List=Array of Item;
    PItem=^Item;
    PList=^List;
    DB=class
    type
      IDs=Class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        ProviderID               : Core.Database.Types.Integer = 2;
        Query                    : Core.Database.Types.Integer = 3;
      end;
      Keys=Class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        ProviderID               : Core.Database.Types.VarString = 'ITMPID';
        Query                    : Core.Database.Types.VarString = 'ITMQRY';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Providers';
        Name                     : 'Queries';
        Value                    : 'scs_srch_qrs';
        Hint                     : 'Storage for search queries';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..3] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Query; KeyP: @Keys.Query; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );
      class Function  Add(Task:Core.Database.Types.TTask; Var ID,ProviderID:QWord; Var QueryString:Core.Strings.VarString):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
      class Function  Find(Task:Core.Database.Types.TTask; Var ProviderID:QWord; Var QueryString:Core.Strings.VarString):QWord;
    end;
    class function  IndexOf(Var QueryString:Core.Strings.VarString; var ID:QWord; Var Entries:List): LongInt;
    class procedure Empty(Var Entries:List); overload;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Copy(Var Source,Destination:Item); overload;
    class procedure Copy(Var Source,Destination:List); overload;
    class procedure Done(var Entry:Item); overload;
    class procedure Done(Var Entries:List); overload;
  end;
  Results=class
  type
    Item=record
      ID                         : QWord;
      ProviderID                 : QWord;
      QueryID                    : QWord;
      Results                    : Core.Arrays.Types.LargeWord;
    end;
    cbData=record
      IDP                        : PQWord;
      ResultsP                   : Core.Arrays.Types.PLargeWord;
    end;
    PcbData=^cbData;
    Items=Array of Item;
    PItems=^Items;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        ProviderID               : Core.Database.Types.Integer = 2;
        QueryID                  : Core.Database.Types.Integer = 3;
        Results                  : Core.Database.Types.Integer = 4;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        ProviderID               : Core.Database.Types.VarString = 'ITMPID';
        QueryID                  : Core.Database.Types.VarString = 'ITMQID';
        Results                  : Core.Database.Types.VarString = 'ITMRA';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Providers';
        Name                     : 'Result Sets';
        Value                    : 'scs_srch_rest';
        Hint                     : 'Storage for search query result sets';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..4] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.QueryID; KeyP: @Keys.QueryID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull;  ),
        (IDP: @IDs.Results; KeyP: @Keys.Results; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNotNull;)
      );
      class Function  Add(Task:Core.Database.Types.TTask;Var Entry:Item):Boolean;
      class Function  Update(Task:Core.Database.Types.TTask; ID:QWord; Var Data:Core.Arrays.Types.LargeWord):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
      class Function  Find(Task:Core.Database.Types.TTask; Var ProviderID,QueryID:QWord; Var Data:Core.Arrays.Types.LargeWord):QWord; overload;
      class Function  Find(Task:Core.Database.Types.TTask; Var QRID:QWord; Var Data:Core.Arrays.Types.LargeWord):Boolean; overload;
    end;
    class function  IndexOf(Var QueryID:QWord; var ID:QWord; Var Entries:Items): LongInt;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:Items); overload;
    class procedure Copy(Var Source,Destination:Item); overload;
    class procedure Copy(Var Source,Destination:Items); overload;
    class procedure Done(Var Entry:Item); overload;
    class procedure Done(var Entries:Items); overload;
  end;

implementation
uses db;


procedure cbDestroyQueries(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyResults(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Results.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
  procedure PushProviderDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.ProviderID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Results.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Results.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Results.DB.TableP,useForCriteria,Results.DB.IDs.ProviderID,poNone,oEqual,ItemID,Commands);
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
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyQueries, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Results.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyResults, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class function  Results.IndexOf(Var QueryID:QWord; var ID:QWord; Var Entries:Items): LongInt;
var
  iCt,iLcv:LongInt;
begin
  iCt:=Length(Entries);
  iLcv:=0; Result:=-1;
  While (Result=-1) and (iLcv<iCt) do begin
    If Entries[iLcv].QueryID=QueryID then begin
      ID:=Entries[iLcv].ID;
      Result:=iLcv;
    end;
    Inc(iLcv);
  end;
end;

class procedure Results.Copy(Var Source,Destination:Items);
var
  iLcv:LongInt;
begin
  Empty(Destination);
  SetLength(Destination,Length(Source));
  For iLcv:=0 to High(Source) do
    Copy(Source[iLcv],Destination[iLcv]);
end;

class procedure Results.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.QueryID:=Source.QueryID;
  Destination.ProviderID:=Source.ProviderID;
  Core.Arrays.LargeWord.Copy(Source.Results,Destination.Results);
end;

class procedure Results.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    QueryID:=0;
    ProviderID:=0;
    Core.Arrays.LargeWord.Empty(Results);
  end;
end;

class procedure Results.Done(Var Entry:Item);
begin
  Core.Arrays.LargeWord.Done(Entry.Results);
  Finalize(Entry);
end;

class procedure Results.Done(var Entries:Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Results.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Empty(Entries[iLcv]);
  SetLength(Entries,0);
end;


class Function  Results.DB.Add(Task:Core.Database.Types.TTask;Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  iInsertID,iReset:QWord;
begin
  Result:=False;
  Try
    iCount:=0;
    iInsertID:=Random(High(Integer));
    iReset:=0;
    Entry.ID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,Entry.ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.QueryID,poNone,oNone,Entry.QueryID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Results,poNone,oNone,Entry.Results,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands) and (Entry.ID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Results.DB.Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
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
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure CB_QueryResults_Find(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  cbDataP:Results.PcbData;
begin
  cbDataP:=DataP;
  cbDataP^.IDP^:=Fields.FieldByName(Results.DB.Keys.ID).AsLargeInt;
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Results.DB.Keys.Results).AsString,cbDataP^.ResultsP^,',');
end;

class Function Results.DB.Find(Task:Core.Database.Types.TTask;Var ProviderID,QueryID:QWord; Var Data:Core.Arrays.Types.LargeWord):QWord; overload;
var
  qrData:cbData;
  iCount:LongInt;
  iReset,iInsertID,iID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=0; iID:=0;

  qrData.IDP:=@iID;
  qrData.ResultsP:=@Data;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ProviderID,poNone,oEqual,ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.QueryID,poAnd,oEqual,QueryID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Results,poNone,oNone,Commands);
    if Core.Database.SQL.Select(Task,@Commands,@CB_QueryResults_Find,@qrData) then begin
      If (iID<>0) then begin
        Result:=iID;
      end else begin
        iCount:=0;
        iReset:=0;
        iInsertID:=Random(High(Integer));

        Core.Database.AddCommand(iCount,TableP,@Commands);

        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
        Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,iID,Commands);
        Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,ProviderID,Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.QueryID,poNone,oNone,QueryID,Commands);

        If Core.Database.SQL.Insert(Task,@Commands) then
          Result:=iID;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_QueryResults_FindResults(Commands:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Results.DB.Keys.Results).AsString,Core.Arrays.Types.PLargeWord(DataP)^,',');
end;

class Function  Results.DB.Find(Task:Core.Database.Types.TTask;Var QRID:QWord; Var Data:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oNone,QRID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Results,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_QueryResults_FindResults,@Data);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Results.DB.Update(Task:Core.Database.Types.TTask;ID:QWord; Var Data:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Results,poNone,oNone,Data,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function  Items.IndexOf(Var QueryString:Core.Strings.VarString; var ID:QWord; Var Entries:List): LongInt;
var
  iCt,iLcv:LongInt;
begin
  iCt:=Length(Entries);
  iLcv:=0; Result:=-1;
  While (Result=-1) and (iLcv<iCt) do begin
    If Core.Strings.SameText(Entries[iLcv].QueryString,QueryString) then begin
      ID:=Entries[iLcv].ID;
      Result:=iLcv;
    end;
    Inc(iLcv);
  end;
end;

class procedure Items.Copy(Var Source,Destination:List);
var
  iLcv:LongInt;
begin
  Empty(Destination);
  SetLength(Destination,Length(Source));
  For iLcv:=0 to High(Source) do
    Copy(Source[iLcv],Destination[iLcv]);
end;

class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.ProviderID:=Source.ProviderID;
  Destination.QueryString:=Source.QueryString;
end;

class procedure Items.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    ProviderID:=0;
    Core.Strings.Empty(QueryString);
  end;
end;

class procedure Items.Empty(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Done(var Entry:Item);
begin
  with Entry do begin
    Core.Strings.Done(QueryString);
  end;
  Finalize(Entry);
end;

class procedure Items.Done(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; Var ID,ProviderID:QWord; Var QueryString:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  iInsertID,iReset:QWord;
begin
  Result:=False;
  Try
    iCount:=0;
    iInsertID:=Random(High(Integer));
    iReset:=0;
    ID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Query,poNone,oNone,QueryString,Commands);
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
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure CB_Queries_Find(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

class Function Items.DB.Find(Task:Core.Database.Types.TTask; Var ProviderID:QWord; Var QueryString:Core.Strings.VarString):QWord;
var
  iCount:LongInt;
  iID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=0; iID:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ProviderID,poNone,oEqual,ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Query,poAnd,oEqual,QueryString,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    if Core.Database.SQL.Select(Task,@Commands,@CB_Queries_Find,@iID) and (iID<>0) then
      Result:=iID;
  Finally
    Core.Database.Done(Commands);
  End;
end;


initialization
  RegisterDBM;

end.


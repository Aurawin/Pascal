{
unit Storage.SrchResults.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

{
 This database module is meant for storing web search results
 Like Google, Bing and the like
}

}

unit Storage.SrchResults;

interface

uses
  Classes,

  Core.Streams,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  SysUtils;


Type
  Store=class
  type
    Item=record  // Result Store Result;
      ID                         : QWord;
      ProviderID                 : QWord;
      DateTime                   : Double;
      Title                      : Core.Strings.VarString;
      Link                       : Core.Strings.VarString;
      Description                : Core.Strings.VarString;
    end;
    PItem=^Item;
    Items=Array of Item;
    PItems=^Items;
    cbData=record
      IDsP                       : Core.Arrays.Types.PLargeWord;
      ItemsP                     : PItems;
    end;
    PcbData=^cbData;
    bcbData=record
      Stream                     : TStream;
      FieldDelim                 : Core.Strings.VarString;
      ItemDelim                  : Core.Strings.VarString;
    end;
    PbcbData=^bcbData;
    Defaults=class
    Type
      Max=class
      Type
        Length=class
        const
          Title                  = 255;
          Link                   = 255;
          Description            = 1024*2;
        end;
      end;
    end;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        ProviderID               : Core.Database.Types.Integer = 2;
        DateTime                 : Core.Database.Types.Integer = 3;
        Title                    : Core.Database.Types.Integer = 4;
        Link                     : Core.Database.Types.Integer = 5;
        Description              : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        ProviderID               : Core.Database.Types.VarString = 'ITMPID';
        DateTime                 : Core.Database.Types.VarString = 'ITMDTS';
        Title                    : Core.Database.Types.VarString = 'ITMTIT';
        Link                     : Core.Database.Types.VarString = 'ITMLNK';
        Description              : Core.Database.Types.VarString = 'ITMDSC';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Providers';
        Name                     : 'Results';
        Value                    : 'scs_srch_res';
        Hint                     : 'Individual search results for each provider';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..6] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull; ),
        (IDP: @IDs.DateTime; KeyP: @Keys.DateTime; DataType:dftDouble; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType:dftString; AutoCreate:True; Verified:False; Precision:Defaults.Max.Length.Title; Flags: cfNone; ),
        (IDP: @IDs.Link; KeyP: @Keys.Link; DataType:dftString; AutoCreate:True; Verified:False; Precision:Defaults.Max.Length.Link; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType:dftString; AutoCreate:True; Verified:False; Precision:Defaults.Max.Length.Description; Flags: cfNone; )
      );
      class Function   IsResultInStore(Task:Core.Database.Types.TTask; Var ProviderID, ItemID:QWord; Var dtStamp:Double; Var Title,Link,Description:Core.Strings.VarString):Boolean;
      class Function   RemoveResult(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
      class Function   List(Task:Core.Database.Types.TTask; Var Results:Core.Arrays.Types.LargeWord; Var Entries:Items): LongInt;
      class Function   BuildResultStream(Task:Core.Database.Types.TTask; UserID:QWord; Var Results:Core.Arrays.Types.LargeWord; Stream:TStream; Const FieldDelim:Core.Strings.VarString=#2; Const ItemDelim:Core.Strings.VarString=#1; Const InteractionFieldDelim:Core.Strings.VarString=#4; Const InteractionDelim:Core.Strings.VarString=#3):Boolean;
    end;
    class procedure  Empty(Var Entries:Items) overload;
    class procedure  Empty(Var Entry:Item); overload;
    class procedure  Done(Var Entries:Items); overload;
    class procedure  Done(Var Entry:Item); overload;
    class procedure  Copy(Var Source,Destination:Item); overload;
    class procedure  Copy(Var Source,Destination:Items); overload;
  end;



implementation
uses db;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Store.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushProviderDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Store.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Store.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Store.DB.TableP,useForCriteria,Store.DB.IDs.ProviderID,poNone,oEqual,ItemID,Commands);
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
  with Store.DB do begin
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

class procedure  Store.Done(Var Entry:Item);
begin
  with Entry do begin
    Core.Strings.Done(Link);
    Core.Strings.Done(Title);
    Core.Strings.Done(Description);
  end;
  Finalize(Entry);
end;

class procedure  Store.Done(Var Entries:Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Store.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    ProviderID:=0;
    DateTime:=0;
    Core.Strings.Empty(Link);
    Core.Strings.Empty(Title);
    Core.Strings.Empty(Description);
  end;
end;

class procedure Store.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure  Store.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.ProviderID:=Source.ProviderID;
  Destination.DateTime:=Source.DateTime;
  Destination.Link:=Source.Link;
  Destination.Title:=Source.Title;
  Destination.Description:=Source.Description;
end;

class procedure  Store.Copy(Var Source,Destination:Items);
var
  iLcv,iLength:LongInt;
begin
  iLength:=Length(Source);
  Empty(Destination);
  SetLength(Destination,iLength);
  For iLcv:=0 to iLength-1 do
    Copy(Source[iLcv],Destination[iLcv]);
end;

class Function   Store.DB.RemoveResult(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
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

procedure CB_ResultStore_Is_Result_In_Store_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Store.DB.Keys.ID).AsLargeInt;
end;

class Function   Store.DB.IsResultInStore(Task:Core.Database.Types.TTask; Var ProviderID,ItemID:QWord; Var dtStamp:Double; Var Title,Link,Description:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  iReset,iInsertID:QWord;
begin
  Result:=False;
  Try
    iCount:=0; ItemID:=0; iReset:=0;  iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Link,poNone,oEqual,Link,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ProviderID,poAnd,oEqual,ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,ItemID,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_ResultStore_Is_Result_In_Store_ID,@ItemID);
    If Result and (ItemID=0) then begin
      // We need to insert stuff here...
      iCount:=0;
      Core.Database.Empty(Commands);
      Core.Database.AddCommand(iCount,TableP,@Commands);

      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,ProviderID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DateTime,poNone,oNone,dtStamp,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Title,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Link,poNone,oNone,Link,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Description,poNone,oNone,Description,Commands);
      Result:=Core.Database.SQL.Insert(Task,@Commands) and (ItemID<>0);
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_BuildResultStream(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  cbDataP:Store.PbcbData;
begin
  cbDataP:=DataP;
  Core.Streams.Write(Fields.FieldByName(Store.DB.Keys.ID).AsString,cbDataP^.Stream);
  Core.Streams.Write(cbDataP^.FieldDelim,cbDataP^.Stream);
  Core.Streams.Write(Fields.FieldByName(Store.DB.Keys.ProviderID).AsString,cbDataP^.Stream);
  Core.Streams.Write(cbDataP^.FieldDelim,cbDataP^.Stream);
  Core.Streams.Write(Fields.FieldByName(Store.DB.Keys.DateTime).AsString,cbDataP^.Stream);
  Core.Streams.Write(cbDataP^.FieldDelim,cbDataP^.Stream);
  Core.Streams.Write(Fields.FieldByName(Store.DB.Keys.Title).AsString,cbDataP^.Stream);
  Core.Streams.Write(cbDataP^.FieldDelim,cbDataP^.Stream);
  Core.Streams.Write(Fields.FieldByName(Store.DB.Keys.Link).AsString,cbDataP^.Stream);
  Core.Streams.Write(cbDataP^.FieldDelim,cbDataP^.Stream);
  Core.Streams.Write(Fields.FieldByName(Store.DB.Keys.Description).AsString,cbDataP^.Stream);
  Core.Streams.Write(cbDataP^.ItemDelim,cbDataP^.Stream);
end;

class Function   Store.DB.BuildResultStream(Task:Core.Database.Types.TTask; UserID:QWord; Var Results:Core.Arrays.Types.LargeWord; Stream:TStream; Const FieldDelim:Core.Strings.VarString=#2; Const ItemDelim:Core.Strings.VarString=#1; Const InteractionFieldDelim:Core.Strings.VarString=#4; Const InteractionDelim:Core.Strings.VarString=#3):Boolean;
var
  iID:QWord;
  iLcv,iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  cbData:bcbData;
begin
  Result:=False;

  cbData.Stream:=Stream;
  cbData.FieldDelim:=FieldDelim;
  cbData.ItemDelim:=ItemDelim;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,iID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ProviderID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DateTime,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);

    For iLcv:=0 to High(Results) do begin
      Commands[1].Value:=@Results[iLcv];
      Core.Database.SQL.Select(Task,@Commands,@CB_BuildResultStream,@cbData);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_ResultStore_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iIndex    : LongInt;
  cbDataP   : Store.PcbData;
begin
  cbDataP:=DataP;
  iIndex:=Length(cbDataP^.ItemsP^);
  Setlength(cbDataP^.ItemsP^,iIndex+1);
  cbDataP^.ItemsP^[iIndex].ID:=Fields.FieldByName(Store.DB.Keys.ID).AsLargeInt;
  cbDataP^.ItemsP^[iIndex].ProviderID:=Fields.FieldByName(Store.DB.Keys.ProviderID).AsLargeInt;
  cbDataP^.ItemsP^[iIndex].DateTime:=Fields.FieldByName(Store.DB.Keys.DateTime).AsDateTime;
  cbDataP^.ItemsP^[iIndex].Title:=Fields.FieldByName(Store.DB.Keys.Title).AsString;
  cbDataP^.ItemsP^[iIndex].Link:=Fields.FieldByName(Store.DB.Keys.Link).AsString;
  cbDataP^.ItemsP^[iIndex].Description:=Fields.FieldByName(Store.DB.Keys.Description).AsString;
  cbDataP^.IDsP^[iIndex]:=cbDataP^.ItemsP^[iIndex].ID;
end;

class Function   Store.DB.List(Task:Core.Database.Types.TTask; Var Results:Core.Arrays.Types.LargeWord; Var Entries:Items): LongInt;
var
  iLcv,iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  qrData:cbData;
begin
  Result:=0;
  If Length(Results)>0 then begin
    Try
      Empty(Entries);
      qrData.IDsP:=@Results;
      qrData.ItemsP:=@Entries;
      iCount:=0;
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Results[0],Commands);
      For iLcv:=1 to High(Results) do
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poOr,oEqual,Results[iLcv],Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ProviderID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DateTime,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);
      if Core.Database.SQL.Select(Task,@Commands,@CB_ResultStore_List,@qrData) then begin
        Result:=Length(Entries);
        if Result>0 then
          SetLength(Entries,Result);
      end;
    Finally
      Core.Database.Done(Commands);
    End;
  end;
end;

initialization
  RegisterDBM;
end.


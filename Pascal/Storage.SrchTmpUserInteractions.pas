{
unit dbmSrchTmpUserInteractions.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

Storage for Interactions for Temporary Users for Search System
}
unit Storage.SrchTmpUserInteractions;


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
  Core.Arrays.Bytes,

  SysUtils;

Type
  Interaction=class
  type
    Kind=(
      // WARNING!!! DO NOT CHANGE THIS LIST.
      // JUST ADD TO IT.
      // CHANGING EXISING LIST WILL CORRUPT!!!
      tsuiNone,
      tsuiDelete,
      tsuiRank,
      tsuiRate,
      tsuiTag,
      tsuiAnnotate,
      tsuiSave
    );
    Item=record
      Kind                       : QWord;       // Like Delete, Tag, Rate, Save Etc...
      Data                       : Core.Arrays.Types.Bytes  // Empty or Position... or Tag Data... Or Even a Note...
    end;
    PItem=^Item;
    Items=Array of Item;
    PItems=^Items;
    Manifest=Array of Items;
    PManifest=^Manifest;
    cbData=record
       ManifestP                 : PManifest;
       ResultsP                  : Core.Arrays.Types.LargeWord;
    end;
    PcbData=^cbData;

    DB=class
    type
      IDs=class
      const
        ProviderID               : Core.Database.Types.Integer = 0;
        DomainID                 : Core.Database.Types.Integer = 1;
        UserID                   : Core.Database.Types.Integer = 2;
        ResultID                 : Core.Database.Types.Integer = 3;
        Kind                     : Core.Database.Types.Integer = 4;
        ResultList               : Core.Database.Types.Integer = 5;
        Data                     : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ProviderID               : Core.Database.Types.VarString = 'ITMPID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        ResultID                 : Core.Database.Types.VarString = 'ITMRID';
        Kind                     : Core.Database.Types.VarString = 'ITMINI';
        ResultList               : Core.Database.Types.VarString = 'ITMIRL';
        Data                     : Core.Database.Types.VarString = 'ITMIND';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Users/Interactions';
        Name                     : 'Anonymous';
        Value                    : 'scs_srch_tui';
        Hint                     : 'Interactions for temporary user accounts';
        PrimaryKeyP              : Core.Database.Types.NoKey;
      );
      Fields: array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.ResultID; KeyP: @Keys.ResultID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.ResultList; KeyP: @Keys.ResultList; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 1024; Flags: cfNone; )
      );

      class Function  Add(Task:Core.Database.Types.TTask; ProviderID,DomainID,UserID,ResultID,InteractionID:QWord; Var Data:Core.Arrays.Types.Bytes):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Var UserID,ResultID:QWord):Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; Var UserID:QWord):Boolean; overload;
      class Function  Retrieve(Task:Core.Database.Types.TTask; Var UserID,ResultID:QWord; Var Entries:Items):Boolean;
      class Function  Remove(Task:Core.Database.Types.TTask; Var UserID,ResultID,InteractionID:QWord):Boolean;
      class Function  CleanUpList(Task:Core.Database.Types.TTask; Var UserID,QueryID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
      class Function  CleanUpLists(Task:Core.Database.Types.TTask; Var UserID:QWord; Var ResultList:Core.Arrays.Types.LargeWord; Var Entries:Manifest):Boolean;
    end;
  Const
    Descriptions:Array[Kind] of Core.Strings.VarString=(
      'Unassigned',
      'Delete',
      'Rank',
      'Rate',
      'Tag',
      'Annotate',
      'Save'
    );

    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:Items); overload;
    class procedure Empty(Var Entries:Manifest); overload;

    class procedure Copy(Var Source,Destination:Item); overload;
    class procedure Copy(Var Source,Destination:Items); overload;

    class procedure Done(Var Entries:Manifest); overload;
    class procedure Done(Var Entries:Items); overload;
    class procedure Done(Var Entry:Item); overload;

    class Function  IsResultDeleted(Var Entries:Manifest; Index:LongInt):Boolean;
    class procedure Add(Var Entries:Items; Interaction:QWord; Var Data:Core.Arrays.Types.Bytes);
  end;

implementation
uses DB;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Interaction.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushProviderDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Interaction.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interaction.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interaction.DB.TableP,useForCriteria,Interaction.DB.IDs.ProviderID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        SetLength(Commands,0);
      End;
    end;
  end;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Interaction.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interaction.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interaction.DB.TableP,useForCriteria,Interaction.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        SetLength(Commands,0);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.PROVIDER_DELETED : PushProviderDeleted;
    Core.Database.Monitor.Notify.DOMAIN_DELETED   : PushDomainDeleted;
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Interaction.DB do begin
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


class procedure Interaction.Copy(Var Source,Destination:Items);
var
  iCount,iLcv:LongInt;
begin
  iCount:=Length(Source);
  Empty(Destination);
  SetLength(Destination,iCount);
  For iLcv:=0 to iCount-1 do
    Copy(Source[iLcv],Destination[iLcv]);
end;

class procedure Interaction.Copy(Var Source,Destination:Item);
begin
  //Destination.ResultID:=Source.ResultID;
  Destination.Kind:=Source.Kind;
  Core.Arrays.Bytes.Copy(Source.Data,Destination.Data);
end;

class procedure Interaction.Add(Var Entries:Items; Interaction:QWord; Var Data:Core.Arrays.Types.Bytes);
var
  iLength:LongInt;
begin
  iLength:=Length(Entries);
  SetLength(Entries,iLength+1);
  Entries[iLength].Kind:=Interaction;
  Core.Arrays.Bytes.Copy(Data,Entries[iLength].Data);
end;

class procedure Interaction.Empty(Var Entry:Item);
begin
  //Item.ResultID:=0;
  Entry.Kind:=0;
  Core.Arrays.Bytes.Empty(Entry.Data);
end;

class procedure Interaction.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Empty(Entries[iLcv]);
  SetLength(Entries,0);
end;

class Function  Interaction.IsResultDeleted(Var Entries:Manifest; Index:LongInt):Boolean;
var
  iCount,iLcv:LongInt;
begin
  Result:=False; iCount:=Length(Entries[Index]); iLcv:=0;
  While (iLcv<iCount) and Not Result do begin
    Result:=Entries[Index][iLcv].Kind=Integer(tsuiDelete);
    Inc(iLcv);
  end;
end;

class procedure Interaction.Empty(Var Entries:Manifest);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Interaction.Done(Var Entries:Manifest);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Interaction.Done(Var Entries:Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Interaction.Done(Var Entry:Item);
begin
  Core.Arrays.Bytes.Done(Entry.Data);
  Finalize(Entry);
end;

class Function  Interaction.DB.Add(Task:Core.Database.Types.TTask; ProviderID,DomainID,UserID,ResultID,InteractionID:QWord; Var Data:Core.Arrays.Types.Bytes):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ResultID,poNone,oNone,ResultID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,InteractionID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Data,poNone,oNone,Data,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Interaction.DB.Remove(Task:Core.Database.Types.TTask; Var UserID,ResultID,InteractionID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResultID,poAnd,oEqual,ResultID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,InterActionID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Interaction.DB.Delete(Task:Core.Database.Types.TTask; Var UserID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
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

class Function  Interaction.DB.Delete(Task:Core.Database.Types.TTask; Var UserID,ResultID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResultID,poAnd,oEqual,ResultID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_TempUserInteraction_Retrieve(CommandP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP:Interaction.PItems;
  iIndex:LongInt;
begin
  ListP:=DataP;
  iIndex:=Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex].Kind:=Fields.FieldByName(Interaction.DB.Keys.Kind).AsLargeInt;
  Core.Arrays.Bytes.fromString(Fields.FieldByName(Interaction.DB.Keys.Data).AsString,@ListP^[iIndex].Data);
end;

class Function  Interaction.DB.Retrieve(Task:Core.Database.Types.TTask; Var UserID,ResultID:QWord; Var Entries:Items):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResultID,poAnd,oEqual,ResultID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Data,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_TempUserInteraction_Retrieve,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_TempUserInteraction_CleanUpList(CommandP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iIndex:LongInt;
  ID:QWord;
  IAP:Core.Arrays.Types.PLargeWord;
begin
  IAP:=DataP;
  iIndex:=Length(IAP^);
  SetLength(IAP^,iIndex+1);
  IAP^[iIndex]:=Fields.FieldByName(Interaction.DB.Keys.ResultID).AsLargeInt;
end;

class Function  Interaction.DB.CleanUpList(Task:Core.Database.Types.TTask; Var UserID,QueryID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iIndex,iLcv,iLength,iCount:LongInt;
  cmdDelete:QWord;
  Commands:Core.Database.Types.Commands;
  DeletedList:Core.Arrays.Types.LargeWord;
  sIN:Core.Strings.VarString;
  sQuery:Core.Strings.VarString;
begin
  Result:=False;
  Try
    cmdDelete:=Integer(tsuiDelete);
    Core.Arrays.LargeWord.Empty(DeletedList);
    Try
      iCount:=0;
      Core.Arrays.LargeWord.toString(Entries,',',sIN);
      Try
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,cmdDelete,Commands);

        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResultID,poAnd,oIn,sIN,Commands);
        Commands[iCount-1].DataType:=dftString;

        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ResultID,poNone,oNone,Commands);
        Result:=Core.Database.SQL.Select(Task,@Commands,@CB_TempUserInteraction_CleanUpList,@DeletedList);
        iLength:=Length(DeletedList);
        if iLength>0 then begin
          // We look at each List item to see if it's in the list of deletes
          For iLcv:=0 to iLength-1 do begin
            iIndex:=Core.Arrays.LargeWord.IndexOf(DeletedList[iLcv],Entries);
            If iIndex<>-1 then
              Core.Arrays.LargeWord.Remove(iIndex,Entries);
          end;
        end;
      Finally
        Core.Strings.Done(sIN);
      end;
    finally
      Core.Arrays.LargeWord.Done(DeletedList);
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_Interaction_CleanupLists(CommandP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin

end;

class Function  Interaction.DB.CleanupLists(Task:Core.Database.Types.TTask; Var UserID:QWord; Var ResultList:Core.Arrays.Types.LargeWord; Var Entries:Manifest):Boolean;
var
  iCount:LongInt;
  cmdDelete:QWord;
  Commands:Core.Database.Types.Commands;
  qrData: cbData;
begin
  Result:=False;
  Try
    iCount:=0;
    cmdDelete:=Integer(tsuiDelete);
    qrData.ManifestP:=@Entries;
    qrData.ResultsP:=@ResultList;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oNotEqual,cmdDelete,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ResultID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Data,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Interaction_CleanupLists,@qrData);
  Finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDBM;

end.


{
unit Storage.SrchSessions.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

Storage for Session Search System
}

unit Storage.SrchSessions;

interface

uses
  Classes,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,

  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,
  Core.Arrays.VarString,

  Storage,
  Storage.Main,

  SysUtils;


Type
  Session=class
  type
    Kind=class
    const
      // Append only.  Do not edit
      // Editing will corrupt stores
      Temp                       : Core.Database.Types.LargeWord = 0;
      Registered                 : Core.Database.Types.LargeWord = 1;
    end;
    Item=record
      ID                         : QWord;
      ClassID                    : QWord;
      UserID                     : QWord;
      ParentID                   : QWord;
      Caption                    : Core.Strings.VarString;
      QueryIDs                   : Core.Arrays.Types.LargeWord;
      SavedIDs                   : Core.Arrays.Types.LargeWord;
    end;
    PItem=^Item;
    Items=Array of PItem;
    PItems=^Items;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        ClassID                  : Core.Database.Types.Integer = 3;
        UserID                   : Core.Database.Types.Integer = 4;
        ParentID                 : Core.Database.Types.Integer = 5;
        Caption                  : Core.Database.Types.Integer = 6;
        QueryIDs                 : Core.Database.Types.Integer = 7;
        SavedIDs                 : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        ClassID                  : Core.Database.Types.VarString = 'ITMCID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        ParentID                 : Core.Database.Types.VarString = 'ITMPID';
        Caption                  : Core.Database.Types.VarString = 'ITMC';
        QueryIDs                 : Core.Database.Types.VarString = 'IDS';
        SavedIDs                 : Core.Database.Types.VarString = 'SIDS';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Sessions';
        Name                     : 'Sessions';
        Value                    : 'scs_srch_sns';
        Hint                     : 'Search session storage for domains';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.ClassID; KeyP: @Keys.ClassID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.ParentID; KeyP: @Keys.ParentID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.Caption; KeyP: @Keys.Caption; DataType: dftString; AutoCreate: True; Verified: False; Precision: 75; Flags: cfNone ),
        (IDP: @IDs.QueryIDs; KeyP: @Keys.QueryIDs; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone ),
        (IDP: @IDs.SavedIDs; KeyP: @Keys.SavedIDs; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone )
      );

      class Function  Add(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  UpdateQueries(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Entry:Item):Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; ClassID,UserID:QWord):Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; ClassID,UserID,SessionID:QWord):Boolean; overload;
      class Function  ListSavedIDs(Task:Core.Database.Types.TTask; SessionID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
      class Function  UpdateSavedIDs(Task:Core.Database.Types.TTask; SessionID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
      class Function  List(Task:Core.Database.Types.TTask; ClassID,UserID:QWord; Var Entries:Items):Boolean;
    end;
    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:Items); overload;
    class function  IndexOf(Var ID:QWord; Var Entries:Items): LongInt;
    class procedure Init(Var Entry:Item); overload;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:Items); overload;
    class procedure Copy(Var Source,Destination:Item); overload;
    class procedure Copy(Var Source,Destination:Items); overload;
  end;
  Data=class
  type
    Item=record
      ID                         : QWord;
      QueryIndex                 : LongInt;
      Path                       : Core.Strings.VarString;
      Queries                    : Core.Arrays.Types.VarString;
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
        DomainID                 : Core.Database.Types.Integer = 2;
        ClassID                  : Core.Database.Types.Integer = 3;
        UserID                   : Core.Database.Types.Integer = 4;
        QueryIndex               : Core.Database.Types.Integer = 5;
        Queries                  : Core.Database.Types.Integer = 6;
        Path                     : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        ClassID                  : Core.Database.Types.VarString = 'ITMCID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        QueryIndex               : Core.Database.Types.VarString = 'ITMQI';
        Queries                  : Core.Database.Types.VarString = 'ITMQ';
        Path                     : Core.Database.Types.VarString = 'ITMP';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Sessions';
        Name                     : 'Properties';
        Value                    : 'scs_srch_snp';
        Hint                     : 'Search session data storage for domains';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.ClassID; KeyP: @Keys.ClassID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.QueryIndex; KeyP: @Keys.QueryIndex; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.Queries; KeyP: @Keys.Queries; DataType: dftString; AutoCreate: True; Verified: False; Precision: 1024*5; Flags: cfNone ),
        (IDP: @IDs.Path; KeyP: @Keys.Path; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone )
      );
      class Function  Delete(Task:Core.Database.Types.TTask; ItemID:QWord):Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; DomainID,GroupID,ClassID,UserID:QWord; Var ItemID:QWord; Var Path:Core.Strings.VarString):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;  // ID Is Already Present
      class Function  Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  List(Task:Core.Database.Types.TTask; DomainID,ClassID,UserID: QWord; Var Entries:Items):Boolean;
    end;
    class procedure Init(Var Entry:Item); overload;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:Items) overload;
    class procedure Done(Var Entries:Items) overload;
    class procedure Done(Var Entry:Item); overload;
  end;
  Interaction=class
  type
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        ProviderID               : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ResultID                 : Core.Database.Types.Integer = 4;
        Command                  : Core.Database.Types.Integer = 5;
        CommandData              : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        ProviderID               : Core.Database.Types.VarString = 'IMPVD';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        ResultID                 : Core.Database.Types.VarString = 'RID';
        Command                  : Core.Database.Types.VarString = 'CMD';
        CommandData              : Core.Database.Types.VarString = 'CMDD';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search/Sessions';
        Name                     : 'Interactions';
        Value                    : 'scs_srch_sni';
        Hint                     : 'Search session interactions storage for domains';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.ResultID; KeyP: @Keys.ResultID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.Command; KeyP: @Keys.Command; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone ),
        (IDP: @IDs.CommandData; KeyP: @Keys.CommandData; DataType: dftString; AutoCreate: True; Verified: False; Precision: 1024; Flags: cfNone )
      );
    end;
  end;


implementation
uses db;


procedure cbDestroySessionTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Session.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyPropertyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Data.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyInteractionTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Interaction.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;


function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;


  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Session.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Session.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Session.DB.TableP,useForCriteria,Session.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Data.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Data.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Data.DB.TableP,useForCriteria,Data.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Interaction.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interaction.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interaction.DB.TableP,useForCriteria,Interaction.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
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
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Session.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
      If MonitorP=nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroySessionTable,@cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Data.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
      If MonitorP=nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyPropertyTable,@cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Interaction.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
      If MonitorP=nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyInteractionTable,@cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;
(*

  if TableP=nil then begin
    New(TableP);
    Init(TableP^, dbsiInteractions);
    iCount:=0;
    Core.Database.AddField(iCount,True,Integer(ssniID),cfNotNull or cfPrimaryKey or cfIdentity,0,SRCH_SSN_FN_ID,dftQWord,TableP^.Fields);
    Core.Database.AddField(iCount,True,Integer(ssniProviderID),0,0,SRCH_SSN_FN_PROVIDER_ID,dftQWord,TableP^.Fields);
    Core.Database.AddField(iCount,True,Integer(ssniDomainID),0,0,SRCH_SSN_FN_DOMAIN_ID,dftQWord,TableP^.Fields);
    Core.Database.AddField(iCount,True,Integer(ssniUserID),0,0,SRCH_SSN_FN_USER_ID,dftQWord,TableP^.Fields);
    Core.Database.AddField(iCount,True,Integer(ssniResultID),0,0,SRCH_SSN_FN_RESULT_ID,dftQWord,TableP^.Fields);
    Core.Database.AddField(iCount,True,Integer(ssniCommand),0,0,SRCH_SSN_FN_CMD,dftQWord,TableP^.Fields);
    Core.Database.AddField(iCount,True,Integer(ssniCommandData),0,1024,SRCH_SSN_FN_CMD_DATA,dftString,TableP^.Fields);
    If dbmiInteractionsP=nil then begin
      New(dbmiInteractionsP);
      Init(dbmiInteractionsP^,TableP^,@cbDestroyInteractions,@cbDBMonitorNotifyied);
      Core.Database.Monitor.Add(dbmiInteractionsP);
    end;
  end;
end;
*)
class procedure Data.Init(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    QueryIndex:=-1;
    Core.Strings.Init(Path);
    Core.Arrays.VarString.Init(Queries);
  end;
end;

class procedure Data.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    QueryIndex:=-1;
    Core.Strings.Empty(Path);
    Core.Arrays.VarString.Empty(Queries);
  end;
end;

class procedure Data.Done(Var Entry:Item);
begin
  with Entry do begin
    Finalize(Path);
    Core.Arrays.VarString.Done(Queries);
  end;
  Finalize(Entry);
end;

class procedure Data.Done(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Session.Done(Var Entry:Item);
begin
  Core.Strings.Done(Entry.Caption);
  Core.Arrays.LargeWord.Done(Entry.QueryIDs);
  Core.Arrays.LargeWord.Done(Entry.SavedIDs);
end;

class procedure Session.Done(Var Entries:Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;


class procedure Data.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class function  Session.IndexOf(var ID:QWord; Var Entries:Items): LongInt;
var
  iCt,iLcv:LongInt;
begin
  iCt:=Length(Entries);
  iLcv:=0; Result:=-1;
  While (Result=-1) and (iLcv<iCt) do begin
    If Entries[iLcv]^.ID=ID then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class procedure Session.Copy(Var Source,Destination:Items);
var
  iLcv:LongInt;
begin
  Empty(Destination);
  SetLength(Destination,Length(Source));
  For iLcv:=0 to High(Source) do begin
    New(Destination[iLcv]);
    Copy(Source[iLcv]^,Destination[iLcv]^);
  end;
end;

class procedure Session.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.ClassID:=Source.ClassID;
  Destination.UserID:=Source.UserID;
  Destination.ParentID:=Source.ParentID;
  Destination.Caption:=Source.Caption;
  Core.Arrays.LargeWord.Copy(Destination.QueryIDs,Source.QueryIDs);
  Core.Arrays.LargeWord.Copy(Source.SavedIDs,Destination.SavedIDs);
end;

class procedure Session.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Session.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    ClassID:=0;
    UserID:=0;
    ParentID:=0;
    Core.Strings.Empty(Caption);
    Core.Arrays.LargeWord.Empty(QueryIDs);
    Core.Arrays.LargeWord.Empty(SavedIDs);
  end;
end;

class procedure Session.Init(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    ClassID:=0;
    UserID:=0;
    ParentID:=0;
    Core.Strings.Init(Caption);
    Core.Arrays.LargeWord.Init(QueryIDs);
    Core.Arrays.LargeWord.Init(SavedIDs);
  end;
end;

class Function  Session.DB.Add(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
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

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ClassID,poNone,oNone,Entry.ClassID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ParentID,poNone,oNone,Entry.ParentID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,Entry.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Caption,poNone,oNone,Entry.Caption,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.QueryIDs,poNone,oNone,Entry.QueryIDs,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.SavedIDs,poNone,oNone,Entry.SavedIDs,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (Entry.ID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Session.DB.UpdateQueries(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.QueryIDs,poNone,oNone,Entry.QueryIDs,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Session.DB.Delete(Task:Core.Database.Types.TTask; Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Session.DB.Delete(Task:Core.Database.Types.TTask; ClassID,UserID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClassID,poNone,oEqual,ClassID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Session.DB.Delete(Task:Core.Database.Types.TTask; ClassID,UserID,SessionID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClassID,poNone,oEqual,ClassID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,SessionID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_SearchSessions_List_Saved_IDs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Session.DB.Keys.SavedIDs).AsString,Core.Arrays.Types.PLargeWord(DataP)^,',');
end;

class Function  Session.DB.ListSavedIDs(Task:Core.Database.Types.TTask; SessionID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,SessionID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.SavedIDs,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_SearchSessions_List_Saved_IDs,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Session.DB.UpdateSavedIDs(Task:Core.Database.Types.TTask; SessionID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,SessionID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.SavedIDs,poNone,oNone,Entries,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_SearchSessions_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP:Session.PItems;
  iIndex:LongInt;
begin
  ListP:=DataP;
  iIndex:=Length(ListP^);
  SetLength(ListP^,iIndex+1);
  New(ListP^[iIndex]);
  Session.Init(ListP^[iIndex]^);
  ListP^[iIndex]^.ID:=Fields.FieldByName(Session.DB.Keys.ID).AsLargeInt;
  ListP^[iIndex]^.ClassID:=Fields.FieldByName(Session.DB.Keys.ClassID).AsLargeInt;
  ListP^[iIndex]^.ParentID:=Fields.FieldByName(Session.DB.Keys.ParentID).AsLargeInt;
  ListP^[iIndex]^.Caption:=Fields.FieldByName(Session.DB.Keys.Caption).AsString;
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Session.DB.Keys.QueryIDs).AsString,ListP^[iIndex]^.QueryIDs,',');
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Session.DB.Keys.SavedIDs).AsString,ListP^[iIndex]^.SavedIDs,',');
end;

class Function Session.DB.List(Task:Core.Database.Types.TTask; ClassID,UserID:QWord; Var Entries:Items):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Entries);
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClassID,poAnd,oEqual,ClassID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ClassID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ParentID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Caption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.QueryIDs,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.SavedIDs,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_SearchSessions_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_SessionLayer_List_Sessions(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iCount:LongInt;
  ListP:Data.PItems;
begin
  ListP:=DataP;
  iCount:=Length(ListP^);
  SetLength(ListP^,iCount+1);
  ListP^[iCount].ID:=Fields.FieldByName(Data.DB.Keys.ID).AsLargeInt;
  ListP^[iCount].QueryIndex:=Fields.FieldByName(Data.DB.Keys.QueryIndex).AsInteger;
  ListP^[iCount].Path:=Fields.FieldByName(Data.DB.Keys.Path).AsString;
  Core.Arrays.VarString.fromString(@ListP^[iCount].Queries,Fields.FieldByName(Data.DB.Keys.Queries).AsString);
end;

class Function  Data.DB.List(Task:Core.Database.Types.TTask; DomainID, ClassID, UserID: QWord; Var Entries:Items):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;

begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClassID,poNone,oEqual,ClassID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.QueryIndex,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Queries,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_SessionLayer_List_Sessions,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Data.DB.Add(Task:Core.Database.Types.TTask; DomainID,GroupID,ClassID,UserID:QWord; Var ItemID:QWord; Var Path:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  iInsertID,iReset:QWord;
begin
  Result:=False;
  Try
    iCount:=0; iInsertID:=Random(High(Integer)); iReset:=0; ItemID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ClassID,poNone,oNone,ClassID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Path,poNone,oNone,Path,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (ItemID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Data.DB.Delete(Task:Core.Database.Types.TTask; ItemID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
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


procedure CB_SessionLayer_Read_Session(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  SSNLDataP:Data.PItem;
begin
  SSNLDataP:=DataP;
  SSNLDataP^.QueryIndex:=Fields.FieldByName(Data.DB.Keys.QueryIndex).AsInteger;
  SSNLDataP^.Path:=Fields.FieldByName(Data.DB.Keys.Path).AsString;
  Core.Arrays.VarString.fromString(@SSNLDataP^.Queries,Fields.FieldByName(Data.DB.Keys.Queries).AsString);
end;

class Function  Data.DB.Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;  // ID Is Already Present
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.QueryIndex,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Queries,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_SessionLayer_Read_Session,@Entry);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Data.DB.Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  sData:Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0;
    sData:=Core.Arrays.VarString.toString(Entry.Queries,#13#10);
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Path,poNone,oNone,Entry.Path,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.QueryIndex,poNone,oNone,Entry.QueryIndex,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Queries,poNone,oNone,sData,Commands);

      Result:=Core.Database.SQL.Update(Task,@Commands);
    Finally
      Finalize(sData);
    End;
  Finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDBM;
end.


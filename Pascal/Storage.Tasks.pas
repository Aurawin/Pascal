unit Storage.Tasks;

{
  unit dbmTasks.pas

  Tasks Database Module

  DBMS facilities to handle Tasks for Spectrum

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}



interface

uses
  Classes,

  RSR,
  RSR.Core,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.CoreObjects,
  Core.Timer,
  Core.Streams,
  Core.Strings,
  Core.XML,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  DOM,
  SysUtils;

type
  Items = class
  type
    Item = record
      ID                       : QWord;
      ProjectID                : QWord;
      GroupID                  : QWord;
      Created                  : double;
      Modified                 : double;
      Due                      : double;
      Creator                  : QWord;
      Frequency                : double;
      Priority                 : LongInt;
      Status                   : LongInt;
      Verified                 : boolean;
      Team                     : Core.Arrays.Types.LargeWord;
      Admins                   : Core.Arrays.Types.LargeWord;
      Files                    : Core.Arrays.Types.LargeWord;
      Comments                 : Core.Arrays.Types.LargeWord;
      Caveats                  : Core.Strings.VarString;
      Skills                   : Core.Strings.VarString;
      Title                    : Core.Strings.VarString;
    end;
    PItem = ^Item;
    PList=^List;
    List=Array of PItem;
    Kind=class
    const
      Unknown                    = -1;
      Low                        = 0;
      ProjectID                  = 0;
      GroupID                    = 1;
      High                       = 1;
    end;
    Priority = class
    const
      Unknown                  = -1;
      Deprecated               = 0;
      Abandoned                = 1;
      Normal                   = 2;
      Important                = 3;
      Elevated                 = 4;
      High                     = 5;
    end;
    Status = class
    const
      Unknown                  = -1;
      Untouched                = 0;
      Assigned                 = 1;
      Unassigned               = 2;
      Pending                  = 3;
      Complete                 = 4;
      Stopped                  = 5;
    end;
    Frequency= class
    const
      Unknown                  = -1;
      Once                     = 0;
      Hourly                   = 1;
      Daily                    = 2;
      Weekly                   = 3;
      BiWeekly                 = 4;
      SemiMonthly              = 5;
      Monthly                  = 6;
      Quartly                  = 7;
      Seasonly                 = 8;
      Annually                 = 9;
      Specific                 = 10; // Will provide ttl
    end;
    Defaults = class
    const
      Priority                   = Priority.Normal;
      Status                     = Status.Untouched;
      Frequency                  = Frequency.Once;
      ProjectID                  = 0;
      GroupID                    = 0;
    end;
    XML=class
    type
      Stanzas=class
      const
        Tasks                    = 'tasks';
        Task                     = 'task';
      end;
      Fields=class
      const
        ID                       = 'id';
        ProjectID                = 'pid';
        GroupID                  = 'gid';
        Created                  = 'created';
        Modified                 = 'modified';
        Due                      = 'due';
        Creator                  = 'creator';
        Frequency                = 'hz';
        Priority                 = 'priority';
        Status                   = 'status';
        Team                     = 'team';
        Admins                   = 'admins';
        Files                    = 'files';
        Comments                 = 'comments';
        Caveats                  = 'caveats';
        Skills                   = 'skills';
        Title                    = 'title';
      end;
    end;
    DB=class
    Type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ProjectID                : Core.Database.Types.Integer = 4;
        GroupID                  : Core.Database.Types.Integer = 5;
        Created                  : Core.Database.Types.Integer = 6;
        Modified                 : Core.Database.Types.Integer = 7;
        Due                      : Core.Database.Types.Integer = 8;
        Creator                  : Core.Database.Types.Integer = 9;
        Frequency                : Core.Database.Types.Integer = 10;
        Priority                 : Core.Database.Types.Integer = 11;
        Status                   : Core.Database.Types.Integer = 12;
        Team                     : Core.Database.Types.Integer = 13;
        Admins                   : Core.Database.Types.Integer = 14;
        Files                    : Core.Database.Types.Integer = 15;
        Comments                 : Core.Database.Types.Integer = 16;
        Caveats                  : Core.Database.Types.Integer = 17;
        Skills                   : Core.Database.Types.Integer = 18;
        Title                    : Core.Database.Types.Integer = 19;
      end;
      Keys = class
      const
        ID                       : Core.Database.Types.VarString = 'TID';
        InsertID                 : Core.Database.Types.VarString = 'TIID';
        DomainID                 : Core.Database.Types.VarString = 'TDID';
        UserID                   : Core.Database.Types.VarString = 'TUID';
        ProjectID                : Core.Database.Types.VarString = 'PID';
        GroupID                  : Core.Database.Types.VarString = 'GID';
        Created                  : Core.Database.Types.VarString = 'TCTD';
        Modified                 : Core.Database.Types.VarString = 'MDFD';
        Due                      : Core.Database.Types.VarString = 'TDUE';
        Creator                  : Core.Database.Types.VarString = 'CRTR';
        Frequency                : Core.Database.Types.VarString = 'THZ';
        Priority                 : Core.Database.Types.VarString = 'PRTY';
        Status                   : Core.Database.Types.VarString = 'STS';
        Team                     : Core.Database.Types.VarString = 'TEAM';
        Admins                   : Core.Database.Types.VarString = 'ADMS';
        Files                    : Core.Database.Types.VarString = 'FLS';
        Comments                 : Core.Database.Types.VarString = 'CMTS';
        Caveats                  : Core.Database.Types.VarString = 'CAVS';
        Skills                   : Core.Database.Types.VarString = 'SKLS';
        Title                    : Core.Database.Types.VarString = 'TIT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Tasks';
        Value                    : 'scs_tsks';
        Hint                     : 'Tasks storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : array [0..19] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ProjectID; KeyP: @Keys.ProjectID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Due; KeyP: @Keys.Due; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Creator; KeyP: @Keys.Creator; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Frequency; KeyP: @Keys.Frequency; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Priority; KeyP: @Keys.Priority; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Status; KeyP: @Keys.Status; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Team; KeyP: @Keys.Team; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Admins; KeyP: @Keys.Admins; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Files; KeyP: @Keys.Files; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Comments; KeyP: @Keys.Comments; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Caveats; KeyP: @Keys.Caveats;  DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Skills; KeyP: @Keys.Skills; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );
      class function Add(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
      class function Read(Task: Core.Database.Types.TTask; var Entry:Item): boolean; overload;
      class function Refresh(Task: Core.Database.Types.TTask; var Entry:Item): boolean; overload;
      class function Write(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID, UserID,ItemID:QWord):boolean; overload;
      class function List(Task:Core.Database.Types.TTask; ListID:QWord; Kind:LongInt; Var Entries:List):boolean; overload;
      class function getSkills(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function setSkills(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function getCaveats(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function setCaveats(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function getStatus(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Integer): boolean;
      class function setStatus(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Integer): boolean;
    end;
    class procedure Init(var Entry:Item);  overload;
    class procedure Empty(var Entry:Item); overload;
    class procedure Done(var Entry:Item);  overload;

    class procedure Done(Var Entries:List); overload;
    class procedure Init(Var Entries:List); overload;
    class procedure Empty(var Entries:List); overload;
    class procedure Delete(Var Entry:Item; var Entries:List); overload;
    class function  setSize(var Entries:List; Count:LongInt): LongInt;
    class procedure setVerified(var Entries:List; Value:boolean);
    class function  IndexOf(var Entry:Item; var Entries:List): LongInt; overload;
    class function  IndexOf(var ID:QWord; var Entries:List): LongInt; overload;
    class function  getFirstUnverified(Var Entries:List): LongInt;

    class Function  fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
    class Function  toXML(var Entries:List; Output:TMemoryStream):boolean; overload;
    class Function  toXML(var Entry:Item; Output:TMemoryStream):boolean; overload;

  end;


implementation

uses DB;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Items.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED: PushDomainDeleted();
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


class procedure Items.Init(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    Created:=0;
    Due:=0;
    Verified:=false;
    Core.Arrays.LargeWord.Init(Team);
    Core.Arrays.LargeWord.Init(Admins);
    Core.Arrays.LargeWord.Init(Files);
    Core.Arrays.LargeWord.Init(Comments);
    Creator:=0;
    Priority:=Items.Defaults.Priority;
    Frequency:=Items.Defaults.Frequency;
    Status:=Items.Defaults.Status;
    SetLength(Caveats,0);
    SetLength(Skills,0);
    SetLength(Title,0);
  end;
end;

class procedure Items.Empty(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    Created:=0;
    Due:=0;
    Verified:=false;
    Core.Arrays.LargeWord.Empty(Team);
    Core.Arrays.LargeWord.Empty(Admins);
    Core.Arrays.LargeWord.Empty(Files);
    Core.Arrays.LargeWord.Empty(Comments);
    Creator:=0;
    Frequency:=Defaults.Frequency;
    Status:=Defaults.Status;
    SetLength(Caveats,0);
    SetLength(Skills,0);
    SetLength(Title,0);
  end;
end;

class procedure Items.Done(var Entry:Item);
begin
  With Entry do begin
    Core.Arrays.LargeWord.Done(Team);
    Core.Arrays.LargeWord.Done(Admins);
    Core.Arrays.LargeWord.Done(Files);
    Core.Arrays.LargeWord.Done(Comments);
    Finalize(Caveats);
    Finalize(Skills);
    Finalize(Title);
  end;
  Finalize(Entry);
end;

class procedure Items.Done(Var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;

class procedure Items.Init(Var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  System.SetLength(Entries,0);
end;

class procedure Items.Empty(var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  System.SetLength(Entries,0);
end;

class procedure Items.setVerified(var Entries:List; Value:boolean);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Entries[iLcv]^.Verified:=Value;
end;

class function Items.getFirstUnverified(Var Entries:List): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]^.Verified=false) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function Items.IndexOf(var ID:QWord; var Entries:List): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function Items.IndexOf(var Entry:Item; var Entries:List): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]=@Entry) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class procedure Items.Delete(Var Entry:Item; var Entries:List);
var
  iLcv:LongInt;
  iDX:LongInt;
  iCt:LongInt;
begin
  iDX:=IndexOf(Entry,Entries);
  if iDX<>-1 then begin
    iCt:=System.Length(Entries);
    For iLcv:=iDX to iCt-2 do
      Entries[iLcv]:=Entries[iLcv+1];
    SetLength(Entries,iCt-1);
  end;
end;

class function Items.SetSize(var Entries:List; Count:LongInt): LongInt;
var
  iCount:LongInt;
  iLcv:LongInt;
begin
  iCount:=System.Length(Entries);
  iLcv:=iCount;
  While (iLcv<Count) and (iLcv>0) do begin // shrink
    Done(Entries[iLcv-1]^);
    Dispose(Entries[iLcv-1]);
    Dec(iLcv);
    Dec(iCount);
  end;
  iLcv:=iCount;
  SetLength(Entries,Count);
  While (iLcv<Count) do begin // grow
    New(Entries[iLcv]);
    Init(Entries[iLcv]^);
    Inc(iLcv);
    Inc(iCount);
  end;
  Result:=iCount;
end;

class Function  Items.fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  with Core.XML.DB do begin
    xItem:=getNode(xDoc,XML.Stanzas.Task);
    if xItem<>nil then begin
      Entry.ID:=toQWord(xItem,XML.Fields.ID);
      Entry.Created:=toDouble(xItem,XML.Fields.Created);
      Entry.Modified:=toDouble(xItem,XML.Fields.Modified);
      Entry.Due:=toDouble(xItem,XML.Fields.Due);
      Entry.Creator:=toQWord(xItem,XML.Fields.Creator);
      Entry.Frequency:=toDouble(xItem,XML.Fields.Frequency);
      Entry.Priority:=toInteger(xItem,XML.Fields.Priority);
      Entry.Status:=toInteger(xItem,XML.Fields.Status);
      toQWordArray(xItem,XML.Fields.Team,Entry.Team);
      toQWordArray(xItem,XML.Fields.Admins,Entry.Admins);
      toQWordArray(xItem,XML.Fields.Files,Entry.Files);
      toQWordArray(xItem,XML.Fields.Comments,Entry.Comments);
      Entry.Caveats:=toString(xItem,XML.Fields.Caveats);
      Entry.Skills:=toString(xItem,XML.Fields.Skills);
      Entry.Title:=toString(xItem,XML.Fields.Title);
      Result:=True;
    end;
  end;
end;

class Function  Items.toXML(var Entries:List; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Tasks,Output);
  Core.Streams.Write('>',1,Output);
    for iLcv:=0 to High(Entries) do
    toXML(Entries[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Tasks,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

class Function  Items.toXML(var Entry:Item; Output:TMemoryStream):boolean;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Task,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Entry.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Entry.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Due,Entry.Due),Output);
    Core.Streams.Write(Print(XML.Fields.Creator,Entry.Creator),Output);
    Core.Streams.Write(Print(XML.Fields.Frequency,Entry.Frequency),Output);
    Core.Streams.Write(Print(XML.Fields.Priority,Entry.Priority),Output);
    Core.Streams.Write(Print(XML.Fields.Status,Entry.Status),Output);
    Core.Streams.Write(Print(XML.Fields.Team,Entry.Team),Output);
    Core.Streams.Write(Print(XML.Fields.Admins,Entry.Admins),Output);
    Core.Streams.Write(Print(XML.Fields.Files,Entry.Files),Output);
    Core.Streams.Write(Print(XML.Fields.Comments,Entry.Comments),Output);
    Core.Streams.Write(Print(XML.Fields.Caveats,Entry.Caveats,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Skills,Entry.Skills,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Entry.Title,CDATA_ON),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Task,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

class function Items.DB.Add(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iReset: QWord;
  iInsertID: QWord;
begin
  Result := False;
  iCount := 0;
  iReset := 0;
  iInsertID := Random(High(LongInt));
  Entry.Created:=Core.Timer.dtUT;
  Entry.Modified:=Entry.Created;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, IDs.ID, poNone, oNone, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, IDs.InsertID, poNone, oNone, iReset, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.UserID, poNone, oNone, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Created, poNone, oNone, Entry.Created, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Due, poNone, oNone, Entry.Due, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Team, poNone, oNone, Entry.Team, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Admins, poNone, oNone, Entry.Admins, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Files, poNone, oNone, Entry.Files, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Comments, poNone, oNone, Entry.Comments, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Creator, poNone, oNone, Entry.Creator, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Frequency, poNone, oNone, Entry.Frequency, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Status, poNone, oNone, Entry.Status, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Caveats, poNone, oNone, Entry.Caveats, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Skills, poNone, oNone, Entry.Skills, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Title, poNone, oNone, Entry.Title, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadTask(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Items.PItem(DataP)^ do begin
    Verified  := true;
    ID        := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    Created   := Fields.FieldByName(Items.DB.Keys.Created).AsFloat;
    Modified  := Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
    Due       := Fields.FieldByName(Items.DB.Keys.Due).AsFloat;
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Team).AsString,Team,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Admins).AsString,Admins,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Files).AsString,Files,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Comments).AsString,Comments,',');
    Creator   := Fields.FieldByName(Items.DB.Keys.Creator).AsLargeInt;
    Frequency := Fields.FieldByName(Items.DB.Keys.Frequency).AsFloat;
    Status    := Fields.FieldByName(Items.DB.Keys.Status).AsInteger;
    Caveats   := Fields.FieldByName(Items.DB.Keys.Caveats).AsString;
    Skills    := Fields.FieldByName(Items.DB.Keys.Skills).AsString;
    Title     := Fields.FieldByName(Items.DB.Keys.Title).AsString;
  end;
end;

class function Items.DB.Read(Task: Core.Database.Types.TTask; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=False;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    {$i Storage.Tasks.Read.Fields.inc}
    Result := ((Core.Database.SQL.Select(Task, @Commands, @cbReadTask, @Entry)=true) and (Entry.Verified=true));
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadTaskList(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Items.PList;
  iID:QWord;
  iDX:LongInt;
begin
  ListP:=DataP;
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iDX:=Items.IndexOf(iID,ListP^);
  if iDX=-1 then
    iDX:=Items.getFirstUnverified(ListP^);
  if iDX=-1 then begin
    iDX:=Length(ListP^);
    SetLength(ListP^,iDX+1);
    New(ListP^[iDX]);
    Items.Init(ListP^[iDX]^);
  end;
  cbReadTask(CommandsP,Fields,ListP^[iDX]);
end;

class function  Items.DB.List(Task:Core.Database.Types.TTask; ListID:QWord; Kind:LongInt; Var Entries:List):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    setVerified(Entries,False);
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    case Kind of
      Items.Kind.ProjectID : Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ProjectID, poNone, oEqual, ListID, Commands);
      Items.Kind.GroupID   : Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.GroupID, poNone, oEqual, ListID, Commands);
    end;
    {$i Storage.Tasks.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadTaskList, @Entries);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Refresh(Task: Core.Database.Types.TTask; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Modified, poAnd, oNotEqual, Entry.Modified, Commands);
    {$i Storage.Tasks.Read.Fields.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadTask, @Entry) and Entry.Verified);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Write(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Due, poNone, oNone, Entry.Due, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Team, poNone, oNone, Entry.Team, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Admins, poNone, oNone, Entry.Admins, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Files, poNone, oNone, Entry.Files, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Comments, poNone, oNone, Entry.Comments, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Frequency, poNone, oNone, Entry.Frequency, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Status, poNone, oNone, Entry.Status, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Caveats, poNone, oNone, Entry.Caveats, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Skills, poNone, oNone, Entry.Skills, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Title, poNone, oNone, Entry.Title, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID, UserID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbgetSkills(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Strings.PVarString(DataP)^ := Fields.FieldByName(Items.DB.Keys.Skills).AsString;
end;

class function Items.DB.getSkills(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Value, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Skills, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetSkills, @Value) and (Length(Value) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetCaveats(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Strings.PVarString(DataP)^ := Fields.FieldByName(Items.DB.Keys.Skills).AsString;
end;

class function Items.DB.getCaveats(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Value, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Caveats, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetCaveats, @Value) and (Length(Value) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetStatus(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PInteger(DataP)^ := Fields.FieldByName(Items.DB.Keys.Status).AsInteger;
end;

class function Items.DB.getStatus(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Integer): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Value:=Defaults.Status;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Status, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetStatus, @Value) and (Value <> Defaults.Status));
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Items.DB.setStatus(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Integer): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtModified: double;
begin
  Result := False;
  try
    iCount := 0;
    dtModified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, dtModified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Status, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setCaveats(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtModified: double;
begin
  Result := False;
  try
    iCount := 0;
    dtModified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, dtModified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Caveats, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Items.DB.setSkills(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  dtModified: double;
begin
  Result := False;
  try
    iCount := 0;
    dtModified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, dtModified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Skills, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


initialization
  RegisterDB;
end.


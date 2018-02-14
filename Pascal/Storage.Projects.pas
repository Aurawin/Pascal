unit Storage.Projects;

{
  unit Storage.Projects.pas

  Projects Database Module

  DBMS facilities to handle Projects for Spectrum

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

interface

uses
  Classes,
  RSR.Core,

  Core.Database,
  Core.Database.SQL,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,

  Core.XML,
  Core.Strings,
  Core.Timer,
  Core.Streams,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.CoreObjects,

  DOM,
  SysUtils;


type
  Items = class
  type
    PProject = ^TProject;
    PProjects= ^TProjects;
    TProjects=Array of PProject;
    TProject = record
      ID                       : QWord;
      UserID                   : QWord;
      GroupID                  : QWord;
      Created                  : double;
      Modified                 : double;
      Due                      : double;
      Frequency                : double;
      Priority                 : LongInt;
      Status                   : LongInt;
      Verified                 : boolean;
      Team                     : Core.Arrays.Types.LargeWord;
      Admins                   : Core.Arrays.Types.LargeWord;
      Files                    : Core.Arrays.Types.LargeWord;
      Comments                 : Core.Arrays.Types.LargeWord;
      Issues                   : Core.Arrays.Types.LargeWord;
      Caveats                  : Core.Strings.VarString;
      Skills                   : Core.Strings.VarString;
      Title                    : Core.Strings.VarString;
      Description              : Core.Strings.VarString;
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
      OneYear                  = 10;
      TwoYears                 = 11;
      ThreeYears               = 12;
      FourYears                = 13;
      FiveYears                = 14;
      Specific                 = 15; // Will provide ttl
    end;
    XML=class
    type
      Stanzas=class
      const
        Projects                 = 'projects';
        Project                  = 'project';
      end;
      Fields=class
      const
        ID                       = 'id';
        GroupID                  = 'gid';
        Created                  = 'created';
        Modified                 = 'modified';
        Due                      = 'due';
        Frequency                = 'hz';
        Priority                 = 'priority';
        Status                   = 'status';
        Team                     = 'team';
        Admins                   = 'admins';
        Files                    = 'files';
        Comments                 = 'comments';
        Issues                   = 'issues';
        Caveats                  = 'caveats';
        Skills                   = 'skills';
        Title                    = 'title';
        Description              = 'description';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        GroupID                  : Core.Database.Types.Integer = 4;
        Created                  : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        Due                      : Core.Database.Types.Integer = 7;
        Frequency                : Core.Database.Types.Integer = 8;
        Priority                 : Core.Database.Types.Integer = 9;
        Status                   : Core.Database.Types.Integer = 10;
        Team                     : Core.Database.Types.Integer = 11;
        Admins                   : Core.Database.Types.Integer = 12;
        Files                    : Core.Database.Types.Integer = 13;
        Comments                 : Core.Database.Types.Integer = 14;
        Issues                   : Core.Database.Types.Integer = 15;
        Caveats                  : Core.Database.Types.Integer = 16;
        Skills                   : Core.Database.Types.Integer = 17;
        Title                    : Core.Database.Types.Integer = 18;
        Description              : Core.Database.Types.Integer = 19;
      end;
      Keys = class
      const
        ID                       : Core.Database.Types.VarString = 'TID';
        InsertID                 : Core.Database.Types.VarString = 'TIID';
        DomainID                 : Core.Database.Types.VarString = 'TDID';
        UserID                   : Core.Database.Types.VarString = 'TUID';
        GroupID                  : Core.Database.Types.VarString = 'TGID';
        Created                  : Core.Database.Types.VarString = 'TCTD';
        Modified                 : Core.Database.Types.VarString = 'MDFD';
        Due                      : Core.Database.Types.VarString = 'TDUE';
        Frequency                : Core.Database.Types.VarString = 'THZ';
        Priority                 : Core.Database.Types.VarString = 'PRTY';
        Status                   : Core.Database.Types.VarString = 'STS';
        Team                     : Core.Database.Types.VarString = 'TEAM';
        Admins                   : Core.Database.Types.VarString = 'TADS';
        Files                    : Core.Database.Types.VarString = 'FLS';
        Comments                 : Core.Database.Types.VarString = 'CMTS';
        Issues                   : Core.Database.Types.VarString = 'IUES';
        Caveats                  : Core.Database.Types.VarString = 'CAVS';
        Skills                   : Core.Database.Types.VarString = 'SKLS';
        Title                    : Core.Database.Types.VarString = 'TIT';
        Description              : Core.Database.Types.VarString = 'DSCT';
      end;
      Defaults = class
      const
        Priority                 = Priority.Normal;
        Status                   = Status.Untouched;
        Frequency                = Frequency.Once;
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Projects';
        Value                    : 'scs_prjs';
        Hint                     : 'Projects storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : array [0..19] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Due; KeyP: @Keys.Due; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Frequency; KeyP: @Keys.Frequency; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Priority; KeyP: @Keys.Priority; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Status; KeyP: @Keys.Status; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Team; KeyP: @Keys.Team; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Admins; KeyP: @Keys.Admins; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Files; KeyP: @Keys.Files; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Comments; KeyP: @Keys.Comments; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Issues; KeyP: @Keys.Issues; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Caveats; KeyP: @Keys.Caveats; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Skills; KeyP: @Keys.Skills; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function getIssues(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
      class function setIssues(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;

      class function getComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
      class function setComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
      class function getSkills(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function setSkills(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function getCaveats(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function setCaveats(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Core.Strings.VarString): boolean;
      class function getStatus(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Integer): boolean;
      class function setStatus(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Integer): boolean;

      class function Granted(Task:Core.Database.Types.TTask; UserID,ItemID:QWord):boolean;

      class function Add(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Item: TProject): boolean;
      class function Read(Task: Core.Database.Types.TTask; var Item: TProject): boolean;
      class function Refresh(Task: Core.Database.Types.TTask; var Item: TProject): boolean;
      class function Write(Task: Core.Database.Types.TTask; var Item: TProject): boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID, UserID, ItemID:QWord):boolean;
      class function List(Task:Core.Database.Types.TTask; DomainID,GroupID:QWord; var Items:TProjects):boolean;
      class function getAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
      class function setAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;

    end;

    class procedure Init(var Item: TProject);  overload;
    class procedure Empty(var Item: TProject); overload;
    class procedure Done(var Item: TProject);  overload;
    class procedure Done(var Item: TProjects);  overload;

    class function IndexOf(ID:QWord; var Items:TProjects): LongInt;
    class function getFirstUnverified(var Items:TProjects): LongInt;
    class procedure setVerified(var Items:TProjects; value:boolean);


    class function fromXML(xDoc:TXMLDocument; var Item:TProject):boolean;
    class function toXML(var Item:TProjects; Output:TMemoryStream):boolean; overload;
    class function toXML(var Item:TProject; Output:TMemoryStream):boolean; overload;


  end;

implementation

uses DB;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
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
  With Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


class procedure Items.Init(var Item: TProject);
begin
  With Item do begin
    ID:=0;
    UserID:=0;
    GroupID:=0;
    Created:=0;
    Modified:=0;
    Due:=0;
    Frequency:=Items.DB.Defaults.Frequency;
    Priority:=Items.DB.Defaults.Priority;
    Status:=Items.DB.Defaults.Status;
    Verified:=false;
    Core.Arrays.LargeWord.Init(Team);
    Core.Arrays.LargeWord.Init(Admins);
    Core.Arrays.LargeWord.Init(Files);
    Core.Arrays.LargeWord.Init(Comments);
    Core.Arrays.LargeWord.Init(Issues);
    SetLength(Caveats,0);
    SetLength(Skills,0);
    SetLength(Title,0);
    SetLength(Description,0);
  end;
end;

class procedure Items.Empty(var Item: TProject);
begin
  With Item do begin
    ID:=0;
    UserID:=0;
    GroupID:=0;
    Created:=0;
    Modified:=0;
    Due:=0;
    Frequency:=Items.DB.Defaults.Frequency;
    Priority:=Items.DB.Defaults.Priority;
    Status:=Items.DB.Defaults.Status;
    Verified:=false;
    Core.Arrays.LargeWord.Empty(Team);
    Core.Arrays.LargeWord.Empty(Admins);
    Core.Arrays.LargeWord.Empty(Files);
    Core.Arrays.LargeWord.Empty(Comments);
    Core.Arrays.LargeWord.Empty(Issues);
    SetLength(Caveats,0);
    SetLength(Skills,0);
    SetLength(Title,0);
    SetLength(Description,0);
  end;
end;

class procedure Items.Done(var Item: TProject);
begin
  With Item do begin
    Core.Arrays.LargeWord.Done(Team);
    Core.Arrays.LargeWord.Done(Admins);
    Core.Arrays.LargeWord.Done(Files);
    Core.Arrays.LargeWord.Done(Comments);
    Core.Arrays.LargeWord.Done(Issues);
    Finalize(Caveats);
    Finalize(Skills);
    Finalize(Title);
    Finalize(Description);
  end;
  Finalize(Item);
end;

class procedure Items.Done(var Item:TProjects);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Items.setVerified(var Items:TProjects; value:boolean);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do
    Items[iLcv]^.Verified:=Value;
end;

class function Items.IndexOf(ID:QWord; var Items:TProjects): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Items) do begin
    if (Items[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class function Items.getFirstUnverified(var Items:TProjects): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Items) do begin
    if Items[iLcv]^.Verified=false then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class function Items.DB.Granted(Task:Core.Database.Types.TTask; UserID,ItemID:QWord):boolean;
var
  Items:Core.Arrays.Types.LargeWord;
begin
  Try
    Result:=getAdmins(Task,ItemID,Items);
    if Result then
        Result:=(Core.Arrays.LargeWord.IndexOf(UserID,Items)<>-1);
  Finally
    Core.Arrays.LargeWord.Done(Items);
  end;
end;


class function Items.DB.Add(Task: Core.Database.Types.TTask; DomainID, UserID: QWord; var Item: TProject): boolean;
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
  Item.Created:=Core.Timer.dtUT;
  Item.Modified:=Item.Created;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, Items.DB.IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, Items.DB.IDs.ID, poNone, oNone, Item.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, Items.DB.IDs.InsertID, poNone, oNone, iReset, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.UserID, poNone, oNone, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.GroupID, poNone, oNone, Item.GroupID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Created, poNone, oNone, Item.Created, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Due, poNone, oNone, Item.Due, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Team, poNone, oNone, Item.Team, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Admins, poNone, oNone, Item.Admins, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Files, poNone, oNone, Item.Files, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Comments, poNone, oNone, Item.Comments, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Issues, poNone, oNone, Item.Issues, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Frequency, poNone, oNone, Item.Frequency, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Status, poNone, oNone, Item.Status, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Caveats, poNone, oNone, Item.Caveats, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Skills, poNone, oNone, Item.Skills, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Title, poNone, oNone, Item.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, Items.DB.IDs.Description, poNone, oNone, Item.Description, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadProject(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Items.PProject(DataP)^ do begin
    Verified    := true;
    ID          := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    GroupID     := Fields.FieldByName(Items.DB.Keys.GroupID).AsLargeInt;
    Created     := Fields.FieldByName(Items.DB.Keys.Created).AsFloat;
    Modified    := Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
    Due         := Fields.FieldByName(Items.DB.Keys.Due).AsFloat;
    Frequency   := Fields.FieldByName(Items.DB.Keys.Frequency).AsFloat;
    Priority    := Fields.FieldByName(Items.DB.Keys.Priority).AsInteger;
    Status      := Fields.FieldByName(Items.DB.Keys.Status).AsInteger;
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Team).AsString,Team,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Admins).AsString,Admins,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Files).AsString,Files,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Comments).AsString,Comments,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Issues).AsString,Issues,',');
    Caveats     := Fields.FieldByName(Items.DB.Keys.Caveats).AsString;
    Skills      := Fields.FieldByName(Items.DB.Keys.Skills).AsString;
    Title       := Fields.FieldByName(Items.DB.Keys.Title).AsString;
    Description := Fields.FieldByName(Items.DB.Keys.Description).AsString;
  end;
end;

class function Items.DB.Read(Task: Core.Database.Types.TTask; var Item: TProject): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, Items.DB.IDs.ID, poNone, oEqual, Item.ID, Commands);

    {$i Storage.Projects.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadProject, @Item) and (Item.Verified));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Refresh(Task: Core.Database.Types.TTask; var Item: TProject): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, Items.DB.IDs.ID, poNone, oEqual, Item.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, Items.DB.IDs.Modified, poAnd, oNotEqual, Item.Modified, Commands);

    {$i Storage.Projects.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadProject, @Item) and (Item.Verified));
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetAdmins(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Admins).AsString,Core.Arrays.Types.PLargeWord(DataP)^,',');
end;

class function Items.DB.getAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Items, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Admins, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetAdmins, @Items) and (Length(Items) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Admins, poNone, oNone, Items, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbProjectList(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Items.PProjects;
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
  cbReadProject(CommandsP,Fields,ListP^[iDX]);
end;

class function  Items.DB.List(Task:Core.Database.Types.TTask; DomainID,GroupID:QWord; Var Items:TProjects):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    setVerified(Items,False);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.GroupID, poAnd, oEqual, GroupID, Commands);
    {$i Storage.Projects.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbProjectList, @Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID, UserID, ItemID:QWord):boolean;
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
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbgetIssues(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Issues).AsString,Core.Arrays.Types.PLargeWord(DataP)^,',');
end;


class function Items.DB.getIssues(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Items, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Issues, poNone, oNone, Commands);
    Result := Core.Database.SQL.Select(Task, @Commands,@cbgetIssues, @Items) ;
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbgetComments(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Comments).AsString,Core.Arrays.Types.PLargeWord(DataP)^,',');
end;


class function Items.DB.getComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Arrays.LargeWord.Empty(Items);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Comments, poNone, oNone, Commands);
    Result := Core.Database.SQL.Select(Task, @Commands,@cbgetComments, @Items) ;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Comments, poNone, oNone, Items, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setIssues(Task:Core.Database.Types.TTask; ItemID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Issues, poNone, oNone, Items, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Items.DB.Write(Task: Core.Database.Types.TTask; var Item: TProject): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Due, poNone, oNone, Item.Due, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Team, poNone, oNone, Item.Team, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Admins, poNone, oNone, Item.Admins, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Files, poNone, oNone, Item.Files, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Frequency, poNone, oNone, Item.Frequency, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Status, poNone, oNone, Item.Status, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Caveats, poNone, oNone, Item.Caveats, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Skills, poNone, oNone, Item.Skills, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Title, poNone, oNone, Item.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Description, poNone, oNone, Item.Description, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetSkills(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PString(DataP)^ := Fields.FieldByName(Items.DB.Keys.Skills).AsString;
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

class Function  Items.fromXML(xDoc:TXMLDocument; var Item:TProject):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Project);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.GroupID:=toQWord(xItem,XML.Fields.GroupID);
      Item.Created:=toDouble(xItem,XML.Fields.Created);
      Item.Modified:=toDouble(xItem,XML.Fields.Modified);
      Item.Due:=toDouble(xItem,XML.Fields.Due);
      Item.Frequency:=toDouble(xItem,XML.Fields.Frequency);
      Item.Priority:=toInteger(xItem,XML.Fields.Priority);
      Item.Status:=toInteger(xItem,XML.Fields.Status);
      toQWordArray(xItem,XML.Fields.Team,Item.Team);
      toQWordArray(xItem,XML.Fields.Admins,Item.Admins);
      toQWordArray(xItem,XML.Fields.Files,Item.Files);
      toQWordArray(xItem,XML.Fields.Comments,Item.Comments);
      toQWordArray(xItem,XML.Fields.Issues,Item.Issues);
      Item.Caveats:=toString(xItem,XML.Fields.Caveats);
      Item.Skills:=toString(xItem,XML.Fields.Skills);
      Item.Title:=toString(xItem,XML.Fields.Title);
      Item.Description:=toString(xItem,XML.Fields.Description);
      Result:=True;
    end;
  end;
end;

class Function  Items.toXML(var Item:TProjects; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Projects,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Projects,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class Function  Items.toXML(var Item:TProject; Output:TMemoryStream):boolean;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Project,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.GroupID,Item.GroupID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Due,Item.Due),Output);
    Core.Streams.Write(Print(XML.Fields.Frequency,Item.Frequency),Output);
    Core.Streams.Write(Print(XML.Fields.Priority,Item.Priority),Output);
    Core.Streams.Write(Print(XML.Fields.Status,Item.Status),Output);
    Core.Streams.Write(Print(XML.Fields.Team,Item.Team),Output);
    Core.Streams.Write(Print(XML.Fields.Admins,Item.Admins),Output);
    Core.Streams.Write(Print(XML.Fields.Files,Item.Files),Output);
    Core.Streams.Write(Print(XML.Fields.Comments,Item.Comments),Output);
    Core.Streams.Write(Print(XML.Fields.Issues,Item.Issues),Output);
    Core.Streams.Write(Print(XML.Fields.Caveats,Item.Caveats,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Skills,Item.Skills,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Item.Title,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Item.Description,CDATA_ON),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Project,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

initialization
  RegisterDB;
end.


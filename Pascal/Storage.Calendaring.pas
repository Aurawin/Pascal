unit Storage.Calendaring;
{
 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

interface

uses

  Core.Utils.Time,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.Strings,
  Core.Timer,
  Classes,
  SysUtils;

Type
  Items=class
  Type
    Index=class
    const
       Previous              = 0;
       Current               = 1;
       Next                  = 2;
       Events                = 3;
    end;
    Event=record
      ID                           : Core.Database.Types.LargeWord;
      DomainID                     : Core.Database.Types.LargeWord;
      dtEvent                      : Core.Database.Types.Double;
      dtModified                   : Core.Database.Types.Double;
      dtPublished                  : Core.Database.Types.Double;
      Title                        : Core.Strings.VarString;
      Description                  : Core.Strings.VarString;
      Notes                        : Core.Strings.VarString;
      URL                          : Core.Strings.VarString;
      Coordinator                  : Core.Strings.VarString;
      CoordinatorIM                : Core.Strings.VarString;
      CoordinatorEM                : Core.Strings.VarString;
      CoordinatorPH                : Core.Strings.VarString;
    end;
    Events=Array of Event;
    PEvents=^Events;
    ConfigItem=record
      TC                           : Core.Strings.VarString;
      THC                          : Core.Strings.VarString;
      TRC                          : Core.Strings.VarString;
      TSC                          : Core.Strings.VarString;
      TDC                          : Core.Strings.VarString;
      TLC                          : Core.Strings.VarString;
      TIC                          : Core.Strings.VarString;
      TFC                          : Core.Strings.VarString;
    end;
    PConfigItem=^ConfigItem;
    Stamp=record
      iYear                        : Core.Database.Types.Word;
      iMonth                       : Core.Database.Types.Word;
      iDay                         : Core.Database.Types.Word;
      iHour                        : Core.Database.Types.Word;
      iMinute                      : Core.Database.Types.Word;
      iSecond                      : Core.Database.Types.Word;
      iMillisecond                 : Core.Database.Types.Word;
    end;
    Stamps=Array[0..2] of Stamp;
    Config=Array[0..3] of ConfigItem;
    PConfig=^Config;
    Page=record
      Description                  : Core.Strings.VarString;
      Title                        : Core.Strings.VarString;
      Content                      : Core.Strings.VarString;
      Blurb                        : Core.Strings.VarString;
    end;
    Pages=Array[0..4] of Page;

    DB=class
    Type
      IDS=class
      const
        ID                       : LongInt = 0;
        InsertID                 : LongInt = 1;
        DomainID                 : LongInt = 2;
        Title                    : LongInt = 3;
        Description              : LongInt = 4;
        dtEvent                  : LongInt = 5;
        dtPublished              : LongInt = 6;
        dtModified               : LongInt = 7;
        Notes                    : LongInt = 8;
        URL                      : LongInt = 9;
        Coordinator              : LongInt = 10;
        CoordinatorIM            : LongInt = 11;
        CoordinatorEM            : LongInt = 12;
        CoordinatorPH            : LongInt = 13;
      end;
      Keys=class
      const
        ID                       : Core.Strings.VarString = 'ITMID';
        InsertID                 : Core.Strings.VarString = 'ITMIID';
        DomainID                 : Core.Strings.VarString = 'ITMDID';
        Title                    : Core.Strings.VarString = 'ITMTIT';
        Description              : Core.Strings.VarString = 'ITMDES';
        dtEvent                  : Core.Strings.VarString = 'ITMDTS';
        dtPublished              : Core.Strings.VarString = 'ITMPDTS';
        dtModified               : Core.Strings.VarString = 'ITMMDTS';
        Notes                    : Core.Strings.VarString = 'ITMN';
        URL                      : Core.Strings.VarString = 'ITMURL';
        Coordinator              : Core.Strings.VarString = 'ITMCORD';
        CoordinatorIM            : Core.Strings.VarString = 'COORD_IM';
        CoordinatorEM            : Core.Strings.VarString = 'COORD_EM';
        CoordinatorPH            : Core.Strings.VarString = 'COORD_PH';
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Events';
        Value                    : 'scs_clevnts';
        Hint                     : 'Storage for domain based calendar events';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..13] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP:@Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP:@Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP:@Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP:@Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNotNull; ),
        (IDP: @IDs.Description; KeyP:@Keys.Description; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*256; Flags: cfNone; ),
        (IDP: @IDs.dtEvent; KeyP:@Keys.dtEvent; DataType: dftDateTime; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.dtPublished; KeyP:@Keys.dtPublished; DataType: dftDateTime; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.dtModified; KeyP:@Keys.dtModified; DataType: dftDateTime; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Notes; KeyP:@Keys.Notes; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*256; Flags: cfNone; ),
        (IDP: @IDs.URL; KeyP:@Keys.URL; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Coordinator; KeyP:@Keys.Coordinator; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.CoordinatorIM; KeyP:@Keys.CoordinatorIM; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.CoordinatorEM; KeyP:@Keys.CoordinatorEM; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.CoordinatorPH; KeyP:@Keys.CoordinatorPH; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class Function  Fill(Task:Core.Database.Types.TTask; DomainID:System.QWord; var Entries:Events):System.Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Entry:Event; Var Entries:Events):System.Boolean;
      class Function  Edit(Task:Core.Database.Types.TTask; Var Entry:Event):System.Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Var Entry:Event; Var Entries:Events):System.Boolean;
    end;



    class Function  IndexOf(Var Entries:Events; ID:LongInt):LongInt;

    class procedure Empty(Var Entries:Events); overload;
    class procedure Empty(Var Entry:Event); overload;
    class procedure Empty(Var Entries:Config); overload;
    class procedure Empty(Var Entry:ConfigItem); overload;
    class procedure Empty(Var Entry:Page); overload;

    class procedure Init(Var Entries:Events); overload;
    class procedure Init(Var Entries:Config); overload;
    class procedure Init(Var Entry:Page); overload;

    class procedure Done(var Entry:Event); overload;
    class procedure Done(var Entries:Events); overload;
    class procedure Done(var Entries:Config); overload;
    class procedure Done(var Entry:ConfigItem); overload;
    class procedure Done(Var Entry:Page); overload;

    class procedure Add(Var Entry:Event; Var Entries:Events); overload;

    class procedure Copy(Var Source,Destination:Event); overload;
    class procedure Copy(Var Source,Destination:ConfigItem); overload;

    class Function  EventsContainedInDay(Var Entries:Events; dtStamp:TDateTime):LongInt;
    class Function  BuildCalendar(Var cfg:Config; Var Entries:Events; dtStamp:TDateTime; Const Kind:Byte):Core.Strings.VarString;
  end;

implementation
uses
  Storage,
  db,
  sqldb,
  DateUtils;
Const
  Calendar_Page_Events           = 0;
  Calendar_Page_Edit             = 1;
  Calendar_Page_Add              = 2;
  Calendar_Page_Delete           = 3;
  Calendar_Page_Calendar         = 4;
  Calender_Commands              : Array[0..4] of Core.Strings.VarString=('events','edit','add','delete','calendar');

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        SetLength(Commands,0);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
  end;
end;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

procedure CB_CalendarEvents_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
var
  iIndex,iID,iLcv,iLen:LongInt;
  EventsP:Items.PEvents;
begin
  EventsP:=DataP;
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iLcv:=0; iLen:=Length(EventsP^); iIndex:=-1;
  While (iLcv<iLen) and (iIndex=-1) do begin
    If EventsP^[iLcv].ID=iID then
      iIndex:=iLcv;
    Inc(iLcv);
  end;
  If iIndex=-1 then begin
    SetLength(EventsP^,iLen+1);
    iIndex:=iLen;
  end;
  EventsP^[iIndex].ID:=iID;
  EventsP^[iIndex].DomainID:=Fields.FieldByName(Items.DB.Keys.DomainID).AsLargeInt;
  EventsP^[iIndex].dtEvent:=Fields.FieldByName(Items.DB.Keys.dtEvent).AsDateTime;
  EventsP^[iIndex].dtPublished:=Fields.FieldByName(Items.DB.Keys.dtPublished).AsDateTime;
  EventsP^[iIndex].dtModified:=Fields.FieldByName(Items.DB.Keys.dtModified).AsDateTime;

  EventsP^[iIndex].Title:=Fields.FieldByName(Items.DB.Keys.Title).AsString;
  EventsP^[iIndex].Description:=Fields.FieldByName(Items.DB.Keys.Description).AsString;
  EventsP^[iIndex].Notes:=Fields.FieldByName(Items.DB.Keys.Notes).AsString;
  EventsP^[iIndex].URL:=Fields.FieldByName(Items.DB.Keys.URL).AsString;

  EventsP^[iIndex].Coordinator:=Fields.FieldByName(Items.DB.Keys.Coordinator).AsString;
  EventsP^[iIndex].CoordinatorIM:=Fields.FieldByName(Items.DB.Keys.CoordinatorIM).AsString;
  EventsP^[iIndex].CoordinatorEM:=Fields.FieldByName(Items.DB.Keys.CoordinatorEM).AsString;
  EventsP^[iIndex].CoordinatorPH:=Fields.FieldByName(Items.DB.Keys.CoordinatorPH).AsString;
end;

class Function Items.DB.Fill(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entries:Events):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.dtEvent,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.dtModified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.dtPublished,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Notes,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.URL,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Coordinator,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.CoordinatorIM,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.CoordinatorEM,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.CoordinatorPH,poNone,oNone,Commands);

    Core.Database.SQL.Select(Task,@Commands,@CB_CalendarEvents_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entry:Event; Var Entries:Events):Boolean;
var
  iCount,iInsertID,iReset:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;

  iInsertID:=Random(High(Integer)); iReset:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Setup Parameters
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.dtEvent,poNone,oNone,Entry.dtEvent,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.dtPublished,poNone,oNone,Entry.dtPublished,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.dtModified,poNone,oNone,Entry.dtModified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Notes,poNone,oNone,Entry.Notes,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.URL,poNone,oNone,Entry.URL,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Coordinator,poNone,oNone,Entry.Coordinator,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.CoordinatorIM,poNone,oNone,Entry.CoordinatorIM,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.CoordinatorEM,poNone,oNone,Entry.CoordinatorEM,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.CoordinatorPH,poNone,oNone,Entry.CoordinatorPH,Commands);

    Result:= Core.Database.SQL.Insert(Task,@Commands);
    If Result then begin
      Entry.URL:=Concat('/core/calendar.srs?',IntToStr(Entry.ID));
      iCount:=0;
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForValues,IDs.URL,poNone,oNone,Entry.URL,Commands);

      Core.Database.SQL.Update(Task,@Commands);

      Items.Add(Entry,Entries);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Edit(Task:Core.Database.Types.TTask; Var Entry:Event):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Entry.dtModified:=Core.Timer.dtUT;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Notes,poNone,oNone,Entry.Notes,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.URL,poNone,oNone,Entry.URL,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Coordinator,poNone,oNone,Entry.Coordinator,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CoordinatorIM,poNone,oNone,Entry.CoordinatorIM,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CoordinatorEM,poNone,oNone,Entry.CoordinatorEM,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.CoordinatorPH,poNone,oNone,Entry.CoordinatorPH,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Event; Var Entries:Events):Boolean;
var
  iLcv,iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Entry.dtModified:=Core.Timer.dtUT;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    If Result then begin
      iCount:=Length(Entries);
      For iLcv:=0 to iCount-2 do
        Copy(Entries[iLcv+1],Entries[iLcv]);
      Done(Entries[iCount-1]);
      SetLength(Entries,iCount-1);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.IndexOf(Var Entries:Events; ID:LongInt):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Entries) do begin
    If Entries[iLcv].ID=ID then begin
      Result:=iLcv;
      break;
    end;
  end;
end;


class procedure Items.Empty(Var Entries:Events);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Empty(Var Entry:Event);
begin
  With Entry do begin
    SetLength(Title,0);
    SetLength(Description,0);
    SetLength(Notes,0);
    SetLength(URL,0);
    SetLength(Coordinator,0);
    SetLength(CoordinatorIM,0);
    SetLength(CoordinatorEM,0);
    SetLength(CoordinatorPH,0);
  end;
end;

class procedure Items.Empty(Var Entries:Config);
var
  iLcv:LongInt;
begin
  for iLcv := 0 to High(Entries) do
    Empty(Entries[iLcv]);
end;

class procedure Items.Empty(Var Entry:ConfigItem);
begin
  With Entry do begin
    SetLength(TC,0);
    SetLength(THC,0);
    SetLength(TRC,0);
    SetLength(TSC,0);
    SetLength(TDC,0);
    SetLength(TLC,0);
    SetLength(TIC,0);
    SetLength(TFC,0);
  end;
end;

class procedure Items.Empty(Var Entry:Page);
begin
  With Entry do begin
    SetLength(Blurb,0);
    SetLength(Content,0);
    SetLength(Description,0);
    SetLength(Title,0);
  end;
end;

class procedure Items.Done(var Entries:Config);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(Entries) to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Items.Done(Var Entry:Page);
begin
  With Entry do begin
    Finalize(Title);
    Finalize(Description);
    Finalize(Content);
    Finalize(Blurb);
  end;
  Finalize(Entry);
end;

class procedure Items.Done(var Entry:ConfigItem);
begin
  With Entry do begin
    Finalize(TC);
    Finalize(THC);
    Finalize(TRC);
    Finalize(TSC);
    Finalize(TDC);
    Finalize(TLC);
    Finalize(TIC);
    Finalize(TFC);
  end;
  Finalize(Entry);
end;

class procedure Items.Init(Var Entries:Events);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Init(Var Entries:Config);
var
  iLcv:LongInt;
begin
  for iLcv := 0 to High(Entries) do
    Empty(Entries[iLcv]);
end;

class procedure Items.Init(Var Entry:Page);
begin
  With Entry do begin
    SetLength(Blurb,0);
    SetLength(Content,0);
    SetLength(Description,0);
    SetLength(Title,0);
  end;
end;

class procedure Items.Done(var Entry:Event);
begin
  With Entry do begin
    Finalize(Title);
    Finalize(Description);
    Finalize(Notes);
    Finalize(URL);
    Finalize(Coordinator);
    Finalize(CoordinatorIM);
    Finalize(CoordinatorEM);
    Finalize(CoordinatorPH);
  end;
  Finalize(Entry);
end;

class procedure Items.Done(var Entries:Events);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(Entries) to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Items.Add(Var Entry:Event; Var Entries:Events);
var
  iCount:LongInt;
begin
  iCount:=Length(Entries);
  SetLength(Entries,iCount+1);
  Copy(Entry,Entries[iCount]);
end;

class procedure Items.Copy(Var Source,Destination:Event);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.dtEvent:=Source.dtEvent;
  Destination.dtModified:=Source.dtModified;
  Destination.dtPublished:=Source.dtPublished;

  Destination.Title:=Source.Title;
  Destination.Description:=Source.Description;
  Destination.Notes:=Source.Notes;
  Destination.URL:=Source.URL;
  Destination.Coordinator:=Source.Coordinator;
  Destination.CoordinatorIM:=Source.CoordinatorIM;
  Destination.CoordinatorEM:=Source.CoordinatorEM;
  Destination.CoordinatorPH:=Source.CoordinatorPH;
end;

class procedure Items.Copy(Var Source,Destination:ConfigItem);
begin
  Destination.TC:=Source.TC;
  Destination.THC:=Source.THC;
  Destination.TRC:=Source.TRC;
  Destination.TSC:=Source.TSC;
  Destination.TDC:=Source.TDC;
  Destination.TLC:=Source.TLC;
  Destination.TIC:=Source.TIC;
  Destination.TFC:=Source.TFC;
end;

class Function  Items.EventsContainedInDay(Var Entries:Events; dtStamp:TDateTime):LongInt;
Var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=Low(Entries) to High(Entries) do begin
    If DateUtils.IsSameDay(dtStamp,Entries[iLcv].dtEvent) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class Function  Items.BuildCalendar(Var cfg:Config; Var Entries:Events; dtStamp:TDateTime; Const Kind:Byte):Core.Strings.VarString;
var
  iTotal                         : LongInt;
  iCount                         : LongInt;
  iLcv                           : LongInt;
  dtCurrent                      : TDateTime;
  sM,sTu,sW,sTh,sF,sSat,sSun     : Core.Strings.VarString;
  CS                             : Stamp;

  Function GetDateTableDef(var dtLookup:TDateTime):Core.Strings.VarString;
  Var
    sDayOf                       : Core.Strings.VarString;
    iDayOf                       : LongInt;
    iLoc                         : LongInt;
    LocalCS                      : Stamp;
  begin
    iDayOf:=DayOf(dtLookup);
    If (iDayOf=DayOf(Now)) and (MonthOf(dtLookup)=MonthOf(Now)) then
      sDayOf:=Concat('<b>',IntToStr(iDayOf),'</b>')
    else
      sDayOf:=IntToStr(iDayOf);
    iLoc:=EventsContainedInDay(Entries,dtLookup);
    If iLoc>-1 then begin
      DecodeDateTime(dtLookup,LocalCS.iYear,LocalCS.iMonth,LocalCS.iDay,LocalCS.iHour,LocalCS.iMinute,LocalCS.iSecond,LocalCS.iMillisecond);
      Result:=Concat('<a class="',cfg[Kind].TLC,'" alt="',Entries[iLoc].Title,'" href="/core/calendar.co?',
        Calender_Commands[Calendar_Page_Events],'?',IntToStr(LocalCS.iYear),'?',IntToStr(LocalCS.iMonth),'?',IntToStr(LocalCS.iDay),'?">',
        sDayOf,'</a>'
      );
    end else
      Result:=sDayOf;
    inc(iTotal);
    dtLookup:=IncDay(dtLookup,1);
  end;

  procedure PushLoop;
  var
    iLcv:LongInt;
  begin
    For iLcv:=1 to 7 do
      Result:=Concat(Result,'<td class="',cfg[Kind].TDC,'">',GetDateTableDef(dtCurrent),'</td>');
    Result:=Concat(Result,'</tr><tr class="',cfg[Kind].TRC,'">');
  end;


begin
  Result:=Concat('<table class="',cfg[Kind].TC,'">');
  DecodeDateTime(dtStamp,CS.iYear,CS.iMonth,CS.iDay,CS.iHour,CS.iMinute,CS.iSecond,CS.iMillisecond);
  sM:='M';
  sTu:='T';
  sW:='W';
  sTh:='T';
  sF:='F';
  sSat:='S';
  sSun:='S';
  Case DayOfWeek(dtStamp) of
  1: sSun:='<b>S</b>';
  2: sM:='<b>M</b>';
  3: sTu:='<b>T</b>';
  4: sW:='<b>W</b>';
  5: sTh:='<b>T</b>';
  6: sF:='<b>F</b>';
  7: sSat:='<b>S</b>';
  end;
  Result:=Concat(Result,
    '<tr class="',cfg[Kind].TRC,'">',
    '<th class="',cfg[Kind].THC,'" colspan=7>',Core.Utils.Time.Month_Long[CS.iMonth],' ',IntToStr(CS.iYear),'</th>',
    '</tr><tr class="',cfg[Kind].TRC,'">',
    '<td class="',cfg[Kind].TDC,'">',sSun,'</td>','<td class="',cfg[Kind].TDC,'">',sM,'</td>',
    '<td class="',cfg[Kind].TDC,'">',sTu,'</td>','<td class="',cfg[Kind].TDC,'">',sW,'</td>',
    '<td class="',cfg[Kind].TDC,'">',sTh,'</td>','<td class="',cfg[Kind].TDC,'">',sF,'</td>',
    '<td class="',cfg[Kind].TDC,'">',sSat,'</td></tr><tr class="',cfg[Kind].TRC,'">'
  );
  // dtNext:=StartOfTheMonth(IncMonth(dtStamp,1));
  dtCurrent:=StartOfTheMonth(dtStamp);
  // dtPrior:=StartOfTheMonth(IncMonth(dtStamp,-1));
  iCount:=DaysInMonth(dtStamp);
  iTotal:=0;
  // Week #1
  For iLcv:=1 to DayOfWeek(dtCurrent)-1 do
    Result:=Concat(Result,'<td class="',cfg[Kind].TDC,'">&nbsp;</td>');
  For iLcv:=DayOfWeek(dtCurrent) to 7 do
    Result:=Concat(Result,'<td class="',cfg[Kind].TDC,'">',GetDateTableDef(dtCurrent),'</td>');
  Result:=Concat(Result,'</tr><tr class="',cfg[Kind].TRC,'">');
  For iLcv:=1 to 3 do
   PushLoop;
  iLcv:=1;
  While (iTotal<iCount) do begin
    If iLcv=8 then begin
      Result:=Concat(Result,'</tr><tr class="',cfg[Kind].TRC,'">');
      iLcv:=1;
    end;
    Result:=Concat(Result,'<td class="',cfg[Kind].TDC,'">',GetDateTableDef(dtCurrent),'</td>');
    inc(iLcv);
  end;
  For iLcv:=iCount to 30 do
    Result:=Concat(Result,'<td class="',cfg[Kind].TDC,'">&nbsp;</td>');
  Result:=Concat(Result,'</tr><tr class="',cfg[Kind].TRC,'"><td class="',cfg[Kind].TDC,'" colspan=7></td></tr></table>');
end;


initialization
  RegisterDB;
end.


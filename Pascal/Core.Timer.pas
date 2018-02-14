unit Core.Timer;


interface

uses
  Classes,
  SysUtils,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Utils.Time,
  Core.Arrays.VarString,
  Core.System;

Const
  AddToStack                     = false;
  UpdateStack                    = true;
  UnloadNoExecute                = false;
  UnloadExecute                  = true;
  LoadNoUpdate                   = false;
  FMT_LOG_SERVICE                = '%s.%s';
  FMT_LOG_SERVICE_ITEM           = '%s.%s timer was removed:%s';
  MEM_DIVISOR                    = 1048576;
  TIMER_STACK_SIZE               = 1024*1024;
  MEM_THRESHOLD:WORD             = 200;
  MEM_TIMER_SECONDS:WORD         = 3;
  MEM_PACK_COUNT:WORD            = 100;
  TIMER_DEF_DELAY_MS_SMALL       = 500;
  TIMER_DEF_DELAY_MS_TYPE        = 1000;

Type
  PItem=^Item;
  TimerError=Procedure(Location,Error:ShortString) of Object;
  TimerEvent=Procedure(ItemP:PItem) of Object;
  TimerEventMode=(temNormal,temSynchronize);
  Item=Record
    Mode                         : TimerEventMode;
    Priority                     : Classes.TThreadPriority;
    Expires                      : TDateTime;
    Event                        : TimerEvent;
    Location                     : Core.Strings.Small;
    Data                         : Pointer;
    Owner                        : TThread;
  end;
  MemoryStatsItem=record
    dwSize                       : Cardinal;      // 4 bytes
    Total                        : Extended;      // 8 bytes
    Available                    : Extended;      // 8 bytes
    Load                         : Byte;          // 1 byte
  end;
  PMemoryStatsItem=^MemoryStatsItem;

  TTimerRegistryEvent=Procedure(Var Timer:Item; Const bFlag:Boolean) of Object;

  procedure Empty(Var Item:Item); overload;
  procedure Init(Var Item:Item); overload;
  procedure Done(Var Item:Item); overload;

Type
  TTimerThread=Class(TThread)
  private
    FSleepP                      : PRTLEvent;
    FCount                       : Cardinal;
    ItemP                        : PItem;
    FLock                        : TRTLCriticalSection;
    FList                        : TList;
    FLastItemLocation            : Core.Strings.VarString;
  private
    procedure ProcessItems();
    procedure SyncProcessItem();
  protected
    procedure DoBeforeProcess(); virtual;
    procedure Execute(); Override;
  public
    Constructor Create(aPriority:Classes.TThreadPriority); Virtual;
    Destructor Destroy(); Override;
  public
    procedure RegisterEvent(Var Item:Item; Const bUpdate:Boolean);
    procedure UnloadEvent(Var Item:Item; Const bExecute:Boolean);
  public
    property  Terminated;
    property  Count:Cardinal read FCount;
  end;

  TSystemTimerThread=Class(TTimerThread)
  protected
    procedure Execute; override;
  end;

  TMemoryStats=Class(TObject)
  private
    MemoryInfo                   : TMemoryInfo;
    FTimerItem                   : Item;
  private
    procedure    _OnTimer(ItemP:PItem);
  public
    Threshold                    : LongInt;  // Amount of free memory that must be present before allowing creation of RSR Objects.
    Total                        : System.Int64;
    Available                    : System.Int64;
    Used                         : System.Int64;
    Load                         : System.Int64;
    Constructor  Create; Reintroduce; Virtual;
    Destructor   Destroy; Override;
  end;


Var
  sEnvironment  : Core.Strings.VarString;

  Environment   : Core.Arrays.Types.VarString;

  Timer         : TTimerThread;
  Background    : TTimerThread;

  MemoryStats   : TMemoryStats;


  IDE_DESIGNING : Boolean;

  dtNow         : TDateTime;
  dtUT          : TDateTime;

const
  TIMER_TIMESLICE = 15;
  TIMER_ENVIRONMENT = 17;

implementation
uses
{$ifdef Windows}
  Windows,
{$else}
  unix,baseunix,
{$endif}
 Core.Logging,DateUtils;
var
  SystemTimer   : TSystemTimerThread;

procedure Empty(Var Item:Item);
begin
  With Item do begin
    Priority:=tpNormal;
    Expires:=0;
    Event:=nil;
    Data:=nil;
    Owner:=nil;
    SetLength(Location,0);
  end;
end;

procedure Init(Var Item:Item);
begin
  With Item do begin
    Mode:=temNormal;
    Priority:=tpNormal;
    Expires:=0;
    Event:=nil;
    Data:=Nil;
    Owner:=nil;
    SetLength(Location,0);
  end;
end;

procedure Done(Var Item:Item);
begin
  Finalize(Item.Location);
  Finalize(Item);
end;


Constructor  TMemoryStats.Create;
begin
  Core.System.GetMemoryInfo(MemoryInfo);
  Total:=MemoryInfo.Total_Ram;
  Threshold:=MEM_THRESHOLD;

  FTimerItem.Location:='Core.Timer._OnTimer';
  FTimerItem.Expires:=IncSecond(dtNow,MEM_TIMER_SECONDS);
  FTimerItem.Event:=@_OnTimer;

  Timer.RegisterEvent(FTimerItem,False);
  Inherited Create;
end;

Destructor   TMemoryStats.Destroy;
begin
  if Timer<>Nil then
    Timer.UnloadEvent(FTimerItem,False);
  Inherited Destroy;
end;

procedure    TMemoryStats._OnTimer(ItemP:PItem);
begin
  Core.System.RefreshMemoryInfo(MemoryInfo);
  Available:=MemoryInfo.Free_Ram;
  Load:=MemoryInfo.Load_Ram;
  Used:=MemoryInfo.Used_Ram;
  FTimerItem.Expires:=IncSecond(dtNow,MEM_TIMER_SECONDS);
end;


procedure TSystemTimerThread.Execute;
const
  dtNextEnvQuery:TDateTime=0;
var
  DayOfWeek:Word;
  iLcv,iLength:LongInt;
begin
  Priority:=tpHigher;
  While Not (Terminated) do begin
    try
      if dtNow>dtNextEnvQuery then begin
        iLength:=GetEnvironmentVariableCount;
        SetLength(Environment,iLength);
        for iLcv:=0 to iLength-1 do begin
          if Terminated then exit;
          Environment[iLcv]:=GetEnvironmentString(iLcv);
        end;
        sEnvironment:=Core.Arrays.VarString.toString(Environment);
        dtNextEnvQuery:=DateUtils.IncSecond(dtNow,TIMER_ENVIRONMENT);
      end;
      dtNow:=SysUtils.Now;
      dtUT:=DateUtils.IncMilliSecond(dtNow,BiasMinutes*60000);

      DateTimeToSystemTime(dtUT,stUniversal);
      GetLocalTime(stLocalTime);

      DayOfWeek:=DateUtils.DayOfTheWeek(dtNow);
      TimeZoneTime:=Concat(Core.Utils.Time.Day_Short[DayOfWeek],', ',Format('%.2d',[stLocalTime.Day]),' ' ,Core.Utils.Time.Month_Short[stLocalTime.Month], ' ',Format('%.4d',[stLocalTime.Year]),' ',Format('%.2d',[stLocalTime.Hour]),':',Format('%.2d',[stLocalTime.Minute]),':'+Format('%.2d',[stLocalTime.Second]), ' ',TZIBias);
      UTCTime:=Concat(Core.Utils.Time.Day_Short[DayOfWeek],', ',Format('%.2d',[stUniversal.Day]),' ' ,Core.Utils.Time.Month_Short[stUniversal.Month], ' ',Format('%.4d',[stUniversal.Year]),' ',Format('%.2d',[stUniversal.Hour]),':',Format('%.2d',[stUniversal.Minute]),':'+Format('%.2d',[stUniversal.Second]), ' UTC');

      ProcessItems;
      RTLeventWaitFor(FSleepP,TIMER_TIMESLICE);
    Except
      On E:Exception do
        if not Terminated then
          Core.Logging.Native.WriteLogEntry(SYSTEM_LOG,Format(FMT_LOG_SERVICE,[UnitName,ClassName]),E.Message);
    end;
  end;
end;

Constructor TTimerThread.Create(aPriority:Classes.TThreadPriority);
begin
  FSleepP:=RTLEventCreate;
  FCount:=0;
  InitCriticalSection(FLock);
  FreeOnTerminate:=False;
  FList:=TList.Create;
  Inherited Create(False, TIMER_STACK_SIZE);
  Priority:=aPriority;
end;

Destructor TTimerThread.Destroy;
begin
  if not Terminated then begin
    Terminate;
    WaitFor;
  end;
  FreeAndNil(FList);
  RTLeventdestroy(FSleepP);
  DoneCriticalSection(FLock);
  Inherited Destroy;
end;

procedure TTimerThread.DoBeforeProcess;
begin

end;

procedure TTimerThread.Execute;
begin
  While Not (Terminated) do begin
    {$ifdef SyncTimers}
      Synchronize(@DoBeforeProcess);
      Synchronize(@ProcessItems);
    {$else}
      DoBeforeProcess();
      ProcessItems;
    {$endif}
    RTLeventWaitFor(FSleepP,TIMER_TIMESLICE);
  end;
end;

procedure TTimerThread.SyncProcessItem;
begin
  ItemP^.Expires:=0;
  ItemP^.Event(ItemP);
end;

Procedure TTimerThread.ProcessItems;
var
  PackNeeded                     : Boolean;
  iLcv                           : LongInt;
  iCount                         : LongInt;
  dtStamp                        : TDateTime;

  procedure ProcessNormal;
  begin
    Try
      System.LeaveCriticalSection(FLock);
      Try
        ItemP^.Event(ItemP);
      Finally
        System.EnterCriticalSection(FLock);
      End;
    Except
      On E:Exception do begin
        FList.Items[iLcv]:=Nil;
        PackNeeded:=True;
        Core.Logging.Native.WriteLogEntry(SYSTEM_LOG,Format(FMT_LOG_SERVICE_ITEM,[UnitName,ClassName,ItemP^.Location]),E.Message);
      end;
    End;
  end;

  procedure ProcessSynchronized;
  begin
    Try
      System.LeaveCriticalSection(FLock);
      Try
        Synchronize(@SyncProcessItem);

      Finally
        System.EnterCriticalSection(FLock);
      End;
    Except
      On E:Exception do begin
        FList.Items[iLcv]:=Nil;
        PackNeeded:=True;
        Core.Logging.Native.WriteLogEntry(SYSTEM_LOG,Format(FMT_LOG_SERVICE_ITEM,[UnitName,ClassName,ItemP^.Location]),E.Message);
      end;
    End;
  end;

begin
  iLcv:=0; PackNeeded:=False;
  System.EnterCriticalSection(FLock);
  Try
    While Not (Terminated) and (iLcv<FList.Count) do begin
      ItemP:=FList.Items[iLcv];
      dtStamp:=dtNow;
      FLastItemLocation:='Core.Timer.TTimerThread.ProcessItems.Inspecting';
      Try
        If (ItemP<>Nil) and (ItemP^.Expires<>0) and (dtStamp>ItemP^.Expires) then begin
          If Assigned(ItemP^.Event) then begin
            FLastItemLocation:=ItemP^.Location;
            Case ItemP^.Mode of
              temNormal      : ProcessNormal;
              temSynchronize : ProcessSynchronized;
            end;
          end;
        end;
        Inc(iLcv);
      Except
        On E:Exception do begin
          iCount:=FList.Count;
          FList.Items[iLcv]:=Nil;
          PackNeeded:=True;
        end;
      End;
    end;
    If PackNeeded then
      FList.Pack;
  Finally
    System.LeaveCriticalSection(FLock);
  End;
end;

procedure TTimerThread.UnloadEvent(Var Item:Item; Const bExecute:Boolean);
begin
  If Terminated then Exit;
  System.EnterCriticalSection(FLock);
  Try
    Try
      If bExecute and Assigned(Item.Event) then
        Item.Event(@Item);
    Finally
      FList.Remove(@Item);
    End;
    FCount:=FList.Count;
  Finally
    System.LeaveCriticalSection(FLock);
  end;
end;

procedure TTimerThread.RegisterEvent(Var Item:Item; Const bUpdate:Boolean);
var
  iLcv:LongInt;
  bAdded:Boolean;
begin
  If Terminated then Exit;
  System.EnterCriticalSection(FLock);
  Try
    bAdded:=False;
    If bUpdate then begin
      For iLcv:=0 to FList.Count-1 do
        If FList.Items[iLcv]=@Item then
          bAdded:=True;
    end;
    If Not bAdded then
      FList.Add(@Item);
    Item.Owner:=Self;
    FCount:=FList.Count;
  Finally
    System.LeaveCriticalSection(FLock);
  end;
end;


procedure SetupTZI;
Const
  BiasCH                         : Array[Boolean] of Char=('+','-');
Var
  DayOfWeek                      : Word;
{$if defined(Unix)}
  timeval                        : TTimeVal;
  timezone                       : TTimeZone;
{$elseif defined(Windows)}
  TZInf                          : TIME_ZONE_INFORMATION;
  iTZCall                        : LongInt;
{$endif}
begin
  {$if defined(Unix)}
    fpGetTimeOfDay (@TimeVal, @TimeZone);
    GetLocalTime(stLocalTime);
    BiasMinutes:=TimeZone.tz_minuteswest;
    TZI.Bias:=BiasMinutes;
  {$elseif defined(Windows)}
    iTZCall:=GetTimeZoneInformation(TZInf);
    BiasMinutes:=TZInf.Bias;
    TZI.Bias:=BiasMinutes;
    GetLocalTime(stLocalTime);
  {$endif}
  if (TZI.Bias>0) then begin
    ToUTCBias:=1*BiasMinutes;
    FromUTCBias:=-1*BiasMinutes;
  end else begin
    ToUTCBias:=-1*BiasMinutes;
    FromUTCBias:=1*BiasMinutes;
  end;
  TZIBias:=Concat(BiasCH[TZI.Bias>0],'0',IntToStr(Abs(TZI.Bias div 60)),'00');
  DayOfWeek:=DateUtils.DayOfTheWeek(dtNow);
  TimeZoneTime:=Concat(Core.Utils.Time.Day_Short[DayOfWeek],', ',Format('%.2d',[stLocalTime.Day]),' ' ,Core.Utils.Time.Month_Short[stLocalTime.Month], ' ',Format('%.4d',[stLocalTime.Year]),' ',Format('%.2d',[stLocalTime.Hour]),':',Format('%.2d',[stLocalTime.Minute]),':'+Format('%.2d',[stLocalTime.Second]), ' ',TZIBias);
end;

procedure InitThreads;
var
  Test:TComponent;
begin
  Test:=TComponent.Create(Nil);
  Try
    IDE_DESIGNING:=(csDesigning in Test.ComponentState);
  Finally
    FreeAndNil(Test);
  End;
  If Not IDE_DESIGNING then begin
    If SystemTimer=Nil then
      SystemTimer:=TSystemTimerThread.Create(tpNormal);
    If Timer=Nil then
      Timer:=TTimerThread.Create(tpLower);
    if Background=nil then
      Background:=TTimerThread.Create(tpIdle);
    If MemoryStats=Nil then
      MemoryStats:=TMemoryStats.Create;
  end;
end;

procedure CloseThreads;
begin
  If Not IDE_DESIGNING then begin
    If SystemTimer<>Nil then begin
      SystemTimer.Terminate();
      SystemTimer:=nil;
    end;
    If Timer<>Nil then begin
      Timer.Terminate;
      Timer:=nil;
    end;
    if Background<>nil then begin
      Background.Terminate();
      Background:=nil;
    end;
    If MemoryStats<>Nil then
      FreeAndNil(MemoryStats);
  end;
end;

initialization
  dtNow:=SysUtils.Now;
  SetupTZI();
  InitThreads();
Finalization
  CloseThreads();

end.


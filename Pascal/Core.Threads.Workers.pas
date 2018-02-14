
unit Core.Threads.Workers;

{
Written By : Andrew Thomas Brunner
Copyright Aurawin LLC 2013-2015.

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}


interface

uses
  Classes, SysUtils;


Const
  WORKER_TIMESLICE_ACTIVE        = 20;   // milliseconds;
  WORKER_TIMESLICE_IDLE          = 200;  // milliseconds;
  MANAGER_TIMESLICE              = 1220; // milliseconds;

  ManagerProgressNone            : cardinal = 0;
  ManagerProgressIdle            : cardinal = 1;
  ManagerProgressWorking         : cardinal = 2;

Type
  TWorkerThread                  = class;
  TManagerThread                 = class;

  TManagerStatus                 = (msNone,msIdle,msStop,msStart,msPause);
  TThreadStatus                  = (tsNone,tsRunning,tsIdle,tsStopped);
  TWorkItemEvent                 = procedure(Thread:TWorkerThread; var Data:pointer) of object;
  TWorkThreadEvent               = procedure(Thread:TWorkerThread) of object;
  TManagerThreadEvent            = procedure(Manager:TManagerThread) of object;
  TThreadDataPointers            = Array of Pointer;
  TManagerThread=Class(TThread)
  private
    FProgress                    : Cardinal;
    FWorkOnItem                  : TWorkItemEvent;
    FInitWorkItem                : TWorkItemEvent;
    FDoneWorkItem                : TWorkItemEvent;
    FEmptyWorkItem               : TWorkItemEvent;
    FAddedWorkItem               : TWorkItemEvent;
    FOnWorkerThreadInit          : TWorkThreadEvent;
    FOnWorkerThreadDone          : TWorkThreadEvent;
    FOnAllWorkComplete           : TManagerThreadEvent;
  private
    FSleepP                      : PEventState;
    FAddLock                     : TRTLCriticalSection;
    FQueueLock                   : TRTLCriticalSection;
    FProcessingLock              : TRTLCriticalSection;
  private
    FAddList                     : TList;  // List of items to add to queue.
    FQueueList                   : TList;  // List of items in queue.
    FProcessingList              : TList;
    FThreadList                  : TList;  // List of Threads created in system.
  private
    FStatus                      : TManagerStatus;
  private
    procedure ClearThreadData(List:TList);
    procedure Clear;
    procedure SetSize(Size:byte);
  protected
    procedure Execute; override;
  protected
    procedure ProcessAddList();
    function  GetNextItem(var DataP:Pointer):boolean;
  public
    constructor Create(cbOnInitItem,cbOnEmptyItem,cbOnDoneItem,cbOnAddedWorkItem,cbWorkOnItem:TWorkItemEvent; cbOnWorkerThreadInit,cbOnWorkerThreadDone:TWorkThreadEvent; aNumberOfWorkerThreads:Byte); reIntroduce;
    destructor  Destroy; override;
  public
    // This is how you "register" a new worker item.
    procedure  AllocateWorkItem(var DataP:Pointer);
  public
    property   OnWorkOnItem:TWorkItemEvent read FWorkOnItem write FWorkOnItem;
    property   OnAllWorkComplete:TManagerThreadEvent read FOnAllWorkComplete write FOnAllWorkComplete;
  public
    // Essential Operations that the Main Application can Perform.
    procedure Start;
    procedure Stop;
    procedure Pause;
    procedure Resume;
  end;

  TWorkerThread=Class(TThread)
  private
    FManager                     : TManagerThread;
    FItemP                       : Pointer;
    FStatus                      : TThreadStatus;
  protected
    procedure  Execute; override;
  public
    Context                      : array of Pointer;
  public
    constructor Create(aManager:TManagerThread); reIntroduce;
    destructor  Destroy; override;
  end;


implementation

Const
  Event_Item_Number:LongInt=1;

function GetNextEventName:string;
begin
  Result:=Concat('uManagerWorkerThreads.',IntToStr(Event_Item_Number));
  Inc(Event_Item_Number);
end;


Constructor TManagerThread.Create(cbOnInitItem,cbOnEmptyItem,cbOnDoneItem,cbOnAddedWorkItem,cbWorkOnItem:TWorkItemEvent; cbOnWorkerThreadInit,cbOnWorkerThreadDone:TWorkThreadEvent; aNumberOfWorkerThreads:Byte);
begin
  FProgress:=ManagerProgressNone;
  FSleepP:=BasicEventCreate(nil,false,false,GetNextEventName);
  FOnAllWorkComplete:=nil;
  FInitWorkItem:=cbOnInitItem;
  FEmptyWorkItem:=cbOnEmptyItem;
  FDoneWorkItem:=cbOnDoneItem;
  FAddedWorkItem:=cbOnAddedWorkItem;
  FWorkOnItem:=cbWorkOnItem;
  FOnWorkerThreadInit:=cbOnWorkerThreadInit;
  FOnWorkerThreadDone:=cbOnWorkerThreadDone;

  FStatus:=msNone;
  FreeOnTerminate:=False;
  InitCriticalSection(FAddLock);
  InitCriticalSection(FQueueLock);
  InitCriticalSection(FProcessingLock);

  FAddList:=TList.Create;
  FQueueList:=TList.Create;
  FProcessingList:=TList.Create;


  FThreadList:=TList.Create;

  inherited Create(false);

  SetSize(aNumberOfWorkerThreads);
end;

Destructor  TManagerThread.Destroy;
begin
  Stop;
  Clear;
  Terminate;
  WaitFor;

  BasicEventDestroy(FSleepP);

  DoneCriticalSection(FAddLock);
  DoneCriticalSection(FQueueLock);

  DoneCriticalSection(FProcessingLock);

  FreeAndNil(FAddList);
  FreeAndNil(FQueueList);
  FreeAndNil(FProcessingList);

  FreeAndNil(FThreadList);

  inherited Destroy;
end;

procedure TManagerThread.AllocateWorkItem(var DataP:Pointer);
begin
  FInitWorkItem(nil,DataP);
  if (DataP<>nil) then begin
    EnterCriticalSection(FQueueLock);
    Try
      FQueueList.Add(DataP);
    Finally
      LeaveCriticalSection(FQueueLock);
    end;
  end;
end;

procedure TManagerThread.ClearThreadData(List:TList);
var
  iLcv                           : LongInt;
  tdDataP                        : Pointer;
begin
  for iLcv:=0 to List.Count-1 do begin
    tdDataP:=List.Items[iLcv];
    FDoneWorkItem(nil,tdDataP);
  end;
  List.Clear();
end;

procedure   TManagerThread.Clear;
begin
  EnterCriticalSection(FQueueLock);
  Try
    EnterCriticalSection(FAddLock);
    Try
      EnterCriticalSection(FProcessingLock);
      Try
        SetSize(0);
        ClearThreadData(FAddList);
        ClearThreadData(FQueueList);
        ClearThreadData(FProcessingList);
      Finally
        LeaveCriticalSection(FProcessingLock);
      end;
    Finally
      LeaveCriticalSection(FAddLock);
    end;
  Finally
    LeaveCriticalSection(FQueueLock);
  end;
end;

procedure   TManagerThread.SetSize(Size:Byte);
var
  tdWorker                       : TWorkerThread;
begin
  if (Size>FThreadList.Count) then begin
    // Grow List
    Repeat
      tdWorker:=TWorkerThread.Create(Self);
      FThreadList.Add(tdWorker);
    Until (Size>=FThreadList.Count);
  end else if (Size<FThreadList.Count) then begin
    // Shrink List
    Repeat
      tdWorker:=TWorkerThread(FThreadList.First);
      Try
        tdWorker.Terminate;
        Try
          tdWorker.WaitFor;
        Finally
          FThreadList.Remove(tdWorker);
        end;
      Finally
        FreeAndNil(tdWorker);
      end;
    Until (FThreadList.Count<=Size);
  end;
end;

function    TManagerThread.GetNextItem(var DataP:Pointer):boolean;
begin
  Result:=False;
  if (DataP<>nil) then begin
    // You had existing Data.
    // Push Data off to complete Bin before you get next one.
    // The Thread as input is just complete.
    // Add Item to Complete List
    EnterCriticalSection(FProcessingLock);
    Try
      FProcessingList.Remove(DataP);
    Finally
      LeaveCriticalSection(FProcessingLock);
    end;
    FDoneWorkItem(nil,DataP);
    DataP:=nil;
  end;
  // Get Next Item & Remove it from the Queue List
  EnterCriticalSection(FQueueLock);
  Try
    DataP:=FQueueList.First;
    if (DataP<>nil) then begin  // If you have data present
      FQueueList.Remove(DataP);
      // Now add it to the Processing List
      EnterCriticalSection(FProcessingLock);
      Try
        FProcessingList.Add(DataP);
      Finally
        LeaveCriticalSection(FProcessingLock);
      end;
    end; // else there was nothing in queue to get
  Finally
    LeaveCriticalSection(FQueueLock);
  end;
  if (DataP<>nil) then begin
    Result:=True;
    System.InterlockedExchange(FProgress,ManagerProgressWorking);
  end else begin
    if (System.InterlockedCompareExchange(FProgress,ManagerProgressIdle,ManagerProgressWorking)=ManagerProgressWorking) then begin
      If Assigned(FOnAllWorkComplete) then
        FOnAllWorkComplete(Self);
    end;
  end;

end;

procedure   TManagerThread.Execute;
begin
  // Generally speaking don't declare variables here.
  // Do all your work in object methods not here.
  // ONLY do simple signal processesing here.
  While Not Terminated do begin
    ProcessAddList();  // Move Add Items To Queue (If Any)
    BasicEventWaitFor(MANAGER_TIMESLICE,FSleepP); // Give it a rest for a bit...
  end;
end;


procedure   TManagerThread.Start;
begin

end;

procedure   TManagerThread.Stop;
begin
end;

procedure   TManagerThread.Pause;
begin

end;

procedure   TManagerThread.Resume;
begin

end;

procedure   TManagerThread.ProcessAddList;
var
  tdData                         : Pointer;
begin
  EnterCriticalSection(FAddLock);
  Try
    While FAddList.Count>0 do begin
      tdData:=FAddList.First;
      Try
        // Lock Queue List now
        EnterCriticalSection(FQueueLock);
        Try
          FQueueList.Add(tdData);
        Finally
          LeaveCriticalSection(FQueueLock);
        end;
      Finally
        FAddList.Remove(tdData);
      end;
    end;
  Finally
    LeaveCriticalSection(FAddLock);
  end;
  FAddedWorkItem(nil,tdData);
end;

constructor TWorkerThread.Create(aManager:TManagerThread);
begin
  FManager:=aManager;
  FItemP:=nil;
  FStatus:=tsNone;

  inherited Create(false);
  FreeOnTerminate:=False;
end;

destructor  TWorkerThread.Destroy;
begin
  FItemP:=nil;
  inherited Destroy;
end;

procedure   TWorkerThread.Execute;
begin
  FManager.FOnWorkerThreadInit(Self);
  FItemP:=nil;
  While Not Terminated do begin
    if FManager.GetNextItem(FItemP) then begin // will recycle last item in queue
      FStatus:=tsRunning;
      FManager.FWorkOnItem(Self,FItemP);
      FStatus:=tsIdle;
      BasicEventWaitFor(WORKER_TIMESLICE_ACTIVE,FManager.FSleepP);
    end else begin
      FStatus:=tsIdle;
      BasicEventWaitFor(WORKER_TIMESLICE_IDLE,FManager.FSleepP);
    end;
  end;
  FManager.FOnWorkerThreadDone(Self);
end;

end.

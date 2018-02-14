unit Core.Queue;

interface

uses
  Classes,

  RSR,

  App.Consts,

  Core.Strings,
  Core.Logging,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Storage,

  Storage.MatrixNodes,
  Storage.MatrixQueue,

  SysUtils;

Type
  TSystemQueueItemEvent=procedure(Task:Core.Database.Types.TTask; var Item:Storage.MatrixQueue.Items.Item) of object;
  TSystemQueue=class(TThread)
  private
    FNode                        : Storage.MatrixNodes.Node.Item;
    FItem                        : Storage.MatrixQueue.Items.Item;
    FSleepP                      : PRTLEvent;
    FKind                        : QWord;
    FOnQueueItem                 : TSystemQueueItemEvent;
    FHeader                      : Core.Database.Types.THeader;
    FTask                        : Core.Database.Types.TTask;
  private
    procedure  OnDBException(sProcedure,sLocation,sTable,sTask,sError:Core.Database.Types.VarString);
  protected
    procedure Execute; override;
  public
    constructor Create(aHeader:Core.Database.Types.THeader; aNodeID:QWord; aKind:QWord); reIntroduce;
    destructor  Destroy; override;
  public
    property  OnQueItem:TSystemQueueItemEvent read FOnQueueItem write FOnQueueItem;
  published
    property Header : Core.Database.Types.THeader read FHeader;
    property Task : Core.Database.Types.TTask read FTask;
  end;

implementation


constructor TSystemQueue.Create(aHeader:Core.Database.Types.THeader; aNodeID:QWord; aKind:QWord);
begin
  FSleepP:=RTLEventCreate();
  FKind:=aKind;

  FHeader:=aHeader;
  FTask:=Core.Database.Types.TTask.Create(aHeader,'Core.Queue.SystemQueue');

  Storage.MatrixNodes.Node.DB.Fill(Task,aNodeId,FNode);

  inherited Create(FALSE);
end;

destructor TSystemQueue.Destroy;
begin
  RTLEventDestroy(FSleepP);
  inherited Destroy;
end;

procedure TSystemQueue.Execute;
begin
  while not Terminated do begin
    if Assigned(FOnQueueItem) then begin
      if Storage.MatrixQueue.Items.DB.Get(Task,FNode.ID,FKind,FItem) then
          FOnQueueItem(Task,FItem);
    end;
    RTLeventWaitFor(FSleepP,QUEUE_YIELD_MS[FItem.Verified=false]);
  end;
end;

procedure TSystemQueue.OnDBException(sProcedure,sLocation,sTable,sTask,sError:Core.Database.Types.VarString);
begin
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_QUEUE,Concat(sProcedure,'.',sLocation,'.',sTable,'.',sTask,':',sError));
end;

end.


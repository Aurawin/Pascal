unit Core.Threads;


interface

uses
  Core.Generics,


  Classes,
  SysUtils;

Const
  RecycleAll=true;
  RecycleFlagged=false;


Type
  CThread=class;
  CThreadObject=class;

  PAllocationRequest=^TAllocationRequest;
  TAllocationRequest=record
    Lock : TRTLCriticalSection;
    Result : CThreadObject;
  end;

  CThreadObject=class(TObject)
  protected
    Releaseable:boolean;
    Owner: CThread;
  public
    procedure Release();
  public
    constructor Create(aOwner:CThread); virtual;
  end;

  GCThreadObjectList=Specialize GObjectThreadList<CThreadObject>;
  CThreadObjectList=class(GCThreadObjectList);

  GCThreadObjectAllocationList=Specialize GStructThreadList<TAllocationRequest>;
  CThreadObjectAllocationList=class(GCThreadObjectList);

  CThread=Class(TThread)
  protected
    FRecycler : CThreadObjectList;
    FAllocationList: CThreadObjectAllocationList;
  protected
    procedure doTerminate; override;
  public
    procedure Recycle(const ReleaseAll:boolean = RecycleFlagged);
  public
    Constructor Create(CreateSuspended: Boolean; const StackSize: SizeUInt = DefaultStackSize); reintroduce;
    Destructor Destroy(); override;
  end;


implementation

constructor CThread.Create(CreateSuspended: Boolean;
                       const StackSize: SizeUInt = DefaultStackSize);
begin
  FRecycler:=CThreadObjectList.Create(true);
  FAllocationList:=CThreadObjectAllocationList.Create(true);

  inherited Create(CreateSuspended,StackSize);
end;

Destructor CThread.Destroy();
begin
  FRecycler.Free();
  FAllocationList.Free();

  inherited Destroy;
end;

procedure CThread.doTerminate();
begin
  Recycle(RecycleAll);
  inherited doTerminate();
end;

procedure CThread.Recycle(const ReleaseAll:boolean=RecycleFlagged);
var
  itms:TList;
  itm:CThreadObject;
  iLcv:integer;
begin
  itms:=FRecycler.LockList;
  try
    iLcv:=0;
    While (iLcv<itms.Count) do begin
      itm:=FRecycler[iLcv];
      if (ReleaseAll=RecycleAll) or (itm.Releaseable=true) then begin
        itms.Remove(itm);
        itm.Free;
      end else
        iLcv+=1;
    end;
  finally
    FRecycler.UnlockList;
  end;
end;

constructor CThreadObject.Create(aOwner:CThread);
begin
  Owner:=aOwner;
  if (GetCurrentThreadId<>aOwner.Handle) then
    Raise Exception.Create('Cannot create external instance of this object');
  Owner.FRecycler.Add(Self);
end;

procedure CThreadObject.Release();
begin
  Releaseable:=true;
end;

end.


{
 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit Core.Generics;

interface

uses
  Classes, SysUtils; 

Type
  Defaults=Class
  const
    FreeOnClear:System.Boolean=true;
    KeepOnClear:System.Boolean=false;
  end;
  generic GObjectList<_T>=class(TList)
  private
    FIndex : LongInt;
  private
    function  Get(Index:LongInt): _T;
    procedure Put(Index:LongInt; Item:_T);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    function  Add(Item: _T): LongInt;
    function  Delete(Item: _T): LongInt;
    function  IndexOf(Item:_T): LongInt;
    function  Next:_T;
  public
    property  Items[Index:LongInt]: _T read Get write Put; default;
  end;

  generic GObjectThreadList<_T>=class(TThreadList)
  private
    FIndex : LongInt;
    FFreeOnClear:System.Boolean;
  private
    function  Get(Index:LongInt): _T;
    procedure Put(Index:LongInt; Item:_T);
  public
    constructor Create(aFreeOnClear:System.Boolean); virtual;
    destructor Destroy; override;
  public
    procedure  Add(Item: _T);
    procedure  Delete(Item: _T);
    procedure  Clear();
    function   Next:_T;
    function   Pop:_T;
  public
    property  Items[Index:LongInt]: _T read Get write Put; default;
  end;

  generic GStructThreadList<_T>=class(TThreadList)
  type
    GValuePointer=^_T;
  private
    FIndex : LongInt;
    FFreeOnClear:System.Boolean;
  private
    function  Get(Index:LongInt): GValuePointer;
    procedure Put(Index:LongInt; Item:GValuePointer);
  public
    constructor Create(aFreeOnClear:System.Boolean); virtual;
    destructor Destroy; override;
  public
    procedure  Add(var Item: _T);
    procedure  Delete(var Item: _T);
    procedure  Clear();
    function   Next:GValuePointer;
    function   Pop:GValuePointer;
  public
    procedure Empty(Var Item:_T); virtual;
    procedure Done(Var Item:_T); virtual;
    procedure Init(Var Item:_T); virtual;
  public
    property  Items[Index:LongInt]: GValuePointer read Get write Put; default;
  end;


  generic GStructList<_T>=class(TList)
  private
    type
      GValuePointer=^_T;
    var
    FIndex : LongInt;
  private
    function  Get(Index:LongInt): GValuePointer;
    procedure Put(Index:LongInt; Item:GValuePointer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    function Add(Var Item:_T): LongInt;
    function Delete(Var Item:_T): LongInt;
    function IndexOf(Var Item:_T): LongInt;
    function Next:GValuePointer;
  public
    procedure Empty(Var Item:_T); virtual;
    procedure Done(Var Item:_T); virtual;
    procedure Init(Var Item:_T); virtual;
  public
    property Items[Index:LongInt]:GValuePointer read Get write Put; default;
  end;


implementation

constructor GObjectList.Create;
begin
  FIndex:=-1;
  inherited Create;
end;

destructor GObjectList.Destroy;
begin
  inherited Destroy;
end;

function GObjectList.Add(Item:_T): LongInt;
begin
  Result:=Inherited Add(Item);
end;

function GObjectList.Delete(Item:_T): LongInt;
begin
  Result:=IndexOf(Item);
  if Result<>-1 then
    Inherited Delete(Result);
end;

function GObjectList.IndexOf(Item:_T): LongInt;
begin
  Result:=Inherited IndexOf(Item);
end;

function  GObjectList.Next:_T;
begin
  Result:=nil;
  if ((Count>0) and ( (FIndex=-1) or (FIndex+1>=Count) )) then begin
    FIndex:=0;
  end else begin
    FIndex+=1;
  end;
  if FIndex<>-1 then
    Result:=Get(FIndex);
end;

function  GObjectList.Get(Index:LongInt):_T;
begin
  Result:=_T(Inherited Get(Index));
end;

procedure GObjectList.Put(Index:LongInt; Item:_T);
begin
  Inherited Put(Index,Item);
end;

constructor GStructList.Create;
begin
  FIndex:=-1;
  inherited Create;
end;

destructor GStructList.Destroy;
begin
  inherited Destroy;
end;

function GStructList.Add(var Item:_T): LongInt;
begin
  Result:=Inherited Add(@Item);
end;

function GStructList.Delete(var Item:_T): LongInt;
begin
  Result:=IndexOf(Item);
  if Result<>-1 then
    Inherited Delete(Result);
end;

function GStructList.IndexOf(var Item:_T): LongInt;
begin
  Result:=Inherited IndexOf(@Item);
end;

function  GStructList.Next:GValuePointer;
begin
  Result:=nil;
  if ((Count>0) and ( (FIndex=-1) or (FIndex+1>=Count) )) then begin
    FIndex:=0;
  end else begin
    FIndex+=1;
  end;
  if FIndex<>-1 then
    Result:=Get(FIndex);
end;

procedure GStructList.Empty(Var Item:_T);
begin

end;

procedure GStructList.Done(Var Item:_T);
begin

end;

procedure GStructList.Init(Var Item:_T);
begin
end;

function  GStructList.Get(Index:LongInt):GValuePointer;
begin
  Result:=GValuePointer(Inherited Get(Index));
end;

procedure GStructList.Put(Index:LongInt; Item:GValuePointer);
begin
  Inherited Put(Index,Item);
end;

constructor GObjectThreadList.Create(AFreeOnClear:System.Boolean);
begin
  FFreeOnClear:=AFreeOnClear;
  FIndex:=-1;
  inherited Create;
end;

destructor GObjectThreadList.Destroy;
begin
  Clear();
  inherited Destroy;
end;

procedure GObjectThreadList.Add(Item:_T);
var
  FList:TList;
begin
  FList:=LockList();
  Try
    FList.Add(Item);
  finally
    UnlockList();
  end;
end;

procedure GObjectThreadList.Delete(Item:_T);
begin
  Inherited Remove(Item);
end;

procedure GObjectThreadList.Clear();
var
  FList:TList;
  iLcv:LongInt;
begin
  FList:=LockList();
  Try
    if (FFreeOnClear= true) then begin
      for iLcv:=0 to FList.Count-1 do
        _T(FList[iLcv]).Free;
    end;
    FList.Clear();
  finally
    UnlockList();
  end;
end;

function  GObjectThreadList.Next:_T;
var
  FList:TList;
begin
  Result:=nil;
  FList:=LockList;
  try
    if ((FList.Count>0) and ( (FIndex=-1) or (FIndex+1>=FList.Count) )) then begin
      FIndex:=0;
    end else begin
      FIndex+=1;
    end;
    if FIndex<>-1 then
      Result:=_T(FList[FIndex]);
  finally
    UnlockList();
  end;
end;

function  GObjectThreadList.POP:_T;
var
  FList:TList;
  idx:LongInt;
begin
  Result:=nil;
  FList:=LockList;
  try
    idx:=FList.Count-1;
    if (idx>-1) then begin
      Result:=_T(FList.Items[idx]);
      FList.Delete(idx);
    end;
  finally
    UnlockList();
  end;
end;

function  GObjectThreadList.Get(Index:LongInt):_T;
var
  FList:TList;
begin
  FList:=LockList();
  Try
    Result:=_T(FList.Items[Index]);
  Finally
    UnLockList();
  end;
end;

procedure GObjectThreadList.Put(Index:LongInt; Item:_T);
var
  FList:TList;
begin
  FList:=LockList();
  Try
    FList.Items[Index]:=Item;
  Finally
    UnLockList();
  end;
end;



// start

constructor GStructThreadList.Create(AFreeOnClear:System.Boolean);
begin
  FFreeOnClear:=AFreeOnClear;
  FIndex:=-1;
  inherited Create;
end;

destructor GStructThreadList.Destroy;
begin
  Clear();
  inherited Destroy;
end;

procedure GStructThreadList.Add(var Item:_T);
var
  FList:TList;
begin
  FList:=LockList();
  Try
    FList.Add(@Item);
  finally
    UnlockList();
  end;
end;

procedure GStructThreadList.Delete(var Item:_T);
begin
  Inherited Remove(@Item);
end;

procedure GStructThreadList.Clear();
var
  FList:TList;
  iLcv:LongInt;
begin
  FList:=LockList();
  Try
    if (FFreeOnClear= true) then begin
      for iLcv:=0 to FList.Count-1 do begin
        Done(Items[iLcv]^);
        Dispose(Items[iLcv]);
      end;
    end;
    FList.Clear();
  finally
    UnlockList();
  end;
end;

function  GStructThreadList.Next:GValuePointer;
var
  FList:TList;
begin
  Result:=nil;
  FList:=LockList;
  try
    if ((FList.Count>0) and ( (FIndex=-1) or (FIndex+1>=FList.Count) )) then begin
      FIndex:=0;
    end else begin
      FIndex+=1;
    end;
    if FIndex<>-1 then
      Result:=FList[FIndex];
  finally
    UnlockList();
  end;
end;

function  GStructThreadList.POP:GValuePointer;
var
  FList:TList;
  idx:LongInt;
begin
  Result:=nil;
  FList:=LockList;
  try
    idx:=FList.Count-1;
    if (idx>-1) then begin
      Result:=FList.Items[idx];
      FList.Delete(idx);
    end;
  finally
    UnlockList();
  end;
end;

function  GStructThreadList.Get(Index:LongInt):GValuePointer;
var
  FList:TList;
begin
  FList:=LockList();
  Try
    Result:=GValuePointer(FList.Items[Index]);
  Finally
    UnLockList();
  end;
end;

procedure GStructThreadList.Put(Index:LongInt; Item:GValuePointer);
var
  FList:TList;
begin
  FList:=LockList();
  Try
    FList.Items[Index]:=Item;
  Finally
    UnLockList();
  end;
end;

procedure GStructThreadList.Empty(Var Item:_T);
begin

end;

procedure GStructThreadList.Done(Var Item:_T);
begin

end;

procedure GStructThreadList.Init(Var Item:_T);
begin

end;

end.


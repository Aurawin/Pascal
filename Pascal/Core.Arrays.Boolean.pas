unit Core.Arrays.Boolean;

interface

uses
  Classes, SysUtils,

  Core.Strings

  ;


const Yes : boolean =true;
Type
  PBooleanArray=^TBooleanArray;
  TBooleanArray=Array of Boolean;


  procedure Done(Var Item:TBooleanArray); overload;

  function  Copy(Var Source,Destination:TBooleanArray): LongInt; overload;
  function  Copy(Var Source,Destination:Boolean):boolean; overload;

  procedure Empty(Var Item:Boolean); overload;
  procedure Empty(Var Item:TBooleanArray); overload;

  function  All(const Value:Boolean; var List:TBooleanArray):Boolean; overload;

  function  Add(Item:Boolean; Var List:TBooleanArray): LongInt; overload;
  function  Delete(Item:Boolean; Var List:TBooleanArray): LongInt; overload;
  function  IndexOf(Item:Boolean; Var List:TBooleanArray): LongInt; overload;

  procedure Fill(Var List:TBooleanArray; Const Value:Boolean=false); overload;
  procedure Init(Var List:TBooleanArray; Const Size:LongInt; Const Value:Boolean=false); overload;

  procedure Remove(Index:LongInt; var List:TBooleanArray); overload;

  function  SetSize(Var List:TBooleanArray; Size:LongInt): LongInt; overload;

  function  fromString(Source:String; Var List:TBooleanArray): LongInt; overload;

  function  toString(Var List:TBooleanArray; Const Delimiter:string):string; overload;


implementation

procedure Done(Var Item:TBooleanArray);
begin
  Finalize(Item);
end;

function Copy(var Source,Destination:TBooleanArray): LongInt;
var
  iLcv                           : LongInt;
begin
  Result:=Length(Source);
  SetSize(Destination,Result);
  For iLcv:=0 to Result-1 do
    Destination[iLcv]:=Source[iLcv];
end;

function  Copy(Var Source,Destination:Boolean):boolean;
begin
  Result:=True;
  Destination:=Source;
end;

procedure Empty(Var Item:Boolean);
begin
  Item:=false;
end;

procedure Empty(Var Item:TBooleanArray);
begin
  SetLength(Item,0);
end;

function  Add(Item:Boolean; Var List:TBooleanArray): LongInt;
begin
  Result:=Length(List);
  SetSize(List,Result+1);
  List[Result]:=Item;
end;

function  All(const Value:Boolean; var List:TBooleanArray):Boolean;
var
  iLcv                           : LongInt;
begin
  Result:=True;
  for iLcv:=0 to High(List) do begin
    if List[iLcv]<>Value then begin
      Result:=false;
      Break;
    end;
  end;
end;

function  Delete(Item:Boolean; Var List:TBooleanArray): LongInt;
var
  iIndex                         : LongInt;
  iCount                         : LongInt;
  iLcv                           : LongInt;
begin
  iIndex:=IndexOf(Item,List);
  if iIndex<>-1 then begin
    iCount:=Length(List);
    For iLcv:=iIndex to iCount-2 do
      Copy(List[iLcv+1],List[iLcv]);
    SetSize(List,iCount-1);
  end;
  Result:=iIndex;
end;

function  IndexOf(Item:Boolean; Var List:TBooleanArray): LongInt;
var
  iLcv                           : LongInt;
  iCount                         : LongInt;
begin
  iCount:=Length(List); iLcv:=0; Result:=-1;
  while (iLcv<iCount) and (Result=-1) do begin
    If List[iLcv]=Item then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

procedure Remove(Index:LongInt; var List:TBooleanArray);
var
  iLcv:LongInt;
  iLength:LongInt;
begin
  iLength:=Length(List);
  if (Index<iLength) and (Index>-1)then begin
    For iLcv:=Index to iLength-2 do
      List[iLcv]:=List[iLcv+1];
    SetLength(List,iLength-1);
  end;
end;

procedure Init(Var List:TBooleanArray; Const Size:LongInt; Const Value:Boolean=false);
var
  iLcv:LongInt;
begin
  SetLength(List,Size);
  For iLcv:=0 to Size-1 do
    List[iLcv]:=Value;
end;

procedure Fill(Var List:TBooleanArray; Const Value:Boolean=false);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(List) do
    List[iLcv]:=Value;
end;

function  SetSize(Var List:TBooleanArray; Size:LongInt): LongInt;
var
  iLcv                           : LongInt;
  iCount                         : LongInt;
begin
  iCount:=System.Length(List);
  if (Size>iCount) then begin
    // Grow List
    SetLength(List,Size);
    if iCount>0 then
      For iLcv:=iCount-1 to Size-1 do
        Empty(List[iLcv]);
    Result:=Size;
  end else if (Size<iCount) then begin
    // Shrink List
    For iLcv:=iCount-1 downto Size-1 do
      Empty(List[iLcv]);
    SetLength(List,Size);
    Result:=Size;
  end;
end;

function  fromString(Source:String; Var List:TBooleanArray): LongInt;
var
  iLength:Int64;
  iLcv:LongInt;
begin
  iLength:=System.Length(Source);
  Result:=iLength;
  SetLength(List,Result);
  for iLcv:=1 to iLength do
    List[iLcv-1]:=(Source[iLcv]<>'0');

end;


function  toString(Var List:TBooleanArray; Const Delimiter:string):string;
Const
  CH_BOOL:Array[Boolean] of Char=('0','1');
var
  iLcv,iSize:LongInt;
begin
  iSize:=System.Length(List); SetLength(Result,0);
  for iLcv:=0 to High(List) do
    Result+=(CH_BOOL[List[iLcv]]+Delimiter);
end;

end.

unit Core.Arrays.LargeInt;

interface

uses
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Streams.Types,
  Core.Strings;


procedure Init(var Item:Core.Arrays.Types.LargeInt); overload;

function  Copy(Var Source,Destination:Core.Arrays.Types.LargeInt):LongInt; overload;
function  Copy(Var Source,Destination:System.Int64):System.boolean; overload;

procedure Empty(Var Item:Core.Arrays.Types.LargeInt); overload;

procedure Done(Var Item:Core.Arrays.Types.LargeInt); overload;

function  Add(Item:System.Int64; Var List:Core.Arrays.Types.LargeInt; const Option:Core.Arrays.AddOption=aoNone):LongInt; overload;
function  Delete(Item:System.Int64; Var List:Core.Arrays.Types.LargeInt):LongInt; overload;
function  IndexOf(Item:System.Int64; Var List:Core.Arrays.Types.LargeInt):LongInt; overload;

function  Range(Value,LowValue,HighValue:LongInt):System.Boolean;
procedure Remove(Index:LongInt; var List:Core.Arrays.Types.LargeInt); overload;
procedure Remove(Var Value:System.Int64; var List:Core.Arrays.Types.LargeInt); overload;
procedure RemoveAll(Value:System.Int64; var List:Core.Arrays.Types.LargeInt); overload;

function  SetSize(Var List:Core.Arrays.Types.LargeInt; Size:LongInt):LongInt; overload;


function  fromBytes(Source:AnsiString; Var List:Core.Arrays.Types.LargeInt):LongInt; overload;
function  fromString(Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeInt):LongInt; overload;
function  fromString(Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeInt; Delimitor:Core.Strings.VarString):LongInt; overload;
function  fromText(var Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeInt; const Delimitor:Core.Strings.VarString=','):LongInt;

procedure toString(Var List:Core.Arrays.Types.LargeInt; Const Delimiter:Core.Strings.VarString; Out Result:Core.Strings.VarString); overload;
function  toString(Var List:Core.Arrays.Types.LargeInt; Const Delimiter:Core.Strings.VarString; ssWorking:Core.Streams.Types.VarString):Core.Strings.VarString; overload;
function  toString(Var List:Core.Arrays.Types.LargeInt; Const Delimiter:Core.Strings.VarString):Core.Strings.VarString; overload;
function  toBlob(Var List:Core.Arrays.Types.LargeInt):AnsiString; overload;
function  toString(var Item:System.Int64):Core.Strings.VarString; overload;


implementation
uses SysUtils;

function toString(var Item:System.Int64):VarString;
begin
  Result:=SysUtils.Format('%.d',[Item]);
end;

procedure Done(Var Item:Core.Arrays.Types.LargeInt);
begin
  Finalize(Item);
end;

procedure Init(var Item:Core.Arrays.Types.LargeInt);
begin
  SetLength(Item,0);
end;

function Copy(var Source,Destination:Core.Arrays.Types.LargeInt):LongInt;
var
  iLcv                           : LongInt;
begin
  Result:=Length(Source);
  SetSize(Destination,Result);
  For iLcv:=0 to Result-1 do
    Destination[iLcv]:=Source[iLcv];
end;

function  Copy(Var Source,Destination:System.Int64):System.boolean;
begin
  Result:=True;
  Destination:=Source;
end;

procedure Empty(Var Item:Core.Arrays.Types.LargeInt);
begin
  SetLength(Item,0);
end;

function  Add(Item:System.Int64; Var List:Core.Arrays.Types.LargeInt; const Option:Core.Arrays.AddOption=aoNone):LongInt;
  procedure PushAddNormal;
  begin
    Result:=Length(List);
    SetSize(List,Result+1);
    List[Result]:=Item;
  end;
  procedure PushAddCheckForDuplicates;
  begin
    Result:=IndexOf(Item,List);
    if Result=-1 then
      PushAddNormal;
  end;

begin
  Case Option of
    aoNone               : PushAddNormal;
    aoCheckForDuplicates : PushAddCheckForDuplicates;
  end;
end;

function  Delete(Item:System.Int64; Var List:Core.Arrays.Types.LargeInt):LongInt;
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

function  IndexOf(Item:System.Int64; Var List:Core.Arrays.Types.LargeInt):LongInt;
var
  iLcv                           : LongInt;
  iCount                         : LongInt;
  iValue                         : System.Int64;
begin
  iCount:=Length(List); iLcv:=0; Result:=-1;
  while (iLcv<iCount) and (Result=-1) do begin
    iValue:=List[iLcv];
    If iValue=Item then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function   Range(Value,LowValue,HighValue:LongInt):System.Boolean;
begin
  Result:=(Value<=HighValue) and (Value>=LowValue);
end;

procedure Remove(Index:LongInt; var List:Core.Arrays.Types.LargeInt);
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

procedure Remove(Var Value:System.Int64; var List:Core.Arrays.Types.LargeInt);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(Value,List);
  if iIndex<>-1 then
    Remove(iIndex,List);
end;

procedure RemoveAll(Value:System.Int64; var List:Core.Arrays.Types.LargeInt);
var
  iIndex:LongInt;
begin
  repeat
    iIndex:=IndexOf(Value,List);
    if iIndex<>-1 then
      Remove(iIndex,List);
  until (iIndex=-1);
end;


function  SetSize(Var List:Core.Arrays.Types.LargeInt; Size:LongInt):LongInt;
var
  iLcv                           : LongInt;
  iCount                         : LongInt;
begin
  iCount:=Length(List);
  if (Size>iCount) then begin
    // Grow List
    System.SetLength(List,Size);
    For iLcv:=iCount to Size-1 do
      List[iLcv]:=0;
    Result:=Size;
  end else if (Size<iCount) then begin
    // Shrink List
    For iLcv:=iCount-1 downto Size-1 do
      List[iLcv]:=0;
    SetLength(List,Size);
    Result:=Size;
  end;
end;

function  fromString(Source:VarString; Var List:Core.Arrays.Types.LargeInt):LongInt;
var
  iCount                         : LongInt;
  iLen                           : LongInt;
  iPos                           : LongInt;
  iIndex                         : LongInt;
  iValue                         : System.int64;
begin
  iLen:=System.Length(Source);
  if ((iLen mod 8)=0) then begin
    iCount:=iLen div 8;
    SetLength(List,iCount);
    iPos:=1; iIndex:=0;
    While (iPos<=iLen) do begin
      iValue:=0;
      System.Move(Source[iPos],iValue,8);
      List[iIndex]:=iValue;
      Inc(iPos,8);
      Inc(iIndex);
    end;
    Result:=iCount;
  end else begin
    Result:=0;
    System.SetLength(List,0);
  end;
end;

function  fromBytes(Source:AnsiString; Var List:Core.Arrays.Types.LargeInt):LongInt;
var
  iCount                         : LongInt;
  iLen                           : LongInt;
  iPos                           : LongInt;
  iIndex                         : LongInt;
  iValue                         : System.int64;
begin
  iLen:=System.Length(Source);
  if ((iLen mod 8)=0) then begin
    iCount:=iLen div 8;
    SetLength(List,iCount);
    iPos:=1; iIndex:=0;
    While (iPos<=iLen) do begin
      iValue:=0;
      System.Move(Source[iPos],iValue,8);
      List[iIndex]:=iValue;
      Inc(iPos,8);
      Inc(iIndex);
    end;
    Result:=iCount;
  end else begin
    Result:=0;
    System.SetLength(List,0);
  end;
end;


function  fromString(Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeInt; Delimitor:Core.Strings.VarString):LongInt;
var
  saItems:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  Result:=0;
  Empty(List);
  Core.Arrays.VarString.fromString(@saItems,Source,Delimitor);
  Try
    for iLcv:=0 to High(saItems) do begin
      if (Length(saItems[iLcv])>0) then
        Core.Arrays.LargeInt.Add(Core.Strings.toInt(saItems[iLcv],System.Int64(0)),List);
    end;
    Result:=System.Length(List);
  finally
    Done(saItems);
  end;
end;

function  fromText(var Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeInt; const Delimitor:Core.Strings.VarString=','):LongInt;
var
  saItems:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  Result:=0;
  Core.Arrays.LargeInt.Empty(List);
  fromString(@saItems,Source,Delimitor);
  Try
    for iLcv:=0 to High(saItems) do begin
      if (Length(saItems[iLcv])>0) then
        Core.Arrays.LargeInt.Add(Core.Strings.toInt(saItems[iLcv],System.Int64(0)),List);
    end;
    Result:=System.Length(List);
  finally
    Done(saItems);
  end;
end;


procedure toString(Var List:Core.Arrays.Types.LargeInt; Const Delimiter:Core.Strings.VarString; Out Result:Core.Strings.VarString);
var
  iLcv:LongInt;
begin
  Empty(Result);
  for iLcv:=0 to High(List) do
    Core.Strings.Append(Result,List[iLcv],Delimiter);
end;

function  toString(Var List:Core.Arrays.Types.LargeInt; Const Delimiter:Core.Strings.VarString; ssWorking:Core.Streams.Types.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  SetLength(Result,0); ssWorking.Size:=0;
  For iLcv:=0 to High(List) do begin
    ssWorking.WriteString(IntToStr(List[iLcv]));
    ssWorking.WriteString(Delimiter);
  end;
  if High(List)>0 then
    ssWorking.Size:=ssWorking.Size-Length(Delimiter);
  Result:=ssWorking.DataString;
  ssWorking.Size:=0;
end;

function toString(Var List:Core.Arrays.Types.LargeInt; Const Delimiter:Core.Strings.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
  iDelimLen:LongInt;
  iLen:LongInt;
begin
  System.SetLength(Result,0);
  iDelimLen:=System.Length(Delimiter);
  for iLcv:=0 to High(List) do
    Core.Strings.Append(Result,List[iLcv],Delimiter);
  iLen:=System.Length(Result);
  if System.Length(List)>0 then
     System.SetLength(Result,iLen-iDelimLen);
end;


function  toBlob(Var List:Core.Arrays.Types.LargeInt):AnsiString;
var
  iSize:System.int64;
  iValue:System.int64;
  iPos:LongInt;
  iLcv:LongInt;
  iLen:LongInt;
  iChunk:System.Byte;
begin
  iLen:=System.Length(List);
  iChunk:=System.SizeOf(Int64);
  iSize:=iLen * iChunk;
  SetLength(Result,iSize);
  iPos:=1;
  for iLcv:=0 to iLen-1 do begin
    iValue:=List[iLcv];
    System.Move(iValue,Result[iPos],iChunk);
    iPos+=8;
  end;
end;

end.


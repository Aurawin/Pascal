unit Core.Arrays.LargeWord;

interface

uses
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Streams.Types,
  Core.Streams,
  Core.Strings;


procedure Init(var Item:Core.Arrays.Types.LargeWord); overload;

function  Copy(Var Source,Destination:Core.Arrays.Types.LargeWord):LongInt; overload;
function  Copy(Var Source,Destination:System.QWord):boolean; overload;

procedure Empty(Var Item:Core.Arrays.Types.LargeWord); overload;

procedure Done(Var Item:Core.Arrays.Types.LargeWord); overload;

function  Add(Item:System.QWord; Var List:Core.Arrays.Types.LargeWord; const Option:Core.Arrays.AddOption=aoNone):LongInt; overload;
function  Delete(Item:System.QWord; Var List:Core.Arrays.Types.LargeWord):LongInt; overload;
function  IndexOf(Item:System.QWord; Var List:Core.Arrays.Types.LargeWord):LongInt; overload;

function  Max(Value1,Value2:QWord):QWord;

function  Range(Value,LowValue,HighValue:LongInt):System.Boolean;
procedure Remove(Index:LongInt; var List:Core.Arrays.Types.LargeWord); overload;
procedure Remove(Var Value:System.QWord; var List:Core.Arrays.Types.LargeWord); overload;
procedure RemoveAll(Value:System.QWord; var List:Core.Arrays.Types.LargeWord); overload;

function  SetSize(Var List:Core.Arrays.Types.LargeWord; Size:LongInt):LongInt; overload;


function  fromBytes(Source:String; Var List:Core.Arrays.Types.LargeWord):LongInt; overload;
function  fromString(Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeWord; Delimitor:Core.Strings.VarString):LongInt; overload;
function  fromText(var Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeWord; const Delimitor:Core.Strings.VarString=','):LongInt;

procedure toString(Var List:Core.Arrays.Types.LargeWord; Const Delimiter:Core.Strings.VarString; Out Result:Core.Strings.VarString); overload;
function  toString(Var List:Core.Arrays.Types.LargeWord; Const Delimiter:Core.Strings.VarString; Working:Core.Streams.Types.Base):Core.Strings.VarString; overload;
function  toString(Var List:Core.Arrays.Types.LargeWord; Const Delimiter:Core.Strings.VarString):Core.Strings.VarString; overload;
function  toBlob(Var List:Core.Arrays.Types.LargeWord):String; overload;
function  toString(Item:System.QWord):Core.Strings.VarString; overload;



implementation
uses SysUtils;

function  Max(Value1,Value2:QWord):QWord;
begin
  if Value2>Value1 then
     Result:=Value2
  else
    Result:=Value1;
end;

function toString(Item:System.QWord):VarString;
begin
  Result:=SysUtils.Format('%.d',[Item]);
end;

procedure Done(Var Item:Core.Arrays.Types.LargeWord);
begin
  Finalize(Item);
end;

procedure Init(var Item:Core.Arrays.Types.LargeWord);
begin
  SetLength(Item,0);
end;

function Copy(var Source,Destination:Core.Arrays.Types.LargeWord):LongInt;
var
  iLcv                           : LongInt;
begin
  Result:=Length(Source);
  SetSize(Destination,Result);
  For iLcv:=0 to Result-1 do
    Destination[iLcv]:=Source[iLcv];
end;

function  Copy(Var Source,Destination:System.QWord):System.boolean;
begin
  Result:=True;
  Destination:=Source;
end;

procedure Empty(Var Item:Core.Arrays.Types.LargeWord);
begin
  SetLength(Item,0);
end;

function  Add(Item:System.QWord; Var List:Core.Arrays.Types.LargeWord; const Option:Core.Arrays.AddOption=aoNone):LongInt;
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

function  Delete(Item:System.QWord; Var List:Core.Arrays.Types.LargeWord):LongInt;
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

function  IndexOf(Item:System.QWord; Var List:Core.Arrays.Types.LargeWord):LongInt;
var
  iLcv                           : LongInt;
  iCount                         : LongInt;
  iValue                         : System.QWord;
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

procedure Remove(Index:LongInt; var List:Core.Arrays.Types.LargeWord);
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

procedure Remove(Var Value:System.QWord; var List:Core.Arrays.Types.LargeWord);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(Value,List);
  if iIndex<>-1 then
    Remove(iIndex,List);
end;

procedure RemoveAll(Value:System.QWord; var List:Core.Arrays.Types.LargeWord);
var
  iIndex:LongInt;
begin
  repeat
    iIndex:=IndexOf(Value,List);
    if iIndex<>-1 then
      Remove(iIndex,List);
  until (iIndex=-1);
end;


function  SetSize(Var List:Core.Arrays.Types.LargeWord; Size:LongInt):LongInt;
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

function  fromBytes(Source:String; Var List:Core.Arrays.Types.LargeWord):LongInt;
var
  iCount                         : LongInt;
  iLen                           : LongInt;
  iPos                           : LongInt;
  iIndex                         : LongInt;
  iValue                         : System.QWord;
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
      iPos+=8;
      Inc(iIndex);
    end;
    Result:=iCount;
  end else begin
    Result:=0;
    System.SetLength(List,0);
  end;
end;

function  fromString(Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeWord; Delimitor:Core.Strings.VarString):LongInt;
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
        Core.Arrays.LargeWord.Add(Core.Strings.toInt(saItems[iLcv],System.QWord(0)),List);
    end;
    Result:=System.Length(List);
  finally
    Done(saItems);
  end;
end;

function  fromText(var Source:Core.Strings.VarString; Var List:Core.Arrays.Types.LargeWord; const Delimitor:Core.Strings.VarString=','):LongInt;
var
  saItems:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  Result:=0;
  Core.Arrays.LargeWord.Empty(List);
  fromString(@saItems,Source,Delimitor);
  Try
    for iLcv:=0 to High(saItems) do begin
      if (Length(saItems[iLcv])>0) then
        Core.Arrays.LargeWord.Add(Core.Strings.toInt(saItems[iLcv],System.QWord(0)),List);
    end;
    Result:=System.Length(List);
  finally
    Done(saItems);
  end;
end;


procedure toString(Var List:Core.Arrays.Types.LargeWord; Const Delimiter:Core.Strings.VarString; Out Result:Core.Strings.VarString);
var
  iLcv:LongInt;
begin
  Empty(Result);
  for iLcv:=0 to High(List) do
    Core.Strings.Append(Result,List[iLcv],Delimiter);
end;

function  toString(Var List:Core.Arrays.Types.LargeWord; Const Delimiter:Core.Strings.VarString; Working:Core.Streams.Types.Base):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  SetLength(Result,0); Working.Size:=0;
  For iLcv:=0 to High(List) do begin
    Core.Streams.Write(IntToStr(List[iLcv]),Working);
    Core.Streams.Write(Delimiter,Working);
  end;
  if High(List)>0 then
    Working.Size:=Working.Size-Length(Delimiter);

  Result:=Core.Streams.ToString(Working);

  Working.Size:=0;
end;

function toString(Var List:Core.Arrays.Types.LargeWord; Const Delimiter:Core.Strings.VarString):Core.Strings.VarString;
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


function  toBlob(Var List:Core.Arrays.Types.LargeWord):String;
var
  iSize:System.int64;
  iValue:System.QWord;
  iPos:LongInt;
  iLcv:LongInt;
  iLen:LongInt;
  iChunk:System.Byte;
begin
  iLen:=System.Length(List);
  iChunk:=System.SizeOf(QWord);
  iSize:=iLen * iChunk;
  SetLength(Result,iSize);
  iPos:=1;
  for iLcv:=0 to iLen-1 do begin
    iValue:=List[iLcv];
    System.Move(iValue,Result[iPos],iChunk);
    iPos+=iChunk;
  end;
end;

end.


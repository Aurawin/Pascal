unit Core.Arrays.KeyString;

interface

uses
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Streams,
  Core.Streams.Types;


Type

  AddCallback=procedure(Var List:Core.Arrays.Types.KeyStrings; Var Name,Value:Core.Strings.VarString);
  RemoveQuoteCallback=procedure(Var Value:Core.Strings.VarString);
  TrimValueCallback=procedure(var Value:Core.Strings.VarString);

procedure Done(Var Item:Core.Strings.KeyString); overload;
procedure Done(var List:Core.Arrays.Types.KeyStrings); overload;
procedure Empty(Var List:Core.Arrays.Types.KeyStrings); overload;
procedure Empty(Var Item:Core.Strings.KeyString); overload;
procedure Init(Var Item:Core.Strings.KeyString); overload;
procedure Init(Var Item:Core.Arrays.Types.KeyStrings); overload;
function  Add(Var List:Core.Arrays.Types.KeyStrings; var sKey,sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt; overload;
function  Add(Var List:Core.Arrays.Types.KeyStrings; var sKey,sItem:Core.Strings.VarString; const Data:System.Pointer; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt; overload;
function  Add(ListP:Core.Arrays.Types.PKeyStrings; sKey,sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt; overload;
function  Add(ListP:Core.Arrays.Types.PKeyStrings; sKey:Core.Strings.VarString; var Item:Core.Arrays.Types.MD5Digest; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt; overload;
function  Insert(Var Item:Core.Strings.KeyString; Index:LongInt; var List:Core.Arrays.Types.KeyStrings):LongInt; overload;
function  Prepend(Var Item:Core.Strings.KeyString; var List:Core.Arrays.Types.KeyStrings):LongInt; overload;
function  Append(Var Item:Core.Strings.KeyString; var List:Core.Arrays.Types.KeyStrings):LongInt; overload;
function  Append(Item:Core.Strings.PKeyString; var List:Core.Arrays.Types.KeyStrings):LongInt; overload;
function  IndexOf(Var List:Core.Arrays.Types.KeyStrings; var sKey:Core.Strings.VarString):System.Int64; overload;
function  Search(Var List:Core.Arrays.Types.KeyStrings; var Term:Core.Strings.VarString):System.Int64; overload;
function  IndexOf(ListP:Core.Arrays.Types.PKeyStrings; sKey:Core.Strings.VarString):System.Int64; overload;
procedure SetSize(Var List:Core.Arrays.Types.KeyStrings; iSize:LongInt); overload;
function Exchange(srcIndex,destIndex:LongInt; var Items:Core.Arrays.Types.KeyStrings):System.boolean;
procedure Copy(var Source,Destination:Core.Strings.KeyString); overload;
procedure Copy(var Source,Destination:Core.Arrays.Types.KeyStrings); overload;

function  GetItem(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt):Core.Strings.PKeyString; overload;

function  GetItemByKey(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString):Core.Strings.VarString; overload;
function  GetItemByKey(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt):Core.Strings.VarString; overload;
procedure GetItemsByKey(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Out Result:Core.Arrays.Types.VarString);

procedure SetStreamsByKey(var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Streams:System.Boolean; Count:LongInt=0);

function  GetItemAsInt64(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Int64=0):System.Int64; overload;
function  GetItemAsQWord(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.QWord=0):System.QWord; overload;
function  GetItemAsWord(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Word=0):System.Word; overload;
function  GetItemAsInt64Array(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; out Items:Core.Arrays.Types.LargeInt; Count:LongInt=0; const Default:System.Int64=0):LongInt; overload;
function  GetItemAsQWordArray(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; out Items:Core.Arrays.Types.LargeWord; Count:LongInt=0; const Default:System.QWord=0):LongInt; overload;
function  GetItemAsDouble(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Double=0):System.Double; overload;
function  GetItemAsShortString(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:Core.Strings.Small=''):Core.Strings.Small; overload;
function  GetItemAsString(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:Core.Strings.VarString=''):Core.Strings.VarString; overload;
function  GetItemAsInteger(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:LongInt=0):LongInt; overload;
function  GetItemAsByte(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Byte=0):System.Byte; overload;

function  Update(Var List:Core.Arrays.Types.KeyStrings; Key,Value:Core.Strings.VarString):System.Int64; overload;
function  Update(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Streams:boolean):System.Int64; overload;
function  Update(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Value:System.QWord):System.Int64; overload;

function  fromStream(var List:Core.Arrays.Types.KeyStrings; Data:Core.Streams.Types.Base; Const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):LongInt; overload;
function  fromString(var List:Core.Arrays.Types.KeyStrings; var sData:Core.Strings.VarString; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):LongInt; overload;
function  fromString(ListP:Core.Arrays.Types.PKeyStrings; sData:Core.Strings.VarString; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):LongInt; overload;
function  fromStringArray(Var List:Core.Arrays.Types.KeyStrings; var Data:Core.Arrays.Types.VarString; const FieldDelim:Core.Strings.VarString='='; const Defaults:SplitOptions=[soClearList]):LongInt;
procedure fromFile(var List:Core.Arrays.Types.KeyStrings; const sFile:Core.Strings.FileName; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10);

function  toString(var List:Core.Arrays.Types.KeyStrings; Refactor:Core.Streams.Types.Base; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10):Core.Strings.VarString; overload;
function  toString(var List:Core.Arrays.Types.KeyStrings; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10):Core.Strings.VarString; overload;
function  toStream(var List:Core.Arrays.Types.KeyStrings; Stream:Core.Streams.Types.Base; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10):LongInt; overload;
procedure toFile(var List:Core.Arrays.Types.KeyStrings; FileName:Core.Strings.VarString; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10); overload;


implementation
uses Core.Arrays.VarString,Core.Arrays.LargeInt,Core.Arrays.LargeWord;

procedure Done(Var Item:Core.Strings.KeyString);
begin
  Finalize(Item.Key);
  Finalize(Item.Value);
  Finalize(Item);
end;

procedure Done(var List:Core.Arrays.Types.KeyStrings);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(List) do Done(List[iLcv]^);
  Finalize(List);
end;

procedure Empty(Var List:Core.Arrays.Types.KeyStrings);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(List) to High(List) do begin
    Done(List[iLcv]^);
    Dispose(List[iLcv]);
  end;
  SetLength(List,0);
end;

procedure Empty(Var Item:Core.Strings.KeyString);
begin
  SetLength(Item.Key,0);
  SetLength(Item.Value,0);
  Item.Streams:=true;
  Item.Data:=nil;
end;

procedure Init(Var Item:Core.Strings.KeyString);
begin
  SetLength(Item.Key,0);
  SetLength(Item.Value,0);
  Item.Data:=nil;
  Item.Streams:=true;
end;

procedure Init(Var Item:Core.Arrays.Types.KeyStrings);
begin
  Empty(Item);
end;

function  Add(ListP:Core.Arrays.Types.PKeyStrings; sKey,sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt;
begin
  Result:=Add(ListP^,sKey,sItem,Defaults);
end;

function  Add(ListP:Core.Arrays.Types.PKeyStrings; sKey:Core.Strings.VarString; var Item:Core.Arrays.Types.MD5Digest; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt;
begin
  Result:=Add(ListP,sKey,Core.Strings.Print(Item),Defaults);
end;
function  Append(Item:Core.Strings.PKeyString; var List:Core.Arrays.Types.KeyStrings):LongInt;
begin
  Result:=System.Length(List);
  System.SetLength(List,Result+1);
  List[Result]:=Item;
end;

function  Prepend(Var Item:Core.Strings.KeyString; var List:Core.Arrays.Types.KeyStrings):LongInt;
var
  iLcv:LongInt;
begin
  Result:=System.Length(List);
  System.SetLength(List,Result+1);
  for iLcv:=1 to Result do
    List[iLcv]:=List[iLcv-1];
  New(List[0]);
  Copy(Item,List[0]^);
end;

function  Insert(Var Item:Core.Strings.KeyString; Index:LongInt; var List:Core.Arrays.Types.KeyStrings):LongInt;
var
  iLcv:LongInt;
begin
  Result:=System.Length(List);
  System.SetLength(List,Result+1);
  if Result>0 then begin
    for iLcv:=Result downto Index+1 do
      List[iLcv]:=List[iLcv-1];
  end;
  New(List[Index]);
  Copy(Item,List[Index]^);
end;

function  Append(Var Item:Core.Strings.KeyString; var List:Core.Arrays.Types.KeyStrings):LongInt;
begin
  Result:=System.Length(List);
  System.SetLength(List,Result+1);
  New(List[Result]);
  Copy(Item,List[Result]^);
end;

function  Add(Var List:Core.Arrays.Types.KeyStrings; var sKey,sItem:Core.Strings.VarString; const Data:System.Pointer; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt; overload;
var
  iIndex:LongInt;
begin
  iIndex:=-1;
  if aoCheckForDuplicates in Defaults then
    iIndex:=IndexOf(List,sKey);
  if (iIndex=-1) then begin
    iIndex:=Length(List);
    SetLength(List,iIndex+1);
    New(List[iIndex]);
    Init(List[iIndex]^);
    List[iIndex]^.Key:=sKey;
    List[iIndex]^.Value:=sItem;
    List[iIndex]^.Data:=Data;
  end else if (aoOverwriteDuplicate in Defaults) then begin
    List[iIndex]^.Value:=sItem;
    List[iIndex]^.Data:=Data;
  end;
  Result:=iIndex;
end;

function  Add(Var List:Core.Arrays.Types.KeyStrings; var sKey,sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):LongInt;
var
  iIndex:LongInt;
begin
  iIndex:=-1;
  if aoCheckForDuplicates in Defaults then
    iIndex:=IndexOf(List,sKey);
  if (iIndex=-1) then begin
    iIndex:=Length(List);
    SetLength(List,iIndex+1);
    New(List[iIndex]);
    Init(List[iIndex]^);
    List[iIndex]^.Key:=sKey;
    List[iIndex]^.Value:=sItem;
  end else if (aoOverwriteDuplicate in Defaults) then
    List[iIndex]^.Value:=sItem;
  Result:=iIndex;
end;

function  IndexOf(Var List:Core.Arrays.Types.KeyStrings; var sKey:Core.Strings.VarString):System.Int64;
var
  iLcv:System.Int64;
  iLength:System.Int64;
begin
  iLength:=Length(List);
  Result:=-1; iLcv:=0;
  While (iLcv<iLength) and (Result=-1) do begin
    If Core.Strings.SameText(List[iLcv]^.Key,sKey) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  Search(Var List:Core.Arrays.Types.KeyStrings; var Term:Core.Strings.VarString):System.Int64;
var
  iLcv:System.Int64;
  iLength:System.Int64;
begin
  Result:=-1;
  iLength:=Length(List);
  iLcv:=0;
  While (iLcv<iLength) and (Result=-1) do begin
    if Core.Strings.Pos(Term,List[iLcv]^.Value)>0 then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  IndexOf(ListP:Core.Arrays.Types.PKeyStrings; sKey:Core.Strings.VarString):System.Int64;
begin
  Result:=IndexOf(ListP^,sKey);
end;

procedure SetSize(Var List:Core.Arrays.Types.KeyStrings; iSize:LongInt);
var
  iLcv:LongInt;
  iLen:LongInt;
begin
  iLen:=Length(List);
  if (iSize<iLen) then begin
    // Going to Shrink the list
    While (iLen>iSize) do begin
      Done(List[iLen]^);
      Dispose(List[iLen]);
    end;
    SetLength(List,iSize);
  end else if (iSize>iLen) then begin
    // Going to Grow the list
    SetLength(List,iSize);
    For iLcv:=iLen-1 to iSize-1 do begin
      New(List[iLcv]);
      Init(List[iLcv]^);
    end;
  end;
end;


procedure  Callback_TrimValue(Var Value:Core.Strings.VarString);
begin
  Core.Strings.Trim(Value);
end;

procedure  Callback_DontTrimValue(Var Value:Core.Strings.VarString);
begin

end;


procedure  Callback_RemoveQuotesFromValue(Var Value:Core.Strings.VarString);
var
  iLen:LongInt;
begin
  iLen:=Length(Value);
  if iLen>1 then begin
    If Value[1]=#34 then begin
      System.Delete(Value,1,1);
      Dec(iLen);
    end;
  end;
  if iLen>1 then begin
    If Value[iLen]=#34 then begin
      System.Delete(Value,iLen,1);
      Dec(iLen);
    end;
  end;
  if iLen>1 then begin
    If Value[1]=#39 then begin
      System.Delete(Value,1,1);
      Dec(iLen);
    end;
  end;
  if iLen>1 then begin
    If Value[iLen]=#39 then begin
      System.Delete(Value,iLen,1);
      Dec(iLen);
    end;
  end;
end;

procedure Callback_DontRemoveQuotesFromValue(Var Value:Core.Strings.VarString);
begin

end;

procedure  Callback_AddToKPListLowercase(var List:Core.Arrays.Types.KeyStrings; Var Name,Value:Core.Strings.VarString);
begin
  Name:=Lowercase(Name);
  Add(List,Name,Value,[]);
end;

procedure  Callback_AddToKPList(Var List:Core.Arrays.Types.KeyStrings;Var Name,Value:Core.Strings.VarString);
begin
  Add(List,Name,Value,[]);
end;

procedure  fromFile(var List:Core.Arrays.Types.KeyStrings; const sFile:Core.Strings.FileName; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10);
var
  FS:Core.Streams.Types.Disk;
  sData:Core.Strings.VarString;
begin
  FS:=Core.Streams.Types.Disk.Create(sFile,fmOpenRead or fmShareDenyNone);
  Try
    SetLength(sData,FS.Size);
    if FS.Size>0 then begin
      FS.Read(sData[1],FS.Size);
      fromString(List,sData,FieldDelim,ItemDelim);
    end else begin
      Empty(List);
    end;
  finally
  end;
end;

function  fromString(ListP:Core.Arrays.Types.PKeyStrings; sData:Core.Strings.VarString; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):LongInt;
begin
  Result:=fromString(ListP^,sData,FieldDelim,ItemDelim,Defaults);
end;

function  fromStream(var List:Core.Arrays.Types.KeyStrings; Data:Core.Streams.Types.Base; Const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):LongInt;
var
  sData:Core.Strings.VarString;
begin
  sData:=Core.Streams.toString(Data);
  Result:=fromString(List,sData,FieldDelim,ItemDelim,Defaults);
end;

function  fromStringArray(Var List:Core.Arrays.Types.KeyStrings; var Data:Core.Arrays.Types.VarString; const FieldDelim:Core.Strings.VarString='='; const Defaults:SplitOptions=[soClearList]):LongInt;
var
  iLcv:LongInt;
  sItem:Core.Strings.VarString;
  saFields:Core.Arrays.Types.VarString;
  bMakeKeyLowercase:System.boolean;
  bRemoveQuotes:System.boolean;
  bTrimValue:System.boolean;

  AddProcs:Array[System.Boolean] of AddCallback;
  RemoveQuoteProcs:Array[System.Boolean] of RemoveQuoteCallback;
  TrimValueProcs:Array[System.Boolean] of TrimValueCallback;

begin
  if soClearList in Defaults then
    Empty(List);

  AddProcs[false]:=@Callback_AddToKPList;
  AddProcs[true]:=@Callback_AddToKPListLowercase;
  RemoveQuoteProcs[false]:=@Callback_DontRemoveQuotesFromValue;
  RemoveQuoteProcs[true]:=@Callback_RemoveQuotesFromValue;
  TrimValueProcs[false]:=@Callback_DontTrimValue;
  TrimValueProcs[true]:=@Callback_TrimValue;

  bMakeKeyLowercase:=(soMakeKeyLowercase in Defaults);
  bRemoveQuotes:=(soRemoveQuotes in Defaults);
  bTrimValue:=(soTrimValue in Defaults);
  for iLcv:=0 to High(Data) do begin
    sItem:=Data[iLcv];
    if Length(sItem)>0 then begin
      Core.Arrays.VarString.Empty(saFields);
      Core.Arrays.VarString.fromString(saFields,sItem,FieldDelim);
      Try
        If Length(saFields)=2 then begin
          TrimValueProcs[bTrimValue](saFields[1]);
          RemoveQuoteProcs[bRemoveQuotes](saFields[1]);
          AddProcs[bMakeKeyLowercase](List,saFields[0],saFields[1]);
        end;
      Finally
        Empty(saFields);
      end;
    end;
  end;
  Result:=Length(List);

end;

function  fromString(var List:Core.Arrays.Types.KeyStrings; var sData:Core.Strings.VarString; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):LongInt;
var
  iLen:LongInt;
  iPosition:LongInt;
  iStart:LongInt;
  iItemDelimLen:LongInt;
  sItem:Core.Strings.VarString;
  saFields:Core.Arrays.Types.VarString;
  bMakeKeyLowercase:System.boolean;
  bRemoveQuotes:System.boolean;
  bTrimValue:System.boolean;

  AddProcs:Array[Boolean] of AddCallback;
  RemoveQuoteProcs:Array[Boolean] of RemoveQuoteCallback;
  TrimValueProcs:Array[Boolean] of TrimValueCallback;

  function GetNextEntry:Core.Strings.VarString;
  var
    iLoc:LongInt;
  begin
    iLoc:=Core.Strings.PosEx(ItemDelim,sData,iStart,iLen);
    if iLoc>0 then begin
      Result:=System.Copy(sData,iStart,(iLoc-iStart));
      iPosition:=iLoc+iItemDelimLen;
      iStart:=iPosition;
    end else begin
      Result:=System.Copy(sData,iStart,(iLen-iStart+1));
      iPosition:=iLen+1;
      iStart:=iLen+1;
    end;
  end;

begin
  if soClearList in Defaults then
    Empty(List);

  AddProcs[false]:=@Callback_AddToKPList;
  AddProcs[true]:=@Callback_AddToKPListLowercase;
  RemoveQuoteProcs[false]:=@Callback_DontRemoveQuotesFromValue;
  RemoveQuoteProcs[true]:=@Callback_RemoveQuotesFromValue;
  TrimValueProcs[false]:=@Callback_DontTrimValue;
  TrimValueProcs[true]:=@Callback_TrimValue;

  iItemDelimLen:=Length(ItemDelim);
  iLen:=Length(sData);
  iPosition:=0;
  iStart:=1;


  bMakeKeyLowercase:=(soMakeKeyLowercase in Defaults);
  bRemoveQuotes:=(soRemoveQuotes in Defaults);
  bTrimValue:=(soTrimValue in Defaults);
  While (iStart<=iLen) do begin
    sItem:=GetNextEntry;
    Core.Arrays.VarString.fromString(saFields,sItem,FieldDelim,[soClearList,soSingleton]);
    Try
      If Length(saFields)=2 then begin
        TrimValueProcs[bTrimValue](saFields[1]);
        RemoveQuoteProcs[bRemoveQuotes](saFields[1]);
        AddProcs[bMakeKeyLowercase](List,saFields[0],saFields[1]);
      end;
    Finally
      Empty(saFields);
    end;
  end;
  Result:=Length(List);
end;

function  toString(var List:Core.Arrays.Types.KeyStrings; Refactor:Core.Streams.Types.Base; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10):Core.Strings.VarString;
var
  iCt,iLcv:LongInt;
begin
  Refactor.Size:=0;
  try
    iCt:=System.Length(List);
    for iLcv:=Low(List) to iCt-1 do
      if List[iLcv]^.Streams then begin
        Core.Streams.Write(
          Concat(
            List[iLcv]^.Key,FieldDelim,
            List[iLcv]^.Value,ItemDelim
          ),
          Refactor
        );
      end;
    if iCt>0 then
      Refactor.Size:=Refactor.Size-System.Length(ItemDelim);
    Refactor.Position:=0;
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Size:=0;
  end;
end;

function  toString(var List:Core.Arrays.Types.KeyStrings; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10):Core.Strings.VarString;
var
  Refactor:Core.Streams.Types.Memory;
  iLcv,iCt:LongInt;
begin
  Refactor:=Core.Streams.Types.Memory.Create();
  try
    iCt:=System.Length(List);
    for iLcv:=Low(List) to iCt-1 do
      if List[iLcv]^.Streams then begin
        Core.Streams.Write(
          Concat(
            List[iLcv]^.Key,FieldDelim,
            List[iLcv]^.Value,ItemDelim
          ),
          Refactor
        );
      end;
    if iCt>0 then
      Refactor.Size:=Refactor.Size-System.Length(ItemDelim);
    Refactor.Position:=0;
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Free();
  end;
end;


function  toStream(var List:Core.Arrays.Types.KeyStrings; Stream:Core.Streams.Types.Base; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10):LongInt;
var
  iLcv:LongInt;
  iWrite:LongInt;
begin
  Result:=0; iWrite:=0;
  for iLcv:=Low(List) to High(List) do begin
    if List[iLcv]^.Streams then begin
      iWrite:=Core.Streams.Write(
        Concat(
          List[iLcv]^.Key,FieldDelim,
          List[iLcv]^.Value,ItemDelim
        ),
        Stream
      );
      Inc(Result,iWrite);
    end;
  end;
end;

procedure toFile(var List:Core.Arrays.Types.KeyStrings; FileName:Core.Strings.VarString; const FieldDelim:Core.Strings.VarString='='; const ItemDelim:Core.Strings.VarString=#13#10);
var
  iLcv:LongInt;
  Stream:Core.Streams.Types.Disk;
begin
  Stream:=Core.Streams.Types.Disk.Create(FileName,fmCreate);
  Try
    for iLcv:=Low(List) to High(List) do
      if List[iLcv]^.Streams then begin
        Core.Streams.Write(
          Concat(
            List[iLcv]^.Key,FieldDelim,
            List[iLcv]^.Value,ItemDelim
          ),
          Stream
        );
      end;
  finally
    Stream.Free();
  end;
end;

procedure Copy(Var Source,Destination:Core.Strings.KeyString);
begin
  Destination.Key:=Source.Key;
  Destination.Value:=Source.Value;
  Destination.Streams:=Source.Streams;
end;

procedure Copy(var Source,Destination:Core.Arrays.Types.KeyStrings);
var
  iLen:LongInt;
  iLcv:LongInt;
begin
  iLen:=Length(Source);
  SetSize(Destination,iLen);
  For iLcv:=Low(Source) to High(Source) do
    Copy(Source[iLcv]^,Destination[iLcv]^);
end;

function  Update(Var List:Core.Arrays.Types.KeyStrings; Key,Value:Core.Strings.VarString):System.Int64;
begin
  Result:=IndexOf(List,Key);
  if Result=-1 then begin
    Result:=Length(List);
    SetLength(List,Result+1);
    New(List[Result]);
    Init(List[Result]^);
    List[Result]^.Key:=Key;
  end;
  List[Result]^.Value:=Value;
end;

function  Update(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Streams:boolean):System.Int64;
begin
  Result:=IndexOf(List,Key);
  if Result<>-1 then
    List[Result]^.Streams:=Streams;
end;

function  Update(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Value:System.QWord):System.Int64;
begin
  Result:=IndexOf(List,Key);
  if Result=-1 then begin
    Result:=Length(List);
    SetLength(List,Result+1);
    New(List[Result]);
    Init(List[Result]^);
    List[Result]^.Key:=Key;
  end;
  List[Result]^.Value:=Core.Strings.toString(Value);
end;

function  GetItem(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt):Core.Strings.PKeyString;
var
  iLcv:LongInt;
begin
  iLcv:=0; Result:=nil;
  While (iLcv<Count) and (Result=nil) do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=List[iLcv];
      Break;
    end;
    Inc(iLcv);
  end;
end;

function  GetItemByKey(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  iLcv:=0; SetLength(Result,0);
  While (iLcv<Count) and (Result='') do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=List[iLcv]^.Value;
      Break;
    end;
    Inc(iLcv);
  end;
end;

function  GetItemAsInt64(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Int64=0):System.Int64;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=Core.Strings.toInt(List[iLcv]^.Value,Default);
      Break;
    end;
  end;
end;

function  GetItemAsQWord(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.QWord=0):System.QWord;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=Core.Strings.toInt(List[iLcv]^.Value,Default);
      Break;
    end;
  end;
end;

function  GetItemAsWord(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Word=0):System.Word;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=Core.Strings.toInt(List[iLcv]^.Value,Default);
      Break;
    end;
  end;
end;

function  GetItemAsInt64Array(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; out Items:Core.Arrays.Types.LargeInt; Count:LongInt=0; const Default:System.Int64=0):LongInt;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=-1;
  for iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Core.Arrays.LargeInt.fromText(List[iLcv]^.Value,Items);
      Result:=System.Length(Items);
      Break;
    end;
  end;
end;

function  GetItemAsQWordArray(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; out Items:Core.Arrays.Types.LargeWord; Count:LongInt=0; const Default:System.QWord=0):LongInt;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=-1;
  for iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Core.Arrays.LargeWord.fromText(List[iLcv]^.Value,Items);
      Result:=System.Length(Items);
      Break;
    end;
  end;
end;

function  GetItemAsShortString(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:Core.Strings.Small=''):Core.Strings.Small;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=List[iLcv]^.Value;
      Break;
    end;
  end;
end;

function  GetItemAsString(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:Core.Strings.VarString=''):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=List[iLcv]^.Value;
      Break;
    end;
  end;
end;

procedure SetStreamsByKey(var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Streams:Boolean; Count:LongInt=0);
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  For iLcv:=0 to Count-1 do begin
      If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
        List[iLcv]^.Streams:=Streams;
        Break;
      end;
    end;
end;

function  GetItemAsDouble(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Double=0):System.Double;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=Core.Strings.toFloat(List[iLcv]^.Value,Default);
      Break;
    end;
  end;
end;

function  GetItemAsInteger(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:LongInt=0):LongInt;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=Core.Strings.toInt(List[iLcv]^.Value,Default);
      Break;
    end;
  end;
end;

function  GetItemAsByte(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Count:LongInt=0; const Default:System.Byte=0):System.Byte;
var
  iLcv:LongInt;
begin
  if Count=0 then Count:=Length(List);
  Result:=Default;
  For iLcv:=0 to Count-1 do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=Core.Strings.toInt(List[iLcv]^.Value,Default);
      Break;
    end;
  end;
end;

function  GetItemByKey(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iLcv:=0; SetLength(Result,0); iCount:=System.Length(List);
  While (iLcv<iCount) and (Result='') do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then begin
      Result:=List[iLcv]^.Value;
      Break;
    end;
    Inc(iLcv);
  end;
end;

procedure GetItemsByKey(Var List:Core.Arrays.Types.KeyStrings; Key:Core.Strings.VarString; Out Result:Core.Arrays.Types.VarString);
var
  iLcv:LongInt;
begin
  iLcv:=0; SetLength(Result,0);
  While (iLcv<Length(List)) do begin
    If Core.Strings.SameText(List[iLcv]^.Key,Key) then
      Core.Arrays.VarString.Add(Result,List[iLcv]^.Value);
    Inc(iLcv);
  end;
end;

function Exchange(srcIndex,destIndex:LongInt; var Items:Core.Arrays.Types.KeyStrings):System.boolean;
var
  iCount:LongInt;
  src,dest:Core.Strings.PKeyString;
begin
  Result:=false;
  iCount:=System.Length(Items);
  if (srcIndex=-1) or (destIndex=-1) or (srcIndex>=iCount) or (destIndex>=iCount) then exit;
  src:=Items[srcIndex];
  dest:=Items[destIndex];
  Items[srcIndex]:=dest;
  Items[destIndex]:=src;
  Result:=True;
end;


initialization


end.



unit Core.Arrays.Bytes;

{$mode objfpc}{$H+}

interface

uses
  db,
  Core.Arrays.Types,
  Core.Streams.Types,
  Core.Strings;

  procedure Copy(var Source,Dest:MD5Digest); overload;

  function  Copy(Var Source,Destination:Core.Arrays.Types.Bytes):Int64; overload;
  function  Replace(Source:Core.Arrays.Types.Bytes; var Destination:Core.Arrays.Types.Bytes):Int64;

  function  Copy(Var Source,Destination:System.Byte):System.boolean; overload;


  procedure Empty(Var Item:MD5Digest); overload;

  procedure Empty(Var Item:Core.Arrays.Types.Bytes); overload;
  procedure Empty(Var Item:Core.Arrays.Types.BytesManifest); overload;

  procedure Init(Var Item:Core.Arrays.Types.Bytes); overload;
  procedure Init(Var Item:Core.Arrays.Types.BytesManifest); overload;

  procedure Done(Var Item:Core.Arrays.Types.Bytes); overload;
  procedure Done(Var Item:Core.Arrays.Types.BytesManifest); overload;

  function  Add(Item:System.Byte; Var List:Core.Arrays.Types.Bytes):Int64;
  function  Delete(Item:System.Byte; Var List:Core.Arrays.Types.Bytes):Int64;
  function  IndexOf(Item:System.Byte; Var List:Core.Arrays.Types.Bytes):Int64;

  function  toHexString(BufferP:Pointer; Len:Int64): Core.Strings.VarString; overload;

  procedure fromString(var Digest:MD5Digest; Value:Core.Strings.VarString); overload;
  function  fromString(var sData:Core.Strings.VarString; Var List:Core.Arrays.Types.Bytes):Int64; overload;
  function  fromString(sData:Core.Strings.VarString; ListP:Core.Arrays.Types.PBytes):Int64; overload;
  function  fromString(sData:Core.Strings.VarString; Var Item:System.Byte):Int64; overload;

  function  toString(Var List:Core.Arrays.Types.Bytes):Core.Strings.VarString; overload;
  function  toBlob(Var List:Core.Arrays.Types.Bytes):TBlobData;
  function  toString(Var List:Core.Arrays.Types.Bytes; const Length:Int64):Core.Strings.VarString; overload;
  function  toFile(Var List:Core.Arrays.Types.Bytes; FileName:Core.Strings.VarString):System.Int64; overload;

  function  fromStream(ListP:Core.Arrays.Types.PBytes; Stream:Core.Streams.Types.Base; Const iOffset:System.Int64):System.Int64; overload;
  function  fromStream(ListP:Core.Arrays.Types.PBytes; ListSize:Int64; Stream:Core.Streams.Types.Base; Const iOffset:System.Int64):System.Int64; overload;
  function  fromFile(FileName:Core.Strings.VarString; var List:Core.Arrays.Types.Bytes):Int64; overload;

  function  SetSize(Var List:Core.Arrays.Types.Bytes; Size:Int64):Int64;

  function  Pos(Var Buffer:Core.Arrays.Types.Bytes; Term:Core.Strings.VarString):System.Int64;
  function  CheckSum(var Buffer:Core.Arrays.Types.Bytes):Core.Strings.VarString; overload;
  procedure CheckSum(var Buffer:Core.Arrays.Types.Bytes; var Digest:MD5Digest); overload;
  function  Same(var One,Two:MD5Digest):System.Boolean;
  procedure Append(var sData:Core.Strings.VarString; Var List:Core.Arrays.Types.Bytes);


implementation
uses MD5,SysUtils,Encryption.Base64;

function  Pos(Var Buffer:Core.Arrays.Types.Bytes; Term:Core.Strings.VarString):System.Int64;
var
  iSize:System.Int64;
  iLcv:System.Int64;
  iLen:System.Int64;
  sBuffer:Core.Strings.VarString;
begin
  Result:=-1; iLcv:=0; iLen:=System.Length(Term);  iSize:=Length(Buffer); SetLength(sBuffer,iLen);
  if iLen>0 then begin
    While (iLcv<iSize) and (Result=-1) do begin
      System.Move(Buffer[iLcv],sBuffer[1],iLen);
      if Core.Strings.SameText(sBuffer,Term) then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;
end;

function  Replace(Source:Core.Arrays.Types.Bytes; var Destination:Core.Arrays.Types.Bytes):Int64;
var
  iLcv                           : LongInt;
begin
  Result:=Length(Source);
  SetSize(Destination,Result);
  For iLcv:=0 to Result-1 do
    Destination[iLcv]:=Source[iLcv];
end;

function Copy(var Source,Destination:Core.Arrays.Types.Bytes):Int64;
var
  iLcv                           : LongInt;
begin
  Result:=Length(Source);
  SetSize(Destination,Result);
  For iLcv:=0 to Result-1 do
    Destination[iLcv]:=Source[iLcv];
end;

function  Copy(Var Source,Destination:System.Byte):boolean;
begin
  Result:=True;
  Destination:=Source;
end;

procedure Empty(Var Item:MD5Digest);
begin
  FillChar(Item[0],SizeOf(Item),0);
end;

procedure Empty(Var Item:Core.Arrays.Types.Bytes);
begin
  SetLength(Item,0);
end;

procedure Empty(Var Item:Core.Arrays.Types.BytesManifest);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Init(Var Item:Core.Arrays.Types.BytesManifest);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;


procedure Done(Var Item:Core.Arrays.Types.Bytes);
begin
  Finalize(Item);
end;

procedure Done(Var Item:Core.Arrays.Types.BytesManifest);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure Init(Var Item:Core.Arrays.Types.Bytes);
begin
  SetLength(Item,0);
end;

function  Add(Item:System.Byte; Var List:Core.Arrays.Types.Bytes):Int64;
begin
  Result:=Length(List);
  SetSize(List,Result+1);
  List[Result]:=Item;
end;

function  Delete(Item:System.Byte; Var List:Core.Arrays.Types.Bytes):Int64;
var
  iIndex                         : Int64;
  iCount                         : Int64;
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

function  IndexOf(Item:System.Byte; Var List:Core.Arrays.Types.Bytes):Int64;
var
  iLcv                           : Int64;
  iCount                         : Int64;
begin
  iCount:=Length(List); iLcv:=0; Result:=-1;
  while (iLcv<iCount) and (Result=-1) do begin
    If List[iLcv]=Item then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  SetSize(Var List:Core.Arrays.Types.Bytes; Size:Int64):Int64;
var
  iLcv                           : LongInt;
  iCount                         : Int64;
begin
  iCount:=Length(List);
  if (Size>iCount) then begin
    // Grow List
    SetLength(List,Size);
    for iLcv:=Size-1 downto iCount do
      List[iLcv]:=0;
    Result:=Size;
  end else if (Size<iCount) then begin
    // Shrink List
    SetLength(List,Size);
    Result:=Size;
  end else begin
    Result:=iCount;
  end;
end;

Function   toHexString(BufferP:Pointer; Len:Int64):Core.Strings.VarString;
Var
  iLcv:LongInt;
begin
  iLcv:=1;
  Result:='';
  While iLcv<=Len do begin
    Result:=Concat(Result,Format('%.2x',[System.PByte(BufferP)^]));
    Inc(PByte(BufferP));
    Inc(iLcv);
  end;
end;

procedure Append(var sData:Core.Strings.VarString; Var List:Core.Arrays.Types.Bytes);
var
  iDataLen:Int64;
  iBufLen:Int64;
begin
  iDataLen:=Length(sData);
  If iDataLen>0 then begin
    iBufLen:=Length(List);
    SetLength(List,iDataLen+iBufLen);
    System.Move(sData[1],List[iBufLen],iDataLen);
  end;
end;


function  fromStream(ListP:Core.Arrays.Types.PBytes; ListSize:System.Int64; Stream:Core.Streams.Types.Base; Const iOffset:System.Int64):System.Int64;
var
  iCount:System.Int64;
begin
  iCount:=Stream.Size-iOffset;
  if iCount>ListSize then
    iCount:=ListSize;
  if iCount>0 then begin
    Stream.Position:=iOffset;
    Stream.ReadBuffer(ListP^[0],iCount);
  end;
  Result:=iCount;
end;

function  fromStream(ListP:Core.Arrays.Types.PBytes; Stream:Core.Streams.Types.Base; Const iOffset:System.Int64):System.Int64;
var
  iCount:System.Int64;
begin
  iCount:=Stream.Size-iOffset;
  if iCount>0 then begin
    System.SetLength(ListP^,iCount);
    Stream.Position:=iOffset;
    Stream.ReadBuffer(ListP^[0],iCount);
  end else begin
    iCount:=0;
    System.SetLength(ListP^,iCount);
  end;
  Result:=iCount;
end;

procedure fromString(var Digest:MD5Digest; Value:Core.Strings.VarString);
const
  MD5DigestSize=16;
begin
  FillByte(Digest,MD5DigestSize,0);
  if System.Length(Value)=MD5DigestSize then
    System.Move(Value[1],Digest[0],MD5DigestSize);
end;

function  fromString(sData:Core.Strings.VarString; ListP:Core.Arrays.Types.PBytes):Int64;
begin
  Result:=fromString(sData,ListP^);
end;

function  fromString(var sData:Core.Strings.VarString; Var List:Core.Arrays.Types.Bytes):Int64;
begin
  Result:=Encryption.Base64.Decode(sData,List);
end;

function  fromString(sData:Core.Strings.VarString; var Item:System.Byte):Int64;
begin
  Result:=Length(sData);
  if Result>0 then
    Item:=Ord(sData[1]);
end;

function  toString(Var List:Core.Arrays.Types.Bytes):Core.Strings.VarString;
var
  iSize:LongInt;
begin
  iSize:=Length(List);
  SetLength(Result,iSize);
  if iSize>0 then
    System.Move(List[0],Result[1],iSize);
end;

function  toBlob(Var List:Core.Arrays.Types.Bytes):TBlobData;
var
  iSize:LongInt;
begin
  iSize:=Length(List);
  SetLength(Result,iSize);
  if iSize>0 then
    System.Move(List[0],Result[1],iSize);
end;

function  toString(Var List:Core.Arrays.Types.Bytes; const Length:Int64):Core.Strings.VarString;
begin
  Encryption.Base64.Encode(List,Result,Length);
end;

function  toFile(Var List:Core.Arrays.Types.Bytes; FileName:Core.Strings.VarString):System.Int64;
var
  FS:Core.Streams.Types.Disk;
  iSize:Int64;
begin
  FS:=Core.Streams.Types.Disk.Create(FileName,fmCreate);
  Try
    iSize:=Length(List);
    if iSize>0 then
      FS.WriteBuffer(List[0],iSize);
  finally
    FreeAndNil(FS);
  end;
  Result:=iSize;
end;

function  fromFile(FileName:Core.Strings.VarString; var List:Core.Arrays.Types.Bytes):Int64;
var
  FS:Core.Streams.Types.Disk;
  iTotal,iChunk,iRead:System.Int64;
begin
  FS:=Core.Streams.Types.Disk.Create(FileName,fmOpenRead);
  Try
    SetLength(List,FS.Size);
    iRead:=0; iChunk:=FS.Size;
    FS.Position:=0; iTotal:=0;
    while (iChunk>0) do begin
      iRead:=FS.Read(List[iRead],iChunk);
      inc(iTotal,iRead);
      iChunk:=(FS.Size-iTotal);
    end;
  finally
    FreeAndNil(FS);
  end;
  Result:=iTotal;
end;

procedure CheckSum(var Buffer:Core.Arrays.Types.Bytes; var Digest:MD5Digest);
var
  ctMD5:MD5Context;
  iLen:Int64;
begin
  iLen:=System.Length(Buffer);
  if iLen>0 then begin
    MD5Init(ctMD5);
    MD5Update(ctMD5,Buffer[0],iLen);
    MD5Final(ctMD5,Digest);
  end else
    System.FillChar(Digest[0],SizeOf(Digest),0);
end;

function  Same(var One,Two:MD5Digest):Boolean;
var
  iLcv:LongInt;
begin
  for iLcv:=Low(MD5Digest) to High(MD5Digest) do begin
    if One[iLcv]<>Two[iLcv] then begin
      Result:=false;
      Exit;
    end;
  end;
  Result:=True;
end;

procedure  Copy(var Source,Dest:MD5Digest);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(MD5Digest) to High(MD5Digest) do
    Dest[iLcv]:=Source[iLcv];
end;

function  CheckSum(var Buffer:Core.Arrays.Types.Bytes):Core.Strings.VarString;
var
  dgMD5:MD5Digest;
  ctMD5:TMD5Context;
  iLen:Int64;
begin
  iLen:=System.Length(Buffer);
  if iLen>0 then begin
    MD5Init(ctMD5);
    MD5Update(ctMD5,Buffer[0],iLen);
    MD5Final(ctMD5,dgMD5);
    Result:=MD5Print(dgMD5);
  end else
    Result:='';;
end;


end.


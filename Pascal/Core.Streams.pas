unit Core.Streams;

interface

uses
  Core,
  Core.Strings,
  Core.Streams.Types,
  Core.Arrays.Types;


  function Skipline(Stream:Base):Int64;
  function Readline(Stream:Base; Var Position:Int64):Core.Strings.VarString; overload;
  function Readline(Stream:Base; Var Position:Cardinal):Core.Strings.VarString; overload;
  function Pos(Stream:Base; Term:Core.Strings.VarString; const iStart:Int64=0):Int64; overload;
  function Pos(Stream:Base; Term:Byte):Int64; overload;
  function  Partial(Stream:Base; iStart,iStop:QWord; out iCount:QWord):Core.Strings.VarString; overload;
  function  Extract(Stream:Base; iStart,iCount:Int64; const Options:Option=soNone):Core.Strings.VarString; overload;
  procedure Extract(Stream:Base; iStart,iCount:Int64; out Data:Core.Strings.VarString; const Options:Option=Core.Streams.Types.soNone); overload;
  procedure Extract(Stream:Base; iStart,iCount:Int64; out Data:Core.Strings.Wide; const Options:Option=Core.Streams.Types.soNone); overload;
  function  Extract(Stream:Base; iStart:Int64; StopTerm:Byte; Refactor:Base):Core.Strings.VarString; overload;
  procedure Extract(Stream:Base; iStart:Int64; StopTerm:Byte; Refactor:Base; out Data:Core.Strings.VarString) overload;
  procedure Extract(Stream:Base; iStart:Int64; StopTerm:Word; Refactor:Base; out Data:Core.Strings.Wide) overload;
  function  Extract(Stream:Base; iStart,iStop:Int64; StopTerm:Byte; var List:Core.Arrays.Types.VarString; Refactor:Core.Streams.Types.Base):QWord; overload;
  function  Extract(Stream:Base; iStart,iStop:Int64; StopTerm:Byte; var List:Core.Arrays.Types.KeyStrings; Refactor:Base; Encoding:Core.Streams.Types.Encoding):QWord; overload;
  procedure fromString(Value:Core.Strings.VarString; Stream:Base); overload;
  procedure fromData(Var Data:Core.Arrays.Types.Bytes; Stream:Base); overload;
  procedure Append(Var Value:Core.Strings.VarString; Length:QWord; Stream:Base); overload;
  procedure fromFile(Value:Core.Strings.FileName; Stream:Base); overload;
  function  toString(Stream:Base):Core.Strings.VarString; overload;
  procedure Write(Value:Core.Strings.VarString; Length:QWord; Stream:Base); overload;
  function  Write(Value:Core.Strings.VarString; Stream:Base):QWord; overload;
  function  Write(var Value:Core.Arrays.Types.Bytes; Stream:Base):QWord; overload;
  function  Write(var Value:Core.Arrays.Types.VarString; Stream:Base):QWord; overload;
  function  ReWrite(var Value:Core.Strings.VarString; Stream:Base):QWord; overload;
  function  WriteLine(Value:Core.Strings.VarString; Stream:Base):QWord; overload;
  function  EnterLine(Value:Core.Strings.VarString; Stream:Base):QWord;
  procedure toFile(Stream:Base; FileName:Core.Strings.Filename); overload;
  procedure toFile(Stream:Base; iOffset,iLength:Cardinal; FileName:Core.Strings.Filename); overload;
  procedure Refactor(Stream,Refactor:Base; iStart:Int64);
  procedure Copy(Source,Destination:Base); overload;
  procedure Copy(var Source:Core.Strings.VarString; Destination:Base); overload;
  procedure Copy(var Source:Core.Arrays.Types.Bytes; Destination:Base); overload;
  procedure CopyFrom(Source,Destination:Base; iStart,iLength:Int64); overload;
  procedure CheckSum(Source:Base; out Digest:Core.Arrays.Types.MD5Digest);
  function  toMemo(Stream:Base):Core.Strings.VarString;

implementation
  uses Core.Arrays.VarString, Core.Arrays.KeyString,MD5;

procedure toFile(Stream:Core.Streams.Types.Base; iOffset,iLength:Cardinal; FileName:Core.Strings.FileName);
const
  MAX_CHUNK=1002040;
var
  FS     : Core.Streams.Types.Disk;
  Buffer : Array[0..MAX_CHUNK] of Byte;
  iChunk : QWord;
  iCount : QWord;
  iRead  : QWord;
begin
  FS:=Core.Streams.Types.Disk.Create(FileName,fmCreate);
  Try
    Stream.Position:=iOffset;
    iCount:=0;
    while (iCount<iLength) do begin
      iChunk:=MAX_CHUNK;
      if (iCount+iChunk>iLength) then
        iChunk:=iLength-iCount;
      iRead:=Stream.Read(Buffer[0],iChunk);
      Inc(iCount,iRead);
      FS.Write(Buffer[0],iRead);
    end;
  finally
    FreeAndNil(FS);
  end;
end;


procedure toFile(Stream:Core.Streams.Types.Base; FileName:Core.Strings.FileName);
const
  MAX_CHUNK=1002040;
var
  FS:Core.Streams.Types.Disk;
  Buffer:Array[0..MAX_CHUNK] of Byte;
  iChunk,iSize,iCount:QWord;
  iRead:Int64;
begin
  FS:=Core.Streams.Types.Disk.Create(FileName,fmCreate);
  Try
    Stream.Position:=0;
    iSize:=Stream.Size;
    iCount:=0;
    while iCount<iSize do begin
      iChunk:=MAX_CHUNK;
      if (iCount+iChunk>iSize) then
        iChunk:=iSize-iCount;
      iRead:=Stream.Read(Buffer[0],iChunk);
      Inc(iCount,iRead);
      FS.Write(Buffer[0],iRead);
    end;
  finally
    FreeAndNil(FS);
  end;
end;

function toMemo(Stream:Core.Streams.Types.Base):Core.Strings.VarString;
var
  ssData:Core.Streams.Types.VarString;
begin
  Stream.Position:=0;
  ssData:=Core.Streams.Types.VarString.Create('');
  try
    ssData.CopyFrom(Stream,0);
    Result:=ssData.DataString;
  Finally
    FreeAndNil(ssData);
  end;
end;

procedure fromFile(Value:Core.Strings.FileName; Stream:Core.Streams.Types.Base);
var
  FS:Core.Streams.Types.Disk;
begin
  FS:=Core.Streams.Types.Disk.Create(Value,fmOpenRead);
  Try
    Stream.Size:=0;
    Stream.CopyFrom(FS,FS.Size);
  finally
    FreeAndNil(FS);
  end;
end;

procedure Append(Var Value:Core.Strings.VarString; Length:QWord; Stream:Core.Streams.Types.Base);
begin
  if (Length>0) then
    Stream.WriteBuffer(Value[1],Length);
end;

procedure Write(Value:Core.Strings.VarString; Length:QWord; Stream:Core.Streams.Types.Base);
begin
  if Length>0 then
    Stream.WriteBuffer(Value[1],Length);
end;

function  Write(Value:Core.Strings.VarString; Stream:Core.Streams.Types.Base):QWord;
const
  BufLen = 1024*1024;
var
  iResult:Int64;
  iRead:Int64;
  iPosition:Int64;
  iSize:Int64;
begin
  Stream.Position:=Stream.Size;
  iPosition:=0; iSize:=System.Length(Value);
  Result:=iSize;
  While (iPosition<iSize) do begin
    iRead:=BufLen;
    if (iRead+iPosition>iSize) then
      iRead:=iSize-iPosition;
    iResult:=Stream.Write(Value[iPosition+1],iRead);
    inc(iPosition,iResult);
  end;
end;

function  Write(var Value:Core.Arrays.Types.Bytes; Stream:Core.Streams.Types.Base):QWord;
begin
  Result:=System.Length(Value);
  if Result>0 then
    Stream.WriteBuffer(Value[0],Result);
end;

function  Write(var Value:Core.Arrays.Types.VarString; Stream:Core.Streams.Types.Base):QWord;
var
  iLcv:LongInt;
  iLen:Int64;
begin
  for iLcv:=0 to High(Value) do begin
    iLen:=Length(Value[iLcv]);
    if iLen>0 then
      Stream.WriteBuffer(Value[iLcv][1],iLen);
    Stream.WriteBuffer(#13#10,2);
  end;
  Result:=Stream.Size;
end;

function  ReWrite(var Value:Core.Strings.VarString; Stream:Core.Streams.Types.Base):QWord;
begin
  Stream.Size:=0;
  Result:=Length(Value);
  if Result>0 then
    Stream.WriteBuffer(Value[1],Result);
end;


function  WriteLine(Value:Core.Strings.VarString; Stream:Core.Streams.Types.Base):QWord;
begin
  Result:=Length(Value);
  if Result>0 then
    Stream.WriteBuffer(Value[1],Result);
  Stream.WriteBuffer(#13#10,2);
end;

function  EnterLine(Value:Core.Strings.VarString; Stream:Core.Streams.Types.Base):QWord;
begin
  Result:=Length(Value);
  if Result>0 then
    Stream.WriteBuffer(Value[1],Result);
  {$if defined(Unix)}
    Stream.WriteBuffer(#10#10,1);
  {$else}
    Stream.WriteBuffer(#13#10,2);
  {$endif}
end;

function  toString(Stream:Core.Streams.Types.Base):Core.Strings.VarString;
var
  iLen:QWord;
  Buffer:Core.Strings.Raw;
begin
  iLen:=Stream.Size;
  SetLength(Buffer,iLen);
  if iLen>0 then begin
    Stream.Position:=0;
    Stream.Read(Buffer[1],iLen);
  end;
  Result:=UTF8Encode(Buffer);
end;

procedure fromData(Var Data:Core.Arrays.Types.Bytes; Stream:Core.Streams.Types.Base);
var
  iLen:QWord;
begin
  Stream.Size:=0; iLen:=System.Length(Data);
  if iLen>0 then
    Stream.Write(Data[0],iLen);
  Stream.Position:=0;
end;

procedure fromString(Value:Core.Strings.VarString; Stream:Core.Streams.Types.Base);
var
  iLen:QWord;
begin
  Stream.Size:=0; iLen:=System.Length(Value);
  if iLen>0 then
    Stream.Write(Value[1],iLen);
  Stream.Position:=0;
end;

function Extract(Stream:Core.Streams.Types.Base; iStart,iCount:Int64; const Options:Option=Core.Streams.Types.Option.soNone):Core.Strings.VarString;
var
  iRead:Int64;
  procedure CleanResult;
  var
    iLcv:Cardinal;
  begin
    for iLcv:=1 to iRead do begin
      if (Result[iLcv]=#0) then begin
        System.SetLength(Result,iLcv);
        Break;
      end;
    end;
  end;

begin
  iRead:=0;
  Stream.Position:=iStart;
  System.SetLength(Result,iCount);
  if iCount>0 then
    iRead:=Stream.Read(Result[1],iCount);
  while (iRead>0) and (Result[iRead]=#0) do
    iRead-=1;
  SetLength(Result,iRead);
  case Options of
    soClean: CleanResult;
  end;
end;

function Partial(Stream:Core.Streams.Types.Base; iStart,iStop:QWord; out iCount:QWord):Core.Strings.VarString;
begin
  iCount:=(iStop-iStart)+1;
  Stream.Position:=iStart;
  if iCount>=1 then begin
    System.SetLength(Result,iCount);
    iCount:=Stream.Read(Result[1],iCount);
    System.SetLength(Result,iCount);
  end else begin
    iCount:=0;
    System.SetLength(Result,0);
  end;
end;

procedure Extract(Stream:Core.Streams.Types.Base; iStart,iCount:Int64; out Data:Core.Strings.VarString; const Options:Option=Core.Streams.Types.soNone);
var
  iRead:Int64;

  procedure CleanResult;
  var
    iLcv:Cardinal;
  begin
    for iLcv:=1 to iRead do begin
      if (Data[iLcv]=#0) then begin
        System.SetLength(Data,iLcv-1);
        Break;
      end;
    end;
  end;
begin
  iRead:=0;
  Stream.Position:=iStart;
  System.SetLength(Data,iCount);
  if iCount>0 then
    iRead:=Stream.Read(Data[1],iCount);
  while (iRead>0) and (Data[iRead]=#0) do
    iRead-=1;
  SetLength(Data,iRead);
  case Options of
    soClean: CleanResult;
  end;
end;

procedure Extract(Stream:Core.Streams.Types.Base; iStart,iCount:Int64; out Data:WideString; const Options:Option=Core.Streams.Types.soNone);
var
  iRead:Int64;

  procedure CleanResult;
  var
    iLcv:Cardinal;
  begin
    for iLcv:=1 to iRead do begin
      if (Data[iLcv]=#0) then begin
        System.SetLength(Data,iLcv-1);
        Break;
      end;
    end;
  end;
begin
  iRead:=0;
  Stream.Position:=iStart;
  System.SetLength(Data,iCount);
  if iCount>0 then
    iRead:=Stream.Read(Data[1],iCount);
  while (iRead>0) and (Data[iRead]=#0) do
    iRead-=1;
  SetLength(Data,iRead);
  case Options of
    soClean: CleanResult;
  end;
end;

function Extract(Stream:Core.Streams.Types.Base; iStart:Int64; StopTerm:Byte; Refactor:Core.Streams.Types.Base ):Core.Strings.VarString;
var
  iRead:Int64;
  iPosition:Int64;
  iSize:Int64;
  iByte:Byte;
begin
  iPosition:=iStart;
  iSize:=Stream.Size;
  Refactor.Size:=0; iByte:=1;
  Stream.Position:=iStart;
  While (iByte>0) and (iByte<>StopTerm) and (iPosition<iSize) do begin
    Stream.Read(iByte,1);
    Refactor.Write(iByte,1);
    iPosition+=1;
  end;
  If (iByte=StopTerm) and (Refactor.Size>0) then
    Refactor.Size:=Refactor.Size-1;

  iRead:=Refactor.Size;
  SetLength(Result,iRead);
  Refactor.Position:=0;
  if iRead>0 then
    Refactor.Read(Result[1],iRead);
  Refactor.Size:=0;
end;

procedure Extract(Stream:Core.Streams.Types.Base; iStart:Int64; StopTerm:Byte; Refactor:Core.Streams.Types.Base; out Data:Core.Strings.VarString);
var
  iRead:Int64;
  iPosition:Int64;
  iSize:Int64;
  iByte:Byte;
begin
  iPosition:=iStart;
  iSize:=Stream.Size;
  Refactor.Size:=0; iByte:=1;
  Stream.Position:=iStart;
  While (iByte>0) and (iByte<>StopTerm) and (iPosition<iSize) do begin
    Stream.Read(iByte,1);
    Refactor.Write(iByte,1);
    iPosition+=1;
  end;
  If (iByte=StopTerm) and (Refactor.Size>0) then
    Refactor.Size:=Refactor.Size-1;

  iRead:=Refactor.Size;
  SetLength(Data,iRead);
  Refactor.Position:=0;
  if iRead>0 then
    Refactor.Read(Data[1],iRead);
  Refactor.Size:=0;
end;

procedure Extract(Stream:Core.Streams.Types.Base; iStart:Int64; StopTerm:Word; Refactor:Core.Streams.Types.Base; out Data:Core.Strings.Wide);
var
  iRead:Int64;
  iPosition:Int64;
  iSize:Int64;
  iByte:Word;

begin
  iPosition:=iStart;
  iSize:=Stream.Size;
  Refactor.Size:=0;
  iByte:=1;
  Stream.Position:=iStart;
  While (iByte>0) and (iByte<>StopTerm) and (iPosition<iSize) do begin
    Stream.Read(iByte,2);
    Refactor.Write(iByte,2);
    iPosition+=1;
  end;
  If (iByte=StopTerm) and (Refactor.Size>0) then
    Refactor.Size:=Refactor.Size-2;

  iRead:=Refactor.Size;
  SetLength(Data,iRead);
  Refactor.Position:=0;
  if iRead>0 then
    Refactor.Read(Data[1],iRead);
  Refactor.Size:=0;
end;


function Extract(Stream:Core.Streams.Types.Base; iStart,iStop:Int64; StopTerm:Byte; var List:Core.Arrays.Types.VarString; Refactor:Core.Streams.Types.Base):QWord;
var
  iRead:Int64;
  iCount:Cardinal;
  iPosition:Int64;
  iSize:Int64;
  iByte:Byte;
  sItem:Core.Strings.VarString;
begin
  iPosition:=iStart;
  iSize:=Stream.Size;
  System.InterLockedExchange(iCount,0);
  Stream.Position:=iStart;
  While (iPosition<iStop) and (iPosition<iSize) do begin
    Refactor.Size:=0; iByte:=1;
    While (iByte>0) and (iByte<>StopTerm) and (iPosition<iStop) and (iPosition<iSize) do begin
      Stream.Read(iByte,1);
      Refactor.Write(iByte,1);
      iPosition+=1;
    end;
    If (iByte=StopTerm) and (Refactor.Size>0) then
      Refactor.Size:=Refactor.Size-1;

    iRead:=Refactor.Size;
    SetLength(sItem,iRead);
    Refactor.Position:=0;
    if iRead>0 then begin
      Refactor.Read(sItem[1],iRead);
      Refactor.Size:=0;
      Core.Arrays.VarString.Add(List,sItem);
      SetLength(sItem,0);
      System.InterLockedIncrement(iCount);
    end;
  end;
  Result:=iCount;
end;

function Extract(Stream:Core.Streams.Types.Base; iStart,iStop:Int64; StopTerm:Byte; var List:Core.Arrays.Types.KeyStrings; Refactor:Core.Streams.Types.Base; Encoding:Core.Streams.Types.Encoding):QWord;
var
  iRead:Int64;
  iCount:Cardinal;
  iPosition:Int64;
  iSize:Int64;
  iByte:Byte;
  wByte:Word;
  sWide:Core.Strings.Wide;
  kpItem:Core.Strings.PKeyString;

  function getValueSingle():Core.Strings.VarString;
  begin
    Refactor.Size:=0;
    iByte:=1;
    While (iByte>0) and (iByte<>StopTerm) and (iPosition<iStop) and (iPosition<iSize) do begin
      Stream.Read(iByte,1);
      Refactor.Write(iByte,1);
      iPosition+=1;
    end;
    If (iByte=StopTerm) and (Refactor.Size>0) then
      Refactor.Size:=Refactor.Size-1;

    iRead:=Refactor.Size;
    SetLength(Result,iRead);
    Refactor.Position:=0;
    if iRead>0 then begin
      Refactor.Read(Result[1],iRead);
      Refactor.Size:=0;
    end;
  end;

  function getValueWide():Core.Strings.VarString;
  begin
    Refactor.Size:=0;
    wByte:=1;
    While (wByte>0) and (wByte<>StopTerm) and (iPosition<iStop) and (iPosition<iSize) do begin
      Stream.Read(wByte,2);
      Refactor.Write(wByte,2);
      iPosition+=1;
    end;
    If (iByte=StopTerm) and (Refactor.Size>0) then
      Refactor.Size:=Refactor.Size-2;

    iRead:=Refactor.Size;
    SetLength(sWide,iRead);
    Refactor.Position:=0;
    if iRead>0 then begin
      Refactor.Read(sWide[1],iRead);
      Refactor.Size:=0;
      Result:=System.Utf8ToAnsi(sWide);
    end else begin
      SetLength(Result,0);
    end;
  end;

  procedure ProcessSingle;
  begin
    While (iPosition<iStop) and (iPosition<iSize) do begin
      New(kpItem);
      Core.Arrays.KeyString.Init(kpItem^);
      Core.Arrays.KeyString.Append(kpItem,List);
      kpItem^.Key:=getValueSingle();
      kpItem^.Value:=getValueSingle();
    end;
  end;

  procedure ProcessUTF;
  begin
    While (iPosition<iStop) and (iPosition<iSize) do begin
      New(kpItem);
      Init(kpItem^);
      Core.Arrays.KeyString.Append(kpItem,List);
      kpItem^.Key:=getValueWide();
      kpItem^.Value:=getValueWide();
    end;
  end;


begin
  Core.Arrays.KeyString.Empty(List);
  iPosition:=iStart;
  iSize:=Stream.Size;
  iCount:=0;
  Stream.Position:=iStart;
  case Encoding of
    seNormal  : ProcessSingle;
    seUTF16   : ProcessUTF;
    seUTF16BE : ProcessUTF;
    seUTF8    : ProcessUTF;
  end;
  Result:=iCount;
end;

procedure Refactor(Stream,Refactor:Base; iStart:Int64);
var
  iSize:Int64;
  iDiff:QWord;
begin
  Refactor.Size:=0;
  Try
    iSize:=Stream.Size;
    iDiff:=iSize-iStart;
    if iDiff>0 then begin
      Stream.Position:=iStart;
      Refactor.CopyFrom(Stream,iDiff);
      iSize:=Refactor.Size;
    end;
    Refactor.Position:=0;
    Stream.Size:=0;
    Stream.CopyFrom(Refactor,iDiff);
    Stream.Position:=0;
  finally
    Refactor.Size:=0;
  end;
end;

procedure Copy(Source,Destination:Base);
const
  BufLen = 1024*1024;
var
  Buffer : Array[0..BufLen-1] of Byte;
  iResult:Int64;
  iRead:Int64;
  iPosition:Int64;
begin
  Destination.Position:=0;
  Destination.Size:=0;
  Source.Position:=0;
  iPosition:=0;
  While (iPosition<Source.Size) do begin
    iRead:=BufLen;
    if (iRead+iPosition>Source.Size) then
      iRead:=Source.Size-iPosition;
    iResult:=Source.Read(Buffer[0],iRead);
    if (iResult>0) then begin
      Destination.WriteBuffer(Buffer[0],iResult);
      inc(iPosition,iResult);
    end;
  end;
end;

procedure Copy(var Source:Core.Strings.VarString; Destination:Base);
const
  BufLen = 1024*1024;
var
  iResult:Int64;
  iRead:Int64;
  iPosition:Int64;
  iSize:Int64;
begin
  Destination.Size:=0;
  iPosition:=0; iSize:=System.Length(Source);
  While (iPosition<iSize) do begin
    iRead:=BufLen;
    if (iRead+iPosition>iSize) then
      iRead:=iSize-iPosition;
    iResult:=Destination.Write(Source[iPosition+1],iRead);
    if iResult>0 then begin
      inc(iPosition,iResult);
    end;
  end;
end;

procedure Copy(var Source:Core.Arrays.Types.Bytes; Destination:Base);
const
  BufLen = 1024*1024;
var
  iResult:Int64;
  iRead:Int64;
  iPosition:Int64;
  iSize:Int64;
begin
  Destination.Size:=0;
  iPosition:=0; iSize:=System.Length(Source);
  While (iPosition<iSize) do begin
    iRead:=BufLen;
    if (iRead+iPosition>iSize) then
      iRead:=iSize-iPosition;
    iResult:=Destination.Write(Source[iPosition],iRead);
    if iResult>0 then begin
      inc(iPosition,iResult);
    end;
  end;
end;


procedure CopyFrom(Source,Destination:Base; iStart,iLength:Int64);
const
  BufLen = 255*1024;
var
  Buf : Array[0..BufLen-1] of Byte;
  iResult:Int64;
  iRead:Int64;
  iAggRead:Int64;
  iPosition:Int64;
begin
  Destination.Size:=0;
  Source.Position:=iStart;
  iPosition:=iStart;
  iAggRead:=0;
  While (iPosition<Source.Size) and (iAggRead<iLength) do begin
    iRead:=BufLen;
    if (iRead+iPosition>Source.Size) then
      iRead:=Source.Size-iPosition;
    if (iRead+iAggRead>iLength) then
      iRead:=iLength-iAggRead;
    iResult:=Source.Read(Buf[0],iRead);
    Destination.Write(Buf[0],iResult);
    inc(iPosition,iResult);
    inc(iAggRead,iResult);
  end;
end;

procedure CheckSum(Source:Base; out Digest:MD5Digest);
const
  BufLen    = 1024*1024;
var
  Buffer    : Array[0..BufLen-1] of Byte;
  ctxMD5    : TMD5Context;
  iResult   : Int64;
  iRead     : Int64;
  iPosition : Int64;
begin
  FillChar(Digest,SizeOf(Digest),#0);
  if (Source<>nil) and (Source.Size>0) then begin
    Source.Position:=0;
    iPosition:=0;
    MD5.MD5Init(ctxMD5);
    While (iPosition<Source.Size) do begin
      iRead:=BufLen;
      if (iRead+iPosition>Source.Size) then
        iRead:=Source.Size-iPosition;
      iResult:=Source.Read(Buffer[0],iRead);
      if (iResult>0) then begin
        MD5.MD5Update(ctxMD5,Buffer[0],iResult);
        inc(iPosition,iResult);
      end;
    end;
    MD5.MD5Final(ctxMD5,Digest);
  end;
end;

function  Pos(Stream:Base; Term:Core.Strings.VarString; const iStart:Int64=0):Int64;
var
  iSize:Int64;
  iLcv:Int64;
  iLen:QWord;
  sBuffer:Core.Strings.VarString;
begin
  Result:=-1; iLcv:=iStart; iLen:=System.Length(Term);  iSize:=Stream.Size; SetLength(sBuffer,iLen);
  if iLen>0 then begin
    While (iLcv<iSize) and (Result=-1) do begin
      Stream.Position:=iLcv;
      Stream.Read(sBuffer[1],iLen);
      if Core.Strings.SameText(sBuffer,Term) then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;
end;

function Skipline(Stream:Base):Int64;
var
  buf:Byte;
  iRead:Int64;
begin
  Result:=0;
  repeat
    iRead:=Stream.Read(Buf,1);
  until(Stream.Position>=Stream.Size) or (Buf=10) or (iRead=0);
  Result:=Stream.Position;
end;

function Pos(Stream:Core.Streams.Types.Base; Term:Byte):Int64;
var
  iSize:Int64;
  iLcv:Int64;
  iByte:Byte;
begin
  Result:=-1; iLcv:=0; iSize:=Stream.Size;
  Stream.Position:=iLcv;
  While (iLcv<iSize) and (Result=-1) do begin
    Stream.Read(iByte,1);
    if iByte=Term then
      Result:=iLcv;
    Inc(iLcv);
  end;

end;

function Readline(Stream:Base; Var Position:Cardinal):Core.Strings.VarString;
{$i Core.Streams.Readline.inc}
end;

function Readline(Stream:Base; var Position:Int64):Core.Strings.VarString;
{$i Core.Streams.Readline.inc}
end;

end.


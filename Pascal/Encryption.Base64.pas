unit Encryption.Base64;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Core.Strings,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Streams,
  MD5;

function Encode(Value: Core.Strings.VarString): Core.Strings.VarString; overload;
function Encode(Value: TStream): Core.Strings.VarString; overload;
function Encode(Value: Core.Strings.VarString; Size:System.QWord): Core.Strings.VarString; overload;

function Encode(var Input: Core.Arrays.Types.Bytes): Core.Strings.VarString; overload;
function Encode(var Value: TMD5Digest): Core.Strings.VarString; overload;

function Encode(var Input:Core.Strings.VarString; var Output:Core.Arrays.Types.Bytes; Size: System.QWord): System.QWord; overload;
function Encode(var Input:Core.Arrays.Types.Bytes; var Output:Core.Strings.VarString; Size: System.QWord): System.QWord; overload;
function Encode(var Input:Core.Arrays.Types.Bytes; Refactor:TStream): Core.Strings.VarString; overload;
function Encode(var Input:Core.Arrays.Types.Bytes; Output,Refactor:TStream):System.QWord; overload;
function Encode(Input:TStream; Refactor:TMemoryStream): Core.Strings.VarString; overload;

function Encode(var Input:TMD5Digest; var Output:Core.Strings.VarString; Size: System.QWord): System.QWord; overload;
function Encode(var Input,Output:Core.Strings.VarString; Size: System.QWord): System.QWord; overload;
function Encode(Input,Output:TStream; Size: System.QWord): System.QWord; overload;

function Decode(Input: Core.Strings.VarString): Core.Strings.VarString; overload;
function Decode(Input:TStream):Core.Strings.VarString; overload;
function Decode(var Input:Core.Strings.VarString; var Output:RawByteString; Size:System.QWord):System.QWord; overload;
function Decode(Input:TStream; var Output:Core.Strings.VarString; Size:System.QWord):System.QWord; overload;
function Decode(var Input,Output:Core.Arrays.Types.Bytes):System.QWord; overload;
function Decode(var Input,Output:Core.Arrays.Types.Bytes; Size:System.QWord):System.QWord; overload;
function Decode(var Input:Core.Strings.VarString; Output:TStream):System.QWord; overload;
function Decode(Input,Output,Refactor:TStream):System.QWord; overload;
function Decode(Input:Core.Strings.VarString; var Output:Core.Arrays.Types.Bytes):System.QWord; overload;
function Decode(var Input:Core.Strings.VarString; var Output:Core.Arrays.Types.Bytes; Size:System.QWord):System.QWord; overload;
function Decode(Input:Core.Strings.VarString; var Output:TMD5Digest):System.QWord; overload;



implementation
uses StrUtils;

{$Q-}{$R-}

const
  B64: array[0..63] of byte= (65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,
    81,82,83,84,85,86,87,88,89,90,97,98,99,100,101,102,103,104,105,106,107,108,
    109,110,111,112,113,114,115,116,117,118,119,120,121,122,48,49,50,51,52,53,
    54,55,56,57,43,47);

function Encode(var Input,Output:Core.Strings.VarString; Size: QWord): QWord;
var
  i:cardinal;
  iptr, optr: QWord;
begin
  iptr:= 1; optr:= 1;
  Result:=0;
  for i:= 1 to (Size div 3) do begin
    Inc(Result,4);
    SetLength(Output,Result);

    Output[optr+0]:= Char(B64[Byte(Input[iptr]) shr 2]);
    Output[optr+1]:= Char(B64[((Byte(Input[iptr]) and 3) shl 4) + (Byte(Input[iptr+1]) shr 4)]);
    Output[optr+2]:= Char(B64[((Byte(Input[iptr+1]) and 15) shl 2) + (Byte(Input[iptr+2]) shr 6)]);
    Output[optr+3]:= Char(B64[Byte(Input[iptr+2]) and 63]);
    Inc(optr,4); Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= Char(B64[Byte(Input[iptr]) shr 2]);
         Output[optr+1]:= Char(B64[(Byte(Input[iptr]) and 3) shl 4]);
         Output[optr+2]:= '=';
         Output[optr+3]:= '=';
       end;
    2: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= Char(B64[Byte(Input[iptr]) shr 2]);
         Output[optr+1]:= Char(B64[((Byte(Input[iptr]) and 3) shl 4) + (Byte(Input[iptr+1]) shr 4)]);
         Output[optr+2]:= Char(B64[(Byte(Input[iptr+1]) and 15) shl 2]);
         Output[optr+3]:= '=';
       end;
  end;
end;

function Encode(Input:TStream; Refactor:TMemoryStream): Core.Strings.VarString;
begin
  Refactor.Size:=0;
  Try
    Input.Position:=0;
    Encode(Input,Refactor,Input.Size);
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Size:=0;
  end;
end;

function Encode(Input,Output:TStream; Size: QWord): QWord;
var
  i:Cardinal;
  iptr, optr: QWord;
  InBuf:Array[0..2] of Byte;
  OutBuf:Array[0..3] of Byte;
begin
  optr:=0; iptr:=0;
  Result:=0;
  for i:= 1 to (Size div 3) do begin
    Input.Read(InBuf[0],3);
    OutBuf[0]:= B64[InBuf[0] shr 2];
    OutBuf[1]:= B64[((InBuf[0] and 3) shl 4) + (InBuf[1] shr 4)];
    OutBuf[2]:= B64[((InBuf[1] and 15) shl 2) + (InBuf[2] shr 6)];
    OutBuf[3]:= B64[InBuf[2] and 63];
    Output.Write(OutBuf[0],4);
    Inc(Result,4);
  end;
  case (Size mod 3) of
    1: begin
         Input.Read(InBuf[0],1);
         OutBuf[0]:= B64[InBuf[0] shr 2];
         OutBuf[1]:= B64[(InBuf[0] and 3) shl 4];
         OutBuf[2]:= 61;
         OutBuf[3]:= 61;
         Output.Write(OutBuf[0],4);
         Inc(Result,4);
       end;
    2: begin
         Input.Read(InBuf[0],2);
         OutBuf[0]:= B64[InBuf[iptr] shr 2];
         OutBuf[1]:= B64[((InBuf[iptr] and 3) shl 4) + (InBuf[iptr+1] shr 4)];
         OutBuf[2]:= B64[(InBuf[iptr+1] and 15) shl 2];
         OutBuf[3]:= 61;
         Output.Write(OutBuf[0],4);
         Inc(Result,4);
       end;
  end;
end;

function Encode(var Input:Core.Arrays.Types.Bytes; var Output:Core.Strings.VarString; Size: QWord): QWord;
var
  i:cardinal;
  iptr, optr: QWord;
begin
  iptr:= 0; optr:= 1;
  Result:=0;
  for i:= 1 to (Size div 3) do begin
    Inc(Result,4);
    SetLength(Output,Result);
    Output[optr+0]:= Char(B64[Input[iptr] shr 2]);
    Output[optr+1]:= Char(B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)]);
    Output[optr+2]:= Char(B64[((Input[iptr+1] and 15) shl 2) + (Input[iptr+2] shr 6)]);
    Output[optr+3]:= Char(B64[Input[iptr+2] and 63]);
    Inc(optr,4); Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= Char(B64[Input[iptr] shr 2]);
         Output[optr+1]:= Char(B64[(Input[iptr] and 3) shl 4]);
         Output[optr+2]:= '=';
         Output[optr+3]:= '=';
       end;
    2: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= Char(B64[Input[iptr] shr 2]);
         Output[optr+1]:= Char(B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)]);
         Output[optr+2]:= Char(B64[(Input[iptr+1] and 15) shl 2]);
         Output[optr+3]:= '=';
       end;
  end;
end;

function Encode(var Input:Core.Arrays.Types.Bytes; Output,Refactor:TStream): QWord;
var
  i:cardinal;
  Size, iptr: QWord;
  OutBuf:Array[0..3] of Byte;
begin
  Size:=System.Length(Input);
  Result:=0;
  iptr:= 0; Refactor.Size:=0;
  for i:= 1 to (Size div 3) do begin

    OutBuf[0]:= B64[Input[iptr] shr 2];
    OutBuf[1]:= B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)];
    OutBuf[2]:= B64[((Input[iptr+1] and 15) shl 2) + (Input[iptr+2] shr 6)];
    OutBuf[3]:= B64[Input[iptr+2] and 63];

    Output.Write(OutBuf[0],4);
    Inc(Result,4);
    Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         OutBuf[0]:= B64[Input[iptr] shr 2];
         OutBuf[1]:= B64[(Input[iptr] and 3) shl 4];
         OutBuf[2]:= 61;
         OutBuf[3]:= 61;
         Inc(Result,4);
         Output.Write(OutBuf[0],4);
       end;
    2: begin
         OutBuf[0]:= B64[Input[iptr] shr 2];
         OutBuf[1]:= B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)];
         OutBuf[2]:= B64[(Input[iptr+1] and 15) shl 2];
         OutBuf[3]:= 61;
         Inc(Result,4);
         Output.Write(OutBuf[0],4);
       end;
  end;
  Refactor.Size:=0;
end;

function Encode(var Input:Core.Arrays.Types.Bytes; Refactor:TStream): Core.Strings.VarString;
var
  i:cardinal;
  Size, iptr: QWord;
  OutBuf:Array[0..3] of Byte;
begin
  SetLength(Result,0);
  Size:=System.Length(Input);
  iptr:= 0; Refactor.Size:=0;
  for i:= 1 to (Size div 3) do begin

    OutBuf[0]:= B64[Input[iptr] shr 2];
    OutBuf[1]:= B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)];
    OutBuf[2]:= B64[((Input[iptr+1] and 15) shl 2) + (Input[iptr+2] shr 6)];
    OutBuf[3]:= B64[Input[iptr+2] and 63];

    Refactor.Write(OutBuf[0],4);

    Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         OutBuf[0]:= B64[Input[iptr] shr 2];
         OutBuf[1]:= B64[(Input[iptr] and 3) shl 4];
         OutBuf[2]:= 61;
         OutBuf[3]:= 61;
         Refactor.Write(OutBuf[0],4);
       end;
    2: begin
         OutBuf[0]:= B64[Input[iptr] shr 2];
         OutBuf[1]:= B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)];
         OutBuf[2]:= B64[(Input[iptr+1] and 15) shl 2];
         OutBuf[3]:= 61;
         Refactor.Write(OutBuf[0],4);
       end;
  end;
  if (Refactor.Size>0) then begin
    SetLength(Result,Refactor.Size);
    Refactor.Position:=0;
    Refactor.Write(Result[1],Refactor.Size);
    Refactor.Size:=0;
  end;
end;

function Encode(var Input:TMD5Digest; var Output:Core.Strings.VarString; Size: QWord): QWord;
var
  i:cardinal;
  iptr, optr: QWord;
begin
  iptr:= 0; optr:= 1;
  Result:=0;
  for i:= 1 to (Size div 3) do begin
    Inc(Result,4);
    SetLength(Output,Result);
    Output[optr+0]:= Char(B64[Input[iptr] shr 2]);
    Output[optr+1]:= Char(B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)]);
    Output[optr+2]:= Char(B64[((Input[iptr+1] and 15) shl 2) + (Input[iptr+2] shr 6)]);
    Output[optr+3]:= Char(B64[Input[iptr+2] and 63]);
    Inc(optr,4); Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= Char(B64[Input[iptr] shr 2]);
         Output[optr+1]:= Char(B64[(Input[iptr] and 3) shl 4]);
         Output[optr+2]:= '=';
         Output[optr+3]:= '=';
       end;
    2: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= Char(B64[Input[iptr] shr 2]);
         Output[optr+1]:= Char(B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)]);
         Output[optr+2]:= Char(B64[(Input[iptr+1] and 15) shl 2]);
         Output[optr+3]:= '=';
       end;
  end;
end;

function Encode(var Input:Core.Strings.VarString; var Output:Core.Arrays.Types.Bytes; Size: QWord): QWord;
var
  i:cardinal;
  iptr, optr: QWord;
begin
  iptr:= 1; optr:= 0;
  Result:=0;
  for i:= 1 to (Size div 3) do begin
    Inc(Result,4);
    SetLength(Output,Result);
    Output[optr+0]:= B64[Byte(Input[iptr]) shr 2];
    Output[optr+1]:= B64[((Byte(Input[iptr]) and 3) shl 4) + (Byte(Input[iptr+1]) shr 4)];
    Output[optr+2]:= B64[((Byte(Input[iptr+1]) and 15) shl 2) + (Byte(Input[iptr+2]) shr 6)];
    Output[optr+3]:= B64[Byte(Input[iptr+2]) and 63];
    Inc(optr,4); Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= B64[Byte(Input[iptr]) shr 2];
         Output[optr+1]:= B64[(Byte(Input[iptr]) and 3) shl 4];
         Output[optr+2]:= byte('=');
         Output[optr+3]:= byte('=');
       end;
    2: begin
         Inc(Result,4);
         SetLength(Output,Result);
         Output[optr+0]:= B64[Byte(Input[iptr]) shr 2];
         Output[optr+1]:= B64[((Byte(Input[iptr]) and 3) shl 4) + (Byte(Input[iptr+1]) shr 4)];
         Output[optr+2]:= B64[(Byte(Input[iptr+1]) and 15) shl 2];
         Output[optr+3]:= byte('=');
       end;
  end;
end;

function Encode(Var Input:Core.Arrays.Types.Bytes): Core.Strings.VarString;
//function Encode(var Input,Output:Core.Strings.VarString; Size: QWord): QWord;
var
  i:cardinal;
  Size,iResult,iptr, optr: QWord;
begin
  iptr:= 0; optr:= 1;
  iResult:=0; Size:=Length(Input);
  for i:= 1 to (Size div 3) do begin
    Inc(iResult,4);
    SetLength(Result,iResult);

    Result[optr+0]:= Char(B64[Input[iptr] shr 2]);
    Result[optr+1]:= Char(B64[((Input[iptr] and 3) shl 4) + (Input[iptr+1] shr 4)]);
    Result[optr+2]:= Char(B64[((Input[iptr+1] and 15) shl 2) + (Input[iptr+2] shr 6)]);
    Result[optr+3]:= Char(B64[Input[iptr+2] and 63]);
    Inc(optr,4); Inc(iptr,3);
  end;
  case (Size mod 3) of
    1: begin
         Inc(iResult,4);
         SetLength(Result,iResult);
         Result[optr+0]:= Char(B64[Input[iptr] shr 2]);
         Result[optr+1]:= Char(B64[(Input[iptr] and 3) shl 4]);
         Result[optr+2]:= '=';
         Result[optr+3]:= '=';
       end;
    2: begin
         Inc(iResult,4);
         SetLength(Result,iResult);
         Result[optr+0]:= Char(B64[Input[iptr] shr 2]);
         Result[optr+1]:= Char(B64[((Input[iptr] and 3) shl 4) + (Byte(Input[iptr+1]) shr 4)]);
         Result[optr+2]:= Char(B64[(Input[iptr+1] and 15) shl 2]);
         Result[optr+3]:= '=';
       end;
  end;
end;

function Encode(Var Value:TMD5Digest): Core.Strings.VarString;
var
  iInLen:QWord;
begin
  SetLength(Result,0);
  iInLen:=System.Length(Value);
  if (iInLen>0) then
    Encode(Value,Result,iInLen);
end;

function Encode(Value:Core.Strings.VarString): Core.Strings.VarString;
var
  iInLen:QWord;
begin
  SetLength(Result,0);
  iInLen:=System.Length(Value);
  if (iInLen>0) then
    Encode(Value,Result,iInLen);
end;

function Encode(Value:TStream): Core.Strings.VarString;
//function Encode(var Input,Output:Core.Strings.VarString; Size: QWord): QWord;
var
  i:cardinal;
  iResult,optr: QWord;
  Input:Array[0..2] of byte;
begin
  optr:= 1;
  iResult:=0;
  Value.Position:=0;
  for i:= 1 to (Value.Size div 3) do begin
    Inc(iResult,4);
    SetLength(Result,iResult);
    Value.Read(Input[0],3);
    Result[optr+0]:= Char(B64[Input[0] shr 2]);
    Result[optr+1]:= Char(B64[((Input[0] and 3) shl 4) + (Input[1] shr 4)]);
    Result[optr+2]:= Char(B64[((Input[1] and 15) shl 2) + (Input[2] shr 6)]);
    Result[optr+3]:= Char(B64[Input[2] and 63]);
    Inc(optr,4);
  end;
  case (Value.Size mod 3) of
    1: begin
         Inc(iResult,4);
         SetLength(Result,iResult);
         Value.Read(Input[0],1);
         Result[optr+0]:= Char(B64[Input[0] shr 2]);
         Result[optr+1]:= Char(B64[(Input[0] and 3) shl 4]);
         Result[optr+2]:= '=';
         Result[optr+3]:= '=';
       end;
    2: begin
         Inc(iResult,4);
         SetLength(Result,iResult);
         Value.Read(Input[0],2);
         Result[optr+0]:= Char(B64[Input[0] shr 2]);
         Result[optr+1]:= Char(B64[((Input[0] and 3) shl 4) + (Byte(Input[1]) shr 4)]);
         Result[optr+2]:= Char(B64[(Input[1] and 15) shl 2]);
         Result[optr+3]:= '=';
       end;
  end;
end;

function Encode(Value:Core.Strings.VarString; Size:QWord): Core.Strings.VarString;
var
  iInLen:QWord;
begin
  SetLength(Result,0);
  iInLen:=Size;
  if (iInLen>0) then
    Encode(Value,Result,iInLen);
end;

function Decode(Input:Core.Strings.VarString):Core.Strings.VarString;
var
  iOutLen:QWord;
  iInLen:QWord;
  Data:RawByteString;
begin
  System.SetLength(Result,0);
  iInLen:=System.Length(Input);
  iOutLen:=(iInLen div 4) * 3;
  If iInLen>0 then begin
    SetLength(Data,iOutLen);
    Decode(Input,Data,iInLen);
    Result:=Data;
  end;
end;

function Decode(Input:TStream):Core.Strings.VarString;
var
  iOutLen:QWord;
  iInLen:QWord;
begin
  System.SetLength(Result,0);
  iInLen:=Input.Size;
  iOutLen:=(iInLen div 4) * 3;
  If iInLen>0 then begin
    SetLength(Result,iOutLen);
    Decode(Input,Result,iInLen);
  end;
end;

function Decode(var Input,Output:Core.Arrays.Types.Bytes):QWord;
var
  iOutLen:QWord;
  iInLen:QWord;
begin
  System.SetLength(Output,0);
  iInLen:=System.Length(Input);
  iOutLen:=(iInLen div 4) * 3;
  If iInLen>0 then begin
    System.SetLength(Output,iOutLen);
    Result:=Decode(Input,Output,iInLen);
  end;
end;

function Decode(var Input:Core.Strings.VarString; Output:TStream):QWord;
begin
  Output.Size:=0;
  Result:=Core.Streams.Write(Decode(Input),Output);
end;

function Decode(Input:Core.Strings.VarString; var Output:Core.Arrays.Types.Bytes):QWord;
var
  iOutLen:QWord;
  iInLen:QWord;
begin
  System.SetLength(Output,0);
  iInLen:=System.Length(Input);
  iOutLen:=(iInLen div 4) * 3;
  If iInLen>0 then begin
    SetLength(Output,iOutLen);
    Result:=Decode(Input,Output,iInLen);
  end;
end;

function Decode(var Input,Output:Core.Arrays.Types.Bytes; Size: QWord): QWord;
var
  i, j : cardinal;
  iptr, optr: QWord;
  Temp: array[0..3] of byte;
begin
  iptr:= 0; optr:= 0;
  Result:= 0;
  for i:= 1 to (Size div 4) do begin
    for j:= 0 to 3 do begin
      case Input[iptr] of
        65..90 : Temp[j]:= Input[iptr] - Ord('A');
        97..122: Temp[j]:= Input[iptr] - Ord('a') + 26;
        48..57 : Temp[j]:= Input[iptr] - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;
      Inc(iptr);
    end;
    Output[optr]:= (Temp[0] shl 2) or (Temp[1] shr 4);
    if (Temp[2]<> $FF) and (Temp[3]= $FF) then begin
      Output[optr+1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Inc(optr);
      Inc(Result);
    end else if (Temp[2]<> $FF) then begin
      Output[optr+1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Output[optr+2]:= (Temp[2] shl 6) or  Temp[3];
      Inc(optr,2);
      Inc(Result,2);
    end;
    Inc(optr);
    Inc(Result);
  end;
  System.SetLength(Output,Result);
end;

function Decode(var Input:Core.Strings.VarString; Var Output:Core.Arrays.Types.Bytes; Size: QWord): QWord;
var
  i, j:cardinal;
  iptr, optr: QWord;
  Temp: array[0..3] of byte;
begin
  iptr:= 1; optr:= 0; Result:= 0;
  for i:= 1 to (Size div 4) do begin
    for j:= 0 to 3 do begin
      case Byte(Input[iptr]) of
        65..90 : Temp[j]:= Byte(Input[iptr]) - Ord('A');
        97..122: Temp[j]:= Byte(Input[iptr]) - Ord('a') + 26;
        48..57 : Temp[j]:= Byte(Input[iptr]) - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;
      Inc(iptr);
    end;
    Output[optr]:= (Temp[0] shl 2) or (Temp[1] shr 4);
    if (Temp[2]<> $FF) and (Temp[3]= $FF) then begin
      Output[optr+1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Inc(optr);
      Inc(Result);
    end else if (Temp[2]<> $FF) then begin
      Output[optr+1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Output[optr+2]:= (Temp[2] shl 6) or  Temp[3];
      Inc(optr,2);
      Inc(Result,2);
    end;
    Inc(Result);
    Inc(optr);
  end;
  System.SetLength(Output,Result);

end;

function Decode(Input:Core.Strings.VarString; var Output:TMD5Digest):QWord;
var
  i, j:cardinal;
  Size,iptr, optr: QWord;
  Temp: array[0..3] of byte;
begin
  iptr:= 1; optr:= 0; Result:=0;  Size:=System.Length(Input);
  for i:= 1 to (Size div 4) do begin
    for j:= 0 to 3 do begin
      case Byte(Input[iptr]) of
        65..90 : Temp[j]:= Byte(Input[iptr]) - Ord('A');
        97..122: Temp[j]:= Byte(Input[iptr]) - Ord('a') + 26;
        48..57 : Temp[j]:= Byte(Input[iptr]) - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;
      Inc(iptr);
    end;
    Output[optr]:= (Temp[0] shl 2) or (Temp[1] shr 4);
    if (Temp[2]<> $FF) and (Temp[3]= $FF) then begin
      Output[optr+1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Inc(optr);
      Inc(Result);
    end else if (Temp[2]<> $FF) then begin
      Output[optr+1]:= (Temp[1] shl 4) or (Temp[2] shr 2);
      Output[optr+2]:= (Temp[2] shl 6) or  Temp[3];
      Inc(optr,2);
      Inc(Result,2);
    end;
    Inc(Result);
    Inc(optr);
  end;
end;

function Decode(var Input:Core.Strings.VarString; var Output:RawByteString; Size: QWord): QWord;
var
  i, j       : cardinal;
  iptr, optr : QWord;
  Temp       : array[0..3] of byte;
begin
  iptr:= 1; optr:= 1; Result:= 0;
  for i:= 1 to (Size div 4) do begin
    for j:= 0 to 3 do begin
      case Byte(Input[iptr]) of
        65..90 : Temp[j]:= Byte(Input[iptr]) - Ord('A');
        97..122: Temp[j]:= Byte(Input[iptr]) - Ord('a') + 26;
        48..57 : Temp[j]:= Byte(Input[iptr]) - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;
      Inc(iptr);
    end;
    Output[optr]:= Char((Temp[0] shl 2) or (Temp[1] shr 4));
    if (Temp[2]<> $FF) and (Temp[3]= $FF) then begin
      Output[optr+1]:= Char((Temp[1] shl 4) or (Temp[2] shr 2));
      Inc(optr);
      Inc(Result);
    end else if (Temp[2]<> $FF) then begin
      Output[optr+1]:= Char((Temp[1] shl 4) or (Temp[2] shr 2));
      Output[optr+2]:= Char((Temp[2] shl 6) or  Temp[3]);
      Inc(optr,2);
      Inc(Result,2);
    end;
    Inc(optr);
    Inc(Result);
  end;
  SetLength(Output,Result);
  Output:=System.UTF8Encode(Output);
  Result:=Length(Output)
end;

function Decode(Input:TStream; var Output:Core.Strings.VarString; Size: QWord): QWord;
var
  i, j:cardinal;
  iptr, optr: QWord;
  Temp: array[0..3] of byte;
  bInput:Byte;
begin
  iptr:= 1; optr:= 1; Result:= 0;
  for i:= 1 to (Size div 4) do begin
    for j:= 0 to 3 do begin
      Input.Read(bInput,1);
      case bInput of
        65..90 : Temp[j]:= bInput - Ord('A');
        97..122: Temp[j]:= bInput - Ord('a') + 26;
        48..57 : Temp[j]:= bInput - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;
      Inc(iptr);
    end;
    Output[optr]:= Char((Temp[0] shl 2) or (Temp[1] shr 4));
    if (Temp[2]<> $FF) and (Temp[3]= $FF) then begin
      Output[optr+1]:= Char((Temp[1] shl 4) or (Temp[2] shr 2));
      Inc(optr);
      Inc(Result);
    end else if (Temp[2]<> $FF) then begin
      Output[optr+1]:= Char((Temp[1] shl 4) or (Temp[2] shr 2));
      Output[optr+2]:= Char((Temp[2] shl 6) or  Temp[3]);
      Inc(optr,2);
      Inc(Result,2);
    end;
    Inc(optr);
    Inc(Result);
  end;
  System.SetLength(Output,Result);
end;

procedure Cleanup(Input,Refactor:TStream);
var
  Val:Byte;
begin
  Input.Position:=0; Refactor.Size:=0;
  While (Input.Position<Input.Size) do begin
    Input.Read(Val,1);
    if not (Val in [10,13]) then
      Refactor.Write(Val,1);
  end;
  Core.Streams.Copy(Refactor,Input);
  Refactor.Size:=0;
end;

function Decode(Input,Output,Refactor:TStream): QWord;
var
  i, j:cardinal;
  Size,iptr, optr: QWord;
  Temp: array[0..3] of byte;
  bInput:Byte;
  bOutput:Byte;
begin
  iptr:= 0; optr:= 0; Result:= 0;
  Cleanup(Input,Refactor);
  Output.Size:=0;
  Input.Position:=0;
  Size:=Input.Size;
  While (iPtr<Size) do begin
    for j:= 0 to 3 do begin
      Input.Read(bInput,1);
      case bInput of
        65..90 : Temp[j]:= bInput - Ord('A');
        97..122: Temp[j]:= bInput - Ord('a') + 26;
        48..57 : Temp[j]:= bInput - Ord('0') + 52;
        43     : Temp[j]:= 62;
        47     : Temp[j]:= 63;
        61     : Temp[j]:= $FF;
      end;
      Inc(iptr);
    end;
    Output.Position:=optr;
    bOutput:=(Temp[0] shl 2) or (Temp[1] shr 4);
    Output.WriteBuffer( bOutput,1);
    if (Temp[2]<> $FF) and (Temp[3]= $FF) then begin
      bOutput:=(Temp[1] shl 4) or (Temp[2] shr 2);
      Output.Write(bOutput,1);
      Inc(optr);
      Inc(Result);
    end else if (Temp[2]<> $FF) then begin
      bOutput:=(Temp[1] shl 4) or (Temp[2] shr 2);
      Output.Write(bOutput,1);
      bOutput:=(Temp[2] shl 6) or  Temp[3];
      Output.Write(bOutput,1);
      Inc(optr,2);
      Inc(Result,2);
    end;
    Inc(optr);
    Inc(Result);
  end;
end;

end.


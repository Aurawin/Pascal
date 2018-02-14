unit Core.Strings;

interface

uses
  Classes, SysUtils, MD5;

Type
  TUID=Array[0..19] of Char;
  TCharSet=set of char;
  VarString=UTF8String;
  Char=System.Char;

  PVarString=^VarString;
  Raw=RawByteString;
  Small=ShortString;
  Wide=WideString;
  Defaults=class
  const
    True:VarString='true';
    False:VarString='false';
    Yes:VarString='yes';
    No:VarString='no';
  end;

  KeyString=record
    Key       : Core.Strings.VarString;
    Value     : Core.Strings.VarString;
    Streams   : System.Boolean;
    Data      : System.Pointer;
  end;
  PKeyString=^KeyString;

  FileName=TFileName;

  TMaxLength={$ifdef CPU_64} System.Int64 {$else} LongInt {$endif};


  function  toString(Item:System.QWord):VarString; overload;
  function  toString(Item:System.Int64):VarString; overload;
  function  toString(Item:System.Extended):VarString; overload;

  function isAllDigits(var Item:VarString):boolean;

  procedure Init(Var Item:VarString); overload;
  procedure Init(Var ITem:Small); overload;

  procedure Done(Var Item:VarString); overload;
  procedure Done(Var Item:Small); overload;

  procedure Empty(Var Item:VarString); overload;
  procedure Empty(Var Item:Small); overload;

  function  fromFile(sFileName:VarString):VarString;
  procedure toFile(Var Item:VarString; Const FileName:VarString); overload;
  function  toHexString(Item:System.Pointer; Length:LongInt):VarString; overload;
  function  toString(Var Item:TMD5Digest):VarString; overload;


  function  toInt(var Item:VarString; Default:LongInt):LongInt; overload;
  function  toInt(var Item:VarString; Default:System.Int64):System.Int64;overload;
  function  toInt(var Item:VarString; Default:System.QWord):System.QWord; overload;
  function  toInt(Var Item:VarString; Default:System.Word):System.Word; overload;
  function  toInt(Var item:VarString; Default:System.Byte):System.Byte; overload;

  function  toFloat(var Item:VarString; Default:System.Double):System.Double; overload;

  procedure CheckSum(var Source:VarString; var Digest:TMD5Digest);

  function  Hash(Data:VarString; var Digest:TMD5Digest):VarString; overload;
  function  Hash(var Data1:TMD5Digest; Data2:VarString; var Digest:TMD5Digest):VarString; overload;
  function  Print(var Digest: TMD5Digest): VarString;

  procedure Append(Var Destination:VarString; Const Value:Int64; Const Delimiter:VarString); overload;
  procedure Append(Var Destination:VarString; Const Value:Boolean; Const Delimiter:VarString);overload;

  function  PosEx(Delim:VarString; var Source:VarString; Offset,Length:System.Int64):System.Int64;
  function  Pos(SubStr:VarString; var Source:VarString):System.Int64;

  procedure Trim(var Data:VarString); overload;
  procedure TrimRight(Var Data:RawByteString); overload;
  procedure TrimRight(var Data:VarString); overload;

  function  EndsWith(var Data:VarString; Criteria:VarString):boolean;
  function  StartsWith(var Data:VarString; Criteria:VarString):boolean;

  function  ClipEnd(var Data:VarString; Criteria:VarString):boolean;
  function  Extract(var Source:VarString; &Until:Char):VarString; overload;
  function  Extract(Var Source:VarString; iCount:LongInt):VarString; overload;
  function  Extract(Var Source:VarString; iStart,iCount:LongInt):VarString; overload;
  function  Extract(var Source:VarString; const chStart,chEnd:char):VarString; overload;

  function  Pos(Var sData:VarString; Term:TCharSet):System.LongInt; overload;


  function  SameText(var One,Two:VarString):System.Boolean; overload;
  function  SameText(var One:VarString; var Two:Small):System.Boolean; overload;
  function  SameText(One:WideString; var Two:VarString):System.Boolean; overload;

  function  Search(sData,sTerm:VarString; const Offset:LongInt=1):System.Int64;

  procedure Deduplicate(var sData:VarString; Term:VarString);
  procedure Replace(Var Data:VarString; Ch:Char; Text:VarString); overload;
  procedure Replace(Var Data:VarString; Term:VarString; Text:VarString); overload;

  function  LineCount(var Data:VarString; iStart,iStop:LongInt):LongInt;
  function  WordCount(var Data:VarString; Const Item:VarString):LongInt;

const
  csNumeric            = ['0'..'9'];
  csHexDigits:TCharSet = csNumeric + ['A'..'F'] + ['a'..'f'];
  YES_NO:Array[System.Boolean] of VarString=('No','Yes');
  Digits:VarString='0123456789';


implementation
uses StrUtils;


function  Extract(Var Source:VarString; iCount:LongInt):VarString;
begin
  Result:=System.Copy(Source,1,iCount);
end;

function  Extract(Var Source:VarString; iStart,iCount:LongInt):VarString;
begin
  SetLength(Result,iCount);
  if iCount>0 then
    System.Move(Source[iStart],Result[1],iCount);
end;

function  Extract(var Source:VarString; const chStart,chEnd:Char):VarString;
var
  iLoc:LongInt;
  iEnd:LongInt;
  iLength:LongInt;
  iBias:LongInt;
  iLcv:LongInt;
begin
  SetLength(Result,0);
  iLength:=System.Length(Source);
  iLoc:=System.Pos(chStart,Source);
  iBias:=0;
  iEnd:=0;
  iLcv:=iLoc+1;
  while (iLcv<=iLength) do begin
    If (Source[iLcv]=chStart) then
      Inc(iBias)
    else if (Source[iLcv]=chEnd) then begin
      Dec(iBias);
      if (iBias<0) then begin
        iEnd:=iLcv;
        iLcv:=iLength;
      end;
    end;
    Inc(iLcv);
  end;
  if iEnd=0 then
    iEnd:=iLength;
  Result:=SysUtils.Trim(
    System.Copy(
      Source,
      iLoc+1,
      (iEnd-(iLoc+1) )
    )
  );
end;

function  toInt(var Item:VarString; Default:System.Word):System.Word;
begin
  Result:=SysUtils.StrToIntDef(Item,Default);
end;

function  toInt(var Item:VarString; Default:System.Byte):System.Byte;
begin
  Result:=SysUtils.StrToIntDef(Item,Default);
end;

function  toFloat(var Item:VarString; Default:System.Double):System.Double;
begin
  Result:=SysUtils.StrToFloatDef(Item,Default);
end;

function  toInt(var Item:VarString; Default:LongInt):LongInt;
begin
  Result:=SysUtils.StrToIntDef(Item,Default);
end;

function  toInt(var Item:VarString; Default:System.Int64):System.Int64;
begin
  Result:=SysUtils.StrToInt64Def(Item,Default);
end;

function  toInt(var Item:VarString; Default:System.QWord):System.QWord;
begin
  Result:=SysUtils.StrToQWordDef(Item,Default);
end;

function  SameText(var One,Two:VarString):System.Boolean;
  {$i Core.Strings.SameText.Declarations.inc}
begin
  {$i Core.Strings.SameText.inc}
end;

function  SameText(var One:VarString; var Two:Small):System.Boolean;
  {$i Core.Strings.SameText.Declarations.inc}
begin
  {$i Core.Strings.SameText.inc}
end;

function  SameText(One:WideString; var Two:VarString):System.Boolean;
  {$i Core.Strings.SameText.Declarations.inc}
begin
  {$i Core.Strings.SameText.inc}
end;

function  Pos(Var sData:VarString; Term:TCharSet):System.LongInt;
var
  iLen:LongInt;
  iLcv:LongInt;
begin
  Result:=-1; iLcv:=1; iLen:=System.Length(sData);
  While (iLcv<iLen) and (Result=-1) do begin
    if sData[iLcv] in Term then
      Result:=iLcv;
    iLcv+=1;
  end;
end;

function  Search(sData,sTerm:VarString; const Offset:LongInt=1):System.Int64;
var
  iLen:Int64;
  iLcv:LongInt;
begin
  sTerm:=Lowercase(sTerm);
  sData:=Lowercase(sData);
  iLen:=Length(sData);
  Result:=PosEx(sTerm,sData,Offset,iLen);
end;

procedure Deduplicate(var sData:VarString; Term:VarString);
var
  sDup:VarString;
  iTermLen:LongInt;
  idx:LongInt;
begin
  sDup:=Concat(Term,Term);
  iTermLen:=System.Length(Term);
  repeat
    idx:=System.Pos(sDup,sData);
    if idx>0 then
      System.Delete(sData,idx,iTermLen);
  until (idx=0);
end;

procedure Replace(Var Data:VarString; Ch:Char; Text:VarString);
var
  iLcv:LongInt;
  iLen:LongInt;
  iTextLen:LongInt;
  iTextWrite:LongInt;
  iTextWriteOffset:LongInt;
begin
  iLen:=Length(Data); iTextLen:=System.Length(Text);
  iTextWrite:=iTextLen;
  iTextWriteOffset:=iTextLen-1;
  iLcv:=1;
  While iLcv<=iLen do begin
    If Data[iLcv]=Ch then begin
      System.Delete(Data,iLcv,1);
      System.Insert(Data,Text,iLcv);
      Inc(iLcv,iTextWrite);
      Inc(iLen,iTextWriteOffset);
    end else
      Inc(iLcv);
  end;
end;

procedure Replace(Var Data:VarString; Term:VarString; Text:VarString);
var
  iLcv:System.Int64;
  iLen:System.Int64;
  iTextLen:LongInt;
  iTermLen:LongInt;
  iTextWrite:LongInt;
  iTextWriteOffset:LongInt;
begin
  iLen:=Length(Data);
  iTextLen:=System.Length(Text);
  iTextWrite:=iTextLen;
  iTextWriteOffset:=iTextLen-1;
  iTermLen:=System.Length(Term);
  iLcv:=Pos(Term,Data);
  While iLcv>0 do begin
    System.Delete(Data,iLcv,iTermLen);
    System.Insert(Text,Data,iLcv);
    iLen:=Length(Data);
    iLcv:=Core.Strings.PosEx(Term,Data,iLcv+1,iLen);
  end;
end;


function  LineCount(var Data:VarString; iStart,iStop:LongInt):LongInt;
var
  iLcv,iLen:LongInt;
begin
  iLcv:=iStart; iLen:=System.Length(Data);
  Result:=0;
  While (iLcv<=iLen) and (iLcv<=iStop) do begin
    if (Data[iLcv]=#10) then
      Inc(Result);
    inc(iLcv);
  end;
end;

function  WordCount(var Data:VarString; Const Item:VarString):LongInt;
var
  iLen:LongInt;
  iLoc:LongInt;
begin
  Result:=0; iLoc:=1; iLen:=Length(Item);
  repeat
    iLoc:=StrUtils.PosEx(Item,Data,iLoc);
    if (iLoc>0) then begin
      Inc(Result);
      Inc(iLoc,iLen);
    end;
  until (iLoc=0);
end;

function  Pos(SubStr:VarString; var Source:VarString):System.Int64;
var
  iSubStrLen:LongInt;
  iSubStrBias:LongInt;
  iLength:System.Int64;
  iLcv:System.Int64;
begin
  Result:=0;
  iLength:=System.Length(Source);
  iSubStrLen:=System.Length(SubStr);
  iSubStrBias:=iSubStrLen-1;
  if iSubStrLen>0 then begin
    iLcv:=1;
    While ((iLcv+iSubStrBias)<=iLength) and (Result=0) do begin
      If System.CompareByte(Source[iLcv],SubStr[1],iSubStrLen)=0 then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;
end;

function  PosEx(Delim:VarString; var Source:VarString; Offset,Length:System.Int64):System.Int64;
var
  iDelimLen:LongInt;
  iDelimBias:LongInt;
  iLcv:System.Int64;
begin
  Result:=0;
  iDelimLen:=System.Length(Delim);
  iDelimBias:=iDelimLen-1;
  if iDelimLen>0 then begin
    iLcv:=Offset;
    While (iLcv+iDelimBias<=Length) and (Result=0) do begin
      If System.CompareByte(Source[iLcv],Delim[1],iDelimLen)=0 then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;
end;

procedure  Trim(Var Data:VarString);
var
  iLen:LongInt;
begin
  iLen:=Length(Data);

  Repeat
    If iLen>0 then begin
      If Data[1]=#32 then begin
        System.Delete(Data,1,1);
        Dec(iLen);
      end;
    end;
  Until (iLen=0) or (Data[1]<>#32);

  Repeat
    If iLen>0 then begin
      If Data[iLen]=#32 then begin
        System.Delete(Data,iLen,1);
        Dec(iLen);
      end;
    end;
  Until (iLen=0) or (Data[iLen]<>#32);
end;

procedure  TrimRight(Var Data:VarString);
{$i Core.Strings.TrimRight.Decs.inc}
begin
  {$i Core.Strings.TrimRight.Code.inc}
end;

procedure  TrimRight(Var Data:RawByteString);
{$i Core.Strings.TrimRight.Decs.inc}
begin
  {$i Core.Strings.TrimRight.Code.inc}
end;

function  EndsWith(var Data:VarString; Criteria:VarString):System.boolean;
var
  iLen    : LongInt;
  iCriLen : LongInt;
  sEnd    : VarString;
begin
  iLen:=System.Length(Data);
  iCriLen:=System.Length(Criteria);
  Result:=(iLen>=iCriLen);
  if Result=true then begin
    sEnd:=System.Copy(Data,(iLen-iCriLen)+1,iCriLen);
    Result:=SameText(sEnd,Criteria);
  end;
end;

function  StartsWith(var Data:VarString; Criteria:VarString):System.boolean;
var
  iLen    : LongInt;
  iCriLen : LongInt;
  sEnd    : VarString;
begin
  iLen:=System.Length(Data);
  iCriLen:=System.Length(Criteria);
  Result:=(iLen>=iCriLen);
  if Result=true then begin
    sEnd:=System.Copy(Data,1,iCriLen);
    Result:=SameText(sEnd,Criteria);
  end;
end;

function  ClipEnd(var Data:VarString; Criteria:VarString):System.boolean;
var
  iLen    : LongInt;
  iCriLen : LongInt;
  sEnd    : VarString;
begin
  iLen:=System.Length(Data);
  iCriLen:=System.Length(Criteria);
  Result:=(iLen>=iCriLen);
  if Result=true then begin
    sEnd:=System.Copy(Data,(iLen-iCriLen)+1,iCriLen);
    Result:=SameText(sEnd,Criteria);
    if Result=true then begin
      iLen-=iCriLen;
      System.SetLength(Data,iLen);
    end;
  end;
end;


function  toString(Item:System.Int64):VarString;
begin
   Result:=FloatToStrF(Item,ffNumber,18,0);
end;

function  toString(Item:System.QWord):VarString;
begin
   Result:=FloatToStrF(Item/1,ffNumber,18,0);
end;

function  toString(Item:System.Extended):VarString;
begin
  Result:=FloatToStrF(Item,ffNumber,18,0);
end;

procedure Init(Var Item:VarString);
begin
  SetLength(Item,0);
end;

procedure Init(Var Item:Small);
begin
  SetLength(Item,0);
end;

procedure Done(Var Item:VarString);
begin
  Finalize(Item);
end;

procedure Done(Var Item:Small);
begin
  Finalize(Item);
end;

function isAllDigits(var Item:VarString):System.boolean;
var
  iLen,iLcv:LongInt;
begin
  iLen:=System.Length(Item);
  Result:=(iLen>0); iLcv:=1;
  While (iLcv<=iLen) and (Result) do begin
    Result:=((Item[iLcv] in csNumeric));
    inc(iLcv);
  end;
end;

procedure Empty(Var Item:VarString);
begin
  SetLength(Item,0);
end;

procedure Empty(Var Item:Small);
begin
  SetLength(Item,0);
end;

procedure Append(Var Destination:VarString; Const Value:System.Int64; Const Delimiter:VarString);
begin
  Destination:=Concat(Destination,IntToStr(Value),Delimiter);
end;

procedure Append(Var Destination:VarString; Const Value:System.Boolean; Const Delimiter:VarString);
const
  CH_BOOL:Array[Boolean] of Char=('0','1');
begin
  Destination:=Concat(Destination,CH_BOOL[Value],Delimiter);
end;

function  toHexString(Item:System.Pointer; Length:LongInt):VarString;
Var
  iLcv:LongInt;
begin
  iLcv:=1;
  SetLength(Result,0);
  While (iLcv<=Length) do begin
    Result:=Concat(Result,Format('%.2x',[PByte(Item)^]));
    Inc(PByte(Item));
    Inc(iLcv);
  end;
end;

function  toString(Var Item:TMD5Digest):VarString;
begin
  SetLength(Result,16);
  System.Move(Item[0],Result[1],16);
end;


procedure CheckSum(var Source:VarString; var Digest:TMD5Digest);
var
  ctxMD5    : TMD5Context;
  iLen      : TMaxLength;
  iLcv      : TMaxLength;
begin
  FillChar(Digest,SizeOf(Digest),#0);
  iLen:=Length(Source);
  if (iLen>0) then begin
    MD5.MD5Init(ctxMD5);
    for iLcv:=1 to iLen do
      MD5.MD5Update(ctxMD5,Source[iLcv],1);
    MD5.MD5Final(ctxMD5,Digest);
  end;
end;


function  Hash(Data:VarString; var Digest:TMD5Digest):VarString;
var
 CTX:TMD5Context;
begin
  MD5.MD5Init(CTX);
  MD5.MD5Update(CTX,Data[1],Length(Data));
  MD5.MD5Final(CTX,Digest);
  Result:=Print(Digest);
end;

function  Hash(var Data1:TMD5Digest; Data2:VarString; var Digest:TMD5Digest):VarString;
var
 CTX:TMD5Context;
begin
  MD5.MD5Init(CTX);
  MD5.MD5Update(CTX,Data1[0],SizeOf(Data1));
  MD5.MD5Update(CTX,Data2[1],System.Length(Data2));
  MD5.MD5Final(CTX,Digest);
  Result:=Print(Digest);
end;

function Print(var Digest: TMD5Digest): VarString;
var
  I: Byte;
begin
  Result := '';
  for I := 0 to 15 do
    Result := Result + HexStr(Digest[i],2);
  Result := LowerCase(Result);
end;

function  fromFile(sFileName:VarString):VarString;
var
  FS:TFileStream;
begin
  FS:=TFileStream.Create(sFileName,fmOpenRead or fmShareDenyNone);
  Try
    SetLength(Result,FS.Size);
    if FS.Size>0 then
      FS.Read(Result[1],FS.Size);
  Finally
    FreeAndNil(FS);
  End;
End;

function  Extract(var Source:VarString; &Until:Char):VarString;
var
  iLcv,iCt:LongInt;
begin
  SetLength(Result,0);
  iLcv:=1;
  iCt:=Length(Source);
  While (iLcv<=iCt) and (Source[iLcv]<>&Until) do
    Inc(iLcv);

  if ( (iLcv<=iCt) and (Source[iLcv]=&Until)) then
    Dec(iLcv);
  if (iLcv>0) then
    Result:=System.Copy(Source,1,iLcv);
end;

procedure toFile(Var Item:VarString; Const FileName:VarString);
var
  FS:TFileStream;
  iLen:LongInt;
begin
  FS:=TFileStream.Create(FileName,fmCreate);
  Try
    iLen:=Length(Item);
    if iLen>0 then
      FS.Write(Item[1],iLen);
  Finally
    FreeAndNil(FS);
  End;
End;

end.


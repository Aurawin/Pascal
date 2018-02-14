{
 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

Unit Core.Utils.Mail;

interface

uses Classes, SysUtils,
   Core.Strings,
   Core.Arrays.Types,
   Core.Arrays.VarString;

type
  EUUInvalidCharacter = class(Exception)
    constructor Create;
  end;

const
   MimeMaxChars              = 57;
type
   TISO = array[0..255] of Pchar;
   TEncMethod = (emNone,emBase64,em7Bit,em8bit,emQtPrn,emBinary);
   TEncoding = (etUU,etMIME);

   Function   ConvertMonth(Const Mon:Core.Strings.VarString):Core.Strings.VarString;
   function   GetDate(DateTimeVal:Core.Strings.VarString): Core.Strings.VarString;
   function   ExtractName(S:Core.Strings.VarString):Core.Strings.VarString;
   function   ExchangeDomain(sAddress,sNewDomain:Core.Strings.VarString):Core.Strings.VarString;
   function   ExtractAddress(S:Core.Strings.VarString):Core.Strings.VarString;
   function   ExtractUserName(S:Core.Strings.Small):Core.Strings.Small;
   function   ExtractRoot(S:Core.Strings.Small):Core.Strings.Small;
   function   ExtractDomain(S:Core.Strings.Small):Core.Strings.Small;
   Function   ExtractListName(var Data:Core.Strings.VarString):Core.Strings.VarString;

   function   StickitsTimeOld(STime:TDateTime):Core.Strings.VarString;
   function   StickitsTimeNew(SDate,STime:TDateTime):Core.Strings.VarString;

   function   JoinLines(const Addr,Name : Core.Strings.VarString) : Core.Strings.VarString;

   function   MakeBoundary : Core.Strings.VarString;
   function   MakeUniqueID : Core.Strings.Small;

   function   GetFirstPart(const s : Core.Strings.Small) : Core.Strings.VarString;
   function   TrimStr(s : Core.Strings.VarString) : Core.Strings.VarString;
   function   GetHeaderValue(Hdr : TStrings; ID : Core.Strings.VarString) : Core.Strings.VarString; overload;
   function   GetHeaderValue(Var List:Core.Arrays.Types.VarString; ID:Core.Strings.VarString): Core.Strings.VarString; overload;
   procedure  ParseHeader(var Input,Name,Value:Core.Strings.VarString);
   function   GetParameter(Param,Data : Core.Strings.VarString) : Core.Strings.VarString;

   function   UnwrapHeaders(var Headers:Core.Strings.VarString):Core.Strings.VarString;


   procedure  ConvertFromISOFile(FName,FAttach:Core.Strings.VarString);
   Procedure  DecodeISO(MS:TStringStream);
   Function   EncodeISO(MS:TStringStream):Core.Strings.VarString;
   procedure  DecodeQP(MS:TStringStream);
   Function   GetSMTPAddress(Const From:Core.Strings.VarString):Core.Strings.VarString;
   Function   GetISOFromTable(Seq:PChar):LongInt;
   function   ParseEmailStrings(S:Core.Strings.VarString):TStringList;
   Function   MonthToStr(Const Mon:LongInt):Core.Strings.VarString;
   Function   DayToStr(Const Day:LongInt):Core.Strings.VarString;
   Function   Filename_Validated(FName:Core.Strings.VarString):Boolean;
   Function   CommaToReturn(CText:Core.Strings.VarString):Core.Strings.VarString;
   function   GetEncMethod(S:Core.Strings.VarString) : TEncMethod;

implementation
Var
  ISO_Table: TISO;


function   UnwrapHeaders(var Headers:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=StringReplace(Headers,';'#13#10,';',[rfReplaceAll]);
  Result:=StringReplace(Result,#9,#32,[rfReplaceAll]);
  Result:=StringReplace(Result,#32#32,#32,[rfReplaceAll]);
end;

constructor EUUInvalidCharacter.Create;
begin
  inherited Create('Invalid character in the input file');
end;

Function Filename_Validated(FName:Core.Strings.VarString):Boolean;
Const
  Table = '\/:*?"<>|';
Var
  Len,Lcv:LongInt;
  Found:Boolean;
begin
  Len:=Length(FName);Lcv:=1;Found:=False;
  While (Lcv<=Len) and Not Found do begin
     Found:= Pos(FName[Lcv],Table)<>0;
     Inc(lcv);
  end;
  Result:=Not Found;
end;

procedure ParseHeader(var Input,Name,Value:Core.Strings.VarString);
var
  idx:LongInt;
  len:LongInt;
begin
  SetLength(Name,0);
  SetLength(Value,0);
  len:=System.Length(Input);
  idx:=System.Pos(': ',Input);
  if (idx>0) then begin
    Name:=System.Copy(Input,1,idx-1);
    Value:=System.Copy(Input,idx+2,len-idx-2);
  end;
end;

function JoinLines(const Addr,Name : Core.Strings.VarString) : Core.Strings.VarString;
begin
  Result:=Addr;
  if Name<>'' then
    Result:=Concat(Result,'|',Name);
end;

function GetFirstPart(const s : Core.Strings.Small) : Core.Strings.VarString;
{Gets first part of the Header line, where descr is truncated}
var
  sLen : byte absolute s;
  i : byte;
begin
  Result:='';
  i:=1;
  while (i<=sLen) and (s[i]<>' ') and (s[i]<>';') do
  begin
    Result:=Concat(Result,s[i]);
    Inc(i);
  end;
  Result:=TrimStr(Result);
end;

function TrimStr(s : Core.Strings.VarString) : Core.Strings.VarString;
begin
  while (Length(s)>0) and (s[1] in [' ',^I]) do
    System.Delete(s,1,1);
  while (Length(s)>0) and (s[Length(s)] in [' ',^I]) do
    System.Delete(s,Length(s),1);
  result:=s;
end;

function GetHeaderValue(Var List:Core.Arrays.Types.VarString; ID:Core.Strings.VarString): Core.Strings.VarString;
var
  Found : boolean;
  i,j : LongInt;
  HeaderValue:Core.Strings.VarString;

  Function Unwrap(LineIndex:LongInt):Core.Strings.VarString;
  Const
    Punct=#9#32;
  Var
     LLine:Core.Strings.VarString;
     iLen:LongInt;
  begin
     Result:='';
     If (LineIndex+1<Length(List)) then begin
        LLine:=List[LineIndex+1];
        iLen:=Length(LLine);
        If (iLen>0) and (Pos(LLine[1],Punct)>0) then begin // this is a segment
           LLine[1]:=#32;// space
           LLine:=SysUtils.Trim(LLine);
           iLen:=Length(LLine);
           If (iLen>0) and (LLine[Length(LLine)]=';') then // this is a segment and there is more
              Result:=Concat(LLine,Unwrap(LineIndex+1))
           else
              Result:=LLine; // this is the end
        end;
     end;
  end;
begin
  Found:=false; HeaderValue:='';Result:=''; ID:=UpperCase(ID);
  for i:=0 to Length(List)-1 do begin
    if Pos(ID,UpperCase(List[i]))=1 then begin
      Found:=true;
      Break;
    end;
  end;
  if Found then begin
    HeaderValue:=Concat(List[i],UnWrap(i));
    j:=Pos(':',HeaderValue);
    System.Delete(HeaderValue,1,j);
    Result:=TrimStr(HeaderValue);
  end;

end;

function GetHeaderValue(Hdr : TStrings; ID : Core.Strings.VarString) : Core.Strings.VarString;
var
  Found : boolean;
  i,j : LongInt;
  HeaderValue:Core.Strings.VarString;

  Function Unwrap(LineIndex:LongInt):Core.Strings.VarString;
  Const
    Punct=#9#32;
  Var
     LLine:Core.Strings.VarString;
  begin
     Result:='';
     If (LineIndex+1<hdr.Count) then begin
        LLine:=Hdr[LineIndex+1];
        If (Length(LLine)>0) and (Pos(LLine[1],Punct)>0) then begin // this is a segment
           LLine[1]:=#32;// space
           LLine:=SysUtils.Trim(LLine);
           If LLine[Length(LLine)]=';' then // this is a segment and there is more
              Result:=Concat(LLine,Unwrap(LineIndex+1))
           else
              Result:=LLine; // this is the end
        end;
     end;
  end;
begin
  Found:=false; HeaderValue:='';Result:=''; ID:=UpperCase(ID);
  for i:=0 to Hdr.Count-1 do begin
    if Pos(ID,UpperCase(Hdr[i]))=1 then begin
      Found:=true;
      Break;
    end;
  end;
  if Found then begin
    HeaderValue:=Concat(Hdr[i],UnWrap(i));
    j:=Pos(':',HeaderValue);
    System.Delete(HeaderValue,1,j);
    Result:=TrimStr(HeaderValue);
  end;
end;

function GetParameter(Param, Data : Core.Strings.VarString) : Core.Strings.VarString;
{Gets p="value" or p=value, returns 'value'}
{April 07, 1996, removing trailing ;}
Var
  saLcv,saParam:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  Result:='';
  Data:=StringReplace(Data,#9,'',[rfReplaceAll]);
  //Data:=StringReplace(Data,'  ',' ',[rfReplaceAll]);
  Data:=StringReplace(Data,'--=','',[rfReplaceAll]);
  Data:=StringReplace(Data,';'#13#10,';',[rfReplaceAll]);
  Data:=StringReplace(Data,'; ',';',[rfReplaceAll]);
  Core.Arrays.VarString.fromString(saLcv,Data,';');
  Try
    iLcv:=0;
    While (iLcv<Length(saLcv)) and (Result='') do begin
      // text/plain
      // Charset=us-ascii
      Core.Arrays.VarString.fromString(saParam,saLcv[iLcv],'=');
      Try
        If (Core.Arrays.VarString.IndexOf(saParam,Param)=0) then
          Result:=saParam[1];
      Finally
        SetLength(saParam,0);
      end;
      Inc(iLcv);
    end;
  Finally
    SetLength(saLcv,0);
  end;
  Core.Arrays.VarString.fromString(saParam,Result,'"');
  Try
    If Length(saParam)=2 then
      Result:=saParam[1];
  Finally
    SetLength(saParam,0);
  End;
end;

function GetEncMethod(S:Core.Strings.VarString) : TEncMethod;
begin
  if Core.Strings.Pos('base64',S)>0 then
    Result:=emBase64
  else if Core.Strings.Pos('7bit',S)>0 then
      Result:=em7Bit
  else if Core.Strings.Pos('8bit',S)>0 then
      Result:=em8Bit
  else if Core.Strings.Pos('quoted-printable',S)>0 then
    Result:=emQtPrn
  else If Core.Strings.Pos('binary',S)>0 then
    Result:=emBinary
  else
    Result:=emNone;
end;


function MakeBoundary : Core.Strings.VarString;
const
  Lcv:LongInt=0;
var
  i : LongInt;
begin
  Result:='----=_NextPart_00'+IntToStr(Lcv)+'_2F91_';
  for i:=1 to 8+Random(24) do begin
    Result:=Concat(Result,IntToStr(Random(9)));
  end;
  Inc(Lcv);
  If Lcv>1500 then
    Lcv:=0;
end;

function MakeUniqueID : Core.Strings.Small;
const
  Lcv:LongInt=0;
var
  i : LongInt;
begin
  Result:='';
  for i:=1 to 8+Random(24) do begin
    Result:=Concat(Result,IntToStr(Random(9)));
  end;
  Inc(Lcv);
  If Lcv>1500 then
    Lcv:=0;
end;

Function StickitsTimeOld(STime:TDateTime):Core.Strings.VarString;
Begin
   Result:=FormatDateTime('mm/dd/yy hh:nn AM/PM',STime);
end;

Function StickitsTimeNew(SDate,STime:TDateTime):Core.Strings.VarString;
Begin
   Result:=Concat(DateToStr(SDate),' ',TimeToStr(STime));
end;

Function DaysOfMonth(Const Month:LongInt):LongInt;
begin
  Case Month of
     1:DaysOfMonth:=31;
     2:DaysOfMonth:=29;
     3:DaysOfMonth:=31;
     4:DaysOfMonth:=30;
     5:DaysOfMonth:=31;
     6:DaysOfMonth:=30;
     7:DaysOfMonth:=31;
     8:DaysOfMonth:=31;
     9:DaysOfMonth:=30;
     10:DaysOfMonth:=31;
     11:DaysOfMonth:=30;
     12:DaysOfMonth:=31;
     else DaysOfMonth:=31;
  end;
end;

function ExchangeDomain(sAddress,sNewDomain:Core.Strings.VarString):Core.Strings.VarString;
Var
  sOldDomain:Core.Strings.VarString;
begin
   If Pos('@',sAddress)=0 then begin
      Result:='';
      Exit;
   end;
   sOldDomain:=ExtractDomain(sAddress);
   Result:=StringReplace(sAddress,sOldDomain,sNewDOmain,[rfReplaceAll]);
end;

Function ExtractListName(var Data:Core.Strings.VarString):Core.Strings.VarString;
var
  iBegin:LongInt;
begin
  iBegin:=Pos('<',Data);
  if iBegin>0 then begin
     // xxxxx <moredata>
    Result:=System.Copy(Data,1,iBegin-2);
  end else
    Result:=Data;
end;

Function ExtractName(S:Core.Strings.VarString):Core.Strings.VarString;
Var
  Lcv,Loc,EEnd,EBegin:LongInt;

Begin
   Ebegin:=Pos('@',S);
   If EBegin=0 then begin
      Result:='';
      Exit;
   end;
   EBegin:=Pos('<',S);
   If EBegin=0 then begin
      EBegin:=Pos('(',S)+1; // Stickits@cybercreek.com (Andy Brunner)
      EEnd:=Pos(')',S)-EBegin;
   end else begin
      EEnd:=EBegin-2;    // Andy Brunner <Stickits@cybercreek.com>
      EBegin:=1;
   end;
   Result:=System.Copy(S,Ebegin,EEnd);
   For Lcv:=1 to 2 do begin
     Loc:=Pos('"',Result);
     If Loc>0 then
        System.Delete(Result,Loc,1)
     else Break;
   end;
   Result:=SysUtils.Trim(Result);
end;
Function CommaToReturn(CText:Core.Strings.VarString):Core.Strings.VarString;
var
  Loc:LongInt;
  Finished:Boolean;
begin
   Result:=CText;
   Repeat
      Loc:=Pos(',',Result);
      Finished:=Loc=0;
      If Not Finished then begin
         System.Delete(Result,Loc,1);
         Insert(#13#10,Result,Loc);
      end;
   until Finished;
end;
Function ExtractUserName(S:Core.Strings.Small):Core.Strings.Small;
var
  Loc:LongInt;
begin
  Result:=ExtractAddress(S);
  Loc:=Pos('@',Result);
  If Loc>0 then
     Result:=System.Copy(Result,1,Loc-1);
end;

Function ExtractRoot(S:Core.Strings.Small):Core.Strings.Small;
var
  Loc,Len,DC:LongInt;
begin
   Len:=Length(S); Loc:=Len; DC:=0;Result:=S;
   While (Loc>0) and (Result=S) do begin
      If (S[Loc]='.') then begin
         Inc(Dc);
         If (Dc>1)then
            Result:=System.Copy(S,Loc+1,Len-Loc);
      end;
      Dec(Loc);
   end;
end;

Function ExtractDomain(S:Core.Strings.Small):Core.Strings.Small;
Var
  Loc:LongInt;
begin
   Result:=ExtractAddress(S);
   Loc:=Pos('@',Result);
   If Loc>0 then
      Result:=System.Copy(Result,Loc+1,Length(Result)-Loc)
   else
      Result:=ExtractRoot(Result);
end;

Function ExtractAddress(S:Core.Strings.VarString):Core.Strings.VarString;
Var
  EBegin:LongInt;
  EEnd:LongInt;
Begin
{
EBegin:=Pos('<',S); // Andy Brunner <Stickits@cybercreek.com>
If EBegin=0 Then begin // Stickits@cybercreek.com (Andy Brunner)
   EBegin:=Pos('(',S);
   If EBegin>0 then begin
      If Pos('@',S)>0 then begin
         EEnd:=EBegin-2;
         Result:=Copy(S,0,EEnd);
      end else Result:=S;
   end else Result:=S;
end else begin
    If Pos('@',S)>0 then begin
       EEnd:=Length(S)-EBegin-1;
       Result:=Copy(S,EBegin+1,EEnd);
    end else result:='';
end;
end;
}
   Result:=SysUtils.Trim(S);
   EBegin:=Pos('<',S); // Andy Brunner <Stickits@cybercreek.com>
   If EBegin>0 then begin
      EEnd:=Pos('>',S);
      Result:=System.Copy(S,EBegin+1,(EEnd-EBegin-1))
   end else If Pos('(',S)>0 then  begin // abrunner@attila (Andy Brunner)
       EBegin:=Pos('(',S);
       If EBegin>0 then begin
          EEnd:=EBegin;
          EBegin:=1;
          Result:=System.Copy(S,EBegin,(EEnd-EBegin-1))
       end;
   end;
end;

function GetDate(DateTimeVal:Core.Strings.VarString): Core.Strings.VarString;
Var
  Code,Lcv,Loc:LongInt;
  SDate,Stime:TDateTime;
  SMonth,SYear,SDay:Word;
  Month,Day,Year:Word;
  SHour,SMin,SSec:Word;
  S,TimeOfString,MonthOfString,YearofString,DayOfString:Core.Strings.VarString;

  function getword: Core.Strings.VarString;
  begin
    Result:='';
    While (Lcv<=Length(DateTimeVal)) and (DateTimeVal[Lcv]<>' ') do begin
       Result:=ConCat(Result,DateTimeVal[Lcv]);
       Inc(Lcv);
    end;
    Inc(Lcv); // Get Past Space
  end;
begin
   //nuke characters befor the ', 'from server
   //'Thu, 7 Nov 1996 02:57:25 -0500'
   Loc:=Pos('from',DateTimeVal);
   If Loc>0 then begin
      System.Delete(DateTimeVal,1,Loc);
      Loc:=Pos(';',DateTimeVal)+1;
      DateTimeVal:=SysUtils.Trim(System.Copy(DateTimeVal,Loc,Length(DateTimeVal)-Loc));
   end;
   Lcv:=1;
   // Eudora Check    //'17 Oct 95 10:47:22 EDT'
   S:=GetWord;
   {$Hints off}
   Val(S,Loc,Code); // Could be Thursday..
   If Code=0 then begin
      SDay:=Loc;
      S:=ConvertMonth(GetWord); //'Oct'
      Val(S,Loc,Code);
      If Code=0 then begin
         SMonth:=Loc;
         S:=GetWord; // << Year in
         Val(S,Loc,Code);
         If Code=0 then begin
            SYear:=Loc;
            // Time now..
            TimeOfString:=GetWord; //<< TimeString completely in!
            S:=System.Copy(TimeOfString,1,2); //<< Hour in
            Val(S,Loc,Code);
            If Code=0 then begin
               SHour:=Loc;
               S:=System.Copy(TimeOfString,3,2);//<< Min in
               Val(S,Loc,Code);
               If Code=0 then begin
                  SMin:=Loc;
                  S:=System.Copy(TimeOfString,6,2);
                  Val(S,Loc,Code); // Second In
                  if Code=0 then begin
                     SSec:=Loc;
                     STime:=EncodeTime(SHour,SMin,SSec,0);
                     SDate:=EncodeDate(SYear,SMonth,SDay);
                     Result:=StickitsTimeNew(SDate,STime);
                     // Dow we want to compensate for TimeZones!
                  end else begin
                     Result:=StickitsTimeNew(Date,Time);
                     Exit;
                  end;
               end else begin
                  Result:=StickitsTimeNew(Date,Time);
                  Exit;
               end;
            end else begin
               Result:=StickitsTimeNew(Date,Time);
               Exit;
            end;
         end else begin
            Result:=StickitsTimeNew(Date,Time);
            Exit;
         end;
      end else begin
         Result:=StickitsTimeNew(Date,Time);
         Exit;
      end;
   end else begin
      Val(GetWord,SDay,Code);
      If Code=0 then begin
         Val(ConvertMonth(GetWord),SMonth,Code);
         If Code=0 then begin
            Val(GetWord,SYear,Code);
            If Code=0 then begin
               Try
                  STime:=StrToTime(GetWord);
               Except
                  STime:=Time;
               End;
               DecodeDate(Now,Year,Month,Day);
               If (SMonth>12) or (SMonth<1) then
                  SMonth:=Month;
               If (SYear<Year) then
                  SYear:=Year;
               If (SDay>31) or (SDay<1) then
                  SDay:=Day;
               SDate:=EncodeDate(SYear,SMonth,SDay);
               Result:=StickitsTimeNew(SDate,STime);
            end else begin
               Result:=StickitsTimeNew(Date,Time);
               Exit;
            end;
         end else begin
            Result:=StickitsTimeNew(Date,Time);
            Exit;
         end;
      end else begin
         Result:=StickitsTimeNew(Date,Time);
         Exit;
      end;
   end;
end;

Function   DayToStr(Const Day:LongInt):Core.Strings.VarString;
begin
   Case Day of
      1: Result:='Sunday';
      2: Result:='Monday';
      3: Result:='Tuesday';
      4: Result:='Wednesday';
      5: Result:='Thursday';
      6: Result:='Friday';
      7: Result:='Saturday';
   else
      Result:='Invalid Month';
   end;
end;

Function MonthToStr(Const Mon:LongInt):Core.Strings.VarString;
begin
   Case Mon of
      1: Result:='January';
      2: Result:='February';
      3: Result:='March';
      4: Result:='April';
      5: Result:='May';
      6: Result:='June';
      7: Result:='July';
      8: Result:='August';
      9: Result:='September';
      10: Result:='October';
      11: Result:='November';
      12: Result:='December';
   else
      Result:='Invalid Month';
   end;
end;
function ConvertMonth(Const Mon:Core.Strings.VarString):Core.Strings.VarString;
Begin
   If Mon= 'Jan' then result:='01' else
   If Mon='Feb' then result:='02' else
   if mon='Mar' then result:='03' else
   If mon='Apr' then result:='04' else
   If mon='May' then result:='05' else
   if mon='Jun' then result:='06' else
   if mon='Jul' then result:='07' else
   if mon='Aug' then result:='08' else
   if mon='Sep' then result:='09' else
   if mon='Oct' then result:='10' else
   if mon='Nov' then result:='11' else
   if mon='Dec' then result:='12';
end;
Function GetSMTPAddress(Const From:Core.Strings.VarString):Core.Strings.VarString;
Var
   N,A:Core.Strings.VarString;
begin
   Result:='';
   N:=ExtractName(From);
   A:=ExtractAddress(From);
   If N<>'' then begin
      N:=Concat('"',N,'"');
      Result:=N+' <'+A+'>';
   end else
      Result:=A;
end;

Function EncodeISO(MS:TStringStream):Core.Strings.VarString;
var
  SS:TStringStream;
  MSPos,MSSize:Int64;
  Ch,ChNext:Char;
begin
  SS:=TStringStream.Create(''); Result:='';
  Try
    MS.Position:=0; MSSize:=MS.Size; MSPos:=0;
    While MSPos<MSSize do begin
      MS.ReadBuffer(Ch,1);
      Inc(MSPos);
      If Ch='=' then begin
        If MS.Position<MSSize then begin
          MS.ReadBuffer(ChNext,1);
          Inc(MSPos);
          If Not (ChNext in [#13,#10]) then
            SS.WriteString('=3D')
          else
            SS.WriteString('=');
          SS.WriteBuffer(ChNext,1);
        end else
          SS.WriteBuffer(Ch,1);
      end else
        SS.WriteBuffer(Ch,1);
    end;
  Finally
    Result:=SS.DataString;
    SS.Free;
  end;
end;

Procedure DecodeISO(MS:TStringStream);
Const
  SafeSize = 252;
  ISO_Vals = ['0'..'9'];
Var
  Body:Core.Strings.VarString;
  KeyWord:Core.Strings.VarString;
  Code,Index,Loc,Len:LongInt;
  NewMS:TStringStream;

  Function GetKeyword:Core.Strings.VarString; // Will get keyword if good
  var
    Lcv:LongInt;
    Ch:Core.Strings.VarString;
  begin
     Result:=''; Lcv:=1;
     While (Lcv<4) do begin
        If Lcv<=Len then begin
           If (Body[Lcv] in ISO_Vals) then
              Result:=Concat(Result,Body[Lcv])
           else
              Lcv:=4; //violate
        end else begin // We went over the body.. Read in from position...
           Ch:=MS.ReadString(1);
           If (Ch[1] in ISO_Vals) then
              Result:=Concat(Result,Body[Lcv])
           else begin
              MS.Position:=MS.Position-1; // Put back
              Lcv:=4;// violate
           end;
        end;
        Inc(Lcv);
     end;
  end;

begin
   MS.Position:=0;
   NewMS:=TStringStream.Create('');
   Try
      Repeat
         Body:=MS.ReadString(SafeSize);
         Repeat
            Loc:=Pos('=',Body);
            If (Loc>0) then begin
               // Write the stuff before and nuke...
               NewMS.WriteString(System.Copy(Body,1,Loc-1));
               System.Delete(Body,1,Loc-1);
               // end of remove!
               Len:=Length(Body);
               Keyword:=GetKeyWord;

               If (Keyword<>'') then begin
                  Index:=StrToInt(Keyword);
                  If Index<256 then begin
                     NewMS.WriteString(ISO_Table[Index]);
                  end else
                     NewMS.WriteString('='+Keyword);
               end else
                  NewMS.WriteString('='+Keyword);
               System.Delete(Body,1,Length(Keyword)+1);
            end else
               NewMS.WriteString(Body);
         until Loc=0;
      until MS.Position=MS.Size;
      MS.Size:=0;
      Ms.WriteString(NewMS.DataString);
   Finally
      NewMS.Free;
   end;
end;
Procedure DecodeQP(MS:TStringStream);
Var
   Steps,   // Number of MS advances
   Code,Lcv:LongInt;
   Buf,QP:Pchar;
   QPVal:Core.Strings.VarString;
   NewStream:TStream;
begin
   Ms.Position:=0;
   Try
      QP:=StrAlloc(4);
   Except
      Exception.Create('Out of Memory');
      Exit;
   end;
   Try
      Buf:=StrAlloc(2);
   Except
      StrDispose(QP);
      Exception.Create('Out of Memory');
      Exit;
   end;
   Try
      NewStream:=TMemoryStream.Create;
   Except
      StrDispose(QP);
      StrDispose(Buf);
      Exception.Create('Out of Memory');
      Exit;
   end;
   Try
      While Ms.Position<Ms.Size do begin
          Steps:=0;
          Ms.readBuffer(Buf^,1);
          Inc(Steps);
          If Buf='=' then begin
             QP[0]:=Buf[0];
             QPVal:='';
             For Lcv:=1 to 2 do begin
                If MS.Position<Ms.Size then begin
                   MS.ReadBuffer(Buf^,1);
                   Inc(Steps);
                   QP[Lcv]:=Buf[0];
                   QPVal:=QPVal+Char(Buf[0]);
                end;
             end;
             // validate the two QP[1] and QP[2]
             Val(QPVal,Lcv,Code);
             If (Code=0) and (Lcv>0) and (Lcv<256) then begin
                Buf[0]:=Chr(Lcv);
                NewStream.WriteBuffer(Buf^,StrLen(Buf));
             end else
                NewStream.WriteBuffer(QP^,Steps);
         end else
            NewStream.WriteBuffer(Buf^,1);
      end;
      MS.Size:=0;
      NewStream.Position:=0;
      Ms.CopyFrom(NewStream,NewStream.Size);
   Finally
      NewStream.Free;
      StrDispose(Buf);
      StrDispose(QP);
   end;
end;

Procedure ConvertFromISOFile(Fname,FAttach:Core.Strings.VarString);
Var
   Loc:LongInt;
   FIn,Fout:TextFile;
   FTName:Core.Strings.VarString;

   Function GetNextLine:Core.Strings.VarString;
   Begin
     If not EOF(FIn) Then
       ReadLn(FIn,Result);
     Loc:=Length(Result);
     If Loc>0 then begin
       If (Result[Loc]='=') Then begin
         SetLength(Result,Loc-1);
         Result:=Concat(Result,GetNextLine);
       End;
       If (Pos('=20',Result)=Loc-2) Then
         System.Delete(Result,Loc-2,3);
     end;
   end;
begin
   Try
      AssignFile(FIn,FName);
      AssignFile(FOut,FAttach);
      Rewrite(FOut);
      Reset(FIn);
      While Not EOF(FIn) do
          WriteLn(Fout,GetNextLine);
      CloseFile(FOut);
      CloseFile(FIn);
   Except
      CloseFile(FOut);
      CloseFile(FIn);
      Exit;
   end;
     DeleteFile(FName);
     RenameFile(FTName,FName);
end;

Function GetISOFromTable(Seq:PChar):LongInt;
Var
 Lcv:LongInt;
 NotFound:Boolean;
begin
   Result:=-1;
   NotFound:=True;
   If StrComp(Seq,ISO_TABLE[9])=0 then begin
      REsult:=9;
      NotFound:=False;
   end else If StrComp(Seq,'='+#13#10)=0 then begin
      Result:=-1;
      NotFound:=False;
   end;
   If NotFound then begin
      For Lcv:=32 to 126 do
         If StrComp(Seq,ISO_Table[Lcv])=0 then begin
           Result:=Lcv;
           NotFound:=False;
           break;
         end;
   end;
   If NotFound then begin
      For Lcv:=160 to 255 do
         If StrComp(Seq,ISO_Table[Lcv])=0 then begin
           Result:=Lcv;
           break;
         end;
   end;
end;

function ParseEmailStrings(S:Core.Strings.VarString):TStringList;
var
EmailList:TStringList;

  function getword: Core.Strings.VarString;
  {return a word from start and on
   set cpos to the position after the word}
  begin
   result:='';
//    skip space
    while (0 < length(S)) and (S[1] in [' ', ',']) do
      System.Delete(S,1,1);
{    go to the end of the word
     Activate if char = " or (
     Deactivate if char = " or )
}
    while (0 < length(S)) do begin
      If (S[1] in ['"','(']) then begin // Move to endof str or )" found!
         Result:=Result+S[1];
         System.Delete(S,1,1);
         while (0<length(S)) and Not (S[1] in ['"',')']) do begin
            Result:=Result+S[1];
            System.Delete(S,1,1);
         end;
      end else if (S[1] <>',') then  begin
         result:=result+S[1];
         System.Delete(S,1,1);
      end else
         Break;
    end;
  end;

begin
  //BEGIN PARSING ROUTINE FOR ADDRESS LISTS
  //Load Email Addresses
  EmailList:=TStringList.Create;
  EmailList.Clear;
  While SysUtils.Trim(S)<>'' do
     EmailList.Add(SysUtils.Trim(GetWord));
  Result:=EmailList;
  EmailList:=Nil;
  EMailList.Free;
end;

initialization
   Randomize;
   FillChar(ISO_Table,255,#0);
   ISO_Table[9]:='=09';
   ISO_Table[32]:='=20';
   ISO_Table[33]:='=21';
   ISO_Table[34]:='=22';
   ISO_Table[35]:='=23';
   ISO_Table[36]:='=24';
   ISO_Table[37]:='=25';
   ISO_Table[38]:='=26';
   ISO_Table[39]:='=27';
   ISO_Table[40]:='=28';
   ISO_Table[41]:='=29';
   ISO_Table[42]:='=2A';
   ISO_Table[43]:='=2B';
   ISO_Table[44]:='=2C';
   ISO_Table[45]:='=2D';
   ISO_Table[46]:='=2E';
   ISO_Table[47]:='=2F';
   ISO_Table[48]:='=30';
   ISO_Table[49]:='=31';
   ISO_Table[50]:='=32';
   ISO_Table[51]:='=33';
   ISO_Table[52]:='=34';
   ISO_Table[53]:='=35';
   ISO_Table[54]:='=36';
   ISO_Table[55]:='=37';
   ISO_Table[56]:='=38';
   ISO_Table[57]:='=39';
   ISO_Table[58]:='=3A';
   ISO_Table[59]:='=3B';
   ISO_Table[60]:='=3C';
   ISO_Table[61]:='=3D';
   ISO_Table[62]:='=3E';
   ISO_Table[63]:='=3F';
   ISO_Table[64]:='=40';
   ISO_Table[65]:='=41';
   ISO_Table[66]:='=42';
   ISO_Table[67]:='=43';
   ISO_Table[68]:='=44';
   ISO_Table[69]:='=45';
   ISO_Table[70]:='=46';
   ISO_Table[71]:='=47';
   ISO_Table[72]:='=48';
   ISO_Table[73]:='=49';
   ISO_Table[74]:='=4A';
   ISO_Table[75]:='=4B';
   ISO_Table[76]:='=4C';
   ISO_Table[77]:='=4D';
   ISO_Table[78]:='=4E';
   ISO_Table[79]:='=4F';
   ISO_Table[80]:='=50';
   ISO_Table[81]:='=51';
   ISO_Table[82]:='=52';
   ISO_Table[83]:='=53';
   ISO_Table[84]:='=54';
   ISO_Table[85]:='=55';
   ISO_Table[86]:='=56';
   ISO_Table[87]:='=57';
   ISO_Table[88]:='=58';
   ISO_Table[89]:='=59';
   ISO_Table[90]:='=5A';
   ISO_Table[91]:='=5B';
   ISO_Table[92]:='=5C';
   ISO_Table[93]:='=5D';
   ISO_Table[94]:='=5E';
   ISO_Table[95]:='=5F';
   ISO_Table[96]:='=60';
   ISO_Table[97]:='=61';
   ISO_Table[98]:='=62';
   ISO_Table[99]:='=63';
   ISO_Table[100]:='=64';
   ISO_Table[101]:='=65';
   ISO_Table[102]:='=66';
   ISO_Table[103]:='=67';
   ISO_Table[104]:='=68';
   ISO_Table[105]:='=69';
   ISO_Table[106]:='=6A';
   ISO_Table[107]:='=6B';
   ISO_Table[108]:='=6C';
   ISO_Table[109]:='=6D';
   ISO_Table[110]:='=6E';
   ISO_Table[111]:='=6F';
   ISO_Table[112]:='=70';
   ISO_Table[113]:='=71';
   ISO_Table[114]:='=72';
   ISO_Table[115]:='=73';
   ISO_Table[116]:='=74';
   ISO_Table[117]:='=75';
   ISO_Table[118]:='=76';
   ISO_Table[119]:='=77';
   ISO_Table[120]:='=78';
   ISO_Table[121]:='=79';
   ISO_Table[122]:='=7A';
   ISO_Table[123]:='=7B';
   ISO_Table[124]:='=7C';
   ISO_Table[125]:='=7D';
   ISO_Table[126]:='=7E';
   // BLAH BLAH BLAH
   ISO_Table[160]:='=A0';
   ISO_Table[161]:='=A1';
   ISO_Table[162]:='=A2';
   ISO_Table[163]:='=A3';
   ISO_Table[164]:='=A4';
   ISO_Table[165]:='=A5';
   ISO_Table[166]:='=A6';
   ISO_Table[167]:='=A7';
   ISO_Table[168]:='=A8';
   ISO_Table[169]:='=A9';
   ISO_Table[170]:='=AA';
   ISO_Table[171]:='=AB';
   ISO_Table[172]:='=AC';
   ISO_Table[173]:='=AD';
   ISO_Table[174]:='=AE';
   ISO_Table[175]:='=AF';
   ISO_Table[176]:='=B0';
   ISO_Table[177]:='=B1';
   ISO_Table[178]:='=B2';
   ISO_Table[179]:='=B3';
   ISO_Table[180]:='=B4';
   ISO_Table[181]:='=B5';
   ISO_Table[182]:='=B6';
   ISO_Table[183]:='=B7';
   ISO_Table[184]:='=B8';
   ISO_Table[185]:='=B9';
   ISO_Table[186]:='=BA';
   ISO_Table[187]:='=BB';
   ISO_Table[188]:='=BC';
   ISO_Table[189]:='=BD';
   ISO_Table[190]:='=BE';
   ISO_Table[191]:='=BF';
   ISO_Table[192]:='=C0';
   ISO_Table[193]:='=C1';
   ISO_Table[194]:='=C2';
   ISO_Table[195]:='=C3';
   ISO_Table[196]:='=C4';
   ISO_Table[197]:='=C5';
   ISO_Table[198]:='=C6';
   ISO_Table[199]:='=C7';
   ISO_Table[200]:='=C8';
   ISO_Table[201]:='=C9';
   ISO_Table[202]:='=CA';
   ISO_Table[203]:='=CB';
   ISO_Table[204]:='=CC';
   ISO_Table[205]:='=CD';
   ISO_Table[206]:='=CE';
   ISO_Table[207]:='=CF';
   ISO_Table[208]:='=D0';
   ISO_Table[209]:='=D1';
   ISO_Table[210]:='=D2';
   ISO_Table[211]:='=D3';
   ISO_Table[212]:='=D4';
   ISO_Table[213]:='=D5';
   ISO_Table[214]:='=D6';
   ISO_Table[215]:='=D7';
   ISO_Table[216]:='=D8';
   ISO_Table[217]:='=D9';
   ISO_Table[218]:='=DA';
   ISO_Table[219]:='=DB';
   ISO_Table[220]:='=DC';
   ISO_Table[221]:='=DD';
   ISO_Table[222]:='=DE';
   ISO_Table[223]:='=DF';
   ISO_Table[224]:='=E0';
   ISO_Table[225]:='=E1';
   ISO_Table[226]:='=E2';
   ISO_Table[227]:='=E3';
   ISO_Table[228]:='=E4';
   ISO_Table[229]:='=E5';
   ISO_Table[230]:='=E6';
   ISO_Table[231]:='=E7';
   ISO_Table[232]:='=E8';
   ISO_Table[233]:='=E9';
   ISO_Table[234]:='=EA';
   ISO_Table[235]:='=EB';
   ISO_Table[236]:='=EC';
   ISO_Table[237]:='=ED';
   ISO_Table[238]:='=EE';
   ISO_Table[239]:='=EF';
   ISO_Table[240]:='=F0';
   ISO_Table[241]:='=F1';
   ISO_Table[242]:='=F2';
   ISO_Table[243]:='=F3';
   ISO_Table[244]:='=F4';
   ISO_Table[245]:='=F5';
   ISO_Table[246]:='=F6';
   ISO_Table[247]:='=F7';
   ISO_Table[248]:='=F8';
   ISO_Table[249]:='=F9';
   ISO_Table[250]:='=FA';
   ISO_Table[251]:='=FB';
   ISO_Table[252]:='=FC';
   ISO_Table[253]:='=FD';
   ISO_Table[254]:='=FE';
   ISO_Table[255]:='=FF';

end.

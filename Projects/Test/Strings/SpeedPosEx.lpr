program SpeedPosEx;

{$mode objfpc}
{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,cwStrings,
  {$ENDIF}{$ENDIF}
  Classes, StrUtils,SysUtils,DateUtils;

Const
  LINE_LEN     = 80;
  LINE_COUNT   = 25000;
  CRLF         = #13#10;
  DELIM        = CRLF;
  DELIMLEN     = 2;
Var
  Data           : UTF8String;
  Test1_Duration : Int64;
  Test2_Duration : Int64;
  Test2_Lead     : Int64;
  LineCount_1    : Integer;
  LineCount_2    : Integer;

Type
  TStringArray = Array of UTF8String;

  function  GenerateData:UTF8String;
  var
    iLcv:integer;
    sLine:UTF8String;
  begin
    SetLength(Result,0);
    SetLength(sLine,0);
    // Simulate Base64 Encoded Message ~2MB
    for iLcv:=1 to LINE_LEN do
      sLine:=Concat(sLine,'X');
    for iLcv:=1 to LINE_COUNT do
        Result:=Concat(Result,sLine,CRLF);
  end;

  function  TestPosEx(Const Delim:UTF8String; var Source:UTF8String; Offset:Int64; Length:Int64):Int64;
  var
    iDelimLen:Int64;
    iLcv:Int64;
    iLength:Int64;
    iDelimBias:integer;
  begin
    Result:=0;
    iDelimLen:=System.Length(Delim);
    iDelimBias:=iDelimLen-1; // To include prior singleton
    if iDelimLen>0 then begin
      iLcv:=Offset;
      While (iLcv+iDelimBias<=iLength) and (Result=0) do begin
        If System.CompareByte(Source[iLcv],Delim[1],iDelimLen)=0 then
          Result:=iLcv;
        Inc(iLcv);
      end;
    end;
  end;

  Function Test1():Int64;
  var
    iLcv:Int64;
    iLen:Int64;
    iPosition:Int64;
    dtStart:TDateTime;
    dtEnd:TDateTime;
  begin
    Result:=0;




    dtStart:=SysUtils.Now();
    iLcv:=1;
    iLen:=System.Length(Data);
    iPosition:=0;
    While (iLcv+DELIMLEN<=iLen) do begin
      iPosition:=StrUtils.PosEx(CRLF,Data,iLcv);
      if iPosition>0 then begin
        iLcv:=iPosition+DELIMLEN;
        Inc(LineCount_1);
      end else
        iLcv:=iLen;
    end;
    dtEnd:=Now;

    Result:=DateUtils.SecondsBetween(dtEnd,dtStart);
  end;

Function Test2():Int64;
var
  iLcv:Int64;
  iLen:Int64;
  iPosition:Int64;
  dtStart:TDateTime;
  dtEnd:TDateTime;
begin
  Result:=0;

  dtStart:=SysUtils.Now();

  iLcv:=1;
  iLen:=System.Length(Data);
  iPosition:=0;
  While (iLcv+DELIMLEN<=iLen) do begin
    iPosition:=TestPosEx(CRLF,Data,iLcv,iLen);
    if iPosition>0 then begin
      iLcv:=iPosition+DELIMLEN;
      Inc(LineCount_2);
    end else
      iLcv:=iLen;
  end;
  dtEnd:=Now;

  Result:=DateUtils.SecondsBetween(dtEnd,dtStart);
end;


begin
  DefaultSystemCodePage:=CP_UTF8;
  Data:=GenerateData();
  LineCount_1:=0;
  LineCount_2:=0;
  Test2_Duration:=Test2();
  Test1_Duration:=Test1();
  Test2_Lead:=Test1_Duration-Test2_Duration;

  if (Test2_Lead>0) then begin
    Raise Exception.Create(
      Concat(
        'Test1 counted ',IntToStr(LineCount_1),' lines'#13#10,
        'Test1 was ',IntToStr(Test1_Duration),' seconds'#13#10,
        'Test2 counted ',IntToStr(LineCount_2),' lines'#13#10,
        'Test2 was ',IntToStr(Test2_Duration),' seconds'#13#10,
        'Test2 faster by ',IntToStr(Test2_Lead),' seconds'
      )
    );
  end;
end.


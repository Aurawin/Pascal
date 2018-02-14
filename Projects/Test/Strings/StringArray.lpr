program StringArray;

{$mode objfpc}
{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,cwStrings,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils;

Const
  LINE_LEN     = 80;
  LINE_COUNT   = 100000;
  CRLF         = #13#10;
  DELIM        = CRLF;
  DELIMLEN     = 2;
Type
  TStringArray = Array of UTF8String;

  function  GenerateData:UTF8String;
  var
    iLcv:integer;
  begin
    SetLength(Result,0);
    for iLcv:=1 to LINE_COUNT do
      Result:=Concat(Result,#9,CRLF);
  end;

  function  TestPosEx(Const Delim:UTF8String; var Source:UTF8String; Offset:Int64):Int64;
  var
    iDelimLen:Int64;
    iLcv:Int64;
    iLength:Int64;
  begin
    Result:=0;
    iDelimLen:=System.Length(Delim);
    iLength:=System.Length(Source);
    if iDelimLen>0 then begin
      iLcv:=Offset;
      While (iLcv+iDelimLen<=iLength) and (Result=0) do begin
        If System.CompareByte(Source[iLcv],Delim[1],iDelimLen)=0 then
          Result:=iLcv;
        Inc(iLcv);
      end;
    end;
  end;

  procedure Test1();
  var
    Data:TVarString;
    iLcv:Int64;
    iLen:Int64;
    iPosition:Int64;
  begin
    Data:=GenerateData();
    iLcv:=1;
    iLen:=System.Length(Data);

    While (iLcv<iLen) do begin

    end;


    FLength:=uStringArray.fromString(FContent,Data,#13#10,[sasoClearList],@Callback);
  end;

begin
  DefaultSystemCodePage:=CP_UTF8;
  Test();
end.


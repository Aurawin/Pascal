program UTF8String;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,SysUtils;

  procedure Test();
  var
    sData    : AnsiString;
    utfData  : UTF8String;
  begin
    sData:='Über';
    utfData:='Über';
    If SameText(sData,uftData)=false then
      raise exception.create('failure');
  end;

begin
  DefaultSystemCodePage:=CP_UTF8;
  Test();
end.


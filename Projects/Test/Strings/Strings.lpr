program Strings;

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
    iDataLen : integer;
    iutfLen  : integer;
  begin
    sData:='Über';
    utfData:=#343'ber';
    iDataLen:=Length(sData);
    iutfLen:=Length(utfData);

    If SameText(sData,utfData)=false then
      raise exception.create(utfData);
  end;

begin
  DefaultSystemCodePage:=CP_UTF8;
  Test();
end.


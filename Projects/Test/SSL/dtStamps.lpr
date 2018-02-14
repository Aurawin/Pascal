program dtStamps;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,DateUtils,SysUtils;

  var
    sInput:string;
    dtFuture:TDateTime;
    dtCert:TDateTime;
    dtNow:TDateTime;
    epSSL:TDateTime;
    epFPC:TDateTime;
    iVal:Int64;
    iAdjVal:Int64;
    iBias:Int64;
begin
  dtNow:=Now;
  dtFuture:=EncodeDateTime(2017,8,6,0,0,0,0);
  sInput:='200530104838';
  epSSL:=EncodeDateTime( 1970, 1, 1, 0, 0, 0, 0 );
  epFPC:=0;
  iVal:=StrToInt64Def(sInput,0);

  iBias:=DateUtils.SecondsBetween(epFPC,epSSL);
  iAdjVal:=iVal-iBias;

  dtCert:=DateUtils.IncSecond(epSSL,iVal);



end.


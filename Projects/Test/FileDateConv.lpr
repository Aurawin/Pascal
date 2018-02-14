program FileDateConv;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, Interfaces, uFileUtils,hDateUtils, hTimer, uTimer, DateUtils, SysUtils;

  procedure Test();
  var
    dtNow         : double;
    dtHourAgo     : double;
    dtNowPlus     : double;
    dtNowMinus    : double;
    dtDiff        : double;
    dtCheck       : double;
    FactorPlus    : Integer;
    FactorMinus   : Integer;
    iBias         : Int64;
    Threshold     : double;
    Factor        : Integer;
  begin
    dtNow:=Now;
    Factor:=50;
    Threshold:=hDateUtils.Millisecond*Factor;

    dtNowPlus:=DateUtils.IncMilliSecond(dtNow,-Factor);
    dtNowMinus:=DateUtils.IncMilliSecond(dtNow,Factor);

    dtDiff:=dtNow-dtNowPlus;
    FactorPlus:=Round(dtDiff/MilliSecond);
    dtDiff:=dtNow-dtNowMinus;
    FactorMinus:=Round(dtDiff/MilliSecond);


    if dtDiff<0 then
      dtDiff*=-1;

    if dtDiff<Threshold then
      dtDiff:=0;
    iBias:=-60*1000;
    dtHourAgo:=DateUtils.IncMilliSecond(dtNow,iBias);


    dtCheck:=DateUtils.IncHour(dtHourAgo,1);

    if dtHourAgo<>dtCheck then
      raise Exception.Create('invalid conversion');
  end;

begin
  Test();
end.


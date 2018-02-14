program SysInfo;

{$mode objfpc}{$H+}

uses
  cmem, cthreads, Classes, Linux, sysUtils;

{$R *.res}


  function Test:boolean;
  const
    FMT_RESULT:Array[boolean] of string= ('Fail (%.d<>%.d'#13#10,'Pass (%.d=.%.d)'#13#10);
  var
    sInfo:TSysInfo; // See memunit!
    iComp1:Cardinal;
    iComp2:Cardinal;
  begin
    Linux.Sysinfo(@sInfo);
    iComp1:=sInfo.totalram;
    iComp2:=(sInfo.bufferram + sInfo.freeram);
    //iComp2:=sInfo.freeram+sInfo.bufferram+sInfo.sharedram;

     Writeln(Format(FMT_RESULT[(iComp1=iComp2)],[iComp1,iComp2]));
  end;

begin
  Test;
end.


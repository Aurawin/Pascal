program AppBuild;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, IniFiles;

{$R AppBuild.res}

Var
  {$i AppBuild.Variables.inc}

begin
  {$i AppBuild.Source.inc}
end.


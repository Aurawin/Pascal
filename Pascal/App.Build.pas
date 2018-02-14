unit App.Build;

interface

uses
  Classes,

  Core.Strings,

  SysUtils;
var
  Title                      : Core.Strings.VarString;
  Caption                    : Core.Strings.VarString;
  Version                    : Core.Strings.VarString = {$i AppBuild.inc};
  Edition                    : Core.Strings.VarString = {$i %FPCTARGETOS%}+' '+{$i %FPCTARGETCPU%};
  Path                       : Core.Strings.VarString;
  EXE                        : Core.Strings.VarString;
  KEY_PUBLIC                 : Core.Strings.VarString;
  KEY_PRIVATE                : Core.Strings.VarString;
  RSR                        : Core.Strings.VarString = {$i RSRBuilder.inc};

implementation

initialization
  Path:=IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
  EXE:=ExtractFileName(ParamStr(0));
end.


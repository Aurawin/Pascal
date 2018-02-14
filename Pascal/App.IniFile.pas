unit App.IniFile;

interface

uses
  Core.Strings,

  IniFiles,
  Classes,
  SysUtils;

var
  Native                         : TIniFile;


  procedure Init(aFileName:Core.Strings.FileName);
  procedure Done();


implementation

procedure Init(aFileName:Core.Strings.FileName);
begin
  if (App.IniFile.Native)=nil then begin
    App.IniFile.Native:=TIniFile.Create(aFileName);
  end;
end;

procedure Done();
begin
  if (App.IniFile.Native<>nil) then begin
    App.IniFile.Native.Free();
  end;
end;

end.


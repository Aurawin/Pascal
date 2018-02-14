unit Storage.Main;

interface

uses

  Core.Database,
  Core.Database.Types,
  Storage.Main.Types;


var
  Header                       : Storage.Main.Types.THeader;
  Task                         : Core.Database.Types.TTask;


  procedure Init();
  procedure Done();

implementation
uses
  App.IniFile,
  Core.Database.Monitor,
  Storage;

procedure Init();
begin
  If (Header=nil) then begin
    Header:=Storage.Main.Types.THeader.Create();
  end;
  if (Task=nil) then
    Task:=Core.Database.Types.TTask.Create(Header,'Storage.Main');

  Core.Database.Monitor.GetIniValues(App.IniFile.Native);
end;

procedure Done();
begin
  If (Task=nil) then
    Task.Free();
  If Header<>nil then
    Header.Free();
end;



end.

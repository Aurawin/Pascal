unit Core.Database.Timer;


interface

uses

  Core.Database.Types,
  Classes,
  SysUtils;

Var
  Background : Core.Database.Types.TTimer;

  procedure Init(Header:Core.Database.Types.THeader);
  procedure Done();

  function Create(Header:Core.Database.Types.THeader):Core.Database.Types.TTimer;

implementation

procedure Init(Header:Core.Database.Types.THeader);
begin
  if (Background=nil) then
    Background:=Create(Header);
end;

procedure Done();
begin
  if (Background=nil) then
    Background.Free();
end;

function Create(Header:Core.Database.Types.THeader):Core.Database.Types.TTimer;
begin
  Result:=Core.Database.Types.TTimer.Create(Header,tpNormal);
end;

end.


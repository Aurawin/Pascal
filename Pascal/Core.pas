unit Core;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  Exception=SysUtils.Exception;

  procedure FreeAndNil(Obj:TObject);

implementation

procedure FreeAndNil(Obj:TObject);
begin
  if Assigned(Obj) then
    Obj.Free;
end;

end.


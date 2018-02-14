library project1;

{$mode objfpc}{$H+}

  uses SysUtils,DynLibs,hMatrixMemory,hCharArray;

var
  MemManager:TMatrixMemory;

  function MyTest(var ArrayP:Pointer):boolean; export; stdcall;
  begin
    Result:=False;
    if Assigned(MemManager) then begin
      ArrayP:=Pointer(MemManager.CharArray_New);
      MemManager.CharArray_SetSize(PCharArray(ArrayP),9);
      MemManager.CharArray_Copy('Test came out OK',PCharArray(ArrayP)^);
      Result:=true;
    end;
  end;

  function Initialize:boolean;  export; stdcall;
  begin
    Result:=true;
    MemManager:=TMatrixMemory.Create;
  end;

  function Finalize:boolean; export; stdcall;
  begin
    result:=true;
    FreeAndNil(MemManager);
  end;

  exports MyTest,Initialize,Finalize;

{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

begin
end.


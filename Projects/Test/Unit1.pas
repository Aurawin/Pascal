unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  DynLibs, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,hCharArray,uCharArray,
  hMatrixMemory,StdCtrls,hMatrixMemoryCollection ,hMatrixMemoryRequest, uMatrixMemoryRequest,uMatrixMemoryCollection;

type
  TCBMyTest=function(Var DataP:Pointer):boolean; stdcall;
  TCBMyTestBool=function () : boolean; stdcall;
  { TForm1 }
  TMyTest=Array[THandle] of pointer;
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FLibHandle                   : TLibHandle;
    FMemManagerHandle            : TLibHandle;
    FMyTest                      : TCBMyTest;
    FMyTestInit                  : TCBMyTestBool;
    FMyTestFinalize              : TCBMyTestBool;
//    FMatrixMemory                : TMatrixMemory;

    FMMM_Initialize              : TMatrixInfoFunction;
    FMMM_Finalize                : TMatrixInfoFunction;
    FMMM_Info                    : TMatrixMemoryInfo;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Empty(FMMM_Info);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  DataP:Pointer;
begin
  if Assigned(FMyTest) then begin
    Label1.Caption:='Loaded';
    DataP:=nil;
    if FMyTest(DataP) then begin
      Label2.Caption:='It Worked';
      Label2.Caption:=uCharArray.toString(PCharArray(DataP)^);
    end else begin
      Label1.Caption:='MyTest Returned False';
      //Label2.Caption:=uCharArray.toString(FResult);
    end;
  end else begin
    Label1.Caption:='MyTest Not Found';
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Button1.Enabled:=False;
  Button2.Enabled:=False;
  if FLibHandle<>0 then begin
    FMyTestFinalize;

    FMyTestInit:=nil;
    FMyTestFinalize:=nil;
    FMyTest:=nil;

    DynLibs.UnloadLibrary(FLibHandle);
    FLibHandle:=0;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  ArrayP                         : PCharArray;
begin
//  New(ArrayP);
  //ArrayP:=PCharArray(FMatrixMemory.CharArray_New);
//  uCharArray.SetSize(ArrayP,10);
//  FMatrixMemory.CharArray_SetSize(ArrayP,10);
//  FMatrixMemory.CharArray_Copy('This is a test',ArrayP^);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
 sLib : String;
 iLastError:Integer;
begin
  sLib:='/home/atbrunner/Pascal/Source/Libraries/MatrixMemory/MatrixMemoryManager.so';
  if FMemManagerHandle=0 then begin
    if FileExists(sLib) then begin
      FMemManagerHandle:=DynLibs.LoadLibrary(sLib);
    end;
    if FMemManagerHandle<>0 then begin
      Pointer(FMMM_Initialize):=DynLibs.GetProcAddress(FMemManagerHandle,'Startup');
      Pointer(FMMM_Finalize):=DynLibs.GetProcAddress(FMemManagerHandle,'Shutdown');
      if Assigned(FMMM_Initialize) then
        FMMM_Initialize(FMMM_Info);
      Label1.Caption:=Concat('Thread ID : ',IntToStr(FMMM_Info.ThreadID));
      Label2.Caption:=Concat('Manifest ID: ',IntToStr(FMMM_Info.ManifestID));
      Button4.Caption:='UnLoad';
    end;
  end else begin
    if Assigned(FMMM_Finalize) then begin
      if FMMM_Finalize(FMMM_Info) then begin
        FMMM_Finalize:=nil;
        FMMM_Initialize:=nil;
        DynLibs.UnLoadLibrary(FMemManagerHandle);
        FMemManagerHandle:=0;
        Button4.Caption:='Load';
        Empty(FMMM_Info);
        Label1.Caption:=Concat('Thread ID : ',IntToStr(FMMM_Info.ThreadID));
        Label2.Caption:=Concat('Manifest ID: ',IntToStr(FMMM_Info.ManifestID));
      end;
    end;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if FLibHandle<>0 then begin
    FMyTest:=nil;
    DynLibs.UnloadLibrary(FLibHandle);
  end;
end;

initialization
  {$I Unit1.lrs}

end.


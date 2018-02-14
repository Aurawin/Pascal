program cacheTest;

{$mode objfpc}{$H+}

uses
  cthreads,cmem,
  Interfaces, // this includes the LCL widgetset
  Forms, uMain
  { you can add units after this };

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

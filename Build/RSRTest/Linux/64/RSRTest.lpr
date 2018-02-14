program RSRTest;

{$mode objfpc}{$H+}

uses
  cthreads,
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, hRSR, uDNS, uFiFoQueue, uSocketUtils
  { you can add units after this };

{$R RSRTest.res}

begin
  Application.Title:='RSR Tester Application';
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

program RSRTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, hRSR, uDNS, uFiFoQueue, uSocketUtils, Unit1
  { you can add units after this };

{$R RSRTest.res}

begin
  Application.Initialize;
  Application.Run;
end.


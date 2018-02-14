Program Service;

Uses
{$IFDEF UNIX}{$IFDEF UseCThreads}
  CThreads,
{$ENDIF}{$ENDIF}
  DaemonApp, lazdaemonapp, uDaemon, uService;

{$R *.res}

begin
  Application.Initialize;
  Application.Title:='Aurawin SCS';
  Application.Run;
end.

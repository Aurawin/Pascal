program AuConsole;
uses
  {$IFDEF UNIX}
    CThreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Form.DBException,
  Form.YesNo,

  App.Build;

  {$R *.res}

begin
  App.Build.Title:='AuConsole';
  App.Build.Caption:='Aurawin SCS Console';
  DefaultSystemCodePage:=CP_UTF8;
  Application.Initialize;
  Application.Run;
end.


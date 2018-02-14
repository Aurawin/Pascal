program Console;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmConsole, LResources, SQLDBLaz, frmLogin, frmException, uStorage,
  hRSR, frmYesNo, hPOP3, hReceive, hHTTP, hDNS, hLDAP, hIMAP, hBMail,
  uListViewUtils, uStreams, uFiFoQueue;

{$IFDEF WINDOWS}{$R Console.rc}{$ENDIF}

begin
  Application.Title:='SCS Console';
  {$I Console.lrs}
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.CreateForm(TExceptionForm, ExceptionForm);
  Application.Run;
end.


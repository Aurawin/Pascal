program AuConsole;
uses
  {$IFDEF UNIX}
    CThreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmLogin, frmConsole, frmServices, frmImage, frmCertReq, frmRSA, hRSR;
  {$R *.res}

begin
  Application.Title:='SCS Console';
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.CreateForm(TRSAGen, RSAGen);
  Application.CreateForm(TCertReqForm, CertReqForm);
  Application.Run;

end.


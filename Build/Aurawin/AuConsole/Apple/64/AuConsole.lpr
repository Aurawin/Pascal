program AuConsole;
uses
  {$IFDEF UNIX}
    CThreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  hRSR,
  uAppBuild,
  hSRConsts,
  frmLogin, frmConsole, frmServices, frmImage, frmCertReq, frmRSA;
  {$R *.res}

begin
  Application.Title:='SCS Console';
  DefaultSystemCodePage:=CP_UTF8;
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.CreateForm(TRSAGen, RSAGen);
  Application.CreateForm(TCertReqForm, CertReqForm);
  Application.Run;
end.


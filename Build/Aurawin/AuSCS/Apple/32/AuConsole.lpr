program AuConsole;
uses
  CThreads,
  Interfaces, // this includes the LCL widgetset
  Forms, frmServices, frmConsole, hRSR, frmDBException, frmException,
  frmYesNo,

  hSRConsts, frmLogin, frmFileMan, frmImage, frmEdit, frmKeyword, frmProvider,
  frmRSA, frmCertReq

  ;

{$R *.res}

begin
  Application.Title:='Aurawin Console';
  Application.Initialize;
  Application.CreateForm(TLoginForm, LoginForm);
  Application.CreateForm(TConsoleForm, ConsoleForm);
  Application.CreateForm(TRSAGen, RSAGen);
  Application.CreateForm(TCertReqForm, CertReqForm);
  Application.Run;
end.

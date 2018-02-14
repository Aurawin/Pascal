program AuCloud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, auLang, auSettings, frmMain, frmResources, hSRConsts, uEngine;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  DefaultSystemCodePage:=CP_UTF8;
  Application.Initialize;
  frmMain.AuSockets:=uEngine.TAuSocketMan.Create();
  Application.CreateForm(TAurawin, Aurawin);
  Application.CreateForm(TfrmResource, frmResource);
  Application.Run;
end.


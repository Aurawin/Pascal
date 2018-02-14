program AuCloud;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,CWString,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, App.Build, auLang, auSettings, frmMain,
  frmResources;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  DefaultSystemCodePage:=CP_UTF8;

  App.Build.Caption:='Aurawin Cloud';
  App.Build.Title:='AuCloud';

  Application.Initialize;


  Application.CreateForm(TAurawin, Aurawin);
  Application.CreateForm(TfrmResource, frmResource);
  Application.Run;
end.


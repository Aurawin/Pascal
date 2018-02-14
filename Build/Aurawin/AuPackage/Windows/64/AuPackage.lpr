program AuPackage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmMain, frmLicense, uAppSettings;

{$R *.res}

begin

  RequireDerivedFormResource := True;
  Application.Initialize;
  uAppSettings.Package.Init();
  uAppSettings.Package.Resources.Init();
  uAppSettings.Manifest.Init();


  Application.CreateForm(TPackageForm, PackageForm);
  Application.CreateForm(TLicenseForm, LicenseForm);
  Application.Run;
end.


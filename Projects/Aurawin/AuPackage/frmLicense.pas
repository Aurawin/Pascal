unit frmLicense;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uAppSettings;

type

  { TLicenseForm }

  TLicenseForm = class(TForm)
    btnAccept: TButton;
    btnCancel: TButton;
    imgLicense: TImage;
    lblDescription: TLabel;
    lblTitle: TLabel;
    pnlImage: TPanel;
    pnlButtons: TPanel;
    pnlText: TPanel;
    pnlRight: TPanel;
    pnlHeader: TPanel;
    txtLicense: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LicenseForm: TLicenseForm;

implementation

{$R *.lfm}

{ TLicenseForm }

procedure TLicenseForm.FormCreate(Sender: TObject);
var
  rcP:Package.Resources.PResource;
  rcsP:Package.Resources.PResources;
begin
  rcsP:=Package.Resources.ResourcesP;
  rcP:=Package.Resources.Get(rcsP^,Package.Resources.License);
  if (rcP<>nil) then begin
    rcP^.Stream.Position:=0;
    txtLicense.Lines.LoadFromStream(rcp^.Stream);
    rcP^.Stream.Position:=0;
  end;
  rcP:=Package.Resources.Get(rcsP^,Package.Resources.Image);
  if (rcP<>nil) then begin
    rcP^.Stream.Position:=0;
    imgLicense.Picture.LoadFromStream(rcp^.Stream);
    rcP^.Stream.Position:=0;
  end;

  pnlButtons.BorderSpacing.Around:=GUI.BUTTON_BOX_A;
  pnlButtons.ChildSizing.LeftRightSpacing:=GUI.BUTTON_BOX_LRS;
  pnlButtons.ChildSizing.TopBottomSpacing:=GUI.BUTTON_BOX_TBS;
  txtLicense.BorderSpacing.Around:=GUI.TEXT_A;
  pnlText.BorderSpacing.Around:=GUI.PANEL_A;
  pnlHeader.BorderSpacing.Around:=GUI.PANEL_A;

  lblTitle.Caption:=Lang.License.Title;
  lblDescription.Caption:=Lang.License.Description;


end;

end.


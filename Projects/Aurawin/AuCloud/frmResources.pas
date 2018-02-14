unit frmResources;

interface

uses
  Classes,
  SysUtils,
  FileUtil,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  ComCtrls,
  Buttons,
  auSettings,
  auLang,
  App,
  App.Consts;

type

  { TfrmResource }
  TResourceMode=(rmList,rmNew,rmEdit);
  TResourceModes=set of TResourceMode;
  TfrmResource = class(TForm)
    btnAdd: TBitBtn;
    btnEditSave: TBitBtn;
    btnSelect: TBitBtn;
    cbRememberCookies: TCheckBox;
    cbEditRememberCookies: TCheckBox;
    Image1: TImage;
    lblEditDescription: TLabel;
    lblEditName: TLabel;
    lblSyncOptions: TLabel;
    lblSyncOptions1: TLabel;
    lbSplash: TLabel;
    lblName: TLabel;
    lblDescription: TLabel;
    lbSelection: TLabel;
    lvResources: TListView;
    pcSelection: TPageControl;
    pnlEditDirection: TPanel;
    pnlEditOptions: TPanel;
    pnlAddSave: TPanel;
    pnlEditSave: TPanel;
    pnlEditResourceDescription: TPanel;
    pnlEditResourceName: TPanel;
    pnlAvailSelect: TPanel;
    pnlAddDirection: TPanel;
    pnlWelcome: TPanel;
    pnlAddOptions: TPanel;
    pnlResourceName: TPanel;
    pnlResourceDescription: TPanel;
    pnlAvailWelcome: TPanel;
    tsEdit: TTabSheet;
    tgEditDownload: TToggleBox;
    tgUpload: TToggleBox;
    tgDownload: TToggleBox;
    tgEditUpload: TToggleBox;
    tsNew: TTabSheet;
    tsList: TTabSheet;
    txtEditDescription: TEdit;
    txtName: TEdit;
    txtDescription: TEdit;
    txtEditName: TEdit;
    procedure btnAddClick(Sender: TObject);
    procedure btnEditSaveClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtNameKeyPress(Sender: TObject; var Key: char);
  private
    FLoading : boolean;
    FModes   : TResourceModes;
  private
    procedure ClearValues();
    procedure SetNew();
    procedure SetEdit();
    procedure RefreshModes();
  public
    procedure SetModes(Value:TResourceModes);
  end;

var
  frmResource: TfrmResource;

implementation

procedure TfrmResource.FormCreate(Sender: TObject);
begin
  cbRememberCookies.Caption:=auLang.Table.Options.SaveLoginInfo;
  cbEditRememberCookies.Caption:=auLang.Table.Options.SaveLoginInfo;
  lblName.Caption:=auLang.Table.Labels.Name;
  lblEditName.Caption:=auLang.Table.Labels.Name;
  lblDescription.Caption:=auLang.Table.Labels.Description;
  lblEditDescription.Caption:=auLang.Table.Labels.Description;
  lbSplash.Caption:=auLang.Table.Splash.ProvideResourceInfo;
  lblSyncOptions.Caption:=auLang.Table.Labels.Synchronize;
  lblSyncOptions1.Caption:=auLang.Table.Labels.Synchronize;
  tsList.Caption:=auLang.Table.Labels.Available;
  tsNew.Caption:=auLang.Table.Labels.AddNew;
  tsEdit.Caption:=auLang.Table.Labels.Edit;
  btnSelect.Caption:=auLang.Table.Labels.Select;
  tgUpload.Caption:=auLang.Table.Labels.Upload;
  tgDownload.Caption:=auLang.Table.Labels.Download;
  tgEditUpload.Caption:=auLang.Table.Labels.Upload;
  tgEditDownload.Caption:=auLang.Table.Labels.Download;
  pnlAvailWelcome.Height:=App.Consts.EntryHeight;
  pnlAvailSelect.Height:=App.Consts.ButtonsHeight;
  pnlResourceName.Height:=App.Consts.EntryHeight;
  pnlResourceDescription.Height:=App.Consts.EntryHeight;
  pnlAddDirection.Height:=App.Consts.EntryHeight;
  pnlAddOptions.Height:=App.Consts.EntryHeight;
  pnlAddSave.Height:=App.Consts.ButtonsHeight;

  pnlEditResourceName.Height:=App.Consts.EntryHeight;
  pnlEditResourceDescription.Height:=App.Consts.EntryHeight;
  pnlEditDirection.Height:=App.Consts.EntryHeight;
  pnlEditOptions.Height:=App.Consts.EntryHeight;
  pnlEditSave.Height:=App.Consts.ButtonsHeight;
  pnlWelcome.BorderSpacing.Top:=App.Consts.HeaderOffsetTop;
  pcSelection.BorderSpacing.Around:=App.Consts.PagesBorder;
end;

procedure TfrmResource.FormShow(Sender: TObject);
begin
  RefreshModes();
end;

procedure TfrmResource.txtNameKeyPress(Sender: TObject; var Key: char);
begin
  if Pos(Key,auLang.Table.Input.Invalid) >0 then
    Key:=#0;
end;

procedure TfrmResource.btnAddClick(Sender: TObject);
begin
  if (txtName.GetTextLen=0) or (txtDescription.GetTextLen=0) then exit;
  ModalResult:=mrOK;
end;

procedure TfrmResource.btnEditSaveClick(Sender: TObject);
begin
  if (txtEditName.GetTextLen=0) or (txtEditDescription.GetTextLen=0) then exit;
    ModalResult:=mrOK;
end;

procedure TfrmResource.btnSelectClick(Sender: TObject);
begin
  if lvResources.Selected=nil then exit;
  ModalResult:=auSettings.mrSelect;
end;

procedure TfrmResource.ClearValues();
begin
  FLoading:=True;
  txtName.Clear;
  txtEditName.Clear;
  txtDescription.Clear;
  txtEditDescription.Clear;
  cbRememberCookies.Checked:=false;
  cbEditRememberCookies.Checked:=false;
  tgUpload.Checked:=false;
  tgEditUpload.Checked:=false;
  tgDownload.Checked:=false;
  tgEditDownload.Checked:=false;
  FLoading:=False;
end;

procedure TfrmResource.SetModes(Value:TResourceModes);
begin
  FModes:=Value;
  tsList.TabVisible:=rmList in FModes;
  tsNew.TabVisible:=rmNew in FModes;
  tsEdit.TabVisible:=rmEdit in FModes;
  frmResource.ClearValues();
end;

procedure TfrmResource.RefreshModes();
begin
  tsList.TabVisible:=(rmList in FModes);
  tsNew.TabVisible:=(rmNew in FModes);
  tsEdit.TabVisible:=(rmEdit in FModes);
  if rmNew in FModes then
    pcSelection.ActivePage:=tsNew;
  if rmEdit in FModes then
    pcSelection.ActivePage:=tsEdit;
  if rmList in FModes then
    pcSelection.ActivePage:=tsList;
end;

procedure TfrmResource.SetNew();
begin
  ClearValues();
  FModes:=[rmNew];
  tsList.TabVisible:=false;
  tsNew.TabVisible:=true;
  tsEdit.TabVisible:=False;
  pcSelection.ActivePage:=tsNew;
end;

procedure TfrmResource.SetEdit();
begin
  ClearValues();
  FModes:=[rmList,rmEdit];
  tsList.TabVisible:=false;
  tsEdit.TabVisible:=True;
  tsNew.TabVisible:=false;
end;

initialization
  {$I frmResources.lrs}

end.


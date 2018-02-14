unit frmCertReq;

interface

uses
  Classes,
  process,
  FileUtil,
  SynHighlighterIni,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  StdCtrls,
  Buttons,
  ComCtrls,

  Core.Streams,
  Storage.Certs,

  SysUtils;

type
  { TCertReqForm }
  TCertMode=(cmGenerate,cmGenerating,cmViewRequest,cmDone);
  TCertReqForm = class(TForm)
    btnCancel: TBitBtn;
    btnGenerate: TBitBtn;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel10: TPanel;
    txtRequest: TMemo;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pcPages: TPageControl;
    procCertReq: TProcess;
    tsInfo: TTabSheet;
    tsRequest: TTabSheet;
    txtCountry: TEdit;
    txtDomain: TEdit;
    txtEmail: TEdit;
    txtName: TEdit;
    txtOrgName: TEdit;
    txtOrgUnit: TEdit;
    Image1: TImage;
    Panel1: TPanel;
    pnlButtons: TGroupBox;
    pnlLogo: TPanel;
    txtPassword: TEdit;
    txtState: TEdit;
    txtTown: TEdit;
    procedure btnGenerateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtCountryChange(Sender: TObject);
    procedure txtOrgNameChange(Sender: TObject);
  private
    { private declarations }
    FDomain:String;
    FFolder:String;
    FMode:TCertMode;
  private
    function isFormValid:Boolean;
  public
    procedure SetMode(Value:TCertMode);
    procedure SetInfo(Folder,Domain:String);
    function Execute(Folder,Domain:String):TModalResult;
  end;
Const
  BTN_CAPTION:Array[TCertMode] of String=('Generate','Generating','Done','Done');

var
  CertReqForm: TCertReqForm;

implementation


function  TCertReqForm.isFormValid:Boolean;
begin
  Result:=(
    (txtCountry.GetTextLen>0) and
    (txtState.GetTextLen>0) and
    (txtOrgName.GetTextLen>0) and
    (txtOrgUnit.GetTextLen>0) and
    (txtDomain.GetTextLen>0) and
    (txtEmail.GetTextLen>0) and
    (txtPassword.GetTextLen>3)
  );
end;

procedure TCertReqForm.txtCountryChange(Sender: TObject);
begin
  btnGenerate.Enabled:=(
    (txtOrgName.GetTextLen>0) and
    (txtOrgUnit.GetTextLen>0) and
    (txtTown.GetTextLen>0) and
    (txtState.GetTextLen>0) and
    (txtCountry.GetTextLen>0) and
    (txtDomain.GetTextLen>0) and
    (txtEmail.GetTextLen>0)
  );
end;

procedure TCertReqForm.txtOrgNameChange(Sender: TObject);
begin

end;

procedure TCertReqForm.btnGenerateClick(Sender: TObject);
begin
  if (FMode=cmDone) or (FMode=cmViewRequest) then begin
    ModalResult:=mrOK;
    exit;
  end;

  SetMode(cmGenerating);

  procCertReq.Executable:='openssl';
  procCertReq.Parameters.Clear();

  procCertReq.Parameters.Add('req');
  procCertReq.Parameters.Add('-new');
  procCertReq.Parameters.Add('-key');
  procCertReq.Parameters.Add(Storage.Certs.getKeyFile(FFolder,FDomain));
  procCertReq.Parameters.Add('-out');
  procCertReq.Parameters.Add(Storage.Certs.getRequestFile(FFolder,FDomain));

  (*
  procCertReq.CommandLine:=Concat(
      'openssl req -new -key ',
      dbmCerts.getKeyFile(FFolder,FDomain),
      ' -out ',
      dbmCerts.getRequestFile(FFolder,FDomain)
  );
  *)
  procCertReq.Execute();

  if procCertReq.Running then
    Core.Streams.EnterLine(txtCountry.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtState.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtTown.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtOrgName.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtOrgUnit.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtDomain.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtEmail.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtPassword.Text,procCertReq.Input);
  if procCertReq.Running then
    Core.Streams.EnterLine(txtName.Text,procCertReq.Input);

  while procCertReq.Running do
    Application.ProcessMessages;

  SetMode(cmDone);

  txtRequest.Lines.LoadFromFile(Storage.Certs.GetRequestFile(FFolder,FDomain,NO_QUOTES));
  pcPages.ActivePage:=tsRequest;
end;

procedure TCertReqForm.FormCreate(Sender: TObject);
begin
  {$ifdef Windows}
    btnCancel.BorderSpacing.Top:=0;
    btnGenerate.BorderSpacing.Top:=0;
    txtOrgName.BorderSpacing.Top:=4;
    txtOrgName.BorderSpacing.Bottom:=4;
    txtOrgUnit.BorderSpacing.Top:=4;
    txtOrgUnit.BorderSpacing.Bottom:=4;
    txtTown.BorderSpacing.Top:=4;
    txtTown.BorderSpacing.Bottom:=4;
    txtState.BorderSpacing.Top:=4;
    txtState.BorderSpacing.Bottom:=4;
    txtCountry.BorderSpacing.Top:=4;
    txtCountry.BorderSpacing.Bottom:=4;
    txtDomain.BorderSpacing.Top:=4;
    txtDomain.BorderSpacing.Bottom:=4;
    txtEmail.BorderSpacing.Top:=4;
    txtEmail.BorderSpacing.Bottom:=4;
    txtPassword.BorderSpacing.Top:=4;
    txtPassword.BorderSpacing.Bottom:=4;
    txtName.BorderSpacing.Top:=4;
    txtName.BorderSpacing.Bottom:=4;
  {$endif}
end;

procedure TCertReqForm.FormShow(Sender: TObject);
begin
  btnCancel.Visible:=(FMode<>cmViewRequest);
  tsInfo.TabVisible:=(FMode<>cmViewRequest);
  tsInfo.Visible:=(FMode<>cmViewRequest);
  Application.ProcessMessages;
  if FMode=cmViewRequest then begin
    pcPages.ActivePageIndex:=-1;
    pcPages.ActivePageIndex:=tsRequest.PageIndex;
  end else begin
    pcPages.ActivePageIndex:=-1;
    pcPages.ActivePageIndex:=tsInfo.PageIndex;
  end;
end;

procedure TCertReqForm.SetInfo(Folder,Domain:String);
begin
  FDomain:=Domain;
  FFolder:=Folder;
end;


procedure TCertReqForm.SetMode(Value:TCertMode);
begin
  FMode:=Value;
  btnGenerate.Enabled:=( ( (FMode=cmGenerate) and isFormValid() ) or (FMode=cmViewRequest) or (FMode=cmDone)) ;
  btnGenerate.Caption:=BTN_CAPTION[FMode];
  txtCountry.Enabled:=(FMode=cmGenerate);
  txtState.Enabled:=(FMode=cmGenerate);
  txtTown.Enabled:=(FMode=cmGenerate);
  txtOrgName.Enabled:=(FMode=cmGenerate);
  txtOrgUnit.Enabled:=(FMode=cmGenerate);
  txtDomain.Enabled:=(FMode=cmGenerate);
  txtEmail.Enabled:=(FMode=cmGenerate);
  txtPassword.Enabled:=(FMode=cmGenerate);
  txtName.Enabled:=(FMode=cmGenerate);
end;

function TCertReqForm.Execute(Folder,Domain:String):TModalResult;
begin
  SetMode(cmGenerate);
  FDomain:=Domain;
  FFolder:=Folder;
  txtRequest.Lines.Clear;
  pcPages.ActivePage:=tsInfo;

  Result:=ShowModal;
end;

initialization
  {$I frmCertReq.lrs}

end.


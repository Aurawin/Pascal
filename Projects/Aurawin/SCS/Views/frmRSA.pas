unit frmRSA;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ExtCtrls, RSR,OpenSSL, process,Storage.Certs;

type

  { TRSAGen }

  TRSAGen = class(TForm)
    btnCancel: TBitBtn;
    btnGenerate: TBitBtn;
    cbLevel: TComboBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    pnlButtons: TGroupBox;
    pnlLogo: TPanel;
    procGen: TProcess;
    procedure btnGenerateClick(Sender: TObject);
    procedure cbLevelChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FDomain:String;
    FFolder:String;
  public
    { public declarations }
    procedure SetInfo(Folder,Domain:String);
    procedure ImportKey();
    function Execute(Folder,Domain:String):TModalResult;
  end; 

var
  RSAGen: TRSAGen;

implementation

{ TRSAGen }

procedure TRSAGen.SetInfo(Folder,Domain:String);
begin
  FDomain:=Domain;
  FFolder:=Folder;
end;

function TRSAGen.Execute(Folder,Domain:String):TModalResult;
begin
  FDomain:=Domain;
  FFolder:=Folder;
  Result:=ShowModal();
end;

procedure TRSAGen.ImportKey();
var
  sKeyFile   : String;
  sDerKeyFile: String;
begin
  sKeyFile:=Storage.Certs.getKeyFile(FFolder,FDomain);
  sDerKeyFile:=Storage.Certs.getKeyFileAsDer(FFolder,FDomain);

  procGen.Executable:='openssl';
  procGen.Parameters.Clear();

  procGen.Parameters.Add('rsa');
  procGen.Parameters.Add('-in');
  procGen.Parameters.Add(sKeyFile);
  procGen.Parameters.Add('-inform');
  procGen.Parameters.Add('pem');
  procGen.Parameters.Add('-out');
  procGen.Parameters.Add(sDerKeyFile);
  procGen.Parameters.Add('-outform');
  procGen.Parameters.Add('der');

  procGen.Execute();
end;

procedure TRSAGen.btnGenerateClick(Sender: TObject);
var
  iValue : Word;
  sKeyFile   : String;
  sDerKeyFile: String;
begin
  iValue:=0;
  case cbLevel.ItemIndex of
     0: iValue:=512;
     1: iValue:=1024;
     2: iValue:=2048;
     3: iValue:=4096;
  end;
  if iValue<>0 then begin
    // Create New Key
    sKeyFile:=Storage.Certs.getKeyFile(FFolder,FDomain);
    sDerKeyFile:=Storage.Certs.getKeyFileAsDer(FFolder,FDomain);
    procGen.Executable:='openssl';
    procGen.Parameters.Clear();

    procGen.Parameters.Add('genrsa');
    procGen.Parameters.Add('-out');
    procGen.Parameters.Add(sKeyFile);
    procGen.Parameters.Add(IntToStr(iValue));

    procGen.Execute();

    procGen.Parameters.Clear();
    procGen.Parameters.Add('rsa');
    procGen.Parameters.Add('-in');
    procGen.Parameters.Add(sKeyFile);
    procGen.Parameters.Add('-inform');
    procGen.Parameters.Add('pem');
    procGen.Parameters.Add('-out');
    procGen.Parameters.Add(sDerKeyFile);
    procGen.Parameters.Add('-outform');
    procGen.Parameters.Add('der');

    procGen.Execute();


    ModalResult:=mrOk

  end;
end;

procedure TRSAGen.cbLevelChange(Sender: TObject);
begin
  btnGenerate.Enabled:=SysUtils.DirectoryExists(FFolder) and (cbLevel.ItemIndex<>-1);
end;

procedure TRSAGen.FormCreate(Sender: TObject);
begin
  {$ifdef Windows}
  btnCancel.BorderSpacing.Top:=0;
  btnGenerate.BorderSpacing.Top:=0;
  {$endif}
end;

initialization
  {$I frmRSA.lrs}

end.


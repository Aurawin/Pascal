unit Form.Exception;

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TExceptionForm }

  TExceptionForm = class(TForm)
    btnLogin: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    lbModule: TLabel;
    lbLocation: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    txtMessage: TMemo;
    pnlButtons: TPanel;
    pnlBody: TPanel;
    pnlTop: TPanel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ExceptionForm: TExceptionForm;

  procedure ShowException(sModule,sLocation,sMessage:String);

implementation

procedure ShowException(sModule,sLocation,sMessage:string);
begin
  if ExceptionForm=nil then
    ExceptionForm:=TExceptionForm.Create(nil);
  ExceptionForm.lbModule.Caption:=sModule;
  ExceptionForm.lbLocation.Caption:=sLocation;
  ExceptionForm.txtMessage.Lines.Text:=sMessage;
  ExceptionForm.ShowModal;
end;

{ TExceptionForm }

procedure TExceptionForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

initialization
  {$I Form.Exception.lrs}


end.


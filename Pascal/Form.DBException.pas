unit Form.DBException;


interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TDBExceptionForm }

  TDBExceptionForm = class(TForm)
    btnLogin: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lbTable: TLabel;
    lbTask: TLabel;
    lbModule: TLabel;
    lbLocation: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
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
  DBExceptionForm : TDBExceptionForm;

  procedure ShowException(sModule,sLocation,sTable,sTask,sMessage:String);

implementation

procedure ShowException(sModule,sLocation,sTable,sTask,sMessage:string);
begin
  if DBExceptionForm=nil then
    DBExceptionForm:=TDBExceptionForm.Create(nil);
  DBExceptionForm.lbModule.Caption:=sModule;
  DBExceptionForm.lbLocation.Caption:=sLocation;
  DBExceptionForm.lbTable.Caption:=sTable;
  DBExceptionForm.lbTask.Caption:=sTask;
  DBExceptionForm.txtMessage.Lines.Text:=sMessage;
  DBExceptionForm.ShowModal;
end;

{ TDBExceptionForm }

procedure TDBExceptionForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caHide;
end;

initialization
  {$I Form.DBException.lrs}


end.


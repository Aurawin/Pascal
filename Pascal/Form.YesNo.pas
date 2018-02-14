unit Form.YesNo;

interface

uses
  Classes, SysUtils, FileUtil, LResources, Interfaces, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls;

type

  { TYesNoForm }
  TYesNoOption=(ynoYes,ynoNo,ynoNoToAll);
  TYesNoOptions=set of TYesNoOption;
Const
  Standard=[ynoYes,ynoNo];
  YesNoAndNoToAll:TYesNoOptions=[ynoYes,ynoNo,ynoNoToAll];

type
  TYesNoForm = class(TForm)
    btnNoToAll: TBitBtn;
    btnNo: TBitBtn;
    btnYes: TBitBtn;
    Image1: TImage;
    lbTitle: TLabel;
    txtBody: TMemo;
    Panel1: TPanel;
    pnlBody: TPanel;
    pnlButtons: TPanel;
    pnlTitle: TPanel;
  private
    { private declarations }
  public
    { public declarations }
  end;

  function ShowMessage(sCaption,sTitle,sMessage:string; Const Options:TYesNoOptions=Standard):TModalResult; overload;

var
  YesNoForm: TYesNoForm;

implementation

{ TYesNoForm }

function ShowMessage(sCaption,sTitle,sMessage:string; Const Options:TYesNoOptions=Standard):TModalResult;
begin
  if YesNoForm=nil then
    YesNoForm:=TYesNoForm.Create(Nil);
  YesNoForm.Caption:=sCaption;
  YesNoForm.lbTitle.Caption:=sTitle;
  YesNoForm.txtBody.Lines.Text:=sMessage;
  YesNoForm.btnNoToAll.Visible:=(ynoNoToAll in Options);
  YesNoForm.btnNo.Visible:=(ynoNo in Options);
  YesNoForm.btnYes.Visible:=(ynoYes in Options);
  Result:=YesNoForm.ShowModal;
end;
initialization
  {$I Form.YesNo.lrs}

end.


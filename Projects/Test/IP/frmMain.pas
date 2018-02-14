unit frmMain;

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RSR,Core.Utils.Sockets;

type

  { TForm1 }

  TForm1 = class(TForm)
    txtAddr: TEdit;
    txtIP: TEdit;
    procedure txtAddrChange(Sender: TObject);
    procedure txtIPChange(Sender: TObject);
  private
    { private declarations }
    FChanging:boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.txtIPChange(Sender: TObject);
begin
  if FChanging then exit;
  FChanging:=true;
  txtAddr.Text:=IntToStr(Core.Utils.Sockets.InAddrFromStr(txtIP.Text));
  FChanging:=false;
end;

procedure TForm1.txtAddrChange(Sender: TObject);
begin
  if FChanging then exit;
  FChanging:=true;
  txtIP.Text:=Core.Utils.Sockets.InAddrToStr(StrToIntDef(txtAddr.Text,0));
  FChanging:=false;
end;

end.


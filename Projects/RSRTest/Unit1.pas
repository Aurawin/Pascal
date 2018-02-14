unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, Sockets, RSR, Core.Utils.Sockets;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Memo1: TMemo;
    Memo2: TMemo;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    FSocket:TSocket;
    FAddress:TSockAddr;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSocket:=Sockets.fpsocket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
  FAddress.Sin_addr.s_Addr:=Core.Utils.Sockets.InAddrFromStr(Edit1.Text);
  FAddress.Sin_family:=AF_INET;
  FAddress.Sin_port:=Sockets.htons(StrToIntDef(Edit2.Text,0));
end;

procedure TForm1.Memo1Change(Sender: TObject);
begin
  Button1.Enabled:=CheckBox1.Checked and (Length(Memo1.Text)>0);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  iWaiting:Cardinal;
  iReceived:Int64;
  sBuffer:String;
begin
  iWaiting:=Core.Utils.Sockets.DataSizeWaiting(FSocket);
  if iWaiting>0 then begin
    SetLength(sBuffer,iWaiting);
    iReceived:=Sockets.fprecv(FSocket,@sBuffer[1],iWaiting,0);
    if iReceived>0 then begin
      SetLength(sBuffer,iReceived);
      Memo2.Append(sBuffer);
    end;
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  If CheckBox1.Checked then begin
    Sockets.fpConnect(FSocket,@FAddress,SizeOf(TSockAddr));
    Timer1.Enabled:=True;
  end else begin
    Sockets.fpshutdown(FSocket,2);
    Sockets.CloseSocket(FSocket);
    Timer1.Enabled:=False;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  sText:String;
begin
  sText:=Memo1.Text;
  Sockets.fpsend(FSocket,@sText[1],Length(sText),0);
end;

initialization
  {$I Unit1.lrs}

end.

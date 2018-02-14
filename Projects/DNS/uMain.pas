unit uMain;


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, RSR, RSR.DNS, Core.Strings,Core.Utils.Sockets;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    cbQuery: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    txtServer: TEdit;
    txtQuery: TEdit;
    txtOutput: TMemo;
    Panel1: TPanel;
    txtBind: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    FServerIP:QWord;
    FBindIP:QWord;
    FQuery:Core.Strings.VarString;
  public
    { public declarations }
  end;
  TEngine=class(TRSRManager)
  protected
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR);override;
    procedure   OnConnect(RSRP:PRSR);override;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:Boolean);override;
    procedure   OnDNSResult(RSRP:PRSR);override;
    procedure   OnBufferChanged(RSRP:PRSR);override;
  protected
    procedure   OnInitialize(RSRP:PRSR);override;
    procedure   OnFinalize(RSRP:PRSR);override;
    procedure   OnDBException(sProcedure,sLocation,sTable,sTask,sError:Core.Strings.VarString);override;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString);override;
  end;

var
  Form1: TForm1;
  Engine:TEngine;
  RSRP:PRSR;

implementation
{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Engine:=TEngine.Create(nil,nil,false,false,1024*64);
  RSRP:=Engine.Allocate(rsrClient,rsrsTCP);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FServerIP:=Core.Utils.Sockets.InAddrFromStr(txtServer.Text);
  FBindIP:=Core.Utils.Sockets.InAddrFromStr(txtBind.Text);
  FQuery:=txtQuery.Text;
  case cbQuery.ItemIndex of
   0:  Engine.DNSLookup(RSRP,FQuery,[dnsIP],FServerIP,FBindIP);
   1:  Engine.DNSLookup(RSRP,FQuery,[dnsMX],FServerIP,FBindIP);
  end;
  Form1.txtOutput.Lines.Add(Concat('Server IP: ',txtServer.Text));
  Form1.txtOutput.Lines.Add(Concat('Bind IP: ',txtBind.Text));
  Form1.txtOutput.Lines.Add(Concat('Query: ',FQuery));
end;


procedure TEngine.OnQueue(RSRP:PRSR);
begin
  Form1.txtOutput.Lines.Add(Concat('Socket queued: ',IntToStr(RSRP^.Info.Socket)));
end;

procedure TEngine.OnError(RSRP:PRSR);
begin

end;

procedure TEngine.OnDisconnect(RSRP:PRSR);
begin

end;

procedure TEngine.OnConnect(RSRP:PRSR);
begin

end;

procedure TEngine.OnDataReceived(RSRP:PRSR; var Handled:Boolean);
begin

end;

procedure TEngine.OnDNSResult(RSRP:PRSR);
var
  iLcv:integer;
begin
  for iLcv:=0 to High(Engine.DNS.Answers) do begin
    Form1.txtOutput.Lines.Add(Engine.DNS.Answers[iLcv]);
  end;

end;

procedure TEngine.OnBufferChanged(RSRP:PRSR);
begin

end;

procedure TEngine.OnInitialize(RSRP:PRSR);
begin

end;

procedure TEngine.OnFinalize(RSRP:PRSR);
begin

end;

procedure TEngine.OnDBException(sProcedure,sLocation,sTable,sTask,sError:Core.Strings.VarString);
begin

end;

procedure TEngine.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin

end;



end.


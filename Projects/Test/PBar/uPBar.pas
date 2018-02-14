unit uPBar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
{$RANGECHECKS ON}
procedure TForm1.Button1Click(Sender: TObject);
const
  BUF_MAX=1024*1024;
  MAX_ITERATIONS = 3048;
  MAX_WORK=100;
var
  Buff:Array[0..BUF_MAX-1] of byte;
  iLcv:LongInt;
  FStream:TMemoryStream;
begin
  FStream:=TMemoryStream.Create();
  Try
    // Simulate a file or buffer with a video that is 1MB larger 2GB on a 64bit System
    ProgressBar1.Max:=BUF_MAX*MAX_ITERATIONS;
    for iLcv:=0 to MAX_ITERATIONS do begin
      FStream.Write(Buff[0],BUF_MAX);

      ProgressBar1.Position:=FStream.Position;
      if (iLcv mod MAX_WORK)=0 then
        Application.ProcessMessages();
    end;
    ProgressBar1.Position:=FStream.Size;
  finally
    FStream.Free();
  end;
end;

end.


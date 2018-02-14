unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, uMPEG;

  { TForm1 }
Type
  TForm1 = class(TForm)
    txtDebug: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Reader:uMPEG.TReader;
    FOutput:TMemoryStream;
    procedure OnMP3TagFrame(Main:TFrame; TagFrame:TFrame; Stream:TStream; var Handled:Boolean);
  public
    { public declarations }
  end;
var
  Form1: TForm1; 

implementation
uses Core.Streams;
{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  FS:TFileStream;
  sData:String;
begin
  txtDebug.Lines.Clear;

  Reader:=uMPEG.TReader.Create;
  Reader.OnTagFrame:=@OnMP3TagFrame;
  //FS:=TFileStream.Create('/home/atbrunner/S.mp3',fmOpenRead); // ID3 3.0
  //FS:=TFileStream.Create('/home/atbrunner/H.mp3',fmOpenRead);  // ID3 2.0
  // FS:=TFileStream.Create('/home/atbrunner/15kz-064.mp2',fmOpenRead);
  //Reader.LoadAll(FS);

  sData:=Concat('Duration : ',IntToStr( trunc(Reader.Duration/60)),'m ' ,IntToStr(trunc(Reader.Duration/3600)),'s');
  txtDebug.Append(sData);
  sData:=Concat('Bit Rate : ',FloatToStr(Reader.BitRate));
  txtDebug.Append(sData);
  sData:=Concat('Frame Count : ',FloatToStr(Reader.FrameCount));
  txtDebug.Append(sData);
  sData:=Concat('Frame Aggregate : ',FloatToStr(Reader.FrameSizeTotal/1));
  txtDebug.Append(sData);
end;

procedure TForm1.OnMP3TagFrame(Main:TFrame; TagFrame:TFrame; Stream:TStream; var Handled:Boolean);
var
  sData:String;
begin
  Case TagFrame.Kind of
    fEncodedBy        : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fComments         : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PCommentPayload(TagFrame.Payload.Data)^.Description,'=',PCommentPayload(TagFrame.Payload.Data)^.Text);
    fCompilation      : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fYear             : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fPartOfaSet       : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fTrackNumber      : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fTitle            : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fComposer         : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fLeadArtist       : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
    fTitleDescription : sData:=Concat(uMPEG.FrameKind[TagFrame.Kind ],' : ',PTextPayload(TagFrame.Payload.Data)^.Information);
  end;
  txtDebug.Append(sData);
end;


end.


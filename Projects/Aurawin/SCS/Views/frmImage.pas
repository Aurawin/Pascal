unit frmImage;

interface

uses
  Classes,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  FPimage,


  App.Consts,
  Core.Strings,

  Storage,
  Storage.Main,
  Storage.FAT,
  Storage.ContentTypes,

  Core.Utils.Forms,

  FileUtil,
  SysUtils;

type

  { TImageViewerForm }

  TImageViewerForm = class(TForm)
    cbZoom: TComboBox;
    imgData: TImage;
    ProgressBar: TProgressBar;
    sbpnlHeight: TPanel;
    sbpnlWidth: TPanel;
    sbpnlStatus: TPanel;
    sbImage: TScrollBox;
    StatusBar: TPanel;
    procedure cbZoomSelect(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  private
    { private declarations }
    FFile:TDSFile;
    FHeight:integer;
    FWidth:integer;
    FFactor:Single;
    FInfoP:PFormInfo;
  private
    procedure OnImageProgress(Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean);
  public
    { public declarations }
    procedure Show(dsFile:TDSFile); overload;
  end; 

implementation
{ TImageViewerForm }

procedure TImageViewerForm.Show(dsFile:TDSFile);
var
  FS:TFileStream;
begin
  FFile:=dsFile;
  ProgressBar.Max:=100;
  ProgressBar.Min:=0;
  ProgressBar.Step:=1;
  imgData.Picture.OnProgress:=@OnImageProgress;
  Caption:=Format(FMT_IMAGE_CAPTION,[FFile.Name]);
  FInfoP:=Core.Utils.Forms.List.Load(Self,FFile,nil,nil);

  FFile.Load(Storage.Main.Task);
  FS:=FFile.AcquireData();
  Try
    FS.Position:=0;
    imgData.Picture.LoadFromStreamWithFileExt(FS,ExtractFileExt(FFile.Name));
  finally
    FS.Free();
  end;
  FWidth:=imgData.Picture.Width;
  FHeight:=imgData.Picture.Height;
  sbpnlHeight.Caption:=IntToStr(FHeight);
  sbpnlWidth.Caption:=IntToStr(FWidth);
  Inherited Show;
end;

procedure TImageViewerForm.OnImageProgress(Sender: TObject; Stage: TFPImgProgressStage;
                                   PercentDone: Byte; RedrawNow: Boolean; const R: TRect;
                                   const Msg: AnsiString; var Continue : Boolean);
begin
  sbpnlStatus.Caption:=Format(FMT_IMAGE_LOADING,[Msg]);
  ProgressBar.Position:=PercentDone;
  Application.ProcessMessages;
end;

procedure TImageViewerForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  Core.Utils.Forms.List.UnLoad(FInfoP^);
end;

procedure TImageViewerForm.cbZoomSelect(Sender: TObject);
var
  sFactor:String;

  iLength:integer;
begin
  sFactor:=cbZoom.Text;
  iLength:=Length(sFactor);
  dec(iLength);
  SetLength(sFactor,iLength);
  FFactor:=StrToIntDef(sFactor,100)/100;
  imgData.Align:=alNone;
  imgData.Stretch:=true;
  imgData.Width:=Trunc(FFactor*FWidth);
  imgData.Height:=Trunc(FFactor*FHeight);
end;

initialization
  {$I frmImage.lrs}

end.


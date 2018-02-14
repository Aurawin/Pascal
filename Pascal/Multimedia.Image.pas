unit Multimedia.Image;

interface

uses
  Multimedia.ImageMagic,
  Multimedia.ImageMagic.MagickWand,

  Classes,
  SysUtils,
  FPimage,
  FPImgCanv,
  FPReadPNG,
  FPWritePNG,
  FPReadJPEG,
  FPWriteJPEG,
  FPReadGIF,
  FPReadBMP,
  FPWriteBMP
  ;

type
  Tool=class
  type
    Kind=class
    type
      PNG=class
      const
        Name      : string = 'Portable Network Graphics';
        Ext       : string = 'png';
      end;
      JPG=class
      const
        Name      : string = 'JPEG Graphics';
        Ext       : string = 'jpg';
        Ext2      : string = 'jpeg';
      end;
      GIF=class
      const
        Name      : string = 'GIF Graphics';
        Ext       : string = 'gif';
      end;
      BMP=class
      const
        Name      : string = 'BMP Format';
        Ext       : string = 'bmp';
      end;
      class function fromString(sExt:string):string;
    end;
    class function Rotate(Stream:TMemoryStream; Angle:Double):boolean;
    class function Transform(Stream:TMemoryStream; var iX,iY:LongInt):boolean; overload;
    class function Transform(Stream:TStream; var sContentType,srcKind,dstKind:string; var iX,iY:LongInt):boolean; overload;

    class function Convert(Stream:TStream; var sContentType,srcKind,dstKind:string; var iX,iY:LongInt):boolean;
  end;

implementation
  uses RSR.HTTP;

class function Tool.Kind.fromString(sExt:string):string;
begin
  if SameText(sExt,PNG.Ext) then
    Result:=PNG.Name
  else if (SameText(sExt,JPG.Ext) or SameText(sExt,JPG.Ext2)) then
    Result:=JPG.Name
  else if SameText(sExt,GIF.Ext) then
    Result:=GIF.Name
  else if SameText(sExt,BMP.Ext) then
    Result:=BMP.Name
  else
    Result:='';
end;

class function Tool.Convert(Stream:TStream; var sContentType,srcKind,dstKind:string; var iX,iY:LongInt):boolean;
var
  FReader      : TFPCustomImageReader;
  FWriter      : TFPCustomImageWriter;
  cReader      : TFPCustomImageReaderClass;
  cWriter      : TFPCustomImageWriterClass;
  Factor       : Double;
  FTransparent : boolean;
  dstImg       : TFPMemoryImage;
  srcImg       : TFPMemoryImage;
  srcCanvas    : TFPImageCanvas;
  dstCanvas    : TFPImageCanvas;

  procedure FreeImageData;
  begin
    if (srcCanvas<>nil) then
      srcCanvas.Free();
    if (dstCanvas<>nil) then
      dstCanvas.Free();
    if (FReader<>nil) then
      FReader.Free();
    if FWriter<>nil then
      FWriter.Free();
    if srcImg<>nil then
      srcImg.Free();
    if dstImg<>nil then
      dstImg.Free();
  end;
begin
  Result:=false; srcCanvas:=nil; dstCanvas:=nil;
  FReader:=nil; FWriter:=nil; srcImg:=nil; dstImg:=nil;
  if SameText(srcKind,Tool.Kind.GIF.Name) then begin
    sContentType:=RSR.HTTP.ctPNG;
    dstKind:=Tool.Kind.PNG.Name;
    FTransparent:=true;
  end else if SameText(srcKind,Tool.Kind.PNG.Name) then begin
    dstKind:=Tool.Kind.PNG.Name;
    FTransparent:=true;
  end else begin
    sContentType:=RSR.HTTP.ctJPG;
    dstKind:=Tool.Kind.JPG.Name;
    FTransparent:=false;
  end;

  cReader:=FPImage.ImageHandlers.ImageReader[srcKind];
  cWriter:=FPImage.ImageHandlers.ImageWriter[dstKind];

  Result:=(cReader<>nil) and (cWriter<>nil) and (Stream.Size>0);
  if Result then begin
    Try
      FReader:=cReader.Create();
      FWriter:=cWriter.Create();

      if SameText(dstKind,Tool.Kind.PNG.Name) then begin
        TFPWriterPNG(FWriter).Indexed:=False;
        TFPWriterPNG(FWriter).UseAlpha:=true;
      end;

      srcImg:=TFPMemoryImage.Create(0,0);
      srcImg.UsePalette:=false;
      Stream.Position:=0;
      Try
        srcImg.LoadFromStream(Stream,FReader);
        Stream.Size:=0;

        if (srcImg.Width>=srcImg.Height) then begin
          Factor:=iX/srcImg.Width;
        end else begin
          Factor:=iY/srcImg.Height;
        end;
        iX:= Round(srcImg.Width * Factor);
        iY:= Round(srcImg.Height * Factor);

        if FTransparent then begin
          if SameText(srcKind,Tool.Kind.GIF.Name) then begin
            FTransparent:=TFPReaderGIF(FReader).Transparent;
            TFPWriterPNG(FWriter).UseAlpha:=true;
          end;
        end;

        dstImg:=TFPMemoryImage.Create(iX,iY);
        dstImg.UsePalette:=false;
        dstCanvas:=TFPImageCanvas.create(dstImg);
        dstCanvas.StretchDraw(0,0,iX,iY,srcImg);

        dstImg.SaveToStream(Stream,FWriter);

        Stream.Position:=0;
        Result:=true;
      Except
        Stream.Position:=0;
        Stream.Size:=0;
        Result:=False;
      end;
    Finally
      FreeImageData();
    end;
  end;
end;

class function Tool.Rotate(Stream:TMemoryStream; Angle:Double):boolean;
var
  status             : MagickBooleanType;
  wand               : PMagickWand;
  pwBackground       : PPixelWand;
  blob               : PByte;
  bSize              : QWord;
begin
  Result:=false;
  wand := NewMagickWand();
  Try
    Stream.Position := 0; bSize:=0;
    pwBackground := NewPixelWand();
    Try
      status := MagickReadImageBlob(wand, Stream.Memory, Stream.Size);
      if (status = MagickTrue) then begin
        GetImageFromMagickWand(wand);
        MagickRotateImage(wand,pwBackground,Angle);
        Stream.Position:=0;
        Stream.Size:=0;
        MagickResetIterator(wand);
        blob:=MagickGetImageBlob(wand,@bSize);
        Try
          Stream.Write(blob^,bSize);
        finally
          MagickRelinquishMemory(blob);
        end;
        Result:=true;
      end;
    finally
      DestroyPixelWand(pwBackground);
    end;
  finally
    DestroyMagickWand(wand);
  end;
end;


class function Tool.Transform(Stream:TMemoryStream; var iX,iY:LongInt):boolean;
var
  status             : MagickBooleanType;
  wand               : PMagickWand;
  srcWidth,srcHeight : Cardinal;
  dstWidth,dstHeight : Cardinal;
  Factor             : Double;
  //img                : Pimage;
  blob               : PByte;
  bSize              : QWord;
begin
  Result:=false;
  wand := NewMagickWand();
  Try
    Stream.Position := 0;

    status := MagickReadImageBlob(wand, Stream.Memory, Stream.Size);
    if (status = MagickTrue) then begin
      //img := GetImageFromMagickWand(wand);

      srcHeight := MagickGetImageHeight(wand);
      srcWidth := MagickGetImageWidth(wand);
      if (srcWidth>=srcHeight) then begin
        Factor:=iX/srcWidth;
      end else begin
        Factor:=iY/srcHeight;
      end;
      dstWidth:=Trunc(srcWidth*Factor);
      dstHeight:=Trunc(srcHeight*Factor);
      if (srcWidth>iX) or (srcHeight>iY) then begin
        MagickScaleImage(wand, dstWidth, dstHeight);
        Stream.Size:=0;
        MagickResetIterator(wand);

        blob:=MagickGetImageBlob(wand,@bSize);
        try
          Stream.Write(blob^,bSize);
        finally
          MagickRelinquishMemory(blob);
        end;
        Result:=true;
      end else begin
        Result:=true;
      end;
    end;
  finally
    DestroyMagickWand(wand);
  end;
end;

class function Tool.Transform(Stream:TStream; var sContentType,srcKind,dstKind:string; var iX,iY:LongInt):boolean;
var
  FReader      : TFPCustomImageReader;
  FWriter      : TFPCustomImageWriter;
  cReader      : TFPCustomImageReaderClass;
  cWriter      : TFPCustomImageWriterClass;
  Factor       : Double;
  FTransparent : boolean;
  dstImg       : TFPCompactImgRGBA8Bit;
  srcImg       : TFPCompactImgRGBA8Bit;
  srcCanvas    : TFPImageCanvas;
  dstCanvas    : TFPImageCanvas;

  procedure FreeImageData;
  begin
    FreeAndNil(srcCanvas);
    FreeAndNil(dstCanvas);
    FreeAndNil(FReader);
    FreeAndNil(FWriter);
    FreeAndNil(srcImg);
    FreeAndNil(dstImg);
  end;

begin
  Result:=false;
  if SameText(srcKind,Tool.Kind.GIF.Name) then begin
    sContentType:=RSR.HTTP.ctPNG;
    dstKind:=Tool.Kind.PNG.Name;
    FTransparent:=true;
  end else if SameText(srcKind,Tool.Kind.PNG.Name) then begin
    dstKind:=Tool.Kind.PNG.Name;
    FTransparent:=true;
  end else begin
    sContentType:=RSR.HTTP.ctJPG;
    dstKind:=Tool.Kind.JPG.Name;
    FTransparent:=false;
  end;
  cReader:=FPImage.ImageHandlers.ImageReader[srcKind];
  cWriter:=FPImage.ImageHandlers.ImageWriter[dstKind];
  dstCanvas:=nil; srcCanvas:=nil; FReader:=nil; FWriter:=nil; srcImg:=nil; dstImg:=nil;
  Result:=(cReader<>nil) and (cWriter<>nil);
  if Result then begin
    Try
      FReader:=cReader.Create();
      FWriter:=cWriter.Create();

      if SameText(dstKind,Tool.Kind.PNG.Name) then begin
        TFPWriterPNG(FWriter).Indexed:=false;
        TFPWriterPNG(FWriter).UseAlpha:=true;
      end;

      srcImg:=TFPCompactImgRGBA8Bit.Create(0,0);
      srcImg.UsePalette:=false;
      Stream.Position:=0;
      Try
        srcImg.LoadFromStream(Stream,FReader);
        Stream.Position:=0;

        if (srcImg.Width>=srcImg.Height) then begin
          Factor:=iX/srcImg.Width;
        end else begin
          Factor:=iY/srcImg.Height;
        end;

        if (srcImg.Width>iX) or (srcImg.Height>iY) then begin
          if FTransparent then begin
            if SameText(srcKind,Tool.Kind.GIF.Name) then begin
              FTransparent:=TFPReaderGIF(FReader).Transparent;
              TFPWriterPNG(FWriter).UseAlpha:=true;
            end;
          end;

          iX:= Round(srcImg.Width * Factor);
          iY:= Round(srcImg.Height * Factor);

          dstImg:=TFPCompactImgRGBA8Bit.Create(iX,iY);
          dstImg.UsePalette:=false;
          dstCanvas:=TFPImageCanvas.create(dstImg);
          dstCanvas.StretchDraw(0,0,iX,iY,srcImg);

          Stream.Size:=0;
          dstImg.SaveToStream(Stream,FWriter);
          Stream.Position:=0;
          Result:=true;
        end;
      Except
        Stream.Size:=0;
        Stream.Position:=0;
        Result:=false;
      end;
    Finally
      FreeImageData();
    end;
  end;
end;

initialization
  Multimedia.ImageMagic.Initialize();
finalization;
  Multimedia.ImageMagic.Finalize();
end.


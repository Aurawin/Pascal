

 (*
 function AdjustStream:Int64;
 var
   iDiff:Cardinal;
 begin
   iDiff:=FTagFrame.FLength-FTagFrame.FPayload.FLength;
   if iDiff>0 then
     Stream.Seek(iDiff,soFromCurrent);
   Result:=Stream.Position;
 end;

 procedure Load_RAW_Frames;
  var
    iPosition:int64;
    iStreamSize:int64;

    hdrVersion:MPEGVersion;
    hdrLayer:MPEGLayer;

    hdrCRC:boolean;
    hdrBitRate:byte;
    hdrFrequency:byte;
    hdrPadded:byte;
    hdrPrivacy:Boolean;

    hdrMode:byte;
    hdrmodeStereoIntensity:boolean;
    hdrmodeStereoMS:boolean;

    hdrFlags:Cardinal;

    hdrCopyRighted:boolean;
    hdrCopyOriginal:boolean;
    hdrEmphasis:byte;

    zBitRate:QWORD;
    IDX:Byte;
    CRC:Word;
    Header:cardinal;
    Duration:Double;
    Found:Boolean;

    function Advance_RAW():boolean;
    var
      Found    : Boolean;
      iRead    : Byte;
      iStop    : qword;
    begin
      iRead:=SizeOf(Header);
      iStop:=iStreamSize-iRead;
      repeat
        Stream.Read(Header,iRead);
        Found:=((Header or TMPEGFrame.ID_MASK)=Header);
        if not Found then
          iPosition:=Stream.Seek(-(iRead-1),soFromCurrent);
      until (Found) or (iPosition>=iStop);
      Result:=Found;
    end;

    procedure Push_MPEG1;
    begin
      case hdrLayer of
        TMPEGFrame.MPEGLayer.layerI : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.I][TMPEGFrame.Version.MPEGI][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGI][hdrFrequency];
          FFrameSize:= trunc((12 * FBitRate / FSampleHz + hdrPadded) * 4);

          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
        TMPEGFrame.MPEGLayer.layerII  : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.II][TMPEGFrame.Version.MPEGI][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGI][hdrFrequency];
          FFrameSize:= trunc(144 * FBitRate / FSampleHz + hdrPadded);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
        TMPEGFrame.MPEGLayer.layerIII : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.III][TMPEGFrame.Version.MPEGI][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGI][hdrFrequency];
          FFrameSize:= trunc(144 * FBitRate / FSampleHz + hdrPadded);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
      end;
    end;

    procedure Push_MPEG2;
    begin
      case hdrLayer of
        TMPEGFrame.MPEGLayer.layerI   : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.I][TMPEGFrame.Version.MPEGII][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGII][hdrFrequency];
          FFrameSize:= trunc((12 * FBitRate / FSampleHz + hdrPadded) * 4);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
        TMPEGFrame.MPEGLayer.layerII  : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.II][TMPEGFrame.Version.MPEGII][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGII][hdrFrequency];
          FFrameSize:= trunc(144 * FBitRate / FSampleHz + hdrPadded);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
        TMPEGFrame.MPEGLayer.layerIII : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.III][TMPEGFrame.Version.MPEGII][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGII][hdrFrequency];
          FFrameSize:= trunc(144 * FBitRate / FSampleHz + hdrPadded);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
      end;
    end;

    procedure Push_MPEG25;
    begin
      case hdrLayer of
        TMPEGFrame.MPEGLayer.layerI   : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.I][TMPEGFrame.Version.MPEGII_V][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGII_V][hdrFrequency];
          FFrameSize:= trunc((12 * FBitRate / FSampleHz + hdrPadded) * 4);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
        TMPEGFrame.MPEGLayer.layerII  : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.II][TMPEGFrame.Version.MPEGII_V][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGII_V][hdrFrequency];
          FFrameSize:= trunc(144 * FBitRate / FSampleHz + hdrPadded);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
        TMPEGFrame.MPEGLayer.layerIII : begin
          FBitRate:=TMPEGFrame.BitRates[TMPEGFrame.Layer.III][TMPEGFrame.Version.MPEGII_V][hdrBitRate];
          zBitRate+=FBitRate;
          FSampleHz:=TMPEGFrame.SampleRates[TMPEGFrame.Version.MPEGII_V][hdrFrequency];
          FFrameSize:= trunc(144 * FBitRate / FSampleHz + hdrPadded);
          FFrameCount+=1;
          FFrameSizeTotal+=FFrameSize;
          if hdrCRC then
            Stream.Read(CRC,2);
          iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
        end;
      end;
    end;

  begin
    iPosition:=Stream.Position;
    iStreamSize:=Stream.Size;
    Repeat
      Found:=Advance_RAW();
      if Found then begin

        hdrVersion:=TMPEGFrame.MPEGVersion( (Header and TMPEGFrame.VERSION_MASK) shr TMPEGFrame.VERSION_SHIFT);
        hdrLayer:=TMPEGFrame.MPEGLayer( (Header and TMPEGFrame.LAYER_MASK) shr TMPEGFrame.LAYER_SHIFT);
        hdrCRC:=( ((Header and TMPEGFrame.CRC_MASK) shr TMPEGFrame.CRC_SHIFT)=0);
        hdrBitRate:=(Header and TMPEGFrame.BITRATE_MASK)  shr TMPEGFrame.BITRATE_SHIFT;
        hdrFrequency:=(Header and TMPEGFrame.FREQUENCY_MASK) shr TMPEGFrame.FREQUENCY_SHIFT;
        hdrPadded:=(Header and TMPEGFrame.PADDED_MASK) shr TMPEGFrame.PADDED_SHIFT ;
        hdrPrivacy:=((Header and TMPEGFrame.PRIVACY_MASK) shr TMPEGFrame.PRIVACY_SHIFT=1);
        case hdrVersion of
          TMPEGFrame.MPEGVersion.MPEG1  : Push_MPEG1;
          TMPEGFrame.MPEGVersion.MPEG2  : Push_MPEG2;
          TMPEGFrame.MPEGVersion.MPEG25 : Push_MPEG25;
        end;
        (*
          FBitRate:=uMPEG.TMPEGFrame.BIT_RATE[IDX];
          zBitRate+=FBitRate;
          IDX:=Flags shr 2 and 3;
          if IDX<3 then begin
            FSampleHz:=uMPEG.TMPEGFrame.SAMPLING_FREQUENCY[IDX];

            FFrameSize:= 144 * FBitRate * 1000 div FSampleHz;
            FFrameSize+=(Flags shr 1 and 1);
            FFrameCount+=1;

            FFrameSizeTotal+=FFrameSize;


            iPosition:=Stream.Seek(FFrameSize,soFromCurrent);
          end;
        end;
        *)
      end;
    Until (Found=false) or (iPosition>=iStreamSize);
    if FFrameCount>0 then begin
      FBitRate:=trunc(zBitRate/FFrameCount);
      FDuration:=FFrameSizeTotal/(FBitRate*8);
    end;
  end;

 *)


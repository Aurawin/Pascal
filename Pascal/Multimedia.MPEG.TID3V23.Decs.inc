  TID3V23TagFrame=class;
  TID3V23TagExtension=record
    Size        : Array[0..3] of byte;
    Count       : byte;
    Flags       : byte; //%0bcd0000
  end;
  TID3V23TagFlagData=record
    Size        : byte;
    Data        : Array[0..254] of byte;
  end;
  TID3V23TagHeader=class(TFrameHeader)
  type
    TSizeRestriction=(sr128frames1MBtag,sr64frames128kbtag,sr32frames40kbtag,sr32frames4kbtag);
    TTextEncodingRestriction=(erNone,erStandards);
    TTextSizeRestriction=(tsrNoLimit,tsr1024maxChars,txr128maxChars,txr30Chars);
    TImageRestriction=(irNone,irPNGorJPEG);
    TImageSizeRestriction=(isrNone,isr256x256OrLess,isr64x64OrLess,isr64x64);
    TTextFrameEncoding=(teISO8859_1,teUTF16,teUTF16BE,teUTF8);
  private
    hdrExtended        : TID3V23TagFlagData;
    hdrCRC             : TID3V23TagFlagData;
    hdrUpdate          : TID3V23TagFlagData;
    hdrRestrictions    : TID3V23TagFlagData;
  private
  public
    procedure Reset(); override;
    function  Load(Stream:TStream):boolean; override;
    function  Frame:TID3V23TagFrame;
  end;

  TID3V23TagPayload=class(TFramePayload)
  public
    function  Header:TID3V23TagHeader;
    procedure Reset(); override;
    procedure Load(Stream:TStream; var Handled:boolean); override;
  end;
  TID3V23Extension=record
    Length           : Cardinal;
    CRC              : boolean;
    paddingSize      : Cardinal;
    crcData          : Array[0..3] of byte;
  end;

  TID3V23TagFrame=class(TFrame)
  private
    FExtension         : TID3V23Extension;
    FUnSynchronization : boolean;
    FExtended          : boolean;
  public
    function  Header:TID3V23TagHeader;
    procedure Reset(); override;
    function  Load(Stream:TStream; var Handled:Boolean):boolean; override;
  public
    constructor Create(aReader:TReader); reIntroduce;
  end;


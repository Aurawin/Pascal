  TID3V20TagHeader=class;
  TID3V20TagFrame=class(TFrame)
  private
    FUnSynchronized : boolean;
    FCompressed     : boolean;
  public
    function  Header:TID3V20TagHeader;
    procedure Reset(); override;
    function  Load(Stream:TStream; var Handled:Boolean):boolean; override;
    procedure PreLoad(Stream:TStream); override;
  public
    constructor Create(aReader:TReader); reIntroduce;
  public
    property UnSynchronized:boolean read FUnSynchronized;
    property Compressed:boolean read FCompressed;
  end;

  TID3V20TagHeader=class(TFrameHeader)
  private
    FID                      : Array[0..2] of char;
    FSize                    : Array[0..2] of byte;
  public
    function  Frame:TID3V20TagFrame;
    procedure Reset(); override;
    function  Load(Stream:TStream):boolean; override;
  end;


  TID3V20TagPayload=class(TFramePayload)
  public
    function  Header:TID3V20TagHeader;
    procedure Reset(); override;
    procedure Load(Stream:TStream; var Handled:boolean); override;
  end;


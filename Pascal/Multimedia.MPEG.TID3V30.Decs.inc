  TID3V30TagHeader=class;
  TID3V30TagFrame=class;
  TID3V30Data=record
    UnSynchronization : boolean;
    Extended          : boolean;
    Experimental      : boolean;
  end;

  TID3V30Extension=record
    Length           : Cardinal;
    CRC              : boolean;
    paddingSize      : Cardinal;
    crcData          : Array[0..3] of byte;
  end;

  TID3V30TagHeader=class(TFrameHeader)
  private
    FID               : array[0..3] of char;
    FSize             : array[0..3] of byte;
    FFlags            : Array[0..1] of byte;
    FEncryptionMethod : byte;
    FGroupID          : byte;
    FDecompressedSize : Cardinal;
    FTagAltered       : TFramePreservation;
    FFileAltered      : TFramePreservation;
    FReadOnly         : boolean;
    FCompressed       : boolean;
    FEncrypted        : boolean;
    FGrouping         : boolean;
  public
    procedure Reset(); override;
    function  Load(Stream:TStream):boolean; override;
    function  Frame:TID3V30TagFrame;
  end;

  TID3V30TagPayload=class(TFramePayload)
  private
    pldAENC             : TEncryptedAudioPayload;
    pldAPIC             : TAttachedPicturePayload;
    pldCOMM             : TCommentPayload;
    pldCOMR             : TCommercialPayload;
    pldENCR             : TEncryptedMethodPayload;
    pldEQUA             : TEqualizationPayload;
    pldETCO             : TEventTimingPayload;
    pldGEOB             : TEncapsulatedObjectPayload;
    pldGRID             : TGroupIdentificationRegistrationPayload;
    pldIPLS             : TInvolvedPeopleListPayload;
    pldLINK             : TLinkedInfoPayload;
    pldMCDI             : TMusicIdentifierPayload;
    pldMLLT             : TLocationPayload;
    pldOWNE             : TOwnershipPayload;
    pldPCNT             : TPlayCounterPayload;
    pldPOPM             : TPopularimeterPayload;
    pldPOSS             : TPositionSynchronizationPayload;
    pldPRIV             : TPrivatePayload;
    pldRBUF             : TBufferSizePayload;
    pldRVAD             : TRelativeVolumeAdjustmentPayload;
    pldRVRB             : TReverbPayload;
    pldSYLT             : TSynchedTranscriptPayload;
    pldSYTC             : TTempoPayload;
  public
    function  Header    : TID3V30TagHeader;
    procedure Reset(); override;
    procedure Load(Stream:TStream; var Handled:boolean); override;
  end;

  TID3V30TagFrame=class(TFrame)
  private
    FIDData            : TID3V30Data;
    FExtension         : TID3V30Extension;
  public
    function  Header : TID3V30TagHeader;
    procedure Reset(); override;
    procedure Preload(Stream:TStream); override;
    function  Load(Stream:TStream; var Handled:Boolean):boolean; override;
  public
    constructor Create(aReader:TReader); reIntroduce;
  end;


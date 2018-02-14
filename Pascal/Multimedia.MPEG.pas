{
 unit Multimedia.MPEG.pas

 Copyright Aurawin LLC 2003-2012
 Written by: Andrew Thomas Brunner

 This code is proected under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit Multimedia.MPEG;

interface

uses

  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Strings,
  Core.Streams,
  Core.Streams.Types,

  Classes,
  SysUtils;

type
  TFrameKind=(
    { WARNING DO NOT CHANGE ORDER, JUST APPEND TO THIS LIST }
    fNone,                                 // 0
    fID3,                                  // 1
    fID3V20,                               // 2
    fID3V23,                               // 3
    fID3V30,                               // 4
    fID3V40,                               // 5
    fRAW,                                  // 6
    fMPEG,                                 // 7
    fCommercial,                           // 8
    fCompilation,                          // 9
    fBufferSize,                           // 10
    fPlayCounter,                          // 11
    fComments,                             // 12
    fAudioEncryption,                      // 13
    fEncryptedMeta,                        // 14
    fEventTimingCodes,                     // 15
    fEqualization,                         // 16
    fGeneralEncapsulatedObject,            // 17
    fGroupIdentificationRegistration,      // 18
    fInvolvedPeople,                       // 19
    fInvolvedPeopleList,                   // 20
    fMusicianCreditsList,                  // 21
    fMood,                                 // 22
    fLinkedInfo,                           // 23
    fMusicCDIdentifier,                    // 24
    fMPEGtable,                            // 25
    fMPEGLocationLookupTable,              // 26
    fOwnership,                            // 27
    fPositionSynchronization,              // 28
    fPrivate,                              // 29
    fPicture,                              // 30
    fAttachedPicture,                      // 31
    fPopularimeter,                        // 32
    fReverb,                               // 33
    fVolumeAdj,                            // 34
    fSyncedText,                           // 35
    fSyncedTempo,                          // 36
    fTitle,                                // 37
    fBeatsPerMinute,                       // 38
    fComposer,                             // 39
    fContentType,                          // 40
    fCopyrightMessage,                     // 41
    fDate,                                 // 42
    fPlaylistDelay,                        // 43
    fEncodedBy,                            // 44
    fFileType,                             // 45
    fFileLicense,                          // 46
    fTime,                                 // 47
    fInitialKey,                           // 48
    fLanguages,                            // 49
    fLength,                               // 50
    fMediaType,                            // 51
    fProducedNotice,                       // 52
    fOriginalArtist,                       // 53
    fOriginalfilename,                     // 54
    fOriginalWriter,                       // 55
    fAlbumSortOrder,                       // 56
    fPerformerSortOrder,                   // 57
    fTitleSortOrder,                       // 58
    fOriginalReleaseYear,                  // 59
    fOriginalReleaseTime,                  // 60
    fOriginalTitle,                        // 61
    fRadioStationOwner,                    // 62
    fLeadArtist,                           // 63
    fAccompaniment,                        // 64
    fConductor,                            // 65
    fPerformerRefinement,                  // 66
    fModifiedBy,                           // 67
    fPartOfaSet,                           // 68
    fPublisher,                            // 69
    fISRC,                                 // 70
    fRecordingDates,                       // 71
    fRecordingTime,                        // 72
    fTaggingTime,                          // 73
    fTrackNumber,                          // 74
    fReleaseTime,                          // 75
    fSetNumber,                            // 76
    fSize,                                 // 77
    fEncodingTime,                         // 78
    fEncodingParams,                       // 79
    fContentGroupDescription,              // 80
    fTitleDescription,                     // 81
    fSubTitleDescription,                  // 82
    fSetSubTitle,                          // 83
    fWriter,                               // 84
    fUserDefined,                          // 85
    fYear,                                 // 86
    fUniqueFileID,                         // 87
    fUnSynchronizedTranscription,          // 88
    fOfficialFileWebpage,                  // 89
    fOfficialArtistWebpage,                // 90
    fOfficialAudioSourceWebpage,           // 91
    fCommericalInfo,                       // 92
    fCopyrightInfo,                        // 93
    fPublishersOfficialWebpage,            // 94
    fStationURL,                           // 95
    fURL,                                  // 96
    fUserDefinedURL,                       // 97
    fPaymentURL,                           // 98
    fUserDefinedTextInformation,           // 99
    fTextWriter,                           // 100
    fPodcastKeywords,                      // 101
    fPodcast,                              // 102
    fPodcastDescription,                   // 103
    fPodcastFeed,                          // 104
    fPodcastID                             // 105
  );
Const
    TextTagFrames:set of TFrameKind=[];
    FrameKind:Array[TFrameKind] of Core.Strings.VarString=(
    'None',                                // 0
    'ID3',                                 // 1
    'ID3 V2.0',                            // 2
    'ID3 V2.3',                            // 3
    'ID3 V3.0',                            // 4
    'ID3 V4.0',                            // 5
    'Raw',                                 // 6
    'MPEG',                                // 7
    'Commercial',                          // 8
    'Compilation',                         // 9
    'Buffer Size',                         // 10
    'Play Counter',                        // 11
    'Comments',                            // 12
    'Audio Encryption',                    // 13
    'Encrypted Meta Data',                 // 14
    'Event Timing Codes',                  // 15
    'Equalization',                        // 16
    'General Encapsulated Object',         // 17
    'Group Identification Registration',   // 18
    'Involved People',                     // 19
    'Involved People List',                // 20
    'Musician Credit List',                // 21
    'Mood',                                // 22
    'Linked Information',                  // 23
    'Music CD Identifier',                 // 24
    'MPEG Lookup Table',                   // 25
    'MPEG Location Lookup Table',          // 26
    'Ownership',                           // 27
    'Position Synchronization',            // 28
    'Private',                             // 29
    'Picture',                             // 30
    'Attached Picture',                    // 31
    'Rating',                              // 32
    'Reverb',                              // 33
    'Volume Adjustment',                   // 34
    'Synchronized Text',                   // 35
    'Synchronized Tempo',                  // 36
    'Title',                               // 37
    'Beats Per Minute',                    // 38
    'Composer',                            // 39
    'Content Type',                        // 40
    'Copyright Message',                   // 41
    'Date',                                // 42
    'Playlist Delay',                      // 43
    'Encoded By',                          // 44
    'File Type',                           // 45
    'File License',                        // 46
    'Time',                                // 47
    'Initial Key',                         // 48
    'Languages',                           // 49
    'Length',                              // 50
    'Media Type',                          // 51
    'Produced Notice',                     // 52
    'Original Artist',                     // 53
    'Original filename',                   // 54
    'Original Writer',                     // 55
    'Album Sort Order',                    // 56
    'Performer Sort Order',                // 57
    'Title Sort Order',                    // 58
    'Original Release Year',               // 59
    'Original Release Time',               // 60
    'Original Title',                      // 61
    'Radio Station Owner',                 // 62
    'Lead Artist',                         // 63
    'Accompaniment',                       // 64
    'Conductor',                           // 65
    'Performer Refinement',                // 66
    'Modified By',                         // 67
    'Part Of a Set',                       // 68
    'Publisher',                           // 69
    'ISRC',                                // 70
    'Recording Dates',                     // 71
    'Recording Time',                      // 72
    'Tagging Time',                        // 73
    'Track Number',                        // 74
    'Release Time',                        // 75
    'Set Number',                          // 76
    'Size',                                // 77
    'Encoding Time',                       // 78
    'Encoding Params',                     // 79
    'Content Group Description',           // 80
    'Title Description',                   // 81
    'Subtitle Description',                // 82
    'Set Subtitle',                        // 83
    'Writer',                              // 84
    'User Defined',                        // 85
    'Year',                                // 86
    'Unique File Identifier',              // 87
    'Unsynchronized Transcription',        // 88
    'Official File Webpage',               // 89
    'Official Artist Webpage',             // 90
    'Official Audio Source Webpage',       // 91
    'Commerical Information',              // 92
    'Copyright Information',               // 93
    'Publishers Official Webpage',         // 94
    'Station URL',                         // 95
    'URL',                                 // 96
    'User Defined URL',                    // 97
    'Payment URL',                         // 98
    'User Defined Text Information',       // 99
    'Text Writer',                         // 100
    'Podcast Keywords',                    // 101
    'Podcast',                             // 102
    'Podcast Description',                 // 103
    'Podcast Feed',                        // 104
    'Podcast ID'                           // 105
  );

Type
  TTextEncoding=(txtISO8859_1=$00,txtUTF16=$01,txtUTF16BE=$02,txtUTF8=$03);
  TTextContentType=(txtOther=$00,txtLyrics=$01,txtTranscription=$02,txtMovement=$03,txtEvents=$04,txtChord=$05);
  TUniqueFileID=record
    Owner                    : Core.Strings.VarString;
    Identifier               : Core.Arrays.Types.Bytes;
  end;
  PCommentPayload=^TCommentPayload;
  TCommentPayload=record
    Encoding                 : TTextEncoding;
    Language                 : Array[0..2] of Char;
    Description              : Core.Strings.VarString;
    Text                     : Core.Strings.VarString;
  end;
  PTextPayload=^TTextPayload;
  TTextPayload=record
    Encoding                 : TTextEncoding;
    Information              : Core.Strings.VarString;
  end;
  PUserTextPayload=^TUserTextPayload;
  TUserTextPayload=record
    Encoding                 : TTextEncoding;
    Description              : Core.Strings.VarString;
    Value                    : Core.Strings.VarString;
  end;
  PURLPayload=^TURLPayload;
  TURLPayload=record
    URL                      : Core.Strings.VarString;
  end;
  PUserURLPayload=^TUserURLPayload;
  TUserURLPayload=record
    Encoding                 : TTextEncoding;
    Description              : Core.Strings.VarString;
    URL                      : Core.Strings.VarString;
  end;
  TTimeStampFormat=(tsNone,tsFrames,tsMilliseconds);
  TTimeStampKind=(
    tskPadding=$00,tskEndOfInitialSilence=$01,tskIntroStart=$02,tskMainPartStart=$03,
    tksOutroStart=$04,tskOutroEnd=$05,tskVerseBegins=$06,tskRefrainBegins=$07,tskInterlude=$08,
    tskThemeStart=$09,tskVariation=$0A,tskKeyChange=$0B,tskTimeChange=$0C,tskUnwantedNoise=$0D,
    tskUserDefined0=$E0,tskUserDefined1=$E1,tskUserDefined2=$E2,tskUserDefined3=$E3,
    tskUserDefined4=$E4,tskUserDefined5=$E5,tskUserDefined6=$E6,tskUserDefined7=$E7,
    tskUserDefined8=$E8,tskUserDefined9=$E9,tskUserDefined10=$EA,tskUserDefined11=$EB,
    tskUserDefined12=$EC,tskUserDefined13=$ED,tskUserDefined14=$EF,
    tskReserved0=$F0,tskReserved1=$F1,tskReserved2=$F2,tskReserved3=$F3,tskReserved4=$F4,
    tskReserved5=$F5,tskReserved6=$F6,tskReserved7=$F7,tskReserved8=$F8,tskReserved9=$F9,
    tskReserved10=$FA,tskReserved11=$FB,tskReserved12=$FC,
    tskAudioEnd=$FD,tskAudioFileEnds=$FE,tskOneByteOfEventsFollows=$FF
  );
  TVolumeAdjustment=(vaDecrement=0,vaIncrement=1);
  TPictureKind=(
    pkOther=$00,pk32x32Icon=$01,pkOtherIcon=$02,pkCoverFront=$03,pkCoverBack=$04,pkLeaflet=$05,
    pkCoverMedia=$06,pkLeadArtist=$07,pkArtist=$08,pkConductor=$09,pkBand=$0A,pkComposer=$0B,
    pkWriter=$0C,pkRecordingLocation=$0D,pkDuringRecording=$0E,pkDuringPerformance=$0F,pkScreenShot=$10,
    pkFish=$11,pkIllustration=$12,pkArtistLogo=$13,pkPublisherLogo=$14
  );
  TCommericalKind=(
    ckOther=$00,ckStandardCDAlbum=$01,ckCompressedAudio=$02,ckFileOverInternet=$03,ckInternetStream=$04,
    ckNoteSheets=$05,ckNoteSheetsInBook=$06,ckMusicOnOtherMedia=$07,ckNonMusicalMerchandise=$08
  );
  TInvolvedPeopleListPayload=record
    Encoding                 : TTextEncoding;
    List                     : Core.Arrays.Types.KeyStrings;
  end;
  TCommercialPayload=record
    Encoding                 : TTextEncoding;
    Price                    : Core.Strings.VarString;
    Expires                  : Core.Strings.VarString;
    Contact                  : Core.Strings.VarString;
    Kind                     : TCommericalKind;
    Seller                   : Core.Strings.VarString;
    Description              : Core.Strings.VarString;
    MimeType                 : Core.Strings.VarString;
    Logo                     : Core.Arrays.Types.Bytes;
  end;

  TMusicIdentifierPayload=record
    TOC                      : Core.Arrays.Types.Bytes;
  end;
  TEventItem=record
    Kind                     : TTimeStampKind;
    Stamp                    : Cardinal;
  end;
  TEventTimingPayload=record
    Format                   : TTimeStampFormat;
    Events                   : Array of TEventItem;
  end;
  TLocationPayloadDeviation=record
    Bytes                    : Byte;
    Milliseconds             : Byte;
  end;
  TLocationPayloadDeviations=Array of TLocationPayloadDeviation;

  TLocationPayload=record
    MPEGFrames               : Array[0..1] of byte;
    BytesBetween             : Array[0..2] of byte;
    MillisecondsBetween      : Array[0..2] of byte;
    BitsForBytesDeviation    : Byte;
    BitsForFramesDeviation   : Byte;
    References               : TLocationPayloadDeviations;
  end;
  TOwnershipPayload=record
    Encoding                 : TTextEncoding;
    PricePayed               : Core.Strings.VarString;
    Date                     : Core.Strings.VarString;
    Seller                   : Core.Strings.VarString;
  end;
  TPositionSynchronizationPayload=record
    Format                   : TTimeStampFormat;
    Position                 : cardinal;
  end;
  TPrivatePayload=record
    Owner                    : Core.Strings.VarString;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TTempoPayload=record
    Format                   : TTimeStampFormat;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TUnSynchedTranscriptPayload=record
    Encoding                 : TTextEncoding;
    Language                 : Array[0..2] of Char;
    Description              : Core.Strings.VarString;
    Text                     : Core.Strings.VarString;
  end;
  TSynchedText=record
    Text                     : Core.Strings.VarString;
    Stamp                    : Cardinal;
  end;
  TSynchedTextList=array of TSynchedText;
  TSynchedTranscriptPayload=record
    Encoding                 : TTextEncoding;
    Language                 : Array[0..2] of Char;
    Format                   : TTimeStampFormat;
    ContentType              : TTextContentType;
    Description              : Core.Strings.VarString;
    List                     : TSynchedTextList;
  end;
  TVolumeAdjustments=record
    Adjustment               : TVolumeAdjustment;
    Delta                    : Core.Arrays.Types.Bytes;
    PeakVolume               : Core.Arrays.Types.Bytes;
  end;
  TRightLeftVolumeAdjustment=record
    Right                    : TVolumeAdjustments;
    Left                     : TVolumeAdjustments;
  end;
  TRelativeVolumeAdjustmentPayload=record
    BitsUsed                 : byte;
    Stereo                   : TRightLeftVolumeAdjustment;
    Front                    : TRightLeftVolumeAdjustment;
    Back                     : TRightLeftVolumeAdjustment;
    Center                   : TVolumeAdjustments;
    Bass                     : TVolumeAdjustments;
  end;
  TRelativeVolumePayload=record
    Adjustment               : TVolumeAdjustment;
    Description              : Byte;
    DeltaRight               : Core.Arrays.Types.Bytes;
    DeltaLeft                : Core.Arrays.Types.Bytes;
    PeakVolumeRight          : Core.Arrays.Types.Bytes;
    PeakVolumeLeft           : Core.Arrays.Types.Bytes;
  end;
  TEqualizationBand=record
    Direction                : TVolumeAdjustment;
    Frequency                : WORD;
    Adjustment               : Core.Arrays.Types.Bytes;
  end;
  TEqualizationPayload=record
    AdjustmentBits           : Byte;
    Bands                    : Array of TEqualizationBand;
  end;
  TReverbPayload=record
    Left                     : WORD;
    Right                    : WORD;
    BouncesLeft              : BYTE;
    BouncesRight             : BYTE;
    FeedbackLeftToLeft       : BYTE;
    FeedbackLeftToRight      : BYTE;
    FeedbackRightToRight     : BYTE;
    FeedbackRightToLeft      : BYTE;
    PremixLeftToRight        : BYTE;
    PremixRightToLeft        : BYTE;
  end;
  PPicturePayload=^TPicturePayload;
  TPicturePayload=record
    Encoding                 : TTextEncoding;
    ImageFormat              : Array[0..2] of Char;
    ImageKind                : TPictureKind;
    Description              : Core.Strings.VarString;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  PAttachedPicturePayload=^TAttachedPicturePayload;
  TAttachedPicturePayload=record
    Encoding                 : TTextEncoding;
    MimeType                 : Core.Strings.VarString;
    ImageKind                : TPictureKind;
    Description              : Core.Strings.VarString;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TEncapsulatedObjectPayload=record
    Encoding                 : TTextEncoding;
    MimeType                 : Core.Strings.VarString;
    FileName                 : Core.Strings.VarString;
    Description              : Core.Strings.VarString;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TPlayCounterPayload=record
    Counter                  : Core.Arrays.Types.Bytes;
  end;
  TPopularimeterPayload=record
    Email                    : Core.Strings.VarString;
    Rating                   : Byte;
    Counter                  : Core.Arrays.Types.Bytes;
  end;
  TBufferSizePayload=record
    BufferSize               : Array[0..2] of Byte;
    Flags                    : Byte;
    Offset                   : Cardinal;
  end;
  TEncryptedMethodPayload=record
    Owner                    : Core.Strings.VarString;
    Method                   : byte;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TEncryptedMetaPayload=record
    Owner                    : Core.Strings.VarString;
    Description              : Core.Strings.VarString;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TEncryptedAudioPayload=record
    Owner                    : Core.Strings.VarString;
    PreviewStart             : WORD;
    PreviewLength            : WORD;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TGroupIdentificationRegistrationPayload=record
    Owner                    : Core.Strings.VarString;
    Symbol                   : byte;
    Data                     : Core.Arrays.Types.Bytes;
  end;
  TLinkedInfoPayload=record
    FrameID                  : Array[0..2] of Byte;
    URL                      : Core.Strings.VarString;
    List                     : Core.Arrays.Types.VarString;
  end;
  TReader=class;
  TFrame=class;

  TFrameEvent=procedure(Frame:TFrame; Stream:TStream; var Handled:Boolean) of Object;
  TTagFrameEvent=procedure(Main:TFrame; TagFrame:TFrame; Stream:TStream; var Handled:Boolean) of Object;

  TFrameHeader=class
  private
    FName                    : Core.Strings.VarString;
    FVersMajor               : Byte;
    FVersMinor               : Byte;
    FReader                  : TReader;
    FStreamStart             : int64;
    FLength                  : cardinal;
  public
    procedure Reset(); virtual;
    function  Load(Stream:TStream):boolean; virtual; abstract;
  public
    constructor Create(aReader:TReader); reIntroduce;
    destructor  Destroy; override;
  public
    property VersionMajor:Byte read FVersMajor;
    property VersionMinor:Byte read FVersMinor;
    property Length:cardinal read FLength;
    property Name:Core.Strings.VarString read FName;
  end;

  TFramePayload=class
  private
    // Payloads
    pldBUF  : TBufferSizePayload;
    pldCNT  : TPlayCounterPayload;
    pldCOM  : TCommentPayload;
    pldCRA  : TEncryptedAudioPayload;
    pldCRM  : TEncryptedMetaPayload;
    pldETC  : TEventTimingPayload;
    pldEQU  : TEqualizationPayload;
    pldGEO  : TEncapsulatedObjectPayload;
    pldIPL  : TInvolvedPeopleListPayload;
    pldLNK  : TLinkedInfoPayload;
    pldMCI  : TMusicIdentifierPayload;
    pldMLL  : TLocationPayload;
    pldPIC  : TPicturePayload;
    pldPOP  : TPopularimeterPayload;
    pldREV  : TReverbPayload;
    pldRVA  : TRelativeVolumePayload;
    pldSLT  : TSynchedTranscriptPayload;
    pldSTC  : TTempoPayload;
    pldWXX  : TUserURLPayload;
    pldURL  : TURLPayload;
    pldUFI  : TUniqueFileID;
    pldULT  : TUnSynchedTranscriptPayload;
    pldText : TTextPayload;
    pldTXX  : TUserTextPayload;
  private
    FStreamStart             : cardinal;
    FLength                  : cardinal;
    FReader                  : TReader;
    FData                    : Pointer;
  public
    procedure Reset(); virtual;
    procedure Load(Stream:TStream; var Handled:boolean); virtual; abstract;
  public
    constructor Create(aReader:TReader); reIntroduce;
    destructor Destroy; override;
  public
    property Data:pointer read FData;
  end;

  TFrame=class
  private
    FPreloaded               : boolean;
    FHeader                  : TFrameHeader;
    FPayload                 : TFramePayload;
  protected
    FKind                    : TFrameKind;
    FReader                  : TReader;
    FPosition                : int64;
    FLength                  : cardinal;
    FAggregateLength         : qword;
  public
    procedure Reset(); virtual;
    procedure Preload(Stream:TStream); virtual; abstract;
    function  Load(Stream:TStream; var Handled:Boolean):boolean; virtual; abstract;
  public
    constructor Create(aKind:TFrameKind; aReader:TReader); reIntroduce;
    destructor Destroy; override;
  public
    property AggregateLength:qword read FAggregateLength;
    property Header:TFrameHeader read FHeader;
    property Payload:TFramePayload read FPayload;
    property Kind:TFrameKind read FKind;
  end;
  TID3Header=class;
  TID3Frame=class(TFrame)
  const
    ID       : Core.Strings.VarString = 'ID3';
  public
    procedure Reset(); override;
    procedure Preload(Stream:TStream); override;
    function  Load(Stream:TStream; var Handled:Boolean):boolean; override;
    function  Header:TID3Header;
  public
    constructor Create(aReader:TReader); reIntroduce;
  end;

  TID3Header=class(TFrameHeader)
  private
    FFLags        : byte;
    FSize         : Array[0..3] of byte;
  public
    procedure Reset(); override;
    function  Load(Stream:TStream):boolean; override;
    function  Frame:TID3Frame;
  end;

  TID3Payload=class(TFramePayload)
  public
    procedure Load(Stream:TStream; var Handled:Boolean); override;
    function Header:TID3Header;
    function Frame:TID3Frame;
  end;
  TFramePreservation=(ftPreserveFrame,ftDiscardFrame);

  {$i Multimedia.MPEG.TID3V20.Decs.inc}
  {$i Multimedia.MPEG.TID3V23.Decs.inc}
  {$i Multimedia.MPEG.TID3V30.Decs.inc}
  {$i Multimedia.MPEG.TID3V40.Decs.inc}
  {$i Multimedia.MPEG.Decs.inc}

  TReader=class
  private
    FFrameCount              : Cardinal;
    FRefactor                : TMemoryStream;
    FAggregateLength         : qword;
    FStreamSize              : qword;
    FStreamPosition          : qword;

    FBitRate                 : dword;
    FSampleHz                : word;
    FFrameSizeTotal          : qword;

    FFrameSize               : Cardinal;
    FDuration                : Double;

    //FData                    : THeaderData;
    FID3Frame                : TID3Frame;
    FMPEGFrame               : TMPEGFrame;
    // ID3 Frames
    FID3V20                  : TID3V20TagFrame;
    FID3V23                  : TID3V23TagFrame;
    FID3V30                  : TID3V30TagFrame;
    FID3V40                  : TID3V40TagFrame;
    // Generic Frame Pointers
    FTagFrame                : TFrame;

    FOnTagFrameEvent         : TTagFrameEvent;
  public
    property FrameCount:Cardinal read FFrameCount;
    property FrameSizeTotal:QWord read FFrameSizeTotal;
    property Duration:Double Read FDuration;
    property BitRate:dword read FBitRate;
  public
    procedure LoadAll(Stream:TStream);
    procedure LoadFirst(Stream:TStream);
  public
    constructor Create(); reIntroduce;
    destructor Destroy(); override;
  public
    property OnTagFrame:TTagFrameEvent read FOnTagFrameEvent write FOnTagFrameEvent;
  end;

  function  BitsToBytes(Value:Byte):Byte;
  procedure SetByteCount(const Bytes:Byte; var Item:TVolumeAdjustments); overload;
  procedure SetByteCount(const Bytes:Byte; var Item:TRightLeftVolumeAdjustment); overload;

  const Genre:Array[0..125] of Core.Strings.VarString=(
    'Blues',                 // 0
    'Classic Rock',          // 1
    'Country',               // 2
    'Dance',                 // 3
    'Disco',                 // 4
    'Funk',                  // 5
    'Grunge',                // 6
    'Hip-Hop',               // 7
    'Jazz',                  // 8
    'Metal',                 // 9
    'New Age',               // 10
    'Oldies',                // 11
    'Other',                 // 12
    'Pop',                   // 13
    'R&B',                   // 14
    'Rap',                   // 15
    'Reggae',                // 16
    'Rock',                  // 17
    'Techno',                // 18
    'Industrial',            // 19
    'Alternative',           // 20
    'Ska',                   // 21
    'Death Metal',           // 22
    'Pranks',                // 23
    'Soundtrack',            // 24
    'Euro-Techno',           // 25
    'Ambient',               // 26
    'Trip-Hop',              // 27
    'Vocal',                 // 28
    'Jazz+Funk',             // 29
    'Fusion',                // 30
    'Trance',                // 31
    'Classical',             // 32
    'Instrumental',          // 33
    'Acid',                  // 34
    'House',                 // 35
    'Game',                  // 36
    'Sound Clip',            // 37
    'Gospel',                // 38
    'Noise',                 // 39
    'Alternative',           // 40
    'Bass',                  // 41
    'Soul',                  // 42
    'Punk',                  // 43
    'Space',                 // 44
    'Meditative',            // 45
    'Instrumental Pop',      // 46
    'Instrumental Rock',     // 47
    'Ethnic',                // 48
    'Gothic',                // 49
    'Darkwave',              // 50
    'Techno-Industrial',     // 51
    'Electronic',            // 52
    'Pop-Folk',              // 53
    'Eurodance',             // 54
    'Dream',                 // 55
    'Southern Rock',         // 56
    'Comedy',                // 57
    'Cult',                  // 58
    'Gangsta',               // 59
    'Top 40',                // 60
    'Christian Rap',         // 61
    'Pop/Funk',              // 62
    'Jungle',                // 63
    'Native American',       // 64
    'Cabaret',               // 65
    'New Wave',              // 66
    'Psychadelic',           // 67
    'Rave',                  // 68
    'Showtunes',             // 69
    'Trailer',               // 70
    'Lo-Fi',                 // 71
    'Tribal',                // 72
    'Acid Punk',             // 73
    'Acid Jazz',             // 74
    'Polka',                 // 75
    'Retro',                 // 76
    'Musical',               // 77
    'Rock & Roll',           // 78
    'Hard Rock',             // 79
    'Folk',                  // 80
    'Folk-Rock',             // 81
    'National Folk',         // 82
    'Swing',                 // 83
    'Fast Fusion',           // 84
    'Bebob',                 // 85
    'Latin',                 // 86
    'Revival',               // 87
    'Celtic',                // 88
    'Bluegrass',             // 89
    'Avantgarde',            // 90
    'Gothic Rock',           // 91
    'Progressive Rock',      // 92
    'Psychedelic Rock',      // 93
    'Symphonic Rock',        // 94
    'Slow Rock',             // 95
    'Big Band',              // 96
    'Chorus',                // 97
    'Easy Listening',        // 98
    'Acoustic',              // 99
    'Humour',                // 100
    'Speech',                // 101
    'Chanson',               // 102
    'Opera',                 // 103
    'Chamber Music',         // 104
    'Sonata',                // 105
    'Symphony',              // 106
    'Botty Bass',            // 107
    'Primus',                // 108
    'Groovy',                // 109
    'Satire',                // 110
    'Slow Jam',              // 111
    'Club',                  // 112
    'Tango',                 // 113
    'Samba',                 // 114
    'Folklore',              // 115
    'Ballad',                // 116
    'Power Ballad',          // 117
    'Rhythmic Soul',         // 118
    'Freestyle',             // 119
    'Duet',                  // 120
    'Punk Rock',             // 121
    'Drum Solo',             // 122
    'A capella',             // 123
    'Euro-House',            // 124
    'Dance Hall'             // 125
  );



implementation

function BitsToBytes(Value:Byte):Byte;
begin
  if Value mod 8=0 then
    Result:=Value div 8
  else begin
    Result:=(Value div 8) + 1;
  end;
end;

procedure SetByteCount(const Bytes:Byte; var Item:TVolumeAdjustments);
begin
  System.SetLength(Item.Delta,Bytes);
  System.SetLength(Item.PeakVolume,Bytes);
end;

procedure SetByteCount(const Bytes:Byte; var Item:TRightLeftVolumeAdjustment);
begin
  SetByteCount(Bytes,Item.Left);
  SetByteCount(Bytes,Item.Right);
end;

constructor TFrame.Create(aKind:TFrameKind; aReader:TReader);
begin
  FReader:=aReader;
  FKind:=aKind;
  inherited Create;
end;

destructor TFrame.Destroy;
begin
  FreeAndNil(FHeader);
  FreeAndNil(FPayload);
  inherited Destroy;
end;

procedure TFrame.Reset();
begin
  FPosition:=0;
  FLength:=0;
  FPreloaded:=false;
  FAggregateLength:=0;
  Header.Reset();
  Payload.Reset();
end;

constructor TFramePayload.Create(aReader:TReader);
begin
  FData:=nil;
  FReader:=aReader;
  Inherited Create;
end;

destructor TFramePayload.Destroy;
begin
  Inherited Destroy;
end;

procedure TFramePayload.Reset();
begin
  FStreamStart:=0;
  FLength:=0;
  FData:=nil;
end;

constructor TFrameHeader.Create(aReader:TReader);
begin
  FVersMajor:=0;
  FVersMinor:=0;
  FReader:=aReader;
  inherited Create;
end;

destructor TFrameHeader.Destroy;
begin
  Inherited Destroy;
end;

procedure TFrameHeader.Reset();
begin
  FVersMajor:=0;
  FVersMinor:=0;
  FStreamStart:=0;
  FLength:=0;
end;


constructor TID3Frame.Create(aReader:TReader);
begin
  FHeader:=TID3Header.Create(aReader);
  FPayload:=TID3Payload.Create(aReader);
  Inherited Create(fID3,aReader);
end;

procedure TID3Frame.Reset();
begin
  inherited Reset();
end;
function  TID3Frame.Header:TID3Header;
begin
  Result:=TID3Header(FReader.FID3Frame.FHeader);
end;

procedure TID3Frame.Preload(Stream:TStream);
begin
  // Could do some parsing here.
end;

function TID3Frame.Load(Stream:TStream; var Handled:Boolean):boolean;
begin
  // start @ pos looking for ID3 repeat until end
  Result:=false;
  Handled:=false;
  FPosition:=Stream.Position;
  FHeader.FStreamStart:=Core.Streams.Pos(Stream,ID,Stream.Position);
  if (FHeader.FStreamStart>-1) then begin
    // found instance...
    Result:=FHeader.Load(Stream);
    If Result then begin
      FPayLoad.FStreamStart:=Stream.Position;
      FPayLoad.FLength:=FHeader.FLength;
      FPayload.Load(Stream,Handled);
      if (Handled=false) then
        Stream.Position:=FPayLoad.FStreamStart+FPayLoad.FLength;
    end;
  end;
end;

procedure TID3Header.Reset();
begin
  Inherited Reset();
end;

function  TID3Header.Frame:TID3Frame;
begin
  Result:=FReader.FID3Frame;
end;

function TID3Header.Load(Stream:TStream):boolean;
var
  Handled:Boolean;
  iData:Cardinal;

  procedure Push_Vers_2;
  begin
    case FVersMinor of
      0 : FReader.FTagFrame:=FReader.FID3V20;
      3 : FReader.FTagFrame:=FReader.FID3V23;
    end;
  end;

  procedure Push_Vers_3;
  begin
    case FVersMinor of
      0 : FReader.FTagFrame:=FReader.FID3V30;
    end;
  end;

  procedure Push_Vers_4;
  begin
    case FVersMinor of
      0 : FReader.FTagFrame:=FReader.FID3V40;
    end;
  end;
begin
  // assume stream is ready at version info
  Result:=False;
  Stream.Read(FVersMajor,1);
  Stream.Read(FVersMinor,1);
  Stream.Read(FFlags,1);
  Stream.Read(FSize[0],SizeOf(FSize));
  FLength:=FSize[3] + FSize[2] shl 8 + FSize[1] shl 16 + FSize[0] shl 24;
  FReader.FTagFrame:=nil;
  Case FVersMajor of
    2 : Push_Vers_2;
    3 : Push_Vers_3;
    4 : Push_Vers_4;
  end;
  Result:=FReader.FTagFrame<>nil;
end;

function TID3Payload.Header:TID3Header;
begin
  Result:=TID3Header(FReader.FID3Frame.FHeader);
end;

function TID3Payload.Frame:TID3Frame;
begin
  Result:=FReader.FID3Frame;
end;

procedure TID3Payload.Load(Stream:TStream; var Handled:boolean);
var
  TagHandled:Boolean;
  bLoopOK:boolean;

  procedure LoopWithCallback;
  begin
    Repeat
      TagHandled:=false;
      bLoopOK:=FReader.FTagFrame.Load(Stream,TagHandled);
      if bLoopOK then begin
        If (TagHandled) then begin
          if (FReader.FTagFrame.Kind<>fNone) then
            FReader.FOnTagFrameEvent(FReader.FID3Frame,FReader.FTagFrame,Stream,Handled);
        end;
        FReader.FStreamPosition:=Stream.Position;
      end;
    until  (bLoopOK=false) or (FReader.FStreamPosition>=FStreamStart+FLength) or (FReader.FStreamPosition>=FReader.FStreamSize);
  end;

  procedure LoopWithNoCallback;
  begin
    Repeat
      TagHandled:=false;
      bLoopOK:=FReader.FTagFrame.Load(Stream,TagHandled);
      if bLoopOK then
        FReader.FStreamPosition:=Stream.Position;
    until  (bLoopOK=false) or (FReader.FStreamPosition>=FStreamStart+FLength) or (FReader.FStreamPosition>=FReader.FStreamSize);
  end;

begin
  // assume just reading tags until ID3.FLength is consumed
  If (FReader.FTagFrame<>nil) then begin
    Handled:=True;
    FReader.FTagFrame.Preload(Stream); // now we get version specific ID3 header extensions
    if Assigned(FReader.FOnTagFrameEvent) then
      LoopWithCallback
    else
      LoopWithNoCallback;
  end;
end;


procedure TMPEGHeader.Reset();
begin
  inherited Reset();
  FVersMajor:=0;
  FVersMinor:=0;
  FStreamStart:=0;
  FLength:=4;
end;

function  TMPEGHeader.Frame:TMPEGFrame;
begin
  Result:=FReader.FMPEGFrame;
end;

function TMPEGHeader.Load(Stream:TStream):boolean;

  function Advance_RAW():boolean;
  var
    Found    : Boolean;
    iRead    : Byte;
    iStop    : qword;
  begin
    iRead:=FLength;
    iStop:=FReader.FStreamSize-iRead;
    repeat
      Stream.Read(FData,iRead);
      Found:=((FData or ID_MASK)=FData);
      if not Found then
        FReader.FStreamPosition:=Stream.Seek(-(iRead-1),soFromCurrent);
    until (Found) or (FReader.FStreamPosition>=iStop);
    Result:=Found;
  end;
begin
  Result:=False;
end;

procedure TReader.LoadFirst(Stream:TStream);
var
  Handled:Boolean;
begin
  FFrameCount:=0;
  FDuration:=0;
  FFrameSizeTotal:=0;


  Handled:=False;
  FStreamSize:=Stream.Size;
  FStreamPosition:=Stream.Position;

  FID3Frame.Load(Stream,Handled);
end;

procedure TReader.LoadAll(Stream:TStream);
var
  Handled:Boolean;
begin
  FFrameCount:=0;
  FDuration:=0;
  FFrameSizeTotal:=0;


  Handled:=False;
  FStreamSize:=Stream.Size;
  FStreamPosition:=Stream.Position;
  repeat
    FID3Frame.Load(Stream,Handled);
  until (Stream.Position>=Stream.Size);
  (*
  See uMPEG.Raw.inc for todo code
  Load_RAW_Frames;
  *)

end;

constructor TReader.Create();
begin
  FRefactor:=TMemoryStream.Create;
  FMPEGFrame:=TMPEGFrame.Create(Self);
  FID3Frame:=TID3Frame.Create(Self);
  FID3V20:=TID3V20TagFrame.Create(Self);
  FID3V23:=TID3V23TagFrame.Create(Self);
  FID3V30:=TID3V30TagFrame.Create(Self);
  FID3V40:=TID3V40TagFrame.Create(Self);
  Inherited Create();
end;

destructor TReader.Destroy();
begin
  FreeAndNil(FRefactor);
  FreeAndNil(FMPEGFrame);
  FreeAndNil(FID3Frame);
  FreeAndNil(FID3V20);
  FreeAndNil(FID3V23);
  FreeAndNil(FID3V30);
  FreeAndNil(FID3V40);

  Inherited Destroy;
end;

{$i Multimedia.MPEG.TID3V20.Code.inc}
{$i Multimedia.MPEG.TID3V23.Code.inc}
{$i Multimedia.MPEG.TID3V30.Code.inc}
{$i Multimedia.MPEG.TID3V40.Code.inc}
{$i Multimedia.MPEG.Code.inc}


end.


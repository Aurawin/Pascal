{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit RSR.HTTP;

interface
uses Classes,

  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Bytes,
  Core.Timer,
  Core.Utils.Time,
  Core.Streams,
  Core.Database.Types,
  Core.Database,
  Core.Utils.Files,
  Encryption.Base64,
  Encryption.Zip,

  RSR,

  HTTPDefs,
  DateUtils;

Type

  THTTPRequest=class;
  THTTPResponse=class;
  THTTPState=(httpStateLookupIP,httpStateConnect,httpStateConnected);
  TIntegerCallback=procedure(Const Loc:LongInt) of Object;
  TVoidCallback=procedure of Object;
  TContentKind=(ckNone,ckContent,ckString,ckStream,ckMedia,ckCors);
  TYesNoIntegerCallbacks=Array[Boolean] of TIntegerCallback;
  TYesNoVoidCallbacks=Array[Boolean] of TVoidCallback;
  TMediaState=(msIdle,msQuery,msEstablished);
  TMediaRange=record
    Start          : QWord;
    Stop           : QWord;
    Size           : QWord;
  end;

  PHTTPMedia=^THTTPMedia;
  THTTPMedia=record
    ContentLength  : QWord;
    State          : TMediaState;
    Content        : TMemoryStream;
    Range          : TMediaRange;
    Modified       : System.Double;
    ETag           : Core.Strings.VarString;
    ContentType    : Core.Strings.VarString;
    URI            : Core.Strings.VarString;
  end;
  TMediaList=Array[0..16] of Core.Strings.VarString;
  TImageMediaList=Array[0..4] of Core.Strings.VarString;
  TImageTransformList=Array[0..3] of Core.Strings.VarString;
  TVideoTransformList=Array[0..2] of Core.Strings.VarString;

  THTTPRequest=Class
  private
    FData          : TMemoryStream;
    FRefactor      : TMemoryStream;
    FAuthSet       : Boolean;
    FProtocol      : Core.Strings.VarString;
    FHeaders       : Core.Strings.VarString;
    FMajorVersion  : LongInt;
    FMinorVersion  : LongInt;
    FURI_Callbacks : TYesNoIntegerCallbacks;
    FAuth_Callbacks: TYesNoVoidCallbacks;

    procedure  _CB_SetAuthorization;
    procedure  _CB_SetAuthoriztaion_Empty;

    Procedure  _CB_URI_EMPTY_QUERY(Const Loc:LongInt);
    procedure  _CB_URI_QUERY(Const Loc:LongInt);
  public
    Authorization  : Core.Strings.VarString;
    Username       : Core.Strings.VarString;
    UserAgent      : Core.Strings.VarString;
    Referer        : Core.Strings.VarString;
    Password       : Core.Strings.VarString;
    Method         : Core.Strings.VarString;
    Host           : Core.Strings.VarString;
    URI            : Core.Strings.VarString;
    QUERY          : Core.Strings.VarString;
    ETag           : Core.Strings.VarString;
    WebSocketKey   : Core.Strings.VarString;
    ContentType    : Core.Strings.VarString;
  public
    ContentLength  : QWord;
    Close          : Boolean;
    Upgrade        : Boolean;
    WebSocket      : Boolean;
    Headers        : Core.Arrays.Types.KeyStrings;
    Cookies        : Core.Arrays.Types.KeyStrings;
    Parameters     : Core.Arrays.Types.VarString;
    Challenge      : TChallenge;
  public
    procedure SetCommand(Value:Core.Strings.VarString); Overload;  // FirstQ = ? in param Core.Strings.VarString i.e. cgi-bin\str.exe?test&test
    procedure SetCommand(Var Buffer:Core.Arrays.Types.Bytes; Const Length:LongInt); Overload;
    procedure SetHeaders(Value:Core.Strings.VarString); overload;
    procedure SetHeaders(); overload;
    procedure SetAuthorization;
    procedure AddParameter(Value:Core.Strings.VarString);
    function  fromBuffer(Var Buffer:TDataBuffer):Boolean;
    function  toBuffer(Var Buffer:TDataBuffer):Int64;
    function  HeaderAsString():Core.Strings.VarString;
    Function  AsString():Core.Strings.VarString;
    Function  FullProtocol:Core.Strings.VarString;
    Procedure Reset();
  public
    property  Data:TMemoryStream read FData;
    property  Refactor : TMemoryStream read FRefactor;
  public
    Constructor Create; reIntroduce;
    Destructor  Destroy; Override;
  end;

  THTTPResponse=Class
  private
    FContent         : TMemoryStream;
    FMediaP          : PHTTPMedia;
    FCacheControl    : Core.Strings.VarString;
  public
    Close            : Boolean;
    Head             : Boolean;
    Headers          : Core.Arrays.Types.KeyStrings;
    Cookies          : Core.Arrays.Types.KeyStrings;
    ContentLength    : QWord;
    Code             : LongInt;
    Major            : LongInt;
    Minor            : LongInt;
    Secure           : Boolean;
    ConnectionHeader : Boolean;

    Cache            : Boolean;
    CacheDate        : System.Double;
    CacheTTL         : LongInt;
    CacheExposure    : Core.Strings.VarString;
    CacheValidation  : Core.Strings.VarString;

    SendNullContentLengthHeader : Boolean;

    Host           : Core.Strings.VarString;
    Status         : Core.Strings.VarString;
    Protocol       : Core.Strings.VarString;
    Server         : Core.Strings.VarString;
    ETag           : Core.Strings.VarString;

    ContentType    : Core.Strings.VarString;
    ContentKind    : TContentKind;
    ContentData    : System.Pointer;

    FRefactor      : TMemoryStream;
  public
    Parameters     : Core.Arrays.Types.VarString;
  public
    function  parameterAsQWord(Index:LongInt; const Default:QWord=0):QWord;
  public
    function  fromBuffer(Var Buffer:TDataBuffer):Boolean;
    function  toStreamHeader(Stream:TStream):LongInt;
    function  toStream(Stream:TStream):LongInt;
    procedure Send(RSRP:PRSR; Refactor:TStream); overload;
    procedure SetHeaders();
    procedure SetContent(var Value:Core.Strings.VarString);overload;
    procedure SetContent(Value:TStream; var Media:THTTPMedia);overload;
    procedure SetContent(Value:TStream);overload;
    procedure SetCommand(Value:Core.Strings.VarString);
    procedure Reset();
    procedure AddToContent(Value:Core.Strings.VarString); overload;
    procedure AddToContent(Value:TStream); overload;
    procedure AddToContent(Value:TStream; iStart,iStop:QWord);overload;
  public
    property  Content:TMemoryStream read FContent;
  public
    Constructor Create(); reIntroduce;
    Destructor  Destroy; Override;
  end;

  TResolveResult = (rrInvalidPath, rrNotFound, rrOK, rrFound, rrCoreObject);
  TContentTypeValidatation = (ctvAdd,ctvEdit);
  PContentType=^TContentType;
  PContentTypes=^TContentTypes;
  TContentType=record
    ID        : QWord;
    Verified  : Boolean;
    Ext       : Core.Strings.VarString;
    Kind      : Core.Strings.VarString;
    SubType   : Core.Strings.VarString;
  end;
  TContentTypes=array of PContentType;
  TDefaultContentTypes=array[0..47] of TContentType;


Const
  HTTP_STACK_SIZE                = {$ifdef Windows} 1024*1024*4 {$else} 1024*1024*2 {$endif};
  HTTP_PORT                      = 80;
  HTTPS_PORT                     = 443;
  HTTP_Protocol                  = 'HTTP';
  HTTP_MajorVersion              = 1;
  HTTP_MinorVersion              = 1;
  HTTP_VERSION                   = 'HTTP/1.1';
  HTTP_HEADER_SEP                = #13#10#13#10;
  HTTP_HEADER_SEP_LEN            = 4;
  HTTP_HEADER_FIELD              = ': ';
  HTTP_HEADER_DELIM              = #13#10;

  { HTTP Status code classes }
  HTTP_SCC_INFORMATIONAL         = 1;     // 1xx Informational
  HTTP_SCC_SUCCESSFUL            = 2;     // 2xx Successful
  HTTP_SCC_REDIRECTION           = 3;     // 3xx Redirection
  HTTP_SCC_CLIENTERROR           = 4;     // 4xx Client Error
  HTTP_SCC_SERVERERROR           = 5;     // 5xx Server Error

  { HTTP Status codes }
  HTTP_SC_CONTINUE               : LongInt = 100;
  HTTP_SCS_CONTINUE              : Core.Strings.VarString = '100';
  HTTP_SC_SWITCH                 : LongInt = 101;
  HTTP_SCS_SWITCH                : Core.Strings.VarString = '101';
  HTTP_SC_OK                     : LongInt = 200;
  HTTP_SCS_OK                    : Core.Strings.VarString = '200';
  HTTP_SC_CREATED                : LongInt = 201;
  HTTP_SCS_CREATED               : Core.Strings.VarString = '201';
  HTTP_SC_ACCEPTED               : LongInt = 202;
  HTTP_SCS_ACCEPTED              : Core.Strings.VarString = '202';
  HTTP_SC_NONAUTHORINFORMATION   : LongInt = 203;
  HTTP_SCS_NONAUTHORINFORMATION  : Core.Strings.VarString = '203';
  HTTP_SC_NOCONTENT              : LongInt = 204;
  HTTP_SCS_NOCONTENT             : Core.Strings.VarString = '204';
  HTTP_SC_RESETCONTENT           : LongInt = 205;
  HTTP_SCS_RESETCONTENT          : Core.Strings.VarString = '205';
  HTTP_SC_PARTIALCONTENT         : LongInt = 206;
  HTTP_SCS_PARTIALCONTENT        : Core.Strings.VarString = '206';
  HTTP_SC_MULTIPLECHOICES        : LongInt = 300;
  HTTP_SCS_MULTIPLECHOICES       : Core.Strings.VarString = '300';
  HTTP_SC_MOVEDPERMANENTLY       : LongInt = 301;
  HTTP_SCS_MOVEDPERMANENTLY      : Core.Strings.VarString = '301';
  HTTP_SC_FOUND                  : LongInt = 302;
  HTTP_SCS_FOUND                 : Core.Strings.VarString = '302';
  HTTP_SC_SEEOTHER               : LongInt = 303;
  HTTP_SCS_SEEOTHER              : Core.Strings.VarString = '303';
  HTTP_SC_NOTMODIFIED            : LongInt = 304;
  HTTP_SCS_NOTMODIFIED           : Core.Strings.VarString = '304';
  HTTP_SC_USEPROXY               : LongInt = 305;
  HTTP_SCS_USEPROXY              : Core.Strings.VarString = '305';
  HTTP_SC_TEMPORARYREDIRECT      : LongInt = 307;
  HTTP_SCS_TEMPORARYREDIRECT     : Core.Strings.VarString = '307';
  HTTP_SC_BADREQUEST             : LongInt = 400;
  HTTP_SCS_BADREQUEST            : Core.Strings.VarString = '400';
  HTPP_SC_UNAUTHORIZED           : LongInt = 401;
  HTPP_SCS_UNAUTHORIZED          : Core.Strings.VarString = '401';
  HTTP_SC_PAYMENTREQUIRED        : LongInt = 402;
  HTTP_SCS_PAYMENTREQUIRED       : Core.Strings.VarString = '402';
  HTPP_SC_FORBIDDEN              : LongInt = 403;
  HTPP_SCS_FORBIDDEN             : Core.Strings.VarString = '403';
  HTTP_SC_NOTFOUND               : LongInt = 404;
  HTTP_SCS_NOTFOUND              : Core.Strings.VarString = '404';
  HTTP_SC_METHODNOTFOUND         : LongInt = 405;
  HTTP_SCS_METHODNOTFOUND        : Core.Strings.VarString = '405';
  HTTP_SC_NOTACCEPTABLE          : LongInt = 406;
  HTTP_SCS_NOTACCEPTABLE         : Core.Strings.VarString = '406';
  HTTP_SC_PROXYAUTHREQUIRED      : LongInt = 407;
  HTTP_SCS_PROXYAUTHREQUIRED     : Core.Strings.VarString = '407';
  HTTP_SC_REQUESTTIMEOUT         : LongInt = 408;
  HTTP_SCS_REQUESTTIMEOUT        : Core.Strings.VarString = '408';
  HTTP_SC_CONFLICT               : LongInt = 409;
  HTTP_SCS_CONFLICT              : Core.Strings.VarString = '409';
  HTTP_SC_GONE                   : LongInt = 410;
  HTTP_SCS_GONE                  : Core.Strings.VarString = '410';
  HTTP_SC_LENGTHREQUIRED         : LongInt = 411;
  HTTP_SCS_LENGTHREQUIRED        : Core.Strings.VarString = '411';
  HTTP_SC_PRECONDITIONFAILED     : LongInt = 412;
  HTTP_SCS_PRECONDITIONFAILED    : Core.Strings.VarString = '412';
  HTTP_SC_ENTITYTOOLARGE         : LongInt = 413;
  HTTP_SCS_ENTITYTOOLARGE        : Core.Strings.VarString = '413';
  HTTP_SC_URITOOLONG             : LongInt = 414;
  HTTP_SCS_URITOOLONG            : Core.Strings.VarString = '414';
  HTTP_SC_UNSUPPORTEDMEDIATYPE   : LongInt = 415;
  HTTP_SCS_UNSUPPORTEDMEDIATYPE  : Core.Strings.VarString = '415';
  HTTP_SC_RANGENOTSATISFIABLE    : LongInt = 416;
  HTTP_SCS_RANGENOTSATISFIABLE   : Core.Strings.VarString = '416';
  HTTP_SC_EXPECTATIONFAILED      : LongInt = 417;
  HTTP_SCS_EXPECTATIONFAILED     : Core.Strings.VarString = '417';
  HTTP_SC_INTERNALSERVERERROR    : LongInt = 500;
  HTTP_SCS_INTERNALSERVERERROR   : Core.Strings.VarString = '500';
  HTTP_SC_NOTIMPLEMENTED         : LongInt = 501;
  HTTP_SCS_NOTIMPLEMENTED        : Core.Strings.VarString = '501';
  HTTP_SC_BADGATEWAY             : LongInt = 502;
  HTTP_SCS_BADGATEWAY            : Core.Strings.VarString = '502';
  HTTP_SC_SERVICEUNAVAILABLE     : LongInt = 503;
  HTTP_SCS_SERVICEUNAVAILABLE    : Core.Strings.VarString = '503';
  HTTP_SC_GATEWAYTIMEOUT         : LongInt = 504;
  HTTP_SCS_GATEWAYTIMEOUT        : Core.Strings.VarString = '504';
  HTTP_SC_VERSIONNOTSUPPORTED    : LongInt = 505;
  HTTP_SCS_VERSIONNOTSUPPORTED   : Core.Strings.VarString = '505';

  ctUnknown                      = 'unknown/unknown';
  ctCSS                          = 'text/css';
  ctHTML                         = 'text/html';
  ctText                         = 'text/plain';
  ctRTF                          = 'text/rtf';
  ctXML                          = 'text/xml';
  ctJPG                          = 'image/jpeg';
  ctGIF                          = 'image/gif';
  ctBMP                          = 'image/bmp';
  ctPNG                          = 'image/png';
  ctIcon                         = 'image/ico';
  ctTIFF                         = 'image/tiff';
  ctSVG                          = 'image/svg+xml';
  ctEMZ                          = 'image/x-emz';
  ctMPG                          = 'video/mpeg';
  ctAVI                          = 'video/avi';
  ctQT                           = 'video/quicktime';
  ctStream                       = 'application/octet-stream';
  ctBinary                       = 'application/binary';
  ctPDF                          = 'application/pdf';
  ctPostscript                   = 'application/postscript';
  ctFlash                        = 'application/x-shockwave-flash';
  ctBasicAudio                   = 'audio/basic';
  ctMP3                          = 'audio/mpeg';
  ctMP4A                         = 'audio/mpeg';
  ctOGGAudio                     = 'audio/ogg';
  ctFLV                          = 'video/x-flv';
  ctWebMVideo                    = 'video/webm';
  ctOGGVideo                     = 'video/ogg';
  ctM4V                          = 'video/m4v';
  ctMP4V                         = 'video/mp4';
  ctMIDI                         = 'audio/mid';
  ctAIFF                         = 'audio/x-aiff';
  ctRA                           = 'audio/x-realaudio';
  ctURLEncoded                   = 'application/x-www-form-urlencoded';
  ctZIP                          = 'application/zip';
  ctJava                         = 'application/java';
  ctMIME                         = 'application/base64';
  ctJavaScript                   = 'application/javascript';
  ctPascal                       = 'text/x-source-pascal';
  ctCPP                          = 'text/x-source-cpp';
  ctINI                          = 'text/x-windows-ini';
  ctBAT                          = 'text/x-windows-bat';
  ctEvent                        = 'text/event-stream';
  ctCacheManifest                = 'text/cache-manifest';
  ctFontTTF                      = 'application/x-font-ttf';
  ctFontWoff                     = 'application/font-woff';
  ctFontWoff2                    = 'application/font-woff2';

  ctImage                        : TImageMediaList=(
    ctJPG,           // 0
    ctGIF,           // 1
    ctBMP,           // 2
    ctPNG,           // 3
    ctSVG            // 4
  );
  ctVideoTransforms              : TVideoTransformList=(
    ctAVI,           // 0,
    ctMPG,           // 1,
    ctQT             // 2
  );
  ctImageTransforms              : TImageTransformList=(
    ctJPG,           // 0
    ctGIF,           // 1
    ctBMP,           // 2
    ctPNG            // 3
  );
  ctMedia                        : TMediaList=(
    ctMPG,           // 0
    ctAVI,           // 1
    ctQT,            // 2
    ctFlash,         // 3
    ctFLV,           // 4
    ctBasicAudio,    // 5
    ctMP3,           // 6
    ctOGGAudio,      // 7
    ctOGGVideo,      // 8
    ctMP4V,          // 9
    ctMP4A,          // 10
    ctM4V,           // 11
    ctMIDI,          // 12
    ctAIFF,          // 13
    ctRA,            // 14
    ctWebMVideo,     // 15
    ctPDF            // 16
  );

  ctDefaultExtensions            : TDefaultContentTypes=(
    (ID: 0; Verified:false; Ext: 'xml';  Kind: ctXML; SubType: ''),                    // 0
    (ID: 0; Verified:false; Ext: 'css';  Kind: ctCSS; SubType: ''),                    // 1
    (ID: 0; Verified:false; Ext: 'kw';   Kind: ctHTML; SubType: ''),                   // 2
    (ID: 0; Verified:false; Ext: 'srs';  Kind: ctHTML; SubType: ''),                   // 3
    (ID: 0; Verified:false; Ext: 'htm';  Kind: ctHTML; SubType: ''),                   // 4
    (ID: 0; Verified:false; Ext: 'html'; Kind: ctHTML; SubType: ''),                   // 5
    (ID: 0; Verified:false; Ext: 'jpg';  Kind: ctJPG; SubType: ''),                    // 6
    (ID: 0; Verified:false; Ext: 'jpeg'; Kind: ctJPG; SubType: ''),                    // 7
    (ID: 0; Verified:false; Ext: 'emz';  Kind: ctEMZ; SubType: ''),                    // 8
    (ID: 0; Verified:false; Ext: 'gif';  Kind: ctGIF; SubType: ''),                    // 9
    (ID: 0; Verified:false; Ext: 'png';  Kind: ctPNG; SubType: ''),                    // 10
    (ID: 0; Verified:false; Ext: 'ico';  Kind: ctIcon; SubType: ''),                   // 11
    (ID: 0; Verified:false; Ext: 'bmp';  Kind: ctBMP; SubType: ''),                    // 12
    (ID: 0; Verified:false; Ext: 'svg';  Kind: ctSVG; SubType: ''),                    // 13
    (ID: 0; Verified:false; Ext: 'txt';  Kind: ctText; SubType: ''),                   // 14
    (ID: 0; Verified:false; Ext: 'mpeg'; Kind: ctMPG; SubType: ''),                    // 15
    (ID: 0; Verified:false; Ext: 'mpg';  Kind: ctMPG; SubType: ''),                    // 16
    (ID: 0; Verified:false; Ext: 'pdf';  Kind: ctPDF; SubType: ''),                    // 17
    (ID: 0; Verified:false; Ext: 'exe';  Kind: ctBinary; SubType: ''),                 // 18
    (ID: 0; Verified:false; Ext: 'ps';   Kind: ctPostScript; SubType: ''),             // 19
    (ID: 0; Verified:false; Ext: 'avi';  Kind: ctAVI; SubType: ''),                    // 20
    (ID: 0; Verified:false; Ext: 'qt';   Kind: ctQT; SubType: ''),                     // 21
    (ID: 0; Verified:false; Ext: 'mov';  Kind: ctQT; SubType: ''),                     // 22
    (ID: 0; Verified:false; Ext: 'au';   Kind: ctBasicAudio; SubType: ''),             // 23
    (ID: 0; Verified:false; Ext: 'wav';  Kind: ctBasicAudio; SubType: ''),             // 24
    (ID: 0; Verified:false; Ext: 'mp3';  Kind: ctMP3; SubType: ''),                    // 25
    (ID: 0; Verified:false; Ext: 'mp2';  Kind: ctMP3; SubType: ''),                    // 26
    (ID: 0; Verified:false; Ext: 'mp4';  Kind: ctMP4V; SubType: ''),                   // 27
    (ID: 0; Verified:false; Ext: 'm4p';  Kind: ctMP4A; SubType: ''),                   // 28
    (ID: 0; Verified:false; Ext: 'm4v';  Kind: ctM4V; SubType: ''),                    // 29
    (ID: 0; Verified:false; Ext: 'flv';  Kind: ctFLV; SubType: ''),                    // 30
    (ID: 0; Verified:false; Ext: 'webm'; Kind: ctWebMVideo; SubType: ''),              // 31
    (ID: 0; Verified:false; Ext: 'oga';  Kind: ctOGGAudio; SubType: ''),               // 32
    (ID: 0; Verified:false; Ext: 'ogg';  Kind: ctOGGAudio; SubType: ''),               // 33
    (ID: 0; Verified:false; Ext: 'ogv';  Kind: ctOGGVideo; SubType: ''),               // 34
    (ID: 0; Verified:false; Ext: 'pdf';  Kind: ctPDF; SubType: ''),                    // 35
    (ID: 0; Verified:false; Ext: 'ra';   Kind: ctRA; SubType: ''),                     // 36
    (ID: 0; Verified:false; Ext: 'tif';  Kind: ctTIFF; SubType: ''),                   // 37
    (ID: 0; Verified:false; Ext: 'tiff'; Kind: ctTIFF; SubType: ''),                   // 38
    (ID: 0; Verified:false; Ext: 'zip';  Kind: ctZIP; SubType: ''),                    // 39
    (ID: 0; Verified:false; Ext: 'ini';  Kind: ctINI; SubType: ''),                    // 40
    (ID: 0; Verified:false; Ext: 'bat';  Kind: ctBAT; SubType: ''),                    // 41
    (ID: 0; Verified:false; Ext: 'js';   Kind: ctJavaScript; SubType: ''),             // 42
    (ID: 0; Verified:false; Ext: 'swf';  Kind: ctFlash; SubType: ''),                  // 43
    (ID: 0; Verified:false; Ext: 'appcache';  Kind: ctCacheManifest; SubType: ''),     // 44
    (ID: 0; Verified:false; Ext: 'ttf';  Kind: ctFontTTF; SubType: ''),                // 45
    (ID: 0; Verified:false; Ext: 'woff'; Kind:ctFontWoff; SubType: ''),                // 46
    (ID: 0; Verified:false; Ext: 'woff2'; Kind:ctFontWoff2; SubType: '')               // 47
    );

Const
  KeepAlive                      : Array[Boolean] of UTF8String=('Keep-Alive','Close');
  HTTP                           : Array[Boolean] of UTF8String=('http','https');
  PUBLIC_CACHE                   : UTF8String = 'public';
  PRIVATE_CACHE                  : UTF8String = 'private';
  MUST_REVALIDATE                : UTF8String = 'must-revalidate';
  CACHE_AGE                      : UTF8String = 'max-age=';
  NO_CACHE                       : UTF8String = 'no-cache';
  METHOD_GET                     : UTF8String = 'GET';
  METHOD_PUT                     : UTF8String = 'PUT';
  METHOD_POST                    : UTF8String = 'POST';
  METHOD_HEAD                    : UTF8String = 'HEAD';
  METHOD_OPTIONS                 : UTF8String = 'OPTIONS';

  Deflate                        : UTF8String = 'deflate';
  fieldAllow                     : UTF8String = 'Allow';
  fieldAccept                    : UTF8String = 'Accept';
  fieldAcceptCharset             : UTF8String = 'Accept-Charset';
  fieldAcceptEncoding            : UTF8String = 'Accept-Encoding';
  fieldAcceptLanguage            : UTF8String = 'Accept-Language';
  fieldAcceptRanges              : UTF8String = 'Accept-Ranges';
  fieldACLMaxAge                 : UTF8String = 'Access-Control-Max-Age';
  fieldACL                       : UTF8String = 'Access-Control';
  fieldACLAllowOrigin            : UTF8String = 'Access-Control-Allow-Origin';
  fieldACLAllowHeaders           : UTF8String = 'Access-Control-Allow-Headers';
  fieldACLExposeHeaders          : UTF8String = 'Access-Control-Expose-Headers';
  fieldACLAllowMethods           : UTF8String = 'Access-Control-Allow-Methods';
  fieldACLAllowMethod            : UTF8String = 'Access-Control-Allow-Method';
  fieldACLRequestHeaders         : UTF8String = 'Access-Control-Request-Headers';
  fieldACLRequestMethod          : UTF8String = 'Access-Control-Request-Method';
  fieldACLAllowCredentials       : UTF8String = 'Access-Control-Allow-Credentials';
  fieldAuthorization             : UTF8String = 'Authorization';
  fieldCookie                    : UTF8String = 'Cookie';
  fieldDate                      : UTF8String = 'Date';
  fieldExpires                   : UTF8String = 'Expires';
  fieldFrom                      : UTF8String = 'From';
  fieldVary                      : UTF8String = 'Vary';
  fieldHost                      : UTF8String = 'Host';
  fieldIfMatch                   : UTF8String = 'If-Match';
  fieldIfNoneMatch               : UTF8String = 'If-None-Match';
  fieldUpgrade                   : UTF8String = 'Upgrade';
  fieldOrigin                    : UTF8String = 'Origin';
  fieldConnection                : UTF8String = 'Connection';
  fieldContentType               : UTF8String = 'Content-Type';
  fieldContentDisposition        : UTF8String = 'Content-Disposition';
  fieldContentEncoding           : UTF8String = 'Content-Encoding';
  fieldContentTransferEncoding   : UTF8String = 'Conent-Transfer-Encoding';
  fieldContentLanguage           : UTF8String = 'Content-Language';
  fieldContentLength             : UTF8String = 'Content-Length';
  fieldContentRange              : UTF8String = 'Content-Range';

  fieldCacheControl              : UTF8String = 'Cache-Control';

  fieldIfModifiedSince           : UTF8String = 'If-Modified-Since';
  fieldIfRange                   : UTF8String = 'If-Range';
  fieldRemoteIP                  : UTF8String = 'Remote-IP';
  fieldLastModified              : UTF8String = 'Last-Modified';
  fieldLocation                  : UTF8String = 'Location';
  fieldPragma                    : UTF8String = 'Pragma';
  fieldRange                     : UTF8String = 'Range';
  fieldReferer                   : UTF8String = 'Referer';
  fieldRetryAfter                : UTF8String = 'Retry-After';
  fieldServer                    : UTF8String = 'Server';
  fieldSetCookie                 : UTF8String = 'Set-Cookie';
  fieldUserAgent                 : UTF8String = 'User-Agent';
  fieldWWWAuthenticate           : UTF8String = 'WWW-Authenticate';

  fieldWebSockAccept             : UTF8String = 'Sec-WebSocket-Accept';
  fieldWebSockKey                : UTF8String = 'Sec-WebSocket-Key';
  fieldWebSockKey1               : UTF8String = 'Sec-WebSocket-Key1';
  fieldWebSockKey2               : UTF8String = 'Sec-WebSocket-Key2';
  fieldWebSockProtocol           : UTF8String = 'Sec-WebSocket-Protocol';
  fieldWebSockOrigin             : UTF8String = 'Sec-WebSocket-Origin';
  fieldWebSockLocation           : UTF8String = 'Sec-WebSocket-Location';
  fieldParameters                : UTF8String = 'Params';
  fieldETag                      : UTF8String = 'ETag';
  fieldID                        : UTF8String = 'ID';    // ID of transaction
  fieldResourceID                : UTF8String = 'RCID';  // ID of device
  fieldEUK                       : UTF8String = 'EUK';   // End User Kind
  fieldAuth                      : UTF8String = 'AUTH';  // End User Auth UTF8String
  fieldAccount                   : UTF8String = 'User';  // User Name
  fieldCode                      : UTF8String = 'CODE';  // Result Code
  fieldSearch                    : UTF8String = 'SRCH';  // Search Term in header
  fieldScale                     : UTF8String = 'SCALE'; // Items per page
  fieldDepth                     : UTF8String = 'DEPTH'; // Depth of query
  fieldNameSpace                 : UTF8String = 'NS';    // NameSpace of query / response
  fieldKind                      : UTF8String = 'KIND';  // generic Kind metric
  GUID_WEBSOCKET                 : UTF8String = '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';

  function  ContentTypeFromFile(var List:TContentTypes; sExtension:Core.Strings.VarString):Core.Strings.VarString;
  function  ContentSubTypeFromName(var List:TContentTypes; sName:Core.Strings.VarString):Core.Strings.VarString;


  procedure  Init(var Item:TMediaRange); overload;
  procedure  Init(var Item:THTTPMedia); overload;
  procedure  Init(var Item:TContentTypes); overload;
  procedure  Init(var Item:TContentType); overload;
  procedure  Empty(var Item:TMediaRange); overload;
  procedure  Empty(Var Item:THTTPMedia); overload;
  procedure  Empty(Var Item:TContentTypes); overload;
  procedure  Empty(Var Item:TContentType); overload;

  procedure  Done(Var Item:TContentTypes); overload;
  procedure  Done(Var Item:TContentType); overload;
  procedure  Done(Var Item:THTTPMedia); overload;

  function   toString(var Item:TMediaRange):Core.Strings.VarString;

  procedure  Add(Var List:TContentTypes; var Item:TContentType); overload;
  procedure  Add(Var List:TContentTypes; ItemP:PContentType); overload;
  procedure  Delete(Var List:TContentTypes; ItemP:PContentType); overload;

  procedure  Copy(Var Source,Destination:TContentType); overload;
  procedure  Invalidate(var List:TContentTypes);
  function   IndexOf(Var List:TContentTypes; ID:Int64): LongInt; overload;
  function   IndexOf(Var List:TContentTypes; Ext:Core.Strings.VarString): LongInt; overload;
  function   IndexOf(Var List:TDefaultContentTypes; Ext:Core.Strings.VarString): LongInt; overload;

  function   Exchange(srcIndex,destIndex:LongInt; var Items:TContentTypes):boolean;
  procedure  SetSize(Var List:TContentTypes; iSize:LongInt); overload;
  procedure  Merge(Var Source:TDefaultContentTypes; Var Destination:TContentTypes); overload;

  function   Encode(const sMethod, sURI: Core.Strings.VarString; var Parameters:Core.Arrays.Types.VarString; Refactor:TStream): Core.Strings.VarString; overload;

  function  HTTP_DateTime(const dtValue:TDateTime; const bShowDay:Boolean=True): Core.Strings.VarString;
  function  HTTP_Time(const dtValue:TDateTime; const bShowSeconds:Boolean=True): Core.Strings.VarString;

  function  Validate(var List:TContentTypes; Kind,Ext:Core.Strings.VarString; const Criteria:TContentTypeValidatation; ItemP:PContentType=nil):Boolean; overload;
  function  Prepare(Path:Core.Strings.VarString):Core.Strings.VarString; overload;

  function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TImageMediaList):LongInt; overload;
  function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TMediaList):LongInt; overload;
  function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TImageTransformList):LongInt; overload;
  function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TVideoTransformList):LongInt; overload;

implementation
uses SysUtils;

function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TImageMediaList):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Media) do begin
    if SameText(Media[iLcv],ContentType) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TMediaList):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Media) do begin
    if SameText(Media[iLcv],ContentType) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TImageTransformList):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Media) do begin
    if SameText(Media[iLcv],ContentType) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function  IndexOf(var ContentType:Core.Strings.VarString; var Media:TVideoTransformList):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Media) do begin
    if SameText(Media[iLcv],ContentType) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;


function  Validate(var List:TContentTypes; Kind,Ext:Core.Strings.VarString; const Criteria:TContentTypeValidatation; ItemP:PContentType=nil):Boolean;

  function PushValidateEditing:Boolean;
  var
    iLcv:LongInt;
    iCount:LongInt;
  begin
    Result:=(System.Length(Kind)>0) and (System.Length(Ext)>0);
    if Result and (ItemP<>nil) then begin
      iLcv:=0; iCount:=System.Length(List);
      While (iLcv<iCount) and (Result) do begin
        if SameText(List[iLcv]^.Ext,Ext) and ( (ItemP=nil) or  (List[iLcv]^.ID<>ItemP^.ID) ) then
          Result:=false;
        Inc(iLcv);
      end;
    end;
  end;

begin
  Case Criteria of
    ctvAdd  : Result:=(System.Length(Ext)>0) and (IndexOf(List,Ext)=-1);
    ctvEdit : Result:=PushValidateEditing;
  end;
end;

function  Prepare(Path:Core.Strings.VarString):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  Result:=Path;
  iLen:=System.Length(Path);
  if (iLen>0) and (Result[iLen]<>'/') then
    Result+='/';
end;

function HTTP_Time(const dtValue:TDateTime; const bShowSeconds:Boolean=True): Core.Strings.VarString;
var
  iHours                         : Word;
  iMinutes                       : Word;
  iSeconds                       : Word;
  iMilliseconds                  : Word;
begin
  DecodeTime(dtValue, iHours, iMinutes, iSeconds, iMilliseconds);
  Result :=Concat(
    Format('%.2d',[iHours]),':',
    Format('%.2d',[iMinutes])
  );
  if bShowSeconds then
    Result+=Concat(':',Format('%.2d',[iSeconds]));
  Result+=' GMT';
end;

function HTTP_DateTime(const dtValue: TDateTime; const bShowDay:Boolean=True): Core.Strings.VarString;
var
  iYear                          : Word;
  iMonth                         : Word;
  iDay                           : Word;
begin
  SetLength(Result,0);
  DecodeDate(dtValue, iYear, iMonth, iDay);
  if bShowDay then
      Result+=Concat(Core.Utils.Time.Day_Short[DayOfWeek(dtValue)],', ');
  Result+=Concat(
    Format('%.2d',[iDay]),' ',
    Core.Utils.Time.Month_Short[iMonth],' ',
    IntToStr(iYear) + ' ',
    HTTP_Time(dtValue,true)
  );
end;

function Encode(const sMethod, sURI: Core.Strings.VarString; var Parameters:Core.Arrays.Types.VarString; Refactor:TStream): Core.Strings.VarString;
begin
  if Length(Parameters)=0 then begin
    Result:=Concat(sMethod,#32,sURI,#32,HTTP_VERSION);
  end else begin
    Result:=Concat(sMethod,#32,sURI,'?',Core.Arrays.VarString.toString(Parameters,'&',Refactor),#32,HTTP_VERSION);
  end;
end;

function  ContentTypeFromFile(var List:TContentTypes; sExtension:Core.Strings.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=Length(sExtension);
  if (iCount>0) and (sExtension[1]='.') then
    System.Delete(sExtension,1,1);
  iCount:=System.Length(List);
  iLcv:=0; Result:=ctUnknown;
  While (iLcv<iCount) and (Result=ctUnknown) do begin
    If SysUtils.SameText(List[iLcv]^.Ext,sExtension) then
      Result:=List[iLcv]^.Kind;
    Inc(iLcv);
  end;
end;

function  ContentSubTypeFromName(var List:TContentTypes; sName:Core.Strings.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  sName:=Core.Utils.Files.Extract(sName,efeoNone);
  iCount:=System.Length(List);
  iLcv:=0;
  Result:='unknown';
  While (iLcv<iCount) and (Result=ctUnknown) do begin
    If SysUtils.SameText(List[iLcv]^.Ext,sName) then begin
      Result:=List[iLcv]^.SubType;
      Break;
    end;
    Inc(iLcv);
  end;
end;


procedure  Init(var Item:TMediaRange);
begin
  Item.Start:=0;
  Item.Stop:=0;
  Item.Size:=0;
end;

procedure  Init(var Item:THTTPMedia);
begin
  SetLength(Item.URI,0);
  SetLength(Item.ContentType,0);
  SetLength(Item.ETag,0);
  Item.Modified:=0;
  Item.ContentLength:=0;
  Item.State:=msIdle;
  Item.Content:=TMemoryStream.Create();
  Init(Item.Range);
end;

procedure  Init(var Item:TContentTypes);
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=System.Length(ctDefaultExtensions);
  SetSize(Item,iCount);
  For iLcv:=0 to iCount-1 do
    Copy(ctDefaultExtensions[iLcv],Item[iLcv]^);
end;

procedure Init(Var Item:TContentType);
begin
  Item.ID:=0;
  Item.Verified:=false;
  SetLength(Item.Ext,0);
  SetLength(Item.Kind,0);
  SetLength(Item.SubType,0);
end;

procedure  Empty(var Item:TMediaRange);
begin
  Item.Start:=0;
  Item.Stop:=0;
  Item.Size:=0;
end;

function   toString(var Item:TMediaRange):Core.Strings.VarString;
begin
  Result:=Concat(IntToStr(Item.Start),'-',IntToStr(Item.Stop),'/',IntToStr(Item.Size));
end;

procedure  Empty(Var Item:THTTPMedia);
begin
  Item.ContentLength:=0;
  Item.Modified:=0;
  Item.Content.Size:=0;
  Item.State:=msIdle;
  SetLength(Item.URI,0);
  SetLength(Item.ETag,0);
  SetLength(Item.ContentType,0);
  Empty(Item.Range);
end;

function Exchange(srcIndex,destIndex:LongInt; var Items:TContentTypes):Boolean;
var
  iCount:LongInt;
  src,dest:PContentType;
begin
  Result:=false;
  iCount:=System.Length(Items);
  if (srcIndex=-1) or (destIndex=-1) or (srcIndex>=iCount) or (destIndex>=iCount) then exit;
  src:=Items[srcIndex];
  dest:=Items[destIndex];
  Items[srcIndex]:=dest;
  Items[destIndex]:=src;
  Result:=True;
end;

procedure SetSize(Var List:TContentTypes; iSize:LongInt);
var
  iLcv:LongInt;
  iLen:LongInt;
begin
  iLen:=Length(List);
  if (iSize<iLen) then begin
    // Going to Shrink the list
    While (iLen>iSize) do begin
      Done(List[iLen]^);
      Dispose(List[iLen]);
    end;
    SetLength(List,iSize);
  end else if (iSize>iLen) then begin
    // Going to Grow the list
    SetLength(List,iSize);
    For iLcv:=iLen-1 to iSize-1 do begin
      New(List[iLcv]);
      Init(List[iLcv]^);
    end;
  end;
end;


procedure  Empty(Var Item:TContentTypes);
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=System.Length(Item);
  For iLcv:=0 to iCount-1 do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

procedure  Empty(Var Item:TContentType);
begin
  Item.ID:=0;
  Empty(Item.Ext);
  Empty(Item.Kind);
  Empty(Item.SubType);
end;

procedure  Done(var Item:THTTPMedia);
begin
  Finalize(Item.URI);
  Finalize(Item.ETag);
  Finalize(Item.ContentType);
  FreeAndNil(Item.Content);
  Finalize(Item.Range);
  Finalize(Item);
end;

procedure  Done(Var Item:TContentTypes);
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=System.Length(Item);
  For iLcv:=0 to iCount-1 do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure  Done(Var Item:TContentType);
begin
  Done(Item.Ext);
  Done(Item.Kind);
  Done(Item.SubType);
  Finalize(Item);
end;

procedure  Add(Var List:TContentTypes; var Item:TContentType);
var
  itmP:PContentType;
begin
  New(itmP);
  Init(itmP^);
  Copy(Item,itmP^);

  Add(List,itmP);
end;

procedure  Add(Var List:TContentTypes; ItemP:PContentType);
var
  iDX:LongInt;
begin
  iDX:=System.Length(List);
  SetLength(List,iDX+1);
  List[idx]:=ItemP;
end;

procedure  Delete(Var List:TContentTypes; ItemP:PContentType);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iCount:LongInt;
begin
   for iLcv:=0 to High(List) do begin
     if List[iLcv]=ItemP then begin
       Done(List[iLcv]^);
       Dispose(List[iLcv]);
       for jLcv:=iLcv to High(List)-1 do
         List[jLcv]:=List[jLcv+1];
       iCount:=System.Length(List);
       System.SetLength(List,iCount-1);
       break;
     end;
   end;
end;

procedure  Copy(Var Source,Destination:TContentType);
begin
  Destination.ID:=Source.ID;
  Destination.Kind:=Source.Kind;
  Destination.Ext:=Source.Ext;
  Destination.SubType:=Source.SubType;
end;

procedure  Merge(Var Source:TDefaultContentTypes; var Destination:TContentTypes);
var
  iLcv:LongInt;
  iIndex:LongInt;
  iSourceLen:LongInt;
begin
  iSourceLen:=System.Length(Source);
  For iLcv:=0 to iSourceLen-1 do begin
    iIndex:=IndexOf(Destination,Source[iLcv].Ext);
    if iIndex=-1 then
      Add(Destination,Source[iLcv]);
  end;
end;

function   IndexOf(Var List:TDefaultContentTypes; Ext:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;

  for iLcv:=0 to High(List) do begin
    If SameText(List[iLcv].Ext,Ext) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function   IndexOf(Var List:TContentTypes; Ext:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(List) do begin
    If ( (List[iLcv]<>nil) and (SameText(List[iLcv]^.Ext,Ext)) )then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

function   IndexOf(Var List:TContentTypes; ID:Int64): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;

  for iLcv:=0 to High(List) do begin
    If (List[iLcv]<>nil) and (List[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      Break;
    end;
  end;

end;

procedure  Invalidate(var List:TContentTypes);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(List) do
    List[iLcv]^.Verified:=False;
end;

Constructor THTTPRequest.Create;
begin
  //InitCriticalSection(Lock);
  FData:=TMemoryStream.Create;
  FRefactor:=TMemoryStream.Create;
  FAuthSet:=False;
  Upgrade:=False;
  WebSocket:=False;
  SetLength(ETag,0);
  FURI_Callbacks[True]:=@_CB_URI_EMPTY_QUERY;
  FURI_Callbacks[False]:=@_CB_URI_QUERY;
  FAuth_Callbacks[True]:=@_CB_SetAuthoriztaion_Empty;
  FAuth_Callbacks[False]:=@_CB_SetAuthorization;
  Core.Arrays.VarString.Empty(Parameters);
  Core.Arrays.KeyString.Empty(Headers);
  Core.Arrays.KeyString.Empty(Cookies);
  Inherited Create;
end;

Destructor  THTTPRequest.Destroy;
begin
  //DoneCriticalSection(Lock);
  FreeAndNil(FData);
  FreeAndNil(FRefactor);
  Done(Parameters);
  Done(Headers);
  Done(Cookies);
  Inherited Destroy;
end;

procedure  THTTPRequest._CB_SetAuthorization;
var
  saData:Core.Arrays.Types.VarString;
begin
  FAuthSet:=True;
  Authorization:=Core.Arrays.KeyString.GetItemByKey(Headers,'Authorization');
  If Core.Strings.Pos(' ',Authorization)=6 then begin
    Authorization := Encryption.Base64.Decode(Extract(Authorization, 7));
    Core.Arrays.VarString.fromString(saData,Authorization,':');
    Try
      Case Length(saData) of
        2: begin
             Authorization:='Basic';
             Username:=saData[0];
             Password:=saData[1];
           end;
        1: begin
             Authorization:='Basic';
             Username:=saData[0];
             Password:='';
           end;
        else begin
          Authorization:='';
          Username:='';
          Password:='';
        end;
      end;
    Finally
      SetLength(saData,0);
    end;
  end else begin
    Authorization := '';
    Username:='';
    Password:='';
  end;
end;

procedure  THTTPRequest._CB_SetAuthoriztaion_Empty;
begin
end;

procedure  THTTPRequest.SetAuthorization;
begin
  FAuth_Callbacks[FAuthSet];
end;

procedure THTTPRequest.AddParameter(Value:Core.Strings.VarString);
begin
  Core.Arrays.VarString.Add(Parameters,Value);
end;

Procedure  THTTPRequest._CB_URI_EMPTY_QUERY(Const Loc:LongInt);
begin
  QUERY:='';
end;

procedure  THTTPRequest._CB_URI_QUERY(Const Loc:LongInt);
var
  iLength:LongInt;
begin
  iLength:=Length(URI);
  QUERY:=System.Copy(URI,Loc+1,iLength-Loc);
  SetLength(URI,Loc-1);
end;

Function  THTTPRequest.FullProtocol:Core.Strings.VarString;
begin
  Result := Concat(FProtocol,'/',IntToStr(FMajorVersion),'.',IntToStr(FMinorVersion));
end;

Procedure THTTPRequest.Reset();
begin
  Close:=False;
  Empty(Parameters);
  Empty(Cookies);
  Empty(Headers);
  FData.Size:=0;
  FRefactor.Size:=0;
  ContentLength:=0;
  SetLength(ETag,0);
  SetLength(FProtocol,0);
  SetLength(FHeaders,0);
  SetLength(Authorization,0);
  SetLength(Username,0);
  SetLength(UserAgent,0);
  SetLength(Referer,0);
  SetLength(Password,0);
  SetLength(Method,0);
  SetLength(Host,0);
  SetLength(URI,0);
  SetLength(WebSocketKey,0);
  SetLength(Query,0);
  SetLength(ContentType,0);
end;

Function  THTTPRequest.HeaderAsString():Core.Strings.VarString;
begin
  Result:=Concat(
    RSR.HTTP.Encode(Method, URI, Parameters,FRefactor),#13#10,
    Core.Arrays.KeyString.toString(Headers,FRefactor,': ',#13#10),
    #13#10#13#10
  );
end;

procedure THTTPRequest.SetCommand(Value:Core.Strings.VarString);
{
 <Method> <URI> <Protocol>
 Examples:
   GET      /                  HTTP/1.1           (Valid HTTP/1.1 request)
   CONNECT  host:443           HTTP/1.1           (URI can be a host)
   GET      http://host:8080/  HTTP/1.1           (Protocol added to URI for proxies)
   GET      host:6636          ICY                (Shoutcast over HTTP request)
   GET      /index/pic.gif                       (Protocol ommitted pre HTTP/1.0)
}
var
  iFirstQ:LongInt;
  saRequest:Core.Arrays.Types.VarString;

  procedure PushNullVersion;
  begin
    FMajorVersion:=1;
    FMinorVersion:=1;
  end;

  procedure PushVersion;
  var
    saVersion:Core.Arrays.Types.VarString;
  begin
    FProtocol:=saRequest[0];
    Core.Arrays.VarString.fromString(saVersion,saRequest[1],'.');
    Try
      FMajorVersion:=StrToIntDef(Core.Arrays.VarString.Parameter(saVersion,1),1);
      FMinorVersion:=StrToIntDef(Core.Arrays.VarString.Parameter(saVersion,1),1);
    Finally
      SetLength(saVersion,0);
    end;
  end;

begin
  FAuthSet:=False;
  Core.Arrays.VarString.fromString(saRequest,Value,#32);
  Try
    Method:=Core.Arrays.VarString.Parameter(saRequest,1);
    URI:=HTTPDefs.HTTPDecode(Core.Arrays.VarString.Parameter(saRequest,2));
    FProtocol:=Core.Arrays.VarString.Parameter(saRequest,3);
  Finally
    SetLength(saRequest,0);
  end;
  Core.Arrays.VarString.fromString(saRequest,FProtocol,'/');
  Try
    Case Length(saRequest) of
      0,1: PushNullVersion;
      2:PushVersion;
    end;
  Finally
    SetLength(saRequest,0);
  end;
  iFirstQ:=Core.Strings.Pos('?',URI);
  FURI_Callbacks[iFirstQ=0](iFirstQ);
end;

procedure THTTPRequest.SetCommand(Var Buffer:Core.Arrays.Types.Bytes; Const Length:LongInt);
begin
  SetCommand(Core.Arrays.Bytes.toString(Buffer,Length));
end;


procedure THTTPRequest.SetHeaders();
var
  iCount,iLcv:LongInt;
begin
  Core.Arrays.VarString.Empty(Parameters);
  Core.Arrays.KeyString.Empty(Cookies);
  if (System.Length(Query)>0) then
    Core.Arrays.VarString.fromString(Parameters,Query,'&',[soClearList]);
  iCount:=System.Length(Headers);
  Close:=SysUtils.SameText(Core.Arrays.KeyString.GetItemByKey(Headers,fieldConnection,iCount),'Close');
  Upgrade:=SysUtils.SameText(Core.Arrays.KeyString.GetItemByKey(Headers,fieldConnection,iCount),'Upgrade');
  WebSocket:=SysUtils.SameText(Core.Arrays.KeyString.GetItemByKey(Headers,fieldUpgrade,iCount),'WebSocket');
  WebSocketKey:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldWebSockKey,iCount);
  ContentType:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldContentType,iCount);
  UserAgent:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldUserAgent,iCount);
  Referer:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldReferer,iCount);
  Host:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldHost,iCount);
  ContentLength:=Core.Arrays.KeyString.GetItemAsQWord(Headers,fieldContentLength,iCount);
  ETag:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldIfNoneMatch,iCount);
  if System.Length(ETag)=0 then begin
    ETag:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldIfMatch,iCount);
    if System.Length(ETag)=0 then begin
      ETag:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldIfRange,iCount);
      if System.Length(ETag)=0 then begin
        ETag:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldETag,iCount);
      end;
    end;
  end;
  if System.Length(ETag)>0 then
    Core.Arrays.KeyString.Update(Headers,fieldETag,ETag);
  iLcv:=Core.Arrays.KeyString.IndexOf(@Headers,fieldCookie);
  if iLcv<>-1 then
    Core.Arrays.KeyString.fromString(Cookies,Headers[iLcv]^.Value,'=','; ');
end;

procedure THTTPRequest.SetHeaders(Value:Core.Strings.VarString);
begin
  Core.Arrays.KeyString.Empty(Cookies);
  Core.Arrays.KeyString.fromString(Headers,Value,': ',#13#10);
  SetHeaders();
end;

function  THTTPRequest.toBuffer(Var Buffer:TDataBuffer):Int64;
var
  sWrite:Core.Strings.VarString;
  iWrite:QWord;
begin
  Result:=0;
  sWrite:=HeaderAsString();
  iWrite:=System.Length(sWrite);

  EnterCriticalSection(Buffer.Lock);
  Try
    Buffer.Stream.Position:=Buffer.posWrite;
    Buffer.Stream.Write(sWrite[1],iWrite);
    Inc(Buffer.posWrite,iWrite);
    RSR.Write(FData,Buffer);
  finally
    LeaveCriticalSection(Buffer.Lock);
  end;
  Result:=iWrite+FData.Size;
end;

function  THTTPRequest.fromBuffer(Var Buffer:TDataBuffer):Boolean;
var
  iCount:QWord;
  iStart:Int64;
  iHeaderLoc:LongInt;
begin
  FData.Clear;
  Result:=false;
  EnterCriticalSection(Buffer.Lock);
  Try
    iHeaderLoc:=RSR.Pos(Buffer,HTTP_HEADER_SEP);
    if (iHeaderLoc>-1) then begin
      iStart:=0;
      SetCommand(Core.Streams.Readline(Buffer.Stream,iStart));
      // iStart is now the current position of the stream
      if (iHeaderLoc>=iStart) then // more to read
        iCount:=iHeaderLoc-iStart
      else // end of buffer
        iCount:=Buffer.posWrite-iStart;

      RSR.Extract(Buffer,iStart,iCount,':',HTTP_HEADER_DELIM,Headers);

      SetHeaders();
      if (WebSocket) then begin
        // Challenge could be in header or content... Check header 1st
        if ( System.Length(WebSocketKey)>0) then begin
          Result:=True;
        end else if (Buffer.Stream.Size>=(Buffer.posRead+8)) then begin
          Result:=RSR.Extract(Buffer,Buffer.PosRead,8,Challenge);
          Inc(Buffer.posRead,8);
        end;
        Buffer.posRead:=iHeaderLoc+HTTP_HEADER_SEP_LEN;
        RSR.Refactor(Buffer,FRefactor,Buffer.PosRead+ContentLength);
      end else begin
        if (ContentLength>0) then begin
          Result:=(Buffer.Stream.Size>=(iHeaderLoc+HTTP_HEADER_SEP_LEN+ContentLength));
          If Result then begin
            Buffer.posRead:=iHeaderLoc+HTTP_HEADER_SEP_LEN;
            RSR.Extract(Buffer,Buffer.posRead,ContentLength,FData);
            RSR.Refactor(Buffer,FRefactor,Buffer.PosRead+ContentLength);
          end;
        end else begin
          Result:=True;
          Buffer.posRead:=iHeaderLoc+HTTP_HEADER_SEP_LEN;
          RSR.Refactor(Buffer,FRefactor,Buffer.PosRead);
        end;
      end;
    end;
  finally
    LeaveCriticalSection(Buffer.Lock);
  end;
end;

Function  THTTPRequest.AsString():Core.Strings.VarString;
begin
  Result:=Concat(HeaderAsString(),Core.Streams.toString(FData));
end;

Constructor THTTPResponse.Create();
begin
  Head:=false;
  Protocol:='HTTP';
  Secure:=false;
  ConnectionHeader:=True;
  SendNullContentLengthHeader:=False;
  Major:=1;
  Minor:=1;
  FMediaP:=nil;
  FRefactor:=TMemoryStream.Create;
  FContent:=TMemoryStream.Create;
  Init(Cookies);
  Init(Headers);
  Inherited Create;
end;

Destructor  THTTPResponse.Destroy;
begin
  FreeAndNil(FRefactor);
  FreeAndNil(FContent);
  SetLength(Headers,0);
  Done(Cookies);
  Done(Headers);
  Inherited Destroy;
end;

function THTTPResponse.parameterAsQWord(Index:LongInt; const Default:QWord=0):QWord;
begin
  Result:=Default;
  if ((Index>-1) and (Index<System.Length(Parameters)) ) then
    Result:=StrToQwordDef(Parameters[Index],Default);
end;

function THTTPResponse.toStream(Stream:TStream):LongInt;
begin
  Result:=0;
  Stream.Size:=0;
  toStreamHeader(Stream);
  if (ContentLength>0) and (Head=false) then begin
    Case ContentKind of
      ckContent : Result:=Stream.CopyFrom(FContent,FContent.Size);
      ckString  : Result:=Core.Streams.Write(PString(ContentData)^,Stream);
      ckStream  : Result:=Stream.CopyFrom(TStream(ContentData),TStream(ContentData).Size);
    end;
  end;
end;

function THTTPResponse.toStreamHeader(Stream:TStream):LongInt;
var
  idxCntType:LongInt;
  iStartSize:Int64;
begin
  iStartSize:=Stream.Size;
  if (ContentLength>0) then begin
    idxCntType:=Core.Arrays.KeyString.IndexOf(@Headers,fieldContentType);
    if (idxCntType=-1) then
      Core.Arrays.KeyString.Update(Headers,fieldContentType,ContentType);
  end;
  if (System.Length(Server)>0) then
    Core.Arrays.KeyString.Update(Headers,fieldServer,Server);
  if (System.Length(Host)>0) then
    Core.Arrays.KeyString.Update(Headers,fieldHost,Host);
  if ConnectionHeader then
    Core.Arrays.KeyString.Update(Headers,fieldConnection,KeepAlive[Close]);
  if (System.Length(Parameters)>0) then
    Core.Arrays.KeyString.Update(Headers,fieldParameters,Core.Arrays.VarString.toString(Parameters,'&',FRefactor));

  Core.Streams.Write(Concat(Protocol,'/',IntToStr(Major),'.',IntToStr(Minor),' ',IntToStr(Code),' ', Status,#13#10),Stream);
  Core.Arrays.KeyString.toStream(Headers,Stream,': ',#13#10);

  Core.Streams.Write(#13#10,Stream);

  Result:=Stream.Size-iStartSize;
end;

function  THTTPResponse.fromBuffer(Var Buffer:TDataBuffer):Boolean;
var
  iCount:Int64;
  iStart:Int64;
  iHeaderLoc:LongInt;

  procedure ExtractContent;
  var
    idx:LongInt;
    Inflate:TInflater;
  begin
    if ContentLength>0 then
      RSR.Extract(Buffer,Buffer.posRead,ContentLength,FContent);
    RSR.Refactor(Buffer,FRefactor,Buffer.PosRead+ContentLength);

    idx:=Core.Arrays.KeyString.IndexOf(Headers,fieldContentEncoding);
    if (idx<>-1) then begin
      if SameText(Headers[idx]^.Value,RSR.HTTP.Deflate) then begin
        FContent.Position:=0;
        Inflate:=TInflater.Create(FContent,FRefactor,COMPRESS_BUFFER);
        try
          Inflate.Decompress();
          FContent.Size:=0;
          FRefactor.Position:=0;
          Core.Streams.Copy(FRefactor,FContent);
        finally
          FreeAndNil(Inflate);
        end;
        FRefactor.Size:=0;
      end;
    end;
  end;

begin
  FContent.Size:=0;
  Result:=false;
  iHeaderLoc:=RSR.Pos(Buffer,HTTP_HEADER_SEP);
  if (iHeaderLoc>-1) then begin
    iStart:=0;
    SetCommand(Core.Streams.Readline(Buffer.Stream,iStart));
    // iStart is now the current position of the stream
    if (iHeaderLoc>=iStart) then // more to read
      iCount:=iHeaderLoc-iStart
    else // end of buffer
      iCount:=Buffer.posWrite-iStart;
    RSR.Extract(Buffer,iStart,iCount,':',HTTP_HEADER_DELIM,Headers);
    SetHeaders();
    if (ContentLength>0) then begin
      Result:=(Buffer.Stream.Size>=(iHeaderLoc+HTTP_HEADER_SEP_LEN+ContentLength));
      If Result then begin
        Buffer.posRead:=iHeaderLoc+HTTP_HEADER_SEP_LEN;
        ExtractContent();
      end;
    end else begin
      Result:=True;
      Buffer.posRead:=iHeaderLoc+HTTP_HEADER_SEP_LEN;
      RSR.Refactor(Buffer,FRefactor,Buffer.PosRead+ContentLength);
    end;
  end;
end;


procedure THTTPResponse.Reset();
begin
  Head:=false;
  FMediaP:=nil;
  ConnectionHeader:=True;
  SetLength(ETag,0);
  SendNullContentLengthHeader:=False;
  Core.Arrays.KeyString.Empty(Headers);
  Core.Arrays.VarString.Empty(Parameters);
  FContent.Size:=0;
  Cache:=False;
  CacheDate:=0;
  CacheTTL:=-1;
  CacheExposure:=PUBLIC_CACHE;
  SetLength(CacheValidation,0);
  ContentLength:=0;
  FRefactor.Size:=0;
  ContentKind:=ckNone;
  ContentData:=nil;
  Close:=False;
end;


procedure THTTPResponse.SetCommand(Value:Core.Strings.VarString);
var
  iLen,iLoc:LongInt;
  sMag,sMin,sVers:Core.Strings.VarString;
begin
  Major:=1;
  Minor:=1;
  SetLength(Protocol,0);
  iLoc:=Core.Strings.Pos(#32,Value);
  if (iLoc>1) then begin
    Protocol:=System.Copy(Value,1,iLoc-1);
    iLoc:=Core.Strings.Pos('/',Protocol);
    if iLoc>0 then begin
      iLen:=System.Length(Protocol);
      sVers:=System.Copy(Protocol,iLoc+1,(iLen-iLoc));
      SetLength(Protocol,iLoc-1);
      iLoc:=Core.Strings.Pos('.',sVers);
      if (iLoc>0) then begin
        iLen:=System.Length(sVers);
        sMag:=System.Copy(sVers,1,iLoc-1);
        sMin:=System.Copy(sVers,iLoc+1,(iLen-iLoc));
        Major:=StrToIntDef(sMag,1);
        Minor:=StrToIntDef(sMin,1);
      end;
    end;
  end;
end;

procedure THTTPResponse.SetContent(var Value:Core.Strings.VarString);
begin
  ContentData:=@Value;
  ContentKind:=ckString;
  //Core.Arrays.KeyString.SetStreamsByKey(Headers,fieldContentLength,false);
  Core.Arrays.KeyString.Update(Headers,fieldContentLength,IntToStr(ContentLength));
  Core.Arrays.KeyString.SetStreamsByKey(Headers,fieldContentLength,true);
end;

procedure THTTPResponse.SetContent(Value:TStream; var Media:THTTPMedia);
begin
  FMediaP:=@Media;
  ContentData:=Value;
  ContentKind:=ckMedia;
end;

procedure THTTPResponse.SetContent(Value:TStream);
begin
  ContentData:=Value;
  ContentKind:=ckStream;
  ContentLength:=Value.Size;
end;

procedure THTTPResponse.AddToContent(Value:Core.Strings.VarString);
begin
  ContentKind:=ckContent;
  FContent.Position:=FContent.Size;
  Core.Streams.Append(Value,System.Length(Value),FContent);
  ContentLength:=FContent.Size;
end;

procedure THTTPResponse.AddToContent(Value:TStream);
begin
  ContentKind:=ckContent;
  FContent.Position:=FContent.Size;
  Value.Position:=0;
  FContent.CopyFrom(Value,Value.Size);
  ContentLength:=FContent.Size;
end;

procedure THTTPResponse.AddToContent(Value:TStream; iStart,iStop:QWord);overload;
begin
  ContentKind:=ckContent;
  FContent.Position:=FContent.Size;
  Value.Position:=iStart;
  FContent.CopyFrom(Value,(iStop-iStart)+1);
  ContentLength:=FContent.Size;
end;

procedure THTTPResponse.SetHeaders();
var
  iCount,iLcv:LongInt;
  sParams:Core.Strings.VarString;
begin
  Core.Arrays.KeyString.Empty(Cookies);
  iCount:=System.Length(Headers);
  Close:=SysUtils.SameText(Core.Arrays.KeyString.GetItemByKey(Headers,fieldConnection,iCount),'Close');
  ContentType:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldContentType,iCount);
  Server:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldReferer,iCount);
  Host:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldHost,iCount);

  sParams:=Core.Arrays.KeyString.GetItemAsString(Headers,fieldParameters,iCount);
  Core.Arrays.VarString.fromString(Parameters,sParams,'&',[soClearList,soIgnoreDelimAtStart]);
  SetLength(sParams,0);

  ContentLength:=Core.Arrays.KeyString.GetItemAsQWord(Headers,fieldContentLength,iCount);

  iLcv:=Core.Arrays.KeyString.IndexOf(@Headers,fieldCookie);
  if iLcv<>-1 then
    Core.Arrays.KeyString.fromString(Cookies,Headers[iLcv]^.Value,'=','; ');
end;

procedure THTTPResponse.Send(RSRP:PRSR; Refactor:TStream);
var
  idx:LongInt;

  procedure ProcessNoContent;
  var
    iLength:LongInt;
    idx:LongInt;
  begin
    ContentLength:=0;

    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentLength,'0');
    Headers[idx]^.Streams:=SendNullContentLengthHeader;

    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentType,ContentType);
    Headers[idx]^.Streams:=false;

    iLength:=toStreamHeader(Refactor);
    Refactor.Position:=0;

    RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
    RSRP^.SendBuffer.Stream.CopyFrom(Refactor,iLength);
    Inc(RSRP^.SendBuffer.posWrite,iLength);
    Refactor.Size:=0;

  end;

  procedure ProcessCors;
  var
    iLength:LongInt;
    idx:LongInt;
  begin
    ContentLength:=0;
    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentLength,'0');
    Headers[idx]^.Streams:=true;
    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentType,ctText);
    Headers[idx]^.Streams:=False;

    iLength:=toStreamHeader(Refactor);
    Refactor.Position:=0;

    RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
    RSRP^.SendBuffer.Stream.CopyFrom(Refactor,iLength);
    Inc(RSRP^.SendBuffer.posWrite,iLength);
    Refactor.Size:=0;
  end;

  procedure ProcessAsContent;
  var
    iLength:LongInt;
    idx:LongInt;
  begin
    ContentLength:=FContent.Size;
    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentLength,IntToStr(ContentLength));
    Headers[idx]^.Streams:=True;
    iLength:=toStreamHeader(Refactor);

    Refactor.Position:=0;
    RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
    RSRP^.SendBuffer.Stream.CopyFrom(Refactor,iLength);
    Inc(RSRP^.SendBuffer.posWrite,iLength);
    Refactor.Size:=0;
    if (ContentLength>0) and (Head=false) then begin
      FContent.Position:=0;
      RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
      RSRP^.SendBuffer.Stream.CopyFrom(FContent,ContentLength);
      Inc(RSRP^.SendBuffer.posWrite,ContentLength);
    end;

  end;

  procedure ProcessAsString;
  var
    iLength:LongInt;
    idx:LongInt;
  begin
    ContentLength:=System.Length(PString(ContentData)^);
    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentLength,IntToStr(ContentLength));
    Headers[idx]^.Streams:=True;

    iLength:=toStreamHeader(Refactor);
    Refactor.Position:=0;
    RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
    RSRP^.SendBuffer.Stream.CopyFrom(Refactor,iLength);
    Inc(RSRP^.SendBuffer.posWrite,iLength);
    Refactor.Size:=0;
    if (ContentLength>0) and (Head=false) then begin
      RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
      Core.Streams.Append(
        Core.Strings.PVarString(ContentData)^,
        ContentLength,
        RSRP^.SendBuffer.Stream
      );
      Inc(RSRP^.SendBuffer.posWrite,ContentLength);
    end;
  end;

  procedure ProcessAsStream;
  var
    iLength:LongInt;
    idx:LongInt;
  begin
    ContentLength:=TStream(ContentData).Size;

    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentLength,IntToStr(ContentLength));
    Headers[idx]^.Streams:=True;

    iLength:=toStreamHeader(Refactor);
    Refactor.Position:=0;

    RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
    RSRP^.SendBuffer.Stream.CopyFrom(Refactor,iLength);
    Inc(RSRP^.SendBuffer.posWrite,iLength);
    Refactor.Size:=0;
    if (ContentLength>0) and (Head=false) then begin
      TStream(ContentData).Position:=0;
      RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
      RSRP^.SendBuffer.Stream.CopyFrom(TStream(ContentData),ContentLength);
      Inc(RSRP^.SendBuffer.posWrite,ContentLength);
    end;
  end;

  procedure ProcessAsMedia;
  var
    iMaxChunk:QWord;
    iChunk:QWord;
    iLength:QWord;
  begin
    if (FMediaP^.Range.Stop>1) then begin
      iMaxChunk:=(FMediaP^.Range.Stop-FMediaP^.Range.Start)+1;
      iChunk:=iMaxChunk;
    end else begin
      // First Chunk Initialize with a chunk size greater than 1024*8;
      iMaxChunk:=(FMediaP^.Range.Size-FMediaP^.Range.Start);
      iChunk:=1024*8;
      if (iChunk>iMaxChunk) then
        iChunk:=iMaxChunk;
      FMediaP^.Range.Start:=0;
      FMediaP^.Range.Stop:=iChunk-1;
    end;
    ContentLength:=iChunk;
    if (ContentLength>RSR.RSR_MAX_MEDIA_PART) then begin
      FMediaP^.Range.Stop:=(FMediaP^.Range.Start+RSR.RSR_MAX_MEDIA_PART)-1;
      iChunk:=RSR.RSR_MAX_MEDIA_PART;
      Code:=HTTP_SC_PARTIALCONTENT;
      Status:='Partial Content';
    end else if ((FMediaP^.Range.Stop-FMediaP^.Range.Start+1)<>FMediaP^.Range.Size) then begin
      Code:=HTTP_SC_PARTIALCONTENT; // probably first byte
      Status:='Partial Content';
    end else begin
      Code:=HTTP_SC_OK;
      Status:='OK';
    end;
    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentLength,IntToStr(iChunk));
    Headers[idx]^.Streams:=True;
    Core.Arrays.KeyString.Exchange(idx,0,Headers);

    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentType,FMediaP^.ContentType);
    Core.Arrays.KeyString.Exchange(idx,1,Headers);

    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentEncoding,'binary');
    Core.Arrays.KeyString.Exchange(idx,2,Headers);

    idx:=Core.Arrays.KeyString.Update(Headers,fieldContentRange,Concat('bytes ',RSR.HTTP.toString(FMediaP^.Range)));
    Core.Arrays.KeyString.Exchange(idx,2,Headers);
    idx:=Core.Arrays.KeyString.Update(Headers,fieldAcceptRanges,'bytes');
    Core.Arrays.KeyString.Exchange(idx,3,Headers);

    idx:=Core.Arrays.KeyString.Update(Headers,fieldETag,FMediaP^.ETag);
    Core.Arrays.KeyString.Exchange(idx,4,Headers);

    idx:=Core.Arrays.KeyString.IndexOf(Headers,fieldCode);
    Core.Arrays.KeyString.Exchange(idx,5,Headers);

    iLength:=toStreamHeader(Refactor);
    Refactor.Position:=0;

    if System.Length(ETag)>0 then
      Core.Arrays.KeyString.Add(@Headers,fieldETag,ETag);




    RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
    RSRP^.SendBuffer.Stream.CopyFrom(Refactor,iLength);
    Inc(RSRP^.SendBuffer.posWrite,iLength);
    Refactor.Size:=0;
    if (iChunk>0) then begin
      FMediaP^.Content.Position:=FMediaP^.Range.Start;
      RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
      RSRP^.SendBuffer.Stream.CopyFrom(FMediaP^.Content,iChunk);
      Inc(RSRP^.SendBuffer.posWrite,iChunk);
    end;
  end;
begin
  Refactor.Size:=0;

  if ( Cache=true) then begin
    idx:=Core.Arrays.KeyString.Add(@Headers,fieldETag,ETag);
    if System.Length(ETag)=0 then
      Headers[idx]^.Streams:=False;
    if (CacheDate<>0) then
      Core.Arrays.KeyString.Add(@Headers,fieldLastModified,HTTP_DateTime(CacheDate));
    if (CacheTTL>0) then
      Core.Arrays.KeyString.Add(@Headers,fieldExpires,HTTP_DateTime(DateUtils.IncSecond(Core.Timer.dtUT,CacheTTL)));
    if (CacheTTL>-1) then begin
      FCacheControl:=Concat(CACHE_AGE,IntToStr(CacheTTL),', ',CacheExposure);
      if Length(CacheValidation)>0 then
        FCacheControl:=Concat(FCacheControl,', ',CacheValidation);
      Core.Arrays.KeyString.Add(Headers,fieldCacheControl,FCacheControl);
    end;
  end else begin
    Core.Arrays.KeyString.Add(@Headers,fieldCacheControl,NO_CACHE);
  end;

  case ContentKind of
    ckStream  :  ProcessAsStream;
    ckMedia   :  ProcessAsMedia;
    ckString  :  ProcessAsString;
    ckContent :  ProcessAsContent;
    ckNone    :  ProcessNoContent;
    ckCors    :  ProcessCors;
  end;
  RSR.MAN_MAP[RSRP^.Info.Socket].Send(RSRP);
end;

end.


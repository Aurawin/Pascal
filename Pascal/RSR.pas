unit RSR;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface
uses
  ctypes,
  Classes,

  App.Consts,

  Core.Threads,
  Core.Generics,
  Core.Strings,
  Core.Streams,
  Core.Timer,
  Core.Keywords,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,
  Core.Database.Types,
  Core.Utils.Sockets,

  Encryption,
  Encryption.SSL,
  RSR.DNS,

  Sockets,

  {$if defined(Windows)}
    Windows,
    Winsock2,
  {$else if defined(UNIX)}
    BaseUnix,Termio,
    {$if defined(Darwin)}
    BSD,
    {$else if defined(Linux)}
    Linux,
    {$endif}
  {$endif}
  DOM,XMLRead,
  md5;

Const
  Test:cInt=0;

  NONCE_LOW                      = 8;
  NONCE_HIGH                     = 16;
  NO_SSL                         = nil;
  SSL_ON                         = true;
  SSL_OFF                        = false;
  THREAD_METHODS_OFF             = false;
  THREAD_METHODS_ON              = true;
  CREATE_SUSPENDED               = true;
  CREATE_START                   = false;
  CONNECT_MX_LOOKUP              = true;
  CONNECT_RESOLVE                = false;
  SSL_LOAD_FAILURE               = 'Failed to load OpenSSL libraries';
  MAXDWORD                       = $FFFFFFFF;
  MAXWORD                        = $FFFF;
  MAXBYTE                        = $FF;
  COMPRESS_BUFFER                = $FFFF;
  MAXARRAY                       = $FFFFF;

  MAX_METHOD_DURATION_SEC        = 15;
  MAX_METHOD_SLEEP_SEC           = 30;
  MAX_SSL_SEED_LEN               = 16;
  MIN_SSL_SEED_LEN               = 4;
  HIGH_SSL_CTX_SSN_ID            = 30; // under 32 limit in api
  MIN_SSL_CTX_SSN_ID             = 4;

  SOCKET_LINGER_CLIENT           = 20; // 20 second linger for clients
  SOCKET_LINGER_SERVER           = 10; // 10 second linger for server connections
  SOCKET_LINGER_SSL              = 30; // 30 second linger for server connections
  SOCKET_ERROR                   = -1;
  OPEN_SSL_ERROR_DELAY           = 400; // 400 millisecond delay for checks
  RSR_THROTTLE_DELAY_SECONDS     =  15;  // 15 second delay if throttle limit is reached
  BUFFER_THRESHOLD               =  1024;
  RSR_INIT_SEND_BUFF             =  1024;
  RSR_INIT_RECV_BUFF             =  1024;
  RSR_MAX_MEDIA_PART             =  1024*1024*1024*2;
  RSR_MAX_RECV_PAYLOAD_SIZE      =  1024*1024*1024*3; // 2 GB in
  RSR_MAN_RECV_BUFF_SIZE         =  1024*1024;
  RSR_MAN_SEND_BUFF_MAX_SIZE     =  {$ifdef Windows} 1024*20; {$else}1024*32;{$endif}
  RSR_MAN_SEND_BUFF_MID_SIZE     =  1024*100;
  RSR_MAN_SEND_BUFF_ADJUST       =  1024*2;
  RSR_MAN_SEND_BUFF_MIN_SIZE     =  1024*10;
  RSR_MAN_SSL_BUFF_SIZE          =  1024*16;
  RSR_MAN_DNS_BUFF_SIZE          =  1024*512;
  RSR_POLL_CONNECTION            =  1000*7;
  RSR_SERVER_TIMEOUT             =  (60*60)*5;
  RSR_MAX_SOCKETS                =  $ffffe;
  RSR_MAX_RECEIVE_FRAME_BUFFER   =  1024*512;
  RSR_MAX_DNS                    =  $ffff;
  RSR_MAX_CYCLE_DURATION         =  1000*20;
  RSR_PROCESS_LOCK_DURATION      =  1000*120;
  RSR_ACCEPT_PAUSE               =  5;
  RSR_LISTEN_QUEUE               =  3000;
  RSR_BIND_PAUSE                 =  1000*5;
  RSR_BIND_ANY_PORT              =  0;
  RSR_REUSE_ADDRESS_ON           = true;
  RSR_REUSE_ADDRESS_OFF          = false;

  SOCKET_ERROR_ACCESS_DENIED     = 13;

  ENGINE_EXCEPTION_LOOP_PAUSE_MS = 10000;
  ENGINE_YIELD_EXCEPTION_MS      = 1000;
  QUEUE_YIELD_MS                 : Array[boolean] of cardinal=(500,1000);
  ENGINE_YIELD_MS                : Array[boolean] of cardinal=(50,700);

  RSR_ENGINE_ON                  =       0;
  RSR_ENGINE_PAUSE               =       1;
  RSR_ENGINE_STOP                =       3;

  RSR_STATE_NONE                 =       0;  // Reset State
  RSR_STATE_OPEN                 = 1 shl 0;  // Connected State
  RSR_STATE_SECURE               = 1 shl 1;  // SSL or TSL enabled Socket
  RSR_STATE_SYNC_RECV            = 1 shl 2;  // Synchronize all receive procedure calls.
  RSR_STATE_DNS                  = 1 shl 3;  // Pending DNS Activity
  RSR_STATE_REUSEABLE            = 1 shl 4;  // Reuseable Sockets (Don't finalize after disconnect)
  RSR_STATE_RECYCLE              = 1 shl 5;  // Recycle State for garbage collection.
  RSR_STATE_POLL                 = 1 shl 6;  // Poll Socket for Data or Hangups
  RSR_STATE_CONNECTING           = 1 shl 7;  // Waiting for connection
  RSR_STATE_INIT                 = 1 shl 8;  // Safe to use (Transport Installed)
  RSR_STATE_POLLED_RCVD          = 1 shl 9;  // A receive poll item has been tripped
  RSR_STATE_ISSUED_CONNECT       = 1 shl 10; // Issued fpConnect on the socket
  RSR_STATE_AUTOMATIC_STEPS      = 1 shl 11; // Use Automation Steps
  RSR_STATE_QUEUED               = 1 shl 12; // Socket is queued (for polling)
  RSR_STATE_RECYCLED             = 1 shl 13; // Socket is recycled.
  RSR_STATE_WRITE                = 1 shl 14; // Socket has received Write signal
  RSR_STATE_ERROR                = 1 shl 15; // Socket Errored
  RSR_STATE_WAIT_TO_PROCESS      = 1 shl 16; // Socket is no accepting more process commands

  RSR_FLAGS_NONE                 =       0;
  RSR_FLAGS_BLOCK_UNTIL_SENT     = 1 shl 0;
  RSR_FLAGS_BLOCK_PROCESSING     = 1 shl 1;
  RSR_FLAGS_ACCEPTED             = 1 shl 2;

  RSR_OP_NONE                    = 0;
  RSR_OP_QUEUE                   = 1 shl 0;
  RSR_OP_CONNECTED               = 1 shl 1;
  RSR_OP_DISCONNECTED            = 1 shl 2;
  RSR_OP_READ                    = 1 shl 3;
  RSR_OP_WRITE                   = 1 shl 4;
  RSR_OP_WRITABLE                = 1 shl 5;
  RSR_OP_FINALIZE                = 1 shl 6;
  RSR_OP_INITIALIZE              = 1 shl 7;
  RSR_OP_ERROR                   = 1 shl 8;
  RSR_OP_AUTOMATIC_NEXT          = 1 shl 9; // Process next step
  RSR_OP_UNUSED                  = 1 shl 10;
  RSR_OP_CLOSE                   = 1 shl 11;
  RSR_OP_STARTTLS                = 1 shl 12;
  RSR_OP_ALLOCATE                = 1 shl 13;
  RSR_OP_DISPOSE                 = 1 shl 14;



  RSR_KIND_UNASSIGNED            =       0;
  RSR_KIND_STRING                =       1;
  RSR_KIND_BYTEBUFFER            =       2;

  //  BUFFER ITEM FLAGS
  RSR_BI_NONE                    =       0;  // No flags;
  RSR_BI_FINALIZE                = 1 shl 0;  // Finalize Buffer Space

  RSR_ENGINE_STATE     : BYTE    = RSR_ENGINE_ON;
  RSR_WAIT_MIN         : WORD    = 40;
  RSR_WAIT_MAX         : WORD    = 80;
  RSR_TX_FLOAT         : Word    = 2;

  RSR_DIVISOR                    = 1048576;

  MAX_MX                         = 10;
  RSR_Build_Number               = {$i RSRBuilder.num};
  RSR_Version                    = {$i AppBuild.inc};
  RSR_Startup                    : TDateTime   = 0;

  RSR_Connection_Count           : Qword = 0;
  RSR_Stream_Count               : Qword = 0;
  RSR_VIOP_Call_Count            : Qword = 0;

  cntrSentBytes                  : Qword = 0;
  cntrRecvBytes                  : Qword = 0;

  cntrSentCount                  : Qword = 0;
  cntrRecvCount                  : Qword = 0;

  cntrPTX                        : Qword = 0;
  cntrTX                         : Qword = 0;

  cntrFiltered                   : Qword = 0;

  SendOptionalFlags              : LongInt=  {$ifdef Unix}MSG_DONTWAIT {$ifndef Darwin} or MSG_NOSIGNAL{$endif} {$else}0{$endif};
  RecvOptionalFlags              : LongInt=  {$ifdef Unix}MSG_DONTWAIT {$ifndef Darwin} or MSG_NOSIGNAL{$endif} {$else}0{$endif};

  LocalHost                      : Int64=0;

  EOperationInProgress           : Cardinal = 115;


  EngineFailure                  : boolean = false;

  FMT_PROCESS_LOCK_WARNING : Core.Strings.VarString = 'Maximum time surpassed during ProcessObjects.  Entry point=%s.  Actual Cycle Length =%s seconds';
  FMT_PROCESS_LOCK_SHUTDOWN : Core.Strings.VarString = 'Last known execution %s.  Actual Cycle Length =%s seconds. Ending program to avoid system failure.';
Type

{
  PRSRStringBuffer=^TRSRStringBuffer;
  TRSRStringBuffer=record
    Data:Core.Strings.VarString;
  end;
  PRSRByteBuffer=^TRSRByteBuffer;
  TRSRByteBuffer=record
    Data:TByteBuffer;
  end;
  PRSRPByteBuffer=^TRSRPByteBuffer;
  TRSRPByteBuffer=record
    Data:PByteBuffer;
  end;
}
  TSocketFilter=(sfkRead,sfkWrite,sfkConnect,sfkDisconnect);
  TSocketFilters=set of TSocketFilter;

  TChallenge=Array[0..7] of Byte;
  TChallResponse=Array[0..15] of Byte;
  PRSRInfo=^TRSRInfo;
  TBufferCallback=procedure(Data:Pointer);
  TSynchronizedMethod=procedure() of Object;
  TCallbackAddSocket=procedure (Manager:Pointer; ItemP:PRSRInfo); stdcall;
  TCallbackProc=procedure(var Handled:Boolean) of Object;
  TCallbackProcBool=Array[Boolean] of TCallbackProc;
  TCallbackFunctionWORD=function :WORD of Object;
  TCallbackBool=Array[Boolean] of TCallbackFunctionWORD;
  TRSRType=(tTCP,tUDP);
  TRSRErrorEvent=procedure(sProcedure,sLocation,sError:Core.Strings.VarString) of Object;
  TRSRError=(
    eReset,               // 0
    eSocket,              // 1
    eException,           // 2
    eDataSizeWaiting,     // 3
    eTimed,               // 4
    eSend,                // 5
    eReceive,             // 6
    eConnect,             // 7
    eHeard,               // 8
    eDisconnect,          // 9
    eDNS,                 // 10
    eNoSuchDomain,        // 11
    eSelect,              // 12
    eFormat,              // 13
    eRemoteServer,        // 14
    eNotImplemented,      // 15
    eRefusedByPolicy,     // 16
    eBuffer,              // 17
    eAccessDenied,        // 18
    eBind,                // 19
    eSSL                  // 20
  );
  TRSRSocketKind=(rsrsNone,rsrsUDP,rsrsTCP,rsrsDNS);
  TRSRErrors=Set of TRSRError;
  TRSRKind=(rsrClient,rsrServer);
  PRSR=^TRSR;
  TRSRMap=Array[0..1000000] of PRSR;
  TRSRDNSMAP=Array[WORD] of PRSR;
  TRSREvent=procedure(RSRP:PRSR) of Object;
  TSocketQueueEvent=procedure(RSRP:PRSR; var Handled:boolean) of object;
  PDataBuffer=^TDataBuffer;
  TDataBuffer=record
    posRead                      : Int64;
    posWrite                     : Int64;
    Lock                         : TRTLCriticalSection;
    Stream                       : TStream;
  end;
  PRSRStep=^TRSRStep;

  TRSRStepKind=(rsrSKNone,rsrSKDnsIP,rsrSKDnsMX,rsrSKConnect);
  TRSRStep=record
    Kind                         : TRSRStepKind;
    Complete                     : Boolean;
    OnSuccess                    : TRSREvent;
    OnFailure                    : TRSREvent;
  end;
  TRSRSteps=record
    Index : LongInt;
    Items : Array of PRSRStep;
  end;

  TRSRInfo=record
    Socket    : Sockets.TSocket;
    Kind      : TRSRKind;
    Port      : WORD;
    Flags     : WORD;
    Server    : ShortString;
    BindIP    : Int64;
    Poll      : LongInt;
    DataP     : Pointer;
    RequestID : Int64;
    idxIP     : Byte;
    IPs       : Core.Arrays.Types.LargeWord;
  end;
  TSSLCertKind=(sslckNone,sslckLevel1,sslckLevel2,sslckLevel3,sslckLevel4);
  PSSLChain=^TSSLChain;
  PSSLCertItem=^TSSLCertItem;

  TSSLChain=array of PSSLCertItem;
  TSSLCertItem=record
    Cert            : PX509;
    crtLen          : LongInt;
    crtData         : Core.Arrays.Types.Bytes;
  end;
  TSSLCertification=record
    Kind            : TSSLCertKind;
    List            : TSSLChain;
  end;

  TSSLInfo=record
    Method          : PSSL_METHOD;
    Context         : PSSL_CTX;

    Handle          : PSSL;
    BIO             : PBIO;

    Manifest        : TSSLCertification;

    ctxSessionID    : Core.Strings.VarString;
    ctxSessionIDLen : LongInt;
    ctxID           : Core.Strings.VarString;
    ctxIDLen        : LongInt;
  end;
  PSecureInfo=^TSecureInfo;
  TSecureMode=(sslServer,sslClient);

  TSecureInfo=record
    Mode            : TSecureMode;
    keyData         : Core.Arrays.Types.Bytes;
    keyLen          : LongInt;
    Manifest        : TSSLCertification;
  end;

  TThrottle=record
    Enabled         : Boolean;
    Channel         : System.Int64;  // user defined value for handle to a particular channel
    Limit           : System.Qword;  // max buffer space used in RAM
    Consumption     : System.Qword;  // Aggregate usage (read/write)
  end;
  TRSR=record
    Info                         : TRSRInfo;
    Throttle                     : TThrottle;
    State                        : Cardinal;
    Operations                   : WORD;
    SSL                          : TSSLInfo;
    DNS                          : TDNSObject;
    Automation                   : TRSRSteps;
    Kind                         : TRSRSocketKind;
    Address                      : Sockets.TSockAddr;
    TaskError                    : Int64;
    LastCall                     : Int64;
    Finite                       : Boolean;
    dtExpires                    : TDateTime;
    dtPoll                       : TDateTime;
    Errors                       : TRSRErrors;
    RecvBuffer                   : TDataBuffer;
    SendBuffer                   : TDataBuffer;
    RecvEvent                    : TSocketQueueEvent;
    SendEvent                    : TSocketQueueEvent;
    SendBufferIncrement          : TRSREvent;
    RecvBufferIncrement          : TRSREvent;
    FillSendBuffer               : TRSREvent;
    Transport                    : System.Pointer;
    Credentials                  : System.Pointer;
    Resource                     : System.Pointer;
  end;

  TRSREventsBool=array[Boolean] of TRSREvent;

  TSocketQueue=class
  private
    FItems                       : TList;
    FRecycled                    : TList;
    FLock                        : TRTLCriticalSection;
    FOnSocketProcess             : TSocketQueueEvent;
  public
    constructor Create; reIntroduce;
    destructor  Destroy; override;
  public
    function  Add(Socket:Sockets.TSocket):PRSR; overload;
    function  Add(var RSRP:PRSR):PRSR; overload;
    procedure Recycle(rsrP:PRSR);
    function  Reclaim(rsrP:PRSR):boolean;
    procedure Shutdown();
    procedure Clear;
    function  Process:LongInt;
  public
    property  OnProcessSocket:TSocketQueueEvent read FOnSocketProcess write FOnSocketProcess;
  end;

  TServerStats=record
    Weeks                        : Cardinal;
    Days                         : Cardinal;
    Hours                        : Cardinal;
    Minutes                      : Cardinal;
    Seconds                      : Cardinal;
  end;

  {$i RSR.SocketEvents.Consts.inc}
  {$i RSR.SocketEvents.Type.inc}


  procedure Init(Var Item:TThrottle); overload;
  procedure Init(Var Buffer:TDataBuffer); overload;
  procedure Init(Var RSR:TRSR); overload;
  procedure Init(Var Info:TRSRInfo); overload;
  procedure Init(Var Step:TRSRStep; const Kind:TRSRStepKind=rsrSKNone); overload;
  procedure Init(Var Steps:TRSRSteps); overload;
  procedure Init(Var Address:Sockets.TSockAddr); overload;
  procedure Init(Var Info:TSSLInfo); overload;
  procedure Init(Var Item:TSSLCertItem); overload;
  procedure Init(Var Item:TSSLCertification); overload;

  procedure Done(Var Item:TThrottle); overload;
  procedure Done(Var Buffer:TDataBuffer); overload;
  procedure Done(Var RSR:TRSR); overload;
  procedure Done(Var Info:TRSRInfo); overload;
  procedure Done(var Info:TSSLInfo); overload;
  procedure Done(Var Step:TRSRStep); overload;
  procedure Done(Var Steps:TRSRSteps); overload;
  procedure Done(Var Item:TSSLCertItem); overload;
  procedure Done(Var Item:TSSLCertification); overload;
  procedure Done(var Item:TSSLChain); overload;

  procedure Empty(Var Item:TThrottle); overload;
  procedure Empty(var Item:TRSR); overload;
  procedure Empty(Var Item:TDataBuffer); overload;
  procedure Empty(Var Item:TRSRInfo); overload;
  procedure Empty(Var Info:TSSLInfo); overload;

  procedure Empty(Var Item:TSSLCertItem); overload;
  procedure Empty(Var Item:TSSLCertification); overload;
  procedure Empty(Var Item:TSSLChain); overload;


  procedure Empty(Var Address:Sockets.TSockAddr); overload;
  procedure Empty(Var Step:TRSRStep); overload;
  procedure Empty(Var Steps:TRSRSteps); overload;

  procedure Copy(Var Source,Destination:TRSRInfo); overload;
  procedure Copy(Var Source,Destination:TSSLChain); overload;
  procedure Copy(Var Source,Destination:TSSLCertItem); overload;
  procedure Copy(Var Source,Destination:TSSLCertification); overload;

  procedure Load(var Cert:TCertData; Var Item:TSSLCertification); overload;

  function  RSRErrorToString(Errors:TRSRErrors):Core.Strings.VarString;

  function  IsConnected(Socket:Sockets.TSocket):boolean;

  function  GetServerStats(dtNow:TDateTime):TServerStats;
  procedure Clean(Var Input:Core.Strings.VarString);
  function  ReadLine(Var Buffer:TDataBuffer; Refactor:TStream):Core.Strings.VarString;
  procedure WriteLine(sLine:Core.Strings.VarString; Var Buffer:TDataBuffer);
  procedure Write(Data:TStream; var Buffer:TDataBuffer); overload;
  procedure Write(sData:Core.Strings.VarString; iCount:LongInt; var Buffer:TDataBuffer); overload;
  procedure Write(var utfData:Core.Strings.VarString; var Buffer:TDataBuffer); overload;
  procedure Write(var Digest:TMD5Digest; var Buffer:TDataBuffer); overload;
  procedure Write(var Response:TChallResponse; var Buffer:TDataBuffer); overload;
  procedure Trim(Var Buffer:TDataBuffer; Refactor:TStream; Const iPosition:Int64); overload;
  function  EndOfLine(Var Buffer:TDataBuffer):boolean;
  function  EndOf(Var Buffer:TDataBuffer; Term:Core.Strings.VarString):boolean; overload;
  function  EndOf(Var Buffer:TDataBuffer; Term:Byte):boolean; overload;
  function  Count(Var Buffer:TDataBuffer; Term:Core.Strings.VarString):Int64;
  function  Pos(Var Buffer:TDataBuffer; Term:Core.Strings.VarString):Int64; overload;
  function  Pos(Var Buffer:TDataBuffer; Term:Byte):Int64; overload;

  procedure Refactor(Var Buffer:TDatabuffer; Refactor:TStream; iStart:QWord);

  function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; out Output:Core.Strings.VarString):boolean; overload;
  function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; Output:TMemoryStream):boolean; overload;
  function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; out Output:TChallenge):boolean; overload;
  function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; out Output:Core.Arrays.Types.VarString; const Defaults:Core.Arrays.SplitOptions=[soClearList] ):boolean; overload;
  function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; FieldDelim,ItemDelim:Core.Strings.VarString; out Output:Core.Arrays.Types.KeyStrings):boolean; overload;

  function  RemoveEscapeCharacters(var S:Core.Strings.VarString; Refactor:TMemoryStream):Core.Strings.VarString; overload;
  procedure  RemoveEscapeCharacters(var sa:Core.Arrays.Types.VarString; Refactor:TMemoryStream); overload;
  function  OpenSSL:boolean;
  procedure CloseSSL;
  //procedure DoneSSL(Var RSR:TRSR);
  //procedure InitSSL(var SR:TRSR; var Info:TSecureInfo);

  function  GetNextIP(RSRP:PRSR):Int64;
  function  Generate_SSL_SessionID(ServerSocket,ClientSocket:TSocket):Core.Strings.VarString;
  function  Generate_Nonce(Low,High:Byte):Core.Strings.VarString;

  const AcceptableErrors:TRSRErrors=[eTimed,eReset];
  function  IsAcceptableError(Const Errors:TRSRErrors):Boolean;

Type
  TRSRManager=class;
  TRSRManagers=Array of TRSRManager;
  PRSRManagers=^TRSRManagers;
  TRSRManagerMap=Array[-1..MAXARRAY] of TRSRManager;
  TRSRServer=class;
  TRSRMethodThread=class;
  TRSRServerEvent=procedure (Server:TRSRServer) of object;

  TRSRDNSEntries=class
  private
    FItems     : Array of TDNSObject;
    FIndex     : LongInt;
  protected
    FOwner     : TRSRManager;
  public
    function   Acquire(ServerIP:QWord):TDNSObject;
  public
    constructor Create(AOwner:TRSRManager); virtual;
    destructor Destroy; override;
  end;

  TRSRMethod=class
  protected
    FRSRP      : PRSR;
    FExecuted  : Boolean;
    FRunOnce   : Boolean;
    FProcessor : TRSRMethodThread;
    FOwner     : TRSRManager;
  protected
    procedure Execute(); virtual; abstract;
  public
    constructor Create(aOwner:TRSRManager; aRSRP:PRSR); virtual;
    destructor Destroy; override;
  end;
  GRSRMethods=specialize GObjectThreadList<TRSRMethod>;

  TRSRMethods=class(GRSRMethods)
  private
    FOwner:TRSRManager;
  protected
    function Process(): LongInt;
  public
    constructor Create(aManager:TRSRManager); reIntroduce;
  end;

  TRSRMethodThread=class(CThread)
  private
    FSleepP                      : PRTLEvent;
    FProcessCount                : LongInt;
    FMethods                     : TRSRMethods;
    FOwner                       : TRSRManager;
    FRunning                     : Boolean;
    FEntryPoint                  : Core.Strings.VarString;
    FXMLParser                   : TDOMParser;
    FRefactor                    : TMemoryStream;
    FEntryLock                   : TRTLCriticalSection;
  private
    procedure _Sync_Method_Process();
    procedure setEntryPoint(Value:Core.Strings.VarString);
    function  getEntryPoint():Core.Strings.VarString;
  protected
    procedure Execute(); override;
  public
    Constructor Create(aManager:TRSRManager); Reintroduce;
    Destructor  Destroy; Override;
  public
    property EntryPoint:Core.Strings.VarString read getEntryPoint write setEntryPoint;
    property XMLParser : TDOMParser read FXMLParser;
    property Refactor  : TMemoryStream read FRefactor;
    property Terminated;
  public
    Task                         : Core.Database.Types.TTask;
  end;

  TRSRServer=Class(TThread)
  private
    TI_ProcessLock       : Core.Timer.Item;
    FPort                : Word;
    FRunning             : Boolean;
    FScale               : Word;
    FBound               : Boolean;
    iManagerLcv          : LongInt;
    FManagersP           : PRSRManagers;
    FRemoteSocket        : TSocket;
    FRemoteAddr          : Sockets.TSockAddr;
    FOnListening         : TRSRServerEvent;
    FOnEngineFailure     : TRSRServerEvent;
    FActive              : Boolean;
    FSockResult          : LongInt;
    FAddress             : Sockets.TSockAddr;
    FIP                  : System.QWord;
    FSocket              : Sockets.TSocket;
    FDefaultSocketKind   : TRSRSocketKind;
    FLinger              : Sockets.TLinger;

    FSleepP              : PRTLEvent;

    procedure   SyncListeningEvent;

    procedure   SetActive(Value:Boolean);
    procedure   SetSecure(Value:Boolean);

    procedure   OnTimer_ProcessLock(TimerP:Core.Timer.PItem);
  protected
    procedure   Execute; Override;
  protected
    function    GetNextManager:TRSRManager;
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); Virtual; Abstract;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); Virtual; Abstract;
  protected
    FSecure              : Boolean;
    FSSLInfo             : TSecureInfo;
    FService             : Core.Strings.VarString;
    FKind                : System.QWord;
    FRefactor            : TMemoryStream;
  protected
    function   cbKW_Loopback(ItemP:PKeyword):Core.Strings.VarString;

    function   cbKW_Date(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Year(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Time(ItemP:PKeyword):Core.Strings.VarString;

    function   cbKW_RSR_ConCount(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_StreamCount(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_TXCount(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_SentCount(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_RecvCount(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_BytesSent(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_BytesRecv(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_RSR_Filtered(ItemP:PKeyword):Core.Strings.VarString;

    function   cbKW_Node_Mem_Total(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Node_Mem_Free(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Node_Mem_Load(ItemP:PKeyword):Core.Strings.VarString;

    function   cbKW_Uptime_Weeks(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Uptime_Days(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Uptime_Hours(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Uptime_Minutes(ItemP:PKeyword):Core.Strings.VarString;
    function   cbKW_Uptime_Seconds(ItemP:PKeyword):Core.Strings.VarString;
  public
    Header      : Core.Database.Types.THeader;
    Task        : Core.Database.Types.TTask;
    Cert        : TCertData;
  public
    Constructor Create(aManagersP:PRSRManagers; Const RSRType:TRSRType; Const IP:System.QWord; Const aPort,aScale:Word; aSecure,aSuspended:Boolean); Reintroduce;
    Destructor  Destroy; Override;
  public
    procedure   Terminate; ReIntroduce;
  public
    property    Bound:Boolean read FBound;
    property    Secure:Boolean read FSecure write SetSecure;
    property    Active:Boolean read FActive write SetActive;
    property    Port:Word read FPort;
    property    Address:Sockets.TSockAddr read FAddress;
    property    IP:System.QWord read FIP;
    property    Scale:Word read FScale;
    property    SocketKind:TRSRSocketKind read FDefaultSocketKind write FDefaultSocketKind;
    property    Refactor:TMemoryStream read FRefactor;
  public
    property    OnListening:TRSRServerEvent read FOnListening write FOnListening;
    property    OnEngineFailure:TRSRServerEvent read FOnEngineFailure write FOnEngineFailure;
  end;

  TRSRManager=Class(TThread)
  private
    FEntryPoint                  : Core.Strings.VarString;
    FSockets                     : TSocketQueue; // Main collection where all sockets are stored.
    // End of Collection Items.
    FSyncRSRP                    : PRSR;
    FRecvProcedures              : TCallbackProcBool;
    FRecvProceduresUDP           : TCallbackProcBool;// ToDo
    FDefaultSocketKind           : TRSRSocketKind;
    FTimeoutProcedure            : TRSREvent;
    FServer                      : TRSRServer;
    FRSRMethodThread             : TRSRMethodThread;

    FDNSServersChecked           : Boolean;
    FSleepP                      : PRTLEvent;

    FClientLinger                : TLinger;
    FServerLinger                : TLinger;
    FSSLLinger                   : TLinger;
    FSocketCount                 : LongInt;

    FHandled                     : Boolean;
    FSSLInfoP                    : PSecureInfo;
    FSecure                      : Boolean;
    FRunning                     : Boolean;
    {$ifdef Unix}
    FSigOA                       : SigActionRec;
    FSigNA                       : SigActionRec;
    {$endif}
    {$ifdef useEventQueues}
    FPollEvents                  : TSocketPollEvents;
    FRcvQueue                    : TSocketEventQueue;
    FConQueue                    : TSocketEventQueue;
    FWriteQueue                  : TSocketEventQueue;
    FDisConQueue                 : TSocketEventQueue;
    {$endif}

    cbWaitDuration               : TCallbackBool;

    dtLastError                  : System.Double;
    dtCycleStart                 : System.Double;
    dtCycleEnd                   : System.Double;
    bCycleWarning                : Boolean;
    dwCycleDuration              : LongInt;

    FTimeout                     : DWORD;
    FRecvBufferSize              : DWORD;
    FSendBufferSize              : DWORD;

    FSKT_MAP_INDEX               : LongInt;

    FRecvBuffer                  : Core.Arrays.Types.Bytes;
    FSendBuffer                  : Core.Arrays.Types.Bytes;

    FDNSEntries                  : TRSRDNSEntries;

    FEntryLock                   : TRTLCriticalSection;

  private
    procedure cb_RSR_Close(RSRP:PRSR; var Handled:Boolean);

    procedure cb_RSR_StartTLS(RSRP:PRSR; var Handled:Boolean);

    procedure cb_RSR_Initialize(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Finalize(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Read(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Read_SSL(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Send(RSRP:PRSR; var Handled:Boolean);
    procedure cb_RSR_Send_SSL(RSRP:PRSR; var Handled:Boolean);

    procedure cb_RSR_Read_UDP(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Write_UDP(RSRP:PRSR; Var Handled:Boolean);

    procedure cb_RSR_Connected(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Disconnected(RSRP:PRSR; var Handled:Boolean);
    procedure cb_RSR_Error(RSRP:PRSR; var Handled:Boolean);
    procedure cb_RSR_Queue(RSRP:PRSR; var Handled:Boolean);
    procedure cb_RSR_Poll(RSRP:PRSR; Var Handled:Boolean);
    procedure cb_RSR_Automatic_Next(RSRP:PRSR; var Handled:Boolean);
  private
    procedure cb_RSR_SendBufferIncrement(RSRP:PRSR);
    procedure cb_RSR_SendBufferIncrementInterlocked(RSRP:PRSR);
    procedure cb_RSR_RecvBufferIncrement(RSRP:PRSR);
    procedure cb_RSR_RecvBufferIncrementInterlocked(RSRP:PRSR);
    procedure cb_RSR_FillSendBuffer(RSRP:PRSR);
    procedure cb_RSR_FillSendBufferInterlocked(RSRP:PRSR);
  private
    procedure   SetSecure(Value:Boolean);
    procedure   SetTimeOut(Value:DWORD);
    procedure   SetRecvBufferSize(Value:DWORD);
    procedure   SetSendBufferSize(Value:DWORD);
  private
    procedure   ReleaseSSL(RSRP:PRSR);
    procedure   InitSSLForConnect(RSRP:PRSR);
    procedure   InitSSLForAcceptAsServer(RSRP:PRSR);
    procedure   InitSSLForAcceptAsServerWithStartTLS(RSRP:PRSR);
  private
    procedure   automationSuccess(RSRP:PRSR);
    procedure   automationSuccessIP(RSRP:PRSR);
    procedure   automationSuccessMX(RSRP:PRSR);
    procedure   automationSuccessConnect(RSRP:PRSR);

    procedure   automationFailIP(RSRP:PRSR);
    procedure   automationFailMX(RSRP:PRSR);
    procedure   automationFailConnect(RSRP:PRSR);
  private
    procedure   ProcessReceiveDNSBuffer(RSRP:PRSR);


    Function    _Callback_Wait_Constant:WORD;
    Function    _Callback_Wait_CycleDuration:WORD;

    procedure   _OnReceive_Sync(var Handled:boolean);
    procedure   _OnReceive_ActSync();
    procedure   _Sync_Socket_Process();
    procedure   _OnReceive_NoSync(var Handled:boolean);

    procedure   Perform_TimeoutCheck(RSRP:PRSR);
    procedure   Perform_TimeoutCheck_Nil(RSRP:PRSR);

    procedure   Queue(Kind:TRSRSocketKind; Remote:Sockets.TSocket; RemoteAddress:Sockets.TSockAddr); overload;

    function    ReAssignSocket(RSRP:PRSR):boolean;
  protected
    procedure   OnQueue(RSRP:PRSR); virtual; Abstract;
    procedure   OnError(RSRP:PRSR); virtual; Abstract;
    procedure   OnDisconnect(RSRP:PRSR); virtual; Abstract;
    procedure   OnConnect(RSRP:PRSR); virtual; Abstract;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:Boolean); virtual; Abstract;
    procedure   OnDNSResult(RSRP:PRSR); virtual; Abstract;
  protected
    procedure   OnInitialize(RSRP:PRSR); virtual; Abstract;
    procedure   OnFinalize(RSRP:PRSR); virtual; Abstract;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); virtual; Abstract;
  protected
    procedure   Execute; Override;
  protected
    procedure   SetEntryPoint(Value:Core.Strings.VarString);
    function    GetEntryPoint():Core.Strings.VarString;
    procedure   Allocate(Kind:TRSRKind; RSRP:PRSR); overload;
    procedure   SetEvents(RSRP:PRSR);
    function    Queue(Kind:TRSRSocketKind; const BindIP:Qword; const BindPort:Word; const Data:System.Pointer):PRSR; overload;
    function    Queue(Kind:TRSRSocketKind; const Data:System.Pointer):PRSR; overload;
  protected
    FService                     : Core.Strings.VarString;
    FKind                        : System.QWord;
    FRefactor                    : TMemoryStream;
  public
    Task                         : Core.Database.Types.TTask;
  public
    procedure   DNSLookup(RSRP:PRSR; Value:Core.Strings.VarString; Types:TDNSStyles; ServerIP:QWord=0); virtual; overload;
    procedure   DNSLookup(RSRP:PRSR; Value:QWord; List:Core.Strings.VarString; ServerIP:QWord=0); virtual; overload;
  public
    procedure   RenewCycle();
    procedure   AddMethod(Method:TRSRMethod);
    function    Allocate(Kind:TRSRKind; sockKind:TRSRSocketKind):PRSR; overload;
  public
    procedure   Send(RSRP:PRSR); overload;
    procedure   Send(RSRP:PRSR; var Data:RawByteString); overload;
    procedure   Send(RSRP:PRSR; Var Data:Core.Strings.VarString); overload;
    procedure   Send(RSRP:PRSR; Var Data:Core.Arrays.Types.VarString); overload;
    procedure   Send(RSRP:PRSR; Data:TStream); overload;
    procedure   Send(RSRP:PRSR; BufferP:Core.Arrays.Types.PBytes; Const Length:LongInt); overload;
    procedure   Send(RSRP:PRSR; Stream:TStream; Const Offset,Length:Int64); overload;
  public
    procedure   StartTLS(RSRP:PRSR);

    procedure   Connect(RSRP:PRSR; Server:Core.Strings.VarString; Port:Word); overload;
    procedure   Connect(RSRP:PRSR; Server:Qword; Port:Word); overload;
    procedure   Connect(RSRP:PRSR; Domain:Core.Strings.VarString; Port:word; MXLookup:boolean); overload;

    procedure   Close(RSRP:PRSR); virtual;
    procedure   Retire(RSRP:PRSR); virtual;
  public
    Constructor Create(const aServer:TRSRServer; const InfoP:PSecureInfo; Const UseSSL:Boolean; Const UseMethods:Boolean; Const StackSize:LongInt); Reintroduce; Virtual;
    Destructor  Destroy; Override;
  public
    property    CommandThread : TRSRMethodThread read FRSRMethodThread;
    property    Terminated;
    property    Service : Core.Strings.VarString read FService;
    property    Kind : QWord read FKind;
    property    TimeOut:DWord read FTimeout write SetTimeout;
    property    RecvBufferSize:DWORD read FRecvBufferSize write SetRecvBufferSize;
    property    SendBufferSize:DWORD read FSendBufferSize write SetSendBufferSize;
    property    DefaultSocketKind:TRSRSocketKind read FDefaultSocketKind write FDefaultSocketKind;
    property    Refactor:TMemoryStream read FRefactor;
    property    EntryPoint:Core.Strings.VarString read GetEntryPoint write SetEntryPoint;
  end;

Var
  RSR_MAP    : TRSRMap;
  MAN_MAP    : TRSRManagerMap;
  DNS_MAP    : TRSRDNSMAP;

  procedure     Empty(Var Item:TRSRManagers); overload;
  procedure     Done(Var Item:TRSRManagers); overload;
  procedure     Add(Var List:TRSRManagers; Item:TRSRManager); overload;

implementation
uses
  DateUtils,SysUtils,StrUtils,Math;

Var
  ProcessLockThread    : TTimerThread;


function  IsAcceptableError(Const Errors:TRSRErrors):Boolean;
var
  Lcv:TRSRError;
begin
  if Errors=[] then begin
    Result:=true;
  end else begin
    Result:=false;
    for Lcv:=Low(AcceptableErrors) to High(AcceptableErrors) do begin
      If (Lcv in Errors) then begin
        Result:=true;
        break;
      end;
    end;
  end;
end;

function  Generate_Nonce(Low,High:Byte):Core.Strings.VarString;
const
  LIB:array[0..51] of char=(
    'A','a',
    'B','b',
    'C','c',
    'D','d',
    'E','e',
    'F','f',
    'G','g',
    'H','h',
    'I','i',
    'J','j',
    'K','k',
    'L','l',
    'M','m',
    'N','n',
    'O','o',
    'P','p',
    'Q','q',
    'R','r',
    'S','s',
    'T','t',
    'U','u',
    'V','v',
    'W','w',
    'X','x',
    'Y','y',
    'Z','z'
  );
var
  iLcv:LongInt;
  Size:LongInt;
begin
  Size:=Low+Random(High-Low);
  SetLength(Result,Size);
  for iLcv:=1 to Size do
    Result[iLcv]:=LIB[Random(52)];
end;



function OpenSSL:boolean;
begin
  if IsSSLloaded=false then begin
    if InitSSLInterface() then begin
      Result:=True;
      SslLibraryInit();
      SslLoadErrorStrings();
      if assigned(Rand_screen) then
         Rand_screen;
      if Assigned(OpenSSL_add_all_algorithms) then
        OpenSSL_add_all_algorithms();
      if Assigned(OpenSSL_add_all_ciphers) then
        OpenSSL_add_all_ciphers();
      if Assigned(OpenSSL_add_all_digests) then
        OpenSSL_add_all_digests();
    end else
      Result:=false;
  end else
    Result:=True;
end;

procedure CloseSSL;
begin
  if IsSSLloaded then begin
    DestroySSLInterface();
    DestroyLibeaInterface();
    DestroySSLEAInterface();
  end;
end;

procedure DoneSSL(Var RSR:TRSR);
begin
  If (RSR.SSL.Handle<>nil) then begin
    SSL_free(RSR.SSL.Handle);
    RSR.SSL.Handle:=nil;
  end;
  RSR.SSL.Context:=nil;
  RSR.SSL.Method:=nil;
  RSR.SSL.ctxSessionIDLen:=0;
  SetLength(RSR.SSL.ctxSessionID,0);
end;

function  GetNextIP(RSRP:PRSR):Int64;
begin
  Result:=-1;
  if RSRP<>nil then begin
    Inc(RSRP^.Info.idxIP,1);
    if RSRP^.Info.idxIP>High(RSRP^.Info.IPs) then
      RSRP^.Info.idxIP:=0;
    if RSRP^.Info.idxIP<=High(RSRP^.Info.IPs) then
      Result:=RSRP^.Info.IPs[RSRP^.Info.idxIP];
  end;
end;

function Generate_SSL_SessionID(ServerSocket,ClientSocket:TSocket):Core.Strings.VarString;
var
  ctx:TMD5Context;
  dg:TMD5Digest;
begin
  md5.MD5Init(ctx);
  md5.MD5Update(ctx,ServerSocket,SizeOf(ServerSocket));
  md5.MD5Update(ctx,ClientSocket,SizeOf(ClientSocket));
  md5.MD5Update(ctx,Core.Timer.dtUT,SizeOf(Core.Timer.dtUT));
  md5.MD5Final(ctx,dg);
  Result:=md5.MD5Print(dg);
end;

procedure InitSSL(var SR:TRSR; var Info:TSecureInfo);
var
  iLcv:LongInt;
begin
  SR.SSL.Method:=SslMethodV23;
  SR.SSL.Context:=SSL_ctx_new(SR.SSL.Method);
  SR.SSL.Handle:=SSL_new(SR.SSL.Context);

  //SSL_CTX_set_tmp_dh(SR.SSL.Context,get_dh512());

  Encryption.SSL.SSL_CTX_set_cipher_list(SR.SSL.Context,CIPHER_LIST_CLIENT);

  SslCtxSetVerify(SR.SSL.Context, SSL_VERIFY_NONE, nil);
  SSLCTXSetOptions(SR.SSL.Context, SSL_OP_SINGLE_DH_USE);
  ssl_ctx_set_mode(SR.SSL.Context, SSL_MODE_AUTO_RETRY);
  ssl_ctx_set_mode(SR.SSL.Context, SSL_MODE_ENABLE_PARTIAL_WRITE);
  if (SR.SSL.ctxSessionIDLen>0) then
    SSL_CTX_set_session_id_context(SR.SSL.Context,PChar(SR.SSL.ctxSessionID),SR.SSL.ctxSessionIDLen);

  if SSL_CTX_use_PrivateKey_ASN1(EVP_PKEY_RSA,SR.SSL.Context,@Info.keyData[0],Info.keyLen)<>1 then begin
    SR.TaskError:=Encryption.SSL.ERR_get_error();
  end;

  Copy(Info.Manifest,SR.SSL.Manifest);
  for iLcv:=0 to High(SR.SSL.Manifest.List) do
    SR.SSL.Manifest.List[iLcv]^.Cert:=Encryption.SSL.d2i_X509(nil,@SR.SSL.Manifest.List[iLcv]^.crtData,SR.SSL.Manifest.List[iLcv]^.crtLen);
  SSL_CTX_use_certificate(SR.SSL.Context,SR.SSL.Manifest.List[0]^.Cert);
  for iLcv:=1 to High(SR.SSL.Manifest.List) do
    Encryption.SSL.SSL_CTX_add_extra_chain_cert(SR.SSL.Context,SR.SSL.Manifest.List[iLcv]^.Cert);
  SSL_set_fd(SR.SSL.Handle,SR.Info.Socket);
  if (SR.Info.Flags or RSR_FLAGS_ACCEPTED=SR.Info.Flags) then begin
    SSL_set_session_id_context(SR.SSL.Handle,PChar(SR.SSL.ctxSessionID),SR.SSL.ctxSessionIDLen);
    SSL_accept(SR.SSL.Handle);
  end;

end;

procedure Copy(Var Source,Destination:TRSRInfo);
begin
  Destination.Socket:=Source.Socket;
  Destination.Port:=Source.Port;
  Destination.Flags:=Source.Flags;
  Destination.Poll:=Source.Poll;
  Destination.Server:=Source.Server;
  Destination.BindIP:=Source.BindIP;
end;

procedure Copy(var Source,Destination:TSSLChain);
var
  iCurSize:LongInt;
  iSize:LongInt;
  iLcv:LongInt;
begin
  iCurSize:=System.Length(Destination);
  iSize:=System.Length(Source);
  if iCurSize<iSize then begin
    // Grow List
    System.SetLength(Destination,iSize);
    for iLcv:=iCurSize to iSize-1 do begin
      New(Destination[iLcv]);
      Init(Destination[iLcv]^);
    end;
  end else if iCurSize>iSize then begin
    // Shrink List
    for iLcv:=iSize to iCurSize-1 do begin
      Done(Destination[iLcv]^);
      Dispose(Destination[iLcv]);
    end;
    System.SetLength(Destination,iSize);
  end;
  // Both lists should be the same.  Now we can copy entries
  for iLcv:=0 to iSize-1 do
    Copy(Source[iLcv]^,Destination[iLcv]^);
end;

procedure Copy(var Source,Destination:TSSLCertItem);
begin
  if Destination.Cert<>nil then
    Encryption.SSL.X509_free(Destination.Cert);
  Destination.Cert:=nil;
  Destination.crtLen:=Source.crtLen;
  Core.Arrays.Bytes.Copy(Source.crtData,Destination.crtData);
end;

procedure Copy(Var Source,Destination:TSSLCertification);
begin
  Destination.Kind:=Source.Kind;
  Copy(Source.List,Destination.List);
end;

procedure Load(var Cert:TCertData; Var Item:TSSLCertification);
begin
  Item.Kind:=TSSLCertKind(Cert.Level);
  Case Cert.Level of
    1 : begin
      SetLength(Item.List,1);

      New(Item.List[0]);

      Init(Item.List[0]^);

      Item.List[0]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[0],Item.List[0]^.crtData);
    end;
    2 : begin
      SetLength(Item.List,2);

      New(Item.List[0]);
      New(Item.List[1]);

      Init(Item.List[0]^);
      Init(Item.List[1]^);

      Item.List[0]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[0],Item.List[0]^.crtData);
      Item.List[1]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[1],Item.List[1]^.crtData);
    end;
    3 : begin
      SetLength(Item.List,3);

      New(Item.List[0]);
      New(Item.List[1]);
      New(Item.List[2]);

      Init(Item.List[0]^);
      Init(Item.List[1]^);
      Init(Item.List[2]^);

      Item.List[0]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[0],Item.List[0]^.crtData);
      Item.List[1]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[1],Item.List[1]^.crtData);
      Item.List[2]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[2],Item.List[2]^.crtData);
    end;
    4 : begin
      SetLength(Item.List,4);

      New(Item.List[0]);
      New(Item.List[1]);
      New(Item.List[2]);
      New(Item.List[3]);

      Init(Item.List[0]^);
      Init(Item.List[1]^);
      Init(Item.List[2]^);
      Init(Item.List[3]^);

      Item.List[0]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[0],Item.List[0]^.crtData);
      Item.List[1]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[1],Item.List[1]^.crtData);
      Item.List[2]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[2],Item.List[2]^.crtData);
      Item.List[3]^.crtLen:=Core.Arrays.Bytes.Copy(Cert.DerCerts[3],Item.List[3]^.crtData);
    end;
  end;

end;

procedure Empty(Var Item:TDataBuffer);
begin
  Item.posRead:=0;
  Item.posWrite:=0;
  if (Item.Stream<>nil) then
    Item.Stream.Size:=0;
end;

procedure Empty(Var Item:TRSRInfo);
begin
  Item.Socket:=0;
  Item.BindIP:=0;
  Item.Flags:=RSR_FLAGS_BLOCK_UNTIL_SENT;
  Item.Port:=0;
  Item.Poll:=0;
  Item.idxIP:=0;
  Item.RequestID:=-1;
  Core.Arrays.LargeWord.Empty(Item.IPs);
  Item.DataP:=nil;
  SetLength(Item.Server,0);
end;

procedure Empty(var Info:TSSLInfo);
begin
  Info.Method:=nil;
  Info.Context:=nil;
  Info.Handle:=nil;
  Info.BIO:=nil;
  Empty(Info.Manifest);
  SetLength(Info.ctxSessionID,0);
  Info.ctxSessionIDLen:=0;
  SetLength(Info.ctxID,0);
  Info.ctxIDLen:=0;
end;


procedure Empty(Var Item:TThrottle);
begin
  Item.Consumption:=0;
  Item.Channel:=0;
  Item.Enabled:=false;
  Item.Limit:=0;
end;


procedure Empty(var Item:TRSR); overload;
begin
  Item.Operations:=RSR_OP_NONE;
  Item.State:=0;
  Item.Kind:=rsrsNone;
  Item.TaskError:=0;
  Item.LastCall:=0;
  Item.Finite:=true;
  Item.dtExpires:=0;
  Item.dtPoll:=0;
  Item.Errors:=[];
  Item.DNS:=nil;
  Item.Transport:=nil;
  Item.Credentials:=nil;
  Item.Resource:=nil;
  Item.RecvEvent:=nil;
  Item.SendEvent:=nil;
  Item.SendBufferIncrement:=nil;
  Item.RecvBufferIncrement:=nil;
  Item.FillSendBuffer:=nil;

  Empty(Item.Throttle);
  Empty(Item.RecvBuffer);
  Empty(Item.SendBuffer);
  Empty(Item.Info);

  DoneSSL(Item);
end;

procedure Init(Var Item:TThrottle);
begin
  Item.Channel:=0;
  Item.Consumption:=0;
  Item.Enabled:=false;
  Item.Limit:=0;
end;

procedure Init(Var Buffer:TDataBuffer);
begin
  Buffer.posWrite:=0;
  Buffer.posRead:=0;
  Buffer.Stream:=nil;
  InitCriticalSection(Buffer.Lock);
end;

Procedure Init(var RSR:TRSR);
begin
  With RSR do begin
    Operations:=RSR_OP_NONE;
    Transport:=nil;
    Credentials:=nil;
    Resource:=nil;
    Init(Throttle);
    Init(Address);
    Init(Info);
    Init(RecvBuffer);
    Init(SendBuffer);
    State:=0;
    Kind:=rsrsNone;
    LastCall:=0;
    Finite:=true;
    TaskError:=0;
    Errors:=[];
    dtExpires:=0;
    dtPoll:=0;
    DNS:=nil;
    Transport:=nil;
    Credentials:=nil;
    Resource:=nil;
    RecvEvent:=nil;
    SendEvent:=nil;
    SendBufferIncrement:=nil;
    RecvBufferIncrement:=nil;
    FillSendBuffer:=nil;
    Init(SSL);
  end;
end;

procedure Init(Var Info:TRSRInfo);
begin
  with Info do begin
    Socket:=0;
    Port:=0;
    Flags:=RSR_FLAGS_BLOCK_UNTIL_SENT;
    BindIP:=0;
    Poll:=0;
    RequestID:=-1;
    DataP:=nil;
    idxIP:=0;
    Core.Arrays.LargeWord.Init(IPs);
    SetLength(Server,0);
  end;
end;

procedure Init(Var Step:TRSRStep;  const Kind:TRSRStepKind=rsrSKNone);
begin
  Step.Complete:=false;
  Step.Kind:=Kind;
  Step.OnFailure:=nil;
  Step.OnSuccess:=nil;
end;

procedure Empty(Var Step:TRSRStep);
begin
  Step.Complete:=false;
  Step.Kind:=rsrSKNone;
  Step.OnFailure:=nil;
  Step.OnSuccess:=nil;
end;

procedure Done(Var Step:TRSRStep);
begin
  Finalize(Step);
end;

procedure Init(Var Steps:TRSRSteps);
var
  iLcv:LongInt;
begin
  Steps.Index:=-1;
  for iLcv:=0 to high(Steps.Items) do begin
    Done(Steps.Items[iLcv]^);
    Dispose(Steps.Items[iLcv]);
  end;
  SetLength(Steps.Items,0);
end;

procedure Init(Var Info:TSSLInfo);
begin
  Info.Method:=nil;
  Info.Context:=nil;
  Init(Info.Manifest);
  Info.Handle:=nil;
  Info.BIO:=nil;
  SetLength(Info.ctxSessionID,0);
  Info.ctxSessionIDLen:=0;
  SetLength(Info.ctxID,0);
  Info.ctxIDLen:=0;
end;

procedure Init(var Item:TSSLCertItem);
begin
  Item.Cert:=nil;
  Item.crtLen:=0;
  SetLength(Item.crtData,0);
end;

procedure Init(var Item:TSSLChain);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Init(var Item:TSSLCertification);
begin
  Item.Kind:=sslckNone;
  Init(Item.List);
end;

procedure Empty(Var Steps:TRSRSteps);
var
  iLcv:LongInt;
begin
  Steps.Index:=-1;
  for iLcv:=0 to high(Steps.Items) do begin
    Done(Steps.Items[iLcv]^);
    Dispose(Steps.Items[iLcv]);
  end;
  SetLength(Steps.Items,0);
end;

procedure Empty(var Item:TSSLCertItem);
begin
  if Item.Cert<>nil then
    Encryption.SSL.X509_free(Item.Cert);
  Item.Cert:=nil;
  Item.crtLen:=0;
  Empty(Item.crtData);
end;

procedure Empty(var Item:TSSLCertification);
begin
  Item.Kind:=sslckLevel1;
  Empty(Item.List);
  SetLength(Item.List,1);
  New(Item.List[0]);
  Init(Item.List[0]^);
end;

procedure Empty(var Item:TSSLChain);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

procedure Done(var Item:TSSLCertItem);
begin
  if Item.Cert<>nil then
    Encryption.SSL.X509_free(Item.Cert);
  Item.Cert:=nil;
  Item.crtLen:=0;
  Finalize(Item.crtData);
end;

procedure Done(var Item:TSSLCertification);
begin
  Done(Item.List);
  Finalize(Item);
end;

procedure Done(var Item:TSSLChain);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure Done(Var Steps:TRSRSteps);
var
  iLcv:LongInt;
begin
  Steps.Index:=-1;
  for iLcv:=0 to high(Steps.Items) do begin
    Done(Steps.Items[iLcv]^);
    Dispose(Steps.Items[iLcv]);
  end;
  SetLength(Steps.Items,0);
  Finalize(Steps.Items);
  Finalize(Steps);
end;

procedure Init(Var Address:Sockets.TSockAddr);
begin
  FillByte(Address,SizeOf(TSockAddr),0);
end;


procedure Empty(Var Address:Sockets.TSockAddr);
begin
  FillByte(Address,SizeOf(TSockAddr),0);
end;

procedure Done(Var Buffer:TDataBuffer);
begin
  DoneCriticalSection(Buffer.Lock);
  FreeAndNil(Buffer.Stream);
  Finalize(Buffer);
end;

procedure Done(Var Item:TThrottle);
begin
  Finalize(Item);
end;

procedure Done(Var RSR:TRSR);
begin
  Done(RSR.Throttle);
  Done(RSR.Info);
  Done(RSR.RecvBuffer);
  Done(RSR.SendBuffer);
  Done(RSR.SSL);
  Finalize(RSR);
end;

procedure Done(Var Info:TRSRInfo);
begin
  Core.Arrays.LargeWord.Done(Info.IPs);
  Finalize(Info);
end;

procedure Done(var Info:TSSLInfo);
begin
  Info.Method:=nil;
  Info.Context:=nil;
  Info.Handle:=nil;
  Info.BIO:=nil;
  Done(Info.Manifest);

  Info.ctxSessionIDLen:=0;
  Info.ctxIDLen:=0;
  Finalize(Info.ctxSessionID);
  Finalize(Info.ctxID);
  Finalize(Info);
end;

(*
Constructor TRSRBuffer.Create;
begin
  FCount:=0;
  FirstP:=Nil;
  LastP:=Nil;
  Inherited Create;
end;

Destructor  TRSRBuffer.Destroy;
begin
  Clear;
  Inherited Destroy;
end;
{
  If we have memory problems here we can switch to a dynamic array of TSocket and
  it's add would increase len by 1 and delete will use MoveMemory()
}
procedure   TRSRBuffer.Clear;
var
  FDisposeP,FLcvP:PRSRBufferItem;
begin
  FCount:=0;
  FLcvP:=FirstP;
  While (FLcvP<>Nil) do begin
    FDisposeP:=FLcvP;
    FLcvP:=FDisposeP^.NextP;
    If FDisposeP^.Flags or RSR_BI_FINALIZE=FDisposeP^.Flags then begin
      Case FDisposeP^.Kind of
        RSR_BI_STRING      : Finalize_Data_String(FDisposeP^.Buffer);
        RSR_BI_PBYTEBUFFER : Finalize_Data_PByteBuffer(FDisposeP^.Buffer);
        RSR_BI_BYTEBUFFER  : Finalize_Data_ByteBuffer(FDisposeP^.Buffer);
      end;
    end;
    Dispose(FDisposeP^.Buffer);
    Dispose(FDisposeP);
  end;
  FirstP:=Nil;
  LastP:=Nil;
end;


procedure   TRSRBuffer.Add(ItemP:PRSRBufferItem);
begin
  ItemP^.NextP:=Nil;
  ItemP^.PrevP:=LastP;
  If FirstP=Nil then begin
    FirstP:=ItemP;
    LastP:=ItemP;
  end else begin
    LastP^.NextP:=ItemP;
    ItemP^.PrevP:=LastP;
    LastP:=ItemP;
  end;
end;

Function   TRSRBuffer.Remove(ItemP:PRSRBufferItem):PRSRBufferItem;
begin
  Result:=ItemP^.NextP;
  If (ItemP^.PrevP=Nil) then begin
    // Only can be forward looking...
    FirstP:=ItemP^.NextP;
    If FirstP<>Nil then
      FirstP^.PrevP:=Nil
    else
      LastP:=Nil;
  end else If ItemP^.NextP=Nil then begin
    // Only can be reverse looking...
    LastP:=ItemP^.PrevP;
    If LastP<>Nil then
      LastP^.NextP:=Nil;
  end else begin // Somwhere in the middle
    ItemP^.PrevP^.NextP:=ItemP^.NextP;
    ItemP^.NextP^.PrevP:=ItemP^.PrevP;
  end;
  If ItemP^.Flags or RSR_BI_FINALIZE=ItemP^.Flags then begin
    Case ItemP^.Kind of
      RSR_BI_STRING      : Finalize_Data_String(ItemP^.Buffer);
      RSR_BI_PBYTEBUFFER : Finalize_Data_PByteBuffer(ItemP^.Buffer);
      RSR_BI_BYTEBUFFER  : Finalize_Data_ByteBuffer(ItemP^.Buffer);
    end;
  end;
  Dispose(ItemP^.Buffer);
  Dispose(ItemP);
end;

*)
procedure Trim(Var Buffer:TDataBuffer; Refactor:TStream; Const iPosition:Int64);
var
  iRead:Int64;
begin
  Refactor.Size:=0;
  Buffer.Stream.Position:=iPosition;
  iRead:=Buffer.Stream.Size-iPosition;
  if iRead>0 then begin
    Core.Streams.CopyFrom(Buffer.Stream,Refactor,iPosition,iRead);
    Buffer.Stream.Size:=0;
    Core.Streams.CopyFrom(Refactor,Buffer.Stream,0,iRead);
    Buffer.Stream.CopyFrom(Refactor,iRead);
    Refactor.Size:=0;
  end else
    Buffer.Stream.Size:=0;
  Buffer.posWrite:=Buffer.Stream.Size;
  Buffer.posRead:=0;
end;

procedure   Clean(Var Input:Core.Strings.VarString);
var
  iPos:LongInt;
begin
  iPos:=System.Pos(#8,Input);
  While iPos>0 do begin
    System.Delete(Input,iPos-1,2);
    iPos:=System.Pos(#8,Input);
  end;
end;

Function   ReadLine(Var Buffer:TDataBuffer; Refactor:TStream):Core.Strings.VarString;
var
  iRead:Cardinal;
begin
  Result:=Core.Streams.Readline(Buffer.Stream,Buffer.posRead);
  Refactor.Size:=0; iRead:=Buffer.Stream.Size-Buffer.Stream.Position;
  if iRead>0 then begin
    Refactor.CopyFrom(Buffer.Stream,iRead);
    Buffer.Stream.Size:=0;
    Refactor.Position:=0;
    Buffer.Stream.CopyFrom(Refactor,Refactor.Size);
    Refactor.Size:=0;
  end else begin
    Buffer.Stream.Size:=0;
  end;
  Buffer.posWrite:=Buffer.Stream.Size;
  Buffer.posRead:=0;
end;

procedure WriteLine(sLine:Core.Strings.VarString; Var Buffer:TDataBuffer);
var
  iLength:System.LongInt;
begin
  sLine+=#13#10;
  InterLockedExchange(iLength,System.Length(sLine));
  Buffer.Stream.Position:=Buffer.posWrite;
  Buffer.Stream.WriteBuffer(sLine[1],iLength);
  Inc(Buffer.posWrite,iLength);
end;

procedure Write(sData:Core.Strings.VarString; iCount:LongInt; var Buffer:TDataBuffer);
begin
  Buffer.Stream.Position:=Buffer.posWrite;
  Buffer.Stream.Write(sData[1],iCount);
  Inc(Buffer.posWrite,iCount);
end;

procedure Write(var Response:TChallResponse; var Buffer:TDataBuffer);
begin
  Buffer.Stream.Position:=Buffer.posWrite;
  Buffer.Stream.WriteBuffer(Response[0],16);
  Inc(Buffer.posWrite,16);
end;


procedure Write(var Digest:TMD5Digest; var Buffer:TDataBuffer);
begin
  Buffer.Stream.Position:=Buffer.posWrite;
  Buffer.Stream.WriteBuffer(Digest[0],16);
  Inc(Buffer.posWrite,16);
end;

procedure Write(Data:TStream; var Buffer:TDataBuffer);
const
  MAX_CHUNK=1020400;
var
  Chunk:Array[0..MAX_CHUNK] of byte;
  iChunk,iSize,iCount:QWord;
  iRead:LongInt;
begin
  iSize:=Data.Size;
  if iSize>0 then begin
    iCount:=0;
    Data.Position:=0;
    Buffer.Stream.Position:=Buffer.posWrite;

    while iCount<iSize do begin
      iChunk:=MAX_CHUNK;
      if (iCount+iChunk>iSize) then
        iChunk:=iSize-iCount;
      iRead:=Data.Read(Chunk[0],iChunk);
      Inc(iCount,iRead);
      Buffer.Stream.Write(Chunk[0],iRead);
    end;
    {$if defined(cpu64)}
      InterlockedExchangeAdd64(Buffer.posWrite,iSize);
    {$else}
      Inc(Buffer.posWrite,iSize);
    {$endif}
  end;
end;

procedure Write(var utfData:Core.Strings.VarString; var Buffer:TDataBuffer);
var
  iCount:LongInt;
begin
  iCount:=System.Length(utfData);
  Buffer.Stream.Position:=Buffer.posWrite;
  Buffer.Stream.Write(utfData[1],iCount);
  Inc(Buffer.posWrite,iCount);
end;

function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; out Output:Core.Strings.VarString):boolean;
begin
  Result:=false;
  Buffer.Stream.Position:=iStart;
  System.SetLength(Output,iLength);
  if iLength>0 then
    Result:=Buffer.Stream.Read(Output[1],iLength)=iLength;
end;

function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; out Output:TChallenge):boolean;
begin
  FillByte(Output,SizeOf(Output),0);
  Buffer.Stream.Position:=iStart;
  Result:=Buffer.Stream.Read(Output[0],SizeOf(TChallenge))=SizeOf(TChallenge);
end;

function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; Output:TMemoryStream):boolean;
begin
  Result:=false;
  Buffer.Stream.Position:=iStart;
  Core.Streams.CopyFrom(Buffer.Stream,Output,iStart,iLength);
  Result:=true;
end;

function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; FieldDelim,ItemDelim:Core.Strings.VarString; out Output:Core.Arrays.Types.KeyStrings):boolean;
var
  sData:Core.Strings.VarString;
  sName,sValue:Core.Strings.VarString;
  saLines:Core.Arrays.Types.VarString;
  iLcv:LongInt;
  iLoc:LongInt;
  iFieldDelimLen:LongInt;
begin
  Core.Arrays.KeyString.Empty(Output);
  Buffer.Stream.Position:=iStart;
  iFieldDelimLen:=Length(FieldDelim);
  System.SetLength(sData,iLength);
  if iLength>0 then begin
    Result:=Buffer.Stream.Read(sData[1],iLength)=iLength;
    if Result then begin
      Core.Arrays.VarString.fromString(saLines,sData,ItemDelim);
      Try
        for iLcv:=0 to high(saLines) do begin
          iLoc:=System.Pos(FieldDelim,saLines[iLcv]);
          if iLoc>0 then begin
            sName:=System.Copy(saLines[iLcv],1,iLoc-1);

            iLoc:=iLoc+iFieldDelimLen;
            iLength:=System.Length(saLines[iLcv]);
            System.SetLength(sValue,0);
            if (iLoc<iLength) then begin
              sValue:=System.Copy(saLines[iLcv],iLoc,iLength-iLoc+1);
              Core.Strings.Trim(sValue);
            end;
            Core.Arrays.KeyString.Add(Output,sName,sValue);
          end else begin
            System.SetLength(sValue,0);
            Core.Arrays.KeyString.Add(Output,saLines[iLcv],sValue);
          end;
        end;
      finally
        Core.Arrays.VarString.Done(saLines);
      end;
    end;
  end;
end;

function  Extract(Var Buffer:TDatabuffer; iStart,iLength:QWord; out Output:Core.Arrays.Types.VarString; const Defaults:Core.Arrays.SplitOptions=[soClearList] ):boolean;
var
  sData:Core.Strings.VarString;
begin
  Buffer.Stream.Position:=iStart;
  System.SetLength(sData,iLength);
  Try
    if iLength>0 then begin
      Result:=Buffer.Stream.Read(sData[1],iLength)=iLength;
      if Result then
        Core.Arrays.VarString.fromString(Output,sData,#13#10,Defaults);
    end;
  finally
    System.SetLength(sData,0);
  end;
end;

procedure Refactor(Var Buffer:TDatabuffer; Refactor:TStream; iStart:QWord);
var
  iSize:Int64;
  iDiff:Int64;
begin
  Refactor.Size:=0;
  Try
    iSize:=Buffer.Stream.Size;
    iDiff:=iSize-iStart;
    if iDiff>0 then begin
      Core.Streams.CopyFrom(Buffer.Stream,Refactor,iStart,iDiff);
      Core.Streams.Copy(Refactor,Buffer.Stream);
    end else
      Buffer.Stream.Size:=0;
    Buffer.posRead:=0;
    Buffer.posWrite:=Buffer.Stream.Size;
  finally
    Refactor.Size:=0;
  end;
end;

function  Count(Var Buffer:TDataBuffer; Term:Core.Strings.VarString):Int64;
var
  iSize:Int64;
  iLcv:int64;
  iLen:LongInt;
  sBuffer:Core.Strings.VarString;
begin
  Result:=0; iLcv:=0; iLen:=System.Length(Term);  iSize:=Buffer.Stream.Size; SetLength(sBuffer,iLen);
  if iLen>0 then begin
    While (iLcv<iSize) do begin
      Buffer.Stream.Position:=iLcv;
      Buffer.Stream.Read(sBuffer[1],iLen);
      if SysUtils.SameText(sBuffer,Term) then begin
        Result+=1;
      end;
      Inc(iLcv);
    end;
  end;
end;

function  Pos(Var Buffer:TDataBuffer; Term:Core.Strings.VarString):Int64;
var
  iSize:Int64;
  iLcv:int64;
  iLen:LongInt;
  sBuffer:Core.Strings.VarString;
begin
  Result:=-1; iLcv:=0; iLen:=System.Length(Term);  iSize:=Buffer.Stream.Size; SetLength(sBuffer,iLen);
  if iLen>0 then begin
    While (iLcv<iSize) and (Result=-1) do begin
      Buffer.Stream.Position:=iLcv;
      Buffer.Stream.Read(sBuffer[1],iLen);
      if SysUtils.SameText(sBuffer,Term) then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;
end;

function  Pos(Var Buffer:TDataBuffer; Term:Byte):Int64; overload;
var
  iSize:Int64;
  iLcv:int64;
  iByte:Byte;
begin
  Result:=-1; iLcv:=Buffer.posRead; iSize:=Buffer.Stream.Size;
  Buffer.Stream.Position:=iLcv;
  While (iLcv<iSize) and (Result=-1) do begin
    Buffer.Stream.Read(iByte,1);
    if iByte=Term then
      Result:=iLcv;
    Inc(iLcv);
  end;

end;

function  EndOf(Var Buffer:TDataBuffer; Term:Core.Strings.VarString):boolean;
var
  bSeek:boolean;
  iSeek:Int64;
  iLen:Word;
  sLast:Core.Strings.VarString;
begin
  Result:=False; iSeek:=Buffer.Stream.Size; iLen:=System.Length(Term);
  bSeek:=(iSeek>0) and (iSeek>=iLen) and (Buffer.posWrite>0) and (Buffer.posRead<>iSeek);
  if bSeek then begin
    Try
      System.SetLength(sLast,iLen);
      iSeek:=(iSeek-iLen);
      Buffer.Stream.Position:=iSeek;
      Buffer.Stream.Read(sLast[1],iLen);
      Result:=SameText(sLast,Term);
    Finally
      System.SetLength(sLast,0);
    end;
  end;
end;

function  EndOf(Var Buffer:TDataBuffer; Term:Byte):boolean;
var
  iByte:byte;
  iPosition:Cardinal;
begin
  Result:=False;
  if (Buffer.posWrite>0) then begin
    iPosition:=Buffer.Stream.Position;
    Buffer.Stream.Position:=Buffer.Stream.Size-1;
    if Buffer.Stream.Read(iByte,1)=1 then
      Result:=iByte=Term;
    Buffer.Stream.Position:=iPosition;
  end;
end;

function  EndOfLine(Var Buffer:TDataBuffer):boolean;
var
  bSeek:boolean;
  iSeek:Int64;
  byLast:Byte;
begin
  Result:=false; iSeek:=Buffer.Stream.Size;
  bSeek:=(iSeek>0) and (Buffer.posWrite>0) and (Buffer.posRead<>iSeek);
  if bSeek then begin
    iSeek:=(iSeek-1);
    Buffer.Stream.Position:=iSeek;
    Buffer.Stream.Read(byLast,1);
    Result:=(byLast=10);
  end;
end;

function  IsConnected(Socket:Sockets.TSocket):boolean;
Var
  iError:LongInt;
  iResult:LongInt;
  Buffer:Byte;
begin
  iResult:=Sockets.fpRecv(Socket,@Buffer,1,MSG_PEEK);
  if (iResult=-1) then begin
    iError:=Sockets.SocketError;
    if (iError=EsockEWOULDBLOCK) then
      iResult:=1
    else
      iResult:=-1;
  end else if (iResult>=1) then
    iResult:=1;
  Result:=(iResult=1);
end;


function   RemoveEscapeCharacters(var S:Core.Strings.VarString; Refactor:TMemoryStream):Core.Strings.VarString;
var
  iLen,iLcv:LongInt;
  iOrd:LongInt;
  bSkip:boolean;
begin
  Refactor.Size:=0;
  iLen:=System.Length(S); bSkip:=false;
  for iLcv:=1 to iLen do begin
    bSkip:=bSkip or (S[iLcv] = #27);
    if (bSkip=false) then begin
      iOrd:=Ord(s[iLcv]);
      if (iOrd>=32) or (iOrd in [9,10,13]) then
        Refactor.Write(S[iLcv],1);
    end;
    if (bSkip and (S[iLcv]=#126)) then bSkip:=false;
  end;
  Result:=Core.Streams.toString(Refactor);
  Refactor.Size:=0;
end;

procedure   RemoveEscapeCharacters(var sa:Core.Arrays.Types.VarString; Refactor:TMemoryStream);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(sa) do
    sa[iLcv]:=RemoveEscapeCharacters(sa[iLcv],Refactor);
end;

Function RSRErrorToString(Errors:TRSRErrors):Core.Strings.VarString;
var
  saResult:Core.Arrays.Types.VarString;
begin
  SetLength(saResult,0);
  Try
    If eReset in Errors then  Add(@saResult,'eReset');
    If eSocket in Errors then  Add(@saResult,'eSocket');
    If eException in Errors then  Add(@saResult,'eException');
    If eDataSizeWaiting in Errors then Add(@saResult,'eDataSizeWaiting');
    If eTimed in Errors then Add(@saResult,'eTimed');
    If eSend in Errors then Add(@saResult,'eSend');
    If eReceive in Errors then Add(@saResult,'eReceive');
    If eConnect in Errors then Add(@saResult,'eConnect');
    If eHeard in Errors then Add(@saResult,'eHeard');
    If eDisconnect in Errors then Add(@saResult,'eDisconnect');
    If eDNS in Errors then Add(@saResult,'eDNS');
    If eNoSuchDomain in Errors then Add(@saResult,'eNoSuchDomain');
    If eSelect in Errors then Add(@saResult,'eSelect');
    If eFormat in Errors then Add(@saResult,'eFormat');
    If eRemoteServer in Errors then Add(@saResult,'eRemoteServer');
    If eNotImplemented in Errors then Add(@saResult,'eNotImplemented');
    If eRefusedByPolicy in Errors then Add(@saResult,'eRefusedByPolicy');
    If eBuffer in Errors then Add(@saResult,'eBuffer');
    if eAccessDenied in Errors then Add(@saResult,'eAccessDenied');
    if eBind in Errors then Add(@saResult,'eBind');
    if eSSL in Errors then Add(@saResult,'eSSL');

    Result:=toString(saResult,',');
  Finally
    SetLength(saResult,0);
  end;
end;

Function GetServerStats(dtNow:TDateTime):TServerStats;
var
  dtStamp:TDateTime;
begin
  dtStamp:=dtNow;
  Result.Weeks:=dateUtils.WeeksBetween(dtStamp,RSR_Startup);

  dtStamp:=dateUtils.IncWeek(dtStamp,-Result.Weeks);

  Result.Days:=dateUtils.DaysBetween(dtStamp,RSR_Startup);
  dtStamp:=DateUtils.IncDay(dtStamp,-Result.Days);
  Result.Hours:=DateUtils.HoursBetween(dtStamp,RSR_Startup);
  dtStamp:=DateUtils.IncHour(dtStamp,-Result.Hours);
  Result.Minutes:=DateUtils.MinutesBetween(dtStamp,RSR_Startup);
  dtStamp:=DateUtils.IncMinute(dtStamp,-Result.Minutes);
  Result.Seconds:=DateUtils.SecondsBetween(dtStamp,RSR_Startup);
end;

constructor TSocketQueue.Create;
begin
  Inherited Create;
  FItems:=TList.Create;
  FRecycled:=TList.Create;
  InitCriticalSection(FLock);
end;

destructor TSocketQueue.Destroy;
begin
  Shutdown();
  FreeAndNil(FRecycled);
  FreeAndNil(FItems);
  DoneCriticalSection(FLock);
  Inherited Destroy;
end;

function  TSocketQueue.Add(Socket:Sockets.TSocket):PRSR;
begin
  Result:=nil;
  EnterCriticalSection(FLock);
  Try
    Result:=FRecycled.First;
    if (Result=nil) then begin
      New(Result);
      Init(Result^);
    end else begin
      FRecycled.Remove(Result);
    end;
    Empty(Result^);
    Result^.Info.Socket:=Socket;
    FItems.Add(Result);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function  TSocketQueue.Add(var RSRP:PRSR):PRSR;
var
  idx:LongInt;
begin
  Result:=nil;
  EnterCriticalSection(FLock);
  Try
    idx:=FItems.IndexOf(RSRP);
    if (idx>-1) then begin
      Result:=FItems[idx];
    end else begin
      idx:=FRecycled.IndexOf(RSRP);
      if idx>-1 then begin
        Result:=FRecycled[idx];
        FRecycled.Remove(Result);
        FItems.Add(Result);
      end else begin
        Result:=RSRP;
        FItems.Add(Result);
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TSocketQueue.Recycle(rsrP:PRSR);
begin
  EnterCriticalSection(FLock);
  Try
    FItems.Remove(rsrP);
    if FRecycled.IndexOf(rsrP)=-1 then
      FRecycled.Add(rsrP);
    rsrP^.State:=RSR_STATE_RECYCLED;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

function TSocketQueue.Reclaim(rsrP:PRSR):boolean;
var
  iDX:LongInt;
begin
  Result:=false;
  EnterCriticalSection(FLock);
  Try
    iDX:=FRecycled.IndexOf(rsrP);
    if iDX<>-1 then begin
      FRecycled.Remove(rsrP);
      FItems.Add(rsrP);
      Result:=true;
    end;
    rsrP^.State:=rsrP^.State and not RSR_STATE_RECYCLED;
    rsrP^.State:=rsrP^.State or RSR_STATE_POLL;
    if rsrP^.Finite=false then
      rsrP^.State:=rsrP^.State or RSR_STATE_INIT;
    rsrP^.Operations:=RSR_OP_QUEUE;
    rsrP^.Errors:=[];
    RSRP^.TaskError:=0;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;


procedure TSocketQueue.Clear();
var
  rsrLcv:PRSR;
  iLcv:LongInt;
begin
  EnterCriticalSection(FLock);
  Try
    While FItems.Count>0 do begin
      rsrLcv:=FItems.First;
      FItems.Remove(rsrLcv);
      if FRecycled.IndexOf(rsrLCv)=-1 then
        FRecycled.Add(rsrLcv);
    end;
    While FRecycled.Count>0 do begin
      rsrLcv:=FRecycled.First;
      FRecycled.Remove(rsrLcv);
      if FItems.IndexOf(rsrLcv)=-1 then
        FItems.Add(rsrLcv);
    end;
    For iLcv:=0 to FItems.Count-1 do begin
      rsrLcv:=FItems[iLcv];
      Done(rsrLcv^);
      Dispose(rsrLcv);
    end;
    FItems.Clear;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

Function TSocketQueue.Process:LongInt;
var
  bRemove:Boolean;
  iLcv:LongInt;
  rsrP:PRSR;

  function getCount:LongInt;
  begin
    EnterCriticalSection(FLock);
    Try
      Result:=FItems.Count;
    Finally
      LeaveCriticalSection(FLock);
    end;
  end;

begin
  Result:=0;
  iLcv:=0;
  while iLcv<getCount() do begin
    Inc(Result);
    bRemove:=false;
    rsrP:=FItems[iLcv];
    FOnSocketProcess(rsrP,bRemove);
    if bRemove then
      Recycle(rsrP)
    else
      inc(iLcv);
  end;
end;

procedure TSocketQueue.Shutdown();
var
  iLcv:LongInt;
  rsrP:PRSR;
begin
  EnterCriticalSection(FLock);
  Try
    for iLcv:=0 to FItems.Count-1 do begin
      rsrP:=FItems[iLcv];
      Sockets.fpshutdown(rsrP^.Info.Socket,SHUT_RDWR);
      Sockets.CloseSocket(rsrP^.Info.Socket);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure     Add(Var List:TRSRManagers; Item:TRSRManager);  overload;
var
  iLen:LongInt;
begin
  iLen:=System.Length(List);
  System.SetLength(List,iLen+1);
  List[iLen]:=Item;
end;

procedure     Empty(Var Item:TRSRManagers);
var
 iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    FreeAndNil(Item[iLcv]);
  SetLength(Item,0);
end;

procedure     Done(Var Item:TRSRManagers);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    FreeAndNil(Item[iLcv]);
  Finalize(Item);
end;

Constructor  TRSRManager.Create(const aServer:TRSRServer; const InfoP:PSecureInfo; Const UseSSL:Boolean; Const UseMethods:Boolean; Const StackSize:LongInt);
begin
  FSleepP:=RTLEventCreate();
  {$ifdef Unix}
    System.FillByte(FSigOA,SizeOf(FSigOA),0);
    System.FillByte(FSigNA,SizeOf(FSigNA),0);
  {$endif}

  InitCriticalSection(FEntryLock);
  FRunning:=false;
  FServer:=aServer;

  if (aServer<>nil) then
    FKind:=aServer.FKind
  else
    FKind:=0;

  bCycleWarning:=false;

  if UseSSL then begin
    FRecvBufferSize:=RSR_MAN_SSL_BUFF_SIZE;
    FSendBufferSize:=RSR_MAN_SSL_BUFF_SIZE;
  end else begin
    FRecvBufferSize:=RSR_MAN_RECV_BUFF_SIZE;
    FSendBufferSize:=RSR_MAN_SEND_BUFF_MAX_SIZE;
  end;
  FRSRMethodThread:=nil;

  if (UseMethods=true) then
    FRSRMethodThread:=TRSRMethodThread.Create(Self); // spawns a thread for code execution
  dtLastError:=0;
  FSecure:=UseSSL;
  FSSLInfoP:=InfoP;
  FreeOnTerminate:=true;

  {$ifdef useEventQueues}
    SocketEventsInitQueues(FRcvQueue,FWriteQueue,FConQueue,FDisConQueue);
  {$endif}

  FSKT_MAP_INDEX:=0;
  FDNSServersChecked:=False;
  System.SetLength(FRecvBuffer,FRecvBufferSize);
  SetLength(FSendBuffer,FSendBufferSize);

  FDefaultSocketKind:=rsrsTCP;
  FClientLinger.l_linger:=SOCKET_LINGER_CLIENT;
  FClientLinger.l_onoff:=Integer(true);
  FServerLinger.l_linger:=SOCKET_LINGER_SERVER;
  FServerLinger.l_onoff:=Integer(false);
  FSSLLinger.l_linger:=SOCKET_LINGER_SSL;
  FSSLLinger.l_onoff:=Integer(true);


  FTimeout:=RSR_SERVER_TIMEOUT;
  FTimeoutProcedure:=@Perform_TimeoutCheck;
  FRecvProcedures[False]:=@_OnReceive_NoSync;
  FRecvProcedures[True]:=@_OnReceive_Sync;

  cbWaitDuration[False]:=@_Callback_Wait_CycleDuration;
  cbWaitDuration[True]:=@_Callback_Wait_Constant;

  FSockets:=TSocketQueue.Create;
  FSockets.OnProcessSocket:=@cb_RSR_Poll;



  FRefactor:=TMemoryStream.Create;

  FDNSEntries:=TRSRDNSEntries.Create(Self);

  Inherited Create(False{$ifndef Unix}, StackSize{$endif});
end;

Destructor  TRSRManager.Destroy;
begin
  If not Terminated then begin
    Terminate();
    If FRunning then
      WaitFor();
  end;
  if Assigned(FRSRMethodThread) then
    FRSRMethodThread.Terminate();

  FreeAndNil(FDNSEntries);
  FreeAndNil(FSockets);

  RTLEventDestroy(FSleepP);


  FreeAndNil(FRefactor);


  Core.Arrays.Bytes.Done(FRecvBuffer);
  Core.Arrays.Bytes.Done(FSendBuffer);


  DoneCriticalSection(FEntryLock);

  Inherited Destroy;
end;


procedure     TRSRManager.cb_RSR_Close(RSRP:PRSR; Var Handled:Boolean);
begin
  Handled:=True;
  RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
  RSRP^.State:=RSRP^.State and not RSR_STATE_POLL;
  if ( ((RSRP^.State or RSR_STATE_REUSEABLE)<>RSRP^.State) and RSRP^.Finite ) then
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_FINALIZE;
end;

procedure TRSRManager.cb_RSR_StartTLS(RSRP:PRSR; Var Handled:Boolean);
begin
  Handled:=false;
  EntryPoint:='TRSRManager.cb_RSR_StartTLS';
  If (RSRP^.State or RSR_STATE_OPEN=RSRP^.State) then begin
    If (RSRP^.SendBuffer.posWrite=0) then begin
      // Ready to handshake
      Case RSRP^.Info.Kind of
        rsrClient  : begin
          If (RSRP^.SSL.Method=nil) then
            InitSSLForConnect(RSRP);
          RSRP^.LastCall:=SSL_do_handshake(RSRP^.SSL.Handle);
        end;
        rsrServer  : begin
          If (RSRP^.SSL.Method=nil) then
            InitSSLForAcceptAsServerWithStartTLS(RSRP);
          RSRP^.LastCall:=SSL_do_handshake(RSRP^.SSL.Handle);
        end;
      end;
      if (RSRP^.LastCall<>1) then begin
        RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
        if (RSRP^.TaskError<>SSL_ERROR_WANT_READ) and (RSRP^.TaskError<>SSL_ERROR_WANT_WRITE) then begin
          RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
          Include(RSRP^.Errors,eSSL);
          Handled:=true;
        end;
      end else begin
        Core.Utils.Sockets.SetBlockingMode(RSRP^.Info.Socket,BLOCKING_OFF);
        ssl_ctx_set_mode(RSRP^.SSL.Context,SSL_MODE_ENABLE_PARTIAL_WRITE);
        RSRP^.SendEvent:=@cb_RSR_Send_SSL;
        RSRP^.RecvEvent:=@cb_RSR_Read_SSL;
        RSRP^.State:=RSRP^.State or RSR_STATE_SECURE;
        Handled:=true;
      end;
    end;
  end;
end;

procedure     TRSRManager.cb_RSR_Initialize(RSRP:PRSR; Var Handled:Boolean);
begin
  EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize (',IntToStr(RSRP^.Info.Socket),')');
  Handled:=false;
  if (RSRP^.RecvBuffer.Stream=nil) then begin
    EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.RecvBuffer.Stream.Create (',IntToStr(RSRP^.Info.Socket),')');
    RSRP^.RecvBuffer.Stream:=TMemoryStream.Create();
  end;
  if (RSRP^.SendBuffer.Stream=nil) then begin
    EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.SendBuffer.Stream.Create (',IntToStr(RSRP^.Info.Socket),')');
    RSRP^.SendBuffer.Stream:=TMemoryStream.Create();
  end;
  EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.Security (',IntToStr(RSRP^.Info.Socket),')');
  if (RSRP^.State or RSR_STATE_SECURE=RSRP^.State) and (FSecure=true) and (FSSLInfoP<>nil) then begin
    if (RSRP^.Info.Flags or RSR_FLAGS_ACCEPTED=RSRP^.Info.Flags) then begin
      EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.SslAccept (',IntToStr(RSRP^.Info.Socket),')');
      if (RSRP^.SSL.Method=nil) then begin
        Case RSRP^.Info.Kind of
          rsrClient  : InitSSLForConnect(RSRP);
          rsrServer  : InitSSLForAcceptAsServer(RSRP);
        end
      end;
      try
        {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug','Entering');
        {$endif}
        //SetBlockingMode(RSRP^.Info.Socket,false);
        RSRP^.LastCall:=SSL_accept(RSRP^.SSL.Handle);
        //SetBlockingMode(RSRP^.Info.Socket,false);
        if RSRP^.LastCall<>1 then begin
          RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
          if (RSRP^.TaskError<>SSL_ERROR_WANT_READ) and (RSRP^.TaskError<>SSL_ERROR_WANT_WRITE) then begin
            //OnException(EntryPoint,'Debug',Concat('failed level(',IntToStr(RSRP^.LastCall),') code (',IntToStr(RSRP^.TaskError),')'));
            RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
            RSRP^.Operations:=RSRP^.Operations and not RSR_OP_QUEUE;
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_CLOSE;
            Include(RSRP^.Errors,eSSL);
            Handled:=true;
          end;
        end else begin
          ssl_ctx_set_mode(RSRP^.SSL.Context,SSL_MODE_ENABLE_PARTIAL_WRITE);
          RSRP^.State:=RSRP^.State or RSR_STATE_WRITE;
          RSRP^.State:=RSRP^.State or RSR_STATE_OPEN;
          RSRP^.SendEvent:=@cb_RSR_Send_SSL;
          RSRP^.RecvEvent:=@cb_RSR_Read_SSL;
          RSRP^.Operations:=RSRP^.Operations and not RSR_OP_READ;
          RSRP^.Operations:=RSRP^.Operations and not RSR_OP_WRITE;

          {$ifdef useEventQueues}
            SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
            SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
            SocketEventsAttach(FDisConQueue,RSRP^.Info.Socket,sfkDisconnect);
          {$else}
            SocketEventsAttach(RSRP^.Info.Socket,[sfkRead,sfkWrite,sfkDisconnect]);
          {$endif}
          EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.OnInitialize (',IntToStr(RSRP^.Info.Socket),')');

          {$if defined(RSR_DEBUG)}
          OnException(EntryPoint,'Debug','Entering');
          {$endif}
          if (RSRP^.Kind<>rsrsDNS) then OnInitialize(RSRP);
          {$if defined(RSR_DEBUG)}
          OnException(EntryPoint,'Debug','Done');
          {$endif}
          Handled:=true;
        end;
      except
        On E:Exception do OnException(EntryPoint,'Exception',E.Message);
      end;
    end else begin
      EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.SslConnect (',IntToStr(RSRP^.Info.Socket),')');
      try
        {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug','Entering');
        {$endif}
        RSRP^.LastCall:=SSL_connect(RSRP^.SSL.Handle);
        RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
        ssl_ctx_set_mode(RSRP^.SSL.Context,SSL_MODE_ENABLE_PARTIAL_WRITE);
        EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.OnInitialize (',IntToStr(RSRP^.Info.Socket),')');
        {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug','Entering');
        {$endif}
        if (RSRP^.Kind<>rsrsDNS) then OnInitialize(RSRP);
        {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug','Done');
        {$endif}
        Handled:=true;
      except
        On E:Exception do OnException(EntryPoint,'Exception',E.Message);
      end;
    end;
  end else if (RSRP^.Info.Flags or RSR_FLAGS_ACCEPTED=RSRP^.Info.Flags) then begin
    // non SSL Server Socket
    EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.OnInitialize (',IntToStr(RSRP^.Info.Socket),')');

    {$ifdef useEventQueues}
      SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
      SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
      SocketEventsAttach(FDisConQueue,RSRP^.Info.Socket,sfkDisconnect);
    {$else}
      SocketEventsAttach(RSRP^.Info.Socket,[sfkRead,sfkWrite,sfkDisconnect]);
    {$endif}

    SetBlockingMode(RSRP^.Info.Socket,false);
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    if (RSRP^.Kind<>rsrsDNS) then OnInitialize(RSRP);
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Done');
    {$endif}
    Handled:=true;
  end else begin
    // Non SSL regular socket
    SetBlockingMode(RSRP^.Info.Socket,false);
    EntryPoint:=Concat('TRSRManager.cb_RSR_Initialize.OnInitialize (',IntToStr(RSRP^.Info.Socket),')');
    // Socket events no present here.
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    if (RSRP^.Kind<>rsrsDNS) then OnInitialize(RSRP);
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Done');
    {$endif}
    Handled:=true;
  end;
end;

procedure     TRSRManager.cb_RSR_Finalize(RSRP:PRSR; Var Handled:Boolean);
begin
  Handled:=true;
  EntryPoint:=Concat('TRSRManager.cb_RSR_Finalize (',IntToStr(RSRP^.Info.Socket),')');
  If (RSRP^.SSL.Handle<>nil) then
    ReleaseSSL(RSRP);
  if (RSRP^.Info.RequestID<>-1) then begin
    if DNS_MAP[ RSRP^.Info.RequestID]=RSRP then
      DNS_MAP[ RSRP^.Info.RequestID]:=nil
  end;
  EntryPoint:=Concat('TRSRManager.cb_RSR_Finalize.OnFinalize (',IntToStr(RSRP^.Info.Socket),')');
  {$if defined(RSR_DEBUG)}
  OnException(EntryPoint,'Debug','Entering');
  {$endif}
  if Self<>nil then OnFinalize(RSRP);
  {$if defined(RSR_DEBUG)}
  OnException(EntryPoint,'Debug','Done');
  {$endif}
  RSRP^.State:=RSRP^.State or RSR_STATE_RECYCLE;
end;

procedure   TRSRManager.SetRecvBufferSize(Value:DWORD);
begin
  If Value=0 then
    FRecvBufferSize:=RSR_MAN_RECV_BUFF_SIZE
  else
    FRecvBufferSize:=Value;
  // Trigger Re-Adjustment of buffersize
  if FRecvBufferSize<>System.Length(FRecvBuffer) then
    System.SetLength(FRecvBuffer,FRecvBufferSize);
  // Trigger Re-Adjustment of buffersize
end;

procedure   TRSRManager.SetSendBufferSize(Value:DWORD);
begin
  if Value<=0 then
    FSendBufferSize:=RSR_MAN_SEND_BUFF_MIN_SIZE
  else
    FSendBufferSize:=Value;
  if FSendBufferSize<>System.Length(FSendBuffer) then
    System.SetLength(FSendBuffer,FSendBufferSize);
  // Trigger Re-Adjustment of buffersize
end;

procedure   TRSRManager.ReleaseSSL(RSRP:PRSR);
var
  iLcv:LongInt;
begin
  EntryPoint:=Concat('TRSRManager.ReleaseSSL.SSL_clear (',IntToStr(RSRP^.Info.Socket),')');
  {$if defined(RSR_DEBUG)}
  OnException(EntryPoint,'Debug','Entering');
  {$endif}
  try
    if RSRP^.SSL.BIO<>nil then
      BIO_free(RSRP^.SSL.BIO);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);

    RSRP^.LastCall:=SSL_clear(RSRP^.SSL.Handle);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug',Concat('SSL_clear Error(',IntToStr(RSRP^.TaskError),')'));
    {$endif}
  except
    On E:Exception do begin
      OnException(EntryPoint,'Exception',E.Message);
    end;
  end;

  EntryPoint:=Concat('TRSRManager.ReleaseSSL.SSL_free (',IntToStr(RSRP^.Info.Socket),')');
  {$if defined(RSR_DEBUG)}
  OnException(EntryPoint,'Debug','Entering');
  {$endif}
  try
    SSL_free(RSRP^.SSL.Handle);
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug',Concat('SSL_free Error('));
    {$endif}
  except
    On E:Exception do begin
      OnException(EntryPoint,'Exception',E.Message);
    end;
  end;
  EntryPoint:='TRSRManager.ReleaseSSL.SSL_ctx_free';
  {$if defined(RSR_DEBUG)}
  OnException(EntryPoint,'Debug','Entering');
  {$endif}
  try
    SSL_ctx_free(RSRP^.SSL.Context);
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug',Concat('SSL_ctx_free Error'));
    {$endif}
  except
    On E:Exception do begin
      OnException(EntryPoint,'Exception',E.Message);
    end;
  end;
  EntryPoint:='TRSRManager.ReleaseSSL.X509_free';
  {$if defined(RSR_DEBUG)}
  OnException(EntryPoint,'Debug','Entering');
  {$endif}
  for iLcv:=0 to High(RSRP^.SSL.Manifest.List) do begin
    try
      if RSRP^.SSL.Manifest.List[iLcv]^.Cert<>nil then begin
        Encryption.SSL.X509_free(RSRP^.SSL.Manifest.List[iLcv]^.Cert);
        RSRP^.SSL.Manifest.List[iLcv]^.Cert:=nil;
        {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug',Concat('X509_free Error'));
        {$endif}
      end;
    except
      On E:Exception do begin
        OnException(EntryPoint,'Exception',E.Message);
      end;
    end;
  end;
  Empty(RSRP^.SSL);
end;

procedure   TRSRManager.InitSSLForConnect(RSRP:PRSR);
var
  iLcv:LongInt;
  store:Encryption.SSL.X509_STORE;
begin
  EntryPoint:='TRSRManager.InitSSLForConnect';

  RSRP^.SSL.Method:=SSLv23_client_method();
  RSRP^.SSL.Context:=SSL_ctx_new(RSRP^.SSL.Method);

  //SSL_CTX_set_tmp_dh(RSRP^.SSL.Context,get_dh512());

  Encryption.SSL.SSL_CTX_set_cipher_list(RSRP^.SSL.Context,CIPHER_LIST_CLIENT);
  SslCtxSetVerify(RSRP^.SSL.Context,SSL_VERIFY_NONE, nil);
  SSLCTXSetOptions(RSRP^.SSL.Context, SSL_OP_SINGLE_DH_USE);

  // Initialize Private Key
  if SSL_CTX_use_RSAPrivateKey_ASN1(RSRP^.SSL.Context,@FSSLInfoP^.keyData[0],FSSLInfoP^.keyLen)<>1 then
    OnException('TRSRServer.InitSSLForConnect.SSL_CTX_use_RSAPrivateKey_ASN1','Exception',Concat('Open SSL Error ',IntToStr(Encryption.SSL.ERR_get_error())));

  // Initialize Cert Objects
  Copy(FSSLInfoP^.Manifest,RSRP^.SSL.Manifest);
  for iLcv:=0 to High(RSRP^.SSL.Manifest.List) do
    RSRP^.SSL.Manifest.List[iLcv]^.Cert:=Encryption.SSL.d2i_X509(nil,@RSRP^.SSL.Manifest.List[iLcv]^.crtData,RSRP^.SSL.Manifest.List[iLcv]^.crtLen);

  If Length(RSRP^.SSL.Manifest.List)>1 then begin
    // Add Preliminary Certs to Store
    Store:=Encryption.SSL.SSL_CTX_get_cert_store(RSRP^.SSL.Context);
    for iLcv:=1 to High(RSRP^.SSL.Manifest.List) do begin
       Encryption.SSL.X509_STORE_add_cert(store,RSRP^.SSL.Manifest.List[iLcv]^.Cert);
    end;
  End;

  SSL_CTX_use_certificate(RSRP^.SSL.Context,RSRP^.SSL.Manifest.List[0]^.Cert);

  //for iLcv:=1 to High(RSRP^.SSL.Manifest.List) do
  //  Encryption.SSL.SSL_CTX_add_extra_chain_cert(RSRP^.SSL.Context,RSRP^.SSL.Manifest.List[iLcv]^.Cert);

  EntryPoint:=Concat('TRSRManager.InitSSLForConnect.SslNew (',IntToStr(RSRP^.Info.Socket),')');
  try
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    RSRP^.SSL.Handle:=SSL_new(RSRP^.SSL.Context);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
  except
    On E:Exception do OnException(EntryPoint,'Exception',E.Message);
  end;
  EntryPoint:=Concat('TRSRManager.InitSSLForConnect.SslSetFd (',IntToStr(RSRP^.Info.Socket),')');
  try
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    RSRP^.LastCall:=SSL_set_fd(RSRP^.SSL.Handle,RSRP^.Info.Socket);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
  except
    On E:Exception do OnException(EntryPoint,'Exception',E.Message);
  end;
  SSL_set_connect_state(RSRP^.SSL.Handle);
end;

procedure   TRSRManager.InitSSLForAcceptAsServer(RSRP:PRSR);
var
  iLcv:LongInt;
  Store:X509_STORE;
begin
  EntryPoint:='TRSRManager.InitSSLForAcceptAsServer';

  RSRP^.SSL.Method:=TLSv1_server_method();
  RSRP^.SSL.Context:=SSL_ctx_new(RSRP^.SSL.Method);

  //SSL_CTX_set_tmp_dh(RSRP^.SSL.Context,get_dh512());

  Encryption.SSL.SSL_CTX_set_cipher_list(RSRP^.SSL.Context,CIPHER_LIST_SERVER);
  SslCtxSetVerify(RSRP^.SSL.Context,SSL_VERIFY_NONE, nil);
  SSL_ctx_set_mode(RSRP^.SSL.Context, SSL_MODE_AUTO_RETRY);

  if SSL_CTX_use_RSAPrivateKey_ASN1(RSRP^.SSL.Context,@FSSLInfoP^.keyData[0],FSSLInfoP^.keyLen)<>1 then
    OnException('TRSRServer.InitSSLForAcceptAsServer.SSL_CTX_use_RSAPrivateKey_ASN1','Exception',Concat('SSL Error ',IntToStr(Encryption.SSL.ERR_get_error())));


  // Initialize Cert Objects
  Copy(FSSLInfoP^.Manifest,RSRP^.SSL.Manifest);
  for iLcv:=0 to High(RSRP^.SSL.Manifest.List) do
    RSRP^.SSL.Manifest.List[iLcv]^.Cert:=Encryption.SSL.d2i_X509(nil,@RSRP^.SSL.Manifest.List[iLcv]^.crtData,RSRP^.SSL.Manifest.List[iLcv]^.crtLen);

  If Length(RSRP^.SSL.Manifest.List)>1 then begin
    // Add Preliminary Certs to Store
    Store:=Encryption.SSL.SSL_CTX_get_cert_store(RSRP^.SSL.Context);
    for iLcv:=1 to High(RSRP^.SSL.Manifest.List) do begin
       Encryption.SSL.X509_STORE_add_cert(store,RSRP^.SSL.Manifest.List[iLcv]^.Cert);
    end;
  End;

  SSL_CTX_use_certificate(RSRP^.SSL.Context,RSRP^.SSL.Manifest.List[0]^.Cert);

  EntryPoint:=Concat('TRSRManager.InitSSLForAcceptAsServer.SslNew (',IntToStr(RSRP^.Info.Socket),')');
  try
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    RSRP^.SSL.Handle:=SSL_new(RSRP^.SSL.Context);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
  except
    On E:Exception do OnException(EntryPoint,'Exception',E.Message);
  end;
  EntryPoint:=Concat('TRSRManager.InitSSLForAcceptAsServer.SslSetFd (',IntToStr(RSRP^.Info.Socket),')');
  try
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    RSRP^.LastCall:=SSL_set_fd(RSRP^.SSL.Handle,RSRP^.Info.Socket);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
  except
    On E:Exception do OnException(EntryPoint,'Exception',E.Message);
  end;
  SSL_set_accept_state(RSRP^.SSL.Handle);
end;

procedure   TRSRManager.InitSSLForAcceptAsServerWithStartTLS(RSRP:PRSR);
var
  iLcv:LongInt;
  Store:X509_STORE;
begin
  EntryPoint:='TRSRManager.InitSSLForAcceptAsServerWithStartTLS';

  RSRP^.SSL.Method:=SslMethodV23();{ chase won't work TLSv1_1_server_method()}
  RSRP^.SSL.Context:=SSL_ctx_new(RSRP^.SSL.Method);

  //SSL_CTX_set_tmp_dh(RSRP^.SSL.Context,get_dh512());

  Encryption.SSL.SSL_CTX_set_cipher_list(RSRP^.SSL.Context,CIPHER_LIST_SERVER);
  SslCtxSetVerify(RSRP^.SSL.Context,SSL_VERIFY_NONE, nil);
  SSL_ctx_set_mode(RSRP^.SSL.Context, SSL_MODE_AUTO_RETRY);

  if SSL_CTX_use_RSAPrivateKey_ASN1(RSRP^.SSL.Context,@FSSLInfoP^.keyData[0],FSSLInfoP^.keyLen)<>1 then
    OnException('TRSRServer.InitSSLForAcceptAsServerWithStartTLS.SSL_CTX_use_RSAPrivateKey_ASN1','Exception',Concat('Open SSL Error ',IntToStr(Encryption.SSL.ERR_get_error())));

  Copy(FSSLInfoP^.Manifest,RSRP^.SSL.Manifest);
  for iLcv:=0 to High(RSRP^.SSL.Manifest.List) do
    RSRP^.SSL.Manifest.List[iLcv]^.Cert:=Encryption.SSL.d2i_X509(nil,@RSRP^.SSL.Manifest.List[iLcv]^.crtData,RSRP^.SSL.Manifest.List[iLcv]^.crtLen);

  Store:=SSL_CTX_get_cert_store(RSRP^.SSL.Context);

  for iLcv:=1 to High(RSRP^.SSL.Manifest.List) do
    RSRP^.LastCall:=X509_STORE_add_cert(Store,RSRP^.SSL.Manifest.List[iLcv]^.Cert);

  if RSRP^.SSL.Manifest.List[0]^.Cert=nil then
    OnException('TRSRServer.InitSSLForAcceptAsServer.d2i_X509','Exception',Concat('SSL Error ',IntToStr(Encryption.SSL.ERR_get_error())));

  SSL_CTX_use_certificate(RSRP^.SSL.Context,RSRP^.SSL.Manifest.List[0]^.Cert);

  EntryPoint:=Concat('TRSRManager.InitSSLForAcceptAsServerWithStartTLS.SslNew (',IntToStr(RSRP^.Info.Socket),')');
  try
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    RSRP^.SSL.Handle:=SSL_new(RSRP^.SSL.Context);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
  except
    On E:Exception do OnException(EntryPoint,'Exception',E.Message);
  end;

  SetBlockingMode(RSRP^.Info.Socket,BLOCKING_ON);

  EntryPoint:=Concat('TRSRManager.InitSSLForAcceptAsServerWithStartTLS.SslSetFd (',IntToStr(RSRP^.Info.Socket),')');
  try
    {$if defined(RSR_DEBUG)}
    OnException(EntryPoint,'Debug','Entering');
    {$endif}
    RSRP^.LastCall:=SSL_set_fd(RSRP^.SSL.Handle,RSRP^.Info.Socket);
    RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
  except
    On E:Exception do OnException(EntryPoint,'Exception',E.Message);
  end;
  SSL_set_accept_state(RSRP^.SSL.Handle);
end;


procedure   TRSRManager.automationSuccess(RSRP:PRSR);
var
  iCt:LongInt;
begin
  iCt:=System.Length(RSRP^.Automation.Items);
  if (RSRP^.Automation.Index<iCt) then begin
    RSRP^.Automation.Items[RSRP^.Automation.Index]^.Complete:=true;
    if Assigned(RSRP^.Automation.Items[RSRP^.Automation.Index]^.OnSuccess) then
      RSRP^.Automation.Items[RSRP^.Automation.Index]^.OnSuccess(RSRP);
  end;
  if (RSRP^.Automation.Index<iCt) then begin
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_AUTOMATIC_NEXT;
  end else begin
    RSRP^.Automation.Index:=-1;
    RSRP^.State:=RSRP^.State and not RSR_STATE_AUTOMATIC_STEPS;
    RSRP^.Operations:=RSRP^.Operations and not RSR_OP_AUTOMATIC_NEXT;
  end;
end;

procedure   TRSRManager.automationSuccessIP(RSRP:PRSR);
var
  iLcv,iIPCt:LongInt;
  iSvrAddr:QWord;
begin
  iIPCt:=System.Length(RSRP^.DNS.Answers);
  for iLcv:=0 to iIPCt-1 do begin
    iSvrAddr:=Core.Utils.Sockets.InAddrFromStr(RSRP^.DNS.Answers[iLcv]);
    if iSvrAddr<>0 then
      Core.Arrays.LargeWord.Add(iSvrAddr,RSRP^.Info.IPs);
    RSRP^.Info.idxIP:=0;
  end;
  RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;
end;

procedure   TRSRManager.automationSuccessMX(RSRP:PRSR);
var
  iLen:LongInt;
begin
  iLen:=System.Length(RSRP^.DNS.Answers);
  if (iLen>0) then begin
    RSRP^.Info.Server:=RSRP^.DNS.Answers[0];
  end;
  RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;
end;

procedure   TRSRManager.automationSuccessConnect(RSRP:PRSR);
begin

end;

procedure   TRSRManager.automationFailIP(RSRP:PRSR);
begin
  RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;
end;

procedure   TRSRManager.automationFailMX(RSRP:PRSR);
begin
  RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;
end;

procedure   TRSRManager.automationFailConnect(RSRP:PRSR);
begin
  // Retry using next Ip
end;

procedure   TRSRManager.Connect(RSRP:PRSR; Domain:Core.Strings.VarString; Port:word; MXLookup:boolean);
begin
  if ( (RSRP^.State or RSR_STATE_RECYCLED)=RSRP^.State) then begin
    FSockets.Reclaim(RSRP);
    Allocate(RSRP^.Info.Kind,RSRP);
  end;

  RSRP^.State:=RSRP^.State or RSR_STATE_AUTOMATIC_STEPS;
  RSRP^.Operations:=RSRP^.Operations or RSR_OP_AUTOMATIC_NEXT;
  RSRP^.Info.Server:=Domain;
  RSRP^.Info.Port:=Port;
  RSRP^.Info.idxIP:=0;
  Core.Arrays.LargeWord.Empty(RSRP^.Info.IPs);
  Empty(RSRP^.Automation);
  if MXLookup then begin
    SetLength(RSRP^.Automation.Items,3);
    System.New(RSRP^.Automation.Items[0]);
    System.New(RSRP^.Automation.Items[1]);
    System.New(RSRP^.Automation.Items[2]);
    Init(RSRP^.Automation.Items[0]^,rsrSKDnsMX);
    Init(RSRP^.Automation.Items[1]^,rsrSKDnsIP);
    Init(RSRP^.Automation.Items[2]^,rsrSKConnect);

    RSRP^.Automation.Items[0]^.OnSuccess:=@automationSuccessMX;
    RSRP^.Automation.Items[0]^.OnFailure:=@automationFailMX;
    RSRP^.Automation.Items[1]^.OnSuccess:=@automationSuccessIP;
    RSRP^.Automation.Items[1]^.OnFailure:=@automationFailIP;
    RSRP^.Automation.Items[2]^.OnSuccess:=@automationSuccessConnect;
    RSRP^.Automation.Items[2]^.OnFailure:=@automationFailConnect;
  end else begin
    SetLength(RSRP^.Automation.Items,2);
    System.New(RSRP^.Automation.Items[0]);
    System.New(RSRP^.Automation.Items[1]);

    Init(RSRP^.Automation.Items[0]^,rsrSKDnsIP);
    Init(RSRP^.Automation.Items[1]^,rsrSKConnect);

    RSRP^.Automation.Items[0]^.OnSuccess:=@automationSuccessIP;
    RSRP^.Automation.Items[0]^.OnFailure:=@automationFailIP;
    RSRP^.Automation.Items[1]^.OnSuccess:=@automationSuccessConnect;
    RSRP^.Automation.Items[1]^.OnFailure:=@automationFailConnect;
  end;
end;

procedure   TRSRManager.SetTimeout(Value:DWord);
begin
  FTimeout:=Value;
  If Value=0 then
    FTimeoutProcedure:=@Perform_TimeoutCheck_Nil
  else
    FTimeoutProcedure:=@Perform_TimeoutCheck;
end;

procedure   TRSRManager.SetSecure(Value:Boolean);
begin
  if (FSecure<>Value) then begin
    if Value=true then begin
      if (FSSLInfoP<>nil) and (Length(FSSLInfoP^.Manifest.List)>0) and (FSSLInfoP^.keyLen>0) then begin
        FSecure:=True;
      end;
    end else begin
      FSecure:=False;
    end;
  end;
end;

procedure   TRSRManager.ProcessReceiveDNSBuffer(RSRP:PRSR);
var
  Read      : Boolean;
  wID       : Word;
  rsrTarget : PRSR;

  procedure PushFormatError;
  begin
    Include(rsrTarget^.Errors,EDNS);
    Include(rsrTarget^.Errors,EFormat);
  end;
  procedure PushServerError;
  begin
    Include(rsrTarget^.Errors,EDNS);
    Include(rsrTarget^.Errors,eRemoteServer);
  end;
  procedure PushNoSuchDomainError;
  begin
    Include(rsrTarget^.Errors,EDNS);
    Include(rsrTarget^.Errors,eNoSuchDomain);
  end;
  procedure PushNotImplementedError;
  begin
    Include(rsrTarget^.Errors,EDNS);
    Include(rsrTarget^.Errors,eNotImplemented);
  end;
  procedure PushPolicyError;
  begin
    Include(rsrTarget^.Errors,EDNS);
    Include(rsrTarget^.Errors,eRefusedByPolicy);
  end;

  procedure PushProcessRead;
  begin
    EntryPoint:='TRSRManager.ProcessReceiveDNSBuffer.PushProcessRead';
    Read:=RSRP^.DNS.StreamIn(RSRP^.RecvBuffer.Stream,RSRP^.RecvBuffer.posRead,RSRP^.RecvBuffer.Stream.Size);
    If Read then begin
      If (rsrTarget^.State <> RSR_STATE_NONE) then begin
        case RSRP^.DNS.RCode of
          0: RSRP^.DNS.Cache();
          1: PushFormatError;
          2: PushServerError;
          3: PushNoSuchDomainError;
          4: PushNotImplementedError;
          5: PushPolicyError;
        end;
        EntryPoint:='TRSRManager.ProcessReceiveDNSBuffer.OnDNSResult';
        If (rsrTarget^.State or RSR_STATE_AUTOMATIC_STEPS=rsrTarget^.State) then begin
          automationSuccess(rsrTarget);
        end else
          OnDNSResult(rsrTarget);
      end;
    end;
    RSR.Refactor(RSRP^.RecvBuffer,FRefactor,RSRP^.RecvBuffer.posRead);
  end;
begin
  EntryPoint:='TRSRManager.ProcessReceiveDNSBuffer';
  Repeat
    EntryPoint:='TRSRManager.ProcessReceiveDNSBuffer.PeekAtID';
    Read:=PeekAtID(wID,RSRP^.RecvBuffer.Stream,RSRP^.RecvBuffer.posRead);
    if Read then begin
      EntryPoint:=Concat('TRSRManager.ProcessReceiveDNSBuffer.Read (',IntToStr(wID),')');
      rsrTarget:=DNS_MAP[wID];
      if (rsrTarget<>nil) then begin
        EntryPoint:=Concat('TRSRManager.ProcessReceiveDNSBuffer.Reading (',IntToStr(wID),')');
        rsrTarget^.DNS:=RSRP^.DNS;
        rsrTarget^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
        PushProcessRead();
        DNS_MAP[wID]:=nil;
        rsrTarget^.DNS:=nil;
      end else begin // clear out any entries - this could be problematic
        RSR.Refactor(RSRP^.RecvBuffer,FRefactor,RSRP^.RecvBuffer.posWrite);
      end;
    end else begin
      // Throw away contents and let requester timeout
      RSR.Refactor(RSRP^.RecvBuffer,FRefactor,RSRP^.RecvBuffer.posWrite);
    end;
    EntryPoint:='TRSRManager.ProcessReceiveDNSBuffer.Loop (Possible)';
  Until (EngineFailure=true) or (Terminated=true) or (RSRP^.RecvBuffer.posWrite=0) or (Read=false);
  Empty(RSRP^.RecvBuffer);
end;


Function    TRSRManager._Callback_Wait_Constant:WORD;
begin
  Result:=RSR_WAIT_MIN;
end;

Function    TRSRManager._Callback_Wait_CycleDuration:WORD;
begin
  Result:=Math.Min(MAX(dwCycleDuration,RSR_WAIT_MIN),RSR_WAIT_MAX);
end;

procedure   TRSRManager.cb_RSR_Poll(RSRP:PRSR; Var Handled:Boolean);
begin
  EntryPoint:='TRSRManager.cb_RSR_Poll';
  Handled:=False;
  if RSRP^.State or RSR_STATE_RECYCLE = RSRP^.State then begin
    EntryPoint:='TRSRManager.cb_RSR_Poll.Recycle';
    Handled:=True;
    if (RSRP^.Info.Socket>0) then begin
      RSR_MAP[RSRP^.Info.Socket]:=nil;
      MAN_MAP[RSRP^.Info.Socket]:=nil;
      EntryPoint:='TRSRManager.cb_RSR_Poll.Recycle.Detach';
      {$ifdef useEventQueues}
        SocketEventsDetach(FWriteQueue,RSRP^.Info.Socket);
        SocketEventsDetach(FRcvQueue,RSRP^.Info.Socket);
        SocketEventsDetach(FConQueue,RSRP^.Info.Socket);
        SocketEventsDetach(FDisConQueue,RSRP^.Info.Socket);
      {$else}
        SocketEventsDetach(RSRP^.Info.Socket);
      {$endif}

      Sockets.fpshutdown(RSRP^.Info.Socket,SHUT_RDWR);
      if (RSRP^.SSL.Handle<>nil) then begin
        EntryPoint:='TRSRManager.cb_RSR_Poll.ReleaseSSL';
        try
          ReleaseSSL(RSRP);
        except
          On E:Exception do OnException(EntryPoint,'Exception',E.Message);
        end;
      end;
      Sockets.CloseSocket(RSRP^.Info.Socket);
      Case RSRP^.Kind of
        rsrsTCP: begin
          {$ifdef cpu64}
            InterlockedDecrement64(RSR_Connection_Count);
          {$else}
            Dec(RSR_Connection_Count);
          {$endif}
          end;
        rsrsUDP: begin
          {$ifdef cpu64}
            InterlockedDecrement64(RSR_Stream_Count);
          {$else}
            Dec(RSR_Stream_Count);
          {$endif}
          end;
        rsrsDNS: begin
          {$ifdef cpu64}
            InterlockedDecrement64(RSR_Stream_Count);
          {$else}
            Dec(RSR_Stream_Count);
          {$endif}
          end;
      end;
      Empty(RSRP^);
    end;
  end else begin
    EntryPoint:='TRSRManager.cb_RSR_Poll.TimeoutProcedure';
    FTimeoutProcedure(RSRP);
    if RSRP^.Operations or RSR_OP_CLOSE=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Close';
      Handled:=False;
      cb_RSR_Close(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_CLOSE;
    end;
    if RSRP^.Operations or RSR_OP_INITIALIZE=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Initialize';
      Handled:=False;
      cb_RSR_Initialize(RSRP,Handled);
      if (Handled=true) then begin
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_INITIALIZE;
        RSRP^.State:=RSRP^.State or RSR_STATE_INIT;
      end;
    end;
    if ((RSRP^.Operations or RSR_OP_QUEUE=RSRP^.Operations) and ((RSRP^.State or RSR_STATE_INIT)=RSRP^.State)) then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Queue';
      Handled:=False;
      cb_RSR_Queue(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_QUEUE;
    end;
    if RSRP^.Operations or RSR_OP_AUTOMATIC_NEXT=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Automation-Next';
      Handled:=False;
      cb_RSR_Automatic_Next(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_AUTOMATIC_NEXT;
    end;
    if (RSRP^.Operations or RSR_OP_READ=RSRP^.Operations) and (RSRP^.RecvEvent<>nil) then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.RecvEvent';
      Handled:=false;
      RSRP^.RecvEvent(RSRP,Handled);
      {$ifdef Windows}
        if Handled then
          Handled:=Core.Utils.Sockets.DataSizeWaiting(RSRP^.Info.Socket)=0;
      {$endif}
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_READ;
    end;
    if (RSRP^.Operations or RSR_OP_WRITE=RSRP^.Operations) and (RSRP^.SendEvent<>nil) then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.SendEvent';
      Handled:=False;
      RSRP^.SendEvent(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_WRITE;
    end;
    if (RSRP^.Operations or RSR_OP_WRITABLE=RSRP^.Operations) then begin
      RSRP^.State:=RSRP^.State or RSR_STATE_WRITE;
      RSRP^.Operations:=RSRP^.Operations and not RSR_OP_WRITABLE;
      Handled:=true;
    end;
    if RSRP^.Operations or RSR_OP_CONNECTED=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Connected';
      Handled:=False;
      RSRP^.State:=RSRP^.State or RSR_STATE_OPEN or RSR_STATE_POLL or RSR_STATE_WRITE;
      RSRP^.State:=RSRP^.State and not RSR_STATE_CONNECTING;
      {$ifdef useEventQueues}
        SocketEventsDetach(FConQueue,RSRP^.Info.Socket);
        SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
        SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
        SocketEventsAttach(FDisConQueue,RSRP^.Info.Socket,sfkDisconnect);
      {$else}
        SocketEventsAttach(RSRP^.Info.Socket,[sfkWrite,sfkRead,sfkDisconnect]);
      {$endif}

      cb_RSR_Connected(RSRP,Handled);

      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_CONNECTED;
    end;
    if RSRP^.Operations or RSR_OP_DISCONNECTED=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Disconnected';
      Handled:=False;
      {$ifdef useEventQueues}
        SocketEventsDetach(FDisConQueue,RSRP^.Info.Socket);
        SocketEventsDetach(FConQueue,RSRP^.Info.Socket);
        SocketEventsDetach(FRcvQueue,RSRP^.Info.Socket);
        SocketEventsDetach(FWriteQueue,RSRP^.Info.Socket);
      {$else}
        SocketEventsDetach(RSRP^.Info.Socket);
      {$endif}
      cb_RSR_Disconnected(RSRP,Handled);
      if Handled then begin
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_DISCONNECTED;
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_FINALIZE;
      end;
    end;
    if RSRP^.Operations or RSR_OP_ERROR=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Error';
      RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
      Handled:=False;
      cb_RSR_Error(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_ERROR;
    end;
    if RSRP^.Operations or RSR_OP_FINALIZE=RSRP^.Operations then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.Finalize';
      Handled:=False;
      cb_RSR_Finalize(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_FINALIZE;
    end;
    if (RSRP^.Operations or RSR_OP_STARTTLS=RSRP^.Operations) then begin
      EntryPoint:='TRSRManager.cb_RSR_Poll.StartTLS';
      Handled:=False;
      cb_RSR_StartTLS(RSRP,Handled);
      if Handled then
        RSRP^.Operations:=RSRP^.Operations and not RSR_OP_STARTTLS;
    end;
    Handled:=False;
  end;
end;

procedure TRSRManager.cb_RSR_Automatic_Next(RSRP:PRSR; var Handled:Boolean);
var
  iCount:LongInt;
  StepP:PRSRStep;

  procedure Push_DNS_Check;
  begin
    if FDNSServersChecked=false then begin
      RSR.DNS.Check_DNS_Servers;
      FDNSServersChecked:=true;
    end;
  end;

  procedure Push_DNS_IP;
  var
    iIP:QWord;
  begin
    Push_DNS_Check;
    iIP:=Core.Utils.Sockets.InAddrFromStr(RSRP^.Info.Server);
    if (iIP=0) then begin
      FHandled:=Handled;
      DNSLookup(RSRP,RSRP^.Info.Server,[dnsIP]);
      Handled:=FHandled;
    end else begin
      Core.Arrays.VarString.Add(RSRP^.DNS.Answers,RSRP^.Info.Server);
      automationSuccess(RSRP);
      Handled:=false; // otherwise it will remove flag
    end;
  end;

  procedure Push_DNS_MX;
  begin
    Push_DNS_Check;
    DNSLookup(RSRP,RSRP^.Info.Server,[dnsMX]);
  end;

  procedure Push_Connect;
  var
    iCt:LongInt;
  begin
    iCt:=System.Length(RSRP^.Info.IPs);
    if (RSRP^.Info.idxIP>iCt) then
      RSRP^.Info.idxIP:=0;
    if (iCt>0) then
      Connect(RSRP,RSRP^.Info.IPs[RSRP^.Info.idxIP],RSRP^.Info.Port);
  end;

begin
  Handled:=True;
  Try
    EntryPoint:='TRSRManager.cb_RSR_AUTOMATIC_NEXT';
    Inc(RSRP^.Automation.Index);
    iCount:=System.Length(RSRP^.Automation.Items);
    if (RSRP^.Automation.Index<iCount) then begin
      StepP:=RSRP^.Automation.Items[RSRP^.Automation.Index];
      case StepP^.Kind of
        rsrSKDnsIP   : Push_DNS_IP();
        rsrSKDnsMX   : Push_DNS_MX();
        rsrSKConnect : Push_Connect();
      end;
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_AUTOMATIC_NEXT','Exception',E.Message);
  end;
end;


procedure   TRSRManager.cb_RSR_Connected(RSRP:PRSR; Var Handled:Boolean);
begin
  Handled:=True;
  Try
    EntryPoint:='TRSRManager.cb_RSR_Connected';
    If (RSRP^.TaskError=0) or (RSRP^.TaskError=EsockEWOULDBLOCK) or (RSRP^.TaskError=EOperationInProgress) then begin
      RSRP^.TaskError:=0;
      RSRP^.State:=RSRP^.State or RSR_STATE_OPEN or RSR_STATE_POLL;
      if (RSRP^.State or RSR_STATE_AUTOMATIC_STEPS = RSRP^.State) then
        automationSuccess(RSRP);
      EntryPoint:='TRSRManager.cb_RSR_Connected.OnConnect';
      OnConnect(RSRP);
    end else begin
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
      Include(RSRP^.Errors,eConnect);
      EntryPoint:='TRSRManager.cb_RSR_Connected.Error';
      RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
      RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Connected','Exception',E.Message);
  end;
end;

procedure   TRSRManager.cb_RSR_Disconnected(RSRP:PRSR; Var Handled:Boolean);
begin
  Handled:=True;
  Try
    EntryPoint:='TRSRManager.cb_RSR_Dissconnected';
    RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    Sockets.fpshutdown(RSRP^.Info.Socket,SHUT_RDWR);
    EntryPoint:='TRSRManager.cb_RSR_Dissconnected.OnDisconnect';
    Try
      OnDisconnect(RSRP);
    finally
      If ( (RSRP^.State or RSR_STATE_REUSEABLE) <> RSRP^.State) and (RSRP^.Finite) then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_FINALIZE;
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Dissconnected','Exception',E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_Error(RSRP:PRSR; var Handled:Boolean);
begin
  Handled:=True;
  Try
    EntryPoint:='TRSRManager.cb_RSR_Error';
    Try
      OnError(RSRP);
    finally
      if (RSRP^.Errors<>[]) then
        Close(RSRP);
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Error','Exception',E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_Queue(RSRP:PRSR; var Handled:Boolean);
begin
  Handled:=True;
  Try
    EntryPoint:='TRSRManager.cb_RSR_Queue';
    RSRP^.State:=RSRP^.State or RSR_STATE_QUEUED;
    if (RSRP^.Kind<>rsrsDNS) then OnQueue(RSRP);
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Queue','Exception',E.Message);
  End;
end;

procedure TRSRManager.cb_RSR_SendBufferIncrement(RSRP:PRSR);
begin
  {$if defined(cpu64)}
    InterlockedIncrement64(cntrSentCount);
    InterlockedExchangeAdd64(cntrSentBytes,RSRP^.LastCall);
  {$else}
    Dec(RSR.cntrSentCount);
    Inc(RSR.cntrSentBytes,RSRP^.LastCall);
  {$endif}
  Inc(RSRP^.SendBuffer.posRead,RSRP^.LastCall);
  if (RSRP^.SendBuffer.posRead>=RSRP^.SendBuffer.posWrite) then
    RSR.Refactor(RSRP^.SendBuffer,FRefactor,RSRP^.SendBuffer.posRead);
  if (RSRP^.Throttle.Enabled=true) then begin
    RSRP^.Throttle.Consumption:=RSRP^.RecvBuffer.posWrite+RSRP^.SendBuffer.posWrite;
  end;
end;

procedure TRSRManager.cb_RSR_SendBufferIncrementInterlocked(RSRP:PRSR);
begin
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrSentCount);
    InterlockedExchangeAdd64(RSR.cntrSentBytes,RSRP^.LastCall);
    InterlockedExchangeAdd64(RSRP^.SendBuffer.posRead,RSRP^.LastCall);
  {$else}
    Inc(RSR.cntrSentCount);
    Inc(RSR.cntrSentBytes,RSRP^.LastCall);
    Inc(RSRP^.SendBuffer.posRead,RSRP^.LastCall);
  {$endif}

  EnterCriticalSection(RSRP^.SendBuffer.Lock);
  Try
    // Buffer Refactoring
    if (RSRP^.SendBuffer.posRead>=RSRP^.SendBuffer.posWrite) then
      RSR.Refactor(RSRP^.SendBuffer,FRefactor,RSRP^.SendBuffer.posRead);
    if (RSRP^.Throttle.Enabled=true) then begin
      RSRP^.Throttle.Consumption:=RSRP^.RecvBuffer.posWrite+RSRP^.SendBuffer.posWrite;
    end;
  finally
    LeaveCriticalSection(RSRP^.SendBuffer.Lock);
  end;
end;

procedure TRSRManager.cb_RSR_RecvBufferIncrement(RSRP:PRSR);
begin
  {$if defined(cpu64)}
    InterLockedIncrement64(RSR.cntrRecvCount);
    InterlockedExchangeAdd64(RSR.cntrRecvBytes,RSRP^.LastCall);
  {$else}
    Inc(RSR.cntrRecvCount);
    Inc(RSR.cntrRecvBytes,RSRP^.LastCall);
  {$endif}
  RSRP^.RecvBuffer.Stream.Position:=RSRP^.RecvBuffer.posWrite;
  RSRP^.RecvBuffer.Stream.Write(FRecvBuffer[0],RSRP^.LastCall);
  RSRP^.RecvBuffer.posWrite:=RSRP^.RecvBuffer.Stream.Position;
  if (RSRP^.Throttle.Enabled=true) then begin
    RSRP^.Throttle.Consumption:=RSRP^.RecvBuffer.posWrite+RSRP^.SendBuffer.posWrite;
  end;
end;

procedure TRSRManager.cb_RSR_RecvBufferIncrementInterlocked(RSRP:PRSR);
begin
  {$if defined(cpu64)}
    InterLockedIncrement64(RSR.cntrRecvCount);
    InterlockedExchangeAdd64(RSR.cntrRecvBytes,RSRP^.LastCall);
  {$else}
    Inc(RSR.cntrRecvCount);
    Inc(RSR.cntrRecvBytes,RSRP^.LastCall);
  {$endif}
  EnterCriticalSection(RSRP^.RecvBuffer.Lock);
  Try
    RSRP^.RecvBuffer.Stream.Position:=RSRP^.RecvBuffer.posWrite;
    RSRP^.RecvBuffer.Stream.Write(FRecvBuffer[0],RSRP^.LastCall);
    RSRP^.RecvBuffer.posWrite:=RSRP^.RecvBuffer.Stream.Position;
    if (RSRP^.Throttle.Enabled=true) then begin
      RSRP^.Throttle.Consumption:=RSRP^.RecvBuffer.posWrite+RSRP^.SendBuffer.posWrite;
    end;
  finally
    LeaveCriticalSection(RSRP^.RecvBuffer.Lock);
  end;
end;

procedure TRSRManager.cb_RSR_FillSendBuffer(RSRP:PRSR);
begin
  RSRP^.LastCall:=Core.Arrays.Bytes.fromStream(@FSendBuffer,FSendBufferSize,RSRP^.SendBuffer.Stream,RSRP^.SendBuffer.posRead);
end;

procedure TRSRManager.cb_RSR_FillSendBufferInterlocked(RSRP:PRSR);
begin
  EnterCriticalSection(RSRP^.SendBuffer.Lock);
  Try
    RSRP^.LastCall:=Core.Arrays.Bytes.fromStream(@FSendBuffer,FSendBufferSize,RSRP^.SendBuffer.Stream,RSRP^.SendBuffer.posRead);
  finally
    LeaveCriticalSection(RSRP^.SendBuffer.Lock);
  end;
end;

procedure   TRSRManager.cb_RSR_Read(RSRP:PRSR; Var Handled:Boolean);
var
  iRead:LongInt;
begin
  iRead:=FRecvBufferSize;
  Try
    EntryPoint:='TRSRManager.cb_RSR_Read';
    If (RSRP^.State or RSR_STATE_INIT = RSRP^.State) and (RSRP^.State or RSR_STATE_OPEN=RSRP^.State) then begin
      EntryPoint:='TRSRManager.cb_RSR_Read.fpRecv';
      RSRP^.LastCall:=Sockets.fpRecv(RSRP^.Info.Socket,@FRecvBuffer[0],iRead,RSR.RecvOptionalFlags);
      RSRP^.State:=RSRP^.State and not RSR_STATE_POLLED_RCVD;
      if (RSRP^.LastCall>0) then begin
        RSRP^.RecvBufferIncrement(RSRP);
        if RSRP^.State<>RSRP^.State or RSR_STATE_WAIT_TO_PROCESS then begin
          EntryPoint:='TRSRManager.cb_RSR_Read.FRecvProcedures(1)';
          FSyncRSRP:=RSRP;
          FRecvProcedures[RSRP^.State or RSR_STATE_SYNC_RECV=RSRP^.State](Handled);
        end;
      end else If (RSRP^.LastCall=0) then begin
        RSRP^.TaskError:=Sockets.socketerror();
        EntryPoint:=Concat('TRSRManager.cb_RSR_Read.Null Socket (',IntToStr(RSRP^.Info.Socket),') Error (',IntToStr(RSRP^.TaskError),')');
        {$if defined(RSR_DEBUG)}
          OnException(EntryPoint,'Debug','Disconnected');
        {$endif}
        Handled:=True;
        Include(RSRP^.Errors,eReceive);
        Include(RSRP^.Errors,eReset);
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
        RSRP^.State:= RSRP^.State and not RSR_STATE_OPEN;
      end else begin
        RSRP^.TaskError:=Sockets.SocketError;
        If (RSRP^.TaskError=EsockEWOULDBLOCK)  then begin  // Hopefully we get another read flag set on the processor
          if RSRP^.State<>RSRP^.State or RSR_STATE_WAIT_TO_PROCESS then begin
            EntryPoint:='TRSRManager.cb_RSR_Read.FRecvProcedures(2)';
            FSyncRSRP:=RSRP;
            FRecvProcedures[RSRP^.State or RSR_STATE_SYNC_RECV=RSRP^.State](Handled);
          end;
        end else begin
          EntryPoint:=Concat(
            'TRSRManager.cb_RSR_Read.Error',
            'Socket (',IntToStr(RSRP^.Info.Socket),
            ') State (',IntToStr(RSRP^.State),
            ') LastCall (',IntToStr(RSRP^.LastCall),
            ') Error (',IntToStr(RSRP^.TaskError),
            ')'
          );
          {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug','Closing');
          {$endif}
          Handled:=True;
          Include(RSRP^.Errors,eReceive);
          RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
          RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
        end;
      end;
      RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Read',Concat('Socket (',IntToStr(RSRP^.Info.Socket),') LastCall=',IntToStr(RSRP^.LastCall),' Read=',IntToStr(iRead),' Stream.Postion=',IntToStr(RSRP^.RecvBuffer.Stream.Position),' posWrite=',IntToStr(RSRP^.RecvBuffer.posWrite),' Stream.Size=',IntToStr(RSRP^.RecvBuffer.Stream.Size)),E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_Read_SSL(RSRP:PRSR; Var Handled:Boolean);
var
  iRead:LongInt;
begin
  iRead:=FRecvBufferSize;
  Try
    EntryPoint:='TRSRManager.cb_RSR_Read_SSL';
    If (RSRP^.State or RSR_STATE_INIT = RSRP^.State) and (RSRP^.State or RSR_STATE_OPEN=RSRP^.State) then begin
      EntryPoint:=Concat('TRSRManager.cb_RSR_Read_SSL.SSL_read (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') Buffer (',IntToStr(iRead),')');
      {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug','Reading');
      {$endif}
      RSRP^.LastCall:=SSL_read(RSRP^.SSL.Handle,@FRecvBuffer[0],iRead);
      EntryPoint:=Concat('TRSRManager.cb_RSR_Read_SSL.SSL_get_error SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') Read (',IntToStr(RSRP^.LastCall),')');
      {$if defined(RSR_DEBUG)}
        OnException(EntryPoint,'Debug','Read');
      {$endif}
      {$if defined(RSR_DEBUG)}
        EntryPoint:=Concat('TRSRManager.cb_RSR_Read_SSL.SSL_get_error SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),')');
        OnException(EntryPoint,'Debug','Checking');
      {$endif}
      RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
      {$if defined(RSR_DEBUG)}
        EntryPoint:=Concat('TRSRManager.cb_RSR_Read_SSL.SSL_get_error SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') Error (',IntToStr(RSRP^.TaskError),')');
        OnException(EntryPoint,'Debug','Checked');
      {$endif}
      RSRP^.State:=RSRP^.State and not RSR_STATE_POLLED_RCVD;
      if (RSRP^.LastCall>0) then begin
        RSRP^.RecvBufferIncrement(RSRP);
        EntryPoint:='TRSRManager.cb_RSR_Read_SSL.FRecvProcedures';
        FSyncRSRP:=RSRP;
        FRecvProcedures[RSRP^.State or RSR_STATE_SYNC_RECV=RSRP^.State](Handled);
      end else If (RSRP^.LastCall=0) then begin
        EntryPoint:=Concat('TRSRManager.cb_RSR_Read_SSL.Null SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') Error (',IntToStr(RSRP^.TaskError),')');
        {$if defined(RSR_DEBUG)}
          OnException(EntryPoint,'Debug','Disconnected');
        {$endif}
        Handled:=True;
        Include(RSRP^.Errors,eReceive);
        Include(RSRP^.Errors,eReset);
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
        RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
      end else begin
        EntryPoint:=Concat(
          'TRSRManager.cb_RSR_Read_SSL.Error',
          ' SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),
          ') Socket (',IntToStr(RSRP^.Info.Socket),
          ') State (',IntToStr(RSRP^.State),
          ') LastCall (',IntToStr(RSRP^.LastCall),
          ') Error (',IntToStr(RSRP^.TaskError),
          ')'
        );
        if (RSRP^.TaskError=SSL_ERROR_WANT_READ) or (RSRP^.TaskError=SSL_ERROR_WANT_WRITE) then begin
          {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug','Ignoring');
          {$endif}
        end else begin
          {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug','Closing');
          {$endif}
          Handled:=True;
          //Core.Streams.toFile(RSRP^.RecvBuffer.Stream,'/home/atbrunner/Desktop/buffer.txt');
          Include(RSRP^.Errors,eReceive);
          Include(RSRP^.Errors,eSSL);
          RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
          RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
        end;
      end;
      RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Read_SSL',Concat('SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') LastCall=',IntToStr(RSRP^.LastCall),' Read=',IntToStr(iRead),' Stream.Postion=',IntToStr(RSRP^.RecvBuffer.Stream.Position),' posWrite=',IntToStr(RSRP^.RecvBuffer.posWrite),' Stream.Size=',IntToStr(RSRP^.RecvBuffer.Stream.Size)),E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_Send(RSRP:PRSR; Var Handled:Boolean);
begin
  Try
    EntryPoint:='TRSRManager.cb_RSR_Send';
    If (RSRP^.State or RSR_STATE_INIT = RSRP^.State) and (RSRP^.State or RSR_STATE_OPEN=RSRP^.State) and (RSRP^.State or RSR_STATE_ERROR<>RSRP^.State) and (RSRP^.State or RSR_STATE_WRITE=RSRP^.State) then begin
      If (RSRP^.SendBuffer.posRead < RSRP^.SendBuffer.posWrite ) then begin
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
        RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
        EntryPoint:='TRSRManager.cb_RSR_Send.FillSendBuffer';
        RSRP^.FillSendBuffer(RSRP);
        //Core.Streams.toFile(RSRP^.SendBuffer.Stream,Concat('/home/atbrunner/Desktop/w.',IntToStr(RSRP^.Info.Socket),'.txt'));
        EntryPoint:=Concat('TRSRManager.cb_RSR_Send.fpSend Socket (',IntToStr(RSRP^.Info.Socket),') State (',IntToStr(RSRP^.State),') Size (',IntToStr(RSRP^.LastCall),')');
        Try
          RSRP^.LastCall:=Sockets.fpSend(RSRP^.Info.Socket,@FSendBuffer[0],RSRP^.LastCall,RSR.SendOptionalFlags);
        except
          on E:Exception do begin
            OnException(EntryPoint,'Exception',E.Message);
            Handled:=True;
            Include(RSRP^.Errors,EReset);
            Include(RSRP^.Errors,ESend);
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
            RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
          end;
        end;
        If (RSRP^.LastCall>0) then begin
          // Increment SendBuffer
          EntryPoint:='TRSRManager.cb_RSR_Send.SendBufferIncrement';
          RSRP^.SendBufferIncrement(RSRP);
          Handled:=(RSRP^.SendBuffer.posWrite=0);
          if (Handled) and ((RSRP^.Info.Flags or RSR_FLAGS_BLOCK_UNTIL_SENT)=RSRP^.Info.Flags) then
            RSRP^.Info.Flags:=RSRP^.Info.Flags and not RSR_FLAGS_BLOCK_PROCESSING;
        end else begin
          RSRP^.TaskError:=Sockets.SocketError;
          If (RSRP^.TaskError<>EsockEWOULDBLOCK) then begin
            EntryPoint:=Concat(
              'TRSRManager.cb_RSR_Send.fpSend.Error',
              'Socket (',IntToStr(RSRP^.Info.Socket),
              ') State (',IntToStr(RSRP^.State),
              ') LastCall (',IntToStr(RSRP^.LastCall),
              ') Error (',IntToStr(RSRP^.TaskError),
              ')'
            );
            Handled:=True;
            Include(RSRP^.Errors,ESend);
            {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug','Closing');
            {$endif}
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR or RSR_OP_DISCONNECTED;
            RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
            RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
          end else begin
            // Trigger Read?
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_READ;

            // SetBlockingMode(RSRP^.Info.Socket,false);
            // ReAdjust BufferFSendBuffer
            // SetSendBufferSize(FSendBufferSize-RSR_MAN_SEND_BUFF_ADJUST);
          end;
        end;
      end else
        Handled:=True;
    end else begin
      Handled:=false;
      if (RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
      if (RSRP^.State or RSR_STATE_ERROR=RSRP^.State)  then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
    end;
  Except
     On E:Exception do OnException('TRSRManager.cb_RSR_Send','Exception',E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_Send_SSL(RSRP:PRSR; Var Handled:Boolean);
begin
  Try
    EntryPoint:='TRSRManager.cb_RSR_Send_SSL';
    If (RSRP^.State or RSR_STATE_INIT = RSRP^.State) and (RSRP^.State or RSR_STATE_OPEN=RSRP^.State) and (RSRP^.State or RSR_STATE_ERROR<>RSRP^.State) and (RSRP^.State or RSR_STATE_WRITE=RSRP^.State) then begin
      If (RSRP^.SendBuffer.posRead < RSRP^.SendBuffer.posWrite ) then begin
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
        RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
        EntryPoint:='TRSRManager.cb_RSR_Send_SSL.FillSendBuffer';
        RSRP^.FillSendBuffer(RSRP);
        EntryPoint:=Concat('TRSRManager.cb_RSR_Send_SSL.fpSend SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') State (',IntToStr(RSRP^.State),') Size (',IntToStr(RSRP^.LastCall),')');
        //Try
          EntryPoint:=Concat('TRSRManager.cb_RSR_Send_SSL.SSL_write SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),') Socket (',IntToStr(RSRP^.Info.Socket),') State (',IntToStr(RSRP^.State),')');
          {$if defined(RSR_DEBUG)}
          OnException(EntryPoint,'Debug',Concat('Writing (',IntToStr(RSRP^.LastCall),')'));
          {$endif}

            RSRP^.LastCall:=SSL_write(RSRP^.SSL.Handle,@FSendBuffer[0],RSRP^.LastCall);
            RSRP^.TaskError:=SSL_get_error(RSRP^.SSL.Handle, RSRP^.LastCall);
            {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug',Concat('Wrote (',IntToStr(RSRP^.LastCall),') TaskError (',IntToStr(RSRP^.TaskError),')'));
            {$endif}
        {
        except
          on E:Exception do begin
            OnException(EntryPoint,'Exception',E.Message);
            Handled:=True;
            Include(RSRP^.Errors,EReset);
            Include(RSRP^.Errors,ESend);
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
            RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
          end;
        end;
        }
        If (RSRP^.LastCall>0) then begin
          EntryPoint:='TRSRManager.cb_RSR_Send_SSL.SendBufferIncrement';
          RSRP^.SendBufferIncrement(RSRP);
          Handled:=(RSRP^.SendBuffer.posWrite=0) ;
          if (Handled) and ((RSRP^.Info.Flags or RSR_FLAGS_BLOCK_UNTIL_SENT)=RSRP^.Info.Flags) then
            RSRP^.Info.Flags:=RSRP^.Info.Flags and not RSR_FLAGS_BLOCK_PROCESSING;
        end else If (RSRP^.LastCall=0) then begin
          EntryPoint:=Concat(
            'TRSRManager.cb_RSR_Send_SSL.Done',
            ' SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),
            ') Socket (',IntToStr(RSRP^.Info.Socket),
            ') State (',IntToStr(RSRP^.State),
            ') LastCall (',IntToStr(RSRP^.LastCall),
            ') Error (',IntToStr(RSRP^.TaskError),
            ')'
          );
          Handled:=True;
          Include(RSRP^.Errors,EReset);
          Include(RSRP^.Errors,ESend);
          {$if defined(RSR_DEBUG)}
          OnException(EntryPoint,'Error','Closing');
          {$endif}
          RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
          RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
        end else begin
          EntryPoint:=Concat(
            'TRSRManager.cb_RSR_Send_SSL.SSL_write.Error',
            ' SSL (',Encryption.SSL.toString(RSRP^.SSL.Handle),
            ') Socket (',IntToStr(RSRP^.Info.Socket),
            ') State (',IntToStr(RSRP^.State),
            ') LastCall (',IntToStr(RSRP^.LastCall),
            ') Error (',IntToStr(RSRP^.TaskError),
            ')'
          );
          if (RSRP^.TaskError=SSL_ERROR_WANT_READ) or (RSRP^.TaskError=SSL_ERROR_WANT_WRITE) then begin
            {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug','Ignoring');
            {$endif}
          end else begin
            {$if defined(RSR_DEBUG)}
            OnException(EntryPoint,'Debug','Closing');
            {$endif}
            Handled:=True;
            Include(RSRP^.Errors,eReceive);
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
            RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
          end;
        end;
      end else
        Handled:=True;
    end else begin
      Handled:=false;
      if (RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
      if (RSRP^.State or RSR_STATE_ERROR=RSRP^.State)  then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
    end;
  Except
     On E:Exception do OnException('TRSRManager.cb_RSR_Send_SSL','Exception',E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_Read_UDP(RSRP:PRSR; Var Handled:Boolean);
var
  iRead:LongInt;
  iLen:TSockLen;
begin
  EntryPoint:='TRSRManager.cb_RSR_Read_UDP';
  If (RSRP^.State or RSR_STATE_INIT = RSRP^.State) then begin
    iRead:=FRecvBufferSize; iLen:=SizeOf(RSRP^.Address);
    Try
      EntryPoint:='TRSRManager.cb_RSR_Read_UDP.fpRecvFrom';
      RSRP^.LastCall:=Sockets.fpRecvFrom(RSRP^.Info.Socket,@FRecvBuffer[0],iRead,0,@RSRP^.Address,@iLen);
      if (RSRP^.LastCall>0) then begin
        RSRP^.RecvBufferIncrement(RSRP);
        If (RSRP^.LastCall<iRead) then begin
          if ((RSRP^.Info.Flags or RSR_FLAGS_BLOCK_PROCESSING)<>RSRP^.Info.Flags) then begin
            // process iff manager is not blocking processing
            Handled:=True;
            EntryPoint:='TRSRManager.cb_RSR_Read_UDP.FRecvProcedures';
            FSyncRSRP:=RSRP;
            If RSRP^.Kind=rsrsDNS then begin
              EntryPoint:='TRSRManager.cb_RSR_NS_Recv.ProcessReceiveDNSBuffer';
              ProcessReceiveDNSBuffer(RSRP);
            end else begin
              FRecvProcedures[RSRP^.State or RSR_STATE_SYNC_RECV=RSRP^.State](Handled);
            end;
          end;
        end; // else more data is on the way
      end else begin
        RSRP^.TaskError:=Sockets.SocketError;
        If RSRP^.TaskError<>EsockEWOULDBLOCK then begin
          Handled:=True;
          Include(RSRP^.Errors,eReceive);
          EntryPoint:='TRSRManager.cb_RSR_Read_UDP.Error';
          RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
          RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
        end else begin
          SetBlockingMode(RSRP^.Info.Socket,false);
        end;
      end;
      RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
    Except
      On E:Exception do OnException('TRSRManager.cb_RSR_Read_UDP',Concat('(',IntToStr(RSRP^.Info.Socket),') LastCall=',IntToStr(RSRP^.LastCall),' Read=',IntToStr(iRead),' Stream.Postion=',IntToStr(RSRP^.RecvBuffer.Stream.Position),' posWrite=',IntToStr(RSRP^.RecvBuffer.posWrite),' Stream.Size=',IntToStr(RSRP^.RecvBuffer.Stream.Size)),E.Message);
    End;
  end;
end;

procedure   TRSRManager.cb_RSR_Write_UDP(RSRP:PRSR; Var Handled:Boolean);
const
  Flags:LongInt={$ifdef Unix}MSG_DONTWAIT{$else}0{$endif};
var
  iSend:LongInt;
begin
  Try
    EntryPoint:='TRSRManager.cb_RSR_Write_UDP';
    If (RSRP^.State or RSR_STATE_INIT = RSRP^.State) and (RSRP^.State or RSR_STATE_OPEN=RSRP^.State) then begin
      If (RSRP^.SendBuffer.posRead < RSRP^.SendBuffer.posWrite) then begin
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
        RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
        EntryPoint:='TRSRManager.cb_RSR_Write_UDP.Core.Arrays.Bytes.fromStream';
        iSend:=Core.Arrays.Bytes.fromStream(@FSendBuffer,FSendBufferSize,RSRP^.SendBuffer.Stream,RSRP^.SendBuffer.posRead); // Will fill buffer and advance posRead
        EntryPoint:='TRSRManager.cb_RSR_Write_UDP.Sockets.fpSend';
        RSRP^.LastCall:=Sockets.fpSendTo(RSRP^.Info.Socket,@FSendBuffer[0],iSend,Flags,@RSRP^.Address,Sizeof(RSRP^.Address));
        If (RSRP^.LastCall>0) then begin
          EntryPoint:='TRSRManager.cb_RSR_Write_UDP.SendBufferIncrement';
          RSRP^.SendBufferIncrement(RSRP);
          Handled:=(RSRP^.SendBuffer.posWrite=0);
        end else If (RSRP^.LastCall=0) then begin
          Handled:=True;
          Include(RSRP^.Errors,EReset);
          Include(RSRP^.Errors,ESend);
          EntryPoint:='TRSRManager.cb_RSR_Write_UDP.Error';
          RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
          RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
        end else begin
          RSRP^.TaskError:=Sockets.SocketError;
          If (RSRP^.TaskError<>EsockEWOULDBLOCK) then begin
            Include(RSRP^.Errors,ESend);
            EntryPoint:='TRSRManager.cb_RSR_Write_UDP.Error';
            RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
            RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
          end else begin
            SetBlockingMode(RSRP^.Info.Socket,false);
          end;
        end;
      end else
        Handled:=True;
    end else
      Handled:=True;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_Write_UDP','Exception',E.Message);
  End;
end;

(*
procedure   TRSRManager.cb_RSR_NS_Send(RSRP:PRSR; Var Handled:Boolean);
var
  iSend:LongInt;
begin
  Try
    EntryPoint:='TRSRManager.cb_RSR_NS_Send';
    Handled:=True;  // Some DNS Queries don't require responses

    FDNSRSRP^.State:=RSRP^.State or RSR_STATE_POLL;

    iSend:=RSRP^.DNS.BufferOut(FDNSBuffer);
    RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeout);
    EntryPoint:='TRSRManager.cb_RSR_NS_Send.Sockets.fpSendTo';
    FDNSRSRP^.LastCall:=Sockets.fpSendTo(FDNSRSRP^.Info.Socket,@FDNSBuffer[0],iSend,hRSR.SendOptionalFlags,@FDNS.Address,Sizeof(FDNS.Address));
    If FDNSRSRP^.LastCall>0 then begin
      EntryPoint:='TRSRManager.cb_RSR_NS_Send.SendBufferIncrement';
      FDNSRSRP^.SendBufferIncrement(FDNSRSRP);
      Handled:=(FDNSRSRP^.SendBuffer.posWrite=0) ;
    end else If (FDNSRSRP^.LastCall=0) then begin
      Include(RSRP^.Errors,EReset);
      Include(RSRP^.Errors,EDNS);
      Include(RSRP^.Errors,ESend);
      EntryPoint:='TRSRManager.cb_RSR_NS_Send.Reset';
      FDNSRSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED or RSR_OP_ERROR;
    end else begin
      FDNSRSRP^.TaskError:=Sockets.SocketError;
      If (FDNSRSRP^.TaskError<>EsockEWOULDBLOCK) then begin
        Include(FDNSRSRP^.Errors,ESend);
        Include(FDNSRSRP^.Errors,EDNS);
        EntryPoint:='TRSRManager.cb_RSR_NS_Send.Error';
        RSRP^.Operations:=FDNSRSRP^.Operations or RSR_OP_ERROR;
        RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
      end else begin
        SetBlockingMode(FDNSRSRP^.Info.Socket,false);
        Handled:=false;
      end;
    end;
  Except
     On E:Exception do OnException('TRSRManager.cb_RSR_NS_Send','Exception',E.Message);
  End;
end;

procedure   TRSRManager.cb_RSR_NS_Recv(RSRP:PRSR; Var Handled:Boolean);
var
  iFromLen:TSockLen;
  iRead:LongInt;
begin
  EntryPoint:='TRSRManager.cb_RSR_NS_Recv';
  iRead:=FRecvBufferSize; iFromLen:=SizeOf(TSockAddr);
  Try
    EntryPoint:='TRSRManager.cb_RSR_NS_Recv.fpRecvFrom';
    FDNSRSRP^.LastCall:=Sockets.fpRecvFrom(FDNSRSRP^.Info.Socket,@FRecvBuffer[0],iRead,hRSR.RecvOptionalFlags,@FDNS.Address,@iFromLen);
    EntryPoint:='TRSRManager.cb_RSR_NS_Recv.Process';
    if (FDNSRSRP^.LastCall>0) then begin
      {$if defined(cpu64)}
        InterlockedIncrement64(hRSR.cntrRecvCount);
        InterlockedExchangeAdd64(hRSR.cntrRecvBytes,RSRP^.LastCall);
      {$else}
        Inc(hRSR.cntrRecvCount);
        Inc(hRSR.cntrRecvBytes,RSRP^.LastCall);
      {$endif}
      FDNSRSRP^.RecvBuffer.Stream.Position:=FDNSRSRP^.RecvBuffer.posWrite;
      FDNSRSRP^.RecvBuffer.Stream.Write(FRecvBuffer[0],RSRP^.LastCall);
      FDNSRSRP^.RecvBuffer.posWrite:=FDNSRSRP^.RecvBuffer.Stream.Position;

      If (FDNSRSRP^.LastCall<iRead) then begin
        // Almost always going to be true...  All data is present
        Handled:=True;
        EntryPoint:='TRSRManager.cb_RSR_NS_Recv.ProcessReceiveDNSBuffer';
        ProcessReceiveDNSBuffer(RSRP);
      end; // else more data is on the way
    end else begin
      RSRP^.TaskError:=Sockets.SocketError;
      If (RSRP^.TaskError<>EsockEWOULDBLOCK) then begin  // Hopefully we get another read flag set on the processor
        Handled:=True;
        Include(RSRP^.Errors,eDNS);
        Include(RSRP^.Errors,eReceive);
        EntryPoint:='TRSRManager.cb_RSR_NS_Recv.Perform_Error';
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
        RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
      end else begin
        FHandled:=false;
        SetBlockingMode(FDNSRSRP^.Info.Socket,false);
      end;
    end;
  Except
    On E:Exception do OnException('TRSRManager.cb_RSR_NS_Recv',Concat(' Exception (',IntToStr(RSRP^.Info.Socket),') LastCall=',IntToStr(RSRP^.LastCall),' Read=',IntToStr(iRead),' Stream.Postion=',IntToStr(RSRP^.RecvBuffer.Stream.Position),' posWrite=',IntToStr(RSRP^.RecvBuffer.posWrite),' Stream.Size=',IntToStr(RSRP^.RecvBuffer.Stream.Size), ' Entrypoint=',EntryPoint),E.Message);
  End;
end;
*)

procedure   TRSRManager.Queue(Kind:TRSRSocketKind; Remote:Sockets.TSocket; RemoteAddress:Sockets.TSockAddr);
var
  RSRP:PRSR;
  {$i RSR.Queue.Pushes.inc}
begin
  // This routine is called by a server after it selected a manager after an accept socket result.
  RSRP:=FSockets.Add(Remote);
  RSRP^.Info.Kind:=rsrServer;

  MAN_MAP[Remote]:=Self;
  RSR_MAP[Remote]:=RSRP;

  If (FSecure and (FSSLInfoP<>nil)) then begin
    RSRP^.State:=RSRP^.State or RSR_STATE_SECURE;
    Sockets.fpSetSockOpt(RSRP^.Info.Socket,SOL_SOCKET,SO_LINGER,@FSSLLinger,System.SizeOf(FSSLLinger));
  end else begin
    Sockets.fpSetSockOpt(RSRP^.Info.Socket,SOL_SOCKET,SO_LINGER,@FServerLinger,System.SizeOf(FServerLinger));
  end;
  {$ifdef Unix}Core.Utils.Sockets.SetSigNoSigPipe(RSRP^.Info.Socket,Core.Utils.Sockets.SIGPIPE_OFF);{$endif}

  RSRP^.Info.Socket:=Remote;
  RSRP^.Address:=RemoteAddress;
  SetTCPDelay(Remote,TCP_DELAY_OFF);

  SetBlockingMode(Remote,false);

  RSRP^.State:=RSRP^.State or RSR_STATE_OPEN or RSR_STATE_POLL;
  RSRP^.Kind:=Kind;
  Case Kind of
    rsrsTCP: Push_Queue_TCP;
    rsrsUDP: Push_Queue_UDP;
    rsrsDNS: Push_Queue_DNS;
  end;

  RSRP^.Info.Flags:=RSRP^.Info.Flags or RSR_FLAGS_ACCEPTED;
  RSRP^.Operations:=RSRP^.Operations or  RSR_OP_INITIALIZE or RSR_OP_QUEUE;
end;

function   TRSRManager.ReAssignSocket(RSRP:PRSR):boolean;
var
  sktOld:TSocket;
  sktNew:TSocket;
begin
  Result:=false; sktOld:=RSRP^.Info.Socket; sktNew:=0;
  case RSRP^.Kind of
    rsrsTCP  : sktNew:=Sockets.fpsocket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    rsrsUDP  : sktNew:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    rsrsDNS  : sktNew:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    rsrsNone : sktNew:=0;
  end;
  if (sktNew>0) then begin
    MAN_MAP[sktNew]:=MAN_MAP[sktOld];
    RSR_MAP[sktNew]:=RSR_MAP[sktOld];
    {$ifdef Unix}Core.Utils.Sockets.SetSigNoSigPipe(sktNew,Core.Utils.Sockets.SIGPIPE_OFF);{$endif}
    Sockets.fpshutdown(sktOld,SHUT_RDWR);
    if (RSRP^.SSL.Handle<>nil) then begin
      EntryPoint:='TRSRManager.ReAssignSocket.ReleaseSSL';
      try
        ReleaseSSL(RSRP);
      except
        On E:Exception do OnException(EntryPoint,'Exception',E.Message);
      end;
    end;
    Sockets.CloseSocket(sktOld);

    RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    RSRP^.State:=RSRP^.State and not RSR_STATE_ISSUED_CONNECT;
    RSRP^.Errors:=[];
    RSRP^.Info.Socket:=sktNew;
    RSRP^.dtExpires:=0;
    RSRP^.dtPoll:=0;
    Result:=True;
  end;
End;

function    TRSRManager.Queue(Kind:TRSRSocketKind; const Data:System.Pointer):PRSR;
begin
  Result:=Queue(Kind,0,0,Data);
end;

function    TRSRManager.Queue(Kind:TRSRSocketKind; const BindIP:QWord; const BindPort:Word; const Data:System.Pointer):PRSR;
var
  RSRP:PRSR;
  sktResult:Sockets.TSocket;
  sktAddress:Sockets.TSockAddr;
  {$i RSR.Queue.Pushes.inc}
begin
  case Kind of
    rsrsNone : sktResult:=0;
    rsrsUDP  : sktResult:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    rsrsTCP  : sktResult:=Sockets.fpsocket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    rsrsDNS  : sktResult:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
  end;
  if (sktResult>0) then begin
    RSRP:=FSockets.Add(sktResult);
    Result:=RSRP;
    Case Kind of
      rsrsTCP: Push_Queue_TCP;
      rsrsUDP: Push_Queue_UDP;
      rsrsDNS: Push_Queue_DNS;
    end;
    if BindIP<>0 then begin
      RSRP^.Info.BindIP:=BindIP;
      sktAddress:=Core.Utils.Sockets.SocketAddress(BindIP,BindPort);
      Sockets.fpBind(sktResult,@sktAddress,SizeOf(sktAddress));
    end;
    RSRP^.Kind:=Kind;
    MAN_MAP[sktResult]:=Self;
    RSR_MAP[sktResult]:=Result;
    RSRP^.Info.DataP:=Data;
    RSRP^.Info.Socket:=sktResult;
    RSRP^.STATE:=RSRP^.STATE or RSR_STATE_OPEN;
    Sockets.fpSetSockOpt(RSRP^.Info.Socket,SOL_SOCKET,SO_LINGER,@FClientLinger,System.SizeOf(FClientLinger));
    SetBlockingMode(sktResult,false);
    SetTCPDelay(sktResult,TCP_DELAY_OFF);
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_INITIALIZE or RSR_OP_QUEUE;
  end;
end;
procedure   TRSRManager.SetEvents(RSRP:PRSR);
  procedure Set_Events_TCP;
  begin
    if (RSRP^.Info.Kind=rsrServer) then begin
      RSRP^.FillSendBuffer:=@cb_RSR_FillSendBuffer;
    end else begin
      RSRP^.FillSendBuffer:=@cb_RSR_FillSendBufferInterlocked;
    end;
    if (FSecure=false) or (RSRP^.State or RSR_STATE_SECURE<>RSRP^.State) then begin
      // no reading or writing on TCP SSL until Accept or Connect
      RSRP^.SendEvent:=@cb_RSR_Send;
      RSRP^.RecvEvent:=@cb_RSR_Read;
    end;
  end;

  procedure Set_Events_UDP;
  begin
    RSRP^.SendEvent:=@cb_RSR_Write_UDP;
    RSRP^.RecvEvent:=@cb_RSR_Read_UDP;
    {$ifdef useEventQueues}
      SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
      SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
    {$else}
      SocketEventsAttach(RSRP^.Info.Socket,[sfkRead,sfkWrite]);
    {$endif}
  end;

  procedure Set_Events_DNS;
  begin
    RSRP^.Finite:=false;
    RSRP^.SendEvent:=@cb_RSR_Write_UDP;
    RSRP^.RecvEvent:=@cb_RSR_Read_UDP;
    {$ifdef useEventQueues}
      SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
      SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
    {$else}
      SocketEventsAttach(RSRP^.Info.Socket,[sfkRead,sfkWrite]);
    {$endif}
  end;
begin
  if RSRP^.Info.Socket<>0 then begin
    if (RSRP^.Info.Kind=rsrServer) then begin
      RSRP^.SendBufferIncrement:=@cb_RSR_SendBufferIncrement;
      RSRP^.RecvBufferIncrement:=@cb_RSR_RecvBufferIncrement;
    end else begin
      RSRP^.SendBufferIncrement:=@cb_RSR_SendBufferIncrementInterlocked;
      RSRP^.RecvBufferIncrement:=@cb_RSR_RecvBufferIncrementInterlocked;
    end;
    case RSRP^.Kind of
      rsrsTCP  : Set_Events_TCP;
      rsrsUDP  : Set_Events_UDP;
      rsrsDNS  : Set_Events_DNS;
    end;
  end;
end;

procedure   TRSRManager.SetEntryPoint(Value:Core.Strings.VarString);
begin
  If Self=nil then exit;
  EnterCriticalSection(FEntryLock);
  Try
    FEntryPoint:=Value;
  Finally
    LeaveCriticalSection(FEntryLock);
  end;
end;

function    TRSRManager.GetEntryPoint():Core.Strings.VarString;
begin
  EnterCriticalSection(FEntryLock);
  Try
    Result:=FEntryPoint;
  Finally
    LeaveCriticalSection(FEntryLock);
  end;
end;

procedure   TRSRManager.Allocate(Kind:TRSRKind; RSRP:PRSR);
  procedure PushTCP;
  begin
    RSRP^.Info.Socket:=Sockets.fpsocket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    Sockets.fpSetSockOpt(RSRP^.Info.Socket,SOL_SOCKET,SO_LINGER,@FClientLinger,System.SizeOf(FClientLinger));
    {$ifdef Unix} Core.Utils.Sockets.SetSigNoSigPipe(RSRP^.Info.Socket,Core.Utils.Sockets.SIGPIPE_OFF);{$endif}
    {$ifdef cpu64}
      InterlockedIncrement64(RSR_Connection_Count);
    {$else}
      Inc(RSR_Connection_Count);
    {$endif}
  end;

  procedure PushUDP;
  begin
    RSRP^.Info.Socket:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    {$ifdef useEventQueues}
      SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
      SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
    {$else}
      SocketEventsAttach(RSRP^.Info.Socket,[sfkRead,sfkWrite]);
    {$endif}
    {$ifdef cpu64}
      InterlockedIncrement64(RSR_Stream_Count);
    {$else}
      Inc(RSR_Stream_Count);
    {$endif}
  end;

  procedure PushDNS;
  begin
    RSRP^.Info.Socket:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    {$ifdef useEventQueues}
      SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
      SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
    {$else}
      SocketEventsAttach(RSRP^.Info.Socket,[sfkRead,sfkWrite]);
    {$endif}
    {$ifdef cpu64}
      InterlockedIncrement64(RSR_Stream_Count);
    {$else}
      Inc(RSR_Stream_Count);
    {$endif}
  end;
begin
  case RSRP^.Kind of
    rsrsTCP  : PushTCP;
    rsrsUDP  : PushUDP;
    rsrsDNS  : PushDNS;
    rsrsNone : RSRP^.Info.Socket:=0;
  end;
  if RSRP^.Info.Socket>0 then begin
    RSR_MAP[RSRP^.Info.Socket]:=RSRP;
    MAN_MAP[RSRP^.Info.Socket]:=Self;
    RSRP^.Info.Kind:=Kind;
    SetBlockingMode(RSRP^.Info.Socket,false);
    SetTCPDelay(RSRP^.Info.Socket,TCP_DELAY_OFF);
    SetEvents(RSRP);
  end;
end;

function    TRSRManager.Allocate(Kind:TRSRKind; sockKind:TRSRSocketKind):PRSR;
var
  sktNew:TSocket;

  procedure PushTCP;
  begin
    sktNew:=Sockets.fpsocket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    {$ifdef cpu64}
      InterlockedIncrement64(RSR_Connection_Count);
    {$else}
      Inc(RSR_Connection_Count);
    {$endif}
  end;

  procedure PushUDP;
  begin
    sktNew:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    {$ifdef useEventQueues}
      SocketEventsAttach(FRcvQueue,sktNew,sfkRead);
      SocketEventsAttach(FWriteQueue,sktNew,sfkWrite);
    {$else}
      SocketEventsAttach(sktNew,[sfkRead,sfkWrite]);
    {$endif}
    {$ifdef cpu64}
      InterlockedIncrement64(RSR_Stream_Count);
    {$else}
      Inc(RSR_Stream_Count);
    {$endif}

  end;

  procedure PushDNS;
  begin
    sktNew:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
    {$ifdef useEventQueues}
      SocketEventsAttach(FRcvQueue,sktNew,sfkRead);
      SocketEventsAttach(FWriteQueue,sktNew,sfkWrite);
    {$else}
      SocketEventsAttach(sktNew,[sfkRead,sfkWrite]);
    {$endif}
    {$ifdef cpu64}
      InterlockedIncrement64(RSR_Stream_Count);
    {$else}
      Inc(RSR_Stream_Count);
    {$endif}
  end;

begin
  Result:=nil;
  case sockKind of
    rsrsTCP  : PushTCP;
    rsrsUDP  : PushUDP;
    rsrsDNS  : PushDNS;
    rsrsNone : sktNew:=0;
  end;
  if (sktNew>0) then begin
    RSR_MAP[sktNew]:=FSockets.Add(sktNew);
    MAN_MAP[sktNew]:=Self;
    RSR_MAP[sktNew]^.Info.Kind:=Kind;
    RSR_MAP[sktNew]^.Kind:=sockKind;
    Result:=RSR_MAP[sktNew];
    SetTCPDelay(sktNew,TCP_DELAY_OFF);
    {$ifdef Unix}Core.Utils.Sockets.SetSigNoSigPipe(sktNew,Core.Utils.Sockets.SIGPIPE_OFF);{$endif}
    SetEvents(RSR_MAP[sktNew]);
    RSR_MAP[sktNew]^.Operations:=RSR_MAP[sktNew]^.Operations or RSR_OP_INITIALIZE;
    RSR_MAP[sktNew]^.Operations:=RSR_MAP[sktNew]^.Operations or RSR_OP_QUEUE;
    case sockKind of
      rsrsUDP  : RSR_MAP[sktNew]^.Finite:=false;
      rsrsDNS  : RSR_MAP[sktNew]^.Finite:=false;
    end;
  end;
end;

procedure   TRSRManager.DNSLookup(RSRP:PRSR; Value:QWord; List:Core.Strings.VarString; ServerIP:QWord=0);
//  This method is for perform white/black list entries.
var
  RequestID : Word;
  dnsCache  : PQCache;
  dns       : TDNSObject;
  dnsRSR    : PRSR;
begin
  if FDNSServersChecked=false then begin
    RSR.DNS.Check_DNS_Servers;
    FDNSServersChecked:=true;
    // WARNING:  This function cannot be called from anything other than self.  It is not re-entrant.
  end;
  if ServerIP=0 then
    ServerIP:=RSR.DNS.GetNextDNSServer;
  if (ServerIP=0) then begin
    OnException('TRSRManager.DNSLookup.List','DNSLookup',Concat('(',IntToStr(RSRP^.Info.Socket),') Server=',IntToStr(ServerIP),' Invalid DNS Server'));
  end else begin
    List:=Concat(Core.Utils.Sockets.InAddrToRevStr(Value),'.',List);
    RequestID:=FSKT_MAP_INDEX;
    DNS_MAP[RequestID]:=RSRP; // Pointer to who made the request.
    System.InterLockedIncrement(FSKT_MAP_INDEX);
    if (FSKT_MAP_INDEX>High(Word)) then
      System.InterLockedExchange(FSKT_MAP_INDEX,0);

    dns:=FDNSEntries.Acquire(ServerIP);
    if (dns<>nil) then begin
      dnsRSR:=RSR_MAP[dns.Socket];
      dnsRSR^.Info.Server:=List;
      dnsRSR^.Errors:=[];
      dns.Reset();


      RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeOut);
      RSRP^.State:=RSRP^.State or RSR_STATE_DNS;
      RSRP^.Info.RequestID:=RequestID;

      dns.Header.ID:=RequestID;
      dns.Header.Options:=QHMC_RD;
      dns.Header.QDCount:=1;
      dns.Questions[0].Q_NAME:=DomainToDNSDomain(List);
      dns.Questions[0].Q_TYPE:=QT_PTR;
      dns.Questions[0].Q_Class:=1;

      dnsCache:=dns.checkCache(List);

      if (dnsCache<>nil) then begin
        RSRP^.Errors:=[];
        EntryPoint:='TRSRManager.DNSLookup.List.dnsCache';
        RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;
        RSRP^.Info.RequestID:=-1;
        OnDNSResult(RSRP);
      end else begin
        // stream out query
        // Set DNS Socket write Op
        dnsRSR^.LastCall:=dns.StreamOut(dnsRSR^.SendBuffer.Stream);
        Inc(dnsRSR^.SendBuffer.posWrite,dnsRSR^.LastCall);
        dnsRSR^.Operations:=dnsRSR^.Operations or RSR_OP_WRITE;
      end;
    end else begin
      OnException('TRSRManager.DNSLookup.List','DNSLookup',Concat('(',IntToStr(RSRP^.Info.Socket),') Server=',IntToStr(ServerIP),' Unable to acquire a DNS object'));
    end;
  end;
end;


procedure   TRSRManager.DNSLookup(RSRP:PRSR; Value:Core.Strings.VarString; Types:TDNSStyles; ServerIP:QWord=0);
Var
  saIP      : Core.Arrays.Types.VarString;
  QType     : Word;
  RequestID : LongInt;
  dnsRSR    : PRSR;
  dns       : TDNSObject;
  dnsCache  : PQCache;
begin
  EntryPoint:='TRSRManager.DNSLookup';
  if (RSRP^.Info.Socket=0) then begin
    OnException('TRSRManager.DNSLookup','DNSLookup',Concat('(',IntToStr(RSRP^.Info.Socket),') Server=',IntToStr(ServerIP),' Cannot perform query on a null Socket'));
  end else begin
    dnsCache:=nil;
    if FDNSServersChecked=false then begin
      RSR.DNS.Check_DNS_Servers();
      FDNSServersChecked:=true;
      // WARNING:  This function cannot be called from anything other than self.  It is not re-entrant.
    end;
    if ServerIP=0 then
      ServerIP:=RSR.DNS.GetNextDNSServer;
    if (ServerIP=0) then begin
      OnException('TRSRManager.DNSLookup','DNSLookup',Concat('(',IntToStr(RSRP^.Info.Socket),') Server=',IntToStr(ServerIP),' Invalid DNS Server'));
    end else begin
      If (dnsPtr in Types) then begin
        Core.Arrays.VarString.fromString(saIP,Value,'.');
        Try
          If (Core.Arrays.VarString.isNumeric(saIP)=true) and (Length(saIP)=4) then begin
            QType:=QT_PTR;
            Value:=Concat(saIP[3],'.',saIP[2],'.',saIP[1],'.',saIP[0],'.','IN-ADDR.ARPA');
          end;
        Finally
          SetLength(saIP,0);
        end;
      end else if dnsMX in Types then begin
        QType:=QT_MX
      end else
        QType:=QT_IP;

      RequestID:=FSKT_MAP_INDEX;
      System.InterLockedIncrement(FSKT_MAP_INDEX);
      if (FSKT_MAP_INDEX>High(Word)) then
        System.InterLockedExchange(FSKT_MAP_INDEX,0);

      RSRP^.Info.RequestID:=RequestID;
      DNS_MAP[RequestID]:=RSRP; // Pointer to who made the request.


      dns:=FDNSEntries.Acquire(ServerIP);
      if (dns<>nil) then begin
        dnsRSR:=RSR_MAP[dns.Socket];
        dnsRSR^.Info.Server:=Value;
        dnsRSR^.Errors:=[];
        dnsRSR^.Info.RequestID:=RequestID;
        dns.Reset();


        RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeOut);
        RSRP^.State:=RSRP^.State or RSR_STATE_DNS;
        RSRP^.Info.RequestID:=RequestID;

        dns.Header.ID:=RequestID;
        dns.Header.Options:=QHMC_RD;
        dns.Header.QDCount:=1;
        dns.Questions[0].Q_NAME:=DomainToDNSDomain(Value);
        dns.Questions[0].Q_TYPE:=QType;
        dns.Questions[0].Q_Class:=1;

        dnsCache:=dns.checkCache(Value);
        RSRP^.DNS:=dns;
        if (dnsCache<>nil) then begin
          RSRP^.Errors:=[];
          EntryPoint:='TRSRManager.DNSLookup.Value.dnsCache';
          RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;
          If (RSRP^.State or RSR_STATE_AUTOMATIC_STEPS=RSRP^.State) then begin
            FHandled:=false;// make sure next step is processed
            automationSuccess(RSRP);
          end else begin
            RSRP^.Info.RequestID:=-1;
            RSRP^.State:=RSRP^.State and not RSR_STATE_DNS;

            OnDNSResult(RSRP);
          end;
        end else begin
          // stream out query
          // Set DNS Socket write Op
          dnsRSR^.LastCall:=dns.StreamOut(dnsRSR^.SendBuffer.Stream);
          Inc(dnsRSR^.SendBuffer.posWrite,dnsRSR^.LastCall);
          dnsRSR^.Operations:=dnsRSR^.Operations or RSR_OP_WRITE;
        end;
      end else begin
        OnException('TRSRManager.DNSLookup.Value','DNSLookup',Concat('(',IntToStr(RSRP^.Info.Socket),') Server=',IntToStr(ServerIP),' Unable to acquire a DNS object'));
      end;
    end;
  end;
end;

procedure   TRSRManager.RenewCycle();
begin
  dtCycleStart:=Core.Timer.dtNow;
end;

procedure   TRSRManager.AddMethod(Method:TRSRMethod);
begin
  FRSRMethodThread.FMethods.Add(Method);
end;

procedure   TRSRManager.Connect(RSRP:PRSR; Server:Core.Strings.VarString; Port:Word);
var
  iSvrAddr:System.QWord;
begin
  EntryPoint:='TRSRManager.Connect(Server:Core.Strings.VarString)';
  RSRP^.Info.Server:=Server;
  iSvrAddr:=Core.Utils.Sockets.InAddrFromStr(Server);
  if (isvrAddr=0) then begin
    DNSLookup(RSRP,Server,[dnsIP],RSR.DNS.GetNextDNSServer);
  end else
    Connect(RSRP,iSvrAddr,Port);
end;

procedure   TRSRManager.Connect(RSRP:PRSR; Server:QWord; Port:Word);
begin
  EntryPoint:='TRSRManager.Connect(Server:LongInt)';
  RSRP^.Errors:=[];
  RSRP^.Info.Port:=Port;
  If ((RSRP^.State or RSR_STATE_ISSUED_CONNECT)=RSRP^.State) and ( (RSRP^.State or RSR_STATE_REUSEABLE)=RSRP^.State) then
    ReAssignSocket(RSRP);
  SetBlockingMode(RSRP^.Info.Socket,false);
  If RSRP^.Info.BindIP<>0 then begin
    RSRP^.Address:=SocketAddress(RSRP^.Info.BindIP,RSR_BIND_ANY_PORT);
    RSRP^.LastCall:=Sockets.fpBind(RSRP^.Info.Socket,@RSRP^.Address,SizeOf(RSRP^.Address));
  end;
  RSRP^.Address:=SocketAddress(Server,Port);
  If (RSRP^.Address.Sin_Addr.S_addr<>0) then begin
    EntryPoint:='TRSRManager.Connect.fpConnect';
    {$ifdef useEventQueues}
      SocketEventsAttach(FConQueue,RSRP^.Info.Socket,sfkConnect);
    {$else}
      SocketEventsAttach(RSRP^.Info.Socket,[sfkConnect]);
    {$endif}
    if (Sockets.fpConnect(RSRP^.Info.Socket,@RSRP^.Address,SizeOf(TSockAddr))=Socket_Error) then begin
      RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeOut);
      RSRP^.State:=RSRP^.State or RSR_STATE_ISSUED_CONNECT;
      RSRP^.TaskError:=Sockets.SocketError;
      if (RSRP^.TaskError<>EOperationInProgress) and (RSRP^.TaskError<>EsockEWOULDBLOCK) then begin
        Include(RSRP^.Errors,eConnect);
        EntryPoint:='TRSRManager.Connect.OnError';
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
        RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
      end else begin
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL or RSR_STATE_CONNECTING;
      end;
    end else begin
      RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FTimeOut);
      RSRP^.State:=RSRP^.State or RSR_STATE_ISSUED_CONNECT or RSR_STATE_POLL or RSR_STATE_CONNECTING;
    end;
  end else begin
    RSRP^.Errors+=[eConnect,eException];
    EntryPoint:='TRSRManager.Connect.NilSocket';
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
    RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
  end;
end;

procedure   TRSRManager.Close(RSRP:PRSR);
begin
  EntryPoint:='TRSRManager.Close';
  RSRP^.Operations:=RSRP^.Operations or RSR_OP_CLOSE;
end;

procedure   TRSRManager.Retire(RSRP:PRSR);
begin
  EntryPoint:='TRSRManager.Retire';
  RSRP^.Operations:=RSRP^.Operations or RSR_OP_FINALIZE;
End;

procedure   TRSRManager.StartTLS(RSRP:PRSR);
begin
  EntryPoint:='TRSRManager.StartTLS';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_STARTTLS;
  end;
end;

procedure   TRSRManager.Send(RSRP:PRSR; BufferP:Core.Arrays.Types.PBytes; Const Length:LongInt);
begin
  EntryPoint:='TRSRManager.Send(PByteArray)';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    EnterCriticalSection(RSRP^.SendBuffer.Lock);
    try
      RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
      RSRP^.SendBuffer.Stream.Write(BufferP^[0],Length);
      RSRP^.SendBuffer.posWrite:=RSRP^.SendBuffer.Stream.Position;
    finally
      LeaveCriticalSection(RSRP^.SendBuffer.Lock);
    end;
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
    RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
  end;
end;

procedure   TRSRManager.Send(RSRP:PRSR; Stream:TStream; Const Offset,Length:Int64);
begin
  EntryPoint:='TRSRManager.Send(Stream)';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    EnterCriticalSection(RSRP^.SendBuffer.Lock);
    try
      RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.posWrite;
      Stream.Position:=Offset;
      RSRP^.SendBuffer.Stream.CopyFrom(Stream,Length);
      RSRP^.SendBuffer.posWrite:=RSRP^.SendBuffer.Stream.Position;
    finally
      LeaveCriticalSection(RSRP^.SendBuffer.Lock);
    end;
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
    RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
  end;
end;

procedure   TRSRManager.Send(RSRP:PRSR; Data:TStream);
var
  iLength:LongInt;
begin
  EntryPoint:='TRSRManager.Send(Stream)';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    Try
      iLength:=Data.Size;
      if iLength>0 then begin
        EnterCriticalSection(RSRP^.SendBuffer.Lock);
        try
          Data.Position:=0;
          RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.Stream.Size;
          RSRP^.SendBuffer.Stream.CopyFrom(Data,iLength);
          RSRP^.SendBuffer.posWrite:=RSRP^.SendBuffer.Stream.Position;
        finally
          LeaveCriticalSection(RSRP^.SendBuffer.Lock);
        end;
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
      end;
    Except
      On E:Exception do  begin
        EntryPoint:='TRSRManager.Send(Stream).OnException';
        OnException('TRSRManager.Send(Stream)','Exception',E.Message);
      end
    end;
  end;
end;

procedure   TRSRManager.Send(RSRP:PRSR);
begin
  RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
  RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
end;

procedure   TRSRManager.Send(RSRP:PRSR; Var Data:Core.Strings.VarString);
var
  iLength:LongInt;
begin
  EntryPoint:='TRSRManager.Send(Core.Strings.VarString)';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    Try
      iLength:=System.Length(Data);
      if iLength>0 then begin
        EnterCriticalSection(RSRP^.SendBuffer.Lock);
        try
          RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.Stream.Size;
          RSRP^.SendBuffer.Stream.Write(Data[1],iLength);
          RSRP^.SendBuffer.posWrite:=RSRP^.SendBuffer.Stream.Position;
        finally
          LeaveCriticalSection(RSRP^.SendBuffer.Lock);
        end;
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
      end;
    Except
      On E:Exception do  begin
        EntryPoint:='TRSRManager.Send(Core.Strings.VarString).OnException';
        OnException('TRSRManager.Send(Core.Strings.VarString)','Exception',E.Message);
      end
    end;
  end;
end;

procedure   TRSRManager.Send(RSRP:PRSR; var Data:RawByteString);
var
  iLength:LongInt;
begin
  EntryPoint:='TRSRManager.Send(RawByteString)';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    Try
      iLength:=System.Length(Data);
      if iLength>0 then begin
        EnterCriticalSection(RSRP^.SendBuffer.Lock);
        try
          RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.Stream.Size;
          RSRP^.SendBuffer.Stream.Write(Data[1],iLength);
          RSRP^.SendBuffer.posWrite:=RSRP^.SendBuffer.Stream.Position;
        finally
          LeaveCriticalSection(RSRP^.SendBuffer.Lock);
        end;

        RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
      end;
    Except
      On E:Exception do  begin
        EntryPoint:='TRSRManager.Send(RawByteString).OnException';
        OnException('TRSRManager.Send(RawByteString)','Exception',E.Message);
      end
    end;
  end;
end;

procedure   TRSRManager.Send(RSRP:PRSR; Var Data:Core.Arrays.Types.VarString);
var
  iLength:LongInt;
  iLcv:LongInt;
begin
  EntryPoint:='TRSRManager.Send(Core.Arrays.Types.VarString)';
  If RSRP^.State or RSR_STATE_OPEN=RSRP^.State then begin
    for iLcv:=0 to High(Data) do begin
      Try
        iLength:=System.Length(Data[iLcv]);
        EnterCriticalSection(RSRP^.SendBuffer.Lock);
        try
          RSRP^.SendBuffer.Stream.Position:=RSRP^.SendBuffer.Stream.Size;
          RSRP^.SendBuffer.Stream.Write(Data[1],iLength);
          RSRP^.SendBuffer.Stream.Write(#13#10,2);
          RSRP^.SendBuffer.posWrite:=RSRP^.SendBuffer.Stream.Position;
        finally
          LeaveCriticalSection(RSRP^.SendBuffer.Lock);
        end;
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_WRITE;
        RSRP^.State:=RSRP^.State or RSR_STATE_POLL;
      Except
        On E:Exception do  begin
          EntryPoint:='TRSRManager.Send(Core.Strings.VarString).OnException';
          OnException('TRSRManager.Send(Core.Strings.VarString)','Exception',E.Message);
        end
      end;
    end;
  end;
end;

procedure   TRSRManager.Perform_TimeoutCheck(RSRP:PRSR);
begin
  EntryPoint:='TRSRManager.Perform_TimeoutCheck';
  If Not (eTimed in RSRP^.Errors) and (RSRP^.Finite) and (RSRP^.dtExpires<>0) and (Core.Timer.dtNow>RSRP^.dtExpires) then begin
    RSRP^.dtExpires:=0;  // Add time so that it can be removed properly and this is not called again in the meantime
    Include(RSRP^.Errors,eTimed);
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
    RSRP^.State:=RSRP^.State or RSR_STATE_ERROR;
  end;
end;


procedure   TRSRManager.Perform_TimeoutCheck_Nil(RSRP:PRSR);
begin
  // EntryPoint:='TRSRManager.Perform_TimeoutCheck_Nil';
end;

procedure   TRSRManager._OnReceive_Sync(var Handled:Boolean);
begin
  EntryPoint:='TRSRManager._OnReceive_Sync';
  Try
    FHandled:=Handled;
    Synchronize(@_OnReceive_ActSync);
    Handled:=FHandled;
  Except
    On E:Exception do  OnException('TRSRManager._OnReceive_Sync','Exception',E.Message);
  End;
end;

procedure   TRSRManager._OnReceive_ActSync;
begin
  EntryPoint:='TRSRManager._OnReceive_ActSync';
  Try
    If (FSyncRSRP^.State=(RSR_STATE_OPEN or FSyncRSRP^.State)) then begin
      EntryPoint:='TRSRManager._OnReceive_ActSync.OnDataReceived';
      OnDataReceived(FSyncRSRP,FHandled);
    end;
  Except
    On E:Exception do  OnException('TRSRManager._OnReceive_ActSync','Exception',E.Message);
  End;
end;

procedure   TRSRManager._OnReceive_NoSync(Var Handled:boolean);
begin
  EntryPoint:='TRSRManager._OnReceive_NoSync';
  Try
    If (FSyncRSRP^.State=(RSR_STATE_OPEN or FSyncRSRP^.State)) then begin
      EntryPoint:='TRSRManager._OnReceive_NoSync.OnDataReceived';
      OnDataReceived(FSyncRSRP,Handled);
    end;
  Except
    On E:Exception do  OnException('TRSRManager._OnReceive_NoSync','Exception',E.Message);
  End;
end;

procedure   TRSRManager._Sync_Socket_Process();
begin
  EntryPoint:='TRSRManager._Sync_Socket_Process.Enter';
  FSocketCount:=FSockets.Process();
  EntryPoint:='TRSRManager._Sync_Socket_Process.Done';
end;

{$ifdef Unix}
procedure sigpipe_handler(signal:Longint); cdecl;
begin

end;
{$endif}

procedure   TRSRManager.Execute;
begin
  FRunning:=True;
  {$ifdef useEventQueues}
    SocketEventsCreateQueues(FRcvQueue,FWriteQueue,FConQueue,FDisConQueue);
  {$endif}
  {$ifdef Unix}
    FSigNA.sa_handler:=SigActionHandler(@sigpipe_handler);
    FSigOA.sa_handler:=FSigNA.sa_handler;
    fpSigAction(SIGPIPE,@FSigNA,@FSigOA);
  (*

  sigactionrec = record
    sa_flags: cuint;
    sa_handler: sigactionhandler_t;
    sa_mask: sigset_t;
    sa_restorer: sigrestorerhandler_t; { Doesn't seem to exist on MIPS }
    sa_resv :  array [0..0] of cint;
  end;
  *)

  {$endif}
  While not (Terminated or EngineFailure) do begin
    Try
      EntryPoint:='TRSRManager.Execute.ProcessObjects';
      dtCycleStart:=Core.Timer.dtNow;
      dtCycleEnd:=0; bCycleWarning:=false;
      FSocketCount:=0;
      {$ifdef useEventQueues}
        EntryPoint:='TRSRManager.Execute.Polling.Close';
        RSR.SocketEventsPoll(FDisConQueue,FPollEvents,@cb_Socket_Event_Close);
        EntryPoint:='TRSRManager.Execute.Polling.Write';
        RSR.SocketEventsPoll(FWriteQueue,FPollEvents,@cb_Socket_Event_Write);
        EntryPoint:='TRSRManager.Execute.Polling.Read';
        RSR.SocketEventsPoll(FRcvQueue,FPollEvents,@cb_Socket_Event_Read);
        EntryPoint:='TRSRManager.Execute.Polling.Open';
        RSR.SocketEventsPoll(FConQueue,FPollEvents,@cb_Socket_Event_Open);
      {$endif}
       dtCycleStart:=Core.Timer.dtNow;
      {$ifdef SyncRSR}
        EntryPoint:='TRSRManager.Execute.FSockets.Process (Sync)';
        Synchronize(@_Sync_Socket_Process);
      {$else}
        EntryPoint:='TRSRManager.Execute.FSockets.Process';
        FSocketCount:=FSockets.Process();
      {$endif}
      dtCycleStart:=Core.Timer.dtNow;

      EntryPoint:='TRSRManager.Execute.Wait';
      dtCycleEnd:=Core.Timer.dtNow;
      dwCycleDuration:=DateUtils.MilliSecondsBetween(dtCycleEnd,dtCycleStart);
      RTLeventWaitFor(FSleepP,ENGINE_YIELD_MS[FSocketCount=0]);
    Except
      On E:Exception do begin
        if DateUtils.MilliSecondsBetween(dtNow,dtLastError)>ENGINE_EXCEPTION_LOOP_PAUSE_MS then begin
          OnException('TRSRManager.Execute',EntryPoint,E.Message);
          dtLastError:=Core.Timer.dtNow;
          RTLeventWaitFor(FSleepP,ENGINE_YIELD_EXCEPTION_MS);
        end;
      end;
    end;
  end;
  // Should loop through each socket and issue close statement
  FSockets.Shutdown();
  FRunning:=False;
end;

Constructor TRSRServer.Create(aManagersP:PRSRManagers; Const RSRType:TRSRType; Const IP:QWord; Const aPort,aScale:WORD; aSecure,aSuspended:Boolean);
begin
  FSleepP:=RTLEventCreate();
  FOnEngineFailure:=nil;
  FRunning:=false;
  if (Encryption.SSL.IsSSLloaded=false) then begin
    if OpenSSL()=false then begin
      EngineFailure:=True;
      raise Exception.Create(RSR.SSL_LOAD_FAILURE);
    end else if (Encryption.SSL.IsThreadSafe=false) then
      InitThreadLocks();
  end;
  Init(Cert);
  FRefactor:=TMemoryStream.Create;
  FSSLInfo.Mode:=sslServer;
  FLinger.l_linger:=0;
  FLinger.l_onoff:=0;
  FIP:=IP;
  FScale:=aScale;
  TI_ProcessLock.Priority:=tpHigher;
  TI_ProcessLock.Expires:=IncSecond(Core.Timer.dtNow,15);
  TI_ProcessLock.Event:=@OnTimer_ProcessLock;
  TI_ProcessLock.Location:='uRSR.TRSRServer.OnTimer_ProcessLock';
  ProcessLockThread.RegisterEvent(TI_ProcessLock,False);
  FDefaultSocketKind:=rsrsTCP;
  FreeOnTerminate:=true;
  FPort:=aPort;
  FBound:=False;
  iManagerLcv:=0;
  FManagersP:=aManagersP;
  Case RSRType of
    tTCP:
      begin
        FDefaultSocketKind:=rsrsTCP;
        FSocket:=Sockets.fpsocket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
      end;
    tUDP:
      begin
        FDefaultSocketKind:=rsrsUDP;
        FSocket:=Sockets.fpsocket(AF_INET,SOCK_DGRAM,IPPROTO_UDP);
      end;
  end;
  FActive:=False;
  FSecure:=aSecure;
  If FSocket<>0 then begin
    Case RSRType of
      tTCP: begin
        {$ifdef cpu64}
          System.InterlockedIncrement64(RSR_Connection_Count);
        {$else}
          System.Inc(RSR_Connection_Count);
        {$endif}
      end;
      tUDP: begin
        {$ifdef cpu64}
          System.InterlockedIncrement64(RSR_Stream_Count);
        {$else}
          System.Inc(RSR_Stream_Count);
        {$endif}
      end;
    end;
    FAddress:=Core.Utils.Sockets.SocketAddress(IP,aPort);
    Core.Utils.Sockets.SetReUseableMode(FSocket,SOCKET_REUSE_ON);
    {$ifdef Unix}Core.Utils.Sockets.SetSigNoSigPipe(FSocket,Core.Utils.Sockets.SIGPIPE_OFF);{$endif}
  end else
    OnError('TRSRServer.Create','Sockets.fpSocket',Core.Utils.Sockets.SocketErrorToString(Sockets.socketerror));

  Inherited Create(aSuspended);
end;

Destructor  TRSRServer.Destroy;
begin

  If FRunning then begin
    Terminate();
    WaitFor();
  end;
  ProcessLockThread.UnloadEvent(TI_ProcessLock,False);

  RTLEventDestroy(FSleepP);

  FreeAndNil(FRefactor);

  Done(Cert);

  Inherited Destroy;
end;

procedure   TRSRServer.Terminate;
var
  iLcv:LongInt;
begin
  fpShutdown(FSocket,SHUT_RDWR);
  CloseSocket(FSocket);
  Case FDefaultSocketKind of
    rsrsTCP: begin
      {$ifdef cpu64}
        System.InterlockedDecrement64(RSR_Connection_Count);
      {$else}
        System.Dec(RSR_Connection_Count);
      {$endif}
    end;
    rsrsUDP: begin
      {$ifdef cpu64}
        System.InterlockedDecrement64(RSR_Stream_Count);
      {$else}
        System.Dec(RSR_Stream_Count);
      {$endif}
    end;
    rsrsDNS:  begin
      {$ifdef cpu64}
        System.InterlockedDecrement64(RSR_Stream_Count);
      {$else}
        System.Dec(RSR_Stream_Count);
      {$endif}
    end;
  end;
  if (FManagersP<>nil) then
    for iLcv:=Low(FManagersP^) to High(FManagersP^) do
      FManagersP^[iLcv].Terminate;
  Inherited Terminate;
end;

procedure   TRSRServer.OnTimer_ProcessLock(TimerP:Core.Timer.PItem);
  {$ifndef RSR_NO_LOCK}
var
  iLcv:LongInt;
  iDuration:System.Int64;
  {$endif}
begin
  {$ifndef RSR_NO_LOCK}
  For iLcv:=Low(FManagersP^) to High(FManagersP^) do begin
    iDuration:=DateUtils.MillisecondsBetween(Core.Timer.dtNow,FManagersP^[iLcv].dtCycleStart);
    if (FManagersP^[iLcv].dtCycleEnd=0) and (iDuration>RSR_MAX_CYCLE_DURATION) then begin
      if FManagersP^[iLcv].bCycleWarning=false then begin
        FManagersP^[iLcv].OnException('TRSRServer.OnTimer_ProcessLock','OnTimer_ProcessLock',Format(FMT_PROCESS_LOCK_WARNING,[FManagersP^[iLcv].EntryPoint,IntToStr(iDuration div 1000)]));
        FManagersP^[iLcv].bCycleWarning:=true;
      end;

      if (iDuration>RSR_PROCESS_LOCK_DURATION) then begin
        FManagersP^[iLcv].OnException('TRSRServer.OnTimer_ProcessLock','OnTimer_ProcessLock',Format(FMT_PROCESS_LOCK_SHUTDOWN,[FManagersP^[iLcv].EntryPoint,IntToStr(iDuration div 1000)]));
        RSR.EngineFailure:=true;
      end;
    end;
  end;
  {$endif}
  TimerP^.Expires:=IncMillisecond(Core.Timer.dtNow,200);  // Wait for another socket
end;

Function    TRSRServer.GetNextManager:TRSRManager;
begin
  If iManagerLcv>=Length(FManagersP^) then
    iManagerLcv:=0;
  Result:=FManagersP^[iManagerLcv];
  Inc(iManagerLcv);
end;

procedure   TRSRServer.Execute;
var
  RemoteSize:LongInt;
  Manager:TRSRManager;
begin
  FRunning:=True;
  RemoteSize:=SizeOf(FRemoteAddr);
  While not (Terminated or EngineFailure or FBound) do begin
    Try
      FSockResult:=Sockets.fpBind(FSocket,@FAddress,SizeOf(FAddress));
      If FSockResult=0 then begin
        Core.Utils.Sockets.SetLinger(FSocket,FLinger);
        FSockResult:=Sockets.fpListen(FSocket,RSR_LISTEN_QUEUE);
        FBound:=true;
        if Assigned(FOnListening) then
          Synchronize(@SyncListeningEvent);
        If (FSockResult<>0) then
          OnError('TRSRServer.Execute','Sockets.fpListen',Core.Utils.Sockets.SocketErrorToString(Sockets.socketerror));
      end else
        RTLeventWaitFor(FSleepP,RSR_BIND_PAUSE);
    Except
      On E:Exception do OnException('TRSRServer.Execute','Bind Loop Exception',E.Message);
    end;
  end;
  While Not (Terminated or EngineFailure) Do begin
    If FBound and FActive then begin
      Try
        FRemoteSocket:=Sockets.fpAccept(FSocket,@FRemoteAddr,@RemoteSize);
        If (FRemoteSocket<>0) and (FRemoteSocket<>0) then begin
          If (FRemoteAddr.sin_addr.S_addr<>0) then begin
            Manager:=GetNextManager;
            If Manager<>Nil then begin
              Manager.Queue(FDefaultSocketKind,FRemoteSocket,FRemoteAddr);
            end else begin
              OnException('TRSRServer.Execute','Manager','No Manager Found for this protocol.');
            end;
          end else begin
            OnException('TRSRServer.Execute','Remote Address','No Remote Address Specified.');
          end;
        end else
          RTLeventWaitFor(FSleepP,RSR_ACCEPT_PAUSE);
      Except
        On E:Exception do OnException('TRSRServer.Execute','Accept Loop Exception',E.Message);
      end;
    end else
      RTLeventWaitFor(FSleepP,RSR_ACCEPT_PAUSE);
  end;
  if (EngineFailure=true) then begin
    ProcessLockThread.UnloadEvent(TI_ProcessLock,False);
    if Assigned(FOnEngineFailure) then
      FOnEngineFailure(self);
  end;
  FRunning:=False;
end;

procedure   TRSRServer.SyncListeningEvent;
begin
  FOnListening(Self);
end;

procedure   TRSRServer.SetActive(Value:Boolean);
begin
  FActive:=Value;
end;

procedure   TRSRServer.SetSecure(Value:Boolean);
begin
  FSecure:=Value;
end;

function   TRSRServer.cbKW_Uptime_Seconds(ItemP:PKeyword):Core.Strings.VarString;
var
  dtStamp:TDateTime;
  iWeeks:Cardinal;
  iDays:Cardinal;
  iHours:Cardinal;
  iMinutes:Cardinal;
begin
  dtStamp:=Core.Timer.dtNow;
  iWeeks:=dateUtils.WeeksBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncWeek(dtStamp,-iWeeks);
  iDays:=dateUtils.DaysBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncDay(dtStamp,-iDays);
  iHours:=DateUtils.HoursBetween(dtStamp,RSR_Startup);
  dtStamp:=DateUtils.IncHour(dtStamp,-iHours);
  iMinutes:=DateUtils.MinutesBetween(dtStamp,RSR_Startup);
  dtStamp:=DateUtils.IncMinute(dtStamp,-iMinutes);
  Result:=Core.Strings.toString(DateUtils.SecondsBetween(dtStamp,RSR_Startup));
end;

function   TRSRServer.cbKW_Loopback(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=ItemP^.Value;
end;

function   TRSRServer.cbKW_Date(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=FormatDateTime('dddd, mmmm d, yyyy',Core.Timer.dtNow);
end;

function   TRSRServer.cbKW_Year(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(dateUtils.YearOf(Core.Timer.dtNow));
end;

function   TRSRServer.cbKW_Time(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=TimeToStr(Core.Timer.dtNow);
end;


function   TRSRServer.cbKW_RSR_ConCount(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=Core.Strings.toString(RSR_Connection_Count);
end;

function   TRSRServer.cbKW_RSR_StreamCount(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=Core.Strings.toString(RSR_Stream_Count);
end;

function   TRSRServer.cbKW_RSR_TXCount(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(RSR.cntrTX);
end;

function   TRSRServer.cbKW_RSR_SentCount(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(RSR.cntrSentCount);
end;

function   TRSRServer.cbKW_RSR_RecvCount(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(RSR.cntrRecvCount);
end;

function   TRSRServer.cbKW_RSR_BytesSent(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(RSR.cntrSentBytes);
end;

function   TRSRServer.cbKW_RSR_BytesRecv(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(RSR.cntrRecvBytes);
end;

function   TRSRServer.cbKW_RSR_Filtered(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=IntToStr(RSR.cntrFiltered);
end;

function   TRSRServer.cbKW_Node_Mem_Total(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=Core.Strings.toString(Core.Timer.MemoryStats.Total/(1024*1024));
end;

function   TRSRServer.cbKW_Node_Mem_Free(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=Core.Strings.toString(Core.Timer.MemoryStats.Available/(1024*1024));
end;

function   TRSRServer.cbKW_Node_Mem_Load(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=Core.Strings.toString(Core.Timer.MemoryStats.Load);
end;

function   TRSRServer.cbKW_Uptime_Weeks(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=Core.Strings.toString(DateUtils.WeeksBetween(Core.Timer.dtNow,RSR_Startup));
end;

function   TRSRServer.cbKW_Uptime_Days(ItemP:PKeyword):Core.Strings.VarString;
var
  dtStamp:TDateTime;
  iWeeks:Cardinal;
begin
  dtStamp:=Core.Timer.dtNow;
  iWeeks:=dateUtils.WeeksBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncWeek(dtStamp,-iWeeks);
  Result:=Core.Strings.toString(DateUtils.DaysBetween(dtStamp,RSR_Startup));
end;

function   TRSRServer.cbKW_Uptime_Hours(ItemP:PKeyword):Core.Strings.VarString;
var
  dtStamp:TDateTime;
  iWeeks:Cardinal;
  iDays:Cardinal;
begin
  dtStamp:=Core.Timer.dtNow;
  iWeeks:=dateUtils.WeeksBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncWeek(dtStamp,-iWeeks);
  iDays:=dateUtils.DaysBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncDay(dtStamp,-iDays);
  Result:=Core.Strings.toString(DateUtils.HoursBetween(dtStamp,RSR_Startup));
end;

function   TRSRServer.cbKW_Uptime_Minutes(ItemP:PKeyword):Core.Strings.VarString;
var
  dtStamp:TDateTime;
  iWeeks:Cardinal;
  iDays:Cardinal;
  iHours:Cardinal;
begin
  dtStamp:=Core.Timer.dtNow;
  iWeeks:=dateUtils.WeeksBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncWeek(dtStamp,-iWeeks);
  iDays:=dateUtils.DaysBetween(dtStamp,RSR_Startup);
  dtStamp:=dateUtils.IncDay(dtStamp,-iDays);
  iHours:=DateUtils.HoursBetween(dtStamp,RSR_Startup);
  dtStamp:=DateUtils.IncHour(dtStamp,-iHours);
  Result:=Core.Strings.toString(DateUtils.MinutesBetween(dtStamp,RSR_Startup));
end;

{$i RSR.SocketEvents.Code.inc}

constructor TRSRMethodThread.Create(aManager:TRSRManager);
begin
  FSleepP:=RTLEventCreate;
  FRefactor:=TMemoryStream.Create();
  FXMLParser:=TDOMParser.Create();
  FMethods:=TRSRMethods.Create(aManager);
  FOwner:=aManager;
  FreeOnTerminate:=true;
  InitCriticalSection(FEntryLock);

  if (FOwner.Task<>nil) then
    Task:=Core.Database.Types.TTask.Create(FOwner.Task.Header,'uRSR.TRSRMethodThread');

  inherited Create(false,App.Consts.RSR.Engine.Settings.STACKSIZE_COMMANDS);
end;

destructor  TRSRMethodThread.Destroy;
begin
  If FRunning then begin
    Terminate();
    WaitFor();
  end;
  RTLEventDestroy(FSleepP);
  DoneCriticalSection(FEntryLock);
  FreeAndNil(FRefactor);
  FreeAndNil(FXMLParser);
  FreeAndNil(FMethods);
  inherited Destroy();
end;

procedure   TRSRMethodThread.setEntryPoint(Value:Core.Strings.VarString);
begin
  EnterCriticalSection(FEntryLock);
  Try
    FEntryPoint:=Value;
  finally
    LeaveCriticalSection(FEntryLock);
  end;
end;

function   TRSRMethodThread.getEntryPoint():Core.Strings.VarString;
begin
  EnterCriticalSection(FEntryLock);
  Try
    Result:=FEntryPoint;
  finally
    LeaveCriticalSection(FEntryLock);
  end;
end;

procedure   TRSRMethodThread._Sync_Method_Process();
begin
  FMethods.Process();
end;

procedure TRSRMethodThread.Execute();
begin
  FRunning:=True;
  While not (Terminated or EngineFailure) do begin
    {$ifdef SyncRSR}
      FProcessCount:=0;
      Synchronize(@_Sync_Method_Process);
    {$else}
      FProcessCount:=FMethods.Process();
    {$endif}
     setEntryPoint('uRSR.TRSRMethodThread.Wait');
     RTLeventWaitFor(FSleepP,ENGINE_YIELD_MS[FProcessCount=0]);
  end;
  FRunning:=True;
end;

constructor TRSRMethods.Create(aManager:TRSRManager);
begin
  FOwner:=aManager;
  inherited Create(Core.Generics.Defaults.FreeOnClear);
end;

function TRSRMethods.Process(): LongInt;
var
  meth:TRSRMethod;
  bExit:boolean;
begin
  Result:=0;
  meth:=Pop(); bExit:=false;
  While (FOwner.Terminated=false) and (meth<>nil) do begin
    Inc(Result);
    Try
      try
        meth.Execute();
        if (meth.FRSRP<>nil) and  (meth.FRSRP^.SendBuffer.posWrite>0) then
          bExit:=true;
      Except
        // Todo log
        bExit:=true;
      end;
    Finally
      FreeAndNil(meth);
      if (bExit=false) then
        meth:=Pop();
    end;
  end;
end;

constructor TRSRMethod.Create(aOwner:TRSRManager; aRSRP:PRSR);
begin
  FExecuted:=false;
  FRunOnce:=true;
  FOwner:=aOwner;
  FRSRP:=aRSRP;
  if aOwner.FRSRMethodThread=nil then
    aOwner.FRSRMethodThread:=TRSRMethodThread.Create(aOwner);
  FProcessor:=aOwner.FRSRMethodThread;
  inherited Create();
end;


destructor TRSRMethod.Destroy;
begin
  inherited Destroy();
end;

constructor TRSRDNSEntries.Create(AOwner:TRSRManager);
begin
  FOwner:=AOwner;
  Inherited Create();
end;

destructor TRSRDNSEntries.Destroy();
var
  iLcv:LongInt;
  dns:TDNSObject;
  rsrP:PRSR;
  Handled:boolean;
begin
  FOwner:=nil;
  Handled:=false;
  for iLcv:=0 to High(FItems) do begin
    dns:=FItems[iLcv];
    rsrP:=RSR_MAP[dns.Socket];
    if (dns.Socket>0) then begin
      Sockets.fpshutdown(dns.Socket,SHUT_RDWR);
      Sockets.CloseSocket(dns.Socket);
      if DNS_MAP[ rsrP^.Info.RequestID]=rsrP then
        DNS_MAP[ rsrP^.Info.RequestID]:=nil;
      rsrP^.State:=rsrP^.State or RSR_STATE_RECYCLE;
      {$ifdef cpu64}
        InterlockedDecrement64(RSR_Stream_Count);
      {$else}
        Dec(RSR_Stream_Count);
      {$endif}
    end;
  end;
  SetLength(FItems,0);
  Inherited Destroy();
end;

function TRSRDNSEntries.Acquire(ServerIP:QWord):TDNSObject;
var
  iLcv:LongInt;
  iLen:LongInt;
  dns:TDNSObject;
  rsrP:PRSR;
  Handled:boolean;
begin
  Result:=nil;
  dns:=nil;
  rsrP:=nil;
  Handled:=false;
  iLen:=Length(FItems);
  for iLcv:=0 to iLen-1 do begin
    if FItems[iLcv].Address.sin_addr.s_addr=ServerIP then begin
      dns:=FItems[iLcv];
      rsrP:=RSR_MAP[dns.Socket];
    end;
  end;
  If dns=nil then begin
    rsrP:=FOwner.Allocate(rsrClient,rsrsDNS);
    rsrP^.State:=RSR_STATE_OPEN or RSR_STATE_DNS or RSR_STATE_POLL or RSR_STATE_QUEUED;
    rsrP^.Info.Port:=53;

    dns:=TDNSObject.Create();
    rsrP^.DNS:=dns;

    SetLength(FItems,iLen+1);
    FItems[iLen]:=dns;

    with dns.Address do begin
      sin_addr.s_addr:=ServerIP;
      sin_family:=AF_INET;
      sin_port:=Sockets.htons(rsrP^.Info.Port);
    end;
    with rsrP^.Address do begin
      sin_addr.s_addr:=ServerIP;
      sin_family:=AF_INET;
      sin_port:=Sockets.htons(rsrP^.Info.Port);
    end;
    dns.Socket:=rsrP^.Info.Socket;
    // Binding of Source IP Address (no longer supported but could redo)
    // FDNSRSRP^.Address:=SocketAddress(FDNSRSRP^.Info.BindIP,0);
    // FDNSRSRP^.LastCall:=Sockets.fpBind(FDNSSocket,@FDNSRSRP^.Address,SizeOf(FDNSRSRP^.Address));

    rsrP^.dtExpires:=0;

    RSR_MAP[dns.Socket]:=rsrP;
    MAN_MAP[dns.Socket]:=FOwner;

    FOwner.cb_RSR_Initialize(rsrP,Handled);
    FOwner.cb_RSR_Queue(rsrP,Handled);
  end;
  Result:=dns;
end;

Initialization
  LocalHost:=Core.Utils.Sockets.InAddrFromStr('127.0.0.1');
  RSR_Startup:=Now;
  {$i RSR.SocketEvents.Init.inc}
  ProcessLockThread:=TTimerThread.Create(tpHigher);

Finalization
  FreeAndNil(ProcessLockThread);

end.

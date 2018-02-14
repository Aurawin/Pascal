{
  WebSocket Data Frame
  0                   1                   2                   3
  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
 +-+-+-+-+-------+-+-------------+-------------------------------+
 |M|R|R|R| opcode|R| Payload len |    Extended payload length    |
 |O|S|S|S|  (4)  |S|     (7)     |             (16/63)           |
 |R|V|V|V|       |V|             |   (if payload len==126/127)   |
 |E|1|2|3|       |4|             |                               |
 +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
 |     Extended payload length continued, if payload len == 127  |
 + - - - - - - - - - - - - - - - +-------------------------------+
 |                               |         Extension data        |
 +-------------------------------+ - - - - - - - - - - - - - - - +
 :                                                               :
 +---------------------------------------------------------------+
 :                       Application data                        :
}
  TWebSocketMode=(wsContinue,wsClose,wsPing,wsPong,wsText,wsBinary);
  TWebSocketHandler=class;
  TWebSocketFrame=class
  private
    FOwner         : TWebSocketHandler;
    FFrameType     : Byte;
    FHeader        : WORD;
    FMore          : Boolean;
    FError         : Boolean;
    FLength        : QWORD;
    FOpCode        : Byte;
    FMode          : TWebSocketMode;
  public
    constructor Create(aOwner:TWebSocketHandler); reIntroduce;
  public
    function  Transpose(var Value:WORD):QWORD; overload;
    procedure Reset();
    function  fromBuffer(Var Buffer:TDataBuffer; Data:TMemoryStream; Refactor:TStream):boolean;
    function  toBuffer(Mode:TWebSocketMode; var Buffer:TDataBuffer; Refactor:TStream):boolean;
  public
    property Error:boolean read FError;
    property More:Boolean read FMore;
    property Mode:TWebSocketMode read FMode;
    property Length:QWORD read FLength;
  end;

  TWebSocketHandler=class
  private
    FKey1          : Core.Strings.VarString;
    FKey2          : Core.Strings.VarString;
    FProtocol      : Core.Strings.VarString;
    FOrigin        : Core.Strings.VarString;
    FLocation      : Core.Strings.VarString;
    iKey1          : cardinal;
    iKey2          : cardinal;
    FRequest       : THTTPRequest;
    FResponse      : THTTPResponse;
    FManager       : THTTPManager;
    FFrame         : TWebSocketFrame;
    FData          : TMemoryStream;
  private
    function Spaces(var sInput:Core.Strings.VarString; iCount:LongInt):LongInt;
    function toKey(var sInput:Core.Strings.VarString):cardinal;
  public
    constructor Create(aManager:THTTPManager; aRequest:THTTPRequest; aResponse:THTTPResponse); reIntroduce;
    destructor  Destroy; override;
  public
    Headers        : Core.Arrays.Types.KeyStrings;
    Parameters     : Core.Arrays.Types.VarString;
  public
    procedure Reset();
    procedure HandShake(RSRP:PRSR);
  public
    property Frame:TWebSocketFrame read FFrame;
    property Data:TMemoryStream read FData;
  end;

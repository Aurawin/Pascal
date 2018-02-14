unit hRTSPd;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface
uses
  Classes,

  App.Build,
  App.Consts,

  Core.Streams,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Arrays.LargeInt,
  Core.Strings,
  RSR,
  RSR.HTTP,
  Core.Timer,
  Core.Utils.Time,

  SysUtils;

Type
  TSMPTEKind=(smpte,smpte30drop,smpte25);
  TRTSPCommand=(cmdUnknown,cmdDescribe,cmdAnnounce,cmdGetParameter,cmdOptions,cmdPause,cmdPlay,cmdRecord,cmdRedirect,cmdSetup,cmdSetParameter,cmdTeardown);
Const
  NO_SSL                         = false;
  SSL                            = true;
  RTSPComandList                 : Array[TRTSPCommand] of Core.Strings.VarString=('UNKNOWN','DESCRIBE','ANNOUNCE','GET PARAMETER','OPTIONS','PAUSE','PLAY','RECORD','REDIRECT','SETUP','SET PARAMETER','TEARDOWN');
  methodDescribe                 : Core.Strings.VarString = 'DESCRIBE';
  methodAnnounce                 : Core.Strings.VarString = 'ANNOUNCE';
  methodGetParameter             : Core.Strings.VarString = 'GET_PARAMETER';   // server to client to get stats - set content-type: text/parameters with content ex. saParameter(packets_received,jitter)
  methodOptions                  : Core.Strings.VarString = 'OPTIONS';
  methodPause                    : Core.Strings.VarString = 'PAUSE';
  methodPlay                     : Core.Strings.VarString = 'PLAY';
  methodRecord                   : Core.Strings.VarString = 'RECORD';
  methodRedirect                 : Core.Strings.VarString = 'REDIRECT';
  methodSetup                    : Core.Strings.VarString = 'SETUP';
  methodSetParameter             : Core.Strings.VarString = 'SET_PARAMETER';
  methodTearDown                 : Core.Strings.VarString = 'TEARDOWN';
  methodUpload                   : Core.Strings.VarString = 'UPLOAD';
  SMPTEKind                      : Array[TSMPTEKind] of Core.Strings.VarString=('smpte','smpte-30-drop','smpte-25');
  KeepAlive                      : Array[Boolean] of Core.Strings.VarString=('Keep-Alive','Close');
  HTTP                           : Array[Boolean] of Core.Strings.VarString=('http','https');
  RTSP                           : Array[Boolean] of Core.Strings.VarString=('rtsp','rtsps');
  RTS                            : Array[Boolean] of Core.Strings.VarString=('RTSP','RTSPS');
  fieldHost                      : Core.Strings.VarString = 'Host';
  fieldUpgrade                   : Core.Strings.VarString = 'Upgrade';
  fieldOrigin                    : Core.Strings.VarString = 'Origin';
  fieldID                        : Core.Strings.VarString = 'ID';    // ID of transaction
  fieldEUK                       : Core.Strings.VarString = 'EUK';   // End User Kind
  fieldAuth                      : Core.Strings.VarString = 'AUTH';  // End User Auth Core.Strings.VarString
  fieldCode                      : Core.Strings.VarString = 'CODE';  // Result Code
  fieldSearch                    : Core.Strings.VarString = 'SRCH';  // Search Term in header
  fieldKind                      : Core.Strings.VarString = 'KIND';  // generic Kind metric
  fieldNPT                       : Core.Strings.VarString = 'npt';
  fieldSequence                  : Core.Strings.VarString = 'CSeq';
  fieldAccept                    : Core.Strings.VarString = 'Accept';
  fieldSession                   : Core.Strings.VarString = 'Session';
  fieldContentType               : Core.Strings.VarString = 'Content-Type';
  fieldContentLength             : Core.Strings.VarString = 'Content-Length';
  fieldTransport                 : Core.Strings.VarString = 'Transport';
  fieldRequire                   : Core.Strings.VarString = 'Require';
  fieldLocation                  : Core.Strings.VarString = 'Location';
  fieldExpires                   : Core.Strings.VarString = 'Expires';
  fieldPublic                    : Core.Strings.VarString = 'Public';
  fieldIfModifedSince            : Core.Strings.VarString = 'If-Modified-Since';
  fieldRang                      : Core.Strings.VarString = 'Range';
  fieldRTPInfo                   : Core.Strings.VarString = 'RTP-Info';
  fieldProxyRequire              : Core.Strings.VarString = 'Proxy-Require';
  fieldUserAgent                 : Core.Strings.VarString = 'User-Agent';
  fieldDate                      : Core.Strings.VarString = 'Date';
  fieldClientChallenge           : Core.Strings.VarString = 'Client-Challenge';
  fieldServer                    : Core.Strings.VarString = 'Server';

  applicationSDP                 : Core.Strings.VarString = 'application/sdp';

  PUBLIC_OPTIONS                 : Core.Strings.VarString = 'DESCRIBE, SETUP, TEARDOWN, PLAY, PAUSE';
  HEADER_SEP                     : Core.Strings.VarString = #13#10#13#10;

   RTSP_PORT                     = 554;
   RTSP_NO_SEQUENCE              = 0;
   RTSP_NO_SESSION               = 0;

   SCI_Continue	                 = 100;
   SCI_OK	                 = 200;
   SCI_Created	                 = 201;
   SCI_Low_On_Storage_Space      = 250;
   SCI_RDR_Multiple_Choices      = 300;
   SCI_RDR_Moved_Permanently     = 301;
   SCI_RDR_Moved_Temporarily     = 302;
   SCI_RDR_See_Other             = 303;
   SCI_Not_Modified              = 304;
   SCI_Use_Proxy                 = 305;
   SCI_Bad_Request	         = 400;
   SCI_Unauthorized	         = 401;
   SCI_Payment_Required	         = 402;
   SCI_Forbidden                 = 403;
   SCI_Not_Found                 = 404;
   SCI_Method_Not_Allowed        = 405;
   SCI_Not_Acceptable            = 406;
   SCI_Proxy_Authentication_Required = 407;
   SCI_Request_Timeout           = 408;
   SCI_Gone                      = 410;
   SCI_Length_Required           = 411;
   SCI_Precondition_Failed       = 412;
   SCI_Request_Entity_Too_Large  = 413;
   SCI_Request_URI_Too_Large     = 414;
   SCI_Unsupported_Media_Type    = 415;
   SCI_Parameter_Not_Understood  = 451;
   SCI_Conference_Not_Found      = 452;
   SCI_Not_Enough_Bandwidth      = 453;
   SCI_Session_Not_Found         = 454;
   SCI_Method_Not_Valid_in_This_State = 455;
   SCI_Header_Field_Not_Valid_For_Resource = 456;
   SCI_Invalid_Range             = 457;
   SCI_Parameter_Is_ReadOnly     = 458;
   SCI_Aggregate_Operation_Not_Allowed = 459;
   SCI_Only_Aggregate_Operation_Allowed = 460;
   SCI_Unsupported_Transport     = 461;
   SCI_Destination_Unreachable   = 462;
   SCI_Internal_Server_Error     = 500;
   SCI_Not_Implemented           = 501;
   SCI_Bad_Gateway               = 502;
   SCI_Service_Unavailable       = 503;
   SCI_Gateway_Timeout           = 504;
   SCI_RTSP_Version_Not_Supported= 505;
   SCI_Option_Not_Supported      = 551;

   SCM_Continue	                 = 'Continue';
   SCM_OK	                 = 'OK';
   SCM_Created	                 = 'Created';
   SCM_Low_On_Storage_Space      = 'Low on Storage Space';
   SCM_RDR_Multiple_Choices      = 'Multiple Choices';
   SCM_RDR_Moved_Permanently     = 'Moved Permanently';
   SCM_RDR_Moved_Temporarily     = 'Moved Temporarily';
   SCM_RDR_See_Other             = 'See Other';
   SCM_Not_Modified              = 'Not Modified';
   SCM_Use_Proxy                 = 'Use Proxy';
   SCM_Bad_Request	         = 'Bad Request';
   SCM_Unauthorized	         = 'Unauthorized';
   SCM_Payment_Required	         = 'Payment Required';
   SCM_Forbidden                 = 'Forbidden';
   SCM_Not_Found                 = 'Not Found';
   SCM_Method_Not_Allowed        = 'Method Not Allowed';
   SCM_Not_Acceptable            = 'Not Acceptable';
   SCM_Proxy_Authentication_Required = 'Proxy Authentication Required';
   SCM_Request_Timeout           = 'Request Time-out';
   SCM_Gone                      = 'Gone';
   SCM_Length_Required           = 'Length Required';
   SCM_Precondition_Failed       = 'Precondition Failed';
   SCM_Request_Entity_Too_Large  = 'Request Entity Too Large';
   SCM_Request_URI_Too_Large     = 'Request-URI Too Large';
   SCM_Unsupported_Media_Type    = 'Unsupported Media Type';
   SCM_Parameter_Not_Understood  = 'Parameter Not Understood';
   SCM_Conference_Not_Found      = 'Conference Not Found';
   SCM_Not_Enough_Bandwidth      = 'Not Enough Bandwidth';
   SCM_Session_Not_Found         = 'Session Not Found';
   SCM_Method_Not_Valid_in_This_State = 'Method Not Valid in This State';
   SCM_Header_Field_Not_Valid_For_Resource = 'Header Field Not Valid for Resource';
   SCM_Invalid_Range             = 'Invalid Range';
   SCM_Parameter_Is_ReadOnly     = 'Parameter Is Read-Only';
   SCM_Aggregate_Operation_Not_Allowed = 'Aggregate operation not allowed';
   SCM_Only_Aggregate_Operation_Allowed = 'Only aggregate operation allowed';
   SCM_Unsupported_Transport     = 'Unsupported transport';
   SCM_Destination_Unreachable   = 'Destination unreachable';
   SCM_Internal_Server_Error     = 'Internal Server Error';
   SCM_Not_Implemented           = 'Not Implemented';
   SCM_Bad_Gateway               = 'Bad Gateway';
   SCM_Service_Unavailable       = 'Service Unavailable';
   SCM_Gateway_Timeout           = 'Gateway Time-out';
   SCM_RTSP_Version_Not_Supported= 'RTSP Version not supported';
   SCM_Option_Not_Supported      = 'Option not supported';

var
  DebugMP3                       : TStream;
  Server_Header                  : Core.Strings.VarString;

Type
   TResource=record
     Protocol                    : Core.Strings.VarString;
     Server                      : Core.Strings.VarString;
     Port                        : Core.Strings.VarString;
     Path                        : Core.Strings.VarString;
   end;

   TRequest=class
   // input format: METHOD RESOURCE(*) RTSP/MAJ.MIN
   // 1.)  OPTIONS * RTSP/1.0
   //      CSeq: 1
   // 2.)  SETUP rtsp://server:554/media/rsks.mov RTSP/1.0
   //      CSeq: 1
   // 3.)  OPTIONS rtsp://server/media/rsks.mov RTSP/1.0
   //      CSeq: 1
   private
     FRefactor      : TMemoryStream;
   public
     Headers        : Core.Arrays.Types.KeyStrings;
     Close          : boolean;
     Code           : LongInt;
     contentLength  : LongInt;
     versionMajor   : LongInt;
     versionMinor   : LongInt;
     Sequence       : LongInt;
     Error          : LongInt;
     Session        : Int64;
     Parameters     : Core.Arrays.Types.VarString;
     Requirements   : Core.Arrays.Types.VarString;
     Accepts        : Core.Arrays.Types.VarString;
     Resource       : TResource;
     Content        : Core.Strings.VarString;
     Command        : Core.Strings.VarString;
     Protocol       : Core.Strings.VarString;
     Method         : TRTSPCommand;
   public
     constructor Create(); reIntroduce;
     destructor  Destroy(); override;
   public
     procedure Reset();
     function  containsRequest(var Buffer:RSR.TDataBuffer):boolean;
   end;

   TResponse=class
   // output format:
   // RTSP/1.0 200 OK
   // CSeq: 1
   private
     FRefactor      : TMemoryStream;
   public
     Headers        : Core.Arrays.Types.KeyStrings;
     statusCode     : LongInt;
     contentLength  : LongInt;
     versionMajor   : LongInt;
     versionMinor   : LongInt;
     Session        : Int64;
     Sequence       : LongInt;
     prePared       : Boolean;
     Content        : Core.Strings.VarString;
     contentType    : Core.Strings.VarString;
     statusMessage  : Core.Strings.VarString;
   public
     constructor Create(); reIntroduce;
     destructor  Destroy(); override;
   public
     procedure Reset();
     procedure Prepare(iCode,iSequence:LongInt; iSession:Int64);
     procedure Send(RSRP:PRSR; Refactor:TStream);
   end;

   function  fromString(Method:Core.Strings.VarString):TRTSPCommand; overload;
   function  fromString(Data:Core.Strings.VarString; var Item:TResource):boolean; overload;

   procedure Init(var Item:TResource); overload;
   procedure Done(var Item:TResource); overload;
   procedure Empty(var Item:TResource); overload;

implementation


constructor TRequest.Create();
begin
  FRefactor:=TMemoryStream.Create();
  Init(Parameters);
  Init(Requirements);
  Init(Accepts);
  Init(Headers);
  Init(Resource);
  System.SetLength(Content,0);
  System.SetLength(Command,0);
  System.SetLength(Protocol,0);
  Method:=cmdUnknown;
  ContentLength:=0;
  Session:=0;
  Error:=0;
  Code:=SCI_OK;
  inherited Create;
end;

destructor TRequest.Destroy();
begin
  FreeAndNil(FRefactor);
  ContentLength:=0;
  Method:=cmdUnknown;
  Error:=0;
  Session:=0;
  Code:=SCI_OK;
  System.SetLength(Content,0);
  System.SetLength(Command,0);
  System.SetLength(Protocol,0);
  Done(Headers);
  Done(Resource);
  Done(Requirements);
  inherited Destroy;
end;

procedure TRequest.Reset();
begin
  Method:=cmdUnknown;
  Session:=0;
  Sequence:=0;
  Code:=SCI_OK;
  Error:=0;
  Close:=False;
  versionMajor:=1;
  versionMinor:=0;

  Empty(Parameters);
  Empty(Headers);
  Empty(Resource);
  Empty(Requirements);
  Empty(Accepts);

  System.SetLength(Content,0);
  System.SetLength(Command,0);
  System.SetLength(Protocol,0);
end;

function  fromString(Method:Core.Strings.VarString):TRTSPCommand;
var
  lcv:TRTSPCommand;
begin
  Result:=cmdUnknown;
  for lcv:=Low(TRTSPCommand) to High(TRTSPCommand) do begin
    if SameText(Method,RTSPComandList[lcv]) then begin
      Result:=lcv;
      Break;
    end;
  end;
end;

function  fromString(Data:Core.Strings.VarString; var Item:TResource):boolean;
var
  iLength:LongInt;
  iLoc:LongInt;
begin
  Result:=false;
  iLength:=System.Length(Data);
  if (iLength>0) then begin
    if  (Data='*') then begin
      Item.Path:='*';
      Result:=true;
    end else begin
      iLoc:=Pos('://',Data);
      if iLoc>0 then begin
        Item.Protocol:=System.Copy(Data,1,iLoc-1);
        System.Delete(Data,1,iLoc+2);
        // server:554/media/rsks.mov
        iLoc:=Pos('/',Data);
        if iLoc>0 then begin
          Item.Server:=System.Copy(Data,1,iLoc-1);
          System.Delete(Data,1,iLoc);
          // Optional Port
          iLength:=System.Length(Item.Server);
          iLoc:=Pos(':',Item.Server);
          if iLoc>0 then begin
            Item.Port:=System.Copy(Item.Server,iLoc,iLength-iLoc);
            System.Delete(Item.Server,iLoc,iLength-iLoc);
          end;
          Item.Path:=Data;
          Result:=true;
        end;
      end;
    end;
  end;
end;

procedure Init(var Item:TResource);
begin
  With Item do begin
    SetLength(Protocol,0);
    SetLength(Server,0);
    SetLength(Port,0);
    SetLength(Path,0);
  end;
end;

procedure Done(var Item:TResource);
begin
  With Item do begin
    Finalize(Protocol);
    Finalize(Server);
    Finalize(Port);
    Finalize(Path);
  end;
  Finalize(Item);
end;

procedure Empty(var Item:TResource);
begin
  With Item do begin
    SetLength(Protocol,0);
    SetLength(Server,0);
    SetLength(Port,0);
    SetLength(Path,0);
  end;
end;


function  TRequest.containsRequest(var Buffer:TDataBuffer):boolean;
var
  iCMDEnd:LongInt;
  iLoc:LongInt;
  iCount:LongInt;
  saData:Core.Arrays.Types.VarString;
  sData:Core.Strings.VarString;
begin
  Result:=False;
  Reset();
  iLoc:=Core.Streams.Pos(Buffer.Stream,HEADER_SEP);
  if (iLoc>0) then begin
    iCMDEnd:=Core.Streams.Pos(Buffer.Stream,#13#10);
    Command:=Core.Streams.Extract(Buffer.Stream,0,iCMDEnd);

    // input format: METHOD RESOURCE(*) RTSP/MAJ.MIN
    Core.Arrays.VarString.fromString(saData,Command,#32);
    Try
      Method:=fromString(Core.Arrays.VarString.Parameter(saData,1));
      if not fromString(Core.Arrays.VarString.Parameter(saData,2),Resource) then
        Error:=SCI_Bad_Request;
      sData:=Core.Arrays.VarString.Parameter(saData,3);
    Finally
      Core.Arrays.VarString.Empty(saData);
    end;
    Core.Arrays.VarString.fromString(saData,sData,'/');
    Try
      Protocol:=Core.Arrays.VarString.Parameter(saData,1);
      sData:=Core.Arrays.VarString.Parameter(saData,2);
    Finally
      Core.Arrays.VarString.Empty(saData);
    end;
    // must break down resource... Also we don't relay so any requests not of this domain cannot be forwarded
    // Resource should be struct
    Core.Arrays.VarString.fromString(saData,sData,'.');
    Try
      versionMajor:=StrToIntDef(Core.Arrays.VarString.Parameter(saData,1),1);
      versionMinor:=StrToIntDef(Core.Arrays.VarString.Parameter(saData,2),0);
    finally
      Core.Arrays.VarString.Empty(saData);
    end;
    Core.Arrays.KeyString.fromString(@Headers,Core.Streams.Extract(Buffer.Stream,iCMDEnd+2,iLoc), ': ');
    iCount:=System.Length(Headers);
    Sequence:=Core.Arrays.KeyString.GetItemAsInteger(Headers,fieldSequence,iCount,0);
    Core.Arrays.VarString.fromString(@Requirements,Core.Arrays.KeyString.GetItemByKey(Headers,fieldRequire),', ');
    Core.Arrays.VarString.fromString(@Accepts,Core.Arrays.KeyString.GetItemByKey(Headers,fieldAccept),', ');
    contentLength:=Core.Arrays.KeyString.GetItemAsInteger(Headers,fieldContentLength,iCount,0);
    if (contentLength>0) then begin
      if (Buffer.Stream.Size>=( (iLoc+4)+contentLength) ) then begin
        Content:=Core.Streams.Extract(Buffer.Stream,iLoc+4,contentLength);
        Result:=True;
        RSR.Refactor(Buffer,FRefactor,iLoc+4);
      end;
    end else begin
      Result:=True;
      RSR.Refactor(Buffer,FRefactor,iLoc+4);
    end;
  end;
end;

constructor TResponse.Create();
begin
  prePared:=false;
  Core.Arrays.KeyString.Init(Headers);
  versionMajor:=1;
  versionMinor:=0;
  ContentLength:=0;
  FRefactor:=TMemoryStream.Create;
  inherited Create();
end;

destructor TResponse.Destroy();
begin
  Core.Arrays.KeyString.Done(Headers);
  ContentLength:=0;
  FreeAndNil(FRefactor);
  inherited Destroy();
end;

procedure TResponse.Reset();
begin
  prePared:=false;
  Core.Arrays.KeyString.Empty(Headers);
  contentLength:=0;
  versionMajor:=1;
  versionMinor:=0;
  statusCode:=SCI_OK;
  SetLength(statusMessage,0);
  SetLength(contentType,0);
  SetLength(Content,0);
end;

procedure TResponse.Prepare(iCode,iSequence:LongInt; iSession:Int64);
begin
  Sequence:=iSequence;
  statusCode:=iCode;
  Session:=iSession;
  case statusCode of
    SCI_Continue                            : statusMessage:=SCM_Continue;
    SCI_OK	                            : statusMessage:=SCM_OK;
    SCI_Created	                            : statusMessage:=SCM_Created;
    SCI_Low_On_Storage_Space                : statusMessage:=SCM_Low_On_Storage_Space;
    SCI_RDR_Multiple_Choices                : statusMessage:=SCM_RDR_Multiple_Choices;
    SCI_RDR_Moved_Permanently               : statusMessage:=SCM_RDR_Moved_Permanently;
    SCI_RDR_Moved_Temporarily               : statusMessage:=SCM_RDR_Moved_Temporarily;
    SCI_RDR_See_Other                       : statusMessage:=SCM_RDR_See_Other;
    SCI_Not_Modified                        : statusMessage:=SCM_Not_Modified;
    SCI_Use_Proxy                           : statusMessage:=SCM_Use_Proxy;
    SCI_Bad_Request	                    : statusMessage:=SCM_Bad_Request;
    SCI_Unauthorized	                    : statusMessage:=SCM_Unauthorized;
    SCI_Payment_Required      	            : statusMessage:=SCM_Payment_Required;
    SCI_Forbidden                           : statusMessage:=SCM_Forbidden;
    SCI_Not_Found                           : statusMessage:=SCM_Not_Found;
    SCI_Method_Not_Allowed                  : statusMessage:=SCM_Method_Not_Allowed;
    SCI_Not_Acceptable                      : statusMessage:=SCM_Not_Acceptable;
    SCI_Proxy_Authentication_Required       : statusMessage:=SCM_Proxy_Authentication_Required;
    SCI_Request_Timeout                     : statusMessage:=SCM_Request_Timeout;
    SCI_Gone                                : statusMessage:=SCM_Gone;
    SCI_Length_Required                     : statusMessage:=SCM_Length_Required;
    SCI_Precondition_Failed                 : statusMessage:=SCM_Precondition_Failed;
    SCI_Request_Entity_Too_Large            : statusMessage:=SCM_Request_Entity_Too_Large;
    SCI_Request_URI_Too_Large               : statusMessage:=SCM_Request_URI_Too_Large;
    SCI_Unsupported_Media_Type              : statusMessage:=SCM_Unsupported_Media_Type;
    SCI_Parameter_Not_Understood            : statusMessage:=SCM_Parameter_Not_Understood;
    SCI_Conference_Not_Found                : statusMessage:=SCM_Conference_Not_Found;
    SCI_Not_Enough_Bandwidth                : statusMessage:=SCM_Not_Enough_Bandwidth;
    SCI_Session_Not_Found                   : statusMessage:=SCM_Session_Not_Found;
    SCI_Method_Not_Valid_in_This_State      : statusMessage:=SCM_Method_Not_Valid_in_This_State;
    SCI_Header_Field_Not_Valid_For_Resource : statusMessage:=SCM_Header_Field_Not_Valid_For_Resource;
    SCI_Invalid_Range                       : statusMessage:=SCM_Invalid_Range;
    SCI_Parameter_Is_ReadOnly               : statusMessage:=SCM_Parameter_Is_ReadOnly;
    SCI_Aggregate_Operation_Not_Allowed     : statusMessage:=SCM_Aggregate_Operation_Not_Allowed;
    SCI_Only_Aggregate_Operation_Allowed    : statusMessage:=SCM_Only_Aggregate_Operation_Allowed;
    SCI_Unsupported_Transport               : statusMessage:=SCM_Unsupported_Transport;
    SCI_Destination_Unreachable             : statusMessage:=SCM_Destination_Unreachable;
    SCI_Internal_Server_Error               : statusMessage:=SCM_Internal_Server_Error;
    SCI_Not_Implemented                     : statusMessage:=SCM_Not_Implemented;
    SCI_Bad_Gateway                         : statusMessage:=SCM_Bad_Gateway;
    SCI_Service_Unavailable                 : statusMessage:=SCM_Service_Unavailable;
    SCI_Gateway_Timeout                     : statusMessage:=SCM_Gateway_Timeout;
    SCI_RTSP_Version_Not_Supported          : statusMessage:=SCM_RTSP_Version_Not_Supported;
    SCI_Option_Not_Supported                : statusMessage:=SCM_Option_Not_Supported;
  end;
  Core.Arrays.KeyString.Add(@Headers,fieldSequence,IntToStr(Sequence));
  if Session>0 then
      Core.Arrays.KeyString.Add(@Headers,fieldSession,IntToStr(Session));
  Core.Arrays.KeyString.Add(Headers,fieldServer,Server_Header);
  Core.Arrays.KeyString.Add(@Headers,fieldDate,UTCTime);
  if contentLength>0 then begin
    Core.Arrays.KeyString.Add(@Headers,fieldContentLength,IntToStr(contentLength));
    Core.Arrays.KeyString.Add(Headers,fieldContentType,contentType);
  end;
  prePared:=true;
end;

procedure TResponse.Send(RSRP:PRSR; Refactor:TStream);
begin
  Refactor.Size:=0;
  FRefactor.Clear;
  if prePared then begin
    Core.Streams.Write(
      Concat(
        RTS[NO_SSL],'/',IntToStr(versionMajor),'.',IntToStr(versionMinor),' ',
        IntToStr(statusCode),' ',
        statusMessage,
        #13#10
      ),
      Refactor
    );
    Core.Arrays.KeyString.toStream(Headers,FRefactor,': ');
    FRefactor.Seek(0,soFromBeginning);
    Refactor.CopyFrom(FRefactor,FRefactor.Size-2);
    Refactor.Seek(0,soFromEnd);
    Core.Streams.Write(HEADER_SEP,4,Refactor);
    if contentLength>0 then
      Core.Streams.Write(Content,Refactor);
    Refactor.Seek(0,soFromBeginning);
    MAN_MAP[RSRP^.Info.Socket].Send(RSRP,Refactor);
  end;
end;

initialization
  Server_Header:=Concat(App.Build.Title,' ',App.Build.Edition,' Version ',App.Build.Version,' RSR Build ',App.Build.RSR);
  //DebugMP3:=TFileStream.Create('/home/atbrunner/H.mp3',fmOpenRead);

end.


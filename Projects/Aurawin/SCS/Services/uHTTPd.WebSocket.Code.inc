constructor TWebSocketFrame.Create(aOwner:TWebSocketHandler);
begin
  FOwner:=aOwner;
  Inherited Create;
end;

procedure TWebSocketFrame.Reset();
begin
  FFrameType:=0;
  FHeader:=0;
  FMore:=false;
  FError:=false;
  FLength:=0;
  FOpCode:=0;
end;

function  TWebSocketFrame.Transpose(var Value:WORD):QWORD;
const
  HIGH                           = 15;
var
  iLcv                           : LongInt;
  bValues                        : Array[0..HIGH] of Boolean;
begin
  Result:=0;
  iLcv:=0;
  FillByte(bValues,HIGH+1,0);
  For iLcv:=0 to HIGH do
    bValues[iLcv]:=(Value and (1 shl (HIGH-iLcv)))=(1 shl (HIGH-iLcv));
  For iLcv:=0 to HIGH do
    if bValues[iLcv] then
      Result+=(1 shl iLcv);
end;


function  TWebSocketFrame.fromBuffer(Var Buffer:TDataBuffer; Data:TMemoryStream; Refactor:TStream):boolean;
var
  iStart,iEnd:Int64;
begin
  //Core.Streams.toFile(Buffer.Stream,'c:\websocket.txt');
  Reset();
  Result:=False;

  Buffer.Stream.Seek(Buffer.posRead,soFromBeginning);
  if (Buffer.posRead=Buffer.posWrite) then begin
    FMode:=wsClose;
    Result:=True;
  end else begin
    iStart:=RSR.Pos(Buffer,0);
    iEnd:=RSR.Pos(Buffer,$FF);
    if (iStart<>-1) and (iEnd<>-1) then begin
      Inc(iStart);
      FLength:=iEnd-iStart;
      if (FLength>0) then begin
        FMode:=wsText;
        Buffer.Stream.Seek(iStart,soFromBeginning);
        Data.CopyFrom(Buffer.Stream,FLength);
      end else begin
        FMode:=wsClose;
      end;
      Buffer.posRead:=iEnd+1;
      RSR.Refactor(Buffer,FOwner.FManager.Refactor,Buffer.posRead);

      FMore:=(RSR.Pos(Buffer,0)<>-1);
      Result:=True;
    end else begin
      FError:=True;
    end;
  end;
end;

function  TWebSocketFrame.toBuffer(Mode:TWebSocketMode; var Buffer:TDataBuffer; Refactor:TStream):boolean;
var
  iByte:Byte;
begin
  Reset();
  Refactor.Size:=0;
  FLength:=FOwner.FData.Size;
  // Header
  iByte:=0;
  Refactor.Write(iByte,1);
  FOwner.FData.Seek(0,soFromBeginning);
  Refactor.CopyFrom(FOwner.FData,FLength);
  iByte:=$FF;
  Refactor.Write(iByte,1);

  Refactor.Seek(0,soFromBeginning);
  Buffer.Stream.Seek(Buffer.posWrite,soFromBeginning);
  Buffer.Stream.CopyFrom(Refactor,Refactor.Size);

  Inc(Buffer.posWrite,Refactor.Size);

  Refactor.Size:=0;
end;


constructor TWebSocketHandler.Create(aManager:THTTPManager; aRequest:THTTPRequest; aResponse:THTTPResponse);
begin
  FData:=TMemoryStream.Create;
  FFrame:=TWebSocketFrame.Create(Self);
  FRequest:=aRequest;
  FResponse:=aResponse;
  FManager:=aManager;

  Core.Arrays.KeyString.Init(Headers);
  Core.Arrays.VarString.Init(Parameters);
  Inherited Create;
end;

destructor  TWebSocketHandler.Destroy;
begin
  FreeAndNil(FData);
  FreeAndNil(FFrame);
  Core.Arrays.KeyString.Done(Headers);
  Core.Arrays.VarString.Done(Parameters);
  Inherited Destroy;
end;

function    TWebSocketHandler.Spaces(var sInput:Core.Strings.VarString; iCount:LongInt):LongInt;
var
  iLcv:LongInt;
begin
  Result:=0;
  for iLcv:=1 to iCount do
    if sInput[iLcv]=#32 then inc(Result);
end;

function    TWebSocketHandler.toKey(var sInput:Core.Strings.VarString):cardinal;
var
  iLcv:LongInt;
  iLen:LongInt;
  iDiv:byte;
  iResult:QWord;
  sResult:String;
begin
  Result:=0;
  iLen:=System.Length(sInput);
  iDiv:=Spaces(sInput,iLen);
  iResult:=0; SetLength(sResult,0);
  for iLcv:=1 to iLen do begin
    if sInput[iLcv] in ['0','1','2','3','4','5','6','7','8','9'] then
      sResult:=Concat(sResult,sInput[iLcv]);
  end;
  iResult:=StrToInt64Def(sResult,0);
  if iDiv<>0 then
    Result:=iResult div iDiv;
end;

procedure TWebSocketHandler.Reset();
begin
  FData.Clear;
  Empty(Headers);
  Frame.Reset();
end;

procedure   TWebSocketHandler.HandShake(RSRP:PRSR);
var
  iCount:LongInt;
  Buffer: TChallResponse;

  dgSHA:TSHA1Digest;
  cxSHA:TSHA1Context;

  dgMD5:TMD5Digest;
  cxMD5:TMD5Context;

  procedure PushValidResponseMD5;
  begin

    FKey1:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldWebSockKey1,iCount);
    FKey2:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldWebSockKey2,iCount);



    FOrigin:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldOrigin,iCount);

    FLocation:=FRequest.URI;
    iCount:=System.Length(FLocation);
    if (iCount>0) and (FLocation[1]<>'/') then
      FLocation:=Concat('/',FLocation); // index.html > /index.html
    FLocation:=Concat('ws://',FRequest.Host,FLocation);

    iKey1:=toKey(FKey1);
    iKey2:=toKey(FKey2);


    Buffer[0]:=iKey1 and $ff000000 shr 24;
    Buffer[1]:=iKey1 and $00ff0000 shr 16;
    Buffer[2]:=iKey1 and $0000ff00 shr 8;
    Buffer[3]:=iKey1 and $000000ff;

    Buffer[4]:=iKey2 and $ff000000 shr 24;
    Buffer[5]:=iKey2 and $00ff0000 shr 16;
    Buffer[6]:=iKey2 and $0000ff00 shr 8;
    Buffer[7]:=iKey2 and $000000ff;

    Buffer[8]:=FRequest.Challenge[0];
    Buffer[9]:=FRequest.Challenge[1];
    Buffer[10]:=FRequest.Challenge[2];
    Buffer[11]:=FRequest.Challenge[3];
    Buffer[12]:=FRequest.Challenge[4];
    Buffer[13]:=FRequest.Challenge[5];
    Buffer[14]:=FRequest.Challenge[6];
    Buffer[15]:=FRequest.Challenge[7];

    MD5Init(cxMD5);
    MD5Update(cxMD5,Buffer[0],16);
    MD5Final(cxMD5,dgMD5);

    RSRP^.Finite:=false;
    PHTTP(RSRP^.Info.DataP)^.WebSocket:=True;
    {$ifdef cpu64}
      InterLockedIncrement64(RSR_Stream_Count);
    {$else}
      Inc(RSR_Stream_Count);
    {$endif}

    RSR.WriteLine('HTTP/1.1 101 WebSocket Protocol Handshake',RSRP^.SendBuffer);
    RSR.WriteLine(fieldUpgrade+': WebSocket',RSRP^.SendBuffer);
    RSR.WriteLine(fieldConnection+': Upgrade',RSRP^.SendBuffer);
    RSR.WriteLine(fieldWebSockOrigin+': '+FOrigin,RSRP^.SendBuffer);
    RSR.WriteLine(fieldWebSockLocation+': '+FLocation,RSRP^.SendBuffer);
    if Length(FProtocol)>0 then RSR.WriteLine(fieldWebSockProtocol+': '+FProtocol,RSRP^.SendBuffer);
    RSR.WriteLine('',RSRP^.SendBuffer);
    RSR.Write(dgMD5,RSRP^.SendBuffer);
    FManager.Send(RSRP);
  end;

  procedure PushValidResponse;
  var
    sCR:Core.Strings.VarString;
    iLen:LongInt;
  begin
    FOrigin:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldWebSockOrigin,iCount);
    FLocation:=FRequest.URI;
    iCount:=System.Length(FLocation);
    if (iCount>0) and (FLocation[1]<>'/') then
      FLocation:=Concat('/',FLocation); // index.html > /index.html
    FLocation:=Concat('ws://',FRequest.Host,FLocation);

    sCR:=Concat(FRequest.WebSocketKey,GUID_WEBSOCKET);
    //sCR:=Concat('dGhlIHNhbXBsZSBub25jZQ==',GUID_WEBSOCKET);
    //sCR:='dGhlIHNhbXBsZSBub25jZQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
    Encryption.SHA.Init(cxSha);
    Encryption.SHA.Update(cxSHA,sCR);
    Encryption.SHA.Final(cxSHA,dgSHA);
    sCR:=Encryption.SHA.Print(dgSHA);
    iLen:=SizeOf(dgSHA);
    SetLength(sCR,iLen);
    System.Move(dgSHA[0],sCR[1],iLen);
    sCR:=Encryption.Base64.Encode(sCR,iLen);

    RSR.WriteLine('HTTP/1.1 101 WebSocket Protocol Handshake',RSRP^.SendBuffer);
    RSR.WriteLine(fieldUpgrade+': WebSocket',RSRP^.SendBuffer);
    RSR.WriteLine(fieldConnection+': Upgrade',RSRP^.SendBuffer);
    RSR.WriteLine(fieldWebSockAccept+': '+sCR,RSRP^.SendBuffer);
    RSR.WriteLine(fieldWebSockOrigin+': '+FOrigin,RSRP^.SendBuffer);
    if Length(FProtocol)>0 then RSR.WriteLine(fieldWebSockProtocol+': '+FProtocol,RSRP^.SendBuffer);
    RSR.WriteLine('',RSRP^.SendBuffer);

  end;

  procedure PushProtocolNotImplemented;
  begin
    RSR.WriteLine('HTTP/1.1 501 WebSocket Protocol Not Implemented',RSRP^.SendBuffer);
    if Length(FProtocol)>0 then RSR.WriteLine(fieldWebSockProtocol+': '+FProtocol,RSRP^.SendBuffer);
    RSR.WriteLine('',RSRP^.SendBuffer);
    FManager.Send(RSRP);
  end;

begin
  Empty(RSRP^.SendBuffer);
  iCount:=System.Length(FRequest.Headers);
  FProtocol:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldWebSockProtocol,iCount);
  // Find Transport based on protocol
  If (FProtocol='AuraCore.Net') then begin
    RSRP^.Transport:=FManager.FTransportAuraCore;
    if (System.Length(FRequest.WebSocketKey)>0) then
      PushValidResponse
    else
      PushValidResponseMD5;
  end else
    PushProtocolNotImplemented;
end;

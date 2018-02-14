procedure TID3V20TagHeader.Reset();
begin
  FillByte(FID,SizeOf(FID),0);
  FillByte(FSize,SizeOf(FSize),0);
end;

function TID3V20TagHeader.Frame:TID3V20TagFrame;
begin
  Result:=FReader.FID3V20;
end;

function TID3V20TagHeader.Load(Stream:TStream):boolean;
begin
  // Assume that position @ TAG
  Result:=false;
  Stream.Read(FID[0],SizeOf(FID));
  Stream.Read(FSize[0],SizeOf(FSize));
  FName:=FID;
  FLength:=FSize[2] + FSize[1] shl 8 + FSize[0] shl 8;
  Result:=(FID[0]<>#0) and (FID[1]<>#0) and (FID[2]<>#0);
end;

procedure TID3V20TagPayload.Reset();
begin
  Inherited Reset();
end;

function  TID3V20TagPayload.Header:TID3V20TagHeader;
begin
  Result:=TID3V20TagHeader(FReader.FID3V20.FHeader);
end;

procedure TID3V20TagPayload.Load(Stream:TStream; var Handled:Boolean);
{$i Multimedia.MPEG.TID3V20TagPayload.Tags.inc}
begin
  case Header.FID[0] of
    'B': Process_B_1;
    'C': Process_C_1;
    'E': Process_E_1;
    'G': Process_G_1;
    'I': Process_I_1;
    'L': Process_L_1;
    'M': Process_M_1;
    'P': Process_P_1;
    'R': Process_R_1;
    'S': Process_S_1;
    'T': Process_T_1;
    'U': Process_U_1;
    'W': Process_W_1;
  end;
end;


constructor TID3V20TagFrame.Create(aReader:TReader);
begin
  FHeader:=TID3V20TagHeader.Create(aReader);
  FHeader.FVersMajor:=2;
  FHeader.FVersMinor:=0;
  FPayload:=TID3V20TagPayload.Create(aReader);
  Inherited Create(fID3V20,aReader);
end;


procedure TID3V20TagFrame.Reset();
begin
  Inherited Reset();
  FHeader.FVersMajor:=2;
  FHeader.FVersMinor:=0;
end;

function  TID3V20TagFrame.Header:TID3V20TagHeader;
begin
  Result:=TID3V20TagHeader(FReader.FID3V20.FHeader);
end;

function TID3V20TagFrame.Load(Stream:TStream; var Handled:Boolean):boolean;
begin
  {  Assume packets like this... are next
     Unique file identifier  "UFI"
     Frame size              $xx xx xx
     Owner identifier        <textstring> $00
     Identifier              <up to 64 bytes binary data>
  }
  Result:=FHeader.Load(Stream);
  if Result then begin
    FPayLoad.FStreamStart:=Stream.Position;
    FPayLoad.FLength:=FHeader.FLength;
    FLength:=FHeader.FLength;
    FPayLoad.Load(Stream,Handled);
    if Handled=false then
      Stream.Position:=FPayLoad.FStreamStart+FPayLoad.FLength;
  end;
end;

procedure TID3V20TagFrame.PreLoad(Stream:TStream);
begin
  // No preload on V2.0
end;


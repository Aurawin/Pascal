procedure TID3V30TagHeader.Reset();
begin
  Inherited Reset();
  System.FillByte(FID,SizeOf(FID),0);
  System.FillByte(FSize,SizeOf(FSize),0);
  System.FillByte(FFlags,SizeOf(FFlags),0);
end;

function  TID3V30TagHeader.Frame:TID3V30TagFrame;
begin
  Result:=FReader.FID3V30;
end;

function TID3V30TagHeader.Load(Stream:TStream):boolean;
var
  Data:Array[0..3] of byte;
begin
  Stream.Read(FID[0],4);
  Stream.Read(FSize[0],4);
  Stream.Read(FFlags[0],2);
  FName:=FID;

  FLength:=FSize[3] + FSize[2] shl 8 + FSize[1] shl 16 + FSize[0] shl 24;

  FTagAltered:=TFramePreservation(  ((FFlags[0] and 1) shl 7) shr 7 );
  FFileAltered:=TFramePreservation(  ((FFlags[0] and 1) shl 6) shr 6 );
  FReadOnly:=((FFlags[0] and 1 shl 5) shr 5)=1;
  FCompressed:=((FFlags[1] and 1 shl 7) shr 7)=1;
  FEncrypted:=((FFlags[1] and 1 shl 6) shr 6)=1;
  FGrouping:=((FFlags[1] and 1 shl 5) shr 5)=1;

  if FCompressed then begin
    Stream.Read(Data[0],4);
    FDecompressedSize:=Data[3] + Data[2] shl 8 + Data[1] shl 16 + Data[0] shl 24;
  end;
  if FEncrypted then
    Stream.Read(FEncryptionMethod,1);
  if FGrouping then
    Stream.Read(FGroupID,1);
  FReader.FStreamPosition:=Stream.Position;

  Result:=(FID[0]<>#0) and (FID[1]<>#0) and (FID[2]<>#0);
end;


constructor TID3V30TagFrame.Create(aReader:TReader);
begin
  FHeader:=TID3V30TagHeader.Create(aReader);
  FPayload:=TID3V30TagPayload.Create(aReader);
  inherited Create(TFrameKind.fID3v30,aReader);
end;

function  TID3V30TagFrame.Header:TID3V30TagHeader;
begin
  Result:=TID3V30TagHeader(FReader.FID3V30.FHeader);
end;

procedure TID3V30TagFrame.Reset();
begin
  Inherited Reset();
end;

procedure TID3V30TagFrame.Preload(Stream:TStream);
var
  Data:Array[0..3] of byte;
begin
  {
  ID3v2/file identifier      "ID3"
  ID3v2 version              $03 00
  ID3v2 flags                %abc00000
  ID3v2 size             4 * %0xxxxxxx

  Entire header was read in at this point but
  extended ID3 header information may follow
  }
  FIDData.Unsynchronization:=(FReader.FID3Frame.Header.FFlags and (1 shl 7) shr 7)=1;
  FIDData.Extended:=(FReader.FID3Frame.Header.FFlags and (1 shl 6) shr 6)=1;
  FIDData.Experimental:=(FReader.FID3Frame.Header.FFlags and (1 shl 5) shr 5)=1;
  If FIDData.Extended then begin
    Stream.Read(Data[0],4);
    FExtension.Length:=Data[3] + Data[2] shl 7 + Data[1] shl 14 + Data[0] shl 21;
    Stream.Read(Data[0],2);
    FExtension.CRC:=(((Data[0] and (1 shl 7)) shr 7)=1);
    Stream.Read(Data[0],4);
    FExtension.PaddingSize:=Data[3] + Data[2] shl 8 + Data[1] shl 16 + Data[0] shl 24;
    if FExtension.CRC then
      Stream.Read(FExtension.crcData[0],4);
  end;
  FReader.FStreamPosition:=Stream.Position;
end;

function TID3V30TagFrame.Load(Stream:TStream; var Handled:Boolean):boolean;
begin
  Result:=FHeader.Load(Stream);
  if Result then begin
    FPayLoad.FStreamStart:=Stream.Position;
    FPayLoad.FLength:=FHeader.FLength;
    FLength:=FHeader.FLength;
    FPayload.Load(Stream,Handled);
    if (Handled=false) then
      Stream.Position:=FPayLoad.FStreamStart+FPayLoad.FLength;
  end;
end;

function  TID3V30TagPayload.Header:TID3V30TagHeader;
begin
  Result:=TID3V30TagHeader(FReader.FID3V30.FHeader);
end;

procedure TID3V30TagPayload.Reset();
begin
  Inherited Reset();
end;

procedure TID3V30TagPayload.Load(Stream:TStream; var Handled:Boolean);
  {$i Multimedia.MPEG.TID3V30TagPayload.Tags.inc}
begin
  case Header.FID[0] of
    'A': Process_A_1;
    'C': Process_C_1;
    'E': Process_E_1;
    'G': Process_G_1;
    'I': Process_I_1;
    'L': Process_L_1;
    'M': Process_M_1;
    'O': Process_O_1;
    'P': Process_P_1;
    'R': Process_R_1;
    'S': Process_S_1;
    'T': Process_T_1;
    'U': Process_U_1;
    'W': Process_W_1;
  end;
end;



procedure Process_URL;
var
  iSize:LongInt;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fURL;
  FData:=@pldURL;
  pldURL.URL:=Core.Streams.Extract(Stream,Stream.Position,FReader.FTagFrame.FLength,soClean);
  iSize:=FLength-(Stream.Position-FStreamStart);
  if (iSize>0) then // Extra
    Stream.Position:=Stream.Position+iSize;
  FLength:=FReader.FTagFrame.FLength;
  Handled:=true;
end;

procedure Process_TEXT;
var
  sUTF8:Core.Strings.VarString;
  sUTF16:Core.Strings.Wide;
  sUTF16BE:Core.Strings.Wide;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fTextWriter;
  System.SetLength(pldText.Information,0);
  FData:=@pldText;
  Stream.Read(pldText.Encoding,1);
  case pldText.Encoding of
    txtISO8859_1 : begin
      pldText.Information:=UTF8Encode(Core.Streams.Extract(Stream,Stream.Position,FLength-1,soClean));
    end;
    txtUTF16 : begin
      Core.Streams.Extract(Stream,Stream.Position,FLength-1,sUTF16,soClean);
      pldText.Information:=UTF8Encode(sUTF16);
    end;
    txtUTF16BE: begin
      Core.Streams.Extract(Stream,Stream.Position,FLength-1,sUTF16,soClean);
      pldText.Information:=UTF8Encode(sUTF16);
    end;
    txtUTF8 : begin
      Core.Streams.Extract(Stream,Stream.Position,FLength-1,sUTF8,soClean);
      pldText.Information:=UTF8Encode(sUTF8);
    end;
  end;
  Handled:=true;
end;

procedure Process_AENC;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fAudioEncryption;
  FData:=@pldAENC;
  FLength:=Stream.Position;
  pldAENC.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  Stream.Read(pldAENC.PreviewStart,2);
  Stream.Read(pldAENC.PreviewLength,2);
  FLength:=FReader.FTagFrame.Header.FLength-(Stream.Position-FLength);
  SetLength(pldAENC.Data,FLength);
  if FLength>0 then
    Stream.Read(pldAENC.Data[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_A_E_N_3;
begin
  case Header.FID[3] of
    'C': Process_AENC;
  end;
end;

procedure Process_A_E_2;
begin
  case Header.FID[2] of
    'N': Process_A_E_N_3;
  end;
end;

procedure Process_APIC;
var
  iRead:LongInt;
  sUTF8:Core.Strings.VarString;
  sUTF16:Core.Strings.Wide;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fAttachedPicture;
  FData:=@pldAPIC;

  Stream.Read(pldAPIC.Encoding,1);
  pldAPIC.MimeType:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  Stream.Read(pldAPIC.ImageKind,1);

  case pldAPIC.Encoding of
    txtUTF8 : begin
      Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
      pldAPIC.Description:=UTF8Encode(sUTF8);
    end;
    txtUTF16 : begin
      Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
      pldAPIC.Description:=UTF8Encode(sUTF16);
    end;
    txtUTF16BE: begin
      Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
      pldAPIC.Description:=UTF8Encode(sUTF16);
    end;
    txtISO8859_1 : begin
      pldAPIC.Description:=UTF8Encode(Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor));
    end;
  end;

  iRead:=FLength - (Stream.Position-FStreamStart);

  if iRead>0 then begin
    System.SetLength(pldAPIC.Data,iRead);
    Stream.Read(pldAPIC.Data[0],iRead);
  end else
    System.SetLength(pldAPIC.Data,0);

  Handled:=True;
end;

procedure Process_A_P_I_3;
begin
  case Header.FID[3] of
    'C': Process_APIC;
  end;
end;

procedure Process_A_P_2;
begin
  case Header.FID[2] of
    'I': Process_A_P_I_3;
  end;
end;

procedure Process_A_1;
begin
  case Header.FID[1] of
    'E': Process_A_E_2;
    'P': Process_A_P_2;
  end;
end;

procedure Process_COMM;
begin
  FData:=@pldCOMM;
  FReader.FTagFrame.FKind:=TFrameKind.fComments;
  FLength:=Stream.Position;
  Stream.Read(pldCOMM.Encoding,1);
  Stream.Read(pldCOMM.Language[0],3);
  pldCOMM.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
  pldCOMM.Text:=Core.Streams.Extract(Stream,Stream.Position,FLength,soClean);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_COMR;
begin
  FData:=@pldCOMR;
  FLength:=Stream.Position;
  Stream.Read(pldCOMR.Encoding,1);
  pldCOMR.Price:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  pldCOMR.Expires:=Core.Streams.Extract(Stream,Stream.Position,8);
  pldCOMR.Contact:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  Stream.Read(pldCOMR.Kind,1);
  pldCOMR.Seller:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  pldCOMR.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  pldCOMR.MimeType:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
  SetLength(pldCOMR.Logo,FLength);
  if FLength>0 then
    Stream.Read(pldCOMR.Logo[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_C_O_M_3;
begin
  case Header.FID[2] of
    'M': Process_COMM;
    'R': Process_COMR;
  end;
end;

procedure Process_C_O_2;
begin
  case Header.FID[2] of
    'M': Process_C_O_M_3;
  end;
end;

procedure Process_C_1;
begin
  case Header.FID[1] of
    'O': Process_C_O_2;
  end;
end;

procedure Process_ENCR;
begin
  FData:=@pldENCR;
  FLength:=Stream.Position;
  pldENCR.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  Stream.Read(pldENCR.Method,1);

  FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
  SetLength(pldENCR.Data,FLength);
  if FLength>0 then
    Stream.Read(pldENCR.Data[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_E_N_C_3;
begin
  case Header.FID[3] of
    'R': Process_ENCR;
  end;
end;

procedure Process_E_N_2;
begin
  case Header.FID[2] of
    'C': Process_E_N_C_3;
  end;
end;

procedure Process_EQUA;
var
  iLcv:Cardinal;
  iCount:Cardinal;
  iBytes:Byte;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fEqualization;
  FData:=@pldEQUA;
  Stream.Read(pldEQUA.AdjustmentBits,1);
  FLength:=FReader.FTagFrame.FLength-1;
  iBytes:=BitsToBytes(pldEQUA.AdjustmentBits);
  iCount:=FLength div (2+iBytes);
  SetLength(pldEQUA.Bands,iCount);
  for iLcv:=0 to iCount do begin
    with pldEQUA.Bands[iLcv] do begin
      SetLength(Adjustment,iBytes);
      Stream.Read(Frequency,2);
      Stream.Read(Adjustment[0],iBytes);
      Direction:=TVolumeAdjustment(((Frequency and (1 shl 15)) shr 15));
      Frequency:=Frequency and not (1 shl 15);
    end;
  end;
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_E_Q_U_3;
begin
  case Header.FID[3] of
    'A': Process_EQUA;
  end;
end;

procedure Process_E_Q_2;
begin
  case Header.FID[2] of
    'U': Process_E_Q_U_3;
  end;
end;

procedure Process_ETCO;
var
  iDX:LongInt;
  iRead,iCount:Cardinal;
  iStop:qword;
begin
  FData:=@pldETCO;
  FReader.FTagFrame.FKind:=TFrameKind.fEventTimingCodes;
  FReader.FStreamPosition:=Stream.Position;
  FLength:=FReader.FTagFrame.Header.FLength;
  iStop:=FReader.FStreamPosition+FLength;
  Stream.Read(pldETCO.Format,1);
  FReader.FStreamPosition+=1;
  iCount:=0; iDX:=-1; iRead:=SizeOf(TEventItem);
  repeat
    iDX+=1;
    iCount+=1;
    System.SetLength(pldETCO.Events,iCount);
    Stream.Read(pldETCO.Events[iDX],iRead);
    FReader.FStreamPosition+=iRead;
  until (pldETCO.Events[iDX].Kind=tskOneByteOfEventsFollows) or (Stream.Position>=iStop);
  if pldETCO.Events[iDX].Kind=tskOneByteOfEventsFollows then begin
    Stream.Read(iRead,1);
    FReader.FStreamPosition+=1;
  end;
  Handled:=True;
end;

procedure Process_E_T_C_3;
begin
  case Header.FID[3] of
    'O': Process_ETCO;
  end;
end;

procedure Process_E_T_2;
begin
  case Header.FID[2] of
    'C': Process_E_T_C_3;
  end;
end;

procedure Process_E_1;
begin
  case Header.FID[1] of
    'N': Process_E_N_2;
    'Q': Process_E_Q_2;
    'T': Process_E_T_2;
  end;
end;

procedure Process_GEOB;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fGeneralEncapsulatedObject;
  FData:=@pldGEOB;
  FLength:=Stream.Position;
  Stream.Read(pldGEOB.Encoding,1);
  pldGEOB.MimeType:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  pldGEOB.FileName:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  pldGEOB.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
  System.SetLength(pldGEOB.Data,FLength);
  if FLength>0 then
    Stream.Read(pldGEOB.Data[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_G_E_O_3;
begin
  case Header.FID[3] of
    'B': Process_GEOB;
  end;
end;

procedure Process_G_E_2;
begin
  case Header.FID[2] of
    'O': Process_G_E_O_3;
  end;
end;

procedure Process_GRID;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fGroupIdentificationRegistration;
  FData:=@pldGRID;
  FLength:=Stream.Position;
  pldGRID.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  Stream.Read(pldGRID.Symbol,1);
  FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
  System.SetLength(pldGRID.Data,FLength);
  if FLength>0 then
    Stream.Read(pldGRID.Data[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_G_R_I_3;
begin
  case Header.FID[3] of
    'D': Process_GRID;
  end;
end;

procedure Process_G_R_2;
begin
  case Header.FID[2] of
    'I': Process_G_R_I_3;
  end;
end;

procedure Process_G_1;
begin
  case Header.FID[1] of
    'E': Process_G_E_2;
    'R': Process_G_R_2;
  end;
end;

procedure Process_IPLS;
begin
  Empty(pldIPLS.List);
  FReader.FTagFrame.FKind:=TFrameKind.fInvolvedPeople;
  FData:=@pldIPLS;
  Stream.Read(pldIPLS.Encoding,1);
  case pldIPL.Encoding of
     txtISO8859_1 : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPLS.List,FReader.FRefactor,Core.Streams.Types.seNormal);
     txtUTF16     : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPLS.List,FReader.FRefactor,Core.Streams.Types.seUTF16);
     txtUTF16BE   : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPLS.List,FReader.FRefactor,Core.Streams.Types.seUTF16BE);
     txtUTF8      : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPLS.List,FReader.FRefactor,Core.Streams.Types.seUTF8);
  end;
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_I_P_L_3;
begin
  case Header.FID[3] of
    'S' : Process_IPLS;
  end;
end;

procedure Process_I_P_2;
begin
  case Header.FID[2] of
    'L': Process_I_P_L_3;
  end;
end;

procedure Process_I_1;
begin
  case Header.FID[1] of
    'P': Process_I_P_2;
  end;
end;

procedure Process_LINK;
begin
  FData:=@pldLINK;
  FReader.FTagFrame.FKind:=TFrameKind.fLinkedInfo;
  Stream.Read(pldLINK.FrameID,3);
  pldLINK.URL:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  FLength:=FReader.FTagFrame.FLength-4;
  Core.Streams.Extract(Stream,Stream.Position,Stream.Position+FLength,0,pldLINK.List,FReader.FRefactor);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_L_I_N_3;
begin
  case Header.FID[3] of
    'K' : Process_LINK;
  end;
end;

procedure Process_L_I_2;
begin
  case Header.FID[2] of
    'N': Process_L_I_N_3;
  end;
end;

procedure Process_L_1;
begin
  case Header.FID[1] of
    'I': Process_L_I_2;
  end;
end;

procedure Process_MCDI;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fMusicCDIdentifier;
  FData:=@pldMCDI;
  FLength:=FReader.FTagFrame.Header.FLength;
  SetLength(pldMCDI.TOC,FLength);
  if FLength>0 then
    Stream.Read(pldMCDI.TOC[0],FLength);
  Handled:=True;
end;

procedure Process_M_C_D_3;
begin
  case Header.FID[3] of
    'I': Process_MCDI;
  end;
end;

procedure Process_M_C_2;
begin
  case Header.FID[2] of
    'D': Process_M_C_D_3;
  end;
end;

procedure Process_MLLT;
var
  IDX:LongInt;
  Count:cardinal;
begin
  FData:=@pldMLLT;
  FReader.FTagFrame.FKind:=TFrameKind.fMPEGLocationLookupTable;
  Stream.Read(pldMLLT.MPEGFrames[0],2);
  Stream.Read(pldMLLT.BytesBetween[0],3);
  Stream.Read(pldMLLT.MillisecondsBetween[0],3);
  Stream.Read(pldMLLT.BitsForBytesDeviation,1);
  Stream.Read(pldMLLT.BitsForFramesDeviation,1);

  FReader.FStreamPosition:=Stream.Position;

  FLength:=(FReader.FTagFrame.FLength - 10);
  Count:=0; IDX:=-1;
  repeat
    Count+=1;
    IDX+=1;
    SetLength(pldMLLT.References,Count);
    Stream.Read(pldMLLT.References[IDX],2);
    FReader.FStreamPosition+=2;
  until (Count>=FLength) or (FReader.FStreamPosition>=FReader.FStreamSize);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_M_L_L_3;
begin
  case Header.FID[3] of
    'T': Process_MLLT;
  end;
end;

procedure Process_M_L_2;
begin
  case Header.FID[2] of
    'L': Process_M_L_L_3;
  end;
end;

procedure Process_M_1;
begin
  case Header.FID[1] of
    'C': Process_M_C_2;
    'L': Process_M_L_2;
  end;
end;

procedure Process_OWNE;
begin
  FData:=@pldOWNE;
  FReader.FTagFrame.FKind:=TFrameKind.fOwnership;
  FLength:=FReader.FTagFrame.Header.FLength;
  FReader.FStreamPosition:=Stream.Position;
  Stream.Read(pldOWNE.Encoding,1);
  pldOWNE.PricePayed:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  SetLength(pldOWNE.Date,8);
  pldOWNE.Date:=Core.Streams.Extract(Stream,Stream.Position,8,soClean);
  FLength:=FLength-(Stream.Position-FReader.FStreamPosition);
  pldOWNE.Seller:=Core.Streams.Extract(Stream,Stream.Position,FLength,soClean);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_O_W_N_3;
begin
  case Header.FID[3] of
    'E': Process_OWNE;
  end;
end;

procedure Process_O_W_2;
begin
  case Header.FID[2] of
    'N': Process_O_W_N_3;
  end;
end;

procedure Process_O_1;
begin
  case Header.FID[1] of
    'W': Process_O_W_2;
  end;
end;

procedure Process_PCNT;
begin
  FData:=@pldPCNT;
  FReader.FTagFrame.FKind:=TFrameKind.fPlayCounter;
  FLength:=FReader.FTagFrame.Header.FLength;
  System.SetLength(pldPCNT.Counter,FLength);
  if FLength>0 then
    Stream.Read(pldPCNT.Counter[0],FLength);
  Handled:=True;
end;

procedure Process_PCST;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPodCast;
end;

procedure Process_P_C_N_3;
begin
  case Header.FID[3] of
    'T' : Process_PCNT;
  end;
end;

procedure Process_P_C_S_3;
begin
  case Header.FID[3] of
    'T' : Process_PCST;
  end;
end;

procedure Process_P_C_2;
begin
  case Header.FID[2] of
    'N': Process_P_C_N_3;
    'S': Process_P_C_S_3;
  end;
end;

procedure Process_POPM;
begin
  FData:=@pldPOPM;
  FReader.FTagFrame.FKind:=TFrameKind.fPopularimeter;
  FReader.FStreamPosition:=Stream.Position;
  FLength:=FReader.FTagFrame.Header.FLength;
  pldPOPM.Email:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  Stream.Read(pldPOPM.Rating,1);
  FLength:=FLength-(Stream.Position-FReader.FStreamPosition);

  System.SetLength(pldPOPM.Counter,FLength);
  if FLength>0 then
    Stream.Read(pldPOPM.Counter[0],FLength);
  Handled:=True;
end;

procedure Process_P_O_P_3;
begin
  case Header.FID[3] of
    'M' : Process_POPM;
  end;
end;

procedure Process_POSS;
begin
  FData:=@pldPOSS;
  FReader.FTagFrame.FKind:=TFrameKind.fPositionSynchronization;
  FReader.FStreamPosition:=Stream.Position;
  FLength:=FReader.FTagFrame.Header.FLength;
  Stream.Read(pldPOSS.Format,1);
  Stream.Read(pldPOSS.Position,4);
  Handled:=True;
end;

procedure Process_P_O_S_3;
begin
  case Header.FID[3] of
    'S' : Process_POSS;
  end;
end;

procedure Process_P_O_2;
begin
  case Header.FID[2] of
    'P': Process_P_O_P_3;
    'S': Process_P_O_S_3;
  end;
end;

procedure Process_PRIV;
begin
  FData:=@pldPRIV;
  FReader.FTagFrame.FKind:=TFrameKind.fPrivate;
  FReader.FStreamPosition:=Stream.Position;
  FLength:=FReader.FTagFrame.Header.FLength;
  pldPRIV.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  FLength:=FLength-(Stream.Position-FReader.FStreamPosition);

  System.SetLength(pldPRIV.Data,FLength);
  if FLength>0 then
    Stream.Read(pldPRIV.Data[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_P_R_I_3;
begin
  case Header.FID[3] of
    'V' : Process_PRIV;
  end;
end;

procedure Process_P_R_2;
begin
  case Header.FID[2] of
    'R': Process_P_R_I_3;
  end;
end;

procedure Process_P_1;
begin
  case Header.FID[1] of
    'C': Process_P_C_2;
    'O': Process_P_O_2;
    'R': Process_P_R_2;
  end;
end;

procedure Process_RBUF;
begin
  FData:=@pldRBUF;
  FReader.FTagFrame.FKind:=TFrameKind.fBufferSize;
  Stream.Read(pldRBUF.BufferSize,3);
  Stream.Read(pldRBUF.Flags,1);
  Stream.Read(pldRBUF.Offset,4);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_R_B_U_3;
begin
  case Header.FID[3] of
    'U': Process_RBUF;
  end;
end;

procedure Process_R_B_2;
begin
  case Header.FID[2] of
    'U': Process_R_B_U_3;
  end;
end;

procedure Process_RVAD;
var
  Flags:Byte;
  Bytes:Byte;

  procedure Push_Read_RL(var Item:TRightLeftVolumeAdjustment);
  begin
    Stream.Read(Item.Right.Delta[0],Bytes);
    Stream.Read(Item.Left.Delta[0],Bytes);
    Stream.Read(Item.Right.PeakVolume[0],Bytes);
    Stream.Read(Item.Left.PeakVolume[0],Bytes);
  end;

  procedure Push_Copy_RL(var Source,Dest:TRightLeftVolumeAdjustment);
  begin
    Dest.Right.Adjustment:=Source.Right.Adjustment;
    Dest.Left.Adjustment:=Source.Left.Adjustment;
    System.Move(Source.Right.Delta[0], Dest.Right.Delta[0],Bytes);
    System.Move(Source.Left.Delta[0], Dest.Left.Delta[0],Bytes);
    System.Move(Source.Right.PeakVolume[0], Dest.Right.PeakVolume[0],Bytes);
    System.Move(Source.Left.PeakVolume[0], Dest.Left.PeakVolume[0],Bytes);
  end;

begin
  FData:=@pldRVAD;
  FReader.FTagFrame.FKind:=TFrameKind.fVolumeAdj;
  FReader.FStreamPosition:=Stream.Position;
  FLength:=FReader.FTagFrame.Header.FLength;

  Stream.Read(Flags,1);
  Stream.Read(pldRVAD.BitsUsed,1);
  Bytes:=BitsToBytes(pldRVAD.BitsUsed);
  SetByteCount(Bytes,pldRVAD.Stereo);
  SetByteCount(Bytes,pldRVAD.Front);
  SetByteCount(Bytes,pldRVAD.Center);
  SetByteCount(Bytes,pldRVAD.Back);
  SetByteCount(Bytes,pldRVAD.Bass);

  // Set Adjustments
  pldRVAD.Stereo.Right.Adjustment:= TVolumeAdjustment( ((Flags and 1 shl 0) shr 0) );
  pldRVAD.Stereo.Left.Adjustment:= TVolumeAdjustment( ((Flags and 1 shl 1) shr 1) );
  pldRVAD.Back.Right.Adjustment:= TVolumeAdjustment( ((Flags and 1 shl 2) shr 2) );
  pldRVAD.Back.Left.Adjustment:= TVolumeAdjustment( ((Flags and 1 shl 3) shr 3) );
  pldRVAD.Center.Adjustment:= TVolumeAdjustment( ((Flags and 1 shl 4) shr 4) );
  pldRVAD.Bass.Adjustment:= TVolumeAdjustment( ((Flags and 1 shl 5) shr 5) );

  Push_Read_RL(pldRVAD.Stereo);

  FLength:=FLength-(Stream.Position-FReader.FStreamPosition);
  FReader.FStreamPosition:=Stream.Position;

  if FLength>0 then begin
    Push_Copy_RL(pldRVAD.Stereo,pldRVAD.Front);
    Push_Read_RL(pldRVAD.Back);
    FLength:=FLength-(Stream.Position-FReader.FStreamPosition);
    FReader.FStreamPosition:=Stream.Position;
    if FLength>0 then begin
      Stream.Read(pldRVAD.Center.Delta[0],Bytes);
      Stream.Read(pldRVAD.Center.PeakVolume[0],Bytes);
      FLength:=FLength-(Stream.Position-FReader.FStreamPosition);
      FReader.FStreamPosition:=Stream.Position;
      if FLength>0 then begin
        Stream.Read(pldRVAD.Bass.Delta[0],Bytes);
        Stream.Read(pldRVAD.Bass.PeakVolume[0],Bytes);
      end;
    end;
  end;

  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_R_V_A_3;
begin
  case Header.FID[3] of
    'D': Process_RVAD;
  end;
end;

procedure Process_RVRB;
begin
  FData:=@pldRVRB;
  FReader.FTagFrame.FKind:=TFrameKind.fReverb;
  FReader.FStreamPosition:=Stream.Position;
  Stream.Read(pldRVRB,SizeOf(pldRVRB));
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_R_V_R_3;
begin
  case Header.FID[3] of
    'B': Process_RVRB;
  end;
end;

procedure Process_R_V_2;
begin
  case Header.FID[2] of
    'A': Process_R_V_A_3;
    'R': Process_R_V_R_3;
  end;
end;

procedure Process_R_1;
begin
  case Header.FID[1] of
    'B': Process_R_B_2;
    'V': Process_R_V_2;
  end;
end;

procedure Process_SYLT;
var
  iDX   : LongInt;
  iCount:cardinal;
begin
  FData:=@pldSYLT;
  FReader.FTagFrame.FKind:=TFrameKind.fSyncedText;
  FLength:=FReader.FTagFrame.Header.FLength;
  FReader.FStreamPosition:=Stream.Position;

  Stream.Read(pldSYLT.Encoding,1);
  Stream.Read(pldSYLT.Language[0],3);
  Stream.Read(pldSYLT.Format,1);
  Stream.Read(pldSYLT.ContentType,1);
  pldSYLT.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);

  iDX:=-1; iCount:=0;
  repeat
    iCount+=1;
    iDX+=1;
    SetLength(pldSYLT.List,iCount);
    pldSYLT.List[iDX].Text:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    Stream.Read(pldSYLT.List[iDX].Stamp,4);
  until (Stream.Position>=FReader.FStreamPosition+FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_SYTC;
begin
  FData:=@pldSYTC;
  FReader.FTagFrame.FKind:=TFrameKind.fSyncedTempo;
  FLength:=FReader.FTagFrame.Header.FLength;
  FReader.FStreamPosition:=Stream.Position;

  Stream.Read(pldSYTC.Format,1);
  FLength-=1;
  SetLength(pldSYTC.Data,FLength);
  if FLength>0 then
    Stream.Read(pldSYTC.Data[0],FLength);
  FLength:=FReader.FTagFrame.Header.FLength;
  Handled:=True;
end;

procedure Process_S_Y_T_3;
begin
  case Header.FID[3] of
    'C': Process_SYTC;
  end;
end;

procedure Process_S_Y_L_3;
begin
  case Header.FID[3] of
    'T': Process_SYLT;
  end;
end;

procedure Process_S_Y_2;
begin
  case Header.FID[2] of
    'L': Process_S_Y_L_3;
    'T': Process_S_Y_T_3;
  end;
end;

procedure Process_S_1;
begin
  case Header.FID[1] of
    'Y': Process_S_Y_2;
  end;
end;

procedure Process_TALB;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fTitle;
end;

procedure Process_T_A_L_3;
begin
  case Header.FID[3] of
    'B': Process_TALB;
  end;
end;

procedure Process_T_A_2;
begin
  case Header.FID[2] of
    'L': Process_T_A_L_3;
  end;
end;

procedure Process_TBPM;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fBeatsPerMinute;
end;

procedure Process_T_B_P_3;
begin
  case Header.FID[3] of
    'M' : Process_TBPM;
  end;
end;

procedure Process_T_B_2;
begin
  case Header.FID[2] of
    'P' : Process_T_B_P_3;
  end;
end;

procedure Process_TCOM;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fComposer;
end;

procedure Process_TCON;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fContentType;
end;

procedure Process_TCOP;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fCopyrightMessage;
end;

procedure Process_T_C_O_3;
begin
  case Header.FID[3] of
    'M' : Process_TCOM;
    'N' : Process_TCON;
    'P' : Process_TCOP;
  end;
end;

procedure Process_TCAT;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fCopyrightMessage;
end;

procedure Process_T_C_A_3;
begin
  case Header.FID[3] of
    'T' : Process_TCAT;
  end;
end;

procedure Process_T_C_2;
begin
  case Header.FID[2] of
    'O' : Process_T_C_O_3;
    'A' : Process_T_C_A_3;
  end;
end;

procedure Process_TDAT;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fDate;
end;

procedure Process_T_D_A_3;
begin
  case Header.FID[3] of
    'T' : Process_TDAT;
  end;
end;

procedure Process_TDLY;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPlaylistDelay;
end;

procedure Process_T_D_L_3;
begin
  case Header.FID[3] of
    'Y' : Process_TDLY;
  end;
end;

procedure Process_TDRL;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fReleaseTime;
end;

procedure Process_TDRC;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fRecordingTime;
end;

procedure Process_T_D_R_3;
begin
  case Header.FID[3] of
    'C' : Process_TDRC;
    'L' : Process_TDRL;
  end;
end;

procedure Process_TDTG;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fTaggingTime;
end;

procedure Process_T_D_T_3;
begin
  case Header.FID[3] of
    'G' : Process_TDTG;
  end;
end;

procedure Process_TDES;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPodcastDescription;
end;

procedure Process_TDEN;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fEncodingTime;
end;

procedure Process_TDOR;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fOriginalReleaseTime;
end;

procedure Process_T_D_E_3;
begin
  case Header.FID[3] of
    'S' : Process_TDES;
    'N' : Process_TDEN;
  end;
end;

procedure Process_T_D_O_3;
begin
  case Header.FID[3] of
    'R' : Process_TDOR;
  end;
end;

procedure Process_T_D_2;
begin
  case Header.FID[2] of
    'A' : Process_T_D_A_3;
    'E' : Process_T_D_E_3;
    'L' : Process_T_D_L_3;
    'O' : Process_T_D_O_3;
    'R' : Process_T_D_R_3;
    'T' : Process_T_D_T_3;
  end;
end;

procedure Process_TENC;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fEncodedBy;
end;

procedure Process_T_E_N_3;
begin
  case Header.FID[3] of
    'C' : Process_TENC;
  end;
end;

procedure Process_T_E_X_3;
begin
  case Header.FID[3] of
    'T' : Process_TEXT;
  end;
end;

procedure Process_T_E_2;
begin
  case Header.FID[2] of
    'N' : Process_T_E_N_3;
    'X' : Process_T_E_X_3;
  end;
end;

procedure Process_TFLT;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fFileType;
end;

procedure Process_T_F_L_3;
begin
  case Header.FID[3] of
    'T' : Process_TFLT;
  end;
end;

procedure Process_T_F_2;
begin
  case Header.FID[2] of
    'L' : Process_T_F_L_3;
  end;
end;

procedure Process_TGID;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPodcastID;
end;

procedure Process_T_G_I_3;
begin
  case Header.FID[3] of
    'D' : Process_TGID;
  end;
end;

procedure Process_T_G_2;
begin
  case Header.FID[2] of
    'I' : Process_T_G_I_3;
  end;
end;

procedure Process_TIME;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fTime;
end;

procedure Process_T_I_M_3;
begin
  case Header.FID[3] of
    'E' : Process_TIME;
  end;
end;

procedure Process_TIPL;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fInvolvedPeopleList;
end;

procedure Process_T_I_P_3;
begin
  case Header.FID[3] of
    'L' : Process_TIPL;
  end;
end;

procedure Process_TIT1;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fContentGroupDescription;
end;

procedure Process_TIT2;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fTitleDescription;
end;

procedure Process_TIT3;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fSubTitleDescription;
end;

procedure Process_T_I_T_3;
begin
  case Header.FID[3] of
    '1' : Process_TIT1;
    '2' : Process_TIT2;
    '3' : Process_TIT3;
  end;
end;

procedure Process_T_I_2;
begin
  case Header.FID[2] of
    'M' : Process_T_I_M_3;
    'P' : Process_T_I_P_3;
    'T' : Process_T_I_T_3;
  end;
end;

procedure Process_TKEY;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fInitialKey;
end;

procedure Process_TKWD;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPodcastKeywords;
end;

procedure Process_T_K_E_3;
begin
  case Header.FID[3] of
    'Y' : Process_TKEY;
  end;
end;

procedure Process_T_K_W_3;
begin
  case Header.FID[3] of
    'D' : Process_TKWD;
  end;
end;

procedure Process_T_K_2;
begin
  case Header.FID[2] of
    'E' : Process_T_K_E_3;
    'W' : Process_T_K_W_3;
  end;
end;

procedure Process_TLAN;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fLanguages;
end;

procedure Process_T_L_A_3;
begin
  case Header.FID[3] of
    'N' : Process_TLAN;
  end;
end;

procedure Process_TLEN;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fLength;
end;

procedure Process_T_L_E_3;
begin
  case Header.FID[3] of
    'N' : Process_TLEN;
  end;
end;

procedure Process_T_L_2;
begin
  case Header.FID[2] of
    'A' : Process_T_L_A_3;
    'E' : Process_T_L_E_3;
  end;
end;

procedure Process_TMCL;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fMusicianCreditsList;
end;

procedure Process_T_M_C_3;
begin
  case Header.FID[3] of
    'L' : Process_TMCL;
  end;
end;

procedure Process_TMED;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fMediaType;
end;

procedure Process_T_M_E_3;
begin
  case Header.FID[3] of
    'D' : Process_TMED;
  end;
end;

procedure Process_TMOO;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fMood;
end;

procedure Process_T_M_O_3;
begin
  case Header.FID[3] of
    'O' : Process_TMOO;
  end;
end;

procedure Process_T_M_2;
begin
  case Header.FID[2] of
    'C' : Process_T_M_C_3;
    'E' : Process_T_M_E_3;
    'O' : Process_T_M_O_3;
  end;
end;

procedure Process_TOAL;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fOriginalArtist;
end;

procedure Process_T_O_A_3;
begin
  case Header.FID[3] of
    'L' : Process_TOAL;
  end;
end;

procedure Process_TOFN;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fOriginalfilename;
end;

procedure Process_T_O_F_3;
begin
  case Header.FID[3] of
    'N' : Process_TOFN;
  end;
end;

procedure Process_TOLY;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fOriginalWriter;
end;

procedure Process_T_O_L_3;
begin
  case Header.FID[3] of
    'Y' : Process_TOLY;
  end;
end;

procedure Process_TOPE;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fOriginalArtist;
end;

procedure Process_T_O_P_3;
begin
  case Header.FID[3] of
    'E' : Process_TOPE;
  end;
end;

procedure Process_TORY;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fOriginalReleaseYear;
end;

procedure Process_T_O_R_3;
begin
  case Header.FID[3] of
    'Y' : Process_TORY;
  end;
end;

procedure Process_TOWN;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fFileLicense;
end;

procedure Process_T_O_W_3;
begin
  case Header.FID[3] of
    'W' : Process_TOWN;
  end;
end;

procedure Process_T_O_2;
begin
  case Header.FID[2] of
    'A' : Process_T_O_A_3;
    'F' : Process_T_O_F_3;
    'L' : Process_T_O_L_3;
    'P' : Process_T_O_P_3;
    'R' : Process_T_O_R_3;
    'W' : Process_T_O_W_3;
  end;
end;

procedure Process_TPE1;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fLeadArtist;
end;

procedure Process_TPE2;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fAccompaniment;
end;

procedure Process_TPE3;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fConductor;
end;

procedure Process_TPE4;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fModifiedBy;
end;

procedure Process_T_P_E_3;
begin
  case Header.FID[3] of
    '1' : Process_TPE1;
    '2' : Process_TPE2;
    '3' : Process_TPE3;
    '4' : Process_TPE4;
  end;
end;

procedure Process_TPOS;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPartOfaSet;
end;

procedure Process_T_P_O_3;
begin
  case Header.FID[3] of
    'S' : Process_TPOS;
  end;
end;

procedure Process_TPUB;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPublisher;
end;

procedure Process_T_P_U_3;
begin
  case Header.FID[3] of
    'B' : Process_TPUB;
  end;
end;

procedure Process_TPRO;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fProducedNotice;
end;

procedure Process_T_P_R_3;
begin
  case Header.FID[3] of
    'O' : Process_TPRO;
  end;
end;

procedure Process_T_P_2;
begin
  case Header.FID[2] of
    'E' : Process_T_P_E_3;
    'O' : Process_T_P_O_3;
    'R' : Process_T_P_R_3;
    'U' : Process_T_P_U_3;
  end;
end;

procedure Process_TRCK;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fTrackNumber;
end;

procedure Process_T_R_C_3;
begin
  case Header.FID[3] of
    'K': Process_TRCK;
  end;
end;

procedure Process_TRDA;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fRecordingDates;
end;

procedure Process_T_R_D_3;
begin
  case Header.FID[3] of
    'A': Process_TRDA;
  end;
end;

procedure Process_TRSN;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fRecordingDates;
end;

procedure Process_TRSO;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fRadioStationOwner;
end;

procedure Process_T_R_S_3;
begin
  case Header.FID[3] of
    'N': Process_TRSN;
    'O': Process_TRSO;
  end;
end;

procedure Process_T_R_2;
begin
  case Header.FID[2] of
    'C': Process_T_R_C_3;
    'D': Process_T_R_D_3;
    'S': Process_T_R_S_3;
  end;
end;

procedure Process_TSIZ;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fSize;
end;

procedure Process_T_S_I_3;
begin
  case Header.FID[3] of
    'Z': Process_TSIZ;
  end;
end;

procedure Process_TSRC;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fISRC;
end;

procedure Process_T_S_R_3;
begin
  case Header.FID[3] of
    'C': Process_TSRC;
  end;
end;

procedure Process_TSSE;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fEncodingParams;
end;

procedure Process_TSST;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fSetSubTitle;
end;

procedure Process_T_S_S_3;
begin
  case Header.FID[3] of
    'E': Process_TSSE;
    'T': Process_TSST;
  end;
end;

procedure Process_TSOA;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fAlbumSortOrder;
end;

procedure Process_TSOP;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fPerformerSortOrder;
end;

procedure Process_TSOT;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fTitleSortOrder;
end;

procedure Process_T_S_O_3;
begin
  case Header.FID[3] of
    'A': Process_TSOA;
    'P': Process_TSOP;
    'T': Process_TSOT;
  end;
end;

procedure Process_T_S_2;
begin
  case Header.FID[2] of
    'I': Process_T_S_I_3;
    'O': Process_T_S_O_3;
    'R': Process_T_S_R_3;
    'S': Process_T_S_S_3;
  end;
end;

procedure Process_TXXX;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fUserDefinedTextInformation;
  FData:=@pldTXX;
  Stream.Read(pldTXX.Encoding,1);
  pldTXX.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
  FLength:=System.Length(pldTXX.Description)+2;
  FLength:=FReader.FTagFrame.FLength-FLength;
  pldTXX.Value:=Core.Streams.Extract(Stream,Stream.Position,FLength,soClean);
  FLength:=FReader.FTagFrame.FLength;
  Handled:=true;
end;

procedure Process_T_X_X_3;
begin
  case Header.FID[3] of
    'X': Process_TXXX;
  end;
end;

procedure Process_T_X_2;
begin
  case Header.FID[2] of
    'X': Process_T_X_X_3;
  end;
end;

procedure Process_TYER;
begin
  Process_TEXT;
  FReader.FTagFrame.FKind:=TFrameKind.fYear;
end;

procedure Process_T_Y_E_3;
begin
  case Header.FID[3] of
    'R': Process_TYER;
  end;
end;

procedure Process_T_Y_2;
begin
  case Header.FID[2] of
    'E': Process_T_Y_E_3;
  end;
end;

procedure Process_T_1;
begin
  case Header.FID[1] of
    'A': Process_T_A_2;
    'B': Process_T_B_2;
    'C': Process_T_C_2;
    'D': Process_T_D_2;
    'E': Process_T_E_2;
    'F': Process_T_F_2;
    'G': Process_T_G_2;
    'I': Process_T_I_2;
    'K': Process_T_K_2;
    'L': Process_T_L_2;
    'M': Process_T_M_2;
    'O': Process_T_O_2;
    'P': Process_T_P_2;
    'R': Process_T_R_2;
    'S': Process_T_S_2;
    'X': Process_T_X_2;
    'Y': Process_T_Y_2;
  end;
end;

procedure Process_UFID;
begin

end;

procedure Process_U_F_I_3;
begin
  case Header.FID[3] of
    'D': Process_UFID;
  end;
end;

procedure Process_U_F_2;
begin
  case Header.FID[2] of
    'I': Process_U_F_I_3;
  end;
end;

procedure Process_USER;
begin

end;

procedure Process_U_S_E_3;
begin
  case Header.FID[3] of
    'R': Process_USER;
  end;
end;

procedure Process_USLT;
begin

end;

procedure Process_U_S_L_3;
begin
  case Header.FID[3] of
    'T': Process_USLT;
  end;
end;

procedure Process_U_S_2;
begin
  case Header.FID[2] of
    'E': Process_U_S_E_3;
    'L': Process_U_S_L_3;
  end;
end;

procedure Process_U_1;
begin
  case Header.FID[1] of
    'F': Process_U_F_2;
    'S': Process_U_S_2;
  end;
end;

procedure Process_WCOM;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fCommericalInfo;
end;

procedure Process_WCOP;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fCopyrightInfo;
end;

procedure Process_W_C_O_3;
begin
  case Header.FID[3] of
    'M': Process_WCOM;
    'P': Process_WCOP;
  end;
end;

procedure Process_W_C_2;
begin
  case Header.FID[2] of
    'O': Process_W_C_O_3;
  end;
end;

procedure Process_WOAF;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fOfficialFileWebpage;
end;

procedure Process_WOAR;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fOfficialArtistWebpage;
end;

procedure Process_WOAS;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fOfficialAudioSourceWebpage;
end;

procedure Process_W_O_A_3;
begin
  case Header.FID[3] of
    'F': Process_WOAF;
    'R': Process_WOAR;
    'S': Process_WOAS;
  end;
end;

procedure Process_WORS;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fStationURL;
end;

procedure Process_W_O_R_3;
begin
  case Header.FID[3] of
    'S': Process_WORS;
  end;
end;

procedure Process_W_O_2;
begin
  case Header.FID[2] of
    'A': Process_W_O_A_3;
    'R': Process_W_O_R_3;
  end;
end;

procedure Process_WPAY;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fPaymentURL;
end;

procedure Process_W_P_A_3;
begin
  case Header.FID[3] of
    'Y': Process_WPAY;
  end;
end;

procedure Process_WPUB;
begin
  Process_URL;
  FReader.FTagFrame.FKind:=TFrameKind.fPublishersOfficialWebpage;
end;

procedure Process_W_P_U_3;
begin
  case Header.FID[3] of
    'B': Process_WPUB;
  end;
end;

procedure Process_W_P_2;
begin
  case Header.FID[2] of
    'A': Process_W_P_A_3;
    'U': Process_W_P_U_3;
  end;
end;

procedure Process_WXXX;
var
  iSize:LongInt;
begin
  FReader.FTagFrame.FKind:=TFrameKind.fUserDefinedURL;
  FData:=@pldWXX;
  Stream.Read(pldWXX.Encoding,1);
  pldWXX.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);

  iSize:=FLength-(Stream.Position-FStreamStart);

  pldWXX.URL:=Core.Streams.Extract(Stream,Stream.Position,iSize,soClean);

  Handled:=true;
end;

procedure Process_W_X_X_3;
begin
  case Header.FID[3] of
    'X': Process_WXXX;
  end;
end;

procedure Process_W_X_2;
begin
  case Header.FID[2] of
    'X': Process_W_X_X_3;
  end;
end;

procedure Process_WFED;
begin
  Process_Text;
  FReader.FTagFrame.FKind:=TFrameKind.fPodcastFeed;
end;

procedure Process_W_F_E_3;
begin
  case Header.FID[3] of
    'D': Process_WFED;
  end;
end;

procedure Process_W_F_2;
begin
  case Header.FID[2] of
    'E': Process_W_F_E_3;
  end;
end;

procedure Process_W_1;
begin
  case Header.FID[1] of
    'C': Process_W_C_2;
    'F': Process_W_F_2;
    'O': Process_W_O_2;
    'P': Process_W_P_2;
    'X': Process_W_X_2;
  end;
end;


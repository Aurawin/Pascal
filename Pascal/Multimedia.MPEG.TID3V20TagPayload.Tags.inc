  procedure Process_BUF;
  begin
    FData:=@pldBUF;
    FReader.FTagFrame.FKind:=TFrameKind.fBufferSize;
    Stream.Read(pldBUF.BufferSize,3);
    Stream.Read(pldBUF.Flags,1);
    Stream.Read(pldBUF.Offset,4);
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_B_U_2;
  begin
    case Header.FID[2] of
      'F': Process_BUF;
    end;
  end;

  procedure Process_B_1;
  begin
    case Header.FID[1] of
      'U': Process_B_U_2;
    end;
  end;

  procedure Process_CNT;
  begin
    FData:=@pldCNT;
    FReader.FTagFrame.FKind:=TframeKind.fPlayCounter;
    SetLength(pldCNT.Counter,FReader.FTagFrame.FLength);
    Stream.Read(pldCNT.Counter[0],FReader.FTagFrame.FLength);
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_COM;
  var
    sUTF16:widestring;
    SUTF16BE:widestring;
    sUTF8:widestring;
    iSize:LongInt;
  begin
    FData:=@pldCOM;
    FReader.FTagFrame.FKind:=TFrameKind.fComments;
    FStreamStart:=Stream.Position;
    Stream.Read(pldCOM.Encoding,1);
    Stream.Read(pldCOM.Language,3);

    case pldCOM.Encoding of
      txtISO8859_1 : begin
        pldCOM.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
        iSize:=(FStreamStart+FLength)-Stream.Position;
        pldCOM.Text:=Core.Streams.Extract(Stream,Stream.Position,iSize,soClean);
      end;
      txtUTF16 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldCOM.Description:=UTF8Encode(sUTF16);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16,soClean);
        pldCOM.Description:=UTF8Encode(sUTF16);
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldCOM.Description:=UTF8Encode(sUTF16BE);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16BE,soClean);
        pldCOM.Description:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldCOM.Description:=UTF8Encode(sUTF8);
        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF8,soClean);
        pldCOM.Description:=UTF8Encode(sUTF8);
      end;
    end;
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_CRA;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fAudioEncryption;
    FData:=@pldCRA;
    FLength:=Stream.Position;
    pldCRA.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    Stream.Read(pldCRA.PreviewStart,2);
    Stream.Read(pldCRA.PreviewLength,2);
    FLength:=FReader.FTagFrame.Header.FLength-(Stream.Position-FLength);
    SetLength(pldCRA.Data,FLength);
    if FLength>0 then
      Stream.Read(pldCRA.Data[0],FLength);
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_CRM;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fEncryptedMeta;
    FData:=@pldCRM;
    FLength:=Stream.Position;
    pldCRM.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    pldCRM.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
    SetLength(pldCRM.Data,FLength);
    if FLength>0 then
      Stream.Read(pldCRM.Data[0],FLength);
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_C_N_2;
  begin
    case Header.FID[2] of
      'T': Process_CNT;
    end;
  end;

  procedure Process_C_O_2;
  begin
    case Header.FID[2] of
      'M': Process_COM;
    end;
  end;

  procedure Process_C_R_2;
  begin
    case Header.FID[2] of
      'A': Process_CRA;
      'M': Process_CRM;
    end;
  end;

  procedure Process_C_1;
  begin
    case Header.FID[1] of
      'N': Process_C_N_2;
      'O': Process_C_O_2;
      'R': Process_C_R_2;
    end;
  end;

  procedure Process_ETC;
  var
    iDX:LongInt;
    iRead,iCount:Cardinal;
    iStop:qword;
  begin
    FData:=@pldETC;
    FReader.FTagFrame.FKind:=TFrameKind.fEventTimingCodes;
    FReader.FStreamPosition:=Stream.Position;
    FLength:=FReader.FTagFrame.Header.FLength;
    iStop:=FReader.FStreamPosition+FLength;
    Stream.Read(pldETC.Format,1);
    FReader.FStreamPosition+=1;
    iCount:=0; iDX:=-1; iRead:=SizeOf(TEventItem);
    repeat
      iDX+=1;
      iCount+=1;
      System.SetLength(pldETC.Events,iCount);
      Stream.Read(pldETC.Events[iDX],iRead);
      FReader.FStreamPosition+=iRead;
    until (pldETC.Events[iDX].Kind=tskOneByteOfEventsFollows) or (Stream.Position>=iStop);
    if pldETC.Events[iDX].Kind=tskOneByteOfEventsFollows then begin
      Stream.Read(iRead,1);
      FReader.FStreamPosition+=1;
    end;
    Handled:=true;
  end;

  procedure Process_EQU;
  var
    iLcv:Cardinal;
    iCount:Cardinal;
    iBytes:Byte;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fEqualization;
    FData:=@pldEQU;
    Stream.Read(pldEQU.AdjustmentBits,1);
    FLength:=FReader.FTagFrame.FLength-1;
    iBytes:=BitsToBytes(pldEQU.AdjustmentBits);
    iCount:=FLength div (2+iBytes);
    SetLength(pldEQU.Bands,iCount);
    for iLcv:=0 to iCount do begin
      with pldEQU.Bands[iLcv] do begin
        SetLength(Adjustment,iBytes);
        Stream.Read(Frequency,2);
        Stream.Read(Adjustment[0],iBytes);
        Direction:=TVolumeAdjustment(((Frequency and (1 shl 15)) shr 15));
        Frequency:=Frequency and not (1 shl 15);
      end;
    end;
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_E_T_2;
  begin
    case Header.FID[2] of
      'C': Process_ETC;
    end;
  end;

  procedure Process_E_Q_2;
  begin
    case Header.FID[2] of
      'U': Process_EQU;
    end;
  end;

  procedure Process_E_1;
  begin
    case Header.FID[1] of
      'T': Process_E_T_2;
      'Q': Process_E_Q_2;
    end;
  end;

  procedure Process_GEO;
  var
    sUTF16:widestring;
    SUTF16BE:widestring;
    sUTF8:widestring;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fGeneralEncapsulatedObject;
    FData:=@pldGEO;
    FLength:=Stream.Position;
    Stream.Read(pldGEO.Encoding,1);
    case pldGEO.Encoding of
      txtISO8859_1 : begin
        pldGEO.MimeType:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
        pldGEO.FileName:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
        pldGEO.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
      end;
      txtUTF16 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldGEO.MimeType:=UTF8Encode(sUTF16);

        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldGEO.FileName:=UTF8Encode(sUTF16);

        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldGEO.Description:=UTF8Encode(sUTF16);
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldGEO.MimeType:=UTF8Encode(sUTF16BE);

        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldGEO.FileName:=UTF8Encode(sUTF16BE);

        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldGEO.Description:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldGEO.MimeType:=UTF8Encode(sUTF8);

        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldGEO.FileName:=UTF8Encode(sUTF8);

        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldGEO.Description:=UTF8Encode(sUTF8);
      end;
    end;
    FLength:=FReader.FTagFrame.FLength-(Stream.Position-FLength);
    System.SetLength(pldGEO.Data,FLength);
    if FLength>0 then
      Stream.Read(pldGEO.Data[0],FLength);
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_G_E_2;
  begin
    case Header.FID[2] of
      'O': Process_GEO;
    end;
  end;

  procedure Process_G_1;
  begin
    case Header.FID[1] of
      'E': Process_G_E_2;
    end;
  end;

  procedure Process_IPL;
  var
    sUTF16:widestring;
    SUTF16BE:widestring;
    sUTF8:widestring;
  begin
    Empty(pldIPL.List);
    FReader.FTagFrame.FKind:=TFrameKind.fInvolvedPeople;
    FData:=@pldIPL;
    Stream.Read(pldIPL.Encoding,1);

    case pldIPL.Encoding of
      txtISO8859_1 : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPL.List,FReader.FRefactor,Core.Streams.Types.seNormal);
      txtUTF16     : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPL.List,FReader.FRefactor,Core.Streams.Types.seUTF16);
      txtUTF16BE   : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPL.List,FReader.FRefactor,Core.Streams.Types.seUTF16BE);
      txtUTF8      : Core.Streams.Extract(Stream,Stream.Position,Stream.Position+(FReader.FTagFrame.FLength-1),0,pldIPL.List,FReader.FRefactor,Core.Streams.Types.seUTF8);
    end;
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_I_P_2;
  begin
    case Header.FID[2] of
      'L': Process_IPL;
    end;
  end;

  procedure Process_I_1;
  begin
    case Header.FID[1] of
      'P': Process_I_P_2;
    end;
  end;

  procedure Process_LNK;
  begin
    FData:=@pldLNK;
    FReader.FTagFrame.FKind:=TFrameKind.fLinkedInfo;
    Stream.Read(pldLNK.FrameID,3);
    pldLNK.URL:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    FLength:=FReader.FTagFrame.FLength-4;
    Core.Streams.Extract(Stream,Stream.Position,Stream.Position+FLength,0,pldLNK.List,FReader.FRefactor);
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_L_N_2;
  begin
    case Header.FID[2] of
      'K': Process_LNK;
    end;
  end;

  procedure Process_L_1;
  begin
    case Header.FID[1] of
      'N': Process_L_N_2;
    end;
  end;

  procedure Process_MCI;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fMusicCDIdentifier;
    FData:=@pldMCI;
    FLength:=FReader.FTagFrame.Header.FLength;
    SetLength(pldMCI.TOC,FLength);
    if FLength>0 then
      Stream.Read(pldMCI.TOC[0],FLength);
    Handled:=true;
  end;

  procedure Process_M_C_2;
  begin
    case Header.FID[2] of
      'I': Process_MCI;
    end;
  end;

  procedure Process_MLL;
  begin
    FData:=@pldMLL;
    FReader.FTagFrame.FKind:=TFrameKind.fMPEGtable;
    Stream.Read(pldMLL,SizeOf(pldMLL));
    FLength:=FReader.FTagFrame.Header.FLength;
    Handled:=true;
  end;

  procedure Process_M_L_2;
  begin
    case Header.FID[2] of
      'L': Process_MLL;
    end;
  end;

  procedure Process_M_1;
  begin
    case Header.FID[1] of
      'C': Process_M_C_2;
      'L': Process_M_L_2;
    end;
  end;

  procedure Process_PIC;
  var
    iSize:LongInt;
    sUTF8:Core.Strings.VarString;
    sUTF16:Core.Strings.Wide;
    sUTF16BE:Core.Strings.Wide;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fPicture;
    FData:=@pldPIC;
    FStreamStart:=Stream.Position;

    Stream.Read(pldPIC.Encoding,1);
    Stream.Read(pldPIC.ImageFormat,3);
    Stream.Read(pldPIC.ImageKind,1);

    case pldPIC.Encoding of
      txtISO8859_1 : begin
        pldPIC.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
      end;
      txtUTF16 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldPIC.Description:=UTF8Encode(sUTF16);
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldPIC.Description:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldPIC.Description:=UTF8Encode(sUTF8);
      end;
    end;

    iSize:=FLength - (Stream.Position-FStreamStart);
    if iSize>0 then begin
      System.SetLength(pldPIC.Data,iSize);
      Stream.Read(pldPIC.Data[0],iSize);
    end else
      System.SetLength(pldPIC.Data,0);
    Handled:=true;
  end;

  procedure Process_P_I_2;
  begin
    case Header.FID[2] of
      'C': Process_PIC;
    end;
  end;

  procedure Process_POP;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fPopularimeter;
    FData:=@pldPOP;
    pldPOP.Email:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    Stream.Read(pldPOP.Rating,1);
    FLength:=FReader.FTagFrame.FLength-System.Length(pldPOP.Email)-2;
    SetLength(pldPOP.Counter,FLength);
    if FLength>0 then
      Stream.Read(pldPOP.Counter[0],FLength);
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_P_O_2;
  begin
    case Header.FID[2] of
      'P': Process_POP;
    end;
  end;

  procedure Process_P_1;
  begin
    case Header.FID[1] of
      'I': Process_P_I_2;
      'O': Process_P_O_2;
    end;
  end;

  procedure Process_REV;
  begin
    FData:=@pldREV;
    FReader.FTagFrame.FKind:=TFrameKind.fReverb;
    Stream.Read(pldREV,SizeOf(pldREV));
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_R_E_2;
  begin
    case Header.FID[2] of
      'V': Process_REV;
    end;
  end;

  procedure Process_RVA;
  var
    iBytes:Byte;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fVolumeAdj;
    FData:=@pldRVA;
    Stream.Read(pldRVA.Adjustment,1);
    Stream.Read(pldRVA.Description,1);
    iBytes:=BitsToBytes(pldRVA.Description);
    SetLength(pldRVA.DeltaRight,iBytes);
    SetLength(pldRVA.DeltaLeft,iBytes);
    SetLength(pldRVA.PeakVolumeRight,iBytes);
    SetLength(pldRVA.PeakVolumeLeft,iBytes);
    Stream.Read(pldRVA.DeltaRight,iBytes);
    Stream.Read(pldRVA.DeltaLeft,iBytes);
    Stream.Read(pldRVA.PeakVolumeRight,iBytes);
    Stream.Read(pldRVA.PeakVolumeLeft,iBytes);
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_R_V_2;
  begin
    case Header.FID[2] of
      'A': Process_RVA;
    end;
  end;

  procedure Process_R_1;
  begin
    case Header.FID[1] of
      'E': Process_R_E_2;
      'V': Process_R_V_2;
    end;
  end;

  procedure Process_SLT;
  var
    sUTF8:Core.Strings.VarString;
    sUTF16:Core.Strings.Wide;
    sUTF16BE:Core.Strings.Wide;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fSyncedText;
    FData:=@pldSLT;
    FStreamStart:=Stream.Position;
    Stream.Read(pldSLT.Encoding,1);
    Stream.Read(pldSLT.Language,3);
    Stream.Read(pldSLT.Format,1);
    Stream.Read(pldSLT.ContentType,1);

    case pldSLT.Encoding of
      txtISO8859_1 : begin
        pldSLT.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
      end;
      txtUTF16 : begin
       Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
       pldSLT.Description:=UTF8Encode(sUTF16);
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldSLT.Description:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldSLT.Description:=UTF8Encode(sUTF8);
      end;
    end;
    // TO DO Missing data
    Stream.Position:=FStreamStart+FReader.FTagFrame.FLength;
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_S_L_2;
  begin
    case Header.FID[2] of
      'T': Process_SLT;
    end;
  end;

  procedure Process_STC;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fSyncedTempo;
    FData:=@pldSTC;
    Stream.Read(pldSTC.Format,1);
    FLength:=FReader.FTagFrame.FLength-1;
    SetLength(pldSTC.Data,FLength);
    if FLength>0 then
      Stream.Read(pldSTC.Data[0],FLength);
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_S_T_2;
  begin
    case Header.FID[2] of
      'C': Process_STC;
    end;
  end;

  procedure Process_S_1;
  begin
    case Header.FID[1] of
      'L': Process_S_L_2;
      'T': Process_S_T_2;
    end;
  end;

  procedure Process_TXT;
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

  procedure Process_TXX;
  var
    iSize:LongInt;
    sUTF8:Core.Strings.VarString;
    sUTF16:Core.Strings.Wide;
    sUTF16BE:Core.Strings.Wide;
  begin
    FReader.FTagFrame.FKind:=fUserDefinedTextInformation;
    FData:=@pldTXX;
    FStreamStart:=Stream.Position;
    Stream.Read(pldTXX.Encoding,1);

    case pldTXX.Encoding of
      txtISO8859_1 : begin
        pldTXX.Description:=UTF8Encode(Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor));
        iSize:=(FStreamStart+FLength)-Stream.Position;
        pldTXX.Value:=UTF8Encode(Core.Streams.Extract(Stream,Stream.Position,iSize,soClean));
      end;
      txtUTF16 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldTXX.Description:=UTF8Encode(sUTF16);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16,soClean);
        pldTXX.Value:=UTF8Encode(sUTF16);
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldTXX.Description:=UTF8Encode(sUTF16BE);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16,soClean);
        pldTXX.Value:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldTXX.Description:=UTF8Encode(sUTF8);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF8,soClean);
        pldTXX.Value:=UTF8Encode(sUTF8);
      end;
    end;



    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_T_A_2;
  begin
    case Header.FID[2] of
      'L': begin
             Process_TXT;
             FReader.FTagFrame.FKind:=fTitle;
           end;
    end;
  end;

  procedure Process_T_B_2;
  begin
    case Header.FID[2] of
      'P': begin
             Process_TXT;
             FReader.FTagFrame.FKind:=fBeatsPerMinute;
           end;
    end;
  end;

  procedure Process_T_C_2;
  begin
    case Header.FID[2] of
      'M': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fComposer;
      end;
      'O': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fContentType;
      end;
      'R': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fCopyrightMessage;
      end;
      'P': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fCompilation;
      end;
    end;
  end;

  procedure Process_T_D_2;
  begin
    case Header.FID[2] of
      'A': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fDate;
      end;
      'Y': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fPlaylistDelay;
      end;
    end;
  end;

  procedure Process_T_E_2;
  begin
    case Header.FID[2] of
      'N': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fEncodedBy;
      end;
    end;
  end;

  procedure Process_T_F_2;
  begin
    case Header.FID[2] of
      'T': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fFileType;
      end;
    end;
  end;

  procedure Process_T_I_2;
  begin
    case Header.FID[2] of
      'M': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fTime;
      end;
      'T': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fTitle;
      end;

    end;
  end;

  procedure Process_T_K_2;
  begin
    case Header.FID[2] of
      'E': Process_TXT;
    end;
  end;

  procedure Process_T_L_2;
  begin
    case Header.FID[2] of
      'A': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fLanguages;
      end;
      'E': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fLength;
      end;
    end;
  end;

  procedure Process_T_M_2;
  begin
    case Header.FID[2] of
      'T': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fMediaType;
      end;
    end;
  end;

  procedure Process_T_O_2;
  begin
    case Header.FID[2] of
      'A': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fOriginalArtist;
      end;
      'F': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fOriginalfilename;
      end;
      'L': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fOriginalWriter;
      end;
      'R': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fOriginalReleaseYear;
      end;
      'T': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fOriginalTitle;
      end;
    end;
  end;

  procedure Process_T_P_2;
  begin
    case Header.FID[2] of
      '1': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fLeadArtist;
      end;
      '2': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fAccompaniment;
      end;
      '3': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fPerformerRefinement;
      end;
      '4': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fModifiedBy;
      end;
      'A': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fPartOfaSet;
      end;
      'B': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fPublisher;
      end;
    end;
  end;

  procedure Process_T_R_2;
  begin
    case Header.FID[2] of
      'C': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fISRC;
      end;
      'D': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fRecordingDates;
      end;
      'K': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fTrackNumber;
      end;
    end;
  end;

  procedure Process_T_S_2;
  begin
    case Header.FID[2] of
      'I': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fSize;
      end;
      'S': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fEncodingParams;
      end;
    end;
  end;

  procedure Process_T_T_2;
  begin
    case Header.FID[2] of
      '1': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fContentGroupDescription;
      end;
      '2': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fTitleDescription;
      end;
      '3': begin
        Process_TXT;
        FReader.FTagFrame.FKind:=TFrameKind.fSubTitleDescription;
      end;
    end;
  end;

  procedure Process_T_X_2;
  begin
    case Header.FID[2] of
      'T': Process_TXT;
      'X': Process_TXX;
    end;
  end;

  procedure Process_T_Y_2;
  begin
    case Header.FID[2] of
      'E': begin
            Process_TXT;
            FReader.FTagFrame.FKind:=fYear;
           end;
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
      'I': Process_T_I_2;
      'K': Process_T_K_2;
      'L': Process_T_L_2;
      'M': Process_T_M_2;
      'O': Process_T_O_2;
      'P': Process_T_P_2;
      'R': Process_T_R_2;
      'S': Process_T_S_2;
      'T': Process_T_T_2;
      'X': Process_T_X_2;
      'Y': Process_T_Y_2;
    end;
  end;

  procedure Process_UFI;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fUniqueFileID;
    FData:=@pldUFI;
    pldUFI.Owner:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
    FLength:=FReader.FTagFrame.FLength-System.Length(pldUFI.Owner)+1;
    System.SetLength(pldUFI.Identifier,FLength);
    if FLength>0 then
      Stream.Read(pldUFI.Identifier[0],FLength);
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_ULT;
  var
    iSize:LongInt;
    sUTF8:Core.Strings.VarString;
    sUTF16:Core.Strings.Wide;
    sUTF16BE:Core.Strings.Wide;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fUnSynchronizedTranscription;
    FData:=@pldULT;
    FStreamStart:=Stream.Position;

    Stream.Read(pldULT.Encoding,1);
    Stream.Read(pldULT.Language,3);

    case pldULT.Encoding of
      txtISO8859_1 : begin
        pldULT.Description:=Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor);
        iSize:=(FStreamStart+FLength)-Stream.Position;
        pldULT.Text:=Core.Streams.Extract(Stream,Stream.Position,iSize,soClean);
      end;
      txtUTF16 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldULT.Description:=UTF8Encode(sUTF16);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16,soClean);
        pldULT.Text:=UTF8Encode(sUTF16);//=sUTF16;
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldULT.Description:=UTF8Encode(sUTF16BE);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16,soClean);
        pldULT.Text:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldULT.Description:=UTF8Encode(sUTF8);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF8,soClean);
        pldULT.Text:=UTF8Encode(sUTF8);
      end;
    end;
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_U_F_2;
  begin
    case Header.FID[2] of
      'I' : Process_UFI;
    end;
  end;

  procedure Process_U_L_2;
  begin
    case Header.FID[2] of
      'T' : Process_ULT;
    end;
  end;

  procedure Process_U_1;
  begin
    case Header.FID[1] of
      'F' : Process_U_F_2;
      'L' : Process_U_L_2;
    end;
  end;

  procedure Process_URL;
  begin
    FReader.FTagFrame.FKind:=fURL;
    FData:=@pldURL;
    pldURL.URL:=Core.Streams.Extract(Stream,Stream.Position,FReader.FTagFrame.FLength,soClean);
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_WAF;
  begin
    Process_URL;
    FReader.FTagFrame.FKind:=TFrameKind.fOfficialFileWebpage;
  end;

  procedure Process_WAR;
  begin
    Process_URL;
    FReader.FTagFrame.FKind:=TFrameKind.fOfficialArtistWebpage;
  end;

  procedure Process_WAS;
  begin
    Process_URL;
    FReader.FTagFrame.FKind:=TFrameKind.fOfficialAudioSourceWebpage;
  end;

  procedure Process_WCM;
  begin
    Process_URL;
    FReader.FTagFrame.FKind:=TFrameKind.fCommericalInfo;
  end;

  procedure Process_WCP;
  begin
    Process_URL;
    FReader.FTagFrame.FKind:=TFrameKind.fCopyrightInfo;
  end;

  procedure Process_WPB;
  begin
    Process_URL;
    FReader.FTagFrame.FKind:=TFrameKind.fPublishersOfficialWebpage;
  end;

  procedure Process_W_A_2;
  begin
    Case Header.FID[2] of
      'F' : Process_WAF;
      'R' : Process_WAR;
      'S' : Process_WAS;
    end;
  end;


  procedure Process_W_C_2;
  begin
    Case Header.FID[2] of
      'M' : Process_WCM;
      'P' : Process_WCP;
    end;
  end;


  procedure Process_W_P_2;
  begin
    Case Header.FID[2] of
      'B' : Process_WPB;
    end;
  end;

  procedure Process_WXX;
  var
    iSize:LongInt;
    sUTF8:Core.Strings.VarString;
    sUTF16:Core.Strings.Wide;
    sUTF16BE:Core.Strings.Wide;
  begin
    FReader.FTagFrame.FKind:=TFrameKind.fUserDefinedURL;
    FData:=@pldWXX;
    Stream.Read(pldWXX.Encoding,1);

    case pldWXX.Encoding of
      txtISO8859_1 : begin
        pldWXX.Description:=UTF8Encode(Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor));
        iSize:=(FStreamStart+FLength)-Stream.Position;
        pldWXX.URL:=UTF8Encode(Core.Streams.Extract(Stream,Stream.Position,iSize,soClean));
      end;
      txtUTF16 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16);
        pldWXX.Description:=UTF8Encode(sUTF16);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16,soClean);
        pldWXX.URL:=UTF8Encode(sUTF16);
      end;
      txtUTF16BE: begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF16BE);
        pldWXX.Description:=UTF8Encode(sUTF16BE);

        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF16BE,soClean);
        pldWXX.URL:=UTF8Encode(sUTF16BE);
      end;
      txtUTF8 : begin
        Core.Streams.Extract(Stream,Stream.Position,0,FReader.FRefactor,sUTF8);
        pldWXX.Description:=UTF8Encode(sUTF8);
        iSize:=(FStreamStart+FLength)-Stream.Position;
        Core.Streams.Extract(Stream,Stream.Position,iSize,sUTF8,soClean);
        pldWXX.URL:=UTF8Encode(sUTF8);
      end;
    end;
    FLength:=FReader.FTagFrame.FLength;
    Handled:=true;
  end;

  procedure Process_W_X_2;
  begin
    Case Header.FID[2] of
      'X' : Process_WXX;
    end;
  end;

  procedure Process_W_1;
  begin
    Case Header.FID[1] of
      'A' : Process_W_A_2;
      'C' : Process_W_C_2;
      'P' : Process_W_P_2;
      'X' : Process_W_X_2;
    end;
  end;

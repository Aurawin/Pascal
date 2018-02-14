EntryPoint:='TIMAPManager.ProcessMessage(RSRP)';
// We're in exclusive Message Data mode.
FMessageP:=IMAPP^.MessageP;
if FMessageP<>nil then begin
  if (RSRP^.RecvBuffer.posWrite>=FMessageP^.Size) then begin
    Try
      RSR.Extract(RSRP^.RecvBuffer,RSRP^.RecvBuffer.posRead,FMessageP^.Size,FMessageP^.Message);
      RSR.Refactor(RSRP^.RecvBuffer,Refactor,RSRP^.RecvBuffer.posWrite+FMessageP^.Size);

      Core.Arrays.VarString.fromStream(FLines,FMessageP^.Message);
      Try
        // Save Message with Flags
        Storage.UserStorage.Items.SMTP.Empty(FSummary);
        Try
          Storage.UserStorage.Items.SMTP.Update(
            FSummary,
            0,                                                                        // Filter ID
            ( (FMessageP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen)=FMessageP^.Flags),  // Read
            (FMessageP^.Folder^.ID=IMAPP^.UAP^.SentBox),                        // Sent
            (FMessageP^.Folder^.ID=IMAPP^.UAP^.SpamBox),                        // Spam
            false,                                                                    // BlackListed
            false                                                                     // WhiteListed
          );
          FFolderID:=FMessageP^.Folder^.ID;
          FFileID:=0;
          if Storage.UserStorage.Items.SMTP.Write(
            Task,IMAPP^.UAP^.AuraNode,
            IMAPP^.UAP^.SpamBox,
            IMAPP^.UAP^.ID,
            IMAPP^.UAP^.DomainID,
            FSummary,
            FLines,
            FMessageP^.Flags,
            Refactor,
            Storage.UserStorage.Items.SMTP.BypassFilters,
            FFolderID,
            FFileID
          ) then begin
            FResponse:=Respond(FSEQ,SSC_OK,Concat('[APPENDUID ',IntToStr(FMessageP^.Folder^.ID),' ',IntToStr(FSummary.ID),'] APPEND completed.'));
            Send(RSRP,FResponse);
          end else begin
            FResponse:=Respond(FSEQ,SSC_NO,'APPEND failed due to a DBMS failure.');
            Send(RSRP,FResponse);
          end;
        finally
          Storage.UserStorage.Items.SMTP.Empty(FSummary);
        end;
      finally
        Core.Arrays.VarString.Empty(FLines);
      end;
    finally
      ReleaseMessage(IMAPP^.MessageP);
    end;
    IMAPP^.State:=IMAPP^.State and not RS_MESSAGE_DATA;
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,'APPEND failed due to a null pointer of your message.');
  Send(RSRP,FResponse);
end;

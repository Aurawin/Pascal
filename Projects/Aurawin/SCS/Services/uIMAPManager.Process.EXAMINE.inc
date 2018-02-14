if TransState()=true then begin
  If (IMAPP^.Examine<>nil) then begin
    Storage.UserStorage.Files.Empty(IMAPP^.Examine^.Files);
    IMAPP^.Examine:=nil;
  end;
  FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP1);
  IMAPP^.Examine:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
  if (IMAPP^.Examine=nil) then begin
    ReloadFolders(FPath,ifrkExamine);
    IMAPP^.Examine:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
  end;
  If (IMAPP^.Examine<>nil) then begin
    FFolderP:=IMAPP^.Examine;
    Storage.UserStorage.Files.Empty(fFolderP^.Files);
    if Storage.UserStorage.Files.DB.List(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID,FFolderP^.Files) then begin
      FUNSEENID:=0;
      FRecent:=0;
      FUnseen:=0;
      FNextID:=0;
      for iLcv:=0 to High(fFolderP^.Files) do begin
        FFileP:=fFolderP^.Files[iLcv];
        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) then
          Inc(FRecent);
        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags) then begin
          Inc(FUnseen);
          FUNSEENID:=FFileP^.ID;
        end;
        if FFileP^.ID>FNextID then
          FNextID:=FFileP^.ID;
      end;
      FResponse:=Respond(Concat(IntToStr(Length(IMAPP^.Selected^.Files)),' ',SELECT_RESP_EXISTS));
      PushSend();
      FResponse:=Respond(Concat(IntToStr(FRecent),' ',SELECT_RESP_RECENT));
      PushSend();
      if FUNSEENID<>0 then
        FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UNSEEN,' ',IntToStr(FUnseen),'] Message ',IntToStr(FUNSEENID),' is first unseen'))
      else
        FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UNSEEN,' ',IntToStr(FUnseen),'] Messages'));
      PushSend();

      FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UIDVALIDITY,' ',IntToStr(fFolderP^.ID),'] IDs valid'));
      PushSend();

      FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UIDNEXT,' ',IntToStr(FNextID),'] next estimated UID'));
      PushSend();

      FResponse:=Respond(
        Concat(
          SELECT_RESP_FLAGS,' ',
          FLAG_ANSWERED,' ',
          FLAG_FLAGGED,' ',
          FLAG_DELETED,' ',
          FLAG_SEEN,' ',
          FLAG_DRAFT,' '
        )
      );
      PushSend();

      FResponse:=Respond(
        SEQ_ANY,
        SSC_OK,
        Concat('[',SELECT_RESP_PERMANENTFLAGS,' ()] No permanent flags permitted')
      );
      PushSend();

      FResponse:=Respond(FSEQ,SSC_OK,'[[READ-ONLY] EXAMINE completed.');
      PushSend();

      FFolderP^.Inspected:=Core.Timer.dtUT;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('EXAMINE was not able to retreive files for ',FPath));
      PushSend();
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('EXAMINE failed: ',FPath,' does not exist.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('EXAMINE failed: you must first login.'));
  PushSend();
end;

if IMAPP^.Selected<>nil then begin
  FFolderP:=IMAPP^.Selected;
  if irkRange in FRangeKinds then begin
    For iLcv:=0 to High(FFolderP^.Files) do begin
      FFileP:=FFolderP^.Files[iLcv];
      if ( (FFileP^.ID<=FRangeEnd) and (FFileP^.ID>=FRangeStart)) then begin
        FFileP^.Flags:=FFileP^.Flags and not FStoreFlags;
        if  FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent = FFileP^.Flags then
          FFileP^.Flags:=FFileP^.Flags and not Storage.UserStorage.Items.IMAP.Flags.Recent;

        Storage.UserStorage.Files.DB.SetFlags(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FFileP^.Flags);
        FResponse:=Respond(Concat(IntToStr(iLcv+1),' FETCH (',Storage.UserStorage.Items.IMAP.toString(FFileP^.Flags),')'));
        Send(RSRP,FResponse);
      end;
    end;
  end;
  if irkSet in FRangeKinds then begin
    For iLcv:=0 to High(FFolderP^.Files) do begin
      FFileP:=FFolderP^.Files[iLcv];
      if (Core.Arrays.LargeWord.IndexOf(FFileP^.ID,FUIDSet)<>-1) then begin
        FFileP^.Flags:=FFileP^.Flags and not FStoreFlags;
        if  FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent = FFileP^.Flags then
          FFileP^.Flags:=FFileP^.Flags and not Storage.UserStorage.Items.IMAP.Flags.Recent;

        Storage.UserStorage.Files.DB.SetFlags(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FFileP^.Flags);
        FResponse:=Respond(Concat(IntToStr(iLcv+1),' FETCH (',Storage.UserStorage.Items.IMAP.toString(FFileP^.Flags),')'));
        Send(RSRP,FResponse);
      end;
    end;
  end;
  FResponse:=Respond(FSEQ,SSC_OK,Concat('STORE completed.'));
  Send(RSRP,FResponse);
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('STORE failed: no folder is selected.'));
  Send(RSRP,FResponse);
end;


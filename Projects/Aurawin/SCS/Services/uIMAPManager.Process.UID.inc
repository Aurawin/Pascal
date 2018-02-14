if TransState()=true then begin
  If (IMAPP^.Selected<>nil) then begin
    FFolderP:=IMAPP^.Selected;
    if Core.Utils.Time.DifferInMilliseconds(Core.Timer.dtUT,FFolderP^.Inspected)>App.Consts.FOLDER_MS_REFRESH then begin
      Storage.UserStorage.Files.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID,FFolderP^.Files,Storage.UserStorage.Files.DB.IDs.ID);
      FFolderP^.Inspected:=Core.Timer.dtUT;
    end;
    if Core.Strings.SameText(CMDP1,RSR.IMAP.Command.Fetch) then begin
      ID_Fetch(RSRP);
    end else if Core.Strings.SameText(CMDP1,RSR.IMAP.Command.Store) then begin
      ID_Store(RSRP);
    end else if Core.Strings.SameText(CMDP1,RSR.IMAP.Command.Copy) then begin
      ID_Copy(RSRP);
    end else if Core.Strings.SameText(CMDP1,RSR.IMAP.Command.Search) then begin
      ID_Search(RSRP);
    end else if Core.Strings.SameText(CMDP1,RSR.IMAP.Command.Expunge) then begin
      ID_Expunge(RSRP);
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('UID failed: "',CMDP1,'" not yet implemented.'));
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('UID failed: no folder was selected.'));
    Send(RSRP,FResponse);
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('UID failed: you must first login.'));
  Send(RSRP,FResponse);
end;

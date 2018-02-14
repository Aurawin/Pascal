if TransState()=true then begin
  If (IMAPP^.Selected<>nil) then begin
    FFolderP:=IMAPP^.Selected;
    if Expunge(RSRP,FFolderP^,STATUS_TAGS_ON) then begin
      FResponse:=Respond(FSEQ,SSC_OK,'EXPUNGE completed.');
      PushSend();
      FFolderP^.Inspected:=Core.Timer.dtUT;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('EXPUNGE was not able to retrieve files for ',FFolderP^.Path));
      PushSend();
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('EXPUNGE failed: no folder was selected.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('EXPUNGE failed: you must first login.'));
  PushSend();
end;

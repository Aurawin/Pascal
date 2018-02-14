if TransState()=true then begin
  If (IMAPP^.Selected<>nil) then begin
    FFolderP:=IMAPP^.Selected;
    if Expunge(RSRP,FFolderP^,STATUS_TAGS_OFF) then begin
      IMAPP^.Selected:=nil;
      IMAPP^.State:=IMAPP^.State and not RS_SELECTED;
      FResponse:=Respond(FSEQ,SSC_OK,'CLOSE completed.');
      PushSend();
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('CLOSE was not able to retrieve files for ',FFolderP^.Path));
      PushSend();
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('CLOSE failed: no folder was selected.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('CLOSE failed: you must first login.'));
  PushSend();
end;

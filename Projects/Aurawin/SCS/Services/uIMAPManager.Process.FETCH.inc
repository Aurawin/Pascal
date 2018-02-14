if TransState()=true then begin
  If (IMAPP^.Selected<>nil) and (Length(FFetch)>0) then begin
    FFolderP:=IMAPP^.Selected;
    SetRange();
    if (FFetch[0]='ALL') then begin
      {$i uIMAPManager.Process.FETCH.ALL.inc}
    end else if (FFetch[0]='FAST') then begin
       {$i uIMAPManager.Process.FETCH.FAST.inc}
    end else if (FFetch[0]='FULL') then begin
       {$i uIMAPManager.Process.FETCH.FULL.inc}
    end else if (FFetch[0]='BODY') then begin
       {$i uIMAPManager.Process.FETCH.BODY.inc}
    end else if (FFetch[0]='BODY.PEEK') then begin
       {$i uIMAPManager.Process.FETCH.BODY.PEEK.inc}
    end else if (FFetch[0]='BODYSTRUCTURE') then begin
       {$i uIMAPManager.Process.FETCH.BODYSTRUCTURE.inc}
    end else if (FFetch[0]='ENVELOPE') then begin
       {$i uIMAPManager.Process.FETCH.ENVELOPE.inc}
    end else if (FFetch[0]='FLAGS') then begin
       {$i uIMAPManager.Process.FETCH.FLAGS.inc}
    end else if (FFetch[0]='INTERNALDATE') then begin
       {$i uIMAPManager.Process.FETCH.INTERNALDATE.inc}
    end else if (FFetch[0]='RFC822') then begin
       {$i uIMAPManager.Process.FETCH.RFC822.inc}
    end else if (FFetch[0]='RFC822.HEADER') then begin
       {$i uIMAPManager.Process.FETCH.RFC822.HEADER.inc}
    end else if (FFetch[0]='RFC822.SIZE') then begin
       {$i uIMAPManager.Process.FETCH.RFC822.SIZE.inc}
    end else if (FFetch[0]='RFC822.TEXT') then begin
       {$i uIMAPManager.Process.FETCH.RFC822.TEXT.inc}
    end else if (FFetch[0]='UID') then begin
       {$i uIMAPManager.Process.FETCH.RFC822.TEXT.inc}
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('FETCH failed: no folder was selected.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('FETCH failed: you must first login.'));
  PushSend();
end;

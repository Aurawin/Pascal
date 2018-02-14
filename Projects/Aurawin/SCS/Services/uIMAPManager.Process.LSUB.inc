if TransState()=true then begin
  FPath:=Storage.UserStorage.Folders.Defaults.Home.Mail;
  FPathSkip:=System.Length(Storage.UserStorage.Folders.Defaults.Home.Mail)+1;
  FPathLoop:=FPath;
  if Storage.UserStorage.Folders.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FPath,IMAPP^.Folders) then begin
    for iLcv:=0 to High(IMAPP^.Folders) do begin
      FFolderP:=IMAPP^.Folders[iLcv];
      SetLength(FBoxFlags,0);
      FPath:=FFolderP^.Path;
      FPathLen:=System.Length(FPath);
      FPath:=System.Copy(FPath,FPathSkip+1,FPathLen-FPathSkip);
      FPathLen:=System.Length(FPath);
      if (FPathLen>0) then begin
        if FFolderP^.ID=IMAPP^.UAP^.SentBox then begin
          FBoxFlags:='\Sent';
        end else if FFolderP^.ID=IMAPP^.UAP^.Outbox then begin
          FBoxFlags:='\Drafts';
        end else if FFolderP^.ID=IMAPP^.UAP^.SpamBox then begin
           FBoxFlags:='\Junk';
        end else if FFolderP^.ID=IMAPP^.UAP^.TrashBox then begin
          FBoxFlags:='\Trash';
        end;
        FResponse:=Respond(Concat('LSUB (',FBoxFlags,') "/" ',FPath));
        PushSend();
      end;
    end;
    FResponse:=Respond(FSEQ,SSC_OK,Concat('LSUB completed.'));
    PushSend();
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('LSUB failed: DBMS failure retrieving folders.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('LSUB failed: you must first login.'));
  Send(RSRP,FResponse);
end;

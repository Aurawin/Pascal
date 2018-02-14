if TransState()=true then begin
  //Storage.UserStorage.Folders.Defaults.Mail.Inbox;
  FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP1);
  FFolderP:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
  if (fFolderP=nil) then begin
    ReloadFolders(FPath,ifrkStatus);
    FFolderP:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
  end;
  If (FFolderP<>nil) then begin
    if Storage.UserStorage.Folders.DB.Delete(Task,IMAPP^.UAP^.AuraNode,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID) then begin
      FResponse:=Respond(FSEQ,SSC_OK,'DELETE completed.');
      PushSend();
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('DELETE ',FPath,' was not deleted.'));
      PushSend();
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('DELETE failed: ',FPath,' does not exist.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('DELETE failed: you must first login.'));
  PushSend();
end;

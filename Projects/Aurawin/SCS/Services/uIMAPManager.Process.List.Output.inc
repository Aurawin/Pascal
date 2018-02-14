
FPath:=Storage.UserStorage.Folders.Defaults.Home.Mail;
FPathSkip:=System.Length(Storage.UserStorage.Folders.Defaults.Home.Mail)+1;
if Length(FMailbox)>0 then
  FPathLoop:=Concat(FPath,'/',FMailbox)
else
  FPathLoop:=FPath;


Storage.UserStorage.Folders.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FPath,IMAPP^.Folders);

for iLcv:=0 to High(IMAPP^.Folders) do begin
  SetLength(FBoxFlags,0);
  FFolderP:=IMAPP^.Folders[iLcv];
  FPath:=FFolderP^.Path;
  FPathLen:=System.Length(FPath);
  FPath:=System.Copy(FPath,FPathSkip+1,FPathLen-FPathSkip);
  FPathLen:=System.Length(FPath);
  if (FPathLen>0) then begin
    if (FFolderP^.ID=IMAPP^.UAP^.SentBox) then begin
      FBoxFlags:='\Sent';
    end else if (FFolderP^.ID=IMAPP^.UAP^.Outbox) then begin
      FBoxFlags:='\Drafts';
    end else if (FFolderP^.ID=IMAPP^.UAP^.SpamBox) then begin
      FBoxFlags:='\Junk';
    end else if (FFolderP^.ID=IMAPP^.UAP^.TrashBox) then begin
      FBoxFlags:='\Trash';
    end else if
      (Core.Strings.SameText(FPath,Storage.UserStorage.Folders.Defaults.Mail.Archive)) or
      (Core.Strings.StartsWith(FPath,Storage.UserStorage.Folders.Defaults.Mail.Archive+'/')) then begin
      FBoxFlags:='\Archive';
    end;
    FResponse:=Respond(Concat('LIST (',FBoxFlags,') "/" ',FPath));
    PushSend();
  end;
end;

FResponse:=Respond(FSEQ,SSC_OK,Concat('LIST completed.'));
PushSend();




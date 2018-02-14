function TIMAPManager.Expunge(RSRP:PRSR; var Folder:Storage.UserStorage.Folders.TFolder; StatusMessages:boolean=false):boolean;
var
  iLcv:LongInt;
begin
  EntryPoint:='TIMAPManager.ID_Store(RSRP, Folder, StatusMessages)';

  if Core.Utils.Time.DifferInMilliseconds(Core.Timer.dtUT,Folder.Inspected)>FOLDER_MS_REFRESH then begin
    Storage.UserStorage.Files.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,Folder.ID,Folder.Files,Storage.UserStorage.Files.DB.IDs.ID);
    Folder.Inspected:=Core.Timer.dtUT;
  end;
  for iLcv:=0 to High(Folder.Files) do begin
    FFileP:=Folder.Files[iLcv];
    if FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags then begin
      Storage.UserStorage.Files.DB.Delete(Task,IMAPP^.UAP^.AuraNode,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.FolderID,FFileP^.ID);
      if (StatusMessages=true) then begin
        FResponse:=Respond(Concat(IntToStr(iLcv+1),' EXPUNGE'));
        Send(RSRP,FResponse)
      end;
    end;
  end;

end;

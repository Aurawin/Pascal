if FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags then begin
  Storage.UserStorage.Files.DB.Delete(Task,IMAPP^.UAP^.AuraNode,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.FolderID,FFileP^.ID);
  FResponse:=Respond(Concat(IntToStr(iLcv+1),' EXPUNGE'));
  Send(RSRP,FResponse)
end;


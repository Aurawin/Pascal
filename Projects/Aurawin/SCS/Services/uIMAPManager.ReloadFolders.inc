procedure  TIMAPManager.ReloadFolders(sPath:Core.Strings.VarString; aKind:TIMAPFolderReloadKind);

  procedure Push_Status;
  begin
    if IMAPP^.Selected<>nil then begin
      Try
        FSelected:=IMAPP^.Selected^.Path;
      Except
        SetLength(FSelected,0);
      end;
    end;
    if IMAPP^.Examine<>nil then begin
      Try
        FExamine:=IMAPP^.Examine^.Path;
      except
        SetLengtH(FExamine,0);
      end;
    end;
    FStatus:=sPath;
  end;

  procedure Push_Selected;
  begin
    if IMAPP^.Status<>nil then begin
      Try
        FStatus:=IMAPP^.Status^.Path;
      Except
        SetLength(FStatus,0);
      end;
    end;
    if IMAPP^.Examine<>nil then begin
      try
        FExamine:=IMAPP^.Examine^.Path;
      Except
        SetLength(FExamine,0);
      end;
    end;
    FSelected:=sPath;
    IMAPP^.LastExists:=-1;
    IMAPP^.LastRecent:=-1;
    IMAPP^.LastDeleted:=-1;
  end;

  procedure Push_Examine;
  begin
    if IMAPP^.Status<>nil then begin
      try
        FStatus:=IMAPP^.Status^.Path;
      except
        SetLength(FStatus,0);
      end;
    end;
    if IMAPP^.Selected<>nil then begin
      Try
        FSelected:=IMAPP^.Selected^.Path;
      Except
        SetLength(FSelected,0);
      end;
    end;
    FExamine:=sPath;
  end;

begin
  case aKind of
    ifrkStatus   : Push_Status;
    ifrkSelected : Push_Selected;
    ifrkExamine  : Push_Examine;
  end;
  IMAPP^.Status:=nil;
  IMAPP^.Examine:=nil;
  IMAPP^.Selected:=nil;

  Storage.UserStorage.Folders.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,Storage.UserStorage.Folders.Defaults.Home.Mail,IMAPP^.Folders);

  If Length(FStatus)>0 then
    IMAPP^.Status:=Storage.UserStorage.Folders.getFolder(FStatus,IMAPP^.Folders);
  If Length(FSelected)>0 then
    IMAPP^.Selected:=Storage.UserStorage.Folders.getFolder(FSelected,IMAPP^.Folders);
  If Length(FExamine)>0 then
    IMAPP^.Examine:=Storage.UserStorage.Folders.getFolder(FExamine,IMAPP^.Folders);

end;


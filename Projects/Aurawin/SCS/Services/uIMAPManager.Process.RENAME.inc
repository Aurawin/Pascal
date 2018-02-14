if TransState()=true then begin
  //Storage.UserStorage.Folders.Defaults.Mail.Inbox;
  If SameText(CMDP1,CMDP2)=false then begin
    FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP1);
    FFolderP:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    if (FFolderP=nil) then begin
      ReloadFolders(FPath,ifrkStatus);
      FFolderP:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    end;
    If (FFolderP<>nil) then begin
      FPathNew:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP2);

      (*
      todo list folders and all sub folders from that point.
      Edit path names for each TFolder
      Issue DBMS rename

      Also... Check to see if first element (local root) is already in existance...
      or else it will create 2 folders with the same path.  If it already exists...
      Write  a Merge feature and push all files and folders into that location...
      then delete the originals from the AuraDisk
      *)

      // Make sure all new pivot folders are created too!
      Core.Arrays.VarString.fromString(FFolderPath,FPathNew,'/',[soClearList,soIgnoreDelimAtStart]);
      SetLength(FPathLoop,0);
      for iLcv:=0 to High(FFolderPath) do begin
        FPathLoop:=Concat(FPathLoop,FFolderPath[iLcv]);
        Storage.UserStorage.Folders.DB.Force(Task, IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID, FPathLoop,FFolderID);
        FPathLoop:=Concat(FPathLoop,'/');
      end;
      if Storage.UserStorage.Folders.DB.List(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FPath,FRenameFolders) then begin
        Try
          iLen:=System.Length(FPath);
          for iLcv:=0 to High(FRenameFolders) do begin
            System.Delete(FRenameFolders[iLcv]^.Path,1,iLen);
            System.Insert(FPathNew,FRenameFolders[iLcv]^.Path,1);
            Storage.UserStorage.Folders.DB.Rename(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FRenameFolders[iLcv]^);
          end;
          FResponse:=Respond(FSEQ,SSC_OK,'RENAME completed.');
          PushSend();
        finally
          Storage.UserStorage.Folders.Empty(FRenameFolders,Storage.UserStorage.Folders.FREE_FILES);
        end;
      end else begin
        FResponse:=Respond(FSEQ,SSC_NO,Concat('RENAME ',FPath,' was not renamed due to a DBMS failure.'));
        PushSend();
      end;
      Core.Arrays.VarString.Empty(FFolderPath);
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('RENAME failed: ',FPath,' does not exist.'));
      PushSend();
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('RENAME failed: paths are the same.'));
    PushSend();
  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('RENAME failed: you must first login.'));
  PushSend();
end;

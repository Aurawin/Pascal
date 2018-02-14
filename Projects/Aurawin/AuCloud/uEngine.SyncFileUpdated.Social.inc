procedure   TAuSocketMan.SyncFileUpdated(FileP:Storage.Social.Files.PSFile; Data:TStream);
var
  qItem : Storage.Social.Sync.Queue.TItem;
  sFolder: Core.Strings.VarString;
begin
  qItem:=FSyncQueue.Find(ItemKind.Read,FileP);
  If (qItem<>nil) then begin
    if ( (qItem.PipeP^.Direction or Direction.Download) = qItem.PipeP^.Direction) then begin

      sFolder:=Core.Utils.Files.Extract(qItem.LocalizedFile,epoAllButName);
      if not SysUtils.DirectoryExists(sFolder) then
        SysUtils.ForceDirectories(sFolder);
      Core.Streams.toFile(Data,qItem.LocalizedFile);
      Core.Utils.Files.SetFileDateTime(qItem.LocalizedFile,FileP^.Modified,FromUTCBias);
      GetFileProfile(qItem.LocalizedFile,FileP^.Size,FileP^.Modified);
      qItem.Status:=Storage.Social.Sync.Queue.Status.Operation.soVerified;
    end;
  end else begin
    qItem:=nil;
  end;
end;


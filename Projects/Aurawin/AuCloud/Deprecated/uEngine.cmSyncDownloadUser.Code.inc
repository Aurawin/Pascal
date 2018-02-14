constructor cmSyncDownloadUser.Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.VDM.Sync.TItem; var aPipe:Storage.VDM.Sync.Pipes.TItem);
begin
  FPipeP:=@aPipe;
  FSyncP:=@aSync;
  FRequest:=THTTPRequest.Create();
  inherited Create(aManager,aRSRP);
end;

destructor cmSyncDownloadUser.Destroy();
begin
  FPipeP:=nil;
  FRequest.Free();
  inherited Destroy();
end;

procedure cmSyncDownloadUser.Execute();
var
  Man:TAuSocketMan;
  bSyncModified   : boolean;
  iCurCon         : QWord;

  mfDevices     : Storage.VDM.Sync.Manifest.PMFSTFolder;
  AuDevices     : Storage.UserStorage.Folders.PFolder;
  mfResource    : Storage.VDM.Sync.Manifest.PMFSTFolder;
  AuResource    : Storage.UserStorage.Folders.PFolder;
  mfPipe        : Storage.VDM.Sync.Manifest.PMFSTFolder;
  AuPipe        : Storage.UserStorage.Folders.PFolder;

  dgMD5         : TMD5Digest;

  sLocalFile      : Core.Strings.VarString;
  sFolder         : Core.Strings.VarString;

  StampDiff       : Int64;

  Est           : QWord;
  iMaxSend      : QWord;

  iFileSize     : QWord;
  dtModified    : TDateTime;
  FileP         : Storage.UserStorage.Files.PItem;

  procedure ScanCloudFile(mfstP:Storage.VDM.Sync.Manifest.PMFSTItem);

    procedure PushFileIsTheSame(mfstP:Storage.VDM.Sync.Manifest.PMFSTItem);
    begin
      mfstP^.Size:=iFileSize;
      mfstP^.Modified:=dtModified;
      Storage.UserStorage.Files.Copy(dgMD5,mfstP^.Digest);

      mfstP^.statusDevice:=Storage.VDM.Sync.Manifest.itemStatus.msiVerified;
    end;

    procedure PushFileRead();
    begin
      iCurCon:=Man.getCurrentConsumption();
      iMaxSend:=Man.Estimate+mfstP^.Size;
      if ( (iCurCon<BUFFER_EMPTY_THRESHOLD) or Man.UnderLimit(FRSRP^,iMaxSend)) then begin
        Man.SetSyncStatus(Format(FMT_PROGRESS_USR_STATUS,[auLang.Table.Labels.SyncDownload,sLocalFile]));
        mfstP^.statusDevice:=Storage.VDM.Sync.Manifest.itemStatus.msiRequested;
        Est:=Man.ReadFile(FRSRP^,FRequest,mfstP^.FileP^);
        Man.Estimate:=Man.Estimate+Est;
        if Man.UnderLimit(FRSRP^,Man.Estimate)=false then
          Man.FMode:=emBuffersFull;
      end else
        Man.FMode:=emBuffersFull;
    end;

  begin
    FileP^.Manifest:=mfstP;
    mfstP^.PipeP:=FPipeP;

    sLocalFile:=Concat(sFolder,SysUtils.PathDelim,FileP^.Name);
    if not SysUtils.FileExists(sLocalFile) then begin
      // Local copy does not exist
      PushFileRead();
    end else if GetFileProfile(sLocalFile,iFileSize,dtModified) then begin
      mfstP^.statusDevice:=Storage.VDM.Sync.Manifest.itemStatus.msiVerified;
      // Local Copy is on the disk
      StampDiff:=Core.Utils.Time.DifferInMilliseconds(dtModified,FileP^.Modified);
      if ( (Abs(StampDiff)>Storage.VDM.Sync.Defaults.ThresholdMilliseconds) and (dtModified<FileP^.Modified) and (dtModified<>mfstP^.Modified) )then begin
        // The SERVER File is newer but is content different?
        if GetFileProfile(sLocalFile,dgMD5) then begin
          if (Storage.UserStorage.Files.Compare(dgMD5,FileP^.Digest)=false) then begin
            // the File Contents are different
            PushFileRead();
          end else begin
            PushFileIsTheSame(mfstP);
          end;
        end else begin
          mfstP^.Size:=iFileSize;
          Core.Utils.Files.SetFileDateTime(sLocalFile,mfstP^.Modified,Core.Utils.Time.FromUTCBias);
          GetFileProfile(sLocalFile,mfstP^.Modified);
          bSyncModified:=true;
        end;
      end;
    end;
  end;

  procedure ScanFolder(mfPivot:Storage.VDM.Sync.Manifest.PMFSTFolder);
  var
    iLcv     : LongInt;
    mfstP    : Storage.VDM.Sync.Manifest.PMFSTItem;
    mfLcv    : Storage.VDM.Sync.Manifest.PMFSTFolder;
    auFldr   : Storage.UserStorage.Folders.PFolder;
  begin
    if (Man.FStatus=esStart) and (mfPivot<>nil) and (Man.FMode in [emWorking,emNone]) and Man.UnderLimit(FRSRP^,Man.Estimate) then begin
      if System.Length(mfPivot^.Path)=0 then
        mfPivot^.Path:=Concat(FPipeP^.Path,Storage.VDM.Sync.Manifest.BuildPath(mfPipe,mfPivot^));
      sFolder:=mfPivot^.Path;
      if not SysUtils.DirectoryExists(sFolder) then
        SysUtils.ForceDirectories(sFolder);
      for iLcv:=0 to High(mfPivot^.Items) do begin
        FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.ScanFolder.Pivot';
        if Man.UnderLimit(FRSRP^,Man.Estimate)=false then break;
        mfstP:=mfPivot^.Items[iLcv];
        mfstP^.PipeP:=FPipeP;
        if (mfstP^.FileP=nil) then
          mfstP^.FileP:=Storage.UserStorage.Files.Get(mfstP^.ID,FSyncP^.Files);
        FileP:=mfstP^.FileP;
        if (FileP<>nil) and (FileP^.Allocated>Storage.UserStorage.Files.Allocate_Base) then
          ScanCloudFile(mfstP);
        if (Man.FStatus<>esStart) or (Man.UnderLimit(FRSRP^,Man.Estimate)=false)  then
          break;
      end;
      iLcv:=0;
      while (Man.Terminated=false) and (Man.FStatus=esStart) and (iLcv<System.Length(mfPivot^.Folders)) and (Man.FMode in [emWorking,emNone]) and Man.UnderLimit(FRSRP^,Man.Estimate)  do begin
        FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.ScanFolder.Loop';
        Try
          mfLcv:=mfPivot^.Folders[iLcv];
          if mfLcv^.ID<>0 then begin
            auFldr:=Storage.UserStorage.Folders.getFolder(mfLcv^.ID,FSyncP^.Folders);
            if auFldr=nil then begin // stale referenced cloud folder.  Not present.  Delete.
               Storage.VDM.Sync.Manifest.Delete(mfLcv);
            end else begin
              ScanFolder(mfLcv);
              inc(iLcv);
            end;
          end else
            inc(iLcv);
        except
          on e:exception do begin
            FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.ScanFolder.Loop.Exception';
            Man.SetSyncStatus(FProcessor.EntryPoint);
          end;
        end;
      end;
    end;
  end;

begin
  FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute';
  Man:=TAuSocketMan(FOwner);
  bSyncModified:=false;
  Man.Estimate:=Man.getCurrentConsumption();
  if (
   (Man.FStatus=esStart) and
   (FSyncP^.stateFiles=Storage.VDM.Sync.State.rqsReceived) and
   (FSyncP^.stateFolders=Storage.VDM.Sync.State.rqsReceived) and
   (FSyncP^.statePipes=Storage.VDM.Sync.State.rqsReceived) and
   (FSyncP^.stateManifest=Storage.VDM.Sync.State.rqsReceived) and
   (FPipeP^.Direction or Storage.VDM.Sync.Pipes.Direction.Download=FPipeP^.Direction) and
   (Man.FMode in [emWorking,emNone]) and (FPipeP^.Mode <> Storage.VDM.Sync.Pipes.Mode.Off)
  ) then begin
    FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.Devices';
    AuDevices:=Storage.UserStorage.Folders.getFolder(Storage.UserStorage.Folders.Defaults.Home.Devices,FSyncP^.Folders);
    mfDevices:=Storage.VDM.Sync.Manifest.Force(AuDevices^.Path,AuDevices^.ID,AuDevices,FSyncP^.Manifest);
    AuResource:=Storage.UserStorage.Folders.getFolder(Concat(Storage.UserStorage.Folders.Defaults.Home.Devices,'/',Settings.ResourceName),FSyncP^.Folders);
    if AuResource=nil then begin
      AuResource:=Storage.UserStorage.Folders.Create(FSyncP^.Folders);
      AuResource^.Path:=Concat(Storage.UserStorage.Folders.Defaults.Home.Devices,'/',Settings.ResourceName);
      Man.CreateFolder(FRSRP^,AuResource^);
    end;
    FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.Folders';
    if AuResource^.ID<>0 then begin
      mfResource:=Storage.VDM.Sync.Manifest.Force(AuResource^.Path,AuResource^.ID,AuResource,mfDevices^.Folders);

      if (FPipeP^.Mode or Storage.VDM.Sync.Pipes.Mode.Cabinet=FPipeP^.Mode) then begin
        AuPipe:=Storage.UserStorage.Folders.getFolder(FPipeP^.Name,FSyncP^.Folders);
        if AuPipe=nil then begin
          AuPipe:=Storage.UserStorage.Folders.Create(FSyncP^.Folders);
          AuPipe^.Path:=FPipeP^.Name;
          FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.Cabinet.AuPipe.CreateFolder';
          Man.CreateFolder(FRSRP^,AuPipe^);
        end;
        if (AuPipe<>nil) and (AuPipe^.ID<>0) then begin
          mfPipe:=Storage.VDM.Sync.Manifest.Force(AuPipe^.Path,AuPipe^.ID,AuPipe,FSyncP^.Manifest);
          FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.Cabinet.AuPipe.ScanFolder';
          ScanFolder(mfPipe);
        end;
      end;
      if (Man.FStatus=esStart) and (FPipeP^.Mode or Storage.VDM.Sync.Pipes.Mode.Devices=FPipeP^.Mode) then begin
        AuPipe:=Storage.UserStorage.Folders.getFolder(Concat(Storage.UserStorage.Folders.Defaults.Home.Devices,'/',Settings.ResourceName,'/',FPipeP^.Name),FSyncP^.Folders);
        if AuPipe=nil then begin
          AuPipe:=Storage.UserStorage.Folders.Create(FSyncP^.Folders);
          AuPipe^.Path:=Concat(Storage.UserStorage.Folders.Defaults.Home.Devices,'/',Settings.ResourceName,'/',FPipeP^.Name);
          FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.Devices.AuPipe.CreateFolder';
          Man.CreateFolder(FRSRP^,AuPipe^);
        end;
        if (AuPipe<>nil) and (AuPipe^.ID<>0) then begin
          mfPipe:=Storage.VDM.Sync.Manifest.Force(AuPipe^.Path,AuPipe^.ID,AuPipe,mfResource^.Folders);
          FProcessor.EntryPoint:='uEngine.cmSyncDownloadUser.Execute.Devices.AuPipe.ScanFolder';
          ScanFolder(mfPipe);
        end;
      end;
    end;
  end;
  if bSyncModified and Assigned(Man.FOnUsrSyncManifestModified) then
    Man.FOnUsrSyncManifestModified(FSyncP^);
  FPipeP^.SyncDownload:=nil;
end;


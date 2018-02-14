constructor cmSyncDownloadSocial.Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.Social.Sync.TItem; var aPipe:Storage.Social.Sync.Pipes.TItem);
begin
  FPipeP:=@aPipe;
  FSyncP:=@aSync;
  FHdrP:=@aSync.Header;

  FRequest:=THTTPRequest.Create();
  inherited Create(aManager,aRSRP);
end;

destructor cmSyncDownloadSocial.Destroy();
begin
  FPipeP:=nil;
  FSyncP:=nil;
  FHdrP:=nil;
  FRequest.Free();
  inherited Destroy();
end;

procedure cmSyncDownloadSocial.Execute();
var
  Man           : TAuSocketMan;
  iCurCon       : QWord;
  iFileSize     : QWord;
  dtModified    : TDateTime;
  bUpdateSyncPass : boolean;


  mfPipe        : Storage.Social.Sync.Manifest.PSMFSTFolder;
  AuPipe        : Storage.Social.Folders.PSFolder;
  FileP         : Storage.Social.Files.PSFile;

  dgMD5         : TMD5Digest;

  sLocalFile    : Core.Strings.VarString;
  sAuFile       : Core.Strings.VarString;
  sFolder       : Core.Strings.VarString;

  StampDiff     : Int64;
  Est           : QWord;
  iMaxSend      : QWord;

  procedure ScanCloudFile(mfstP:Storage.Social.Sync.Manifest.PSMFSTItem);

    procedure PushReadFile();
    begin
        bUpdateSyncPass:=false;
        Man.FSyncQueue.Schedule(
          FProcessor,
          Queue.ItemKind.Read,
          FPipeP,
          FSyncP,
          FHdrP,
          mfstP^.FolderP,
          mfstP^.ParentP,
          mfstP^.FileP,
          mfstP,
          sLocalFile,
          sAuFile
        );
    end;

  begin
    FileP:=mfstP^.FileP;
    FileP^.Manifest:=mfstP;
    mfstP^.PipeP:=FPipeP;
    if (FileP^.Allocated>Storage.Social.Files.Allocate_Base) then begin // Already checked if Allocated
      sLocalFile:=Concat(sFolder,SysUtils.PathDelim,FileP^.Name);
      sAuFile:=Concat(mfstP^.ParentP^.Path,'/',FileP^.Name);
      if not SysUtils.FileExists(sLocalFile) then begin
        PushReadFile();
      end else if GetFileProfile(sLocalFile,iFileSize,dtModified) then begin
        StampDiff:=Core.Utils.Time.DifferInMilliseconds(dtModified,FileP^.Modified);
        if ( (Abs(StampDiff)>Storage.Social.Sync.Defaults.ThresholdMilliseconds) and (dtModified<FileP^.Modified) and (dtModified<>mfstP^.Modified) )then begin
          Man.SetSyncStatus(Format(FMT_PROGRESS_SOC_STATUS,[auLang.Table.Labels.Scanning,FSyncP^.Network.Title,sLocalFile]));
          if GetFileProfile(sLocalFile,dgMD5) then begin
            if (Core.Arrays.Bytes.Same(FileP^.Digest,dgMD5)=false) then begin
              PushReadFile();
            end else begin
              Core.Utils.Files.SetFileDateTime(sLocalFile,FileP^.Modified,Core.Utils.Time.FromUTCBias);
              mfstP^.Status:=Storage.Social.Sync.Manifest.itemStatus.msiVerified;
              GetFileProfile(sLocalFile,mfstP^.Modified);
              mfstP^.Size:=iFileSize;
              bUpdateSyncPass:=false;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ScanFolder(mfPivot:Storage.Social.Sync.Manifest.PSMFSTFolder);
  var
    iCt,iLcv : LongInt;
    mfstP    : Storage.Social.Sync.Manifest.PSMFSTItem;
    mfLcv    : Storage.Social.Sync.Manifest.PSMFSTFolder;
    auFldr   : Storage.Social.Folders.PSFolder;
  begin
    if (Man.Terminated=false) and (FProcessor.Terminated=false) and (Man.FStatus=esStart) and (mfPivot<>nil) and (Man.FMode in [emWorking,emNone]) then begin
      if System.Length(mfPivot^.Path)=0 then
        mfPivot^.Path:=Concat(FPipeP^.Path,Storage.Social.Sync.Manifest.BuildPath(mfPipe,mfPivot^));
      sFolder:=mfPivot^.Path;
      if not SysUtils.DirectoryExists(sFolder) then
        SysUtils.ForceDirectories(sFolder);
      for iLcv:=0 to High(mfPivot^.Items) do begin
        FProcessor.EntryPoint:=Concat('uEngine.cmSyncDownloadSocial.Execute.ScanFolder.Pivot:',sFolder);

        mfstP:=mfPivot^.Items[iLcv];
        if (mfstP^.FileP=nil) then
          mfstP^.FileP:=Storage.Social.Files.Get(mfstP^.ID,FSyncP^.Files);
        if ( mfstP^.FileP<>nil)  then
          ScanCloudFile(mfstP);
        if (Man.FStatus<>esStart) then  break;
      end;
      iLcv:=0; iCt:=System.Length(mfPivot^.Folders);
      while (Man.FStatus=esStart) and (iLcv<iCt) and (Man.FMode in [emWorking,emNone]) do begin
        FProcessor.EntryPoint:='uEngine.cmSyncDownloadSocial.Execute.ScanFolder.Loop';
        mfLcv:=mfPivot^.Folders[iLcv];
        if mfLcv^.ID<>0 then begin
          auFldr:=Storage.Social.Folders.getFolder(mfLcv^.ID,FSyncP^.Folders);
          if auFldr=nil then begin // stale referenced cloud folder.  Not present.  Delete.
             Storage.Social.Sync.Manifest.Delete(mfLcv);
             Dec(iCt);
          end else begin
            ScanFolder(mfLcv);
            inc(iLcv);
          end;
        end else
          inc(iLcv);
      end;
    end;
  end;

begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmSyncDownloadSocial.Execute';
  bUpdateSyncPass:=true;
  AuPipe:=nil;
  if (
   (Man.FStatus=esStart) and
   ( FSyncP^.statePipes = Storage.Social.Sync.State.rqsReceived) and
   ( FSyncP^.stateManifest = Storage.Social.Sync.State.rqsReceived) and
   ( FSyncP^.uiDownload.stateFolders  = Storage.Social.Sync.State.rqsReceived) and
   ( FSyncP^.uiDownload.stateFiles    = Storage.Social.Sync.State.rqsReceived) and
   (FPipeP^.Direction or Storage.Social.Sync.Pipes.Direction.Download=FPipeP^.Direction) and
   (Man.FMode in [emWorking,emNone])
  ) then begin
    FProcessor.EntryPoint:='uEngine.cmSyncDownloadSocial.Execute.Folders';
    AuPipe:=Storage.Social.Folders.getFolder(FPipeP^.Name,FSyncP^.Folders);
    if AuPipe=nil then begin
      AuPipe:=Storage.Social.Folders.Create(FSyncP^.Network.ID,FSyncP^.Folders);
      AuPipe^.Path:=FPipeP^.Name;
      FProcessor.EntryPoint:='uEngine.cmSyncDownloadSocial.Execute.Cabinet.AuPipe.CreateFolder';
      Man.CreateFolder(FRSRP^,AuPipe^);
    end;
    if (AuPipe<>nil) and (AuPipe^.ID<>0) then begin
      mfPipe:=Storage.Social.Sync.Manifest.Force(AuPipe^.Path,AuPipe^.ID,AuPipe,FSyncP^.Manifest);
      FProcessor.EntryPoint:='uEngine.cmSyncDownloadSocial.Execute.Cabinet.AuPipe.ScanFolder';
      ScanFolder(mfPipe);
    end;
  end;
  if (FSyncP^.Header.Modified>FSyncP^.uiDownload.LastModified) then begin
    // Invalidate folders and Files
    FSyncP^.uiDownload.LastModified:=FSyncP^.Header.Modified;
    FSyncP^.uiDownload.stateFolders:=Storage.Social.Sync.State.rqsNone;
    FSyncP^.uiDownload.stateFiles:=Storage.Social.Sync.State.rqsNone;
  end;
  if (bUpdateSyncPass=true) then begin
    Inc(FSyncP^.uiDownload.Lcv,1);
  end else if Assigned(Man.FOnSocSyncManifestModified) then
    Man.FOnSocSyncManifestModified(FSyncP^);
  FPipeP^.SyncDownload:=nil;
  FProcessor.EntryPoint:='uEngine.cmSyncDownloadSocial.Execute.Done';
end;


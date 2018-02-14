procedure   TAuSocketMan.SyncDownloadSocial(Var SR:TRSR; var Pipe:Storage.Social.Sync.Pipes.TItem);
var
  iEstimate       : QWord;
  iLcv,iCt        : LongInt;
  iFileSize       : Int64;
  dtModified      : TDateTime;
  bUpdateSyncPass : boolean;
  Est             : QWord;
  msDiffers       : Int64;

  mfPipe          : Storage.Social.Sync.Manifest.PMFSTFolders;
  mfFolder        : Storage.Social.Sync.Manifest.PSMFSTFolder;

  ctxMD5          : TMD5Context;
  dgMD5           : TMD5Digest;

  sLocalFile      : String;
  sLocalFileNew   : String;
  sFolder         : String;
  dtUTFile        : TDateTime;

  function  OkToDownload():boolean;
  begin
    Result:=(
      (FStatus=esStart) and
      (Pipe.ManifestLoaded=true) and
      (Pipe.FilesLoaded=true) and
      (Pipe.Direction or Storage.Social.Sync.Pipes.Direction.Download=Pipe.Direction) and
      (ServerChannel.ServerBuffer.Consumption<ServerChannel.Limit) and
      (RSR.Throttle.Consumption<RSR.Throttle.Limit) and
      (iEstimate<RSR.Throttle.Limit)
    );
  end;
  procedure ScanCloudFile(mfstP:Storage.Social.Sync.Manifest.PSMFSTItem);
  begin
    sLocalFile:=Concat(sFolder,SysUtils.PathDelim,mfstP^.FileP^.Name);
    FSyncStatus:=Format(FMT_PROGRESS_STATUS,[auLang.Table.Labels.Inspecting,sLocalFile]);
    // is this a file rename?
    if (mfstP^.Name<>mfstP^.FileP^.Name) then begin
      sLocalFileNew:=Concat(sFolder,SysUtils.PathDelim,mfstP^.FileP^.Name);
      SysUtils.RenameFile(sLocalFile,sLocalFileNew);
      mfstP^.Name:=mfstP^.FileP^.Name;
      GetFileProfile(sLocalFile,mfstP^.Size,mfstP^.Modified);
      mfstP^.DownloadPass:=FSyncDownloadPass;
      bUpdateSyncPass:=false;
      exit;
    end;
    if not SysUtils.FileExists(sLocalFile) then begin
      mfstP^.DownloadPass:=FSyncDownloadPass;
      Est:=ReadFile(RSR,mfstP^.FileP^);
      Est:=Est+mfstP^.FileP^.Size;
      inc(iEstimate,Est);
      bUpdateSyncPass:=false;
      exit;
    end else begin
      GetFileProfile(sLocalFile,iFileSize,dtModified);
      msDiffers:=Core.Utils.Time.DifferInMilliseconds(dtModified,FileP^.Modified);

      if Abs(msDiffers>Storage.Social.Sync.ThresholdMilliseconds) and (FileP^.Modified>dtModified) then begin
      //if (mfstP^.Size<>iFileSize) or (mfstP^.Modified<>dtModified) then begin
        if GetFileProfile(sLocalFile,mfstP^.FileP^.Data,dgMD5,ctxMD5) then begin
          uByteArray.Empty(mfstP^.FileP^.Data);
          Storage.UserStorage.Files.Copy(dgMD5,mfstP^.Digest);
          mfstP^.Size:=iFileSize;
          mfstP^.Modified:=dtModified;
          bUpdateSyncPass:=false;
        end;
      end;
      if Storage.UserStorage.Files.Compare(mfstP^.Digest,mfstP^.FileP^.Digest)=false then begin
        // the File Contents are different
        dtUTFile:=DateUtils.IncMillisecond(mfstP^.Modified,ToUTCBias*60000);
        DifferInMilliseconds
        if (dtUTFile<mfstP^.FileP^.Modified) then begin
          mfstP^.DownloadPass:=FSyncDownloadPass;
          Est:=ReadFile(RSR,mfstP^.FileP^);
          Est:=Est+mfstP^.FileP^.Size;
          inc(iEstimate,Est);
          bUpdateSyncPass:=false;
          exit;
        end;
      end;
    end;
  end;

  procedure ScanFolder(mfPivot:Storage.Social.Sync.Manifest.PSMFSTFolder);
  var
    iCt,iLcv  : LongInt;
    mfstP     : Storage.Social.Sync.Manifest.PSMFSTItem;
    mfFolderP : Storage.Social.Sync.Manifest.PSMFSTFolder;
  begin
    if (mfPivot<>nil) and (OkToDownload()=true) then begin
      sFolder:=Concat(Pipe.Path,Storage.Social.Sync.Manifest.BuildPath(mfPivot^));
      FSyncStatus:=Format(FMT_PROGRESS_STATUS,[auLang.Table.Labels.Scanning,sFolder]);
      if not SysUtils.DirectoryExists(sFolder) then
        SysUtils.ForceDirectories(sFolder);
      for iLcv:=0 to High(mfPivot^.Items) do begin
        mfstP:=mfPivot^.Items[iLcv];
        mfFolderP:=mfstP^.ParentP;
        if (mfstP^.FileP=nil) then
          mfstP^.FileP:=Storage.Social.Folders.getFile(mfstP^.ID,mfFolderP^.FolderP^);
        if (mfstP^.FileP<>nil) and (mfstP^.DownloadPass<FSyncDownloadPass) then
          ScanCloudFile(mfstP);
        if (OkToDownload()=false) then exit;
      end;
      iLcv:=0; iCt:=System.Length(mfPivot^.Folders);
      while (OkToDownload()=true) do begin
        ScanFolder(mfPivot^.Folders[iLcv]);
        inc(iLcv);
      end;
    end;
  end;

begin
  bUpdateSyncPass:=true;
  iEstimate:=RSR.Throttle.Consumption;
  iLcv:=0;
  mfPipe:=Pipe.Manifest;
  While ( (OkToDownload()=true) and (iLcv< System.Length(mfPipe^)) ) do begin
    mfFolder:=mfPipe^[iLcv];
    ScanFolder(mfFolder);
    inc(iLcv);
  end;
  if bUpdateSyncPass then
    Inc(FSyncDownloadPass);
  FProgressValue:=0;
  System.SetLength(FSyncStatus,0);
  if Assigned(FOnSyncModified) then FOnSyncModified(FSyncP^);
end;

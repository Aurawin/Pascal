procedure   TAuSocketMan.SyncUploadSocial(Var SR:TRSR; var Pipe:Storage.Social.Sync.Pipes.TItem);
var
  bUpdateSyncPass : boolean;
  iLcv            : LongInt;
  iEstimate       : Qword;
  mfPipe          : Storage.Social.Sync.Manifest.PMFSTFolders;
  function  OkToUpload:boolean;
  begin
    Result:= (
      (Terminated=false) and
      (FStatus=esStart) and
      (FConnected=true) and
      (Pipe.ManifestLoaded=true) and
      (Pipe.FoldersLoaded=true) and
      (Pipe.FilesLoaded=true) and
      (Pipe.Direction or Sync.Pipes.Direction.Upload=Pipe.Direction) and
      (ServerChannel.ServerBuffer.Consumption<ServerChannel.Limit) and
      (ServerChannel.ClientBuffer.Consumption<ServerChannel.Limit) and
      (RSR.Throttle.Consumption<RSR.Throttle.Limit) and
      (iEstimate<RSR.Throttle.Limit)
    );
  end;

  procedure ScanFolder(var Folder:Storage.Social.Folders.TSFolder; prePath:String);
  var
    SR:TSearchRec;
    CurP:Storage.Social.Folders.PSFolder;
    SubP:Storage.Social.Folders.PSFolder;
    FileP:Storage.Social.Files.PSFile;
    mfFolderP:Storage.Social.Sync.Manifest.PSMFSTFolder;
    mfFileP:Storage.Social.Sync.Manifest.PSMFSTItem;
    sSubPath:String;
    sAuPath:String;
    sAuFile:String;
    sLoFile:String;
    iFileSize:Int64;
    iID:Int64;
    Est:QWord;
    dtModified:TDateTime;

    iIndex:integer;
    iResult:Integer;
    bException:Boolean;
    bKeywords:boolean;
    ctxMD5:TMD5Context;
    dgMD5:TMD5Digest;

    Data:uByteArray.Core.Arrays.Types.Bytes;
  begin
    mfFolderP:=Storage.Social.Sync.Manifest.Force(Folder.Path,Folder.ID,mfPipe^);
    FSyncStatus:=Format(FMT_PROGRESS_STATUS,[auLang.Table.Labels.Synchronizing,mfFolderP^.Name]);
    iResult:=FindFirst(Core.Utils.Files.Append(prePath,'*'),faDirectory or faAnyFile,SR);
    Try
      While ( (iResult=0) and (OkToUpload()=true)) do begin
        if isDirectory(SR.Attr) then begin
          // Any lbFolders get added as lbFolders and parsed
          if (SR.Name<>'..') and (SR.Name<>'.') and (SR.Name<>'.svn') then begin
            sAuPath:=Append(Folder.Path,SR.Name);
            sSubPath:=Append(prePath,SR.Name);
            FSyncStatus:=Format(FMT_PROGRESS_STATUS,[auLang.Table.Labels.Synchronizing,sAuPath]);
            //if Assigned(FOnStatusChanged) then Synchronize(@SyncStatus);
            SubP:=Storage.Social.Folders.Force(sAuPath,0,Pipe.Folders);
            if (SubP^.ID=0) then
              CreateFolder(RSR,SubP^);
            ScanFolder(SubP^,sSubPath);
          end;
        end else if (not SameText(SR.Name,'thumbs.db')) and (Folder.ID<>0) then begin // we'll get this next sync
          // This is a file
          sAuFile:=Append(Folder.Path,SR.Name);
          sLoFile:=Append(prePath,SR.Name);

          FSyncStatus:=Format(FMT_PROGRESS_STATUS,[auLang.Table.Labels.Inspecting,sAuFile]);
          //Inc(FProgressValue);
          //if Assigned(FOnStatusChanged) then Synchronize(@SyncStatus);

          FileP:=Storage.Social.Folders.getFile(Sr.Name,Folder); // Contains Digest, DateTimes, Name from Cloud (Folder.Files[])
          if (FileP=nil) then begin
            // This file was deleted or is not yet present in the Cloud, Obtain Digest, and issue Create Statement.
            mfFileP:=Storage.Social.Sync.Manifest.getItem(Sr.Name,mfFolderP^);
            if (mfFileP<>nil) then begin
              // this item was previously in the cloud but is no longer there.
              // We shoud delete this file
              mfFileP^.DeviceVerified:=false;    // mark the manifest item for
              mfFileP^.ServerVerified:=false;    // removal;
              // No delete yet.  Need rescue push first SysUtils.DeleteFile(sLoFile);
            end else begin
              // this item does not exist in the cloud, and needs to be uploaded
              if GetFileProfile(sLoFile,Data,iFileSize,dtModified,dgMD5,ctxMD5) then begin
                bUpdateSyncPass:=false;
                // now we add to system
                FileP:=Storage.Social.Folders.Add(Sr.Name,dgMD5,Data,Folder);
                FileP^.Created:=dtModified;
                CreateFile(RSR,FileP^); // Send file with data (empty Data)
                // not needed Files.Add(FileP^,FSocFileList);
                SetLength(FileP^.Data,0);// empty data buffer
                EnterCriticalSection(SyncLock);
                try
                  mfFileP:=Storage.Social.Sync.Manifest.Force(FileP^,mfFolderP^,iFileSize,dtModified);
                  mfFileP^.UploadPass:=FSyncUploadPass;
                  mfFileP^.DeviceVerified:=true;
                  mfFileP^.ServerVerified:=true;
                  FileP^.Manifest:=mfFileP;
                  mfFIleP^.PipeP:=@Pipe;
                finally
                  LeaveCriticalSection(SyncLock);
                end;
              end;
            end;
          end;
          if (FileP<>nil) then begin
            mfFileP:=FileP^.Manifest;
            if mfFileP=nil then begin
              if GetFileProfile(sLoFile,iFileSize,dtModified) then begin
                EnterCriticalSection(SyncLock);
                try
                  mfFileP:=Storage.Social.Sync.Manifest.Force(FileP^,mfFolderP^,iFileSize,dtModified);
                  mfFileP^.DeviceVerified:=true;
                  FileP^.Manifest:=mfFileP;
                  mfFileP^.UploadPass:=FSyncUploadPass;
                  mfFIleP^.PipeP:=@Pipe;
                finally
                  LeaveCriticalSection(SyncLock);
                end;
              end;
            end;
            if (mfFileP<>nil) then begin
              if (mfFileP^.UploadPass<FSyncUploadPass) then begin
                if GetFileProfile(sLoFile,iFileSize,dtModified) then begin
                  // Compare Sized, Modified first, then Digest If Modified<> same then build digest
                  If (mfFileP^.Modified<>dtModified) or (mfFileP^.Size<>iFileSize) then begin
                    if GetFileProfile(sLoFile,Data,iFileSize,dtModified,dgMD5,ctxMD5) then begin
                      if Storage.UserStorage.Files.Compare(dgMD5,FileP^.Digest) = false then begin
                        uByteArray.Copy(Data,FileP^.Data);
                        FileP^.Size:=System.Length(Data);
                        mfFileP^.Size:=FileP^.Size;
                        mfFileP^.Modified:=dtModified;
                        mfFileP^.UploadPass:=FSyncUploadPass;
                        mfFileP^.DeviceVerified:=true;
                        mfFileP^.ServerVerified:=true;
                        Storage.UserStorage.Files.Copy(dgMD5,mfFileP^.Digest);
                        Storage.UserStorage.Files.Copy(dgMD5,FileP^.Digest);
                        Est:=WriteFile(RSR,FileP^);
                        Est:=Est+FileP^.Size;
                        inc(iEstimate,Est);
                        bUpdateSyncPass:=false;
                      end else begin
                        // Modified datetime is out of whack but the digest is the same so just update RAM
                        mfFileP^.Size:=iFileSize;
                        mfFileP^.Modified:=dtModified;
                        mfFileP^.UploadPass:=FSyncUploadPass;
                        mfFileP^.DeviceVerified:=true;
                        mfFileP^.ServerVerified:=true;
                        bUpdateSyncPass:=false;
                      end;
                      if Assigned(FOnSyncModified) then FOnSyncModified(FSyncP^);
                    end;
                  end;
                end;
              end else
                mfFileP^.UploadPass:=FSyncUploadPass;
            end;
          end;
          Inc(FProgressValue);
        end;
        iResult:=FindNext(SR);
      end;
    finally
      FindClose(SR);
    end;
    System.SetLength(FSyncStatus,0);
  end;
begin
  bUpdateSyncPass:=true; iLcv:=0;
  mfPipe:=Pipe.Manifest;
  iEstimate:=RSR.Throttle.Consumption;
  while ( (OkToUpload()=true) and (iLcv<System.Length(Pipe.Folders)) ) do begin
    ScanFolder(Pipe.Folders[iLcv]^,Pipe.Path);
    FProgressValue:=0;
    System.SetLength(FSyncStatus,0);
    if bUpdateSyncPass then
      Inc(FSyncUploadPass);
    Inc(iLcv);
  end;
end;


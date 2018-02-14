constructor cmSyncUploadSocial.Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.Social.Sync.TItem; var aPipe:Storage.Social.Sync.Pipes.TItem);
begin
  FPipeP:=@aPipe;
  FSyncP:=@aSync;
  FHdrP:=@aSync.Header;
  inherited Create(aManager,aRSRP);
end;

destructor cmSyncUploadSocial.Destroy();
begin
  FPipeP:=nil;
  FSyncP:=nil;
  inherited Destroy();
end;

procedure cmSyncUploadSocial.Execute();
var
  Man             : TAuSocketMan;
  AuFolderP       : Storage.Social.Folders.PSFolder;
  dgMD5           : TMD5Digest;
  FData           : TFileStream;
  bSyncModified   : boolean;
  iMaxSend        : QWord;
  StampModified   : boolean;
  StampDiff       : Int64;

  procedure ScanFolder(FolderP:Storage.Social.Folders.PSFolder; prePath:Core.Strings.VarString);
  var
     SR:TSearchRec;
     SubP:Storage.Social.Folders.PSFolder;
     FileP:Storage.Social.Files.PSFile;
     mfFolderP:Storage.Social.Sync.Manifest.PSMFSTFolder;
     mfFileP:Storage.Social.Sync.Manifest.PSMFSTItem;
     sSubPath:Core.Strings.VarString;
     sAuPath:Core.Strings.VarString;
     sAuFile:Core.Strings.VarString;
     sLoFile:Core.Strings.VarString;

     srName:Core.Strings.VarString;
     iFileSize:QWord;

     dtModified:TDateTime;
     iResult:Integer;


     procedure PushAssignDigest();
     begin
       Core.Arrays.Bytes.Copy(dgMD5,mfFileP^.Digest);
       Core.Arrays.Bytes.Copy(dgMD5,FileP^.Digest);
       bSyncModified:=true;
     end;

     procedure PushAssignManifestData();
     begin
       mfFileP^.Size:=iFileSize;
       mfFileP^.Modified:=dtModified;
       bSyncModified:=true;
     end;

     procedure PushRemoteAndLocalFileAreTheSame();
     begin
       mfFileP^.Size:=iFileSize;
       mfFileP^.Modified:=dtModified;
       bSyncModified:=true;
     end;

     procedure PushForceVerifiedManifestData();
     begin
       mfFileP:=Storage.Social.Sync.Manifest.Force(FileP^,mfFolderP^,iFileSize,dtModified);

       FileP^.Manifest:=mfFileP;
       mfFileP^.PipeP:=FPipeP;
       bSyncModified:=true;
     end;

     procedure PushWriteFile();
     begin
       Man.FSyncQueue.Schedule(
         FProcessor,
         Queue.ItemKind.Write,
         FPipeP,
         FSyncP,
         FHdrP,
         SubP,
         mfFolderP,
         FileP,
         mfFileP,
         sLoFile,
         sAuFile
       );
     end;

     procedure PushCreateFileAsFirst();
     begin

       FileP:=Storage.Social.Files.Add(srName,Settings.UserID,FHdrP^.NetworkID,FolderP^.ID,iFileSize,Storage.Social.Files.Kind.BIN,dgMD5,FolderP^.Files);
       FileP^.Allocated:=Storage.Social.Files.Allocate_Creating;
       FileP^.Created:=dtModified;
       FileP^.Modified:=dtModified;

       Storage.Social.Files.Add(FileP^,FSyncP^.Files);
       mfFileP:=Storage.Social.Sync.Manifest.Force(FileP^,mfFolderP^,iFileSize,dtModified);

       FileP^.Manifest:=mfFileP;
       mfFileP^.PipeP:=FPipeP;

       PushAssignDigest();
       PushAssignManifestData();

       Man.FSyncQueue.Schedule(
         FProcessor,
         Queue.ItemKind.Create,
         FPipeP,
         FSyncP,
         FHdrP,
         SubP,
         mfFolderP,
         FileP,
         mfFileP,
         sLoFile,
         sAuFile
       );
     end;

   begin
     FProcessor.EntryPoint:='uEngine.cmSyncUploadSocial.Execute.ScanFolder';
     mfFolderP:=Storage.Social.Sync.Manifest.Force(FolderP^.Path,FolderP^.ID,FolderP,FSyncP^.Manifest);
     Try
       if (Length(mfFolderP^.Path)=0) then
         mfFolderP^.Path:=PrePath;
       iResult:=FindFirst(Core.Utils.Files.Append(prePath,'*'),faDirectory or faAnyFile,SR);
       While (
            (Man.Terminated=false) and
            (FProcessor.Terminated=false) and
            (Man.FStatus=esStart) and
            (iResult=0) and
            (Man.FConnected=true) and
            (Man.FMode in [emWorking,emNone])
       ) do begin
         srName:=SR.Name;
         if isDirectory(SR.Attr) then begin
           if (srName<>'..') and (srName<>'.') and (srName<>'.svn') then begin
             sAuPath:=Append(FolderP^.Path,srName,'/');
             sSubPath:=Append(prePath,srName);
             SubP:=Storage.Social.Folders.getFolder(srName,FolderP^.Folders);
             if (SubP=nil) then begin
               SubP:=Storage.Social.Folders.Add(FHdrP^.NetworkID,0,sAuPath,srName,FolderP^.Folders,FolderP);
               Storage.Social.Folders.Add(SubP,FSyncP^.Folders);
               Man.CreateFolder(FRSRP^,SubP^);
             end;
             if ((SubP<>nil) and (SubP^.ID<>0)) then
               ScanFolder(SubP,sSubPath);
           end;
         end else if (Man.UploadAllowed(srName)=true) and (FolderP^.ID<>0) then begin
           // This is a file
           sLoFile:=Append(prePath,srName);
           //iIndex:=0;
           FileP:=Storage.Social.Files.Get(srName,FolderP^.Files);
           sAuFile:=Append(FolderP^.Path,srName,'/');
           if FileP=nil then
             FileP:=Storage.Social.Files.Get(srName,FolderP^.Files);
           if FileP=nil then
             FileP:=Storage.Social.Files.Get(FolderP^.ID,srName,FSyncP^.Files);
           if GetFileProfile(sLoFile,iFileSize,dtModified) then begin
             if (FileP=nil) then begin
               GetFileProfile(sLoFile,dgMD5);
               PushCreateFileAsFirst();
             end;
             if (FileP<>nil)  then begin
               if (FileP^.ID<>0) then begin
                 mfFileP:=FileP^.Manifest;
                 if (mfFileP=nil) then
                   PushForceVerifiedManifestData();
                 if (mfFileP<>nil) then begin
                   StampDiff:=Core.Utils.Time.DifferInMilliseconds(dtModified,FileP^.Modified);
                   StampModified:= (
                     (FileP^.Allocated>Storage.Social.Files.Allocate_Base) and
                     (Abs(StampDiff)>Storage.Social.Sync.Defaults.ThresholdMilliseconds) and
                     (dtModified>FileP^.Modified) and
                     (dtModified<>mfFileP^.Modified)
                   );
                   if (
                       (StampModified= true) or
                       (mfFileP^.Status in Storage.Social.Sync.Manifest.NotPresent)
                   ) then begin
                     // Compare Sized, Modified first, then Digest If Modified<> same then build digest
                     if (StampModified=true) then begin
                       Man.SetSyncStatus(Format(FMT_PROGRESS_USR_STATUS,[auLang.Table.Labels.Scanning,sLoFile]));
                       if GetFileProfile(sLoFile,dgMD5) then begin
                         if (Core.Arrays.Bytes.Same(dgMD5,FileP^.Digest) = false)  then begin
                           PushAssignDigest();
                           PushAssignManifestData();
                           PushWriteFile();
                         end else begin
                           // Modified datetime is out of sync but the digest is the same so just update RAM
                           bSyncModified:=true;
                           mfFileP^.Status:=Storage.Social.Sync.Manifest.itemStatus.msiVerified;
                           PushAssignDigest();
                           PushAssignManifestData();
                           Core.Utils.Files.SetFileDateTime(sLoFile,FileP^.Modified,Core.Utils.Time.FromUTCBias);
                           GetFileProfile(sLoFile,mfFileP^.Modified);
                         end;
                       end;
                     end else if ( ( mfFileP^.Status in Storage.Social.Sync.Manifest.notPresent) ) then begin
                       PushAssignManifestData();
                       PushWriteFile();
                     end;
                   end;
                 end;
               end else begin
                 FileP^.Allocated:=FileP^.Allocated;
               end;
             end;
           end;
         end;
         iResult:=FindNext(SR);
       end;
     finally
       FindClose(SR);
     end;
   end;

begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmSyncUploadSocial.Execute';
  bSyncModified:=false;
  if (
    (Man.FStatus=esStart) and
    (FSyncP^.uiUpload.stateFiles=rqsReceived ) and
    (FSyncP^.uiUpload.stateFolders=rqsReceived) and
    (FSyncP^.statePipes=rqsReceived) and
    (FSyncP^.stateManifest=rqsReceived) and
    (Man.FMode in [emWorking,emNone]) and
    (FPipeP^.Direction or Sync.Pipes.Direction.Upload=FPipeP^.Direction)
   ) then begin

       Man.FMode:=emWorking;
       AuFolderP:=Storage.Social.Folders.getFolder(FPipeP^.Name,FSyncP^.Folders);
       if AuFolderP=nil then begin
         AuFolderP:=Storage.Social.Folders.Create(FHdrP^.NetworkID,FSyncP^.Folders);
         AuFolderP^.Path:=FPipeP^.Name;
         AuFolderP^.Name:=FPipeP^.Name;
         Man.CreateFolder(FRSRP^,AuFolderP^);
       end;
       if (AuFolderP<>nil) and (AuFolderP^.ID<>0) then
         ScanFolder(AuFolderP,FPipeP^.Path);

   end;
   if (FSyncP^.Header.Modified>FSyncP^.uiUpload.LastModified) then begin
     // Invalidate folders and Files
     FSyncP^.uiUpload.LastModified:=FSyncP^.Header.Modified;
     FSyncP^.uiUpload.stateFolders:=Storage.Social.Sync.State.rqsNone;
     FSyncP^.uiUpload.stateFiles:=Storage.Social.Sync.State.rqsNone;
   end;
   if (bSyncModified=false) then begin
     Inc(FSyncP^.uiUpload.Lcv,1);
   end else if Assigned(Man.FOnSocSyncManifestModified) then
     Man.FOnSocSyncManifestModified(FSyncP^);
   if Man.FMode=emWorking then
     Man.FMode:=emNone;
   FPipeP^.SyncUpload:=nil;
end;


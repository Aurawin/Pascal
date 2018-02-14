constructor TScanThread.Create(
    aOwner   : TAuSocketMan;
    aPipeP   : Storage.Social.Sync.Pipes.PItem;
    aSyncP   : Storage.Social.Sync.PSync;
    aRSRP    : PRSR
);
begin
  FOwner:=aOwner;
  FPipeP:=aPipeP;
  FSyncP:=aSyncP;
  FRSRP:=aRSRP;
  FSleepP:=RTLEventCreate();
  inherited Create(false,App.Consts.RSR.Engine.Settings.STACKSIZE_SCANNER);
end;

destructor TScanThread.Destroy();
begin
  RTLEventDestroy(FSleepP);
  inherited Destroy;
end;

procedure TScanThread.Execute();
begin
  While (Terminated=false) and (FOwner.Terminated=false) do begin
    RTLeventWaitFor(FSleepP,Process(FRSRP));
    Recycle(RecycleFlagged);
  end;
end;

function TScanThread.checkForFoldersRefresh(RSRP:PRSR; Expires:Double):boolean;
begin
  Result:=False;
  if (FSyncP^.UpdateInfo.stateFolders<>rqsReceived)then begin
    if (FSyncP^.UpdateInfo.stateFolders<>rqsRequested ) then begin
      FOwner.ListFolders(RSRP^,FSyncP^);
    end;
    Result:=True;
  end else if (Core.Timer.dtUT>Expires) then begin
    FOwner.ListFolders(RSRP^,FSyncP^);
    Result:=True;
  end;
end;

function TScanThread.checkForFilesRefresh(RSRP:PRSR; Expires:Double):boolean;
begin
  Result:=False;
  if (FSyncP^.UpdateInfo.stateFiles<>rqsReceived)then begin
    if (FSyncP^.UpdateInfo.stateFiles<>rqsRequested ) then begin
      FOwner.ListAllFiles(RSRP^,FSyncP^);
    end;
    Result:=True;
  end else if (Core.Timer.dtUT>Expires) then begin
    FOwner.ListAllFiles(RSRP^,FSyncP^);
    Result:=True;
  end;
end;

function TScannerDownloadThread.Process(RSRP:PRSR):DWORD;
var
  iCurCon       : QWord;
  iFileSize     : QWord;
  dtExpires     : TDateTime;
  dtModified    : Double;
  fldrPipeP     : Storage.Social.Folders.PSFolder;

  dgMD5         : TMD5Digest;

  sLocalFile    : Core.Strings.VarString;
  sAuFile       : Core.Strings.VarString;


  StampDiff     : Int64;
  Est           : QWord;
  iMaxSend      : QWord;

  procedure ScanCloudFile(const flP:Storage.Social.Files.PSFile);
  var
    fdP:Storage.Social.Folders.PSFolder;

    procedure PushReadFile();
    begin
        FOwner.FSyncQueue.Schedule(
          Self,
          ItemKind.Read,
          FPipeP,
          FSyncP,
          fdP,
          flP,
          sLocalFile,
          sAuFile
        );
    end;

  begin
    if (flP^.Allocated>Storage.Social.Files.Allocate_Base) then begin // Already checked if Allocated
      fdP:=FSyncP^.Folders.Find(flP^.FolderID);
      sLocalFile:=Concat(fdP^.Location,System.DirectorySeparator,flP^.Name);
      sAuFile:=Concat(fdP^.Path,App.Consts.PathDelim,flP^.Name);
      if not SysUtils.FileExists(sLocalFile) then begin
        PushReadFile();
      end else if GetFileProfile(sLocalFile,iFileSize,dtModified) then begin
        StampDiff:=Core.Utils.Time.DifferInMilliseconds(dtModified,flP^.Modified);
        if ( (Abs(StampDiff)>Storage.Social.Sync.Defaults.ThresholdMilliseconds) and (dtModified<flP^.Modified) ) then begin
          if GetFileProfile(sLocalFile,dgMD5) then begin
            if (Core.Arrays.Bytes.Same(flP^.Digest,dgMD5)=false) then begin
              PushReadFile();
            end else begin
              Core.Utils.Files.SetFileDateTime(sLocalFile,flP^.Modified,Core.Utils.Time.FromUTCBias);
              GetFileProfile(sLocalFile,flP^.Modified);
              flP^.Size:=iFileSize;
            end;
          end;
        end;
      end;
    end;
  end;

  procedure ScanFolder(const cpP:Storage.Social.Folders.PSFolder);
  var
    iCt,iLcv : LongInt;
    fdP      : Storage.Social.Folders.PSFolder;
  begin

    if (Terminated=false) and (FOwner.Terminated=false) and (FOwner.FStatus=esStart) and (cpP<>nil) and (FOwner.FMode in [emWorking,emNone]) then begin
      if (TryEnterCriticalsection(FSyncP^.Lock)<>0) then begin
        Try
          if not SysUtils.DirectoryExists(cpP^.Location) then
            SysUtils.ForceDirectories(cpP^.Location);
          iCt:=Length(cpP^.Files);
          iLcv:=0;
          while (FOwner.FStatus=esStart) and (iLcv<iCt) and (FOwner.FMode in [emWorking,emNone]) do begin
            ScanCloudFile(cpP^.Files[iLcv]);
            inc(iLcv);
          end;
          iLcv:=0; iCt:=System.Length(cpP^.Folders);
          while (FOwner.FStatus=esStart) and (iLcv<iCt) and (FOwner.FMode in [emWorking,emNone]) do begin
            fdP:=cpP^.Folders[iLcv];
            if fdP^.ID<>0 then begin
              ScanFolder(fdP);
            end;
            inc(iLcv);
          end;
        finally
          LeaveCriticalSection(FSyncP^.Lock);
        end;
      end;
    end;
  end;

begin
  Result:=auSettings.SYNC_SCAN_DELAY_FULL;

  dtExpires:=IncMillisecond(FSyncP^.UpdateInfo.LastChecked,auSettings.SYNC_FAT_REFRESH);

  if checkForFoldersRefresh(RSRP,dtExpires)=true then begin
    Result:=auSettings.SYNC_SCAN_DELAY_RETRY;
    exit;
  end;

  if checkForFilesRefresh(RSRP,dtExpires)=true then begin
    Result:=auSettings.SYNC_SCAN_DELAY_RETRY;
    exit;
  end;

  if (
   (FOwner.FStatus=esStart) and
   ( FSyncP^.statePipes = rqsReceived) and
   ( FSyncP^.UpdateInfo.stateFolders  = rqsReceived) and
   ( FSyncP^.UpdateInfo.stateFiles    = rqsReceived) and
   (FPipeP^.Direction or Storage.Social.Sync.Pipes.Direction.Download=FPipeP^.Direction) and
   (FOwner.FMode in [emWorking,emNone])
  ) then begin
    fldrPipeP:=FSyncP^.Folders.FindByPath(FPipeP^.Name);
    fldrPipeP^.Location:=FPipeP^.Path;
    if fldrPipeP=nil then begin
      EnterCriticalSection(FSyncP^.Lock);
      Try
        New(fldrPipeP);
        Storage.Social.Folders.Init(fldrPipeP^,nil);
        fldrPipeP^.Path:=FPipeP^.Name;
        fldrPipeP^.NetworkId:=FSyncP^.Network.Id;
        FOwner.CreateFolder(FRSRP^,fldrPipeP^);
        FSyncP^.UpdateInfo.stateFolders:= rqsNone;
        FSyncP^.UpdateInfo.stateFiles:= rqsNone;
      finally
        LeaveCriticalSection(FSyncP^.Lock);
      end;
    end;
    if (fldrPipeP<>nil) and (fldrPipeP^.ID<>0) then begin
      ScanFolder(fldrPipeP);
    end;
  end;
end;


function TScannerUploadThread.Process(RSRP:PRSR):DWORD;
var
  fldrPipeP       : Storage.Social.Folders.PSFolder;
  dgMD5           : TMD5Digest;
  FData           : TFileStream;

  iMaxSend        : QWord;
  StampModified   : boolean;
  dtExpires       : TDateTime;
  dtModified      : Double;
  StampDiff       : Int64;

  procedure ScanFolder(FolderP:Storage.Social.Folders.PSFolder);
  var
     sQ:Storage.Social.Sync.Queue.TItem;
     SR:TSearchRec;
     SubP:Storage.Social.Folders.PSFolder;
     FileP:Storage.Social.Files.PSFile;

     sAuPath:Core.Strings.VarString;
     sAuFile:Core.Strings.VarString;
     sLoFile:Core.Strings.VarString;

     srName:Core.Strings.VarString;
     iFileSize:QWord;


     iResult:Integer;

     procedure PushWriteFile();
     begin
       FOwner.FSyncQueue.Schedule(
         Self,
         ItemKind.Write,
         FPipeP,
         FSyncP,
         FolderP,
         FileP,
         sLoFile,
         sAuFile
       );
     end;

     procedure PushCreateFileAsFirst();
     begin

       FileP:=Storage.Social.Files.Add(srName,Settings.UserID,FSyncP^.Network.ID,FolderP^.ID,iFileSize,Storage.Social.Files.Kind.BIN,dgMD5,FolderP^.Files);
       FileP^.Allocated:=Storage.Social.Files.Allocate_Creating;
       FileP^.Created:=dtModified;
       FileP^.Modified:=dtModified;
       FileP^.PipeP:=FPipeP;
       Storage.Social.Files.Add(FileP^,SubP^.Files);

       FOwner.FSyncQueue.Schedule(
         Self,
         ItemKind.Create,
         FPipeP,
         FSyncP,
         FolderP,
         FileP,
         sLoFile,
         sAuFile
       );
     end;

     procedure PushCheckFile(qItem:Storage.Social.Sync.Queue.TItem);
     begin
       if (qItem=nil) then begin
         qItem:=FOwner.FSyncQueue.Find(ItemKind.Write,FolderP^.Id,FileP^.Name);
         if (qItem=nil) then begin
           FOwner.SetSyncStatus(Format(FMT_PROGRESS_USR_STATUS,[auLang.Table.Labels.Scanning,sLoFile]));
           if GetFileProfile(sLoFile,dgMD5) then begin
             if (Core.Arrays.Bytes.Same(dgMD5,FileP^.Digest) = false)  then begin
               Core.Arrays.Bytes.Copy(dgMD5,FileP^.Digest);
               FileP^.Size:=iFileSize;
               FileP^.Modified:=dtModified;
               PushWriteFile();
             end else begin
               // Modified datetime is out of sync but the digest is the same so just update RAM
               Core.Utils.Files.SetFileDateTime(sLoFile,FileP^.Modified,Core.Utils.Time.FromUTCBias);
             end;
           end;
         end;
       end else begin
         qItem:=nil;
       end;
     end;

   begin
     If TryEnterCriticalSection(FSyncP^.Lock)<>0 then begin
       Try
         Try
           iResult:=FindFirst(Core.Utils.Files.Append(FolderP^.Location,'*'),faDirectory or faAnyFile,SR);
           While (
                (FOwner.Terminated=false) and
                (Terminated=false) and
                (FOwner.FStatus=esStart) and
                (iResult=0) and
                (FOwner.FConnected=true) and
                (FOwner.FMode in [emWorking,emNone])
           ) do begin
             srName:=SR.Name;
             if isDirectory(SR.Attr) then begin
               if (srName<>'..') and (srName<>'.') and (srName<>'.svn') then begin
                 sAuPath:=Append(FolderP^.Path,srName,App.Consts.PathDelim);
                 SubP:=Storage.Social.Folders.getFolder(srName,FolderP^.Folders);
                 if (SubP=nil) then begin
                   SubP:=Storage.Social.Folders.Add(FSyncP^.Network.ID,0,sAuPath,srName,FolderP^.Folders,FolderP);
                   SubP^.Location:=Append(FolderP^.Location,srName,DirectorySeparator);
                   FSyncP^.Folders.Add(SubP^);
                   FOwner.CreateFolder(RSRP^,SubP^);
                 end;
                 if (SubP^.Parent=nil) then begin
                   SubP^.Parent:=FolderP;
                 end;
                 if ((SubP<>nil) and (SubP^.ID<>0)) then
                   ScanFolder(SubP);
               end;
             end else if (FOwner.UploadAllowed(srName)=true) and (FolderP^.ID<>0) then begin
               // This is a file
               sLoFile:=Append(FolderP^.Location,srName);
               //iIndex:=0;
               FileP:=Storage.Social.Files.Get(srName,FolderP^.Files);
               sAuFile:=Append(FolderP^.Path,srName,'/');
               if FileP=nil then
                 FileP:=Storage.Social.Files.Get(srName,FolderP^.Files);
               if GetFileProfile(sLoFile,iFileSize,dtModified) then begin
                 if (FileP=nil) then begin
                   GetFileProfile(sLoFile,dgMD5);
                   PushCreateFileAsFirst();
                 end;
                 if (FileP<>nil)  then begin
                   if (FileP^.ID<>0) then begin
                     StampDiff:=Core.Utils.Time.DifferInMilliseconds(dtModified,FileP^.Modified);
                     StampModified:= (
                       (FileP^.Allocated>Storage.Social.Files.Allocate_Base) and
                       (Abs(StampDiff)>Storage.Social.Sync.Defaults.ThresholdMilliseconds) and
                       (dtModified>FileP^.Modified)
                     );
                     if ( StampModified= true) then begin
                       pushCheckFile(FOwner.FSyncQueue.Find(ItemKind.Write,FolderP^.Id,FileP^.Name));
                     end else if (FileP^.Allocated<Storage.Social.Files.Allocate_Base) then begin
                       if (FOwner.FSyncQueue.Find(ItemKind.Write,FolderP^.Id,FileP^.Name)=nil) then begin
                         pushCheckFile(nil);
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
       finally
         LeaveCriticalSection(FSyncP^.Lock);
       end;
     end;
   end;

begin
  Result:=auSettings.SYNC_SCAN_DELAY_FULL;
  dtExpires:=IncMillisecond(FSyncP^.UpdateInfo.LastChecked,auSettings.SYNC_FAT_REFRESH);
  if checkForFoldersRefresh(RSRP,dtExpires)=true then begin
    Result:=auSettings.SYNC_SCAN_DELAY_RETRY;
    exit;
  end;

  if checkForFilesRefresh(RSRP,dtExpires)=true then begin
    Result:=auSettings.SYNC_SCAN_DELAY_RETRY;
    exit;
  end;

  if (
    (FOwner.FStatus=esStart) and
    (FSyncP^.UpdateInfo.stateFiles=rqsReceived ) and
    (FSyncP^.UpdateInfo.stateFolders=rqsReceived) and
    (FSyncP^.statePipes=rqsReceived) and
    (FOwner.FMode in [emWorking,emNone]) and
    (FPipeP^.Direction or Direction.Upload=FPipeP^.Direction)
   ) then begin
       FOwner.FMode:=emWorking;
       fldrPipeP:=FSyncP^.Folders.FindbyPath(FPipeP^.Name);
       if fldrPipeP=nil then begin
         EnterCriticalSection(FSyncP^.Lock);
         Try
           new(fldrPipeP);
           Storage.Social.Folders.Init(fldrPipeP^,nil);
           fldrPipeP^.Path:=FPipeP^.Name;
           fldrPipeP^.Name:=FPipeP^.Name;
           fldrPipeP^.NetworkID:=FSyncP^.Network.ID;
           FSyncP^.Folders.Add(fldrPipeP^);
           FOwner.CreateFolder(RSRP^,fldrPipeP^);
         finally
           LeaveCriticalSection(FSyncP^.Lock);
         end;
       end;
       if (fldrPipeP<>nil) and (fldrPipeP^.ID<>0) then begin
         fldrPipeP^.Location:=FPipeP^.Path;
         ScanFolder(fldrPipeP);
       end;
  end;
end;

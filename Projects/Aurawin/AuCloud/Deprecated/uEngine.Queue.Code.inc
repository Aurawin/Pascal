constructor Queue.Items.Create(aOwner:TAuSocketMan);
begin
  Owner:=aOwner;
  InitCriticalSection(FLock);
  FList:=ItemsList.Create(Core.Generics.Defaults.FreeOnClear);
  Inherited Create();
end;

destructor Queue.Items.Destroy();
begin
  FList.Clear();
  FList.Free();
  DoneCriticalSection(FLock);
  Inherited Destroy();
end;


Constructor Queue.Item.Create(
  aKind          : ItemKind;

  aPipeP         : Storage.Social.Sync.Pipes.PItem;
  aSyncP         : Storage.Social.Sync.PItem;
  aHdrP          : Storage.Social.Sync.PHeader;

  aFolderP       : Storage.Social.Folders.PSFolder;
  aMfFolderP     : Storage.Social.Sync.Manifest.PSMFSTFolder;
  aFileP         : Storage.Social.Files.PSFile;
  aMfFileP       : Storage.Social.Sync.Manifest.PSMFSTItem;

  aLocalized     : Core.Strings.VarString;
  aRemote        : Core.Strings.VarString
);
begin
  Kind:=aKind;

  PipeP:=aPipeP;
  SyncP:=aSyncP;
  HdrP:=aHdrP;

  FolderP:= aFolderP;
  mfFolderP:= aMfFolderP;
  FileP:=aFileP;
  mfFileP:=aMfFileP;

  LocalizedFile:=aLocalized;
  RemoteFile:=aRemote;

end;

procedure Queue.Items.processWrite(RSRP:PRSR; Request:THTTPRequest; aItem:Item);
var
  FData:TFileStream;
begin
  FData:=TFileStream.Create(aItem.LocalizedFile,fmOpenRead or fmShareDenyNone);
  Try
    Owner.WriteFile(RSRP^,Request,aItem.FileP^,FData);
  finally
    FData.Free();
  end;
  aItem.mfFileP^.Status:=Sync.Manifest.itemStatus.msiRequested;
end;

procedure Queue.Items.processCreate(RSRP:PRSR; Request:THTTPRequest; aItem:Item);
begin
  Owner.CreateFile(RSRP^,Request,aItem.FileP^);
  aItem.mfFileP^.Status:=Sync.Manifest.itemStatus.msiRequested;
end;

procedure Queue.Items.processRead(RSRP:PRSR; Request:THTTPRequest; aItem:Item);
begin
   Owner.ReadFile(RSRP^,Request,aItem.FileP^);
   aItem.mfFileP^.Status:=Sync.Manifest.itemStatus.msiRequested;
end;

procedure Queue.Items.NotifyQueued(aCaller:TThread; aItem:Item);
begin
  EnterCriticalSection(FLock);
  Try
   Owner.FSyncItem:=aItem;
   if Assigned(Owner.FOnSyncItemQueued) then
     aCaller.Synchronize(aCaller,@Owner.SyncItemQueued);
   Owner.FSyncItem:=nil;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure Queue.Items.NotifyRemoved(aCaller:TThread; aItem:Item);
begin
  EnterCriticalSection(FLock);
  Try
   Owner.FSyncItem:=aItem;
   if Assigned(Owner.FOnSyncItemRemoved) then
     aCaller.Synchronize(aCaller,@Owner.SyncItemRemoved);
   Owner.FSyncItem:=nil;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

function Queue.Items.Schedule(
  aCaller        : TThread;
  aKind          : ItemKind;
  aPipeP         : Storage.Social.Sync.Pipes.PItem;
  aSyncP         : Storage.Social.Sync.PItem;
  aHdrP          : Storage.Social.Sync.PHeader;

  aFolderP       : Storage.Social.Folders.PSFolder;
  aMfFolderP     : Storage.Social.Sync.Manifest.PSMFSTFolder;
  aFileP         : Storage.Social.Files.PSFile;
  aMfFileP       : Storage.Social.Sync.Manifest.PSMFSTItem;
  aLocalized     : Core.Strings.VarString;
  aRemote        : Core.Strings.VarString

) : Item;
begin
  Result :=Queue.Item.Create(aKind,aPipeP,aSyncP,aHdrP,aFolderP,aMfFolderP,aFileP,aMfFileP,aLocalized,aRemote);
  aMfFileP^.Status:=Sync.Manifest.itemStatus.msiScheduled;
  FList.Add(Result);

  NotifyQueued(aCaller,Result);

end;

procedure Queue.Items.Process(aCaller:TThread; RSRP:PRSR; Request:THTTPrequest);
var
  iLcv:integer;
  itms:TList;
  itm:Queue.Item;

  //itemStatus=(msiNone,msiVerified,msiRequested,msiEmpty,msiPosted,msiMissing,msiDeleted);
begin
  itms:=FList.LockList;
  try
    iLcv:=0;
    while (iLcv<itms.Count) do begin
      RSRP^.Throttle.Consumption:=RSRP^.SendBuffer.Stream.Size+RSRP^.RecvBuffer.Stream.Size;
      itm:=Queue.Item(itms[iLcv]);
      case itm.Kind of
        ItemKind.Create: begin
          // after create, and id!=0, switch to write
          if (itm.mfFileP^.Status=Sync.Manifest.itemStatus.msiScheduled) then begin
            processCreate(RSRP,Request,itm);
          end else if (itm.mfFileP^.Status=Sync.Manifest.itemStatus.msiVerified) then begin
            itm.Kind:=ItemKind.Write;
            itm.mfFileP^.Status:=msiScheduled;
          end;
        end;
        ItemKind.Read : begin
          if (itm.mfFileP^.Status=Sync.Manifest.itemStatus.msiScheduled) then begin
              if (
                ((itm.mfFileP^.Size+RSRP^.SendBuffer.Stream.Size) <=  RSRP^.Throttle.Limit) or
                ((FTransferCount<=TRANSFER_COUNT_THRESHOLD) and (FTransferSize<=TRANSFER_CHUNK_THRESHOLD))
              ) then begin
                InterLockedIncrement64(FTransferCount);
                InterLockedExchangeAdd64(FTransferSize,itm.mfFileP^.Size);
                processRead(RSRP,Request,itm);
              end;
          end else if (itm.mfFileP^.Status=Sync.Manifest.itemStatus.msiVerified) then begin
             InterLockedDecrement64(FTransferCount);
             Core.Interlocked.Subtract(FTransferSize,itm.mfFileP^.Size);

             itms.Remove(itm);
             iLcv-=1;

             NotifyRemoved(aCaller,itm);

             itm.Free();

          end;
        end;
        ItemKind.Write: begin
          if (itm.mfFileP^.Status=Sync.Manifest.itemStatus.msiScheduled) then begin
              if (
                ((RSRP^.Throttle.Consumption) <=  RSRP^.Throttle.Limit) or
                ((FTransferCount<=TRANSFER_COUNT_THRESHOLD) and (FTransferSize<=TRANSFER_CHUNK_THRESHOLD))
              ) then begin
                InterLockedIncrement64(FTransferCount);
                InterLockedExchangeAdd64(FTransferSize,itm.mfFileP^.Size);
                processWrite(RSRP,Request,itm);
              end;
          end else if (itm.mfFileP^.Status=Sync.Manifest.itemStatus.msiVerified) then begin
             InterLockedDecrement64(FTransferCount);
             Core.Interlocked.Subtract(FTransferSize,itm.mfFileP^.Size);


             itms.Remove(itm);
             iLcv-=1;

             NotifyRemoved(aCaller,itm);

             itm.Free();

          end;
        end;
      end;
      iLcv+=1;
    end;

  finally
    FList.UnlockList;
  end;
end;

function Queue.Items.calculateSize():QWord;
var
  iLcv:integer;
  itms:TList;
  itm:Queue.Item;
begin
   itms:=FList.LockList;
   Result:=0;
   try
     for iLcv:=0 to itms.Count-1 do begin
       itm:=Queue.Item(itms[iLcv]);
       Result+=itm.mfFileP^.Size;
     end;
   finally
     FList.UnlockList;
   end;
end;

function Queue.Items.Find(aFolderId:QWord; aFileId:QWord):Item;
var
  iLcv:integer;
  itms:TList;
  itm:Queue.Item;
begin
   itms:=FList.LockList;
   try
     for iLcv:=0 to itms.Count-1 do begin
         itm:=Queue.Item(itms[iLcv]);
         if (itm.FolderP^.Id=aFolderId) and (itm.FileP^.Id=aFileId) then begin
            Result:=itm;
            break;
         end;
     end;
   finally
     FList.UnlockList;
   end;
end;

function Queue.Items.Find(aFolderId:QWord; aFile:Core.Strings.VarString):Item;
var
  iLcv:integer;
  itms:TList;
  itm:Queue.Item;
begin
   itms:=FList.LockList;
   try
     for iLcv:=0 to itms.Count-1 do begin
         itm:=Queue.Item(itms[iLcv]);
         if (itm.FolderP^.Id=aFolderId) and (itm.FileP^.Name=aFile) then begin
            Result:=itm;
            break;
         end;
     end;
   finally
     FList.UnlockList;
   end;
end;



end.


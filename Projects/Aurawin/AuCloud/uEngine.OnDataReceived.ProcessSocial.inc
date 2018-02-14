procedure ProcessSocial();
var
  iLcv:integer;
  qItem:Storage.Social.Sync.Queue.TItem;

begin
  if SameText(FCoreCommand,NS_CORE_CMD_SOC_FLDRS_ADD) then begin
   EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FLDRS_ADD';
   if FXMLDocument<>nil then begin
     Storage.Social.Folders.fromXML(FXMLDocument,FSocFolder);
     // Update Current Folder
     FSocHdrP:=Storage.Social.Sync.Get(FSocFolder.NetworkID,FSocSyncList);
     if (FSocHdrP<>nil) then begin
       FSocSyncP:=FSocHdrP^.ItemP;
       EnterCriticalSection(FSocSyncP^.Lock);
       Try
         FSocFolderP:= FSocSyncP^.Folders.FindByPath(FSocFolder.Path);
         if FSocFolderP<>nil then begin
           FSocFolderP^.ID:=FSocFolder.ID;
           FSocFolderP^.OwnerID:=FSocFolder.OwnerID;
         end else begin
             FSocFolderP:=nil;
         end;
       finally
         LeaveCriticalSection(FSocSyncP^.Lock);
       end;
     end else begin
       FSocFolderP:=nil;
     end;
   end else
      DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_NETWORK_LIST) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_NETWORK_LIST';
    if FXMLDocument<>nil then begin
      Storage.Social.Network.fromXML(FXMLDocument,FNetworks);
      for iLcv:=0 to High(FNetworks) do begin
        FSocHdrP:=Storage.Social.Sync.Force(FNetworks[iLcv]^,FSocSyncList);
        FSocHdrP^.ResourceID:=Settings.ResourceID;
        ReadSync(RSRP^,FSocHdrP^.ItemP^);
      end;
      FNetworksLoaded:=true;
      if Assigned(FOnNetworksListed) then
        Synchronize(@SyncNetworksListed);
    end else
       DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_CONNECTION_LIST) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_CONNECTION_LIST';
    if FXMLDocument<>nil then begin
      Storage.Social.Connection.fromXML(FXMLDocument,FConnections);
      Storage.Social.Network.fromXML(FXMLDocument,FConNetworks);
      for iLcv:=0 to High(FConNetworks) do begin
        FSocHdrP:=Storage.Social.Sync.Force(FConNetworks[iLcv]^,FSocSyncList);
        FSocHdrP^.ResourceID:=FResource.ID;
        ReadSync(RSRP^,FSocHdrP^.ItemP^);
      end;
      if Assigned(FOnConnectionsListed) then
        Synchronize(@SyncConnectionsListed);
    end else
       DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_FLDRS_LIST) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FLDRS_LIST';
    if FXMLDocument<>nil then begin
      ID:=Core.Arrays.KeyString.GetItemAsQword(FResponse.Headers,fieldSearch,iHdrCount);
      if (ID<>0) then begin
        FSocHdrP:=Storage.Social.Sync.Get(ID,FSocSyncList);
        if FSocHdrP<>nil then
          AddMethod(cmProcessListFoldersSocial.Create(Self,RSRP,FXMLDocument,FSocHdrP^));
      end;
    end else
       DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_FILE_LIST_ALL) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FILE_LIST_ALL';
    if (FXMLDocument<>nil) then begin
      ID:=Core.Arrays.KeyString.GetItemAsQWord(FResponse.Headers,fieldSearch,iHdrCount);
      if (ID<>0) then begin
        FSocHdrP:=Storage.Social.Sync.Get(ID,FSocSyncList);
        if FSocHdrP<>nil then
          AddMethod(cmProcessAllFilesSocial.Create(Self,RSRP,FXMLDocument,FSocHdrP^));
      end;
    end else
       DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_FILE_ADD) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FILE_ADD';
    if FXMLDocument<>nil then begin
      Storage.Social.Files.fromXML(FXMLDocument,FSocFile);
      FSocHdrP:=Storage.Social.Sync.Get(FSocFile.NetworkID,FSocSyncList);
      if FSocHdrP<>nil then begin
        FSocSyncP:=FSocHdrP^.ItemP;
        FSocFolderP:=FSocSyncP^.Folders.Find(FSocFile.FolderID);
        if (FSocFolderP<>nil) then begin
          FSocFileP:=Storage.Social.Files.Get(FSocFile.Name,FSocFolderP^.Files);
          qItem:=FSyncQueue.Find(Storage.Social.Sync.Queue.ItemKind.Create,FSocFileP);
          qItem.Id:=FSocFile.Id;
          FSocFile.Size:=qItem.Size;
          // Returned Digest is ZERO because SetData Computes that
          Core.Arrays.Bytes.Copy(qItem.Digest,FSocFile.Digest);
          Storage.Social.Files.Copy(FSocFile,FSocFileP^);
          if (FCoreCode=CO_STATUS_OK) then begin
            qItem.Status:=soVerified;
          end else begin
            qItem.Status:=soErrored;
          end;
        end else
          DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
      end else
        DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
    end else
      DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_FILE_READ) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FILE_READ';
    if FXMLDocument<>nil then begin
      Storage.Social.Files.fromXML(FXMLDocument,FSocFileReads);
      for iLcv:=0 to High(FSocFileReads) do begin
        FSocHdrP:=Storage.Social.Sync.Get(FSocFileReads[iLcv]^.NetworkID,FSocSyncList);
        if (FSocHdrP<>nil) then begin
          FSocSyncP:=FSocHdrP^.ItemP;
          FSocFolderP:=FSocSyncP^.Folders.Find(FSocFileReads[iLcv]^.FolderID);
          if (FSocFolderP<>nil) then begin
            FSocFileP:=Storage.Social.Files.Get(FSocFileReads[iLcv]^.Name,FSocFolderP^.Files);
            if FSocFileP<>nil then begin
              qItem:=Self.FSyncQueue.Find(ItemKind.Create,FSocFolderP^.Id,FSocFileP^.Name);
              if (qItem<>nil) then begin
                if (FCoreCode=CO_STATUS_OK) then begin
                  qItem.Status:=soVerified;
                  Storage.Social.Files.Copy(FSocFileReads[iLcv]^,FSocFileP^);
                end else begin
                  qItem.Status:=soErrored;
                end;
              end else begin
                qItem:=nil;
              end;
            end;
          end;
        end;
      end;
      Storage.Social.Files.Empty(FSocFileReads);
    end else
      DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_FILE_SETDATA) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FILE_SETDATA';
                            //  ?---0---&----1----&---2----&--3--|
    // Parameters from /core/soc?fls/ged&NetworkID&FolderID&FileId
    Storage.Social.Files.Empty(FSocFile);
    if (FXMLDocument<>nil) then begin
      Storage.Social.Files.FromXML(FXMLDocument,FSocFile);
      FSocHdrP:=Storage.Social.Sync.Get(FSocFile.NetworkID,FSocSyncList);
      if (FSocHdrP<>nil) then begin
        FSocSyncP:=FSocHdrP^.ItemP;
        FSocFolderP:=FSocSyncP^.Folders.Find(FSocFile.FolderID);
        if FSocFolderP<>nil then begin
          FSocFileP:=Storage.Social.Files.Get(FSocFile.ID,FSocFolderP^.Files);
          if FSocFileP<>nil then begin
            if FCoreCode=CO_STATUS_OK then begin
              qItem:=FSyncQueue.Find(ItemKind.Write,FSocFileP);
              if (qItem<>nil) then begin
                Storage.Social.Files.Copy(FSocFile,FSocFileP^);
                Core.Utils.Files.SetFileDateTime(qItem.LocalizedFile,FSocFileP^.Modified,Core.Utils.Time.FromUTCBias);
                qItem.Status:=soVerified;
              end else begin
                // not supposed to happen
                qItem:=nil;
              end;
            end else begin
              qItem:=nil;
            end;
          end else begin
            // the file was not found
            qItem:=nil;
          end;
        end else begin
          qItem:=nil;
        end;
      end else begin
        qItem:=nil;
      end;
    end else begin
      FSocFile.NetworkID:=FResponse.parameterAsQWord(1);
      FSocFile.FolderID:=FResponse.parameterAsQWord(2);
      FSocFile.ID:=FResponse.parameterAsQWord(3);
      FSocHdrP:=Storage.Social.Sync.Get(FSocFile.NetworkID,FSocSyncList);
      if (FSocHdrP<>nil) then begin
        FSocSyncP:=FSocHdrP^.ItemP;
        FSocFolderP:=FSocSyncP^.Folders.Find(FSocFile.FolderID);
        FSocFileP:=Storage.Social.Files.Get(FSocFile.Name,FSocFolderP^.Files);
        if FSocFileP<>nil then begin
          qItem:=FSyncQueue.Find(ItemKind.Write,FSocFileP);
          if (qItem<>nil) then begin
            if FCoreCode=CO_STATUS_OK then begin
              qItem.Status:=soVerified;
            end else begin
              qItem.Status:=soErrored;
            end;
          end else begin
              qItem:=nil;
          end;
        end else begin
            qItem:=nil;
        end;
      end;
    end;
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_FILE_GETDATA) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessSocial.NS_CORE_CMD_SOC_FILE_GETDATA';
                            //  ?---0---?----1----?---2----?--3--|
    // Parameters from /core/soc?fls/ged?NetworkID?FolderID?FileId
    Storage.Social.Files.Empty(FSocFile);
    FSocFile.NetworkID:=FResponse.parameterAsQWord(1);
    FSocFile.FolderID:=FResponse.parameterAsQWord(2);
    FSocFile.ID:=FResponse.parameterAsQWord(3);
    FSocHdrP:=Storage.Social.Sync.Get(FSocFile.NetworkID,FSocSyncList);
    if FSocHdrP<>nil then begin
      FSocSyncP:=FSocHdrP^.ItemP;
      FSocFolderP:=FSocSyncP^.Folders.Find(FSocFile.FolderID);
      FSocFileP:=Storage.Social.Files.Get(FSocFile.ID,FSocFolderP^.Files);
      if (FSocFileP<>nil) then begin
        qItem:=FSyncQueue.Find(ItemKind.Read,FSocFileP);
        if qItem<>nil then begin
          if FCoreCode=CO_STATUS_OK then begin
            Try
              SyncFileUpdated(FSocFileP,FResponse.Content);
            Except
              On e:Exception do OnException(EntryPoint,'SyncFileUpdated',E.Message);
            end;
            qItem.Status:=soVerified;
          end else begin
            qItem.Status:=soErrored;
          end;
        end else begin
          qItem:=nil;
        end;
      end else begin
        qItem:=nil;
      end;
    end;
  end else if SameText(FCoreCommand,NS_CORE_CMD_SOC_SYNC_READ) then begin
    if FXMLDocument<>nil then begin
      AddMethod(cmSyncReadSocial.Create(Self,FRSRP,FXMLDocument,FConNetworks,FNetworks,FSocSyncList));
    end else
      DoCoreError(FRSRP,CO_STATUS_ERR_CO_CMD_INVALID_XML);
  end;
end;

procedure ProcessVDM();
var
  iLcv:integer;
begin
  if SameText(FCoreCommand,NS_CORE_CMD_RC_LIST) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessVDM.NS_CORE_CMD_RC_LIST';
    if (FCoreCode=CO_STATUS_OK) and (FXMLDocument<>nil) then begin
      Storage.VDM.Resources.fromXML(FXMLDocument,FResources);
      if Assigned(FOnResourcesListed) then
        Synchronize(@SyncResourcesListed);
    end else begin
      DoCoreError(FRSRP,FCoreCode);
    end;
  end else if SameText(FCoreCommand,NS_CORE_CMD_RC_ADD) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessVDM.NS_CORE_CMD_RC_ADD';
    if FXMLDocument<>nil then begin
      Storage.VDM.Resources.fromXML(FXMLDocument,FResource);
      FResourceP:=Storage.VDM.Resources.getItem(FResource.ID,FResources);
      if FResourceP=nil then
        FResourceP:=Storage.VDM.Resources.Add(FResource,FResources);
      if Assigned(FOnResourceAdded) then
        Synchronize(@SyncResourceAdded);
    end else
      DoCoreError(FRSRP,FCoreCode);
  end else if SameText(FCoreCommand,NS_CORE_CMD_RC_WRITE) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessVDM.NS_CORE_CMD_RC_WRITE';
    if FXMLDocument<>nil then begin
      Storage.VDM.Resources.fromXML(FXMLDocument,FResource);
      FResourceP:=Storage.VDM.Resources.getItem(FResource.ID,FResources);
      if (FResourceP<>nil) and (FOnResourceWritten<>nil) then
        Synchronize(@SyncResourceWritten);
    end else
      DoCoreError(FRSRP,FCoreCode);
  end else if SameText(FCoreCommand,NS_CORE_CMD_RC_DEL) then begin
    EntryPoint:='uEngine.OnDataReceived.ProcessVDM.NS_CORE_CMD_RC_DEL';
    if FXMLDocument<>nil then begin
      Storage.VDM.Resources.fromXML(FXMLDocument,FResource);
      FResourceP:=Storage.VDM.Resources.getItem(FResource.ID,FResources);
      if (FResourceP<>nil) and Assigned(FOnResourceDeleted) then
        Synchronize(@SyncResourceDeleted);
      Storage.VDM.Resources.Delete(FResource,FResources);
    end else
      DoCoreError(FRSRP,FCoreCode);
  end;
end;

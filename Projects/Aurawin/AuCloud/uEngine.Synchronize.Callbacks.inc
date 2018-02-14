
procedure   TAuSocketMan.SyncAccountRetrieved();
begin
  FAuthenticated:=True;
  FOnAccountRetrieved(FAccount);
end;

procedure   TAuSocketMan.SyncSocSyncRetrieved();
begin
  FOnSocSyncRetrieved(FSocSyncP^);
end;

procedure   TAuSocketMan.SyncItemQueued();
begin
  FOnSyncItemQueued(FSyncQueue.SyncItem);
end;

procedure   TAuSocketMan.SyncItemRemoved();
begin
  FOnSyncItemRemoved(FSyncQueue.SyncItem);
end;

procedure   TAuSocketMan.DoCoreError(RSRP:PRSR; Code:Word);
begin
  If Assigned(FOnCoreError) then begin
    FCoreCode:=Code;
    Synchronize(@SyncCoreError);
  end;
  Core.Logging.Native.WriteLogEntry(
    SYSTEM_LOG,
    'TAuSocketMan.DoCoreError',
    Format(auLang.Table.Format.CoreError,[FCoreObject,FCoreCommand,Code])
  );
end;

function     TAuSocketMan.getSyncStatus:Core.Strings.VarString;
begin
  EnterCriticalSection(FStatusLock);
  Try
    Result:=FEngineStatus;
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

procedure     TAuSocketMan.setSyncStatus(Value:Core.Strings.VarString);
begin
  EnterCriticalSection(FStatusLock);
  Try
    FEngineStatus:=Value;
  finally
    LeaveCriticalSection(FStatusLock);
  end;
end;

function    TAuSocketMan.getSyncFileSize():QWord;
begin
  Result:=FSyncQueue.CalculatedFileSize;
end;

procedure   TAuSocketMan.SyncConnected();
begin
  FOnConnected(FRSRP)
end;

procedure   TAuSocketMan.SyncDisconnected();
begin
  FOnDisconnected(FRSRP)
end;


procedure   TAuSocketMan.SyncError();
begin
  FOnError(FRSRP)
end;

procedure   TAuSocketMan.SyncFinalized();
begin
  FOnFinalized(FRSRP);
end;

procedure   TAuSocketMan.SyncResourcesListed();
begin
  FOnResourcesListed(FResources);
end;

procedure   TAuSocketMan.SyncResourceAdded();
begin
  FOnResourceAdded(FResource);
end;

procedure   TAuSocketMan.SyncResourceWritten();
begin
  FOnResourceWritten(FResourceP^);
end;


procedure   TAuSocketMan.SyncResourceDeleted();
begin
  FOnResourceDeleted(FResource);
end;

procedure   TAuSocketMan.SyncNetworksListed();
begin
  FOnNetworksListed(FNetworks);
end;

procedure   TAuSocketMan.SyncConnectionsListed();
begin
  FOnConnectionsListed(FConnections,FConNetworks);
end;

procedure   TAuSocketMan.SyncCoreError();
begin
  FOnCoreError(FRSRP^,FCoreObject,FCoreCommand,FCoreCode);
end;


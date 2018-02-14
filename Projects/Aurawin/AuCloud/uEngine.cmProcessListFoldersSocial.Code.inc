constructor cmProcessListFoldersSocial.Create(aManager:TRSRManager; aRSRP:PRSR; var Document:TXMLDocument; var aHdr:Storage.Social.Sync.THeader);
begin
  FDocument:=Document;
  FHdrP:=@aHdr;
  FSyncP:=aHdr.ItemP;
  Document:=nil;
  Inherited Create(aManager,aRSRP);
end;

destructor cmProcessListFoldersSocial.Destroy();
begin
  if FDocument<>nil then
    FDocument.Free();
  inherited Destroy;
end;

procedure cmProcessListFoldersSocial.Execute();
var
  iLcv        : LongInt;
  auFldrP     : Storage.Social.Folders.PSFolder;

begin
  FProcessor.EntryPoint:='uEngine.cmProcessListFoldersSocial.Execute';
  EnterCriticalSection(FSyncP^.Lock);
  Try
    FSyncP^.Folders.Invalidate();
    FSyncP^.Folders.fromXML(FDocument);
    FSyncP^.Folders.Purge();

    FSyncP^.UpdateInfo.stateFolders:=rqsReceived;
    FSyncP^.UpdateInfo.stateFiles:=rqsNone;
    FSyncP^.UpdateInfo.LastChecked:=Core.Timer.dtUT;

  finally
    LeaveCriticalSection(FSyncP^.Lock);
  end;
end;



cmSyncDownloadUser=class(TRSRMethod)
private
  FPipeP   : Storage.VDM.Sync.Pipes.PItem;
  FSyncP   : Storage.VDM.Sync.PItem;
  FRequest : THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.VDM.Sync.TItem; var aPipe:Storage.VDM.Sync.Pipes.TItem); reIntroduce;
  destructor  Destroy(); override;
end;


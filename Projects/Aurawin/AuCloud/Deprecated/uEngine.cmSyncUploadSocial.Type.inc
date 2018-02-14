cmSyncUploadSocial=class(TRSRMethod)
private
  FPipeP   : Storage.Social.Sync.Pipes.PItem;
  FSyncP   : Storage.Social.Sync.PItem;
  FHdrP    : Storage.Social.Sync.PHeader;
  FRequest : THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.Social.Sync.TItem; var aPipe:Storage.Social.Sync.Pipes.TItem); reIntroduce;
  destructor  Destroy(); override;
end;


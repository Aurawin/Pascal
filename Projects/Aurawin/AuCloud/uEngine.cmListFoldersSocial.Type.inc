cmListFoldersSocial=class(TRSRMethod)
private
  FSyncP   : Storage.Social.Sync.PSync;
  FRequest : THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; aSyncP:Storage.Social.Sync.PSync); reIntroduce;
  destructor  Destroy(); override;
end;


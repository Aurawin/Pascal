cmWriteSyncsSocial=class(TRSRMethod)
private
  FRequest : THTTPRequest;
  FHdrP    : Storage.Social.Sync.PHeader;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; aHeaderP:Storage.Social.Sync.PHeader); reIntroduce;
  destructor  Destroy(); override;
end;


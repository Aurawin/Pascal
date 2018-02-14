cmReadSocialSync=class(TRSRMethod)
private
  FSyncP    : Storage.Social.Sync.PSync;
  FRequest  : THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.Social.Sync.TSync); reIntroduce;
  destructor  Destroy(); override;
end;


cmSyncChannelClose=class(TRSRMethod)
private
  FRequest:THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR); reIntroduce;
  destructor  Destroy(); override;
end;

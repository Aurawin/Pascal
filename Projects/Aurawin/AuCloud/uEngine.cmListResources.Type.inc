cmListResources=class(TRSRMethod)
private
  FRequest:THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create (aOwner:TRSRManager; aRSRP:PRSR); override;
  destructor  Destroy(); override;
end;

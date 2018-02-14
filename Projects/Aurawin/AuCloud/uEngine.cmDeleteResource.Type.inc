cmDeleteResource=class(TRSRMethod)
private
  FID  : QWord;
  FRequest:THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; aID:QWord); reIntroduce;
  destructor  Destroy; override;
end;


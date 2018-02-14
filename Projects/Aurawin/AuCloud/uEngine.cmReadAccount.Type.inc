cmReadAccount=class(TRSRMethod)
private
  FRequest: THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aOwner:TRSRManager; aRSRP:PRSR); reIntroduce;
  destructor  Destroy(); override;
end;


cmWriteResource=class(TRSRMethod)
private
  rcP  : Storage.VDM.Resources.PResource;
  FRequest : THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var RC:Storage.VDM.Resources.TResource); reIntroduce;
  destructor  Destroy(); override;
end;


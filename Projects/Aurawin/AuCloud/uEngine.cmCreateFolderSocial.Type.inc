cmCreateFolderSocial=class(TRSRMethod)
private
  FFolderP  : Storage.Social.Folders.PSFolder;
  FRequest  : THTTPRequest;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; aFolderP:Storage.Social.Folders.PSFolder); reIntroduce;
  destructor  Destroy(); override;
end;


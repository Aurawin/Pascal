cmProcessAllFilesSocial=class(TRSRMethod)
private
  FDocument    : TXMLDocument;
  FSyncP       : Storage.Social.Sync.PSync;
  FHdrP        : Storage.Social.Sync.PHeader;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var Document:TXMLDocument; var aHdr:Storage.Social.Sync.THeader); reIntroduce;
  destructor  Destroy(); override;
end;


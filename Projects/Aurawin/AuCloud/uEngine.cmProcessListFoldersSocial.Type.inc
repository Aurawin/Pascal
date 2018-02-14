cmProcessListFoldersSocial=class(TRSRMethod)
private
  FHdrP     : Storage.Social.Sync.PHeader;
  FSyncP    : Storage.Social.Sync.PSync;
  FDocument : TXMLDocument;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var Document:TXMLDocument; var aHdr:Storage.Social.Sync.THeader); reIntroduce;
  destructor  Destroy(); override;
end;


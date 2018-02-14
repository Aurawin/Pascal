cmSyncReadSocial=class(TRSRMethod)
private
  FXMLDocument:TXMLDocument;
  FSyncP:Storage.Social.Sync.PSync;
  FHdrP:Storage.Social.Sync.PHeader;
  FNetsP:Storage.Social.Network.PNetworks;
  FConsP:Storage.Social.Network.PNetworks;
  FListP:Storage.Social.Sync.PHeaderList;
  FNetP:Storage.Social.Network.PNetwork;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; var aDocument:TXMLDocument; var aCons,aNets:Storage.Social.Network.TNetworks; var aList:Storage.Social.Sync.THeaderList); reIntroduce;
  destructor Destroy; override;
end;


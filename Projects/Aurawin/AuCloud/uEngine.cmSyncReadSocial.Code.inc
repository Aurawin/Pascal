constructor cmSyncReadSocial.Create(aManager:TRSRManager; aRSRP:PRSR; var aDocument:TXMLDocument; var aCons,aNets:Storage.Social.Network.TNetworks;  var aList:Storage.Social.Sync.THeaderList);
begin
  FXMLDocument:=ADocument;
  ADocument:=nil;
  FSyncP:=nil;
  FHdrP:=nil;
  FListP:=@aList;
  FConsP:=@aCons;
  FNetsP:=@aNets;
  inherited Create(aManager,aRSRP);
end;

destructor cmSyncReadSocial.Destroy();
begin
  FXMLDocument.Free();
  inherited Destroy;
end;

procedure cmSyncReadSocial.Execute();
var
  Man             : TAuSocketMan;
  xSync           : TDOMNode;
  iNetworkID      : QWord;

begin
  FProcessor.EntryPoint:='uEngine.cmSyncReadSocial.Execute';
  Man:=TAuSocketMan(FOwner);
  with Core.XML.DB do begin
    xSync:=getNode(FXMLDocument,Storage.Social.Sync.XML.Stanza);

    iNetworkID:=toQword(xSync,Storage.Social.Sync.XML.Fields.NetworkID);
    FNetP:=Storage.Social.Network.Get(iNetworkID,FNetsP^);
    if (FNetP=nil) then
      FNetP:=Storage.Social.Network.Get(iNetworkID,FConsP^);
    if (FNetP<>nil) then begin
      FHdrP:=Storage.Social.Sync.Force(FNetP^,FListP^);
      if (FHdrP<>nil) then begin
        FSyncP:=FHdrP^.ItemP;
        Storage.Social.Sync.fromXML(FXMLDocument,FHdrP^,FSyncP);
        Man.FSocSyncP:=FSyncP;
        Man.FSocHdrP:=FHdrP;
        if Assigned(Man.FOnSocSyncRetrieved) then
          Man.Synchronize(@Man.SyncSocSyncRetrieved);
      end;
    end;
  end;
end;

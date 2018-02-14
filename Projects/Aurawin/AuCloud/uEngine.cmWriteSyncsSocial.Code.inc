constructor cmWriteSyncsSocial.Create(aManager:TRSRManager; aRSRP:PRSR; aHeaderP:Storage.Social.Sync.PHeader);
begin
  FHdrP:=aHeaderP;
  FRequest :=THTTPRequest.Create();
  inherited Create(aManager,aRSRP);
end;

destructor cmWriteSyncsSocial.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmWriteSyncsSocial.Execute();
var
  Man   : TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmWriteSyncsSocial.Execute';

  FProcessor.Refactor.Size:=0;
  Storage.Social.Sync.Pipes.toXML(FHdrP^.ItemP^.Pipes,FProcessor.Refactor,XML_HEADER_OFF);
  FHdrP^.Pipes:=Core.Streams.toString(FProcessor.Refactor);
  FProcessor.Refactor.Size:=0;

  FRequest.Reset();
  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_PUT;
  FRequest.ContentType:=RSR.HTTP.ctXML;

  Storage.Social.Sync.toXML(FHdrP^,FRequest.Data,XML_HEADER_ON);

  FRequest.ContentLength:=FRequest.Data.Size;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_SYNC_WRITE);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentType,FRequest.ContentType);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentLength,IntToStr(FRequest.ContentLength));

  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP); // Trip Flag

end;

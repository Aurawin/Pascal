constructor cmSyncChannelClose.Create(aManager:TRSRManager; aRSRP:PRSR);
begin
  FRequest:=THTTPRequest.Create();
  inherited Create(aManager,aRSRP);
end;

destructor cmSyncChannelClose.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmSyncChannelClose.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmSyncChannelClose.Execute';

  FRequest.Reset();
  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_PUT;
  FRequest.ContentType:=RSR.HTTP.ctXML;

  Storage.VDM.Sync.Channel.toXML(Man.Channel,FRequest.Data,XML_HEADER_ON);

  FRequest.ContentLength:=FRequest.Data.Size;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_VDM);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SYNC_CLOSE_CHANNEL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentType,FRequest.ContentType);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentLength,IntToStr(FRequest.ContentLength));
  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP); // Trip Flag
end;


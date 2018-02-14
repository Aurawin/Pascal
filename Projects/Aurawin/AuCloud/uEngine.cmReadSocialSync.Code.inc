constructor cmReadSocialSync.Create(aManager:TRSRManager; aRSRP:PRSR; var aSync:Storage.Social.Sync.TSync);
begin
  FSyncP:=@aSync;
  FRequest:=THTTPRequest.Create();
  Inherited Create(aManager,aRSRP);
end;

destructor cmReadSocialSync.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmReadSocialSync.Execute();
var
  Man:TAuSocketMan;
begin
  FProcessor.EntryPoint:='uEngine.cmReadSocialSync.Execute';
  Man:=TAuSocketMan(FOwner);

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_SOC;
  FRequest.Method:=RSR.HTTP.METHOD_PUT;
  FRequest.ContentType:=RSR.HTTP.ctXML;

  Storage.Social.Sync.toXML(FSyncP^.Header,FRequest.Data,XML_HEADER_ON);

  FRequest.ContentLength:=FRequest.Data.Size;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_SYNC_READ);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentType,FRequest.ContentType);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentLength,IntToStr(FRequest.ContentLength));

  FRequest.toBuffer(FRSRP^.SendBuffer);

  {$ifdef Buffer_Debug}
  Core.Logging.Native.WriteLogEntry(
    SYSTEM_LOG,
    FProcessor.EntryPoint,
    Core.Streams.toString(FRSRP^.SendBuffer.Stream)
  );
  {$endif}

  FRequest.Reset();
  Man.Send(FRSRP);
end;


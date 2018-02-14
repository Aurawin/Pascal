constructor cmListResources.Create(aOwner:TRSRManager; aRSRP:PRSR);
begin
  FRequest:=THTTPRequest.Create();
  inherited Create(aOwner,aRSRP);
end;

destructor cmListResources.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmListResources.Execute();
var
  Man:TAuSocketMan;
begin
  FProcessor.EntryPoint:='uEngine.cmListResources.Execute';
  Man:=TAuSocketMan(FOwner);

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_GET;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_VDM);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_RC_LIST);

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

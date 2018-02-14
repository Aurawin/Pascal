constructor cmListConnections.Create(aManager:TRSRManager; aRSRP:PRSR);
begin
  FRequest:=THTTPRequest.Create();
  inherited Create(aManager,aRSRP);
end;

destructor cmListConnections.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmListConnections.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmListConnections.Execute';

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_GET;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_CONNECTION_LIST);
  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP);
end;


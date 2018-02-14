constructor cmReadAccount.Create(aOwner:TRSRManager; aRSRP:PRSR);
begin
  FRequest:=THTTPRequest.Create();
  inherited Create(aOwner,aRSRP);
end;

destructor  cmReadAccount.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmReadAccount.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);

  FProcessor.EntryPoint:='uEngine.cmReadAccount.Execute';

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_VDM_ACCOUNT;
  FRequest.Method:=RSR.HTTP.METHOD_GET;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_VDM_ACCOUNT);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_ACCT_READ);

  FRequest.toBuffer(FRSRP^.SendBuffer);

  FRequest.Reset();
  Man.Send(FRSRP);
end;

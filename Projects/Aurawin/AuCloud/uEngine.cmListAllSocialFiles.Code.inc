constructor cmListAllSocialFiles.Create(aManager:TRSRManager; aRSRP:PRSR; aInfoP:Storage.Social.Sync.PSync);
begin
  FSyncP:=aInfoP;
  FSyncP^.UpdateInfo.stateFiles:=rqsRequested;
  FRequest:=THTTPRequest.Create();
  inherited Create(aManager,aRSRP);
end;

destructor cmListAllSocialFiles.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmListAllSocialFiles.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmListAllSocialFiles.Execute';

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_GET;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldSearch,IntToStr(FSyncP^.Network.ID));
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FILE_LIST_ALL);
  FRequest.toBuffer(FRSRP^.SendBuffer);
  Man.Send(FRSRP);
  FRequest.Reset();
end;

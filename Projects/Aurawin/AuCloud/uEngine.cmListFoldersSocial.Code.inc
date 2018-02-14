constructor cmListFoldersSocial.Create(aManager:TRSRManager; aRSRP:PRSR; aSyncP:Storage.Social.Sync.PSync);
begin
  FSyncP:=aSyncP;
  FSyncP^.UpdateInfo.stateFolders:=rqsRequested;
  FRequest:=THTTPRequest.Create();
  FSyncP^.UpdateInfo.stateFolders:=rqsRequested;
  FSyncP^.UpdateInfo.stateFiles:=rqsNone;

  inherited Create(aManager,aRSRP);
end;

destructor cmListFoldersSocial.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmListFoldersSocial.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  Man.EntryPoint:='uEngine.cmListFoldersSocial.Execute';

  FRequest.Reset();
  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_GET;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldSearch,IntToStr(FSyncP^.Network.ID));
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FLDRS_LIST);
  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP);
end;


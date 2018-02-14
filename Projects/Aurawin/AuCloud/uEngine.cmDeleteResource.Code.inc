constructor cmDeleteResource.Create(aManager:TRSRManager; aRSRP:PRSR; aID:QWord);
begin
  FID:=aID;
  FRequest:=THTTPRequest.Create();
  Inherited Create(aManager,aRSRP);
end;

destructor cmDeleteResource.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmDeleteResource.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmDeleteResource.Execute';

  FRequest.Reset();

  Storage.VDM.Resources.Empty(Man.FResource);
  Man.FResource.ID:=FID;


  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_PUT;
  FRequest.ContentType:=RSR.HTTP.ctXML;
  Storage.VDM.Resources.toXML(Man.FResource,FRequest.Data,XML_HEADER_ON);
  FRequest.ContentLength:=FRequest.Data.Size;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_VDM);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_RC_DEL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentType,FRequest.ContentType);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentLength,IntToStr(FRequest.ContentLength));
  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP);
end;


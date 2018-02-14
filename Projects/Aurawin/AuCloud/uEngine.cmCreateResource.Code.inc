constructor cmCreateResource.Create(aManager:TRSRManager; aRSRP:PRSR; var RC:Storage.VDM.Resources.TResource);
begin
  rcP:=@RC;
  FRequest:=THTTPRequest.Create();
  Inherited Create(aManager,aRSRP);
end;

destructor cmCreateResource.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmCreateResource.Execute();
var
  Man:TAuSocketMan;
begin
  FProcessor.EntryPoint:='uEngine.CreateResource.Execute';
  Man:=TAuSocketMan(FOwner);

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_PUT;
  FRequest.ContentType:=RSR.HTTP.ctXML;
  Storage.VDM.Resources.toXML(rcP^,FRequest.Data,XML_HEADER_ON);
  FRequest.ContentLength:=FRequest.Data.Size;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_VDM);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_RC_ADD);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentType,FRequest.ContentType);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentLength,IntToStr(FRequest.ContentLength));
  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP);
end;


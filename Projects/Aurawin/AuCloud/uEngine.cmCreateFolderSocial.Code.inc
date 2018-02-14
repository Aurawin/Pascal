constructor cmCreateFolderSocial.Create(aManager:TRSRManager; aRSRP:PRSR; aFolderP:Storage.Social.Folders.PSFolder);
begin
  FFolderP:=aFolderP;
  FRequest:=THTTPRequest.Create;
  Inherited Create(aManager,aRSRP);
end;

destructor cmCreateFolderSocial.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmCreateFolderSocial.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmCreateFolderSocial.Execute';

  FRequest.Reset();

  FRequest.Close:=False;
  FRequest.URI:=URI_VDM;
  FRequest.Method:=RSR.HTTP.METHOD_PUT;
  FRequest.ContentType:=RSR.HTTP.ctXML;
  Storage.Social.Folders.toXML(FFolderP^,FRequest.Data,XML_HEADER_ON);
  FRequest.ContentLength:=FRequest.Data.Size;

  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FLDRS_ADD);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentType,FRequest.ContentType);
  Core.Arrays.KeyString.Update(FRequest.Headers,fieldContentLength,IntToStr(FRequest.ContentLength));
  FRequest.toBuffer(FRSRP^.SendBuffer);
  FRequest.Reset();
  Man.Send(FRSRP);
end;

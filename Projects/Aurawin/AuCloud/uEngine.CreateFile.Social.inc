function   TAuSocketMan.CreateFile(Var SR:TRSR; Request:THTTPRequest; var Item:Storage.Social.Files.TSFile):Int64;
begin
  Result:=0;
  Request.Reset();
  Request.Close:=False;
  Request.URI:=URI_VDM;
  Request.Method:=RSR.HTTP.METHOD_PUT;
  Request.ContentType:=RSR.HTTP.ctXML;
  Storage.Social.Files.toXML(Item,Request.Data,Request.Refactor,XML_HEADER_ON);
  Request.ContentLength:=Request.Data.Size;

  Core.Arrays.KeyString.Update(Request.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(Request.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(Request.Headers,fieldResourceID,IntToStr(Settings.ResourceID));
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FILE_ADD);
  Core.Arrays.KeyString.Update(Request.Headers,fieldContentType,Request.ContentType);
  Core.Arrays.KeyString.Update(Request.Headers,fieldContentLength,IntToStr(Request.ContentLength));

  Result:=Request.toBuffer(SR.SendBuffer);
  Send(@SR);

  Request.Reset();
end;

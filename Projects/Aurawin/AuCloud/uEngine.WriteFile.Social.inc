
function   TAuSocketMan.WriteFile(Var SR:TRSR; Request:THTTPRequest; var Item:Storage.Social.Files.TSFile; Data:TFileStream):Int64;
begin
  Result:=0;
  Request.Reset();

  Request.Close:=False;
  Request.URI:=URI_SOC;
  Request.Method:=RSR.HTTP.METHOD_PUT;
  Request.ContentType:=RSR.HTTP.ctXML;

  Storage.Social.Files.toXML(Item,Request.Data,Request.Refactor,XML_HEADER_ON);

  Request.ContentLength:=Request.Data.Size;

  Core.Arrays.KeyString.Update(Request.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(Request.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FILE_WRITE);
  Core.Arrays.KeyString.Update(Request.Headers,fieldContentType,Request.ContentType);
  Core.Arrays.KeyString.Update(Request.Headers,fieldContentLength,IntToStr(Request.ContentLength));

  Result:=Request.toBuffer(SR.SendBuffer);
  Send(@SR);

  Request.Reset();
  Request.Close:=False;
  Request.URI:=URI_SOC;
  Request.Method:=RSR.HTTP.METHOD_PUT;
  Request.ContentType:=RSR.HTTP.ctStream;
  SetLength(Request.Parameters,4);
  Request.Parameters[0]:=NS_CORE_CMD_SOC_FILE_SETDATA_PARAM;
  Request.Parameters[1]:=IntToStr(Item.NetworkID);
  Request.Parameters[2]:=IntToStr(Item.FolderID);
  Request.Parameters[3]:=IntToStr(Item.ID);
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?---2----?--3---|
  // Parameters from /core/soc?fls/sed?NetworkID?FolderID?FileId

  Core.Streams.Copy(Data,Request.Data);
  Request.ContentLength:=Request.Data.Size;

  Core.Arrays.KeyString.Update(Request.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(Request.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(Request.Headers,fieldResourceID,IntToStr(Settings.ResourceID));
  Core.Arrays.KeyString.Update(Request.Headers,fieldContentType,Request.ContentType);
  Core.Arrays.KeyString.Update(Request.Headers,fieldContentLength,IntToStr(Request.ContentLength));
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FILE_SETDATA);

  Result:=Request.toBuffer(SR.SendBuffer);
  Send(@SR);

  Request.Reset();
end;


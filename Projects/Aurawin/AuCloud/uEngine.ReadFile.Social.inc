function   TAuSocketMan.ReadFile(Var SR:TRSR; Request:THTTPRequest; var Item:Storage.Social.Files.TSFile):Int64;
begin
  Result:=0;
  Request.Reset();
  Request.Close:=False;
  Request.URI:=NS_CORE_OBJ_SOCIAL;

  Request.AddParameter(NS_CORE_CMD_SOC_FILE_GETDATA_PARAM);
  Request.AddParameter(IntToStr(Item.NetworkID));
  Request.AddParameter(IntToStr(Item.FolderID));
  Request.AddParameter(IntToStr(Item.ID));
  Request.Method:=RSR.HTTP.METHOD_GET;

  Core.Arrays.KeyString.Update(Request.Headers,fieldAuth,Settings.Auth);
  Core.Arrays.KeyString.Update(Request.Headers,fieldAccount,Settings.Username);
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreObject,NS_CORE_OBJ_SOCIAL);
  Core.Arrays.KeyString.Update(Request.Headers,fieldCoreCommand,NS_CORE_CMD_SOC_FILE_GETDATA);

  Result:=Request.toBuffer(SR.SendBuffer);
  Send(@SR);

  Request.Reset();
end;


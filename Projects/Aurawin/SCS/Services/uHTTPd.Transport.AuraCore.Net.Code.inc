constructor TTransportAuraCore.Create(aManager:THTTPManager; aRequest:THTTPRequest; aResponse:THTTPResponse);
begin
  FWebSocket:=TWebSocketHandler.Create(aManager,aRequest,aResponse);
  FAccounts:=aManager.FUserAccounts;
  Inherited Create('AuraCore.Net',@aManager.Owner.RootDomain,aManager.Owner.FResources,aManager,@aManager.Owner.MatrixNode,aRequest.Data,aManager.FCoreObjects,@aManager.Owner.FKeywords);
end;

destructor  TTransportAuraCore.Destroy;
begin
  FAccounts:=nil;
  FreeAndNil(FWebSocket);
  Inherited Destroy;
end;

procedure   TTransportAuraCore.OnCoreObjectRequestAuth(CommandP:PCoreCommand; Var SR:TRSR; sTitle,sPrompt:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectRequestAuth';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FWebSocket.FData.Clear;
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldID,IntToStr(FID));
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldCode,IntToStr(CO_STATUS_AUTHENTICATE));
  Core.Arrays.KeyString.toStream(FWebSocket.Headers,FWebSocket.FData);
  Core.Streams.Write(RSR.HTTP.HTTP_HEADER_SEP,RSR.HTTP.HTTP_HEADER_SEP_LEN,FWebSocket.FData);
  Core.Streams.WriteLine(sTitle,FWebSocket.FData);
  Core.Streams.WriteLine(sPrompt,FWebSocket.FData);
  FWebSocket.FFrame.toBuffer(wsBinary,SR.SendBuffer,FManager.Refactor);
  FManager.Send(@SR);
end;

procedure   TTransportAuraCore.OnCoreObjectError(CommandP:PCoreCommand; Var SR:TRSR; Error:WORD);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectError';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FWebSocket.FData.Clear();
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldID,IntToStr(FID));
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldCode,IntToStr(Error));
  Core.Arrays.KeyString.toStream(FWebSocket.Headers,FWebSocket.FData);
  Core.Streams.Write(RSR.HTTP.HTTP_HEADER_SEP,RSR.HTTP.HTTP_HEADER_SEP_LEN,FWebSocket.FData);
  FWebSocket.FFrame.toBuffer(wsBinary,SR.SendBuffer,FManager.Refactor);
  FManager.Send(@SR);
end;

procedure   TTransportAuraCore.OnCoreObjectSuccess(CommandP:PCoreCommand; Var SR:TRSR; Code:Word);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectSuccess';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldCode,Code);
  FWebSocket.FData.Clear();
  Core.Arrays.KeyString.toStream(FWebSocket.Headers,FWebSocket.FData);
  Core.Streams.Write(RSR.HTTP.HTTP_HEADER_SEP,RSR.HTTP.HTTP_HEADER_SEP_LEN,FWebSocket.FData);
  FWebSocket.FData.CopyFrom(FOutput,0);
  FWebSocket.FFrame.toBuffer(wsBinary,SR.SendBuffer,FManager.Refactor);
  FManager.Send(@SR);
end;

procedure   TTransportAuraCore.OnCoreObjectRedirect(CommandP:PCoreCommand; Var SR:TRSR; URL:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectRedirect';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  {
  Legacy HTTP Transport
  FResponse.ContentType:=ctHTML;
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_FOUND;
  FResponse.Status:='Redirection';

  URL:=Concat(HTTP[FResponse.Secure],'://',FRequest.Host,URL);
  Core.Arrays.KeyString.Update(FResponse.Headers,'Location',URL);
  FResponse.Content:=hHttpd.ErrorPage(
    'Automatic Redirection',
    'Redirection',
    Format('The proper location is %s',[URL])
  );
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Empty;
  FManager.FLogItem.StatusCode:=HTTP_SC_FOUND;
  }
end;


function    TTransportAuraCore.OnCoreObjectCheckCredentials(CommandP:PCoreCommand; Var SR:TRSR; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectCheckCredentials.Authorized';
  DataP:=SR.Info.DataP;
  if Storage.UserAccounts.Items.DB.Authorized(FManager.Task,FDomainP^.ID,DataP^.UAP,FAccounts,DataP^.AuthCount, DataP^.Account,DataP^.Auth, SR.Credentials) then begin
    FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectCheckCredentials.UserAccount.Fill';
    UserP:=SR.Credentials;
    if Storage.UserAccounts.Items.DB.Fill(FManager.Task,UserP^.ID,UserP^) then begin
      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
  end;
end;

procedure   TTransportAuraCore.OnCoreObjectNotFound(Var SR:TRSR; NameSpace:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectNotFound';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FWebSocket.FData.Clear;
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldID,IntToStr(FID));
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldCode,IntToStr(FCoreObjects.LastError));
  Core.Arrays.KeyString.toStream(FWebSocket.Headers,FWebSocket.FData);
  Core.Streams.Write(RSR.HTTP.HTTP_HEADER_SEP,RSR.HTTP.HTTP_HEADER_SEP_LEN,FWebSocket.FData);
  FWebSocket.FFrame.toBuffer(wsBinary,SR.SendBuffer,FManager.Refactor);
  FManager.Send(@SR);
end;

procedure   TTransportAuraCore.OnCoreObjectCommandNotFound(Var SR:TRSR; nsObject,nsCommand:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnCoreObjectCommandNotFound';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FWebSocket.FData.Clear;
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldID,IntToStr(FID));
  Core.Arrays.KeyString.Update(FWebSocket.Headers,fieldCode,IntToStr(FCoreObjects.LastError));
  Core.Arrays.KeyString.toStream(FWebSocket.Headers,FWebSocket.FData);
  Core.Streams.Write(RSR.HTTP.HTTP_HEADER_SEP,RSR.HTTP.HTTP_HEADER_SEP_LEN,FWebSocket.FData);
  FWebSocket.FFrame.toBuffer(wsBinary,SR.SendBuffer,FManager.Refactor);
  FManager.Send(@SR);
end;

procedure   TTransportAuraCore.OnBadRequest(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnBadRequest';
end;

procedure   TTransportAuraCore.OnInvalidMethod(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnInvalidMethod';
end;

procedure   TTransportAuraCore.OnNotFound(Var SR:TRSR; Title,Content:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.OnNotFound';
end;

function    TTransportAuraCore.ResolvePath(var URI: Core.Strings.VarString; var Folder:TDSFolder; var Resource:TDSFile; Var Path,FileName,Ext: Core.Strings.VarString; Refactor:TStream): TResolveResult;
var
  sNewURI:Core.Strings.VarString;
  iLength,iCount:LongInt;
  saPath:Core.Arrays.Types.VarString;
begin
  FManager.EntryPoint:='TTransportAuraCore.ResolvePath';
  sNewURI:=URI;
  Resource:=nil; Folder:=nil;
  iLength:=Length(sNewURI);
  if (iLength=0) or ((iLength>0) and (sNewURI[iLength]='/')) then begin
    sNewURI:=Concat(sNewURI,'index.html');
    iLength:=Length(sNewURI);
  end;
  if (iLength>0) and (sNewURI[1] = '/') then System.Delete(sNewURI,1,1);
  Core.Arrays.VarString.fromString(@saPath,sNewURI,'/',[soClearList,soIgnoreDelimAtStart]);
  Try
    iCount:=Length(saPath);
    FileName:=saPath[iCount-1];
    Ext:=Core.Utils.Files.Extract(FileName,efeoNone);
    if (Length(Ext)>0) then begin
      Core.Arrays.VarString.Remove(saPath,iCount-1);
      Dec(iCount);
    end;
    if (iCount>0) and SameText(saPath[0],RequiredFolders[RFI_CORE]) then begin
      Path:=Core.Arrays.VarString.toString(saPath,'/',Refactor);
      Folder:=FFAT.Acquire(Path,Refactor) // Core objects?
    end else begin
      Path:=Concat(RequiredFolders[RFI_WEB],'/',Core.Arrays.VarString.toString(saPath,'/',Refactor));
      Folder:=FFAT.Acquire(Path,Refactor);
    end;
  Finally
    Core.Arrays.VarString.Done(saPath);
  End;
  if (Folder<>nil) then begin
    Resource:=FFAT.Acquire(Folder,FileName);
    If (Resource=nil) then begin
      If (SysUtils.SameText(Folder.Name,FileName)) then begin
        Result:=rrFound;
        iLength:=Length(URI);
        SetLength(URI,iLength+1);
        URI[iLength+1]:='/';
      end else
        Result:=rrNotFound;
    end else
      Result:=rrOK;
  end else
    Result:=rrNotFound;
end;


procedure   TTransportAuraCore.OnDataReceived(Var SR:TRSR; var Handled:boolean);

  procedure PushData;
  var
    iCount:LongInt;
    sNS:Core.Strings.VarString;
  begin
    FManager.EntryPoint:='TTransportAuraCore.OnDataReceived.PushData';
    iCount:=Core.Streams.Pos(FWebSocket.FData,RSR.HTTP.HTTP_HEADER_SEP);
    if iCount>0 then begin
      Handled:=(FWebSocket.Frame.More=false) and  (FWebSocket.Frame.Error=false);
      Core.Arrays.KeyString.fromString(@FWebSocket.Headers, Core.Streams.Extract(FWebSocket.FData,0,iCount));
      // Refactor Data Stream now
      Core.Streams.Refactor(FWebSocket.FData,FManager.Refactor,iCount+RSR.HTTP.HTTP_HEADER_SEP_LEN);


      iCount:=System.Length(FWebSocket.Headers);

      FCoreObject.NameSpace:=Core.Arrays.KeyString.GetItemByKey(FWebSocket.Headers,RSR.Core.NameSpace.Field.co.NameSpace,iCount);
      FCoreObject.Path:=Core.Utils.Files.Extract(FCoreObject.NameSpace,epoAllButName,SCS_PATH_DELIM);
      FCoreObject.Name:=Core.Utils.Files.Extract(FCoreObject.NameSpace,epoName,SCS_PATH_DELIM);

      FCoreCommand.NameSpace:=Core.Arrays.KeyString.GetItemByKey(FWebSocket.Headers,RSR.Core.NameSpace.Field.co.cmd.NameSpace,iCount);
      FCoreCommand.Path:=Core.Utils.Files.Extract(FCoreCommand.NameSpace,epoAllButName,SCS_PATH_DELIM);
      FCoreCommand.Name:=Core.Utils.Files.Extract(FCoreCommand.NameSpace,epoName,SCS_PATH_DELIM);

      FResource.NameSpace:=Core.Arrays.KeyString.GetItemByKey(FWebSocket.Headers,RSR.Core.NameSpace.Field.Resource,iCount);
      FResource.Path:=Core.Utils.Files.Extract(FResource.NameSpace,epoAllButName,SCS_PATH_DELIM);
      FResource.Name:=Core.Utils.Files.Extract(FResource.NameSpace,epoName,SCS_PATH_DELIM);

      FID:=Core.Arrays.KeyString.GetItemAsInt64(FWebSocket.Headers,fieldID,iCount,0);
      DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(FWebSocket.Headers,fieldAuth,iCount);
      DataP^.Account:=Core.Arrays.KeyString.GetItemByKey(FWebSocket.Headers,fieldAccount,iCount);

      GetResponse(SR);

    end;
    if (FWebSocket.Frame.Error) then
      RSR.Empty(SR.RecvBuffer);

  end;

  procedure PushClose;
  begin
    FManager.Close(@SR);
  end;

  procedure PushPing;
  begin
    FWebSocket.Frame.toBuffer(wsPong,SR.SendBuffer,FManager.Refactor);
  end;

begin
  FManager.EntryPoint:='TTransportAuraCore.OnDataReceived';
  Handled:=true;
  DataP:=SR.Info.DataP;
  Try
    FWebSocket.Reset();
    FManager.EntryPoint:='TTransportAuraCore.OnDataReceived.FFrame.fromBuffer';

    If FWebSocket.FFrame.fromBuffer(SR.RecvBuffer,FWebSocket.FData,FManager.Refactor) then begin
      case FWebSocket.Frame.Mode of
        wsText,wsBinary : PushData;
     // wsContinue      : PushContinue;
        wsClose         : PushClose;
        wsPing          : PushPing;
     // wsPong          : PushPong;
      end;
    end else begin
       Include(SR.Errors,eReceive);
    end;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportAuraCore.OnDataEventWebSocket.fromBuffer Exception: ',E.Message));
  end;
end;

procedure   TTransportAuraCore.Redirect(Var SR:TRSR; URL:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.Redirect';
end;

procedure   TTransportAuraCore.RequestPassword(Var SR:TRSR; Title,Prompt:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.RequestPassword';
end;

procedure   TTransportAuraCore.SendContent(Var SR:TRSR; Content:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.SendContent';
end;

procedure   TTransportAuraCore.SendFile(Var SR:TRSR; Folder:TDSFolder; Resource:TDSFile; Var sExt:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportAuraCore.SendFile';
end;

procedure   TTransportAuraCore.GetResponse(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportAuraCore.GetResponse';
  FCoreObjects.Execute(SR,Self,FWebSocket.Parameters,FWebSocket.Headers,FWebSocket.Headers,FWebSocket.Data);
end;

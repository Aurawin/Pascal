constructor TTransportMedia.Create(aManager:THTTPManager);
begin
  FRequest:=THTTPRequest.Create;
  FResponse:=THTTPResponse.Create();
  FResponse.Server:=Server_Header;
  FAccounts:=aManager.FUserAccounts;
  FWebSocket:=TWebSocketHandler.Create(aManager,FRequest,FResponse);
  FResponse.Host:=aManager.Owner.RootDomain.Name;
  Inherited Create('HTTP',@aManager.Owner.RootDomain,aManager.Owner.FResources,aManager,@aManager.Owner.MatrixNode,FRequest.Data,aManager.FCoreObjects,@aManager.Owner.FKeywords);
end;

destructor  TTransportMedia.Destroy;
begin
  FManager:=nil;
  FAccounts:=nil;
  FreeAndNil(FWebSocket);
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  Inherited Destroy;
end;

procedure   TTransportMedia.OnCoreObjectRequestAuth(CommandP:PCoreCommand; Var SR:TRSR; sTitle,sPrompt:Core.Strings.VarString);
begin
  RequestPassword(SR,'Access to this core object requires authentication','Please provide your username and password.');
end;

procedure   TTransportMedia.OnCoreObjectError(CommandP:PCoreCommand; Var SR:TRSR; Error:WORD);
begin
  FManager.EntryPoint:='TTransportMedia.OnCoreObjectError';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.Close:=FRequest.Close;
  FResponse.ContentType := ctHtml;
  FResponse.Code:=HTTP_SC_EXPECTATIONFAILED;
  FResponse.Status:='Expectation Failure';
  Core.Arrays.KeyString.Update(FResponse.Headers,fieldCode,IntToStr(Error));
  FResponse.AddToContent(hHttpd.ErrorPage(
    'Core Object Error',
    Format('Core Object "%s" has encountered an error [%.0d] in command [%s]',[CommandP^.HeaderP^.CLSInfo^.Name,Error,CommandP^.ACLInfo^.Name]),
    Format(
      #13#10+
      'CoreObject-Location: %s'#13#10+
      'CoreObject-Name: %s'#13#10+
      'CoreObject-Enabled: %s'#13#10+
      'CoreObject-Scale: %s'#13#10+

      'CoreObject-ACL-Name: %s'#13#10+
      'CoreObject-ACL-NameSpace: %s'#13#10+
      'CoreObject-ACL-Description: %s'#13#10 +

      'CoreCommand-ACL-Name: %s'#13#10+
      'CoreCommand-ACL-NameSpace: %s'#13#10+
      'CoreCommand-ACL-Description: %s'#13#10+
      'CoreCommand-Enabled: %s'#13#10+
      'CoreCommand-Anonymous: %s'#13#10 +
      'CoreCommand-XML: %s'#13#10
      ,
      [
        CommandP^.HeaderP^.CLSInfo^.Location,
        CommandP^.HeaderP^.CLSInfo^.Name,
        Yes_No[CommandP^.HeaderP^.Enabled],
        IntToStr(CommandP^.HeaderP^.Scale),

        CommandP^.HeaderP^.ACLInfo^.Name,
        CommandP^.HeaderP^.ACLInfo^.NameSpace,
        CommandP^.HeaderP^.ACLInfo^.Description,

        CommandP^.ACLInfo^.Name,
        CommandP^.ACLInfo^.NameSpace,
        CommandP^.ACLInfo^.Description,
        Yes_No[CommandP^.Enabled],
        Yes_No[CommandP^.Anonymous],
        Yes_No[CommandP^.XMLInfo^.Enabled]
      ]
    )
  ));
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

procedure   TTransportMedia.OnCoreObjectSuccess(CommandP:PCoreCommand; Var SR:TRSR; Code:Word);
begin
  FManager.EntryPoint:='TTransportMedia.OnCoreObjectSuccess';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.ContentType:=Core.Arrays.KeyString.GetItemAsString(FResponse.Headers,fieldContentType,0,FResponse.ContentType);
  if (CommandP^.Cache) and (FCacheRequest=FCacheResponse)  then begin
    FResponse.Close:=FRequest.Close;
    FResponse.Code:=HTTP_SC_NOTMODIFIED;
    FResponse.Status:='Not Modified';
    FResponse.Send(@SR,FManager.Refactor);
  end else if RSR.HTTP.IndexOf(FResponse.ContentType,ctMedia)<>-1 then begin
    // First time through setup streaming media
    DataP:=SR.Info.DataP;
    MediaP:=DataP^.Media;
    if MediaP<>nil then begin
      Empty(MediaP^);
    end else begin
      New(MediaP);
      RSR.HTTP.Init(MediaP^);
      DataP^.Media:=MediaP;
    end;
    Output.Position:=0;
    MediaP^.ContentType:=FResponse.ContentType;
    MediaP^.ContentLength:=Output.Size;
    MediaP^.Content.CopyFrom(Output,Output.Size);
    MediaP^.URI:=FRequest.URI;

    Core.Arrays.KeyString.Update(FResponse.Headers,fieldAcceptRanges,'bytes');
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldCode,Code);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldHost,FResponse.Host);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldContentType,MediaP^.ContentType);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldContentLength,IntToStr(MediaP^.ContentLength));


    FResponse.Code:=HTTP_SC_OK;
    FResponse.Status:='OK';
    FResponse.Send(@SR,FManager.Refactor);

    SR.Transport:=THTTPManager(FManager).FTransportMedia;
    Output.Size:=0;
    // Note no response coming from this query!
  end else begin;
    // Don't change content type
    FResponse.Code:=HTTP_SC_OK;
    FResponse.Status:='OK';
    if Output.Size>0 then begin
      if CommandP^.Compress then begin
        Try
          FManager.Refactor.Size:=0;
          Output.Position:=0;
          Deflate:=TDeflater.Create(Output,FManager.Refactor,COMPRESS_BUFFER);
          Deflate.Compress(Encryption.zStream.clmax);
          Output.Size:=0;
        finally
          FreeAndNil(Deflate);
        end;
        FResponse.AddToContent(FManager.Refactor);
        FManager.Refactor.Size:=0;
        Core.Arrays.KeyString.Update(FResponse.Headers,fieldContentEncoding,RSR.HTTP.Deflate);
      end else begin
        FResponse.SetContent(Output);
      end;
    end;
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldCode,Code);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldHost,FResponse.Host);
    FResponse.Send(@SR,FManager.Refactor);
  end;
  FResponse.Reset();
end;

procedure   TTransportMedia.OnCoreObjectRedirect(CommandP:PCoreCommand; Var SR:TRSR; URL:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportMedia.OnCoreObjectRedirect';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.ContentType:=ctHTML;
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_FOUND;
  FResponse.Status:='Redirection';
  URL:=Concat(HTTP[FResponse.Secure],'://',FRequest.Host,URL);
  Core.Arrays.KeyString.Update(FResponse.Headers,'Location',URL);
  FResponse.AddToContent(hHttpd.ErrorPage(
    'Automatic Redirection',
    'Redirection',
    Format('The proper location is %s',[URL])
  ));
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

function    TTransportMedia.OnCoreObjectCheckCredentials(CommandP:PCoreCommand; Var SR:TRSR; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
begin
  FManager.EntryPoint:='TTransportMedia.OnCoreObjectCheckCredentials.Authorized';
  DataP:=SR.Info.DataP;
  UserP:=SR.Credentials;
  DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAuth);
  DataP^.Account:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAccount);

  if (System.Length(DataP^.Auth)=0) then
    DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(FRequest.Cookies,fieldAuth);
  if (System.Length(DataP^.Account)=0) then
    DataP^.Account:=Core.Arrays.KeyString.GetItemByKey(FRequest.Cookies,fieldAccount);

  if Storage.UserAccounts.Items.DB.Authorized(FManager.Task,FDomainP^.ID,DataP^.UAP,FAccounts,DataP^.AuthCount, DataP^.Account,DataP^.Auth, UserP) then begin
    Result:=CO_STATUS_OK;
    SR.Credentials:=UserP;
    Core.Arrays.KeyString.Update(respHeaders,fieldAccount,UserP^.User);
    Core.Arrays.KeyString.Update(respHeaders,fieldAuth,UserP^.Auth);
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
  end;
end;

procedure TTransportMedia.RequestPassword(Var SR:TRSR; Title,Prompt:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportMedia.RequestPassword';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.Close:=FRequest.Close;
  FResponse.ContentType := ctHtml;
  If Length(Title)=0 then
    Title:='Please provide credentials';
  If Length(Prompt)=0 then
    Prompt:='Please enter username/password';
  FResponse.AddToContent(Format('<html><head><title>%s</title></head><body>%s</body></html>',[Title,Prompt]));
  FResponse.Code:=HTPP_SC_UNAUTHORIZED;
  FResponse.Status:='Unauthorized';
  Core.Arrays.KeyString.Update(FResponse.Headers,'WWW-Authenticate','Basic');
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;


procedure   TTransportMedia.OnCoreObjectNotFound(Var SR:TRSR; NameSpace:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportMedia.OnCoreObjectNotFound';
  OnNotFound(SR,'Core Object Not Found',Format('The Core Object "%s" was not found.  Check to make sure that the requested object is injected into the server core at startup.',[NameSpace]));
end;

procedure   TTransportMedia.OnCoreObjectCommandNotFound(Var SR:TRSR; nsObject,nsCommand:Core.Strings.VarString);
begin
  OnNotFound(SR,'Core Object Command Not Found',Format('The Core Object (%s) Command "%s" was not found.  Check to make sure that the version expected is injected into the server core at startup.',[nsObject,nsCommand]));
end;

procedure   TTransportMedia.Redirect(Var SR:TRSR; URL:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportMedia.Redirect';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.ContentType:=ctHTML;
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_FOUND;
  FResponse.Status:='Redirection';

  URL:=Concat(HTTP[FResponse.Secure],'://',FRequest.Host,URL);
  Core.Arrays.KeyString.Update(FResponse.Headers,'Location',URL);
  FResponse.AddToContent(hHttpd.ErrorPage(
    'Automatic Redirection',
    'Redirection',
    Format('The proper location is %s',[URL])
  ));
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

procedure TTransportMedia.OnNotFound(Var SR:TRSR; Title,Content:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportMedia.OnNotFound';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.ContentType:=ctHTML;
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_NOTFOUND;
  FResponse.Status:='Not found';
  FResponse.AddToContent(hHttpd.ErrorPage(
    Title,
    'Not found',
    Content
  ));
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

procedure TTransportMedia.SendContent(Var SR:TRSR; Content:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportMedia.SendContent';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_OK;
  FResponse.Status:='OK';
  FResponse.ContentType:=ctHtml;
  FResponse.SetContent(Content);
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

function    TTransportMedia.ResolvePath(Var SR:TRSR; var URI: Core.Strings.VarString): TResolveResult;
begin
  FManager.EntryPoint:='TTransportMedia.ResolvePath';
  //Result:=rrOK;
  Result:=rrNotFound;
  DataP:=SR.Info.DataP;
  MediaP:=DataP^.Media;
  if (MediaP<>nil) and SameText(URI,MediaP^.URI) then
    Result:=rrOK;
end;

procedure TTransportMedia.GetResponse(Var SR:TRSR);
var
  RR          : TResolveResult;
  iIndex      : LongInt;
begin
  FManager.EntryPoint:='TTransportMedia.GetResponse';
  FCacheRequest:=0; FCacheResponse:=0;
  iIndex:=Core.Arrays.KeyString.IndexOf(@FRequest.Headers,fieldIfModifiedSince);
  If iIndex<>-1 then
    FCacheRequest:=Core.Utils.Time.toGMTTime(FRequest.Headers[iIndex]^.Value);
  Try RR:=ResolvePath(SR,FRequest.URI); Except on E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.GetResponse.ResolvePath Exception: ',FRequest.URI,' ',E.Message)); end;
  Case RR of
    rrNotFound    : Try OnNotFound(SR,'Resource Not Found',Format('The requested resource %s was not found.',[FRequest.URI])); Except On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.GetResponse.OnNotFound Exception: ',E.Message)); end;
    rrOK          : Try StreamMedia(SR); Except On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.GetResponse.StreamMedia Exception: ',E.Message)); end;
  end;
end;

procedure  TTransportMedia.StreamMedia(Var SR:TRSR);
var
  iIndex:LongInt;
  dtLastAge:TDateTime;

  procedure SetMediaStartFromBytes;
  begin
    MediaP^.Range.Start:=SysUtils.StrToQWordDef(saBytes[0],0);
    MediaP^.Range.Stop:=Math.Max(0,MediaP^.ContentLength-1);
    Mediap^.Range.Size:=MediaP^.ContentLength;
  end;

  procedure SetMediaStartStopFromBytes;
  begin
    MediaP^.Range.Start:=SysUtils.StrToQWordDef(saBytes[0],0);
    MediaP^.Range.Stop:=SysUtils.StrToQWordDef(saBytes[1],Math.Max(0,MediaP^.ContentLength-1));
    Mediap^.Range.Size:=MediaP^.ContentLength;
  end;

  procedure SetMediaDefaults;
  begin
    MediaP^.Range.Start:=0;
    MediaP^.Range.Stop:=Math.Max(0,MediaP^.ContentLength-1);
    MediaP^.Range.Size:=MediaP^.ContentLength;
  end;

begin
  FManager.EntryPoint:='TTransportMedia.StreamMedia';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  dtLastAge:=0;
  DataP:=SR.Info.DataP;
  MediaP:=DataP^.Media;
  if MediaP<>nil then begin
    iIndex:=Core.Arrays.KeyString.IndexOf(FRequest.Headers,fieldRange);
    if (iIndex>-1) then begin
      // bytes=0-
      // bytes=0-933939
      // bytes=1010-39393
      Core.Arrays.VarString.fromString(saRange,FRequest.Headers[iIndex]^.Value,'=');
      Try
        if System.Length(saRange)=2 then begin
          Core.Arrays.VarString.fromString(saBytes,saRange[1],'-');
          Try
            iIndex:=System.Length(saBytes);
            case iIndex of
              1  : SetMediaStartFromBytes;
              2  : SetMediaStartStopFromBytes;
              else SetMediaDefaults;
            end;
          Finally
            Core.Arrays.VarString.Empty(saBytes);
          end;
        end else begin
          SetMediaDefaults;
        end;
      finally
        Core.Arrays.VarString.Empty(saRange);
      end;
    end else begin
      SetMediaDefaults;
    end;
    FResponse.Code:=HTTP_SC_PARTIALCONTENT;
    FResponse.Status:='Partial Content';
    FResponse.ContentType:=MediaP^.ContentType;
    FResponse.ConnectionHeader:=False;
    FResponse.AddToContent(MediaP^.Content,MediaP^.Range.Start,MediaP^.Range.Stop);
    // Content Length is set
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldContentType,MediaP^.ContentType);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldContentRange,Concat('bytes ',RSR.HTTP.toString(MediaP^.Range)));
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldAcceptRanges,'bytes');
    FResponse.Send(@SR,FManager.Refactor);
  end else begin
    FResponse.Close:=FRequest.Close;
    FResponse.Code:=HTTP_SC_GONE;
    FResponse.Status:='Gone';
    FResponse.ContentType:=ctHtml;
    FResponse.AddToContent(hHttpd.ErrorPage('Gone',
      'Gone',
      'The requested stream was released.'
    ));
    FResponse.Send(@SR,FManager.Refactor);
  end;
  FResponse.Reset();
end;


procedure TTransportMedia.OnInvalidMethod(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportMedia.OnInvalidMethod';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.Code:=HTTP_SC_METHODNOTFOUND;
  FResponse.Close:=FRequest.Close;
  FResponse.Status:='Method not supported';
  FResponse.ContentType:='';
  FResponse.AddToContent(hHttpd.ErrorPage(
      'Method not supported',
      'Method ' + FRequest.Method + ' not supported.',
      'Your client made a request using a method that is not recognised by this server.'
  ));
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

procedure TTransportMedia.OnBadRequest(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportMedia.OnBadRequest';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.ContentType:=ctHTML;
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_BADREQUEST;
  FResponse.Status:='Bad request';
  FResponse.AddToContent(hHttpd.ErrorPage(
    'Bad request',
    'Invalid request',
    'Your client made a request that was malformed or not expected.'
  ));
  FResponse.Send(@SR,FManager.Refactor);
  FResponse.Reset();
end;

procedure TTransportMedia.OnDataReceived(Var SR:TRSR; var Handled:boolean);
var
  Complete:boolean;
  iHeaderSepLoc:LongInt;
begin
  //System.Inc(iCtr);
  Handled:=False;
  FManager.EntryPoint:='TTransportMedia.OnDataReceived';
  iHeaderSepLoc:=RSR.Pos(SR.RecvBuffer,HTTP_HEADER_SEP);
  If ( iHeaderSepLoc>-1) and not ( FManager.Terminated or (((SR.State or RSR_STATE_OPEN)<>SR.State) or (SR.Errors<>[]) ) )  then begin
    FManager.EntryPoint:='TTransportMedia.OnDataReceived.Request.fromBuffer';
    Try
      Complete:=FRequest.fromBuffer(SR.RecvBuffer);
      if (Complete) then begin
        if FRequest.Upgrade and FRequest.WebSocket then begin
          Try
            FWebSocket.HandShake(@SR);
          Except
            On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.OnDataReceived.HandShake Exception: ',E.Message));
          end;
        end else begin
          Try
            FManager.EntryPoint:='TTransportMedia.OnDataReceived.GetResponse';
            GetResponse(SR);
            FRequest.Reset();
            //try Core.Streams.toFile(SR.RecvBuffer.Stream,Concat('/Developer/Logs/In/'+IntToStr(RSR.Info.Socket)+'/'+IntToStr(iCtr),'-1.txt'));  except end;
            //try Core.Streams.toFile(SR.SendBuffer.Stream,Concat('/Developer/Logs/Out/'+IntToStr(RSR.Info.Socket)+'/'+IntToStr(iCtr),'.txt'));  except end;
            If FResponse.Close then
              FManager.Close(@SR);
          Except
            On E:Exception do begin
              RSR.Empty(SR.RecvBuffer);
              Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.OnDataReceived.GetResponse Exception: ',E.Message));
            end;
          end;
        end;
      end;
    Except
      On E:Exception do begin
        RSR.Empty(SR.RecvBuffer);
        Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.OnDataReceived.fromBuffer Exception: ',E.Message));
      end;
    end;
  end else If (iHeaderSepLoc=0) and (SR.RecvBuffer.posWrite>RSR.RSR_MAX_RECEIVE_FRAME_BUFFER) then begin
    Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportMedia.OnDataReceived No Frame Header Detected inside ',IntToStr(SR.RecvBuffer.posWrite),' bytes!'));
    RSR.Empty(SR.RecvBuffer);
  end;
  Handled:=(SR.RecvBuffer.posWrite=0);
end;

constructor TTransportHTTP.Create(aManager:THTTPManager);
begin
  FMedia:=nil;
  FRequest:=THTTPRequest.Create;
  FResponse:=THTTPResponse.Create();
  FResponse.Server:=Server_Header;
  FAccounts:=aManager.FUserAccounts;
  FWebSocket:=TWebSocketHandler.Create(aManager,FRequest,FResponse);
  FResponse.Host:=aManager.Owner.RootDomain.Name;
  Inherited Create('HTTP',@aManager.Owner.RootDomain,aManager.Owner.FResources,aManager,@aManager.Owner.MatrixNode,FRequest.Data,aManager.FCoreObjects,@aManager.Owner.FKeywords);
end;

destructor  TTransportHTTP.Destroy;
begin
  FMedia:=nil;
  FManager:=nil;
  FAccounts:=nil;
  FreeAndNil(FWebSocket);
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  Inherited Destroy;
end;


procedure   TTransportHTTP.Reset;
begin
  FID:=0;
  FRequest.Reset();
  FResponse.Reset();
  Inherited Reset();
end;

procedure   TTransportHTTP.OnCoreObjectRequestAuth(CommandP:PCoreCommand; Var SR:TRSR; sTitle,sPrompt:Core.Strings.VarString);
begin
  RequestPassword(SR,'Access to this core object requires authentication','Please provide your username and password.');
end;

procedure   TTransportHTTP.OnCoreObjectError(CommandP:PCoreCommand; Var SR:TRSR; Error:WORD);
begin
  FManager.EntryPoint:='TTransportHTTP.OnCoreObjectError';
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
end;

procedure   TTransportHTTP.OnCoreObjectSuccess(CommandP:PCoreCommand; Var SR:TRSR; Code:Word);
const
  LEVEL:Array[boolean] of Tcompressionlevel = (clFastest,clMax);
var
  Deflate:TDeflater;
  DataP:PHTTP;

  saRange:Core.Arrays.Types.VarString;
  saBytes:Core.Arrays.Types.VarString;
  iIndex:LongInt;

  procedure SetMediaStartFromBytes;
  begin
    PHTTPMedia(FMedia)^.Range.Start:=SysUtils.StrToQWordDef(saBytes[0],0);
    if (saBytes[0]='0') then
      PHTTPMedia(FMedia)^.Range.Stop:=0
    else
      PHTTPMedia(FMedia)^.Range.Stop:=Math.Max(0,PHTTPMedia(FMedia)^.ContentLength-1);
    PHTTPMedia(FMedia)^.Range.Size:=PHTTPMedia(FMedia)^.ContentLength;
  end;

  procedure SetMediaStartStopFromBytes;
  begin
    PHTTPMedia(FMedia)^.Range.Start:=SysUtils.StrToQWordDef(saBytes[0],0);
    PHTTPMedia(FMedia)^.Range.Stop:=SysUtils.StrToQWordDef(saBytes[1],Math.Max(0,PHTTPMedia(FMedia)^.ContentLength-1));
    PHTTPMedia(FMedia)^.Range.Size:=PHTTPMedia(FMedia)^.ContentLength;
  end;

  procedure SetMediaDefaults;
  begin
    PHTTPMedia(FMedia)^.Range.Start:=0;
    PHTTPMedia(FMedia)^.Range.Stop:=Math.Max(0,PHTTPMedia(FMedia)^.ContentLength-1);
    PHTTPMedia(FMedia)^.Range.Size:=PHTTPMedia(FMedia)^.ContentLength;
  end;

  procedure PushCoreOutput();
  begin
    FResponse.Code:=HTTP_SC_OK;
    FResponse.Status:='OK';
    if Output.Size>0 then begin
      if CommandP^.Compress and (Output.Size<COMPRESS_NONE_THRESHOLD) then begin
        Try
          FManager.Refactor.Size:=0;
          Output.Position:=0;
          FManager.EntryPoint:='uHTTPd.Transport.HTTP.Code.Compress';
          Deflate:=TDeflater.Create(Output,FManager.Refactor,COMPRESS_BUFFER);
          Deflate.Compress(LEVEL[Output.Size<App.Consts.COMPRESS_MAX_THRESHOLD]);
          FManager.RenewCycle();
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
    end else begin
      FResponse.SendNullContentLengthHeader:=True;
    end;
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldCode,Code);
    FResponse.Send(@SR,FManager.Refactor);

  end;

begin
  FManager.EntryPoint:='TTransportHTTP.OnCoreObjectSuccess';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  DataP:=SR.Info.DataP;

  FResponse.ContentType:=FContentType;
  FResponse.Cache:=FCache;
  FResponse.CacheDate:=FCacheDate;
  FResponse.CacheTTL:=FCacheTTL;
  FResponse.ETag:=FCacheTag;
  FResponse.CacheExposure:=FCacheExposure;
  FResponse.CacheValidation:=FCacheValidation;
  if (FMedia<>nil) then begin
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
            Core.Arrays.VarString.Done(saBytes);
          end;
        end else begin
          SetMediaDefaults;
        end;
      finally
        Core.Arrays.VarString.Done(saRange);
      end;
    end else begin
      SetMediaDefaults;
    end;
    FResponse.ConnectionHeader:=true;
    FResponse.SetContent(PHTTPMedia(FMedia)^.Content,PHTTPMedia(FMedia)^);

    FResponse.CacheExposure:=PRIVATE_CACHE;
    FResponse.CacheDate:=PHTTPMedia(FMedia)^.Modified;
    FResponse.CacheTTL:=CommandP^.Resource.CacheTTL;
    FResponse.ETag:=PHTTPMedia(FMedia)^.ETag;

    FResponse.Code:=HTTP_SC_PARTIALCONTENT;
    FResponse.Status:='Partial Content';
    FResponse.Send(@SR,FManager.Refactor);
    // Note no response coming from this query!
  end else begin;
    if ( (FCacheRequest<>0) or (FETagRequested=true)) then begin
      FCacheExpired:=( (FETagRequested=false) or ( (FETagRequested=true) and (FETagRequest<>FCacheTag) ) );
      if ((MilliSecondsBetween(FCacheResponse,FCacheRequest)>MODIFIED_THRESHOLD) or (FCacheExpired=true) ) then begin
        PushCoreOutput();
      end else begin
        FResponse.Close:=FRequest.Close;
        FResponse.Code:=HTTP_SC_NOTMODIFIED;
        FResponse.ContentKind:=RSR.HTTP.ckNone;
        FResponse.Status:='Not Modified';
        FResponse.Send(@SR,FManager.Refactor);
      end;
    end else begin
      PushCoreOutput();
    end;
  end;

end;

procedure   TTransportHTTP.OnCoreObjectRedirect(CommandP:PCoreCommand; Var SR:TRSR; URL:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportHTTP.OnCoreObjectRedirect';
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

end;

function    TTransportHTTP.OnCoreObjectCheckCredentials(CommandP:PCoreCommand; Var SR:TRSR; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
var
  DataP : PHTTP;
  UserP : Storage.UserAccounts.Items.PItem;
begin
  FManager.EntryPoint:='TTransportHTTP.OnCoreObjectCheckCredentials.Authorized';
  DataP:=SR.Info.DataP;
  UserP:=SR.Credentials;
  DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAuth);
  if (System.Length(DataP^.Auth)=0) then
    DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(FRequest.Cookies,fieldAuth);

  DataP^.Account:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAccount);
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

procedure TTransportHTTP.RequestPassword(Var SR:TRSR; Title,Prompt:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportHTTP.RequestPassword';
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
end;


procedure   TTransportHTTP.OnCoreObjectNotFound(Var SR:TRSR; NameSpace:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportHTTP.OnCoreObjectNotFound';
  OnNotFound(SR,'Core Object Not Found',Format('The Core Object "%s" was not found.  Check to make sure that the requested object is injected into the server core at startup.',[NameSpace]));
end;

procedure   TTransportHTTP.OnCoreObjectCommandNotFound(Var SR:TRSR; nsObject,nsCommand:Core.Strings.VarString);
begin
  OnNotFound(SR,'Core Object Command Not Found',Format('The Core Object (%s) Command "%s" was not found.  Check to make sure that the version expected is injected into the server core at startup.',[nsObject,nsCommand]));
end;

procedure   TTransportHTTP.Redirect(Var SR:TRSR; URL:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportHTTP.Redirect';
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
end;

procedure TTransportHTTP.OnNotFound(Var SR:TRSR; Title,Content:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportHTTP.OnNotFound';
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
end;

procedure TTransportHTTP.SendContent(Var SR:TRSR; Content:Core.Strings.VarString);
begin
  FManager.EntryPoint:='TTransportHTTP.SendContent';
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
end;

function    TTransportHTTP.ResolvePath(var URI: Core.Strings.VarString; var Folder:TDSFolder; var Resource:TDSFile; Var Path,FileName,Ext: Core.Strings.VarString; Refactor:TStream): TResolveResult;
var
  sNewURI : Core.Strings.VarString;
  sFile   : Core.Strings.VarString;
  //idxChar : LongInt;
  iParamCount:LongInt;
  iLength : LongInt;
  iCount  : LongInt;
  iHdrCount : LongInt;
  iSubCount:LongInt;
  idxFile : LongInt;
  saPath  : Core.Arrays.Types.VarString;
  saSubPath:Core.Arrays.Types.VarString;

  procedure PushAsFile(PreFolder:Core.Strings.VarString);
  begin
    FileName:=saPath[iCount-1];
    Ext:=Core.Utils.Files.Extract(FileName,efeoNone);
    if (Length(Ext)>0)  then begin
      FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,Ext);
      Core.Arrays.VarString.Remove(saPath,iCount-1);
      Dec(iCount);
    end;
    Path:=Concat(PreFolder,'/',Core.Arrays.VarString.toString(saPath,'/',Refactor));
    Folder:=FFAT.Acquire(Path,Refactor);
  end;

  procedure PushCoreObjectInfo;
  begin
    FResource.NameSpace:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,RSR.Core.NameSpace.Field.Resource,iHdrCount);
    FID:=Core.Arrays.KeyString.GetItemAsInt64(FRequest.Headers,fieldID,iHdrCount,0);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldID,IntToStr(FID));
    FCoreObject.Path:=Core.Utils.Files.Extract(FCoreObject.NameSpace,epoAllButName,SCS_PATH_DELIM);
    FCoreObject.Name:=Core.Utils.Files.Extract(FCoreObject.NameSpace,epoName,SCS_PATH_DELIM);
    if System.Length(FCoreObject.NameSpace)>0 then
      Core.Arrays.KeyString.Update(FResponse.Headers,RSR.Core.NameSpace.Field.co.NameSpace,FCoreObject.NameSpace);

    FCoreCommand.Path:=Core.Utils.Files.Extract(FCoreCommand.NameSpace,epoAllButName,SCS_PATH_DELIM);
    FCoreCommand.Name:=Core.Utils.Files.Extract(FCoreCommand.NameSpace,epoName,SCS_PATH_DELIM);
    if System.Length(FCoreCommand.NameSpace)>0 then
      Core.Arrays.KeyString.Update(FResponse.Headers,RSR.Core.NameSpace.Field.co.cmd.NameSpace,FCoreCommand.NameSpace);

    FResource.Path:=Core.Utils.Files.Extract(FResource.NameSpace,epoAllButName,SCS_PATH_DELIM);
    FResource.Name:=Core.Utils.Files.Extract(FResource.NameSpace,epoName,SCS_PATH_DELIM);
    if System.Length(FResource.NameSpace)>0 then
      Core.Arrays.KeyString.Update(FResponse.Headers,RSR.Core.NameSpace.Field.Resource,FResource.NameSpace);


  end;

begin
  FManager.EntryPoint:='TTransportHTTP.ResolvePath';
  sNewURI:=URI;
  Resource:=nil; Folder:=nil;
  iLength:=Length(sNewURI);
  iParamCount:=System.Length(FRequest.Parameters);
  iHdrCount:=System.Length(FRequest.Headers);
  if (iLength=0) or ((iLength>0) and (sNewURI[iLength]='/')) then begin
    sNewURI:=Concat(sNewURI,'index.html');
    iLength:=Length(sNewURI);
    Result:=rrFound;
  end;
  if (iLength>0) and (sNewURI[1] = '/') then System.Delete(sNewURI,1,1);
  Core.Arrays.VarString.fromString(@saPath,sNewURI,'/',[soClearList,soIgnoreDelimAtStart]);
  Try
    iCount:=Length(saPath);
    if (iCount>0) then begin
      if (Length(saPath)>0) and (SameText(saPath[0],RequiredFolders[RFI_CORE])) then begin
        FCoreObject.NameSpace:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,RSR.Core.NameSpace.Field.co.NameSpace,iHdrCount);
        FCoreCommand.NameSpace:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,RSR.Core.NameSpace.Field.co.cmd.NameSpace,iHdrCount);
        if (Length(FCoreCommand.NameSpace)=0) and (iParamCount>0) then begin
          Core.Arrays.VarString.fromString(@saSubPath,FRequest.Parameters[0],'/',[soClearList,soIgnoreDelimAtStart]);
          Try
            iSubCount:=System.Length(saSubPath);
            if iSubCount>0 then begin
              FileName:=saSubPath[iSubCount-1];
              Path:=Core.Arrays.VarString.toString(saPath,'/',Refactor,TRAILING_DELIM_OFF,LEADING_DELIM_ON);
              Folder:=FFAT.Acquire(Path,Refactor); // Core objects?
              if (Folder=nil) then begin
                PushAsFile('');
              end else begin
                FCoreObject.NameSpace:=Path;
                FCoreCommand.NameSpace:=Concat('/',FRequest.Parameters[0]);
                Core.Arrays.KeyString.Update(FRequest.Headers,RSR.Core.NameSpace.Field.co.NameSpace,FCoreObject.NameSpace);
                Core.Arrays.KeyString.Update(FRequest.Headers,RSR.Core.NameSpace.Field.co.cmd.NameSpace,FCoreCommand.NameSpace);
              end;
            end;
          Finally
            Core.Arrays.VarString.Done(saSubPath);
          end;
        end else begin
          If ( (Length(FCoreObject.NameSpace)>0) and (Length(FCoreCommand.NameSpace)>0) ) then begin
            Core.Arrays.VarString.Remove(saPath,iCount-1);
            Dec(iCount);
            SetLength(FileName,0);
            Path:=FCoreObject.NameSpace;
          end else begin
            FileName:=saPath[iCount-1];
            Ext:=Core.Utils.Files.Extract(FileName,efeoNone);
            FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,Ext);
            if (Length(Ext)>0)  then begin
              Core.Arrays.VarString.Remove(saPath,iCount-1);
              Dec(iCount);
            end;
            Path:=Core.Arrays.VarString.toString(saPath,'/',Refactor,TRAILING_DELIM_OFF);
          end;
          Folder:=FFAT.Acquire(Path,Refactor); // Core objects?
        end;
      end else begin
        PushAsFile(RequiredFolders[RFI_WEB]);
      end;
    end;
  Finally
    Core.Arrays.VarString.Done(saPath);
  End;
  if (Folder<>nil) then begin
    if ((Folder.Attributes or FS_ATTR_COREOBJECT)=Folder.Attributes) then begin
      //DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldAuth,iCount);
      if (System.Length(FileName)>0) then begin
        Resource:=FFAT.Acquire(Folder,FileName);   // won't really happen unless index.html
        if (Resource=nil) or (  (Resource.Attributes or FS_ATTR_COREOBJECT)=Resource.Attributes) then begin
          PushCoreObjectInfo;
          if System.Length(FCoreObject.NameSpace)>0 then begin
            Result:=rrCoreObject;
          end else if Sysutils.SameText(Folder.Name,FileName) then begin
            URI:=Concat('/',sNewURI,'/');
            Result:=rrFound;
          end else begin
            Result:=rrNotFound;
          end;
        end else begin
          Ext:=Core.Utils.Files.Extract(FileName,efeoNone);
          if (Length(Ext)>0)  then
            FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,Ext);
          if Resource<>nil then begin
            FCache:=Resource.Cache;
            FCacheDate:=Resource.Modified;
            FCacheResponse:=Resource.Modified;
            FCacheTag:=MD5.MD5Print(Resource.Digest);
            FCacheTTL:=Resource.CacheTTL;
          end;
          Result:=rrOK;  // regular object
        end;
      end else begin
        PushCoreObjectInfo;
        Result:=rrCoreObject;
      end;
    end else begin
      Resource:=FFAT.Acquire(Folder,FileName);
      If ( ( (Folder.Attributes or FS_ATTR_COREOBJECT)<>Folder.Attributes ) and (Resource=nil) ) then begin
        If (Sysutils.SameText(Folder.Name,FileName)) then begin
          if (iParamCount>0) then begin
            if Folder.Files.Find(Resource,'index.html') then begin
              Result:=rrOK;
              FContentType:=ctHTML;
            end else begin
              Result:=rrFound;
              iLength:=Length(URI);
              SetLength(URI,iLength+1);
              URI[iLength+1]:='/';
            end;
          end else begin
            Result:=rrFound;
            iLength:=Length(URI);
            SetLength(URI,iLength+1);
            URI[iLength+1]:='/';
          end;
        end else
          Result:=rrNotFound;
      end else begin
        FCache:=Resource.Cache;
        FCacheTTL:=Resource.CacheTTL;
        FCacheDate:=Resource.Modified;
        FCacheResponse:=Resource.Modified;
        FCacheTag:=MD5.MD5Print(Resource.Digest);
        Result:=rrOK;
      end;
    end;
  end else
    Result:=rrNotFound;
end;

procedure TTransportHTTP.GetResponse(Var SR:TRSR);
var
  Resource    : TDSFile;
  Folder      : TDSFolder;
  FileName    : Core.Strings.VarString;
  Path        : Core.Strings.VarString;
  Ext         : Core.Strings.VarString;
  Origin      : Core.Strings.VarString;
  RR          : TResolveResult;
  iIndex      : LongInt;

  procedure PushCoreObject;
  var
    iRcID:QWord;
  begin
    SR.Finite:=false;
    FResponse.ContentType:=ctXML;
    FResponse.Close:=False;
    PHTTP(SR.Info.DataP)^.Auth:=Core.Arrays.KeyString.GetItemByKey(FRequest.Headers,fieldAuth);
    if (System.Length(PHTTP(SR.Info.DataP)^.Auth)=0) then
      PHTTP(SR.Info.DataP)^.Auth:=Core.Arrays.KeyString.GetItemByKey(FRequest.Cookies,fieldAuth);

    iRcID:=Core.Arrays.KeyString.GetItemAsInt64(FRequest.Headers,fieldResourceID);
    if (iRcID=0) then begin
      iRcID:=Core.Arrays.KeyString.GetItemAsInt64(FRequest.Cookies,fieldResourceID);
      if (iRcID<>0) then
        Core.Arrays.KeyString.Add(@FRequest.Headers,fieldResourceID,IntToStr(iRcID));
    end;

    Core.Arrays.KeyString.Add(@FResponse.Headers,fieldAuth,PHTTP(SR.Info.DataP)^.Auth);
    Core.Arrays.KeyString.Add(@FResponse.Headers,fieldID,IntToStr(FID));

    Core.Arrays.VarString.Copy(FRequest.Parameters,FResponse.Parameters);

    FManager.EntryPoint:='TTransportHTTP.GetResponse.CoreObjects.Execute';
    try
      FCoreObjects.Execute(SR,Self,FRequest.Parameters,FRequest.Headers,FResponse.Headers,FRequest.Data);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.CoreObjects.Execute Exception: ',E.Message));
    end;
  end;

begin
  if SameText(FRequest.Method,METHOD_OPTIONS) then begin
    FResponse.Close:=FRequest.Close;
    FResponse.Code:=HTTP_SC_OK;
    FResponse.Status:='OK';
    FResponse.ContentKind:=ckCors;
    Core.Arrays.KeyString.Add(@FResponse.Headers,fieldACLAllowMethods,Concat(METHOD_GET,', ',METHOD_PUT,', ',METHOD_POST,', ',METHOD_HEAD,', ',METHOD_OPTIONS));
    iIndex:=Core.Arrays.KeyString.Add(@FResponse.Headers,fieldACLAllowHeaders,Core.Arrays.KeyString.GetItemAsString(FRequest.Headers,fieldACLRequestHeaders));
    if (System.Length(FResponse.Headers[iIndex]^.Value)=0) then begin
      FResponse.Headers[iIndex]^.Streams:=false;
    end else begin
      Core.Arrays.KeyString.Add(@FResponse.Headers,fieldACLExposeHeaders,FResponse.Headers[iIndex]^.Value);
    end;
    Origin:=Core.Arrays.KeyString.GetItemAsString(FRequest.Headers,fieldOrigin);
    if Length(Origin)>0 then
      iIndex:=Core.Arrays.KeyString.Update(FResponse.Headers,fieldACLAllowOrigin,Origin);
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldDate,HTTP_DateTime(Core.Timer.dtUT));
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldVary,'Accept-Encoding');
    Core.Arrays.KeyString.Update(FResponse.Headers,fieldACLAllowCredentials,'true');
    FResponse.Send(@SR,FManager.Refactor);
  end else begin
    FManager.EntryPoint:='TTransportHTTP.GetResponse';
    FCacheRequest:=0;FCacheResponse:=0;
    FResponse.Head:=SameText(FRequest.Method,METHOD_HEAD);
    iIndex:=Core.Arrays.KeyString.IndexOf(@FRequest.Headers,fieldIfModifiedSince);
    If iIndex<>-1 then
      FCacheRequest:=Core.Utils.Time.toGMTTime(FRequest.Headers[iIndex]^.Value);

    FETagRequest:=FRequest.ETag;
    FETagRequested:=(System.Length(FETagRequest)>0);

    Try RR:=ResolvePath(FRequest.URI,Folder,Resource,Path,FileName,Ext,FManager.Refactor); Except on E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.ResolvePath Exception: ',Path,' ',E.Message)); end;
    Case RR of
      rrInvalidPath : Try OnBadRequest(SR); Except on E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.OnBadRequest Exception: ',E.Message)); end;
      rrNotFound    : Try OnNotFound(SR,'Resource Not Found',Format('The requested resource %s was not found.',[FRequest.URI])); Except On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.OnNotFound Exception: ',E.Message)); end;
      rrFound       : Try Redirect(SR,FRequest.URI); Except On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.OnRedirect Exception: ',E.Message)); end;
      rrOK          : Try SendFile(SR,Folder,Resource,Ext); Except On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.SendFile Exception: ',E.Message)); end;
      rrCoreObject  : Try PushCoreObject(); Except On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.GetResponse.PushCoreObject Exception: ',E.Message)); end;
    end;
    If (SR.Info.DataP<>Nil) and (PHTTP(SR.Info.DataP)^.UAP<>Nil) and (PHTTP(SR.Info.DataP)^.UAP^.Modified) then begin
      FManager.EntryPoint:='TTransportHTTP.GetResponse.UserAccount_UpdateMods';
      Storage.UserAccounts.Items.DB.UpdateMods(FManager.Task,PHTTP(SR.Info.DataP)^.UAP^);
      PHTTP(SR.Info.DataP)^.UAP^.Modified:=False;
    end;
  end;
end;

procedure  TTransportHTTP.SendFile(Var SR:TRSR; Folder:TDSFolder; Resource:TDSFile; Var sExt:Core.Strings.VarString);
  procedure PushDeflate();
  var
    Deflate : TDeflater;
  begin
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
    Core.Arrays.KeyString.Add(@FResponse.Headers,fieldContentEncoding,RSR.HTTP.Deflate);
  end;

  procedure PushSetCache();
  begin
    FResponse.Cache:=FCache;
    FResponse.CacheTTL:=FCacheTTL;
    FResponse.CacheDate:=FCacheDate;
    FResponse.CacheExposure:=FCacheExposure;
    FResponse.ETag:=FCacheTag;
  end;

  procedure PushPageOutput();
  begin
    if (Resource.Downloaded=0) then
      Resource.Load(FManager.Task);
    FResponse.Close:=FRequest.Close;
    FResponse.Code:=HTTP_SC_OK;
    FResponse.Status:='OK';

    Resource.toStream(Output);
    if Resource.Deflate and (Output.Size>0) then
      PushDeflate()
    else
      FResponse.SetContent(Output);

    FResponse.Send(@SR,FManager.Refactor);
  end;

begin
  FManager.EntryPoint:='TTransportHTTP.SendFile';
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  FResponse.ContentType:=FContentType;

  If (FContentType=ctHTML) then begin
    If Resource<>Nil then begin
      If Resource.HasKeywords then begin
        FResponse.Code:=HTTP_SC_OK;
        FResponse.Status:='OK';
        FResponse.Close:=FRequest.Close;
        FResponse.Cache:=false;
        Resource.toStream(Output);
        if Resource.Deflate and (Output.Size>0) then
          PushDeflate()
        else
          FResponse.SetContent(Output);
        FResponse.Send(@SR,FManager.Refactor);
      end else begin
        PushSetCache();
        If (FCache=true) then begin
          if ( (FCacheRequest<>0) or (FETagRequested=true)) then begin
            FCacheExpired:=( (FETagRequested=false) or ( (FETagRequested=true) and (FETagRequest<>FCacheTag) ) );
            if (MilliSecondsBetween(FCacheResponse,FCacheRequest)>MODIFIED_THRESHOLD) or (FCacheExpired=true) or (Resource.HasKeywords=true) then begin
              PushPageOutput();
            end else begin
              FResponse.Close:=FRequest.Close;
              FResponse.Code:=HTTP_SC_NOTMODIFIED;
              FResponse.Status:='Not Modified';
              FResponse.Send(@SR,FManager.Refactor);
            end;
          end else begin
            PushPageOutput();
          end;
        end else begin
          if (Resource.Downloaded=0) then
            Resource.Load(FManager.Task);
          FResponse.Close:=FRequest.Close;
          FResponse.Code:=HTTP_SC_OK;
          FResponse.Status:='OK';

          Resource.toStream(Output);
          if Resource.Deflate and (Output.Size>0) then
            PushDeflate()
          else
            FResponse.SetContent(Output);
          FResponse.Send(@SR,FManager.Refactor);
        end;
      end;
    end else begin
      FResponse.Close:=FRequest.Close;
      FResponse.Code:=HTTP_SC_GONE;
      FResponse.Status:='Gone';
      FResponse.ContentType:=ctHtml;
      FResponse.Cache:=false;
      FResponse.AddToContent(hHttpd.ErrorPage('Gone',
        'Gone',
        'The requested resource was moved.'
      ));
      FResponse.Send(@SR,FManager.Refactor);
    end;
  end else begin
    If (Resource<>Nil) then begin
      PushSetCache();
      FCacheExpired:=( (FETagRequested=false) or ( (FETagRequested=true) and (FETagRequest<>FCacheTag) ) );
      If (FCacheExpired=true) or (MilliSecondsBetween(Resource.Modified,FCacheRequest)>MODIFIED_THRESHOLD) or (Resource.HasKeywords=true) then begin
        PushPageOutput();
      end else begin
        FResponse.Close:=FRequest.Close;
        FResponse.Code:=HTTP_SC_NOTMODIFIED;
        FResponse.Status:='Not Modified';
        FResponse.Send(@SR,FManager.Refactor);
      end;
    end else begin
      FResponse.Close:=FRequest.Close;
      FResponse.Code:=HTTP_SC_GONE;
      FResponse.Status:='Gone';
      FResponse.Cache:=false;
      FResponse.Send(@SR,FManager.Refactor);
    end;
  end;
end;


procedure TTransportHTTP.OnInvalidMethod(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportHTTP.OnInvalidMethod';
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
end;

procedure TTransportHTTP.OnBadRequest(Var SR:TRSR);
begin
  FManager.EntryPoint:='TTransportHTTP.OnBadRequest';
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
end;

procedure TTransportHTTP.OnDataReceived(Var SR:TRSR; var Handled:boolean);
var
  Complete:boolean;
  iHeaderSepLoc:LongInt;
begin
  //System.InterLockedIncrement(iCtr);
  Handled:=False;
  FManager.EntryPoint:='TTransportHTTP.OnDataReceived';
  iHeaderSepLoc:=RSR.Pos(SR.RecvBuffer,HTTP_HEADER_SEP);
  If ( iHeaderSepLoc>-1) and not ( FManager.Terminated or (((SR.State or RSR_STATE_OPEN)<>SR.State) or (SR.Errors<>[]) ) )  then begin
    FManager.EntryPoint:='TTransportHTTP.OnDataReceived.Request.fromBuffer';
    Try
      Complete:=FRequest.fromBuffer(SR.RecvBuffer);
      if (Complete) then begin
        if FRequest.Upgrade and FRequest.WebSocket then begin
          Try
            FWebSocket.HandShake(@SR);
          Except
            On E:Exception do Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.OnDataReceived.HandShake Exception: ',E.Message));
          end;
        end else begin
          Try
            FManager.EntryPoint:='TTransportHTTP.OnDataReceived.GetResponse';
            GetResponse(SR);
            If FResponse.Close then
              FManager.Close(@SR);
            FRequest.Reset();
            FResponse.Reset();
          Except
            On E:Exception do begin
              RSR.Empty(SR.RecvBuffer);
              Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.OnDataReceived.GetResponse Exception: ',E.Message));
            end;
          end;
        end;
      end;
    Except
      On E:Exception do begin
        RSR.Empty(SR.RecvBuffer);
        Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.OnDataReceived.fromBuffer Exception: ',E.Message));
      end;
    end;
  end else If (iHeaderSepLoc=0) and (SR.RecvBuffer.posWrite>RSR.RSR_MAX_RECEIVE_FRAME_BUFFER) then begin
    Core.Logging.Native.WriteLogEntry(FDomainP^.Name,Service_HTTP,Concat('TTransportHTTP.OnDataReceived No Frame Header Detected inside ',IntToStr(SR.RecvBuffer.posWrite),' bytes!'));
    RSR.Empty(SR.RecvBuffer);
  end;
  Handled:=(SR.RecvBuffer.posWrite=0);
end;

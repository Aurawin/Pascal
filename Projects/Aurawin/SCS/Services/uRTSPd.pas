{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This protected by the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit uRTSPd;

interface
uses
  Types,
  classes,
  hRTSPd,
  RSR,
  RSR.DNS,
  App.Build,
  App.Consts,


  Core.Database,
  Core.Database.Types,
  Core.Database.Timer,

  Core.Utils.Sockets,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,

  Core.Strings,
  Core.Streams,
  Core.Keywords,
  Core.Timer,
  Core.Logging,
  Core.Utils.Files,

  Storage,
  Storage.Main,
  Storage.FAT,
  Storage.AuraDisks,
  Storage.UserAccounts,
  Storage.UserStorage,
  Storage.Domains,
  Storage.CoreObjects,
  Storage.RTSP,
  Storage.Keywords,
  Storage.MatrixClusters,
  Storage.MatrixResources,
  Storage.MatrixNodes,
  Storage.KeepAlive,
  SysUtils;

Const
  RTSP_STACK_SIZE=1024*5;

Type
  PRTSP=^TRTSP;
  TRTSP=record
    UAP                          : Storage.UserAccounts.Items.PItem;
    sUser                        : Core.Strings.VarString;
    sPass                        : Core.Strings.VarString;
    sAuth                        : Core.Strings.VarString;
    ErrorCount                   : Byte;
    State                        : Byte;
    SessionID                    : int64;
    ManifestP                    : Storage.RTSP.PManifest;
    binIndex                     : LongInt;
  end;

  TRTSPServer=Class(TRSRServer)
  private
    FManagers                    : TRSRManagers;
    RootDomain                   : Storage.Domains.Items.TDomain;
    FResources                   : TDSFAT;
    FKeywords                    : TKeywords;
    FFATTimer                    : Core.Timer.Item;
    FKwdTimer                    : Core.Timer.Item;

    FMatrixCluster               : Storage.MatrixClusters.Cluster.Item;
    FMatrixResource              : Storage.MatrixResources.Resource.Item;
    FMatrixNode                  : Storage.MatrixNodes.Node.Item;
  protected
    procedure  _OnKeywordTimer(ItemP:Core.Timer.PItem);
    procedure  _OnFATTimer(ItemP:Core.Timer.PItem);
  protected
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    Constructor Create(aDomain:ShortString; Const aNodeID,aIP:Int64; aPort, aScale:WORD; aSuspended:boolean); ReIntroduce;
    Destructor  Destroy; override;
  public
    property    Resources:TDSFAT read FResources;
  end;

  TRTSPManager=Class(TRSRManager)
  private
    Owner                        : TRTSPServer;
    UserAccounts                 : Storage.UserAccounts.Items.TList;
    FReader                      : Storage.RTSP.TReader;
    FRequest                     : hRTSPd.TRequest;
    FResponse                    : hRTSPd.TResponse;
  private
    procedure   ClipBuffer(Const CRLFCount:LongInt; Var Buffer:Core.Strings.VarString);
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:Boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  private
    procedure   pushDescribe(RSRP:PRSR);
    procedure   pushAnnounce(RSRP:PRSR);
    procedure   pushGetParameter(RSRP:PRSR);
    procedure   pushOptions(RSRP:PRSR);
    procedure   pushPause(RSRP:PRSR);
    procedure   pushPlay(RSRP:PRSR);
    procedure   pushRecord(RSRP:PRSR);
    procedure   pushRedirect(RSRP:PRSR);
    procedure   pushSetup(RSRP:PRSR);
    procedure   pushSetParameter(RSRP:PRSR);
    procedure   pushTeardown(RSRP:PRSR);
    procedure   pushMethodUnknown(RSRP:PRSR);
  protected
    function ResolvePath(Path:Core.Strings.VarString; out dsFolder:TDSFolder; out dsFile:TDSFile):boolean;
  public
    Constructor Create(AOwner:TRTSPServer); reintroduce;
    Destructor  Destroy; override;
  end;
  procedure Init(Var Item:TRTSP); overload;
  procedure Done(Var Item:TRTSP); overload;
  procedure StartBackgroundTimers();
implementation
uses DateUtils;

var FATTimer:Core.Database.Types.TTimer;
var KeywordTimer:Core.Database.Types.TTimer;

Const
  MAX_ERRORS = 5;
  RS_NONE=0;

  RS_AUTHORIZATION=1 shl 0;
  RS_TRANSACTION  =1 shl 1;
  RS_SESSION      =1 shl 2;
  RS_PLAY         =1 shl 3;
  RS_PAUSE        =1 shl 4;
  RS_TEARDOWN     =1 shl 5;

procedure StartBackgroundTimers();
begin
  if FATTimer=nil then
    FATTimer:=Core.Database.Types.TTimer.Create(Storage.Main.Header,tpNormal);
  if KeywordTimer=nil then
    KeywordTimer:=Core.Database.Types.TTimer.Create(Storage.Main.Header,tpNormal);
end;

procedure ShutdownTimers();
begin
  if (FATTimer<>nil) then FATTimer.Terminate();
  FATTimer:=nil;
  if (KeywordTimer<>nil) then KeywordTimer.Terminate();
  KeywordTimer:=nil;
end;

procedure Init(Var Item:TRTSP);
begin
  Item.UAP:=nil;
  SetLength(Item.sUser,0);
  SetLength(Item.sPass,0);
  SetLength(Item.sAuth,0);
  Item.ErrorCount:=0;
  Item.SessionID:=0;
  Item.binIndex:=0;
  Item.State:=RS_NONE;
  //dbmRTSP.Manifest.Init(Item.Manifest);
  //dbmRTSP.ByteBin.Init(Item.Bins);
end;

procedure Done(Var Item:TRTSP);
begin
  Finalize(Item.sUser);
  Finalize(Item.sPass);
  Finalize(Item.sAuth);
  //dbmRTSP.Manifest.Done(Item.Manifest);
  //dbmRTSP.ByteBin.Done(Item.Bins);
  Finalize(Item);
end;

Constructor TRTSPServer.Create(aDomain:ShortString; Const aNodeID,aIP:Int64; aPort,aScale:Word; aSuspended:boolean);
var
  iLcv   : LongInt;
  kwFill : Storage.Keywords.Items.DB.Request;
begin
  FService:=Service_RTSP;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Server'));

  RootDomain.Name:=aDomain;
  Storage.Domains.Items.DB.Fill(Task,RootDomain);

  kwFill.cbDefault:=@cbKW_Loopback;
  kwFill.DomainID:=RootDomain.ID;
  kwFill.KeywordsP:=@FKeywords;

  Storage.Keywords.Items.DB.Fill(Task,kwFill);
  Storage.MatrixClusters.Cluster.DB.Fill(Task,ClusterID,FMatrixCluster);
  Storage.MatrixResources.Resource.DB.Fill(Task,ResourceID,FMatrixResource);
  Storage.MatrixNodes.Node.DB.Fill(Task,aNodeID,FMatrixNode);


  Core.Keywords.Add('app_edition',App.Build.Edition,FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,NO_CALLBACK);
  Core.Keywords.Add('app_title',App.Build.Title,FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,NO_CALLBACK);
  Core.Keywords.Add('rsr_version',RSR_Version,FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,NO_CALLBACK);
  Core.Keywords.Add('rsr_build',IntToStr(RSR_Build_Number),FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,NO_CALLBACK);

  Core.Keywords.Add('date','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Date);
  Core.Keywords.Add('year','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Year);
  Core.Keywords.Add('time','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Time);

  Core.Keywords.Add('rsr_connection_count','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_ConCount);
  Core.Keywords.Add('rsr_stream_count','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_StreamCount);
  Core.Keywords.Add('rsr_transaction_count','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_TXCount);
  Core.Keywords.Add('rsr_send_count','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_SentCount);
  Core.Keywords.Add('rsr_recv_count','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_RecvCount);
  Core.Keywords.Add('rsr_bytes_sent','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_BytesSent);
  Core.Keywords.Add('rsr_bytes_recv','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_BytesRecv);
  Core.Keywords.Add('rsr_filter_count','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_RSR_Filtered);

  Core.Keywords.Add('node_mem_total','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Node_Mem_Total);
  Core.Keywords.Add('node_mem_free','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Node_Mem_Free);
  Core.Keywords.Add('node_mem_load','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Node_Mem_Load);

  Core.Keywords.Add('cluster_name',FMatrixCluster.Group,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('cluster_id',IntToStr(FMatrixCluster.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('resource_name',FMatrixResource.Name,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('resource_id',IntToStr(FMatrixResource.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_name',FMatrixNode.Alias,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_address',InAddrToStr(FMatrixNode.IP),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_id',IntToStr(FMatrixNode.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);

  Core.Keywords.Add('domain_id',IntToStr(RootDomain.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('domain_name',RootDomain.Name,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('domain_alias',RootDomain.FriendlyName,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);

  Core.Keywords.Add('uptime_weeks','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Weeks);
  Core.Keywords.Add('uptime_days','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Days);
  Core.Keywords.Add('uptime_hours','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Hours);
  Core.Keywords.Add('uptime_minutes','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Minutes);
  Core.Keywords.Add('uptime_seconds','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Seconds);


  FResources:=TDSFAT.Create();
  FResources.Load(RootDomain.ID,Storage.ClusterID,Storage.ResourceID,aNodeID,dsfatloBoth);
  FResources.Scan(Task,@FKeywords);


  iLcv:=0;
  While iLcv<aScale do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TRTSPManager.Create(Self);
    Except
      iLcv:=aScale;
    end;
    Inc(iLcv);
  end;
  Inherited Create(@FManagers,tTCP,aIP,aPort,aScale,SSL_OFF,aSuspended);

  Core.Timer.Init(FKwdTimer);
  FKwdTimer.Event:=@_OnKeywordTimer;
  FKwdTimer.Location:='uRTSPd._OnKeywordTimer';
  FKwdTimer.Expires:=IncMinute(Core.Timer.dtNow,1);

  Core.Timer.Init(FFATTimer);
  FFATTimer.Event:=@_OnFATTimer;
  FFATTimer.Location:='uRTSPd._OnFATTimer';
  FFATTimer.Expires:=IncMinute(Core.Timer.dtNow,1);

  KeywordTimer.RegisterEvent(FKwdTimer,LoadNoUpdate);
  FatTimer.RegisterEvent(FFATTimer,LoadNoUpdate);
end;

Destructor  TRTSPServer.Destroy;
begin
  KeywordTimer.UnloadEvent(FKwdTimer,UnloadNoExecute);
  FatTimer.UnloadEvent(FFATTimer,UnloadNoExecute);

  FreeAndNil(FResources);
  Core.Keywords.Done(FKeywords);

  Storage.Domains.Items.Done(RootDomain);

  Inherited Destroy;
end;

procedure TRTSPServer._OnFATTimer(ItemP:Core.Timer.PItem);
Const                                        // 30 seconds                 // 120 seconds
  Delay: Array[Boolean] of LongInt=(DOMAIN_FAT_REFRESH_ACTIVE,DOMAIN_FAT_REFRESH_INACTIVE);
var
  Refreshed:QWord;
begin
  Try
    Try
      if not FResources.Refresh(FatTimer.Task,Refreshed) then
        Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,'TRTSPServer._OnFATTimer Error: Refresh command failed.');
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat('TRTSPServer._OnFATTimer Exception: ',E.Message));
    End;
  Finally
    ItemP^.Expires:=IncSecond(Core.Timer.dtNow,Delay[Refreshed=0]);
  end;
end;

procedure  TRTSPServer._OnKeywordTimer(ItemP:Core.Timer.PItem);
var
  bKWStale:Boolean;
begin
  bKWStale:=false;
  Storage.Keywords.Items.DB.Refresh(KeywordTimer.Task,RootDomain.ID,FKeywords,bKWStale);
  if bKWStale then
     FResources.Flush;
  ItemP^.Expires:=IncSecond(Core.Timer.dtNow,10);
end;


procedure   TRTSPServer.OnError(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':Error ',sError));
end;

procedure   TRTSPServer.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':Exception ',sError));
end;

Constructor TRTSPManager.Create(AOwner:TRTSPServer);
begin
  FService:=Service_RTSP;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Manager'));

  Owner:=AOwner;
  UserAccounts:=Storage.UserAccounts.Items.TList.Create(@AOwner.RootDomain,FService);
  FReader:=Storage.RTSP.TReader.Create();

  FRequest:=hRTSPd.TRequest.Create();
  FResponse:=hRTSPd.TResponse.Create();

  Inherited Create(AOwner,@AOwner.FSSLInfo,SSL_OFF,THREAD_METHODS_OFF,RTSP_STACK_SIZE);

  TimeOut:=120000;
end;

Destructor  TRTSPManager.Destroy;
begin
  FreeAndNil(UserAccounts);
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  Inherited Destroy;
end;

procedure   TRTSPManager.ClipBuffer(Const CRLFCount:LongInt; Var Buffer:Core.Strings.VarString);
var
  iEnd,Count,Len,Lcv:LongInt;
begin
  Len:=Length(Buffer);  Lcv:=1; Count:=0; iEnd:=0;
  While (Count<CRLFCount) and (Lcv<=Len) do begin
    If Buffer[Lcv]=#10 then
      Inc(Count);
    Inc(Lcv);
    Inc(iEnd);
  end;
  SetLength(Buffer,iEnd);
end;

procedure  TRTSPManager.OnQueue(RSRP:PRSR);
var
  sResponse:Core.Strings.VarString;
  DataP:PRTSP;
begin
  DataP:=RSRP^.Info.DataP;
  If DataP<>Nil then begin
    DataP^.State:=RS_NONE;
    DataP^.ErrorCount:=0;
    // sResponse:=Concat('+OK ',Owner.RootDomain.Name,' running ',APP_TITLE,' ',APP_EDITION,' Build (',APP_VERSION,') RSR Build (',APP_RSR_BUILD,'). AURA Streaming Service Ready.',#13#10);
    // Send(RSRP,sResponse);
  end else begin
    // sResponse:=Concat('-ERR ',Owner.RootDomain.Name,' Aura Streaming Service Not Ready.',#13#10);
    //Send(RSRP,sResponse);
    Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TRTSPManager.OnQueue Error:','Null pointer to Data'));
    Close(RSRP);
  end;
end;

procedure  TRTSPManager.OnConnect(RSRP:PRSR);
begin
end;

procedure  TRTSPManager.OnDisconnect(RSRP:PRSR);
begin
end;

procedure  TRTSPManager.OnError(RSRP:PRSR);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('Socket Error:',Core.Utils.Sockets.SocketErrorToString(RSRP^.TaskError),' RSR Error:',RSRErrorToString(RSRP^.Errors)));
end;

procedure  TRTSPManager.OnInitialize(RSRP:PRSR);
var
  DataP:PRTSP;
begin
  New(DataP);
  Init(DataP^);
  RSRP^.Info.DataP:=DataP;
end;

procedure  TRTSPManager.OnFinalize(RSRP:PRSR);
var
  DataP:PRTSP;
begin
  DataP:=RSRP^.Info.DataP;
  If DataP<>Nil then begin
    Try
      EntryPoint:='TRTSPManager.OnFinalize.Done';
      Done(DataP^);
    Finally
      RSRP^.Info.DataP:=Nil;
      Dispose(DataP);
    end;
  end;
end;

procedure  TRTSPManager.OnDNSResult(RSRP:PRSR);
begin

end;

function TRTSPManager.ResolvePath(Path:Core.Strings.VarString; out dsFolder:TDSFolder; out dsFile:TDSFile):boolean;
var
  iCount   : LongInt;
  iLength  : LongInt;
  saPath   : Core.Arrays.Types.VarString;
  FileName : Core.Strings.VarString;
  Ext      : Core.Strings.VarString;
begin
  iLength:=System.Length(FRequest.Resource.Path);
  if (iLength>0) and (FRequest.Resource.Path[1] = '/') then System.Delete(FRequest.Resource.Path,1,1);
  Core.Arrays.VarString.fromString(@saPath,Path,'/',[soClearList,soIgnoreDelimAtStart]);
  dsFolder:=nil;
  dsFile:=nil;
  iCount:=Length(saPath);
  FileName:=saPath[iCount-1];
  Ext:=Core.Utils.Files.Extract(FileName,efeoNone);
  if (Length(Ext)>0) then begin
    Core.Arrays.VarString.Remove(saPath,iCount-1);
    Dec(iCount);
  end;
end;

procedure TRTSPManager.pushDescribe(RSRP:PRSR);
var
  dsFolder : TDSFolder;
  dsFile   : TDSFile;
  mItem    : Storage.RTSP.Manifest.TItem;
  sItem    : Storage.RTSP.SDP.TItem;
  DataP    : PRTSP;
begin
  DataP:=RSRP^.Info.DataP;

  if Core.Arrays.VarString.Find(FRequest.Accepts,applicationSDP) then begin
    if ResolvePath(FRequest.Resource.Path,dsFolder,dsFile) then begin
      Try
        Try
          if Storage.RTSP.Manifest.DB.Read(Task,Owner.RootDomain.ID,UA_ANY_ACCOUNT,dsFolder.ID,dsFile.ID, Storage.AuraDisks.Kinds.Domain, mItem) then begin
            if Storage.RTSP.SDP.Read(Task,mItem.ID,sItem) then begin
              FResponse.Reset();
              FResponse.contentType:=applicationSDP;
              FResponse.Content:=sItem.Data;
              FResponse.contentLength:=System.Length(sItem.Data);
            end else begin
              FResponse.Reset();
              FResponse.Prepare(SCI_Internal_Server_Error,FRequest.Sequence,FRequest.Session);
              FResponse.Send(RSRP,Refactor);
            end;
          end else begin
            FResponse.Reset();
            FResponse.Prepare(SCI_Internal_Server_Error,FRequest.Sequence,FRequest.Session);
            FResponse.Send(RSRP,Refactor);
          end;
        finally
          Storage.RTSP.SDP.Done(sItem);
        end;
      finally
        Storage.RTSP.Manifest.Done(mItem);
      end;
    end else begin
      FResponse.Reset();
      FResponse.Prepare(SCI_Not_Found,FRequest.Sequence,FRequest.Session);
      FResponse.Send(RSRP,Refactor);
    end;
  end else begin
    FResponse.Reset();
    FResponse.Prepare(SCI_Unsupported_Transport,FRequest.Sequence,FRequest.Session);
    FResponse.Send(RSRP,Refactor);
  end;
end;

procedure TRTSPManager.pushAnnounce(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushGetParameter(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushOptions(RSRP:PRSR);
var
  DataP:PRTSP;
begin
  DataP:=RSRP^.Info.DataP;
  if (FRequest.Resource.Path='*') then begin
    FResponse.Reset();
    FResponse.Prepare(SCI_OK,FRequest.Sequence,FRequest.Session);
    Core.Arrays.KeyString.Add(FResponse.Headers,fieldPublic,PUBLIC_OPTIONS);
    FResponse.Send(RSRP,Refactor);
  end else if ( (SameText(FRequest.Resource.Server,Owner.RootDomain.Name))  or  (FRequest.Resource.Server=InAddrToStr(Owner.FMatrixNode.IP))  )then begin
    // Use FAT to Request stuff
    // if FileFound else 404
    // Just testing
    FResponse.Reset();
    FResponse.Prepare(SCI_OK,FRequest.Sequence,FRequest.Session);
    Core.Arrays.KeyString.Add(FResponse.Headers,fieldPublic,PUBLIC_OPTIONS);
    FResponse.Send(RSRP,Refactor);
    //Core.Streams.toFile(RSRP^.SendBuffer.Stream,'/rtsp-out.txt');
  end else begin
    FResponse.Reset();
    FResponse.Prepare(SCI_Destination_Unreachable,FRequest.Sequence,FRequest.Session);
    FResponse.Send(RSRP,Refactor);
  end;
end;

procedure TRTSPManager.pushPause(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushPlay(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushRecord(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushRedirect(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushSetup(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushSetParameter(RSRP:PRSR);
begin

end;

procedure TRTSPManager.pushTeardown(RSRP:PRSR);
begin

end;

procedure   TRTSPManager.pushMethodUnknown(RSRP:PRSR);
begin

end;

procedure   TRTSPManager.OnDataReceived(RSRP:PRSR; Var Handled:Boolean);
var
  DataP:PRTSP;

  function CloseSession:Boolean;
  begin
    Result:=(RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) or (DataP^.ErrorCount>5) or (DataP^.State or RS_TEARDOWN=DataP^.State);
  end;

begin
  Handled:=true;
  DataP:=RSRP^.Info.DataP;
  If DataP<>Nil then begin
    //Core.Streams.toFile(RSRP^.RecvBuffer.Stream,'/rtsp-in.txt');
    If ( RSR.EndOfLine(RSRP^.RecvBuffer) and FRequest.containsRequest(RSRP^.RecvBuffer) ) then begin
      // pipeline requests
      Repeat
        if FRequest.Error=0 then begin
          Case FRequest.Method of
            cmdDescribe     : pushDescribe(RSRP);
            cmdAnnounce     : pushAnnounce(RSRP);
            cmdGetParameter : pushGetParameter(RSRP);
            cmdOptions      : pushOptions(RSRP);
            cmdPause        : pushPause(RSRP);
            cmdPlay         : pushPlay(RSRP);
            cmdRecord       : pushRecord(RSRP);
            cmdRedirect     : pushRedirect(RSRP);
            cmdSetup        : pushSetup(RSRP);
            cmdSetParameter : pushSetParameter(RSRP);
            cmdTeardown     : pushTeardown(RSRP);
          else
            pushMethodUnknown(RSRP);
          end;
        end else begin
          // PushRequest Error
        end;
      until (  (FRequest.containsRequest(RSRP^.RecvBuffer)=false) or CloseSession or Terminated);
    end;
    If CloseSession then
      Close(RSRP);
    If (DataP^.UAP<>Nil) and (DataP^.UAP^.Modified) then begin
      EntryPoint:='TRTSPManager.OnDataReceived.UserAccount_UpdateMods';
      Storage.UserAccounts.Items.DB.UpdateMods(Task,DataP^.UAP^);
      DataP^.UAP^.Modified:=False;
    end;
    If CloseSession then
      Close(RSRP);
  end else begin
    Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TRTSPManager.OnDataReceived.Process Error:','Null pointer to Data'));
    Close(RSRP);
  end;
end;

procedure   TRTSPManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

finalization
  ShutdownTimers();
end.

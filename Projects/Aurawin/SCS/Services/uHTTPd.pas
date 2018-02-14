{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit uHTTPd;

interface
uses
  Classes,

  hHTTPd,

  RSR,
  RSR.DNS,
  RSR.HTTP,
  RSR.Core,


  App,
  App.Consts,
  App.Build,

  Encryption.Zip,
  Encryption.zStream,
  Encryption.Base64,
  Encryption.SHA,

  Core.Timer,
  Core.Streams,
  Core.Logging,

  Core.Strings,
  Core.Keywords,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Boolean,

  Core.Utils.Sockets,
  Core.Utils.Files,
  Core.Utils.Time,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Storage,
  Storage.Main,
  Storage.Domains,
  Storage.FAT,
  Storage.HTTPLogs,
  Storage.UserAccounts,
  Storage.CoreObjects,
  Storage.Calendaring,
  Storage.RSS,
  Storage.Keywords,
  Storage.ContentTypes,
  Storage.UserStorage,
  Storage.SrchProviders,
  Storage.MatrixClusters,
  Storage.MatrixResources,
  Storage.MatrixNodes,
  Storage.KeepAlive,
  Storage.Certs,
  Storage.Social,
  Storage.Intrusion,

  Sockets,
  SysUtils,
  MD5,
  Process;

Type
  THTTPManager=class;
  {$i uHTTPd.WebSocket.Type.inc}

  {$i uHTTPd.Transport.HTTP.Type.inc}
  {$i uHTTPd.Transport.AuraCore.Net.Type.inc}
  {$i uHTTPd.Transport.Media.Type.inc}

  THTTPServer=Class(TRSRServer)
  private
    FManagers               : TRSRManagers;
    //FProxies                : TRSRManagers;
    FSQLLock                : TRTLCriticalSection;
    FSQLQueue               : TStringList;
    FResources              : TDSFAT;

    sCalendar_Previous      : Core.Strings.VarString;
    sCalendar_Current       : Core.Strings.VarString;
    sCalendar_Next          : Core.Strings.VarString;
    FProxyIndex             : LongInt;

    FKwdTimer               : Core.Timer.Item;
    FRSSTimer               : Core.Timer.Item;
    FTemplateTimer          : Core.Timer.Item;


    FKeywords               : TKeywords;

  private
    procedure  _OnRSSTimer(ItemP:Core.Timer.PItem);
    procedure  _OnKeywordTimer(ItemP:Core.Timer.PItem);
    procedure  _OnTemplateTimer(ItemP:Core.Timer.PItem);
    function   _OnTemplateizeItem(Folder:TDSFolder; Item:TDSFile):TTemplate;
  protected
    procedure  OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure  OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    RootDomain     : Storage.Domains.Items.TDomain;
    RootUser       : Storage.UserAccounts.Items.Item;
    DefaultUser    : Storage.UserAccounts.Items.Item;
    MatrixCluster  : Storage.MatrixClusters.Cluster.Item;
    MatrixResource : Storage.MatrixResources.Resource.Item;
    MatrixNode     : Storage.MatrixNodes.Node.Item;

  public
    Destructor  Destroy; override;
    Constructor Create(ADomain:Core.Strings.VarString; Const aNodeID,aIP:QWord; aPort,aScale:Word; UseSSL:Boolean); reintroduce;
  public
    property    Resources:TDSFAT read FResources;
  end;

  THTTPManager=Class(TRSRManager)
  private
    DataP               : PHTTP;
    FLogItem            : Storage.HTTPLogs.Items.Item;
    FLogItems           : Storage.HTTPLogs.Items.List;
    FUserAccounts       : Storage.UserAccounts.Items.TList;
    Owner               : THTTPServer;
    FCoreObjects        : TCoreObjects;
    FTransportHT        : TTransportHTTP;
    FTransportMedia     : TTransportMedia;
    FTransportAuraCore  : TTransportAuraCore;
 protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
 protected
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  public
    Constructor Create(AOwner:THTTPServer; UseSSL:Boolean); reintroduce;
    Destructor  Destroy; override;
  end;

procedure StartBackgroundTimers();

implementation
uses
  coSearch,
  coSyndication,
  coProviders,
  coLogin,
  coSessions,
  DateUtils,
  Math;

var
  TemplateTimer                  : Core.Database.Types.TTimer;
  KeywordTimer                   : Core.Database.Types.TTimer;
  ProviderTimer                  : Core.Database.Types.TTimer;
  RSSTimer                       : Core.Database.Types.TTimer;
  CalTimer                       : Core.Database.Types.TTimer;

procedure StartBackgroundTimers();
begin
  if TemplateTimer=nil then begin
    TemplateTimer:=Core.Database.Types.TTimer.Create(Storage.Main.Header,tpNormal);
    TemplateTimer.Task.Name:='uHTTPd.TemplateTimer';
  end;
  if KeywordTimer=nil then begin
    KeywordTimer:=Core.Database.Types.TTimer.Create(Storage.Main.Header,tpNormal);
    KeywordTimer.Task.Name:='uHTTPd.KeywordTimer';
  end;
  if ProviderTimer=nil then begin
    ProviderTimer:=Core.Database.Types.TTimer.Create(Storage.Main.Header,tpNormal);
    ProviderTimer.Task.Name:='uHTTPd.ProviderTimer';
  end;
  if RSSTimer=nil then begin
    RSSTimer:=Core.Database.Types.TTimer.Create(Storage.Main.Header,tpNormal);
    RSSTimer.Task.Name:='uHTTPd.RSSTimer';
  end;
end;

procedure ShutdownTimers();
begin
  if (TemplateTimer<>nil) then  TemplateTimer.Terminate();
  TemplateTimer:=nil;
  if (KeywordTimer<>nil) then  KeywordTimer.Terminate();
  KeywordTimer:=nil;
  if (ProviderTimer<>nil) then  ProviderTimer.Terminate();
  ProviderTimer:=nil;
  if (RSSTimer<>nil) then  RSSTimer.Terminate();
  RSSTimer:=nil;
  if (CalTimer<>nil) then  CalTimer.Terminate();
  CalTimer:=nil;
end;

{$i uHTTPd.WebSocket.Code.inc}
{$i uHTTPd.Transport.HTTP.Code.inc}
{$i uHTTPd.Transport.AuraCore.Net.Code.inc}
{$i uHTTPd.Transport.Media.Code.inc}

Constructor THTTPServer.Create(ADomain:Core.Strings.VarString; Const aNodeID,aIP:QWord; aPort,aScale:Word; UseSSL:Boolean);
var
  iLcv                           : LongInt;
  Request                        : Storage.Keywords.Items.DB.Request;
  Node_Service                   : Array[Boolean] of Core.Strings.VarString = ('http','https');
  ba                             : Core.Arrays.Types.Bytes;
begin
  Inherited Create(@FManagers,tTCP,aIP,aPort,aScale,UseSSL,CREATE_SUSPENDED);

  FProxyIndex:=-1;


  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(UpperCase(Node_Service[UseSSL]),' Server'));

  InitCriticalSection(FSQLLock);

  FResources:=TDSFAT.Create();

  Storage.Domains.Items.Init(RootDomain);

  RootDomain.Name:=ADomain;

  Storage.Domains.Items.DB.Fill(Task,RootDomain);
  Storage.Calendaring.Items.DB.Fill(Task,RootDomain.ID,RootDomain.EventsP^);
  Storage.RSS.Channels.DB.Fill(Task,RootDomain.ID,RootDomain.FeedsP^);

  RootUser.DomainID:=RootDomain.ID;
  DefaultUser.DomainID:=RootDomain.ID;
  Storage.UserAccounts.Items.DB.Fill(Task,RootDomain.Root,RootUser);
  Storage.UserAccounts.Items.DB.Fill(Task,UA_DEFAULT_ACCOUNT,DefaultUser);

  if RootDomain.CertID<>0 then begin
    Storage.Certs.Items.DB.Read(Task,RootDomain.ID,RootDomain.CertID,Cert);
    FSSLInfo.keyLen:=Core.Arrays.Bytes.Copy(Cert.DerKey,FSSLInfo.keyData);
    Load(Cert,FSSLInfo.Manifest);
  end;

  FSecure:=UseSSL;

  Storage.MatrixClusters.Cluster.DB.Fill(Task,ClusterID,MatrixCluster);
  Storage.MatrixResources.Resource.DB.Fill(Task,ResourceID,MatrixResource);
  Storage.MatrixNodes.Node.DB.Fill(Task,aNodeID,MatrixNode);

  FSQLQueue:=TStringList.Create;
  FreeOnTerminate:=True;

  if (RootDomain.ID=0) then
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,FService,Concat('THTTPServer.Create Exception: No Domain is bound to this node!'));

  FResources.Load(RootDomain.ID,Storage.ClusterID,Storage.ResourceID,aNodeID,dsfatloBoth);
  iLcv:=0;
  While iLcv<aScale do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=THTTPManager.Create(Self,FSecure);
    Except
      iLcv:=aScale;
    end;
    Inc(iLcv);
  end;

  Request.cbDefault:=@cbKW_Loopback;
  Request.DomainID:=RootDomain.ID;
  Request.KeywordsP:=@FKeywords;
  Storage.Keywords.Items.DB.Fill(Task,Request);

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

  Core.Keywords.Add('cluster_name',MatrixCluster.Group,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('cluster_id',IntToStr(MatrixCluster.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('resource_name',MatrixResource.Name,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('resource_id',IntToStr(MatrixResource.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_name',MatrixNode.Alias,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_address',InAddrToStr(MatrixNode.IP),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_id',IntToStr(MatrixNode.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('node_service',Node_Service[UseSSL],FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);

  Core.Keywords.Add('domain_id',IntToStr(RootDomain.ID),FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('domain_name',RootDomain.Name,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);
  Core.Keywords.Add('domain_alias',RootDomain.FriendlyName,FKeywords,KW_REFRESH_OFF,NO_CALLBACK,NO_CALLBACK);

  Core.Keywords.Add('uptime_weeks','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Weeks);
  Core.Keywords.Add('uptime_days','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Days);
  Core.Keywords.Add('uptime_hours','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Hours);
  Core.Keywords.Add('uptime_minutes','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Minutes);
  Core.Keywords.Add('uptime_seconds','',FKeywords,KW_REFRESH_OFF,@cbKW_Loopback,@cbKW_Uptime_Seconds);

  FResources.Scan(Task,@FKeywords);

  Core.Timer.Init(FRSSTimer);
  FRSSTimer.Event:=@_OnRSSTimer;
  FRSSTimer.Location:='uHttpd._OnRSSTimer';
  FRSSTimer.Mode:=temNormal;
  FRSSTimer.Expires:=IncMinute(Core.Timer.dtNow,1);

  Core.Timer.Init(FKwdTimer);
  FKwdTimer.Event:=@_OnKeywordTimer;
  FKwdTimer.Location:='uHttpd._OnKeywordTimer';
  FKwdTimer.Mode:=temNormal;
  FKwdTimer.Expires:=IncMinute(Core.Timer.dtNow,1);


  Core.Timer.Init(FTemplateTimer);
  FTemplateTimer.Event:=@_OnTemplateTimer;
  FTemplateTimer.Location:='uHttpd._OnTemplateTimer';
  FTemplateTimer.Mode:=temNormal;
  FTemplateTimer.Expires:=IncSecond(Core.Timer.dtNow,1);

  KeywordTimer.RegisterEvent(FKwdTimer,LoadNoUpdate);
  TemplateTimer.RegisterEvent(FTemplateTimer,LoadNoUpdate);

  RSSTimer.RegisterEvent(FRSSTimer,LoadNoUpdate);
end;

Destructor THTTPServer.Destroy;
begin
  KeywordTimer.UnloadEvent(FKwdTimer,UnloadNoExecute);
  TemplateTimer.UnloadEvent(FTemplateTimer,UnloadNoExecute);
  RSSTimer.UnloadEvent(FRSSTimer,UnloadNoExecute);

  FreeAndNil(FResources);
  FSQLQueue.Free;
  Core.Keywords.Done(FKeyWords);
  Storage.Domains.Items.Empty(RootDomain);
  Storage.Domains.Items.Done(RootDomain);
  DoneCriticalSection(FSQLLock);

  Inherited Destroy; // Module and Task info is cleared here anyway
end;

function  THTTPServer._OnTemplateizeItem(Folder:TDSFolder; Item:TDSFile):TTemplate;
var
  Template:TTemplate;
begin
  Template:=Folder.Template;
  If (Item.Downloaded=0) then
    Item.Load(TemplateTimer.Task);
  If (Template=nil) then begin
     Template:=TTemplate.Create(Item.Modified);
     Template.Load(TemplateTimer.Task,Folder,Item);
  end else if (
    ( (MillisecondsBetween(Item.Modified,Template.Modified)>TOL_MODIFIED_MS) or (Template.Stale=true) ) and
    ( (Item.HasKeywords=false) or ( (Item.HasKeywords=true) and (Item.Stale=false)) )
  )
  then
    Template.Load(TemplateTimer.Task,Folder,Item);
  Result:=Template;
end;

procedure  THTTPServer._OnTemplateTimer(ItemP:Core.Timer.PItem);
begin
  Try
    Try          // read only no create items
      FResources.Templatetize(Storage.FAT.INI_NAME,@_OnTemplateizeItem);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat('THTTPServer._OnTemplateTimer Exception: ',E.Message));
    End;
  Finally
    ItemP^.Expires:=IncMilliSecond(Core.Timer.dtNow,TEMPLATE_REBUILD_DELAY);
  end;
end;

procedure  THTTPServer._OnKeywordTimer(ItemP:Core.Timer.PItem);
var
  bKWStale:Boolean;
begin
  bKWStale:=false;
  Storage.Keywords.Items.DB.Refresh(KeywordTimer.Task,RootDomain.ID,FKeywords,bKWStale);
  if bKWStale then
     FResources.Flush;
  ItemP^.Expires:=IncSecond(Core.Timer.dtNow,30);
end;

procedure THTTPServer._OnRSSTimer(ItemP:Core.Timer.PItem);
begin
  Storage.RSS.Channels.DB.Fill(RSSTimer.Task,RootDomain.ID,RootDomain.FeedsP^);
  ItemP^.Expires:=IncSecond(Core.Timer.dtNow,App.Consts.RSS_REFRESH);
end;

procedure   THTTPServer.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure   THTTPServer.OnError(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

Constructor THTTPManager.Create(AOwner:THTTPServer; UseSSL:Boolean);
begin
  if (UseSSL=true) then
    FService:=SERVICE_HTTPS
  else
    FService:=SERVICE_HTTP;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Manager'));

  Storage.HTTPLogs.Items.Init(FLogItem);
  Storage.HTTPLogs.Items.Init(FLogItems);
  Owner:=AOwner;

  FUserAccounts:=Storage.UserAccounts.Items.TList.Create(@Owner.RootDomain,FService);

  FCoreObjects:=TCoreObjects.Create(Storage.Main.Header,Owner,Self,@aOwner.MatrixNode, @Owner.RootDomain,@Owner.RootUser,@Owner.DefaultUser,@Owner.FKeywords,Owner.FResources,FUserAccounts);
  FCoreObjects.Load();
  FCoreObjects.VerifyFAT();

  FTransportHT:=TTransportHTTP.Create(Self);
  FTransportMedia:=TTransportMedia.Create(Self);
  FTransportAuraCore:=TTransportAuraCore.Create(Self,FTransportHT.FRequest,FTransportHT.FResponse);


  Inherited Create(AOwner,@AOwner.FSSLInfo,UseSSL,THREAD_METHODS_OFF,HTTP_STACK_SIZE);

  TimeOut:=120000;

end;

Destructor  THTTPManager.Destroy;
begin
  FreeAndNil(FCoreObjects);

  FreeAndNil(FTransportHT);
  FreeAndNil(FTransportMedia);
  FreeAndNil(FTransportAuraCore);

  FreeAndNil(FUserAccounts);

  Storage.HTTPLogs.Items.Done(FLogItem);
  Storage.HTTPLogs.Items.Done(FLogItems);

  Inherited Destroy; // Leave DBMS to be deallocated super.
end;

procedure   THTTPManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

procedure  THTTPManager.OnQueue(RSRP:PRSR);
begin
end;

procedure  THTTPManager.OnConnect(RSRP:PRSR);
begin
end;

procedure  THTTPManager.OnDisconnect(RSRP:PRSR);
begin
end;

procedure  THTTPManager.OnError(RSRP:PRSR);
begin
end;


procedure  THTTPManager.OnDNSResult(RSRP:PRSR);
begin

end;

procedure  THTTPManager.OnDataReceived(RSRP:PRSR; var Handled:boolean);
begin
  TTransportBase(RSRP^.Transport).OnDataReceived(RSRP^,Handled);
  TTransportBase(RSRP^.Transport).Reset();
end;

procedure  THTTPManager.OnInitialize(RSRP:PRSR);
begin
  New(DataP);
  hHTTPd.Init(DataP^);
  RSRP^.Transport:=FTransportHT;
  RSRP^.Info.DataP:=DataP;
end;

procedure  THTTPManager.OnFinalize(RSRP:PRSR);
begin
  DataP:=RSRP^.Info.DataP;
  If ((DataP)<>Nil)then begin
    if DataP^.WebSocket then begin
      {$ifdef cpu64}
        System.InterLockedDecrement64(RSR_Stream_Count);
      {$else}
        System.Dec(RSR_Stream_Count);
      {$endif}
    end;
    hHttpd.Done(DataP^);
    Dispose(DataP);
    RSRP^.Info.DataP:=nil;
  end;
end;



finalization
  ShutdownTimers();
end.

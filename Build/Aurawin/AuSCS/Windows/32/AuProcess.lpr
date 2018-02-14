program AuProcess;
uses
  {$IFDEF UNIX}
  CThreads,BaseUnix,
  {$ENDIF}
  Classes, hSRConsts, hRSR, uRSR, uPOP3d, uPOP3SSLd, uSMTPd, uSMTPSSLd, hHTTP, uHTTPd, uXMPPd,
  uRTSPd, uStorage, hTimer, uTimer, hDatabase, uProviders, uDatabase, uAppBuild, uSocketUtils,
  uKPList, uStringArray, uInt64Array, dbmMatrixServices, dbmMatrixClusters, dbmMatrixResources,
  dbmMatrixNodes, dbmDomains,dbmConfigData, dbmContentTypes, dbmSrchProviders, dbmSecurity,
  dbmCoreObjects, uCoreObjects,  dbmKeepAlive, dbmDNS, uSSL, DateUtils, dbmAuraDisks,

  {$i coList.Uses.inc}

  ,SysUtils, uLogging;

Type
  AuraProcess=class
  private
    FServiceDefaults             : TMatrixServiceDefaults; // default services (not domain specific)
    FService                     : TMatrixService;
    FDomain                      : TDomain;
    FParams                      : TStringArray;
    FParamKeys                   : TKPList;
    FCluster                     : TMatrixCluster;       // Each node as an instance of this structure
    FResource                    : TMatrixResource;      // Hardware/virtual Resource asset
    FNode                        : TMatrixNode;
    FServer                      : TRSRServer;


    FProcessGroupName            : TCFG_Data;
    FProcessGroupID              : TCFG_Data;
    FProcessUserName             : TCFG_Data;
    FProcessUserID               : TCFG_Data;

    FRaidGroupName               : TCFG_Data;
    FRaidGroupID                 : TCFG_Data;

    FRaidUserName                : TCFG_Data;
    FRaidUserID                  : TCFG_Data;

    FMatrixSetup                 : boolean;
    FMatrixFail                  : boolean;
  private
    procedure OnDBMSConnected(SessionID:TSessionHandle);
    procedure OnDBMSDisconnected(SessionID:TSessionHandle);
    procedure OnDBMSException(sProcedure,sLocation,sTable,sTask,sError:String);
    procedure OnServerListening(Server:TRSRServer);
    procedure OnServerEngineFailure(Server:TRSRServer);
  private
    function  SetupStorage:boolean;
    procedure SetupDNS;
    procedure LoadCoreObjects;
    procedure LoadSearchProviders;
    procedure LoadContentTypes;
    function  SetupMatrix:boolean;
    procedure SetupParams;
    procedure SetupTimers;
    procedure SetupDomain;
    procedure InitializeNodeConfig;
  private
    function  StartSMTP:TRSRServer;
    function  StartSMTPS:TRSRServer;
    function  StartSMTPSO:TRSRServer;
    function  StartPOP3:TRSRServer;
    function  StartPOP3S:TRSRServer;
    function  StartHTTP:TRSRServer;
    function  StartHTTPS:TRSRServer;
    function  StartXMPPCToS:TRSRServer;
    function  StartXMPPSToS:TRSRServer;
  private
    procedure StartServer;
  public
    constructor Run;
    destructor Destroy; override;
  end;

  procedure AuraProcess.SetupDNS;
  begin
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupDNS','DNS loading.');
    {$endif}
    dbmDNS.StartupDNS;
    dbmDNS.DNS.Load(uStorage.Main.Module,uStorage.Main.Task);
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupDNS',Concat(IntToStr(Length(dbmDNS.DNS.Items)),' DNS loaded.'));
    {$endif}
  end;

  procedure AuraProcess.LoadCoreObjects;
  begin
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.LoadCoreObjects','Core Objects loading.');
    {$endif}
    {$i coList.Injections.inc}
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.LoadCoreObjects',Concat(IntToStr(Length(CoreObjectItems)),' Core Objects loaded.'));
    {$endif}
  end;

  function AuraProcess.SetupStorage:boolean;
  var
    bMask:byte;
  begin
    Result:=false; bMask:=0;
    if (Length(uStorage.Main.Header.Username)=0) then begin
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupStorage',Concat('Entry for Username cannot be empty.  Please check INI File.'));
      bMask:=1 shl 0;
    end;
    if (Length(uStorage.Main.Header.Schema)=0) then begin
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupStorage',Concat('Entry for Schema cannot be empty.  Please check INI File.'));
      bMask:=1 shl 1;
    end;
    if (Length(uStorage.Main.Header.HostName)=0) then begin
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupStorage',Concat('Entry for Hostname cannot be empty.  Please check INI File.'));
      bMask:=1 shl 2;
    end;
    if bMask=0 then begin
      Result:=true;
      uStorage.Main.Header.OnConnected:=@OnDBMSConnected;
      uStorage.Main.Header.OnDisconnected:=@OnDBMSDisconnected;

      uStorage.Main.Module.hThreadID:=GetCurrentThreadID;
      uStorage.Main.Module.hWindow:=-1;
      uStorage.Main.Module.OnException:=@OnDBMSException;
      uStorage.Main.Module.SessionID:=uDatabase.Allocate(Main.Header,Main.Module);

      uStorage.Main.Task.TaskID:=uDatabase.Allocate(uStorage.Main.Module.SessionID,uStorage.Main.Task);
    end;
  end;

  Procedure AuraProcess.OnDBMSConnected(SessionID:TSessionHandle);
  begin
    if FMatrixFail=false then
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.OnDBMSConnected',Concat('DBMS has connected Node ',FNode.Alias,' (',IntToStr(FNode.ID),')'));
  end;


  procedure AuraProcess.OnDBMSDisconnected(SessionID:TSessionHandle);
  begin
    SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.OnDBMSDisconnected',Concat('DBMS has disconnected Node ',FNode.Alias,' (',IntToStr(FNode.ID),')'));
    Halt(EXIT_CODE_DBMS_FAIL);
  end;

  function AuraProcess.StartSMTP:TRSRServer;
  begin
    SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartSMTP',Concat(Service_SMTP,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    Try
      FService.Service:=uSMTPd.TReceiveMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartSMTP',Concat(Service_SMTP,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartSMTPS:TRSRServer;
  begin
    SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartSMTPS',Concat(Service_SMTP_SSL,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    Try
      FService.Service:=uSMTPSSLd.TReceiveMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartSMTPS',Concat(Service_SMTP,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartSMTPSO:TRSRServer;
  begin
    SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartSMTPSO',Concat(Service_SMTP_SSL,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    Try
      FService.Service:=uSMTPSSLd.TReceiveMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartSMTPSO',Concat(Service_SMTP,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartPOP3:TRSRServer;
  begin
    Try
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartPOP3',Concat(Service_POP3,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
      FService.Service:=uPOP3d.TPOPMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartPOP3',Concat(Service_POP3,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartPOP3S:TRSRServer;
  begin
    Try
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartPOP3S',Concat(Service_POP3_SSL,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
      FService.Service:=uPOP3SSLd.TPOPMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartPOP3S',Concat(Service_POP3,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartXMPPCToS:TRSRServer;
  begin
    Try
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartXMPPCToS',Concat(Service_XMPP,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
      FService.Service:=TXMPPClientSessionServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartXMPPCToS',Concat(Service_XMPP,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartXMPPSToS:TRSRServer;
  begin
    Try
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartXMPPSToS',Concat(Service_XMPP,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
      FService.Service:=TXMPPServerToServerServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartXMPPSToS',Concat(Service_XMPP,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartHTTP:TRSRServer;
  begin
    Try
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartHTTP',Concat(Service_HTTP,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
      LoadCoreObjects();
      LoadSearchProviders();
      LoadContentTypes();
      FService.Service:=THTTPServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,SSL_OFF,CREATE_START);
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartHTTP',Concat(Service_HTTP,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function AuraProcess.StartHTTPS:TRSRServer;
  begin
    Try
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartHTTPS',Concat(Service_HTTPS,' Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',uSocketUtils.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
      LoadCoreObjects();
      LoadSearchProviders();
      LoadContentTypes();
      FService.Service:=THTTPServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,SSL_ON,CREATE_START);
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartHTTPS',Concat(Service_HTTPS,' Certificate: ',IntToStr(THTTPServer(FService.Service).Certificate.ID)));
      TRSRServer(FService.Service).OnListening:=@OnServerListening;
      TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    Except
      On E:Exception do SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.StartHTTPS',Concat(Service_HTTPS,' Exception: ',E.Message));
    end;
    Result:=TRSRServer(FService.Service);
  end;

  function  AuraProcess.SetupMatrix:boolean;
  const
    YES_NO:array[boolean] of String=('no','yes');
  begin
    Result:=False;
    if FMatrixSetup then exit;
    dbmMatrixServices.MatrixServices_Fill(uStorage.Main.Module,uStorage.Main.Task,FServiceDefaults);
    if (FService.ID=0) then begin
      dbmMatrixNodes.MatrixNode_Fill(uStorage.Main.Module,uStorage.Main.Task,FService.NodeID,FNode);
      FService.ClusterID:=FNode.ClusterID;
      FService.ResourceID:=FNode.ResourceID;
      FService.DomainID:=FNode.DomainID;
      dbmMatrixClusters.Cluster_Fill(uStorage.Main.Module,uStorage.Main.Task, FService.ClusterID,FCluster);
      dbmMatrixResources.MatrixResource_Fill(uStorage.Main.Module,uStorage.Main.Task,FService.ResourceID,FResource);
      dbmMatrixServices.MatrixServices_Fill(uStorage.Main.Module,uStorage.Main.Task,FService,msfoUseCRNDK);
    end else begin;
      dbmMatrixServices.MatrixServices_Fill(uStorage.Main.Module,uStorage.Main.Task,FService,msfoUseID);
      dbmMatrixClusters.Cluster_Fill(uStorage.Main.Module,uStorage.Main.Task, FService.ClusterID,FCluster);
      dbmMatrixResources.MatrixResource_Fill(uStorage.Main.Module,uStorage.Main.Task,FService.ResourceID,FResource);
      dbmMatrixNodes.MatrixNode_Fill(uStorage.Main.Module,uStorage.Main.Task,FService.NodeID,FNode);
    end;
    dbmMatrixServices.Copy(FService,dbmMatrixServices.Current);
    if (FService.Scale=0) then
      FService.Scale:=FServiceDefaults[FService.Kind].Scale;
    if (FService.Port=0) then
      FService.Port:=FServiceDefaults[FService.Kind].Port;
    Result:=FService.Enabled;
    FMatrixSetup:=True;
    if FService.Enabled=false then begin
      FMatrixFail:=true;
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupMatrix',Concat('Could not load service id=',IntToStr(FService.ID),' node=',IntToStr(FService.NodeID),' domain=',IntToStr(FService.DomainID),' kind=',IntToStr(FService.Kind),' enabled=',YES_NO[FService.Enabled]));
    end;
  end;

  procedure    AuraProcess.SetupDomain;
  begin
    FDOmain.ID:=FService.DomainID;
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupDomain',Concat('Inspecting domain id=',IntToStr(FService.DomainID)));
    {$endif}
    if not dbmDomains.Domain_Fill(uStorage.Main.Module,uStorage.Main.Task,FDomain) then
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupDomain.Domain_Fill',Concat('Could not load domain id=',IntToStr(FService.DomainID)));
  end;

  procedure   AuraProcess.SetupParams;
  var
    iCount:integer;
    iLcv:integer;
  begin
    SetLength(FParams,ParamCount);
    for iLcv:=1 to ParamCount do
      FParams[iLcv-1]:=ParamStr(iLcv);
    uKPList.fromStringArray(FParamKeys,FParams,'=');

    iCount:=System.Length(FParamKeys);

    FCluster.ID:=uKPList.GetItemAsQword(FParamKeys,'cluster',iCount,0);
    FResource.ID:=uKPList.GetItemAsQword(FParamKeys,'resource',iCount,0);
    FNode.ID:=uKPList.GetItemAsQword(FParamKeys,'node',iCount,0);
    FService.ID:=uKPList.GetItemAsQword(FParamKeys,'service',iCount,0);
    FService.Kind:=uKPList.GetItemAsQword(FParamKeys,'kind',iCount,0);
    FService.ClusterID:=FCluster.ID;
    FService.ResourceID:=FResource.ID;
    FService.NodeID:=FNode.ID;
  end;

  procedure  AuraProcess.LoadContentTypes;
  begin
    dbmContentTypes.LoadContentTypes(uStorage.Main.Module,uStorage.Main.Task);
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.LoadContentTypes',Concat(IntToStr(Length(ContentTypes)),' ContentTypes loaded.'));
    {$endif}
  end;

  procedure  AuraProcess.LoadSearchProviders;
  var
    iCt,iManLcv,iLcv:integer;
  begin
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.LoadSearchProviders','Loading');
    {$endif}
    dbmSrchProviders.Providers_List(uStorage.Main.Module,uStorage.Main.Task,dbmSrchProviders.SearchProviders);
    iCt:=System.Length(dbmSrchProviders.SearchProviders);
    for iLcv:=0 to iCt-1 do begin
      SetLength(SearchProviders[iLcv].Managers,SearchProviders[iLcv].Scale);
      for iManLcv:=0 to High(SearchProviders[iLcv].Managers) do
        SearchProviders[iLcv].Managers[iManLcv]:=uProviders.TProviderManager.Create(@SearchProviders[iLcv]);
    end;
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(FDomain.Name,'AuraProcess.LoadSearchProviders',Concat('Loaded (',IntToStr(ict),') providers'));
    {$endif}
  end;
  procedure  AuraProcess.SetupTimers();
  begin
    {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupTimers','uDatabase.StartBackgroundTimer starting.'); {$endif}
    uDatabase.StartBackgroundTimer(uStorage.Main.Header,tpNormal);
    {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupTimers','uDatabase.StartBackgroundTimer done.'); {$endif}
    if FService.Kind in [mkHTTP,mkHTTPS] then begin
      {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupTimers','uHTTPd.StartBackgroundTimers starting.'); {$endif}
      uHTTPd.StartBackgroundTimers();
      {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupTimers','uHTTPd.StartBackgroundTimers done.'); {$endif}
    end;
    if FService.Kind in [mkRealTimeStreaming] then begin
      {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupTimers','uRTSPd.StartBackgroundTimers starting.'); {$endif}
      uRTSPd.StartBackgroundTimers();
      {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.SetupTimers','uRTSPd.StartBackgroundTimers done.'); {$endif}
    end;
  end;

  procedure   AuraProcess.StartServer;
  begin
    If FService.Service<>nil then exit;
    case FService.Kind of
      mkXMPPCToS : FServer:=StartXMPPCToS;
      mkXMPPSToS : FServer:=StartXMPPSToS;
      mkPOP3     : FServer:=StartPOP3;
      mkPOP3S    : FServer:=StartPOP3S;
      mkSMTP     : FServer:=StartSMTP;
      mkSMTPS    : FServer:=StartSMTPS;
      mkSMTPSO   : FServer:=StartSMTPSO;
      mkHTTP     : FServer:=StartHTTP;
      mkHTTPS    : FServer:=StartHTTPS;
    end;
    If FServer<>nil then begin
      FServer.Active:=true;
      FServer.WaitFor();
    end;
  end;

  constructor AuraProcess.Run;
  begin
    FMatrixFail:=false;
    FMatrixSetup:=False;
    FServer:=nil;
    Init(FService);
    Init(FCluster);
    Init(FResource);
    Init(FDomain);
    Init(FNode);
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Run','Entered creation.');
    {$endif}
    SetupParams;
    if (FService.ID<>0) or ((FService.NodeID<>0) and (FService.Kind<>0) ) then begin
      uDatabase.StartupStorage;
      Try
        if SetupStorage then begin
          If uStorage.Main.Module.SessionID<>-1 then begin
            If uDatabase.Connect(uStorage.Main.Module.SessionID) then begin
              if SetupMatrix() then begin
                SetupTimers();
                SetupDomain();
                SetupDNS();
                InitializeNodeConfig();
                StartServer();
              end else begin
                SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Run','Matrix failure');
              end;
            end else begin
              SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Run','DBMS connect failure');
            end;
          end else begin
            SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Run','Storage Main Module Failed to create session');
          end;
        end else begin
          SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Run','SetupStorage Failed');
        end;
      Except
        On E:Exception do SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Run',Concat('Exception: ',E.Message));
      end;
    end else begin
      SystemLog.WriteLogEntry(
        SERVICE_PROCESS,
        'AuraProcess.Run',
        Concat(
          'Missing Parameters ID=',
          IntToStr(FService.ID),
          ' cluster=',IntToStr(FService.ClusterID),
          ' resource=',IntToStr(FService.ResourceID),
          ' node=',IntToStr(FService.NodeID),
          ' service=',IntToStr(FService.ID),
          ' kind=',IntToStr(FService.Kind)
        )
      );
    end;
  end;

  procedure AuraProcess.OnDBMSException(sProcedure,sLocation,sTable,sTask,sError:String);
  begin
    SystemLog.WriteLogEntry(FDomain.Name,Service_XMPP,Concat(sProcedure,'.',sLocation,'.',sTable,'.',sTask,':',sError));
  end;

  procedure AuraProcess.InitializeNodeConfig();
  begin
    Init(FProcessGroupName);
    FProcessGroupName.Name:=CFG_NS_OS_GROUP_NAME;
    FProcessGroupName.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FProcessGroupName);

    Init(FProcessGroupID);
    FProcessGroupID.Name:=CFG_NS_OS_GROUP_ID;
    FProcessGroupID.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FProcessGroupID);

    dbmAuraDisks.Process.GroupID:=StrToInt64Def(FProcessGroupID.Value,-1);
    dbmMatrixNodes.PROCESS_GROUP_ID:=StrToInt64Def(FProcessGroupID.Value,-1);

    Init(FProcessUserName);
    FProcessUserName.Name:=CFG_NS_OS_USER_NAME;
    FProcessUserName.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FProcessUserName);

    Init(FProcessUserID);
    FProcessUserID.Name:=CFG_NS_OS_USER_ID;
    FProcessUserID.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FProcessUserID);

    dbmAuraDisks.Process.UserID:=StrToInt64Def(FProcessUserID.Value,-1);
    dbmMatrixNodes.PROCESS_USER_ID:=StrToInt64Def(FProcessUserID.Value,-1);

    Init(FRaidGroupName);
    FRaidGroupName.Name:=CFG_NS_OS_RAID_GROUP_NAME;
    FRaidGroupName.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FRaidGroupName);

    Init(FRaidGroupID);
    FRaidGroupID.Name:=CFG_NS_OS_RAID_GROUP_ID;
    FRaidGroupID.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FRaidGroupID);

    Init(FRaidUserName);
    FRaidUserName.Name:=CFG_NS_OS_RAID_USER_NAME;
    FRaidUserName.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FRaidUserName);

    Init(FRaidUserID);
    FRaidUserID.Name:=CFG_NS_OS_RAID_USER_ID;
    FRaidUserID.DomainID:=CFG_DAT_ALL_DOMAINS;
    dbmConfigData.CFG_Data_Get(uStorage.Main.Module,uStorage.Main.Task,FRaidUserID);
  end;

  procedure AuraProcess.OnServerListening(Server:TRSRServer);
  begin
    {$ifdef Unix}
      BaseUnix.FpSetuid(StrToIntDef(FProcessUserID.Value,0));
      BaseUnix.FpSetgid(StrToIntDef(FProcessGroupID.Value,0));
    {$endif}
  end;

  procedure AuraProcess.OnServerEngineFailure(Server:TRSRServer);
  begin
    SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Failure',Concat('Module Cluster ',FCluster.Group,' Node ',FNode.Alias,' ID(',IntToStr(FNode.ID),') IP (',uSocketUtils.InAddrToStr(FNode.IP),')'));
  end;

  destructor AuraProcess.Destroy;
  var
    iProviderLcv,iManLcv:Integer;
  begin
    {$ifdef RSR_DEBUG}
      SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Destroy',Concat('Module Cluster ',FCluster.Group,' Node ',FNode.Alias,' ID(',IntToStr(FNode.ID),') IP (',uSocketUtils.InAddrToStr(FNode.IP),')'));
    {$endif}
    For iProviderLcv:=0 to High(dbmSrchProviders.SearchProviders) do begin
      for iManLcv:=0 to High(dbmSrchProviders.SearchProviders[iProviderLcv].Managers) do
        TProviderManager(dbmSrchProviders.SearchProviders[iProviderLcv].Managers[iManLcv]).Free;
    end;
    Done(dbmSrchProviders.SearchProviders);
    Done(FServiceDefaults);

    uDatabase.DeAllocate(uStorage.Main.Module.SessionID);

    ShutdownStorage;
  end;

  Procedure ProcessException(Obj : TObject; Addr : Pointer; FrameCount:Longint; Frame: PPointer);
  var
    sMessage: string;
    iLcv: LongInt;
  begin
    sMessage:=Concat('Address:',HexStr(PtrUInt(Addr), SizeOf(PtrUInt) * 2),' ');
    if Obj is exception then
      sMessage:=Concat(sMessage,Exception(Obj).ClassName,' : ',Exception(Obj).Message,' ')
    else
      sMessage:=Concat(sMessage,'Object ', Obj.ClassName);
    sMessage:=Concat(sMessage,BackTraceStrFunc(Addr),' ');

    for iLcv := 0 to FrameCount - 1 do
      sMessage:=Concat(sMessage,BackTraceStrFunc(Frame[iLcv]),' ');
    SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Exception',sMessage);
  end;
{$R *.res}
begin
  // required parameters
  // scale=# service=kind resource=id cluster=id node=id domain=id
  {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess','Process Entering'); {$endif}
  System.ExceptProc:=@ProcessException;
  AuraProcess.Run();
  {$ifdef RSR_DEBUG} SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess','Process Exiting'); {$endif}
end.


constructor AuraProcess.Run;
begin
  FMatrixFail:=false;
  FMatrixSetup:=False;
  FServer:=nil;
  FQueue:=nil;
  Storage.MatrixServices.Items.Init(FService);
  Storage.MatrixClusters.Cluster.Init(FCluster);
  Storage.MatrixResources.Resource.Init(FResource);
  Storage.MatrixNodes.Node.Init(FNode);
  Storage.Domains.Items.Init(FDomain);
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.Run','Entered creation.');
  {$endif}
  SetupParams();
  if (FService.ID<>0) or ((FService.NodeID<>0) and (FService.Kind<>0) ) then begin
    Try
      if SetupStorage then begin
        Storage.Main.Task.Connection.Connected:=true;
        If (Storage.Main.Task.Connection.Connected=true) then begin
          if SetupMatrix() then begin
            SetupTimers();
            SetupDomain();
            SetupDNS();
            InitializeNodeConfig();
            StartServer();
          end else begin
            Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.Run : Matrix failure');
          end;
        end else begin
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.Run: DBMS connect failure');
        end;
      end else begin
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.Run: SetupStorage Failed');
      end;
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.Run: Exception: ',E.Message));
    end;
  end else begin
    Core.Logging.Native.WriteLogEntry(
      DOMAIN_RSR,
      SERVICE_PROCESS,
      Concat(
        'AuraProcess.Run: ',
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


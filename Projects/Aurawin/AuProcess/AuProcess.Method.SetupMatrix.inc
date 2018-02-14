function  AuraProcess.SetupMatrix:boolean;
begin
  Result:=False;
  if FMatrixSetup then exit;
  Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FServiceDefaults);
  if (FService.ID=0) then begin
    Storage.MatrixNodes.Node.DB.Fill(Storage.Main.Task,FService.NodeID,FNode);
    FService.ClusterID:=FNode.ClusterID;
    FService.ResourceID:=FNode.ResourceID;
    FService.DomainID:=FNode.DomainID;
    Storage.MatrixClusters.Cluster.DB.Fill(Storage.Main.Task, FService.ClusterID,FCluster);
    Storage.MatrixResources.Resource.DB.Fill(Storage.Main.Task,FService.ResourceID,FResource);
    Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FService,foUseCRNDK);
  end else begin;
    Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FService,foUseID);
    Storage.MatrixClusters.Cluster.DB.Fill(Storage.Main.Task, FService.ClusterID,FCluster);
    Storage.MatrixResources.Resource.DB.Fill(Storage.Main.Task,FService.ResourceID,FResource);
    Storage.MatrixNodes.Node.DB.Fill(Storage.Main.Task,FService.NodeID,FNode);
  end;
  Storage.MatrixServices.Items.Copy(FService,Storage.MatrixServices.Current);
  if (FService.Scale=0) then
    FService.Scale:=FServiceDefaults[FService.Kind].Scale;

  if (FService.Scale=0) then
    FService.Scale:=1;

  if (FService.Port=0) then
    FService.Port:=FServiceDefaults[FService.Kind].Port;
  Result:=FService.ID<>0;
  Storage.Main.Header.ServiceName:=Storage.MatrixServices.Items.mkList[FService.Kind];
  FMatrixSetup:=True;
  FQueue:=TSystemQueue.Create(Storage.Main.Header,FService.NodeID,FService.Kind);
  if FService.Enabled=false then begin
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.SetupMatrix - Running disabled service id=',IntToStr(FService.ID),' node=',IntToStr(FService.NodeID),' domain=',IntToStr(FService.DomainID),' kind=',IntToStr(FService.Kind) ));
  end;
end;


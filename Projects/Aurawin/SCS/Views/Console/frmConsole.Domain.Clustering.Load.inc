// Load Domain Resources
tvDomainClustering.BeginUpdate;
Try
  tnAllocated:=tvDomainClustering.Items.Add(nil,'Allocated');
  tnAvailable:=tvDomainClustering.Items.Add(nil,'Available');
  tnAll:=tvDomainClustering.Items.Add(nil,'All Items');
  for iLcv:=0 to High(FClusters) do begin
    // Add Cluster Items to Available group
    tnClusterAsAllocated:=tvDomainClustering.Items.AddChild(tnAllocated,FClusters[iLcv]^.Group);
    tnClusterAsAvailable:=tvDomainClustering.Items.AddChild(tnAvailable,FClusters[iLcv]^.Group);
    tnClusterAsAll:=tvDomainClustering.Items.AddChild(tnAll,FClusters[iLcv]^.Group);
    Storage.MatrixResources.Resource.DB.List(Storage.Main.Task,FClusters[iLcv]^.ID,kplResources);
    Try
      for iRLcv:=0 to High(kplResources) do begin
        iRID:=StrToIntDef(kplResources[iRLcv]^.Key,0);
        tnResourceAsAllocated:=tvDomainClustering.Items.AddChild(tnClusterAsAllocated,kplResources[iRLcv]^.Value);
        tnResourceAsAvailable:=tvDomainClustering.Items.AddChild(tnClusterAsAvailable,kplResources[iRLcv]^.Value);
        tnResourceAsAll:=tvDomainClustering.Items.AddChild(tnClusterAsAll,kplResources[iRLcv]^.Value);
        Storage.MatrixNodes.Node.DB.List(Storage.Main.Task,FClusters[iLcv]^.ID,iRID,FDomainNodes);
        for iNLcv:=0 to High(FDomainNodes) do begin
          iNID:=FDomainNodes[iNLcv]^.ID;
          tnNode:=tvDomainClustering.Items.AddChild(tnResourceAsAll,FDomainNodes[iNLcv]^.Alias);
          tnNode.Data:=Pointer(iNID);
          if Storage.MatrixNodes.Node.DB.Allocated(Storage.Main.Task,FClusters[iLcv]^.ID,iRID,iNID) then begin
            tnNode:=tvDomainClustering.Items.AddChild(tnResourceAsAllocated,Format(Storage.MatrixNodes.Node.FMT_MN_DISPLAY_CAPTION,[FDomainNodes[iNLcv]^.Alias,Core.Utils.Sockets.InAddrToStr(FDomainNodes[iNLcv]^.IP)]));
            tnNode.Data:=Pointer(iNID);
          end else begin
            tnNode:=tvDomainClustering.Items.AddChild(tnResourceAsAvailable,Format(Storage.MatrixNodes.Node.FMT_MN_DISPLAY_CAPTION,[FDomainNodes[iNLcv]^.Alias,Core.Utils.Sockets.InAddrToStr(FDomainNodes[iNLcv]^.IP)]));
            tnNode.Data:=Pointer(iNID);
          end;
        end;
      end;
    finally
      Core.Arrays.KeyString.Empty(kplResources);
    end;
  end;
Finally
  tvDomainClustering.EndUpdate;
end;

GUI.Lock;
Try

  {
  Empty(FDomainServices);
  FDomainServices.Cluster.ID:=Clusters[cbDomain_Clusters.ItemIndex].ID;
  FDomainServices.Node.ID:=FDomainNodes[cbDomain_Nodes.ItemIndex].ID;
  FDomainServices.Node.ClusterID:=FDomainServices.Cluster.ID;
  MatrixServices_Fill(uStorage.Module,FDomainServices,FDomainP^.ID,FDomainServices.Node.ID);
  lvDomainNodeServices.BeginUpdate;
  Try
    lvDomainNodeServices.Clear;
    for iLcv:=0 to high(FDomainServices.List) do begin
      li:=lvDomainNodeServices.Items.Add;
      li.Checked:=FDomainServices.List[iLcv].Enabled;
      li.Caption:=Yes_No[FDomainServices.List[iLcv].Enabled];
      li.SubItems.Add(IntToStr(FDomainServices.List[iLcv].Scale));
      li.SubItems.Add(mkLongNames[FDomainServices.List[iLcv].Kind]);
      li.SubItems.Add(mkDescriptions[FDomainServices.List[iLcv].Kind]);
      li.Data:=Pointer(FDomainServices.List[iLcv].ID);
    end;
  Finally
    lvDomainNodeServices.EndUpdate;
  end;
  }
Finally
  GUI.UnLock;
end;

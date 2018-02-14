FDomainServiceP:=lvDomainServices.Selected.Data;
tbDomainScale.Position:=FDomainServiceP^.Scale;
cbDomain_Service.Checked:=FDomainServiceP^.Enabled;
if gbDomainServiceSettings.Visible=false then
  gbDomainServiceSettings.Visible:=true;

{
  Empty(FDomainMatrixService);
  FDomainMatrixService.ClusterID:=MS_DEFAULT_CLUSTER;
  FDomainMatrixService.NodeID:=MS_DEFAULT_NODE;
  FDomainMatrixService.ResourceID:=MS_DEFAULT_RESOURCE;

  FDomainMatrixService.DomainID:=FDomainP^.ID;
  FDomainMatrixService.Kind:=MatrixService_FromLongName(lvDomainServices.Selected.SubItems[1]);
  if FDomainMatrixService.Kind<>-1 then begin
    GUI.Lock;
    Try
      MatrixServices_Fill(uStorage.Module,uStorage.Task,FDomainMatrixService,msfoUseCRNDK);
      if FDomainMatrixService.ID<>0 then begin
        tbDomainScale.Position:=FDomainMatrixService.Scale;
        cbDomain_Service.Checked:=FDomainMatrixService.Enabled;
      end;
      if gbDomainServiceSettings.Visible=false then
        gbDomainServiceSettings.Visible:=true;
    Finally
      GUI.UnLock;
    end;
  end;
}

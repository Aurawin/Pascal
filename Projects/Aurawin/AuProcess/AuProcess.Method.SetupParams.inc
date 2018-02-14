procedure   AuraProcess.SetupParams;
var
  iCount:integer;
  iLcv:integer;
begin
  SetLength(FParams,ParamCount);
  for iLcv:=1 to ParamCount do
    FParams[iLcv-1]:=ParamStr(iLcv);
  Core.Arrays.KeyString.fromStringArray(FParamKeys,FParams,'=');

  iCount:=System.Length(FParamKeys);

  FCluster.ID:=Core.Arrays.KeyString.GetItemAsQword(FParamKeys,'cluster',iCount,0);
  FResource.ID:=Core.Arrays.KeyString.GetItemAsQword(FParamKeys,'resource',iCount,0);
  FNode.ID:=Core.Arrays.KeyString.GetItemAsQword(FParamKeys,'node',iCount,0);
  FService.ID:=Core.Arrays.KeyString.GetItemAsQword(FParamKeys,'service',iCount,0);
  FService.Kind:=Core.Arrays.KeyString.GetItemAsQword(FParamKeys,'kind',iCount,0);
  FService.ClusterID:=FCluster.ID;
  FService.ResourceID:=FResource.ID;
  FService.NodeID:=FNode.ID;
end;


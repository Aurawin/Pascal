lbDCNINValue.Caption:='';
lbDCNIIPValue.Caption:='';
Core.Utils.TreeView.Clear(tvDomainClustering);
lbDCNIStatusValue.Caption:=Format(Storage.MatrixNodes.Node.FMT_MN_BOUND_STATUS[false],['Unknown']);

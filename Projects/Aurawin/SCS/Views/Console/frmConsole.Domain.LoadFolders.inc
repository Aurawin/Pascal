lbStatus.Caption:='Loading Domain Information...';
Try
  FCoreObjects.Reset(FDomainP,@Root,@Default,@FKeywords,FManagerFat,nil);
  FManagerFat.Load(FDomainP^.ID,Storage.MatrixServices.Default.Cluster,Storage.MatrixServices.Default.Resource,Storage.MatrixServices.Default.Node,dsfatloBoth);
  FCoreObjects.VerifyFAT;
  FCoreObjects.Started;
finally
  lbStatus.Caption:='';
end;



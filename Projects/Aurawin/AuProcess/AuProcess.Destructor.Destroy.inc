destructor AuraProcess.Destroy;
var
  iProviderLcv,iManLcv:Integer;
begin
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.Destroy',Concat('Module Cluster ',FCluster.Group,' Node ',FNode.Alias,' ID(',IntToStr(FNode.ID),') IP (',Core.Utils.Sockets.InAddrToStr(FNode.IP),')'));
  {$endif}
  {$ifndef RSR_IMAP_ONLY}
    For iProviderLcv:=0 to High(Storage.SrchProviders.List) do begin
      for iManLcv:=0 to High(Storage.SrchProviders.List[iProviderLcv].Managers) do
        TProviderManager(Storage.SrchProviders.List[iProviderLcv].Managers[iManLcv]).Free;
    end;
    Storage.SrchProviders.Items.Done(Storage.SrchProviders.List);
  {$endif}
  Storage.MatrixServices.Items.Done(FServiceDefaults);
end;

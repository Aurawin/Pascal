procedure AuraProcess.OnServerEngineFailure(Server:TRSRServer);
begin
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.Failure: Module Cluster ',FCluster.Group,' Node ',FNode.Alias,' ID(',IntToStr(FNode.ID),') IP (',Core.Utils.Sockets.InAddrToStr(FNode.IP),')'));
end;

function AuraProcess.StartHTTP:TRSRServer;
begin
  Try
    Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_HTTP,Concat('AuraProcess.StartHTTP Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',Core.Utils.Sockets.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    LoadCoreObjects();
    LoadSearchProviders();
    LoadContentTypes();
    FService.Service:=THTTPServer.Create(FDomain.Name,FNode.ID,FNode.IP,FService.Port,FService.Scale,SSL_OFF);
    TRSRServer(FService.Service).OnListening:=@OnServerListening;
    TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    TThread(FService.Service).Start();
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_HTTP,Concat('AuraProcess.StartHTTP Exception: ',E.Message));
  end;
  Result:=TRSRServer(FService.Service);
end;


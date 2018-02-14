function AuraProcess.StartXMPPSToS:TRSRServer;
begin
  Try
    Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_XMPP,Concat('AuraProcess.StartXMPPSToS Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',Core.Utils.Sockets.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    FService.Service:=TXMPPServerToServerServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale);
    TRSRServer(FService.Service).OnListening:=@OnServerListening;
    TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    TThread(FService.Service).Start();
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_XMPP,Concat('AuraProcess.StartXMPPSToS Exception: ',E.Message));
  end;
  Result:=TRSRServer(FService.Service);
end;


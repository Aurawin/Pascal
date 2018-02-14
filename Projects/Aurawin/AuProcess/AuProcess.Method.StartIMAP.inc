function AuraProcess.StartIMAP:TRSRServer;
begin
  Try
    Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_IMAP,Concat('AuraProcess.StartIMAP Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',Core.Utils.Sockets.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    FService.Service:=uIMAPd.TIMAPServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,CREATE_SUSPENDED,SSL_OFF);
    TRSRServer(FService.Service).OnListening:=@OnServerListening;
    TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    TThread(FService.Service).Start();
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_IMAP,Concat('AuraProcess.StartIMAP Exception: ',E.Message));
  end;
  Result:=TRSRServer(FService.Service);
end;


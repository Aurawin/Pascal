function AuraProcess.StartPOP3:TRSRServer;
begin
  Try
    Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_POP3,Concat('AuraProcess.StartPOP3 Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',Core.Utils.Sockets.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    FService.Service:=uPOP3d.TPOPMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,SSL_OFF);
    TRSRServer(FService.Service).OnListening:=@OnServerListening;
    TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    TThread(FService.Service).Start();
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_POP3,Concat('AuraProcess.StartPOP3 Exception: ',E.Message));
  end;
  Result:=TRSRServer(FService.Service);
end;


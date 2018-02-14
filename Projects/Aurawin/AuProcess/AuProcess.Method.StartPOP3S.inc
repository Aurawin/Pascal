function AuraProcess.StartPOP3S:TRSRServer;
begin
  Try
    Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_POP3_SSL,Concat('AuraProcess.StartPOP3S Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',Core.Utils.Sockets.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
    FService.Service:=uPOP3d.TPOPMailServer.Create(FDomain.Name,FNode.IP,FService.Port,FService.Scale,SSL_ON);
    TRSRServer(FService.Service).OnListening:=@OnServerListening;
    TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    TThread(FService.Service).Start();
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_POP3_SSL,Concat('AuraProcess.StartPOP3S Exception: ',E.Message));
  end;
  Result:=TRSRServer(FService.Service);
end;


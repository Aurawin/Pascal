function AuraProcess.StartSMTP:TRSRServer;
begin
  Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_SMTP,Concat('AuraProcess.StartSMTP Adding (',IntToStr(FService.ID),'): ',FDomain.Name,' IP=',Core.Utils.Sockets.InAddrToStr(FNode.IP),' Scale=',IntToStr(FService.Scale)));
  Try
    FService.Service:=uSMTPd.TReceiveMailServer.Create(FDomain.Name,FService.Kind,FNode.ID,FNode.IP,FService.Port,FService.Scale,SSL_OFF);
    TRSRServer(FService.Service).OnListening:=@OnServerListening;
    TRSRServer(FService.Service).OnEngineFailure:=@OnServerEngineFailure;
    TThread(FService.Service).Start();
    FQueue.OnQueItem:=@TReceiveMailServer(FService.Service).OnQueueItemReceived;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FDomain.Name,Service_SMTP,Concat('AuraProcess.StartSMTP Exception: ',E.Message));
  end;
  Result:=TRSRServer(FService.Service);
end;


procedure AuraProcess.OnDBMSDisconnected(Task:Core.Database.Types.TTask);
begin
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.OnDBMSDisconnected: DBMS has disconnected Node ',FNode.Alias,' (',IntToStr(FNode.ID),')'));
  //Halt(EXIT_CODE_DBMS_FAIL);
end;


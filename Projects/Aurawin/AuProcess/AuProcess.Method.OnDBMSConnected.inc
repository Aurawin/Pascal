Procedure AuraProcess.OnDBMSConnected(Task:Core.Database.Types.TTask);
begin
  if FMatrixFail=false then
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.OnDBMSConnected - DBMS has connected Node ',FNode.Alias,' (',IntToStr(FNode.ID),')'));
end;

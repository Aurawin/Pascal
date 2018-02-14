procedure AuraProcess.OnDBMSException(sProcedure,sLocation,sTable,sTask,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(FDomain.Name,Storage.MatrixServices.Items.mkList[FService.Kind],Concat(sProcedure,'.',sLocation,'.',sTable,'.',sTask,':',sError));
end;


procedure   TIMAPManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


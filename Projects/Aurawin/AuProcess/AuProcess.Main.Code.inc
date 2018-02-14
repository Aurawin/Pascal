// required parameters
// scale=# service=kind resource=id cluster=id node=id domain=id
App.Build.Title:='AuProcess';
App.Build.Caption:='Aurawin Social Computing Server';
DefaultSystemCodePage:=CP_UTF8;
{$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess','Process Entering'); {$endif}
System.ExceptProc:=@ProcessException;
AuraProcess.Run();
{$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess','Process Exiting'); {$endif}


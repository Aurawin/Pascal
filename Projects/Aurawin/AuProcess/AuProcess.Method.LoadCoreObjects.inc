procedure AuraProcess.LoadCoreObjects;
begin
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.LoadCoreObjects','Core Objects loading.');
  {$endif}
  {$i coList.Injections.inc}
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.LoadCoreObjects',Concat(IntToStr(Length(CoreObjectItems)),' Core Objects loaded.'));
  {$endif}
end;


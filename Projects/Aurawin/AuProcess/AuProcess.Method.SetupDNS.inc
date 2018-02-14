procedure AuraProcess.SetupDNS;
begin
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupDNS','DNS loading.');
  {$endif}
  Storage.DNS.StartupDNS();
  Storage.DNS.Native.Load(Storage.Main.Task);
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupDNS',Concat(IntToStr(Length(dbmDNS.DNS.Items)),' DNS loaded.'));
  {$endif}
end;


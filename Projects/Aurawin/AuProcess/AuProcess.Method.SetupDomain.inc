procedure    AuraProcess.SetupDomain;
begin
  FDOmain.ID:=FService.DomainID;
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupDomain',Concat('Inspecting domain id=',IntToStr(FService.DomainID)));
  {$endif}
  if not Storage.Domains.Items.DB.Fill(Storage.Main.Task,FDomain) then
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.SetupDomain.Domain_Fill: Could not load domain id=',IntToStr(FService.DomainID)));
  Storage.Main.Header.DomainName:=FDomain.Name;
end;


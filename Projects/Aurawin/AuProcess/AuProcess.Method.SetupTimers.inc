
procedure  AuraProcess.SetupTimers();
begin
  {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupTimers','uDatabase.StartBackgroundTimer starting.'); {$endif}
  Core.Database.Timer.Init(Storage.Main.Header);
  Storage.FAT.StartBackgroundTimers();

  TI_Ping.Priority:=tpNormal;
  TI_Ping.Expires:=Core.Timer.dtNow;
  TI_Ping.Event:=@OnTimer_Ping;
  TI_Ping.Location:='AuProcess.Method.SetupTimers.inc';

  Core.Database.Timer.Background.RegisterEvent(TI_Ping,LoadNoUpdate);


  {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupTimers','uDatabase.StartBackgroundTimer done.'); {$endif}
  {$ifndef RSR_IMAP_ONLY}
    if FService.Kind in [Storage.MatrixServices.Items.mkHTTP,Storage.MatrixServices.Items.mkHTTPS] then begin
      {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupTimers','uHTTPd.StartBackgroundTimers starting.'); {$endif}
      uHTTPd.StartBackgroundTimers();
      {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupTimers','uHTTPd.StartBackgroundTimers done.'); {$endif}
    end;
    if FService.Kind in [Storage.MatrixServices.Items.mkRealTimeStreaming] then begin
      {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupTimers','uRTSPd.StartBackgroundTimers starting.'); {$endif}
      uRTSPd.StartBackgroundTimers();
      {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,'AuraProcess.SetupTimers','uRTSPd.StartBackgroundTimers done.'); {$endif}
    end;
  {$endif}
end;

procedure AuraProcess.OnTimer_Ping(ItemP:Core.Timer.PItem);
begin
  Storage.MatrixServices.Items.DB.Ping(Core.Database.Timer.Background.Task,FService);
  ItemP^.Expires:=DateUtils.IncSecond(Core.Timer.dtNow,10);
end;

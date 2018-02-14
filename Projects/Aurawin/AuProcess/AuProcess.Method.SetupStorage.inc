function AuraProcess.SetupStorage:boolean;
var
  bMask:byte;
begin
  App.IniFile.Init(App.Files.IniName());
  Core.Logging.Start(App.Files.LogName());

  Storage.ReadINIFile();
  Storage.Main.Init();

  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Storage.Main.Header.Password);


  Result:=false; bMask:=0;
  if (Length(Storage.Main.Header.Username)=0) then begin
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.SetupStorage: Entry for Username cannot be empty.  Please check INI File.'));
    bMask:=1 shl 0;
  end;
  if (Length(Storage.Main.Header.Schema)=0) then begin
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.SetupStorage: Entry for Schema cannot be empty.  Please check INI File.'));
    bMask:=1 shl 1;
  end;
  if (Length(Storage.Main.Header.HostName)=0) then begin
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_PROCESS,Concat('AuraProcess.SetupStorage: Entry for Hostname cannot be empty.  Please check INI File.'));
    bMask:=1 shl 2;
  end;
  if bMask=0 then begin
    Result:=true;
    Storage.Main.Task.OnConnected:=@OnDBMSConnected;
    Storage.Main.Task.OnDisconnected:=@OnDBMSDisconnected;
  end;
end;


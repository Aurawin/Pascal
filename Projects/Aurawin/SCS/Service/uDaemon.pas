Unit uDaemon;

Interface

Uses
  Classes,
  LResources,
  DaemonApp,
  Process,

  App,
  App.Build,
  App.Consts,
  App.IniFile,

  RSR,
  RSR.Core,


  Core.Database,
  Core.Database.Types,
  Core.Database.Timer,


  Core.Timer,
  Core.Strings,
  Core.Logging,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Core.Utils.Sockets,
  Core.Utils.Unix.Account,

  Storage,
  Storage.Main,
  Storage.MatrixServices,
  Storage.MatrixClusters,
  Storage.MatrixResources,
  Storage.MatrixNodes,
  Storage.Domains,
  Storage.ContentTypes,
  Storage.SrchProviders,
  Storage.Security,
  Storage.CoreObjects,
  Storage.KeepAlive,
  Storage.ConfigData,
  Storage.AuraDisks,


  FileUtil,
  SysUtils;

Type

  { TAurawinSCS }


  TAurawinSCS = Class(TDaemon)
    procedure DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleControlCode(Sender: TCustomDaemon; ACode: DWord; var Handled: Boolean);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleShutDown(Sender: TCustomDaemon);
    procedure DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
    procedure DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
  private
    FServiceDefaults             : Storage.MatrixServices.Items.Defaults; // default services (not domain specific)
    FNodalDefaults               : Storage.MatrixServices.Items.Defaults; // default entries based on node (limits on scale and disable kinds of service for this node)

    FNodeServiceLimits           : Storage.MatrixServices.Items.Items;  // Nodal Services that are enabled/disabled based on (C,R,N)

    FServices                    : Storage.MatrixServices.Items.Manifest;        // Explicit Services that are Runable for this CRNDK

    FNodes                       : Storage.MatrixNodes.Node.Items;           // Primary and Aux Nodes for NodeStat and auDisk mounts

    FDisks                       : Storage.MatrixNodes.Node.Items;

    FProcessTimer                : Core.Timer.Item;
    FNodeStat                    : Core.Timer.Item;
    FNodeConfig                  : Core.Timer.Item;

    FStarted                     : Boolean;
    FPaused                      : Boolean;
    FStopped                     : Boolean;
  private
    FError                       : Core.Strings.VarString;

    FWorking                     : Storage.ConfigData.Items.Item;

    FProcessGroupName            : Storage.ConfigData.Items.Item;
    FProcessGroupID              : Storage.ConfigData.Items.Item;
    FProcessUserName             : Storage.ConfigData.Items.Item;
    FProcessUserID               : Storage.ConfigData.Items.Item;

    FRaidGroupName               : Storage.ConfigData.Items.Item;
    FRaidGroupID                 : Storage.ConfigData.Items.Item;

    FRaidUserName                : Storage.ConfigData.Items.Item;
    FRaidUserID                  : Storage.ConfigData.Items.Item;
  Private
    { private declarations }
    procedure  OnProcessTimer(ItemP:Core.Timer.PItem);
    procedure  OnDBMSException(sProcedure,sLocation,sTable,sTask,sError:Core.Strings.VarString);
    procedure  OnDBMSConnected(Task:Core.Database.Types.TTask);
    procedure  OnDBMSDisconnected(Task:Core.Database.Types.TTask);
  private
    procedure  OnNodeStat(ItemP:Core.Timer.PItem);
    procedure  OnNodeConfig(ItemP:Core.Timer.PItem);
  private
    procedure  EnforceProcessGroupName(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceProcessGroupID(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceProcessUserName(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceProcessUserID(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceRaidGroupName(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceRaidGroupID(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceRaidUserName(cfgP:Storage.ConfigData.Items.PItem);
    procedure  EnforceRaidUserID(cfgP:Storage.ConfigData.Items.PItem);
  private
    procedure  InitializeNodeConfig();
    procedure  SetupProcess();
    procedure  InitializeTimers();
  private
    function   CreateService(Var MS:Storage.MatrixServices.Items.Item):boolean;
    procedure  StopService(Var MS:Storage.MatrixServices.Items.Item);
    procedure  StartService(Var MS:Storage.MatrixServices.Items.Item);
    procedure  ContinueService(Var MS:Storage.MatrixServices.Items.Item);
    procedure  PauseService(Var MS:Storage.MatrixServices.Items.Item);
    procedure  CheckService(Var MS:Storage.MatrixServices.Items.Item);
    procedure  ReCreateService(Var MS:Storage.MatrixServices.Items.Item);
  private

  Public
    { public declarations }

  End; 

Var
  AurawinSCS: TAurawinSCS; 

Implementation
uses
  {$if defined(Unix)}
    BaseUnix,
  {$endif}


  {$i coList.Uses.inc}

 ,DateUtils;

Procedure RegisterDaemon; 
Begin
  RegisterDaemonClass(TAurawinSCS);
End;

Procedure TAurawinSCS.OnDBMSConnected(Task:Core.Database.Types.TTask);
begin
  FNodeStat.Expires:=DateUtils.IncSecond(Core.Timer.dtNow,App.Consts.NODE_STAT_REFRESH);
  FNodeConfig.Expires:=DateUtils.IncSecond(Core.Timer.dtNow,App.Consts.NODE_CONFIG_REFRESH);

  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnDBMSConnected: Main DBMS connected on node (',IntToStr(FServices.Node.ID),')'));
end;


procedure TAurawinSCS.OnDBMSDisconnected(Task:Core.Database.Types.TTask);
begin
  FNodeStat.Expires:=0;
  FNodeConfig.Expires:=0;
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnDBMSDisconnected: Main DBMS Failure on node (',IntToStr(FServices.Node.ID),')'));
end;

Function  TAurawinSCS.CreateService(Var MS:Storage.MatrixServices.Items.Item):Boolean;
var
  iLcv:integer;
begin
  Result:=False;
  if (MS.Scale=0) then
    MS.Scale:=FServiceDefaults[MS.Kind].Scale;
  MS.servicePath:=Concat(App.Build.Path,App.Consts.SCS_PROCESS);
  MS.State:=Storage.MatrixServices.Items.State.Created;

  Storage.MatrixServices.Items.getParams(MS,MS.serviceParams);

  MS.Service:=TProcess.Create(nil);
  TProcess(MS.Service).Options:=[poNoConsole,poNewProcessGroup];
  TProcess(MS.Service).Parameters.Delimiter:=#32;
  TProcess(MS.Service).Parameters.Clear;
  TProcess(MS.Service).CommandLine:='';
  TProcess(MS.Service).Executable:=MS.servicePath;
  for iLcv:=0 to High(MS.serviceParams) do
    TProcess(MS.Service).Parameters.Add(MS.serviceParams[iLcv]);
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.CreateService: Create ',TProcess(MS.Service).Executable,' ',TProcess(MS.Service).Parameters.DelimitedText));

  Storage.MatrixServices.Items.DB.setState(Storage.Main.Task,MS);
end;

procedure TAurawinSCS.ReCreateService(Var MS:Storage.MatrixServices.Items.Item);
var
  iLcv:integer;
begin
  MS.Service:=TProcess.Create(nil);
  TProcess(MS.Service).Options:=[poNoConsole,poNewProcessGroup];
  TProcess(MS.Service).Parameters.Delimiter:=' ';
  TProcess(MS.Service).Parameters.Clear;
  TProcess(MS.Service).CommandLine:='';
  TProcess(MS.Service).Executable:=MS.servicePath;
  for iLcv:=0 to High(MS.serviceParams) do
    TProcess(MS.Service).Parameters.Add(MS.serviceParams[iLcv]);

  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.ReCreateService: Restart ',TProcess(MS.Service).Executable,' ',TProcess(MS.Service).Parameters.DelimitedText));
  TProcess(MS.Service).Execute();
  MS.State:=Storage.MatrixServices.Items.State.Created;
  Storage.MatrixServices.Items.DB.setState(Storage.Main.Task,MS);
end;

procedure TAurawinSCS.StopService(Var MS:Storage.MatrixServices.Items.Item);
begin
  {$ifdef RSR_DEBUG}
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.StopService: Stopping (',IntToStr(MS.ID),') Kind=',IntToStr(MS.Kind)));
  {$endif}
  if MS.Service<>nil then begin
    if (TProcess(MS.Service).Running=true) then begin
      {$if defined(Unix)}
        fpKill(TProcess(MS.Service).ProcessHandle,SIGKILL);
        TProcess(MS.Service).Terminate(0);
      {$else}
        TProcess(MS.Service).Terminate(0);
      {$endif}
    end;
    MS.Enabled:=False;
    TProcess(MS.Service).Free;
    MS.Service:=nil;
    MS.State:=Storage.MatrixServices.Items.State.Terminated;
    Storage.MatrixServices.Items.DB.setState(Storage.Main.Task,MS);
  end;
end;


procedure TAurawinSCS.EnforceProcessGroupID(cfgP:Storage.ConfigData.Items.PItem);
  procedure PushCheck;
  var
    iID:Int64;
    iID2:Int64;
  begin
    iID:=Core.Utils.Unix.Account.GroupID(FProcessGroupName.Value,FError);
    iID2:=StrToInt64Def(FProcessGroupID.Value,0);
    if (iID=-1) then begin
      iID:=Core.Utils.Unix.Account.GroupAdd(FProcessGroupName.Value,FProcessGroupID.Value,FError);
      if iID <>-1 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessGroupID: Created system group ',FProcessGroupName.Value,' (',IntToStr(iID),')' ))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessGroupID: Error creating system group ',FProcessGroupName.Value,' (',FProcessGroupID.Value,') with :',FError));
    end else if ( iID<>iID2) then begin
      iID:=Core.Utils.Unix.Account.GroupMod(FProcessGroupName.Value,FProcessGroupID.Value,FError);
      if iID<>iID2 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessGroupID: Modified system group ',FProcessGroupName.Value,' (',IntToStr(iID),')'))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessGroupID: Error changing system group ',FProcessGroupName.Value,' (',FProcessGroupID.Value,') with :',FError));
    end;
  end;
begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    PushCheck();
  end else if (cfgP^.Value<>FProcessGroupID.Value) then begin
    FProcessGroupID.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure TAurawinSCS.EnforceProcessGroupName(cfgP:Storage.ConfigData.Items.PItem);

  procedure PushCheck;
  var
    iID:QWord;
  begin
    if Core.Utils.Unix.Account.GroupID(FProcessGroupName.Value,FError)=0 then begin
      iID:=Core.Utils.Unix.Account.GroupAdd(FProcessGroupName.Value,FProcessGroupID.Value,FError);
      if iID <>0 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessGroupName: Created system group ',FProcessGroupName.Value,' (',IntToStr(iID),')'))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessGroupName: Error creating system group ',FProcessGroupName.Value,' (',FProcessGroupID.Value,') with :',FError));
    end;
  end;

begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    // Boot
    PushCheck();
  end else if (cfgP^.Value<>FProcessGroupName.Value) then begin
    // Group value has changed
    FProcessGroupName.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure TAurawinSCS.EnforceProcessUserID(cfgP:Storage.ConfigData.Items.PItem);
  procedure PushCheck;
  var
    iID:Int64;
    iID2:Int64;
  begin
    iID:=Core.Utils.Unix.Account.UserID(FProcessUserName.Value,FError);
    iID2:=StrToInt64Def(FProcessUserID.Value,0);
    if (iID=-1) then begin
      iID:=Core.Utils.Unix.Account.UserAdd(FProcessUserName.Value,FProcessGroupName.Value,'/dev/null',FProcessUserID.Value,FError);
      if iID <>-1 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessUserID: Created system user ',FProcessUserName.Value,' (',IntToStr(iID),') @',FProcessGroupName.Value,' with :',FError))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessUserID: Error creating system user ',FProcessUserName.Value,' (',FProcessGroupName.Value,') with :',FError));
    end else if ( iID<>iID2) then begin
      iID:=Core.Utils.Unix.Account.UserMod(FProcessUserName.Value,FProcessUserID.Value,FError);
      if iID<>iID2 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessUserID: Modified system user ',FProcessUserName.Value,' (',IntToStr(iID),')'))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessUserID: Error changing system user ',FProcessUserName.Value,' (',FProcessUserID.Value,') with :',FError));
    end;
  end;
begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    PushCheck();
  end else if (cfgP^.Value<>FProcessUserID.Value) then begin
    FProcessUserID.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure TAurawinSCS.EnforceProcessUserName(cfgP:Storage.ConfigData.Items.PItem);

  procedure PushCheck;
  var
    iID:QWord;
  begin
    if Core.Utils.Unix.Account.UserID(FProcessUserName.Value,FError)=0 then begin
      iID:=Core.Utils.Unix.Account.UserAdd(FProcessUserName.Value,FProcessGroupName.Value,'/dev/null',FProcessUserID.Value,FError);
      if iID <>0 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessUserName: Created system user ',FProcessUserName.Value,' (',IntToStr(iID),') @',FProcessGroupName.Value))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceProcessUserName: Error creating system user ',FProcessUserName.Value,' (',FProcessUserID.Value,') with :',FError));
    end;
  end;

begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    // Boot
    PushCheck();
  end else if (cfgP^.Value<>FProcessUserName.Value) then begin
    // Group value has changed
    FProcessUserName.Value:=cfgP^.Value;
    PushCheck();
  end;
end;


procedure TAurawinSCS.EnforceRaidUserID(cfgP:Storage.ConfigData.Items.PItem);
  procedure PushCheck;
  var
    iID:Int64;
    iID2:Int64;
  begin
    iID:=Core.Utils.Unix.Account.UserID(FRaidUserName.Value,FError);
    iID2:=StrToInt64Def(FRaidUserID.Value,0);
    if (iID=-1) then begin
      iID:=Core.Utils.Unix.Account.UserAdd(FRaidUserName.Value,FRaidGroupName.Value,'/dev/null',FRaidUserID.Value,FError);
      if iID <>-1 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidUserID: Created system user ',FRaidUserName.Value,' (',IntToStr(iID),') @',FRaidGroupName.Value,' with :',FError))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidUserID: Error creating system group ',FRaidGroupName.Value,' (',FRaidGroupID.Value,') with :',FError));
    end else if ( iID<>iID2) then begin
      iID:=Core.Utils.Unix.Account.UserMod(FRaidUserName.Value,FRaidUserID.Value,FError);
      if iID<>iID2 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidUserID: Modified system user ',FRaidUserName.Value,' (',IntToStr(iID),')'))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidUserID: Error changing system user ',FRaidUserName.Value,' (',FRaidUserID.Value,') with :',FError));
    end;
  end;
begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    PushCheck();
  end else if (cfgP^.Value<>FRaidUserID.Value) then begin
    FRaidUserID.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure TAurawinSCS.EnforceRaidGroupID(cfgP:Storage.ConfigData.Items.PItem);
  procedure PushCheck;
  var
    iID:Int64;
    iID2:Int64;
  begin
    iID:=Core.Utils.Unix.Account.GroupID(FRaidGroupName.Value,FError);
    iID2:=StrToInt64Def(FRaidGroupID.Value,0);
    if (iID=-1) then begin
      iID:=Core.Utils.Unix.Account.GroupAdd(FRaidGroupName.Value,FRaidGroupID.Value,FError);
      if iID <>-1 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupID: Created system group ',FRaidGroupName.Value,' (',IntToStr(iID),')' ))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupID: Error creating system group ',FRaidGroupName.Value,' (',FRaidGroupID.Value,') with :',FError));
    end else if ( iID<>iID2) then begin
      iID:=Core.Utils.Unix.Account.GroupMod(FProcessGroupName.Value,FProcessGroupID.Value,FError);
      if iID<>iID2 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupID: Modified system group ',FRaidGroupName.Value,' (',IntToStr(iID),')'))
      else
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupID: Error changing system group ',FRaidGroupName.Value,' (',FRaidGroupID.Value,') with :',FError));
    end;
  end;
begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    PushCheck();
  end else if (cfgP^.Value<>FRaidGroupID.Value) then begin
    FRaidGroupID.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure TAurawinSCS.EnforceRaidUserName(cfgP:Storage.ConfigData.Items.PItem);

  procedure PushCheck;
  var
    iID:QWord;
  begin
    Try
      if Core.Utils.Unix.Account.UserID(FRaidUserName.Value,FError)=0 then begin
        iID:=Core.Utils.Unix.Account.UserAdd(FRaidUserName.Value,FRaidGroupName.Value,'/dev/null',FRaidUserID.Value,FError);
        if iID <>0 then
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidUserName: Created system user ',FRaidUserName.Value,' (',IntToStr(iID),') @',FRaidGroupName.Value))
        else
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidUserName: Error creating system user ',FRaidUserName.Value,' (',FRaidUserID.Value,') with :',FError));
      end;
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupName.PushCheck: Error uNixUsersGroups.GroupID ',FRaidGroupName.Value,' (',FRaidGroupID.Value,') exception :',E.Message));
    end;
  end;

begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    // Boot
    PushCheck();
  end else if (cfgP^.Value<>FRaidUserName.Value) then begin
    // Group value has changed
    FRaidUserName.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure TAurawinSCS.EnforceRaidGroupName(cfgP:Storage.ConfigData.Items.PItem);

  procedure PushCheck;
  var
    iID:QWord;
  begin
    Try
      if Core.Utils.Unix.Account.GroupID(FRaidGroupName.Value,FError)=0 then begin
        iID:=Core.Utils.Unix.Account.GroupAdd(FRaidGroupName.Value,FRaidGroupID.Value,FError);
        if iID <>0 then
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupName: Created system group ',FRaidGroupName.Value,' (',IntToStr(iID),')'))
        else
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupName: Error creating system group ',FRaidGroupName.Value,' (',FRaidGroupID.Value,') with :',FError));
      end;
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.EnforceRaidGroupName.PushCheck: Error uNixUsersGroups.GroupID ',FRaidGroupName.Value,' (',FRaidGroupID.Value,') exception :',E.Message));
    end;
  end;

begin
  SetLength(FError,0);
  if (cfgP=nil) then begin
    // Boot
    PushCheck();
  end else if (cfgP^.Value<>FRaidGroupName.Value) then begin
    // Group value has changed
    FRaidGroupName.Value:=cfgP^.Value;
    PushCheck();
  end;
end;

procedure   TAurawinSCS.OnNodeConfig(ItemP:Core.Timer.PItem);
var
  iIO:Integer;
begin
  Try
    FWorking.ID:=FProcessGroupName.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FProcessGroupName.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Process Group Name exception: ',e.Message));
  end;

  Try
    FWorking.ID:=FProcessUserName.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FProcessUserName.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Process User Name exception: ',e.Message));
  end;

  Try
    FWorking.ID:=FProcessGroupID.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FProcessGroupID.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Process Group ID exception: ',e.Message));
  end;
  Try
    FWorking.ID:=FProcessUserID.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FProcessUserID.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Process User ID exception: ',e.Message));
  end;
  Try
    FWorking.ID:=FRaidGroupName.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FRaidGroupName.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Raid Group Name exception: ',e.Message));
  end;
  Try
    FWorking.ID:=FRaidUserName.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FRaidUserName.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Raid User Name exception: ',e.Message));
  end;
  Try
    FWorking.ID:=FRaidGroupID.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FRaidGroupID.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Raid Group ID exception: ',e.Message));
  end;
  Try
    FWorking.ID:=FRaidUserID.ID;
    Storage.ConfigData.Items.DB.Read(Core.Database.Timer.Background.Task,FWorking);
    FRaidUserID.OnEnforce(@FWorking);
  Except
    on e:exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeConfig: CFG_Data_Get Raid User ID exception: ',e.Message));
  end;

  Storage.MatrixNodes.Node.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);
  Storage.MatrixNodes.Node.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);
  Storage.AuraDisks.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);
  Storage.AuraDisks.Process.UserID:=StrToInt64Def(FProcessUserID.Value,0);

  ItemP^.Expires:=DateUtils.IncSecond(Core.Timer.dtNow,App.Consts.NODE_CONFIG_REFRESH);
end;

procedure   TAurawinSCS.OnNodeStat(ItemP:Core.Timer.PItem);
var
  iLcv:integer;
begin
  Try
    Storage.MatrixNodes.Node.DB.SetupDisks(Core.Database.Timer.Background.Task,FDisks,FError);
    if Length(FError)>0 then begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: SetupDisks mount errors were detected: ',FError));
    end;
  Except
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: ',FServices.Cluster.Group,' Node ',FNodes[iLcv]^.Alias,' (',IntToStr(FNodes[iLcv]^.ID),') SetupDisks Failed'));
  end;
for iLcv:=0 to High(FNodes) do begin
  try
    Node.StatRAM(FNodes[iLcv]^);
  Except
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: ','Cluster ',FServices.Cluster.Group,' Node ',FNodes[iLcv]^.Alias,' (',IntToStr(FNodes[iLcv]^.ID),') Memory Stat Failed'));
  end;
  Try
    Storage.MatrixNodes.Node.DB.Write(Core.Database.Timer.Background.Task,FNodes[iLcv]^.ID,FNodes[iLcv]^.Stats);
  Except
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: Cluster ',FServices.Cluster.Group,' Node ',FNodes[iLcv]^.Alias,' (',IntToStr(FNodes[iLcv]^.ID),') Memory Stat Write Failed'));
  end;
  If FNodes[iLcv]^.Disk.Enabled and (System.Length(FNodes[iLcv]^.Disk.Device)>0) then begin
    Try
      Node.StatDisk(FNodes[iLcv]^,FError);
      if Length(FError)>0 then
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: Mount error warning: ',FError));
      Except
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: Cluster ',FServices.Cluster.Group,' Node ',FNodes[iLcv]^.Alias,' (',IntToStr(FNodes[iLcv]^.ID),') Disk Stat Failed'));
      end;
      Try
        Storage.MatrixNodes.Node.DB.Write(Core.Database.Timer.Background.Task,FNodes[iLcv]^.ID,FNodes[iLcv]^.Disk);
      Except
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnNodeStat: Cluster ',FServices.Cluster.Group,' Node ',FNodes[iLcv]^.Alias,' (',IntToStr(FNodes[iLcv]^.ID),') Disk Stat Write Failed'));
      end;
    end;
  end;

  ItemP^.Expires:=DateUtils.IncSecond(Core.Timer.dtNow,App.Consts.NODE_STAT_REFRESH);
end;

procedure TAurawinSCS.StartService(Var MS:Storage.MatrixServices.Items.Item);
begin
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.StartService: Starting (',IntToStr(MS.ID),') Kind=',IntToStr(MS.Kind),' Scale=',IntToStr(MS.Scale),' Port=',IntToStr(MS.Port)));
  if (MS.Service<>nil) then begin
    TProcess(MS.Service).Execute();
    MS.Started:=True;
    {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.StartService: Started (',IntToStr(MS.ID),') Kind=',IntToStr(MS.Kind),' Scale=',IntToStr(MS.Scale),' Port=',IntToStr(MS.Port)));
    {$endif}
  end;
end;

procedure TAurawinSCS.PauseService(Var MS:Storage.MatrixServices.Items.Item);
begin
  {$ifdef RSR_DEBUG}
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.PauseService: 'Pausing (',IntToStr(MS.ID),') Kind=',IntToStr(MS.Kind)));
  {$endif}
end;

procedure TAurawinSCS.CheckService(Var MS:Storage.MatrixServices.Items.Item);
var
  msUpdate:Storage.MatrixServices.Items.Item;
begin
  Storage.MatrixServices.Items.Copy(MS,msUpdate);
  If Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,msUpdate,foUseID)=true then
    Storage.MatrixServices.Items.Copy(msUpdate,MS);
  if (MS.Service<>nil) then begin
    If (MS.Enabled=true) and (MS.DomainID>0) and (MS.Scale>0) and (MS.Kind in Storage.MatrixServices.Items.RunInProcess) then begin
      if (TProcess(MS.Service).Running=false) then begin
        {$ifdef RSR_DEBUG}
        Core.Logging.Native.WriteLogEntry(
          SERVICE_MONITOR,
          SERVICE_MONITOR,
          Concat(
            'uDaemon.CheckService: ' ,
            'Closed (',IntToStr(MS.ID),') ',
            'Kind=',IntToStr(MS.Kind), ' ',
            'Status=',IntToStr(TProcess(MS.Service).ExitStatus)
          )
        );
        {$endif}
        TProcess(MS.Service).Free();
        MS.State:=Storage.MatrixServices.Items.State.Terminated;
        Storage.MatrixServices.Items.DB.setState(Storage.Main.Task,MS);
        MS.Service:=nil;
        ReCreateService(MS);
      end;
    end else if (TProcess(MS.Service).Running=true) then begin
      // Service is being de-activated
      TProcess(MS.Service).Terminate(MS.State);
      TProcess(MS.Service).Free();
      MS.State:=Storage.MatrixServices.Items.State.Terminated;
      Storage.MatrixServices.Items.DB.setState(Storage.Main.Task,MS);
      MS.Service:=nil;
    end;
  end else If (MS.Enabled=true) and (MS.DomainID>0) and (MS.Scale>0) and (MS.Kind in Storage.MatrixServices.Items.RunInProcess) then begin
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.CheckService: Recreating (',IntToStr(MS.ID),') Kind=',IntToStr(MS.Kind),' Scale=',IntToStr(MS.Scale),' Port=',IntToStr(MS.Port)));
    ReCreateService(MS);
  end;
end;

procedure TAurawinSCS.ContinueService(Var MS:Storage.MatrixServices.Items.Item);
begin
  {$ifdef RSR_DEBUG}
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.ContinueService: Continuing (',IntToStr(MS.ID),') Kind=',IntToStr(MS.Kind)));
  {$endif}
end;

procedure TAurawinSCS.DataModuleContinue(Sender: TCustomDaemon; var OK: Boolean);
var
  iLcv: Integer;
begin
  FPaused:=false;
  OK:=True;
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleContinue: Continuing with ',IntToStr(Length(FServices.List)),' services.'));
  {$endif}
  for iLcv := 0 to High(FServices.List) do
    ContinueService(FServices.List[iLcv]^);
end;

procedure TAurawinSCS.DataModuleControlCode(Sender: TCustomDaemon; ACode: DWord; var Handled: Boolean);
begin
  Handled:=True;
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Format('uDaemon.DataModuleControlCode: Code=%s',[intToStr(ACode)]));
end;

procedure  TAurawinSCS.SetupProcess();
begin
  {$ifdef Unix}
    BaseUnix.FpGetgid;
  {$endif}
end;

procedure  TAurawinSCS.InitializeNodeConfig();
begin
  Storage.ConfigData.Items.Init(FProcessGroupName);
  FProcessGroupName.Name:=CFG_NS_OS_GROUP_NAME;
  FProcessGroupName.DomainID:=Defaults.AllDomains;
  FProcessGroupName.OnEnforce:=@EnforceProcessGroupName;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessGroupName);

  Storage.ConfigData.Items.Init(FProcessGroupID);
  FProcessGroupID.Name:=CFG_NS_OS_GROUP_ID;
  FProcessGroupID.DomainID:=Defaults.AllDomains;
  FProcessGroupID.OnEnforce:=@EnforceProcessGroupID;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessGroupID);

  Storage.MatrixNodes.Node.Process.GroupID:=StrToInt64Def(FProcessGroupID.Value,0);
  Storage.AuraDisks.Process.GroupID:=StrToInt64Def(FProcessGroupID.Value,0);

  Storage.ConfigData.Items.Init(FProcessUserName);
  FProcessUserName.Name:=CFG_NS_OS_USER_NAME;
  FProcessUserName.DomainID:=Defaults.AllDomains;
  FProcessUserName.OnEnforce:=@EnforceProcessUserName;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessUserName);

  Storage.ConfigData.Items.Init(FProcessUserID);
  FProcessUserID.Name:=CFG_NS_OS_USER_ID;
  FProcessUserID.DomainID:=Defaults.AllDomains;
  FProcessUserID.OnEnforce:=@EnforceProcessUserID;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessUserID);

  Storage.MatrixNodes.Node.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);
  Storage.AuraDisks.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);

  Storage.ConfigData.Items.Init(FRaidGroupName);
  FRaidGroupName.Name:=CFG_NS_OS_RAID_GROUP_NAME;
  FRaidGroupName.DomainID:=Defaults.AllDomains;
  FRaidGroupName.OnEnforce:=@EnforceRaidGroupName;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidGroupName);

  Storage.ConfigData.Items.Init(FRaidGroupID);
  FRaidGroupID.Name:=CFG_NS_OS_RAID_GROUP_ID;
  FRaidGroupID.DomainID:=Defaults.AllDomains;
  FRaidGroupID.OnEnforce:=@EnforceRaidGroupID;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidGroupID);

  Storage.ConfigData.Items.Init(FRaidUserName);
  FRaidUserName.Name:=CFG_NS_OS_RAID_USER_NAME;
  FRaidUserName.DomainID:=Defaults.AllDomains;
  FRaidUserName.OnEnforce:=@EnforceRaidUserName;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidUserName);

  Storage.ConfigData.Items.Init(FRaidUserID);
  FRaidUserID.Name:=CFG_NS_OS_RAID_USER_ID;
  FRaidUserID.DomainID:=Defaults.AllDomains;
  FRaidUserID.OnEnforce:=@EnforceRaidUserID;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidUserID);

  FProcessGroupName.OnEnforce(nil);
  FProcessUserName.OnEnforce(nil);

  FProcessGroupID.OnEnforce(nil);
  FProcessUserID.OnEnforce(nil);

  FRaidGroupName.OnEnforce(nil);
  FRaidUserName.OnEnforce(nil);

  FRaidGroupID.OnEnforce(nil);
  FRaidUserID.OnEnforce(nil);
end;

procedure TAurawinSCS.InitializeTimers();
begin
  FNodeStat.Expires:=0;
  FNodeStat.Event:=@OnNodeStat;
  FNodeStat.Location:='uDaemon.OnNodeStat';
  FNodeStat.Mode:=temNormal;
  Core.Database.Timer.Background.RegisterEvent(FNodeStat,LoadNoUpdate);

  FNodeConfig.Expires:=0;
  FNodeConfig.Event:=@OnNodeConfig;
  FNodeConfig.Location:='uDaemon.OnNodeConfig';
  FNodeConfig.Mode:=temNormal;
  Core.Database.Timer.Background.RegisterEvent(FNodeConfig,LoadNoUpdate);
end;

procedure TAurawinSCS.DataModuleCreate(Sender: TObject);
var
  bstarted                       : boolean;
  iLcv                           : LongInt;
  iManLcv                        : LongInt;
  iaDomains                      : Core.Arrays.Types.LargeWord;

  function PushStorageSetup:boolean;
  var
    bMask:byte;
  begin
    Result:=false; bMask:=0;
    if (Length(Storage.Main.Header.Username)=0) then begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.PushStorageSetup: Entry for Username cannot be empty.  Please check INI File.'));
      bMask:=1 shl 0;
    end;
    if (Length(Storage.Main.Header.Schema)=0) then begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.PushStorageSetup: Entry for Schema cannot be empty.  Please check INI File.'));
      bMask:=1 shl 1;
    end;
    if (Length(Storage.Main.Header.HostName)=0) then begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.PushStorageSetup: Entry for Hostname cannot be empty.  Please check INI File.'));
      bMask:=1 shl 2;
    end;
    if bMask=0 then begin
      Result:=true;
      Storage.Main.Task.OnConnected:=@OnDBMSConnected;
      Storage.Main.Task.OnDisconnected:=@OnDBMSDisconnected;
    end;
  end;

  procedure PushCreateServices;
  var
    iLcv:Integer;
  begin
    Try
      for iLcv:=0 to High(FServices.List) do
        CreateService(FServices.List[iLcv]^);
      FProcessTimer.Expires:=DateUtils.IncMillisecond(Core.Timer.dtNow,SERVICE_CHECK_DELAY);
    Except
      On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.PushCreateServices:',E.Message));
    end;
  end;

  procedure PushProcessServices;
  var
    iLcv:integer;
  begin
    if Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FServiceDefaults) then begin
      for iLcv:=0 to High(FNodes) do begin
        Storage.MatrixServices.Items.DB.Verify(Storage.Main.Task,FServices.Cluster.ID,FServices.Resource.ID,FNodes[iLcv]^.ID,iaDomains,FServiceDefaults);
        Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FServices.Cluster.ID,FServices.Resource.ID,FNodes[iLcv]^.ID,FNodeServiceLimits);
        Storage.MatrixServices.Items.DB.Runable(Storage.Main.Task,FServices.Cluster.ID,FServices.Resource.ID,FNodes[iLcv]^.ID,FServices.List);
      end;
      PushCreateServices;
    end else begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.ServiceCreate: MatrixServices_Fill(Defaults) Failed');
    end;
  end;

  procedure PushLoadCoreObjects;
  begin
    {$i coList.Injections.inc}
  end;

begin
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.DataModuleCreate: Entered creation.');
  {$endif}

  App.IniFile.Init(App.Files.IniName());
  Core.Logging.Start(App.Files.LogName());

  Storage.ReadINIFile();
  Storage.Main.Init();


  FProcessTimer.Location:='uDaemon.OnProcessTimer';
  FProcessTimer.Mode:=temSynchronize;
  FProcessTimer.Event:=@OnProcessTimer;
  FProcessTimer.Expires:=0;
  Core.Timer.Timer.RegisterEvent(FProcessTimer,LoadNoUpdate);



  Storage.MatrixServices.Items.Init(FServices);
  FServices.Node.ID:=Storage.NodeID;
  FServices.Resource.ID:=Storage.ResourceID;
  FServices.Cluster.ID:=Storage.ClusterID;


  Try
    if PushStorageSetup then begin
      Storage.Main.Task.Connection.Connected:=True;
      If (Storage.Main.Task.Connection.Connected=true) then begin
        {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.DataModuleCreate.DataModuleStart: Core.Database.Timer.Init'); {$endif}
        Core.Database.Timer.Init(Storage.Main.Header);
        {$ifdef RSR_DEBUG} Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.DataModuleCreate.DataModuleStart: Core.Database.Timer loaded.'); {$endif}
        if Storage.MatrixNodes.Node.DB.Fill(Storage.Main.Task,FServices.Node.ID,FServices.Node) then begin
          System.SetLength(FNodes,System.Length(Storage.AuxNodes)+1);
          FNodes[0]:=@FServices.Node;
          for iLcv:=1 to High(FNodes) do begin
            New(FNodes[iLcv]);
            Storage.MatrixNodes.Node.Init(FNodes[iLcv]^);
            Storage.MatrixNodes.Node.DB.Fill(Storage.Main.Task,AuxNodes[iLcv-1],FNodes[iLcv]^);
            Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.DataModuleStart: Auxiliary Node "',FNodes[iLcv]^.Alias,'" ID(',IntToStr(FNodes[iLcv]^.ID),') loaded.'));
          end;
          if Storage.MatrixClusters.Cluster.DB.Exists(Storage.Main.Task,FServices.Node.ClusterID) then begin
            Storage.MatrixClusters.Cluster.DB.Fill(Storage.Main.Task,FServices.Node.ClusterID,FServices.Cluster);
            if Storage.MatrixResources.Resource.DB.Fill(Storage.Main.Task,FServices.Resource.ID,FServices.Resource) then begin
              {$ifdef RSR_DEBUG}
                Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.DataModuleStart: Cluster "',FServices.Cluster.Group,'" ID(',IntToStr(FServices.Cluster.ID),') loaded.'));
                Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.DataModuleStart: Resource "',FServices.Resource.Name,'" ID(',IntToStr(FServices.Resource.ID),') loaded.'));
                Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate.DataModuleStart: Node "',FServices.Node.Alias,'" ID(',IntToStr(FServices.Node.ID),') loaded.'));
              {$endif}
              InitializeTimers();
              InitializeNodeConfig();

              Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.DataModuleCreate: Mounting all cloud drives...');
              Storage.MatrixNodes.Node.DB.SetupDisks(Storage.Main.Task,FDisks,FError);
              if Length(FError)>0 then begin
                Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleCreate: SetupDisks mount errors: ',FError));
                Halt(EXIT_CODE_AUDISK_FAIL);
              end;
              Try
                if Storage.Domains.Items.DB.List(Storage.Main.Task,iaDomains) then begin
                  PushProcessServices;
                end else begin
                  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.ServiceCreate: Domain_List(IDs) Failed');
                end;
              finally
                Core.Arrays.LargeWord.Done(iaDomains);
              end;
            end else
              Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.ServiceCreate: Resource ID(',IntToStr(FServices.Resource.ID),',) was not found or does not exist'));
          end else
            Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.ServiceCreate: Cluster ID(',IntToStr(FServices.Cluster.ID),',) was not found or does not exist'));
        end else
          Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.ServiceCreate: Node ID(',IntToStr(FServices.Node.ID),') was not found or does not exist'));
      end else begin
        Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.ServiceCreate: Connect Failed');
      end;

    end else begin
      Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,'uDaemon.ServiceCreate: PushStorageSetup Failed');
    end;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(SERVICE_MONITOR,Service_RSR,Concat('ServiceCreate:',E.Message));
  end;
end;

procedure TAurawinSCS.DataModuleDestroy(Sender: TObject);
var
  iProviderLcv,iManLcv:Integer;
begin
  Core.Database.Timer.Background.UnloadEvent(FNodeStat,UnloadNoExecute);
  Core.Database.Timer.Background.UnloadEvent(FNodeConfig,UnloadNoExecute);
  Core.Timer.Timer.UnloadEvent(FProcessTimer,UnloadNoExecute);
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleDestroy: Module Cluster ',FServices.Cluster.Group,' Node ',FServices.Node.Alias,' ID(',IntToStr(FServices.Node.ID),') IP (',Core.Utils.Sockets.InAddrToStr(FServices.Node.IP),')'));
  {$endif}
  Storage.MatrixServices.Items.Done(FServiceDefaults);
  Storage.MatrixServices.Items.Done(FServices);
end;

procedure TAurawinSCS.DataModulePause(Sender: TCustomDaemon; var OK: Boolean);
var
  iLcv: Integer;
begin
  FPaused:=true;
  OK:=True;
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModulePause: Pausing ',IntToStr(Length(FServices.List)),' services.'));
  {$endif}
  for iLcv := 0 to High(FServices.List) do
    PauseService(FServices.List[iLcv]^);
end;

procedure TAurawinSCS.DataModuleShutDown(Sender: TCustomDaemon);
var
  iLcv: Integer;
begin
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleShutdown: Shutting down ',IntToStr(Length(FServices.List)),' services.'));
  {$endif}
  FStopped:=true;
  for iLcv := 0 to High(FServices.List) do
    StopService(FServices.List[iLcv]^);
end;

procedure TAurawinSCS.DataModuleStart(Sender: TCustomDaemon; var OK: Boolean);
var
  iLcv: Integer;
begin
  FStarted:=true; FPaused:=false;
  OK:=False;
  Try
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleStart Module Cluster ',FServices.Cluster.Group,' Node ',FServices.Node.Alias,' ID(',IntToStr(FServices.Node.ID),') IP (',Core.Utils.Sockets.InAddrToStr(FServices.Node.IP),')'));
    Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleStart Inspecting ',IntToStr(Length(FServices.List)),' from the list of services.'));
    for iLcv := 0 to High(FServices.List) do
      if (FServices.List[iLcv]^.State=Storage.MatrixServices.Items.State.Created) then
        StartService(FServices.List[iLcv]^);
    OK:=True;
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleStart.Error ',E.Message));
  end;
end;

procedure TAurawinSCS.DataModuleStop(Sender: TCustomDaemon; var OK: Boolean);
var
  iLcv: Integer;
begin
  FPaused:=false; FStopped:=true;
  FProcessTimer.Expires:=0;
  OK:=True;
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.DataModuleStop Stopping ',IntToStr(Length(FServices.List)),' services.'));
  for iLcv := 0 to High(FServices.List) do
    StopService(FServices.List[iLcv]^);
  Halt(EXIT_CODE_OK);
end;

procedure TAurawinSCS.OnProcessTimer(ItemP:Core.Timer.PItem);
var iLcv:integer;
begin
  if (FStarted=true) and (FPaused=false) and (FStopped=false) then begin
    for iLcv := 0 to High(FServices.List) do
      CheckService(FServices.List[iLcv]^);
  end;
  ItemP^.Expires:=DateUtils.IncMilliSecond(Core.Timer.dtNow,SERVICE_CHECK_DELAY);
end;

Procedure  TAurawinSCS.OnDBMSException(sProcedure,sLocation,sTable,sTask,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(DOMAIN_RSR,SERVICE_MONITOR,Concat('uDaemon.OnDBMSException ',sProcedure,'.',sLocation,'.',sTable,'.',sTask,':',sError));
end;


Initialization
 {$i uDaemon.lrs}
  RegisterDaemon;
End.


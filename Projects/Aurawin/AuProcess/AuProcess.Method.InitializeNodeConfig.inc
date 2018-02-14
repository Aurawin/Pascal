procedure AuraProcess.InitializeNodeConfig();
begin
  Storage.ConfigData.Items.Init(FProcessGroupName);
  FProcessGroupName.Name:=CFG_NS_OS_GROUP_NAME;
  FProcessGroupName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessGroupName);

  Storage.ConfigData.Items.Init(FProcessGroupID);
  FProcessGroupID.Name:=CFG_NS_OS_GROUP_ID;
  FProcessGroupID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessGroupID);

  Storage.AuraDisks.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);
  Storage.MatrixNodes.Node.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);

  Storage.ConfigData.Items.Init(FProcessUserName);
  FProcessUserName.Name:=CFG_NS_OS_USER_NAME;
  FProcessUserName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessUserName);

  Storage.ConfigData.Items.Init(FProcessUserID);
  FProcessUserID.Name:=CFG_NS_OS_USER_ID;
  FProcessUserID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessUserID);

  Storage.AuraDisks.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);
  Storage.MatrixNodes.Node.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);

  Storage.ConfigData.Items.Init(FRaidGroupName);
  FRaidGroupName.Name:=CFG_NS_OS_RAID_GROUP_NAME;
  FRaidGroupName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidGroupName);

  Storage.ConfigData.Items.Init(FRaidGroupID);
  FRaidGroupID.Name:=CFG_NS_OS_RAID_GROUP_ID;
  FRaidGroupID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidGroupID);

  Storage.ConfigData.Items.Init(FRaidUserName);
  FRaidUserName.Name:=CFG_NS_OS_RAID_USER_NAME;
  FRaidUserName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidUserName);

  Storage.ConfigData.Items.Init(FRaidUserID);
  FRaidUserID.Name:=CFG_NS_OS_RAID_USER_ID;
  FRaidUserID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidUserID);
end;




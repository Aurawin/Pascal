
procedure   TAuSocketMan.ListAllFiles(Var SR:TRSR; var Sync:Storage.Social.Sync.TSync);
begin
  AddMethod(cmListAllSocialFiles.Create(Self,@SR,@Sync));
end;

procedure   TAuSocketMan.ListResources(Var SR:TRSR);
begin
  AddMethod(cmListResources.Create(Self,@SR));
end;

procedure   TAuSocketMan.ListNetworks(Var SR:TRSR);
begin
  AddMethod(cmListNetworks.Create(Self,@SR));
end;

procedure   TAuSocketMan.ListConnections(Var SR:TRSR);
begin
  AddMethod(cmListConnections.Create(Self,@SR));
end;

procedure   TAuSocketMan.ReadAccount(Var SR:TRSR);
begin
  AddMethod(cmReadAccount.Create(Self,@SR));
end;

procedure   TAuSocketMan.ReadSync(Var SR:TRSR; var Sync:Storage.Social.Sync.TSync);
begin
  AddMethod(cmReadSocialSync.Create(Self,@SR,Sync));
end;

procedure  TAuSocketMan.DeleteResource(Var SR:TRSR; iID:Int64);
begin
  AddMethod(cmDeleteResource.Create(Self,@SR,iID));
end;

procedure   TAuSocketMan.WriteResource(Var SR:TRSR; var Item:Storage.VDM.Resources.TResource);
begin
  AddMethod(cmWriteResource.Create(Self,@SR,Item));
end;

procedure   TAuSocketMan.ListFolders(Var SR:TRSR; var Sync:Storage.Social.Sync.TSync);
begin
  AddMethod(cmListFoldersSocial.Create(Self,@SR,@Sync));
end;

procedure  TAuSocketMan.CreateResource(Var SR:TRSR; Name,Description:Core.Strings.VarString; aRemember,aSyncUpload,aSyncDownload:boolean);
begin
  Storage.VDM.Resources.Empty(FResource);
  FResource.Name:=Name;
  FResource.Description:=Description;
  if aRemember then
    FResource.Flags:=FResource.Flags or Storage.VDM.Resources.Flags.SaveSession;
  if aSyncUpload then
    FResource.Flags:=FResource.Flags or Storage.VDM.Resources.Flags.SyncUpload;
  if aSyncDownload then
    FResource.Flags:=FResource.Flags or Storage.VDM.Resources.Flags.SyncDownload;
  AddMethod(cmCreateResource.Create(Self,@SR,FResource));
end;

procedure   TAuSocketMan.WriteSyncSocial(Var SR:TRSR; var aSync:Storage.Social.Sync.TSync);
begin
  AddMethod(cmWriteSyncsSocial.Create(Self,@SR,@aSync.Header));
end;

procedure   TAuSocketMan.CreateFolder(Var SR:TRSR; var Folder:Storage.Social.Folders.TSFolder);
begin
  AddMethod(cmCreateFolderSocial.Create(Self,@SR,@Folder));
end;



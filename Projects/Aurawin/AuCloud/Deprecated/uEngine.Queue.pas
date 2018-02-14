unit uEngine.Queue;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  Core.Strings,
  Core.Generics,

  Storage.Social;

Type
implementation


constructor Items.Create(aOwner:TAuSocketMan);
begin
  Owner:=aOwner;
  FList:=ItemsList.Create(Defaults.FreeOnClear);
  Inherited Create();
end;

destructor Items.Destroy();
begin
  FList.Clear();
  FList.Free();
  Inherited Destroy();
end;


Constructor Item.Create(
  aKind          : ItemKind;

  aPipeP         : Storage.Social.Sync.Pipes.PItem;
  aSyncP         : Storage.Social.Sync.PItem;
  aHdrP          : Storage.Social.Sync.PHeader;

  aFolderP       : Storage.Social.Folders.PSFolder;
  aMfFolderP     : Storage.Social.Sync.Manifest.PSMFSTFolder;
  aFileP         : Storage.Social.Files.PSFile;
  aMfFileP       : Storage.Social.Sync.Manifest.PSMFSTItem;

  aLocalized     : Core.Strings.VarString;
  aRemote        : Core.Strings.VarString
);
begin
  Kind:=aKind;

  PipeP:=aPipeP;
  SyncP:=aSyncP;
  HdrP:=aHdrP;

  FolderP:= aFolderP;
  mfFolderP:= aMfFolderP;
  FileP:=aFileP;
  mfFileP:=aMfFileP;

  LocalizedFile:=aLocalized;
  RemoteFile:=aRemote;

end;

procedure Items.processWrite(aItem:Item);
begin
  Man.SetSyncStatus(Format(FMT_PROGRESS_SOC_STATUS,[auLang.Table.Labels.SyncUpload,FSyncP^.Network.Title,sAuFile]));
  FData:=TFileStream.Create(sLoFile,fmOpenRead or fmShareDenyNone);
  Try
    Man.WriteFile(FRSRP^,FRequest,FileP^,FData);
  finally
    FData.Free();
  end;
end;

procedure Items.processCreate(aItem:Item);
begin

end;

procedure Items.processRead(aItem:Item);
begin

end;

function Items.Schedule(
  aKind          : ItemKind;
  aPipeP         : Storage.Social.Sync.Pipes.PItem;
  aSyncP         : Storage.Social.Sync.PItem;
  aHdrP          : Storage.Social.Sync.PHeader;

  aFolderP       : Storage.Social.Folders.PSFolder;
  aMfFolderP     : Storage.Social.Sync.Manifest.PSMFSTFolder;
  aFileP         : Storage.Social.Files.PSFile;
  aMfFileP       : Storage.Social.Sync.Manifest.PSMFSTItem;
  aLocalized     : Core.Strings.VarString;
  aRemote        : Core.Strings.VarString

) : Item;
begin
  Result :=Item.Create(aKind,aPipeP,aSyncP,aHdrP,aFolderP,aMfFolderP,aFileP,aMfFileP,aLocalized,aRemote);
  FList.Add(Result);
end;

end.


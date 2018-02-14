unit coAvatar;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  RSR,
  RSR.Core,
  RSR.HTTP,

  App.Consts,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  uHTTPd,
  hHTTPd,

  Encryption.Base64,

  Core.Strings,
  Core.Keywords,
  Core.Streams,
  Core.XML,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,
  Core.Arrays.Bytes,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.UserStorage,
  Storage.Social,
  Storage.Social.Network,
  Storage.Social.Folders,
  Storage.Social.Files,
  Storage.ContentTypes,
  Storage.Avatars,
  Storage.Roster,
  Storage.VDM,
  Storage.CoreObjects,

  Core.Utils.Files,

  SysUtils;

type
  Avatar=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'Avatar';
      NameSpace                  : '/core/avatar';
      Caption                    : 'Avatar Core Object';
      Prompt                     : 'Avatar provisioning for media types';
      Description                : 'Domain level application for avatar images';
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TAvatarCore';
      Location                   : 'coAvatar.pas';
    );
    Header:TCoreObjectInfo=(
      ID                         : 0;
      ProviderID                 : 0;
      Enabled                    : true;
      Anonymous                  : true;
      Scale                      : 0;
      CLSInfo                    : @CLSInf;
      ACLInfo                    : @ACLInf;
    );
    XMLInf:TXMLInfo=(
      Enabled                    : true;
    );
  Type
    Add=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Add';
        NameSpace                : '/a';
        Caption                  : 'Add avatar image';
        Prompt                   : 'Users and Groups can add avatar images';
        Description              : 'Adds avatar images to the Database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : false;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method             : nil;
        Resource           : nil;
      );
    end;
    Update=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Update';
        NameSpace                : '/u';
        Caption                  : 'Update avatar image';
        Prompt                   : 'Users and Groups can update avatar images';
        Description              : 'Updates avatar images in the Database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : false;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method             : nil;
        Resource           : nil;
      );
    end;
    Get=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Get';
        NameSpace                : '/g';
        Caption                  : 'Get avatar image';
        Prompt                   : 'Anonymous avatar images retrieval';
        Description              : 'Retrieves avatar images from the Database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : true;
        Cache                    : true;
        Compress                 : false;
        Secure                   : false;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method             : nil;
        Resource           : nil;
      );
    end;
  end;
  TAvatarCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    FSAList                      : Core.Arrays.Types.VarString;
    FLand                        : QWord;
    FFolderID                    : QWord;
    FFileID                      : QWord;
    FNetworkID                   : QWord;
    FFileNetworkID               : QWord;
    FLength                      : LongInt;
    FDiff                        : Int64;
    FCacheExpired                : boolean;
    FContentType                 : string;
    FUserFile                    : Storage.UserStorage.Files.TItem;
    FNetwork                     : Storage.Social.Network.TNetwork;
    FNetworkFile                 : Storage.Social.Files.TSFile;
    FAvatar                      : Storage.Avatars.Items.TItem;
    FRoster                      : Storage.Roster.Items.Item;
    FSFile                       : TFileStream;
  private
    function  cmdAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdUpdate(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Started; override;
  protected
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  protected
  end;
  procedure Install(Task:Core.Database.Types.TTask);


implementation
uses DateUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TAvatarCore.Install(Task);
end;

class procedure TAvatarCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TAvatarCore);
  With Avatar do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Add.cmd);
    COREOBJECT_VerifyID(Task,Update.cmd);
    COREOBJECT_VerifyID(Task,Get.cmd);
  end;
end;

class procedure TAvatarCore.UnInstall;
begin
  UnRegisterClass(TAvatarCore);
end;

procedure TAvatarCore.Initialize;
begin
  FSFile:=nil;
  Core.Arrays.VarString.Init(FSAList);
  with Avatar do begin
    Storage.CoreObjects.Add(Get.cmd,FCommands,Header,@cmdGet);
    Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdAdd);
    Storage.CoreObjects.Add(Update.cmd,FCommands,Header,@cmdUpdate);
  end;
end;


procedure TAvatarCore.Finalize;
begin
  FreeAndNil(FSFile);
  Core.Arrays.VarString.Done(FSAList);
end;

procedure TAvatarCore.Started;
begin
end;

function TAvatarCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Handled:=True;
  DataP:=SR.Info.DataP;
  FLand:=0;
  FFolderID:=0;
  FFileID:=0;
  FLength:=0;
  FNetworkID:=0;
  Result:=CO_STATUS_OK;
end;

function  TAvatarCore.cmdAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  procedure Push_Add_UserLand;
  begin
    if (FFolderID<>0) and (FFileID<>0)then begin
      if FFileNetworkID<>0 then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFileNetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFileNetworkID,FFolderID,FFileID,FNetworkFile,FSFile) then begin
              Try
                FAvatar.Extension:=Core.Utils.Files.Extract(FNetworkFile.Name,efeoNone);
                Core.Streams.Copy(FSFile,FRefactor);
                Result:=CO_STATUS_OK;
              finally
                FSFile.Free();
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
          end else
            Result:=CO_STATUS_ERR_CO_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else begin
        if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID, FFileID,FUserFile,FSFile) then begin
          Try
            Core.Streams.Copy(FSFile,FRefactor);
            FAvatar.Extension:=Core.Utils.Files.Extract(FUserFile.Name,efeoNone);
            Result:=CO_STATUS_OK;
          finally
            FSFile.Free();
          end;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
      end;
      if Result=CO_STATUS_OK then begin
        if Storage.Avatars.Items.DB.Add(FTask,UAP(SR)^.DomainID,FAvatar.OwnerID,FAvatar.Kind,FAvatar.Extension,FRefactor,FAvatar) then begin
          SetLength(FAvatar.Data,0);
          Storage.Avatars.Items.toXML(FAvatar,Transport(SR).Output,XML_HEADER_ON);
          If FAvatar.Kind = Storage.Avatars.Items.Kinds.User then begin
            Storage.Roster.Items.DB.setAvatarID(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAvatar.OwnerID,FAvatar.ID,FRoster.Modified)
          end else if FAvatar.Kind=Storage.Avatars.Items.Kinds.Network  then begin
            Storage.Social.Network.setAvatarID(FTask,UAP(SR)^.DomainID,FAvatar.OwnerID,FAvatar.ID);
          end;

          Storage.UserStorage.Files.Empty(FUserFile);
          Storage.Avatars.Items.Empty(FAvatar);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end;

  procedure Push_Add_NetworkLand;
  begin
    if ( (FNetworkID<>0) and (FFolderID<>0) and (FFileID<>0)) then begin
      if Storage.Avatars.Items.fromXML(FXMLDocument,FAvatar) and (FAvatar.ID<>0) and (FAvatar.OwnerID<>0) and (System.Length(FAvatar.Extension)>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetworkID,FNetwork) then begin
          if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
            // We could be looking at network file
            if FFileNetworkID<>0 then begin
              // Cleared to read Avatar File
              if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFileNetworkID,FNetwork) then begin
                if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
                  if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFileNetworkID,FFolderID,FFileID,FNetworkFile,FSFile) then begin
                    Try
                      FAvatar.Extension:=Core.Utils.Files.Extract(FNetworkFile.Name,efeoNone);
                      Core.Streams.Copy(FSFile,FRefactor);
                      Result:=CO_STATUS_OK;
                    finally
                      FSFile.Free();
                    end;
                  end else
                    Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                end else
                  Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else begin
              if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID,FFileID,FUserFile,FSFile) then begin
                Try
                  FAvatar.Extension:=Core.Utils.Files.Extract(FUserFile.Name,efeoNone);
                  Core.Streams.Copy(FSFile,FRefactor);
                  Result:=CO_STATUS_OK;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
            end;
            if Result=CO_STATUS_OK then begin
              if Storage.Avatars.Items.DB.Add(FTask,UAP(SR)^.DomainID,FAvatar.OwnerID,FAvatar.Kind,FAvatar.Extension,FRefactor,FAvatar) then begin
                If FAvatar.Kind = Storage.Avatars.Items.Kinds.User then begin
                  Storage.Roster.Items.DB.setAvatarID(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAvatar.OwnerID,FAvatar.ID,FRoster.Modified);
                  SetLength(FAvatar.Data,0);
                  Storage.Avatars.Items.toXML(FAvatar,Transport(SR).Output,XML_HEADER_ON);
                end else if FAvatar.Kind=Storage.Avatars.Items.Kinds.Network  then begin
                  Storage.Social.Network.setAvatarID(FTask,UAP(SR)^.DomainID,FAvatar.OwnerID,FAvatar.ID);
                  SetLength(FAvatar.Data,0);
                  Storage.Avatars.Items.toXML(FAvatar,Transport(SR).Output,XML_HEADER_ON);
                end;
                Storage.UserStorage.Files.Empty(FUserFile);
                Storage.Avatars.Items.Empty(FAvatar);
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end;

begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Avatars.Items.fromXML(FXMLDocument,FAvatar) and (FAvatar.ID<>0) and (FAvatar.OwnerID<>0) and (System.Length(FAvatar.Extension)>0) then begin
      //                                 |--0-|----1----|---2----|---3--|-----4*------|
      // Parameters from /core/avatar/add?Kind&NetworkID&FolderID&FileId&FileNetworkID&
      FLength:=System.Length(Parameters);
      if (FLength>2) then begin
        FLand:=StrToQWordDef(Parameters[0],0);
        FNetworkID:=StrToQWordDef(Parameters[1],0);
        FFolderID:=StrToQWordDef(Parameters[2],0);
        FFileID:=StrToQWordDef(Parameters[3],0);
        if FLength>4 then
          FFileNetworkID:=StrToQWordDef(Parameters[4],0);
        Case FLand of
          Storage.Avatars.Items.Lands.User    : Push_Add_UserLand;
          Storage.Avatars.Items.Lands.Network : Push_Add_NetworkLand;
        else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TAvatarCore.cmdUpdate  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure Push_Userland;
  begin
    if (FFolderID<>0) and (FFileID<>0) then begin
      if Storage.Avatars.Items.fromXML(FXMLDocument,FAvatar) and (FAvatar.ID<>0) and (System.Length(FAvatar.Extension)>0) then begin
        if FFileNetworkID<>0 then begin
          if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFileNetworkID,FNetwork) then begin
            if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
              if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFileNetworkID,FFolderID,FFileID,FNetworkFile,FSFile) then begin
                Try
                  FAvatar.Extension:=Core.Utils.Files.Extract(FNetworkFile.Name,efeoNone);
                  Core.Streams.Copy(FSFile,FRefactor);
                  Result:=CO_STATUS_OK;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
            end else
              Result:=CO_STATUS_ERR_CO_ACCESS_DENIED;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else begin
          if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID,FFileID,FUserFile,FSFile) then begin
            Try
              Core.Streams.Copy(FSFile,FRefactor);
              FAvatar.Extension:=Core.Utils.Files.Extract(FUserFile.Name,efeoNone);
              Storage.UserStorage.Files.Empty(FUserFile);
              Result:=CO_STATUS_OK;
            Finally
              FSFile.Free();
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end;
        if (Result=CO_STATUS_OK) then begin
          if Storage.Avatars.Items.DB.Update(FTask,UAP(SR)^.DomainID,FAvatar.Extension,FRefactor,FAvatar) then begin
            Storage.Avatars.Items.toXML(FAvatar,Transport(SR).Output,XML_HEADER_ON);
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end;
        Storage.Avatars.Items.Empty(FAvatar);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
  end;

  procedure Push_Networkland;
  begin
    if (FNetworkID<>0) and (FFolderID<>0) and (FFileID<>0) then begin
      if Storage.Avatars.Items.fromXML(FXMLDocument,FAvatar) and (FAvatar.ID<>0) and (System.Length(FAvatar.Extension)>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetworkId,FNetwork) then begin
          if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
            // We could be looking at network file
            if FFileNetworkID<>0 then begin
              // Cleared to read Network Aura Node
              if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFileNetworkID,FNetwork) then begin
                if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
                  if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFileNetworkID,FFolderID,FFileID,FNetworkFile,FSFile) then begin
                    Try
                      FAvatar.Extension:=Core.Utils.Files.Extract(FNetworkFile.Name,efeoNone);
                      Core.Streams.Copy(FSFile,FRefactor);
                      Storage.Social.Files.Empty(FNetworkFile);
                      Result:=CO_STATUS_OK;
                    finally
                      FSFile.Free();
                    end;
                  end else
                    Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
                end else
                  Result:=CO_STATUS_ERR_CO_ACCESS_DENIED;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else begin
              // User File
              if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID,FFileID,FUserFile,FSFile) then begin
                Try
                  Core.Streams.Copy(FSFile,FRefactor);
                  FAvatar.Extension:=Core.Utils.Files.Extract(FUserFile.Name,efeoNone);
                  Storage.UserStorage.Files.Empty(FUserFile);
                  Result:=CO_STATUS_OK;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end;
            if Result=CO_STATUS_OK then begin
              if Storage.Avatars.Items.DB.Update(FTask,UAP(SR)^.DomainID,FAvatar.Extension,FRefactor,FAvatar) then begin
                Storage.Avatars.Items.toXML(FAvatar,Transport(SR).Output,XML_HEADER_ON);
                Result:=CO_STATUS_OK;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
  end;

begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    //                               |--0-|----1----|----2---|---3--|-----4*------|
    // Parameters from /core/avatar/u?Kind&NetworkID&FolderID&FileId&FileNetworkID&
    FLength:=System.Length(Parameters);
    if (FLength>2) then begin
      FLand:=StrToQWordDef(Parameters[0],0);
      FNetworkID:=StrToQWordDef(Parameters[1],0);
      FFolderID:=StrToQWordDef(Parameters[2],0);
      FFileID:=StrToQWordDef(Parameters[3],0);
      if FLength>4 then
        FFileNetworkID:=StrToQWordDef(Parameters[4],0);
      Case FLand of
        Storage.Avatars.Items.Lands.User    : Push_Userland;
        Storage.Avatars.Items.Lands.Network : Push_NetworkLand;
      else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TAvatarCore.cmdGet     (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  // Parameters from /core/avatar?get&FileId
  FLength:=System.Length(Parameters);
  if (FLength>1) then begin
    FFileID:=StrToQWordDef(Parameters[1],0);
    if FFileID<>0 then begin
      if Storage.Avatars.Items.DB.Read(FTask,OwnerP^.DomainP^.ID,FFileID,FAvatar) then begin
        Transport(SR).CacheResponse:=FAvatar.Modified;
        Transport(SR).CacheTag:=FAvatar.Digest;
        FDiff:=DateUtils.MillisecondsBetween(FAvatar.Modified,Transport(SR).CacheRequest);
        FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));
        if (FDiff>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
          Transport(SR).CacheDate:=FAvatar.Modified;
          Transport(SR).CacheResponse:=FAvatar.Modified;
          Transport(SR).ContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FAvatar.Extension);
          Transport(SR).CacheExposure:=PUBLIC_CACHE;
          Transport(SR).CacheTag:=FAvatar.Digest;

          Core.Streams.fromData(FAvatar.Data,Transport(SR).Output);

          Transport(SR).CacheResponse:=0;
        end else begin
          Transport(SR).CacheDate:=FAvatar.Modified;
        end;
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
end;

end.


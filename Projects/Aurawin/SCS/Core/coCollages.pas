{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}
unit coCollages;

interface

uses
  Classes,

  App.Consts,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  HTTPDefs,
  hHttpd,

  Core.Timer,
  Core.Strings,
  Core.Keywords,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.Collages,
  Storage.Social,
  Storage.Social.Network,
  Storage.Social.Files,

  Storage.UserAccounts,
  Storage.UserStorage,
  Storage.Roster,
  Storage.Avatars,
  Storage.ContentTypes,
  Storage.AuraDisks,

  Core.Streams,
  Core.XML,
  Multimedia.Image,
  Encryption.Base64,

  SysUtils;

type
  Collages=class
  const
    ACLInf:TACLInfo=(
      Name                     : 'Collages';
      NameSpace                : '/core/collages';
      Caption                  : 'Collages Core Object';
      Prompt                   : 'User can access collage system';
      Description              : 'Back-end system to facilitate collages'
    );
    CLSInf:TCLSInfo=(
      Name                     : 'TCollagesCore';
      Location                 : 'coCollages.pas';
    );
    Header:TCoreObjectInfo=(
      ID                       : 0;
      ProviderID               : 0;
      Enabled                  : true;
      Anonymous                : true;
      Scale                    : 0;
      CLSInfo                  : @CLSInf;
      ACLInfo                  : @ACLInf;
    );
  Type
    Item=class
    const
      XMLInf:TXMLInfo=(
        Enabled                : true;
      );
    type
      Add=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Add';
          NameSpace            : '/ca';
          Caption              : 'Create Collage';
          Prompt               : 'User can create a collage';
          Description          : 'Access to create collage';
        );
        cmd:TCoreCommand=(
          HeaderP              : @Header;
          ID                   : 0;
          Enabled              : true;
          Anonymous            : false;
          Cache                : false;
          Compress             : true;
          Secure               : false;
          XMLInfo              : @XMLInf;
          ACLInfo              : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Read';
          NameSpace            : '/cr';
          Caption              : 'Read Collage';
          Prompt               : 'User can read collage';
          Description          : 'Access to read collages from database';
        );
        cmd:TCoreCommand=(
          HeaderP              : @Header;
          ID                   : 0;
          Enabled              : true;
          Anonymous            : false;
          Cache                : false;
          Compress             : true;
          Secure               : false;
          XMLInfo              : @XMLInf;
          ACLInfo              : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Write=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Write';
          NameSpace            : '/cw';
          Caption              : 'Write Collage';
          Prompt               : 'User can write collages';
          Description          : 'Access to write collages to database';
        );
        cmd:TCoreCommand=(
          HeaderP              : @Header;
          ID                   : 0;
          Enabled              : true;
          Anonymous            : false;
          Cache                : false;
          Compress             : true;
          Secure               : false;
          XMLInfo              : @XMLInf;
          ACLInfo              : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Delete=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Delete';
          NameSpace            : '/cd';
          Caption              : 'Delete Collage';
          Prompt               : 'User can delete collages';
          Description          : 'Access to delete collages from database';
        );
        cmd:TCoreCommand=(
          HeaderP              : @Header;
          ID                   : 0;
          Enabled              : true;
          Anonymous            : false;
          Cache                : false;
          Compress             : true;
          Secure               : false;
          XMLInfo              : @XMLInf;
          ACLInfo              : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      List=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'List';
          NameSpace            : '/cl';
          Caption              : 'List Collages';
          Prompt               : 'User can list collages';
          Description          : 'Access to list collages from database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      PalmView=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Palm View';
          NameSpace              : '/plmv';
          Caption                : 'Palm View of Collage';
          Prompt                 : 'User can view a published collage';
          Description            : 'Provides a palm view output for a collage';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : true;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
  end;
  TCollagesCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    FLength                      : LongInt;
    FNetwork                     : Storage.Social.Network.TNetwork;
    FCollage                     : Storage.Collages.Items.TItem;
    FCollages                    : Storage.Collages.Items.TItems;
    FPalmView                    : Storage.Collages.PalmView.TItem;
    FCreator                     : Storage.UserAccounts.Items.Item;
    FContact                     : Storage.Roster.Items.Item;
    FAvatar                      : Storage.Avatars.Items.TItem;
    FGlyphP                      : Storage.Collages.Images.PItem;
    FGlyphData                   : TFileStream;
    FGlyphExt                    : Core.Strings.VarString;
    FSourceKind                  : Core.Strings.VarString;
    FContentKind                 : Core.Strings.VarString;
    FContentType                 : Core.Strings.VarString;
    FGlyphStream                 : TMemoryStream;
    FX                           : LongInt;
    FY                           : LongInt;
  private
    procedure AssignGlyphDataAsXML(GlyphStream:TStream);
  private
    function  Perform_Item_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Item_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Item_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Item_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Item_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Palm_View(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(
        CommandP:PCoreCommand;
        Var SR:TRSR;
        var Parameters:Core.Arrays.Types.VarString;
        var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings;
        Data:TMemoryStream;
        var Handled:Boolean
    ):WORD; override;
  end;

  procedure Install(Task:Core.Database.Types.TTask);
implementation

procedure Install(Task:Core.Database.Types.TTask);
begin
  TCollagesCore.Install(Task);
end;

class procedure TCollagesCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TCollagesCore);

  Storage.CoreObjects.Add(Collages.Header,CoreObjectItems);
  COREOBJECT_VerifyID(Task,Collages.Header);

  COREOBJECT_VerifyID(Task,Collages.Item.PalmView.cmd);
  COREOBJECT_VerifyID(Task,Collages.Item.Add.cmd);
  COREOBJECT_VerifyID(Task,Collages.Item.Read.cmd);
  COREOBJECT_VerifyID(Task,Collages.Item.Write.cmd);
  COREOBJECT_VerifyID(Task,Collages.Item.Delete.cmd);
  COREOBJECT_VerifyID(Task,Collages.Item.List.cmd);
end;

class procedure TCollagesCore.UnInstall;
begin
  UnRegisterClass(TCollagesCore);
end;

procedure TCollagesCore.Initialize;
begin
  FGlyphStream:=TMemoryStream.Create();
  Storage.CoreObjects.Add(Collages.Item.PalmView.cmd,FCommands,Header,@Perform_Palm_View);
  Storage.CoreObjects.Add(Collages.Item.Read.cmd,FCommands,Header,@Perform_Item_Read);
  Storage.CoreObjects.Add(Collages.Item.Add.cmd,FCommands,Header,@Perform_Item_Add);
  Storage.CoreObjects.Add(Collages.Item.Write.cmd,FCommands,Header,@Perform_Item_Write);
  Storage.CoreObjects.Add(Collages.Item.Delete.cmd,FCommands,Header,@Perform_Item_Delete);
  Storage.CoreObjects.Add(Collages.Item.List.cmd,FCommands,Header,@Perform_Item_List);
end;

procedure TCollagesCore.Finalize;
begin
  FGlyphStream.Free();
end;

function  TCollagesCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_OK;
  FLength:=0;
  DataP:=SR.Info.DataP;
  FGlyphStream.Clear();
  FGlyphData:=nil;
  SetLength(FGlyphExt,0);
  SetLength(FSourceKind,0);
  SetLength(FContentKind,0);
  SetLength(FContentType,0);
  Storage.Social.Network.Empty(FNetwork);
  Storage.Collages.Items.Empty(FCollages);
  Storage.Collages.Items.Empty(FCollage);
  Storage.Collages.PalmView.Empty(FPalmView);
  Storage.UserAccounts.Items.Empty(FCreator);
  Storage.Roster.Items.Empty(FContact);
  Storage.Avatars.Items.Empty(FAvatar);
end;

function  TCollagesCore.Perform_Item_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  ( Storage.Collages.Items.fromXML(FXMLDocument,FCollage) and (FCollage.ID<>0) ) then begin
      FNetwork.ID:=FCollage.NetworkID;
      if FNetwork.ID<>0 then begin
        If Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID, FNetwork.ID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_NETWORK_READ_FAIL;
      end else
        Result:=CO_STATUS_OK;
      if Result=CO_STATUS_OK then begin
        if Storage.Collages.Items.DB.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,UAP(SR)^.ID,FCollage.ID,FCollage) then begin
          Storage.Collages.Items.toXML(FCollage,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TCollagesCore.Perform_Item_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  ( Storage.Collages.Items.fromXML(FXMLDocument,FCollage) and (FCollage.ID<>0) ) then begin
      FNetwork.ID:=FCollage.NetworkID;
      if FNetwork.ID<>0 then begin
        If Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID, FNetwork.ID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_NETWORK_READ_FAIL;
      end else
        Result:=CO_STATUS_OK;
      if (Result=CO_STATUS_OK) then begin
        if Storage.Collages.Items.DB.Write(FTask,UAP(SR)^.DomainID,FNetwork.ID,UAP(SR)^.ID,FCollage) then begin
          Storage.Collages.Images.DB.Update(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FCollage.ID,FCollage.Glyphs);

          Storage.UserAccounts.Items.Copy(UAP(SR)^,FCreator);
          FContact.DomainID:=FCreator.DomainID;
          FContact.UserID:=FCreator.ID;
          FContact.ID:=FCreator.Contact;
          Storage.Roster.Items.DB.Read(FTask,FContact);

          AssignGlyphDataAsXML(FGlyphStream);

          // We need to re-write dbmCollages.Images data

          Storage.AuraDisks.Files.Acquire(UAP(SR)^.AuraNode,FCreator.DomainID,FCreator.ID,Header.ID,FCollage.ID,Storage.AuraDisks.Kinds.CoreData,FGlyphData);
          if FGlyphData<>nil then begin
            Try
              Core.Streams.Copy(FGlyphStream,FGlyphData);
            finally
              FGlyphData.Free();
              FGlyphData:=nil;
            end;
          end;
          FGlyphStream.Clear();
          Storage.Collages.Items.toXML(FCollage,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TCollagesCore.Perform_Item_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  Storage.Collages.Items.fromXML(FXMLDocument,FCollage)   then begin
      FNetwork.ID:=FCollage.NetworkID;
      if FNetwork.ID<>0 then begin
        If Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID, FNetwork.ID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_NETWORK_READ_FAIL;
      end else
        Result:=CO_STATUS_OK;
      if Result=CO_STATUS_OK then begin
        if Storage.Collages.Items.DB.Add(FTask,UAP(SR)^.DomainID,FNetwork.ID,UAP(SR)^.ID,FCollage) then begin
          Storage.UserAccounts.Items.Copy(UAP(SR)^,FCreator);
          FContact.DomainID:=FCreator.DomainID;
          FContact.UserID:=FCreator.ID;
          FContact.ID:=FCreator.Contact;
          Storage.Roster.Items.DB.Read(FTask,FContact);
          AssignGlyphDataAsXML(FGlyphStream);
          Storage.AuraDisks.Files.Create(UAP(SR)^.AuraNode,FCreator.DomainID,FCreator.ID,Header.ID,FCollage.ID,Storage.AuraDisks.Kinds.CoreData,FGlyphData);
          if FGlyphData<>nil then begin
            Try
              Core.Streams.Copy(FGlyphStream,FGlyphData);
            finally
              FGlyphData.Free();
              FGlyphData:=nil;
            end;
          end;
          FGlyphStream.Clear();
          Storage.Collages.Items.toXML(FCollage,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TCollagesCore.Perform_Item_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  ( Storage.Collages.Items.fromXML(FXMLDocument,FCollage) and (FCollage.ID<>0) ) then begin
      FNetwork.ID:=FCollage.NetworkID;
      if FNetwork.ID<>0 then begin
        If Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID, FNetwork.ID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_NETWORK_READ_FAIL;
      end else
        Result:=CO_STATUS_OK;

      if Result=CO_STATUS_OK then begin
        if Storage.Collages.Items.DB.Delete(FTask,UAP(SR)^.DomainID,FNetwork.ID,UAP(SR)^.ID,FCollage.ID) then begin
          Storage.AuraDisks.Files.Delete(UAP(SR)^.AuraNode,UAP(SR)^.DomainID,FCreator.ID,Header.ID,FCollage.ID,Storage.AuraDisks.Kinds.CoreData);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TCollagesCore.Perform_Item_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FNetwork.ID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,fieldSearch);
    if (FNetwork.ID<>0) then begin
      If Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID, FNetwork.ID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_NETWORK_READ_FAIL;
    end else
      Result:=CO_STATUS_OK;

    If Result=CO_STATUS_OK then begin
      if Storage.Collages.Items.DB.List(FTask,UAP(SR)^.DomainID, FNetwork.ID,UAP(SR)^.ID,FCollages) then begin
        Storage.Collages.Items.toXML(FCollages,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

procedure TCollagesCore.AssignGlyphDataAsXML(GlyphStream:TStream);
var
  iLcv:integer;
begin
  GlyphStream.Size:=0;
  for iLcv:=0 to High(FCollage.Glyphs) do begin
    FGlyphP:=FCollage.Glyphs[iLcv];
    if (FGlyphP^.NetworkID<>0) and (FGlyphP^.NetworkID<>FNetwork.ID) then
      Storage.Social.Network.Read(FTask,FContact.DomainID,FGlyphP^.NetworkID,FNetwork);
    if (FGlyphP^.NetworkID<>0) then begin
      Storage.Social.Files.Data(FNetwork.Node,FContact.DomainID,FGlyphP^.NetworkID,FGlyphP^.FolderID,FGlyphP^.FileID, FGlyphData);
      Storage.Social.Files.Extension(FTask,FContact.DomainID,FGlyphP^.NetworkID,FGlyphP^.FolderID,FGlyphP^.FileID,FGlyphExt);
    end else begin
      // Going to read user file
      Storage.UserStorage.Files.Data(FCreator.AuraNode,FContact.DomainID,FCreator.ID,FGlyphP^.FolderID,FGlyphP^.FileID,FGlyphData);
      Storage.UserStorage.Files.DB.Extension(FTask,FContact.DomainID,FCreator.ID,FGlyphP^.FileID,FGlyphExt);
    end;
    // Process Image for Palm-print
    if (FGlyphData<>nil) then begin
      Try
        FContentType:=RSR.HTTP.ContentTypeFromFile(Storage.ContentTypes.List,FGlyphExt);
        FSourceKind:=Multimedia.Image.Tool.Kind.fromString(FGlyphExt);
        FContentKind:=FSourceKind;
        FX:=512;
        FY:=384;
        Core.Streams.Copy(FGlyphData,FRefactor);
        If Multimedia.Image.Tool.Transform(FRefactor,FX,FY) then begin
        //If uImage.Image.Transform(FRefactor,FContentType,FSourceKind,FContentKind,FX,FY) then begin
          FGlyphP^.Data:=Concat(
            'data:',
            FContentType,
            ';base64,',
            Encryption.Base64.Encode(FRefactor)
          );
        end; // todo replacement image
        Storage.Collages.Images.toXML(FGlyphP^,FGlyphStream);
        SetLength(FGlyphP^.Data,0);
      Finally
        FGlyphData.Free();
        FGlyphData:=nil;
      end;
    end;
  end;
end;

function  TCollagesCore.Perform_Palm_View(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if ( Storage.Collages.PalmView.fromXML(FXMLDocument,FPalmView) and (FPalmView.ID<>0) ) then begin
    if Storage.Collages.Items.DB.Read(FTask,OwnerP^.DomainP^.ID,FPalmView.ID,FCollage) then begin
      FCreator.ID:=FCollage.UserID;
      FCreator.DomainID:=OwnerP^.DomainP^.ID;
      if Storage.UserAccounts.Items.DB.Fill(FTask,FCreator.ID,FCreator) then begin
        FContact.DomainID:=FCreator.DomainID;
        FContact.UserID:=FCreator.ID;
        FContact.ID:=FCreator.Contact;

        if Storage.Roster.Items.DB.Read(FTask,FContact) then begin
          FPalmView.Created:=FCollage.Created;
          FPalmView.Modified:=FCollage.Modified;
          FPalmView.Kind:=FCollage.Kind;
          FPalmView.Delay:=FCollage.Delay;
          if (FContact.AvatarID<>0) then begin
            if Storage.Avatars.Items.DB.Read(FTask,FContact.DomainID,FContact.AvatarID,FAvatar) then
              FPalmView.Avatar:=Storage.Avatars.Items.asDataString(FAvatar,FContentType);
          end;
          Storage.AuraDisks.Files.Acquire(FCreator.AuraNode,FCreator.DomainID,FCreator.ID,Header.ID,FCollage.ID,Storage.AuraDisks.Kinds.CoreData,FGlyphData);
          if FGlyphData<>nil then begin
            Try
              FPalmView.Glyphs:=Core.Streams.toString(FGlyphData);
            finally
              FGlyphData.Free();
              FGlyphData:=nil;
            end;
          end;
          Try
            FPalmView.By:=Concat(FContact.FirstName,' ',FContact.LastName);
            FPalmView.Title:=FCollage.Title;
            FPalmView.Description:=FCollage.Description;
            Storage.Collages.PalmView.toXML(FPalmView,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          finally
            Storage.Collages.Items.Empty(FCollage);
          end;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
     Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
end;

end.


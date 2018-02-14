unit coAccount;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,
  SysUtils,

  uProviders,
  hHTTPd,

  App.Consts,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Keywords,


  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  Core.Timer,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.XML,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.UserAccounts,

  HTTPDefs;

type
  Account=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'Account';
      NameSpace                  : '/core/vdm/account';
      Caption                    : 'Account Core Object';
      Prompt                     : 'User can access account information';
      Description                : 'Back-end system to facilitate account settings'
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TAccountCore';
      Location                   : 'coAccount.pas';
    );
    XMLInfOff:TXMLInfo=(
      Enabled                    : false;
    );
    XMLInfOn:TXMLInfo=(
      Enabled                    : true;
    );
    Header:TCoreObjectInfo=(
      ID                         : 0;
      ProviderID                 : 0;
      Enabled                    : true;
      Anonymous                  : false;
      Scale                      : 0;
      CLSInfo                    : @CLSInf;
      ACLInfo                    : @ACLInf;
    );
  Type
    Read=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Read';
        NameSpace                : '/ar';
        Caption                  : 'Read Account';
        Prompt                   : 'User can read account information';
        Description              : 'Account access to read information from database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : false;
        XMLInfo                  : @XMLInfOn;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    Write=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Write';
        NameSpace                : '/aw';
        Caption                  : 'Write Account';
        Prompt                   : 'User can write account information';
        Description              : 'Account access to write information to database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInfOn;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    SetContact=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Set Contact';
        NameSpace                : '/sc';
        Caption                  : 'Set Contact';
        Prompt                   : 'User can set the contact record for their account';
        Description              : 'Access to set contact record to database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInfOn;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    DiskConsumption=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Disk Use';
        NameSpace                : '/dc';
        Caption                  : 'Disk Consumption';
        Prompt                   : 'User can calculate disk usage';
        Description              : 'Access to disk consumption aggregate';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInfOn;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
  end;
  TAccountCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    FAccount                     : Storage.UserAccounts.Items.Item;
  private
    function  Perform_Account_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Account_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Account_SetContact(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Account_DiskConsumption(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    // RSR Core Object Initialization Call happens on entry
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  end;

  procedure Install(Task:Core.Database.Types.TTask);
implementation

procedure Install(Task:Core.Database.Types.TTask);
begin
  TAccountCore.Install(Task);
end;

class procedure TAccountCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TAccountCore);
  with Account do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Read.cmd);
    COREOBJECT_VerifyID(Task,Write.cmd);
    COREOBJECT_VerifyID(Task,DiskConsumption.cmd);
    COREOBJECT_VerifyID(Task,SetContact.cmd);
  end;
end;

class procedure TAccountCore.UnInstall;
begin
  UnRegisterClass(TAccountCore);
end;

procedure TAccountCore.Initialize;
begin
  Storage.UserAccounts.Items.Init(FAccount);
  with Account do begin
    Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Perform_Account_Read);
    Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Perform_Account_Write);
    Storage.CoreObjects.Add(DiskConsumption.cmd,FCommands,Header,@Perform_Account_DiskConsumption);
    Storage.CoreObjects.Add(SetContact.cmd,FCommands,Header,@Perform_Account_SetContact);
  end;
end;

procedure TAccountCore.Finalize;
begin
  Storage.UserAccounts.Items.Done(FAccount);
end;

function  TAccountCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
  Storage.UserAccounts.Items.Empty(FAccount);
  Result:=CO_STATUS_OK;
end;

function  TAccountCore.Perform_Account_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FAccount.DomainID:=UAP(SR)^.DomainID;
    if Storage.UserAccounts.Items.DB.Fill(FTask,UAP(SR)^.ID,FAccount) then begin
      Storage.UserAccounts.Items.toXML(FAccount,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TAccountCore.Perform_Account_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserAccounts.Items.fromXML(FXMLDocument,FAccount,UAP(SR)^.DomainID,UAP(SR)^.ID) and (FAccount.ID=UAP(SR)^.ID) then begin
      if Storage.UserAccounts.Items.DB.Write(FTask,FAccount) then begin
        Storage.UserAccounts.Items.toXML(FAccount,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TAccountCore.Perform_Account_SetContact(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserAccounts.Items.fromXML(FXMLDocument,FAccount,UAP(SR)^.DomainID,UAP(SR)^.ID) and (FAccount.ID=UAP(SR)^.ID) and (FAccount.Contact<>0) then begin
      if Storage.UserAccounts.Items.DB.SetContact(FTask,FAccount.DomainID,FAccount.ID,FAccount.Contact) then begin
        Storage.UserAccounts.Items.toXML(FAccount,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TAccountCore.Perform_Account_DiskConsumption(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FAccount.DomainID:=UAP(SR)^.DomainID;
    FAccount.ID:=UAP(SR)^.ID;
    if Storage.UserAccounts.Items.DB.Fill(FTask,FAccount.ID,FAccount) then begin
      if Storage.UserAccounts.Items.DB.UpdateConsumption(FTask,FAccount.DomainID,FAccount.ID,FAccount.Consumption) then begin
        Storage.UserAccounts.Items.toXML(FAccount,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;
end.


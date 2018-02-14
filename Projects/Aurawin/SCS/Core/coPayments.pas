{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}
unit coPayments;

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

  Core.Keywords,
  Core.XML,

  hHttpd,

  Core.Timer,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,


  Storage,
  Storage.Main,
  Storage.Commerce,
  Storage.CoreObjects,
  Storage.UserAccounts,

  HTTPDefs,
  SysUtils;

type
  Payments=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'Payments';
      NameSpace                  : '/core/vdm/payments';
      Caption                    : 'Payments Core Object';
      Prompt                     : 'User can access payment information';
      Description                : 'Back-end system to facilitate payments'
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TPaymentsCore';
      Location                   : 'coPayments.pas';
    );
    XMLInf:TXMLInfo=(
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
        NameSpace                : '/pr';
        Caption                  : 'Read Payment';
        Prompt                   : 'User can read payment accounts';
        Description              : 'Payment access to read information from database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    Write=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Write';
        NameSpace                : '/pw';
        Caption                  : 'Write Payment';
        Prompt                   : 'User can write payment accounts';
        Description              : 'Payment access to write account information to database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    Delete=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Delete';
        NameSpace                : '/pd';
        Caption                  : 'Delete Payment';
        Prompt                   : 'User can delete payment accounts';
        Description              : 'Payment account access to delete information from database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    List=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'List';
        NameSpace                : '/pl';
        Caption                  : 'List Payment Accounts';
        Prompt                   : 'User can list payment accounts';
        Description              : 'Payment access to list account information from database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    Add=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Add';
        NameSpace                : '/pa';
        Caption                  : 'Add Payment';
        Prompt                   : 'User can add a payment account';
        Description              : 'Payment access to add an account to database';
      );
      cmd:TCoreCommand=(
        HeaderP                  : @Header;
        ID                       : 0;
        Enabled                  : true;
        Anonymous                : false;
        Cache                    : false;
        Compress                 : true;
        Secure                   : true;
        XMLInfo                  : @XMLInf;
        ACLInfo                  : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
  end;
  TPaymentsCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    FCard                        : Storage.Commerce.Card.TItem;
    FCards                       : Storage.Commerce.Card.TItems;
  private
    function  Perform_Payment_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Payment_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Payment_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Payment_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Payment_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
  TPaymentsCore.Install(Task);
end;

class procedure TPaymentsCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TPaymentsCore);
  with Payments do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Read.cmd);
    COREOBJECT_VerifyID(Task,Write.cmd);
    COREOBJECT_VerifyID(Task,List.cmd);
    COREOBJECT_VerifyID(Task,Add.cmd);
    COREOBJECT_VerifyID(Task,Delete.cmd);
  end;
end;

class procedure TPaymentsCore.UnInstall;
begin
  UnRegisterClass(TPaymentsCore);
end;

procedure TPaymentsCore.Initialize;
begin
  Storage.Commerce.Card.Init(FCard);
  Storage.Commerce.Card.Init(FCards);
  with Payments do begin
    Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Perform_Payment_Read);
    Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Perform_Payment_Write);
    Storage.CoreObjects.Add(List.cmd,FCommands,Header,@Perform_Payment_List);
    Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Perform_Payment_Add);
    Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Perform_Payment_Delete);
  end;
end;

procedure TPaymentsCore.Finalize;
begin
  Storage.Commerce.Card.Done(FCard);
  Storage.Commerce.Card.Done(FCards);
end;

function  TPaymentsCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
  Storage.Commerce.Card.Empty(FCard);
  Storage.Commerce.Card.Empty(FCards);
  Result:=CO_STATUS_OK;
end;

function  TPaymentsCore.Perform_Payment_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Card.fromXML(FXMLDocument,FCard) then begin
      if Storage.Commerce.Card.Add(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FCard) then begin
        Storage.Commerce.Card.toXML(FCard,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TPaymentsCore.Perform_Payment_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Card.fromXML(FXMLDocument,FCard) and (FCard.ID<>0) then begin
      if Storage.Commerce.Card.Read(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FCard.ID,FCard) then begin
        Storage.Commerce.Card.toXML(FCard,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TPaymentsCore.Perform_Payment_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Card.fromXML(FXMLDocument,FCard) and (FCard.ID<>0) then begin
      if Storage.Commerce.Card.Write(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FCard) then begin
        Storage.Commerce.Card.toXML(FCard,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TPaymentsCore.Perform_Payment_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Card.fromXML(FXMLDocument,FCard) and (FCard.ID<>0) then begin
      if Storage.Commerce.Card.Delete(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FCard.ID) then begin
        Storage.Commerce.Card.toXML(FCard,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TPaymentsCore.Perform_Payment_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Card.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FCards) then begin
      Storage.Commerce.Card.toXML(FCards,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

end.


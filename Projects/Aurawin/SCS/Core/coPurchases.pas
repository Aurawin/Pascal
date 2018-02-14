{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}
unit coPurchases;

interface

uses
  Classes,

  App.Consts,

  RSR,
  RSR.HTTP,
  RSR.Core,

  Core.Keywords,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  HTTPDefs,
  hHttpd,

  Core.Timer,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.Commerce,

  Core.XML,

  SysUtils;

type
  Purchases=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'Purchases';
      NameSpace                  : '/core/vdm/purchases';
      Caption                    : 'Purchases Core Object';
      Prompt                     : 'User can access purchase information';
      Description                : 'Back-end system to facilitate purchases'
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TPurchasesCore';
      Location                   : 'coPurchases.pas';
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
        Caption                  : 'Read Purchase';
        Prompt                   : 'User can read purchase items';
        Description              : 'Purchase access to read items from database';
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
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Write=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Write';
        NameSpace                : '/pw';
        Caption                  : 'Write Purchase';
        Prompt                   : 'User can write purchase items';
        Description              : 'Purchase access to write items to database';
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
        Caption                  : 'Delete Purchase';
        Prompt                   : 'User can delete purchase items';
        Description              : 'Purchase access to delete items from database';
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
        Caption                  : 'List Purchases';
        Prompt                   : 'User can list purchase items';
        Description              : 'Purchase access to list items from database';
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
        Caption                  : 'Add Purchase';
        Prompt                   : 'User can add a purchase item';
        Description              : 'Payment access to add a purchase item to database';
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
  TPurchasesCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    FPurchase                    : Storage.Commerce.Purchase.Item.TItem;
    FPurchases                   : Storage.Commerce.Purchase.Item.TItems;
  private
    function  Perform_Purchase_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Purchase_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Purchase_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Purchase_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Purchase_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
  TPurchasesCore.Install(Task);
end;

class procedure TPurchasesCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TPurchasesCore);
  with Purchases do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Read.cmd);
    COREOBJECT_VerifyID(Task,Write.cmd);
    COREOBJECT_VerifyID(Task,List.cmd);
    COREOBJECT_VerifyID(Task,Add.cmd);
    COREOBJECT_VerifyID(Task,Delete.cmd);
  end;
end;

class procedure TPurchasesCore.UnInstall;
begin
  UnRegisterClass(TPurchasesCore);
end;

procedure TPurchasesCore.Initialize;
begin
  Storage.Commerce.Purchase.Item.Init(FPurchase);
  Storage.Commerce.Purchase.Item.Init(FPurchases);
  with Purchases do begin
    Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Perform_Purchase_Read);
    Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Perform_Purchase_Write);
    Storage.CoreObjects.Add(List.cmd,FCommands,Header,@Perform_Purchase_List);
    Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Perform_Purchase_Add);
    Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Perform_Purchase_Delete);
  end;
end;

procedure TPurchasesCore.Finalize;
begin
  Storage.Commerce.Purchase.Item.Done(FPurchase);
  Storage.Commerce.Purchase.Item.Done(FPurchases);
end;

function  TPurchasesCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
  Storage.Commerce.Purchase.Item.Empty(FPurchase);
  Storage.Commerce.Purchase.Item.Empty(FPurchases);
  Result:=CO_STATUS_OK;
end;

function  TPurchasesCore.Perform_Purchase_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Purchase.Item.fromXML(FXMLDocument,FPurchase) then begin
      if Storage.Commerce.Purchase.Item.DB.Add(FTask,UAP(SR)^.DomainID,FPurchase) then begin
        Storage.Commerce.Purchase.Item.toXML(FPurchase,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TPurchasesCore.Perform_Purchase_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Purchase.Item.fromXML(FXMLDocument,FPurchase) and (FPurchase.ID<>0) then begin
      if Storage.Commerce.Purchase.Item.DB.Read(FTask,UAP(SR)^.DomainID, FPurchase.ID,FPurchase) then begin
        Storage.Commerce.Purchase.Item.toXML(FPurchase,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TPurchasesCore.Perform_Purchase_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Purchase.Item.fromXML(FXMLDocument,FPurchase) and (FPurchase.ID<>0) then begin
      if Storage.Commerce.Purchase.Item.DB.Write(FTask,UAP(SR)^.DomainID, FPurchase) then begin
        Storage.Commerce.Purchase.Item.toXML(FPurchase,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TPurchasesCore.Perform_Purchase_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Purchase.Item.fromXML(FXMLDocument,FPurchase) and (FPurchase.ID<>0) then begin
      if Storage.Commerce.Purchase.Item.DB.Delete(FTask,UAP(SR)^.DomainID, FPurchase.ID) then begin
        Storage.Commerce.Purchase.Item.toXML(FPurchase,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TPurchasesCore.Perform_Purchase_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Commerce.Purchase.Item.DB.List(FTask,UAP(SR)^.DomainID, FPurchases) then begin
      Storage.Commerce.Purchase.Item.toXML(FPurchases,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

end.


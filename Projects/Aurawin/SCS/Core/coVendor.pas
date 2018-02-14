unit coVendor;

{
 unit coVendor.pas
 partially implements dbmApps;

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html


 Vendor System for Application Framework API

 coVendor offers back-end access to
   Vendor :
     Registration :

     Information :
       Name
       When they were founded
       Contact Information
       Status
}

interface

uses
  Classes,

  hHTTPd,

  App.Consts,

  RSR,
  RSR.HTTP,
  RSR.Core,

  Core.Database,
  Core.Database.Types,

  Core.Timer,
  Core.Keywords,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,

  Core.Utils.Time,
  Core.Utils.Files,

  Storage,
  Storage.Main,
  Storage.Domains,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.RSS,
  Storage.Calendaring,
  Storage.Apps,
  Storage.Vendors,


  SysUtils;


type
  ns=class
  type
    Vendor=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Vendor';
        NameSpace                : '/core/vendor';
        Caption                  : 'Vendor Core Object';
        Prompt                   : 'User can access vendor system';
        Description              : 'Back-end system to manage vendors'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TVendorCore';
        Location                 : 'coVendor.pas';
      );
      Header:TCoreObjectInfo=(
        ID                       : 0;
        ProviderID               : 0;
        Enabled                  : true;
        Anonymous                : false;
        Scale                    : 0;
        CLSInfo                  : @CLSInf;
        ACLInfo                  : @ACLInf;
      );
      XMLInf:TXMLInfo=(
        Enabled                  : false;
      );
    Type
      Register=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Register';
          NameSpace              : '/reg';
          Caption                : 'Registration';
          Prompt                 : 'User can register as a vendor for the system';
          Description            : 'System-wide vendor registration';
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
          Method             : nil;
          Resource           : nil;
        );
      end;
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/r';
          Caption                : 'Read Vendor';
          Prompt                 : 'User can read vendor information for the system';
          Description            : 'System-wide vendor retrieval';
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
          Method             : nil;
          Resource           : nil;
        );
      end;
      Write=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Write';
          NameSpace              : '/w';
          Caption                : 'Write Vendor';
          Prompt                 : 'User can write vendor information for the system';
          Description            : 'System-wide vendor write access';
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
          Method             : nil;
          Resource           : nil;
        );
      end;
      Status=class
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/st/r';
            Caption              : 'Read Status';
            Prompt               : 'User can get vendor status';
            Description          : 'Read system-wide status for a vendor';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : true;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        write=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Write';
            NameSpace            : '/st/w';
            Caption              : 'Write Status';
            Prompt               : 'User can set vendor status';
            Description          : 'Write system-wide status for a vendor';
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
            Method             : nil;
            Resource           : nil;
          );
        end;
      end;
      State=class
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/se/r';
            Caption              : 'Read State';
            Prompt               : 'User can get vendor state';
            Description          : 'Read system-wide state for a vendor';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : true;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Write=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Write';
            NameSpace            : '/se/w';
            Caption              : 'Write State';
            Prompt               : 'User can set vendor state';
            Description          : 'Write system-wide state for a vendor';
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
            Method             : nil;
            Resource           : nil;
          );
        end;
      end;
    end;
  end;
  TVendorCore=Class(TCoreObject)
  private
    FPacket                      : Core.Arrays.Types.KeyStrings;
    DataP                        : PHTTP;
    FVendor                      : Storage.Vendors.Items.Item;
  private
     // Commands for System Vendors
    function  cmdRegister(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdStatusRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdStatusWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdStateRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdStateWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  private
  protected
     class procedure Install(Task:Core.Database.Types.TTask); override;
     class procedure UnInstall; override;
     procedure Initialize; override;
     procedure Finalize; override;
     function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; Var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  End;

  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses DateUtils,md5;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TVendorCore.Install(Task);
end;

class procedure TVendorCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TVendorCore);
  with ns.Vendor do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Register.cmd);
    COREOBJECT_VerifyID(Task,Status.Read.cmd);
    COREOBJECT_VerifyID(Task,Status.Write.cmd);
    COREOBJECT_VerifyID(Task,State.Read.cmd);
    COREOBJECT_VerifyID(Task,State.Write.cmd);
    COREOBJECT_VerifyID(Task,Read.cmd);
    COREOBJECT_VerifyID(Task,Write.cmd);
  end;
end;

class procedure TVendorCore.UnInstall;
begin
  UnRegisterClass(TVendorCore);
end;

procedure TVendorCore.Initialize;
begin
  Storage.Vendors.Items.Init(FVendor);
  with ns.Vendor do begin
    Storage.CoreObjects.Add(Register.cmd,FCommands,Header,@cmdRegister);
    Storage.CoreObjects.Add(Status.Read.cmd,FCommands,Header,@cmdStatusRead);
    Storage.CoreObjects.Add(Status.Write.cmd,FCommands,Header,@cmdStatusWrite);
    Storage.CoreObjects.Add(State.Write.cmd,FCommands,Header,@cmdStateWrite);
    Storage.CoreObjects.Add(State.Read.cmd,FCommands,Header,@cmdStateRead);
    Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdRead);
    Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdWrite);
  end;
end;

procedure TVendorCore.Finalize;
begin
  Storage.Vendors.Items.Done(FVendor);
end;

function  TVendorCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; Var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Handled:=True;
  DataP:=SR.Info.DataP;
  Result:=CO_STATUS_OK;
  Storage.Vendors.Items.Empty(FVendor);
end;

function  TVendorCore.cmdRegister(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
  Digest                         : TMD5Digest;
  Context                        : TMD5Context;
  iID                            : QWord;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    FVendor.Founded:=Core.Arrays.KeyString.GetItemAsDouble(srcHeaders,Storage.Vendors.Items.DB.Keys.Founded,iHdrCount);
    FVendor.Joined:=Core.Timer.dtUT;
    FVendor.Modified:=FVendor.Joined;

    FVendor.Size:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Vendors.Items.DB.Keys.Size,iHdrCount);
    FVendor.State:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Vendors.Items.DB.Keys.State,iHdrCount,Storage.Vendors.Items.DB.Defaults.State);
    FVendor.Name:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Name,iHdrCount);
    FVendor.Email:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Email,iHdrCount);
    FVendor.Website:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Website,iHdrCount);
    FVendor.Phone:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Phone,iHdrCount);
    FVendor.Street1:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Street1,iHdrCount);
    FVendor.Street2:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Street2,iHdrCount);
    FVendor.City:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.City,iHdrCount);
    FVendor.Province:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Province,iHdrCount);
    FVendor.Postal:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Postal,iHdrCount);
    FVendor.Country:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Country,iHdrCount);
    FVendor.Status:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Status,iHdrCount);


    md5.MD5Init(Context);
    md5.MD5Update(Context,UAP(SR)^.ID,SizeOf(UAP(SR)^.ID));
    md5.MD5Update(Context,OwnerP^.DomainP^.ID,SizeOf(OwnerP^.DomainP^.ID));
    md5.MD5Update(Context,FVendor.Joined,SizeOf(FVendor.Joined));
    md5.MD5Final(Context,Digest);
    FVendor.Auth:=md5.MD5Print(Digest);

    Empty(respHeaders);
    Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Auth,FVendor.Auth);
    Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Joined,FloatToStr(FVendor.Joined));

    if Storage.Vendors.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FVendor) then begin
      Result:=CO_STATUS_OK;
      TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;

function  TVendorCore.cmdStatusRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
  iVendorID                      : QWord;
  sAuth                          : Core.Strings.VarString;
  sStatus                        : Core.Strings.VarString;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    sAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Auth,iHdrCount);
    if (Length(sAuth)>0) then begin
      if Storage.Vendors.Items.DB.getStatus(FTask,OwnerP^.DomainP^.ID,sAuth,sStatus) then begin
        Empty(respHeaders);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Status,sStatus);
        Result:=CO_STATUS_OK;
        TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;

function  TVendorCore.cmdStatusWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
  iVendorID                      : QWord;
  sAuth                          : Core.Strings.VarString;
  sStatus                        : Core.Strings.VarString;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    sAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Auth,iHdrCount);
    sStatus:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Status,iHdrCount);
    if (Length(sAuth)>0) then begin
      if Storage.Vendors.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,sAuth,iVendorID) then begin
        if Storage.Vendors.Items.DB.setStatus(FTask,iVendorID,sStatus) then begin
          Empty(respHeaders);
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
        end;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;

function  TVendorCore.cmdStateWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
  iState                         : LongInt;
  iVendorID                      : QWord;
  sAuth                          : Core.Strings.VarString;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    sAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Auth,iHdrCount);
    iState:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Vendors.Items.DB.Keys.State,iHdrCount,-1);
    if (iState<>-1) and (Length(sAuth)>0) then begin
      if Storage.Vendors.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,sAuth,iVendorID) then begin
        if Storage.Vendors.Items.DB.setState(FTask,iVendorID,iState) then begin
          Empty(respHeaders);
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
        end;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;

function  TVendorCore.cmdStateRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
  iState                         : LongInt;
  iVendorID                      : QWord;
  sAuth                          : Core.Strings.VarString;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    sAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Auth,iHdrCount);
    iState:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Vendors.Items.DB.Keys.State,iHdrCount,-1);
    if (iState<>-1) and (Length(sAuth)>0) then begin
      if Storage.Vendors.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,sAuth,iVendorID) then begin
        if Storage.Vendors.Items.DB.getState(FTask,iVendorID,iState) then begin
          Empty(respHeaders);
          Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.State,IntToStr(iState));
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
        end;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;


function  TVendorCore.cmdRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    FVendor.Auth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Auth,iHdrCount);
    if Length(FVendor.Auth)>0 then begin
      // We should retrieve item from DB and
      if Storage.Vendors.Items.DB.Read(FTask,FVendor.Auth,FVendor) then begin
        Empty(respHeaders);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Founded,FloatToStr(FVendor.Founded));
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Joined,FloatToStr(FVendor.Joined));
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Modified,FloatToStr(FVendor.Modified));
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Size,IntToStr(FVendor.Size));
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.State,IntToStr(FVendor.State));
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Auth,FVendor.Auth);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Name,FVendor.Name);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Email,FVendor.Email);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Website,FVendor.Website);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Phone,FVendor.Phone);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Street1,FVendor.Street1);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Street2,FVendor.Street2);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.City,FVendor.City);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Province,FVendor.Province);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Postal,FVendor.Postal);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Country,FVendor.Country);
        Result:=CO_STATUS_OK;
        TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;

function  TVendorCore.cmdWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    FVendor.Auth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Vendors.Items.DB.Keys.Auth,iHdrCount);
    if Length(FVendor.Auth)>0 then begin
      // We should retrieve item from DB and
      if Storage.Vendors.Items.DB.Read(FTask,FVendor.Auth,FVendor) then begin
        FVendor.Founded:=Core.Arrays.KeyString.GetItemAsDouble(srcHeaders,Storage.Vendors.Items.DB.Keys.Founded,iHdrCount,FVendor.Founded);
        FVendor.Size:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Vendors.Items.DB.Keys.Size,iHdrCount,FVendor.Size);
        FVendor.State:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Vendors.Items.DB.Keys.State,iHdrCount,FVendor.State);
        FVendor.Name:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Name,iHdrCount,FVendor.Name);
        FVendor.Email:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Email,iHdrCount,FVendor.Email);
        FVendor.Website:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Website,iHdrCount,FVendor.Website);
        FVendor.Phone:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Phone,iHdrCount,FVendor.Phone);
        FVendor.Street1:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Street1,iHdrCount,FVendor.Street1);
        FVendor.Street2:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Street2,iHdrCount,FVendor.Street2);
        FVendor.City:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.City,iHdrCount,FVendor.City);
        FVendor.Province:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Province,iHdrCount,FVendor.Province);
        FVendor.Postal:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Postal,iHdrCount,FVendor.Postal);
        FVendor.Country:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Country,iHdrCount,FVendor.Country);
        FVendor.Status:=Core.Arrays.KeyString.GetItemAsShortString(srcHeaders,Storage.Vendors.Items.DB.Keys.Status,iHdrCount,FVendor.Status);
        Empty(respHeaders);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Vendors.Items.DB.Keys.Auth,FVendor.Auth);
        if Storage.Vendors.Items.DB.Write(FTask,FVendor) then begin
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
        end;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;
end;

end.


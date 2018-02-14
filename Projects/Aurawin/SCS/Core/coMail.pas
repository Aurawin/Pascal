{
 Copyright Aurawin LLC 2003-2011
 Written by: Andrew Thomas Brunner

 This code is protected by the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit coMail;

interface
  uses Classes,uCoreObjects,RSR,uRSRServer,uKeywords,uStorage,hSRConsts,hHttpd,
    hDatabase,RSR.HTTP,Core.Arrays.VarString,uByteArray,Core.Arrays.KeyString,Core.Timer,Storage.CoreObjects,
    dbmDomains,Storage.UserAccounts,dbmTmpAccounts;

Type
  ns=class
  type
    Mail=class
    const
      ACLInf:TACLInfo=(
        Name        : 'Mail';
        NameSpace   : '/core/mail';
        Caption     : 'Mail System Core Object';
        Prompt      : 'User access for e-mail';
        Description : 'Provides access to send and recieve e-mail';
      );
      CLSInf:TCLSInfo=(
        Name        : 'TMailCore';
        Location    : 'coMail.pas';
      );
      Header:TCoreObjectInfo=(
        ID          : 0;
        Enabled     : true;
        Scale       : 0;
        CLSInfo     : @CLSInf;
        ACLInfo     : @ACLInf;
      );
    type
      Anonymous=class
      const
        ACLInf:TACLinfo=(
          Name            : 'Login';
          NameSpace       : '/anon';
          Caption         : 'Anonymous system login';
          Prompt          : 'User can uses cookies to establish identity';
          Description     : 'Cookie based authentication for ad-hoc applications';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          InfoP                  : nil;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : true;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Auth=class
      const
        ACLInf:TACLinfo=(
          Name            : 'Auth';
          NameSpace       : '/auth';
          Caption         : 'Instant system authorization and verification';
          Prompt          : 'User uses md5 to verify previous identity credential match';
          Description     : 'Authentication established during transport for secure system access to applications, core objects, and core commands';
        );
        cmd:TCoreCommand=(
          HeaderP         : @Header;
          InfoP           : nil;
          ID              : 0;
          Enabled         : true;
          Anonymous       : false;
          ACLInfo         : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Credentials=class
      const
        ACLInf:TACLinfo=(
          Name            : 'Credentials';
          NameSpace       : '/creds';
          Caption         : 'System login';
          Prompt          : 'User uses md5 to provide username and or password for authorization';
          Description     : 'Authentication for secure system access to applications, core objects, and core commands';
        );
        cmd:TCoreCommand=(
          HeaderP         : @Header;
          InfoP           : nil;
          ID              : 0;
          Enabled         : true;
          Anonymous       : true;
          ACLInfo         : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Register=class
      const
        ACLInf:TACLinfo=(
          Name            : 'Register';
          NameSpace       : '/reg';
          Caption         : 'System Registraion';
          Prompt          : 'Users can signup for membership';
          Description     : 'System signup to applications, core objects, and core commands';
        );
        cmd:TCoreCommand=(
          HeaderP         : @Header;
          InfoP           : nil;
          ID              : 0;
          Enabled         : true;
          Anonymous       : false;
          ACLInfo         : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
  end;
  TMailCore=Class(TCoreObject)
  const
    fieldAccount                 : String = 'USER';
    fieldDomain                  : String = 'DOMAIN';
  private
    iRemoteIP                    : QWord;
    DataP                        : PHTTP;
  private
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Headers:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD; override;
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses SysUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TMailCore.Install(Task);
end;

class procedure TMailCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TLoginCore);
  with ns.Login do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Anonymous.cmd);
  end;
end;

class procedure TMailCore.UnInstall;
begin
  UnRegisterClass(TLoginCore);
end;

procedure TMailCore.Initialize;
begin

end;

procedure TMailCore.Finalize;
begin

end;

function  TMailCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Headers:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
begin
  Result:=CO_STATUS_OK;
  DataP:=SR.Info.DataP;
end;

function  TMailCore.Process_Anonymous(CommandP:PCoreCommand; Var SR:TRSR; var Headers:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
var
  iID        : QWord;

  procedure PushCreateNewAccount;
  begin
    If TempUser_Add(FModuleP^,FTaskP^,OwnerP^.DomainP^.ID,iID,SR.Address.sin_addr.S_addr,DataP^.Auth) then begin
      DataP^.ULIP:=OwnerP^.Accounts.Acquire(iID);
      DataP^.ULIP^.User.Kind:=akTemporary;
      Core.Arrays.KeyString.Update(Headers,fieldEUID,IntToStr(DataP^.ULIP^.User.ID));
      Core.Arrays.KeyString.Update(Headers,fieldAuth,DataP^.Auth);
      Result:=CO_STATUS_OK;
      TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,RSR);
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
    end;
  end;

  procedure VerifyAuth;
  begin
    if TempUser_Find(FModuleP^,FTaskP^,OwnerP^.DomainP^.ID,iID,DataP^.Auth) then begin
      TempUser_SetLastIP(FModuleP^,FTaskP^,DataP^.ULIP^.User.ID,SR.Address.sin_addr.S_addr);
      Core.Arrays.KeyString.Update(Headers,fieldEUID,IntToStr(DataP^.ULIP^.User.ID));
      Core.Arrays.KeyString.Update(Headers,fieldAuth,DataP^.Auth);
      Result:=CO_STATUS_OK;
      TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,RSR);
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_AUTH_REQD);
    end;
  end;

begin
  If (DataP<>Nil) then begin
    iID:=Core.Arrays.KeyString.GetItemAsQWord(Headers,fieldEUID,0);
    DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldAuth);
    Core.Arrays.KeyString.Update(Headers,fieldEUK,IntToStr(Integer(akTemporary)));
    if (DataP^.ULIP<>nil) then begin
      if (DataP^.ULIP^.User.ID<>0) then begin
        if (iID<>0) and (DataP^.ULIP^.User.ID=iID) then begin
          TempUser_SetLastIP(FModuleP^,FTaskP^,DataP^.ULIP^.User.ID,SR.Address.sin_addr.S_addr);
          Core.Arrays.KeyString.Update(Headers,fieldEUID,IntToStr(DataP^.ULIP^.User.ID));
          Core.Arrays.KeyString.Update(Headers,fieldAuth,DataP^.Auth);
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,RSR);
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_AUTH_REQD);
        end;
      end else if (Length(DataP^.Auth)>0) then begin
        // Check credentials and evaluate
        VerifyAuth;
      end else begin
        OwnerP^.Accounts.Recycle(DataP^.ULIP);
        DataP^.ULIP:=nil;
        Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_AUTH_REQD);
      end;
    end else
      PushCreateNewAccount;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function  TMailCore.Process_Auth(CommandP:PCoreCommand; Var SR:TRSR; var Headers:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
var
  iID:QWord;

  procedure PushAuthRequired;
  begin
    Core.Arrays.KeyString.Update(Headers,fieldAuth,'');
    Core.Arrays.KeyString.Update(Headers,fieldEUK,IntToStr(Integer(akNormal)));
    Core.Arrays.KeyString.Update(Headers,fieldEUID,'0');
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_AUTH_REQD);
  end;

  procedure PushSuccess;
  begin
    DataP^.ULIP^.User.Kind:=akNormal;
    DataP^.ULIP^.User.Auth:=DataP^.Auth;
    TempUser_SetLastIP(FModuleP^,FTaskP^,iID,SR.Address.sin_addr.S_addr);
    Core.Arrays.KeyString.Update(Headers,fieldEUK,IntToStr(Integer(akNormal)));
    Core.Arrays.KeyString.Update(Headers,fieldEUID,IntToStr(iID));
    Core.Arrays.KeyString.Update(Headers,fieldAuth,DataP^.Auth);
    Result:=CO_STATUS_OK;
    TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,RSR);
  end;

begin
  If (DataP<>Nil) then begin
    iID:=StrToIntDef(Core.Arrays.KeyString.GetItemByKey(Headers,fieldEUID),0);
    DataP^.Auth:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldAuth);
    if (DataP^.ULIP=nil) then begin
      if UserAccount_Find(FModuleP^,FTaskP^,OwnerP^.DomainP^.ID,iID,DataP^.Auth) then begin
        DataP^.ULIP:=OwnerP^.Accounts.Acquire(iID);
        PushSuccess;
      end else
        PushAuthRequired;
    end else if (DataP^.ULIP^.User.Auth=DataP^.Auth) and (DataP^.ULIP^.User.ID=iID) then begin
      PushSuccess;
    end else
      PushAuthRequired;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function  TMailCore.Process_Credentials(CommandP:PCoreCommand; Var SR:TRSR; var Headers:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
var
  iID:QWord;
  iCount:Integer;
  sAuth:String;
  sUser:String;
  sDomain:String;

  procedure PushAuthRequired;
  begin
    Core.Arrays.KeyString.Update(Headers,fieldAuth,'');
    Core.Arrays.KeyString.Update(Headers,fieldEUK,IntToStr(Integer(akNormal)));
    Core.Arrays.KeyString.Update(Headers,fieldEUID,'0');
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_AUTH_REQD);
  end;
  procedure PushError(Code:Integer);
  begin
    OwnerP^.LastError:=Code;
    Result:=Code;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,Code);
  end;

  procedure PushSuccess;
  begin
    Result:=CO_STATUS_OK;
    OwnerP^.LastError:=0;
    Core.Arrays.KeyString.Update(Headers,fieldEUK,IntToStr(Integer(akNormal)));
    Core.Arrays.KeyString.Update(Headers,fieldEUID,IntToStr(iID));
    Core.Arrays.KeyString.Update(Headers,fieldAuth,DataP^.Auth);
    TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,RSR);
  end;

  procedure PushFindAccount;
  begin
  end;

begin
  //  Process_Credentials is called when an app wants to ensure authorization
  //  It is called after AUTH is established.  It is quick with no DB calls.
  If (DataP<>Nil) then begin
    iCount:=System.Length(Headers);
    iID:=Core.Arrays.KeyString.GetItemAsQWord(Headers,fieldEUID,iCount,0);
    sAuth:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldAuth,iCount);
    sUser:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldAccount,iCount);
    if UserAccount_Find(FModuleP^,FTaskP^,OwnerP^.DomainP^.ID,iID,sUser,sAuth) then begin
      DataP^.ULIP:=OwnerP^.Accounts.Acquire(iID);
      DataP^.Auth:=sAuth;
      PushSuccess;
    end else
      PushAuthRequired;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function  TMailCore.Process_Register(CommandP:PCoreCommand; Var SR:TRSR; var Headers:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
begin
  // Not yet implemented
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;


end.

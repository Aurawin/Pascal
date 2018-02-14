{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit coLogin;

interface
  uses
    Classes,

    RSR,
    RSR.Core,
    RSR.HTTP,

    Core.Keywords,

    App.Consts,

    hHttpd,


    Core.Strings,

    Core.Arrays,
    Core.Arrays.Types,
    Core.Arrays.VarString,
    Core.Arrays.Bytes,
    Core.Arrays.KeyString,
    Core.Arrays.LargeWord,
    Core.Timer,
    Core.Logging,
    Core.Utils.Sockets,


    Storage,
    Storage.Main,
    Storage.Domains,
    Storage.CoreObjects,
    Storage.UserAccounts,
    Storage.TmpAccounts,
    Storage.Roster,
    Storage.Intrusion,
    Storage.Security,

    Core.Database,
    Core.Database.Types,
    Core.Database.SQL,

    Core.XML,

    SysUtils;

Type
  ns=class
  type
    Login=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Login';
        NameSpace                : '/core/login';
        Caption                  : 'System Login Core Object';
        Prompt                   : 'User authentication for system access';
        Description              : 'Provides security for system, core objects, and core commands';
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TLoginCore';
        Location                 : 'coLogin.pas';
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
      XMLInf:TXMLInfo=(
        Enabled                  : false;
      );
    type
      Anonymous=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Login';
          NameSpace              : '/anon';
          Caption                : 'Anonymous system login';
          Prompt                 : 'User can uses cookies to establish identity';
          Description            : 'Cookie based authentication for ad-hoc applications';
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
      Auth=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Auth';
          NameSpace              : '/auth';
          Caption                : 'Instant system authorization and verification';
          Prompt                 : 'User uses md5 to verify previous identity credential match';
          Description            : 'Authentication established during transport for secure system access to applications, core objects, and core commands';
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
      Credentials=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Credentials';
          NameSpace              : '/creds';
          Caption                : 'System login';
          Prompt                 : 'User uses md5 to provide username and or password for authorization';
          Description            : 'Authentication for secure system access to applications, core objects, and core commands';
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
      Exists=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Exists';
          NameSpace              : '/exs';
          Caption                : 'User account exists';
          Prompt                 : 'Users can verify account availability';
          Description            : 'Provides account availability for domains.';
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
      Signup=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : true;
        );
        ACLInf:TACLinfo=(
          Name                   : 'Signup';
          NameSpace              : '/su';
          Caption                : 'System Signup';
          Prompt                 : 'Users can signup for membership';
          Description            : 'System signup to applications, core objects, and core commands';
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
  TLoginCore=Class(TCoreObject)
  private
    FItemP                       : Storage.UserAccounts.Items.PItem;
    FAccount                     : Storage.UserAccounts.Items.Item;
    FContact                     : Storage.Roster.Items.Item;
    FIntrusions                  : QWord;
    FUser                        : Core.Strings.VarString;
    FPass                        : Core.Strings.VarString;
    FAuth                        : Core.Strings.VarString;
    FDomain                      : Core.Strings.VarString;
    FRemoteIP                    : Core.Strings.VarString;
    FConnectionFilter            : Storage.Security.Filter.Item;
  private
    function  Process_Anonymous(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Process_Auth(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Process_Credentials(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Process_Signup(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Process_Exists(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses DateUtils,DOM;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TLoginCore.Install(Task);
end;

class procedure TLoginCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TLoginCore);
  with ns.Login do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Anonymous.cmd);
    COREOBJECT_VerifyID(Task,Auth.cmd);
    COREOBJECT_VerifyID(Task,Credentials.cmd);
    COREOBJECT_VerifyID(Task,Exists.cmd);
    COREOBJECT_VerifyID(Task,Signup.cmd);
  end;
end;

class procedure TLoginCore.UnInstall;
begin
  UnRegisterClass(TLoginCore);
end;

procedure TLoginCore.Initialize;
begin
  With ns.Login do begin
    Storage.CoreObjects.Add(Anonymous.cmd,FCommands,Header,@Process_Anonymous);
    Storage.CoreObjects.Add(Auth.cmd,FCommands,Header,@Process_Auth);
    Storage.CoreObjects.Add(Credentials.cmd,FCommands,Header,@Process_Credentials);
    Storage.CoreObjects.Add(Exists.cmd,FCommands,Header,@Process_Exists);
    Storage.CoreObjects.Add(Signup.cmd,FCommands,Header,@Process_Signup);
  end;
end;

procedure TLoginCore.Finalize;
begin

end;

function  TLoginCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_OK;
  Handled:=True;
  Storage.UserAccounts.Items.Empty(FAccount);
end;

function  TLoginCore.Process_Anonymous(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iID        : QWord;

  procedure PushCreateNewAccount;
  begin
    If Storage.TmpAccounts.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,iID,SR.Address.sin_addr.S_addr,PHTTP(SR.Info.DataP)^.Auth) then begin
      PHTTP(SR.Info.DataP)^.UAP:=OwnerP^.Accounts.Acquire(iID);
      PHTTP(SR.Info.DataP)^.UAP^.Kind:=akTemporary;
      Core.Arrays.KeyString.Update(respHeaders,fieldAuth,PHTTP(SR.Info.DataP)^.Auth);
      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
  end;

  procedure VerifyAuth;
  begin
    if Storage.TmpAccounts.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,iID,PHTTP(SR.Info.DataP)^.Auth) then begin
      Storage.TmpAccounts.Items.DB.SetLastIP(FTask,PHTTP(SR.Info.DataP)^.UAP^.ID,SR.Address.sin_addr.S_addr);
      Core.Arrays.KeyString.Update(respHeaders,fieldAuth,PHTTP(SR.Info.DataP)^.Auth);
      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    end;
  end;

begin
  If (SR.Info.DataP<>Nil) then begin
    // if needed check transport's onResponse and put there... PHTTP(SR.Info.DataP)^.Auth:=Core.Arrays.KeyString.GetItemByKey(Headers,fieldAuth);
    Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akTemporary)));
    if (PHTTP(SR.Info.DataP)^.UAP<>nil) then begin
      if (PHTTP(SR.Info.DataP)^.UAP^.ID<>0) then begin
        if (iID<>0) and (PHTTP(SR.Info.DataP)^.UAP^.ID=iID) then begin
          Storage.TmpAccounts.Items.DB.SetLastIP(FTask,PHTTP(SR.Info.DataP)^.UAP^.ID,SR.Address.sin_addr.S_addr);
          Core.Arrays.KeyString.Update(respHeaders,fieldAuth,PHTTP(SR.Info.DataP)^.Auth);
          Result:=CO_STATUS_OK;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
        end;
      end else if (Length(PHTTP(SR.Info.DataP)^.Auth)>0) then begin
        // Check credentials and evaluate
        VerifyAuth;
      end else begin
        PHTTP(SR.Info.DataP)^.UAP:=nil;
        Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
      end;
    end else
      PushCreateNewAccount;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
  end;
end;

function  TLoginCore.Process_Auth(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iID:QWord;
  iCount : integer;

  procedure PushAuthRequired;
  begin
    Core.Arrays.KeyString.Update(respHeaders,fieldAuth,'');
    Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akNormal)));
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_AUTH_REQD);
  end;

  procedure PushSuccess;
  begin
    PHTTP(SR.Info.DataP)^.UAP^.Kind:=akNormal;
    PHTTP(SR.Info.DataP)^.Auth:=FAuth;
    PHTTP(SR.Info.DataP)^.Account:=FUser;
    SR.Credentials:=PHTTP(SR.Info.DataP)^.UAP;
    Storage.UserAccounts.Items.DB.Fill(FTask,iID,PHTTP(SR.Info.DataP)^.UAP^);
    Storage.UserAccounts.Items.DB.SetLastIP(FTask,iID,SR.Address.sin_addr.S_addr);
    Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akNormal)));
    Core.Arrays.KeyString.Update(respHeaders,fieldAccount,FUser);
    Core.Arrays.KeyString.Update(respHeaders,fieldAuth,FAuth);

    Result:=CO_STATUS_OK;
    TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
  end;

  procedure PushReCheck;
  begin
    Inc(PHTTP(SR.Info.DataP)^.AuthCount);
    if Storage.UserAccounts.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,iID,FUser) then begin
      PHTTP(SR.Info.DataP)^.LastAuth:=0;
      PHTTP(SR.Info.DataP)^.AuthCount:=0;
      PushSuccess();
    end else begin
      PHTTP(SR.Info.DataP)^.LastAuth:=Core.Timer.dtUT;
      PushAuthRequired();
    end;
  end;

  procedure PushCheck;
  begin
    Inc(PHTTP(SR.Info.DataP)^.AuthCount);
    if Storage.UserAccounts.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,iID,FUser) then begin
      PHTTP(SR.Info.DataP)^.LastAuth:=0;
      PHTTP(SR.Info.DataP)^.AuthCount:=0;
      PushSuccess();
    end else begin
      PHTTP(SR.Info.DataP)^.LastAuth:=Core.Timer.dtUt;
      PushAuthRequired();
    end;
  end;
begin
  Handled:=True;
  If (SR.Info.DataP<>Nil) then begin
    iID:=0;
    iCount:=System.Length(srcHeaders);
    FUser:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAccount,iCount);
    FAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAuth,iCount);

    if (PHTTP(SR.Info.DataP)^.UAP=nil) then begin
      if (PHTTP(SR.Info.DataP)^.AuthCount<5) then begin
        PushCheck();
      end else
        PushAuthRequired;
    end else
      PushReCheck();
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function  TLoginCore.Process_Credentials(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iID:QWord;
  iCount:Integer;

  procedure PushAuthRequired;
  begin
    Core.Arrays.KeyString.Update(respHeaders,fieldAuth,'');
    Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akNormal)));
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
    Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akNormal)));
    Core.Arrays.KeyString.Update(respHeaders,fieldAuth,PHTTP(SR.Info.DataP)^.Auth);
    Core.Arrays.KeyString.Update(respHeaders,fieldAccount,PHTTP(SR.Info.DataP)^.Account);
    TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
  end;

  procedure PushInvalidLogin();
  begin
    Storage.Intrusion.Account.DB.Add(FTask,OwnerP^.DomainP^.ID,FUser,FAuth,SR.Address.sin_addr.S_addr);
    FIntrusions:=Storage.Intrusion.Account.DB.Recent(FTask,OwnerP^.DomainP^.ID,SR.Address.sin_addr.S_addr);
    if (FIntrusions>Storage.Intrusion.Account.Defaults.MaxRecentBeforeBlocked) then begin
      FRemoteIP:=Core.Utils.Sockets.InAddrToStr(SR.Address.sin_addr.S_addr);
      Storage.Intrusion.Intruder.DB.Add(FTask,OwnerP^.DomainP^.ID,SR.Address.sin_addr.S_addr,Core.Timer.dtUT);
      Storage.Security.Filter.Empty(FConnectionFilter);
      Try
        FConnectionFilter.Counter:=1;
        FConnectionFilter.Enabled:=True;
        FConnectionFilter.Value:=Core.Utils.Sockets.MaskClassC(FRemoteIP);
        if Storage.Security.Filter.DB.Exists(FTask,secViolatorIP,FConnectionFilter) = false then begin
          Storage.Security.Filter.DB.Identify(FTask,secViolatorIP,FConnectionFilter);
          Core.Logging.Native.WriteLogEntry(
            OwnerP^.DomainP^.Name,
            OwnerP^.Manager.Service,
            Concat(
              'Security alert : Account credentials blocked for (',FUser,'=',FAuth,') and rejected IPs (',FConnectionFilter.Value,'* )'
            )
          );
        end else begin
          Storage.Security.Filter.DB.Identify(FTask,secViolatorIP,FConnectionFilter);
        end;
      Finally
        Storage.Security.Filter.Empty(FConnectionFilter);
      end;
    end;
    PushAuthRequired();
  end;

begin
  //  Process_Credentials is called when an app wants to ensure authorization
  //  It is called after AUTH is established.
  Handled:=True;
  If (SR.Info.DataP<>Nil) then begin
    iCount:=System.Length(srcHeaders);
    iID:=0;
    FAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAuth,iCount);
    FUser:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAccount,iCount);
    if Storage.UserAccounts.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,iID,FUser) then begin
      FItemP:=PHTTP(SR.Info.DataP)^.UAP;
      if (FItemP<>nil) then begin
        Storage.UserAccounts.Items.DB.Fill(FTask,iID,FItemP^);
        If ((Length(FAuth)>0) and (FItemP^.Auth=FAuth) ) then begin
          If (
              (FItemP^.LockoutCount>0) and
                (
                  (FItemP^.LockoutCount<=Max_LockCount) or
                  (MinutesBetween(FItemP^.LastAccessed,Core.Timer.dtUT)>=2)
                ) or (
                  (FItemP^.LockoutCount<=Max_LockCount)
                )
              )
          then begin
            PHTTP(SR.Info.DataP)^.UAP:=FItemP;
            PHTTP(SR.Info.DataP)^.Auth:=FItemP^.Auth;
            PHTTP(SR.Info.DataP)^.Account:=FItemP^.User;
            SR.Credentials:=FItemP;
            PushSuccess();
          end else
            PushInvalidLogin();
        end else
          PushInvalidLogin();
      end else
          PushInvalidLogin();
    end else
      PushInvalidLogin();
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function  TLoginCore.Process_Exists(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iCount:Integer;
  sUser:Core.Strings.VarString;
begin
  //  Process_Credentials is called when an app wants to ensure authorization
  //  It is called after AUTH is established.  It is quick with no DB calls.
  If (SR.Info.DataP<>Nil) then begin
    iCount:=System.Length(srcHeaders);
    sUser:=Lowercase(Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAccount,iCount));
    if Storage.UserAccounts.Items.DB.Exists(FTask,sUser,OwnerP^.DomainP^.ID) then begin
      Result:=CO_STATUS_OK;
      OwnerP^.LastError:=0;
      Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akNormal)));
    end else begin
      Result:=CO_STATUS_NOT_FOUND;
      OwnerP^.LastError:=CO_STATUS_NOT_FOUND;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
  end;
end;

function  TLoginCore.Process_Signup(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iCount:Integer;
  sUser:Core.Strings.VarString;
  sAuth:Core.Strings.VarString;
  xAcct:TDOMNode;
  xRoster:TDOMNode;
  xContact:TDOMNode;
begin
  If (SR.Info.DataP<>Nil) then begin
    iCount:=System.Length(srcHeaders);
    sUser:=Lowercase(Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAccount,iCount));
    sAuth:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldAuth,iCount);
    if Storage.UserAccounts.Items.DB.Exists(FTask,sUser,OwnerP^.DomainP^.ID) then begin
      Result:=CO_STATUS_ERR_CO_CMD_DUPLICATE;
    end else begin
      xContact:=nil; xRoster:=Core.XML.DB.getNode(FXMLDocument,Storage.Roster.Items.XML.Stanzas.Contacts);
      if (xRoster<>nil) then xContact:=xRoster.FirstChild;
      if (
        (xRoster<>nil) and
        (xContact<>nil) and
        Storage.UserAccounts.Items.fromXML(FXMLDocument,FAccount,OwnerP^.DomainP^.ID,UA_ANY_ACCOUNT) and
        Storage.Roster.Items.fromXML(xContact,FContact,OwnerP^.DomainP^.ID,FAccount.ID)
      ) then begin
        FAccount.User:=Lowercase(FAccount.User);
        FAccount.Kind:=akNormal;
        FAccount.Enabled:=true;
        Core.Arrays.LargeWord.Copy(OwnerP^.DefaultP^.aclCoreObjects,FAccount.aclCoreObjects);
        Core.Arrays.LargeWord.Copy(OwnerP^.DefaultP^.aclCoreCommands,FAccount.aclCoreCommands);
        // FAccount.Password:=;
        if Storage.UserAccounts.Items.DB.Create(FTask,OwnerP^.DomainP^.ID,FAccount,FContact) then begin
          Result:=CO_STATUS_OK;
          OwnerP^.LastError:=0;
          Core.Arrays.KeyString.Update(respHeaders,fieldEUK,IntToStr(Integer(akNormal)));
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
end;


end.

unit auSettings;

interface

uses
  Core.Generics,
  App,
  App.Consts,
  App.Build,
  App.IniFile,
  Core.Logging,
  Encryption.Simple,
  Classes,
  SysUtils,
  Forms,
  IniFiles,
  MD5,
  Controls;

Const

  INI_SEC_SETTINGS               : String = 'Settings';
  INI_SEC_RESOURCE               : String = 'Resource';
  INI_SEC_PATH                   : String = 'Path';
  INI_KEY_PATH_COUNT             : String = 'Path Count';
  INI_KEY_NAME                   : String = 'Name';
  INI_KEY_ID                     : String = 'Id';
  INI_KEY_DESCRIPTION            : String = 'Description';
  INI_KEY_PATH                   : String = 'Folder';
  INI_KEY_SYNC                   : String = 'Sync';
  INI_KEY_PASSWORD               : String = 'Pass';
  INI_KEY_UNAME                  : String = 'User';
  INI_KEY_DOMAIN                 : String = 'Domain';
  INI_KEY_ENCODING               : String = 'Encoding';
  INI_KEY_AUTH                   : String = 'Auth';
  INI_KEY_SAVE_LOGIN             : String = 'Save Login';
  INI_KEY_AUTOSTART_SYNC         : String = 'Autostart Sync';



  SELECT_DELAY                   : WORD = 450;
  AUTO_SAVE_DELAY                : WORD = 5000;

  SYNC_STATUS_REFRESH              : DWORD = 500;
  SYNC_FAT_REFRESH                 : DWORD = 1000*60*3;
  SYNC_CALCULATE_SIZE              : DWORD = 1500;
  SYNC_STATUS_LINGER               : DWORD = 5000;
  SYNC_PROCESS_ITEMS_DELAY         : DWORD = 5000;
  SYNC_CHANNEL_SERVER_BUFFER_DELAY : DWORD = 7000;
  SYNC_CHANNEL_CLIENT_BUFFER_DELAY : DWORD = 7000;
  SYNC_SCAN_DELAY_FULL             : DWORD = 1000*60*5;
  SYNC_SCAN_DELAY_RETRY            : DWORD = 1000*10;


  mrSelect                       : Word=mrOK+14;
Type
   TStorage=Class;

   { TStorage }

   TStorage=class(TIniFile)
   private
     FUsername                   : String;
     FPassword                   : String;
     FAuth                       : String;
     FDomain                     : String;

     FManifestID                 : QWord;
     FResourceID                 : QWord;
     FUserID                     : QWord;
     FThrottle                   : QWord;

     FResourceName               : String;
     FResourceDescription        : String;
     FEncoding                   : String;
     FLoading                    : boolean;
     FSaveLogin                  : boolean;
     FAutoStartSync              : boolean;
   private
     procedure AuthChanged();
     procedure ResourceChanged();
     procedure EncodingChanged();
   private
     procedure setAuth(Value:String);
     procedure setDomain(Value:String);
     procedure setThrottle(Value: QWord);
     procedure setUsername(Value:String);
     procedure setPassword(Value:String);
     procedure setSaveLogin(Value:boolean);
     procedure setAutoStartSync(Value:boolean);
     procedure setResourceName(Value:String);
     procedure setResourceDescription(Value:String);
     procedure setEncoding(Value:string);
     procedure setResourceID(Value:QWord);
     procedure setUserID(Value:QWord);
     procedure setManifestID(Value:QWord);
   public
     procedure   Load();
   public
     constructor Create(); reIntroduce;
     destructor  Destroy(); override;
   public
     property AutoStartSync:boolean read FAutoStartSync write setAutoStartSync;
     property Auth:String read FAuth write setAuth;
     property Username:String read FUserName write setUsername;
     property Domain:string read FDomain write setDomain;
     property Password:String read FPassword write setPassword;
     property SaveLogin:boolean read FSaveLogin write setSaveLogin;
     property ResourceName:String read FResourceName write setResourceName;
     property ResourceID:QWord read FResourceID write setResourceID;
     property ManifestID:QWord read FManifestID write setManifestID;
     property Encoding:string read FEncoding write setEncoding;
     property UserID:QWord read FUserID write setUserID;
     property Throttle:QWord read FThrottle write setThrottle;
     property ResourceDescription: String read FResourceDescription write setResourceDescription;
   end;

Const Settings:TStorage=nil;

implementation
uses
  Storage,
  Storage.Main;

procedure Init;
begin
  App.Consts.APP_BASE:='AuCloud';
  App.Consts.APP_INI_NAME:= 'Settings.ini';
  App.Files.IniName:=@App.SandboxGenerateIniFileName;
  App.Files.LogName:=@App.SandboxGenerateLogFileName;
  App.IniFile.Init(App.Files.IniName());
  Core.Logging.Start(App.Files.LogName());

  Storage.Main.Init();

  if Settings=nil then
    Settings:=TStorage.Create();
end;

procedure Done;
begin
  Storage.Main.Done();
  If Assigned(Settings) then
    Settings.Free();
End;

constructor TStorage.Create();
begin
  FLoading:=False;
  inherited Create(App.Files.IniName());
end;

destructor TStorage.Destroy();
begin
  Inherited Destroy;
end;

procedure TStorage.Load();
begin
  FLoading:=True;
  FAutoStartSync:=ReadBool(INI_SEC_SETTINGS,INI_KEY_AUTOSTART_SYNC,false);
  FSaveLogin:=ReadBool(INI_SEC_SETTINGS,INI_KEY_SAVE_LOGIN,false);
  FAuth:=ReadString(INI_SEC_SETTINGS,INI_KEY_AUTH,'');
  FUserName:=ReadString(INI_SEC_SETTINGS,INI_KEY_UNAME,'');
  FPassword:=Encryption.Simple.DecryptString(ReadString(INI_SEC_SETTINGS,INI_KEY_PASSWORD,''));
  FDomain:=ReadString(INI_SEC_SETTINGS,INI_KEY_DOMAIN,'aurawin.com');
  FEncoding:=ReadString(INI_SEC_SETTINGS,INI_KEY_ENCODING,'UTF-8');
  FResourceID:=ReadInteger(INI_SEC_RESOURCE,INI_KEY_ID,0);
  FResourceName:=ReadString(INI_SEC_RESOURCE,INI_KEY_NAME,'');
  FResourceDescription:=ReadString(INI_SEC_RESOURCE,INI_KEY_DESCRIPTION,'');
  FLoading:=False;
  Storage.Main.Header.Encoding:=FEncoding;
end;

procedure TStorage.setAuth(Value:String);
begin
  FAuth:=Value;
  AuthChanged();
end;

procedure TStorage.setUsername(Value:String);
begin
  FUsername:=Value;
  AuthChanged();
end;

procedure TStorage.setDomain(Value:String);
begin
  FDomain:=Value;
  if FSaveLogin then begin
    WriteString(INI_SEC_SETTINGS,INI_KEY_DOMAIN,FDomain);
  end else begin
    WriteString(INI_SEC_SETTINGS,INI_KEY_DOMAIN,'');
  end;
end;

procedure TStorage.setPassword(Value:String);
begin
  FPassword:=Value;
  AuthChanged();
end;

procedure TStorage.setThrottle(Value:QWord);
begin
  FThrottle:=Value;
end;

procedure TStorage.setResourceName(Value:String);
begin
  FResourceName:=Value;
  ResourceChanged();
end;

procedure TStorage.setResourceDescription(Value:String);
begin
  FResourceDescription:=Value;
  ResourceChanged();
end;

procedure TStorage.setEncoding(Value: string);
begin
  FEncoding:=Value;
  EncodingChanged();
end;

procedure TStorage.setResourceID(Value:QWord);
begin
  FResourceID:=Value;
  ResourceChanged();
end;

procedure TStorage.setUserID(Value:QWord);
begin
  FuserID:=Value;
end;

procedure TStorage.setManifestID(Value:QWord);
begin
  FManifestID:=Value;
end;

procedure TStorage.AuthChanged();
var
  dgMD5:TMD5Digest;
begin
  dgMD5:=MD5.MD5String(Concat(FUserName,FPassword));
  FAuth:=MD5.MD5Print(dgMD5);
  if FLoading=false then begin
    if FSaveLogin then begin
      WriteString(INI_SEC_SETTINGS,INI_KEY_AUTH,FAuth);
      WriteString(INI_SEC_SETTINGS,INI_KEY_UNAME,FUsername);
      WriteString(INI_SEC_SETTINGS,INI_KEY_DOMAIN,FDomain);
      WriteString(INI_SEC_SETTINGS,INI_KEY_PASSWORD,Encryption.Simple.EncryptString(FPassword));
    end else begin
      WriteString(INI_SEC_SETTINGS,INI_KEY_AUTH,'');
      WriteString(INI_SEC_SETTINGS,INI_KEY_UNAME,'');
      WriteString(INI_SEC_SETTINGS,INI_KEY_DOMAIN,'');
      WriteString(INI_SEC_SETTINGS,INI_KEY_PASSWORD,'');
    end;
  end;
end;

procedure TStorage.ResourceChanged();
begin
  WriteInteger(INI_SEC_RESOURCE,INI_KEY_ID,FResourceID);
  WriteString(INI_SEC_RESOURCE,INI_KEY_NAME,FResourceName);
  WriteString(INI_SEC_RESOURCE,INI_KEY_DESCRIPTION,FResourceDescription);
end;

procedure TStorage.EncodingChanged();
begin
  WriteString(INI_SEC_SETTINGS,INI_KEY_ENCODING,FEncoding);
  Storage.Main.Header.Encoding:=FEncoding;
end;

procedure TStorage.setSaveLogin(Value:boolean);
begin
  FSaveLogin:=Value;
  WriteBool(INI_SEC_SETTINGS,INI_KEY_SAVE_LOGIN,FSaveLogin);
  AuthChanged();
end;

procedure TStorage.setAutoStartSync(Value:boolean);
begin
  FAutoStartSync:=Value;
  WriteBool(INI_SEC_SETTINGS,INI_KEY_AUTOSTART_SYNC,FAutoStartSync);
end;

initialization
  Init();

finalization
  Done();
end.


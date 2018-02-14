unit App;


interface
uses
  App.Consts,
  SysUtils,
  Core.Strings,
  Core.Utils.Files;

Type
  cbGenerateFileName=Function():TFileName;

  Files=Class
  const
    IniName : cbGenerateFileName=nil;
    LogName : cbGenerateFileName=nil;
  end;
  Folders=class
  const
    User    : cbGenerateFileName=nil;
    UserSSL : cbGenerateFileName=nil;
  end;

  function  SandboxGenerateIniFileName():Core.Strings.FileName;
  function  SandboxGenerateLogFileName():Core.Strings.FileName;

  function  DefaultGenerateIniFileName():Core.Strings.FileName;


implementation
uses
  App.Build;

function DefaultGenerateUserFolderName():Core.Strings.FileName;
begin
  Result:=IncludeTrailingPathDelimiter(GetAppConfigDir(false));
  ForceDirectories(Result);
end;

function DefaultGenerateUserSSLFolderName():Core.Strings.FileName;
begin
  Result:=Concat(
      ExcludeTrailingPathDelimiter(Folders.User()),
      SysUtils.PathDelim,
      'Certs'
  );
  ForceDirectories(Result);
end;

function DefaultGenerateIniFileName():Core.Strings.FileName;
begin
  {$if defined(Windows)}
    Result:=Concat(Folders.User(),App.Consts.APP_INI_NAME);
  {$elseif defined(Unix)}
    {$ifdef Darwin}
      Result:=Concat(Folders.User(),App.Consts.APP_INI_NAME);
    {$else}
      Result:=Concat('/etc/',App.Consts.APP_BASE,'/',App.Consts.APP_INI_NAME);
    {$endif}
  {$endif}
end;

function  SandboxGenerateIniFileName():Core.Strings.FileName;
begin
  {$if defined(Windows)}
    Result:=Concat(Folders.User(),App.Consts.APP_INI_NAME);
  {$elseif defined(Unix)}
    {$ifdef Darwin}
      Result:=Concat(APP_USER_FOLDER,App.Consts.APP_INI_NAME);
    {$else}
      Result:=Concat(GetAppConfigDir(false),App.Consts.APP_INI_NAME);
    {$endif}
  {$endif}
end;


function DefaultGenerateLogFileName():Core.Strings.FileName;
begin
  {$if defined(Windows)}
    Result:=Concat(Folders.User(),App.Consts.APP_LOG_NAME);
  {$elseif defined(Unix)}
    {$ifdef Darwin}
      Result:=Concat(APP_USER_FOLDER,App.Consts.APP_LOG_NAME);
    {$else}
      Result:=Concat('/var/log/',App.Consts.APP_BASE,'/',App.Consts.APP_LOG_NAME);
    {$endif}
  {$endif}
end;

function  SandboxGenerateLogFileName():Core.Strings.FileName;
begin
  {$if defined(Windows)}
    Result:=Concat(Folders.User(),App.Consts.APP_LOG_NAME);
  {$elseif defined(Unix)}
    {$ifdef Darwin}
      Result:=Concat(APP_USER_FOLDER,App.Consts.APP_LOG_NAME);
    {$else}
      Result:=Concat(GetAppConfigDir(false),App.Consts.APP_LOG_NAME);
    {$endif}
  {$endif}
end;

initialization
  Files.IniName:=@DefaultGenerateIniFileName;
  Files.LogName:=@DefaultGenerateLogFileName;
  Folders.User:=@DefaultGenerateUserFolderName;
  Folders.UserSSL:=@DefaultGenerateUserSSLFolderName;
end.


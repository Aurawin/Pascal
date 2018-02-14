unit hSRConsts;

interface
uses SysUtils,Classes,Core.Arrays.VarString,Core.Utils.Files,uAppBuild,IniFiles;

Const
  DOMAIN_RSR              = 'engine.rsr';
  SERVICE_AUTH            = 'AUTH';
  SERVICE_HTTP            = 'HTTP';
  SERVICE_SMTP            = 'SMTP';
  SERVICE_POP3            = 'POP3';
  SERVICE_XMPP            = 'XMPP';
  SERVICE_RTSP            = 'RTSP';
  SERVICE_KEYWORDS        = 'KEYWORDS';
  SERVICE_RSR             = 'RSR';
  SERVICE_CORE_OBJECTS    = 'Core';
  SERVICE_DNS_RESOLVER    = 'DNS_RESOLVER';
  SERVICE_CONFIG          = 'CONFIG';
  SERVICE_PROVIDER        = 'PROVIDER';
  SMTP_LAST_DOT           = #13#10'.'#13#10;
  SMTP_LAST_DOT_LEN       = 5;
  SMTP_HEADER             = #13#10#13#10;
  SMTP_CMD                = #10;
  INI_GUI_LOAD_SERVICES   = 'GUI Load Services';
  INI_FMT_PATH            = '%sSCS.ini';
  FMT_FORCE_PATH_FAILURE : String='%s could not be created.';
  RSR_STATS_FORMAT        = '###,###,###,###,###,###,###,###';

  DOMAIN_FAT_REFRESH_ACTIVE       = 5;        // seconds
  DOMAIN_FAT_REFRESH_INACTIVE     = 60*2;     // s->minutes
  DOMAIN_FAT_REFRESH_DWELL        = 60*60*1;  // s->hours
  DOMAIN_FAT_REFRESH_DELETE       = 1000*2;   // ms->s

const
  CO_STATUS_FAIL                           = 0;
  CO_STATUS_OK                             = 1;
  CO_STATUS_REDIRECT                       = 2;
  CO_STATUS_AUTHENTICATE                   = 3;
  CO_STATUS_RESET                          = 4;
  CO_STATUS_TIMEOUT                        = 5;
  CO_STATUS_NOT_FOUND                      = 6;
  CO_STATUS_BUFFERS_EXEEDED                = 7;


  CO_STATUS_ERROR                          = 100;
  CO_STATUS_EXCEPTION                      = 101;

  CO_STATUS_ERR_CO_NOT_FOUND               = 102;
  CO_STATUS_ERR_CO_FOLDER_NOT_FOUND        = 103;
  CO_STATUS_ERR_CO_RESOURCE_NOT_FOUND      = 104;
  CO_STATUS_ERR_CO_ACCESS_DENIED           = 105;
  CO_STATUS_ERR_CO_BEFORE_EXECUTE          = 106;
  CO_STATUS_ERR_CO_NO_DEVICE_ID            = 107;
  CO_STATUS_ERR_CO_RESERVED_1              = 108;
  CO_STATUS_ERR_CO_RESERVED_2              = 109;
  CO_STATUS_ERR_CO_RESERVED_3              = 110;

  CO_STATUS_ERR_CO_CMD_NOT_FOUND           = 200;
  CO_STATUS_ERR_CO_CMD_INITIALIZATION      = 201;
  CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND    = 202;
  CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND  = 203;
  CO_STATUS_ERR_CO_CMD_AUTH_REQD           = 204;
  CO_STATUS_ERR_CO_CMD_REDIRECT            = 205;
  CO_STATUS_ERR_CO_CMD_DBMS_FAILURE        = 206;
  CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD      = 207;
  CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED     = 208;
  CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED     = 209;
  CO_STATUS_ERR_CO_CMD_ACCESS_DENIED       = 210;
  CO_STATUS_ERR_CO_CMD_MISSING_FIELDS      = 211;
  CO_STATUS_ERR_CO_CMD_INVALID_SEARCH      = 212;
  CO_STATUS_ERR_CO_CMD_INVALID_XML         = 213;
  CO_STATUS_ERR_CO_CMD_DUPLICATE           = 214;
  CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER   = 215;
  CO_STATUS_ERR_CO_CMD_MEDIA_NOT_FOUND     = 216;
  CO_STATUS_ERR_CO_CMD_CHANNEL_NOT_FOUND   = 217;
  CO_STATUS_ERR_CO_CMD_INVALID_EXECUTION   = 218;
  CO_STATUS_ERR_CO_CMD_INVALID_PROPERTY    = 219;
  CO_STATUS_ERR_CO_CMD_INVALID_MEDIA       = 220;
  CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER   = 221;

  APP_MAX_STORAGE_SIZE=2000000000;
  Invalid_DomainChars='\/:* ?"<>|''';
  ERROR_Name='A domain or user name cannot be blank nor can it contain any of '+^M+
             'the folowing characters: \/:* ?"<>|';
  ERROR_DUPName='A mail server for this domain already exists.  Please'+^M+
             'select another domain name for this mail server.';
  ERROR_DUPUName='This user already exists in this domain.  Please'+^M+
             'select another username for this person.';
  DIALOG_CONFIRM_DROPTABLE='Confirm Table Delete.'^M^M+
             'Are you sure you want to delete this campaign and all of its data?'^M+
             'Once you delete the Campaign you will permenantly lose all information on this campaign.'^M^M+
             'Press Yes to delete this campaign.'^M;

  MAX_SYNC_BUFFER                : QWORD =1024*1024*30; // 30MB Buffer per socket
  CONTENT_TYPE_REFRESH           = 120;
  FMT_PROGRESS_USR_STATUS        : String=' %s %s';
  FMT_PROGRESS_SOC_STATUS        : String=' %s %s %s';
  FMT_PROGRESS_SOC_FOLDER_LIST   : String=' %s %s %s/../%s/';
  FMT_PROGRESS_SOC_FILE_LIST     : String=' %s %s %s/../%s/%s';
  SCS_PATH_DELIM          = '/';

  PWD_REQUEST_TITLE   : String ='%s requires authentication';
  PWD_REQUEST_MESSAGE : String='%s requires your username and password.';

  SIF_NONE=0;
  SIF_DELETE=1 shl 0;  // Item is marked for deletion
  SIF_READ=1 shl 1;    // Item has been marked as read/retrieved.

  PagesBorder=(
    {$if defined(Unix)}
      {$ifdef Darwin}
        4
      {$else}
        4
      {$endif}
    {$elseif defined(Windows)}
      4
    {$else}
      4
    {$endif}
  );
  EntryHeight=(
    {$if defined(Unix)}
      {$ifdef Darwin}
        34
      {$else}
        40
      {$endif}
    {$elseif defined(Windows)}
      34
    {$else}
      32
    {$endif}
  );
  HeaderOffsetTop=(
    {$if defined(Unix)}
      {$ifdef Darwin}
        0
      {$else}
        0
      {$endif}
    {$elseif defined(Windows)}
      4
    {$else}
      0
    {$endif}
  );
  ButtonsHeight=(
    {$if defined(Unix)}
      {$ifdef Darwin}
        40
      {$else}
        42
      {$endif}
    {$elseif defined(Windows)}
      40
    {$else}
      32
    {$endif}
  );
  HeaderHeight=(
    {$ifdef Unix}
      {$ifdef Darwin}
        24
      {$else}
        22
      {$endif}
    {$else}
      30
    {$endif}
  );
  StatusHeight=(
    {$ifdef Unix}
      {$ifdef Darwin}
        28
      {$else}
        28
      {$endif}
    {$else}
      30
    {$endif}
  );
type
  BorderSpacing=class
  type
    ProgressBar=class
    const
      Around         : LongInt = 3;
      Right          : LongInt = 4;
    end;
  end;

var
  SystemINI:TIniFile;
  RSR_LOG_FILE : String;
  SYS_INI_FILE:String;

  Function IsDomainStrValid(Const Domain:AnsiString):boolean;

implementation
uses Graphics,Controls, uLogging;

Function IsDomainStrValid(Const Domain:AnsiString):boolean;
var
  Len,Lcv:Integer;
begin
  Len:=Length(Domain); Result:=Domain<>''; Lcv:=1;
  While (Lcv<=Len) and (Result) do begin
    Result:=Pos(Domain[Lcv],Invalid_DomainChars)=0;
    Inc(Lcv);
  end;
end;


initialization
  Randomize;
  APP_Version:={$i AppBuild.inc};
  APP_Edition:=Concat({$i %FPCTARGETOS%},' ',{$i %FPCTARGETCPU%});
  APP_Title:='Aurawin Cloud Desktop';
  {$if defined(Windows)}
    RSR_LOG_FILE:=Concat(APP_USER_FOLDER,'RSR.Log');
    SYSTEM_DUMP:=Concat(APP_USER_FOLDER,'System.dmp');
    SYS_INI_FILE:=Concat(Format(INI_FMT_PATH,[APP_USER_FOLDER]));
  {$elseif defined(Unix)}
    {$ifdef Darwin}
      RSR_LOG_FILE:=Concat(APP_USER_FOLDER,'RSR.Log');
      SYSTEM_DUMP:=Concat(APP_USER_FOLDER,'System.dmp');
      SYS_INI_FILE:=Format(INI_FMT_PATH,[APP_USER_FOLDER]);
    {$else}
      RSR_LOG_FILE:=Concat(APP_USER_FOLDER,'RSR.Log');
      SYSTEM_DUMP:=Concat(APP_USER_FOLDER,'System.dmp');
      SYS_INI_FILE:=Format(INI_FMT_PATH,['/etc/scs/']);
    {$endif}
  {$endif}
  if SystemIni=nil then
    SystemIni:=TIniFile.Create(SYS_INI_FILE);
  if Core.Logging.Native=nil then
    Core.Logging.Native:=TSystemLog.Create(RSR_LOG_FILE);
finalization
  FreeAndNil(SystemIni);
  FreeAndNil(Core.Logging.Native);
end.

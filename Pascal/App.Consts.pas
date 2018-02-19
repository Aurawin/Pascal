unit App.Consts;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface
uses
  Core.Strings;

Const
  Max_Error                      =4;
  Max_LockCount                  =5;
  MaxLocksBeforeIntruder         = 40;

  PathDelim                      : Core.Strings.Char = '/';
  DOMAIN_RSR                     : Core.Strings.VarString = 'engine.rsr';
  SERVICE_MONITOR                : Core.Strings.VarString = 'SERVICE';
  SERVICE_PROCESS                : Core.Strings.VarString = 'PROCESS';
  SERVICE_AUDISK                 : Core.Strings.VarString = 'AUDISK';
  SERVICE_AUDISK_SSL             : Core.Strings.VarString = 'AUDISKS';
  SERVICE_AUTH                   : Core.Strings.VarString = 'AUTH';
  SERVICE_HTTP                   : Core.Strings.VarString = 'HTTP';
  SERVICE_HTTPS                  : Core.Strings.VarString = 'HTTPS';
  SERVICE_PROXY                  : Core.Strings.VarString = 'PROXY';
  SERVICE_SMTP                   : Core.Strings.VarString = 'SMTP';
  SERVICE_SMTP_SSL               : Core.Strings.VarString = 'SMTPS';
  SERVICE_IMAP                   : Core.Strings.VarString = 'IMAP';
  SERVICE_IMAP_SSL               : Core.Strings.VarString = 'IMAPS';
  SERVICE_SMTP_RELAY             : Core.Strings.VarString = 'SMTPRLY';
  SERVICE_POP3                   : Core.Strings.VarString = 'POP3';
  SERVICE_POP3_SSL               : Core.Strings.VarString = 'POP3S';
  SERVICE_XMPP                   : Core.Strings.VarString = 'XMPP';
  SERVICE_XMPPSTOS               : Core.Strings.VarString = 'XMPPSTOS';
  SERVICE_RTSP                   : Core.Strings.VarString = 'RTSP';
  SERVICE_KEYWORDS               : Core.Strings.VarString = 'KEYWORDS';
  SERVICE_RSR                    : Core.Strings.VarString = 'RSR';
  SERVICE_CORE_OBJECTS           : Core.Strings.VarString = 'Core';
  SERVICE_DNS_RESOLVER           : Core.Strings.VarString = 'DNS_RESOLVER';
  SERVICE_QUEUE                  : Core.Strings.VarString = 'QUEUE';
  SERVICE_CONFIG                 : Core.Strings.VarString = 'CONFIG';
  SERVICE_PROVIDER               : Core.Strings.VarString = 'PROVIDER';

  SCS_PATH_DELIM                 : Core.Strings.Char = '/';
  SCS_PROCESS                    = {$ifdef Unix} 'AuProcess' {$else} 'AuProcess.exe' {$endif};
  SCS_PROCESS_PATH               = {$ifdef Unix} '/usr/bin/scs/' {$else} '' {$endif};

  // Should not need SCS_ETC_PATH                   : Core.Strings.VarString = {$ifdef Unix} '/etc/scs/' {$else} '$APP_DATA_PATH' {$endif};

  APP_BASE                       : Core.Strings.VarString = 'scs';
  APP_INI_NAME                   : Core.Strings.VarString = 'scs.ini';
  APP_LOG_NAME                   : Core.Strings.VarString = 'rsr.log';

  SMTP_LAST_DOT                  : Core.Strings.VarString = #13#10'.'#13#10;
  SMTP_LAST_DOT_LEN              : System.LongInt = 5;
  SMTP_HEADER                    : Core.Strings.VarString = #13#10#13#10;
  SMTP_CMD                       : Core.Strings.VarString = #10;
  INI_GUI_LOAD_SERVICES          : Core.Strings.VarString = 'GUI Load Services';

  MODIFIED_THRESHOLD              = 1000 ; //milliseconds

  COMPRESS_MAX_THRESHOLD          = 1024*1024*5; // Compress 5MB or less for quick loads otherwise use faster
  COMPRESS_NONE_THRESHOLD         = 1024*1024*25; // Don't compress files over 25MB

  FOLDER_MS_REFRESH              : LongInt = 1000*60*1;

  FMT_FORCE_PATH_FAILURE         : Core.Strings.VarString = '%s could not be created.';
  FMT_FORCE_PATH_NULL_NODE       : Core.Strings.VarString = '%s is missing nodal identifiers.';
  RSR_STATS_FORMAT               : Core.Strings.VarString = '###,###,###,###,###,###,###,###';

  TEMPLATE_REBUILD_DELAY          = 1000*5; // milliseconds
  DOMAIN_FAT_REFRESH_ACTIVE       = 5;        // seconds
  DOMAIN_FAT_REFRESH_INACTIVE     = 60*2;     // s->minutes
  DOMAIN_FAT_REFRESH_DWELL        = 60*60*1;  // s->hours
  DOMAIN_FAT_REFRESH_DELETE       = 1000*2;   // ms->s

  APP_MAX_STORAGE_SIZE=2000000000;

  NODE_STAT_REFRESH               = 30;    // seconds
  NODE_CONFIG_REFRESH             = 60*15;  // seconds
  CONTENT_TYPE_REFRESH            = 120; // seconds
  RSS_REFRESH                     = 120; // seconds
  SERVICE_CHECK_DELAY             = 1000*5;

  Invalid_DomainChars='\/:* ?"<>|''';

  FMT_CONFORM_FOLDER_DELETE_TITLE = 'Confirm deleting this folder...';
  FMT_CONFIRM_FOLDER_DELETE_MSG   = 'The folder "%s" you want to delete will be permenantly deleted along with all sub folder and files.'^M'Are you sure you want to delete this folder?';
  FMT_FILES_LOAD                  = ' Loading folder "%s".  Please wait...';
  FMT_FILE_COUNT                  = ' %.0n %s';
  FMT_FOLDER_COUNT                = ' %.0n %s';
  FMT_BYTES_COUNT                 = ' %.0n %s';
  FMT_IMAGE_LOADING               = ' %s';
  FMT_IMAGE_CAPTION               = 'Image - %s';

  ERR_SMTP_MX_DIALOG             : Core.Strings.VarString='Permanent error encountered.  There was a problem while delivering your message.';
  ERR_SMTP_NO_MX_RECORDS:Core.Strings.VarString='Permanent error encountered.  There are no mail exchange server records found.';
  ERR_SMTP_NO_IP_RECORDS:Core.Strings.VarString='Permanent error encountered.  There are no IP address records found.';
  ERR_SMTP_TOO_MANY_TRIES:Core.Strings.VarString='Permanent error encountered.  There were too many attempts to deliver this message.';
  ERR_SMPT_NO_RELAYS_RUNNING:Core.Strings.VarString='Temporary error encountered.  There are no running relay servers.';

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

  Hacker:Array[boolean] of UTF8String = ('','Hacker');
  Intruder:Array[boolean] of UTF8String = ('','Intruder');

  MAX_SYNC_BUFFER                : QWORD =1024*1024*30; // 30MB Buffer per socket
  FMT_PROGRESS_USR_STATUS        : Core.Strings.VarString =' %s %s';
  FMT_PROGRESS_SOC_STATUS        : Core.Strings.VarString =' %s %s %s';
  FMT_PROGRESS_SOC_FOLDER_LIST   : Core.Strings.VarString =' %s %s %s/../%s/';
  FMT_PROGRESS_SOC_FILE_LIST     : Core.Strings.VarString =' %s %s %s/../%s/%s';

  PWD_REQUEST_TITLE              : Core.Strings.VarString ='%s requires authentication';
  PWD_REQUEST_MESSAGE            : Core.Strings.VarString ='%s requires your username and password.';

  SIF_NONE=0;
  SIF_DELETE=1 shl 0;  // Item is marked for deletion
  SIF_READ=1 shl 1;    // Item has been marked as read/retrieved.

  EXIT_CODE_OK        : DWORD = 0;
  EXIT_CODE_DBMS_FAIL : DWORD = 1010;
  EXIT_CODE_ENGINE_FAIL : DWORD = 1011;
  EXIT_CODE_AUDISK_FAIL : DWORD = 1014;

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
  StatusHeaderWidth=80;
  StatusHeaderSizeBias=25;
  StatusHeight=(
    {$ifdef Unix}
      {$ifdef Darwin}
        28
      {$else}
        35
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
  RSR=class
  type
    Engine=class
    type
      Settings=class
      const
        QueueItemsMax                    : DWORD = 100;
        COMMAND_TIMEOUT                  : DWORD = 60*35;
        BUFFER_EMPTY_THRESHOLD           : DWORD = 1024*1024;
        STACKSIZE_SCANNER                : DWORD = 1024*1024*20;   // 5MB Stack Size
        STACKSIZE_COMMANDS               : DWORD = 1024*1024*10;  // 10MB Stack Size
      end;
    end;
  end;

implementation

end.


unit Storage.Types;

interface
uses
  Core.Strings,
  Classes,
  SysUtils;

Const
  DB_GROUP_SYSTEM                : Core.Strings.VarString = 'System';
  INI_DB_VALUE_CLUSTERID         : Core.Strings.VarString = 'ClusterID';
  SQ                             : Core.Strings.VarString = #39;
  INI_DB_SECT_MATRIX             : Core.Strings.VarString ='Matrix';
  INI_DB_VALUE_RESOURCEID        : Core.Strings.VarString ='ResourceID';
  INI_DB_VALUE_NODEID            : Core.Strings.VarString ='NodeID';
  INI_DB_VALUE_AUXILIARY_NODES   : Core.Strings.VarString ='Auxiliary Nodes';
  INI_DB_VALUE_LOAD_DISKS        : Core.Strings.VarString ='Load Disks';

  FMT_TXT_MODIFIED : Array[Boolean] of Core.Strings.VarString = ('Unchanged','Modified');
  FMT_EDITOR_CAPTION='%s/%s';
  FMT_EDITOR_STATUS_PATH='%s%s/%s %s';
  FMT_EDITOR_STATUS_READ_ONLY:Array[Boolean] of Core.Strings.VarString=('','(Read Only)');

  InvalidTableNameChars: TSysCharSet=[' ',':',';','^','!','#','%','&','(',')','+','=','{','}','|','\','/','?',',','<','>','.','`','~','@','$'];

  //  File Store Kinds
  FS_BINARY             =0;
  FS_SMTP               =1;
  FS_XMPP_Chat          =3;
  FS_XMPP_Error         =4;
  FS_XMPP_GroupChat     =5;
  FS_XMPP_Headline      =6;
  FS_XMPP_Normal        =7;

  UA_TYPES_Strings      : Array[0..5] of Core.Strings.VarString=('POP3','From Address','Reply To','Remove Address','Complaints','Bounce Address');
  UA_TYPES_Descriptions : Array[0..5] of Core.Strings.VarString=
  ( 'Use typical POP3 or Web to access messages',
    'Use in Campaign as From: Address',
    'Use in Campaign as Reply-To: Address',
    'Use in Campaign MailTo:Remove Address',
    'Use in Campaign and Forward Complaints to this account',
    'Use in Campaign as Return-Path: Address'
  );

  Var
    SPAM_FILTER_SUBJECTS              : Core.Strings.VarString;
    SPAM_FILTER_BODY                  : Core.Strings.VarString;
    SPAM_FILTER_SERVICE               : Core.Strings.VarString;
    SPAM_FILTER_WHITELIST             : Core.Strings.VarString;
    SPAM_FILTER_BLACKLIST             : Core.Strings.VarString;

implementation

end.


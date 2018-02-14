unit Core.Database.Types;

interface

uses
  Classes,
  Core.Generics,
  Core.Timer,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  DB,
  sqldb;

Const
  SQL_LONGVARBINARY=(-4);
  SQL_BIGINT=(-5);
  SQL_TINYINT=(-6);
  SQL_BIT=(-7);
  SQL_TEXT=(-1);
  SQL_LONGVARCHAR=(-1);
  SQL_UNKNOWN_TYPE=0;
  SQL_CHAR=1;
  SQL_NUMERIC=2;
  SQL_DECIMAL=3;
  SQL_INTEGER=4;
  SQL_SMALLINT=5;
  SQL_FLOAT=6;
  SQL_REAL=7;
  SQL_DOUBLE=8;
  SQL_DATETIME=9;
  SQL_VARCHAR=12;

  { One-parameter smallintcuts for date/time data types }
  SQL_TYPE_DATE=91;
  SQL_TYPE_TIME=92;
  SQL_TYPE_TIMESTAMP=93;

  { Statement attribute values for cursor sensitivity }
  Singleton                      : LongInt = 1;
  SQL_UNSPECIFIED=0;
  SQL_INSENSITIVE                = 1;
  SQL_SENSITIVE                  = 2;
  SQL_ALL_TYPES                  = 0;
  SQL_DEFAULT                    = 99;
  cfNone                         = 0;
  cfNotNull                      = 1;
  cfUnique                       = 2;
  cfPrimaryKey                   = 4;
  cfIdentity                     = 8;

  TTL_BACKGROUND_CONNECTIONS     = 60*10; // Disconnect at 10 Minute Intervals

  MD5DigestSize                  = 16;

Type
  FieldType=(dftUnknown,dftBoolean,dftByte,dftWord,dftInteger,dftInt64,dftQWord,dftInt64Array,dftQWordArray,dftDateTime,dftFloat,dftDouble,dftMD5Digest,dftSmallString,dftString,dftMemo,dftByteBuffer);

  Driver=(MySQL51,Oracle,PostgreSQL);
  PDriver=^Driver;
  Drivers=Array[Driver] of Core.Strings.VarString;
  PDrivers=^Drivers;

  TFields=DB.TFields;

  SQLPreOperator=(poNone,poAND,poOR);
  SQLOperator=(oNone,oEqual,oNotEqual,oGreaterThan,oLessThan,oGreaterThanEqualTo,oLessThanEqualTo,oBetween,oLike,oILike,oNotNull,oIN,oDataLength,oIdentity,oOpenBracket,oCloseBracket,oAscending,oDescending);

  Bool=System.Boolean;
  PBool=^Bool;
  Byte=System.Byte;
  PByte=^Byte;
  Integer=System.LongInt;
  Word=System.Word;
  PWord=^Word;
  PInteger=^Integer;
  LargeInt=System.Int64;
  PLargeInt=^LargeInt;
  LargeWord=System.QWord;
  PLargeWord=^LargeWord;

  LargeIntArray=Core.Arrays.Types.LargeInt;
  LargeWordArray=Core.Arrays.Types.LargeWord;
  PLargeIntArray=^LargeIntArray;
  PLargeWordArray=^LargeWordArray;

  DateTime=System.Double;
  PDateTime=^DateTime;
  Float=System.Single;
  PFloat=^Float;
  Double=System.Double;
  PDouble=^Double;
  SmallString=Core.Strings.VarString;
  PSmallString=^SmallString;
  MD5Digest=Core.Arrays.Types.MD5Digest;
  PMD5Digest=^MD5Digest;
  Memo=Core.Strings.VarString;
  PMemo=^Memo;
  PVarString=^Core.Strings.VarString;
  VarString=Core.Strings.VarString;
  Buffer=Core.Arrays.Types.Bytes;
  PBuffer=^Buffer;

  PCommandParameters=^CommandParameters;

  CommandUse=(
    useForCriteria,useForFields,useAsTable,
    useForView,useForValues,useForUpdates,
    useForInsert,useForPrimaryID,
    useForResetInsertID,useForOrderBy,
    useForDataLength,useForCriteriaBracket,
    useForLimit,useForIncrement,useForDecrement
  );
  CommandUses=Set of CommandUse;
  PFields=^Fields;
  PField=^Field;

  PTable=^Table;

  PCommand=^Command;
  PCommands=^Commands;


  // Use in conjunction with Table.Field to update/delete/insert etc.
  Command=Record
    Operation                 : Core.Strings.VarString;
    Useage                    : CommandUse;
    DataType                  : FieldType;
    PropertyID                : LongInt;
    Value                     : Pointer;
    SQLPreOperator            : SQLPreOperator;
    SQLOperator               : SQLOperator;
  end;
  Commands=Array of Command;
  CommandParameter=Record
    DataType                  : FieldType;
    Value                     : Pointer;
    Index                     : LongInt;
    Size                      : LongInt;
    Location                  : LongInt;
  end;
  CommandParameters=Array of CommandParameter;
  Field=Record
    IDP                       : PInteger;
    KeyP                      : Core.Strings.PVarString;
    DataType                  : FieldType;
    AutoCreate                : Boolean;
    Verified                  : Boolean;
    Precision                 : LongInt;
    Flags                     : LongInt;
  end;
  Fields=Array of Field;
  TableIni=record
    AutoCreate                : Boolean;
    AutoCommit                : Boolean;
    Group                     : Core.Strings.VarString;
    Name                      : Core.Strings.VarString;
    Value                     : Core.Strings.VarString;
    Hint                      : Core.Strings.VarString;
    PrimaryKeyP               : Core.Strings.PVarString;
  end;
  PTableIni=^TableIni;
  TTask=Class;
  THeader=class;

  ExceptionEvent=Procedure(sModule,sLocation,sTable,sTask,sMessage:VarString) of Object;
  ConnectionEvent=procedure(Task:TTask) of Object;
  Table=Record
    Name                      : Core.Strings.VarString;
    StartupP                  : PTableIni;
    Fields                    : Fields;
  end;
  TTaskList=specialize GObjectThreadList<TTask>;

  THeader=Class(TObject)
  private
    FOnException                 : ExceptionEvent;
    FItems                       : TTaskList;
  public
    Mode                         : Core.Database.Types.Driver;
    Username                     : Core.Strings.VarString;
    Password                     : Core.Strings.VarString;
    Schema                       : Core.Strings.VarString;
    HostName                     : Core.Strings.VarString;
    ModuleName                   : Core.Strings.VarString;
    ServiceName                  : Core.Strings.VarString;
    DomainName                   : Core.Strings.VarString;
    Encoding                     : Core.Strings.VarString;
  public
    Constructor Create(aMode:Core.Database.Types.Driver; aUser,aPassword,aSchema,aHost,aModule,aEncoding:Core.Strings.VarString; aExceptionEvent:ExceptionEvent); ReIntroduce; Virtual;
    Destructor  Destroy(); override;
  published
    property    OnException : ExceptionEvent read FOnException write FOnException;
  end;

  TTask=Class(TObject)
  private
    FOwner                       : THeader;
  private
    FQuery                       : TSQLQuery;
    FConnection                  : TSQLConnection;
    FTransaction                 : TSQLTransaction;
    FName                        : Core.Strings.VarString;
    FOnConnected                 : ConnectionEvent;
    FOnDisconnected              : ConnectionEvent;
    FLastUsed                    : TDateTime;
  protected
    procedure cbAfterConnection(Sender:TObject);
    procedure cbAfterDisconnection(Sender:TObject);
  public
    TransactionLock              : boolean;
  public
    Constructor Create(aOwner:THeader; aName:Core.Strings.VarString); virtual;
    Destructor  Destroy(); override;
  published
    property Connection : TSQLConnection read FConnection;
    property Transaction: TSQLTransaction read FTransaction;
    property Query : TSQLQuery read FQuery;
    property Header: THeader read FOwner;
    property Name: Core.Strings.VarString read FName write FName;
    property LastUsed: TDateTime read FLastUsed write FLastUsed;
  published
    property OnConnected : ConnectionEvent read FOnConnected write FOnConnected;
    property OnDisconnected  : ConnectionEvent read FOnDisconnected write FOnDisconnected;
  end;

  TTimer=Class(TTimerThread)
  private
    dtConnected                  : TDateTime;
    dtExpires                    : TDateTime;
    FHeader                      : THeader;
    FTask                        : TTask;
  protected
    procedure DoBeforeProcess(); override;
  public
    Constructor Create(aHeader:THeader; aPriority:TThreadPriority); ReIntroduce;
    Destructor Destroy; Override;
  published
    property Task: TTask read FTask;
    property Header:THeader read FHeader;
  end;



  Tables=Array of Table;
  PTables=^Tables;

  Callback=Procedure(CommandsP:PCommands; Fields:TFields; Const DataP:System.Pointer);
  Event=Procedure(CommandsP:PCommands; Fields:TFields; Const DataP:System.Pointer) of Object;

Const

  DB_FIELD_TYPE:Array[FieldType] of TFieldType=(
    ftUnknown                   , // dftUnknown
    ftBoolean                   , // dftBoolean
    ftSmallint                  , // dftByte
    ftInteger                   , // dftWord
    ftInteger                   , // dftInteger
    ftLargeint                  , // dftInt64
    ftLargeint                  , // dftQword
    ftString                    , // dftInt64Array
    ftString                    , // dftQwordArray
    ftDateTime                  , // dftDateTime
    ftFloat                     , // dftFloat
    ftFloat                     , // dftDouble
    ftString                    , // dftMD5Digest
    ftString                    , // dftSmallString
    ftString                    , // dftString
    ftString                    , // dftMemo
    ftString                     // dftByteBuffer
  );
  DB_FIELD:Array[FieldType] of Core.Strings.VarString=(
    'dftUnknown',
    'dftBoolean',
    'dftByte',
    'dftWord',
    'dftInteger',
    'dftInt64',
    'dftQword',
    'dftInt64Array',
    'dftQWordArray',
    'dftDateTime',
    'dftFloat',
    'dftDouble',
    'dftMD5Digest',
    'dftSmallString',
    'dftString',
    'dftMemo',
    'dftByteBuffer'
  );

  PROPERTY_ID_VOID               = -1;
  DATATYPE_VOID                  = dftUnknown;
  VALUE_VOID                     = NIL;
  QWORD_VALUE_NULL               : QWord = 0;
  DOUBLE_VALUE_NULL              : Double = 0.0;

  TABLE_NULL                     = '';
  FIELD_NULL                     = '';
  QUOTE                          = #39;

  DB_MODES:Drivers=('MySQL 5.1','Oracle','PostgreSQL');

  FMT_INVALID_TYPE='Index %s is an invalid DataType or Datatype Mismatch: %s was supposd to be %s';
  DB_EXCEPTION_MESSAGE_BODY      = #13#10'Module: %s'#13#10'Location: %s'#13#10#13#10'%s'#13#10;

  AUTOCREATE_ON                  = True;
  AUTOCREATE_OFF                 = False;

  SQL_EXECUTE_EXCEPTION_MESSAGE  = 'SQL Statement:'#13#10'%s'#13#10#13#10'Notice:'#13#10'%s'#13#10;
  SQL_EXCEPTION_MESSAGE          = 'An exception message was generated that needs your attention.'#13#10#13#10+'The exception was encountered during %s.'#13#10#13#10'Exception: %s'#13#10;
  SQL_COMMIT_EXCEPTION_MESSAGE   = #13#10'COMMIT FAILED'#13#10'Notice: %s'#13#10;
  SQL_DATALENGTH_EXCEPTION_MESSAGE = #13#10'SQL Statement= %s'#13#10'Notice: %s'#13#10;
  SQL_CRITERIA_EXCEPTION_MESSAGE  = #13#10'Function: %s'#13#10'Table: %s'#13#10'Operation: %s'#13#10'PropertyID: %s'#13#10'Notice: %s'#13#10;
  SQL_KEEP_ALIVE_STATEMENT        = 'SELECT COUNT(*) FROM %s';
  SQL_SELECT_STATEMENT           = 'SELECT $FIELDS$ FROM $TABLE$ WHERE $CRITERIA$ $ORDERBY$ $LIMIT$;';
  SQL_COUNT_STATEMENT            = 'SELECT COUNT(*) FROM $TABLE$ WHERE $CRITERIA$;';
  SQL_EXISTS_STATEMENT           = 'SELECT $FIELDS$ FROM $TABLE$ WHERE EXISTS $CRITERIA$;';
  //SQL_SUM_STATEMENT              = 'SELECT SUM(CAST($FIELD$ AS BIGINT)) AS length FROM $TABLE$ WHERE $CRITERIA$;';
  //SQL_DATALENGTH_STATEMENT       = 'SELECT DATALENGTH($FIELDS$) as length FROM $TABLE$ WHERE $CRITERIA$;';
  //SQL_SUMDATALENGTH_STATEMENT    = 'SELECT SUM(DATALENGTH($FIELD$)) as length FROM $TABLE$ WHERE $CRITERIA$;';
  SQL_LIMIT                      = 'LIMIT $LIMIT$';
  SQL_DROPVIEW_STATEMENT         = 'DROP VIEW $VIEW$;';
  SQL_ORDER_BY                   = ' ORDER BY $FIELD$ $BY$';
  SQL_DELETE_STATEMENT           = 'DELETE FROM $TABLE$ WHERE $CRITERIA$;';
  SQL_RESET_INSERT_ID_STATEMENT  = 'UPDATE $TABLE$ SET $INSERTID$=:PARAM_1_ WHERE $PRIMARYID$=:PARAM_2_';
  SQL_INSERT_STATEMENT           = 'INSERT INTO $TABLE$ ($FIELDS$) VALUES ($VALUES$);';
  SQL_UPDATE_STATEMENT           = 'UPDATE $TABLE$ SET $UPDATES$ WHERE $CRITERIA$;';
  SQL_SELECT_PRIMARYID           = 'SELECT $ITEM_ID$ FROM $TABLE$ WHERE $CRITERIA$;';
  SQL_SELECT_CRITERIA            = '$PREOPERATOR$$OPERATION$$OPERATOR$';

  CHAR_ENCODING:Core.Strings.VarString=
    'BIG5,EUC_CN,EUC_JP,EUC_JIS_2004,EUC_KR,EUC_TW,GB18030,GBK,ISO_8859_5,ISO_8859_6,ISO_8859_7,ISO_8859_8,'+
    'JOHAB,KOI8R,KOI8U,LATIN1,LATIN2,LATIN3,LATIN4,LATIN5,LATIN6,LATIN7,LATIN8,LATIN9,LATIN10,MULE_INTERNAL,'+
    'SJIS,SHIFT_JIS_2004,SQL_ASCII,UHC,UTF-8,WIN866,WIN874,WIN1250,WIN1251,WIN1252,WIN1253,WIN1254,WIN1255,'+
    'WIN1256,WIN1257,WIN1258';

  //SQL_CREATEVIEW_STATEMENT       = 'CREATE VIEW $VIEW$ ($FIELDS$) AS SELECT $NEWFIELDS$ FROM $TABLE$ WHERE $CRITERIA$;';
  //SQL_CREATEVIEW_MYSQL_STATEMENT       = 'CREATE OR REPLACE VIEW $VIEW$ ($FIELDS$) AS SELECT $NEWFIELDS$ FROM $TABLE$ WHERE $CRITERIA$;';
  FMT_TASK_NAME : Core.Strings.VarString = ('Sytem Task for connection awareness for %s');

  FMT_PARAM_FIELD='PARAM_%s_';
  FMT_VARCHAR_FIELD_PRECISION='VARCHAR(%s)';
  FMT_VARCHAR_FIELD_NO_PRECISION='TEXT';
  FMT_VARCHAR_FIELD:Array[Boolean] of Core.Strings.VarString = (FMT_VARCHAR_FIELD_NO_PRECISION,FMT_VARCHAR_FIELD_PRECISION);
  FMT_TABLE_DUPLICATED_EXCEPTION='Cannot create another table with this name :%s';

  // CREATE TABLE OPTIONS
  SQL_CREATE_TABLE_STATEMENT           = 'CREATE TABLE $TABLE$ ($FIELDS$);';
  SQL_CREATE_TABLE_FIELD_STATEMENT     = '$NAME$ $KIND$ $NOT_NULL$ $UNIQUE$ $AUTO_INCREMENT$ $PRIMARY_KEY$';
  SQL_CREATE_TABLE_PRIMARY_KEY         = 'PRIMARY KEY ($NAME$)';

  SQL_BIG_INT:Array[Driver] of Core.Strings.VarString=(
    {dbmMySQL51    } 'BIGINT',
    {dbmOracle     } 'NUMBER(19)',
    {dbmPostgreSQL } 'BIGINT'
  );
  SQL_CREATE_VIEW:Array[Core.Database.Types.Driver] of Core.Strings.VarString=(
   'CREATE OR REPLACE VIEW $VIEW$ ($FIELDS$) AS SELECT $NEWFIELDS$ FROM $TABLE$ WHERE $CRITERIA$;',
   'CREATE OR REPLACE VIEW $VIEW$ ($FIELDS$) AS SELECT $NEWFIELDS$ FROM $TABLE$ WHERE $CRITERIA$;',
   'CREATE OR REPLACE VIEW $VIEW$ ($FIELDS$) AS SELECT $NEWFIELDS$ FROM $TABLE$ WHERE $CRITERIA$;'
  );
  SQL_LENGTH:Array[Core.Database.Types.Driver] of Core.Strings.VarString =(
    'select length($FIELD$) as res_len FROM $TABLE$ WHERE $CRITERIA$;',
    'select length($FIELD$) as res_len FROM $TABLE$ WHERE $CRITERIA$;',
    'select length($FIELD$) as res_len FROM $TABLE$ WHERE $CRITERIA$;'
  );
  SQL_SUM_LENGTH:Array[Core.Database.Types.Driver] of Core.Strings.VarString =(
    'select sum(length($FIELD$)) as res_len FROM $TABLE$ WHERE $CRITERIA$;',
    'select sum(length($FIELD$)) as res_len FROM $TABLE$ WHERE $CRITERIA$;',
    'select sum(length($FIELD$)) as res_len FROM $TABLE$ WHERE $CRITERIA$;'
  );
  SQL_SUM:Array[Core.Database.Types.Driver] of Core.Strings.VarString =(
    'select sum($FIELD$)::$TYPE$ as res_len FROM $TABLE$ WHERE $CRITERIA$;',
    'select sum($FIELD$)::$TYPE$ as res_len FROM $TABLE$ WHERE $CRITERIA$;',
    'select sum($FIELD$)::$TYPE$ as res_len FROM $TABLE$ WHERE $CRITERIA$;'
  );
  SQL_LIST_TABLES:Array[Core.Database.Types.Driver] of Core.Strings.VarString =(
    'select table_name from INFORMATION_SCHEMA.TABLES where table_schema='+QUOTE+'%s'+QUOTE+';',
    'select table_name from information_schema.tables where table_catalog='+QUOTE+'%s'+QUOTE+' and table_schema='+QUOTE+'public'+QUOTE+' and table_type='+QUOTE+'BASE TABLE'+QUOTE+';',
    'select table_name from information_schema.tables where table_catalog='+QUOTE+'%s'+QUOTE+' and table_schema='+QUOTE+'public'+QUOTE+' and table_type='+QUOTE+'BASE TABLE'+QUOTE+';'
  );
  SQL_LIST_COLUMNS:Array[Core.Database.Types.Driver] of Core.Strings.VarString =(
    'select column_name from INFORMATION_SCHEMA.COLUMNS where table_schema='+QUOTE+'%s'+QUOTE+' and table_name='+QUOTE+'%s'+QUOTE+';',
    'select column_name from information_schema.columns where table_catalog='+QUOTE+'%s'+QUOTE+' and table_name='+QUOTE+'%s'+QUOTE+';',
    'select column_name from information_schema.columns where table_catalog='+QUOTE+'%s'+QUOTE+' and table_name='+QUOTE+'%s'+QUOTE+';'
  );
  AUTO_INCREMENT:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'AUTO_INCREMENT', //dbmMySQL
    'AUTO_INCREMENT', //dbmOracle
    ''                //dbmPostgreSQL
  );
  BYTE_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'TINYINT(1) UNSIGNED', //dbmMySQL
    'TINYINT(1) UNSIGNED', //dbmOracle
    'SMALLINT'             //dbmPostgreSQL
  );
  DOUBLE_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'DOUBLE',              //dbmMySQL
    'DOUBLE',              //dbmOracle
    'DOUBLE PRECISION'     //dbmPostgreSQL
  );
  SMALLSTRING_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'VARCHAR(255)',              //dbmMySQL
    'VARCHAR(255)',              //dbmOracle
    'VARCHAR(255)'               //dbmPostgreSQL
  );
  LONG_TEXT_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'LONGTEXT',              //dbmMySQL
    'LONGTEXT',              //dbmOracle
    'TEXT'                   //dbmPostgreSQL
  );
  SMALL_BLOB_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'TINYBLOB',              //dbmMySQL
    'SMALLBLOB',              //dbmOracle
    'BYTEA'                   //dbmPostgreSQL
  );
  MEDIUM_BLOB_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'MEDIUMBLOB',              //dbmMySQL
    'MEDIUMBLOB',              //dbmOracle
    'BYTEA'                   //dbmPostgreSQL
  );
  LARGE_BLOB_FIELD:Array[Core.Database.Types.Driver] of Core.Strings.VarString = (
    'LONGBLOB',              //dbmMySQL
    'LONGBLOB',              //dbmOracle
    'BYTEA'                   //dbmPostgreSQL
  );


  // ALTER TABLE OPTIONS
  SQL_ALTER_TABLE_STATEMENT            = 'ALTER TABLE $TABLE$ $FIELDS$;';
  // You can add multiple specification per statement (at least in MySQL)
  SQL_ALTER_ADD_FIELD                  = 'ADD $FIELD$';
  SQL_ALTER_DROP_FIELD                 = 'DROP $NAME$';

  SQL_PREOPERATOR:ARRAY[SQLPreOperator] of Core.Strings.VarString=
  (
    '',
    ' AND ',
    ' OR '
  );
  SQL_OPERATOR:Array[SQLOperator] of Core.Strings.VarString=
  (
    '',
    '=:PARAM_$PARAMETER$_',
    '<>:PARAM_$PARAMETER$_',
    '>:PARAM_$PARAMETER$_',
    '<:PARAM_$PARAMETER$_',
    '>=:PARAM_$PARAMETER$_',
    '<=:PARAM_$PARAMETER$_',
    ' BETWEEN :PARAM_$PARAMETER$_',
    ' LIKE :PARAM_$PARAMETER$_',
    ' ILIKE :PARAM_$PARAMETER$_',
    ' IS NOT NULL',
    ' IN(:PARAM_$PARAMETER$_)',
    'DATALENGTH(:PARAM_$PARAMETER$_)',
    'IDENTITY(int, 1,1) as :PARAM_$PARAMETER$_)',
    '(',
    ')',
    'ASC',
    'DESC'
  );

  SQL_DATALENGTH_FIELD='res_len';
  SQL_MYSQL_DATALENGTH_STR='LENGTH';
  SQL_DATALENGTH_STR='DATALENGTH';

  SQL_DATALENGTH_STRING:Array[Core.Database.Types.Driver] of Core.Strings.VarString=('LENGTH','DATALENGTH','DATALENGTH');

  INI_DB_LOGIN_HOST='Host';
  INI_DB_LOGIN_USER='User';
  INI_DB_LOGIN_PASSWORD='Password';
  INI_DB_LOGIN_SCHEMA='Schema';
  INI_DB_LOGIN_ENCODING='Encoding';
  INI_DB_LOGIN_MODE='Mode';

  INI_DB_SECT_LOGIN='Login';
  INI_DB_SECT_TABLES='Tables';


  TIMEOUT_SQL_SERVER_SECONDS = 60 * 2;

  KEEP_ALIVE_OFF             = false;
  KEEP_ALIVE_ON              = true;

  None                           : Pointer = nil;
  NoKey                          = nil;
implementation
uses
  pqconnection,
  mysql51conn,
  OracleConnection,
  DateUtils;


Constructor THeader.Create(aMode:Core.Database.Types.Driver; aUser,aPassword,aSchema,aHost,aModule,aEncoding:Core.Strings.VarString; aExceptionEvent:ExceptionEvent);
begin
  Mode:=aMode;
  UserName:=aUser;
  Password:=aPassword;
  Schema:=aSchema;
  HostName:=aHost;
  ModuleName:=aModule;
  Encoding:=aEncoding;
  FOnException:=aExceptionEvent;
  FItems:=TTaskList.Create(Core.Generics.Defaults.KeepOnClear);

  Inherited Create();
end;

Destructor  THeader.Destroy();
begin
  FItems.Free();
  Inherited Destroy();
end;

procedure TTask.cbAfterConnection(Sender:TObject);
begin
  if Assigned(FOnConnected) then
    Try FOnConnected(Self); Except End;
end;

procedure TTask.cbAfterDisconnection(Sender:TObject);
begin
  if Assigned(FOnConnected) then
    Try FOnConnected(Self); Except End;
end;

Constructor TTask.Create(aOwner:THeader; aName:Core.Strings.VarString);
begin
  FOwner:=AOwner;
  FName:=aName;

  Inherited Create();

  FConnection:=nil;

  Case FOwner.Mode of
    PostgreSQL : FConnection:=TPQConnection.Create(Nil);
    MySQL51    : FConnection:=TMYSQL51Connection.Create(Nil);
    Oracle     : FConnection:=TOracleConnection.Create(Nil);
  end;
  FConnection.KeepConnection:=true;
  FConnection.LoginPrompt:=false;

  FConnection.CharSet:=FOwner.Encoding;
  FConnection.UserName:=FOwner.Username;
  FConnection.Password:=FOwner.Password;
  FConnection.HostName:=FOwner.HostName;
  FConnection.DatabaseName:=FOwner.Schema;
  FConnection.AfterConnect:=@cbAfterConnection;
  FConnection.AfterDisconnect:=@cbAfterDisconnection;


  FTransaction:=TSQLTransaction.Create(nil);
  FQuery:=TSQLQuery.Create(Nil);
  FQuery.Database:=FConnection;
  FQuery.Transaction:=FTransaction;
  FConnection.Transaction:=FTransaction;

  FOwner.FItems.Add(Self);
end;

Destructor  TTask.Destroy();
begin

  FQuery.Free();
  FConnection.Free();
  FTransaction.Free();

  FOwner.FItems.Remove(Self);

  FOwner:=nil;

  Inherited Destroy;
end;


Constructor TTimer.Create(aHeader:THeader; aPriority:TThreadPriority);
begin

  Inherited Create(aPriority);

  dtConnected:=0;
  dtExpires:=0;

  FTask:=TTask.Create(aHeader,'Core.Database.Types.TTimer.Background');
end;

Destructor TTimer.Destroy;
begin
  FTask.Free();
  Inherited Destroy;
end;

procedure TTimer.DoBeforeProcess();
begin
  (*
  if (Core.Timer.dtNow>dtExpires) then begin
    dtExpires:=DateUtils.IncSecond(Core.Timer.dtNow,FTimeout);
    uDatabase.Disconnect(Module.SessionID);
    uDatabase.Connect(Module.SessionID);
    dtConnected:=uTimer.dtNow;
  end;
  *)
end;



end.


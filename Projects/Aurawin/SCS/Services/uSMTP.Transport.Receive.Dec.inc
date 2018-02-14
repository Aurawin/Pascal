  TTransportReceive=class(TTransportBase)
  private
    FHacker              : Boolean;
    FIntruder            : Boolean;
    FViolator            : Boolean;
    FMailP               : Storage.UserStorage.Items.SMTP.PRecvMessage;
    FDeliveryP           : Storage.UserStorage.Items.SMTP.PDelivery;
    FRelayMailP          : Storage.UserStorage.Items.SMTP.PRelayMail;
    FsrP                 : PFileStorageRequest;
    FRelayService        : Storage.MatrixServices.Items.Item;
    FRelayRSRP           : PRSR;
    USMTPRR              : TUserSMTPRelayRequest;
    SR                   : TFileStorageRequest;
    FUserName            : Core.Strings.VarString;
    FRealm               : Core.Strings.VarString;
    FAuth                : Core.Strings.VarString;
    FPassword            : Core.Strings.VarString;
    FResponse            : Core.Strings.VarString;
    FHeaders             : Core.Strings.VarString;
    FlRDomain            : Core.Strings.VarString;
    RUser                : Core.Strings.VarString;
    RDomain              : Core.Strings.VarString;
    FTLD                 : Core.Strings.VarString;
    Recip                : Core.Strings.VarString;
    RecipU               : Core.Strings.VarString;
    InputN               : Core.Strings.VarString;
    FContentType         : Core.Strings.VarString;
    FMTALoop             : Core.Strings.VarString;

    dgA1                 : TMD5Digest;
    dgA2                 : TMD5Digest;
    dgA1Pre              : TMD5Digest;
    dgA2Pre              : TMD5Digest;
    dgCheck              : TMD5Digest;


    HA1                  : Core.Strings.VarString;
    HA2                  : Core.Strings.VarString;
    A1                   : Core.Strings.VarString;
    A2                   : Core.Strings.VarString;
    FStamp               : Core.Strings.VarString;
    FH                   : Core.Strings.VarString;
    FNonce               : Core.Strings.VarString;
    FCNonce              : Core.Strings.VarString;
    FNC                  : Core.Strings.VarString;
    FQOP                 : Core.Strings.VarString;
    FAuthZID             : Core.Strings.VarString;
    FURI                 : Core.Strings.VarString;
    FCheck               : Core.Strings.VarString;
    FMeta                : Core.Strings.VarString;
    FData                : Core.Strings.VarString;
    FIntrusions          : QWord;
    FQueueID             : QWord;
    FTTL                 : Double;

    FsaParams            : Core.Arrays.Types.VarString;
    FAuthData            : Core.Arrays.Types.KeyStrings;

    HeloInetAddr         : QWord;
    FRelayNodeId         : QWord;
    FFolderID            : QWord;
    FFileID              : QWord;
    FConnectionFilter    : Storage.Security.Filter.Item;
    FLength              : Int64;
    FEndOfMessage        : Int64;
  private
    procedure   ProcessCredentials();
    procedure   PushError();
    procedure   ValidateCredentials();
    procedure   InvalidateCredentials();
    procedure   ClearRecipients(var List:Core.Arrays.Types.KeyStrings);
    procedure   ProcessDataLine();
    procedure   RelayMessage(var Service:Storage.MatrixServices.Items.Item; Recipient:Core.Strings.VarString; var Summary:Storage.UserStorage.Items.SMTP.TSummary; var Mail:Storage.UserStorage.Items.SMTP.TRecvMessage);
    function    ExtractDomain (var Recipient:Core.Strings.VarString):Core.Strings.VarString;
    function    CMDToFrom  (CMD:Core.Strings.VarString):Boolean;
    procedure   CMDToRecipient(var Input:Core.Strings.VarString);
  protected
    procedure OnInitialized(RSRP:PRSR); override;
    procedure OnQueued(RSRP:PRSR); override;
    procedure OnConnected(RSRP:PRSR); override;
    procedure OnDisconnected(RSRP:PRSR); override;
    procedure OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
    procedure OnDNSResult(RSRP:PRSR); override;
    procedure OnFinalized(RSRP:PRSR); override;
    procedure OnErrored(RSRP:PRSR); override;
  end;


FAntiSpamLock        : TRTLCriticalSection;
TI_RootUser          : Core.Timer.Item;
TI_Filters           : Core.Timer.Item;
FManagers            : TRSRManagers;
FManager             : TMailManager;
FMatrixNode          : Storage.MatrixNodes.Node.Item;

RootDomain           : Storage.Domains.Items.TDomain;
RootUA               : Storage.UserAccounts.Items.Item;
FFilters             : Storage.Security.Filter.Manifest;
RelayLcv             : LongInt;
FDisposePtr          : Boolean;
FRelayMailP          : Storage.UserStorage.Items.SMTP.PRelayMail;
FRelayRSRP           : RSR.PRSR;
saDNSBlackLists      : Core.Arrays.Types.VarString;

FXMLParser           : TDOMParser;
FXMLDocument         : TXMLDocument;
FXMLSource           : TXMLInputSource;
FNode                : TDOMNode;


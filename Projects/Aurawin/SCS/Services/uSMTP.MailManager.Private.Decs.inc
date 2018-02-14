FUserAccounts        : Storage.UserAccounts.Items.TList;
Owner                : TReceiveMailServer;
dtNextAntiSpamUpdate : TDateTime;
FResponse            : Core.Strings.VarString;


FTransportRecv       : TTransportReceive;
FTransportRelay      : TTransportRelay;
FXMLParser           : TDOMParser;


saDNSBlackLists      : Core.Arrays.Types.VarString;
saNoFilters          : Core.Arrays.Types.VarString;
smNoFilters          : Core.Arrays.Types.StringManifest;
NoProfiles           : Storage.Security.Filter.Items;
NoPhrases            : Storage.Security.Filter.Items;
ContentProfiles      : Storage.Security.Filter.Items;
ContentPhrases       : Storage.Security.Filter.Items;
WhiteList            : Storage.Security.Filter.Items;
BlackList            : Storage.Security.Filter.Items;
FSummary             : Storage.UserStorage.Items.SMTP.TSummary;


procedure   CopyAntiSpamData;


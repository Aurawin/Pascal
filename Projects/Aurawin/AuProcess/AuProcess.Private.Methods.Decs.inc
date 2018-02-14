{$ifndef RSR_IMAP_ONLY}
procedure LoadSearchProviders;
{$endif}
function  SetupStorage:boolean;
procedure SetupDNS;
procedure LoadCoreObjects;
procedure LoadContentTypes;
function  SetupMatrix:boolean;
procedure SetupParams;
procedure SetupTimers;
procedure SetupDomain;
procedure InitializeNodeConfig;
procedure OnTimer_Ping(ItemP:Core.Timer.PItem);

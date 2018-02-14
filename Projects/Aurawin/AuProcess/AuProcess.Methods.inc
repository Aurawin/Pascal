{$i AuProcess.Method.SetupStorage.inc}
{$i AuProcess.Method.SetupDNS.inc}
{$i AuProcess.Method.LoadCoreObjects.inc}
{$i AuProcess.Method.OnDBMSConnected.inc}
{$i AuProcess.Method.OnDBMSDisconnected.inc}
{$ifdef RSR_IMAP_ONLY}
  {$i AuProcess.Method.StartIMAP.inc}
  {$i AuProcess.Method.StartIMAPS.inc}
{$else}
  {$i AuProcess.Method.StartSMTP.inc}
  {$i AuProcess.Method.StartSMTPS.inc}
  {$i AuProcess.Method.StartSMTPSO.inc}
  {$i AuProcess.Method.StartPOP3.inc}
  {$i AuProcess.Method.StartPOP3S.inc}
  {$i AuProcess.Method.StartIMAP.inc}
  {$i AuProcess.Method.StartIMAPS.inc}
  {$i AuProcess.Method.StartXMPPCToS.inc}
  {$i AuProcess.Method.StartXMPPSToS.inc}
  {$i AuProcess.Method.StartHTTP.inc}
  {$i AuProcess.Method.StartHTTPS.inc}
  {$i AuProcess.Method.LoadSearchProviders.inc}
{$endif}
{$i AuProcess.Method.SetupMatrix.inc}
{$i AuProcess.Method.SetupDomain.inc}
{$i AuProcess.Method.SetupParams.inc}
{$i AuProcess.Method.LoadContentTypes.inc}

{$i AuProcess.Method.SetupTimers.inc}

{$i AuProcess.Method.StartServer.inc}
{$i AuProcess.Method.OnDBMSException.inc}
{$i AuProcess.Method.InitializeNodeConfig.inc}

{$i AuProcess.Method.OnServerListening.inc}
{$i AuProcess.Method.OnServerEngineFailure.inc}
{$i AuProcess.ProcessException.inc}


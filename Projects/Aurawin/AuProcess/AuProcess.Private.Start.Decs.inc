procedure StartServer;
{$ifdef RSR_IMAP_ONLY}
  function  StartIMAP:TRSRServer;
  function  StartIMAPS:TRSRServer;
{$else}
  function  StartSMTP:TRSRServer;
  function  StartSMTPS:TRSRServer;
  function  StartSMTPSO:TRSRServer;
  function  StartPOP3:TRSRServer;
  function  StartPOP3S:TRSRServer;
  function  StartIMAP:TRSRServer;
  function  StartIMAPS:TRSRServer;
  function  StartHTTP:TRSRServer;
  function  StartHTTPS:TRSRServer;
  function  StartXMPPCToS:TRSRServer;
  function  StartXMPPSToS:TRSRServer;
{$endif}

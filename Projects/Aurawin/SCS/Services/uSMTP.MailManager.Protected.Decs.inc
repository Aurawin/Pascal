procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
procedure   OnError(RSRP:PRSR); override;

procedure   OnDisconnect(RSRP:PRSR); override;
procedure   OnConnect(RSRP:PRSR); override;
procedure   OnDataReceived(RSRP:PRSR; Var Handled:Boolean); override;
procedure   OnQueue(RSRP:PRSR); override;
procedure   OnDNSResult(RSRP:PRSR); override;
procedure   OnInitialize(RSRP:PRSR); override;
procedure   OnFinalize(RSRP:PRSR); override;


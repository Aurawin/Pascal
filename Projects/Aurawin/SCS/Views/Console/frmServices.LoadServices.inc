Storage.Domains.Items.DB.List(Storage.Main.Task,FDomains);
cbDomains.Items.Text:=Core.Arrays.VarString.toString(FDomains);
iCount:=Length(FDomains);
SetLength(DebugItems,iCount);
iLcv:=Core.Arrays.VarString.IndexOf(FDomains,DEBUG_DOMAIN);
if iLcv<>-1 then begin
  DebugItems[iLcv].HTTPServer:=THTTPServer.Create(FDomains[iLcv],Storage.NodeID,FNodeIP,PORT_HTTP,1,SSL_OFF);
  DebugItems[iLcv].HTTPServer.Active:=true;
  DebugItems[iLcv].HTTPServer.Start();
end;
sbStatus.SimpleText:=Concat('Debugging services [',Core.Utils.Sockets.InAddrToStr(FNodeIP),']');
if iCount>0 then begin
  cbDomains.ItemIndex:=0;
  cbDomainsChange(cbDomains);
end;

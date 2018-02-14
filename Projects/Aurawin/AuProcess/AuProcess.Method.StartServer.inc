procedure   AuraProcess.StartServer;
begin
  If FService.Service<>nil then exit;
  {$ifdef RSR_IMAP_ONLY}
    case FService.Kind of
      mkIMAP     : FServer:=StartIMAP;
      mkIMAPS    : FServer:=StartIMAPS;
    end;
  {$else}
    case FService.Kind of
      Storage.MatrixServices.Items.mkXMPPCToS : FServer:=StartXMPPCToS;
      Storage.MatrixServices.Items.mkXMPPSToS : FServer:=StartXMPPSToS;
      Storage.MatrixServices.Items.mkPOP3     : FServer:=StartPOP3;
      Storage.MatrixServices.Items.mkPOP3S    : FServer:=StartPOP3S;
      Storage.MatrixServices.Items.mkSMTP     : FServer:=StartSMTP;
      Storage.MatrixServices.Items.mkSMTPS    : FServer:=StartSMTPS;
      Storage.MatrixServices.Items.mkSMTPSO   : FServer:=StartSMTPSO;
      Storage.MatrixServices.Items.mkIMAP     : FServer:=StartIMAP;
      Storage.MatrixServices.Items.mkIMAPS    : FServer:=StartIMAPS;
      Storage.MatrixServices.Items.mkHTTP     : FServer:=StartHTTP;
      Storage.MatrixServices.Items.mkHTTPS    : FServer:=StartHTTPS;
    end;
  {$endif}
  If FServer<>nil then begin
    FServer.Active:=true;
    FServer.WaitFor();
  end;
end;


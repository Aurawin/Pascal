procedure   TTransportRelay.OnConnected(RSRP:PRSR);
begin
  FRelayP:=RSRP^.Info.DataP;
  FRelayP^.State:=RS_WELCOME;
end;

procedure   TTransportRelay.OnErrored(RSRP:PRSR);
begin
  FRelayP:=RSRP^.Info.DataP;
  if (FRelayP<>nil) and (FRelayP^.Sent=false) then begin
    Case FRelayP^.State of
      RS_NONE       : Retry(true); // Message was not accepted
      RS_HELO       : Retry(true); // Message was not accepted
      RS_MailFrom   : Retry(true); // Message was not accepted
      RS_Recip      : Retry(true); // Message was not accepted
      RS_Data       : Retry(true); // Message was not accepted
      RS_Body       : Retry(true); // Message was not accepted
      RS_EndSession : begin end; // Message Has been Relayed
      RS_Lookup_MX  : begin
        Include(RSRP^.Errors,eDNS);
        FMeta:=Concat(
             'TTransportRelay.OnErrored.NS_MX ',
             'NS: ',Core.Utils.Sockets.InAddrToStr(FRelayP^.DNSIP),'; ',
             'Query: ',FRelayP^.DNSQuery,'; ',
             'RSR Error(s):',RSR.RSRErrorToString(RSRP^.Errors)
        );
        FRelayP^.Error:=Concat(FRelayP^.Error,#13#10,FMeta);
        Core.Logging.Native.WriteLogEntry(
           FOwner.Owner.RootDomain.Name,
           FOwner.FService,
           FMeta
        );
        Storage.MatrixServices.Items.DB.GetRandomInstance(FOwner.Task,Storage.MatrixServices.Items.mkSMTP,FOwner.Owner.RootDomain.ID,FRelayService);
        FTTL:=DateUtils.IncSecond(Core.Timer.dtUT,15);
        FMeta:=Storage.UserStorage.Items.SMTP.toXML(FRelayP^,XML_HEADER_ON);
        FNodeID:=FRelayService.NodeID;
        Storage.MatrixQueue.Items.DB.Fail(FOwner.Task,FRelayP^.QueueID,FNodeID,FTTL,FMeta);
        FRelayP^.State:=RS_EndSession;
      end;
      RS_Lookup_IP  : begin
        Include(RSRP^.Errors,eDNS);
        FMeta:=Concat(
             'TTransportRelay.OnErrored.NS_IP ',
             'NS: ',Core.Utils.Sockets.InAddrToStr(FRelayP^.DNSIP),'; ',
             'Query: ',FRelayP^.DNSQuery,'; ',
             'RSR Error(s):',RSR.RSRErrorToString(RSRP^.Errors)
        );
        FRelayP^.Error:=Concat(FRelayP^.Error,#13#10,FMeta);
        Core.Logging.Native.WriteLogEntry(
           FOwner.Owner.RootDomain.Name,
           FOwner.FService,
           FMeta
        );

        FTTL:=DateUtils.IncSecond(Core.Timer.dtUT,15);
        FMeta:=Storage.UserStorage.Items.SMTP.toXML(FRelayP^,XML_HEADER_ON);
        FNodeID:=FRelayService.NodeID;
        Storage.MatrixQueue.Items.DB.Fail(FOwner.Task,FRelayP^.QueueID,FNodeID,FTTL,FMeta);
        FRelayP^.State:=RS_EndSession;
      end;
      RS_EHELO      : Retry(true); // Message was not accepted
      RS_WELCOME    : Retry(true); // Message was not accepted
      RS_CONNECT    : Retry(true); // Message was not accepted
    end;
  end;
end;

procedure   TTransportRelay.OnDisconnected(RSRP:PRSR);
begin
  FRelayP:=RSRP^.Info.DataP;
  if (FRelayP<>nil) and (FRelayP^.State<>RS_EndSession) and (FRelayP^.Sent=false) then
    Retry(true);
end;

procedure   TTransportRelay.OnDataReceived(RSRP:PRSR; var Handled:boolean);

  procedure PushSend;
  begin
    FOwner.Send(RSRP,FResponse);
  end;

  procedure PushBody;
  begin
    FRelayP^.State:=RS_Body;
    FRelayP^.Sent:=True;
    FOwner.Send(RSRP,FRelayP^.Data);
  end;

  procedure PushEndSession;
  begin
    FRelayP^.State:=RS_EndSession;
    FOwner.Close(RSRP);
  end;

  procedure PushQuit;
  begin
    FResponse:='QUIT'#13#10;
    PushSend();
    PushEndSession();
  end;

  procedure PushDelivered;
  begin
    if ( (FRelayP^.Status=354) or (FRelayP^.Status=250) ) then begin
      // FRelayP^.sResponse holds delivery confirmation
      // Confirmation as
      WriteConfirmation();
      PushQuit();
    end else begin
      FRelayP^.Error:=Concat(
        'SMTP Server rejected delivery with message:'#13#10,
        FRelayP^.Response,#13#10
      );
      Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_MX_DIALOG);
      PushQuit();
    end;
    Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
  end;

  procedure PushHelo;
  begin
    FRelayP^.State:=RS_HELO;
    FResponse:=Concat('HELO ',FRelayP^.Helo,#13#10);
    PushSend;
  end;

  procedure PushMailFrom;
  begin
    FRelayP^.State:=RS_MailFrom;
    FResponse:=Concat('MAIL FROM:<',FRelayP^.From,'>'#13#10);
    PushSend;
  end;

  procedure PushRecipient;
  begin
    FRelayP^.State:=RS_Recip;
    FResponse:=Concat('RCPT TO:<',FRelayP^.&To,'>'#13#10);
    PushSend;
  end;

  procedure PushData;
  begin
    if (FRelayP^.Status=250) or (FRelayP^.Status=251) then begin
      FRelayP^.State:=RS_Data;
      FResponse:=Concat('DATA',#13#10);
      PushSend();
      Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
    end else begin
      FRelayP^.Error:=Concat(
        FRelayP^.Error,
        'SMTP Server rejected delivery with message:'#13#10,
        FRelayP^.Response,#13#10
      );
      PushQuit();
      if (( FRelayP^.iTransitTry<FRelayP^.iTryMax) and (FRelayP^.Status>400) and (FRelayP^.Status<500) ) then begin
        FTTL:=DateUtils.IncSecond(Core.Timer.dtUT,60);
        FNodeID:=FOwner.Owner.FMatrixNode.ID;
        if Storage.MatrixServices.Items.DB.GetRandomInstance(FOwner.Task,FOwner.FKind,FOwner.Owner.RootDomain.ID,FRelayService) then
          FNodeID:=FRelayService.NodeID;
        Inc(FRelayP^.iTransitTry);
        FRelayP^.iTry:=0;
        FRelayP^.State:=RS_NONE;
        FMeta:=Storage.UserStorage.Items.SMTP.toXML(FRelayP^,XML_HEADER_ON);
        Storage.MatrixQueue.Items.DB.Fail(FOwner.Task,FRelayP^.QueueID,FNodeID,FTTL,FMeta);
      end else begin
        Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_MX_DIALOG);
        Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
      end;
    end;
  end;


begin
  Handled:=true;
  FRelayP:=RSRP^.Info.DataP;
  If (FRelayP<>Nil) then begin
    if RSR.EndOf(RSRP^.RecvBuffer,SMTP_CMD) then begin
      FRelayP^.Response:=RSR.ReadLine(FRSRP^.RecvBuffer,FOwner.Refactor);
      Core.Arrays.VarString.fromString(FParameters,FRelayP^.Response,#32);
      FRelayP^.Status:=StrToIntDef(Core.Arrays.VarString.Parameter(FParameters,1),0);
      Try
        If (FRelayP^.Sent=false) then begin
          {$ifdef DROP_ALL_MAIL_RELAY}
             PushEndSession();
          {$else}
            Case FRelayP^.State of
              RS_WELCOME    : PushHelo();
              RS_HELO       : PushMailFrom();
              RS_MailFrom   : PushRecipient();
              RS_Recip      : PushData();
              RS_Data       : PushBody();
            end;
          {$endif}
        end else begin
          Case FRelayP^.State of
            RS_Body       : PushDelivered;
            RS_EndSession : PushEndSession;
          end;
        end;
      Except
        On E:Exception do Core.Logging.Native.WriteLogEntry(FOwner.Owner.RootDomain.Name,FOwner.FService,Concat('TTransportRelay.OnDataReceived Exception:',E.Message));
      end;
    end;
  end else begin
    Core.Logging.Native.WriteLogEntry(FOwner.Owner.RootDomain.Name,FOwner.FService,Concat('TTransportRelay.OnDataReceived.NilValue:',Concat('Socket=',IntToStr(RSRP^.Info.Socket))));
  end;
end;

procedure   TTransportRelay.OnDNSResult(RSRP:PRSR);

  procedure PushNextMX;
  begin
    FRelayP^.State:=RS_Lookup_IP;
    If Length(FRelayP^.MXServers)=0 then begin
      SetLength(FRelayP^.MXServers,1);
      FRelayP^.iMXLcv:=0;
      FRelayP^.MXServers[0]:=FRelayP^.Domain;
    end;
    FRelayP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);

    If FRelayP^.iMXLcv>High(FRelayP^.MXServers) then
      FRelayP^.iMXLcv:=0;

    FRelayP^.MXServer:=FRelayP^.MXServers[FRelayP^.iMXLcv];
    FRelayP^.DNSQuery:=FRelayP^.MXServer;
    Inc(FRelayP^.iMXLcv);
    If FRelayP^.iMXLcv>High(FRelayP^.MXServers) then
      FRelayP^.iMXLcv:=0;
    FOwner.DNSLookup(RSRP,FRelayP^.DNSQuery,[dnsCache,dnsIP],FRelayP^.DNSIP);
  end;

  procedure PushMXResult;
  begin
    If (eRemoteServer in RSRP^.Errors) then begin
      if (System.Length(FRelayP^.MXServers)=1) then begin
        FRelayP^.Error:=Concat(FRelayP^.Error,'Permanent Error encountered.  Server "',FRelayP^.MXServer,'" does not exist.'#13#10);
        Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_NO_MX_RECORDS);
        FRelayP^.State:=RS_EndSession;
        Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
      end else
        PushNextMX;
    end else begin
      Core.Arrays.VarString.Copy(RSRP^.DNS.Answers,FRelayP^.MXServers);
      PushNextMX;
    end;
  end;

  procedure PushIPResult;
  begin
    RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FOwner.TimeOut);
    Core.Arrays.VarString.Copy(RSRP^.DNS.Answers,FRelayP^.IPs);
    FRelayP^.iIPLcv:=0;
    If ( Length(FRelayP^.IPs)=0 ) then begin
      // The server does not have any IPs listed for this MX Server.  Push error and fail message.
      If ( System.Length(FRelayP^.MXServers)=0 ) then begin
        FRelayP^.Error:=Concat(FRelayP^.Error,ERR_SMTP_NO_IP_RECORDS,#13#10);
        FRelayP^.State:=RS_EndSession;
        Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_NO_IP_RECORDS);
        Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
      end else
        Retry(false);
    end else begin
      FRelayP^.State:=RS_Connect;
      RSRP^.Errors:=[];
      if (RSRP^.State or RSR_STATE_ISSUED_CONNECT=RSRP^.State) then begin
        Retry(false);
      end else begin
        FOwner.Connect(RSRP,FRelayP^.IPs[FRelayP^.iIPLcv],25);
      end;
    end;
  end;

  procedure MXError();
  begin
    // There was NO MX Record and we need to try root domain
    RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FOwner.TimeOut);
    if (eNoSuchDomain in RSRP^.Errors) then begin
      FRelayP^.Error:=Concat(FRelayP^.Error,'Domain "',FRelayP^.Domain,'" does not have any MX records.'#13#10);
      Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_NO_MX_RECORDS);
      FRelayP^.State:=RS_EndSession;
      Storage.MatrixQueue.Items.Db.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
    end else begin
      FRelayP^.iMXLcv:=0;
      if (FRelayP^.iTry>=FRelayP^.iTryMax) then begin
        // we tried multiple times to get an MX server.
        // There must be no MX records associated with this domain
        FRelayP^.iTry:=0;
        SetLength(RSRP^.DNS.Answers,1);
        FRelayP^.iMXLcv:=0;
        RSRP^.DNS.Answers[0]:=FRelayP^.Domain;
        PushNextMX();
      end else begin
        Retry(true);
      end;
    end;
  end;

  procedure IPError();
  begin
    RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FOwner.TimeOut);
    if (eNoSuchDomain in RSRP^.Errors) then begin
      FRelayP^.Error:=Concat(FRelayP^.Error,'Permanent Error encountered.  Server "',FRelayP^.MXServer,'" does not exist.'#13#10);
      Core.Arrays.VarString.Remove(FRelayP^.MXServers,FRelayP^.MXServer);
    end;
    Retry(true);
  end;

  procedure PushInvalidResult;
  begin
    Include(RSRP^.Errors,eDNS);
    FRelayP^.Error:=Concat(FRelayP^.Error,'DNS ',Core.Utils.Sockets.InAddrToStr(FRelayP^.DNSIP),' returned ',IntToStr(RSRP^.DNS.Header.Ancount),' entries.'#13#10);
    Case FRelayP^.State of
      RS_Lookup_MX: MXError();
      RS_Lookup_IP: IPError();
    end;
  end;
begin
  FRelayP:=RSRP^.Info.DataP;
  if (RSRP^.DNS.Header.Ancount>0) then begin
    Case FRelayP^.State of
      RS_Lookup_MX: PushMXResult();
      RS_Lookup_IP: PushIPResult();
    else
      PushInvalidResult();
    end;
  end else
    PushInvalidResult;
end;

procedure   TTransportRelay.Retry(Increment:boolean);

  procedure Push_MX_Retry;
  begin
    FRelayP^.State:=RS_Lookup_MX;
    FRSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FOwner.TimeOut);
    FRelayP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);
    FRelayP^.iIPLcv:=0;
    FRelayP^.DNSQuery:=FRelayP^.Domain;
    FOwner.DNSLookup(FRSRP,FRelayP^.DNSQuery,[dnsCache,dnsMX],FRelayP^.DNSIP);
  end;

  procedure Push_IP_Retry;
  begin
    FRelayP^.State:=RS_Lookup_IP;
    FRSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,FOwner.TimeOut);
    Inc(FRelayP^.iIPLcv);
    If FRelayP^.iIPLcv>High(FRelayP^.IPs) then begin
      Inc(FRelayP^.iMXLcv);
      If FRelayP^.iMXLcv>High(FRelayP^.MXServers) then
        FRelayP^.iMXLcv:=0;
    end;
    FRelayP^.MXServer:=FRelayP^.MXServers[FRelayP^.iMXLcv];
    FRelayP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);
    FRelayP^.DNSQuery:=FRelayP^.MXServer;
    FOwner.DNSLookup(FRSRP,FRelayP^.DNSQuery,[dnsCache,dnsIP],FRelayP^.DNSIP);
  end;

  procedure Push_Connect;
  var
    RelayRSRP:PRSR;
  begin
    FRelayP^.State:=RS_Connect;
    RelayRSRP:=FOwner.Allocate(rsrClient,rsrsTCP);
    RelayRSRP^.Info.DataP:=FRelayP;
    FRSRP^.Info.DataP:=nil;// Moved to new RSR
    RelayRSRP^.Transport:=FOwner.FTransportRelay;
    FRelayP^.Error:=Concat(FRelayP^.Error,'Communication dialog with server failed.'#13#10);
    FOwner.Connect(RelayRSRP,FRelayP^.IPs[FRelayP^.iIPLcv],25);
    FOwner.Retire(FRSRP);
  end;

begin
  FRelayP:=FRSRP^.Info.DataP;
  If (FRelayP<>Nil) then begin
    If (FRelayP^.Sent=false) then begin
      if (FRelayP^.iTry<=FRelayP^.iTryMax) then begin
        if Increment then
          Inc(FRelayP^.iTry);
        if (FRelayP^.State=RS_Connect) then begin
          Push_Connect();
        end else If (eNoSuchDomain in FRSRP^.Errors) then begin
          FRelayP^.Error:=Concat(FRelayP^.Error,'Possible Error encountered.  Mail server "',FRelayP^.MXServer,'" does not exist.'#13#10);
          Core.Arrays.VarString.Remove(FRelayP^.MXServers,FRelayP^.MXServer);
          if System.Length(FRelayP^.MXServers)>0 then begin
            Push_IP_Retry;
          end else begin
            FRelayP^.Error:=Concat(FRelayP^.Error,'Cannot deliver mail to a non-existing mail server "',FRelayP^.MXServer,'".'#13#10);
            FRelayP^.State:=RS_EndSession;
            Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_NO_MX_RECORDS);
            Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
          end;
        end else begin
          FRSRP^.Errors:=[];
          if System.Length(FRelayP^.MXServers)>0 then begin
            if System.Length(FRelayP^.IPs)>0 then begin
              Inc(FRelayP^.iIPLcv);
              if (FRelayP^.iIPLcv>=Length(FRelayP^.IPs)) then begin
                // Go to next MX Lcv
                FRelayP^.iIPLcv:=0;
                Empty(FRelayP^.IPs);
                Inc(FRelayP^.iMXLcv);
                if (FRelayP^.iMXLcv>=Length(FRelayP^.MXServers)) then
                  FRelayP^.iMXLcv:=0;

                if Length(FRelayP^.MXServers)=0 then begin
                  Push_MX_Retry();
                end else begin
                  FRelayP^.MXServer:=FRelayP^.MXServers[FRelayP^.iMXLcv];
                  FRelayP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);
                  FRelayP^.State:=RS_Lookup_IP;
                  FRelayP^.DNSQuery:=FRelayP^.MXServer;
                  FOwner.DNSLookup(FRSRP,FRelayP^.DNSQuery,[dnsCache,dnsIP],FRelayP^.DNSIP);
                end;
              end else begin
                Push_Connect();
              end;
            end else begin
              Push_IP_Retry();
            end;
          end else begin
            Push_MX_Retry();
          end;
        end;
      end else begin
        FRelayP^.Error:=Concat(FRelayP^.Error,'Sorry, but the system is giving up after ',IntToStr(FRelayP^.iTryMax),' attempts to deliver your message.',#13#10#9'MX Server:',FRelayP^.MXServer,#13#10#9'Network Error(s): ',RSRErrorToString(FRSRP^.Errors),#13#10#9'Socket: ',IntToStr(FRSRP^.Info.Socket),#13#10#9'Socket Error(s): ',SocketErrorToString(FRSRP^.TaskError),#13#10,'Delivery Failure.  No more attempts to deliver this message will be made.');
        FRelayP^.State:=RS_EndSession;
        Bounce(FOwner.Task,FOwner.FRefactor,FRelayP,ERR_SMTP_TOO_MANY_TRIES);
        Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
      end;
    end else begin
      FRelayP^.State:=RS_EndSession;
      FOwner.Retire(FRSRP);
      Storage.MatrixQueue.Items.DB.Delete(FOwner.Task,FRelayP^.Disk,FRelayP^.QueueID);
    end;
  end;
end;

procedure   TTransportRelay.Bounce(Task:Core.Database.Types.TTask; Refactor:TMemoryStream; RelayP:Storage.UserStorage.Items.SMTP.PRelayMail; Notice:Core.Strings.VarString);
var
  sBuffer     : string;
  ContentDrop : string;
  saFrom      : Core.Arrays.Types.VarString;
  Summary     : Storage.UserStorage.Items.SMTP.TSummary;
  Headers     : Core.Arrays.Types.KeyStrings;
  iLcv        : LongInt;
  iBodyIndex  : LongInt;
  Body        : Core.Arrays.Types.VarString;
  FolderID    : QWord;
  FileID      : QWord;
begin
  Core.Arrays.VarString.fromString(saFrom,RelayP^.From,'@');
  Try
    If (Length(saFrom)=2) then begin
      Storage.UserStorage.Items.SMTP.Empty(Summary);
      Try
        Summary.&To:=RelayP^.From;
        Summary.Domain:=FOwner.Owner.RootDomain.Name;
        Summary.User:=FOwner.Owner.RootDomain.Root;
        Summary.From:=Concat(FOwner.Owner.RootDomain.Root,'@',FOwner.Owner.RootDomain.Name);
        Summary.RemoteIP:=InAddrToStr(FOwner.Owner.FMatrixNode.IP);
        Summary.RemoteDomain:=FOwner.Owner.RootDomain.Name;
        Summary.Subject:='Undeliverable Email Message';
        Core.Arrays.KeyString.Init(Headers);
        Try
          Core.Arrays.KeyString.Add(@Headers,'To',Summary.&To);
          Core.Arrays.KeyString.Add(@Headers,'From',Summary.From);
          Core.Arrays.KeyString.Add(@Headers,'Subject',Summary.Subject);
          Core.Arrays.KeyString.Add(@Headers,'Date',Core.Utils.Time.TimeZoneTime);
          Core.Arrays.KeyString.Add(@Headers,'Content-Type','text/plain; charset=utf-8');
          Core.Arrays.KeyString.Add(@Headers,'Content-Transfer-Encoding',Storage.UserStorage.Items.SMTP.Encoding.Value[Storage.UserStorage.Items.SMTP.Encoding.em8Bit]);
          Core.Arrays.KeyString.Add(@Headers,'Content-Disposition','inline');
          Core.Arrays.KeyString.Add(@Headers,'Mime-Version','1.0');
          Core.Arrays.KeyString.Add(@Headers,'X-Mailer',Concat(App.Build.Title,' ',App.Build.Edition,' (',App.Build.Version,'); RSR Build ',App.Build.RSR,';'));

          Core.Arrays.VarString.Init(Body);
          Try
            for iLcv:=0 to High(Headers) do
              Core.Arrays.VarString.Add(@Body,Concat(Headers[iLcv]^.Key,': ',Headers[iLcv]^.Value),[]);
            iBodyIndex:=Core.Arrays.VarString.Add(@Body,'',[]);
            Core.Arrays.VarString.Add(@Body,Concat('The message you sent to ',RelayP^.&To,' was not delivered.'),[]);
            Core.Arrays.VarString.Add(@Body,Notice,[]);
            Core.Arrays.VarString.Add(@Body,'',[]);
            Core.Arrays.VarString.Add(@Body,'------------------------------------------------------------',[]);
            Core.Arrays.VarString.Add(@Body,Concat('To: ',RelayP^.&To),[]);
            Core.Arrays.VarString.Add(@Body,Concat('From: ',RelayP^.From),[]);
            Core.Arrays.VarString.Add(@Body,Concat('Subject: ',RelayP^.Subject),[]);
            Core.Arrays.VarString.Add(@Body,Concat('Date: ',Core.Utils.Time.DateTimeToString(RelayP^.Date)),[]);

            Core.Arrays.VarString.Add(@Body,'');

            Core.Arrays.VarString.Add(@Body,Concat('DNS Server: ',Core.Utils.Sockets.InAddrToStr(RelayP^.DNSIP)),[]);
            Core.Arrays.VarString.Add(@Body,Concat('MX Server: ',RelayP^.MXServer),[]);
            Core.Arrays.VarString.Add(@Body,Concat('MX Servers: ',Core.Arrays.VarString.toString(RelayP^.MXServers,',')),[]);
            Core.Arrays.VarString.Add(@Body,Concat('MX IPs: ',Core.Arrays.VarString.toString(RelayP^.IPs,',')),[]);
            Core.Arrays.VarString.Add(@Body,Concat('Transit Error(s):',#13#10#13#10),[]);
            Core.Arrays.VarString.Add(@Body,Concat(RelayP^.Error,#13#10#13#10),[]);
            Core.Arrays.VarString.Add(@Body,'------------------------------------------------------------',[]);

            Storage.UserStorage.Mimes(Headers,Body,Summary,Refactor);

            FolderID:=RelayP^.InboxID;
            FileID:=0;
            Storage.UserStorage.Items.SMTP.Write(
              Task,
              RelayP^.Disk,
              RelayP^.SpamID,
              RelayP^.UserID,
              RelayP^.DomainID,
              Summary,
              Body,
              Storage.UserStorage.Items.IMAP.Flags.Recent,
              Refactor,
              Storage.UserStorage.Items.SMTP.BypassFilters,
              FolderID,
              FileID
            );
          finally
            Core.Arrays.VarString.Done(Body);
          end;
        finally
           Core.Arrays.KeyString.Done(Headers);
        end;
      finally
        Storage.UserStorage.Items.SMTP.Done(Summary);
      end;
    end;
  finally
    Core.Arrays.VarString.Done(saFrom);
  end;
end;

procedure   TTransportRelay.WriteConfirmation();
var
  itemP       : Storage.UserStorage.Items.SMTP.PDelivery;
begin
  New(itemP);
  Storage.UserStorage.Items.SMTP.Init(itemP^);
  itemP^.Code:=FRelayP^.Status;
  itemP^.Message:=FRelayP^.Response;
  itemP^.Address:=FRelayP^.&To;
  itemP^.Date:=Core.Timer.dtUT;
  Storage.UserStorage.Items.SMTP.Append(FOwner.Task,FOwner.FXMLParser,itemP,FRelayP^.DomainID,FRelayP^.UserID,FRelayP^.FileID);
  if (itemP<>nil) then begin
    Storage.UserStorage.Items.SMTP.Done(itemP^);
    Dispose(itemP);
  end;
end;

procedure   TTransportRelay.OnInitialized(RSRP:PRSR);
begin
end;

procedure   TTransportRelay.OnQueued(RSRP:PRSR);
begin
  FRelayP:=RSRP^.Info.DataP;
  If (Length(FRelayP^.MXServers)=0) then begin
    FRelayP^.DNSQuery:=FRelayP^.Domain;
    FRelayP^.State:=RS_Lookup_MX;
    FRelayP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);
    FOwner.DNSLookup(RSRP,FRelayP^.DNSQuery,[dnsCache,dnsMX],FRelayP^.DNSIP);
  end else begin
    if FRelayP^.iMXLcv>Length(FRelayP^.MXServers) then
      FRelayP^.iMXLcv:=0;
    FRelayP^.State:=RS_Lookup_IP;
    FRelayP^.MXServer:=FRelayP^.MXServers[FRelayP^.iMXLcv];
    FRelayP^.DNSQuery:=FRelayP^.MXServer;
    FRelayP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);
    FOwner.DNSLookup(RSRP,FRelayP^.DNSQuery,[dnsCache,dnsIP],FRelayP^.DNSIP);
  end;
end;

procedure   TTransportRelay.OnFinalized(RSRP:PRSR);
begin
  FRelayP:=RSRP^.Info.DataP;
  If (FRelayP<>Nil) then begin
    Storage.UserStorage.Items.SMTP.Done(FRelayP^);
    Dispose(FRelayP);
    FRelayP:=nil;
    RSRP^.Info.DataP:=Nil;
  end;
end;


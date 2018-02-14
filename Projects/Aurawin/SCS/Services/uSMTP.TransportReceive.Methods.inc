procedure   TTransportReceive.OnConnected(RSRP:PRSR);
begin

end;

procedure   TTransportReceive.OnDisconnected(RSRP:PRSR);
begin

end;


procedure   TTransportReceive.OnInitialized(RSRP:PRSR);

  procedure PushWelcome();
  begin
    FOwner.EntryPoint:='TTransportReceive.OnQueued';
    FMailP:=RSRP^.Info.DataP;
    FMailP^.BlackListDNS:=Core.Arrays.VarString.Random(FOwner.saDNSBlackLists);
    FOwner.FResponse:=Concat('220 ',FOwner.Owner.RootDomain.Name,' running ',App.Build.Title,' ',App.Build.Edition,' Build (',App.Build.Version,') RSR Build (',App.Build.RSR,'). AURA SMTP Service Ready.',#13#10);
    FOwner.Send(RSRP,FOwner.FResponse);
    If Core.Timer.dtNow>FOwner.dtNextAntiSpamUpdate then
      FOwner.CopyAntiSpamData();
  end;

begin
  FOwner.EntryPoint:='TTransportReceive.OnInitialize';
  FMailP:=RSRP^.Info.DataP;
  if (FMailP=nil) then begin
    FOwner.EntryPoint:='TTransportReceive.OnInitialize.New';
    New(FMailP);
    FOwner.EntryPoint:='TTransportReceive.OnInitialize.Storage.Items.SMTP.Init';
    Storage.UserStorage.Items.SMTP.Init(FMailP^);
    FMailP^.Exchanger:=Concat(FOwner.Owner.FMatrixNode.Alias,'.',FOwner.Owner.RootDomain.Name);
    RSRP^.Info.DataP:=FMailP;
  end;
  RSRP^.Transport:=FOwner.FTransportRecv;
  FMailP^.SenderIP:=Core.Utils.Sockets.InAddrToStr(RSRP^.Address.sin_addr.S_addr);
  FOwner.EntryPoint:='TTransportReceive.OnInitialize.Intrusion.Intruder.DB.Count';
  FIntruder:=(Storage.Intrusion.Intruder.DB.Recent(FOwner.Task,FOwner.Owner.RootDomain.ID,RSRP^.Address.sin_addr.s_addr)>0);
  FHacker:=(Storage.Intrusion.Account.DB.Recent(FOwner.Task,FOwner.Owner.RootDomain.ID,RSRP^.Address.sin_addr.s_addr)>0);
  if (FIntruder=false) and (FHacker=false) then begin
    PushWelcome();
  end else begin
    FOwner.Close(RSRP);
    FOwner.FResponse:=Concat('221 ',FOwner.Owner.RootDomain.Name,' blocked by ',App.Build.Title,' ',App.Build.Edition,' Build (',App.Build.Version,') RSR Build (',App.Build.RSR,'). AURA SMTP Service Denied.',#13#10);
    Core.Logging.Native.WriteLogEntry(
      FOwner.Owner.RootDomain.Name,
      FOwner.FService,
      Concat(
        'Security alert : Rejected ',
        App.Consts.Hacker[FHacker],' ',
        App.Consts.Intruder[FIntruder],' ',
        'IP ',
        IntToStr(RSRP^.Address.sin_addr.s_addr),
        ' (',FMailP^.SenderIP,')'
      )
    );
  end;
end;

procedure   TTransportReceive.OnQueued(RSRP:PRSR);
begin
  FMailP:=RSRP^.Info.DataP;
  if (FMailP=nil) then begin
    FOwner.EntryPoint:='TTransportReceive.OnQueued.New';
    New(FMailP);
    FOwner.EntryPoint:='TTransportReceive.OnQueued.Storage.Items.SMTP.Init';
    Storage.UserStorage.Items.SMTP.Init(FMailP^);
    FMailP^.Exchanger:=Concat(FOwner.Owner.FMatrixNode.Alias,'.',FOwner.Owner.RootDomain.Name);
    RSRP^.Info.DataP:=FMailP;
  end;
end;

procedure   TTransportReceive.OnFinalized(RSRP:PRSR);
begin
  FMailP:=RSRP^.Info.DataP;
  If FMailP<>Nil then begin
    ClearRecipients(FMailP^.Recipients);
    Storage.UserStorage.Items.SMTP.Done(FMailP^);
    Dispose(FMailP);
    FMailP:=nil;
    RSRP^.Info.DataP:=Nil;
  end;
end;

procedure   TTransportReceive.OnErrored(RSRP:PRSR);
begin
end;

procedure   TTransportReceive.OnDNSResult(RSRP:PRSR);
begin
  FMailP:=RSRP^.Info.DataP;
  if (FMailP<>nil) and (FMailP^.BlackListed=false) then
    FMailP^.BlackListed:=(RSRP^.DNS.Header.Ancount>0);
end;

procedure   TTransportReceive.OnDataReceived(RSRP:PRSR; var Handled:boolean);
  Function CloseSession:Boolean;
  begin
    Result:=(
              (RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) or
              (FMailP=Nil)
            )
            or
            (
              (FMailP<>Nil) and
              (
                (FMailP^.ErrorCount>=MAX_MTA_ERRORS) or
                (FMailP^.State or RS_EndSession=FMailP^.State)
              )
            );
  end;

  procedure PushProcessLoop;
  var
    bEndsWithLF:boolean;
  begin
    FEndOfMessage:=-1;
    Repeat
      ProcessDataLine();
      FOwner.RenewCycle();
      bEndsWithLF:=RSR.EndOfLine(RSRP^.RecvBuffer);
    until (
      (FOwner.Terminated=true) or
      (RSRP^.RecvBuffer.posRead=RSRP^.RecvBuffer.posWrite) or
      (FMailP^.State or RS_EndSession = FMailP^.State) or
      (( (FMailP^.State or RS_DATA)=FMailP^.State) and ( (bEndsWithLF=false) or (FEndOfMessage=-1)    ))
    );
  end;

begin
  Handled:=true;
  Try
    FOwner.EntryPoint:='TTransportReceive.OnDataReceived';
    FRSRP:=RSRP;
    FMailP:=RSRP^.Info.DataP;
    If (FMailP<>nil) then begin
      FOwner.EntryPoint:='TTransportReceive.OnDataReceived Packet Size Check';
      if (RSRP^.RecvBuffer.posWrite<RSR.RSR_MAX_RECV_PAYLOAD_SIZE) then begin
        FOwner.EntryPoint:='TTransportReceive.OnDataReceived Packet End Check';
        if RSR.EndOf(RSRP^.RecvBuffer,SMTP_CMD) then begin
          if  (FMailP^.ErrorCount<=MAX_MTA_ERRORS) and (CloseSession=false) then begin
            FOwner.EntryPoint:='TTransportReceive.OnDataReceived Packet Process Loop';
            Try
              PushProcessLoop();
            Except
              On E:Exception do begin
                Inc(FMailP^.ErrorCount);
                Core.Logging.Native.WriteLogEntry(FOwner.Owner.RootDomain.Name,FOwner.FService,Concat('TTransportReceive.OnDataReceived.PushProcessLoop Exception:',E.Message));
              end;
            End;
          end;
          If (FMailP^.UAP<>nil) and (FMailP^.UAP^.Modified) and (FMailP^.UAP^.ID<>0) then begin
            Storage.UserAccounts.Items.DB.UpdateMods(FOwner.Task,FMailP^.UAP^);
            FMailP^.UAP^.Modified:=False;
          end;
        end;
      end else begin
         FMailP^.State:=FMailP^.State or RS_EndSession;
         FResponse:=Concat('221 Transport channel closed due to buffer overuse.',#13#10);
         FOwner.Send(FRSRP,FResponse);
         Core.Logging.Native.WriteLogEntry(FOwner.Owner.RootDomain.Name,FOwner.FService,Concat('TTransportReceive.OnDataReceived.Buffer Overuse:','Exceeded Limits'));
      end;
    end else begin
      FMailP^.State:=FMailP^.State or RS_EndSession;
      FResponse:=Concat('221 Transport channel closed due to null pointer to mail data.',#13#10);
      FOwner.Send(FRSRP,FResponse);
      Core.Logging.Native.WriteLogEntry(FOwner.Owner.RootDomain.Name,FOwner.FService,Concat('TTransportReceive.OnDataReceived.Error:','Null pointer to Mail Data'));
    end;
    if (FOwner.Terminated=false) and CloseSession then
      FOwner.Close(RSRP);
  Except
    On E:Exception do Core.Logging.Native.WriteLogEntry(FOwner.Owner.RootDomain.Name,FOwner.FService,Concat('TTransportReceive.OnDataReceived Exception:',E.Message));
  end;
end;


Function    TTransportReceive.CMDToFrom(CMD:Core.Strings.VarString):Boolean;
begin
  FOwner.EntryPoint:='TTransportReceive.CMDToFrom';
  // MAIL FROM:<XXX@DDDDDDDD> <auth asldfkas;dfkasdf
  // MAIL FROM: <XXX@DDDDDDDD>
  // MAIL FROM:<>;
  // MAIL FROM:<> SIZE=XXXX
  // MAIL FROM:<XXX@YYY> SIZE=ZZZZ
  // xxx@xxx>
  FLength:=Length(CMD);
  FMailP^.MailFrom:=SysUtils.Trim(System.Copy(CMD,11,FLength-10));
  Result:=(Pos('<',FMailP^.MailFrom)>0) and (Pos('>',FMailP^.MailFrom)>0);
  If Result then begin
    If (FMailP^.MailFrom[1]='<') then
      System.Delete(FMailP^.MailFrom,1,1);
    FLength:=System.Pos('>',FMailP^.MailFrom)-1;
    System.SetLength(FMailP^.MailFrom,FLength);
  end;

  Storage.Security.Filter.Empty(FConnectionFilter);
  Try
    CMD:=ExtractDomain(FMailP^.MailFrom);
    if (Length(CMD)=0) then begin
      FMailP^.Blacklisted:=(FMailP^.Blacklisted=true) or (FMailP^.WhiteListed=false);
      CMD:=FMailP^.SenderDomain;
      FConnectionFilter.Counter:=1;
      FConnectionFilter.Enabled:=True;
      FConnectionFilter.Value:=Storage.Security.Prepare(CMD);
      FConnectionFilter.Value:=Storage.Security.Filter.getTopLevel(FConnectionFilter.Value);
      Storage.Security.Filter.DB.Elevate(FOwner.Task,secConnections,FConnectionFilter);
    end else begin
      FConnectionFilter.Counter:=1;
      FConnectionFilter.Enabled:=True;
      FConnectionFilter.Value:=Storage.Security.Prepare(CMD);
      FConnectionFilter.Value:=Storage.Security.Filter.getTopLevel(FConnectionFilter.Value);
      Storage.Security.Filter.DB.Identify(FOwner.Task,secConnections,FConnectionFilter);

      FMailP^.WhiteListed:=(FMailP^.WhiteListed=true) or (Filter.IsTopLevelDomainListed(FConnectionFilter.Value,FOwner.WhiteList,FMailP^.FilterID)=true);
      FMailP^.Blacklisted:=(
        (FMailP^.WhiteListed=false) and
        ((FMailP^.Blacklisted=true) or (Filter.IsTopLevelDomainListed(FConnectionFilter.Value,FOwner.BlackList,FMailP^.FilterID)=true))
      );
    end;
  Finally
    Storage.Security.Filter.Empty(FConnectionFilter);
  end;
end;

procedure   TTransportReceive.CMDToRecipient(Var Input:Core.Strings.VarString);
var
  ELoc,Loc:LongInt;
  Check:Boolean;

  procedure PushSend;
  begin
    FOwner.Send(FRSRP,FResponse);
  end;

  procedure PushVerifyMailDrop(CurrentDomain,CurrentUser:Core.Strings.VarString);
  var
    srP:PFileStorageRequest;
  begin
    Try
      Empty(SR);
      SR.Kind:=Storage.UserStorage.Kind.SMTP;
      SR.UserName:=CurrentUser;
      SR.DomainName:=CurrentDomain;

      If Core.Strings.SameText(SR.DomainName,FOwner.Owner.RootDomain.Name) then begin
        SR.DomainID:=FOwner.Owner.RootDomain.ID;
      end;
      If Storage.UserStorage.FillStorageRequest(FOwner.Task,SR) then begin
        If SR.Exists then begin
          If SR.Enabled then begin
            FMailP^.State:=FMailP^.State or RS_Recip;
            If SR.Forwarding and (Length(SR.ForwardingAddress)>0) then begin
              FMailP^.State:=FMailP^.State or RS_Recip;
              FMailP^.FForward:=SR.ForwardingAddress;
              FResponse:=Concat('250 Mailbox action taken: Will forward to ',SR.ForwardingAddress,'.',#13#10);
              PushSend;
            end else If (SR.Quota>0) and (SR.Consumption>=SR.Quota) then begin
              FResponse:='452 Mailbox action not taken: Maildrop already exceeds disk quota.'#13#10;
              PushSend;
            end else begin
              new(srP);
              Storage.UserStorage.Copy(SR,srP^);
              Core.Arrays.KeyString.Add(FMailP^.Recipients,CurrentUser,CurrentDomain,srP,[]);
              FResponse:=Concat('250 Mailbox action taken: will deliver to ',SR.Username,'@',SR.DomainName,#13#10);
              PushSend();
            end;
          end else begin
            FResponse:='450 Mailbox action not taken: Maildrop is currently disabled.'#13#10;
            Inc(FMailP^.RecipientMissCount,1);
            PushSend;
          end;
        end else if SR.DomainID<>FOwner.Owner.RootDomain.ID then begin // Add as inter-domain relay
          If SR.Forwarding and (Length(SR.ForwardingAddress)>0) then begin
            FMailP^.FForward:=SR.ForwardingAddress;
            FMailP^.State:=FMailP^.State or RS_Recip;
            FResponse:=Concat('250 Mailbox action taken: Will forward to ',SR.ForwardingAddress,'.',#13#10);
            PushSend;
          end else If (SR.Enabled) then begin
            new(srP);
            Storage.UserStorage.Copy(SR,srP^);
            Core.Arrays.KeyString.Add(FMailP^.Recipients,SR.Username,CurrentDomain,srP,[]);
            FMailP^.State:=FMailP^.State or RS_Recip;
            FResponse:=(Concat('250 Mailbox action taken: will deliver to ',SR.Username,'@',SR.DomainName,#13#10));
            PushSend;
          end else begin
            Inc(FMailP^.RecipientMissCount,1);
            FResponse:=Concat('450 Mailbox action not taken: Maildrop is currently disabled.',#13#10);
            PushSend;
          end;
        end else If (FOwner.Owner.RootDomain.DefaultOptionCatchAll=true) then begin  // Add user catchall account!!!
          If FOwner.Owner.RootUA.Forwarding and (Length(FOwner.Owner.RootUA.Forward)>0) then begin
            new(srP);
            Storage.UserStorage.Copy(SR,srP^);
            Core.Arrays.KeyString.Add(FMailP^.Recipients,SR.Username,CurrentDomain,srP,[]);
            FMailP^.FForward:=FOwner.Owner.RootUA.Forward;
            FMailP^.State:=FMailP^.State or RS_Recip;
            FResponse:=Concat('250 Mailbox action taken: Will forward to ',FMailP^.FForward,'.',#13#10);
            PushSend;
          end else If (FOwner.Owner.RootUA.Enabled) then begin
            SR.Kind:=Storage.UserStorage.Kind.SMTP;
            SR.Username:=FOwner.Owner.RootDomain.Root;
            SR.DomainName:=FOwner.Owner.RootDomain.Name;
            SR.DomainID:=FOwner.Owner.RootUA.DomainID;
            SR.UserID:=FOwner.Owner.RootUA.ID;
            If Storage.UserStorage.FillStorageRequest(FOwner.Task,SR) then begin
              if (SR.Quota=0) or (SR.Consumption<SR.Quota) then begin
                new(srP);
                Storage.UserStorage.Copy(SR,srP^);
                Core.Arrays.KeyString.Add(FMailP^.Recipients,SR.Username,CurrentDomain,srP,[]);
                FMailP^.State:=FMailP^.State or RS_Recip;
                FResponse:=(Concat('250 Mailbox action taken: will deliver to ',SR.Username,'@',SR.DomainName,#13#10));
                PushSend;
              end else begin
                FResponse:=Concat('452 Mailbox action not taken: Routed mailbox exceed disk quota.',#13#10);
                PushSend;
              end;
            end else begin
              FResponse:=Concat('450 Mailbox action not taken: No route to any mailboxes.',#13#10);
              PushSend;
            end;
          end else begin
            FResponse:=Concat('450 Mailbox action not taken: Maildrop is currently disabled.',#13#10);
            PushSend;
          end;
        end else begin
          Inc(FMailP^.RecipientMissCount,1);
          FResponse:='450 Mailbox action not taken: mailbox does not exist.'#13#10;
          PushSend;
        end;
      end else begin
        FResponse:='451 Mailbox action not taken: Could not aquire a maildrop.'#13#10;
        PushSend;
      end;
    Finally
      Empty(SR);
    end;
  end;
begin
  // RCPT TO:<XXX@DDDDDDDD>#13#10
  // RCPT TO: <XXX@DDDDDDDD>#13#10
  //          10
  //                       23
  Loc:=Core.Strings.Pos('<',Input);
  ELoc:=Core.Strings.Pos('>',Input);
  If Length(FMailP^.Recipients)>19 then begin
    FResponse:='553 Mailbox action not taken: Too many recipients.'#13#10;
    PushSend;
  end else If (Loc>0) and (ELoc>0) then begin
    Recip:=System.Copy(InPut,Loc+1,ELoc-Loc-1);
    RDomain:=ExtractDomain(Recip);
    FlRDomain:=Lowercase(RDomain);
    RUser:=Lowercase(ExtractUser(Recip));
    Check:=(RUser<>'') and (Pos('@',Recip)>0) and (RDomain<>'');

    If Check then begin
      If Core.Strings.SameText(FOwner.Owner.RootDomain.Name,FlRDomain) or Storage.Domains.Items.DB.IsInternal(FOwner.Task,FlRDomain) then begin
        // Message is local then drop it..
        PushVerifyMailDrop(RDomain,RUser);
      end else begin
        // Message is requesting to be relayed!!!!!
        If FMailP^.State or RS_LOGIN_OK=FMailP^.State then begin
          if (FRelayService.NodeID=0) then
            Storage.MatrixServices.Items.DB.GetRandomInstance(FOwner.Task,Storage.MatrixServices.Items.mkSMTP,FOwner.Owner.RootDomain.ID,FRelayService);
          if (FRelayService.NodeID<>0) then begin
            FResponse:=Concat('250 Mailbox action taken: will relay to ',Recip,#13#10);
            Core.Arrays.VarString.Add(FMailP^.RelayRecipients,Recip);
          end else begin
            FResponse:=Concat('451 Mailbox action failed: cannot relay to <',Recip,'> due to a null relay node id',#13#10);
          end;
          PushSend();
        end else begin
          Empty(USMTPRR);
          USMTPRR.DomainID:=FOwner.Owner.RootDomain.ID;
          USMTPRR.UserName:=Lowercase(ExtractUser(FMailP^.MailFrom));
          Storage.UserStorage.FillRelayRequest(FOwner.Task,USMTPRR);
          If USMTPRR.Exists then begin
            If USMTPRR.Enabled then begin
              if USMTPRR.LastIP=FRSRP^.Address.sin_addr.s_Addr then begin
                Core.Arrays.VarString.Add(FMailP^.RelayRecipients,Recip);
                if ((FMailP^.State or RS_LOGIN_OK)<>FMailP^.State) then begin
                  FMailP^.UAP:=FOwner.FUserAccounts.Acquire(USMTPRR.UserName);
                  FMailP^.UAP^.DomainID:=FOwner.Owner.RootDomain.ID;
                  If Storage.UserAccounts.Items.DB.Fill_Auth(FOwner.Task,FMailP^.UAP^) then begin
                    ValidateCredentials();
                    FResponse:=Concat('250 Mailbox action taken: will relay to ',Recip,#13#10);
                    PushSend();
                  end else begin
                    FResponse:='451 Mailbox action not taken: could not validate account credentials.'#13#10;
                    PushSend;
                  end;
                end;
              end else begin
                FResponse:='553 Mailbox action not taken: Please check mail before sending.'#13#10;
                PushSend;
              end;
            end else begin
              FResponse:='450 Mailbox Mailbox action not taken: The account is currently disabled.'#13#10;
              PushSend;
            end;
          end else begin
            FResponse:=Concat('553 Mailbox action not taken: will not relay to ',Recip,#13#10);
            PushSend;
          end;
        end;
      end;
    end else begin
      FResponse:=Concat('501 "',Recip,'" is an invalid recipient command'#13#10);
      PushSend;
    end;
  end else begin
    FResponse:=Concat('501 Command "',Input,'" Not Understood.'#13#10);
    PushSend;
  end;
end;

Function    TTransportReceive.ExtractDomain(Var Recipient:Core.Strings.VarString):Core.Strings.VarString;
var
  Loc,Len:LongInt;
begin
  Len:=Length(Recipient);
  Loc:=Core.Strings.Pos('@',Recipient);
  Result:=System.Copy(Recipient,Loc+1,Len-Loc);
end;

procedure   TTransportReceive.ClearRecipients(var List:Core.Arrays.Types.KeyStrings);
var
  iLcv:LongInt;
  srP:PFileStorageRequest;
begin
  For iLcv:=0 to High(List) do begin
    srP:=List[iLcv]^.Data;
    if (srP<>nil) then begin
      List[iLcv]^.Data:=nil;
      Storage.UserStorage.Done(srP^);
      Dispose(srP);
    end;
  end;
  Core.Arrays.KeyString.Empty(List);
end;

procedure   TTransportReceive.ValidateCredentials();
begin
  Storage.MatrixServices.Items.DB.GetRandomInstance(FOwner.Task,Storage.MatrixServices.Items.mkSMTP,FOwner.Owner.RootDomain.ID,FRelayService);
  FMailP^.UAP^.LastIP:=FRSRP^.Address.sin_addr.S_addr;
  FMailP^.UAP^.LastAccessed:=Core.Timer.dtUT;
  FMailP^.UAP^.Modified:=True;
  FMailP^.UAP^.LockoutCount:=0;
  FMailP^.State:=FMailP^.State or RS_LOGIN_OK;
end;

procedure   TTransportReceive.InValidateCredentials();
begin
  Inc(FMailP^.UAP^.LockoutCount);
  FMailP^.UAP^.LastAccessed:=Core.Timer.dtUT;
  FMailP^.UAP^.Modified:=True;
end;

procedure   TTransportReceive.ProcessCredentials();
begin
  If ((Length(FPassword)>0) and (FMailP^.UAP^.Password=FPassword) ) then begin
    // Password Match
    If (
          (FMailP^.UAP^.LockoutCount>0) and
          (
            (FMailP^.UAP^.LockoutCount<=Max_LockCount) or
            (MinutesBetween(FMailP^.UAP^.LastAccessed,Core.Timer.dtUT)>=2)
          ) or (
          (FMailP^.UAP^.LockoutCount<=Max_LockCount)
          )
       )
    then begin
      ValidateCredentials();
      FResponse:='235 Credentials Valid... You may proceed'#13#10;
      FOwner.Send(FRSRP,FResponse);
    end else begin
      InvalidateCredentials();
      FResponse:='535 Invalid Credentials.'#13#10;
      PushError();
    end;
  end else begin
    InvalidateCredentials();
    Storage.Intrusion.Account.DB.Add(FOwner.Task,FOwner.Owner.RootDomain.ID,FMailP^.UAP^.User,FPassword,FRSRP^.Address.sin_addr.S_addr);
    FIntrusions:=Storage.Intrusion.Account.DB.Recent(FOwner.Task,FOwner.Owner.RootDomain.ID,FRSRP^.Address.sin_addr.S_addr);
    if (FIntrusions>Storage.Intrusion.Account.Defaults.MaxRecentBeforeBlocked) then begin
      Storage.Intrusion.Intruder.DB.Add(FOwner.Task,FOwner.Owner.RootDomain.ID,FRSRP^.Address.sin_addr.S_addr,Core.Timer.dtUT);
      Storage.Security.Filter.Empty(FConnectionFilter);
      Try
        FConnectionFilter.Counter:=1;
        FConnectionFilter.Enabled:=True;
        FConnectionFilter.Value:=Core.Utils.Sockets.MaskClassC(FMailP^.SenderIP);
        if Storage.Security.Filter.DB.Exists(FOwner.Task,secViolatorIP,FConnectionFilter) = false then begin
          Storage.Security.Filter.DB.Identify(FOwner.Task,secViolatorIP,FConnectionFilter);
          Core.Logging.Native.WriteLogEntry(
            FOwner.Owner.RootDomain.Name,
            FOwner.Owner.FService,
            Concat(
              'Security alert : Account credentials blocked for (',FUserName,'=',FPassword,') and rejected IPs (',FConnectionFilter.Value,'* )'
            )
          );
        end else begin
          Storage.Security.Filter.DB.Identify(FOwner.Task,secViolatorIP,FConnectionFilter);
        end;
      Finally
        Storage.Security.Filter.Empty(FConnectionFilter);
      end;
      FOwner.Close(FRSRP);
    end;
    FResponse:='535 Invalid Credentials.'#13#10;
    PushError();
  end;
end;

Procedure TTransportReceive.PushError();
begin
  FMailP^.ErrorPushed:=True;
  Inc(FMailP^.ErrorCount);
  FOwner.Send(FRSRP,FResponse);
end;

Procedure   TTransportReceive.ProcessDataLine();


  Function TransState:Boolean;
  begin
    Result:=(FMailP^.State or RS_HELO)=FMailP^.State;
  end;

  procedure RemoveCh(DecVal:Byte; var Value:Core.Strings.VarString);
  begin
    Value:=SysUtils.StringReplace(Value,Char(DecVal),'',[rfReplaceAll]);
  end;

  procedure PushMessageDelivered();
  var
    iLcv:LongInt;
  begin
    // a delivery confirmation message that each time a message is sent
    // the master copy of it is stored in the "Delivered" folder.
    // delivery tracking will be usefull there.
    for iLcv:=0 to High(FMailP^.Recipients) do begin
      fsrP:=FMailP^.Recipients[iLcv]^.Data;
      new(FDeliveryP);
      Storage.UserStorage.Items.SMTP.Init(FDeliveryP^);
      FDeliveryP^.Code:=250;
      FDeliveryP^.Message:=Storage.UserStorage.Items.SMTP.MSG_MESSAGE_ACCEPTED;
      FDeliveryP^.Date:=Core.Timer.dtUT;
      FDeliveryP^.Address:=Concat(FMailP^.Recipients[iLcv]^.Key,'@',FMailP^.Recipients[iLcv]^.Value);
      Storage.UserStorage.Items.SMTP.Append(FDeliveryP,FOwner.FSummary.Deliveries);
    end;
    Storage.UserStorage.Items.SMTP.Update(
      FOwner.FSummary,
      FMailP^.Headers,
      FOwner.Owner.RootDomain.Name,
      FMailP^.UAP^.User,
      FMailP^.SenderIP,
      FMailP^.SenderDomain,
      FMailP^.MailFrom,
      FMailP^.Exchanger,
      Storage.UserStorage.Items.SMTP.Defaults.Outbound,
      FMailP^.ContentType
    );
    Storage.UserStorage.Items.SMTP.Update(
      FOwner.FSummary,
      FMailP^.FilterID,
      Storage.UserStorage.Items.SMTP.Defaults.Read,
      Storage.UserStorage.Items.SMTP.Defaults.Sent,
      false, // FMailP^.Spam,
      false, // FMailP^.BlackListed,
      true   // FMailP^.WhiteListed
    );
    Storage.UserStorage.Items.SMTP.Stamp(
      FOwner.FSummary,
      FMailP^.Headers,
      FMailP^.Content,
      FOwner.Refactor
    );
    Storage.UserStorage.Mimes(
      FMailP^.Headers,
      FMailP^.Content,
      FOwner.FSummary,
      FOwner.Refactor
    );
    FMailP^.FolderID:=FMailP^.UAP^.SentBox;
    FMailP^.FileID:=0;

    Storage.UserStorage.Items.SMTP.Write(
      FOwner.Task,FMailP^.UAP^.AuraNode,
      FMailP^.UAP^.SpamBox,
      FMailP^.UAP^.ID,
      FMailP^.UAP^.DomainID,
      FOwner.FSummary,
      FMailP^.Content,
      Storage.UserStorage.Items.IMAP.Flags.Recent or Storage.UserStorage.Items.IMAP.Flags.Seen,
      FOwner.Refactor,
      Storage.UserStorage.Items.SMTP.BypassFilters,
      FMailP^.FolderID,
      FMailP^.FileID
    );
  end;

  procedure PushMessageProcess();
  var
    iLcv:LongInt;
    bLoggedIn:Boolean;
    iHdrCount:LongInt;
    iHdrLoc:LongInt;
  begin
    iHdrLoc:=IndexOfHeaderBreak(FMailP^.Content);
    if (iHdrLoc<1) then begin
      FMailP^.Spam:=True;
      FResponse:='550 Your message contains an invalid Envelope'#13#10;
      FOwner.Send(FRSRP,FResponse);
      FMailP^.State:=FMailP^.State or RS_EndSession;
      exit;
    end else begin
      FMailP^.Spam:=False;
    end;
    FMailP^.WhiteListed:= ( (FMailP^.WhiteListed) or ((FMailP^.State or RS_LOGIN_OK)=FMailP^.State) );
    If (FMailP^.Spam=false) then begin
      FHeaders:=ExtractHeaders(FMailP^.Content,iHdrLoc-1);
      FHeaders:=UnwrapHeaders(FHeaders);
      iHdrCount:=Core.Arrays.KeyString.fromString(FMailP^.Headers,FHeaders,': ',#13#10);
      System.SetLength(FHeaders,0);

      FMailP^.From:=GetItemByKey(FMailP^.Headers,'From',iHdrCount);
      FMTALoop:=GetItemByKey(FMailP^.Headers,'X-MTA-Loop',iHdrCount);

      FMailP^.Subject:=GetItemByKey(FMailP^.Headers,'Subject',iHdrCount);

      FContentType:=GetItemByKey(FMailP^.Headers,'Content-Type',iHdrCount);

      FMailP^.ContentType:=getType(FMailP^.Headers,iHdrCount,FMailP^.CharSet,FMailP^.CharFormat,FMailP^.ContentName);
      FMailP^.Encoding:=getTransferEncoding(FMailP^.Headers,iHdrCount);
      RSR.RemoveEscapeCharacters(FMailP^.Content,FOwner.Refactor);

      FMailP^.MTALoopNotPushed:=FMTALoop='';
    end;
    If System.Length(FMailP^.FForward)=0 then begin
      FCheck:=Core.Utils.Sockets.MaskClassC(FMailP^.SenderIP);
      if (FMailP^.WhiteListed=false) and Storage.Security.Filter.DB.Exists(FOwner.Task,secViolatorIP,FCheck) then
        FMailP^.Spam:=true;

      for iLcv:=0 to High(FMailP^.Recipients) do begin

        fsrP:=FMailP^.Recipients[iLcv]^.Data;
        Storage.UserStorage.Items.SMTP.Empty(FOwner.FSummary);
        Storage.UserStorage.Items.SMTP.Update(
          FOwner.FSummary,
          FMailP^.Headers,
          FMailP^.Recipients[iLcv]^.Value,
          FMailP^.Recipients[iLcv]^.Key,
          FMailP^.SenderIP,
          FMailP^.SenderDomain,
          FMailP^.MailFrom,
          FMailP^.Exchanger,
          Storage.UserStorage.Items.SMTP.Defaults.Inbound,
          FMailP^.ContentType
        );
        Storage.UserStorage.Items.SMTP.Update(
          FOwner.FSummary,
          FMailP^.FilterID,
          Storage.UserStorage.Items.SMTP.Defaults.Unread,
          Storage.UserStorage.Items.SMTP.Defaults.Sent,
          FMailP^.Spam,
          FMailP^.BlackListed,
          FMailP^.WhiteListed
        );
        Storage.UserStorage.Items.SMTP.Stamp(
          FOwner.FSummary,
          FMailP^.Headers,
          FMailP^.Content,
          FOwner.Refactor
        );
        Storage.UserStorage.Mimes(
          FMailP^.Headers,
          FMailP^.Content,
          FOwner.FSummary,
          FOwner.Refactor
        );
        Storage.UserStorage.Items.SMTP.SecurityCheck(
          FOwner.ContentProfiles,
          FOwner.ContentPhrases,
          FOwner.BlackList,
          FMailP^.FilterID,
          FOwner.FSummary.Risk,
          FMailP^.Spam,
          Storage.UserStorage.Items.SMTP.EnforceFilters,
          FOwner.FSummary,
          FMailP^.Headers,
          FMailP^.Content,
          FOwner.Refactor
        );
        Storage.UserStorage.Items.SMTP.Update(
          FOwner.FSummary,
          FMailP^.FilterID,
          Storage.UserStorage.Items.SMTP.Defaults.Unread,
          Storage.UserStorage.Items.SMTP.Defaults.Sent,
          FMailP^.Spam,
          FMailP^.BlackListed,
          FMailP^.WhiteListed
        );
        Storage.UserStorage.Items.SMTP.Stamp(  // basic update of any SEC headers that were already stamped
          FOwner.FSummary,
          FMailP^.Headers,
          FMailP^.Content,
          FOwner.Refactor
        );
        FFolderID:=fsrP^.InboxID; // might change on parsing to Spambox
        FFileID:=0;
        Storage.UserStorage.Items.SMTP.Write(
          FOwner.Task,fsrP^.Node,
          fsrP^.SpamID,
          fsrP^.UserID,
          fsrP^.DomainID,
          FOwner.FSummary,
          FMailP^.Content,
          Storage.UserStorage.Items.IMAP.Flags.Recent,
          FOwner.Refactor,
          Storage.UserStorage.Items.SMTP.EnforceFilters,
          FFolderID,
          FFileID
        );
        If (FMailP^.Spam=true) or (FMailP^.Blacklisted=true) then begin
          {$if defined(cpu64)}
            InterlockedIncrement64(RSR.cntrFiltered);
          {$else}
            Inc(RSR.cntrFiltered);
          {$endif}
        end;
      end;
      if (FMailP^.Spam=true) then begin
        Storage.Intrusion.Intruder.DB.Add(FOwner.Task,FOwner.Owner.RootDomain.ID,FRSRP^.Address.sin_addr.S_addr,Core.Timer.dtUT);
      end;
      if (FMailP^.State  or RS_LOGIN_OK = FMailP^.State) then begin
        // Authenticated User.  Place a confirmation in sent folder
        PushMessageDelivered();
        for iLcv:=0 to High(FMailP^.RelayRecipients) do
          RelayMessage(FRelayService,FMailP^.RelayRecipients[iLcv],FOwner.FSummary,FMailP^);
      end;
      FResponse:=Concat(Storage.UserStorage.Items.SMTP.MSG_MESSAGE_ACCEPTED,#13#10);
      FOwner.Send(FRSRP,FResponse);
    end else If (Length(FMailP^.Recipients)>0) and (FMailP^.MTALoopNotPushed) then begin
      RelayMessage(FRelayService,FMailP^.FForward,FOwner.FSummary,FMailP^);
    end;
    ClearRecipients(FMailP^.Recipients);
    Empty(FMailP^.RelayRecipients);
    Storage.UserStorage.Items.SMTP.Empty(FMailP^);
    Storage.UserStorage.Items.SMTP.Empty(FOwner.FSummary);

    bLoggedIn:=FMailP^.State or RS_LOGIN_OK=FMailP^.State;
    FMailP^.State:=RS_HELO;
    if bLoggedIn then
       FMailP^.State:=FMailP^.State or RS_LOGIN_OK;
    SetLength(FResponse,0);
  end;

  procedure ProcessCommand;
  begin
    Core.Arrays.VarString.fromString(FsaParams,InputN,' ');
    Try
      FMailP^.CMD:=Uppercase(Core.Arrays.VarString.Parameter(FsaParams,1));
      FMailP^.Param1:=Core.Arrays.VarString.Parameter(FsaParams,2);
      FMailP^.Param2:=Core.Arrays.VarString.Parameter(FsaParams,3);
    Finally
      SetLength(FsaParams,0);
    end;
    If (FMailP^.CMD='QUIT') then begin
      FResponse:='221 Quit accepted gracefully. Good bye!'#13#10;
      FOwner.Send(FRSRP,FResponse);
      FMailP^.State:=FMailP^.State or RS_EndSession;
      FOwner.Close(FRSRP);
    end else if (FMailP^.CMD='HELO') or (FMailP^.CMD='EHLO') then begin
      FMailP^.SenderDomain:=Storage.UserStorage.Items.SMTP.ExtractSenderDomain(FMailP^.Param1);
      if Core.Strings.Search(FMailP^.SenderDomain,'chase')>0 then
        FResponse:='';
      If (Length(FMailP^.SenderDomain)>=1) then begin
        HeloInetAddr:=Core.Utils.Sockets.InAddrFromStr(FMailP^.SenderDomain);
        if HeloInetAddr<>0 then begin
          FTLD:=FMailP^.SenderDomain;
          FMailP^.WhiteListed:=Storage.Security.Filter.IsTopLevelDomainListed(FMailP^.SenderDomain,FOwner.WhiteList,FMailP^.FilterID);
          FMailP^.BlackListed:=(
            (FMailP^.WhiteListed=false) and
            (Storage.Security.Filter.IsTopLevelDomainListed(FMailP^.SenderDomain,FOwner.BlackList,FMailP^.FilterID)=true)
          );
        end else begin
          FTLD:=LowerCase(Storage.Security.Filter.getTopLevel(FMailP^.SenderDomain));
          if Length(FTLD)<=1 then
             FTLD:=FMailP^.SenderDomain;
          Storage.Security.Filter.Empty(FConnectionFilter);
          Try
            FConnectionFilter.Counter:=1;
            FConnectionFilter.Enabled:=True;
            FConnectionFilter.Value:=Storage.Security.Prepare(FTLD);
            Storage.Security.Filter.DB.Identify(FOwner.Task,secConnections,FConnectionFilter);
          Finally
            Storage.Security.Filter.Empty(FConnectionFilter);
          end;
          FMailP^.WhiteListed:=Storage.Security.Filter.IsTopLevelDomainListed(FTLD,FOwner.WhiteList,FMailP^.FilterID);
          FMailP^.BlackListed:=(
            (FMailP^.WhiteListed=false) and
            (Storage.Security.Filter.IsTopLevelDomainListed(FTLD,FOwner.BlackList,FMailP^.FilterID)=true)
          );
        end;
        FMailP^.State:=RS_HELO;
        If FMailP^.CMD='EHLO' then begin
          FMailP^.State:=FMailP^.State or RS_EHELO;
          FResponse:=Concat(
            '250-',FOwner.Owner.RootDomain.Name,' Hello ',FMailP^.SenderDomain,' pleased to meet you.  ',FMailP^.SenderIP,' ',WHITELIST_NOTICE[FMailP^.WhiteListed],'.'#13#10,
            '250-8BITMIME'#13#10,
            '250-SIZE'#13#10,
            '250-PIPELINING'#13#10,
            '250-STARTTLS'#13#10,
            '250-LOGIN'#13#10,
            '250 AUTH PLAIN LOGIN DIGEST-MD5'#13#10
          );
          FOwner.Send(FRSRP,FResponse);
          If (FMailP^.BlackListDNS<>'') and (FMailP^.BlackListed=false) and (FMailP^.WhiteListed=false) then
            FOwner.DNSLookup(FRSRP,FRSRP^.Address.sin_addr.S_addr,FMailP^.BlackListDNS,0);
        end else begin;
          FResponse:=Concat('250 Hello ',FMailP^.SenderDomain,' pleased to meet you.  ',FMailP^.SenderIP,' ',WHITELIST_NOTICE[FMailP^.WhiteListed],'.'#13#10);
          FOwner.Send(FRSRP,FResponse);
          If (FMailP^.BlackListDNS<>'') and (FMailP^.BlackListed=false) and (FMailP^.WhiteListed=false) then
            FOwner.DNSLookup(FRSRP,FRSRP^.Address.sin_addr.S_addr,FMailP^.BlackListDNS,0);
        end;
      end else begin
        FResponse:='500 Polite people say hello before sending mail.'#13#10;
        PushError;
      end;
    end else if (FMailP^.CMD='AUTH') then begin
      If TransState then begin
        If (Sysutils.SameText(FMailP^.Param1,'LOGIN')) then begin
          FMailP^.State:=FMailP^.State or RS_LOGIN_USER;
          FResponse:='334 VXNlcm5hbWU6'#13#10;
          FOwner.Send(FRSRP,FResponse);
        end else if (Sysutils.SameText(FMailP^.Param1,'PLAIN')) then begin
          FResponse:=Encryption.Base64.Decode(FMailP^.Param2);
          FLength:=Core.Arrays.VarString.fromString(FSaParams,FResponse,#0,[soClearList,soIgnoreDelimAtStart]);
          Case FLength of
            2 : begin
                FUserName:=FSaParams[0];
                FPassword:=FsaParams[1];
                FMailP^.UAP:=FOwner.FUserAccounts.Acquire(FUserName);
                FMailP^.UAP^.DomainID:=FOwner.Owner.RootDomain.ID;
                If Storage.UserAccounts.Items.DB.Fill_Auth(FOwner.Task,FMailP^.UAP^) then begin
                  ProcessCredentials();
                end else begin
                  FResponse:='454 Unable to retrieve credentials.'#13#10;
                  PushError;
                end;
            end else begin;
              FResponse:='501 Unable to parse credentials.'#13#10;
              PushError;
            end;
          end;
        end else if (Sysutils.SameText(FMailP^.Param1,'DIGEST-MD5')) then begin
          FMailP^.State:=FMailP^.State or RS_LOGIN_USER;
          FMailP^.AuthMD5:=True;
          FMailP^.Nonce:=RSR.Generate_Nonce(NONCE_LOW,NONCE_HIGH);
          FResponse:=Concat('realm="',FOwner.Owner.RootDomain.Name,'", nonce="',FMailP^.Nonce,'",qop="auth",algorithm=md5,charset=',Storage.Main.Header.Encoding);
          FResponse:=Encryption.Base64.Encode(FResponse);
          FResponse:=Concat('334 ',FResponse,#13#10);
          FOwner.Send(FRSRP,FResponse);
        end else begin
          FResponse:='504 Unrecognized authentication type.'#13#10;
          PushError;
        end;
      end else begin
        FResponse:='500 Polite people say hello before sending mail.'#13#10;
        PushError;
      end;
    end else if (FMailP^.CMD='MAIL') then begin
      If TransState then begin
        If Core.Strings.Search(FMailP^.Param1,'FROM:')=1 then begin
          If CMDToFrom(InputN) then begin
            FMailP^.State:=FMailP^.State or RS_MailFrom;
            If FMailP^.BlackListed then begin
              {$if defined(cpu64)}
                InterlockedIncrement64(RSR.cntrFiltered);
                InterlockedIncrement64(RSR.cntrTX);
              {$else}
                Inc(RSR.cntrFiltered);
                Inc(RSR.cntrTX);
              {$endif}
            end;
            FResponse:=Concat('250 Authentication successful.  You can proceed ',FMailP^.MailFrom,'.'#13#10);
            FOwner.Send(FRSRP,FResponse);
          end else begin
            FResponse:=Concat('501 Syntax error: "',InputN,'" is a malformed from address.'#13#10);
            PushError;
          end;
        end else begin
          FResponse:=Concat('500 Syntax error: MAIL Parameter "',FMailP^.Param1,'" is not recognized.'#13#10);
          PushError;
        end;
      end else begin
        FResponse:='500 Polite people say hello before sending mail.'#13#10;
        PushError;
      end;
    end else if (FMailP^.CMD='RCPT') then begin
      If FMailP^.State or RS_MailFrom=FMailP^.State then begin
        if (FMailP^.RecipientMissCount<=Storage.UserStorage.Items.SMTP.MaxMissedRecipients) then begin
          FCheck:=UpperCase(FMailP^.Param1);
          If Core.Strings.Pos('TO:',FCheck)>0 then
            CMDToRecipient(InputN)
          else begin
            FResponse:=Concat('500 Syntax error: command "',InputN,'" unrecognized.'#13#10);
            PushError;
          end;
        end else begin
          Storage.Intrusion.Intruder.DB.Add(FOwner.Task,FOwner.Owner.RootDomain.ID,FRSRP^.Address.sin_addr.S_addr,Core.Timer.dtUT);
          Core.Logging.Native.WriteLogEntry(
            FOwner.Owner.RootDomain.Name,
            FOwner.Owner.FService,
            Concat('Security alert: IP (',FMailP^.SenderIP,') was added to the intruders list via wrong recipients trap.')
          );
          FMailP^.State:=FMailP^.State or RS_EndSession;
          FResponse:=Concat('554 security error: "',InputN,'" too many failures.'#13#10);
          PushError();
          FOwner.Close(FRSRP);
        end;
      end else begin
        FResponse:='503 Syntax error: MAIL FROM not specified.'#13#10;
        PushError;
      end;
    end else if (FMailP^.CMD='DATA') then begin
      If (Length(FMailP^.Recipients)>0) or (Length(FMailP^.RelayRecipients)>0) then begin
        {$if defined(cpu64)}
          InterlockedIncrement64(RSR.cntrTX);
        {$else}
          Inc(RSR.cntrTX);
        {$endif}
        FMailP^.State:=FMailP^.State or RS_Data;
        FResponse:='354 Start mail input; end with <CRLF>.<CRLF>'#13#10;
        FOwner.Send(FRSRP,FResponse);
      end else begin
        FResponse:='554 Transaction failed: Need recipients before data can be sent.'#13#10;
        PushError;
      end;
    end else if (FMailP^.CMD='RSET') then begin
      ClearRecipients(FMailP^.Recipients);
      {$if defined(cpu64)}
        InterlockedIncrement64(RSR.cntrTX);
      {$else}
        Inc(RSR.cntrTX);
      {$endif}
      If FMailP^.State or RS_HELO=FMailP^.State then
        FMailP^.State:=RS_HELO
      else
        FMailP^.State:=RS_NONE;
      FResponse:='250 Action action taken.'#13#10;
      FOwner.Send(FRSRP,FResponse);
    end else if FMailP^.CMD='NOOP' then begin
      {$if defined(cpu64)}
        InterlockedIncrement64(RSR.cntrTX);
      {$else}
        Inc(RSR.cntrTX);
      {$endif}
      FMailP^.ErrorCount:=0;
      FResponse:='250 NOOP accepted gracefully.'#13#10;
      FOwner.Send(FRSRP,FResponse);
    end else if FMailP^.CMD='STARTTLS' then begin
      {$if defined(cpu64)}
        InterlockedIncrement64(RSR.cntrTX);
      {$else}
        Inc(RSR.cntrTX);
      {$endif}
      If ((FRSRP^.State or RSR_STATE_SECURE)<>FRSRP^.State) then begin
        If (FOwner.Owner.FSSLInfo.Manifest.Kind=sslckNone)then begin
          FResponse:=Concat('554 Administrator needs to create a certificate.',#13#10);
          PushError();
        end else If (FOwner.Owner.FSSLInfo.keyLen=0)then begin
          FResponse:=Concat('554 Administrator needs to assign an encryption key.',#13#10);
          PushError();
        end else If (FOwner.Owner.FSSLInfo.Manifest.List[0]^.crtLen=0)then begin
          FResponse:=Concat('554 Administrator needs to sign the certificate request.',#13#10);
          PushError();
        end else begin
          FResponse:=Concat('220 Begin TLS Handshake',#13#10);
          FOwner.Send(FRSRP,FResponse);
          FOwner.StartTLS(FRSRP);
        end;
      end else begin
        FResponse:=Concat('554 service already encrypted.',#13#10);
        PushError();
      end;
    end else begin
      FResponse:=Concat('500 Syntax error: command "',FMailP^.CMD,'" is not recognized.'#13#10);
      PushError();
    end;
  end;

begin
  If ((FMailP^.State or RS_Data)=FMailP^.State) then begin
    // Continue Receiving Message Packet
    // Look for last dot...
    FEndOfMessage:=RSR.Pos(FRSRP^.RecvBuffer,Storage.UserStorage.Items.SMTP.EndOfMessage);
    if FEndOfMessage>0 then begin
      //Core.Streams.toFile(FRSRP^.RecvBuffer.Stream,'/home/atbrunner/Desktop/buffer.txt');
      RSR.Extract(FRSRP^.RecvBuffer,0,FEndOfMessage,FMailP^.Content,[]);

      RSR.Refactor(FRSRP^.RecvBuffer,FOwner.Refactor,FEndOfMessage+Length(Storage.UserStorage.Items.SMTP.EndOfMessage));
      FLength:=System.Length(FMailP^.Content);
      PushMessageProcess();
    end;
  end else if  (FMailP^.State or RS_LOGIN_USER=FMailP^.State) then begin
    If TransState then begin
      InputN:=RSR.ReadLine(FRSRP^.RecvBuffer,FOwner.Refactor);
      FMailP^.State:=FMailP^.State and not RS_LOGIN_USER;

      if (FMailP^.AuthMD5=true) then begin
        FAuth:=Encryption.Base64.Decode(InputN);
        Core.Arrays.KeyString.fromString(FAuthData,FAuth,'=',',',[soClearList,soRemoveQuotes]);
        FUserName:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'username');
        FRealm:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'realm');
        FAuth:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'response');
        FNonce:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'nonce');
        FCNonce:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'cnonce');
        FNC:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'nc');
        FQOP:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'qop');
        FAuthZID:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'authzid');
        FURI:=Core.Arrays.KeyString.GetItemAsString(FAuthData,'digest-uri');
        if Sysutils.SameText(FQOP,'auth') then
          A2:=Concat('AUTHENTICATE:',FURI)
        else
          A2:=Concat('AUTHENTICATE:',FURI,':00000000000000000000000000000000');

        HA2:=Hash(A2,dgA2);


        FMailP^.UAP:=FOwner.FUserAccounts.Acquire(FUserName);
        FMailP^.UAP^.DomainID:=FOwner.Owner.RootDomain.ID;
        If Storage.UserAccounts.Items.DB.Fill_Auth(FOwner.Task,FMailP^.UAP^) then begin
          FPassword:=FMailP^.UAP^.Password;
          if (Length(FAuthZID)>0) then begin
            Hash(
              Concat(
                FUserName,':',
                FRealm,':',
                FPassword
              ),
              dgA1Pre
            );
            HA1:=Hash(
              dgA1Pre,
              Concat(
                ':',
                FNonce,':',
                FCNonce,':',
                FAuthZID
              ),
              dgA1
            );
          end else begin
              Hash(
                Concat(
                  FUserName,':',
                  FRealm,':',
                  FPassword
                ),
                dgA1Pre
              );
              HA1:=Hash(
                dgA1Pre,
                Concat(
                  ':',
                  FNonce,':',
                  FCNonce
                ),
                dgA1
              );
          end;

          FCheck:=Hash(
            Concat(
              HA1,':',
              FNonce,':',
              FNC,':',
              FCNonce,':',
              FQOP,':',
              HA2
            ),
            dgCheck
          );
          if SameText(FAuth,FCheck) =false then begin
            FPassword:=FAuth;
          end;
          ProcessCredentials();
        end else begin
          FResponse:='451 Unable to validate credentials at this time.'#13#10;
          PushError();
        end;
      end else begin
        FMailP^.State:=FMailP^.State or RS_LOGIN_PSWD;
        FUserName:=Encryption.Base64.Decode(InputN);
        FUserName:=Core.Strings.Extract(FUserName,'@');
        FMailP^.UAP:=FOwner.FUserAccounts.Acquire(FUserName);
        FResponse:='334 UGFzc3dvcmQ6'#13#10;
        FOwner.Send(FRSRP,FResponse);
      end;
    end else begin
      FResponse:='500 Polite people say hello before sending mail.'#13#10;
      PushError;
    end;
  end else if  (FMailP^.State or RS_LOGIN_PSWD=FMailP^.State) then begin
    If TransState then begin
      InputN:=RSR.ReadLine(FRSRP^.RecvBuffer,FOwner.Refactor);
      FMailP^.State:=FMailP^.State and not RS_LOGIN_PSWD;
      FMailP^.UAP^.DomainID:=FOwner.Owner.RootDomain.ID;

      If Storage.UserAccounts.Items.DB.Fill_Auth(FOwner.Task,FMailP^.UAP^) then begin
        FPassword:=Encryption.Base64.Decode(InputN);
        ProcessCredentials();
      end else begin
        FResponse:='451 Unable to validate credentials at this time.'#13#10;
        PushError;
      end;
    end else begin
      FResponse:='500 Polite people say hello before sending mail.'#13#10;
      PushError;
    end;
  end else begin
    // Partial Message or Commands
    InputN:=RSR.ReadLine(FRSRP^.RecvBuffer,FOwner.Refactor);
    ProcessCommand();
  end;
end;

procedure   TTransportReceive.RelayMessage(var Service:Storage.MatrixServices.Items.Item; Recipient:Core.Strings.VarString; var Summary:Storage.UserStorage.Items.SMTP.TSummary; var Mail:Storage.UserStorage.Items.SMTP.TRecvMessage);
begin
  New(FRelayMailP);
  Storage.UserStorage.Items.SMTP.Init(FRelayMailP^);

  FRelayMailP^.UserID:=Mail.UAP^.ID;
  FRelayMailP^.DomainID:=Mail.UAP^.DomainID;
  FRelayMailP^.InboxID:=Mail.UAP^.Inbox;
  FRelayMailP^.SpamID:=Mail.UAP^.SpamBox;
  FRelayMailP^.FolderID:=Mail.FolderID;
  FRelayMailP^.FileID:=Mail.FileID;
  FRelayMailP^.DNSIP:=Storage.DNS.Native.GetNextHost(dnskRegular);
  FRelayMailP^.State                :=RS_Lookup_MX;
  FRelayMailP^.iMXLcv               :=1;
  FRelayMailP^.iTryMax              :=MAX_MTA_RETRIES;
  FRelayMailP^.iTry                 :=1;
  FRelayMailP^.Date                 :=Core.Timer.dtNow;
  FRelayMailP^.Error                :='';
  FRelayMailP^.Domain               :=ExtractDomain(Recipient);
  FRelayMailP^.MXServer             :=FRelayMailP^.Domain;
  FRelayMailP^.Helo                 :=Mail.Exchanger;
  FRelayMailP^.From                 :=Mail.MailFrom;
  FRelayMailP^.&To                  :=Recipient;
  FRelayMailP^.Subject              :=Mail.Subject;
  FRelayMailP^.SenderIP             :=Mail.SenderIP;
  Storage.MatrixNodes.Node.Copy(Mail.UAP^.AuraNode,FRelayMailP^.Disk);

  FStamp:=Concat(
    'Received: from ',Summary.RemoteDomain,' [',Summary.RemoteIP,'] by ',
    Mail.Exchanger,' (envelope-from <',Mail.MailFrom,'>) ',
     'for <',Recipient,'> with ',App.Build.Title,' (',App.Build.Edition,' Version ',
     App.Build.Version,' RSR Build ',App.Build.RSR,') AURA SMTP; ',
     Core.Utils.Time.TimeZoneTime,
     #13#10
  );
  FData:=Concat(FStamp,Core.Arrays.VarString.toString(Mail.Content),#13#10'.'#13#10);
  FTTL:=DateUtils.IncSecond(Core.Timer.dtUT,15);
  FMeta:=Storage.UserStorage.Items.SMTP.toXML(FRelayMailP^,XML_HEADER_ON);
  Storage.MatrixQueue.Items.DB.Add(FOwner.Task,Mail.UAP^.AuraNode,FRelayService.NodeID,Storage.MatrixServices.Items.mkSMTP,FTTL,FMeta,FData,FQueueID);
end;


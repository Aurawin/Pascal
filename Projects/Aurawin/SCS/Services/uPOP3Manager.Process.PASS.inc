If TransState then begin
  FResponse:='-ERR User already logged in.'#13#10;
  PushError;
end else If (POP3P^.sUser<>'') then begin
  POP3P^.sPass:=CMDP1;
  POP3P^.UAP:=UserAccounts.Acquire(POP3P^.sUser);
  If (POP3P^.UAP<>Nil) then begin
    If Storage.UserAccounts.Items.DB.Fill_Auth(Task,POP3P^.UAP^) then begin
      If ((Length(POP3P^.sPass)>0) and (POP3P^.UAP^.Password=CMDP1)) then begin
        // They might have it right...
        If (POP3P^.UAP^.LockoutCount<=Max_LockCount) or (MinutesBetween(POP3P^.UAP^.LastAccessed,Core.Timer.dtUT)>=2) then begin
          POP3P^.UAP^.LastIP:=RSRP^.Address.sin_addr.S_addr;
          POP3P^.UAP^.LastAccessed:=Core.Timer.dtUT;
          POP3P^.UAP^.Modified:=True;
          POP3P^.UAP^.LockoutCount:=0;
          POP3P^.State:=POP3P^.State or RS_TRANSACTION or RS_MAILDROP;
          FResponse:='+OK Password accepted. User logged in.'#13#10;
          PushSend;
        end else begin
          POP3P^.UAP^.LastAccessed:=Core.Timer.dtUT;
          Inc(POP3P^.UAP^.LockoutCount);
          POP3P^.UAP^.Modified:=True;
          Storage.Intrusion.Account.DB.Add(Task,Owner.RootDomain.ID, POP3P^.UAP^.User,CMDP1,RSRP^.Address.sin_addr.S_addr);
          FResponse:='-ERR Authorization Failed.  Check your Username and Password.'#13#10;
          PushError;
        end;
      end else begin
        POP3P^.UAP^.LastAccessed:=Core.Timer.dtUT;
        Inc(POP3P^.UAP^.LockoutCount);
        POP3P^.UAP^.Modified:=True;
        Storage.Intrusion.Account.DB.Add(Task,Owner.RootDomain.ID, POP3P^.UAP^.User,CMDP1,RSRP^.Address.sin_addr.S_addr);
        FResponse:='-ERR Authorization Failed.  Check your Username and Password.'#13#10;
        PushError;
      end;
    end else begin
      FResponse:='-ERR Authorization Failed.  Service Retrieval Failed.'#13#10;
      PushError;
      OnException(
        'Process',
        'UserAccount_Fill_Auth',
        Concat(
          'Retrieval Failed',' ',
          'Domain ID : ',IntToStr(POP3P^.UAP^.DomainID),' ',
          'User ID : ',IntToStr(POP3P^.UAP^.ID),' ',
          'Password : ',POP3P^.UAP^.Password
        )
      );
    end;
  end else begin
    FResponse:='-ERR Authorization Failed.  Check your Username and Password.'#13#10;
    PushError;
  end;
end else begin
  FResponse:='-ERR User already logged in.'#13#10;
  PushError;
end;


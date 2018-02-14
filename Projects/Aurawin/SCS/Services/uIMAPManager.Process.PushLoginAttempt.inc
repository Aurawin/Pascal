procedure PushLoginAttempt();
begin
  IMAPP^.UAP:=UserAccounts.Acquire(IMAPP^.sUser);
  If (IMAPP^.UAP<>Nil) then begin
    If Storage.UserAccounts.Items.DB.Fill_Auth(Task,IMAPP^.UAP^) then begin
      if ( (IMAPP^.UAP^.ID>0) and (System.Length(IMAPP^.sPass)>0) and (IMAPP^.UAP^.Password=IMAPP^.sPass) ) then begin
        // They might have it right...
        If (IMAPP^.UAP^.LockoutCount<=Max_LockCount) or (MinutesBetween(IMAPP^.UAP^.LastAccessed,Core.Timer.dtUT)>=2) then begin
          Storage.UserAccounts.Items.DB.Fill(Task,IMAPP^.UAP^.ID,IMAPP^.UAP^);

          IMAPP^.UAP^.LastIP:=RSRP^.Address.sin_addr.S_addr;
          IMAPP^.UAP^.LastAccessed:=Core.Timer.dtUT;
          IMAPP^.UAP^.Modified:=True;
          IMAPP^.UAP^.LockoutCount:=0;

          IMAPP^.State:=IMAPP^.State or RS_AUTHENTICATED;

          FResponse:=Respond(FSEQ,SSC_OK,'Login complete.');
          PushSend();
        end else begin
          IMAPP^.UAP^.LastAccessed:=Core.Timer.dtUT;
          IMAPP^.UAP^.Modified:=True;
          FResponse:=Respond(FSEQ,SSC_NO,'Login failure: username or password rejected.');
          PushError();
        end;
      end else begin
        Storage.Intrusion.Intruder.DB.Add(Task,Owner.RootDomain.ID,RSRP^.Address.sin_addr.S_addr,Core.Timer.dtUT);
        Storage.Intrusion.Account.DB.Add(Task,Owner.RootDomain.ID,IMAPP^.sUser,IMAPP^.sPass,RSRP^.Address.sin_addr.S_addr);
        FIntrusions:=Storage.Intrusion.Account.DB.Recent(Task,Owner.RootDomain.ID,RSRP^.Address.sin_addr.S_addr);
        if (FIntrusions>Storage.Intrusion.Account.Defaults.MaxRecentBeforeBlocked) then begin
          Storage.Intrusion.Intruder.DB.Add(Task,Owner.RootDomain.ID,RSRP^.Address.sin_addr.S_addr,Core.Timer.dtUT);
          Storage.Security.Filter.Empty(FConnectionFilter);
          Try
            FConnectionFilter.Counter:=1;
            FConnectionFilter.Enabled:=True;
            FConnectionFilter.Value:=Core.Utils.Sockets.MaskClassC(IMAPP^.RemoteIP);
            if Storage.Security.Filter.DB.Exists(Task,secViolatorIP,FConnectionFilter) = false then begin
              Storage.Security.Filter.DB.Identify(Task,secViolatorIP,FConnectionFilter);
              Core.Logging.Native.WriteLogEntry(
                Owner.RootDomain.Name,
                Owner.FService,
                Concat(
                  'Security alert : Account credentials blocked for (',IMAPP^.sUser,'=',IMAPP^.sPass,') and rejected IPs (',FConnectionFilter.Value,'* )'
                )
              );
            end else begin
                Storage.Security.Filter.DB.Identify(Task,secViolatorIP,FConnectionFilter);
            end;
          Finally
            Storage.Security.Filter.Empty(FConnectionFilter);
          end;
          Close(RSRP);
        end;
        IMAPP^.UAP^.LastAccessed:=Core.Timer.dtUT;
        Inc(IMAPP^.UAP^.LockoutCount);
        IMAPP^.UAP^.Modified:=True;
        SetLength(IMAPP^.sUser,0);
        FResponse:=Respond(FSEQ,SSC_NO,'Login failure: username or password rejected.');
        PushError();
      end;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,'Login failure: DBMS service failure.');
      PushError();
      OnException(
        'Process',
        'UserAccount_Fill_Auth',
        Concat(
          'Retrieval Failed',' ',
          'Domain ID : ',IntToStr(IMAPP^.UAP^.DomainID),' ',
          'User ID : ',IntToStr(IMAPP^.UAP^.ID),' ',
          'Password : ',IMAPP^.UAP^.Password
        )
      );
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,'Login failure: System memory fault.');
    PushError();
  end;
end;


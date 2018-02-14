procedure ProcessVDMAccount();
begin
  EntryPoint:='uEngine.OnDataReceived.ProcessVDMAccount';
  if SameText(FCoreCommand,NS_CORE_CMD_ACCT_READ) then begin
    if (FXMLDocument<>nil) then begin
      try
        Storage.UserAccounts.Items.fromXML(FXMLDocument,FAccount,UA_ANY_DOMAIN,UA_ANY_ACCOUNT);
      except
        On E:Exception do begin
          Core.Logging.Native.WriteLogEntry(
            SYSTEM_LOG,
            EntryPoint,
            Format(auLang.Table.Format.AccountParseErrror,[E.Message, FAccount.ID])
          );
        end;
      end;
      if Assigned(FOnAccountRetrieved) then
        Synchronize(@SyncAccountRetrieved);
    end else
      DoCoreError(FRSRP,FCoreCode);
  end;
end;

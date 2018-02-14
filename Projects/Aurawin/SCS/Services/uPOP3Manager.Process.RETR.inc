If TransState then begin
  POP3P^.ErrorCount:=0;
  Val(CMDP1,iIndex,Code);
  If (Code=0) then begin // Nth item
    if ((iIndex>0) and (iIndex<=POP3P^.UniqueIDs.Count)) then begin
      If POP3P^.UniqueIDs.Items[iIndex-1].Deleted then begin
        FResponse:=Concat('-ERR Message ',CMDP1,' was already deleted.'#13#10);
        PushSend;
      end else begin
        EntryPoint:='Process.UserStorage_FillPOP3RetrRequest';
        Storage.UserStorage.FillPOP3RetrRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs.Items[iIndex-1].ID,POP3P^.RetrRequest);
        Try
          iSize:=Length(POP3P^.RetrRequest.Data);
          POP3P^.RetrRequest.Data+=#13#10'.'#13#10;
          Inc(iSize,5);
          POP3P^.UAP^.LastMessage:=iIndex;
          POP3P^.UAP^.Modified:=True;
          FResponse:=Concat('+OK Message ',CMDP1,' (',IntToStr(iSize),' bytes) follows...'#13#10);
          PushSend;
          Send(RSRP,POP3P^.RetrRequest.Data);
        Finally
          Empty(POP3P^.RetrRequest);
        End;
      end;
    end else begin
      FResponse:=Concat('-ERR There is no such message ',CMDP1,'.'#13#10);
      PushSend;
    end;
  end else begin
    FResponse:=Concat('-ERR RETR parameter "',CMDP1,'" is not acceptable.'#13#10);
    PushSend;
  end;
end else begin
  FResponse:=Concat('-ERR You cannot give the "',CMD,'" during the Authorization state.'#13#10);
  PushError;
end;


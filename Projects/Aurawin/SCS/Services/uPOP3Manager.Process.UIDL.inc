If TransState then begin
  POP3P^.ErrorCount:=0;
  Val(CMDP1,iIndex,Code);
  If (CMDP1='') then begin // All items
    EntryPoint:='Process.UIDL.UserStorage_FillUIDLAllRequest';
    Storage.UserStorage.FillUIDLAllRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs);
    if (POP3P^.DeleteIndex<>-1) then begin
      if (POP3P^.UAP^.LastMessage>POP3P^.UniqueIDs.Count) then begin
        POP3P^.UAP^.LastMessage:=POP3P^.UniqueIDs.Count;
        POP3P^.UAP^.Modified:=True;
      end;
      POP3P^.DeleteIndex:=-1;
    end;
    FResponse:=Concat('+OK There are ',IntToStr(POP3P^.UniqueIDs.Count),' messages in this maildrop.'#13#10);
    PushSend;
    for iLcv:=0 to POP3P^.UniqueIDs.Count-1 do begin
      FResponse:=Concat(IntToStr(iLcv+1),' ',POP3P^.UniqueIDs.Items[iLcv].UID,#13#10);
      PushSend;
    end;
    FResponse:='.'#13#10;
    PushSend;
  end else If (Code=0) then begin // Nth item
    EntryPoint:='Process.UIDL.UserStorage_FillUIDLAllRequest';
    Storage.UserStorage.FillUIDLAllRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs);
    if (POP3P^.DeleteIndex<>-1)then begin
      if (POP3P^.UAP^.LastMessage>POP3P^.UniqueIDs.Count) then begin
        POP3P^.UAP^.LastMessage:=POP3P^.UniqueIDs.Count;
        POP3P^.UAP^.Modified:=True;
      end;
      POP3P^.DeleteIndex:=-1;
    end;
    If (iIndex>0) and (iIndex<=POP3P^.UniqueIDs.Count) then begin
      POP3P^.UIDLRequest.User_ID:=POP3P^.UAP^.ID;
      POP3P^.UIDLRequest.Field_ID:=POP3P^.UniqueIDs.Items[iIndex-1].ID;
      EntryPoint:='Process.UserStorage_FillPOP3UIDLRequest';
      Storage.UserStorage.FillPOP3UIDLRequest(Task,POP3P^.UIDLRequest);
      if Length(POP3P^.UIDLRequest.Data)=0 then begin
        FResponse:=Concat('-ERR There is no such message ',CMDP1,'.'#13#10);
        PushSend;
      end else begin
        FResponse:=Concat('+OK ',CMDP1,' ',POP3P^.UIDLRequest.Data,#13#10);
        PushSend;
      end;
    end else begin
      FResponse:=Concat('-ERR There is no message ',CMDP1,'.'#13#10);
      PushSend;
    end;
  end else begin
      FResponse:=Concat('-ERR UIDL parameter "',CMDP1,'" is not acceptable.'#13#10);
    PushError;
  end;
end else begin
  FResponse:=Concat('-ERR You cannot give the "',CMD,'" during the Authorization state.'#13#10);
  PushError;
end;


If TransState then begin
  POP3P^.ErrorCount:=0;
  If (CMDP1='') then begin // All items
    EntryPoint:='Process.LIST.UserStorage_FillUIDLAllRequest';
    Storage.UserStorage.FillUIDLAllRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs);
    if (POP3P^.DeleteIndex<>-1) then begin
      if (POP3P^.UAP^.LastMessage>POP3P^.UniqueIDs.Count) then begin
        POP3P^.UAP^.LastMessage:=POP3P^.UniqueIDs.Count;
        POP3P^.UAP^.Modified:=True;
      end;
      POP3P^.DeleteIndex:=-1;
    end;
    FResponse:=Concat('+OK There are ',IntToStr(POP3P^.UniqueIDs.Count),' messages (',IntToStr(POP3P^.UniqueIDs.Size),' bytes) in this dropbox including messages marked for deletion.',#13#10);
    PushSend;
    for iLcv := 0 to POP3P^.UniqueIDs.Count-1 do begin
      FResponse:=Concat(IntToStr(iLcv+1),' ', IntToStr(POP3P^.UniqueIDs.Items[iLcv].Size),#13#10);
      PushSend;
    end;
    FResponse:='.'#13#10;
    PushSend;
  end else begin
    Val(CMDP1,iIndex,Code);
    If iIndex=0 then begin
      SetLength(CMDP1,0);
      Code:=-1;
    end;
    If (Code=0) then begin // Nth item
      // No need to lock
      EntryPoint:='Process.LIST.UserStorage_FillUIDLAllRequest';
      Storage.UserStorage.FillUIDLAllRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs);
      if (POP3P^.DeleteIndex<>-1) then begin
        if (POP3P^.UAP^.LastMessage>POP3P^.UniqueIDs.Count) then begin
          POP3P^.UAP^.LastMessage:=POP3P^.UniqueIDs.Count;
          POP3P^.UAP^.Modified:=True;
        end;
        POP3P^.DeleteIndex:=-1;
      end;
      If (iIndex>0) and (iIndex<=POP3P^.UniqueIDs.Count) then begin
        FResponse:=Concat('+OK ',CMDP1,' ',IntToStr(POP3P^.UniqueIDs.Items[iIndex-1].Size),#13#10);
        PushSend;
      end else begin
        FResponse:=Concat('-ERR There is no message ',CMDP1,'.'#13#10);
        PushSend;
      end;
    end else begin
      FResponse:=Concat('-ERR LIST parameter "',CMDP1,'" is not acceptable.'#13#10);
      PushSend;
    end;
  end;
end else begin
  FResponse:=Concat('-ERR You cannot give the "',CMD,'" during the Authorization state.'#13#10);
  PushError;
end;


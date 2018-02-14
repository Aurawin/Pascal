If TransState then begin
  POP3P^.ErrorCount:=0;
  Storage.UserStorage.FillUIDLAllRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs);
  if (POP3P^.DeleteIndex<>-1) then begin
    if (POP3P^.UAP^.LastMessage>POP3P^.UniqueIDs.Count) then begin
      POP3P^.UAP^.LastMessage:=POP3P^.UniqueIDs.Count;
      POP3P^.UAP^.Modified:=True;
    end;
    POP3P^.DeleteIndex:=-1;
  end;
  FResponse:=Concat('+OK ',IntToStr(POP3P^.UAP^.LastMessage),#13#10);
  PushSend;
end else begin
  FResponse:=Concat('-ERR You cannot give the "',CMD,'" during the Authorization state.'#13#10);
  PushError;
end;


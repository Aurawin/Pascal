If TransState then begin
  POP3P^.ErrorCount:=0;
  POP3P^.DeleteIndex:=-1;
  POP3P^.UAP^.LastMessage:=0;
  POP3P^.UAP^.Modified:=True;
  EntryPoint:='Process.CLEAR.UserStorage_Execute_POP3_Clear';
  Storage.UserStorage.Execute_POP3_Clear(Task,POP3P^.UAP^.AuraNode,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox);
  Empty(POP3P^.UniqueIDs);
  FResponse:='+OK CLEAR complete.'#13#10;
  PushSend;
end else begin
  FResponse:='-ERR You cannot give the CLEAR command during the Authorization state!'#13#10;
  PushError;
end;


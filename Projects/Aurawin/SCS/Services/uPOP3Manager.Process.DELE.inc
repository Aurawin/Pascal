If TransState then begin
  POP3P^.ErrorCount:=0;
  Val(CMDP1,iIndex,Code);
  If (Code=0) then begin // Nth item
    If (iIndex>0) and (iIndex<=POP3P^.UniqueIDs.Count) then begin
      if POP3P^.UniqueIDs.Items[iIndex-1].Deleted then begin
        FResponse:=Concat('+OK Message ',CMDP1,' was already marked for deletion.'#13#10);
        PushSend;
      end else begin
        POP3P^.DeleteIndex:=iIndex;
        POP3P^.UAP^.LastMessage:=iIndex;
        POP3P^.UAP^.Modified:=True;

        POP3P^.UniqueIDs.Items[iIndex-1].Deleted:=True;
        EntryPoint:='Process.UserStorage_Execute_POP3_Delete';
        Storage.UserStorage.Execute_POP3_Delete(Task,POP3P^.UAP^.AuraNode,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs.Items[iIndex-1].ID);

        FResponse:=Concat('+OK Message ',CMDP1,' was marked for deletion.'#13#10);
        PushSend;
      end;
    end else begin
      FResponse:=Concat('-ERR There is no such message ',CMDP1,'.'#13#10);
      PushSend;
    end;
  end else begin
    FResponse:=Concat('-ERR DELE parameter "',CMDP1,'" is not acceptable.'#13#10);
    PushSend;
  end;
end else begin
  FResponse:=Concat('-ERR You cannot give the "',CMD,'" during the Authorization state.'#13#10);
  PushError;
end;


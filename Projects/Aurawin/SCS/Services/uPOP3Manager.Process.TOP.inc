If TransState then begin
  POP3P^.ErrorCount:=0;
  Val(CMDP1,iIndex,Code);
  If (Code=0) then begin // Nth item
    If iIndex>0 then begin
      Val(CMDP2,iCount,Code);
      If (Code=0) then begin
        If (iCount>=0) then begin
          EntryPoint:='Process.TOP.UserStorage_FillUIDLAllRequest';
          Storage.UserStorage.FillUIDLAllRequest(Task,POP3P^.UAP^.DomainID,POP3P^.UAP^.ID,POP3P^.UAP^.Inbox,POP3P^.UniqueIDs);
          if (POP3P^.DeleteIndex<>-1) then begin
            if (POP3P^.UAP^.LastMessage>POP3P^.UniqueIDs.Count) then begin
              POP3P^.UAP^.LastMessage:=POP3P^.UniqueIDs.Count;
              POP3P^.UAP^.Modified:=True;
            end;
            POP3P^.DeleteIndex:=-1;
          end;
          if (iIndex<=POP3P^.UniqueIDs.Count) then begin
            POP3P^.TopRequest.DomainID:=POP3P^.UAP^.DomainID;
            POP3P^.TopRequest.FolderID:=POP3P^.UAP^.Inbox;
            POP3P^.TopRequest.UserID:=POP3P^.UAP^.ID;
            POP3P^.TopRequest.ID:=POP3P^.UniqueIDs.Items[iIndex-1].ID;
            EntryPoint:='Process.UserStorage_FillPOP3TopRequest';
            Storage.UserStorage.FillPOP3TopRequest(Task,POP3P^.UAP^.AuraNode,POP3P^.TopRequest);
            Try
              FResponse:=Concat('+OK ','Top of message ',CMDP1,' follows...'#13#10);
              PushSend;
              if iCount>0 then
                ClipBuffer(iCount,POP3P^.TopRequest.Data);
              FResponse:=Concat(POP3P^.TopRequest.Data,'.'#13#10);
              PushSend;
            Finally
              Empty(POP3P^.TopRequest);
            end;
          end else begin
            FResponse:=Concat('-ERR There is no such message ',CMDP1,'.'#13#10);
            PushError;
          end;
        end else begin
          FResponse:=Concat('-ERR TOP msg n syntax error. msg="',CMDP1, '" and n="',CMDP2,'" Message argument is invalid.'#13#10);
          PushError;
        end;
      end else begin
        FResponse:=Concat('-ERR TOP msg n syntax error. msg="',CMDP1, '" and n="',CMDP2,'" Message argument is invalid.'#13#10);
        PushError;
      end;
    end else begin
      FResponse:=Concat('-ERR There is no such message ',CMDP1,'.'#13#10);
      PushError;
    end;
  end else begin
    FResponse:=Concat('-ERR TOP parameter "',CMDP1,'" is not acceptable.'#13#10);
    PushError;
  end;
end else begin
  FResponse:=Concat('-ERR You cannot give the "',CMD,'" during the Authorization state.'#13#10);
  PushError;
end;


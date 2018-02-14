If TransState then begin
  Empty(POP3P^.UniqueIDs);
  FResponse:='+OK RSET accepted.'#13#10;
  PushSend;
end else begin
  FResponse:='-ERR You cannot give the RSET command during the Authorization state!'#13#10;
  PushError;
end;


If TransState then begin
  FResponse:='-ERR User already logged in.'#13#10;
  PushError;
end else begin
  POP3P^.sUser:=CMDP1;
  If CMDP1<>'' then begin
    FResponse:='+OK Username accepted.'#13#10;
    PushSend;
  end else begin
    FResponse:='-ERR Please specify a username.'#13#10;
    PushError;
  end;
end;


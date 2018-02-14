If TransState then begin
  FResponse:='+OK NOOP accepted.'#13#10;
  PushSend;
end else begin
  FResponse:='-ERR You cannot give the NOOP command during the Authorization state!'#13#10;
  PushError;
end;


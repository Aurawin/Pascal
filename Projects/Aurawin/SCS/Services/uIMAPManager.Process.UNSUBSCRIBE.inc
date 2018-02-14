if TransState()=true then begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('UNSUBSCRIBE failed: no subscriptions available'));
  PushSend();
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('UNSUBSCRIBE failed: you must first login.'));
  PushSend();
end;

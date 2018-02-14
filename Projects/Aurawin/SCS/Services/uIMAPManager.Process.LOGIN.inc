If TransState then begin
  FResponse:=Respond(FSEQ,SSC_NO,'A user already logged in.');
  PushError();
end else begin
  If (CMDP1='') then begin
    FResponse:=Respond(FSEQ,SSC_BAD,'You must specify username...');
    PushError();
  end else If (CMDP2='') then begin
    FResponse:=Respond(FSEQ,SSC_BAD,'You must specify password...');
    PushError();
  end else begin
    IMAPP^.sUser:=CMDP1;
    IMAPP^.sPass:=CMDP2;
    PushLoginAttempt();
  end;
end;


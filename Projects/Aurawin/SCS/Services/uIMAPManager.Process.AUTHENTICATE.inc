If TransState then begin
  FResponse:=Respond(FSEQ,SSC_NO,'A user already logged in.');
  Send(RSRP,FResponse);
end else begin
  If Core.Strings.SameText(CMDP1,Commands.Authenticate.Plain) then begin
    IMAPP^.State:=IMAPP^.State or RS_AUTHENTICATE;
    FResponse:=Concat('+',#13#10);
    Send(RSRP,FResponse);
  end else If Core.Strings.SameText(CMDP1,Commands.Authenticate.Login) then begin
    IMAPP^.State:=IMAPP^.State or RS_AUTHENTICATE;
    FResponse:=Concat('+',#13#10);
    Send(RSRP,FResponse);
  end else If Core.Strings.SameText(CMDP1,Commands.Authenticate.DigestMD5) then begin
    IMAPP^.Nonce:=RSR.Generate_Nonce(NONCE_LOW,NONCE_HIGH);
    FResponse:=Concat('realm="',Owner.RootDomain.Name,'", nonce="',IMAPP^.Nonce,'",qop="auth",algorithm=md5,charset=',Storage.Main.Header.Encoding);
    FResponse:=Encryption.Base64.Encode(FResponse);
    FResponse:=Concat('+',FResponse,#13#10);
    Send(RSRP,FResponse);
    IMAPP^.State:=IMAPP^.State or RS_AUTHENTICATE;
  end else begin
    FResponse:=Respond(SEQ_ANY,SSC_NO,'Only "PLAIN" mechanism currently supported');
    Send(RSRP,FResponse);
  end;
end;


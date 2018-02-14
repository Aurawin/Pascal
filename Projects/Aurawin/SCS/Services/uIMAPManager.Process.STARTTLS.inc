If (Length(Owner.FSSLInfo.Manifest.List)=0)then begin
  FResponse:=Respond(FSEQ,SSC_BAD,'Administrator has not specified a certificate.');
  PushError();
end else if (Owner.FSSLInfo.keyLen=0)then begin
  FResponse:=Respond(FSEQ,SSC_BAD,'Administrator has not specified an encryption key.');
  PushError();
end else begin
  FResponse:=Respond(FSEQ,SSC_OK,'Begin TLS negotiation now.');
  PushSend();
  StartTLS(RSRP);
end;


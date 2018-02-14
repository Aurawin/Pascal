If ((RSRP^.State or RSR_STATE_SECURE)<>RSRP^.State) then begin
  //FResponse:=Concat('-ERR service certificate not complete.',#13#10);
//  PushError();
  If (Owner.FSSLInfo.Manifest.Kind=sslckNone)then begin
    FResponse:=Concat('-ERR Administrator needs to create a certificate.',#13#10);
    PushError();
  end else If (Owner.FSSLInfo.keyLen=0)then begin
    FResponse:=Concat('-ERR Administrator needs to assign an encryption key.',#13#10);
    PushError();
  end else If (Owner.FSSLInfo.Manifest.List[0]^.crtLen=0)then begin
    FResponse:=Concat('-ERR Administrator needs to sign the certificate request.',#13#10);
    PushError();
  end else begin
    FResponse:=Concat('+OK Begin TLS Handshake',#13#10);
    PushSend();
    StartTLS(RSRP);
  end;
end else begin
  FResponse:=Concat('-ERR service already encrypted.',#13#10);
  PushError();
end;


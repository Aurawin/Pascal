itmP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
itmP^.Date:=Fields.FieldByName(Items.DB.Keys.Date).AsFloat;
itmP^.Key:=Fields.FieldByName(Items.DB.Keys.Key).AsString;

Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerKey).AsString,itmP^.DerKey);


itmP^.Request:=Fields.FieldByName(Items.DB.Keys.Request).AsString;
itmP^.Level:=Fields.FieldByName(Items.DB.Keys.Level).AsInteger;
SetLength(itmP^.Certs,itmP^.Level);
SetLength(itmP^.DerCerts,itmP^.Level);
case itmP^.Level of
  1: begin
    itmP^.Certs[0]:=Fields.FieldByName(Items.DB.Keys.Cert1).AsString;
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert1).AsString,itmP^.DerCerts[0]);
  end;
  2: begin
    itmP^.Certs[0]:=Fields.FieldByName(Items.DB.Keys.Cert1).AsString;
    itmP^.Certs[1]:=Fields.FieldByName(Items.DB.Keys.Cert2).AsString;

    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert1).AsString,itmP^.DerCerts[0]);
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert2).AsString,itmP^.DerCerts[1]);
  end;
  3: begin
    itmP^.Certs[0]:=Fields.FieldByName(Items.DB.Keys.Cert1).AsString;
    itmP^.Certs[1]:=Fields.FieldByName(Items.DB.Keys.Cert2).AsString;
    itmP^.Certs[2]:=Fields.FieldByName(Items.DB.Keys.Cert3).AsString;

    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert1).AsString,itmP^.DerCerts[0]);
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert2).AsString,itmP^.DerCerts[1]);
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert3).AsString,itmP^.DerCerts[2]);


  end;
  4: begin
    itmP^.Certs[0]:=Fields.FieldByName(Items.DB.Keys.Cert1).AsString;
    itmP^.Certs[1]:=Fields.FieldByName(Items.DB.Keys.Cert2).AsString;
    itmP^.Certs[2]:=Fields.FieldByName(Items.DB.Keys.Cert3).AsString;
    itmP^.Certs[3]:=Fields.FieldByName(Items.DB.Keys.Cert4).AsString;

    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert1).AsString,itmP^.DerCerts[0]);
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert2).AsString,itmP^.DerCerts[1]);
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert3).AsString,itmP^.DerCerts[2]);
    Encryption.Base64.Decode(Fields.FieldByName(Items.DB.Keys.DerCert4).AsString,itmP^.DerCerts[3]);
  end;
end;


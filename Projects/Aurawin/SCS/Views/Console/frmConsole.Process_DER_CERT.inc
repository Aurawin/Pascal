procedure Process_DER_CERT();
begin
procSelfSign.Executable:='openssl';
procSelfSign.Parameters.Clear();
procSelfSign.Parameters.Add('x509');
procSelfSign.Parameters.Add('-in');
procSelfSign.Parameters.Add(sCertFile);
procSelfSign.Parameters.Add('-inform');
procSelfSign.Parameters.Add('PEM');
procSelfSign.Parameters.Add('-out');
procSelfSign.Parameters.Add(sDerCertFile);
procSelfSign.Parameters.Add('-outform');
procSelfSign.Parameters.Add('DER');
procSelfSign.Execute();
end;


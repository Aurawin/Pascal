procedure AuraProcess.OnServerListening(Server:TRSRServer);
begin
  {$ifdef Unix}
    BaseUnix.FpSetuid(StrToIntDef(FProcessUserID.Value,0));
    BaseUnix.FpSetgid(StrToIntDef(FProcessGroupID.Value,0));
  {$endif}
end;

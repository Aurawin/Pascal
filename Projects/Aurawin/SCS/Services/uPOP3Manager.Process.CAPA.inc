POP3P^.ErrorCount:=0;
FResponse:=Concat(
  '+OK Capability List of Available Commands',#13#10,
  //'AUTH',#13#10,
  //'SASL PLAIN',#13#10,
  'STLS',#13#10,
  'STARTTLS',#13#10,
  'TOP',#13#10,
  'USER',#13#10,
  'PIPELINING',#13#10,
  'UIDL',#13#10,
  'ID',#13#10,
  'LAST',#13#10,
  'RSET',#13#10,
  'CLEAR',#13#10,
  'VERS'#13#10,
  '.',#13#10
);
PushSend;


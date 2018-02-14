if POP3P^.UAP<>nil then
  FResponse:=Concat('+OK User ',POP3P^.UAP^.User,' (',IntToStr(POP3P^.UAP^.ID),')' ,#13#10)
else
  FResponse:=Concat('-ERR no such user.',#13#10);
PushSend();


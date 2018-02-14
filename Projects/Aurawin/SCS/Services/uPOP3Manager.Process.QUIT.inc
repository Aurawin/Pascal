FResponse:='+OK Quit accepted gracefully.  Good bye!'#13#10;
PushSend;
POP3P^.State:=POP3P^.State or RS_EndSession;


IMAPP^.State:=IMAPP^.State or RS_LOGOUT;
FResponse:=Respond(SEQ_ANY,SSC_BYE,'Server logging out...');
PushSend();
FResponse:=Respond(FSEQ,SSC_OK,'LOGOUT complete.');
PushSend();
IMAPP^.State:=IMAPP^.State or RS_EndSession;


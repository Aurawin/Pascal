IMAPP^.ErrorCount:=0;
FResponse:=Concat(
  SEQ_ANY,' ',
  'CAPABILITY',' ',
  Capabilities,#13#10,
  Respond(FSEQ,SSC_OK,'CAPABILITY commmand complete.')
);
PushSend();


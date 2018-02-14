procedure SigPipeHandler(sig:cInt); cdecl;
begin
  // nothing
end;

procedure InstallSigPipeBlock();
Var
   oa,na : PSigActionRec;
begin
  new(na);
  new(oa);
  na^.sa_Handler:=SigActionHandler(@SigPipeHandler);
  fillchar(na^.Sa_Mask,sizeof(na^.sa_mask),#0);
  na^.Sa_Flags:=0;
  {$ifdef Linux}               // Linux specific
    na^.Sa_Restorer:=Nil;
  {$endif}
  fpSigAction(SigPipe,na,oa);
end;


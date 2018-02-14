Function   TIMAPManager.Respond(sSequence,sCode,sResult:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat(sSequence,' ',sCode,' ',sResult,#13#10);
end;

Function   TIMAPManager.Respond(sResult:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat(SEQ_ANY,' ',sResult,#13#10);
end;


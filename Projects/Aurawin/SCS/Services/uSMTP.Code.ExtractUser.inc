Function    ExtractUser (Recipient:Core.Strings.VarString):Core.Strings.VarString;
var
  Loc:LongInt;
begin
  Loc:=Core.Strings.Pos('@',Recipient);
  Result:=System.Copy(Recipient,1,Loc-1);
end;


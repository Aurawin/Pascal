procedure   TIMAPManager.ClipBuffer(Const CRLFCount:LongInt; Var Buffer:Core.Strings.VarString);
var
  iEnd,Count,Len,Lcv:LongInt;
begin
  Len:=Length(Buffer);  Lcv:=1; Count:=0; iEnd:=0;
  While (Count<CRLFCount) and (Lcv<=Len) do begin
    If Buffer[Lcv]=#10 then
      Inc(Count);
    Inc(Lcv);
    Inc(iEnd);
  end;
  SetLength(Buffer,iEnd);
end;


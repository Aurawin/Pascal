if (soBraceWraps in Defaults) then begin
  iBraceStart:=StrUtils.PosEx('{',sData,iPosition);
  BraceWraps:=(iBraceStart>0) and (iBraceStart<=iLoc);
end else
  BraceWraps:=false;


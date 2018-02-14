if (soBracketWraps in Defaults) then begin
  iBracketStart:=StrUtils.PosEx('[',sData,iPosition);
  BracketWraps:=(iBracketStart>0) and (iBracketStart<=iLoc);
end else
  BracketWraps:=false;


if (soParenWraps in Defaults) then begin
  iParenStart:=StrUtils.PosEx('(',sData,iPosition);
  ParenWraps:=(iParenStart>0) and (iParenStart<=iLoc);
end else
  ParenWraps:=false;

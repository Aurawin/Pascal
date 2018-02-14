iEnd:=StrUtils.PosEx('"',sData,iPosition);
Result:=System.Copy(sData,iBraceStart+1,iEnd-(iQuoteStart+1));
Core.Strings.Trim(Result);
iPosition:=iEnd+1;
If StrUtils.PosEx(Delim,sData,iPosition)=iPosition then
  Inc(iPosition);


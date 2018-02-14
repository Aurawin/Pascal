if (soQuoteWraps in Defaults) then begin
  iQuoteStart:=StrUtils.PosEx('"',sData,iPosition);
  QuoteWraps:=(iQuoteStart>0) and (iQuoteStart<=iLoc);
end else
  QuoteWraps:=false;


iWrapBias:=0;  iEnd:=0;
iWrapLcv:=iBracketStart+1;
while (iWrapLcv<=iLen) do begin
  If (sData[iWrapLcv]='[') then
    Inc(iWrapBias)
  else if (sData[iWrapLcv]=']') then begin
    Dec(iWrapBias);
    if iWrapBias<0 then begin
      iEnd:=iWrapLcv;
      if (sData[iPosition]='[') then
        Dec(iEnd);
      iWrapLcv:=iLen;
    end;
  end;
  Inc(iWrapLcv);
end;
if iEnd=0 then
  iEnd:=iLen;
if (sData[iPosition]='[') then
  inc(iPosition);
Result:=System.Copy(sData,iPosition,(iEnd-iPosition)+1);
Core.Strings.Trim(Result);
iPosition:=iEnd+1;
If StrUtils.PosEx(Delim,sData,iPosition)=iPosition then
  Inc(iPosition);

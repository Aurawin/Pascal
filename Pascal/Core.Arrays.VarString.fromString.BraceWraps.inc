iWrapBias:=0;  iEnd:=0;
iWrapLcv:=iBraceStart+1;
while (iWrapLcv<=iLen) do begin
  If (sData[iWrapLcv]='{') then
    Inc(iWrapBias)
  else if (sData[iWrapLcv]='}') then begin
    Dec(iWrapBias);
    if (iWrapBias<0) then begin
      iEnd:=iWrapLcv-1;
      iWrapLcv:=iLen;
      iAdjBias:=1;
    end;
  end;
  Inc(iWrapLcv);
end;
if iEnd=0 then
  iEnd:=iLen;

iWrapBias:=0;
if (sData[iPosition]='{') then begin
  iWrapBias+=1;
  inc(iPosition);
end;
Result:=System.Copy(sData,iPosition,(iEnd-iPosition)+iWrapBias);
Core.Strings.Trim(Result);
iPosition:=iEnd+iWrapBias+iAdjBias;
If StrUtils.PosEx(Delim,sData,iPosition)=iPosition then
  Inc(iPosition);

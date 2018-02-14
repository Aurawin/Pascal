iLen:=Length(Data);
Repeat
  If (iLen>0) then begin
    If Data[iLen]=#32 then
      Dec(iLen);
  end;
Until (iLen=0) or (Data[iLen]<>#32);
SetLength(Data,iLen);


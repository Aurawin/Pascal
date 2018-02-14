FCacheWIX:=0;
Error:=False;
Empty(Header);
For iLcv:=Low(Questions) to High(Questions) do With  Questions[iLcv] do begin
  Q_Class:=0;
  Q_Type:=0;
  SetLength(Q_Name,0);
end;
SetLength(Questions,1);

SetLength(QD_Data,0);
SetLength(AN_Data,0);
SetLength(NS_Data,0);
SetLength(AD_Data,0);

SetLength(Answers,0);


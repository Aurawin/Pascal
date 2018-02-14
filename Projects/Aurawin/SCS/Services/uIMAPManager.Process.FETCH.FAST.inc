for iLcv:=FRangeStart-1 to FRangeEnd-1 do begin
  if (iLcv<FFileCount) then begin
    FFileP:=FFolderP^.Files[iLcv];
    OutputFetchFast(RSRP,iLcv,FFileP^);
  end else begin
    FResponse:=Respond(Concat(IntToStr(iLcv),' FETCH cannot retrieve data for this message.'));
    PushSend();
  end;
end;
FResponse:=Respond(FSEQ,SSC_OK,Concat('FETCH completed.'));
PushSend();


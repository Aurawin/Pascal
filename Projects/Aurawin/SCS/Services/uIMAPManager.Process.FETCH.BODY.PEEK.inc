for iLcv:=FRangeStart-1 to FRangeEnd-1 do begin
  Storage.UserStorage.Items.SMTP.Empty(FSummary);
  Storage.UserStorage.Items.IMAP.Empty(FEnvelope);
  Storage.UserStorage.Items.IMAP.Empty(FBody);
  if (iLcv<FFileCount) then begin
    FFileP:=FFolderP^.Files[iLcv];
    ParseSummary(FFileP^.Summary,FSummary);
    ParseEnvelope(FLines,FSummary,FEnvelope);
    ParseBody(FSummary,FBody);
    OutputFetchBodyStructure(RSRP,iLcv,FBody);
  end else begin
    FResponse:=Respond(Concat(IntToStr(iLcv),' FETCH cannot retrieve data for this message.'));
    PushSend();
  end;
end;
FResponse:=Respond(FSEQ,SSC_OK,Concat('FETCH completed.'));
PushSend();


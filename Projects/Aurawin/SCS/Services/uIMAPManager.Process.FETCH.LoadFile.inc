if Length(FFileP^.Data)=0 then begin
  // Get File Data
  if Storage.UserStorage.Files.Data(IMAPP^.ULIP^.User.AuraNode, IMAPP^.ULIP^.User.DomainID,IMAPP^.ULIP^.User.ID,FFolderP^.ID,FFileP^.ID,FSData) then begin
    FFileP^.Data:=Core.Streams.toString(FSData);
    FreeAndNil(FSData);
  end;
end;


With Kind.DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.NameSpace:=Fields.FieldByName(NameSpace).AsString;
end;

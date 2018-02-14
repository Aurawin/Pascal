With Connection.DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(NetworkID).AsLargeInt;
  ItmP^.OwnerID:=Fields.FieldByName(OwnerID).AsLargeInt;
  ItmP^.Created:=Fields.FieldByName(Created).AsFloat;
  ItmP^.Accepted:=Fields.FieldByName(Accepted).AsFloat;
end;

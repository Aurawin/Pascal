With Bann.DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(NetworkID).AsLargeInt;
  ItmP^.AdminID:=Fields.FieldByName(AdminID).AsLargeInt;
  ItmP^.UserID:=Fields.FieldByName(UserID).AsLargeInt;
  ItmP^.Posted:=Fields.FieldByName(Posted).AsFloat;
  ItmP^.Expires:=Fields.FieldByName(Expires).AsFloat;
end;

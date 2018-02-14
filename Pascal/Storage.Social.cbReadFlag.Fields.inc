With Flags.DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(NetworkID).AsLargeInt;
  ItmP^.UserID:=Fields.FieldByName(UserID).AsLargeInt;
  ItmP^.TargetID:=Fields.FieldByName(TargetID).AsLargeInt;
  ItmP^.Kind:=Fields.FieldByName(Kind).AsLargeInt;
  ItmP^.Posted:=Fields.FieldByName(Posted).AsFloat;
end;

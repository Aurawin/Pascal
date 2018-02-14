With Cache.DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.QueryID:=Fields.FieldByName(QueryID).AsLargeInt;

  ItmP^.ProviderID:=Fields.FieldByName(ProviderID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(NetworkID).AsLargeInt;
  ItmP^.GroupID:=Fields.FieldByName(GroupID).AsLargeInt;
  ItmP^.SessionID:=Fields.FieldByName(SessionID).AsLargeInt;
  ItmP^.UserID:=Fields.FieldByName(UserID).AsLargeInt;

  ItmP^.Created:=Fields.FieldByName(Created).AsFloat;
  ItmP^.Modified:=Fields.FieldByName(Modified).AsFloat;
  ItmP^.Expires:=Fields.FieldByName(Expires).AsFloat;
  ItmP^.Executed:=Fields.FieldByName(Executed).AsFloat;
end;

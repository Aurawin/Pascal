With DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(NetworkID).AsLargeInt;
  ItmP^.QueryID:=Fields.FieldByName(QueryID).AsLargeInt;
  ItmP^.ResponseID:=Fields.FieldByName(ResponseID).AsLargeInt;
  ItmP^.Opened:=Fields.FieldByName(Opened).AsFloat;
  ItmP^.Closed:=Fields.FieldByName(Closed).AsFloat;
  ItmP^.Expires:=Fields.FieldByName(Expires).AsFloat;
  ItmP^.Flags:=Fields.FieldByName(Flags).AsInteger;

  ItmP^.Query:=Fields.FieldByName(Query).AsString;
  ItmP^.Response:=Fields.FieldByName(Response).AsString;
end;

With ItemP^ do begin
  ID:=Fields.FieldByName(Resource.DB.Keys.ID).AsLargeInt;
  ClusterID:=Fields.FieldByName(Resource.DB.Keys.ClusterID).AsLargeInt;
  Name:=Fields.FieldByName(Resource.DB.Keys.Name).AsString;
  Verified:=True;
end;

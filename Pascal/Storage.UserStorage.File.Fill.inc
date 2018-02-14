with itmP^ do begin
  ID:=Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt;
  FolderID:=Fields.FieldByName(Files.DB.Keys.FolderID).AsLargeInt;
  Size:=Fields.FieldByName(Files.DB.Keys.Size).AsLargeInt;
  Kind:=Fields.FieldByName(Files.DB.Keys.Kind).AsInteger;
  Flags:=Fields.FieldByName(Files.DB.Keys.Flags).AsInteger;
  Created:=Fields.FieldByName(Files.DB.Keys.Created).AsFloat;
  Modified:=Fields.FieldByName(Files.DB.Keys.Modified).AsFloat;
  Allocated:=Fields.FieldByName(Files.DB.Keys.Allocated).AsFloat;
  Name:=Fields.FieldByName(Files.DB.Keys.Name).AsString;
  Files.fromString(Digest,Fields.FieldByName(Files.DB.Keys.Digest).AsString);
  Valid:=true;
end;

with itmP^ do begin
  ID:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
  NetworkID:=Fields.FieldByName(DB.Keys.NetworkID).AsLargeInt;
  OwnerID:=Fields.FieldByName(DB.Keys.OwnerID).AsLargeInt;
  FolderID:=Fields.FieldByName(DB.Keys.FolderID).AsLargeInt;
  Size:=Fields.FieldByName(DB.Keys.Size).AsLargeInt;
  Kind:=Fields.FieldByName(DB.Keys.Kind).AsInteger;
  Created:=Fields.FieldByName(DB.Keys.Created).AsFloat;
  Modified:=Fields.FieldByName(DB.Keys.Modified).AsFloat;
  Allocated:=Fields.FieldByName(DB.Keys.Allocated).AsFloat;
  Name:=Fields.FieldByName(DB.Keys.Name).AsString;
  Encryption.Base64.Decode(Fields.FieldByName(DB.Keys.Digest).AsString,Digest);
end;

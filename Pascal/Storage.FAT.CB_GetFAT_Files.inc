dsFile.Name:=Fields.FieldByName(Files.DB.Keys.Name).AsString;
dsFile.Attributes:=Fields.FieldByName(Files.DB.Keys.Attributes).AsLargeInt;
dsFile.ContentType:=Fields.FieldByName(Files.DB.Keys.ContentType).AsInteger;
dsFile.Imported:=Fields.FieldByName(Files.DB.Keys.Imported).AsInteger;
dsFile.Cache:=Fields.FieldByName(Files.DB.Keys.Cache).AsBoolean;
dsFile.CacheTTL:=Fields.FieldByName(Files.DB.Keys.CacheTTL).AsInteger;
dsFile.Size:=Fields.FieldByName(Files.DB.keys.Size).AsInteger;
dsFile.HasKeywords:=Fields.FieldByName(Files.DB.Keys.HasKeywords).AsBoolean;
dsFile.Deflate:=Fields.FieldByName(Files.DB.Keys.Deflate).AsBoolean;
dsFile.Created:=Fields.FieldByName(Files.DB.Keys.Created).AsFloat;
dsFile.Modified:=Fields.FieldByName(Files.DB.Keys.Modified).AsFloat;
Encryption.Base64.Decode(Fields.FieldByName(Files.DB.Keys.Digest).AsString,dsFile.Digest);


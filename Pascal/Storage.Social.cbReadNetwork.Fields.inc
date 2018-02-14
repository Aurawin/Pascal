With DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.OwnerID:=Fields.FieldByName(OwnerID).AsLargeInt;
  ItmP^.AvatarID:=Fields.FieldByName(AvatarID).AsLargeInt;
  ItmP^.Created:=Fields.FieldByName(Created).AsFloat;
  ItmP^.Modified:=Fields.FieldByName(Modified).AsFloat;
  ItmP^.MemberCount:=Fields.FieldByName(MemberCount).AsInteger;

  ItmP^.DocumentsID:=Fields.FieldByName(DocumentsID).AsLargeInt;
  ItmP^.TrashID:=Fields.FieldByName(TrashID).AsLargeInt;
  ItmP^.MusicID:=Fields.FieldByName(MusicID).AsLargeInt;
  ItmP^.PicturesID:=Fields.FieldByName(PicturesID).AsLargeInt;
  ItmP^.VideosID:=Fields.FieldByName(VideosID).AsLargeInt;

  ItmP^.Privacy:=Fields.FieldByName(Privacy).AsInteger;
  ItmP^.Title:=Fields.FieldByName(Title).AsString;
  ItmP^.Description:=Fields.FieldByName(Description).AsString;

  Encryption.Base64.Decode(Fields.FieldByName(Tag).asString,ItmP^.Tag);

end;

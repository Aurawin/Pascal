With Conversation.DB.Keys do begin
  ItmP^.ID:=Fields.FieldByName(ID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(NetworkID).AsLargeInt;
  ItmP^.UserID:=Fields.FieldByName(UserID).AsLargeInt;
  ItmP^.ParentID:=Fields.FieldByName(ParentID).AsLargeInt;
  ItmP^.Posted:=Fields.FieldByName(Posted).AsFloat;
  ItmP^.Content:=Fields.FieldByName(Content).AsString;
end;

ItemP^.Number:=Fields.FieldByName(Card.DB.Keys.Number).AsLargeInt;
ItemP^.ExpMonth:=Fields.FieldByName(Card.DB.Keys.ExpMonth).AsInteger;
ItemP^.ExpYear:=Fields.FieldByName(Card.DB.Keys.ExpYear).AsInteger;
ItemP^.Code:=Fields.FieldByName(Card.DB.Keys.Code).AsInteger;
ItemP^.Active:=Fields.FieldByName(Card.DB.Keys.Active).AsBoolean;
ItemP^.Alias:=Fields.FieldByName(Card.DB.Keys.Alias).AsString;
ItemP^.Holder:=Fields.FieldByName(Card.DB.Keys.Holder).AsString;

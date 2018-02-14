Groups.PItem(DataP)^.Modified:=Fields.FieldByName(Groups.DB.Keys.Modified).AsFloat;
Groups.PItem(DataP)^.System:=Fields.FieldByName(Groups.DB.Keys.System).AsBoolean;
Groups.PItem(DataP)^.Name:=Fields.FieldByName(Groups.DB.Keys.Name).AsString;
Groups.PItem(DataP)^.Description:=Fields.FieldByName(Groups.DB.Keys.Description).AsString;


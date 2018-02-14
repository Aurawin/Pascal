  ItemP^.Title:=Fields.FieldByName(Purchase.Item.DB.Keys.Title).AsString;
  ItemP^.Description:=Fields.FieldByName(Purchase.Item.DB.Keys.Description).AsString;
  ItemP^.Kind:=Fields.FieldByName(Purchase.Item.DB.Keys.Kind).AsInteger;
  ItemP^.Price:=Fields.FieldByName(Purchase.Item.DB.Keys.Price).AsFloat;
  ItemP^.Taxable:=Fields.FieldByName(Purchase.Item.DB.Keys.Taxable).AsBoolean;
  ItemP^.Available:=Fields.FieldByName(Purchase.Item.DB.Keys.Available).AsLargeInt;
  ItemP^.Inventory:=Fields.FieldByName(Purchase.Item.DB.Keys.Inventory).AsLargeInt;
  ItemP^.Backorder:=Fields.FieldByName(Purchase.Item.DB.Keys.Backorder).AsLargeInt;
  ItemP^.Enabled:=Fields.FieldByName(Purchase.Item.DB.Keys.Enabled).AsBoolean;


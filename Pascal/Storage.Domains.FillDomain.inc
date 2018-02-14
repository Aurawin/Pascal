DomainP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
DomainP^.CertID:=Fields.FieldByName(Items.DB.Keys.CertID).AsLargeInt;
DomainP^.DefaultOptionQuota:=Fields.FieldByName(Items.DB.Keys.DefaultOptionQuota).AsLargeInt;
If bAssignDomain then
  DomainP^.Name:=Fields.FieldByName(Items.DB.Keys.Name).AsString;
DomainP^.Root:=Fields.FieldByName(Items.DB.Keys.Root).AsString;
DomainP^.FriendlyName:=Fields.FieldByName(Items.DB.Keys.FriendlyName).AsString;
DomainP^.DefaultOptionCatchAll:=Fields.FieldByName(Items.DB.Keys.DefaultOptionCatchAll).AsBoolean;
DomainP^.DefaultOptionFiltering:=Fields.FieldByName(Items.DB.Keys.DefaultOptionFiltering).AsBoolean;

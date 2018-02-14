UAP^.ID:=iID;//Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt
UAP^.User:=Fields.FieldByName(Items.DB.Keys.UserName).AsString;
UAP^.Password:=Fields.FieldByName(Items.DB.Keys.Password).AsString;
UAP^.First:=Fields.FieldByName(Items.DB.Keys.FirstName).AsString;
UAP^.Last:=Fields.FieldByName(Items.DB.Keys.LastName).AsString;
UAP^.Telephone:=Fields.FieldByName(Items.DB.Keys.Telephone).AsString;
UAP^.Auth:=Fields.FieldByName(Items.DB.Keys.Auth).AsString;
UAP^.Forward:=Fields.FieldByName(Items.DB.Keys.ForwardAddress).AsString;
UAP^.LastAccessed:=Fields.FieldByName(Items.DB.Keys.LastAccessed).AsFloat;
UAP^.LastQuotaEnforce:=Fields.FieldByName(Items.DB.Keys.LastQuotaEnforce).AsFloat;
UAP^.LastMessage:=Fields.FieldByName(Items.DB.Keys.LastMessage).AsInteger;
UAP^.LastIP:=Fields.FieldByName(Items.DB.Keys.LastIP).AsLargeInt;
UAP^.Quota:=Fields.FieldByName(Items.DB.Keys.Quota).AsLargeInt;
UAP^.Consumption:=Fields.FieldByName(Items.DB.Keys.Consumption).AsLargeInt;
UAP^.Throttle:=Fields.FieldByName(Items.DB.Keys.Throttle).AsLargeInt;
UAP^.Trash:=Fields.FieldByName(Items.DB.Keys.Trash).AsLargeInt;
UAP^.Inbox:=Fields.FieldByName(Items.DB.Keys.Inbox).AsLargeInt;

UAP^.Trashbox:=Fields.FieldByName(Items.DB.Keys.Trashbox).AsLargeInt;
UAP^.SentBox:=Fields.FieldByName(Items.DB.Keys.Sentbox).AsLargeInt;
UAP^.ArchiveBox:=Fields.FieldByName(Items.DB.Keys.ArchiveBox).AsLargeInt;

UAP^.Devices:=Fields.FieldByName(Items.DB.Keys.Devices).AsLargeInt;
UAP^.Contact:=Fields.FieldByName(Items.DB.Keys.Contact).AsLargeInt;

UAP^.Forwarding:=Fields.FieldByName(Items.DB.Keys.Forwarding).AsBoolean;
UAP^.Enabled:=Fields.FieldByName(Items.DB.Keys.Enabled).AsBoolean;
UAP^.LockoutCount:=Fields.FieldByName(Items.DB.Keys.LockoutCount).AsInteger;
UAP^.Kind:=akNormal;

Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.ACLCoreObjects).AsString,UAP^.aclCoreObjects,',');
Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.ACLCoreCommands).AsString,UAP^.aclCoreCommands,',');

Core.Arrays.LargeWord.RemoveAll(0,UAP^.aclCoreObjects);
Core.Arrays.LargeWord.RemoveAll(0,UAP^.aclCoreCommands);

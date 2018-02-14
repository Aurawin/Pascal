unit uUserEditing;

interface
uses ccWinSock,ccUtils,HSRConsts,Classes,uStorage,OCL,uItemScrollBox;

Type
  TUserEditItem=record
    DomainP        : PDomain;
    User           : TUserAccount;
    GP_User        : TGroup;
    GP_Admin       : TGroup;
    GP_Stats       : TGroup;
    GI_Password    : TGroupItem;
    GI_Enabled     : TGroupItem;
    GI_WebMail     : TGroupItem;
    GI_Filter      : TGroupItem;
    GI_Stats       : TGroupItem;
    GI_Admin       : TGroupItem;
    GI_Calendar    : TGroupItem;
    GI_RSS         : TGroupItem;
    GI_Forwarding  : TGroupItem;
    GI_Forward     : TGroupItem;
    GI_Quota       : TGroupItem;

    GI_LastIP       : TGroupItem;
    GI_LastLogin    : TGroupItem;
    GI_MailDropSize : TGroupItem;
    GI_Lockout      : TGroupItem;
  end;

  Function  Load_User(ISB:TItemScrollBox; Var Item:TUserEditItem):Boolean;
  Function  Add_User(Var Item:TUserEditItem):Boolean;
  Function  Delete_User(Var Item:TUserEditItem):Boolean;
  Function  Save_User(ISB:TItemScrollBox; Var Item:TUserEditItem):Boolean;
  Function  Exists_User(Var Item:TUserEditItem):Boolean;

implementation
uses SysUtils;

Function  Load_User(ISB:TItemScrollBox; Var Item:TUserEditItem):Boolean;
var
  Prop:TProperty;
begin
  Result:=False;
  ISB.Caption:='Finding User...';
  ISB.Clear;
  Item.User.DomainID:=Item.DomainP^.ID;
  Result:=uStorage.Storage.UserAccounts.Items.DB.Fill(uStorage.Module,Item.User);
  if Result then begin
    Item.GP_Admin:=ISB.AddGroup('Administration');
    Item.GP_User:=ISB.AddGroup('Account');
    Item.GP_Stats:=ISB.AddGroup('Statistics');
    Empty(Prop);
    Prop.Style:=psString;
    Prop.MaskValue:=True;
    Prop.Value:=VarStringToString(Item.User.Password);
    Prop.Caption:='Password';
    Prop.Name:=Prop.Caption;
    Item.GI_Password:=Item.GP_User.AddPropertyItem(-1,Prop);
    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Caption:='Enabled';
    Prop.Name:=Prop.Caption;
    Prop.Value:=uItemScrollBox.YesNo[Item.User.Enabled];
    Item.GI_Enabled:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psNumber;
    Prop.MaskValue:=False;
    Prop.Caption:='Lockout Count';
    Prop.Name:=Prop.Caption;
    Prop.Value:=IntToStr(Item.User.LockCount);
    Item.GI_Lockout:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Caption:='Web Mail';
    Prop.Name:=Prop.Caption;
    Prop.Value:=YesNo[Item.User.Webmail];
    Item.GI_WebMail:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Value:=YesNo[Item.User.SpamFilter];
    Prop.Caption:='Spam Filter';
    Prop.Name:=Prop.Caption;
    Item.GI_Filter:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Caption:='Site Stats';
    Prop.Name:=Prop.Caption;
    Prop.Value:=YesNo[Item.User.SRS_SiteStats];
    Item.GI_Stats:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Caption:='Forwarding';
    Prop.Name:=Prop.Caption;
    Prop.Value:=YesNo[Item.User.Forwarding];
    Item.GI_Forwarding:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.MaskValue:=False;
    Prop.Caption:='Forward';
    Prop.Name:=Prop.Caption;
    Prop.Value:=VarStringToString(Item.User.Forward);
    Item.GI_Forward:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.MaskValue:=False;
    Prop.Caption:='Quota';
    Prop.Name:=Prop.Caption;
    Prop.Value:=IntToStr(Item.User.Quota);
    Item.GI_Quota:=Item.GP_User.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Caption:='Enabled';
    Prop.Name:=Prop.Caption;
    Prop.Value:=YesNo[Item.User.SRS_Admin];
    Item.GI_Admin:=Item.GP_Admin.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Caption:='Edit Calendar';
    Prop.Name:=Prop.Caption;
    Prop.Value:=YesNo[Item.User.SRS_CalEdit];
    Item.GI_Calendar:=Item.GP_Admin.AddPropertyItem(-1,Prop);

    Prop.Style:=psYesNo;
    Prop.MaskValue:=False;
    Prop.Value:=YesNo[Item.User.SRS_RSSEdit];
    Prop.Caption:='Edit RSS';
    Prop.Name:=Prop.Caption;
    Item.GI_RSS:=Item.GP_Admin.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.MaskValue:=False;
    Prop.Caption:='Last IP';
    Prop.Name:=Prop.Caption;
    Prop.Value:=ccWinSock.InAddrToStr(Item.User.LastIP);
    Item.GI_LastIP:=Item.GP_Stats.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.MaskValue:=False;
    Prop.Caption:='Last Login';
    Prop.Name:=Prop.Caption;
    Prop.Value:=DateTimeToStr(Item.User.LastAccessed);
    Item.GI_LastLogin:=Item.GP_Stats.AddPropertyItem(-1,Prop);
  end;
end;

Function  Exists_User(Var Item:TUserEditItem):Boolean;
begin
  Result:=uStorage.UserAccount_Exists(uStorage.Module,Item.User.User,Item.DomainP^.ID);
end;

Function  Save_User(ISB:TItemScrollBox; Var Item:TUserEditItem):Boolean;
begin
  ISB.Caption:='Saving...';
  With Item do begin
    ccUtils.StringToVarString(GI_Password.Properties.Value,User.Password);
    ccUtils.StringToVarString(GI_Forward.Properties.Value,User.Forward);
    User.Enabled:=GI_Enabled.Properties.Value=YesNo[True];
    User.WebMail:=GI_WebMail.Properties.Value=YesNo[True];
    User.SpamFilter:=GI_Filter.Properties.Value=YesNo[True];
    User.SRS_SiteStats:=GI_Stats.Properties.Value=YesNo[True];
    User.SRS_Admin:=GI_Admin.Properties.Value=YesNo[True];
    User.SRS_CalEdit:=GI_Calendar.Properties.Value=YesNo[True];
    User.SRS_RSSEdit:=GI_RSS.Properties.Value=YesNo[True];
    User.Forwarding:=GI_Forwarding.Properties.Value=YesNo[True];
    User.Quota:=StrToIntDef(GI_Quota.Properties.Value,0);
    User.LockCount:=StrToIntDef(GI_LockOut.Properties.Value,0);
  end;
  Result:=uStorage.UserAccount_Save(uStorage.Module,Item.User);
end;

Function  Add_User(Var Item:TUserEditItem):Boolean;
begin
  Result:=Not uStorage.UserAccount_Exists(uStorage.Module,Item.User.User,Item.DomainP^.ID);
  if Result then begin
    SetLength(Item.User.Forward,0);
    SetLength(Item.User.Password,0);
    Item.User.WebMail:=Item.DomainP^.Default_WebMail;
    Item.User.SpamFilter:=Item.DomainP^.Default_SpamFiltering;
    Item.User.SRS_SiteStats:=Item.DomainP^.Default_SiteStats;
    Item.User.SRS_Admin:=False;
    Item.User.SRS_RSSEdit:=Item.DomainP^.Default_RSS;
    Item.User.SRS_CalEdit:=Item.DomainP^.Default_Calendar;
    Item.User.Forwarding:=False;
    Item.User.Quota:=Item.DomainP^.Default_Quota;
    Result:=uStorage.UserAccount_Create(uStorage.Module,Item.User,Item.DomainP^.ID);
  end;

end;

Function  Delete_User(Var Item:TUserEditItem):Boolean;
begin
  Result:=uStorage.UserAccount_Exists(uStorage.Module,Item.User.User,Item.DomainP^.ID);
  if Result then
    Result:=uStorage.UserAccount_Delete(uStorage.Module,Item.User,Item.DomainP^.ID);
end;


end.

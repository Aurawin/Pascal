tsUser.TabVisible:=true;

txtUser_Edit_First.Text:=FUserP^.First;
txtUser_Edit_Last.Text:=FUserP^.Last;
txtUser_Edit_Password.Text:=FUserP^.Password;
txtUser_Edit_Telephone.Text:=FUserP^.Telephone;

cbUser_Edit_Settings.Checked[0]:=FUserP^.Enabled;
cbUser_Edit_Settings.Checked[1]:=FUserP^.Forwarding;

seUser_Edit_Quota.Value:=FUserP^.Quota div Megabyte;
Storage.UserAccounts.Items.DB.Fill(Storage.Main.Task,FUserP^.ID,FUserP^);

LoadUserACLCoreObjects(FUserP);

pcDomain.ActivePage:=tsUser;


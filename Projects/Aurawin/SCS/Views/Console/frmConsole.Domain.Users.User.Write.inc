FUserP^.First:=txtUser_Edit_First.Text;
FUserP^.Last:=txtUser_Edit_Last.Text;
FUserP^.Password:=txtUser_Edit_Password.Text;
FUserP^.Telephone:=txtUser_Edit_Telephone.Text;

FUserP^.Enabled:=cbUser_Edit_Settings.Checked[0];
FUserP^.Forwarding:=cbUser_Edit_Settings.Checked[1];
FUserP^.Quota:=seUser_Edit_Quota.Value*Megabyte;

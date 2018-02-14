GUI.Lock;
try
  tsUser.TabVisible:=false;
  pcDomain.ActivePage:=tsDomainUsers;
  txtUser_Edit_First.Clear();
  txtUser_Edit_Last.Clear();
  txtUser_Edit_Password.Clear();
  txtUser_Edit_Telephone.Clear();
  seUser_Edit_Quota.Clear();
  for iLcv:=0 to cbUser_Edit_Settings.Items.Count-1 do
    cbUser_Edit_Settings.Checked[iLcv]:=false;
finally
  GUI.UnLock;
end;

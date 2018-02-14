lvUsers.Clear;
FFindUsers.List(FUAPL);
Try
  for iLcv:=0 to High(FUAPL) do begin
    li:=lvUsers.Items.Add;
    li.Caption:=IntToStr(FUAPL[iLcv]^.ID);
    li.SubItems.Add(FUAPL[iLcv]^.User);
    li.SubItems.Add(FUAPL[iLcv]^.First);
    li.SubItems.Add(FUAPL[iLcv]^.Last);
    li.SubItems.Add(FUAPL[iLcv]^.Telephone);
    li.SubItems.Add(IntToStr(FUAPL[iLcv]^.LockoutCount));
    li.SubItems.Add(Core.Strings.toString(FUAPL[iLcv]^.Quota));
    li.SubItems.Add(Core.Strings.toString(FUAPL[iLcv]^.Consumption));
    li.Data:=FUAPL[iLcv];
  end;
Finally
  Storage.UserAccounts.Items.Empty(FUAPL);
end;

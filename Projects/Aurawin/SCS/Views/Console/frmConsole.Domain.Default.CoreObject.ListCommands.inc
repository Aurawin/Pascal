GUI.Lock;
Try
  lvDefaultACLCoreCommands.Items.Clear;
  for iLcv:=0 to High(FCoreObject.Commands) do begin
    liCommand:=lvDefaultACLCoreCommands.Items.Add;
    liCommand.Checked:=Granted(FCoreObject.Commands[iLcv],@Default);
    liCommand.Caption:=YES_NO[liCommand.Checked];
    liCommand.SubItems.Add(FCoreObject.Commands[iLcv]^.ACLInfo^.Caption);
    liCommand.SubItems.Add(FCoreObject.Commands[iLcv]^.ACLInfo^.Prompt);
    liCommand.SubItems.Add(FCoreObject.Commands[iLcv]^.ACLInfo^.Description);
    liCommand.Data:=FCoreObject.Commands[iLcv];
  end;
Finally
  GUI.UnLock;
end;
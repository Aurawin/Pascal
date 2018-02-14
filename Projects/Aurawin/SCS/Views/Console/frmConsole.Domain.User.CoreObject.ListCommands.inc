GUI.Lock;
Try
  lvUserACLCoreCommands.Items.Clear;
  for iLcv:=0 to High(FCoreObject.Commands) do begin
    liCommand:=lvUserACLCoreCommands.Items.Add;
    bGranted:=Granted(FCoreObject.Commands[iLcv],FUserP);
    liCommand.Checked:=bGranted;;
    liCommand.Caption:=YES_NO[bGranted];
    liCommand.SubItems.Add(IntToStr(FCoreObject.Commands[iLcv]^.ID));
    liCommand.SubItems.Add(FCoreObject.Commands[iLcv]^.ACLInfo^.Caption);
    liCommand.SubItems.Add(FCoreObject.Commands[iLcv]^.ACLInfo^.Prompt);
    liCommand.SubItems.Add(FCoreObject.Commands[iLcv]^.ACLInfo^.Description);
    liCommand.Data:=FCoreObject.Commands[iLcv];
  end;
Finally
  GUI.UnLock;
end;

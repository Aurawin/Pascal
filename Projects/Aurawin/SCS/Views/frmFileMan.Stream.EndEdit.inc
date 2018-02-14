if Canceled then begin
  gbStreamEditor.Visible:=false;
  txtStreamName.Clear;
  FStreamEdit:=nil;
end else begin
  FStreamEdit.Name:=txtFileName.Text;
  FStreamEdit.Save(FTask,dsFileSaveInfo); // save name only
  txtStreamName.Clear;
  gbStreamEditor.Visible:=false;
  lvFiles.Selected.Caption:=FStreamEdit.Name;
  lvFiles.Selected.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(FStreamEdit.Modified,dtpoShort,BIAS_STAMP_OFF);
end;

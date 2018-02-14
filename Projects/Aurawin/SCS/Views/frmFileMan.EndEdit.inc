if Canceled then begin
  gbFileNameEditor.Visible:=false;
  txtFileName.Clear;
  FFileEdit:=nil;
end else begin
  FFileEdit.Name:=txtFileName.Text;
  FFileEdit.Save(FTask,dsFileSaveInfo);
  txtFileName.Clear;
  gbFileNameEditor.Visible:=false;
  if ( Core.Utils.ListView.IndexOf(lvFiles,FFileEdit,li)<>-1) then begin
    li.Caption:=FFileEdit.Name;
    li.SubItems[IDX_FILE_MODIFIED]:=Core.Utils.Time.DateTimeToString(FFileEdit.Modified,dtpoShort,BIAS_STAMP_OFF);
  end;
  FFileEdit:=nil;
end;

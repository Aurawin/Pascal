FFolder.Loading:=true;
FFolder.Load(FTask,FAT_RECURSE_OFF);

if cbPath.Items.IndexOf(FFolder.Path)=-1 then cbPath.Items.Add(FFolder.Path);
cbPath.Text:=FFolder.Path;
FPath:=FFolder.Path;
lvFiles.BeginUpdate;
Try
  lvFiles.Clear;
  FLList:=FFolder.Files.LockList();
  Try
    lbStatus.Caption:=Format(FMT_FILES_LOAD,[FFolder.Name]);
    pnlProgress.Visible:=True;
    pbProgress.Position:=0;

    pbProgress.Max:=FLList.Count-1;
    Application.ProcessMessages;
    for iLcv:=0 to FLList.Count-1 do begin
      pbProgress.Position:=iLcv;
      pbProgress.Update;
      if Canceled then break;
      li:=lvFiles.Items.Add;
      with TDSFile(FLList[iLcv]) do begin
        li.Data:=FLList[iLcv];
        li.Caption:=Name;
        li.SubItems.Add(YES_NO[Cache]);
        li.SubItems.Add(YES_NO[Deflate]);
        li.SubItems.Add(YES_NO[HasKeywords]);
        li.SubItems.Add(Format('%.0N',[Double(Size)]));
        li.SubItems.Add(IntToStr(CacheTTL));
        li.SubItems.Add(Core.Utils.Time.DateTimeToString(Modified,dtpoShort,BIAS_STAMP_OFF));
        li.SubItems.Add(Core.Utils.Time.DateTimeToString(Created,dtpoShort,BIAS_STAMP_OFF));
      end;
      if Canceled then break;
    end;
    lbStatus.Caption:='';
    lbPath.Caption:=Core.Strings.toString(FFolder.FileCount(FAT_RECURSE_ON));
    lbFiles.Caption:=Core.Strings.toString(lvFiles.Items.Count);
    pbProgress.Position:=0;
    pnlProgress.Visible:=False;
    Application.ProcessMessages;
  finally
    FFolder.Files.UnlockList();
  end;
Finally
  lvFiles.EndUpdate;
  FFolder.Loading:=False;
End;



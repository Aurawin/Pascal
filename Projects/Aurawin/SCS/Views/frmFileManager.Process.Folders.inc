procedure PushProcessFolder(Folder:TDSFolder);
var
  iLcv:integer;
  List:TList;
begin
  Core.Utils.TreeView.Add(Folder.Path,tvFolders,Folder);
  List:=Folder.Folders.LockList();
  Try
    for iLcv:=0 to List.Count-1 do
      PushProcessFolder(TDSFolder(List[iLcv]));
  Finally
    Folder.Folders.UnlockList();
  End;
end;

procedure PushProcessFolders(Folders:TDSFolders);
var
  iLcv:integer;
  List:TList;
begin
  List:=Folders.LockList();
  try
    for iLcv:=0 to List.Count-1 do
      PushProcessFolder(TDSFolder(List[iLcv]));
  Finally
    Folders.UnlockList();
  end;
end;

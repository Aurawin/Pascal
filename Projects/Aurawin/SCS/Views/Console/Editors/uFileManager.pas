unit uFileManager;

interface


  uses SysUtils,ccUtils,Classes,uItemScrollBox,uStorage,OCL,HSRConsts,Menus;

  procedure AddNewFile(Var Directory:TDDirectory; ISB:TItemScrollbox; Group:TGroup; DomainID:Int64; Caption:String; giFile:TGroupItem=Nil); overload;
  procedure EditFile(Var Directory:TDDirectory; ID:Int64; sFileName:String); overload;
  procedure AddNewFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; Group:TGroup); overload;
  procedure AddNewFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; DomainID:Int64; Caption:String; Group:TGroup=Nil); overload;
  procedure AddNewFolder(Var Directory:TDDirectory; DomainID:Int64; Path:String); overload;
  procedure LoadFolders(Var Directory:TDDirectory; ISB:TItemScrollbox; Var Domain:TDomain; PopupMenu:TPopupMenu);

  procedure LoadFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; Item:TGroupItem);

  procedure PasteFile(Var Directory:TDDirectory; DomainID:Int64; ISB:TItemScrollbox; Source,Destination:TGroupItem; DeleteSource:Boolean);

  procedure DeleteFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; GI:TGroupItem);
  procedure DeleteFile(Var Directory:TDDirectory; ISB:TItemScrollbox; GI:TGroupItem);
  Function  FolderExists(Var Directory:TDDirectory; ISB:TItemScrollbox; DomainID:Int64; Caption:String):Boolean; overload;
  Function  FileExists(Var Directory:TDDirectory; FolderID:Int64; Caption:String):Boolean; overload;
  procedure RenameFolder(Var Directory:TDDirectory; Const ID:Int64; NewPath:String); overload;
  procedure RenameFile(Var Directory:TDDirectory; Const ID:Int64; NewName:String); overload;


implementation

procedure AddNewFile(Var Directory:TDDirectory; ISB:TItemScrollbox; Group:TGroup; DomainID:Int64; Caption:String; giFile:TGroupItem=Nil);
var
  iLcv:Integer;
  GI:TGroupItem;
  sPath:String;
  sCaptionLcv:String;
  bbFile:TByteBuffer;
  FileName:Core.Strings.VarString;
  FileID:Int64;
begin
  Try
    Empty(bbFile);
    Empty(FileName);
    If ccUtils.FileExists(Caption) then
      ccUtils.FileToByteBuffer(Caption,bbFile);
    sPath:=ExtractFilePath(Caption);
    If (sPath<>'') and (sPath[Length(sPath)]<>'\') then
      sPath:=Concat(sPath,'\');
    Caption:=ExtractFileName(Caption);
    sCaptionLcv:=Caption;   FileID:=0; iLcv:=1;
    Repeat
      GI:=Group.Find(sCaptionLcv);
      if GI<>Nil then begin
        sCaptionLcv:=Format('Copy (%s) of %s',[IntToStr(iLcv),Caption]);
        Inc(iLcv);
      end;
    Until GI=Nil;
    Caption:=sCaptionLcv;
    ccUtils.StringToVarString(Caption,FileName);
    If uStorage.CreateFile(uStorage.Module,Directory,DomainID,Group.Tag,FileID,FileName,bbFile) then begin
      If giFile=Nil then
        giFile:=Group.AddSubItem(-1,Caption);
      giFile.Tag:=FileID;
    end;
  Finally
    Empty(bbFile);
    Empty(FileName);
  End;
end;

procedure EditFile(Var Directory:TDDirectory; ID:Int64; sFileName:String);
var
  FileName:Core.Strings.VarString;
  bbFile:TByteBuffer;
begin
  Try
    ccUtils.FileToByteBuffer(sFileName,bbFile);
    StringToVarString(ExtractFileName(sFileName),FileName);
    uStorage.EditFile(uStorage.Module,ID,bbFile);
  Finally
    Empty(FileName);
    Empty(bbFile);
  End;
end;

Function  FileExists(Var Directory:TDDirectory; FolderID:Int64; Caption:String):Boolean;
var
  vsPath:Core.Strings.VarString;
begin
  Try
    ccUtils.StringToVarString(Caption,vsPath);
    Result:=uStorage.FileExists(uStorage.Module,FolderID,vsPath);
  Finally
    SetLength(vsPath,0);
  End;
end;

Function FolderExists(Var Directory:TDDirectory;ISB:TItemScrollbox; DomainID:Int64; Caption:String):Boolean;
var
  vsPath:Core.Strings.VarString;
begin
  If ISB.Selected<>nil then
    ccUtils.StringToVarString(Concat(ISB.Selected.Group.Path,'/',Caption),vsPath)
  else
    ccUtils.StringToVarString(Caption,vsPath);
  Try
    Result:=uStorage.FolderExists(uStorage.Module,DomainID,vsPath);
  Finally
    SetLength(vsPath,0);
  End;
end;

procedure AddNewFolder(Var Directory:TDDirectory; DomainID:Int64; Path:String); overload;
var
  vsPath:Core.Strings.VarString;
  FolderID:Int64;
begin
  ccUtils.StringToVarString(SysUtils.StringReplace(Path,'\','/',[rfReplaceAll]),vsPath);
  Try
    uStorage.CreateFolder(uStorage.Module,Directory,DomainID,vsPath,FolderID);
  Finally
    SetLength(vsPath,0);
  End;
end;

procedure AddNewFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; DomainID:Int64; Caption:String; Group:TGroup=Nil);
var
  FolderID:Int64;
  sPath:Core.Strings.VarString;
begin
  If Group=Nil then begin
    If ISB.Selected=Nil then
      Group:=ISB.AddGroup(Caption,False)
    else
      Group:=ISB.Selected.Group.AddGroup(Caption);
  end;
  StringToVarString(SysUtils.StringReplace(Group.Path,'\','/',[rfReplaceAll]),sPath);
  Try
    uStorage.CreateFolder(uStorage.Module,Directory,DomainID,sPath,FolderID);
    Group.Tag:=FolderID;
    Group.HeaderItem.Tag:=FolderID;
  Finally
    SetLength(sPath,0);
  End;
end;

procedure AddNewFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; Group:TGroup);
var
  iLcv:Integer;
  sCaptionLcv,sCaption:String;
  giItem:TGroupItem;
  gpItem:TGroup;
begin
  sCaptionLcv:='New Folder';
  sCaption:='New Folder';
  iLcv:=1;
  Repeat
    giItem:=Group.Find(sCaptionLcv);
    if giItem<>Nil then begin
      sCaptionLcv:=Concat(sCaption,' ',IntToStr(iLcv));
      Inc(iLcv);
    end;
  Until giItem=Nil;
  sCaption:=sCaptionLcv;
  gpItem:=Group.AddGroup(sCaption);
  ISB.BeginEdit(gpItem.HeaderItem);
end;

procedure LoadFolders(Var Directory:TDDirectory; ISB:TItemScrollbox; Var Domain:TDomain; PopupMenu:TPopupMenu);
var
  iLcv,iItemLcv:Integer;
  GP:TGroup;
  GI:TGroupItem;
  sPath:String;
begin
  ISB.Caption:='Loading Folders.';
  Try
    ISB.Clear;
    ISB.PopUpMenu:=PopupMenu;
    ISB.AcceptFiles:=True;
    ISB.AutoExpand:=False;
    Empty(Directory);
    If uStorage.GetDirectory(uStorage.Module,Domain.ID,Directory) then begin
      ISB.DisableAlign;
      Try
        For iLcv:=0 to High(Directory) do begin
          sPath:=VarStringToString(Directory[iLcv].Path);
          sPath:=SysUtils.StringReplace(sPath,'/','\',[rfReplaceAll]);
          GP:=ISB.AddGroup(sPath,False);
          GP.Tag:=Directory[iLcv].ID;
          GP.HeaderItem.Tag:=Directory[iLcv].ID;
          {
          For iItemLcv:=0 to High(Directory[iLcv].Files) do begin
            GI:=GP.AddSubItem(-1,VarStringToString(Directory[iLcv].Files[iItemLcv].Name));
            GI.Tag:=Directory[iLcv].Files[iItemLcv].ID;
          end;
          }
        end;
      Finally
        ISB.EnableAlign;
      End;
      //ISB.AdjustItemPositions;
    end;
  Finally
    ISB.Caption:=Format('%s Folders',[VarStringToString(Domain.Domain)]);
  End;
end;

procedure LoadFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; Item:TGroupItem);
var
  iFolderCount,iFileLcv,iFolderLcv:Integer;
  gID:Int64;
  GI:TGroupItem;
  GP:TGroup;
begin
  If (Item<>Nil) then begin
    GP:=Item.Group;
    gID:=GP.Tag;
    iFolderLcv:=0; iFolderCount:=Length(Directory);
    While (iFolderLcv<iFolderCount) do begin
      If Directory[iFolderLcv].ID=gID then begin
        Try
          GP.DisableAlign;
          Try
            For iFileLcv:=0 to High(Directory[iFolderLcv].Files) do begin
              GI:=GP.Find(Directory[iFolderLcv].Files[iFileLcv].ID);
              If GI=Nil then begin
                GI:=GP.AddSubItem(-1,VarStringToString(Directory[iFolderLcv].Files[iFileLcv].Name));
                GI.Tag:=Directory[iFolderLcv].Files[iFileLcv].ID;
              end;
            end;
          Finally
            GP.EnableAlign;
          End;
          GP.State:=gsExpanded;
          GP.AdjustParentGroupHeight;
        Finally
          iFolderLcv:=iFolderCount;
        End;
      end;
      Inc(iFolderLcv);
    end;
  end;
end;

procedure RenameFile(Var Directory:TDDirectory; Const ID:Int64; NewName:String);
var
  vsName:Core.Strings.VarString;
begin
  ccUtils.StringToVarString(NewName,vsName);
  Try
    uStorage.RenameFile(uStorage.Module,ID,vsName);
  Finally
    Empty(vsName);
  End;
end;

procedure RenameFolder(Var Directory:TDDirectory; Const ID:Int64; NewPath:String);
var
  vsPath:Core.Strings.VarString;
begin
  ccUtils.StringToVarString(SysUtils.StringReplace(NewPath,'\','/',[rfReplaceAll]),vsPath);
  Try
    uStorage.RenameFolder(uStorage.Module,ID,vsPath);
  Finally
    Empty(vsPath);
  End;
end;

procedure DeleteFolder(Var Directory:TDDirectory; ISB:TItemScrollbox; GI:TGroupItem);
var
  List:TList;
  iLcv:Integer;
  GP:TGroup;
begin
  List:=TList.Create;
  Try
    List.Add(GI.Group);
    GI.Group.Groups(List);
    For iLcv:=0 to List.Count-1 do begin
      GP:=List[iLcv];
      uStorage.DeleteFolder(uStorage.Module,GP.Tag);
      ISB.RemoveGroup(GP);
    end;
  Finally
    FreeAndNil(List);
  End;
end;

procedure DeleteFile(Var Directory:TDDirectory; ISB:TItemScrollbox; GI:TGroupItem);
begin
  uStorage.DeleteFile(uStorage.Module,GI.Tag);
  ISB.RemoveSubItem(GI);
end;

procedure PasteFile(Var Directory:TDDirectory; DomainID:Int64; ISB:TItemScrollbox; Source,Destination:TGroupItem; DeleteSource:Boolean);
var
  bbData:TByteBuffer;
  vsFileName:Core.Strings.VarString;
  New_File_Details  : TFileDetails;
  Old_File_Details  : TFileDetails;
begin
  Try
    If uStorage.GetFile(uStorage.Module,Source.Tag,bbData) then begin
      ccUtils.StringToVarString(Source.Caption,vsFileName);
      If PrepareCopyFileName(uStorage.Module,Destination.Group.Tag,vsFileName) then begin
        Try
          If uStorage.CreateFile(uStorage.Module,Directory,DomainID,Destination.Group.Tag,New_File_Details.ID,vsFileName,bbData) then begin
            Old_File_Details.ID:=Source.Tag;
            Try
              If uStorage.GetFile(uStorage.Module,Old_File_Details) then begin
                If uStorage.GetFile(uStorage.Module,New_File_Details) then begin
                  New_File_Details.ContentType:=Old_File_Details.ContentType;
                  New_File_Details.HasKeywords:=Old_File_Details.HasKeywords;
                  New_File_Details.Cache:=Old_File_Details.Cache;
                  uStorage.SetFile(uStorage.Module,New_File_Details);
                  If DeleteSource then begin
                    uStorage.DeleteFile(uStorage.Module,Source.Tag);
                    ISB.RemoveSubItem(Source);
                  end;
                end;
              end;
            Finally
              Empty(New_File_Details);
              Empty(Old_File_Details);
            end;
          end;
          uStorage.GetDirectory(uStorage.Module,DomainID,Directory);
          LoadFolder(Directory,ISB,Destination.Group.HeaderItem);
        Finally
          Empty(vsFileName);
        end;
      end;
    end;
  Finally
    Empty(bbData);
  end;

end;

end.

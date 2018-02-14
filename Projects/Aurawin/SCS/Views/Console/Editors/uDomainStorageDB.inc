procedure Register_DomainStorage_DB;
var
  iCount:Integer;
  TableP:PDatabaseTable;
begin
  DB_TI_DomainStorage_Folders:=Length(DB_Tables);
  SetLength(DB_Tables,DB_TI_DomainStorage_Folders+1);

  TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
  TableP^.Name:=DB_Table_Domain_Storage_Folders;
  TableP^.AutoCreate:=True;
  TableP^.PrimaryField:=GN_FN_ID;

  iCount:=0;
  hDatabase.AddField(iCount,True,Integer(domainSPFId),cfNotNull or cfPrimaryKey or cfIdentity,0,GN_FN_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPFInsertID),0,0,GN_FN_Insert_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPFDomainID),cfNotNull,0,DS_FN_DOMAIN_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPFPath),cfNotNull,255,DS_FN_PATH,dftVarString,TableP^.Fields);

  DB_TI_DomainStorage_Files:=Length(DB_Tables);
  SetLength(DB_Tables,DB_TI_DomainStorage_Files+1);
  TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
  TableP^.Name:=DB_Table_Domain_Storage_Files;
  TableP^.AutoCreate:=True;
  TableP^.PrimaryField:=GN_FN_ID;
  iCount:=0;
  hDatabase.AddField(iCount,True,Integer(domainSPIId),cfNotNull or cfPrimaryKey or cfIdentity,0,GN_FN_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIInsertID),0,0,GN_FN_Insert_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIDomainID),cfNotNull,0,DS_FN_DOMAIN_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIPathID),cfNotNull,0,DS_FN_PATH_ID,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIName),cfNotNull,255,DS_FN_NAME,dftVarString,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIContentType),0,0,DS_FN_CT,dftInteger,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIHasKeywords),0,0,DS_FN_KEYWORDS,dftBoolean,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPICache),0,0,DS_FN_CACHE,dftBoolean,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIACLID),0,0,DS_FN_ACL,dftInt64,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPICreated),cfNotNull,0,DS_FN_CREATE_DT,dftDateTime,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIModified),cfNotNull,0,DS_FN_MODIFIED_DT,dftDateTime,TableP^.Fields);
  hDatabase.AddField(iCount,True,Integer(domainSPIData),cfNotNull,0,DS_FN_DATA,dftByteBuffer,TableP^.Fields);

end;

Function  IndexOf(Var Item:TDSFolders; FolderID:Int64):Integer; overload;
var
  iCount,iLcv:Integer;
begin
  Result:=-1; iLcv:=0; iCount:=Length(Item);
  While (Result=-1) and (iLcv<iCount) do begin
    If Item[iLcv].ID=FolderID then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function  IndexOf(Var Item:TDSFolder; FileID:Int64):Integer; overload;
var
  iCount,iLcv:Integer;
begin
  Result:=-1; iLcv:=0; iCount:=Length(Item.Files);
  While (Result=-1) and (iLcv<iCount) do begin
    If Item.Files[iLcv].ID=FileID then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function  IndexOf(Var Item:TDDirectory; FolderID:Int64):Integer; overload;
var
  iCount,iLcv:Integer;
begin
  Result:=-1; iLcv:=0; iCount:=Length(Item);
  While (Result=-1) and (iLcv<iCount) do begin
    If Item[iLcv].ID=FolderID then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

procedure Empty(Var Item:TFileDetails);
begin
  Item.ID:=0;
  Item.Size:=0;
  Item.Created:=0;
  Item.Modified:=0;
  Item.ContentType:=0;
  Item.HasKeywords:=False;
  Item.Cache:=False;
  Empty(Item.Name);
end;

procedure Empty(Var Item:TDSFolders);
var
  iLcv:Integer;
begin
  For iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Empty(Var Item:TDSFolder);
begin
  Item.ID:=0;
  Empty(Item.Path);
  Empty(Item.Files);
end;

procedure Empty(Var Item:TDSFiles);
var
  iLcv:Integer;
begin
  For iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Empty(Var Item:TDSFile);
begin
  Item.ID:=0;
  Item.ACLID:=0;
  Item.ContentType:=0;
  Item.HasKeywords:=False;
  Item.Cache:=False;
  Item.Created:=0;
  Item.Modified:=0;
  Empty(Item.Data);
end;


procedure Empty(Var Item:TDSFAT);
begin
  Item.DomainID:=0;
  Empty(Item.Folders);
end;

procedure Empty(Var Item:TDFile);
begin
  Empty(Item.Name);
end;

procedure Empty(Var Item:TDFiles);
var
  iLcv:Integer;
begin
  For iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Empty(Var Item:TDFolder);
begin
  Empty(Item.Path);
end;

procedure Empty(Var Item:TDDirectory);
var
  iLcv:Integer;
begin
  For iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Copy(Var Source,Destination:TDFile);
begin
  Destination.ID:=Source.ID;
  ccUtils.Copy(Source.Name,Destination.Name);
end;

procedure Copy(Var Source,Destination:TDFiles);
var
  iLcv,iSourceCount:Integer;
begin
  Empty(Destination);
  iSourceCount:=Length(Source);
  SetLength(Destination,iSourceCount);
  For iLcv:=0 to iSourceCount-1 do
    Copy(Source[iLcv],Destination[iLcv]);
end;

procedure Copy(Var Source,Destination:TDFolder);
begin
  Destination.ID:=Source.ID;
  ccUtils.Copy(Source.Path,Destination.Path);
  Copy(Source.Files,Destination.Files);
end;

procedure Copy(Var Source,Destination:TDDirectory);
var
  iLcv,iSourceCount:Integer;
begin
  Empty(Destination);
  iSourceCount:=Length(Source);
  SetLength(Destination,iSourceCount);
  For iLcv:=0 to iSourceCount-1 do
    Copy(Source[iLcv],Destination[iLcv]);
end;

procedure CB_GetFile_Details_ID(RecordP:PDatabaseResults; Const DataP:Pointer);
var
  DetailsP:PFileDetails;
begin
  DetailsP:=DataP;
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIName),DetailsP^.Name,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIHasKeywords),DetailsP^.HasKeywords,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPICache),DetailsP^.Cache,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIContentType),DetailsP^.ContentType,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPICreated),DetailsP^.Created,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIModified),DetailsP^.Modified,RecordP);
end;

Function  GetFile(Var Module:TDBMSModule; Var Details:TFileDetails):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_FILES];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIId),poNone,oEqual,Details.ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIName),poNone,oNone,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIHasKeywords),poNone,oNone,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPICache),poNone,oNone,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIContentType),poNone,oNone,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPICreated),poNone,oNone,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIModified),poNone,oNone,Commands);
    Result:=uDatabase.Select(Module.SessionID,@Commands,CB_GetFile_Details_ID,@Details);
    If Result then begin
      iCount:=0;
      hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
      hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFID),poNone,oEqual,Details.ID,Commands);
      hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIData),poNone,oNone,Commands);
      Details.Size:=uDatabase.DataLength(Module.SessionID,@Commands);
    end;
  Finally
    SetLength(Commands,0);
  end;
end;

procedure CB_GetFile_Path_ID(RecordP:PDatabaseResults; Const DataP:Pointer);
begin
  With PDSFolder(DataP)^ do begin
    hDatabase.GetDatabaseFieldValue(Integer(domainSPIId),ID,RecordP);
  end;
end;

Function  GetFile(Var Module:TDBMSModule; DomainID:Int64; Var Folder:TDSFolder; Var Item:TDSFile):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIDomainID),poNone,oEqual,DomainID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIPathID),poAnd,oEqual,Folder.ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIId),poNone,oNone,Commands);
    Result:=uDatabase.Select(Module.SessionID,@Commands,CB_GetFile_Path_ID,@Folder);
  Finally
    SetLength(Commands,0);
  end;
end;

Function  SetFile(Var Module:TDBMSModule; ID:Int64; Var Data:TByteBuffer):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
  dtModified:Double;
begin
  Result:=False;
  Try
    iCount:=0;dtModified:=Core.Timer.dtNow;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIId),poNone,oEqual,ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIModified),poNone,oNone,dtModified,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIData),poNone,oNone,Data,Commands);
    Result:=uDatabase.Update(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  end;
end;

Function  SetFile(Var Module:TDBMSModule; Var Details:TFileDetails):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIId),poNone,oEqual,Details.ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIName),poNone,oNone,Details.Name,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIContentType),poNone,oNone,Details.ContentType,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIHasKeywords),poNone,oNone,Details.HasKeywords,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPICache),poNone,oNone,Details.Cache,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPICreated),poNone,oNone,Details.Created,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIModified),poNone,oNone,Details.Modified,Commands);
    Result:=uDatabase.Update(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  end;
end;

procedure CB_GetFile_File_ID(RecordP:PDatabaseResults; Const DataP:Pointer);
begin
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIData),PByteBuffer(DataP)^,RecordP);
end;

Function  GetFile(Var Module:TDBMSModule; ID:Int64; Var Data:TByteBuffer):Boolean; overload;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIId),poNone,oEqual,ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIData),poNone,oNone,Commands);
    Result:=uDatabase.Select(Module.SessionID,@Commands,CB_GetFile_File_ID,@Data);
  Finally
    SetLength(Commands,0);
  end;
end;


procedure CB_GetDirectory_Folders(RecordP:PDatabaseResults; Const DataP:Pointer);
var
  DirectoryP:PDDirectory;
  iLen:Integer;
begin
  DirectoryP:=DataP;
  iLen:=Length(DirectoryP^);
  SetLength(DirectoryP^,iLen+1);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFId),DirectoryP^[iLen].ID,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFPath),DirectoryP^[iLen].Path,RecordP);
end;

procedure CB_GetDirectory_Files(RecordP:PDatabaseResults; Const DataP:Pointer);
var
  FolderP:PDFolder;
  iLen:Integer;
begin
  FolderP:=DataP;
  iLen:=Length(FolderP^.Files);
  SetLength(FolderP^.Files,iLen+1);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFId),FolderP^.Files[iLen].ID,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIName),FolderP^.Files[iLen].Name,RecordP);
end;

Function  GetDirectory(Var Module:TDBMSModule; DomainID:Int64; Var Directory:TDDirectory):Boolean;
var
  iLcv,iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
  TempDirectory:TDDirectory;
begin
  Result:=False;
  Try
    Empty(TempDirectory);
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFDomainID),poNone,oEqual,DomainID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPFId),poNone,oNone,Commands);
    hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPFPath),poNone,oNone,Commands);
    Result:=uDatabase.Select(Module.SessionID,@Commands,CB_GetDirectory_Folders,@TempDirectory);
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    For iLcv:=0 to High(TempDirectory) do begin
      iCount:=0;
      hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
      hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIPathID),poNone,oEqual,TempDirectory[iLcv].ID,Commands);
      hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIId),poNone,oNone,Commands);
      hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIName),poNone,oNone,Commands);
      uDatabase.Select(Module.SessionID,@Commands,CB_GetDirectory_Files,@TempDirectory[iLcv]);
    end;
    Copy(TempDirectory,Directory);
  Finally
    SetLength(Commands,0);
  end;
end;

procedure CB_FAT_Folders(RecordP:PDatabaseResults; Const DataP:Pointer);
var
  FoldersP:PDSFolders;
  iID:Int64;
  iIndex:Integer;
begin
  FoldersP:=DataP;
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFId),iID,RecordP);
  iIndex:=IndexOf(FoldersP^,iId);
  If iIndex=-1 then begin
    iIndex:=Length(FoldersP^);
    SetLength(FoldersP^,iIndex+1);
  end;
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFId),FoldersP^[iIndex].ID,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFPath),FoldersP^[iIndex].Path,RecordP);
  Lowercase(FoldersP^[iIndex].Path);
end;


procedure CB_GetFAT_FileData(RecordP:PDatabaseResults; Const DataP:Pointer);
var
  FileP:PDSFile;
  dtModified:Boolean;
begin
  FileP:=DataP;
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIData),FileP^.Data,RecordP);
end;

procedure CB_GetFAT_Files(RecordP:PDatabaseResults; Const DataP:Pointer);
var
  FolderP:PDSFolder;
  iID:Int64;
  iIndex:Integer;
  dtModified:Double;
begin                                     
  FolderP:=DataP;
  hDatabase.GetDatabaseFieldValue(Integer(domainSPFId),iID,RecordP);
  iIndex:=IndexOf(FolderP^,iId);
  If iIndex=-1 then begin
    iIndex:=Length(FolderP^.Files);
    SetLength(FolderP^.Files,iIndex+1);
    FolderP^.Files[iIndex].ID:=iID;
  end;
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIName),FolderP^.Files[iIndex].Name,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIACLID),FolderP^.Files[iIndex].Aclid,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIContentType),FolderP^.Files[iIndex].ContentType,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIHasKeywords),FolderP^.Files[iIndex].HasKeywords,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPICache),FolderP^.Files[iIndex].Cache,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPICreated),FolderP^.Files[iIndex].Created,RecordP);
  hDatabase.GetDatabaseFieldValue(Integer(domainSPIModified),dtModified,RecordP);
  FolderP^.Files[iIndex].DownloadNeeded:=(FolderP^.Files[iIndex].Modified<>dtModified);
  FolderP^.Files[iIndex].Modified:=dtModified;
  //hDatabase.GetDatabaseFieldValue(Integer(domainSPIData),FolderP^.Files[iIndex].Data,RecordP);
  Lowercase(FolderP^.Files[iIndex].Name);
end;


Function  GetFAT(Var Module:TDBMSModule; DomainID:Int64; FAT:TDomainFat):Boolean;
var
  iLcv,iFileLcv,iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    Fat.Lock;
    Try
      iCount:=0;
      TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
      hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
      hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFDomainID),poNone,oEqual,DomainID,Commands);
      hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPFId),poNone,oNone,Commands);
      hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPFPath),poNone,oNone,Commands);
      Result:=uDatabase.Select(Module.SessionID,@Commands,CB_FAT_Folders,@FAT.FileSystem.Folders);
      TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
      For iLcv:=0 to High(Fat.Filesystem.Folders) do begin
        iCount:=0;
        hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
        hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIPathID),poNone,oEqual,Fat.FileSystem.Folders[iLcv].ID,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIId),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIName),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIACLID),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIContentType),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIHasKeywords),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPICache),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPICreated),poNone,oNone,Commands);
        hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIModified),poNone,oNone,Commands);
        //hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIData),poNone,oNone,Commands);
        uDatabase.Select(Module.SessionID,@Commands,CB_GetFAT_Files,@Fat.Filesystem.Folders[iLcv]);
        For iFileLcv:=0 to High(Fat.FileSystem.Folders[iLcv].Files) do begin
          If Fat.FileSystem.Folders[iLcv].Files[iFileLcv].DownloadNeeded then begin
            iCount:=0;
            hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
            hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIId),poNone,oEqual,Fat.FileSystem.Folders[iLcv].Files[iFileLcv].ID,Commands);
            hDatabase.AddCommand(iCount,TableP,useForFields,Integer(domainSPIData),poNone,oNone,Commands);
            uDatabase.Select(Module.SessionID,@Commands,CB_GetFAT_FileData,@Fat.Filesystem.Folders[iLcv].Files[iFileLcv]);
          end;
        end;
      end;
    Finally
      Fat.Unlock;
    End;
  Finally
    SetLength(Commands,0);
  end;
end;

Function  FileExists(Var Module:TDBMSModule; FolderID:Int64; Var Path:Core.Strings.VarString):Boolean; overload;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIName),poNone,oEqual,Path,Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIPathID),poAnd,oEqual,FolderID,Commands);
    Result:=uDatabase.Count(Module.SessionID,@Commands)>0;
  Finally
    SetLength(Commands,0);
  end;
end;

Function  PrepareCopyFileName(Var Module:TDBMSModule; FolderID:Int64; Var Name:Core.Strings.VarString):Boolean;
Const
  FrmtFileStr='Copy (%s) of %s';
var
  iLcv,iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
  sStartName:String;
  // Copy (1) of FileName
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIName),poNone,oEqual,Name,Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIPathID),poAnd,oEqual,FolderID,Commands);
    Result:=uDatabase.Count(Module.SessionID,@Commands)=0;
    If Not Result then begin
      iLcv:=1; sStartName:=ccUtils.VarStringToString(Name);
      Repeat
        ccUtils.StringToVarString(Format(FrmtFileStr,[IntToStr(iLcv),sStartName]),Name);
        Result:=uDatabase.Count(Module.SessionID,@Commands)=0;
        Inc(iLcv);
      Until Result;
    end;
  Finally
    SetLength(Commands,0);
  end;
end;


Function  FolderExists(Var Module:TDBMSModule; DomainID:Int64; Var Path:Core.Strings.VarString):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFDomainID),poNone,oEqual,DomainID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFPath),poAnd,oEqual,Path,Commands);
    Result:=uDatabase.Count(Module.SessionID,@Commands)>0;
  Finally
    SetLength(Commands,0);
  end;
end;


Function  CreateFile(Var Module:TDBMSModule; Var Directory:TDDirectory; DomainID,FolderID:Int64; Var FileID:Int64; Var FileName:Core.Strings.VarString; Var Data:TByteBuffer):Boolean;
var
  iFolderIndex,iCount:Integer;
  bFalse,bTrue:Boolean;
  dtStamp:Double;
  iInsertID,iReset:Int64;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    bTrue:=True; bFalse:=False; iCount:=0; iReset:=0; iInsertID:=ccUtils.Random(High(Integer)); FileID:=0; dtStamp:=Core.Timer.dtNow;
    iFolderIndex:=IndexOf(Directory,FolderID);
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    // Setup Primary ID
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIInsertID),poNone,oNone,iInsertID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIInsertID),poNone,oEqual,iInsertID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForPrimaryID,Integer(domainSPIId),poNone,oNone,FileID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForResetInsertID,Integer(domainSPIInsertID),poNone,oNone,iReset,Commands);

    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIDomainID),poNone,oNone,DomainID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIPathID),poNone,oNone,FolderID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPICreated),poNone,oNone,dtStamp,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIModified),poNone,oNone,dtStamp,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPICache),poNone,oNone,bTrue,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIHasKeywords),poNone,oNone,bFalse,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIName),poNone,oNone,FileName,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPIData),poNone,oNone,Data,Commands);
    Result:=uDatabase.Insert(Module.SessionID,@Commands);
    If Result then begin
      iCount:=Length(Directory[iFolderIndex].Files);
      SetLength(Directory[iFolderIndex].Files,iCount+1);
      Directory[iFolderIndex].Files[iCount].ID:=FileID;
      ccUtils.Copy(FileName,Directory[iFolderIndex].Files[iCount].Name);
    end;
  Finally
    SetLength(Commands,0);
  End;
end;

Function  CreateFolder(Var Module:TDBMSModule; Var Directory:TDDirectory; DomainID:Int64; Var Path:Core.Strings.VarString; Var FolderID:Int64):Boolean;
var
  iCount:Integer;
  iInsertID,iReset:Int64;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=ccUtils.Random(High(Integer)); FolderID:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
    // Setup Primary ID
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPFInsertID),poNone,oNone,iInsertID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFInsertID),poNone,oEqual,iInsertID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForPrimaryID,Integer(domainSPFId),poNone,oNone,FolderID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForResetInsertID,Integer(domainSPFInsertID),poNone,oNone,iReset,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPFDomainID),poNone,oNone,DomainID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForInsert,Integer(domainSPFPath),poNone,oNone,Path,Commands);
    Result:=uDatabase.Insert(Module.SessionID,@Commands);
    If Result then begin
      iCount:=Length(Directory);
      SetLength(Directory,iCount+1);
      Directory[iCount].ID:=FolderID;
      ccutils.Copy(Path,Directory[iCount].Path);
    end;
  Finally
    SetLength(Commands,0);
  End;
end;

Function  DeleteFolder(Var Module:TDBMSModule; ID:Int64):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFId),poNone,oEqual,ID,Commands);
    Result:=uDatabase.Delete(Module.SessionID,@Commands);
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIPathID),poNone,oEqual,ID,Commands);
    Result:=uDatabase.Delete(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  End;
end;

Function  DeleteFile(Var Module:TDBMSModule; ID:Int64):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIID),poNone,oEqual,ID,Commands);
    Result:=uDatabase.Delete(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  End;
end;

Function  EditFile(Var Module:TDBMSModule; ID:Int64; Var Data:TByteBuffer):Boolean; overload;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
  dtModified:Double;
begin
  Result:=False;
  Try
    iCount:=0; dtModified:=Core.Timer.dtNow;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIID),poNone,oEqual,ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIData),poNone,oNone,Data,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIModified),poNone,oNone,dtModified,Commands);
    Result:=uDatabase.Update(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  End;
end;


Function  RenameFile(Var Module:TDBMSModule; ID:Int64; Var Name:Core.Strings.VarString):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Files];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPIID),poNone,oEqual,ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPIName),poNone,oNone,Name,Commands);
    Result:=uDatabase.Update(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  End;
end;


Function  RenameFolder(Var Module:TDBMSModule; ID:Int64; Var Path:Core.Strings.VarString):Boolean;
var
  iCount:Integer;
  Commands:TDatabaseCommands;
  TableP:PDatabaseTable;
begin
  Result:=False;
  Try
    iCount:=0;
    TableP:=@DB_Tables[DB_TI_DomainStorage_Folders];
    hDatabase.AddCommand(iCount,TableP^.Name,useForTable,DATATYPE_VOID,PROPERTY_ID_VOID,poNone,oNone,@Commands);
    hDatabase.AddCommand(iCount,TableP,useForCriteria,Integer(domainSPFID),poNone,oEqual,ID,Commands);
    hDatabase.AddCommand(iCount,TableP,useForUpdates,Integer(domainSPFPath),poNone,oNone,Path,Commands);
    Result:=uDatabase.Update(Module.SessionID,@Commands);
  Finally
    SetLength(Commands,0);
  End;
end;

constructor  TDomainFat.Create;
begin
  //FWNDHandle:=AllocateHWND(WinProc);
  InitializeCriticalSection(FLock);
  //FFileIndexes:=TFileBasedIndex.Create(Concat(APP_PATH,'\',IntToStr(FWNDHandle),'.FLM'));
  //FFolderIndexes:=TFileBasedIndex.Create(Concat(APP_PATH,'\',IntToStr(FWNDHandle),'.FDM'));
  Inherited Create;
end;

Destructor  TDomainFat.Destroy;
begin
  //FreeAndNil(FFileIndexes);
  //FreeAndNil(FFolderIndexes);
  DeleteCriticalSection(FLock);
  //DeAllocateHWND(FWNDHandle);
  Empty(FFileSystem);
  Inherited Destroy;
end;

procedure  TDomainFat.LoadFiles(Var Module:TDBMSModule; DomainID:Int64);
begin
  // Assume Main Thread here.
  GetFAT(Module,DomainID,Self);
end;
(*
procedure  TDomainFat.WinProc(Var Msg:Messages.TMessage);
begin
  Try
    CASE MSG.MSG of
      WM_SCANFILES : begin
        ScanForChanges;
        Msg.Result:=0;
      end
    else
      Msg.Result := DefWindowProc(FWNDHandle, Msg.Msg, Msg.wParam, Msg.lParam);
    end;
  Except
    On E:Exception do  Core.Logging.Native.WriteLogEntry('TDomainFat.WinProc','Exception',E.Message);
  end;
end;
*)

Function   TDomainFat.FindFolderID(Var Path:String; Var FolderID:Int64):Boolean;
var
  iCount,iFolderLcv:Integer;
begin
  iFolderLcv:=0; Result:=False; FolderID:=0;
  EnterCriticalSection(FLock);
  Try
    iCount:=Length(FFileSystem.Folders);
    While (iFolderLcv<iCount) and (FolderID=0) do begin
      If ccUtils.Compare(Path,FFileSystem.Folders[iFolderLcv].Path) then begin
        FolderID:=FFileSystem.Folders[iFolderLcv].ID;
        Result:=True;
      end;
      Inc(iFolderLcv);
    end;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

Function   TDomainFat.FindFileID(Var Path,FileName:String; Var FolderID:Int64; Var dtCreated,dtModified:Double; Var HasKeywords,Cacheable:Boolean):Int64;
var
  FileP:PDSFile;
  iFileIndex,iFolderIndex,iFolderLcv,iFileLcv,iFileCount,iFolderCount:Integer;
begin
  iFolderIndex:=-1; iFolderLcv:=0;FolderID:=0; Result:=0;
  EnterCriticalSection(FLock);
  Try
    iFolderCount:=Length(FFileSystem.Folders);
    While (iFolderLcv<iFolderCount) and (iFolderIndex=-1) do begin
      If ccUtils.Compare(Path,FFileSystem.Folders[iFolderLcv].Path) then begin
        FolderID:=FFileSystem.Folders[iFolderLcv].ID;
        iFolderIndex:=iFolderLcv;
      end;
      Inc(iFolderLcv);
    end;
    If iFolderIndex<>-1 then begin
      iFileLcv:=0; iFileCount:=Length(FFileSystem.Folders[iFolderIndex].Files);
      While (iFileLcv<iFileCount) and (Result=0) do begin
        If ccUtils.Compare(FileName,FFileSystem.Folders[iFolderIndex].Files[iFileLcv].Name) then begin
          Result:=FFileSystem.Folders[iFolderIndex].Files[iFileLcv].ID;
          dtCreated:=FFileSystem.Folders[iFolderIndex].Files[iFileLcv].Created;
          dtModified:=FFileSystem.Folders[iFolderIndex].Files[iFileLcv].Modified;
          HasKeywords:=FFileSystem.Folders[iFolderIndex].Files[iFileLcv].HasKeywords;
          Cacheable:=FFileSystem.Folders[iFolderIndex].Files[iFileLcv].Cache;
          iFileIndex:=iFileLcv;
        end;
        Inc(iFileLcv);
      end;
    end;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

Function   TDomainFat.AcquireFile(var FolderID,FileID:Int64):PByteBuffer;
var
  iFolderLcv,iFileLcv:Integer;
  iFileCount,iFolderCount:Int64;
begin
  Result:=Nil;
  EnterCriticalSection(FLock);
  Try
    iFolderCount:=Length(FFileSystem.Folders); iFolderLcv:=0;
    While (iFolderLcv<iFolderCount) and (Result=nil) do begin
      If FFileSystem.Folders[iFolderLcv].ID=FolderId then begin
        iFileLcv:=0;  iFileCount:=Length(FFileSystem.Folders[iFolderLcv].Files);
        While (iFileLcv<iFileCount) and (Result=Nil) do begin
          If FFileSystem.Folders[iFolderLcv].Files[iFileLcv].ID=FileID then begin
            Inc(FFileSystem.Folders[iFolderLcv].Files[iFileLcv].LockCount);
            Result:=@FFileSystem.Folders[iFolderLcv].Files[iFileLcv].Data;
          end;
          Inc(iFileLcv);
        end;
      end;
      Inc(iFolderLcv)
    end;
  Finally
    LeaveCriticalSection(FLock);
  End;
end;

procedure  TDomainFat.ReleaseFile(Var FolderID,FileID:Int64);
var
  iFileIndex,iFolderLcv,iFileLcv:Integer;
  iFileCount,iFolderCount:Int64;
begin
  EnterCriticalSection(FLock);
  Try
    iFolderCount:=Length(FFileSystem.Folders); iFileIndex:=-1;  iFolderLcv:=0;
    While (iFolderLcv<iFolderCount) and (iFileIndex=-1) do begin
      If FFileSystem.Folders[iFolderLcv].ID=FolderId then begin
        iFileLcv:=0;  iFileCount:=Length(FFileSystem.Folders[iFolderLcv].Files);
        While (iFileLcv<iFileCount) and (iFileIndex=-1) do begin
          If FFileSystem.Folders[iFolderLcv].Files[iFileLcv].ID=FileID then begin
            iFileIndex:=iFileLcv;
            Dec(FFileSystem.Folders[iFolderLcv].Files[iFileLcv].LockCount);
          end;
          Inc(iFileLcv);
        end;
      end;
      Inc(iFolderLcv)
    end;
  Finally
    LeaveCriticalSection(FLock);
  End;
end;
procedure   TDomainFat.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure   TDomainFat.Unlock;
begin
  LeaveCriticalSection(FLock);
end;

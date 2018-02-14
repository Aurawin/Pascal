constructor cmProcessAllFilesSocial.Create(aManager:TRSRManager; aRSRP:PRSR; var Document:TXMLDocument; var aHdr:Storage.Social.Sync.THeader);
begin
  FDocument:=Document;
  FHdrP:=@aHdr;
  FSyncP:=aHdr.ItemP;
  Document:=nil;
  Inherited Create(aManager,aRSRP);
end;

destructor cmProcessAllFilesSocial.Destroy();
begin
  if FDocument<>nil then
    FDocument.Free();
  inherited Destroy;
end;

procedure cmProcessAllFilesSocial.Execute();
var
  Man:TAuSocketMan;
  iLcv,iCt : LongInt;

  qItem       : Storage.Social.Sync.Queue.TItem;
  dtModified  : double;

  folderID  : QWord;
  fileName  : Core.Strings.VarString;

  xItems    : TDOMNode;
  xItem     : TDOMNode;
  xSmry     : TDOMNode;

  fileLcv     : Storage.Social.Files.PSFile;
  folderLcv   : Storage.Social.Folders.PSFolder;
  itm         : Storage.Social.Sync.Queue.TItem;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmProcessAllFilesSocial.Execute';
  xItems:=Core.XML.DB.getNode(FDocument,Storage.Social.Files.DB.XML.Stanzas.Items);

  if (xItems<>nil) then begin
    System.EnterCriticalSection(FSyncP^.Lock);
    Try
      for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
        xItem:=xItems.ChildNodes[iLcv];
        folderId:=Core.XML.DB.toQWord(xItem,Storage.Social.Files.DB.XML.FolderID);
        fileName:=Core.XML.DB.toString(xItem,Storage.Social.Files.DB.XML.Name);
        dtModified:=Core.XML.DB.toDouble(xitem,Storage.Social.Files.DB.XML.Modified);

        folderLcv:=FSyncP^.Folders.Find(folderId);
        if (folderLcv<>nil) then begin
          fileLcv:=Storage.Social.Files.Get(fileName,folderLcv^.Files);
          if (fileLcv=nil) then begin
            new(fileLcv);
            Init(fileLcv^);
            Storage.Social.Files.Add(fileLcv^,folderLcv^.Files);
          end;
          if (fileLcv^.Modified<dtModified) then begin
            Storage.Social.Files.fromXML(xItem,fileLcv^);
            if (fileLcv^.Allocated<Storage.Social.Files.Allocate_Base) then begin
              qItem:=Man.FSyncQueue.Find(folderId,fileName);
              if (qItem<>nil) then
                Core.Arrays.Bytes.Copy(qItem.Digest,fileLcv^.Digest);
            end;
          end;
        end else begin
          folderLcv:=nil;
        end;
      end;
      Storage.Social.Sync.Discover(FSyncP^.Folders,FSyncP^.Pipes);

      FSyncP^.UpdateInfo.stateFiles:=rqsReceived;
      FSyncP^.UpdateInfo.LastChecked:=Core.Timer.dtUT;
    finally
      LeaveCriticalSection(FSyncP^.Lock);
    end;
  end;
end;



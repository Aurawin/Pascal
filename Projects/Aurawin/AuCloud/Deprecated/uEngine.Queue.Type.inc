  Queue=class
  Type
    Item=class;
    ItemKind=(Create,Read,Write);
    ItemEvent = procedure(Item:Item) of object;
    Status=class
    const
      Kind : Array[ItemKind] of Core.Strings.PVarString = (
         @auLang.Table.Engine.Status.Sync.Add,
         @auLang.Table.Engine.Status.Sync.Read,
         @auLang.Table.Engine.Status.Sync.Write
      );


    end;

    Item=class
      SyncP         : Storage.Social.Sync.PItem;
      PipeP         : Storage.Social.Sync.Pipes.PItem;
      HdrP          : Storage.Social.Sync.PHeader;

      FolderP       : Storage.Social.Folders.PSFolder;
      FileP         : Storage.Social.Files.PSFile;
      LocalizedFile : Core.Strings.VarString;
      RemoteFile    : Core.Strings.VarString;
      Kind          : ItemKind;
    public
      Constructor Create(
        aKind          : ItemKind;
        aPipeP         : Storage.Social.Sync.Pipes.PItem;
        aSyncP         : Storage.Social.Sync.PItem;
        aHdrP          : Storage.Social.Sync.PHeader;

        aFolderP       : Storage.Social.Folders.PSFolder;
        aFileP         : Storage.Social.Files.PSFile;
        aLocalized     : Core.Strings.VarString;
        aRemote        : Core.Strings.VarString
      );
    end;
    ItemsList = specialize GObjectThreadList<Item>;
    Items=class
    private
      Owner         : TAuSocketMan;
      FList         : ItemsList;
      FTransferCount: QWord;
      FTransferSize : QWord;
      FLock         : TRTLCriticalSection;
    private
      procedure processWrite(RSRP:PRSR; Request:THTTPRequest; aItem:Item);
      procedure processCreate(RSRP:PRSR; Request:THTTPRequest; aItem:Item);
      procedure processRead(RSRP:PRSR; Request:THTTPRequest; aItem:Item);
    private
      procedure NotifyQueued(aCaller:TThread; aItem:Item);
      procedure NotifyRemoved(aCaller:TThread; aItem:Item);
    public
      constructor Create(aOwner:TAuSocketMan); virtual;
      destructor Destroy(); override;
    public
      procedure Process(aCaller:TThread; RSRP:PRSR; Request:THTTPrequest);
    public
      function calculateSize():QWord;

      function Find(aFolderId:QWord; aFileId:QWord):Item; overload;
      function Find(aFolderId:QWord; aFile:Core.Strings.VarString):Item; overload;
      function Schedule(
        aCaller        : TThread;
        aKind          : ItemKind;

        aPipeP         : Storage.Social.Sync.Pipes.PItem;
        aSyncP         : Storage.Social.Sync.PItem;
        aHdrP          : Storage.Social.Sync.PHeader;

        aFolderP       : Storage.Social.Folders.PSFolder;
        aFileP         : Storage.Social.Files.PSFile;
        aLocalized     : Core.Strings.VarString;
        aRemote        : Core.Strings.VarString

      ) : Item;

    end;
  end;


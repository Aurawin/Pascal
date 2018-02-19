unit Storage.Social.Sync.Queue;


interface

uses
  App.Consts,
  App.Lang,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,


  Core.Generics,
  Core.Interlocked,
  Core.Timer,
  Core.Threads,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,
  Core.Utils.Files,
  Core.Strings,
  Core.Streams,
  Core.XML,
  Encryption.Base64,

  Storage,
  Storage.Social,
  Storage.Social.Sync,
  Storage.Social.Sync.Pipes,
  Storage.Social.Network,
  Storage.Social.Network.Requests,
  Storage.Social.Folders,
  Storage.Social.Files,


  Storage.Avatars,
  Storage.UserStorage,
  Storage.CoreObjects,
  Storage.Roster,
  Storage.UserAccounts,
  Storage.Vendors,
  Storage.Domains,
  Storage.Main,
  Storage.MatrixNodes,
  Storage.AuraDisks,


  DOM,
  MD5,
  XMLRead,
  Classes,
  SysUtils;
Type
  TItem=class;
  ItemKind=(Create,Read,Write);
  ItemEvent = procedure(Item:TItem) of object;
  FileEvent=procedure(RSRP:PRSR; Request:THTTPRequest; Item:TItem) of object;

  Status=class
  type
    Operation=(soNone,soErrored,soVerified,soScheduled,soRequested,soPosted);
  const
    Kind : Array[ItemKind] of Core.Strings.PVarString = (
       @App.Lang.Table.Engine.Status.Sync.Add,
       @App.Lang.Table.Engine.Status.Sync.Read,
       @App.Lang.Table.Engine.Status.Sync.Write
    );
  end;

  TItem=class(CThreadObject)
  public
    ID            : QWord;
    Size          : QWord;
    Status        : Status.Operation;
    Digest        : TMD5Digest;
    Created       : Double;
    Commanded     : Double;
    SyncP         : Storage.Social.Sync.PSync;
    PipeP         : Storage.Social.Sync.Pipes.PItem;
    HdrP          : Storage.Social.Sync.PHeader;

    FolderP       : Storage.Social.Folders.PSFolder;
    FileP         : Storage.Social.Files.PSFile;
    LocalizedFile : Core.Strings.VarString;
    RemoteFile    : Core.Strings.VarString;
    Kind          : ItemKind;

  public
    destructor Destroy(); override;
    Constructor Create(
      aThread        : CThread;
      aKind          : ItemKind;
      aPipeP         : Storage.Social.Sync.Pipes.PItem;
      aSyncP         : Storage.Social.Sync.PSync;

      aFolderP       : Storage.Social.Folders.PSFolder;
      aFileP         : Storage.Social.Files.PSFile;
      aLocalized     : Core.Strings.VarString;
      aRemote        : Core.Strings.VarString
    ); reIntroduce;
  end;
  ItemsList = specialize GObjectThreadList<TItem>;
  TItems=class
  private
    Owner         : TRSRManager;
    FList         : ItemsList;
    FTransferCount: DWORD;
    FTransferSize : QWord;
    FCalculatedFileSize : QWord;
    FLock         : TRTLCriticalSection;
  private
    FOnFileCreate : FileEvent;
    FOnFileWrite  : FileEvent;
    FOnFileRead   : FileEvent;
  private
    function calculateSize():QWord;
  public
    SyncItem                     : TItem;
  protected
    FOnSyncItemQueued            : TSynchronizedMethod;
    FOnSyncItemRemoved           : TSynchronizedMethod;
  private
    procedure processWrite(RSRP:PRSR; Request:THTTPRequest; aItem:TItem);
    procedure processCreate(RSRP:PRSR; Request:THTTPRequest; aItem:TItem);
    procedure processRead(RSRP:PRSR; Request:THTTPRequest; aItem:TItem);
  private
    procedure NotifyQueued(aCaller:CThread; aItem:TItem);
    procedure NotifyRemoved(aCaller:CThread; aItem:TItem);
  public
    constructor Create(aOwner:TRSRManager); virtual;
    destructor Destroy(); override;
  public
    procedure Process(aCaller:CThread; RSRP:PRSR; Request:THTTPrequest);
  public
    property OnFileCreate : FileEvent read FOnFileCreate write FOnFileCreate;
    property OnFileWrite  : FileEvent read FOnFileWrite write FOnFileWrite;
    property OnFileRead   : FileEvent read FOnFileRead write FOnFileRead;
  public
    property CalculatedFileSize:QWord read FCalculatedFileSize;
  public
    property OnSyncItemQueued : TSynchronizedMethod read FOnSyncItemQueued write FOnSyncItemQueued;
    property OnSyncItemRemoved : TSynchronizedMethod read FOnSyncItemRemoved write FOnSyncItemRemoved;
  public
    function Find(aFolderId:QWord; aFile:Core.Strings.VarString):TItem; overload;
    function Find(aKind:ItemKind; aFileP:PSFile) : TItem; overload;
    function Find(aKind:ItemKind; aFolderId:QWord; aFileId:QWord):TItem; overload;
    function Find(aKind:ItemKind; aFolderId:QWord; aFile:Core.Strings.VarString):TItem; overload;

    function Schedule(
      aCaller        : CThread;
      aKind          : ItemKind;

      aPipeP         : Storage.Social.Sync.Pipes.PItem;
      aSyncP         : Storage.Social.Sync.PSync;

      aFolderP       : Storage.Social.Folders.PSFolder;
      aFileP         : Storage.Social.Files.PSFile;
      aLocalized     : Core.Strings.VarString;
      aRemote        : Core.Strings.VarString

    ) : TItem;

  end;
implementation
  uses DateUtils;

constructor TItems.Create(aOwner:TRSRManager);
begin
  FOnFileCreate:=nil;
  FOnFileWrite:=nil;
  FOnFileRead:=nil;
  FTransferCount:=0;
  Owner:=aOwner;
  InitCriticalSection(FLock);
  FList:=ItemsList.Create(Core.Generics.Defaults.FreeOnClear);
  Inherited Create();
end;

destructor TItems.Destroy();
begin
  FList.Clear();
  FList.Free();
  DoneCriticalSection(FLock);
  Inherited Destroy();
end;


Constructor TItem.Create(
  aThread        : CThread;
  aKind          : ItemKind;

  aPipeP         : Storage.Social.Sync.Pipes.PItem;
  aSyncP         : Storage.Social.Sync.PSync;

  aFolderP       : Storage.Social.Folders.PSFolder;
  aFileP         : Storage.Social.Files.PSFile;

  aLocalized     : Core.Strings.VarString;
  aRemote        : Core.Strings.VarString
);

var
  aCallerId      : TThreadID;
  aCurrentId     : TThreadID;

begin

  aCallerId:=System.GetCurrentThreadId;
  aCurrentId:=aThread.Handle;

  //if (aCallerId<>aCurrentId) then
  //  raise Exception.Create('Queue Item must be created in the context of specified thread');

  Kind:=aKind;

  PipeP:=aPipeP;
  SyncP:=aSyncP;

  FolderP:= aFolderP;
  FileP:=aFileP;

  LocalizedFile:=aLocalized;
  RemoteFile:=aRemote;

  Created:=Core.Timer.dtUT;
  Commanded:=0;

  inherited Create(aThread);
end;

Destructor TItem.Destroy();
begin
  SyncP:=nil;
  PipeP:=nil;
  HdrP:=nil;
  FolderP:=nil;
  FileP:=nil;

  inherited Destroy();
end;



procedure TItems.processWrite(RSRP:PRSR; Request:THTTPRequest; aItem:TItem);
begin
  if Assigned(FOnFileWrite) then FOnFileWrite(RSRP,Request,aItem);
end;

procedure TItems.processCreate(RSRP:PRSR; Request:THTTPRequest; aItem:TItem);
begin
  if Assigned(FOnFileCreate) then FOnFileCreate(RSRP,Request,aItem);
end;

procedure TItems.processRead(RSRP:PRSR; Request:THTTPRequest; aItem:TItem);
begin
  if Assigned(FOnFileRead) then FOnFileRead(RSRP,Request,aItem);
end;

procedure TItems.NotifyQueued(aCaller:CThread; aItem:TItem);
begin
  EnterCriticalSection(FLock);
  Try
   SyncItem:=aItem;
   aCaller.Synchronize(aCaller,FOnSyncItemQueued);
   SyncItem:=nil;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TItems.NotifyRemoved(aCaller:CThread; aItem:TItem);
begin
  EnterCriticalSection(FLock);
  Try
   SyncItem:=aItem;
   aCaller.Synchronize(aCaller,FOnSyncItemRemoved);
   SyncItem:=nil;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

function TItems.Schedule(
  aCaller        : CThread;
  aKind          : ItemKind;
  aPipeP         : Storage.Social.Sync.Pipes.PItem;
  aSyncP         : Storage.Social.Sync.PSync;

  aFolderP       : Storage.Social.Folders.PSFolder;
  aFileP         : Storage.Social.Files.PSFile;

  aLocalized     : Core.Strings.VarString;
  aRemote        : Core.Strings.VarString

) : TItem;
begin
  Result := Find(aKind,aFileP);
  if (Result<>nil) then begin
    Result.Status:=Result.Status;
  end else begin
    Result :=TItem.Create(aCaller,aKind,aPipeP,aSyncP,aFolderP,aFileP,aLocalized,aRemote);
    FList.Add(Result);

    Result.Status:=Status.Operation.soScheduled;
    Result.Size:=aFileP^.Size;
    Result.Id:=aFileP^.Id;

    Core.Arrays.Bytes.Copy(aFileP^.Digest,Result.Digest);
    FCalculatedFileSize:=calculateSize();
    if Assigned(FOnSyncItemQueued) then
      NotifyQueued(aCaller,Result);
  end;
end;


procedure TItems.Process(aCaller:CThread; RSRP:PRSR; Request:THTTPrequest);
var
  iLcv:integer;
  itms:TList;
  itm:TItem;
  iCount:QWord;

begin
  itms:=FList.LockList;
  try
    iLcv:=0;
    iCount:=itms.Count;
    while (iLcv<iCount) do begin
      RSRP^.Throttle.Consumption:=RSRP^.SendBuffer.Stream.Size+RSRP^.RecvBuffer.Stream.Size;
      itm:=FList[iLcv];
      case itm.Kind of
        ItemKind.Create: begin
          // after create, and id!=0, switch to write
          if (itm.Status=soScheduled) then begin
            if (
              (FTransferCount=0) or
              (
                (FTransferCount<App.Consts.RSR.Engine.Settings.QueueItemsMax) and
                ((itm.Size+RSRP^.Throttle.Consumption) <=  RSRP^.Throttle.Limit)
              )
            ) then begin
              InterlockedIncrement(FTransferCount);
              processCreate(RSRP,Request,itm);
              itm.Status:=soRequested;
              itm.Commanded:=Core.Timer.dtUT;
            end;
          end else if (itm.Status=soVerified) then begin
            InterlockedDecrement(FTransferCount);
            itm.Kind:=ItemKind.Write;
            itm.Status:=soScheduled;
          end;
        end;
        ItemKind.Read : begin
          if (itm.Status=soScheduled) then begin
            if (
              (FTransferCount=0) or
              (
                (FTransferCount<App.Consts.RSR.Engine.Settings.QueueItemsMax) and
                ((itm.Size+RSRP^.Throttle.Consumption) <=  RSRP^.Throttle.Limit)
              )
            ) then begin
                InterlockedIncrement(FTransferCount);
                FTransferSize+=itm.Size;
                processRead(RSRP,Request,itm);
                itm.Status:=soRequested;
                itm.Commanded:=Core.Timer.dtUT;
              end;
          end else if (itm.Status=soVerified) then begin
             InterlockedDecrement(FTransferCount);
             FTransferSize-=itm.Size;

             itms.Remove(itm);
             iLcv-=1;
             iCount-=1;
             FCalculatedFileSize:=calculateSize();

             if Assigned(FOnSyncItemRemoved) then
               NotifyRemoved(aCaller,itm);

             itm.Release();

          end;
        end;
        ItemKind.Write: begin
          if (itm.Status=soScheduled) then begin
               if (
                 (FTransferCount=0) or
                 (
                   (FTransferCount<App.Consts.RSR.Engine.Settings.QueueItemsMax) and
                   ((itm.Size+RSRP^.Throttle.Consumption) <=  RSRP^.Throttle.Limit)
                 )
               ) then begin
                InterlockedIncrement(FTransferCount);
                FTransferSize+=itm.Size;
                processWrite(RSRP,Request,itm);
                itm.Status:=soRequested;
                itm.Commanded:=Core.Timer.dtUT;
              end;
          end else if (itm.Status=soVerified) then begin
             InterlockedDecrement(FTransferCount);
             FTransferSize-=itm.Size;

             itms.Remove(itm);
             iCount-=1;
             iLcv-=1;

             FCalculatedFileSize:=calculateSize();

             if Assigned(FOnSyncItemRemoved) then
               NotifyRemoved(aCaller,itm);

             itm.Release();

          end;
        end;
      end;
      iLcv+=1;
    end;

  finally
    FList.UnlockList;
  end;
end;

function TItems.calculateSize():QWord;
var
  iLcv:integer;
  itms:TList;
  itm:TItem;
begin
   itms:=FList.LockList;
   Result:=0;
   try
     for iLcv:=0 to itms.Count-1 do begin
       itm:=FList[iLcv];
       Result+=itm.Size;
     end;
   finally
     FList.UnlockList;
   end;
end;

function TItems.Find(aKind:ItemKind; aFileP:PSFile):TItem;
var
  iLcv:integer;
  itms:TList;
  itm:TItem;
begin
  Result:=nil;
  itms:=FList.LockList;
  try
    for iLcv:=0 to itms.Count-1 do begin
        itm:=FList[iLcv];
        if (itm.Kind=aKind) and (itm.FileP=aFileP) then begin
           Result:=itm;
           break;
        end;
    end;
  finally
    FList.UnlockList;
  end;
end;

function TItems.Find(aKind:ItemKind; aFolderId:QWord; aFileId:QWord):TItem;
var
  iLcv:integer;
  itms:TList;
  itm:TItem;
begin
  Result:=nil;
  itms:=FList.LockList;
  try
     for iLcv:=0 to itms.Count-1 do begin
         itm:=FList[iLcv];
         if (itm.Kind=aKind) and (itm.FolderP^.Id=aFolderId) and (itm.FileP^.Id=aFileId) then begin
            Result:=itm;
            break;
         end;
     end;
  finally
    FList.UnlockList;
  end;
end;

function TItems.Find(aFolderId:QWord; aFile:Core.Strings.VarString):TItem;
var
  iLcv:integer;
  itms:TList;
  itm:TItem;
begin
  Result:=nil;
  itms:=FList.LockList;
  try
     for iLcv:=0 to itms.Count-1 do begin
         itm:=TItem(itms[iLcv]);
         if (itm.FolderP^.Id=aFolderId) and (itm.FileP^.Name=aFile) then begin
            Result:=itm;
            exit;
         end;
     end;
  finally
     FList.UnlockList;
  end;

end;

function TItems.Find(aKind:ItemKind; aFolderId:QWord; aFile:Core.Strings.VarString):TItem;
var
  iLcv:integer;
  itms:TList;
  itm:TItem;
begin
  Result:=nil;
  itms:=FList.LockList;
  try
     for iLcv:=0 to itms.Count-1 do begin
         itm:=TItem(itms[iLcv]);
         if (itm.Kind=aKind) and (itm.FolderP^.Id=aFolderId) and (itm.FileP^.Name=aFile) then begin
            Result:=itm;
            break;
         end;
     end;
  finally
     FList.UnlockList;
  end;
end;

end.


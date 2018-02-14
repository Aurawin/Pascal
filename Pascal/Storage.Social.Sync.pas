unit Storage.Social.Sync;



interface

uses
  App.Consts,

  RSR,
  RSR.Core,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.Timer,
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

type
  XML=class
  Const
    Stanza                     : Core.Strings.VarString ='sync';
  Type
    Fields=class
    const
      ID                       : Core.Database.Types.VarString = 'id';
      ResourceID               : Core.Database.Types.VarString = 'rcid';
      ChannelID                : Core.Database.Types.VarString = 'chid';
      NetworkID                : Core.Database.Types.VarString = 'nid';
      Modified                 : Core.Database.Types.VarString = 'mtd';
      Pipes                    : Core.Database.Types.VarString = 'pipes';
    end;
  end;
  DB = class
  type
    IDs = class
    const
      ID                       : Core.Database.Types.Integer = 0;
      InsertID                 : Core.Database.Types.Integer = 1;
      DomainID                 : Core.Database.Types.Integer = 2;
      UserID                   : Core.Database.Types.Integer = 3;
      ResourceID               : Core.Database.Types.Integer = 4;
      NetworkID                : Core.Database.Types.Integer = 5;
      Modified                 : Core.Database.Types.Integer = 6;
      Pipes                    : Core.Database.Types.Integer = 7;
    end;
    Keys=class
    const
      ID                       : Core.Database.Types.VarString = 'ITID';
      InsertID                 : Core.Database.Types.VarString = 'IIID';
      DomainID                 : Core.Database.Types.VarString = 'IDID';
      UserID                   : Core.Database.Types.VarString = 'IUID';
      ResourceID               : Core.Database.Types.VarString = 'IRCD';
      NetworkID                : Core.Database.Types.VarString = 'INID';
      Modified                 : Core.Database.Types.VarString = 'IMOD';
      Pipes                    : Core.Database.Types.VarString = 'IPIPS';
    end;
  const
    TableP   : Core.Database.Types.PTable = nil;
    MonitorP : Core.Database.Monitor.Types.PItem = nil;
    Startup  : Core.Database.Types.TableIni = (
      AutoCreate               : True;
      AutoCommit               : True;
      Group                    : 'System/Applications/Social';
      Name                     : 'Synchronize';
      Value                    : 'scs_soc_sync';
      Hint                     : 'Network Synchronization Data';
      PrimaryKeyP              : @Keys.ID;
      );
    Fields: array [0..7] of Core.Database.Types.Field = (
      (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
      (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.DomainID;  KeyP: @Keys.DomainID;DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.ResourceID; KeyP: @Keys.ResourceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Pipes; KeyP: @Keys.Pipes; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
    );
  end;
  TTL=class
  const
    Files                    = 240; // seconds
    Folders                  = 240; // seconds
  end;
  Defaults=class
  const
    ThresholdMilliseconds    = 1000;  // milliseconds
  end;
  PHeader=^THeader;
  PSync=^TSync;
  THeaderList=Array of PHeader;
  State=(rqsNone,rqsRequested,rqsReceived);
  PHeaderList=^THeaderList;
  THeader=record
    ID                       : QWord;
    Modified                 : double;
    ResourceID               : QWord;
    NetworkID                : QWord;
    ChannelID                : QWord;
    Pipes                    : Core.Strings.VarString;
    // runtime
    ItemP                    : PSync;
  end;
  TUpdateInfo=record
    LastChecked              : double;
    stateFolders             : State;
    stateFiles               : State;
  end;
  TSync=record
    Header                   : THeader;
    TimerP                   : Core.Timer.PItem;
    Modified                 : boolean;
    statePipes               : State;
    UpdateInfo               : TUpdateInfo;
    Lock                     : TRTLCriticalSection;
    Network                  : Storage.Social.Network.TNetwork;
    Pipes                    : Storage.Social.Sync.Pipes.TItems;
    Folders                  : Storage.Social.Folders.TMasterList;
    Verified                 : Boolean;
  end;
  TSyncEvent=procedure (var Item:TSync) of object;
  THeaderEvent=procedure (var Item:THeader) of object;

  function  fromXML(xDoc:TXMLDocument; var Hdr:THeader; ItemP:PSync):boolean;

  function  toXML(var Item:THeader; Output:TMemoryStream; Header:Boolean):boolean;
  function  Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID,NetworkID:QWord; Var Item:THeader):boolean; overload;
  function  Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID,NetworkID:QWord; out ItemID:QWord):boolean; overload;
  function  Read(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID,NetworkID:QWord; var Item:THeader):boolean;

  function  setModified(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Value:Double):boolean;
  function  getModified(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; out Value:Double):boolean;

  function  Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:THeader):boolean; overload;
  function  Write(Task:Core.Database.Types.TTask; Refactor:TMemoryStream; var DomainID,UserID,ItemID:QWord; var Items:Storage.Social.Sync.Pipes.TItems):Boolean; overload;

  function  Create(var aHeader:THeader; var aNetwork:TNetwork; var List:THeaderList):PHeader; overload;
  function  Create(var aNetwork:TNetwork; var List:THeaderList):PHeader; overload;
  function  Get(var Network:TNetwork; var List:THeaderList):PHeader overload;
  function  Get(NetworkID:QWord; var List:THeaderList):PHeader; overload;
  function  Get(const PipeP:Storage.Social.Sync.Pipes.PItem; var List:THeaderList):PHeader; overload;
  function  Force(var aHdr:THeader; Var aNetwork:TNetwork; var List:THeaderList):PHeader; overload;
  function  Force(var aNetwork:TNetwork; var List:THeaderList):PHeader; overload;
  procedure Force(var Networks:TNetworks; var List:THeaderList); overload;
  function  Force(NetworkID:QWord; var List:THeaderList):PHeader; overload;

  procedure Discover(Folders:Storage.Social.Folders.TMasterList; Pipes:Storage.Social.Sync.Pipes.TItems);

  procedure Empty(var Item:THeader); overload;
  procedure Empty(var Item:TSync); overload;
  procedure Empty(var Item:TUpdateInfo); overload;
  procedure Empty(var Item:THeaderList); overload;

  procedure Invalidate(var Items:THeaderList);
  procedure Purge(var Items:THeaderList);

  procedure Done(Var Item:THeader); overload;
  procedure Done(var Item:TSync); overload;
  procedure Done(var Item:TUpdateInfo); overload;
  procedure Done(var Item:THeaderList); overload;

  procedure Init(var Item:THeader; OwnerP:PSync); overload;
  procedure Init(var Item:TSync); overload;
  procedure Init(var Item:TUpdateInfo); overload;
  procedure Init(var Item:THeaderList); overload;

  procedure Copy(var Source,Destination:THeader);

implementation

procedure cbDestroySync(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP=DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if (ItemP=DB.MonitorP) then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushDeviceDeleted();
  begin
    if ItemP = DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ResourceID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;
begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED      : PushDomainDeleted;
    Core.Database.Monitor.Notify.USER_DELETED        : PushUserDeleted;
    Core.Database.Monitor.Notify.USER_DEVICE_DELETED : PushDeviceDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then  begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroySync, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

procedure Empty(Var Item:THeader);
begin
  Item.ID:=0;
  Item.Modified:=0;
  Item.ItemP:=nil;
  Item.ResourceID:=0;
  Item.NetworkID:=0;
  Item.ChannelID:=0;
  SetLength(Item.Pipes,0);
end;

procedure Empty(var Item:TUpdateInfo);
begin
  Item.LastChecked:=0;
  Item.stateFiles:=rqsNone;
  Item.stateFolders:=rqsNone;
end;


procedure Empty(Var Item:TSync);
begin
  if (Item.TimerP<>nil) then begin
    TTimerThread(Item.TimerP^.Owner).UnloadEvent(Item.TimerP^,UnloadExecute);
    Core.Timer.Done(Item.TimerP^);
    Dispose(Item.TimerP);
    Item.TimerP:=nil;
  end;
  Empty(Item.Header);
  Item.Verified:=false;
  Item.Modified:=false;
  Item.statePipes:=rqsNone;
  Empty(Item.UpdateInfo);

  Item.Folders.Clear();

  Storage.Social.Network.Empty(Item.Network);
  Storage.Social.Sync.Pipes.Empty(Item.Pipes);
end;

procedure Empty(Var Item:THeaderList);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

procedure Init(Var Item:THeader; OwnerP:PSync);
begin
  Item.ID:=0;
  Item.Modified:=0;
  Item.ResourceID:=0;
  Item.NetworkID:=0;
  Item.ChannelID:=0;
  Item.ItemP:=OwnerP;
  SetLength(Item.Pipes,0);
end;

procedure Init(var Item:TUpdateInfo);
begin
  Item.LastChecked:=0;
  Item.stateFiles:=rqsNone;
  Item.stateFolders:=rqsNone;
end;

procedure Init(var Item:TSync);
begin
  Item.TimerP:=nil;
  Item.Verified:=false;
  Item.Modified:=false;
  Item.statePipes:=rqsNone;

  InitCriticalSection(Item.Lock);

  Init(Item.UpdateInfo);
  Init(Item.Header,@Item);

  Item.Folders:=TMasterList.Create();

  Storage.Social.Network.Init(Item.Network);
  Storage.Social.Sync.Pipes.Init(Item.Pipes);
end;

procedure Init(Var Item:THeaderList);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

procedure Done(Var Item:THeader);
begin
  Finalize(Item.Pipes);
  Finalize(Item);
end;

procedure Done(Var Item:TSync);
begin
  if (Item.TimerP<>nil) then begin
    TTimerThread(Item.TimerP^.Owner).UnloadEvent(Item.TImerP^,UnloadExecute);
    Core.Timer.Done(Item.TimerP^);
    Dispose(Item.TimerP);
    Item.TimerP:=nil;
  end;
  DoneCriticalSection(Item.Lock);
  Done(Item.UpdateInfo);
  Done(Item.Header);

  Storage.Social.Network.Done(Item.Network);
  Storage.Social.Sync.Pipes.Done(Item.Pipes);

  Item.Folders.Free();
  Finalize(Item);
end;


procedure Done(Var Item:TUpdateInfo);
begin
  Finalize(Item);
end;

procedure Done(Var Item:THeaderList);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure Invalidate(var Items:THeaderList);
var
  iLcv:LongInt;
  itmP:PHeader;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    if (itmP<>nil) and (itmP^.ItemP<>nil) then
      itmP^.ItemP^.Verified:=false;
  end;
end;

procedure Purge(var Items:THeaderList);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iStartCt:LongInt;
  iCt:LongInt;
  itmP:PHeader;
begin
  iStartCt:=System.Length(Items);
  iCt:=iStartCt;
  for iLcv:=0 to iCt-1 do begin
    itmP:=Items[iLcv];
    if (itmP<>nil) and  (itmP^.ItemP<>nil) and (itmP^.ItemP^.Verified=false) then begin
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
  while (iLcv<iCt) do begin
    itmP:=Items[iLcv];
    if itmP=nil then begin
      for jLcv:=iLcv to iCt-2 do
        Items[jLcv]:=Items[jLcv+1];
      Dec(iCt);
    end else
      inc(iLcv);
  end;
  if (iStartCt<>iCt) then
    SetLength(Items,iCt);
end;

function Create(var aHeader:THeader; var aNetwork:TNetwork; var List:THeaderList):PHeader;
var
  itmP:PSync;
  iCt:LongInt;
begin
  new(itmP);
  Init(itmP^);
  Copy(aHeader,itmP^.Header);
  Storage.Social.Network.Copy(aNetwork,itmP^.Network);

  iCt:=System.Length(List);
  System.SetLength(List,iCt+1);
  List[iCt]:=@itmP^.Header;
  Result:=List[iCt];
end;

function Create(var aNetwork:TNetwork; var List:THeaderList):PHeader;
var
  itmP:PSync;
  iCt:LongInt;
begin
  new(itmP);
  Init(itmP^);

  itmP^.Header.NetworkID:=aNetwork.ID;
  Storage.Social.Network.Copy(aNetwork,itmP^.Network);

  iCt:=System.Length(List);
  System.SetLength(List,iCt+1);
  List[iCt]:=@itmP^.Header;
  Result:=List[iCt];
end;

function Get(var Network:TNetwork; var List:THeaderList):PHeader;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    if (List[iLcv]^.NetworkID=Network.ID) then begin
      Result:=List[iLcv];
      Break;
    end;
  end;
end;

function Get(NetworkID:QWord; var List:THeaderList):PHeader;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    if (List[iLcv]^.NetworkID=NetworkID) then begin
      Result:=List[iLcv];
      Break;
    end;
  end;
end;

function Get(const PipeP:Storage.Social.Sync.Pipes.PItem; var List:THeaderList):PHeader;
var
  iLcv:LongInt;
  idx:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    idx:=Storage.Social.Sync.Pipes.IndexOf(PipeP,List[iLcv]^.ItemP^.Pipes);
    if idx<>-1 then begin
      Result:=List[iLcv];
      Break;
    end;
  end;
end;

function Force(var aHdr:THeader; Var aNetwork:TNetwork; var List:THeaderList):PHeader;
begin
  Result:=Get(aNetwork,List);
  if Result=nil then
    Result:=Create(aHdr,aNetwork,List);
end;

function Force(Var aNetwork:TNetwork; var List:THeaderList):PHeader;
begin
  Result:=Get(aNetwork,List);
  if Result=nil then
    Result:=Create(aNetwork,List);
end;

procedure Force(Var Networks:TNetworks; var List:THeaderList);
var
  iLcv:LongInt;
  netP:Storage.Social.Network.PNetwork;
  hdrP:PHeader;
begin
  for iLcv:=0 to High(Networks) do begin
    netP:=Networks[iLcv];
    hdrP:=Get(netP^,List);
    if hdrP=nil then
      hdrP:=Create(netP^,List);
  end;
end;

function Force(NetworkID:QWord; var List:THeaderList):PHeader;
var
  itmP:PSync;
  hdrP:PHeader;
  iCt:LongInt;
begin
  hdrP:=Get(NetworkID,List);
  if (hdrP=nil) then begin
    new(itmP);
    Init(itmP^);

    itmP^.Header.NetworkID:=NetworkID;

    iCt:=System.Length(List);
    System.SetLength(List,iCt+1);
    List[iCt]:=@itmP^.Header;
    hdrP:=List[iCt];
  end;
  Result:=hdrP;
end;

procedure Copy(var Source,Destination:THeader);
begin
  Destination.ID:=Source.ID;
  Destination.ChannelID:=Source.ChannelID;
  Destination.NetworkID:=Source.NetworkID;
  Destination.ResourceID:=Source.ResourceID;
  Destination.Modified:=Source.Modified;
  Destination.Pipes:=Source.Pipes;
end;

procedure Discover(Folders:Storage.Social.Folders.TMasterList; Pipes:Storage.Social.Sync.Pipes.TItems);
var
  pipeFldr:Storage.Social.Folders.PSFolder;
  pipeLcv:Storage.Social.Sync.Pipes.PItem;
  iLcv:Integer;

  procedure pushDiscoverSubs(fP:Storage.Social.Folders.PSFolder);
  var
    iLcv:integer;
    fldr:Storage.Social.Folders.PSFolder;
  begin
    for iLcv:=0 to High(fP^.Folders) do begin
      fldr:=fP^.Folders[iLcv];
      fldr^.Location:=Concat(
        fP^.Location,
        App.Consts.PathDelim,
        fldr^.Name
      );
      pushDiscoverSubs(fldr);
    end;
  end;

begin
  for iLcv:=0 to High(Pipes) do begin
    pipeLcv:=Pipes[iLcv];
    pipeFldr:=Folders.FindByPath(pipeLcv^.Name);
    pipeFldr^.Location:=pipeLcv^.Path;
    pushDiscoverSubs(pipeFldr);
  end;

end;

function fromXML(xDoc:TXMLDocument; var Hdr:THeader; ItemP:PSync):boolean;
var
  xItem:TDOMNode;
  xPipes:TDOMNode;
  xItems:TDOMNode;
begin
  Result:=False;
  Empty(Hdr);
  if ItemP<>nil then begin
    Hdr.ItemP:=ItemP;
  end;
  with Core.XML.DB do begin
    xItem:=getNode(xDoc,XML.Stanza);
    if xItem<>nil then begin
      xPipes:=Core.XML.DB.getChildNode(xItem,XML.Fields.Pipes);
      Hdr.ID:=toQWord(xItem,XML.Fields.ID);
      Hdr.ResourceID:=toQWord(xItem,XML.Fields.ResourceID);
      Hdr.NetworkID:=toQWord(xItem,XML.Fields.NetworkID);
      Hdr.ChannelID:=toQWord(xItem,XML.Fields.ChannelID);
      Hdr.Modified:=toDouble(xItem,XML.Fields.Modified);
      if (xPipes<>nil) then begin
        if (ItemP<>nil) then begin
          xItems:=getChildNode(xPipes,Storage.Social.Sync.Pipes.XML.Items);
          if (xItems<>nil) then
            Storage.Social.Sync.Pipes.fromXML(xItems,ItemP^.Pipes);
          ItemP^.statePipes:=rqsReceived;
        end else
          Hdr.Pipes:=getNodeText(xPipes);
      end;
      if ItemP<>nil then
        Storage.Social.Sync.Pipes.VerifyDefaults(ItemP^.Pipes);
      Result:=True;
    end else
      Result:=false;
  end;
end;

function toXML(var Item:THeader; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanza,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.ResourceID,Item.ResourceID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.ChannelID,Item.ChannelID),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Pipes,Item.Pipes,CDATA_OFF),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanza,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

function Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID,NetworkID:QWord; Var Item:THeader):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Item.Modified:=Core.Timer.dtUT;
    iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    iCount:=0;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    {$i Storage.Social.Sync.Add.Fields.inc}
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Pipes,poNone,oNone,Item.Pipes,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID,NetworkID:QWord; out ItemID:QWord):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; ItemID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    {$i Storage.Social.Sync.Add.Fields.inc}
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function setModified(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Value:Double):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Value, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:THeader):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ResourceID, poAnd, oEqual, Item.ResourceID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, Item.NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Pipes, poNone, oNone, Item.Pipes, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetSyncModified(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PDouble(DataP)^:=Fields.FieldByName(DB.Keys.Modified).AsFloat;
end;

procedure cbGetSync(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with PHeader(DataP)^ do begin
    ID:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
    ResourceID:=Fields.FieldByName(DB.Keys.ResourceID).AsLargeInt;
    NetworkID:=Fields.FieldByName(DB.Keys.NetworkID).AsLargeInt;
    Modified:=Fields.FieldByName(DB.Keys.Modified).AsFloat;
    Pipes:=Fields.FieldByName(DB.Keys.Pipes).AsString;
  end;
end;

function Read(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID,NetworkID:QWord; var Item:THeader):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ResourceID, poAnd, oEqual, ResourceID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.NetworkID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ResourceID,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Pipes,poNone,oNone,Commands);
    Result := Core.Database.SQL.Select(Task, @Commands, @cbGetSync, @Item);
    if (Item.ID=0) then
      Result:=Add(Task,DomainID,UserID,ResourceID,NetworkID,Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

function getModified(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; out Value:Double):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbGetSyncModified, @Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  Write(Task:Core.Database.Types.TTask; Refactor:TMemoryStream; var DomainID,UserID,ItemID:QWord; var Items:Storage.Social.Sync.Pipes.TItems):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sValue                         : Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0;
    Refactor.Size:=0;
    Storage.Social.Sync.Pipes.toXML(Items,Refactor,XML_HEADER_OFF);
    sValue:=Core.Streams.toString(Refactor);
    Refactor.Size:=0;


    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Pipes,poNone,oNone,sValue,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Modified,poNone,oNone,dtUT,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDB;

end.


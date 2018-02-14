unit Core.Keywords;

interface
  uses
    Classes,
    Core.Strings,
    Core.Arrays.Types,
    Core.Arrays.VarString,
    Core.Arrays.Boolean,
    Core.Streams,
    Core.Timer
    ;

Const
  FMT_FILE_CAPTION='Please select a file.';
  FMT_FILE_FILTER='HTML Files (*.html)|*.html|HTML Files (*.htm)|*.htm|JavaScript Files (*.js)|*.js|All Files (*.*)|*.*';
  FMT_FILE_INDEX=4;
  FMT_SCAN_ERROR_DISABLED:Core.Strings.VarString='Cannot scan with keyword search engine disabled for this network node.  Please check settings for Cluster (%.D), Resource (%.D), Node (%.D), Domain(%.D)';
  FMT_SCAN_ERROR_NO_DEFAULTS:Core.Strings.VarString='Cannot scan with keyword search engine with node missing default service configuration.  Please check default settings or settings for Cluster (%.D), Resource (%.D), Node (%.D), Domain(%.D)';
Const
  TI_AUTOSAVE=120; // 2m 0 seconds.
type
  PKeyword=^TKeyword;
  TKeywordMethod=function(ItemP:PKeyword):UTF8String of object;
  TKeywordMethods=Array [Boolean] of TKeywordMethod;
  TKeyword=record
    ID                           : Int64;
    Order                        : LongInt;
    UseRefresh                   : Boolean;
    UseCallback                  : Boolean;
    Refresh                      : Boolean;
    Verified                     : Boolean;
    Deleted                      : Boolean;
    References                   : Cardinal;
    Modified                     : TDateTime;
    Method                       : TKeywordMethods;
    Editor                       : Pointer;
    Name                         : Core.Strings.VarString;
    Value                        : Core.Strings.VarString;
  end;

  TKeywords=Array of PKeyword;
  PKeywords=^TKeywords;
  PKWScanInfo=^TKWScanInfo;

  TKWSIList=Array of PKWScanInfo;
  PKWSIList=^TKWSIList;

  TKWScanInfo=record
    KeywordP                     : PKeyword; // Contains ID of Keyword
    Offset                       : Cardinal;
    Length                       : Cardinal;
  end;

  // Instead of Resource content, we read this.
  PKWSFileManifest=^TKWSFileManifest;
  TKWSFileManifest=record
    Count                        : Cardinal;
    PartsP                       : Core.Arrays.Types.PVarString;
    ScanP                        : PKWSIList;
  end;
  PKWSFile=^TKWSFile;
  TKWSFile=record
    dtFile                       : TDateTime;
    dtDownloaded                 : TDateTime;
    Read                         : PKWSFileManifest;
    Write                        : PKWSFileManifest;
  end;

  TKWSEngine=class;
  TKWScanner=class;
  TKWSThreads=Array of TKWScanner;
  TKWRemoveOption=(kwroDeleted,kwroNils,kwroReferences);
  TKWRemoveOptions=set of TKWRemoveOption;
  TKWSCommand=(kwsEngineNone,kwsEngineStart,kwsEngineStop,kwsEngineSetThreads,kwsEngineDestroy);

  TKWSCMDKeyHistory=set of TKWSCommand;
  TScanStatus=(ssNone,ssIdle,ssRunning,ssStop,ssDestroy);
  TKWSECommandKind=(ckNone,ckByte,ckWord,ckInt64,ckPointer);
  PKWSECommand=^TKWSECommand;
  TKWSEValue=Array[0..7] of Byte;
  TKWSECommand=record
    Key                          : TKWSCommand;
    Kind                         : TKWSECommandKind;
    Value                        : TKWSEValue;
    Interval                     : Cardinal;
    dtLive                       : TDateTime;
    KeywordsP                    : PKeywords;
    CompletedP                   : PBooleanArray;
    Flush                        : boolean;
    dtStart                      : Double;
    dtFlush                      : Double;
  end;

  TKWSGetNextKeywordResource=function(CommandP:PKWSECommand; var FileP:PKWSFile; var Resource:Core.Strings.VarString; var Length:Int64):Boolean of object;
  TKWSOnScanComplete=procedure(CommandP:PKWSECommand) of object;

  TKWScanner=class(TThread)
  private
    FOwner                       : TKWSEngine;
    FIndex                       : LongInt;
    FData                        : Core.Strings.VarString;
    FDataLen                     : Int64;
    FKeywordsP                   : PKeywords;
    FKeywordP                    : PKeyword;
    FFileP                       : PKWSFile;
  private
    procedure ProcessFile;
  protected
    procedure Execute; override;
  public
    constructor Create(aIndex:LongInt; aOwner:TKWSEngine); reIntroduce;
    destructor  Destroy; override;
  end;

  TKWSEngine=class(TThread)
  private
    FThreads                     : TKWSThreads;
    FTerminated                  : TBooleanArray;
    FCmdQueue                    : TList;
    FHistory                     : TKWSCMDKeyHistory;
    FLock                        : TRTLCriticalSection;
    FSleepP                      : PRTLEvent;
    FStatus                      : TScanStatus;
    FCommandP                    : PKWSECommand; // Command in iteration.  Will not move to next until completed
    FRecycleP                    : PKWSECommand;

    FOnGetNextItem               : TKWSGetNextKeywordResource;
    FOnScanComplete              : TKWSOnScanComplete;
  private
    procedure SetThreadCount(Value:LongInt);
    function  GetThreadCount:LongInt;
  private
    procedure ProcessCmdQueue;
    procedure ClearQueue;
    procedure ClearList;
  protected
    procedure Execute; override;
  public
    constructor Create; reIntroduce;
    destructor Destroy; override;
  public
    procedure Awake(aValue:pointer);
    procedure Flush(aValue:pointer);
  public
    procedure Execute(aCommand:TKWSCommand; aValue:Byte); overload;
    procedure Execute(aCommand:TKWSCommand; aValue:WORD); overload;
    procedure Execute(aCommand:TKWSCommand; aValue:Int64); overload;
    procedure Execute(aCommand:TKWSCommand; Const Interval:Cardinal; KeywordsP:PKeywords; aValue:Pointer); overload;
    procedure Execute(aCommand:TKWSCommand); overload;
  public
    property Threads:LongInt read GetThreadCount;
    property Status:TScanStatus read FStatus;
    property OnGetNextItem:TKWSGetNextKeywordResource read FOnGetNextItem write FOnGetNextItem;
    property OnScanComplete:TKWSOnScanComplete read FOnScanComplete write FOnScanComplete;
  end;
  procedure Init(Var Item:TKeyword; UseRefresh:Boolean; cbDefault,cbCallback:TKeywordMethod); overload;
  procedure Done(Var Item:TKeyword); overload;
  procedure Done(Var Item:TKeywords); overload;
  procedure Empty(Var Item:TKeyword); overload;
  procedure Empty(Var Item:TKeywords); overload;
  procedure Verify(Var Item:TKeywords; Value:Boolean); overload;
  function  Exchange(srcIndex,destIndex:LongInt; var Items:TKeywords):boolean; overload;
  procedure Remove(Var Item:TKeywords; const Options:TKWRemoveOptions=[kwroDeleted,kwroNils]); overload;


  procedure Empty(var Item:TKWSEValue); overload;
  procedure Init(Var Item:TKWSEValue); overload;
  procedure Done(Var Item:TKWSEValue); overload;

  procedure Init(Var Item:TKWSFileManifest); overload;
  procedure Done(Var Item:TKWSFileManifest); overload;
  procedure Empty(Var Item:TKWSFileManifest); overload;

  function  toString(Var Item:TKWSFileManifest):Core.Strings.VarString; overload;
  procedure toStream(Var Item:TKWSFileManifest; Output:TStream); overload;

  procedure Init(Var Item:TKWSFile); overload;
  procedure Done(Var Item:TKWSFile); overload;
  procedure Swap(Var Item:TKWSFile); overload;


  procedure Add(KeywordP:PKeyword; Offset,Length:Cardinal; Count:Word; Var List:TKWSIList); overload;
  procedure Add(var sData:Core.Strings.VarString; Count:Word; var List:Core.Arrays.Types.VarString); overload;
  procedure Init(Var Item:TKWSIList); overload;
  procedure Empty(Var Item:TKWSIList); overload;
  procedure Done(Var Item:TKWSIList); overload;

  procedure Init(Var Item:TKWSECommand; const Interval:Cardinal; const ThreadCount:Word; KeywordsP:PKeywords; const Key:TKWSCommand=kwsEngineNone); overload;
  procedure Init(Var Item:TKWSECommand; const ThreadCount:Word; const Key:TKWSCommand=kwsEngineNone); overload;
  procedure Done(Var Item:TKWSECommand); overload;

  function toByte(var Source:TKWSEValue):Byte; overload;
  function toWord(var Source:TKWSEValue):Word; overload;
  function toInt64(var Source:TKWSEValue):Int64; overload;
  function toPointer(var Source:TKWSEValue):Pointer; overload;



  procedure fromByte(Source:Byte; var Dest:TKWSECommand); overload;
  procedure fromWord(Source:Word; var Dest:TKWSECommand); overload;
  procedure fromInt64(Source:int64; var Dest:TKWSECommand); overload;
  procedure fromPointer(Source:Pointer; var Dest:TKWSECommand); overload;


  Function  Add(Name,Value:Core.Strings.VarString; Var List:TKeywords; UseRefresh:Boolean; cbDefault,cbCallback:TKeywordMethod):PKeyword; overload;
  Function  Add(ID:Int64; Var List:TKeywords; UseRefresh:boolean; cbDefault,cbCallback:TKeywordMethod):PKeyword; overload;

  Function  IndexOf(Name:Core.Strings.VarString; Var List:TKeywords): LongInt; overload;
  Function  IndexOf(ID:Int64; Var List:TKeywords): LongInt; overload;
  Function  IndexOf(ID:Int64; Var List:TKeywords; out ItemP:PKeyword): LongInt; overload;

  Function  Find(Name:Core.Strings.VarString; Var List:TKeywords):PKeyword; overload;

  Function  fromID(ID:Int64; Var List:TKeywords):PKeyword; overload;

  procedure Delete(ID:Int64; Var List:TKeywords);overload;

  procedure Copy(Var Source,Destination:TKeyword); overload;

Const
  ENGINE_TIME_SLICE_YIELD_MS     : Cardinal = 50;
  WORKER_TIME_SLICE_YIELD_MS     : Array [Boolean] of Cardinal = (250,50);
  ENGINE_AWAKE_PAUSE_MS          : Cardinal = 2000;

  NO_CALLBACK                    : TKeywordMethod = nil;
  KW_REFRESH_OFF                 : boolean = false;
  KW_REFRESH_ON                  : boolean = true;
    
implementation
uses SysUtils,StrUtils,DateUtils;

procedure Empty(Var Item:TKeyword);
begin
  Item.ID:=0;
  Item.Order:=0;
  Item.UseRefresh:=false;
  Item.UseCallback:=false;
  Item.Refresh:=false;
  Item.Verified:=false;
  Item.Deleted:=false;
  Item.References:=0;
  Item.UseCallback:=false;
  Item.Modified:=0;

  Item.Method[false]:=nil;
  Item.Method[true]:=nil;
  Item.Editor:=nil;

  Empty(Item.Name);
  Empty(Item.Value);
end;

procedure Empty(Var Item:TKeywords);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do begin
    if (Item[iLcv]<>nil) then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
    end;
  end;
  SetLength(Item,0);
end;

procedure Verify(Var Item:TKeywords; Value:Boolean);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    if (Item[iLcv]<>nil) and (Item[iLcv]^.UseRefresh) then
      Item[iLcv]^.Verified:=Value;
end;

function Exchange(srcIndex,destIndex:LongInt; var Items:TKeywords):boolean;
var
  iCount:LongInt;
  src,dest:PKeyword;
begin
  Result:=false;
  iCount:=System.Length(Items);
  if (srcIndex=-1) or (destIndex=-1) or (srcIndex>=iCount) or (destIndex>=iCount) then exit;
  src:=Items[srcIndex];
  dest:=Items[destIndex];
  Items[srcIndex]:=dest;
  Items[destIndex]:=src;
  Result:=True;
end;

procedure Remove(Var Item:TKeywords; const Options:TKWRemoveOptions=[kwroDeleted,kwroNils]);
var
  iLength                        : LongInt;
  iCount                         : LongInt;
  iLcv                           : LongInt;

  procedure RemoveNils(iStart:LongInt);
  var
    iLcv:LongInt;
    iJLcv:LongInt;
  begin
    for iLcv:=iStart to iCount-1 do begin
      if Item[iLcv]=nil then begin
        for iJLcv:=iLcv to iCount-2 do
          InterlockedExchange(Item[iJLcv],Item[iJLcv+1]);
        Dec(iCount);
        RemoveNils(iLcv);
        Break;
      end;
    end;
  end;

begin
  iCount:=Length(Item); iLength:=iCount;
  if kwroReferences in Options then begin
    for iLcv:=0 to iCount-1 do
      if (Item[iLcv]<>nil) then
        Item[iLcv]^.References:=0;
  end;
  if kwroDeleted in Options then begin
    for iLcv:=0 to iCount-1  do begin
      if (Item[iLcv]<>nil) and (Item[iLcv]^.References=0) and Item[iLcv]^.Deleted then begin
        Done(Item[iLcv]^);
        Dispose(Item[iLcv]);
        Item[iLcv]:=nil;
      end;
    end;
  end;
  if kwroNils in Options then begin
    RemoveNils(0);
    if iLength<>iCount then
      SetLength(Item,iCount);
  end;
end;

procedure Init(Var Item:TKeyword; UseRefresh:Boolean; cbDefault,cbCallback:TKeywordMethod);
begin
  Item.Order:=0;
  Item.ID:=0;
  Item.Modified:=0;
  Item.References:=0;
  Item.Refresh:=false;
  Item.UseRefresh:=UseRefresh;
  Item.Verified:=false;
  Item.Deleted:=false;
  Item.Editor:=nil;
  Item.Method[false]:=cbDefault;
  Item.Method[true]:=cbCallback;
  Item.UseCallback:=(cbCallback<>nil) and (cbDefault<>cbCallback);
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
end;

procedure Done(Var Item:TKeyword);
begin
  Finalize(Item.Name);
  Finalize(Item.Value);
  Finalize(Item);
end;

procedure Done(Var Item:TKeywords);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do begin
    if Item[iLcv]<>nil then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
    end;
  end;
  Finalize(Item);
end;

Function Add(Name,Value:Core.Strings.VarString; Var List:TKeywords; UseRefresh:Boolean; cbDefault,cbCallback:TKeywordMethod):PKeyword;
var
  iIndex:LongInt;
begin
  New(Result);
  Init(Result^,UseRefresh,cbDefault,cbCallback);
  Result^.Name:=Name;
  Result^.Value:=Value;

  iIndex:=Length(List);
  SetLength(List,iIndex+1);
  List[iIndex]:=Result;
end;

Function  Add(ID:Int64; Var List:TKeywords; UseRefresh:boolean; cbDefault,cbCallback:TKeywordMethod):PKeyword;
var
  iIndex:LongInt;
begin
  New(Result);
  Init(Result^,UseRefresh,cbDefault,cbCallback);
  Result^.ID:=ID;

  iIndex:=Length(List);
  SetLength(List,iIndex+1);
  List[iIndex]:=Result;
end;


Function  IndexOf(Name:Core.Strings.VarString; Var List:TKeywords): LongInt;
var
  iLcv,iCount:LongInt;
begin
  iLcv:=0; iCount:=Length(List); Result:=-1;
  While (iLcv<iCount) and (Result=-1) do begin
    If (List[iLcv]<>nil) and (SameText(List[iLcv]^.Name,Name) ) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function  Find(Name:Core.Strings.VarString; Var List:TKeywords):PKeyword;
var
  iLcv,iCount:LongInt;
begin
  iLcv:=0; iCount:=Length(List); Result:=nil;
  While (iLcv<iCount) and (Result=nil) do begin
    If (List[iLcv]<>nil) and (not List[iLcv]^.Deleted) and SameText(List[iLcv]^.Name,Name) then
      Result:=List[iLcv];
    Inc(iLcv);
  end;
end;

Function  fromID(ID:Int64; Var List:TKeywords):PKeyword;
var
  iIndex:LongInt;
begin
  Result:=nil;
  iIndex:=IndexOf(ID,List);
  if iIndex<>-1 then
    Result:=List[iIndex];
end;

Function  IndexOf(ID:Int64; Var List:TKeywords): LongInt;
var
  iLcv,iCount:LongInt;
begin
  iLcv:=0; iCount:=Length(List); Result:=-1;
  While (iLcv<iCount) and (Result=-1) do begin
    If (List[iLcv]<>nil) and (List[iLcv]^.ID=ID) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function  IndexOf(ID:Int64; Var List:TKeywords; out ItemP:PKeyword): LongInt;
begin
  ItemP:=nil;
  Result:=IndexOf(ID,List);
  if Result<>-1 then
    ItemP:=List[Result];
end;

procedure Delete(ID:Int64; Var List:TKeywords);
var
  iIndex : LongInt;
  iLcv   : LongInt;
  iCount : LongInt;
begin
  iIndex:=IndexOf(ID,List);
  if iIndex<>-1 then begin
    iCount:=Length(List);
    Done(List[iIndex]^);
    Dispose(List[iIndex]);
    for iLcv:=iIndex to iCount-2 do
      List[iLcv]:=List[iLcv+1];
    SetLength(List,iCount-1);
  end;
end;

procedure Copy(Var Source,Destination:TKeyword);
begin
  Destination.Name:=Source.Name;
  Destination.Value:=Source.Value;
  Destination.ID:=Source.ID;
end;

procedure Init(Var Item:TKWSEValue);
begin
  System.FillByte(Item,SizeOf(Item),0);
end;

procedure Empty(var Item:TKWSEValue);
begin
  System.FillByte(Item,SizeOf(Item),0);
end;

procedure Done(Var Item:TKWSEValue);
begin
  Finalize(Item);
end;

procedure Init(Var Item:TKWSFileManifest);
begin
  Item.Count:=0;
  New(Item.PartsP);
  New(Item.ScanP);
end;

procedure Empty(Var Item:TKWSFileManifest);
begin
  Item.Count:=0;
  Empty(Item.PartsP^);
  Empty(Item.ScanP^);
end;

procedure Done(Var Item:TKWSFileManifest);
begin
  Done(Item.PartsP^);
  Done(Item.ScanP^);
  Dispose(Item.PartsP);
  Dispose(Item.ScanP);
  Finalize(Item);
end;

function  toString(Var Item:TKWSFileManifest):Core.Strings.VarString;
var
  iLcv:LongInt;
  InfoP:PKWScanInfo;
begin
  SetLength(Result,0);
  for iLcv:=0 to Item.Count-1 do begin
    Result:=Concat(Result,Item.PartsP^[iLcv]);
    Try
    InfoP:=Item.ScanP^[iLcv];
    if (InfoP<>nil) and (InfoP^.KeywordP<>nil) then begin
      if InfoP^.KeywordP^.UseCallback and (InfoP^.KeywordP^.Method[InfoP^.KeywordP^.UseCallback]<>nil) then
        Result:=Concat(Result,InfoP^.KeywordP^.Method[InfoP^.KeywordP^.UseCallback](InfoP^.KeywordP))
      else
        Result:=Concat(Result,InfoP^.KeywordP^.Value);
    end else
      InfoP:=nil;
    Except
      On E:Exception do begin
        Result:=Concat(Result,'');
      end;
    end;
  end;
end;

procedure toStream(Var Item:TKWSFileManifest; Output:TStream);
var
  iLcv:LongInt;
  InfoP:PKWScanInfo;
begin
  Output.Size:=0;
  for iLcv:=0 to Item.Count-1 do begin
    Core.Streams.Write(Item.PartsP^[iLcv],Output);
    Try
    InfoP:=Item.ScanP^[iLcv];
    if (InfoP<>nil) and (InfoP^.KeywordP<>nil) then begin
      if InfoP^.KeywordP^.UseCallback and (InfoP^.KeywordP^.Method[InfoP^.KeywordP^.UseCallback]<>nil) then
        Core.Streams.Write(InfoP^.KeywordP^.Method[InfoP^.KeywordP^.UseCallback](InfoP^.KeywordP),Output)
      else
        Core.Streams.Write(InfoP^.KeywordP^.Value,Output);
    end else
      InfoP:=nil;
    Except
      On E:Exception do begin
        // to do
      end;
    end;
  end;
end;

procedure Init(Var Item:TKWSFile);
begin
  Item.dtFile:=0;
  Item.dtDownloaded:=0;
  New(Item.Read);
  Init(Item.Read^);
  New(Item.Write);
  Init(Item.Write^);
end;

procedure Done(Var Item:TKWSFile);
begin
  Done(Item.Read^);
  Done(Item.Write^);
  Dispose(Item.Read);
  Dispose(Item.Write);
  Finalize(Item);
end;

procedure Swap(Var Item:TKWSFile);
begin
  InterLockedExchange(Item.Read,InterLockedExchange(Item.Write,Item.Read));
end;

procedure Init(Var Item:TKWSIList);
begin
  SetLength(Item,0);
end;

procedure Done(Var Item:TKWSIList);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    if Item[iLcv]<>nil then begin
      Finalize(Item[iLcv]^);
      Dispose(Item[iLcv]);
    end;
  end;
  SetLength(Item,0);
  Finalize(Item);
end;

procedure Add(KeywordP:PKeyword; Offset,Length:Cardinal; Count:Word; Var List:TKWSIList);
begin
  SetLength(List,Count+1);
  New(List[Count]);
  List[Count]^.KeywordP:=KeywordP;
  List[Count]^.Offset:=Offset;
  List[Count]^.Length:=Length;
end;

procedure Empty(Var Item:TKWSIList); // Could use a recycler here to speed things up
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Dispose(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Add(var sData:Core.Strings.VarString; Count:Word; var List:Core.Arrays.Types.VarString);
begin
  SetLength(List,Count+1);
  List[Count]:=sData;
end;

procedure Init(Var Item:TKWSECommand; const Interval:Cardinal; Const ThreadCount:Word; KeywordsP:PKeywords; const Key:TKWSCommand=kwsEngineNone);
begin
  Item.Flush:=false;
  Item.dtFlush:=0;
  Item.dtStart:=0;
  Item.Interval:=Interval;
  Item.dtLive:=Core.Timer.dtNow;
  Item.Key:=Key;
  Item.KeywordsP:=KeywordsP;
  Item.CompletedP:=nil;
  Item.Kind:=ckNone;
  Init(Item.Value);
end;

procedure Init(Var Item:TKWSECommand; Const ThreadCount:Word; const Key:TKWSCommand=kwsEngineNone);
begin
  Item.Flush:=false;
  Item.dtFlush:=0;
  Item.dtStart:=0;
  Item.Key:=Key;
  Item.Interval:=0;
  Item.dtLive:=0;
  Item.KeywordsP:=nil;
  Item.CompletedP:=nil;
  Item.Kind:=ckNone;
  Init(Item.Value);
end;

procedure Done(Var Item:TKWSECommand);
begin
  if Item.CompletedP<>nil then begin
    Done(Item.CompletedP^);
    Dispose(Item.CompletedP);
  end;
  Finalize(Item);
end;

function toByte(var Source:TKWSEValue):Byte;
begin
  System.Move(Source[0],Result,SizeOf(Result));
end;

function toWord(var Source:TKWSEValue):word;
begin
  System.Move(Source[0],Result,SizeOf(Result));
end;

function toInt64(var Source:TKWSEValue):Int64;
begin
  System.Move(Source[0],Result,SizeOf(Result));
end;

function toPointer(var Source:TKWSEValue):pointer;
begin
  System.Move(Source[0],Result,SizeOf(Result));
end;

procedure fromByte(Source:Byte; var Dest:TKWSECommand);
begin
  Empty(Dest.Value);
  Dest.Kind:=ckByte;
  System.Move(Source,Dest.Value[0],SizeOf(Source));
end;

procedure fromWord(Source:Word; var Dest:TKWSECommand);
begin
  Empty(Dest.Value);
  Dest.Kind:=ckWord;
  System.Move(Source,Dest.Value[0],SizeOf(Source));
end;

procedure fromInt64(Source:Int64; var Dest:TKWSECommand);
begin
  Empty(Dest.Value);
  Dest.Kind:=ckInt64;
  System.Move(Source,Dest.Value[0],SizeOf(Source));
end;

procedure fromPointer(Source:Pointer; var Dest:TKWSECommand);
begin
  Empty(Dest.Value);
  Dest.Kind:=ckPointer;
  System.Move(Source,Dest.Value[0],SizeOf(Source));
end;

constructor TKWSEngine.Create();
begin
  FHistory:=[];
  FStatus:=ssNone;
  FreeOnTerminate:=True;
  FSleepP:=RTLEventCreate;
  SetLength(FThreads,0);
  FCmdQueue:=TList.Create;
  InitCriticalSection(FLock);
  Inherited Create(false);
end;

destructor TKWSEngine.Destroy;
begin
  ClearQueue;
  ClearList;
  Finalize(FThreads);
  FreeAndNil(FCmdQueue);
  DoneCriticalSection(FLock);
  RTLEventDestroy(FSleepP);
  Inherited Destroy;
end;

procedure TKWSEngine.Execute;
begin
  Priority:=tpNormal;
  While Not Terminated and (FStatus<>ssDestroy) do begin
    {$ifdef SyncRSR}
      Synchronize(@ProcessCmdQueue);
    {$else}
      ProcessCmdQueue;
    {$endif}
    // If running check for completeness and set to idle if all complete
    if (FCommandP<>nil) and (FStatus=ssRunning) then begin
      if Core.Arrays.Boolean.All(true,FCommandP^.CompletedP^) then begin
        // Set Yourself to idle
        FStatus:=ssIdle;
        FOnScanComplete(FCommandP);
        Remove(FCommandP^.KeywordsP^,[kwroDeleted,kwroNils]);
        if (FCommandP^.Interval=0) then begin
          Done(FCommandP^);
          Dispose(FCommandP);
          FCommandP:=nil;
        end else begin
          InterlockedExchange(FRecycleP,FCommandP);
          InterlockedExchange(FCommandP,nil);
          Fill(FRecycleP^.CompletedP^,False);
          EnterCriticalSection(FLock);
          Try
            FRecycleP^.dtStart:=0;
            if FRecycleP^.Flush and (FRecycleP^.dtFlush<FRecycleP^.dtStart) then begin
              FRecycleP^.Flush:=false;
              FRecycleP^.dtFlush:=0;
            end;
            FRecycleP^.dtLive:=DateUtils.IncSecond(Core.Timer.dtNow,FRecycleP^.Interval);
            FCmdQueue.Add(FRecycleP);
          finally
            LeaveCriticalSection(FLock);
          end;
        end;
      end;
    end;

    {
    Case FStatus of
      msRunning : If AreAllComplete then FStatus:=msIdle;
      msStop    : If AreAllComplete then FStatus:=msIdle;
      msDestroy : If AllAreTerminated then Terminate;
    end;
    }
    RTLeventWaitFor(FSleepP,ENGINE_TIME_SLICE_YIELD_MS);
  end;
end;

procedure TKWSEngine.Execute(aCommand:TKWSCommand; aValue:Byte);
var
  cmdP:PKWSECommand;
begin
  New(cmdP);
  Init(cmdP^,GetThreadCount,aCommand);
  fromByte(aValue,cmdP^);
  EnterCriticalSection(FLock);
  Try
    FCmdQueue.Add(cmdP);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.Execute(aCommand:TKWSCommand; aValue:WORD);
var
  cmdP:PKWSECommand;
begin
  New(cmdP);
  Init(cmdP^,GetThreadCount,aCommand);
  fromWord(aValue,cmdP^);
  EnterCriticalSection(FLock);
  Try
    FCmdQueue.Add(cmdP);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.Execute(aCommand:TKWSCommand; aValue:Int64);
var
  cmdP:PKWSECommand;
begin
  New(cmdP);
  Init(cmdP^,GetThreadCount,aCommand);
  fromInt64(aValue,cmdP^);
  EnterCriticalSection(FLock);
  Try
    FCmdQueue.Add(cmdP);
  finally
    LeaveCriticalSection(FLock);
  end;
end;


procedure TKWSEngine.Execute(aCommand:TKWSCommand; Const Interval:Cardinal; KeywordsP:PKeywords; aValue:Pointer);
var
  cmdP:PKWSECommand;
begin
  New(cmdP);
  Init(cmdP^,Interval,GetThreadCount,KeywordsP,aCommand);
  fromPointer(aValue,cmdP^);
  EnterCriticalSection(FLock);
  Try
    FCmdQueue.Add(cmdP);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.Awake(aValue:pointer);
var
  iLcv:LongInt;
  cmdP:PKWSECommand;
begin
  EnterCriticalSection(FLock);
  Try
    for iLcv:=0 to FCmdQueue.Count-1 do begin
      Try
        cmdP:=FCmdQueue.Items[iLcv];
        if (cmdP<>nil) and (cmdP^.Key=kwsEngineStart) and (cmdP^.Interval>0) and (cmdP^.Kind=ckPointer)  and (System.CompareByte(cmdP^.Value,aValue,SizeOf(aValue))=0) then begin
          cmdP^.dtLive:=IncMilliSecond(Core.Timer.dtNow,ENGINE_AWAKE_PAUSE_MS);
          Break;
        end;
      except
        cmdP:=nil;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.Flush(aValue:pointer);
var
  iLcv:LongInt;
  cmdP:PKWSECommand;
begin
  EnterCriticalSection(FLock);
  Try
    for iLcv:=0 to FCmdQueue.Count-1 do begin
      Try
        cmdP:=FCmdQueue.Items[iLcv];

        if (cmdP<>nil) and (cmdP^.Key=kwsEngineStart) and (cmdP^.Interval>0) and (cmdP^.Kind=ckPointer)  and (System.CompareByte(cmdP^.Value,aValue,SizeOf(aValue))=0) then begin
          cmdP^.Flush:=true;
          cmdP^.dtFlush:=Core.Timer.dtNow;
          cmdP^.dtLive:=IncMilliSecond(Core.Timer.dtNow,ENGINE_AWAKE_PAUSE_MS);
          Break;
        end;
      Except
        cmdP:=nil;
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.Execute(aCommand:TKWSCommand);
var
  cmdP:PKWSECommand;
begin
  New(cmdP);
  Init(cmdP^,GetThreadCount,aCommand);
  EnterCriticalSection(FLock);
  Try
    FCmdQueue.Add(cmdP);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.SetThreadCount(Value:LongInt);
var
  iLcv,iCt:LongInt;
begin
  EnterCriticalSection(FLock);
  Try
    ClearList;
    iCt:=Value;
    SetLength(FThreads,iCt);
    SetLength(FTerminated,iCt);
    for iLcv:=0 to iCt-1 do begin
      Try
        TKWScanner.Create(iLcv,Self);
      Except;
        ict:=iLcv;
        SetLength(FThreads,iCt);
        break;
        // Ohboy
      end;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function  TKWSEngine.GetThreadCount:LongInt;
begin
  EnterCriticalSection(FLock);
  Try
    Result:=Length(FThreads);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.ClearQueue;
var
  iCount,iLcv:LongInt;
  cmdP:PKWSECommand;
begin
  EnterCriticalSection(FLock);
  Try
    iCount:=FCmdQueue.Count;
    for iLcv:=0 to iCount-1 do begin
      cmdP:=FCmdQueue.Items[iLcv];
      if cmdP<>nil then begin
        Finalize(cmdP^);
        Dispose(cmdP);
      end;
    end;
    FCmdQueue.Clear;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TKWSEngine.ClearList;
var
  iCount,iLcv:LongInt;
  tdItem:TKWScanner;
begin
  iCount:=Length(FThreads);
  for iLcv:=0 to iCount-1 do begin
    tdItem:=FThreads[iLcv];
    if tdItem<>nil then begin
      System.InterLockedExchange(tdItem.FIndex,-1);
      System.InterLockedExchange(System.Pointer(FThreads[iLcv]),nil);
    end;
  end;
end;

procedure TKWSEngine.ProcessCmdQueue;
var
  iLcv:LongInt;
  cmdP:PKWSECommand;

  procedure ProcessSetCount;
  begin
    if not (kwsEngineSetThreads in FHistory) then begin
      // Safe to enforce destired Count
      Include(FHistory,kwsEngineSetThreads);
      case cmdP^.Kind of
        ckByte : SetThreadCount(toByte(cmdP^.Value));
        ckWord : SetThreadCount(toWord(cmdP^.Value));
      end;
      FStatus:=ssIdle;
    end;
    Finalize(cmdP^);
    Dispose(cmdP);
    FCmdQueue.Items[iLcv]:=nil;
  end;

  procedure ProcessStart;
  begin
    if (kwsEngineSetThreads in FHistory) and (FStatus<>ssRunning) then begin
      if not (kwsEngineDestroy in FHistory) then begin
        if (cmdP^.Interval=0) or
           ( (cmdP^.Interval>0) and (Core.Timer.dtNow>cmdP^.dtLive)) then begin

          // Enforce Thread Count
          if cmdP^.CompletedP=nil then
            New(cmdP^.CompletedP);
          Init(cmdP^.CompletedP^,GetThreadCount,False);
          Remove(cmdP^.KeywordsP^,[kwroReferences]);
          cmdP^.dtStart:=Core.Timer.dtNow;
          InterlockedExchange(FCommandP,cmdP);

          FCmdQueue.Items[iLcv]:=nil;
          Include(FHistory,kwsEngineStart);
          // Safe to startup system
          FStatus:=ssRunning;
        end;
      end else begin
        // Remove command. Engine is shutting down
        Finalize(cmdP^);
        Dispose(cmdP);
        FCmdQueue.Items[iLcv]:=nil;
      end;
    end; // else save for later
    // Careful here.  This is where we could see a memory leak
    // Finalize(cmdP^);
    // Dispose(cmdP);
    // FCmdQueue.Items[iLcv]:=nil;
  end;

  procedure ProcessStop;
  begin
    if (kwsEngineSetThreads in FHistory) and (kwsEngineStart in FHistory) then begin
      // Safe to stop system
      FStatus:=ssStop;
      Include(FHistory,kwsEngineStop);
    end;
    Finalize(cmdP^);
    Dispose(cmdP);
    FCmdQueue.Items[iLcv]:=nil;
  end;

  procedure ProcessDestroy;
  begin
    FStatus:=ssDestroy;
    Include(FHistory,kwsEngineDestroy);
    Finalize(cmdP^);
    Dispose(cmdP);
    FCmdQueue.Items[iLcv]:=nil;
  end;

begin
  EnterCriticalSection(FLock);
  Try
    For iLcv:=0 to FCmdQueue.Count-1 do begin
      try
        cmdP:=FCmdQueue.Items[iLcv];
        if cmdP<>nil then begin
          //kwsEngineStart,kwsEngineStop,kwsEngineSetThreads,kwsEngineDestroy
          case cmdP^.Key of
            kwsEngineSetThreads : ProcessSetCount;
            kwsEngineStart      : ProcessStart;
            kwsEngineStop       : ProcessStop;
            kwsEngineDestroy    : ProcessDestroy;
          end;
        end;
      except
        cmdP:=nil;
      end;
    end;
    FCmdQueue.Pack;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

constructor TKWScanner.Create(aIndex:LongInt; aOwner:TKWSEngine);
begin
  FreeOnTerminate:=True;
  FOwner:=aOwner;
  FIndex:=aIndex;
  FOwner.FTerminated[FIndex]:=false;
  Inherited Create(false);
end;

destructor  TKWScanner.Destroy;
begin
  Try
  if (FIndex<>-1) then FOwner.FTerminated[FIndex]:=true; // could improve with CAS
  if (FIndex<>-1) then FOwner.FThreads[FIndex]:=nil; // could improve with CAS

  Except
    FIndex:=-1;
  end;
  Inherited Destroy;
end;

procedure TKWScanner.Execute;
begin
  Priority:=tpNormal;
  Try
  While (Terminated=false) and (FOwner.FStatus<>ssDestroy) do begin
    If (FOwner.FStatus=ssRunning) and (FOwner.FCommandP<>nil) and (FOwner.FCommandP^.CompletedP^[FIndex]=false) then
      ProcessFile;
    if (FOwner<>nil) then
      RTLeventWaitFor(FOwner.FSleepP,WORKER_TIME_SLICE_YIELD_MS[FOwner.Status=ssRunning]);
  end;
  Except
    FOwner.FStatus:=ssDestroy;
  end;
end;

procedure TKWScanner.ProcessFile;
var
  iIndex                         : LongInt;
  iLength                        : LongInt;
  iChunkStart                    : LongInt;
  sKeyword                       : Core.Strings.VarString;
  sChunk                         : Core.Strings.VarString;

  procedure PushNextKeyword;
  var
    iEnd                         : LongInt;
    iStart                       : LongInt;
    iChunkLen                    : LongInt;
    iKWNStart                    : LongInt;
    iCount                       : LongInt;
  begin
    iStart:=iIndex;
    iEnd:=Core.Strings.PosEx('}',FData,iStart,FDataLen);
    if (iEnd>0) then begin
      // Primative/Valid keyword structure possibly found
      // {$i Name} or {$i name }
      // it may be {$i thought I was something this or that but not
      // really a real keyword }
      iKWNStart:=StrUtils.PosEx(#32,FData,iStart);
      sKeyword:=System.Copy(FData,iKWNStart,iEnd-iKWNStart);
      sKeyword:=Trim(sKeyword);
      // Look up referenced Keyword
      FKeywordP:=Find(sKeyword,FKeywordsP^);
      If FKeywordP<>nil then begin
        InterlockedIncrement(FKeywordP^.References);
        iChunkLen:=iStart-iChunkStart;
        sChunk:=System.Copy(FData,iChunkStart,iChunkLen);
        Add(sChunk,FFileP^.Write^.Count,FFileP^.Write^.PartsP^);
        Add(FKeywordP,iStart,iEnd-iStart,FFileP^.Write^.Count,FFileP^.Write^.ScanP^);
        InterlockedIncrement(FFileP^.Write^.Count);
        iChunkStart:=iEnd+1;
        iIndex:=Core.Strings.PosEx('{$i ',FData, iChunkStart,FDataLen);
        if iIndex=0 then begin
          // Done... Add rest to Additional Chunk
          iChunkLen:=(iLength-iChunkStart)+1;
          if iChunkLen>0 then begin
            sChunk:=System.Copy(FData,iChunkStart,iChunkLen);
            Add(nil,iChunkStart,iChunkLen,FFileP^.Write^.Count,FFileP^.Write^.ScanP^);
            Add(sChunk,FFileP^.Write^.Count,FFileP^.Write^.PartsP^);
            InterlockedIncrement(FFileP^.Write^.Count);
          end;
          iIndex:=iLength;
        end;
      end else begin
        iChunkLen:=(iEnd-iChunkStart)+1;
        sChunk:=System.Copy(FData,iChunkStart,iChunkLen);
        Add(nil,iChunkStart,iChunkLen,FFileP^.Write^.Count,FFileP^.Write^.ScanP^);
        Add(sChunk,FFileP^.Write^.Count,FFileP^.Write^.PartsP^);
        InterlockedIncrement(FFileP^.Write^.Count);
        iChunkStart:=iEnd+1;
        iIndex:=Core.Strings.PosEx('{$i ',FData, iChunkStart,FDataLen);
        if iIndex=0 then begin
          // Done... Add rest to Additional Chunk
          iChunkLen:=(iLength-iChunkStart)+1;
          if iChunkLen>0 then begin
            sChunk:=System.Copy(FData,iChunkStart,iChunkLen);
            Add(nil,iChunkStart,iChunkLen,FFileP^.Write^.Count,FFileP^.Write^.ScanP^);
            Add(sChunk,FFileP^.Write^.Count,FFileP^.Write^.PartsP^);
            InterlockedIncrement(FFileP^.Write^.Count);
          end;
          iIndex:=iLength;
        end;
      end;
    end else begin
      // We may need to add rest of data to Manfest b/c 1 invalid keyword d/n make
      // for a bad parse entirely.  So all that last data should be pushed to the end
      // of the list and an nil pointer for the manifest too.
      iChunkLen:=(iLength-iChunkStart)+1;
      if iChunkLen>0 then begin
        sChunk:=System.Copy(FData,iChunkStart,iChunkLen);
        Add(nil,iChunkStart,iChunkLen,FFileP^.Write^.Count,FFileP^.Write^.ScanP^);
        Add(sChunk,FFileP^.Write^.Count,FFileP^.Write^.PartsP^);
        InterlockedIncrement(FFileP^.Write^.Count);
      end;
      iIndex:=iLength;  // Process exit;
    end;
  end;

begin
  if FOwner.FOnGetNextItem(FOwner.FCommandP,FFileP,FData,FDataLen) then begin
    iChunkStart:=1;
    FKeywordsP:=FOwner.FCommandP^.KeywordsP;
    iIndex:=Pos('{$i ',FData); iLength:=Length(FData);
    While (iIndex>0) and (iIndex<iLength) and (FOwner.FStatus<>ssDestroy) do
      PushNextKeyword;
    Swap(FFileP^);
    Empty(FFileP^.Write^);
  end else
    FOwner.FCommandP^.CompletedP^[FIndex]:=True;
end;

end.

{
 snit dbmDNS.pas
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit Storage.DNS;


interface

uses
  Classes,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,
  Core.Database.Timer,

  Storage,
  Storage.Main,
  Core.Timer,

  SysUtils;


Const

  TI_DNS_REFRESH                 = 60*2;

  dnskRegular                    = 0;
  dnskBlackList                  = 1;
  dnskWhiteList                  = 2;

Type
  Items=class
  type
    Item=record
      ID                         : QWord;
      Verified                   : Boolean;
      Kind                       : Byte;
      IP                         : QWord;
    end;
    PItem=^Item;
    List=Array of PItem;
    PList=^List;
    Lcv=Array[0..2] of Integer;
    Manifest=Array[0..2] of Items.List;
    DB=class
    type
      FillRequest=record
        Filled                   : boolean;
        Kind                     : byte;
        ListP                    : Items.PList;
      end;
      PFillRequest=^FillRequest;
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        Kind                     : Core.Database.Types.Integer = 2;
        IP                       : Core.Database.Types.Integer = 3;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        Kind                     : Core.Database.Types.VarString = 'ITMKND';
        IP                       : Core.Database.Types.VarString = 'ITMIP';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System';
        Name                     : 'DNS';
        Value                    : 'scs_dns';
        Hint                     : 'Storage of dns host entries';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..3] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.IP; KeyP: @Keys.IP;  DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;)
      );
      class function  Fill(Task:Core.Database.Types.TTask; Var Request:Items.DB.FillRequest):Boolean; overload;
      class function  Fill(Task:Core.Database.Types.TTask; Var Entries:Items.List; Kind:Byte):Boolean; overload;
      class function  Add(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
      class function  Delete(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
      class function  Write(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
      class function  Read(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;

    end;
    class procedure Init(Var Entries:Items.Lcv); overload;
    class procedure Init(Var Entry:Items.Item); overload;
    class procedure Init(Var Entries:Items.List); overload;
    class procedure Init(Var Entries:Items.Manifest); overload;

    class procedure Empty(Var Entry:Items.Item); overload;
    class procedure Empty(Var Entries:Items.List); overload;
    class procedure Empty(Var Entries:Items.Lcv); overload;

    class procedure Done(Var Entry:Items.Item); overload;
    class procedure Done(Var Entries:Items.List); overload;
    class procedure Done(Var Entries:Items.Lcv); overload;
    class procedure Done(Var Entries:Items.Manifest); overload;

    class procedure Remove(Var Entries:Items.List; Index:LongInt); overload;
    class procedure Remove(Var Entries:Items.List; var Entry:Items.PItem); overload;
    class procedure Invalidate(var Entries:Items.List); overload;
    class procedure Purge(Var Entries:Items.List); overload;

    class procedure Copy(Var Source,Destination:Items.Item); overload;

    class function  IndexOf(Var Entries:Items.List; ID:QWord): LongInt; overload;
    class function  IndexOf(Var Entries:Items.List; Entry:Items.PItem): LongInt; overload;
  end;


  TDNSSystem=class
  private
    FLcv                         : Items.Lcv;
    FTask                        : Core.Database.Types.TTask;
    FTI_Refresh                  : Core.Timer.Item;
    FRequest                     : Items.DB.FillRequest;
    FLock                        : TRTLCriticalSection;
  private
    procedure On_Timer_Refresh(ItemP:Core.Timer.PItem);
  public
    Manifest                     : Items.Manifest;
  public
    Constructor Create(aHeader:Core.Database.Types.THeader); reIntroduce;
    Destructor  Destroy; override;
  public
    procedure Load(Task:Core.Database.Types.TTask);
    function  GetNextHost(Kind:Byte):QWord;
  end;

var
  Native                         : TDNSSystem;

procedure StartupDNS();

implementation
uses db,DateUtils;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure StartupDNS();
begin
  if Native=nil then
    Native:=TDNSSystem.Create(Storage.Main.Header);
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  With Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, Core.Database.Monitor.Notify.None);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


procedure CB_Items_Fill_Request(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iItemIndex:LongInt;
  rP:Items.DB.PFillRequest;
  itmP:Items.PItem;
  iItemID:QWord;
begin
  rP:=DataP;
  rP^.Filled:=true;

  iItemID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iItemIndex:=Items.IndexOf(rP^.ListP^,iItemID);
  if iItemIndex=-1 then begin
    iItemIndex:=Length(rP^.ListP^);
    SetLength(rP^.ListP^,iItemIndex+1);
    New(itmP);
    Items.Init(itmP^);
    itmP^.ID:=iItemID;;
    itmP^.Kind:=rP^.Kind;
    rP^.ListP^[iItemIndex]:=itmP;
  end else
    itmP:=rP^.ListP^[iItemIndex];
  itmP^.Verified:=true;
  itmP^.IP:=Fields.FieldByName(Items.DB.Keys.IP).AsLargeInt;
end;

procedure CB_Items_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iItemIndex:LongInt;
  ListP:Items.PList;
  itmP:Items.PItem;
  iItemID:QWord;
begin
  ListP:=DataP;
  iItemID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iItemIndex:=Items.IndexOf(ListP^,iItemID);
  if iItemIndex=-1 then begin
    iItemIndex:=Length(ListP^);
    SetLength(ListP^,iItemIndex+1);
    New(itmP);
    Items.Init(itmP^);
    itmP^.ID:=iItemID;;
    ListP^[iItemIndex]:=itmP;
  end else
    itmP:=ListP^[iItemIndex];
  itmP^.Verified:=true;
  itmP^.Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsInteger;
  itmP^.IP:=Fields.FieldByName(Items.DB.Keys.IP).AsLargeInt;
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entries:Items.List; Kind:Byte):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poNone,oEqual,Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.IP,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Items_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Request:Items.DB.FillRequest):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poNone,oEqual,Request.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.IP,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Items_Fill_Request,@Request);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Int64));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Data Properties
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.IP,poNone,oNone,Entry.IP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,Entry.Kind,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Write(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
var
  iCount,iIndex                  : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.IP,poNone,oNone,Entry.IP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Kind,poNone,oNone,Entry.Kind,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure CB_Items_Read(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  with Items.PItem(DataP)^ do begin
    IP:=Fields.FieldByName(Items.DB.Keys.IP).AsLargeInt;
    Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsInteger;
    Verified:=True;
  end;
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; Var Entry:Items.Item):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.IP,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Items_Read,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.Empty(Var Entries:Items.List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
    Entries[iLcv]:=nil;
  end;
  SetLength(Entries,0);
end;

class procedure Items.Init(Var Entry:Items.Item);
begin
  Entry.ID:=0;
  Entry.Verified:=false;
  Entry.IP:=0;
  Entry.Kind:=0;
end;

class procedure Items.Empty(Var Entry:Items.Item);
begin
  Entry.ID:=0;
  Entry.Verified:=false;
  Entry.IP:=0;
  Entry.Kind:=0;
end;

class procedure Items.Empty(Var Entries:Items.Lcv);
begin
  Entries[dnskRegular]:=0;
  Entries[dnskBlackList]:=0;
  Entries[dnskWhiteList]:=0;
end;

class procedure Items.Done(Var Entry:Items.Item);
begin
  Finalize(Entry);
end;

class procedure Items.Done(Var Entries:Items.List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]<>nil then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
      Entries[iLcv]:=nil;
    end;
  end;
  Finalize(Entries);
end;

class procedure Items.Done(Var Entries:Items.Lcv);
begin
  Finalize(Entries[dnskRegular]);
  Finalize(Entries[dnskBlackList]);
  Finalize(Entries[dnskWhiteList]);
  Finalize(Entries);
end;

class procedure Items.Done(Var Entries:Items.Manifest);
begin
  Done(Entries[dnskRegular]);
  Done(Entries[dnskBlackList]);
  Done(Entries[dnskWhiteList]);
  Finalize(Entries);
end;

class procedure Items.Init(Var Entries:Items.Lcv);
begin
  Entries[dnskRegular]:=0;
  Entries[dnskBlackList]:=0;
  Entries[dnskWhiteList]:=0;
end;

class procedure Items.Init(Var Entries:Items.List);
begin
  Empty(Entries);
end;

class procedure Items.Init(Var Entries:Items.Manifest);
begin
  Init(Entries[dnskRegular]);
  Init(Entries[dnskBlackList]);
  Init(Entries[dnskWhiteList]);
end;

class function  Items.IndexOf(Var Entries:Items.List; ID:QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]<>nil) and (Entries[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function  Items.IndexOf(Var Entries:Items.List; Entry:Items.PItem): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]=Entry) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class procedure Items.Copy(Var Source,Destination:Items.Item);
begin
  Destination.ID:=Source.ID;
  Destination.IP:=Source.IP;
  Destination.Kind:=Source.Kind;
end;

class procedure Items.Invalidate(var Entries:Items.List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    if Entries[iLcv]<>nil then
      Entries[iLcv]^.Verified:=false;
end;

class procedure Items.Purge(var Entries:Items.List);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iLen:LongInt;
begin
  iLen:=Length(Entries);
  iLcv:=0;
  while iLcv<iLen do begin
    if (Entries[iLcv]=nil) then begin
      for jLcv:=iLcv to iLen-2 do
        Entries[jLcv]:=Entries[jLcv+1];
      SetLength(Entries,iLen-1);
    end else if (Entries[iLcv]^.Verified=false) then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
      for jLcv:=iLcv to iLen-2 do
        Entries[jLcv]:=Entries[jLcv+1];
      SetLength(Entries,iLen-1);
    end else
      Inc(iLcv);
    iLen:=Length(Entries);
  end;
end;

class procedure Items.Remove(Var Entries:Items.List; Index:LongInt);
var
  iLen:LongInt;
  iLcv:LongInt;
  itmP:Items.PItem;
begin
  iLen:=Length(Entries);
  if (Index<iLen) then
    itmP:=Entries[Index]
  else
    itmP:=nil;
  if itmP<>nil then begin
    Try
      for iLcv:=Index to iLen-2 do
        Entries[iLcv]:=Entries[iLcv+1];
      SetLength(Entries,iLen-1);
    finally
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
end;

class procedure Items.Remove(Var Entries:Items.List; var Entry:Items.PItem);
var
  iLen:LongInt;
  iLcv:LongInt;
  iDX:LongInt;
begin
  iDX:=IndexOf(Entries,Entry);
  if iDX<>-1 then begin
    Try
      iLen:=System.Length(Entries);
      for iLcv:=iDX to iLen-2 do
        Entries[iLcv]:=Entries[iLcv+1];
      SetLength(Entries,iLen-1);
    finally
      Done(Entry^);
      Dispose(Entry);
      Entry:=nil;
    end;
  end;
end;

constructor TDNSSystem.Create(aHeader:Core.Database.Types.THeader);
begin
  InitCriticalSection(FLock);
  Items.Init(FLcv);
  Items.Init(Manifest);

  FTask:=Core.Database.Types.TTask.Create(aHeader,'Storage.DNS Background');

  FTI_Refresh.Event:=@On_Timer_Refresh;
  FTI_Refresh.Location:='dbmDNS.TDNSSystem';
  FTI_Refresh.Expires:=IncSecond(Core.Timer.dtNow,TI_DNS_REFRESH);

  Core.Database.Timer.Background.RegisterEvent(FTI_Refresh,LoadNoUpdate);
  Inherited Create;
end;

destructor TDNSSystem.Destroy;
begin
  Core.Database.Timer.Background.UnloadEvent(FTI_Refresh,UnloadNoExecute);
  DoneCriticalsection(FLock);
  Items.Done(FLcv);
  Items.Done(Manifest);
  Inherited Destroy;
end;

procedure TDNSSystem.On_Timer_Refresh(ItemP:Core.Timer.PItem);
begin
  Load(Core.Database.Timer.Background.Task);
  ItemP^.Expires:=IncSecond(Core.Timer.dtNow,TI_DNS_REFRESH);
end;

function  TDNSSystem.GetNextHost(Kind:Byte):QWord;
var
  iCount:LongInt;
begin
  EnterCriticalSection(FLock);
  Try
    Result:=0;
    iCount:=System.Length(Manifest[Kind]);
    if FLcv[Kind]>=iCount then
      FLcv[Kind]:=0;
    if FLcv[Kind]<iCount then
      Result:=Manifest[Kind][FLcv[Kind]]^.IP;
    inc(FLcv[Kind]);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TDNSSystem.Load(Task:Core.Database.Types.TTask);
begin
  EnterCriticalSection(FLock);
  Try
    FRequest.Kind:=dnskRegular;
    FRequest.ListP:=@Manifest[dnskRegular];
    FRequest.Filled:=False;
    Items.Invalidate(Manifest[dnskRegular]);
    Items.DB.Fill(Task,FRequest);
    if FRequest.Filled then
      Items.Purge(Manifest[dnskRegular]);
  finally
    LeaveCriticalSection(FLock);
  end;
  EnterCriticalSection(FLock);
  Try
    FRequest.Kind:=dnskBlackList;
    FRequest.ListP:=@Manifest[dnskBlackList];
    FRequest.Filled:=False;
    Items.Invalidate(Manifest[dnskBlackList]);
    Items.DB.Fill(Task,FRequest);
    if FRequest.Filled then
      Items.Purge(Manifest[dnskBlackList]);
  finally
    LeaveCriticalSection(FLock);
  end;
  EnterCriticalSection(FLock);
  Try
    FRequest.Kind:=dnskWhiteList;
    FRequest.ListP:=@Manifest[dnskWhiteList];
    FRequest.Filled:=False;
    Items.Invalidate(Manifest[dnskWhiteList]);
    Items.DB.Fill(Task,FRequest);
    if FRequest.Filled then
      Items.Purge(Manifest[dnskWhiteList]);
  finally
    LeaveCriticalSection(FLock);
  end;
end;


initialization
  RegisterDBM;
end.


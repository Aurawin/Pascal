unit Core.Database.Monitor;

interface

uses
  Classes, SysUtils,IniFiles,
  Core.Arrays.Types,
  Core.Database,
  Core.Database.SQL,
  Core.Database.Types,
  Core.Database.Monitor.Types;
Type
  TMonitor=class
  private
    FList          : TList;
  private
    procedure Clear();
    function  GetCount:LongInt;
  public
    Constructor Create; virtual;
    Destructor  Destroy; override;
  public
    procedure Add(ItemP:Core.Database.Monitor.Types.PItem);
    procedure Remove (Var ItemP:Core.Database.Monitor.Types.PItem);
    procedure Notify(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:System.QWord; Flag:System.Cardinal);
    procedure CheckTables(Task:Core.Database.Types.TTask);
    function  Build(out List:Core.Arrays.Types.Pointers):LongInt;
    function  Retrieve(Index:LongInt):Core.Database.Monitor.Types.PItem;
    procedure SetIniValues(IniFile:TIniFile);
    procedure GetIniValues(IniFile:TIniFile);
  public
    property Count:LongInt read GetCount;
  end;

  procedure Init(Var Item:Core.Database.Monitor.Types.Item; Var Table:Core.Database.Types.Table; OnDestroy:Core.Database.Monitor.Types.Event; OnNotify:Core.Database.Monitor.Types.Callback); overload;
  procedure Empty(Var Item:Core.Database.Monitor.Types.Item); overload;
  procedure Done(Var Item:Core.Database.Monitor.Types.Item); overload;
  procedure Add(ItemP:Core.Database.Monitor.Types.PItem); overload;
  procedure Remove(ItemP:Core.Database.Monitor.Types.PItem); overload;
  function  Build(out List:Core.Arrays.Types.Pointers):LongInt;
  function  Retrieve(Index:LongInt):Core.Database.Monitor.Types.PItem;

  procedure Cascade(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:System.QWord; Flag:System.Cardinal);
  procedure CheckTables(Task:Core.Database.Types.TTask);

  procedure SetIniValues(IniFile:TIniFile);
  procedure GetIniValues(IniFile:TIniFile);

implementation

var  Monitor:TMonitor;

procedure Cascade(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:System.QWord; Flag:System.Cardinal);
begin
  if Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Monitor.Notify(Task,TableP,ItemID,Flag);
end;

procedure CheckTables(Task:Core.Database.Types.TTask);
begin
  if Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Monitor.CheckTables(Task);
end;

function  Build(out List:Core.Arrays.Types.Pointers):LongInt;
begin
  If Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Result:=Monitor.Build(List);
end;

function  Retrieve(Index:LongInt):Core.Database.Monitor.Types.PItem;
begin
  if Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Result:=Monitor.Retrieve(Index);
end;

procedure Add(ItemP:Core.Database.Monitor.Types.PItem);
begin
  if Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Monitor.Add(ItemP);
end;

procedure Remove(ItemP:Core.Database.Monitor.Types.PItem);
begin
  if Assigned(Monitor) then
    Monitor.Remove(ItemP);
end;

procedure SetIniValues(IniFile:TIniFile);
begin
  if Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Monitor.SetIniValues(IniFile);
end;
procedure GetIniValues(IniFile:TIniFile);
begin
  if Not Assigned(Monitor) then
    Monitor:=TMonitor.Create();
  Monitor.GetIniValues(IniFile);
end;

procedure Init(Var Item:Core.Database.Monitor.Types.Item; Var Table:Core.Database.Types.Table; OnDestroy:Core.Database.Monitor.Types.Event; OnNotify:Core.Database.Monitor.Types.Callback);
begin
  Item.TableP:=@Table;
  Item.OnNotify:=OnNotify;
  Item.OnDestroy:=OnDestroy;
end;

procedure Empty(Var Item:Core.Database.Monitor.Types.Item);
begin
  Item.TableP:=nil;
  Item.OnNotify:=nil;
  Item.OnDestroy:=nil;
end;

procedure Done(Var Item:Core.Database.Monitor.Types.Item);
begin
  Finalize(Item);
end;

Constructor TMonitor.Create;
begin
  FList:=TList.Create;
  Inherited Create;
end;

Destructor TMonitor.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  Inherited Destroy;
end;

procedure TMonitor.Clear;
var
  iLcv   : LongInt;
  ItemP  : Core.Database.Monitor.Types.PItem;
begin
  for iLcv:=0 to FList.Count-1 do begin
    ItemP:=FList[iLcv];
    if ItemP<>nil then begin
      ItemP^.OnDestroy(ItemP);
      Done(ItemP^);
      Dispose(ItemP);
      FList.Items[iLcv]:=nil;
    end;
  end;
  FList.Pack;
end;

procedure TMonitor.SetIniValues(IniFile:TIniFile);
var
  iLcv                           : LongInt;
  ItemP                          : Core.Database.Monitor.Types.PItem;
begin
  For iLcv:=0 to FList.Count-1 do begin
    ItemP:=FList[iLcv];
    if ItemP<>nil then
      IniFile.WriteString(INI_DB_SECT_TABLES,Core.Database.IniGetSection(ItemP^.TableP^),ItemP^.TableP^.Name);
  end;
end;

procedure TMonitor.GetIniValues(IniFile:TIniFile);
var
  iLcv                           : LongInt;
  ItemP                          : Core.Database.Monitor.Types.PItem;
begin
  For iLcv:=0 to FList.Count-1 do begin
    ItemP:=FList[iLcv];
    if ItemP<>nil then
      ItemP^.TableP^.Name:=IniFile.ReadString(INI_DB_SECT_TABLES,IniGetSection(ItemP^.TableP^),ItemP^.TableP^.StartupP^.Value);
  end;
end;

procedure TMonitor.CheckTables(Task:Core.Database.Types.TTask);
var
  iLcv                           : LongInt;
  ItemP                          : Core.Database.Monitor.Types.PItem;
begin
  For iLcv:=0 to FList.Count-1 do begin
    ItemP:=FList[iLcv];
    if ItemP<>nil then
      Core.Database.CheckTable(Task,ItemP^.TableP);
  end;
end;

function  TMonitor.Build(out List:Core.Arrays.Types.Pointers):LongInt;
var
  iLcv:LongInt;
begin
  Result:=FList.Count;
  SetLength(List,Result);
  For iLcv:=0 to Result-1 do
    List[iLcv]:=FList.Items[iLcv];
end;

function  TMonitor.Retrieve(Index:LongInt):Core.Database.Monitor.Types.PItem;
begin
  Result:=nil;
  If (Index>-1) and (Index<FList.Count) then
    Result:=FList.items[Index];
end;

procedure TMonitor.Notify(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:System.QWord; Flag:System.Cardinal);
var
  iLcv   : LongInt;
  ItemP  : Core.Database.Monitor.Types.PItem;
begin
  For iLcv:=0 to FList.Count-1 do begin
    ItemP:=FList[iLcv];
    if (ItemP<>nil) and (ItemP^.OnNotify<>nil) then
      ItemP^.OnNotify(Task,ItemP^.TableP,ItemID,ItemP,Flag);
  end;
end;

procedure TMonitor.Add(ItemP:Core.Database.Monitor.Types.PItem);
begin
  FList.Add(ItemP);
end;

function  TMonitor.GetCount:LongInt;
begin
  Result:=FList.Count;
end;

procedure TMonitor.Remove(Var ItemP:Core.Database.Monitor.Types.PItem);
begin
  FList.Remove(ItemP);
  ItemP^.OnDestroy(ItemP);
  Done(ItemP^);
  Dispose(ItemP);
  ItemP:=nil;
end;

end.


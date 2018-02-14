{
 unit Storage.ContentTypes.pas

 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
}

unit Storage.ContentTypes;

interface

uses

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.Strings,
  Core.Timer,

  Storage,
  Storage.Types,

  App.Consts,

  RSR.HTTP,


  Classes,
  SysUtils;
Type
  Items=class
  Type
    DB = class
    type
      IDs = class
      const
        ID                         : Core.Database.Types.Integer = 0;
        InsertID                   : Core.Database.Types.Integer = 1;
        Kind                       : Core.Database.Types.Integer = 2;
        Ext                        : Core.Database.Types.Integer = 3;
      end;
      Keys=class
      const
        ID                         : Core.Strings.VarString = 'ITMID';
        InsertID                   : Core.Strings.VarString = 'ITMIID';
        Kind                       : Core.Strings.VarString = 'ITMKND';
        Ext                        : Core.Strings.VarString = 'ITMEXT';
      end;
    Const
      TableP                       : Core.Database.Types.PTable = nil;
      MonitorP                     : Core.Database.Monitor.Types.PItem = nil;
      Startup                      : Core.Database.Types.TableIni = (
        AutoCreate                 : True;
        AutoCommit                 : True;
        Group                      : 'System';
        Name                       : 'Content Types';
        Value                      : 'scs_cntypes';
        Hint                       : 'Registry of supported content types';
        PrimaryKeyP                : @Keys.ID;
      );
      Fields: array [0..3] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Ext; KeyP: @Keys.Ext; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
      class function  Fill(Task:Core.Database.Types.TTask; Var Entries:TContentTypes):Boolean;
      class function  Add(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
      class function  Delete(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
      class function  Write(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
      class function  Read(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
      class procedure Load(Task:Core.Database.Types.TTask; Var Entries:TContentTypes);
    end;
  end;
  TValidator=class
  private
    FData:Core.Timer.Item;
  private
    procedure onRefresh(ItemP:Core.Timer.PItem);
  public
    constructor Create;
    destructor  Destroy; override;
  end;

Var
  List                           : RSR.HTTP.TContentTypes;
  Validator                      : TValidator;

implementation

uses db,DateUtils;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  Core.Database.Done(Items.DB.TableP^);
  Items.DB.TableP:=nil;
  Items.DB.MonitorP:=nil;
end;

procedure RegisterDBM;
var
  iLcv                          : LongInt;
begin
  with Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, Core.Database.Monitor.Notify.None);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

procedure CheckValidator;
begin
  if Validator=nil then
    Validator:=TValidator.Create();
end;


procedure CB_ContentType_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iItemIndex:LongInt;
  ListP:PContentTypes;
  itmP:PContentType;
  sExt:String;
  iItemID:QWord;
  iLen:LongInt;
  iLoc:LongInt;
begin
  ListP:=DataP;
  sExt:=Fields.FieldByName(Items.DB.Keys.Ext).AsString;
  iItemID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iItemIndex:=IndexOf(ListP^,iItemID);
  if iItemIndex=-1 then
    iItemIndex:=IndexOf(ListP^,sExt);
  if iItemIndex=-1 then begin
    New(itmP);
    Init(itmP^);
    itmP^.ID:=iItemID;
    iItemIndex:=Length(ListP^);
    SetLength(ListP^,iItemIndex+1);
    ListP^[iItemIndex]:=itmP;
  end else
    itmP:=ListP^[iItemIndex];

  itmP^.Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsString;
  iLen:=System.Length(itmP^.Kind);
  iLoc:=System.Pos('/',itmP^.Kind);
  itmP^.SubType:=System.Copy(itmP^.Kind,iLoc+1,iLen-iLoc);

  itmP^.Ext:=sExt;
  itmP^.Verified:=True;
end;

class Function Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entries:TContentTypes):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Ext,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Ext,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_ContentType_Fill,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Data Properties
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Ext,poNone,oNone,Entry.Ext,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,Entry.Kind,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    if Entry.ID<>0 then Entry.Verified:=true;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Write(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
var
  iCount,iIndex                  : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Ext,poNone,oNone,Entry.Ext,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Kind,poNone,oNone,Entry.Kind,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure CB_ContentType_Read(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iLoc:LongInt;
  iLen:LongInt;
  cntP:PContentType;
begin
  cntP:=DataP;
  cntP^.Ext:=Fields.FieldByName(Items.DB.Keys.Ext).AsString;
  cntP^.Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsString;
  iLoc:=System.Pos('/',cntP^.Kind);
  iLen:=System.Length(cntP^.Kind);
  cntP^.SubType:=System.Copy(cntP^.Kind,iLoc+1,iLen-iLoc);
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; Var Entry:TContentType):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Ext,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_ContentType_Read,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.DB.Load(Task:Core.Database.Types.TTask; Var Entries:TContentTypes);
var
  iLcv:LongInt;
begin
  Invalidate(Entries);
  Items.DB.Fill(Task,Entries);
  Merge(RSR.HTTP.ctDefaultExtensions,Entries);
  for iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]^.ID=0 then
      Add(Task,Entries[iLcv]^);
  end;
  CheckValidator();
  Validator.FData.Expires:=IncSecond(Core.Timer.dtNow,CONTENT_TYPE_REFRESH);
end;

constructor TValidator.Create;
begin
  FData.Event:=@onRefresh;
  FData.Location:='Storage.ContentTypes.TValidator.onRefresh';
  FData.Expires:=0;
  FData.Mode:=temNormal;
  FData.Priority:=tpIdle;
  Core.Database.Timer.Background.RegisterEvent(FData,Core.Timer.LoadNoUpdate);
  inherited Create;
end;

destructor TValidator.Destroy;
begin
  Core.Database.Timer.Background.UnloadEvent(FData,Core.Timer.UnloadNoExecute);
  Inherited Destroy;
end;

procedure TValidator.onRefresh(ItemP:Core.Timer.PItem);
begin
  Items.DB.Load(Core.Database.Timer.Background.Task,List);
  ItemP^.Expires:=IncSecond(Core.Timer.dtNow,CONTENT_TYPE_REFRESH);
end;

initialization
  RegisterDBM;
end.


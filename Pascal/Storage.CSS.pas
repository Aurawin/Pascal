unit Storage.CSS;

{
  Cascading Style Sheets based on One Table

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  Core.Database,
  Core.Database.SQL,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,

  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,

  Storage.Domains,

  SysUtils;

Type
  Items=class
  Type
    Item=Record
      ID                             : QWord;
      DomainID                       : QWord;
      Name                           : Core.Strings.VarString;
      Kind                           : Core.Strings.VarString;
      Properties                     : Core.Arrays.Types.KeyStrings;
    end;
    PItem=^Item;
    Kind=(ctColor,ctFontFamily,ctURL,ctStringList,ctNumber, ctString, ctPixels,ctPercentage,ctDimension,ctRect);
    Rect=record
      Top                              : Core.Strings.VarString;
      Left                             : Core.Strings.VarString;
      Bottom                           : Core.Strings.VarString;
      Right                            : Core.Strings.VarString
    end;
    PRect=^Rect;

    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        Name                     : Core.Database.Types.Integer = 3;
        Kind                     : Core.Database.Types.Integer = 4;
        Data                     : Core.Database.Types.Integer = 5;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        Name                     : Core.Database.Types.VarString = 'ITMNME';
        Kind                     : Core.Database.Types.VarString = 'ITMKND';
        Data                     : Core.Database.Types.VarString = 'ITMVAL';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains';
        Name                     : 'CSS Objects';
        Value                    : 'scs_css';
        Hint                     : 'Storage for domain based css objects';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..5] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType:dftString; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType:dftString; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType:dftByteBuffer; AutoCreate:True; Verified:False; Precision:1024*1024*1; Flags: cfNone; )
      );
      class Function  Names(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var List:Core.Arrays.Types.VarString):Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item; Refactor:TStream):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item):Boolean;
      class Function  Write(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item; Refactor:TStream):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item):Boolean; overload;
      class Function  Read(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Name:Core.Strings.VarString; Var ID:QWord):Boolean;  overload;
      class Function  Exists(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Name:Core.Strings.VarString):Boolean;
    end;

    class procedure Empty(Var Entry:Item); overload;
    class procedure Done(Var Entry:Item); overload;
    class Function  ToString(Var CSS:Item; Refactor:TStream):Core.Strings.VarString;
    class Procedure FromString(Var CSS:Item; Var Data:Core.Strings.VarString); overload;
    class Procedure FromString(Var CSS:Item; Name,Data:Core.Strings.VarString); overload;
  end;






implementation
uses db;


function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted();
  end;
end;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class Function  Items.ToString(Var CSS:Item; Refactor:TStream):Core.Strings.VarString;
begin
  Result:=Concat(CSS.Name,'{',Core.Arrays.KeyString.toString(CSS.Properties,Refactor,':',';'#13#10),'}');
end;

class Procedure Items.FromString(Var CSS:Item; Var Data:Core.Strings.VarString);
var
  iELoc,iLoc:LongInt;
  sSplit:Core.Strings.VarString;
begin
  SetLength(CSS.Name,0);
  SetLength(CSS.Properties,0);
  iLoc:=Pos('{',Data); iELoc:=Pos('}',Data);
  if (iLoc>0) and (iELoc>0) then begin
    CSS.Name:=SysUtils.Trim(System.Copy(Data,1,iLoc-1));
    sSplit:=System.Copy(Data,iLoc+1,iELoc-iLoc-1);
    Core.Arrays.KeyString.fromString(CSS.Properties,sSplit,':',';');
  end;
end;

class Procedure Items.FromString(Var CSS:Item; Name,Data:Core.Strings.VarString);
begin
  CSS.Name:=Name;
  Core.Arrays.KeyString.fromString(CSS.Properties,Data,':',';');
end;

procedure CB_CSS_List_Names(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.VarString.Add(Core.Arrays.Types.PVarString(DataP),Fields.FieldByName(Items.DB.Keys.Name).AsString,[]);
end;

class Function  Items.DB.Names(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var List:Core.Arrays.Types.VarString):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainP^.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_CSS_List_Names,@List);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  ITems.DB.Add(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item; Refactor:TStream):Boolean;
var
  iCount                         : LongInt;
  InsertID,iReset                : QWord;
  Commands                       : Core.Database.Types.Commands;
  sData                          : Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; InsertID:=Random(High(Integer));
    sData:=Core.Arrays.KeyString.toString(CSS.Properties,Refactor,':',';'#13#10);
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      // Set Primary ID
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,InsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,InsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,CSS.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

      Core.Database.AddCommand(iCount,TableP,useForValues,IDs.DomainID,poNone,oNone,DomainP^.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Name,poNone,oNone,CSS.Name,Commands);
      Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Kind,poNone,oNone,CSS.Kind,Commands);
      Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Data,poNone,oNone,sData,Commands);
      Result:=Core.Database.SQL.Insert(Task,@Commands);
    Finally
      Finalize(sData);
    End;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,CSS.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Write(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item; Refactor:TStream):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sData                          : Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0;
    sData:=Core.Arrays.KeyString.toString(CSS.Properties,Refactor,':',';'#13#10);
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,CSS.Name,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Kind,poNone,oNone,CSS.Kind,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Data,poNone,oNone,sData,Commands);
      Result:=Core.Database.SQL.Update(Task,@Commands);
    Finally
      Finalize(sData);
    End;
  Finally
    Core.Database.Done(Commands);
  End;
end;



procedure CB_CSS_Get_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Name:Core.Strings.VarString; Var ID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainP^.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_CSS_Get_ID,@ID);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure CB_CSS_Get_CSS(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  With Items.PItem(DataP)^ do begin
    ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsString;
    Core.Arrays.KeyString.fromString(@Properties,Fields.FieldByName(Items.DB.Keys.Data).AsString,':',';'#13#10);
  end;
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Var CSS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainP^.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,CSS.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Kind,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Data,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_CSS_Get_CSS,@CSS);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Exists(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain; Name:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Count                          : QWord;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainP^.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Name,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.Empty(Var Entry:Item);
begin
  Core.Arrays.KeyString.Empty(Entry.Properties);
  SetLength(Entry.Name,0);
  SetLength(Entry.Kind,0);
end;


class procedure Items.Done(Var Entry:Item);
begin
  Core.Arrays.KeyString.Done(Entry.Properties);
  Finalize(Entry.Name);
  Finalize(Entry.Kind);
  Finalize(Entry);
end;


initialization
  RegisterDBM;
end.


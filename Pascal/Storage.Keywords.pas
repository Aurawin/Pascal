unit Storage.Keywords;


{
  unit: Storage.Keywords.pas

  Based on 2 Tables

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}


interface

uses
  Classes,

  Core.Keywords,
  Core.Strings,
  Core.Timer,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  SysUtils;

Type
  Items=class
  type
    DB=class
    Type
      Request=record
        ctFilled                 : cardinal;
        DomainID                 : QWord;
        cbDefault                : TKeywordMethod;
        KeywordsP                : PKeywords;
      end;
      PRequest=^Request;
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        Modified                 : Core.Database.Types.Integer = 3;
        Name                     : Core.Database.Types.Integer = 4;
        Value                    : Core.Database.Types.Integer = 5;
        Order                    : Core.Database.Types.Integer = 6;
      end;
      Keys=Class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        Modified                 : Core.Database.Types.VarString = 'ITMOD';
        Name                     : Core.Database.Types.VarString = 'ITMNME';
        Value                    : Core.Database.Types.VarString = 'ITMVAL';
        Order                    : Core.Database.Types.VarString = 'ITMORD';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Services/Keywords';
        Name                     : 'Keywords';
        Value                    : 'scs_kywrds';
        Hint                     : 'Storage for domain based keywords';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNotNull;  ),
        (IDP: @IDs.Value; KeyP: @Keys.Value; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*1024; Flags: cfNone;  ),
        (IDP: @IDs.Order; KeyP: @Keys.Order; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class Function  Fill(Task:Core.Database.Types.TTask; Var Data:Request):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
      class Function  Exists(Task:Core.Database.Types.TTask; DomainID:QWord; Keyword:Core.Strings.VarString):Boolean;
      class Function  Create(Task:Core.Database.Types.TTask; DomainID:QWord; Var Keyword:TKeyword):Boolean;
      class Function  Save(Task:Core.Database.Types.TTask; Var Keyword:TKeyword):Boolean;
      class Function  Refresh(Task:Core.Database.Types.TTask; DomainID:QWord; Var List:TKeywords; Var Changes:Boolean):Boolean;
    end;
  end;
  Files=class
  type
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        DomainID                 : Core.Database.Types.Integer = 1;
        FileID                   : Core.Database.Types.Integer = 2;
        Offset                   : Core.Database.Types.Integer = 3;
        Length                   : Core.Database.Types.Integer = 4;
        Modified                 : Core.Database.Types.Integer = 5;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'IWKID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        FileID                   : Core.Database.Types.VarString = 'IFID';
        Offset                   : Core.Database.Types.VarString = 'IOST';
        Length                   : Core.Database.Types.VarString = 'ILEN';
        Modified                 : Core.Database.Types.VarString = 'IMFD';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Services/Keywords';
        Name                     : 'File Manifest';
        Value                    : 'scs_kwsmfst';
        Hint                     : 'Storage for domain based keyword scan manifests';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..5] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.FileID; KeyP: @Keys.FileID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Offset; KeyP: @Keys.Offset; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Length; KeyP: @Keys.Length; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
    end;
  end;

implementation
uses
  db;

procedure cbDestroyTableItems(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyTableFiles(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Files.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;


function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        With Items.DB do begin
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        end;
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Files.DB.MonitorP then begin
      Try
        With Files.DB do begin
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        end;
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
  end;
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
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTableItems, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  With Files.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTableFiles, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


procedure CB_KeywordFill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  RequestP                       : Items.DB.PRequest;
  ItemP                          : PKeyword;
begin
  RequestP:=DataP;
  ItemP:=Add(
    Fields.FieldByName(Items.DB.Keys.Name).AsString,
    Fields.FieldByName(Items.DB.Keys.Value).AsString,
    RequestP^.KeywordsP^,
    KW_REFRESH_ON,
    RequestP^.cbDefault,
    NO_CALLBACK
  );
  ItemP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  ItemP^.Order:=Fields.FieldByName(Items.DB.Keys.Order).AsInteger;
  ItemP^.Modified:=Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
  InterlockedIncrement(RequestP^.ctFilled);
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Data:Request):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Data.ctFilled:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,Data.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Order,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Order,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Value,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_KeywordFill,@Data);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.Exists(Task:Core.Database.Types.TTask; DomainID:QWord; Keyword:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Count:Qword;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Keyword,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count)and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Create(Task:Core.Database.Types.TTask; DomainID:QWord; Var Keyword:TKeyword):Boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Keyword.ID:=0; iInsertID:=Random(High(Integer)); Keyword.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Keyword.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,Keyword.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Name,poNone,oNone,Keyword.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Value,poNone,oNone,Keyword.Value,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Save(Task:Core.Database.Types.TTask; Var Keyword:TKeyword):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; Keyword.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Keyword.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Order,poNone,oNone,Keyword.Order,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Keyword.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Name,poNone,oNone,Keyword.Name,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Value,poNone,oNone,Keyword.Value,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_KeywordUpdate_Keyword(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  KeywordP                       : PKeyword;
  iIndex                         : LongInt;
  ID                             : QWord;
begin
  KeywordP:=DataP;
  KeywordP^.Order:=Fields.FieldByName(Items.DB.Keys.Order).AsInteger;
  KeywordP^.Modified:=Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
  KeywordP^.Name:=Fields.FieldByName(Items.DB.Keys.Name).AsString;
  KeywordP^.Value:=Fields.FieldByName(Items.DB.Keys.Value).AsString;
end;

procedure CB_KeywordUpdate_Keywords(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP                          : PKeywords;
  ItemP                          : PKeyword;
  ID                             : QWord;
  dtModified                     : TDateTime;
begin
  ListP:=DataP;
  ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  dtModified:=Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
  IndexOf(ID,ListP^,ItemP);
  if (ItemP=nil) then
    ItemP:=Add(ID,ListP^,KW_REFRESH_ON,NO_CALLBACK,NO_CALLBACK);
  ItemP^.Verified:=true;
  ItemP^.Refresh:=(ItemP^.Modified<>dtModified);
  ItemP^.Modified:=dtModified;
end;


class Function  Items.DB.Refresh(Task:Core.Database.Types.TTask; DomainID:QWord; Var List:TKeywords; Var Changes:Boolean):Boolean;
var
  iLcv                           : LongInt;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; Changes:=false;
  Try
    iCount:=0;
    Verify(List,false); // set all to false
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);
    Core.Database.SQL.Select(Task,@Commands,@CB_KeywordUpdate_Keywords,@List);
    For ilcv:=0 to High(List) do begin
      If (List[iLcv]<>nil) then begin
        if List[iLcv]^.UseRefresh then begin
          if (List[iLcv]^.Refresh) and (List[iLcv]^.ID<>0) then begin
            Changes:=True;
            iCount:=0;
            Core.Database.AddCommand(iCount,TableP,@Commands);
            Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,List[iLcv]^.ID,Commands);
            Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Order,poNone,oNone,Commands);
            Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);
            Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
            Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Value,poNone,oNone,Commands);
            Core.Database.SQL.Select(Task,@Commands,@CB_KeywordUpdate_Keyword,List[iLcv]);
          end else if not List[iLcv]^.Verified then
            List[iLcv]^.Deleted:=true;
        end;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDBM;
end.


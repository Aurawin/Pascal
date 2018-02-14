unit Storage.Apps;

{
  unit Storage.Apps.pas

  Application Database Module

  DBMS facilities to handle scalable client-server applications over Core Objects

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}


interface

uses
  Classes,


  Core.Strings,
  Core.Timer,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,


  Storage,
  Storage.UserAccounts,
  Storage.Vendors,
  Storage.Domains,
  Storage.CoreObjects,

  RSR.Core,

  SysUtils;


Type
  Items=Class
  type
    PApp=^App;
    App=record
      ID                         : QWord;
      VersionMajor               : LongInt;
      VersionMinor               : LongInt;
      VersionMicro               : LongInt;
      VersionBuild               : LongInt;
      Visibility                 : LongInt;
      Created                    : Double;
      Modified                   : Double;
      Auth                       : Core.Strings.VarString;
      TitleLong                  : Core.Strings.VarString;
      TitleShort                 : Core.Strings.VarString;
      Copyright                  : Core.Strings.VarString;
      Data                       : Core.Strings.VarString;
      Defines                    : Core.Strings.VarString;
      Declares                   : Core.Strings.VarString;
      Dependencies               : Core.Strings.VarString;
      Manifest                   : Core.Strings.VarString;
      Description                : Core.Strings.VarString;
      Patents                    : Core.Strings.VarString;
      LanguageTable              : Core.Strings.VarString;
      Vendor                     : Storage.Vendors.Items.PItem;
    end;

    Visibility=class
    const
      Hidden                     = 0;
      Everyone                   = 1;
      Owner                      = 2;
    end;
    Defaults=class
    const
      UserID                     : QWord = 0;
      DomainID                   : QWord = 0;
      Visibility                 = Visibility.Owner;
    end;
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        VendorID                 : Core.Database.Types.Integer = 4;
        Created                  : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        VersionMajor             : Core.Database.Types.Integer = 7;
        VersionMinor             : Core.Database.Types.Integer = 8;
        VersionMicro             : Core.Database.Types.Integer = 9;
        VersionBuild             : Core.Database.Types.Integer = 10;
        Visibility               : Core.Database.Types.Integer = 11;
        Auth                     : Core.Database.Types.Integer = 12;
        Data                     : Core.Database.Types.Integer = 13;
        Defines                  : Core.Database.Types.Integer = 14;
        Declares                 : Core.Database.Types.Integer = 15;
        Dependencies             : Core.Database.Types.Integer = 16;
        Manifest                 : Core.Database.Types.Integer = 17;
        LanguageTable            : Core.Database.Types.Integer = 18;
        TitleLong                : Core.Database.Types.Integer = 19;
        TitleShort               : Core.Database.Types.Integer = 20;
        Description              : Core.Database.Types.Integer = 21;
        Copyright                : Core.Database.Types.Integer = 22;
        Patents                  : Core.Database.Types.Integer = 23;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        VendorID                 : Core.Database.Types.VarString = 'VNDRID';
        Created                  : Core.Database.Types.VarString = 'DTACRT';
        Modified                 : Core.Database.Types.VarString = 'DTAMOD';
        VersionMajor             : Core.Database.Types.VarString = 'VSRMAJ';
        VersionMinor             : Core.Database.Types.VarString = 'VSNMIN';
        VersionMicro             : Core.Database.Types.VarString = 'VSMCRO';
        VersionBuild             : Core.Database.Types.VarString = 'VSBLD';
        Visibility               : Core.Database.Types.VarString = 'ITMVIS';
        Auth                     : Core.Database.Types.VarString = 'AUTH';
        Data                     : Core.Database.Types.VarString = 'ITMDAT';
        Defines                  : Core.Database.Types.VarString = 'ITDEFS';
        Declares                 : Core.Database.Types.VarString = 'ITDECS';
        Dependencies             : Core.Database.Types.VarString = 'ITDEPS';
        Manifest                 : Core.Database.Types.VarString = 'ITMFST';
        LanguageTable            : Core.Database.Types.VarString = 'LNGTBL';
        TitleLong                : Core.Database.Types.VarString = 'TITLNG';
        TitleShort               : Core.Database.Types.VarString = 'TITSHT';
        Description              : Core.Database.Types.VarString = 'ITMDES';
        Copyright                : Core.Database.Types.VarString = 'CPYRGT';
        Patents                  : Core.Database.Types.VarString = 'PATNTS';
      end;
      Precision=class
      const
        Data                     = 1024*1024*10;
        Defines                  = 1024*50;
        Declares                 = 1024*100;
        Dependencies             = 1024*1;
        Manifest                 = 1024*500;
        LanguageTable            = 1024*1024;
        TitleLong                = 255;
        TitleShort               = 120;
        Description              = 1024;
        Copyright                = 500;
        Patents                  = 1024;
        Auth                     = 32;
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System';
        Name                     : 'Applications';
        Value                    : 'scs_apps';
        Hint                     : 'Application information and configuration storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..23] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.VendorID; KeyP: @Keys.VendorID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType:dftDouble; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType:dftDouble; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.VersionMajor; KeyP: @Keys.VersionMajor; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.VersionMinor; KeyP: @Keys.VersionMinor; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.VersionMicro; KeyP: @Keys.VersionMicro; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.VersionBuild; KeyP: @Keys.VersionBuild; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Visibility; KeyP: @Keys.Visibility; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Auth; KeyP: @Keys.Auth; DataType:dftString; AutoCreate:True; Verified:False; Precision:Precision.Auth; Flags: cfNone; ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:Precision.Data; Flags: cfNone; ),
        (IDP: @IDs.Defines; KeyP: @Keys.Defines; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:Precision.Defines; Flags: cfNone; ),
        (IDP: @IDs.Declares; KeyP: @Keys.Declares; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:Precision.Declares; Flags: cfNone; ),
        (IDP: @IDs.Dependencies; KeyP: @Keys.Dependencies; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:Precision.Dependencies; Flags: cfNone; ),
        (IDP: @IDs.Manifest; KeyP: @Keys.Manifest; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:Precision.Manifest; Flags: cfNone; ),
        (IDP: @IDs.LanguageTable; KeyP: @Keys.LanguageTable; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:Precision.LanguageTable; Flags: cfNone; ),
        (IDP: @IDs.TitleLong; KeyP: @Keys.TitleLong; DataType:dftSmallString; AutoCreate:True; Verified:False; Precision:Precision.TitleLong; Flags: cfNone; ),
        (IDP: @IDs.TitleShort; KeyP: @Keys.TitleShort; DataType:dftSmallString; AutoCreate:True; Verified:False; Precision:Precision.TitleShort; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType:dftString; AutoCreate:True; Verified:False; Precision:Precision.Description; Flags: cfNone; ),
        (IDP: @IDs.Copyright; KeyP: @Keys.Copyright; DataType:dftString; AutoCreate:True; Verified:False; Precision:Precision.Copyright; Flags: cfNone; ),
        (IDP: @IDs.Patents; KeyP: @Keys.Patents; DataType:dftString; AutoCreate:True; Verified:False; Precision:Precision.Patents; Flags: cfNone; )
      );
      class function Install(Task:Core.Database.Types.TTask; idDomain,idUser:QWord; Var Entry:App):boolean;
      class function Remove(Task:Core.Database.Types.TTask; Var Entry:App):boolean;

      class function setDefinitions(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
      class function setDeclarations(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
      class function setDependencies(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
      class function setLanguageTable(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
      class function setManifest(Task:Core.Database.Types.TTask; Var Entry:App):boolean;

      class function getData(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
      class function setData(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
    end;
    class procedure Init(var Entry:App);
    class procedure Empty(var Entry:App);
    class procedure Done(var Entry:App);
  end;

implementation
uses db;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;


function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Empty(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
    if TableP=nil then begin
      New(TableP);
      Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Init(MonitorP^,TableP^,@cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Init(var Entry:App);
begin
  Entry.ID:=0;
  Entry.Created:=0;
  Entry.Modified:=0;
  Entry.Visibility:=Items.Defaults.Visibility;
  SetLength(Entry.Auth,0);
  SetLength(Entry.TitleLong,0);
  SetLength(Entry.TitleShort,0);
  SetLength(Entry.Copyright,0);
  SetLength(Entry.Data,0);
  SetLength(Entry.Defines,0);
  SetLength(Entry.Declares,0);
  SetLength(Entry.Dependencies,0);
  SetLength(Entry.Manifest,0);
  SetLength(Entry.Description,0);
  SetLength(Entry.Patents,0);
  SetLength(Entry.LanguageTable,0);
  Entry.VersionMajor:=0;
  Entry.VersionMinor:=0;
  Entry.VersionMicro:=0;
  Entry.VersionBuild:=0;
  New(Entry.Vendor);
  Storage.Vendors.Items.Init(Entry.Vendor^);
end;

class procedure Items.Empty(var Entry:App);
begin
  Entry.ID:=0;
  Entry.Created:=0;
  Entry.Modified:=0;

  SetLength(Entry.Auth,0);
  SetLength(Entry.TitleLong,0);
  SetLength(Entry.TitleShort,0);
  SetLength(Entry.Copyright,0);
  SetLength(Entry.Data,0);
  SetLength(Entry.Defines,0);
  SetLength(Entry.Declares,0);
  SetLength(Entry.Dependencies,0);
  SetLength(Entry.Manifest,0);
  SetLength(Entry.Description,0);
  SetLength(Entry.Patents,0);
  SetLength(Entry.LanguageTable,0);
  Entry.VersionMajor:=0;
  Entry.VersionMinor:=0;
  Entry.VersionMicro:=0;
  Entry.VersionBuild:=0;
  If Entry.Vendor<>nil then
    Storage.Vendors.Items.Empty(Entry.Vendor^);
end;

class procedure Items.Done(var Entry:App);
begin
  Finalize(Entry.Auth);
  Finalize(Entry.TitleLong);
  Finalize(Entry.TitleShort);
  Finalize(Entry.Copyright);
  Finalize(Entry.Data);
  Finalize(Entry.Defines);
  Finalize(Entry.Declares);
  Finalize(Entry.Dependencies);
  Finalize(Entry.Manifest);
  Finalize(Entry.Description);
  Finalize(Entry.Patents);
  Finalize(Entry.LanguageTable);
  If Entry.Vendor<>nil then begin
    Storage.Vendors.Items.Done(Entry.Vendor^);
    Dispose(Entry.Vendor);
    Entry.Vendor:=nil;
  end;
  Finalize(Entry);
end;

class function Items.DB.Install(Task:Core.Database.Types.TTask; idDomain,idUser:QWord; Var Entry:App):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  iReset                         : QWord;
  iInsertID                      : QWord;
begin
  Result:=False;
  iCount:=0;
  iReset:=0;
  iInsertID:=Random(High(Integer));
  Entry.Created:=Core.Timer.dtUT;
  Entry.Modified:=Core.Timer.dtUT;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,idDomain,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,idUser,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.VendorID,poNone,oNone,Entry.Vendor^.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,Entry.Created,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Visibility,poNone,oNone,Entry.Visibility,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.VersionMajor,poNone,oNone,Entry.VersionMajor,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.VersionMinor,poNone,oNone,Entry.VersionMinor,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.VersionMicro,poNone,oNone,Entry.VersionMicro,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.VersionBuild,poNone,oNone,Entry.VersionBuild,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Auth,poNone,oNone,Entry.Auth,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Data,poNone,oNone,Entry.Data,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Defines,poNone,oNone,Entry.Defines,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Declares,poNone,oNone,Entry.Declares,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Dependencies,poNone,oNone,Entry.Dependencies,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Manifest,poNone,oNone,Entry.Manifest,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LanguageTable,poNone,oNone,Entry.LanguageTable,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.TitleLong,poNone,oNone,Entry.TitleLong,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.TitleShort,poNone,oNone,Entry.TitleShort,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Copyright,poNone,oNone,Entry.Copyright,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Patents,poNone,oNone,Entry.Patents,Commands);


    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setDefinitions(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Entry.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Defines,poNone,oNone,Entry.Defines,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.setDeclarations(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Entry.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Declares,poNone,oNone,Entry.Declares,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.setDependencies(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Entry.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Dependencies,poNone,oNone,Entry.Dependencies,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.setLanguageTable(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Entry.Modified:=Core.Timer.dtUT;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.LanguageTable,poNone,oNone,Entry.LanguageTable,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.setManifest(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Entry.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Manifest,poNone,oNone,Entry.Manifest,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure cbgetData(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Items.PApp(DataP)^.Data:=Fields.FieldByName(Items.DB.Keys.Data).AsString;
end;

class function Items.DB.getData(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Data,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cbgetData,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.setData(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Entry.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForValues,IDs.Data,poNone,oNone,Entry.Data,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.Remove(Task:Core.Database.Types.TTask; Var Entry:App):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


initialization
  RegisterDB;

end.


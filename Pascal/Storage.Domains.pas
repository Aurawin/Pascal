unit Storage.Domains;
{
  Based on One Table

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,


  Core.Strings,
  Core.Streams,
  Core.Timer,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,


  Core.XML,

  Storage.Types,
  Storage.Main,
  Storage.Calendaring,
  Storage.RSS,


  XMLRead,
  DOM,
  SysUtils;

Const
  FMT_CONFIRM_DOMAIN_DELETE_CAPTION = 'Delete domain %s?';
  FMT_CONFIRM_DOMAIN_DELETE_TITLE = 'Are you sure you want to delete domain %s?';
  FMT_CONFIRM_DOMAIN_DELETE_MESSAGE = 'You have selected domain "%s" to be deleted.  '^M^M'All information for this domain will be removed from the database.  Database assets like users, emails, files, and content will be deleted during this operation.'+^M^M+'Press Yes to delete this domain or No to cancel this operation.';

Type
  Items=class
  Type
    PDomain=^TDomain;
    TDomain=Record
      ID                            : Core.Database.Types.LargeWord;
      CertID                        : Core.Database.Types.LargeWord;

      Name                          : Core.Database.Types.VarString;
      Root                          : Core.Database.Types.VarString;
      FriendlyName                  : Core.Database.Types.VarString;

      DefaultOptionCatchAll         : Core.Database.Types.Bool;
      DefaultOptionQuota            : Core.Database.Types.LargeWord;
      DefaultOptionFiltering        : Core.Database.Types.Bool;

      FeedsP                        : PRSSChannels;
      EventsP                       : Storage.Calendaring.Items.PEvents;
    end;
    TDomains=Array of TDomain;
    PDomains=^TDomains;
    DB=class
    Type
      XML=class
      type
        Stanza=class
        const
          Domain                   : Core.Strings.VarString = 'dm';
        end;
        Field=class
        const
          ID                       : Core.Strings.VarString = 'id';
          CertID                   : Core.Strings.VarString = 'cid';
          Name                     : Core.Strings.VarString = 'nme';
          Root                     : Core.Strings.VarString = 'rot';
          FriendlyName             : Core.Strings.VarString = 'fne';
          DefaultOptionCatchAll    : Core.Strings.VarString = 'dca';
          DefaultOptionQuota       : Core.Strings.VarString = 'dqa';
          DefaultOptionFiltering   : Core.Strings.VarString = 'dfl';
        end;
      end;
      IDS=class
      const
        ID                         : LongInt = 0;
        InsertID                   : LongInt = 1;
        CertID                     : LongInt = 2;
        Name                       : LongInt = 3;
        FriendlyName               : LongInt = 4;
        Root                       : LongInt = 5;
        DefaultOptionQuota         : LongInt = 6;
        DefaultOptionCatchAll      : LongInt = 7;
        DefaultOptionFiltering     : LongInt = 8;
      end;
      Keys=class
      const
        ID                         : Core.Strings.VarString = 'ITMID';
        InsertID                   : Core.Strings.VarString = 'ITMIID';
        CertID                     : Core.Strings.VarString = 'ITMCID';
        Name                       : Core.Strings.VarString = 'DMNM';
        FriendlyName               : Core.Strings.VarString = 'DMFRND';
        Root                       : Core.Strings.VarString = 'DRU';
        DefaultOptionQuota         : Core.Strings.VarString = 'DEFQTA';
        DefaultOptionCatchAll      : Core.Strings.VarString = 'DCALL';
        DefaultOptionFiltering     : Core.Strings.VarString = 'DEFSPM';
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'System';
        Name                 : 'Domains';
        Value                : 'scs_dmns';
        Hint                 : 'Domain and configuration storage';
        PrimaryKeyP          : @Keys.ID;
      );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP:@Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP:@Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.CertID; KeyP:@Keys.CertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Name; KeyP:@Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNotNull; ),
        (IDP: @IDs.FriendlyName; KeyP:@Keys.FriendlyName; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Root; KeyP:@Keys.Root; DataType: dftString; AutoCreate: True; Verified: False; Precision: 35; Flags: cfNotNull; ),
        (IDP: @IDs.DefaultOptionQuota; KeyP:@Keys.DefaultOptionQuota; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.DefaultOptionCatchAll; KeyP:@Keys.DefaultOptionCatchAll; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DefaultOptionFiltering; KeyP:@Keys.DefaultOptionFiltering; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );

      class Function  Fill(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; ID:QWord; Var Domain:TDomain):Boolean; overload;
      class Function  List(Task:Core.Database.Types.TTask; Var Entries:Core.Arrays.Types.VarString):Boolean; overload;
      class Function  List(Task:Core.Database.Types.TTask; Var Entries:TDomains):Boolean; overload;
      class Function  List(Task:Core.Database.Types.TTask; Var Entries:Core.Arrays.Types.LargeWord):Boolean; overload;
      class Function  IsInternal(Task:Core.Database.Types.TTask;  Domain:Core.Strings.VarString):Boolean;
      class Function  Create(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
      class Function  Update(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
      class Function  GetID(Task:Core.Database.Types.TTask; Domain:Core.Strings.VarString; Var ID:QWord):Boolean;
      class Function  GetName(Task:Core.Database.Types.TTask; Var ID:QWord; var Domain:Core.Strings.VarString):Boolean;
    end;
    class Function  IndexOf(Var Domains:TDomains; Const ID:QWord): LongInt; overload;
    class Function  IndexOf(Var Domains:TDomains; HostName:Core.Strings.VarString): LongInt; overload;

    class procedure Init(Var Item:TDomain); overload;

    class procedure Empty(Var Item:TDomain); overload;
    class procedure Empty(Var Item:TDomains); overload;

    class procedure Copy(Var Source,Destination:TDomain); overload;

    class procedure Done(Var Item:TDomain); overload;
    class procedure Done(Var Item:TDomains); overload;

    class function  fromXML(xDoc:TXMLDocument; var Item:TDomain):boolean;
    class function  toXML(var Item:TDomain; Output:TMemoryStream; Header:boolean):boolean;
  end;
implementation
uses
  Storage,
  Storage.AuraDisks,
  Storage.UserAccounts,
  db,
  sqldb;

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
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.ID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        SetLength(Commands,0);
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
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

class procedure Items.Init(Var Item:TDomain);
begin
  With Item do begin
    ID:=0;
    CertID:=0;
    SetLength(Name,0);
    SetLength(Root,0);
    SetLength(FriendlyName,0);
    DefaultOptionFiltering:=true;
    DefaultOptionCatchAll:=true;
    DefaultOptionQuota:=Storage.UserAccounts.Items.Throttle.Level1.Limit;
    DefaultOptionFiltering:=true;
    FeedsP:=nil;
    EventsP:=nil;
  end;
end;

class procedure Items.Empty(Var Item:TDomain);
begin
  Item.CertID:=0;
  If Item.FeedsP<>Nil then begin
    Storage.RSS.Channels.Done(Item.FeedsP^);
    Dispose(Item.FeedsP);
    Item.FeedsP:=nil;
  end;
  If Item.EventsP<>Nil then begin
    Storage.Calendaring.Items.Done(Item.EventsP^);
    Dispose(Item.EventsP);
    Item.EventsP:=nil;
  end;

  SetLength(Item.Name,0);
  SetLength(Item.Root,0);
  SetLength(Item.FriendlyName,0);

  Item.DefaultOptionCatchAll:=true;
  Item.DefaultOptionFiltering:=true;
  Item.DefaultOptionQuota:=Storage.UserAccounts.Items.Throttle.Level1.Limit;
end;

class procedure Items.Done(Var Item:TDomain);
begin
  If Item.FeedsP<>Nil then begin
    Storage.RSS.Channels.Done(Item.FeedsP^);
    Dispose(Item.FeedsP);
    Item.FeedsP:=nil;
  end;
  If Item.EventsP<>Nil then begin
    Storage.Calendaring.Items.Done(Item.EventsP^);
    Dispose(Item.EventsP);
    Item.EventsP:=nil;
  end;
  Core.Strings.Done(Item.Name);
  Core.Strings.Done(Item.Root);
  Core.Strings.Done(Item.FriendlyName);
  Finalize(Item);
end;

class procedure Items.Empty(Var Item:TDomains);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;

class procedure Items.Done(Var Item:TDomains);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure CB_Domain_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
begin
  Core.Arrays.VarString.Add(Core.Arrays.Types.PVarString(DataP),Fields.FieldByName(Items.DB.Keys.Name).AsString);
end;

class Function Items.DB.List(Task:Core.Database.Types.TTask; Var Entries:Core.Arrays.Types.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  iCount:=0; Result:=False; SetLength(Entries,0);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_List,@Entries);
  Finally
    SetLength(Commands,0);
  End;
end;

procedure CB_Domain_List_IDs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=nil);
var
  ListP:Core.Arrays.Types.PLargeWord;
  iID:QWord;
begin
  ListP:=DataP;
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  Core.Arrays.LargeWord.Add(iID,ListP^);
end;

procedure CB_Domain_List_Domains(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
const
  bAssignDomain:boolean=true;
var
  DomainsP:Items.PDomains;
  DomainP:Items.PDomain;
  iID:QWord;
  iIndex:LongInt;
begin
  DomainsP:=DataP;
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iIndex:=Items.IndexOf(DomainsP^,iID);
  if iIndex=-1 then begin
    iIndex:=Length(DomainsP^);
    SetLength(DomainsP^,iIndex+1);
    Items.Init(DomainsP^[iIndex]);
  end;
  DomainP:=@DomainsP^[iIndex];
  {$i Storage.Domains.FillDomain.inc}
end;

class Function  Items.DB.List(Task:Core.Database.Types.TTask; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_List_IDs,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.List(Task:Core.Database.Types.TTask; Var Entries:TDomains):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    {$i Storage.Domains.FillDomain.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_List_Domains,@Entries);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_Domain_GetID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
begin
  Core.Database.Types.PLargeWord(DataP)^:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

procedure CB_Domain_GetName(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
begin
  Core.Database.Types.PVarString(DataP)^:=Fields.FieldByName(Items.DB.Keys.Name).AsString;
end;

class Function  Items.DB.GetID(Task:Core.Database.Types.TTask; Domain:Core.Strings.VarString; Var ID:QWord):Boolean;
var
  Count:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; ID:=0; Count:=0;
  Try
    Core.Database.AddCommand(Count,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForCriteria,Items.DB.IDs.Name,poNone,oEqual,Domain,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_GetID,@ID);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.GetName(Task:Core.Database.Types.TTask; Var ID:QWord; var Domain:Core.Strings.VarString):Boolean;
var
  Count:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; SetLength(Domain,0); Count:=0;
  Try
    Core.Database.AddCommand(Count,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForCriteria,Items.DB.IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_GetName,@Domain);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Domain_Fill_By_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
begin
  Items.PDomain(DataP)^.Name:=Fields.FieldByName(Items.DB.Keys.Name).AsString;
end;

class Function Items.DB.Fill(Task:Core.Database.Types.TTask; ID:QWord; Var Domain:TDomain):Boolean;
var
  Count:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; Count:=0; Domain.ID:=ID;
  Try
    Core.Database.AddCommand(Count,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForCriteria,Items.DB.IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.Name,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_Fill_By_ID,@Domain);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Domain_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer=Nil);
const
  bAssignDomain:boolean=true;
var
  DomainP:Storage.Domains.Items.PDomain;
begin
  DomainP:=DataP;
  {$i Storage.Domains.FillDomain.inc}
end;


class Function Items.DB.Fill(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
var
  Count:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  If Domain.FeedsP=Nil then begin
    New(PRSSChannels(Domain.FeedsP));
    Storage.RSS.Channels.Init(Domain.FeedsP^);
  end;
  If Domain.EventsP=Nil then begin
    New(Storage.Calendaring.Items.PEvents(Domain.EventsP));
    Storage.Calendaring.Items.Init(Domain.EventsP^);
  end;
  Result:=False; Count:=0;
  Try
    Core.Database.AddCommand(Count,Items.DB.TableP,@Commands);
    if (Domain.ID<>0) then
      Core.Database.AddCommand(Count,Items.DB.TableP,useForCriteria,Items.DB.IDs.ID,poNone,oEqual,Domain.ID,Commands)
    else
      Core.Database.AddCommand(Count,Items.DB.TableP,useForCriteria,Items.DB.IDs.Name,poNone,oEqual,Domain.Name,Commands);

    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.CertID,poNone,oNone,Commands);

    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.Root,poNone,oNone,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.FriendlyName,poNone,oNone,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.Name,poNone,oNone,Commands);

    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,Items.DB.IDs.DefaultOptionQuota,poNone,oNone,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,IDs.DefaultOptionCatchAll,poNone,oNone,Commands);
    Core.Database.AddCommand(Count,Items.DB.TableP,useForFields,IDs.DefaultOptionFiltering,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Domain_Fill,@Domain) and (Domain.ID>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.IsInternal(Task:Core.Database.Types.TTask; Domain:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Count:Int64;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.Name,poNone,oEqual,Domain,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count);
    Result:=(Result=true) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.Create(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  iReset                         : QWord;
  iInsertID                      : QWord;
  DiskID                         : QWord;
begin
  Result:=False;
  iCount:=0;
  iReset:=0;
  iInsertID:=Random(High(Integer));
  Try
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForPrimaryID,Items.DB.IDs.ID,poNone,oNone,Domain.ID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Domain Settings
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDs.Name,poNone,oNone,Domain.Name,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDs.FriendlyName,poNone,oNone,Domain.FriendlyName,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDs.Root,poNone,oNone,Domain.Root,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,IDs.DefaultOptionCatchAll,poNone,oNone,Domain.DefaultOptionCatchAll,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,IDs.DefaultOptionQuota,poNone,oNone,Domain.DefaultOptionQuota,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (Domain.ID<>0);
    if Result then
      Result:=Storage.AuraDisks.Router.Allocate(Task,Domain.ID,Use.Global,Kinds.Domain,DiskID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.Update(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.ID,poNone,oEqual,Domain.ID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDs.Name,poNone,oNone,Domain.Name,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDs.FriendlyName,poNone,oNone,Domain.FriendlyName,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDs.Root,poNone,oNone,Domain.Root,Commands);

    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDs.CertID,poNone,oNone,Domain.CertID,Commands);

    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,IDs.DefaultOptionFiltering,poNone,oNone,Domain.DefaultOptionFiltering,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,IDs.DefaultOptionCatchAll,poNone,oNone,Domain.DefaultOptionCatchAll,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,IDs.DefaultOptionQuota,poNone,oNone,Domain.DefaultOptionQuota,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Domain:TDomain):Boolean;
begin
  Result:=True;
  Core.Database.Monitor.Cascade(Task,Items.DB.TableP, Domain.ID, Core.Database.Monitor.Notify.DOMAIN_DELETED);
end;

class procedure Items.Copy(Var Source,Destination:TDomain);
begin
  Destination.ID:=Source.ID;
  Destination.CertID:=Source.CertID;
  Destination.Name:=Source.Name;
  Destination.Root:=Source.Root;
  Destination.FriendlyName:=Source.FriendlyName;
  Destination.DefaultOptionQuota:=Source.DefaultOptionQuota;
  Destination.DefaultOptionCatchAll:=Source.DefaultOptionCatchAll;
  Destination.DefaultOptionFiltering:=Source.DefaultOptionFiltering;
  Destination.FeedsP:=Source.FeedsP;
  Destination.EventsP:=Source.EventsP;
end;

class Function  Items.IndexOf(Var Domains:TDomains; HostName:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1; iCount:=Length(Domains); iLcv:=0;
  While (iLcv<iCount) and (Result=-1) do begin
    if SameText(Domains[iLcv].Name,HostName) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class Function  Items.IndexOf(Var Domains:TDomains; Const ID:QWord): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1;
  iLcv:=0;
  iCount:=System.Length(Domains);
  While (iLcv<iCount) and (Result=-1) do begin
    if Domains[iLcv].ID=ID then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class function  Items.fromXML(xDoc:TXMLDocument; var Item:TDomain):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Empty(Item);
  xItem:=Core.XML.DB.getNode(xDoc,DB.XML.Stanza.Domain);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.ID:=toQword(xItem,DB.XML.Field.ID);
      Item.CertID:=toQword(xItem,DB.XML.Field.CertID);
      Item.Name:=toString(xItem,DB.XML.Field.Name);
      Item.Root:=toString(xItem,DB.XML.Field.Root);
      Item.Friendlyname:=toString(xItem,DB.XML.Field.FriendlyName);
      Item.Friendlyname:=toString(xItem,DB.XML.Field.FriendlyName);
      Item.DefaultOptionQuota:=toQWord(xItem,DB.XML.Field.DefaultOptionQuota);
      Item.DefaultOptionCatchAll:=toBoolean(xItem,DB.XML.Field.DefaultOptionCatchAll);
      Item.DefaultOptionFiltering:=toBoolean(xItem,DB.XML.Field.DefaultOptionFiltering);
      Result:=True;
    end;
  end;
end;

class function  Items.toXML(var Item:TDomain; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(DB.XML.Stanza.Domain,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(DB.XML.Field.ID,Item.ID),Output);
    Core.Streams.Write(Print(DB.XML.Field.CertID,Item.CertID),Output);
    Core.Streams.Write(Print(DB.XML.Field.Name,Item.Name,CDATA_ON),Output);
    Core.Streams.Write(Print(DB.XML.Field.Root,Item.Root,CDATA_ON),Output);
    Core.Streams.Write(Print(DB.XML.Field.FriendlyName,Item.FriendlyName,CDATA_ON),Output);
    Core.Streams.Write(Print(DB.XML.Field.DefaultOptionQuota,Item.DefaultOptionQuota),Output);
    Core.Streams.Write(Print(DB.XML.Field.DefaultOptionCatchAll,Item.DefaultOptionCatchAll),Output);
    Core.Streams.Write(Print(DB.XML.Field.DefaultOptionFiltering,Item.DefaultOptionFiltering),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(DB.XML.Stanza.Domain,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

initialization
  RegisterDB;
end.


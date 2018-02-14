unit Storage.Search;


{
  unit dbmSearch.pas

  Search Datagbase Module

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

interface

uses

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,


  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Storage.Main,
  Storage.Domains,
  Storage.UserAccounts,
  Storage.CoreObjects,
  Core.XML,
  Core.Arrays.Bytes,
  Core.Streams,
  Core.Arrays.KeyString,
  Core.Strings,
  Core.Timer,

  XMLRead,
  DOM,
  Classes,
  SysUtils;

type
  Provider=class
  Type
    TItem=record
      ID                       : QWord;
      Namespace1               : Core.Strings.VarString;
      Namespace2               : Core.Strings.VarString;
    end;
    PItem=^TItem;
    Defaults=class
    const
      NoNamespace              : Core.Strings.VarString = '';
    end;
    XML=class
    type
      Stanzas=class
      const
        provider               = 'provider';
        providers              = 'providers';
      end;
      Fields=class
      const
        ID                     = 'id';
        Namespace1             = 'ns1';
        Namespace2             = 'ns2';

        Created                = 'ctd';
        Modified               = 'mtd';

        Limit                  = 'lmt';
        Index                  = 'idx';
        Term                   = 'trm';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                     : Core.Database.Types.Integer = 0;
        InsertID               : Core.Database.Types.Integer = 1;
        Namespace1             : Core.Database.Types.Integer = 2;
        Namespace2             : Core.Database.Types.Integer = 3;
      end;
      Keys=class
      const
        ID                     : Core.Database.Types.VarString = 'ITID';
        InsertID               : Core.Database.Types.VarString = 'IIID';
        Namespace1             : Core.Database.Types.VarString = 'INSP';
        Namespace2             : Core.Database.Types.VarString = 'INSS';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit   : True;
        Group        : 'Search';
        Name         : 'Provider';
        Value        : 'scs_srch_prv';
        Hint         : 'Search Provider Store';
        PrimaryKeyP  : @Keys.ID;
      );
      Fields: array [0..3] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Namespace1; KeyP: @Keys.Namespace1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Namespace2; KeyP: @Keys.Namespace2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function  Add(Task:Core.Database.Types.TTask; var Namespace1,Namespace2:Core.Strings.VarString; out ID:QWord ):boolean;
      class function  Identify(Task:Core.Database.Types.TTask; Namespace1,Namespace2:Core.Strings.VarString):QWord; overload;
      class function  Identify(Task:Core.Database.Types.TTask; var Item:TItem):QWord; overload;
      class function  Read(Task:Core.Database.Types.TTask; ItemID:QWord; var Item:TItem):boolean;
    end;

    class function  fromXML(xNode:TDOMNode; var Item:TItem):boolean;
    class function  toXML(var Item:TItem; Output:TStream; Header:Boolean):boolean;

    class procedure Empty(var Item:TItem);
    class procedure Done(var Item:TItem);
    class procedure Init(var Item:TItem);
  end;
  Query  = class
  type
    TItem=record
      ID                       : QWord;
      NetworkID                : QWord;
      UserID                   : QWord;
      GroupID                  : QWord;
      SessionID                : QWord;
      TimeToLive               : Core.Database.Types.Integer;
      NameSpacePrimary         : Core.Strings.VarString;
      NameSpaceSecondary       : Core.Strings.VarString;
      Term                     : Core.Strings.VarString;
    end;
    PItem=^TItem;
    XML=class
    type
      Stanzas=class
      const
        Query                  = 'query';
      end;
      Fields=class
      const
        ID                     = 'id';
        DomainID               = 'did';
        NetworkID              = 'nid';
        GroupID                = 'gid';
        SessionID              = 'sid';
        UserID                 = 'uid';
        TimeToLive             = 'ttl';
        NamespacePrimary       = 'ns1';
        NamespaceSecondary     = 'ns2';
        Term                   = 't';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                     : Core.Database.Types.Integer = 0;
        InsertID               : Core.Database.Types.Integer = 1;
        DomainID               : Core.Database.Types.Integer = 2;
        NetworkID              : Core.Database.Types.Integer = 3;
        GroupID                : Core.Database.Types.Integer = 4;
        SessionID              : Core.Database.Types.Integer = 5;
        UserID                 : Core.Database.Types.Integer = 6;
        TimeToLive             : Core.Database.Types.Integer = 7;
        NamespacePrimary       : Core.Database.Types.Integer = 8;
        NamespaceSecondary     : Core.Database.Types.Integer = 9;
        Term                   : Core.Database.Types.Integer = 10;
      end;
      Keys=class
      const
        ID                     : Core.Database.Types.VarString = 'ITID';
        InsertID               : Core.Database.Types.VarString = 'IIID';
        DomainID               : Core.Database.Types.VarString = 'IDID';
        NetworkID              : Core.Database.Types.VarString = 'INID';
        GroupID                : Core.Database.Types.VarString = 'IGID';
        SessionID              : Core.Database.Types.VarString = 'ISID';
        UserID                 : Core.Database.Types.VarString = 'IUID';
        TimeToLive             : Core.Database.Types.VarString = 'ITTL';
        NamespacePrimary       : Core.Database.Types.VarString = 'INSP';
        NamespaceSecondary     : Core.Database.Types.VarString = 'INSS';
        Term                   : Core.Database.Types.VarString = 'ITRM';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit   : True;
        Group        : 'Search';
        Name         : 'Query';
        Value        : 'scs_srch_qry';
        Hint         : 'Search Query Store';
        PrimaryKeyP  : @Keys.ID;
      );
      Fields: array [0..10] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.SessionID; KeyP: @Keys.SessionID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.TimeToLive; KeyP: @Keys.TimeToLive; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NamespacePrimary; KeyP: @Keys.NamespacePrimary; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NamespaceSecondary; KeyP: @Keys.NamespaceSecondary; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Term; KeyP: @Keys.Term; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function  Identify(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):QWord; overload;
      class function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TItem):boolean;
      class function  Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TItem):boolean;
    end;

    class procedure Empty(var Item:TItem);
    class procedure Init(var Item:TItem);
    class procedure Done(var Item:TItem);


    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;
    class function  fromXML(xNode:TDOMNode; var Item:TItem):boolean; overload;
    class function  toXML(var Item:TItem; Output:TStream; Header:Boolean):boolean;
  end;
  Cache = class
  type
    TItem=record
      ID                       : Core.Database.Types.LargeWord;
      ProviderID               : Core.Database.Types.LargeWord;
      NetworkID                : Core.Database.Types.LargeWord;
      GroupID                  : Core.Database.Types.LargeWord;
      SessionID                : Core.Database.Types.LargeWord;
      UserID                   : Core.Database.Types.LargeWord; // assigned by core
      QueryID                  : Core.Database.Types.LargeWord;
      Limit                    : Core.Database.Types.Integer;
      Index                    : Core.Database.Types.Integer;  // streamed in from XML
      Created                  : Core.Database.Types.Double;
      Modified                 : Core.Database.Types.Double;
      Expires                  : Core.Database.Types.Double;
      Executed                 : Core.Database.Types.Double;
      ReadDisco                : Core.Database.Types.Bool; // Runtime
      WriteBack                : Core.Database.Types.Bool; // Runtime
      ResultsAsXML             : Core.Database.Types.VarString;
      Results                  : Core.Database.Types.LargeWordArray;
    end;
    PItem=^TItem;
    TItems=Array of PItem;
    PItems=^TItems;
    XML=class
    type
      Stanzas=class
      const
        search                 = 'search';
        searches               = 'searches';
      end;
      Fields=class
      const
        ID                     = 'id';
        ProviderID             = 'pid';
        NetworkID              = 'nid';
        GroupID                = 'gid';
        SessionID              = 'sid';
        UserID                 = 'uid';
        QueryID                = 'qid';

        Created                = 'ctd';
        Modified               = 'mtd';
        Expires                = 'exp';
        Executed               = 'exe';

        ReadDisco              = 'rdo';
        WriteBack              = 'wbk';

        Limit                  = 'lmt';
        Index                  = 'idx';
        TimeToLive             = 'ttl';
        ResultIDs              = 'rds';
        ResultsAsXML           = 'rax';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                     : Core.Database.Types.Integer = 0;
        InsertID               : Core.Database.Types.Integer = 1;
        DomainID               : Core.Database.Types.Integer = 2;
        ProviderID             : Core.Database.Types.Integer = 3;
        NetworkID              : Core.Database.Types.Integer = 4;
        GroupID                : Core.Database.Types.Integer = 5;
        SessionID              : Core.Database.Types.Integer = 6;
        UserID                 : Core.Database.Types.Integer = 7;
        QueryID                : Core.Database.Types.Integer = 8;
        Created                : Core.Database.Types.Integer = 9;
        Modified               : Core.Database.Types.Integer = 10;
        Expires                : Core.Database.Types.Integer = 11;
        Executed               : Core.Database.Types.Integer = 12;
        ResultIDs              : Core.Database.Types.Integer = 13;
        ResultsAsXML           : Core.Database.Types.Integer = 14;
      end;
      Keys=class
      const
        ID                     : Core.Database.Types.VarString = 'ITID';
        InsertID               : Core.Database.Types.VarString = 'IIID';
        DomainID               : Core.Database.Types.VarString = 'IDID';
        ProviderID             : Core.Database.Types.VarString = 'IPID';
        NetworkID              : Core.Database.Types.VarString = 'INID';
        GroupID                : Core.Database.Types.VarString = 'IGID';
        SessionID              : Core.Database.Types.VarString = 'ISND';
        UserID                 : Core.Database.Types.VarString = 'IUID';
        QueryID                : Core.Database.Types.VarString = 'IQID';
        Created                : Core.Database.Types.VarString = 'ICTD';
        Modified               : Core.Database.Types.VarString = 'IMTD';
        Expires                : Core.Database.Types.VarString = 'IEXP';
        Executed               : Core.Database.Types.VarString = 'IEXE';
        ResultIDs              : Core.Database.Types.VarString = 'IRES';
        ResultsAsXML           : Core.Database.Types.VarString = 'IRAX';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit   : True;
        Group        : 'Search';
        Name         : 'Cache';
        Value        : 'scs_srch_cache';
        Hint         : 'Search System Result Cache';
        PrimaryKeyP  : @Keys.ID;
      );
      Fields: array [0..14] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ProviderID; KeyP: @Keys.ProviderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.SessionID; KeyP: @Keys.SessionID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.QueryID; KeyP: @Keys.QueryID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),

        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Expires; KeyP: @Keys.Expires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Executed; KeyP: @Keys.Executed; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),

        (IDP: @IDs.ResultIDs; KeyP: @Keys.ResultIDs; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.ResultsAsXML; KeyP: @Keys.ResultsAsXML; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
      class function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; TTL:LongInt; var Item:TItem):boolean;
      class function  Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
      class function  Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TItem):boolean;
      class function  Write(Task:Core.Database.Types.TTask; DomainID:QWord; TTL:LongInt; var Item:TItem):boolean;

      class function  Find(Task:Core.Database.Types.TTask; DomainID,ProviderID,NetworkID,UserID,QueryID:QWord; var Item:TItem):boolean;

      class function  Identify(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; TTL:LongInt; var Item:TItem):QWord;
    end;



    class function  fromXML(xNode:TDOMNode; var Item:TItem):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TItems):boolean; overload;

    class function  toXML(var Item:TItems; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean; overload;

    class procedure Empty(Var Item:TItem); overload;
    class procedure Empty(Var Item:TItems); overload;

    class procedure Init(Var Item:TItem); overload;
    class procedure Init(Var Item:TItems); overload;

    class procedure Done(Var Item:TItem); overload;
    class procedure Done(Var Item:TItems); overload;
  end;

implementation
uses
  DB, sqldb, DateUtils;

procedure cbDestroyCache(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Cache.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyProvider(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Provider.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyQuery(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Query.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Cache.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Cache.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Cache.DB.TableP, useForCriteria, Cache.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Query.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Query.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Query.DB.TableP, useForCriteria, Query.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Cache.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Cache.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Cache.DB.TableP, useForCriteria, Cache.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Query.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Query.DB.TableP, @Commands);
        Core.Database.AddCommand(iCount, Query.DB.TableP, useForCriteria, Query.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;
begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Cache.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyCache, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Query.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyQuery, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Provider.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyProvider, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class function  Provider.fromXML(xNode:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.Namespace1:=toString(xNode,XML.Fields.NameSpace1);
      Item.Namespace2:=toString(xNode,XML.Fields.NameSpace2);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function Provider.toXML(var Item:TItem; Output:TStream; Header:Boolean):boolean;
begin
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(
      Concat(
        '<',XML.Stanzas.Provider,'>',
        Print(XML.Fields.ID,Item.ID),
        Print(XML.Fields.Namespace1,Item.Namespace1,CDATA_OFF),
        Print(XML.Fields.Namespace2,Item.Namespace2,CDATA_OFF),
        '</',XML.Stanzas.Provider,'>'
      ),
      Output
    );
  end;
  Result:=true;
end;

class function Cache.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; TTL:LongInt; var Item:TItem):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Item.Created:=Core.Timer.dtUT;
    Item.Modified:=Core.Timer.dtUT;
    Item.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,TTL);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ProviderID,poNone,oNone,Item.ProviderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.NetworkID,poNone,oNone,Item.NetworkID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.GroupID,poNone,oNone,Item.GroupID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.SessionID,poNone,oNone,Item.SessionID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,Item.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.QueryID,poNone,oNone,Item.QueryID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,Item.Created,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Expires,poNone,oNone,Item.Expires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Executed,poNone,oNone,Item.Executed,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ResultIDs,poNone,oNone,Item.Results,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ResultsAsXML,poNone,oNone,Item.ResultsAsXML,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Cache.DB.Write(Task:Core.Database.Types.TTask; DomainID:QWord; TTL:LongInt; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified:=Core.Timer.dtUT;
    Item.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,TTL);

    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForUpdates, IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForUpdates, IDs.Expires, poNone, oNone, Item.Expires, Commands);
    Core.Database.AddCommand(iCount, TableP, useForUpdates, IDs.Executed, poNone, oNone, Item.Executed, Commands);

    Core.Database.AddCommand(iCount, TableP, useForUpdates, IDs.ResultIDs, poNone, oNone, Item.Results, Commands);
    Core.Database.AddCommand(iCount, TableP, useForUpdates, IDs.ResultsAsXML, poNone, oNone, Item.ResultsAsXML, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadSearch(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Cache.PItem;
begin
  ItmP:=DataP;
  {$i Storage.Search.cbRead.Fields.inc}
  if ItmP^.ReadDisco then begin
    ItmP^.ResultsAsXML:=Fields.FieldByName(Cache.DB.Keys.ResultsAsXML).asString;
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Cache.DB.Keys.ResultIDs).AsString,ItmP^.Results,',');
  end;
end;

class function Cache.DB.Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Item.ID:=ItemID;

    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Search.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadSearch, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbFindResults(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Cache.PItem;
begin
  ItmP:=DataP;
  {$i Storage.Search.cbRead.Fields.inc}

  ItmP^.ResultsAsXML:=Fields.FieldByName(Cache.DB.Keys.ResultsAsXML).asString;
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Cache.DB.Keys.ResultIDs).AsString,ItmP^.Results,',');
end;

class function Cache.DB.Find(Task:Core.Database.Types.TTask; DomainID,ProviderID,NetworkID,UserID,QueryID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; Item.ID:=0;
    SetLength(Item.ResultsAsXML,0);
    Core.Arrays.LargeWord.Empty(Item.Results);
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ProviderID, poAnd, oEqual, ProviderID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.QueryID, poAnd, oEqual, QueryID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForOrderBy,  IDs.Modified,poNone,oAscending,Commands);

    {$i Storage.Search.Read.Fields.inc}
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ResultsAsXML, poNone, oNone, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ResultIDs, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbFindResults, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Cache.DB.Identify(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; TTL:LongInt; var Item:TItem):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result:=0; iCount:=0; Item.ID:=0;
  SetLength(Item.ResultsAsXML,0);
  Core.Arrays.LargeWord.Empty(Item.Results);
  Try
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ProviderID, poAnd, oEqual, Item.ProviderID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NetworkID, poAnd, oEqual, Item.NetworkID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.QueryID, poAnd, oEqual, Item.QueryID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForOrderBy,  IDs.Modified,poNone,oAscending,Commands);

    {$i Storage.Search.Read.Fields.inc}

    if (Item.ReadDisco) then begin
      Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ResultsAsXML, poNone, oNone, Commands);
      Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ResultIDs, poNone, oNone, Commands);
    end;

    Core.Database.SQL.Select(Task, @Commands, @cbReadSearch, @Item);
    if Item.ID=0 then begin
      Item.UserID:=UserID;
      Add(Task,DomainID,TTL,Item);
    end;
    Result:=Item.ID;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Cache.DB.Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Empty(Commands);
  end;
end;


class procedure Cache.Empty(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Cache.Init(var Item:TItem);
begin
  With Item do begin
    ID:=0;
    ProviderID:=0;
    NetworkID:=0;
    GroupID:=0;
    SessionID:=0;
    UserID:=0;
    QueryID   :=0;
    Limit:=0;
    Index:=-1;
    Created:=0.0;
    Modified:=0.0;
    Expires:=0.0;
    Executed:=0.0;

    Writeback:=false;
    ReadDisco:=true;

    SetLength(ResultsAsXML,0);
    Core.Arrays.LargeWord.Init(Results);
  end;
end;

class procedure Cache.Init(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Cache.Empty(Var Item:TItem);
begin
  With Item do begin
    ID:=0;
    ProviderID:=0;
    NetworkID:=0;
    GroupID:=0;
    SessionID:=0;
    UserID:=0;
    QueryID   :=0;
    Limit:=0;
    Index:=-1;
    Created:=0.0;
    Modified:=0.0;
    Expires:=0.0;
    Executed:=0.0;
    ReadDisco:=true;
    Writeback:=false;

    SetLength(ResultsAsXML,0);
    Core.Arrays.LargeWord.Empty(Results);
  end;
end;

class procedure Cache.Done(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Cache.Done(Var Item:TItem);
begin
  Finalize(Item.ResultsAsXML);
  Core.Arrays.LargeWord.Done(Item.Results);
  Finalize(Item);
end;

class function  Cache.fromXML(xNode:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID       :=toQWord(xNode,XML.Fields.ID);
      Item.ProviderID  :=toQWord(xNode,XML.Fields.ProviderID);
      Item.NetworkID :=toQWord(xNode,XML.Fields.NetworkID);
      Item.GroupID :=toQWord(xNode,XML.Fields.GroupID);
      Item.SessionID:=toQWord(xNode,XML.Fields.SessionID);
      Item.UserID:=toQWord(xNode,XML.Fields.UserID);
      Item.QueryID:=toQWord(xNode,XML.Fields.QueryID);

      Item.Limit      := toInteger(xNode,XML.Fields.Limit);
      Item.Index      := toInteger(xNode,XML.Fields.Index);

      Item.Created  := toDouble(xNode,XML.Fields.Created);
      Item.Modified := toDouble(xNode,XML.Fields.Modified);
      Item.Executed := toDouble(xNode,XML.Fields.Executed);

      Item.ReadDisco := toBoolean(xNode,XML.Fields.ReadDisco);
      Item.WriteBack := toBoolean(xNode,XML.Fields.WriteBack);

      Item.ResultsAsXML := toString(xNode,XML.Fields.ResultsAsXML);

      toQWordArray(xNode,XML.Fields.ResultIDs,Item.Results);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function  Cache.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Search),Item);
end;

class function  Cache.fromXML(xDoc:TXMLDocument; var Items:TItems):boolean;
var
  xItms:TDOMNode;
  xItm:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PItem;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Empty(Items);
  with Core.XML.DB do begin
    xItms:=getNode(xDoc,XML.Stanzas.Searches);
    if xItms<>nil then begin
      for iLcv:=0 to xItms.ChildNodes.Count-1 do begin
        xItm:=xItms.ChildNodes[iLcv];
        if SysUtils.SameText(xItm.NodeName,XML.Stanzas.Search) then begin
          New(itmP);
          Init(itmP^);
          SetLength(Items,iCount+1);
          Items[iCount]:=itmP;
          Inc(iCount);
          fromXML(xItm,itmP^);
        end;
      end;
      Result:=True;
    end else
      Result:=false;
  end;
end;

class Function  Cache.toXML(var Item:TItems; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Searches,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Searches,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Cache.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Search,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.ProviderID,Item.ProviderID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.GroupID,Item.GroupID),Output);
    Core.Streams.Write(Print(XML.Fields.SessionID,Item.SessionID),Output);
    Core.Streams.Write(Print(XML.Fields.QueryID,Item.QueryID),Output);
    Core.Streams.Write(Print(XML.Fields.UserID,Item.UserID),Output);
    Core.Streams.Write(Print(XML.Fields.Limit,Item.Limit),Output);
    Core.Streams.Write(Print(XML.Fields.Index,Item.Index),Output);


    Core.Streams.Write(Print(XML.Fields.ReadDisco,Item.ReadDisco),Output);
    Core.Streams.Write(Print(XML.Fields.WriteBack,Item.WriteBack),Output);

    Core.Streams.Write(Print(XML.Fields.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Expires,Item.Expires),Output);

    Core.Streams.Write(Print(XML.Fields.ResultsAsXML,Item.ResultsAsXML,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.ResultIDs,Item.Results),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Search,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;


class procedure Provider.Empty(var Item:TItem);
begin
  Item.ID:=0;
  SetLength(Item.Namespace1,0);
  SetLength(Item.Namespace2,0);
end;

class procedure Provider.Done(var Item:TItem);
begin
  Finalize(Item.Namespace1);
  Finalize(Item.Namespace2);
  Finalize(Item);
end;

class procedure Provider.Init(var Item:TItem);
begin
  Item.ID:=0;
  SetLength(Item.Namespace1,0);
  SetLength(Item.Namespace2,0);
end;

procedure cbIdentifyProvider(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Provider.DB.Keys.ID).AsLargeInt;
end;

class function  Provider.DB.Identify(Task:Core.Database.Types.TTask; Namespace1,Namespace2:Core.Strings.VarString):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iID:QWord;
begin
  Result := 0;
  try
    Namespace1:=Lowercase(Namespace1);
    Namespace1:=Lowercase(Namespace2);
    iCount := 0;
    iID:=0;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Namespace1, poNone, oEqual, Namespace1, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Namespace2, poAnd, oEqual, Namespace2, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ID, poNone, oNone, Commands);

    Core.Database.SQL.StartTransaction(Task);
    Try
      Core.Database.SQL.Select(Task, @Commands, @cbIdentifyProvider, @iID);
      if (iID=0) then Add(Task,NameSpace1,Namespace2,iID);
      Result:=iID;
    finally
      Core.Database.SQL.CommitTransaction(Task);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Provider.DB.Identify(Task:Core.Database.Types.TTask; Var Item:TItem):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iReset,iInsertID:QWord;
begin
  Result := 0;
  try
    iCount := 0; Item.ID:=0;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Namespace1, poNone, oEqual, Item.Namespace1, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Namespace2, poAnd, oEqual, Item.Namespace2, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.ID, poNone, oNone, Commands);

    Core.Database.SQL.StartTransaction(Task);
    Try
      Core.Database.SQL.Select(Task, @Commands, @cbIdentifyProvider, @Item.ID);
      if (Item.ID=0) then
        Add(Task,Item.NameSpace1,Item.NameSpace2,Item.ID);
      Result:=Item.ID;
    finally
      Core.Database.SQL.CommitTransaction(Task);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadProvider(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  itmP:Provider.PItem;
begin
  itmP:=DataP;
  itmP^.ID:=Fields.FieldByName(Provider.DB.Keys.ID).AsLargeInt;
  itmP^.NameSpace1:=Fields.FieldByName(Provider.DB.Keys.NameSpace1).AsString;
  itmP^.NameSpace2:=Fields.FieldByName(Provider.DB.Keys.NameSpace2).AsString;
end;


class function Provider.DB.Read(Task:Core.Database.Types.TTask; ItemID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Item.ID:=ItemID;

    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone, oNone, Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NameSpace1,poNone, oNone, Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NameSpace2,poNone, oNone, Commands);


    Result:=Core.Database.SQL.Select(Task, @Commands, @cbReadProvider, @Item) ;
  finally
    Core.Database.Done(Commands);
  end;
end;



class function Provider.DB.Add(Task:Core.Database.Types.TTask; var Namespace1,Namespace2:Core.Strings.VarString; out ID:QWord ):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Namespace1:=Lowercase(Namespace1);
    Namespace2:=Lowercase(Namespace2);

    iCount:=0; iReset:=0; ID:=0; iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Namespace1,poNone,oNone,Namespace1,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Namespace2,poNone,oNone,Namespace2,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Query.Empty(var Item:TItem);
begin
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.GroupID:=0;
  Item.UserID:=0;
  Item.SessionID:=0;
  Item.TimeToLive:=120;
  SetLength(Item.NameSpacePrimary,0);
  SetLength(Item.NameSpaceSecondary,0);
  SetLength(Item.Term,0);
end;

class procedure Query.Init(var Item:TItem);
begin
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.GroupID:=0;
  Item.UserID:=0;
  Item.SessionID:=0;
  Item.TimeToLive:=120;
  SetLength(Item.NameSpacePrimary,0);
  SetLength(Item.NameSpaceSecondary,0);
  SetLength(Item.Term,0);
end;

class procedure Query.Done(Var Item:TItem);
begin
  Finalize(Item.NameSpacePrimary,0);
  Finalize(Item.NameSpaceSecondary,0);
  Finalize(Item.Term,0);
  Finalize(Item);
end;

procedure cbReadQuery(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Query.PItem;
begin
  ItmP:=DataP;
  ItmP^.ID:=Fields.FieldByName(Query.DB.Keys.ID).AsLargeInt;
  ItmP^.NetworkID:=Fields.FieldByName(Query.DB.Keys.NetworkID).AsLargeInt;
  ItmP^.GroupID:=Fields.FieldByName(Query.DB.Keys.GroupID).AsLargeInt;
  ItmP^.SessionID:=Fields.FieldByName(Query.DB.Keys.SessionID).AsLargeInt;
  ItmP^.UserID:=Fields.FieldByName(Query.DB.Keys.UserID).AsLargeInt;
  ItmP^.TimeToLive:=Fields.FieldByName(Query.DB.Keys.TimeToLive).AsInteger;
  ItmP^.NamespacePrimary:=Fields.FieldByName(Query.DB.Keys.NamespacePrimary).AsString;
  ItmP^.NamespaceSecondary:=Fields.FieldByName(Query.DB.Keys.NamespaceSecondary).AsString;
  ItmP^.Term:=Fields.FieldByName(Query.DB.Keys.Term).AsString;

end;

class function  Query.DB.Identify(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iReset,iInsertID:QWord;
begin
  Result := 0;
  try
    iCount := 0; Item.ID:=0;
    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.GroupID, poAnd, oEqual, Item.GroupID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.SessionID, poAnd, oEqual, Item.SessionID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NetworkID, poAnd, oEqual, Item.NetworkID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NamespacePrimary, poAnd, oEqual, Item.NamespacePrimary, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NamespaceSecondary, poAnd, oEqual, Item.NamespaceSecondary, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Term, poAnd, oEqual, Item.Term, Commands);

    {$i Storage.Search.Query.Read.Fields.inc}

    //Core.Database.SQL.StartTransaction(Task);
    //Try
      Core.Database.SQL.Select(Task, @Commands, @cbReadQuery, @Item);
      if (Item.ID=0) then begin
        Item.UserID:=UserID;
        Add(Task,DomainID,Item);
      end;
      Result:=Item.ID;
    //finally
      //Core.Database.SQL.CommitTransaction(Task);
    //end;
  finally
    Core.Database.Done(Commands);
  end;
end;


class function  Query.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.NetworkID,poNone,oNone,Item.NetworkID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.GroupID,poNone,oNone,Item.GroupID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.SessionID,poNone,oNone,Item.SessionID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,Item.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.TimeToLive,poNone,oNone,Item.TimeToLive,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.NameSpacePrimary,poNone,oNone,Item.NameSpacePrimary,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.NameSpaceSecondary,poNone,oNone,Item.NameSpaceSecondary,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Term,poNone,oNone,Item.Term,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Query.DB.Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Item.ID:=ItemID;

    Core.Database.AddCommand(iCount, TableP, @Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Search.Query.Read.Fields.inc}

    Result:=Core.Database.SQL.Select(Task, @Commands, @cbReadQuery, @Item) ;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Query.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Empty(Item);
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Query);
  if (xItem<>nil) then
    Result:=Query.fromXML(xItem,Item);
end;

class function  Query.fromXML(xNode:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.NetworkID:=toQWord(xNode,XML.Fields.NetworkID);
      Item.UserID:=toQWord(xNode,XML.Fields.UserID);
      Item.GroupID:=toQWord(xNode,XML.Fields.GroupID);
      Item.SessionID:=toQWord(xNode,XML.Fields.SessionID);
      Item.TimeToLive:=toInteger(xNode,XML.Fields.TimeToLive);
      Item.NamespacePrimary:=toString(xNode,XML.Fields.NamespacePrimary);
      Item.NamespaceSecondary:=toString(xNode,XML.Fields.NamespaceSecondary);
      Item.Term:=toString(xNode,XML.Fields.Term);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function Query.toXML(var Item:TItem; Output:TStream; Header:Boolean):boolean;
begin
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(
      Concat(
        '<',XML.Stanzas.Query,'>',
        Print(XML.Fields.ID,Item.ID),
        Print(XML.Fields.NetworkID,Item.NetworkID),
        Print(XML.Fields.UserID,Item.UserID),
        Print(XML.Fields.GroupID,Item.GroupID),
        Print(XML.Fields.SessionID,Item.SessionID),
        Print(XML.Fields.TimeToLive,Item.TimeToLive),
        Print(XML.Fields.NameSpacePrimary,Item.NameSpacePrimary,CDATA_OFF),
        Print(XML.Fields.NameSpaceSecondary,Item.NameSpaceSecondary,CDATA_ON),
        Print(XML.Fields.Term,Item.Term,CDATA_ON),
        '</',XML.Stanzas.Query,'>'
      ),
      Output
    );
  end;
  Result:=true;
end;

initialization
  RegisterDB;
end.


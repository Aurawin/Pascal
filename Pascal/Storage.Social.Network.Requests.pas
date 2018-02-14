unit Storage.Social.Network.Requests;


interface

uses
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

  Storage,
  Encryption.Base64,
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
  type
    Stanzas=class
    const
      Request                  : Core.Database.Types.VarString = 'request';
      Requests                 : Core.Database.Types.VarString = 'requests';
    end;
    Fields=class
    const
      ID                       : Core.Database.Types.VarString = 'id';
      NetworkID                : Core.Database.Types.VarString = 'nid';
      QueryID                  : Core.Database.Types.VarString = 'qid';
      ResponseID               : Core.Database.Types.VarString = 'rid';
      Opened                   : Core.Database.Types.VarString = 'odt';
      Closed                   : Core.Database.Types.VarString = 'cdt';
      Expires                  : Core.Database.Types.VarString = 'exp';
      Flags                    : Core.Database.Types.VarString = 'fls';
      Query                    : Core.Database.Types.VarString = 'qry';
      Response                 : Core.Database.Types.VarString = 'rsp';
    end;
  end;
  DB = class
  type
    IDs = class
    const
      ID                       : Core.Database.Types.Integer = 0;
      InsertID                 : Core.Database.Types.Integer = 1;
      DomainID                 : Core.Database.Types.Integer = 2;
      NetworkID                : Core.Database.Types.Integer = 3;
      QueryID                  : Core.Database.Types.Integer = 4;
      ResponseID               : Core.Database.Types.Integer = 5;
      Opened                   : Core.Database.Types.Integer = 6;
      Closed                   : Core.Database.Types.Integer = 7;
      Expires                  : Core.Database.Types.Integer = 8;
      Flags                    : Core.Database.Types.Integer = 9;
      Query                    : Core.Database.Types.Integer = 10;
      Response                 : Core.Database.Types.Integer = 11;
    end;
    Keys=class
    const
      ID                       : Core.Database.Types.VarString = 'ITID';
      InsertID                 : Core.Database.Types.VarString = 'IIID';
      DomainID                 : Core.Database.Types.VarString = 'IDID';
      NetworkID                : Core.Database.Types.VarString = 'INID';
      QueryID                  : Core.Database.Types.VarString = 'IQID';
      ResponseID               : Core.Database.Types.VarString = 'IRID';
      Opened                   : Core.Database.Types.VarString = 'IOPD';
      Closed                   : Core.Database.Types.VarString = 'ICLD';
      Expires                  : Core.Database.Types.VarString = 'IEXP';
      Flags                    : Core.Database.Types.VarString = 'IFGS';
      Query                    : Core.Database.Types.VarString = 'IQRY';
      Response                 : Core.Database.Types.VarString = 'IRSP';
    end;
  const
    TableP   : Core.Database.Types.PTable = nil;
    MonitorP : Core.Database.Monitor.Types.PItem = nil;
    Startup  : Core.Database.Types.TableIni = (
      AutoCreate               : True;
      AutoCommit               : True;
      Group                    : 'System/Applications/Social';
      Name                     : 'Request';
      Value                    : 'scs_soc_rq';
      Hint                     : 'Network Subscription Requests';
      PrimaryKeyP              : @Keys.ID;
      );
    Fields: array [0..11] of Core.Database.Types.Field = (
      (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
      (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
      (IDP: @IDs.QueryID; KeyP: @Keys.QueryID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.ResponseID; KeyP: @Keys.ResponseID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Opened;  KeyP: @Keys.Opened; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Closed; KeyP: @Keys.Closed; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Expires; KeyP: @Keys.Expires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Flags;  KeyP: @Keys.Flags; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
      (IDP: @IDs.Query; KeyP: @Keys.Query; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 10240; Flags: cfNone;  ),
      (IDP: @IDs.Response; KeyP: @Keys.Response; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 10240; Flags: cfNone; )
    );
  end;
  Type Defaults=class
  const
    DaysToLive                 = 60;
    Opened                     : double = 0.0;
  end;
  Type Flags=class
  const
    None                       = 0;
    Invited                    = 1 shl 0;
    Rejected                   = 1 shl 1;
    Accepted                   = 1 shl 2;
    Visible                    = 1 shl 3;
  end;
  PRequest=^TRequest;
  TRequests=array of PRequest;
  PRequests=^TRequests;
  TRequest=record
    ID                         : QWord;
    NetworkID                  : QWord;
    QueryID                    : QWord;
    ResponseID                 : QWord;
    Opened                     : double;
    Closed                     : double;
    Expires                    : double;
    Flags                      : Byte;
    Query                      : Core.Strings.VarString;
    Response                   : Core.Strings.VarString;
  end;

  function  fromXML(xNode:TDOMNode; var Item:TRequest):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Item:TRequest):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Items:TRequests):boolean; overload;

  function  toXML(var Requests:TRequests; var Contacts:Storage.Roster.Items.List; Output:TMemoryStream; Header:boolean):boolean; overload;
  function  toXML(var Item:TRequests; Output:TMemoryStream; Header:boolean):boolean; overload;
  function  toXML(var Item:TRequest; Output:TMemoryStream; Header:boolean):boolean; overload;

  function  Make(Task:Core.Database.Types.TTask; DomainID,NetworkID,QueryID:QWord; Var Item:TRequest):boolean; overload;
  function  Invite(Task:Core.Database.Types.TTask; DomainID,NetworkID,QueryID,ResponseID:QWord; Var Item:TRequest):boolean; overload;
  function  Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TRequest):boolean;
  function  SetResponse(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID,UserID:QWord; aFlags:Byte; aResponse:Core.Strings.VarString):boolean;
  function  Accept(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TRequest):boolean;
  function  Reject(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TRequest):boolean;
  function  getFlags(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):Byte;
  function  getQueryID(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):QWord;

  function  List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TRequests):boolean;
  function  Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):boolean;

  procedure Empty(var Item:TRequests); overload;
  procedure Empty(var Item:TRequest); overload;

  procedure Done(Var Item:TRequests); overload;
  procedure Done(Var Item:TRequest); overload;

  procedure Init(var Item:TRequests); overload;
  procedure Init(var Item:TRequest); overload;

implementation
uses
  Storage.Social,
  Storage.Social.Network,


  DateUtils;

procedure cbDestroyRequest(ItemP: Core.Database.Monitor.Types.PItem);
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
    if ItemP = DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.QueryID, poNone, oEqual, ItemID, Commands);
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
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyRequest, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

procedure Empty(var Item:TRequests);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Init(var Item:TRequest);
begin
  With Item do begin
    ID:=0;
    NetworkID:=0;
    QueryID:=0;
    ResponseID:=0;
    Opened:=0.0;
    Closed:=0.0;
    Flags:=Storage.Social.Network.Requests.Flags.None;
    SetLength(Query,0);
    SetLength(Response,0);
  end;
end;

procedure Init(var Item:TRequests);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Empty(Var Item:TRequest);
begin
  With Item do begin
    ID:=0;
    NetworkID:=0;
    QueryID:=0;
    ResponseID:=0;
    Opened:=0.0;
    Closed:=0.0;
    Flags:=Storage.Social.Network.Requests.Flags.None;
    SetLength(Query,0);
    SetLength(Response,0);
  end;
end;

procedure Done(Var Item:TRequests);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure Done(Var Item:TRequest);
begin
  With Item do begin
    Finalize(Query);
    Finalize(Response);
  end;
  Finalize(Item);
end;

function  fromXML(xNode:TDOMNode; var Item:TRequest):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.NetworkID:=toQWord(xNode,XML.Fields.NetworkID);
      Item.QueryID:=toQWord(xNode,XML.Fields.QueryID);
      Item.ResponseID:=toQWord(xNode,XML.Fields.ResponseID);
      Item.Opened:=toDouble(xNode,XML.Fields.Opened);
      Item.Closed:=toDouble(xNode,XML.Fields.Closed);
      Item.Expires:=toDouble(xNode,XML.Fields.Expires);
      Item.Flags:=toByte(xNode,XML.Fields.Flags);
      Item.Query:=toString(xNode,XML.Fields.Query);
      Item.Response:=toString(xNode,XML.Fields.Response);
    end;
    Result:=True;
  end else
    Result:=false;
end;

function fromXML(xDoc:TXMLDocument; var Item:TRequest):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Request),Item);
end;

function fromXML(xDoc:TXMLDocument; var Items:TRequests):boolean;
var
  xItms:TDOMNode;
  xItm:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PRequest;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Empty(Items);
  with Core.XML.DB do begin
    xItms:=getNode(xDoc,XML.Stanzas.Requests);
    if xItms<>nil then begin
      for iLcv:=0 to xItms.ChildNodes.Count-1 do begin
        xItm:=xItms.ChildNodes[iLcv];
        if SameText(xItm.NodeName,XML.Stanzas.Request) then begin
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

function toXML(var Requests:TRequests; var Contacts:Storage.Roster.Items.List; Output:TMemoryStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Requests,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Requests) do
    toXML(Requests[iLcv]^,Output,XML_HEADER_OFF);

  Storage.Social.Network.Members.toXML(Contacts,Output,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Requests,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;

end;

function toXML(var Item:TRequests; Output:TMemoryStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Requests,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Requests,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

function toXML(var Item:TRequest; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Request,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.QueryID,Item.QueryID),Output);
    Core.Streams.Write(Print(XML.Fields.ResponseID,Item.ResponseID),Output);
    Core.Streams.Write(Print(XML.Fields.Opened,Item.Opened),Output);
    Core.Streams.Write(Print(XML.Fields.Closed,Item.Closed),Output);
    Core.Streams.Write(Print(XML.Fields.Expires,Item.Expires),Output);
    Core.Streams.Write(Print(XML.Fields.Flags,Item.Flags),Output);
    Core.Streams.Write(Print(XML.Fields.Query,Item.Query,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Response,Item.Response,CDATA_ON),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Request,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

function Make(Task:Core.Database.Types.TTask; DomainID,NetworkID,QueryID:QWord; var Item:TRequest):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    SetLength(Item.Response,0);
    Item.ResponseID:=0;
    Item.NetworkID:=NetworkID;
    Item.QueryID:=QueryID;
    Item.Flags:=Flags.None;
    Item.Opened:=Core.Timer.dtUT;
    Item.Expires:=DateUtils.IncDay(Core.Timer.dtUT,Defaults.DaysToLive);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    {$i Storage.Social.Request.Add.Fields.inc}
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
  incRequestCount(Task,DomainID,NetworkID);
end;

function Invite(Task:Core.Database.Types.TTask; DomainID,NetworkID,QueryID,ResponseID:QWord; var Item:TRequest):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    SetLength(Item.Response,0);
    Item.NetworkID:=NetworkID;
    Item.QueryID:=QueryID;
    Item.ResponseID:=ResponseID;
    Item.Flags:=Flags.Invited;
    Item.Opened:=Core.Timer.dtUT;
    Item.Expires:=DateUtils.IncDay(Core.Timer.dtUT,Defaults.DaysToLive);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    {$i Storage.Social.Request.Add.Fields.inc}

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function SetResponse(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID,UserID:QWord; aFlags:Byte; aResponse:Core.Strings.VarString):boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.ResponseID, poNone, oNone, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Flags, poNone, oNone, aFlags, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Response, poNone, oNone, aResponse, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function Accept(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TRequest):boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;
  Conn     : Storage.Social.Connection.TConnection;
begin
  Result := False;
  try
    iCount := 0;
    Item.Closed:=Core.Timer.dtUT;
    Item.ResponseID:=UserID;
    Item.Flags:=Item.Flags or Flags.Accepted;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, Item.NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Flags, poNone, oNone, Item.Flags, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Closed, poNone, oNone, Item.Closed, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.ResponseID, poNone, oNone, Item.ResponseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Response, poNone, oNone, Item.Response, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);

    if (Item.Flags or Flags.Visible=Item.Flags) then
      Members.Add(Task,DomainID,Item.QueryID,Item.NetworkID,Members.Face.Open)
    else
      Members.Add(Task,DomainID,Item.QueryID,Item.NetworkID,Members.Face.Closed);

    if Result then begin
      Connection.Add(Task,DomainID,Item.NetworkID,Item.QueryID,Conn);
      decRequestCount(Task,DomainID,Item.NetworkID);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

function Reject(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TRequest):boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Closed:=Core.Timer.dtUT;
    Item.ResponseID:=UserID;
    Item.Flags:=Item.Flags or Flags.Rejected;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, Item.NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Flags, poNone, oNone, Item.Flags, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Closed, poNone, oNone, Item.Closed, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.ResponseID, poNone, oNone, Item.ResponseID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Response, poNone, oNone, Item.Response, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
    if Result then
      decRequestCount(Task,DomainID,Item.NetworkID);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadRequest(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:PRequest;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadRequest.Fields.inc}
end;

function Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TRequest):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Social.Request.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadRequest, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListRequests(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:PRequests;
  ItmP:PRequest;
  iIndex:LongInt;
begin
  New(ItmP);
  Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadRequest.Fields.inc}
end;

function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TRequests):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Closed, poAnd, oEqual, Defaults.Opened, Commands);
    {$i Storage.Social.Request.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListRequests, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetRequestFlags(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PByte(DataP)^:=Fields.FieldByName(DB.Keys.Flags).AsInteger;
end;

function getFlags(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):Byte;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Value:Byte;
begin
  Result := 0;
  try
    iCount := 0;
    Value:=0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Flags, poNone, oNone, Commands);

    Core.Database.SQL.Select(Task, @Commands, @cbGetRequestFlags, @Value);

    Result:=Value;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetQueryID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(DB.Keys.QueryID).AsLargeInt;
end;

function getQueryID(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):QWord;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Value:QWord;
begin
  Result := 0;
  try
    iCount := 0;
    Value:=0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.QueryID, poNone, oNone, Commands);

    Core.Database.SQL.Select(Task, @Commands, @cbGetQueryID, @Value);

    Result:=Value;
  finally
    Core.Database.Done(Commands);
  end;
end;

function Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  // Maybe we should check before if UserID is present in Network's Admins[QWord] or if (UserID = QueryID)
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

initialization
  RegisterDB;
end.


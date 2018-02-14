unit Storage.Collages;


{
  unit Storage.Collages

  Collage Database Module

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

interface

uses
  Classes,

  RSR.Core,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.CoreObjects,
  Storage.Social,

  Core.XML,
  Core.Timer,
  Core.Streams,
  Core.Strings,

  SysUtils,
  DOM;


type
  Images=class
  Type
    TItem=record
      CollageID                : QWord;
      NetworkID                : QWord;
      OwnerID                  : QWord;
      FolderID                 : QWord;
      FileID                   : QWord;
      Element                  : LongInt;
      SubItem                  : LongInt;
      ScaleX                   : double;
      ScaleY                   : double;
      MoveX                    : LongInt;
      MoveY                    : LongInt;
      Rotate                   : LongInt;
      Data                     : Core.Strings.VarString;
    end;
    PItem=^TItem;
    TItems=Array of PItem;
    PItems=^TItems;
    XML=class
    type
      Stanzas=class
      const
        Items                    = 'imgs';
        Item                     = 'img';
      end;
      Fields=class
      const
        CollageID                = 'cid';
        OwnerID                  = 'oid';
        FolderId                 = 'fld';
        FileId                   = 'fid';
        NetworkId                = 'nid';
        Element                  = 'elm';
        SubItem                  = 'esi';
        MoveX                    = 'mvx';
        MoveY                    = 'mvy';
        Rotate                   = 'rot';
        ScaleX                   = 'scx';
        ScaleY                   = 'scy';
        Data                     = 'dat';
      end;
    end;
    DB = class
      type
      IDs = class
      const
        CollageID              : Core.Database.Types.Integer = 0;
        DomainID               : Core.Database.Types.Integer = 1;
        UserID                 : Core.Database.Types.Integer = 2;
        OwnerID                : Core.Database.Types.Integer = 3;
        FolderID               : Core.Database.Types.Integer = 4;
        FileID                 : Core.Database.Types.Integer = 5;
        NetworkID              : Core.Database.Types.Integer = 6;
        Element                : Core.Database.Types.Integer = 7;
        SubItem                : Core.Database.Types.Integer = 8;
        MoveX                  : Core.Database.Types.Integer = 9;
        MoveY                  : Core.Database.Types.Integer = 10;
        ScaleX                 : Core.Database.Types.Integer = 11;
        ScaleY                 : Core.Database.Types.Integer = 12;
        Rotate                 : Core.Database.Types.Integer = 13;
      end;
      Keys = class
      const
        CollageID              : Core.Database.Types.VarString = 'TCLD';
        DomainID               : Core.Database.Types.VarString = 'TDID';
        UserID                 : Core.Database.Types.VarString = 'TUID';
        OwnerID                : Core.Database.Types.VarString = 'FOID';
        FolderID               : Core.Database.Types.VarString = 'IFLD';
        FileID                 : Core.Database.Types.VarString = 'IFID';
        NetworkID              : Core.Database.Types.VarString = 'TNID';
        Element                : Core.Database.Types.VarString = 'TELM';
        SubItem                : Core.Database.Types.VarString = 'TESI';
        MoveX                  : Core.Database.Types.VarString = 'MVX';
        MoveY                  : Core.Database.Types.VarString = 'MVY';
        ScaleX                 : Core.Database.Types.VarString = 'ISCX';
        ScaleY                 : Core.Database.Types.VarString = 'ISCY';
        Rotate                 : Core.Database.Types.VarString = 'IRT';
      end;
    const
      TableP                   : Core.Database.Types.PTable = nil;
      MonitorP                 : Core.Database.Monitor.Types.PItem = nil;
      Startup                  : Core.Database.Types.TableIni = (
        AutoCreate             : True;
        AutoCommit             : True;
        Group                  : 'System/Applications/Collages';
        Name                   : 'Image Map';
        Value                  : 'scs_clgs_im';
        Hint                   : 'Collage item images storage';
        PrimaryKeyP            : Core.Database.Types.NoKey;
        );
      Fields                   : array [0..13] of Core.Database.Types.Field = (
        (IDP: @IDs.CollageID; KeyP: @Keys.CollageID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.FolderID; KeyP: @Keys.FolderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.FileID; KeyP: @Keys.FileID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Element; KeyP: @Keys.Element; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.SubItem; KeyP: @Keys.SubItem; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.MoveX; KeyP: @Keys.MoveX; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.MoveY; KeyP: @Keys.MoveY; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ScaleX; KeyP: @Keys.ScaleX; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ScaleY; KeyP: @Keys.ScaleY; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Rotate; KeyP: @Keys.Rotate; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function Add(Task: Core.Database.Types.TTask; DomainID,NetworkID,UserID:QWord; var Item: TItem): boolean;
      class function Update(Task:Core.Database.Types.TTask; DomainID,UserID,CollageID:QWord; var Item:TItems):boolean;

      class function Delete(Task: Core.Database.Types.TTask; DomainID,UserID,CollageID:QWord): boolean; overload;
      class function Delete(Task: Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem): boolean; overload;
      class function List(Task: Core.Database.Types.TTask; DomainID,UserID,CollageID:QWord; var Item: TItems): boolean; overload;
      class function List(Task: Core.Database.Types.TTask; DomainID,CollageID:QWord; var Item: TItems): boolean; overload;
    end;
    class function  Push(var Item:TItem; var List:TItems): LongInt;
    class procedure Init(var Item: TItem); overload;
    class procedure Init(var Item: TItems); overload;
    class procedure Empty(var Item: TItem); overload;
    class procedure Empty(var Item: TItems); overload;
    class procedure Done(var Item: TItem); overload;
    class procedure Done(var Item: TItems); overload;



    class function fromXML(xItem:TDOMNode; var Item:TItem):boolean; overload;
    class function fromXML(xItems:TDOMNode; var Item:TItems):boolean; overload;
    class function toXML(var Item:TItems; Output:TMemoryStream):boolean; overload;
    class function toXML(var Item:TItem; Output:TMemoryStream):boolean; overload;
  end;
  Items = class
  type
    TItem = record
      ID                       : QWord;
      UserID                   : QWord;
      NetworkID                : QWord;
      Created                  : double;
      Modified                 : double;
      Kind                     : LongInt;
      Delay                    : LongInt;
      Title                    : Core.Strings.VarString;
      Description              : Core.Strings.VarString;
      Glyphs                   : Images.TItems;
    end;
    PItem = ^TItem;
    PItems= ^TItems;
    TItems=Array of PItem;

    XML=class
    type
      Stanzas=class
      const
        Items                    = 'clgs';
        Item                     = 'clg';
      end;
      Fields=class
      const
        ID                       = 'id';
        Created                  = 'ctd';
        Modified                 = 'mdt';
        Delay                    = 'dly';
        Kind                     = 'knd';
        Title                    = 'tle';
        Description              = 'dsc';
      end;
    end;
    DB = class
      type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        NetworkID                : Core.Database.Types.Integer = 4;
        Created                  : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        Kind                     : Core.Database.Types.Integer = 7;
        Delay                    : Core.Database.Types.Integer = 8;
        Title                    : Core.Database.Types.Integer = 9;
        Description              : Core.Database.Types.Integer = 10;
      end;
      Keys = class
      const
        ID                       : Core.Database.Types.VarString = 'TID';
        InsertID                 : Core.Database.Types.VarString = 'TIID';
        DomainID                 : Core.Database.Types.VarString = 'TDID';
        UserID                   : Core.Database.Types.VarString = 'TUID';
        NetworkID                : Core.Database.Types.VarString = 'TNID';
        Created                  : Core.Database.Types.VarString = 'TCTD';
        Modified                 : Core.Database.Types.VarString = 'MDFD';
        Kind                     : Core.Database.Types.VarString = 'TKND';
        Delay                    : Core.Database.Types.VarString = 'TDLY';
        Title                    : Core.Database.Types.VarString = 'ITIT';
        Description              : Core.Database.Types.VarString = 'IDES';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Collages';
        Name                     : 'Main';
        Value                    : 'scs_clgs_m';
        Hint                     : 'Collage item storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..10] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Delay; KeyP: @Keys.Delay; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function Add(Task: Core.Database.Types.TTask; DomainID, NetworkID,UserID: QWord; var Item: TItem): boolean;
      class function Read(Task: Core.Database.Types.TTask; DomainID,NetworkID,UserID,ItemID: QWord; var Item: TItem): boolean; overload;
      class function Read(Task: Core.Database.Types.TTask; DomainID,ItemID: QWord; var Item: TItem): boolean; overload;
      class function Write(Task: Core.Database.Types.TTask; DomainID, NetworkID,UserID: QWord; var Item: TItem): boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID, NetworkID,UserID, ItemID:QWord):boolean;
      class function List(Task:Core.Database.Types.TTask; DomainID, NetworkID, UserID:QWord; var Items:TItems):boolean;
    end;
    class procedure Init(var Item: TItem);
    class procedure Empty(var Item: TItem); overload;
    class procedure Empty(var Item: TItems); overload;
    class procedure Done(var Item: TItem); overload;
    class procedure Done(var Item: TItems); overload;
    class function IndexOf(ID:QWord; var Items:TItems): LongInt;

    class function fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
    class function toXML(var Item:TItems; Output:TMemoryStream; Header:boolean):boolean; overload;
    class function toXML(var Item:TItem; Output:TMemoryStream; Header:boolean):boolean; overload;
  end;

  PalmView=class
  Type
    XML=class
    type
      Stanzas=class
      const
        Item                   = 'plmv';
      end;
      Fields=class
      const
        ID                     = 'id';
        Created                = 'ctd';
        Modified               = 'mtd';
        Delay                  = 'dly';
        Kind                   = 'knd';
        By                     = 'by';
        Title                  = 'ttl';
        Description            = 'des';
        Avatar                 = 'ava';
        Glyphs                 = 'imgs';
      end;
    end;
    TItem=record
      ID                       : QWord;
      Created                  : Double;
      Modified                 : Double;
      Kind                     : LongInt;
      Delay                    : LongInt;
      Avatar                   : Core.Strings.VarString;
      By                       : Core.Strings.VarString;
      Title                    : Core.Strings.VarString;
      Description              : Core.Strings.VarString;
      Glyphs                   : Core.Strings.VarString;
    end;
    class procedure Empty(var Item:TItem);
    class procedure Init(var Item:TItem);
    class procedure Done(var Item:TItem);

    class function toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
    class function fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;
    class function fromXML(xItem:TDOMNode; var Item:TItem):boolean; overload;

  end;

implementation

uses DB;

procedure cbDestroyCollages(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyCollageImages(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Images.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Items.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Items.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushNetworkDeleted;
  begin
    if ItemP = Items.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.NetworkID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED: PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED: PushUserDeleted();
    Core.Database.Monitor.Notify.SOCIAL_NETWORK_DELETED : PushNetworkDeleted();
  end;
end;

function cbDBMonitorNotifiedImages(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Images.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Images.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Images.DB.TableP, useForCriteria, Images.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Images.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Images.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Images.DB.TableP, useForCriteria, Images.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushNetworkDeleted;
  begin
    if ItemP = Images.DB.MonitorP then
    begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Images.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Images.DB.TableP, useForCriteria, Images.DB.IDs.NetworkID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED         : PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED           : PushUserDeleted();
    Core.Database.Monitor.Notify.SOCIAL_NETWORK_DELETED : PushNetworkDeleted();
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  if Items.DB.TableP = nil then begin
    New(Items.DB.TableP);
    Core.Database.Init(Items.DB.TableP^, Items.DB.Startup);
    for iLcv := 0 to High(Items.DB.Fields) do
      Core.Database.AddField(@Items.DB.Fields[iLcv], Items.DB.TableP);
  end;
  if Items.DB.MonitorP = nil then begin
    New(Items.DB.MonitorP);
    Core.Database.Monitor.Init(Items.DB.MonitorP^, Items.DB.TableP^, @cbDestroyCollages, @cbDBMonitorNotified);
    Core.Database.Monitor.Add(Items.DB.MonitorP);
  end;
  if Images.DB.TableP = nil then begin
    New(Images.DB.TableP);
    Core.Database.Init(Images.DB.TableP^, Images.DB.Startup);
    for iLcv := 0 to High(Images.DB.Fields) do
      Core.Database.AddField(@Images.DB.Fields[iLcv], Images.DB.TableP);
  end;
  if Images.DB.MonitorP = nil then begin
    New(Images.DB.MonitorP);
    Core.Database.Monitor.Init(Images.DB.MonitorP^, Images.DB.TableP^, @cbDestroyCollageImages, @cbDBMonitorNotifiedImages);
    Core.Database.Monitor.Add(Images.DB.MonitorP);
  end;
end;


class procedure Items.Init(var Item: TItem);
begin
  With Item do begin
    ID:=0;
    UserID:=0;
    NetworkID:=0;
    Created:=0;
    Modified:=0;
    Kind:=0;
    Delay:=0;
    SetLength(Title,0);
    SetLength(Description,0);
    Images.Init(Glyphs);
  end;
end;


class procedure Items.Empty(var Item: TItem);
begin
  With Item do begin
    ID:=0;
    UserID:=0;
    Created:=0;
    Modified:=0;
    Kind:=0;
    Delay:=0;
    SetLength(Title,0);
    SetLength(Description,0);
    Images.Empty(Glyphs);
  end;
end;


class procedure Items.Empty(var Item: TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Items.Done(var Item: TItem);
begin
  With Item do begin
    Finalize(Title);
    Finalize(Description);
    Images.Done(Glyphs);
  end;
  Finalize(Item);
end;

class procedure Items.Done(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class function Items.IndexOf(ID:QWord; var Items:TItems): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Items) do begin
    if (Items[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;


class function Items.DB.Add(Task: Core.Database.Types.TTask; DomainID, NetworkID,UserID: QWord; var Item: TItem): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iReset: QWord;
  iInsertID: QWord;
  iLcv:LongInt;
begin
  Result := False;
  iCount := 0;
  iReset := 0;
  iInsertID := Random(High(LongInt));
  Item.Created:=Core.Timer.dtUT;
  Item.Modified:=Item.Created;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, IDs.ID, poNone, oNone, Item.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, IDs.InsertID, poNone, oNone, iReset, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.UserID, poNone, oNone, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.NetworkID, poNone, oNone, NetworkID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Kind, poNone, oNone, Item.Kind, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Delay, poNone, oNone, Item.Delay, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Created, poNone, oNone, Item.Created, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Title, poNone, oNone, Item.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Description, poNone, oNone, Item.Description, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);

    if (Item.ID<>0) then begin
      for iLcv:=0 to High(Item.Glyphs) do begin
        Item.Glyphs[iLcv]^.CollageID:=Item.ID;
        Images.DB.Add(Task,DomainID,Item.Glyphs[iLcv]^.NetworkID,UserID,Item.Glyphs[iLcv]^);
      end;
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadCollageItem(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Items.PItem(DataP)^ do begin
    ID          := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    UserID      := Fields.FieldByName(Items.DB.Keys.UserID).AsLargeInt;
    Created     := Fields.FieldByName(Items.DB.Keys.Created).AsFloat;
    Modified    := Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
    Kind        := Fields.FieldByName(Items.DB.Keys.Kind).AsInteger;
    Delay       := Fields.FieldByName(Items.DB.Keys.Delay).AsInteger;
    Title       := Fields.FieldByName(Items.DB.Keys.Title).AsString;
    Description := Fields.FieldByName(Items.DB.Keys.Description).AsString;
  end;
end;

class function Items.DB.Read(Task: Core.Database.Types.TTask; DomainID,NetworkID,UserID,ItemID: QWord; var Item: TItem): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False; Empty(Item);
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    {$i Storage.Collages.Fields.inc}

    Result:=Core.Database.SQL.Select(Task, @Commands, @cbReadCollageItem, @Item);

    If Item.ID<>0 then
      Result:=Images.DB.List(Task,DomainID,UserID,ItemID,Item.Glyphs);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Read(Task: Core.Database.Types.TTask; DomainID,ItemID: QWord; var Item: TItem): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False; Empty(Item);
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    {$i Storage.Collages.Fields.inc}
    Result:=Core.Database.SQL.Select(Task, @Commands, @cbReadCollageItem, @Item);
    If Item.ID<>0 then
      Result:=Images.DB.List(Task,DomainID,ItemID,Item.Glyphs);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbCollagesList(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Items.PItems;
  iDX:LongInt;
begin
  ListP:=DataP;
  iDX:=Length(ListP^);
  SetLength(ListP^,iDX+1);
  New(ListP^[iDX]);
  Items.Init(ListP^[iDX]^);
  cbReadCollageItem(CommandsP,Fields,ListP^[iDX]);
end;

class function  Items.DB.List(Task:Core.Database.Types.TTask; DomainID,NetworkID,UserID:QWord; Var Items:TItems):boolean;
var
  iCount:LongInt;
  iLcv:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Items);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    {$i Storage.Collages.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbCollagesList, @Items);
    for iLcv:=0 to High(Items) do
      Images.DB.List(Task,DomainID,UserID,Items[iLcv]^.ID,Items[iLcv]^.Glyphs);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID, NetworkID, UserID, ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=true;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    Result:=Images.DB.Delete(Task,DomainID,UserID,ItemID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.Write(Task: Core.Database.Types.TTask; DomainID, NetworkID, UserID: QWord; var Item: TItem): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, Item.ID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Kind, poNone, oNone, Item.Kind, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Delay, poNone, oNone, Item.Delay, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Title, poNone, oNone, Item.Title, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Description, poNone, oNone, Item.Description, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False; Empty(Item);
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Item);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.Created:=toDouble(xItem,XML.Fields.Created);
      Item.Modified:=toDouble(xItem,XML.Fields.Modified);
      Item.Kind:=toInteger(xItem,XML.Fields.Kind);
      Item.Delay:=toInteger(xItem,XML.Fields.Delay);
      Item.Title:=toString(xItem,XML.Fields.Title);
      Item.Description:=toString(xItem,XML.Fields.Description);
      Images.fromXML(getChildNode(xItem,Images.XML.Stanzas.Items),Item.Glyphs);
      Result:=True;
    end;
  end;
end;

class Function  Items.toXML(var Item:TItems; Output:TMemoryStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class Function  Items.toXML(var Item:TItem; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=False;

  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(XML.Fields.Delay,Item.Delay),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Item.Title,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Item.Description,CDATA_ON),Output);
    Images.toXML(Item.Glyphs,Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

class function  Images.Push(var Item:TItem; var List:TItems): LongInt;
begin
  Result:=System.Length(List);
  SetLength(List,Result+1);
  List[Result]:=@Item;
end;

class procedure Images.Init(var Item: TItem);
begin
  With Item do begin
    CollageID:=0;
    NetworkID:=0;
    OwnerID:=0;
    FolderID:=0;
    FileID:=0;
    Element:=0;
    SubItem:=0;
    ScaleX:=0;
    ScaleY:=0;
    MoveX:=0;
    MoveY:=0;
    Rotate:=0;
    SetLength(Data,0);
  end;
end;

class procedure Images.Init(var Item: TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Images.Empty(var Item: TItem);
begin
  With Item do begin
    CollageID:=0;
    NetworkID:=0;
    OwnerID:=0;
    FolderID:=0;
    FileID:=0;
    Element:=0;
    SubItem:=0;
    ScaleX:=0;
    ScaleY:=0;
    MoveX:=0;
    MoveY:=0;
    Rotate:=0;
    SetLength(Data,0);
  end;
end;

class procedure Images.Empty(var Item: TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Images.Done(var Item: TItem);
begin
  Finalize(Item.Data);
  Finalize(Item);
end;

class procedure Images.Done(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;


class Function  Images.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Result:=False;
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.CollageID:=toQWord(xItem,XML.Fields.CollageID);
      Item.NetworkID:=toQWord(xItem,XML.Fields.NetworkID);
      Item.FolderID:=toQWord(xItem,XML.Fields.FolderID);
      Item.FileID:=toQWord(xItem,XML.Fields.FileID);
      Item.Element:=toInteger(xItem,XML.Fields.Element);
      Item.SubItem:=toInteger(xItem,XML.Fields.SubItem);
      Item.ScaleX:=toDouble(xItem,XML.Fields.ScaleX);
      Item.ScaleY:=toDouble(xItem,XML.Fields.ScaleY);
      Item.MoveX:=toInteger(xItem,XML.Fields.MoveX);
      Item.MoveY:=toInteger(xItem,XML.Fields.MoveY);
      Item.Rotate:=toInteger(xItem,XML.Fields.Rotate);
      Result:=True;
    end;
  end;
end;

class Function  Images.fromXML(xItems:TDOMNode; var Item:TItems):boolean;
var
  xItem:TDOMNode;
  iLcv:LongInt;
  itmP:PItem;
begin
  Result:=False; Empty(Item);
  for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
    xItem:=xItems.ChildNodes[iLcv];
    if SysUtils.SameText(xItem.NodeName,XML.Stanzas.Item) then begin
      new(itmP);
      Init(itmP^);
      Push(itmP^,Item);
      fromXML(xItem,itmP^);
    end;
  end;
end;

class Function  Images.toXML(var Item:TItems; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class Function  Images.toXML(var Item:TItem; Output:TMemoryStream):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.CollageID,Item.CollageID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.OwnerID,Item.OwnerID),Output);
    Core.Streams.Write(Print(XML.Fields.FolderID,Item.FolderID),Output);
    Core.Streams.Write(Print(XML.Fields.FileID,Item.FileID),Output);
    Core.Streams.Write(Print(XML.Fields.Element,Item.Element),Output);
    Core.Streams.Write(Print(XML.Fields.SubItem,Item.SubItem),Output);
    Core.Streams.Write(Print(XML.Fields.ScaleX,Item.ScaleX),Output);
    Core.Streams.Write(Print(XML.Fields.ScaleY,Item.ScaleY),Output);
    Core.Streams.Write(Print(XML.Fields.MoveX,Item.MoveX),Output);
    Core.Streams.Write(Print(XML.Fields.MoveY,Item.MoveY),Output);
    Core.Streams.Write(Print(XML.Fields.Rotate,Item.Rotate),Output);
    Core.Streams.Write(Print(XML.Fields.Data,Item.Data,CDATA_OFF),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

procedure cbReadCollageImageItem(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Images.PItem(DataP)^ do begin
    CollageID   := Fields.FieldByName(Images.DB.Keys.CollageID).AsLargeInt;
    NetworkID   := Fields.FieldByName(Images.DB.Keys.NetworkID).AsLargeInt;
    OwnerID     := Fields.FieldByName(Images.DB.Keys.OwnerID).AsLargeInt;
    FolderID    := Fields.FieldByName(Images.DB.Keys.FolderID).AsLargeInt;
    FileID      := Fields.FieldByName(Images.DB.Keys.FileID).AsLargeInt;
    Element     := Fields.FieldByName(Images.DB.Keys.Element).AsInteger;
    SubItem     := Fields.FieldByName(Images.DB.Keys.SubItem).AsInteger;
    ScaleX      := Fields.FieldByName(Images.DB.Keys.ScaleX).AsFloat;
    ScaleY      := Fields.FieldByName(Images.DB.Keys.ScaleY).AsFloat;
    MoveX       := Fields.FieldByName(Images.DB.Keys.MoveX).AsInteger;
    MoveY       := Fields.FieldByName(Images.DB.Keys.MoveY).AsInteger;
    Rotate      := Fields.FieldByName(Images.DB.Keys.Rotate).AsInteger;
  end;
end;

procedure cbCollageImagesList(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Images.PItems;
  iDX:LongInt;
begin
  ListP:=DataP;
  iDX:=Length(ListP^);
  SetLength(ListP^,iDX+1);
  New(ListP^[iDX]);
  Images.Init(ListP^[iDX]^);
  cbReadCollageImageItem(CommandsP,Fields,ListP^[iDX]);
end;

class function Images.DB.Update(Task:Core.Database.Types.TTask; DomainID,UserID,CollageID:QWord; var Item:TItems):boolean;
var
  iLcv:LongInt;
begin
  Result:=Delete(Task,DomainID,UserID,CollageID);
  if (Result=true) then begin
    for iLcv:=0 to High(Item) do begin
      Item[iLcv]^.CollageID:=CollageID;
      Add(Task,DomainID,Item[iLcv]^.NetworkID,UserID,Item[iLcv]^);
    end;
  end;
end;

class function Images.DB.Add(Task: Core.Database.Types.TTask; DomainID,NetworkID,UserID:QWord; var Item: TItem): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := false;
  iCount := 0;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.NetworkID, poNone, oNone, NetworkID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.UserID, poNone, oNone, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.CollageID, poNone, oNone, Item.CollageID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.OwnerID, poNone, oNone, Item.OwnerID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.FolderID, poNone, oNone, Item.FolderID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.FileID, poNone, oNone, Item.FileID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Element, poNone, oNone, Item.Element, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.SubItem, poNone, oNone, Item.SubItem, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.ScaleX, poNone, oNone, Item.ScaleX, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.ScaleY, poNone, oNone, Item.ScaleY, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.MoveX, poNone, oNone, Item.MoveX, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.MoveY, poNone, oNone, Item.MoveY, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Rotate, poNone, oNone, Item.Rotate, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Images.DB.Delete(Task: Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem): boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=true;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.CollageID,poAnd,oEqual,Item.CollageID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,Item.NetworkID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,Item.FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FileID,poAnd,oEqual,Item.FileID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Element,poAnd,oEqual,Item.Element,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.SubItem,poAnd,oEqual,Item.SubItem,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Images.DB.Delete(Task: Core.Database.Types.TTask; DomainID,UserID,CollageID:QWord): boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=true;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.CollageID,poAnd,oEqual,CollageID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Images.DB.List(Task: Core.Database.Types.TTask; DomainID,UserID,CollageID:QWord; var Item: TItems): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.CollageID, poAnd, oEqual, CollageID,Commands);
    {$i Storage.Collages.Images.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbCollageImagesList, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Images.DB.List(Task: Core.Database.Types.TTask; DomainID,CollageID:QWord; var Item: TItems): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.CollageID, poAnd, oEqual, CollageID,Commands);
    {$i Storage.Collages.Images.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbCollageImagesList, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure PalmView.Empty(var Item:TItem);
begin
  Item.ID:=0;
  Item.Kind:=0;
  Item.Delay:=0;
  Item.Created:=0;
  Item.Modified:=0;
  SetLength(Item.Avatar,0);
  SetLength(Item.By,0);
  SetLength(Item.Title,0);
  SetLength(Item.Description,0);
  SetLength(Item.Glyphs,0);
end;

class procedure PalmView.Init(var Item:TItem);
begin
  Item.ID:=0;
  Item.Created:=0;
  Item.Modified:=0;
  Item.Kind:=0;
  Item.Delay:=5000;
  SetLength(Item.Avatar,0);
  SetLength(Item.By,0);
  SetLength(Item.Title,0);
  SetLength(Item.Description,0);
  SetLength(Item.Glyphs,0);
end;

class procedure PalmView.Done(var Item:TItem);
begin
  Finalize(Item.Avatar);
  Finalize(Item.By);
  Finalize(Item.Title);
  Finalize(Item.Description);
  Finalize(Item.Glyphs);
  Finalize(Item);
end;

class Function  PalmView.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Item),Item);
end;

class Function  PalmView.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Result:=False;
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.Created:=toDouble(xItem,XML.Fields.Created);
      Item.Modified:=toDouble(xItem,XML.Fields.Modified);
      Item.Kind:=toInteger(xItem,XML.Fields.Kind);
      Item.Delay:=toInteger(xItem,XML.Fields.Delay);
      Item.Avatar:=toString(xItem,XML.Fields.Avatar);
      Item.By:=toString(xItem,XML.Fields.By);
      Item.Title:=toString(xItem,XML.Fields.Title);
      Item.Description:=toString(xItem,XML.Fields.Description);
      Result:=True;
    end;
  end;
end;

class function PalmView.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;

  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(XML.Fields.Delay,Item.Delay),Output);
    Core.Streams.Write(Print(XML.Fields.Avatar,Item.Avatar),Output);
    Core.Streams.Write(Print(XML.Fields.By,Item.By),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Item.Title),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Item.Description),Output);
    Core.Streams.Write(Print(XML.Fields.Glyphs,Item.Glyphs,CDATA_OFF),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

initialization
  RegisterDB;
end.


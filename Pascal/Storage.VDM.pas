unit Storage.VDM;

{
  unit Storage.VDM.pas

  VDM Database Module

  DBMS facilities to handle VDM specific request

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

interface

uses
  Classes,

  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Core.Strings,
  Core.Streams,
  Core.XML,
  Core.Timer,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,


  Storage,
  Storage.Main,
  Storage.AuraDisks,
  Storage.CoreObjects,
  Storage.Domains,
  Storage.UserStorage,
  Storage.UserAccounts,
  Storage.Vendors,

  RSR,
  RSR.Core,

  MD5,
  XMLRead,
  DOM,
  SysUtils;

type
  VDM = class
  type
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ResourceID               : Core.Database.Types.Integer = 4;
        Modified                 : Core.Database.Types.Integer = 5;
        State                    : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        ResourceID               : Core.Database.Types.VarString = 'IRID';
        Modified                 : Core.Database.Types.VarString = 'ITMD';
        State                    : Core.Database.Types.VarString = 'ISTE';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'VDM';
        Value                    : 'scs_vdm';
        Hint                     : 'VDM general storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ResourceID; KeyP: @Keys.ResourceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.State; KeyP: @Keys.State; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PVDM=^TVDM;
    TVDM=record
      ID                       : QWord;
      Modified                 : Double;
      State                    : Core.Strings.VarString;
    end;
    class function Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; out ItemID:QWord):boolean;
    class function setState(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
    class function getState(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
  end;
  Resources=class
  type
    XML=class
    const
      Resources                  : Core.Database.Types.VarString = 'resources';
      Resource                   : Core.Database.Types.VarString = 'resource';
      ManifestID                 : Core.Database.Types.VarString = 'manifest-id';
      ID                         : Core.Database.Types.VarString = 'id';
      SyncID                     : Core.Database.Types.VarString = 'sync-id';
      Flags                      : Core.Database.Types.VarString = 'flags';
      Modified                   : Core.Database.Types.VarString = 'mtd';
      Name                       : Core.Database.Types.VarString = 'name';
      Description                : Core.Database.Types.VarString = 'description';
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ManifestID               : Core.Database.Types.Integer = 4;
        SyncID                   : Core.Database.Types.Integer = 5;
        Flags                    : Core.Database.Types.Integer = 6;
        Modified                 : Core.Database.Types.Integer = 7;
        PartialResponses         : Core.Database.Types.Integer = 8;
        Name                     : Core.Database.Types.Integer = 9;
        Description              : Core.Database.Types.Integer = 10;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        ManifestID               : Core.Database.Types.VarString = 'IMID';
        SyncID                   : Core.Database.Types.VarString = 'ISID';
        Flags                    : Core.Database.Types.VarString = 'IFGS';
        Modified                 : Core.Database.Types.VarString = 'IMOD';
        PartialResponses         : Core.Database.Types.VarString = 'IRSP';
        Name                     : Core.Database.Types.VarString = 'INME';
        Description              : Core.Database.Types.VarString = 'IDSC';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/VDM';
        Name                     : 'Resources';
        Value                    : 'scs_vdm_rcs';
        Hint                     : 'VDM resource names';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..9] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ManifestID; KeyP: @Keys.ManifestID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.SyncID; KeyP: @Keys.SyncID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Flags; KeyP: @Keys.Flags; DataType: dftWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );
    end;
    Flags=class
    const
      SaveSession              = 1 shl 0;
      SyncDownload             = 1 shl 1;
      SyncUpload               = 1 shl 2;
      PartialResponses         = 1 shl 3;
    end;
    PResource=^TResource;
    PResources=^TResources;
    TResources=Array of PResource;
    TResource=record
      ID                       : QWord;
      ManifestID               : QWord;
      SyncID                   : QWord;
      Flags                    : Word;
      Modified                 : double;
      Name                     : Core.Strings.VarString;
      Description              : Core.Strings.VarString;
    end;
    TResourcesEvent=procedure(var Items:TResources) of Object;
    TResourceEvent=procedure(Var Item:TResource) of Object;

    class function  Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Name:Core.Strings.VarString; out ItemID:QWord):boolean;
    class function  setName(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
    class function  setManifestID(Task:Core.Database.Types.TTask; ItemID,ManifestID:QWord):boolean;
    class function  getName(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
    class function  Exists(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
    class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; out Items:TResources):boolean;
    class function  getID(Item:PResource):QWord;

    class procedure Empty(var Item:TResources); overload;
    class procedure Empty(Var Item:TResource); overload;
    class procedure Done(Var Item:TResources); overload;
    class procedure Done(Var Item:TResource); overload;
    class procedure Init(var Item:TResource); overload;
    class procedure Init(var Item:TResources); overload;
    class procedure Copy(var Source,Destination:TResource);


    class function  getItem(ID:QWord; Var Items:TResources):PResource;

    class function  getDirection(var Item:TResource; var sDownload,sUpload:Core.Strings.VarString):Core.Strings.VarString;
    class function  Add(Var Item:TResource; var Items:TResources):PResource;
    class function  Add(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Item:TResource): Boolean;
    class function  Delete(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord): Boolean;
    class function  Delete(var Item:TResource; var Items:TResources): LongInt;
    class function  Read(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Item:TResource): Boolean;
    class function  Write(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Item:TResource): Boolean; overload;


    class function  toXML(var Item:TResource; Output:TMemoryStream; Header:boolean):boolean; overload;
    class function  toXML(var Items:TResources; Output:TMemoryStream; Header:boolean):boolean; overload;

    class function  fromXML(xDoc:TXMLDocument; var Items:TResources):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TResource):boolean; overload;
    class function  fromXML(xItem:TDOMNode; var Item:TResource):boolean; overload;
    class function  fromXML(xItems:TDOMNode; var Items:TResources):boolean; overload;

  end;
  Manifest=class
  type
    XML=class
    type
      Stanzas=class
      const
        Manifest                 : Core.Database.Types.VarString = 'manifest';
        Item                     : Core.Database.Types.VarString = 'item';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        Modified                 : Core.Database.Types.VarString = 'modified';
        Data                     : Core.Database.Types.VarString = 'data';
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
        ResourceID               : Core.Database.Types.Integer = 4;
        Modified                 : Core.Database.Types.Integer = 5;
        Data                     : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        ResourceID               : Core.Database.Types.VarString = 'IRCD';
        Modified                 : Core.Database.Types.VarString = 'IMOD';
        Data                     : Core.Database.Types.VarString = 'IDAT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/VDM';
        Name                     : 'Manifest';
        Value                    : 'scs_vdm_mfst';
        Hint                     : 'VDM Manifest Data associated with devices';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ResourceID; KeyP: @Keys.ResourceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PItem=^TItem;
    TItem=record
      ID                       : QWord;
      Modified                 : double;
      Data                     : Core.Strings.VarString;
    end;
    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
    class function  toXML(var Item:TItem; Output:TMemoryStream):boolean;
    class function  Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; Var Item:TItem):boolean; overload;
    class function  Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; out ItemID:QWord):boolean; overload;

    class function  Write(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; var Item:TItem):boolean;
    class function  Read(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; var Item:TItem):boolean;
    class procedure Empty(var Item:TItem); overload;
    class procedure Done(Var Item:TItem); overload;
    class procedure Init(var Item:TItem); overload;
  end;
  Groups=class
  type
    XML=class
    type
      Stanzas=class
      const
        Groups                   : Core.Database.Types.VarString = 'groups';
        Group                    : Core.Database.Types.VarString = 'group';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        Modified                 : Core.Database.Types.VarString = 'modified';
        System                   : Core.Database.Types.VarString = 'system';
        Name                     : Core.Database.Types.VarString = 'name';
        Description              : Core.Database.Types.VarString = 'description';
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
        Modified                 : Core.Database.Types.Integer = 4;
        System                   : Core.Database.Types.Integer = 5;
        Name                     : Core.Database.Types.Integer = 6;
        Description              : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        Modified                 : Core.Database.Types.VarString = 'IMOD';
        System                   : Core.Database.Types.VarString = 'ISYS';
        Name                     : Core.Database.Types.VarString = 'NAME';
        Description              : Core.Database.Types.VarString = 'DSCP';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/VDM';
        Name                     : 'Groups';
        Value                    : 'scs_vdm_grps';
        Hint                     : 'Application Group names';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.System; KeyP: @Keys.System; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );
    end;
    PItem=^TItem;
    PItems=^TItems;
    TItems=Array of PItem;
    TItem=record
      ID                       : QWord;
      Modified                 : double;
      System                   : boolean;
      Name                     : Core.Strings.VarString;
      Description              : Core.Strings.VarString;
    end;
    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
    class function  toXML(var List:TItems; Output:TMemoryStream):boolean; overload;
    class function  toXML(var Item:TItem; Output:TMemoryStream):boolean; overload;
    class function  Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Item:TItem):boolean;

    class function  Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
    class function  Read(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
    class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Items:TItems):boolean;
    class function  Delete(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;

    class function  Verify(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;

    class procedure Empty(var Item:TItems); overload;
    class procedure Empty(Var Item:TItem); overload;
    class procedure Done(Var Item:TItems); overload;
    class procedure Done(Var Item:TItem); overload;
    class procedure Init(var Item:TItem); overload;
    class procedure Init(var Item:TItems); overload;
  end;



implementation
uses
  DB,
  sqldb;

procedure cbDestroyVDM(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With VDM.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyResources(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Resources.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyGroups(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Groups.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyManifest(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Manifest.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = VDM.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, VDM.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, VDM.DB.TableP, useForCriteria, VDM.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Resources.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Resources.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Resources.DB.TableP, useForCriteria, Resources.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Groups.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Groups.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Groups.DB.TableP, useForCriteria, Groups.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Manifest.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Manifest.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Manifest.DB.TableP, useForCriteria, Manifest.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = VDM.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, VDM.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, VDM.DB.TableP, useForCriteria, VDM.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Resources.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Resources.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Resources.DB.TableP, useForCriteria, Resources.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Groups.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Groups.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Groups.DB.TableP, useForCriteria, Groups.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Manifest.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Manifest.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Manifest.DB.TableP, useForCriteria, Manifest.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushDeviceDeleted();
  begin
    if ItemP = VDM.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, VDM.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, VDM.DB.TableP, useForCriteria, VDM.DB.IDs.ResourceID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Manifest.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Manifest.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Manifest.DB.TableP, useForCriteria, Manifest.DB.IDs.ResourceID, poNone, oEqual, ItemID, Commands);
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
    Core.Database.Monitor.Notify.USER_DEVICE_DELETED : PushDeviceDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with VDM.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyVDM, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Resources.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyResources, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Groups.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyGroups, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Manifest.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyManifest, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class function VDM.Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; out ItemID:QWord):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; ItemID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,VDM.DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ResourceID),poNone,oNone,ResourceID,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function VDM.setState(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.State, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetState(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PString(DataP)^:=Fields.FieldByName(VDM.DB.Keys.State).AsString;
end;

class function VDM.getState(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Value, 0);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.State,poNone,oNone,Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbGetState, @Value) and (Length(Value)>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Resources.toXML(var Item:TResource; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Resource,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.ManifestID,Item.ManifestID),Output);
    Core.Streams.Write(Print(XML.SyncID,Item.SyncID),Output);
    Core.Streams.Write(Print(XML.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Flags,Item.Flags),Output);
    Core.Streams.Write(Print(XML.Name,Item.Name,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Description,Item.Description,CDATA_ON),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Resource,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Resources.toXML(var Items:TResources; Output:TMemoryStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Resources,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to High(Items) do
    toXML(Items[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Resources,Output);
  Core.Streams.Write('>',1,Output);
  Result:=true;
end;

class function  Resources.fromXML(xItem:TDOMNode; var Item:TResource):boolean;
begin
  Empty(Item);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.Flags:=toByte(xItem,XML.Flags);
      Item.ID:=toQWord(xItem,XML.ID);
      Item.ManifestID:=toQWord(xItem,XML.ManifestID);
      Item.SyncID:=toQWord(xItem,XML.SyncID);
      Item.Modified:=toDouble(xItem,XML.Modified);
      Item.Name:=toString(xItem,XML.Name);
      Item.Description:=toString(xItem,XML.Description);
      Result:=True;

    end;
  end else
    Result:=false;
end;

class function  Resources.fromXML(xDoc:TXMLDocument; var Item:TResource):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Resource),Item);
end;

class function  Resources.fromXML(xDoc:TXMLDocument; var Items:TResources):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Resources),Items);
end;

class function  Resources.fromXML(xItems:TDOMNode; var Items:TResources):boolean;
var
  xItem:TDOMNode;
  itmP:PResource;
  iCt:LongInt;
  iLcv:LongInt;
begin
  Result:=False;
  Empty(Items);
  iCt:=0;
  if (xItems<>nil) then begin
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      xItem:=xItems.ChildNodes[iLcv];
      if Core.Strings.SameText(xItem.NodeName,XML.Resource) then begin
        New(itmP);
        Init(itmP^);
        SetLength(Items,iCt+1);
        Items[iCt]:=itmP;
        fromXML(xItem,itmP^);
        inc(iCt);
      end;
    end;
  end;
end;


class function Resources.Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Name:Core.Strings.VarString; out ItemID:QWord):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
  ManifestID:QWord;

begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; ItemID:=0; iInsertID:=Random(High(Integer)); ManifestID:=0;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,Core.Timer.dtUT,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Name),poNone,oNone,Name,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    if (Result and (ItemID<>0)) then begin
      Manifest.Add(Task,DomainID,UserID,ItemID,ManifestID);
      Resources.setManifestID(Task,ItemID,ManifestID);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Resources.setName(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Name, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Resources.setManifestID(Task:Core.Database.Types.TTask; ItemID,ManifestID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.ManifestID, poNone, oNone, ManifestID, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetName(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PString(DataP)^:=Fields.FieldByName(Resources.DB.Keys.Name).AsString;
end;

class function Resources.getName(Task:Core.Database.Types.TTask; ItemID:QWord; var Value:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Value,0);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Name,poNone,oNone,Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbGetName, @Value) and (Length(Value)>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListResources(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Resources.PResources;
  ItemP:Resources.PResource;
  iIndex:LongInt;
begin
  New(ItemP);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItemP;

  with ItemP^ do begin
    ID        := Fields.FieldByName(Resources.DB.Keys.ID).AsLargeInt;
    ManifestID:= Fields.FieldByName(Resources.DB.Keys.ManifestID).AsLargeInt;
    SyncID    := Fields.FieldByName(Resources.DB.Keys.SyncID).AsLargeInt;
    Flags     := Fields.FieldByName(Resources.DB.Keys.Flags).AsInteger;
    Modified  := Fields.FieldByName(Resources.DB.Keys.Modified).AsFloat;
    Name      := Fields.FieldByName(Resources.DB.Keys.Name).AsString;
    Description := Fields.FieldByName(Resources.DB.Keys.Description).AsString;
  end;
end;

class function Resources.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; out Items:TResources):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Items);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ManifestID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.SyncID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Flags,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Name,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Description,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task, @Commands, @cbListResources, @Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Resources.Exists(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Count:QWord;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Result := Core.Database.SQL.Count(Task, @Commands,Count) and (Count>0);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Resources.Empty(var Item:TResources);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Resources.Init(var Item:TResource);
begin
  Item.ID:=0;
  Item.Modified:=0;
  Item.ManifestID:=0;
  Item.SyncID:=0;
  Item.Flags:=0;
  SetLength(Item.Description,0);
  SetLength(Item.Name,0);
end;

class procedure Resources.Init(var Item:TResources);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Resources.Copy(var Source,Destination:TResource);
begin
  Destination.ID:=Source.ID;
  Destination.ManifestID:=Source.ManifestID;
  Destination.SyncID:=Source.SyncID;
  Destination.Flags:=Source.Flags;
  Destination.Modified:=Source.Modified;
  Destination.Name:=Source.Name;
  Destination.Description:=Source.Description;
end;

class procedure Resources.Empty(Var Item:TResource);
begin
   Item.ID:=0;
   Item.Modified:=0;
   Item.ManifestID:=0;
   Item.SyncID:=0;
   Item.Flags:=0;
   SetLength(Item.Name,0);
   SetLength(Item.Description,0);
end;

class procedure Resources.Done(Var Item:TResources);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class function  Resources.getID(Item:PResource):QWord;
begin
  Result:=0;
  if Item<>Nil then
    Result:=Item^.ID;
end;


class procedure Resources.Done(Var Item:TResource);
begin
  Finalize(Item.Name);
  Finalize(Item.Description);
  Finalize(Item);
end;


class function  Resources.getItem(ID:QWord; Var Items:TResources):PResource;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]^.ID=ID then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

class function  Resources.getDirection(var Item:TResource; var sDownload,sUpload:Core.Strings.VarString):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  SetLength(Result,0);
  if Item.Flags or Resources.Flags.SyncDownload=Item.Flags then
    Result:=Concat(Result,sDownload,',');
  if Item.Flags or Resources.Flags.SyncUpload = Item.Flags then
    Result:=Concat(Result,sUpload,',');
  iLen:=Length(Result);
  if iLen>0 then SetLength(Result,iLen-1);
end;

class function  Resources.Add(Var Item:TResource; var Items:TResources):PResource;
var
  iCt:LongInt;
  itmP:PResource;
begin
  new(itmP);
  Init(itmP^);
  Copy(Item,itmP^);
  iCt:=System.Length(Items);
  SetLength(Items,iCt+1);
  Items[iCt]:=itmP;
  Result:=itmP;
end;

class function  Resources.Add(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Item:TResource): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  ItemID                         : QWord;
  ManifestID                     : QWord;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);

    // Setup Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Flags,poNone,oNone,Item.Flags,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Name,poNone,oNone,Item.Name,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Description,poNone,oNone,Item.Description,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    ItemID:=Item.ID; ManifestID:=0;
    if (Result and (Item.ID<>0)) then begin
      Manifest.Add(Task,DomainID,UserID,ItemID,ManifestID);
      Resources.setManifestID(Task,ItemID,ManifestID);
    end;
    Item.ManifestID:=ManifestID;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Resources.Delete(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,ItemID,Commands);

    Result:=Core.Database.SQL.Delete(Task,@Commands);
    if Result then
      Core.Database.Monitor.Cascade(Task,DB.TableP,ItemID,Core.Database.Monitor.Notify.USER_DEVICE_DELETED);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class function  Resources.Delete(Var Item:TResource; var Items:TResources): LongInt;
var
  iCt:LongInt;
  jLcv,iLcv:LongInt;
begin
  Result:=-1;
  iCt:=System.Length(Items);
  for iLcv:=0 to iCt-1 do begin
    if Items[iLcv]^.ID=Item.ID then begin
      Result:=iLcv;
      for jLcv:=iLcv to iCt-2 do
        Items[jLcv]:=Items[jLcv+1];
      SetLength(Items,iCt-1);
      Break;
    end;
  end;
end;

procedure cb_Resource_Read(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmP:Resources.PResource;
begin
  itmP:=DataP;
  {$i Storage.VDM.Resources.Item.Read.inc};
end;

procedure cb_Storage_Resource_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP:Resources.PResources;
  itmP:Resources.PResource;
  iCt:LongInt;
begin
  ListP:=DataP;
  new(itmP);
  iCt:=System.Length(ListP^);
  SetLength(ListP^,iCt+1);
  ListP^[iCt]:=itmP;
  Resources.Init(itmP^);
  {$i Storage.VDM.Resources.Item.Read.inc};
end;

class function  Resources.Read(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Item:TResource): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.ManifestID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.SyncID,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Name,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForFields,DB.IDs.Description,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Resource_Read,@Item);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Resources.Write(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Item:TResource): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Flags,poNone,oNone,Item.Flags,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Name,poNone,oNone,Item.Name,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Description,poNone,oNone,Item.Description,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class procedure Manifest.Empty(Var Item:TItem);
begin
   Item.ID:=0;
   Item.Modified:=0;
   SetLength(Item.Data,0);
end;



class procedure Manifest.Init(Var Item:TItem);
begin
   Item.ID:=0;
   Item.Modified:=0;
   SetLength(Item.Data,0);
end;

class procedure Manifest.Done(Var Item:TItem);
begin
   Finalize(Item.Data);
   Finalize(Item);
end;

class function  Manifest.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
var
  xMfst,xData:TDOMNode;
begin
  Result:=False;
  Empty(Item);
  with Core.XML.DB do begin
    xMfst:=getNode(xDoc,XML.Stanzas.Manifest);
    if xMfst<>nil then begin
      Item.ID:=toQWord(xMfst,XML.Fields.ID);
      Item.Modified:=toDouble(xMfst,XML.Fields.Modified);
      xData:=Core.XML.DB.getChildNode(xMfst,XML.Fields.Data);
      if xData<>nil then begin
        Item.Data:=getNodeText(xData);
        Result:=True;
      end;
    end else
      Result:=false;
  end;
end;

class function  Manifest.toXML(var Item:TItem; Output:TMemoryStream):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Manifest,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Data,Item.Data,CDATA_OFF),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Manifest,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Manifest.Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; Var Item:TItem):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Item.Modified:=Core.Timer.dtUT;
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ResourceID),poNone,oNone,ResourceID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,Item.Modified,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Manifest.Add(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; out ItemID:QWord):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; ItemID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ResourceID),poNone,oNone,ResourceID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,Core.Timer.dtUT,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Manifest.Write(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ResourceID, poAnd, oEqual, ResourceID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Data, poNone, oNone, Item.Data, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetManifest(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Manifest.PItem(DataP)^.ID:=Fields.FieldByName(Manifest.DB.Keys.ID).AsLargeInt;
  Manifest.PItem(DataP)^.Modified:=Fields.FieldByName(Manifest.DB.Keys.Modified).AsFloat;
  Manifest.PItem(DataP)^.Data:=Fields.FieldByName(Manifest.DB.Keys.Data).AsString;
end;

class function  Manifest.Read(Task:Core.Database.Types.TTask; DomainID,UserID,ResourceID:QWord; var Item:TItem):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ResourceID, poAnd, oEqual, ResourceID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Data,poNone,oNone,Commands);
    Result := Core.Database.SQL.Select(Task, @Commands, @cbGetManifest, @Item) and (Item.ID<>0);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Groups.Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Item:TItem):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Item.Modified:=Core.Timer.dtUT;
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.System),poNone,oNone,Item.System,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Name),poNone,oNone,Item.Name,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Description),poNone,oNone,Item.Description,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Groups.Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.System, poNone, oNone, Item.System, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Name, poNone, oNone, Item.Name, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Description, poNone, oNone, Item.Description, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetGroup(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  {$i Storage.VDM.Groups.Fields.Get.inc}
end;

class function Groups.Read(Task:Core.Database.Types.TTask;  DomainID,UserID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Item.Name,0);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    {$i Storage.VDM.Groups.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbGetGroup, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbGetVerify(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Groups.PItem(DataP)^.ID:=Fields.FieldByName(Groups.DB.Keys.ID).AsLargeInt;
  {$i Storage.VDM.Groups.Fields.Get.inc}
end;

class function  Groups.Verify(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    if Item.ID=0 then begin
      iCount := 0;
      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Name, poAnd, oEqual, Item.Name, Commands);

      Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID,poNone,oNone,Commands);
      {$i Storage.VDM.Groups.Read.Fields.inc}

      Core.Database.SQL.Select(Task, @Commands, @cbGetVerify, @Item);
      if (Item.ID=0) then
        Result:=Groups.Add(Task,DomainID,UserID,Item);
    end else
      Result:=true;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListGroups(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Groups.PItems;
  ItemP:Groups.PItem;
  iIndex:LongInt;
begin
  New(ItemP);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItemP;

  with ItemP^ do begin
    ID        := Fields.FieldByName(Groups.DB.Keys.ID).AsLargeInt;
    Modified  := Fields.FieldByName(Groups.DB.Keys.Modified).AsFloat;
    System    := Fields.FieldByName(Groups.DB.Keys.System).AsBoolean;
    Name      := Fields.FieldByName(Groups.DB.Keys.Name).AsString;
    Description := Fields.FieldByName(Groups.DB.Keys.Description).AsString;
  end;
end;

class function Groups.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Items:TItems):boolean;
var
  iCount:LongInt;
  iDefault:QWord;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; iDefault:=0;
    Empty(Items);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForCriteriaBracket, PROPERTY_ID_VOID, poNone, oOpenBracket, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, iDefault, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, iDefault, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteriaBracket, PROPERTY_ID_VOID, poNone, oCloseBracket,Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForCriteriaBracket, PROPERTY_ID_VOID, poOr, oOpenBracket, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteriaBracket, PROPERTY_ID_VOID, poNone, oCloseBracket, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.System,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Name,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Description,poNone,oNone,Commands);
    Result := Core.Database.SQL.Select(Task, @Commands, @cbListGroups, @Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Groups.Delete(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Groups.Empty(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Groups.Empty(Var Item:TItem);
begin
  Item.ID:=0;
  Item.Modified:=0.0;
  Item.System:=false;
  SetLength(Item.Name,0);
  SetLength(Item.Description,0);
end;

class procedure Groups.Done(Var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Groups.Done(Var Item:TItem);
begin
  Finalize(Item.Name);
  Finalize(Item.Description);
  Finalize(Item);
end;

class procedure Groups.Init(var Item:TItem);
begin
  Item.ID:=0;
  Item.Modified:=0.0;
  SetLength(Item.Name,0);
  SetLength(Item.Description,0);
end;

class procedure Groups.Init(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class function  Groups.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Group);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.Modified:=toDouble(xItem,XML.Fields.Modified);
      Item.Name:=toString(xItem,XML.Fields.Name);
      Item.Description:=toString(xItem,XML.Fields.Description);
      Result:=True;
    end;
  end;
end;

class Function  Groups.toXML(var List:TItems; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Groups,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(List) do
    toXML(List[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Groups,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class Function  Groups.toXML(var Item:TItem; Output:TMemoryStream):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Group,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Name,Item.Name),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Item.Description),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Group,Output);
  Core.Streams.Write('>',1,Output);
end;

initialization
  RegisterDB;
end.


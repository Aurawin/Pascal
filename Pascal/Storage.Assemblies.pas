unit Storage.Assemblies;
{
  unit Storage.Assemblies.pas

  Assemblies Database Module

  DBMS facilities to handle Assemblies for Spectrum

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
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  RSR.Core,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.Projects,
  Storage.Tasks,
  Storage.CoreObjects,

  Core.Timer,
  Core.Strings,
  Core.Streams,
  Core.XML,

  DOM,
  XMLRead,
  SysUtils;

type
  Items = class
  type
    XML=class
    type
      Stanzas=class
      const
        Assemblies               : Core.Database.Types.VarString = 'assemblies';
        Assembly                 : Core.Database.Types.VarString = 'assembly';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        ParentID                 : Core.Database.Types.VarString = 'pid';
        Created                  : Core.Database.Types.VarString = 'created';
        Modified                 : Core.Database.Types.VarString = 'modified';
        Scheduled                : Core.Database.Types.VarString = 'scheduled';
        State                    : Core.Database.Types.VarString = 'state';
        Privacy                  : Core.Database.Types.VarString = 'privacy';
        Admins                   : Core.Database.Types.VarString = 'admins';
        Files                    : Core.Database.Types.VarString = 'files';
        Comments                 : Core.Database.Types.VarString = 'comments';
        Assemblies               : Core.Database.Types.VarString = 'assemblies';
        Title                    : Core.Database.Types.VarString = 'title';
        Description              : Core.Database.Types.VarString = 'description';
        Location                 : Core.Database.Types.VarString = 'location';
      end;
    end;
    Privacy = class
    const
      Unknown                  = -1;
      Open                     = 0;
      Closed                   = 1;
      Subscription             = 2;
    end;
    State = class
    const
      Unknown                  = -1;
      Scheduled                = 0;
      Canceled                 = 1;
      Postponed                = 2;
      Running                  = 3;
      Suspended                = 4;
    end;
    Defaults = class
    const
      State                    = State.Unknown;
      Privacy                  = Privacy.Open;
      ParentID                 = 0;
    end;
    Item=record
      ID                       : QWord;
      ParentID                 : QWord;
      Verified                 : Boolean; // run-time for refresh
      Created                  : double;
      Modified                 : double;
      Scheduled                : double;
      State                    : LongInt;
      Privacy                  : LongInt;
      Admins                   : Core.Arrays.Types.LargeWord;
      Files                    : Core.Arrays.Types.LargeWord;
      Comments                 : Core.Arrays.Types.LargeWord;
      Assemblies               : Core.Arrays.Types.LargeWord;
      Title                    : Core.Strings.VarString;
      Description              : Core.Strings.VarString;
      Location                 : Core.Strings.VarString;
    end;
    PItem=^Item;
    List=Array of PItem;
    PList=^List;
    DB=class
    Type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ParentID                 : Core.Database.Types.Integer = 4;
        Created                  : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        Scheduled                : Core.Database.Types.Integer = 7;
        State                    : Core.Database.Types.Integer = 8;
        Privacy                  : Core.Database.Types.Integer = 9;
        Admins                   : Core.Database.Types.Integer = 10;
        Files                    : Core.Database.Types.Integer = 11;
        Comments                 : Core.Database.Types.Integer = 12;
        Assemblies               : Core.Database.Types.Integer = 13;
        Title                    : Core.Database.Types.Integer = 14;
        Description              : Core.Database.Types.Integer = 15;
        Location                 : Core.Database.Types.Integer = 16;
      end;
      Keys = class
      const
        ID                       : Core.Database.Types.VarString = 'TID';
        InsertID                 : Core.Database.Types.VarString = 'TIID';
        DomainID                 : Core.Database.Types.VarString = 'TDID';
        UserID                   : Core.Database.Types.VarString = 'TUID';
        ParentID                 : Core.Database.Types.VarString = 'PRID';
        Created                  : Core.Database.Types.VarString = 'TCTD';
        Modified                 : Core.Database.Types.VarString = 'MDFD';
        Scheduled                : Core.Database.Types.VarString = 'SCHED';
        State                    : Core.Database.Types.VarString = 'STATE';
        Privacy                  : Core.Database.Types.VarString = 'PRIV';
        Admins                   : Core.Database.Types.VarString = 'TADS';
        Files                    : Core.Database.Types.VarString = 'FLS';
        Comments                 : Core.Database.Types.VarString = 'CMTS';
        Assemblies               : Core.Database.Types.VarString = 'ASSMS';
        Title                    : Core.Database.Types.VarString = 'TIT';
        Description              : Core.Database.Types.VarString = 'DSCT';
        Location                 : Core.Database.Types.VarString = 'ALOC';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Assemblies';
        Value                    : 'scs_asm';
        Hint                     : 'Assembly storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..16] of Core.Database.Types.Field= (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ParentID; KeyP: @Keys.ParentID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Scheduled; KeyP: @Keys.Scheduled; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.State; KeyP: @Keys.State; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Privacy; KeyP: @Keys.Privacy; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Admins; KeyP: @Keys.Admins; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Files; KeyP: @Keys.Files; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Comments; KeyP: @Keys.Comments;  DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
        (IDP: @IDs.Assemblies; KeyP: @Keys.Assemblies; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Location; KeyP: @Keys.Location; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );

      class function Granted(Task:Core.Database.Types.TTask; UserID,ItemID:QWord):boolean;
      class function Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
      class function Read(Task:Core.Database.Types.TTask; var Entry:Item):boolean;
      class function Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
      class function Refresh(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;

      class function getAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
      class function setAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;

      class function getFiles(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
      class function setFiles(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;

      class function  Find(Task:Core.Database.Types.TTask; DomainID:QWord; Term:Core.Strings.VarString; var Entries:List):boolean;

      class function getComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
      class function setComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;

      class function getAssemblies(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
      class function setAssemblies(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;

      class function getState(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;
      class function setState(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;

      class function getPrivacy(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;
      class function setPrivacy(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;

      class function getScheduled(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Double): boolean;
      class function setScheduled(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Double): boolean;
    end;
    class procedure Init(var Entry:Item);
    class procedure Empty(Var Entry:Item);
    class procedure Done(Var Entry:Item);

    class procedure setVerified(var Entry:Item; Value:boolean);

    class Function  fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
    class Function  toXML(var Entry:Item; Output:TMemoryStream):boolean;

    class procedure Init(var Entries:List);
    class procedure Empty(Var Entries:List);
    class procedure Done(Var Entries:List);

    class Function  toXML(var Entries:List; Output:TMemoryStream):boolean;
  end;
  Subscription=class
  Type
    XML=class
    type
      Stanzas=class
      const
        Subscriptions          = 'subscriptions';
        Subscription           = 'subscription';
      end;
      Fields=class
      const
        ID                     = 'id';
        AssemblyID             = 'aid';
        ProjectID              = 'pid';
        TaskID                 = 'tid';
        Created                = 'created';
        Modified               = 'modified';
        Scheduled              = 'scheduled';
        Alerts                 = 'alerts';
        Status                 = 'status';
      end;
    end;
    Alerts=class
    const
      None                     = 0;
      OnJoin                   = 1;
      OnLeave                  = 1 shl 1;
      OnGather                 = 1 shl 2;
    end;
    Status=class
    const
      None                     = 0;
      Pending                  = 1;
      Confirmed                = 1 shl 1;
      Deferred                 = 1 shl 2;
      Rejected                 = 1 shl 3;
    end;
    Defaults = class
    const
      Status                   = Status.None;
      AssemblyID               = 0;
      ProjectID                = 0;
      TaskID                   = 0;
      Alerts                   = Alerts.OnJoin + Alerts.OnLeave + Alerts.OnGather;
    end;
    Item=record
      ID                       : QWord;
      DomainID                 : QWord;
      UserID                   : QWord;
      AssemblyID               : QWord;
      ProjectID                : QWord;
      TaskID                   : QWord;
      Verified                 : Boolean; // run-time for refresh
      Created                  : double;
      Modified                 : double;
      Alerts                   : LongInt;
      Status                   : LongInt;
    end;
    PItem=^Item;
    List=Array of PItem;
    PList=^List;
    DB=class
    Type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        AssemblyID               : Core.Database.Types.Integer = 4;
        ProjectID                : Core.Database.Types.Integer = 5;
        TaskID                   : Core.Database.Types.Integer = 6;
        Created                  : Core.Database.Types.Integer = 7;
        Modified                 : Core.Database.Types.Integer = 8;
        Alerts                   : Core.Database.Types.Integer = 9;
        Status                   : Core.Database.Types.Integer = 10;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'IMIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        AssemblyID               : Core.Database.Types.VarString = 'IAID';
        ProjectID                : Core.Database.Types.VarString = 'IPID';
        TaskID                   : Core.Database.Types.VarString = 'ITID';
        Created                  : Core.Database.Types.VarString = 'ICTD';
        Modified                 : Core.Database.Types.VarString = 'IMDT';
        Alerts                   : Core.Database.Types.VarString = 'ALRT';
        Status                   : Core.Database.Types.VarString = 'STAT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Assemblies';
        Name                     : 'Subscriptions';
        Value                    : 'scs_asm_srx';
        Hint                     : 'Assembly subscription storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..10] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.AssemblyID; KeyP: @Keys.AssemblyID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ProjectID; KeyP: @Keys.ProjectID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.TaskID; KeyP: @Keys.TaskID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Alerts; KeyP: @Keys.Alerts; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Status; KeyP: @Keys.Status; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class function List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entries:List):boolean;
      class function Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
      class function Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
      class function Read(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
      class function Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
      class function Refresh(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):Boolean;
    end;
    class function IndexOf(var Entries:List; ID:QWord): LongInt;
    class procedure Init(var Entry:Item); overload;
    class procedure Init(var Entries:List); overload;
    class procedure Empty(Var Entry:Item);  overload;
    class procedure Empty(Var Entries:List); overload;
    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:List); overload;

    class Function  fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;

    class Function  toXML(var Entry:Item; Output:TMemoryStream):boolean; overload;
    class Function  toXML(var Entries:List; Output:TMemoryStream):boolean; overload;

    class procedure setVerified(var Entry:Item; Value:boolean);
  end;
  Templates= class
  type
    XML=class
    type
      Stanzas=class
      const
        Template                 : Core.Database.Types.VarString = 'template';
      type
        Fields=class
        const
          ID                     : Core.Database.Types.VarString = 'id';
          Created                : Core.Database.Types.VarString = 'created';
          Modified               : Core.Database.Types.VarString = 'modified';
          Privacy                : Core.Database.Types.VarString = 'privacy';
          Title                  : Core.Database.Types.VarString = 'title';
          Description            : Core.Database.Types.VarString = 'description';
          Data                   : Core.Database.Types.VarString = 'data';
        end;
      end;
    end;
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        Created                  : Core.Database.Types.Integer = 4;
        Modified                 : Core.Database.Types.Integer = 5;
        Privacy                  : Core.Database.Types.Integer = 6;
        Title                    : Core.Database.Types.Integer = 7;
        Description              : Core.Database.Types.Integer = 8;
        Data                     : Core.Database.Types.Integer = 9;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        Created                  : Core.Database.Types.VarString = 'IDTC';
        Modified                 : Core.Database.Types.VarString = 'IDTM';
        Privacy                  : Core.Database.Types.VarString = 'IPRIV';
        Title                    : Core.Database.Types.VarString = 'TITL';
        Description              : Core.Database.Types.VarString = 'DESCN';
        Data                     : Core.Database.Types.VarString = 'IDAT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Assemblies';
        Name                     : 'Templates';
        Value                    : 'scs_asm_tpl';
        Hint                     : 'Assembly template storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : array [0..9] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Privacy; KeyP: @Keys.Privacy; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
    end;
  end;


implementation

uses DB;

procedure cbDestroyItemsTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyTemplatesTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Templates.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroySubscriptionTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Subscription.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Templates.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Templates.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Templates.DB.TableP, useForCriteria, Templates.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Items.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Subscription.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Subscription.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Subscription.DB.TableP, useForCriteria, Subscription.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Templates.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Templates.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Templates.DB.TableP, useForCriteria, Templates.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Items.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Subscription.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Subscription.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Subscription.DB.TableP, useForCriteria, Subscription.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted();
  end;
end;

procedure RegisterDB;
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
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyItemsTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Templates.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyTemplatesTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Subscription.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroySubscriptionTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Init(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    ParentID:=Defaults.ParentID;
    Verified:=false;
    Created:=0;
    Modified:=0;
    Scheduled:=0;
    State:=Defaults.State;
    Privacy:=Defaults.Privacy;
    Core.Arrays.LargeWord.Init(Admins);
    Core.Arrays.LargeWord.Init(Files);
    Core.Arrays.LargeWord.Init(Comments);
    Core.Arrays.LargeWord.Init(Assemblies);
    SetLength(Title,0);
    SetLength(Description,0);
    SetLength(Location,0);
  end;
end;

class procedure Items.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    ParentID:=Defaults.ParentID;
    Verified:=false;
    Created:=0;
    Modified:=0;
    Scheduled:=0;
    State:=Defaults.State;
    Privacy:=Defaults.Privacy;
    Core.Arrays.LargeWord.Empty(Admins);
    Core.Arrays.LargeWord.Empty(Files);
    Core.Arrays.LargeWord.Empty(Comments);
    Core.Arrays.LargeWord.Empty(Assemblies);
    SetLength(Title,0);
    SetLength(Description,0);
    SetLength(Location,0);
  end;
end;


class procedure Items.Done(Var Entry:Item);
begin
  With Entry do begin
    Core.Arrays.LargeWord.Done(Admins);
    Core.Arrays.LargeWord.Done(Files);
    Core.Arrays.LargeWord.Done(Comments);
    Core.Arrays.LargeWord.Done(Assemblies);
    Finalize(Title);
    Finalize(Description);
    Finalize(Location);
  end;
  Finalize(Entry);
end;

class procedure Items.setVerified(var Entry:Item; Value:boolean);
begin
  Entry.Verified:=Value;
end;

class function  Items.fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  with Core.XML.DB do begin
    xItem:=getNode(xDoc,XML.Stanzas.Assembly);
    if xItem<>nil then begin
      Entry.ID:=toQWord(xItem,XML.Fields.ID);
      Entry.ParentID:=toQWord(xItem,XML.Fields.ParentID);
      Entry.Created:=toDouble(xItem,XML.Fields.Created);
      Entry.Modified:=toDouble(xItem,XML.Fields.Modified);
      Entry.Scheduled:=toDouble(xItem,XML.Fields.Scheduled);
      Entry.State:=toInteger(xItem,XML.Fields.State);
      Entry.Privacy:=toInteger(xItem,XML.Fields.Privacy);
      toQwordArray(xItem,XML.Fields.Admins,Entry.Admins);
      toQwordArray(xItem,XML.Fields.Files,Entry.Files);
      toQwordArray(xItem,XML.Fields.Comments,Entry.Comments);
      toQwordArray(xItem,XML.Fields.Assemblies,Entry.Assemblies);
      Entry.Title:=toString(xItem,XML.Fields.Title);
      Entry.Description:=toString(xItem,XML.Fields.Description);
      Entry.Location:=toString(xItem,XML.Fields.Location);
      Result:=True;
    end;
  end;
end;

class function  Items.toXML(var Entry:Item; Output:TMemoryStream):boolean;
var
  iStart:LongInt;
  iLength:LongInt;
begin
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Assembly,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.ParentID,Entry.ParentID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Entry.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Entry.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Scheduled,Entry.Scheduled),Output);
    Core.Streams.Write(Print(XML.Fields.State,Entry.State),Output);
    Core.Streams.Write(Print(XML.Fields.Privacy,Entry.Privacy),Output);
    Core.Streams.Write(Print(XML.Fields.Admins,Entry.Admins),Output);
    Core.Streams.Write(Print(XML.Fields.Files,Entry.Files),Output);
    Core.Streams.Write(Print(XML.Fields.Comments,Entry.Comments),Output);
    Core.Streams.Write(Print(XML.Fields.Assemblies,Entry.Assemblies),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Entry.Title,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Entry.Description,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Location,Entry.Location,CDATA_ON),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Assembly,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;


class function Items.DB.Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
var
  iCount       : LongInt;
  Commands     : Core.Database.Types.Commands;
  iReset       : QWord;
  iInsertID    : QWord;
begin
  Result := False;
  iCount := 0;
  iReset := 0;
  iInsertID := Random(High(LongInt));
  Entry.Created:=Core.Timer.dtUT;
  Entry.Modified:=Entry.Created;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, IDs.ID, poNone, oNone, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, IDs.InsertID, poNone, oNone, iReset, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.UserID, poNone, oNone, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.ParentID, poNone, oNone, Entry.ParentID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Created, poNone, oNone, Entry.Created, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Scheduled, poNone, oNone, Entry.Scheduled, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.State, poNone, oNone, Entry.State, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Admins, poNone, oNone, Entry.Admins, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Files, poNone, oNone, Entry.Files, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Comments, poNone, oNone, Entry.Comments, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Assemblies, poNone, oNone, Entry.Assemblies, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Title, poNone, oNone, Entry.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Description, poNone, oNone, Entry.Description, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Location, poNone, oNone, Entry.Location, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadAssembly(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Items.PItem;
begin
  ItmP:=DataP;

  ItmP^.Verified    := true;
  ItmP^.ID          := Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  ItmP^.ParentID    := Fields.FieldByName(Items.DB.Keys.ParentID).AsLargeInt;
  ItmP^.Created     := Fields.FieldByName(Items.DB.Keys.Created).AsFloat;
  ItmP^.Modified    := Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
  ItmP^.Scheduled   := Fields.FieldByName(Items.DB.Keys.Scheduled).AsFloat;
  ItmP^.State       := Fields.FieldByName(Items.DB.Keys.State).AsInteger;
  ItmP^.Privacy     := Fields.FieldByName(Items.DB.Keys.Privacy).AsInteger;
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Admins).AsString,ItmP^.Admins,',');
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Files).AsString,ItmP^.Files,',');
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Comments).AsString,ItmP^.Comments,',');
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Assemblies).AsString,ItmP^.Assemblies,',');
  ItmP^.Title       := Fields.FieldByName(Items.DB.Keys.Title).AsString;
  ItmP^.Description := Fields.FieldByName(Items.DB.Keys.Description).AsString;
end;

class function Items.DB.Read(Task: Core.Database.Types.TTask; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    {$i Storage.Assemblies.Assembly.Read.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadAssembly, @Entry) and Entry.Verified);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.Write(Task: Core.Database.Types.TTask;DomainID,UserID:QWord; var Entry:Item): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.ParentID, poNone, oNone, Entry.ParentID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Scheduled, poNone, oNone, Entry.Scheduled, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.State, poNone, oNone, Entry.State, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Privacy, poNone, oNone, Entry.Privacy, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Title, poNone, oNone, Entry.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Description, poNone, oNone, Entry.Description, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Refresh(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Modified, poAnd, oNotEqual, Entry.Modified, Commands);
    {$i Storage.Assemblies.Assembly.Read.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadAssembly, @Entry) and Entry.Verified);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetState(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PInteger(DataP)^ := Fields.FieldByName(Items.DB.Keys.State).AsInteger;
end;

class function Items.DB.getState(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.State, poNone, oNone, Commands);
    Result := Core.Database.SQL.Select(Task, @Commands,@cbgetState, @Value) ;
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setState(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.State, poNone, oNone, Value, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbgetComments(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Comments).AsString,Core.Database.Types.PLargeWordArray(DataP)^,',');
end;

class function Items.DB.getComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Entries, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Comments, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetComments, @Entries) and (Length(Entries) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setComments(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Comments, poNone, oNone, Entries, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetAssemblies(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Assemblies).AsString,Core.Database.Types.PLargeWordArray(DataP)^,',');
end;

class function Items.DB.getAssemblies(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Entries, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Assemblies, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetAssemblies, @Entries) );
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setAssemblies(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Assemblies, poNone, oNone, Entries, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetAdmins(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Admins).AsString,Core.Database.Types.PLargeWordArray(DataP)^,',');
end;

class function Items.DB.getAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Entries, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Admins, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetAdmins, @Entries) and (Length(Entries) > 0));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setAdmins(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Admins, poNone, oNone, Entries, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.Granted(Task:Core.Database.Types.TTask; UserID,ItemID:QWord):boolean;
var
  List:Core.Arrays.Types.LargeWord;
begin
  Try
    Result:=getAdmins(Task,ItemID,List);
    if Result then
        Result:=(Core.Arrays.LargeWord.IndexOf(UserID,List)<>-1);
  Finally
    Core.Arrays.LargeWord.Done(List);
  end;
end;


procedure cbgetFiles(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.Files).AsString,Core.Database.Types.PLargeWordArray(DataP)^,',');
end;

class function Items.DB.getFiles(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    SetLength(Entries, 0);
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Files, poNone, oNone, Commands);
    Result := (Core.Database.SQL.Select(Task, @Commands,@cbgetFiles, @Entries));
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setFiles(Task:Core.Database.Types.TTask; ItemID:QWord; var Entries:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Files, poNone, oNone, Entries, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetPrivacy(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PInteger(DataP)^:=Fields.FieldByName(Items.DB.Keys.Files).AsInteger;
end;

class function Items.DB.getPrivacy(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Privacy, poNone, oNone, Commands);
    Result := Core.Database.SQL.Select(Task, @Commands,@cbgetPrivacy, @Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setPrivacy(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Integer): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Privacy, poNone, oNone, Value, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbgetScheduled(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PDouble(DataP)^:=Fields.FieldByName(Items.DB.Keys.Scheduled).AsFloat;
end;

class function Items.DB.getScheduled(Task: Core.Database.Types.TTask;ItemID: QWord; var Value: Double): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForFields, IDs.Scheduled, poNone, oNone, Commands);
    Result := Core.Database.SQL.Select(Task, @Commands,@cbgetScheduled, @Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Items.DB.setScheduled(Task: Core.Database.Types.TTask; ItemID: QWord; var Value: Double): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Core.Timer.dtUT, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Scheduled, poNone, oNone, Value, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Items.DB.Find(Task:Core.Database.Types.TTask; DomainID:QWord; Term:Core.Strings.VarString; var Entries:List):boolean;
begin
  Result:=false; // todo
end;

class Function  Items.toXML(var Entries:List; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Assemblies,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to High(Entries) do
    toXML(Entries[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Assemblies,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;


class procedure Items.Init(var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Empty(Var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Done(Var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;

class procedure Subscription.Init(var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Subscription.Empty(Var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;


class procedure Subscription.Done(Var Entries:List);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;

class procedure Subscription.Init(var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    DomainID:=0;
    UserID:=0;
    AssemblyID:=Defaults.AssemblyID;
    ProjectID:=Defaults.ProjectID;
    TaskID:=Defaults.TaskID;
    Verified:=False;
    Created:=0;
    Modified:=0;
    Alerts:=Defaults.Alerts;
    Status:=Defaults.Status;
  end;
end;

class procedure Subscription.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    DomainID:=0;
    UserID:=0;
    AssemblyID:=Defaults.AssemblyID;
    ProjectID:=Defaults.ProjectID;
    TaskID:=Defaults.TaskID;
    Verified:=False;
    Created:=0;
    Modified:=0;
    Alerts:=Defaults.Alerts;
    Status:=Defaults.Status;
  end;
end;

class function Subscription.IndexOf(var Entries:List; ID:QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]^.ID = ID then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class procedure Subscription.Done(Var Entry:Item);
begin
  Finalize(Entry);
end;

class procedure Subscription.setVerified(var Entry:Item; Value:boolean);
begin
  Entry.Verified:=Value;
end;

class function  Subscription.fromXML(xDoc:TXMLDocument; var Entry:Item):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  with Core.XML.DB do begin
    xItem:=getNode(xDoc,XML.Stanzas.Subscription);
    if xItem<>nil then begin
      Entry.ID:=toQWord(xItem,XML.Fields.ID);
      Entry.AssemblyID:=toQWord(xItem,XML.Fields.AssemblyID);
      Entry.ProjectID:=toQWord(xItem,XML.Fields.ProjectID);
      Entry.TaskID:=toQWord(xItem,XML.Fields.TaskID);
      Entry.Created:=toDouble(xItem,XML.Fields.Created);
      Entry.Modified:=toDouble(xItem,XML.Fields.Modified);
      Entry.Alerts:=toInteger(xItem,XML.Fields.Alerts);
      Entry.Status:=toInteger(xItem,XML.Fields.Status);
      Result:=True;
    end;
  end;
end;

class function  Subscription.toXML(var Entry:Item; Output:TMemoryStream):boolean;
begin
  Output.Seek(0,soFromEnd);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Subscription,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.AssemblyID,Entry.AssemblyID),Output);
    Core.Streams.Write(Print(XML.Fields.ProjectID,Entry.ProjectID),Output);
    Core.Streams.Write(Print(XML.Fields.TaskID,Entry.TaskID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Entry.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Entry.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.Alerts,Entry.Alerts),Output);
    Core.Streams.Write(Print(XML.Fields.Status,Entry.Status),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Subscription,Output);
  Core.Streams.Write('>',1,Output);
end;

class function Subscription.DB.Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iReset: QWord;
  iInsertID: QWord;
begin
  Result := False;
  iCount := 0;
  iReset := 0;
  iInsertID := Random(High(LongInt));
  Entry.Created:=Core.Timer.dtUT;
  Entry.Modified:=Entry.Created;
  try
    Core.Database.AddCommand(iCount, TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.InsertID, poNone, oNone, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.InsertID, poNone, oEqual, iInsertID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForPrimaryID, IDs.ID, poNone, oNone, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForResetInsertID, IDs.InsertID, poNone, oNone, iReset, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.DomainID, poNone, oNone, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.UserID, poNone, oNone, UserID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.AssemblyID, poNone, oNone, Entry.AssemblyID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.ProjectID, poNone, oNone, Entry.ProjectID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.TaskID, poNone, oNone, Entry.TaskID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Created, poNone, oNone, Entry.Created, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Alerts, poNone, oNone, Entry.Alerts, Commands);
    Core.Database.AddCommand(iCount, TableP, useForInsert, IDs.Status, poNone, oNone, Entry.Status, Commands);

    Result := Core.Database.SQL.Insert(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Subscription.DB.Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbReadSubscription(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Subscription.PItem;
begin
  ItmP:=DataP;

  ItmP^.Verified    := true;
  ItmP^.ID          := Fields.FieldByName(Subscription.DB.Keys.ID).AsLargeInt;
  ItmP^.UserID      := Fields.FieldByName(Subscription.DB.Keys.UserID).AsLargeInt;
  ItmP^.AssemblyID  := Fields.FieldByName(Subscription.DB.Keys.AssemblyID).AsLargeInt;
  ItmP^.ProjectID   := Fields.FieldByName(Subscription.DB.Keys.ProjectID).AsLargeInt;
  ItmP^.TaskID      := Fields.FieldByName(Subscription.DB.Keys.TaskID).AsLargeInt;
  ItmP^.Created     := Fields.FieldByName(Subscription.DB.Keys.Created).AsFloat;
  ItmP^.Modified    := Fields.FieldByName(Subscription.DB.Keys.Modified).AsFloat;
  ItmP^.Alerts      := Fields.FieldByName(Subscription.DB.Keys.Alerts).AsInteger;
  ItmP^.Status      := Fields.FieldByName(Subscription.DB.Keys.Status).AsInteger;
end;

class function Subscription.DB.Read(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID,poAnd,oEqual,UserID,Commands);
    {$i Storage.Assemblies.Subscription.Read.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadSubscription, @Entry) and Entry.Verified);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Subscription.DB.Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Modified := Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.AssemblyID, poNone, oNone, Entry.AssemblyID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.ProjectID, poNone, oNone, Entry.ProjectID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.TaskID, poNone, oNone, Entry.TaskID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Modified, poNone, oNone, Entry.Modified, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Alerts, poNone, oNone, Entry.Alerts, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Status, poNone, oNone, Entry.Status, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Subscription.DB.Refresh(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Entry.Verified:=false;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, Entry.ID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.UserID, poAnd, oEqual, UserID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDS.Modified, poAnd, oNotEqual, Entry.Modified, Commands);
    {$i Storage.Assemblies.Subscription.Read.inc}
    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadSubscription, @Entry) and Entry.Verified);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListSubscriptions(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmsP:Subscription.PList;
  ItmP:Subscription.PItem;
  iIndex:LongInt;
  ID:QWord;
begin
  ItmsP:=DataP;
  ID:=Fields.FieldByName(Subscription.DB.Keys.ID).AsLargeInt;
  iIndex:=Subscription.IndexOf(ItmsP^,ID);
  if iIndex=-1 then begin
    iIndex:=Length(ItmsP^);
    SetLength(ItmsP^,iIndex+1);
    New(ItmsP^[iIndex]);
    Subscription.Init(ItmsP^[iIndex]^);
  end;
  ItmP:=ItmsP^[iIndex];
  cbReadSubscription(CommandsP,Fields,ItmP);
end;

class function Subscription.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Entries:List):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria,IDs.UserID, poAnd, oEqual, UserID, Commands);
    {$i Storage.Assemblies.Subscription.Read.inc}
     Result := Core.Database.SQL.Select(Task, @Commands,@cbListSubscriptions, @Entries) ;
  finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Subscription.toXML(var Entries:List; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Subscriptions,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to High(Entries) do
    Subscription.toXML(Entries[iLcv]^,Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Subscriptions,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;


initialization
  RegisterDB;
end.


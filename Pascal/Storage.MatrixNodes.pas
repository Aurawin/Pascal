{
unit Storage.MatrixNodes.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html


Matrix Nodes Table storage for IP address allocation
so Matrix Services can be bound

}
unit Storage.MatrixNodes;

interface

uses
  Classes,

  Core,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.Utils.Sockets,
  Core.Utils.Files,
  Core.Timer,

  SysUtils;

Type
  Node=class
  Const
    MN_ALL                           = 'All';


    FMT_MN_DISPLAY_CAPTION           = '%s [%s]';
    FMT_MN_BOUND_STATUS:Array[Boolean] of Core.Strings.VarString = ('%s','bound to %s');
    FMT_MN_UNBOUND                   = 'unbound';
    FMT_MN_UNKNOWN                   = 'unknown';

    NO_DEVICE                        : Core.Strings.VarString = '';
    DISK_ENABLED                     : boolean = true;
    DISK_DISABLED                    : boolean = false;

  Type
    Process=class
    const
      GroupID                    : QWord = 0; // Root
      UserID                     : QWord = 0; // Root
      Permissions                : LongInt = &774;
    end;
    TMNStats=Record
      Streams                        : QWord;
      Sessions                       : QWord;
      Filtered                       : QWord;
      Transactions                   : QWord;
      PTX_Sent                       : QWord;
      PTX_Received                   : QWord;
      MB_Sent                        : QWord;
      MB_Received                    : QWord;
      MEM_Total                      : QWord;
      MEM_Free                       : QWord;
      MEM_Used                       : QWord;
      CPU_Usage                      : Byte;   // 0-100%
    end;
    TMNDisk=record
      Enabled                        : Boolean;
      Live                           : Boolean;
      Device                         : Core.Strings.VarString;
      Capacity                       : QWord;
      Consumption                    : QWord;
      Available                      : QWord;
    end;
    Item=Record
      ID                             : QWord; // ID
      ClusterID                      : QWord;
      ResourceID                     : QWord;
      DomainID                       : QWord;
      IP                             : Int64;
      GroupID                        : QWord; // operating system reports this at runtime
      UserID                         : QWord; // operating system reports this at runtime
      Alias                          : Core.Strings.VarString;
      Disk                           : TMNDisk;
      Stats                          : TMNStats;
    end;
    PItem=^Item;
    Items=Array of PItem;
    PItems=^Items;
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        ClusterID                : Core.Database.Types.Integer = 2;
        ResourceID               : Core.Database.Types.Integer = 3;
        DomainID                 : Core.Database.Types.Integer = 4;
        GroupID                  : Core.Database.Types.Integer = 5;
        UserID                   : Core.Database.Types.Integer = 6;
        InfoIP                   : Core.Database.Types.Integer = 7;
        InfoAlias                : Core.Database.Types.Integer = 8;
        DiskDevice               : Core.Database.Types.Integer = 9;
        DiskEnabled              : Core.Database.Types.Integer = 10;
        DiskLive                 : Core.Database.Types.Integer = 11;
        DiskCapacity             : Core.Database.Types.Integer = 12;
        DiskConsumption          : Core.Database.Types.Integer = 13;
        DiskAvailable            : Core.Database.Types.Integer = 14;
        StatsStreams             : Core.Database.Types.Integer = 15;
        StatsSessions            : Core.Database.Types.Integer = 16;
        StatsFiltered            : Core.Database.Types.Integer = 17;
        StatsTX                  : Core.Database.Types.Integer = 18;
        StatsPTXSent             : Core.Database.Types.Integer = 19;
        StatsPTXRcvd             : Core.Database.Types.Integer = 20;
        StatsMBSent              : Core.Database.Types.Integer = 21;
        StatsMBRcvd              : Core.Database.Types.Integer = 22;
        MemTotal                 : Core.Database.Types.Integer = 23;
        MemFree                  : Core.Database.Types.Integer = 24;
        MemUsed                  : Core.Database.Types.Integer = 25;
        CPU                      : Core.Database.Types.Integer = 26;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'MNID';
        InsertID                 : Core.Database.Types.VarString = 'MNIID';
        ClusterID                : Core.Database.Types.VarString = 'MNCID';
        ResourceID               : Core.Database.Types.VarString = 'MNRID';
        DomainID                 : Core.Database.Types.VarString = 'MNDID';
        GroupID                  : Core.Database.Types.VarString = 'MNGID';
        UserID                   : Core.Database.Types.VarString = 'MNUID';
        InfoIP                   : Core.Database.Types.VarString = 'MNIP';
        InfoAlias                : Core.Database.Types.VarString = 'MNALS';
        DiskDevice               : Core.Database.Types.VarString = 'MNDDV';
        DiskEnabled              : Core.Database.Types.VarString = 'MNDDE';
        DiskLive                 : Core.Database.Types.VarString = 'MNDDL';
        DiskCapacity             : Core.Database.Types.VarString = 'MNDDC';
        DiskConsumption          : Core.Database.Types.VarString = 'MNDCN';
        DiskAvailable            : Core.Database.Types.VarString = 'MNDAL';
        StatsStreams             : Core.Database.Types.VarString = 'MNSSMS';
        StatsSessions            : Core.Database.Types.VarString = 'MNSSNS';
        StatsFiltered            : Core.Database.Types.VarString = 'MNSFLT';
        StatsTX                  : Core.Database.Types.VarString = 'MNSTX';
        StatsPTXSent             : Core.Database.Types.VarString = 'MNSPTS';
        StatsPTXRcvd             : Core.Database.Types.VarString = 'MNSPTR';
        StatsMBSent              : Core.Database.Types.VarString = 'MNSMBS';
        StatsMBRcvd              : Core.Database.Types.VarString = 'MNSMBR';
        MemTotal                 : Core.Database.Types.VarString = 'MNSMT';
        MemFree                  : Core.Database.Types.VarString = 'MNSMF';
        MemUsed                  : Core.Database.Types.VarString = 'MNSML';
        CPU                      : Core.Database.Types.VarString = 'MNSCPU';
      end;
      const
        TableP                   : Core.Database.Types.PTable = nil;
        MonitorP                 : Core.Database.Monitor.Types.PItem = nil;
        Startup                  : Core.Database.Types.TableIni = (
          AutoCreate             : True;
          AutoCommit             : True;
          Group                  : 'Clustering';
          Name                   : 'Nodes';
          Value                  : 'scs_mtx_nds';
          Hint                   : 'Storage for matrix based nodal information';
          PrimaryKeyP            : @Keys.ID;
        );
        Fields: array [0..26] of Core.Database.Types.Field = (
          (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
          (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.ClusterID; KeyP: @Keys.ClusterID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
          (IDP: @IDs.ResourceID; KeyP: @Keys.ResourceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
          (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
          (IDP: @IDs.GroupID; KeyP: @Keys.GroupID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.InfoIP; KeyP: @Keys.InfoIP; DataType: dftInt64; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
          (IDP: @IDs.InfoAlias; KeyP: @Keys.InfoAlias; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
          (IDP: @IDs.DiskDevice; KeyP: @Keys.DiskDevice; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
          (IDP: @IDs.DiskEnabled; KeyP: @Keys.DiskEnabled; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.DiskLive; KeyP: @Keys.DiskLive; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.DiskCapacity; KeyP: @Keys.DiskCapacity; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.DiskConsumption; KeyP: @Keys.DiskConsumption; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.DiskAvailable; KeyP: @Keys.DiskAvailable; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),

          (IDP: @IDs.StatsStreams; KeyP: @Keys.StatsStreams; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsSessions; KeyP: @Keys.StatsSessions; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsFiltered; KeyP: @Keys.StatsFiltered; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsTX; KeyP: @Keys.StatsTX; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsPTXSent; KeyP: @Keys.StatsPTXSent; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsPTXRcvd; KeyP: @Keys.StatsPTXRcvd; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsMBSent; KeyP: @Keys.StatsMBSent; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.StatsMBRcvd; KeyP: @Keys.StatsMBRcvd; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),

          (IDP: @IDs.MemTotal; KeyP: @Keys.MemTotal; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.MemFree; KeyP: @Keys.MemFree; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.MemUsed; KeyP: @Keys.MemUsed; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),

          (IDP: @IDs.CPU; KeyP: @Keys.CPU; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )

        );
        class Function  Fill(Task:Core.Database.Types.TTask; Var MN:Item):Boolean; overload;
        class Function  Fill(Task:Core.Database.Types.TTask; ID:QWord; Var MN:Item):Boolean; overload;
        class Function  Create(Task:Core.Database.Types.TTask; Var MN:Item): Boolean;
        class Function  Delete(Task:Core.Database.Types.TTask; Var MN:Item): Boolean; overload;
        class Function  Delete(Task:Core.Database.Types.TTask; ClusterID:QWord): Boolean; overload;
        class Function  Delete(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord): Boolean; overload;
        class Function  GetIP(Task:Core.Database.Types.TTask; ID:QWord; Var IP:QWord):Boolean;
        class Function  Write(Task:Core.Database.Types.TTask; Var MN:Item):Boolean; overload;
        class Function  Write(Task:Core.Database.Types.TTask; ID:QWord; var Disk:TMNDisk):Boolean; overload;
        class Function  Write(Task:Core.Database.Types.TTask; ID:QWord; Var Stats:TMNStats):Boolean; overload;
        class Function  Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord):Boolean; overload;
        class Function  Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID:QWord):Boolean; overload;
        class Function  SetDomainID(Task:Core.Database.Types.TTask; NodeID,DomainID:QWord):Boolean;
        class Function  Count(Task:Core.Database.Types.TTask; ClusterID:QWord):QWord; overload;
        class Function  Count(Task:Core.Database.Types.TTask):QWord; overload;
        class Function  CountAllocated(Task:Core.Database.Types.TTask):QWord;
        class Function  CountAvailable(Task:Core.Database.Types.TTask):QWord;

        class Function  Stats(Task:Core.Database.Types.TTask; Var MN:Item):Boolean;

        class Function  Exists(Task:Core.Database.Types.TTask; ID:QWord):Boolean; overload;

        class Function  List(Task:Core.Database.Types.TTask; Var Entries:Node.Items):boolean; overload;
        class Function  List(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Node.Items):boolean; overload;
        class Function  List(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Core.Arrays.Types.KeyStrings):Boolean; overload;
        class Function  List(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Core.Arrays.Types.VarString):Boolean; overload;

        class Function  Disks(Task:Core.Database.Types.TTask; Var Entries:Items):boolean; overload;
        class Function  Disks(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Items):boolean; overload;

        class Function  Disks_Poll(Task:Core.Database.Types.TTask; ClusterID:QWord; Var Entries:Node.Items):boolean;
        class procedure SetupDisks(Task:Core.Database.Types.TTask; out Nodes:Items; out sErrors:Core.Strings.VarString);
      end;

      class procedure Empty(Var Entries:Node.Items); overload;
      class procedure Empty(Var Entry:Node.Item); overload;
      class procedure Empty(Var Entry:TMNStats); overload;
      class procedure Empty(Var Entry:TMNDisk); overload;

      class procedure Init(Var Entry:TMNStats); overload;
      class procedure Init(Var Entry:TMNDisk); overload;
      class procedure Init(Var Entry:Node.Item); overload;
      class procedure Init(Var Entries:Node.Items); overload;

      class procedure Done(Var Entries:Node.Items); overload;
      class procedure Done(Var Entry:TMNStats); overload;
      class procedure Done(Var Entry:TMNDisk); overload;
      class procedure Done(Var Entry:Node.Item); overload;

      class procedure Remove(Var Entries:Node.Items; var ItemP:PItem); overload;

      class Function  IndexOf(Var Entries:Node.Items; ID:QWord): LongInt; overload;
      class Function  AliasOf(Var Entries:Node.Items; ID:QWord):Core.Strings.VarString; overload;

      class procedure Copy(Var Source,Destination:Node.Item); overload;
      class procedure Copy(Var Source,Dest:TMNDisk); overload;
      class procedure Copy(Var Source,Dest:TMNStats); overload;


      class function  StatRAM(Var Entry:Node.Item):boolean;
      class function  StatDisk(Var Entry:Node.Item; out sError:Core.Strings.VarString):boolean;
      class function  StatIDs(Var Entry:Item; var sGroup,sID,sError:Core.Strings.VarString):boolean;
      class function  IsMounted(var Entry:Item; out sError:Core.Strings.VarString):boolean;

      {$i Storage.MatrixNodes.Decs.inc}
    end;
implementation
uses
  db,
  Storage.MatrixServices,
  Process;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushClusterDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Node.DB.MonitorP then begin
      with Node.DB do begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        Finally
          Core.Database.Done(Commands);
        End;
      end;
    end;
  end;

  procedure PushNodeDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Node.DB.MonitorP then begin
      with Node.DB do begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        Finally
          Core.Database.Done(Commands);
        End;
      end;
    end;
  end;

  procedure PushResourceDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Node.DB.MonitorP then begin
      With Node.DB do begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        Finally
          Core.Database.Done(Commands);
        End;
      end;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.CLUSTER_DELETED           : PushClusterDeleted;
    Core.Database.Monitor.Notify.CLUSTER_NODE_DELETED      : PushNodeDeleted;
    Core.Database.Monitor.Notify.CLUSTER_RESOURCE_DELETED  : PushResourceDeleted;
  end;
end;

procedure RegisterDBM;
var
  iLcv                           : LongInt;
begin
  With Node.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

procedure CB_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
 NodeP:Node.PItem;
begin
  NodeP:=DataP;
  {$i Storage.MatrixNodes.Node.Fill.inc}
end;

procedure CB_Fill_Stats(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
 NodeP:Node.PItem;
begin
  NodeP:=DataP;
  With NodeP^ do begin
    Stats.MEM_Total:=Fields.FieldByName(Node.DB.Keys.MemTotal).AsLargeInt;
    Stats.MEM_Free:=Fields.FieldByName(Node.DB.Keys.MemFree).AsLargeInt;
    Stats.MEM_Used:=Fields.FieldByName(Node.DB.Keys.MemUsed).AsLargeInt;
    with Disk do begin
      Enabled:=Fields.FieldByName(Node.DB.Keys.DiskEnabled).AsBoolean;
      Live:=Fields.FieldByName(Node.DB.Keys.DiskLive).AsBoolean;
      Capacity:=Fields.FieldByName(Node.DB.Keys.DiskCapacity).AsLargeInt;
      Consumption:=Fields.FieldByName(Node.DB.Keys.DiskConsumption).AsLargeInt;
      Available:=Fields.FieldByName(Node.DB.Keys.DiskAvailable).AsLargeInt;
    end;
  end;
end;

procedure CB_GetIP(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Database.Types.PLargeWord(DataP)^:=Fields.FieldByName(Node.DB.Keys.InfoIP).AsLargeInt;
end;

class Function  Node.DB.Stats(Task:Core.Database.Types.TTask; Var MN:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MN.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poAnd,oEqual,MN.ClusterID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DiskEnabled,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DiskLive,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DiskCapacity,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DiskConsumption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DiskAvailable,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.MemTotal,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.MemFree,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.MemUsed,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Stats,@MN);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Fill(Task:Core.Database.Types.TTask; Var MN:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InfoAlias,poNone,oEqual,MN.Alias,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poAnd,oEqual,MN.ClusterID,Commands);
    {$i Storage.MatrixNodes.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill,@MN);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Fill(Task:Core.Database.Types.TTask; ID:QWord; Var MN:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; Empty(MN); MN.ID:=ID;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MN.ID,Commands);
    {$i Storage.MatrixNodes.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill,@MN);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.GetIP(Task:Core.Database.Types.TTask; ID:QWord; Var IP:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; IP:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.InfoIP,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GetIP,@IP) and (IP<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Create(Task:Core.Database.Types.TTask; Var MN:Item): Boolean;
var
  iCount                         : LongInt;
  InsertID,iReset                : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    InsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,MN.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ClusterID,poNone,oNone,MN.ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ResourceID,poNone,oNone,MN.ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,MN.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.GroupID,poNone,oNone,MN.GroupID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,MN.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InfoIP,poNone,oNone,MN.IP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InfoAlias,poNone,oNone,MN.Alias,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DiskDevice,poNone,oNone,MN.Disk.Device,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DiskEnabled,poNone,oNone,MN.Disk.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DiskLive,poNone,oNone,MN.Disk.Live,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DiskCapacity,poNone,oNone,MN.Disk.Capacity,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DiskConsumption,poNone,oNone,MN.Disk.Consumption,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DiskAvailable,poNone,oNone,MN.Disk.Available,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Delete(Task:Core.Database.Types.TTask; ClusterID:QWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Delete(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Delete(Task:Core.Database.Types.TTask; Var MN:Item): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MN.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Node.DB.Write(Task:Core.Database.Types.TTask; Var MN:Item):Boolean;
var
  iCount                        : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MN.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DomainID,poNone,oNone,MN.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.GroupID,poNone,oNone,MN.GroupID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.UserID,poNone,oNone,MN.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.InfoIP,poNone,oNone,MN.IP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.InfoAlias,poNone,oNone,MN.Alias,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskDevice,poNone,oNone,MN.Disk.Device,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskEnabled,poNone,oNone,MN.Disk.Enabled,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Node.DB.SetDomainID(Task:Core.Database.Types.TTask; NodeID,DomainID:QWord):Boolean;
var
  iCount                        : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Write(Task:Core.Database.Types.TTask; ID:QWord; var Disk:TMNDisk):Boolean;
var
  iCount                        : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskEnabled,poNone,oNone,Disk.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskLive,poNone,oNone,Disk.Live,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskCapacity,poNone,oNone,Disk.Capacity,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskConsumption,poNone,oNone,Disk.Consumption,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DiskAvailable,poNone,oNone,Disk.Available,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Write(Task:Core.Database.Types.TTask; ID:QWord; var Stats:TMNStats):Boolean;
var
  iCount                        : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.MemTotal,poNone,oNone,Stats.MEM_Total,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.MemUsed,poNone,oNone,Stats.MEM_Used,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.MemFree,poNone,oNone,Stats.MEM_Free,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Count(Task:Core.Database.Types.TTask; ClusterID:QWord):QWord;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.SQL.Count(Task,@Commands,Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Count(Task:Core.Database.Types.TTask):QWord;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.SQL.Count(Task,@Commands,Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID:QWord):Boolean;
var
  iCount                         : LongInt;
  qCount                         : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Try
    iCount:=0;
    qCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poAnd,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,qCount) and (qCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord):Boolean;
var
  iCount                         : LongInt;
  qCount                         : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Try
    iCount:=0;
    qCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poAnd,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oNotEqual,Default.Domain,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,qCount) and (qCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.CountAllocated(Task:Core.Database.Types.TTask):QWord;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oNotEqual,Default.Cluster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oNotEqual,Default.Resource,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oNotEqual,Default.Domain,Commands);
    Core.Database.SQL.Count(Task,@Commands,Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.CountAvailable(Task:Core.Database.Types.TTask):QWord;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oNotEqual,Default.Cluster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oNotEqual,Default.Resource,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,Default.Domain,Commands);
    Core.Database.SQL.Count(Task,@Commands,Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Exists(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
var
  iCount                         : LongInt;
  qCount                         : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    qCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,qCount) and (qCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure CB_List_Nodes_KPL(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.KeyString.Add(Core.Arrays.Types.PKeyStrings(DataP),Fields.FieldByName(Node.DB.Keys.ID).AsString,Fields.FieldByName(Node.DB.Keys.InfoAlias).AsString);
end;

class Function  Node.DB.List(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Core.Arrays.Types.KeyStrings):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Arrays.KeyString.Empty(Entries);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.InfoAlias,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List_Nodes_KPL,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_List_Nodes_SA(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.VarString.Add(Core.Arrays.Types.PVarString(DataP),Fields.FieldByName(Node.DB.Keys.InfoAlias).AsString);
end;

class Function  Node.DB.List(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Core.Arrays.Types.VarString):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Arrays.VarString.Empty(Entries);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.InfoAlias,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List_Nodes_SA,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  NodesP:Node.PItems;
  NodeP:Node.PItem;
  iID:QWord;
  iIndex:LongInt;
begin
  NodesP:=DataP;
  iID:=Fields.FieldByName(Node.DB.Keys.ID).AsLargeInt;
  iIndex:=Node.IndexOf(NodesP^,iID);
  if iIndex=-1 then begin
    iIndex:=Length(NodesP^);
    SetLength(NodesP^,iIndex+1);
    New(NodeP);
    Node.Init(NodeP^);
    NodeP^.ID:=iID;
    NodesP^[iIndex]:=NodeP;
  end else
    NodeP:=NodesP^[iIndex];
  {$i Storage.MatrixNodes.Node.Fill.inc}
end;

class Function  Node.DB.List(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Node.Items):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Empty(Entries);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    {$i Storage.MatrixNodes.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.List(Task:Core.Database.Types.TTask; Var Entries:Node.Items):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Entries);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    {$i Storage.MatrixNodes.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Disks_Poll(Task:Core.Database.Types.TTask; ClusterID:QWord; Var Entries:Node.Items):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Node.Empty(Entries);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DiskDevice,poAnd,oNotNull,Commands);
    {$i Storage.MatrixNodes.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Disks(Task:Core.Database.Types.TTask; ClusterID,ResourceID:QWord; Var Entries:Node.Items):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Entries);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    {$i Storage.MatrixNodes.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Node.DB.Disks(Task:Core.Database.Types.TTask; Var Entries:Node.Items):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Entries);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DiskDevice,poNone,oNotNull,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DiskEnabled,poAnd,oEqual,DISK_ENABLED,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DiskDevice,poAnd,oNotEqual,NO_DEVICE,Commands);

    {$i Storage.MatrixNodes.Fill.Select.inc}

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List,@Entries);

  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Node.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.ClusterID:=Source.ClusterID;
  Destination.ResourceID:=Source.ResourceID;
  Destination.DomainID:=Source.DomainID;
  Destination.IP:=Source.IP;

  Destination.GroupID:=Source.GroupID;
  Destination.UserID:=Source.UserID;

  Destination.Alias:=Source.Alias;

  Copy(Source.Disk,Destination.Disk);
  Copy(Source.Stats,Destination.Stats);
end;

class procedure Node.Copy(var Source,Dest:TMNDisk);
begin
  Dest.Device:=Source.Device;
  Dest.Enabled:=Source.Enabled;
  Dest.Live:=Source.Live;
  Dest.Capacity:=Source.Capacity;
  Dest.Consumption:=Source.Consumption;
  Dest.Available:=Source.Available;
end;

class procedure Node.Copy(Var Source,Dest:TMNStats);
begin
  Dest.Streams:=Source.Streams;
  Dest.Sessions:=Source.Sessions;
  Dest.Filtered:=Source.Filtered;
  Dest.Transactions:=Source.Transactions;
  Dest.PTX_Sent:=Source.PTX_Sent;
  Dest.PTX_Received:=Source.PTX_Received;
  Dest.MB_Sent:=Source.MB_Sent;
  Dest.MB_Received:=Source.MB_Received;
  Dest.MEM_Total:=Source.MEM_Total;
  Dest.MEM_Free:=Source.MEM_Free;
  Dest.MEM_Used:=Source.MEM_Used;
  Dest.CPU_Usage:=Source.CPU_Usage;
end;

class procedure Node.Empty(Var Entry:Node.Item);
begin
  With Entry do begin
    ID:=0;
    ClusterID:=0;
    ResourceID:=0;
    DomainID:=0;
    UserID:=0;
    GroupID:=0;
    IP:=0;
    SetLength(Alias,0);
    Empty(Disk);
    Empty(Stats);
  end;
end;

class procedure Node.Empty(Var Entry:TMNDisk);
begin
  With Entry do begin
    SetLength(Device,0);
    Enabled:=false;
    Live:=false;
    Capacity:=0;
    Consumption:=0;
    Available:=0;
  end;
end;

class procedure Node.Empty(Var Entry:TMNStats);
begin
  With Entry do begin
    Streams:=0;
    Sessions:=0;
    Filtered:=0;
    Transactions:=0;
    PTX_Sent:=0;
    PTX_Received:=0;
    MB_Sent:=0;
    MB_Received:=0;
    MEM_Total:=0;
    MEM_Free:=0;
    MEM_Used:=0;
    CPU_Usage:=0;
  end;
end;

class procedure Node.Init(Var Entry:TMNStats);
begin
  Empty(Entry);
end;

class procedure Node.Empty(Var Entries: Node.Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]<>nil then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
    end;
  end;
  SetLength(Entries,0);
end;

class procedure Node.Init(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]<>nil then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
    end;
  end;
  SetLength(Entries,0);
end;

class procedure  Node.Init(Var Entry:TMNDisk);
begin
  with Entry do begin
    SetLength(Device,0);
    Enabled:=false;
    Live:=false;
    Capacity:=0;
    Consumption:=0;
    Available:=0;
  end;
end;

class procedure Node.Init(Var Entry:Node.Item);
begin
  With Entry do begin
    ID:=0;
    ClusterID:=0;
    ResourceID:=0;
    DomainID:=0;
    UserID:=0;
    GroupID:=0;
    IP:=0;
    SetLength(Alias,0);
    Init(Stats);
    Init(Disk);
  end;
end;

class procedure Node.Done(Var Entry:TMNStats);
begin
  Finalize(Entry);
end;

class procedure Node.Done(Var Entry:TMNDisk);
begin
  Finalize(Entry.Device);
  Finalize(Entry);
end;

class procedure Node.Done(Var Entry:Node.Item);
begin
  Finalize(Entry.Alias);
  Finalize(Entry.Stats);
  Finalize(Entry);
end;

class procedure Node.Done(Var Entries:Node.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]<>nil) then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
    end;
  end;
  Finalize(Entries);
end;

class Function  Node.IndexOf(Var Entries:Node.Items; ID:QWord): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1;
  iLcv:=0;
  iCount:=Length(Entries);
  While (iLcv<iCount) and (Result=-1) do begin
    if (Entries[iLcv]<>nil) and (Entries[iLcv]^.ID=ID) then
      Result:=iLcv;
    inc(iLcv);
  end;
end;

class procedure  Node.Remove(Var Entries:Node.Items; var ItemP:PItem);
var
  jLcv,iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=Length(Entries);
  for iLcv:=0 to iCount-1 do begin
    if (Entries[iLcv]=ItemP) then begin
      for jLcv:=iLcv to iCount-2 do
        Entries[jLcv]:=Entries[jLcv+1];
      Dec(iCount);
      SetLength(Entries,iCount);
      Done(ItemP^);
      Dispose(ItemP);
      ItemP:=nil;
      break;
    end;
  end;
end;

class Function  Node.AliasOf(Var Entries:Node.Items; ID:QWord):Core.Strings.VarString;
var
  iIndex:LongInt;
begin
  if ID=0 then begin
    Result:=MN_ALL;
  end else begin
    SetLength(Result,0);
    iIndex:=IndexOf(Entries,ID);
    if iIndex<>-1 then
      Result:=Entries[iIndex]^.Alias;
  end;
end;

class function Node.StatRAM(Var Entry:Node.Item):boolean;
begin
  Result:=true;
  Entry.Stats.MEM_Free:=Core.Timer.MemoryStats.Available div (1024*1024);
  Entry.Stats.MEM_Total:=Core.Timer.MemoryStats.Total div (1024*1024);
  Entry.Stats.MEM_Used:=Core.Timer.MemoryStats.Used div (1024*1024);
end;

{$i Storage.MatrixNodes.StatDisk.inc}
{$i Storage.MatrixNodes.IsMounted.inc}
{$i Storage.MatrixNodes.StatIDS.inc}
{$i Storage.MatrixNodes.SetupDisks.inc}


initialization
  RegisterDBM;
end.


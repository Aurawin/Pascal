{
unit Storage.MatrixClusters.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html


Matrix Clusters Table

}
unit Storage.MatrixClusters;

interface

uses
  Classes, SysUtils,Sockets,
  Core.Strings,
  Core.Arrays.VarString,
  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL;

Type
  Cluster=Class
  Type
    Location=Record
      Country                        : Core.Database.Types.VarString;
      Region                         : Core.Database.Types.VarString;
      Locality                       : Core.Database.Types.VarString;
      Area                           : Core.Database.Types.VarString;
      Street                         : Core.Database.Types.VarString;
      Building                       : Core.Database.Types.VarString;
      Floor                          : Core.Database.Types.VarString;
      Room                           : Core.Database.Types.VarString;
      Zip                            : Core.Database.Types.VarString;
      Description                    : Core.Database.Types.VarString;
    end;
    Connections=Record
      Streams                        : Core.Database.Types.Integer;
      Sessions                       : Core.Database.Types.Integer;
    End;
    Memory=record
      Total                          : Core.Database.Types.LargeWord;
      Free                           : Core.Database.Types.LargeWord;
      Load                           : Core.Database.Types.Byte;
    end;
    Uptime=record
      Years                          : Core.Database.Types.Integer;
      Months                         : Core.Database.Types.Byte;
      Weeks                          : Core.Database.Types.Byte;
      Days                           : Core.Database.Types.Integer;
      Hours                          : Core.Database.Types.Byte;
      Minutes                        : Core.Database.Types.Byte;
      Seconds                        : Core.Database.Types.Byte;
    end;
    Item=record
      ID                             : Core.Database.Types.LargeWord;
      ReportingIP                    : Core.Database.Types.LargeWord;
      ReportingSocket                : TSocket;
      BytesSent                      : Core.Database.Types.LargeWord;
      BytesReceived                  : Core.Database.Types.LargeWord;
      Transactions                   : Core.Database.Types.LargeWord;
      TXFiltered                     : Core.Database.Types.LargeWord;
      PTXSent                        : Core.Database.Types.LargeWord;
      PTXRecvd                       : Core.Database.Types.LargeWord;
      Connections                    : Connections;
      Memory                         : Memory;
      Uptime                         : Uptime;
      Location                       : Location;
      Group                          : Core.Database.Types.VarString;
    end;
    PItem=^Item;
    Items=Array of PItem;
    PItems=^Items;
    DB=Class
    type
      IDS=Class
      Const
        ID                           : LongInt = 0;
        InsertID                     : LongInt = 1;
        Group                        : LongInt = 2;
        ReportingIP                  : LongInt = 3;
        BytesSent                    : LongInt = 4;
        BytesReceived                : LongInt = 5;
        Transactions                 : LongInt = 6;
        TXFiltered                   : LongInt = 7;
        PTXSent                      : LongInt = 8;
        PTXRecvd                     : LongInt = 9;
        Sessions                     : LongInt = 10;
        Streams                      : LongInt = 11;
        MemoryTotal                  : LongInt = 12;
        MemoryFree                   : LongInt = 13;
        MemoryLoad                   : LongInt = 14;
        UptimeYears                  : LongInt = 15;
        UptimeMonths                 : LongInt = 16;
        UptimeWeeks                  : LongInt = 17;
        UptimeDays                   : LongInt = 18;
        UptimeHours                  : LongInt = 19;
        UptimeMinutes                : LongInt = 20;
        UptimeSeconds                : LongInt = 21;
        LocationCountry              : LongInt = 22;
        LocationRegion               : LongInt = 23;
        LocationLocality             : LongInt = 24;
        LocationArea                 : LongInt = 25;
        LocationStreet               : LongInt = 26;
        LocationBuilding             : LongInt = 27;
        LocationFloor                : LongInt = 28;
        LocationRoom                 : LongInt = 29;
        LocationZip                  : LongInt = 30;
        LocationDescription          : LongInt = 31;
      end;
      Keys=Class
      Const
        ID                           : Core.Strings.VarString = 'MCID';
        InsertID                     : Core.Strings.VarString = 'MCIID';
        Group                        : Core.Strings.VarString = 'MCG';
        ReportingIP                  : Core.Strings.VarString = 'MCRIP';
        BytesSent                    : Core.Strings.VarString = 'MCBS';
        BytesReceived                : Core.Strings.VarString = 'MCBR';
        Transactions                 : Core.Strings.VarString = 'MCTX';
        TXFiltered                   : Core.Strings.VarString = 'MCTXF';
        PTXSent                      : Core.Strings.VarString = 'MCPTXS';
        PTXRecvd                     : Core.Strings.VarString = 'MCPTXR';
        Sessions                     : Core.Strings.VarString = 'MCCSN';
        Streams                      : Core.Strings.VarString = 'MCCS';
        MemoryTotal                  : Core.Strings.VarString = 'MCMT';
        MemoryFree                   : Core.Strings.VarString = 'MCMF';
        MemoryLoad                   : Core.Strings.VarString = 'MCML';
        UptimeYears                  : Core.Strings.VarString = 'MCUTY';
        UptimeMonths                 : Core.Strings.VarString = 'MCUTM';
        UptimeWeeks                  : Core.Strings.VarString = 'MCUTW';
        UptimeDays                   : Core.Strings.VarString = 'MCUTD';
        UptimeHours                  : Core.Strings.VarString = 'MCUTH';
        UptimeMinutes                : Core.Strings.VarString = 'MCUMN';
        UptimeSeconds                : Core.Strings.VarString = 'MCUTS';
        LocationCountry              : Core.Strings.VarString = 'MCLC';
        LocationRegion               : Core.Strings.VarString = 'MCLR';
        LocationLocality             : Core.Strings.VarString = 'MCLL';
        LocationArea                 : Core.Strings.VarString = 'MCLA';
        LocationStreet               : Core.Strings.VarString = 'MCLS';
        LocationBuilding             : Core.Strings.VarString = 'MCLB';
        LocationFloor                : Core.Strings.VarString = 'MCLF';
        LocationRoom                 : Core.Strings.VarString = 'MCLRM';
        LocationZip                  : Core.Strings.VarString = 'MCLZP';
        LocationDescription          : Core.Strings.VarString = 'MCLDS';

        // Runtime
        All                          : Core.Strings.VarString= 'All';
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Clustering';
        Name                 : 'Clusters';
        Value                : 'scs_mtx_cltrs';
        Hint                 : 'Storage for matrix based cluster information';
        PrimaryKeyP          : @Keys.ID;
      );
      Fields: array [0..31] of Core.Database.Types.Field= (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.ReportingIP; KeyP: @Keys.ReportingIP; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Streams; KeyP: @Keys.Streams; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Sessions; KeyP: @Keys.Sessions; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.BytesReceived; KeyP: @Keys.BytesReceived; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.BytesSent; KeyP: @Keys.BytesSent; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Transactions; KeyP: @Keys.Transactions; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.TXFiltered; KeyP: @Keys.TXFiltered; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.PTXSent; KeyP: @Keys.PTXSent; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.PTXRecvd; KeyP: @Keys.PTXRecvd; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.MemoryTotal; KeyP: @Keys.MemoryTotal; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.MemoryFree; KeyP: @Keys.MemoryFree; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.MemoryLoad; KeyP: @Keys.MemoryLoad; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeYears; KeyP: @Keys.UptimeYears; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeMonths; KeyP: @Keys.UptimeMonths; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeWeeks; KeyP: @Keys.UptimeWeeks; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeDays; KeyP: @Keys.UptimeDays; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeHours; KeyP: @Keys.UptimeHours; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeMinutes; KeyP: @Keys.UptimeMinutes; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.UptimeSeconds; KeyP: @Keys.UptimeSeconds; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Group; KeyP: @Keys.Group; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationCountry; KeyP: @Keys.LocationCountry; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationRegion; KeyP: @Keys.LocationRegion; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationLocality; KeyP: @Keys.LocationLocality; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationArea; KeyP: @Keys.LocationArea; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationStreet; KeyP: @Keys.LocationStreet; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationBuilding; KeyP: @Keys.LocationBuilding; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationFloor; KeyP: @Keys.LocationFloor; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationRoom; KeyP: @Keys.LocationRoom; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone),
        (IDP: @IDs.LocationZip; KeyP: @Keys.LocationZip; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone),
        (IDP: @IDs.LocationDescription; KeyP: @Keys.LocationDescription; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*3; Flags: cfNone)
      );
      class Function  Create(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
      class Function  Save(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
      class Function  UpdateStats(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
      class Function  Exists(Task:Core.Database.Types.TTask; Group:Core.Strings.VarString):Boolean; overload;
      class Function  Exists(Task:Core.Database.Types.TTask; ID:QWord):Boolean; overload;

      class Function  Fill(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; Var Entry:Cluster.Items):System.boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; ID:System.QWord; Var Entry:Cluster.Item):System.Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; ID:System.QWord):System.Boolean; overload;

      class Function  ListAsString(Task:Core.Database.Types.TTask):Core.Strings.VarString;
    end;


    class Function  IndexOf(Var List:Cluster.Items; ID:QWord): LongInt; overload;
    class Function  AliasOf(Var List:Cluster.Items; ID:QWord):Core.Strings.VarString; overload;

    class procedure Empty(Var Entry:Cluster.Item); overload;
    class procedure Empty(Var Entry:Cluster.Location); overload;
    class procedure Empty(Var Entry:Cluster.Items); overload;
    class procedure Empty(Var Entry:Cluster.Connections); overload;
    class procedure Empty(Var Entry:Cluster.Memory); overload;
    class procedure Empty(Var Entry:Cluster.Uptime); overload;

    class procedure Done(Var Entry:Cluster.Item); overload;
    class procedure Done(Var Entry:Cluster.Location); overload;
    class procedure Done(Var Entry:Cluster.Items); overload;
    class procedure Done(Var Entry:Cluster.Connections); overload;
    class procedure Done(Var Entry:Cluster.Memory); overload;
    class procedure Done(Var Entry:Cluster.Uptime); overload;

    class procedure Init(Var Entry:Cluster.Memory); overload;
    class procedure Init(Var Entry:Cluster.Connections); overload;
    class procedure Init(Var Entry:Cluster.Uptime); overload;
    class procedure Init(Var Entry:Cluster.Location); overload;
    class procedure Init(Var Entry:Cluster.Item); overload;


    class procedure Copy(Var Source,Destination:Cluster.Item); overload;
    class procedure Copy(Var Source,Destination:Cluster.Location); overload;
    class procedure Copy(Var Source,Destination:Cluster.Connections); overload;
    class procedure Copy(Var Source,Destination:Cluster.Memory); overload;
    class procedure Copy(Var Source,Destination:Cluster.Uptime); overload;

  end;



implementation

uses db;


procedure cbDestroyCluster(ItemP: Core.Database.Monitor.Types.PItem);
begin
  Core.Database.Done(Cluster.DB.TableP^);
  Cluster.DB.TableP := nil;
  Cluster.DB.MonitorP := nil;
end;

function cbDBMonitorNotifyied(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:System.QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:System.Cardinal):System.Boolean;

  procedure PushClusterDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Cluster.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.ID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.CLUSTER_DELETED : PushClusterDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  if Cluster.DB.TableP = nil then begin
    New(Cluster.DB.TableP);
    Init(Cluster.DB.TableP^, Cluster.DB.Startup);
    for iLcv := 0 to High(Cluster.DB.Fields) do
      Core.Database.AddField(@Cluster.DB.Fields[iLcv], Cluster.DB.TableP);
    if Cluster.DB.MonitorP = nil then begin
      New(Cluster.DB.MonitorP);
      Init(Cluster.DB.MonitorP^, Cluster.DB.TableP^, @cbDestroyCluster, @cbDBMonitorNotifyied);
      Core.Database.Monitor.Add(Cluster.DB.MonitorP);
    end;
  end;
end;

procedure CB_Cluster_ListAsString(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Database.Types.PVarString(DataP)^:=Concat(Core.Database.Types.PVarString(DataP)^,Fields.FieldByName(Cluster.DB.Keys.Group).AsString,#13#10);
end;

class Function  Cluster.DB.ListAsString(Task:Core.Database.Types.TTask):Core.Strings.VarString;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
 begin
  iCount:=0; Result:='';
  Try
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForFields,Cluster.DB.IDS.Group,poNone,oNone,Commands);
    Core.Database.SQL.Select(Task,@Commands,@CB_Cluster_ListAsString,@Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Cluster.DB.Exists(Task:Core.Database.Types.TTask; Group:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  Count                          : Int64;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.Group,poNone,oEqual,Group,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Cluster.DB.Exists(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
var
  iCount                         : LongInt;
  Count                          : System.Int64;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Cluster_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ClusterP:Cluster.PItem;
begin
  ClusterP:=DataP;
  {$i Storage.MatrixClusters.Fill.inc}
end;

class Function  Cluster.DB.Fill(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0; Result:=False;
  Try
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.Group,poNone,oEqual,Entry.Group,Commands);
    {$i Storage.MatrixClusters.Select.Fields.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Cluster_Fill,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Cluster.DB.Fill(Task:Core.Database.Types.TTask; ID:System.QWord; Var Entry:Cluster.Item):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0; Result:=False; Entry.ID:=ID;
  Try
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.ID,poNone,oEqual,Entry.ID,Commands);
    {$i Storage.MatrixClusters.Select.Fields.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Cluster_Fill,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Cluster_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ClustersP:Cluster.PItems;
  ClusterP:Cluster.PItem;
  iIndex:LongInt;
  iID:System.QWord;
begin
  ClustersP:=DataP;
  iID:=Fields.FieldByName(Cluster.DB.Keys.ID).AsLargeInt;
  iIndex:=Cluster.IndexOf(ClustersP^,iID);
  if iIndex=-1 then begin
    iIndex:=Length(ClustersP^);
    SetLength(ClustersP^,iIndex+1);
    New(ClusterP);
    Cluster.Init(ClusterP^);
    ClusterP^.ID:=iID;
    ClustersP^[iIndex]:=ClusterP;
  end else
    ClusterP:=ClustersP^[iIndex];
  {$i Storage.MatrixClusters.Fill.inc}
end;

class Function  Cluster.DB.Fill(Task:Core.Database.Types.TTask; Var Entry:Cluster.Items):System.boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  iCount:=0; Result:=False;
  Try
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    {$i Storage.MatrixClusters.Select.Fields.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Cluster_List,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Cluster.DB.UpdateStats(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.ID,poNone,oEqual,Entry.ID,Commands);


    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.BytesSent,poNone,oNone,Entry.BytesSent,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.BytesReceived,poNone,oNone,Entry.BytesReceived,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.Transactions,poNone,oNone,Entry.Transactions,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.TXFiltered,poNone,oNone,Entry.TXFiltered,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.PTXSent,poNone,oNone,Entry.PTXSent,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.PTXRecvd,poNone,oNone,Entry.PTXRecvd,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.Streams,poNone,oNone,Entry.Connections.Streams,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.Sessions,poNone,oNone,Entry.Connections.Sessions,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.MemoryTotal,poNone,oNone,Entry.Memory.Total,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.MemoryFree,poNone,oNone,Entry.Memory.Free,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.MemoryLoad,poNone,oNone,Entry.Memory.Load,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeYears,poNone,oNone,Entry.Uptime.Years,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeMonths,poNone,oNone,Entry.Uptime.Months,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeWeeks,poNone,oNone,Entry.Uptime.Weeks,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeDays,poNone,oNone,Entry.Uptime.Days,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeHours,poNone,oNone,Entry.Uptime.Hours,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeMinutes,poNone,oNone,Entry.Uptime.Minutes,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.UptimeSeconds,poNone,oNone,Entry.Uptime.Seconds,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Cluster.DB.Save(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.Group,poNone,oNone,Entry.Group,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationCountry,poNone,oNone,Entry.Location.Country,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationRegion,poNone,oNone,Entry.Location.Region,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationLocality,poNone,oNone,Entry.Location.Locality,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationArea,poNone,oNone,Entry.Location.Area,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationStreet,poNone,oNone,Entry.Location.Street,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationBuilding,poNone,oNone,Entry.Location.Building,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationFloor,poNone,oNone,Entry.Location.Floor,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationRoom,poNone,oNone,Entry.Location.Room,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationZip,poNone,oNone,Entry.Location.Zip,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.LocationDescription,poNone,oNone,Entry.Location.Description,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForUpdates,Cluster.DB.IDS.ReportingIP,poNone,oNone,Entry.ReportingIP,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Cluster.DB.Create(Task:Core.Database.Types.TTask; Var Entry:Cluster.Item):Boolean;
var
  iCount                         : LongInt;
  InsertID,iReset                : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; InsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,Cluster.DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForCriteria,Cluster.DB.IDS.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForPrimaryID,Cluster.DB.IDS.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForResetInsertID,Cluster.DB.IDS.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.Group,poNone,oNone,Entry.Group,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.ReportingIP,poNone,oNone,Entry.ReportingIP,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationCountry,poNone,oNone,Entry.Location.Country,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationRegion,poNone,oNone,Entry.Location.Region,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationLocality,poNone,oNone,Entry.Location.Locality,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationArea,poNone,oNone,Entry.Location.Area,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationStreet,poNone,oNone,Entry.Location.Street,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationBuilding,poNone,oNone,Entry.Location.Building,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationFloor,poNone,oNone,Entry.Location.Floor,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationRoom,poNone,oNone,Entry.Location.Room,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationZip,poNone,oNone,Entry.Location.Zip,Commands);
    Core.Database.AddCommand(iCount,Cluster.DB.TableP,useForInsert,Cluster.DB.IDS.LocationDescription,poNone,oNone,Entry.Location.Description,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Cluster.DB.Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
begin
  Core.Database.Monitor.Cascade(Task,Cluster.DB.TableP,ID,Core.Database.Monitor.Notify.CLUSTER_DELETED);
  Result:=True;
end;


class procedure Cluster.Copy(Var Source,Destination:Cluster.Item);
begin
  Destination.ID:=Source.ID;
  Destination.ReportingIP:=Source.ReportingIP;
  Destination.ReportingSocket:=Source.ReportingSocket;
  Destination.BytesSent:=Source.BytesSent;
  Destination.BytesReceived:=Source.BytesReceived;
  Destination.Transactions:=Source.Transactions;
  Destination.TXFiltered:=Source.TXFiltered;
  Destination.PTXSent:=Source.PTXSent;
  Destination.PTXRecvd:=Source.PTXRecvd;
  Destination.Connections:=Source.Connections;
  Destination.Memory:=Source.Memory;
  Destination.Uptime:=Source.Uptime;
  Destination.Location:=Source.Location;
  Destination.Group:=Source.Group;
end;

class procedure Cluster.Copy(Var Source,Destination:Cluster.Location);
begin
  Destination.Country:=Source.Country;
  Destination.Region:=Source.Region;
  Destination.Locality:=Source.Locality;
  Destination.Area:=Source.Area;
  Destination.Street:=Source.Street;
  Destination.Building:=Source.Building;
  Destination.Floor:=Source.Floor;
  Destination.Room:=Source.Room;
  Destination.Zip:=Source.Zip;
  Destination.Description:=Source.Description;
end;

class procedure Cluster.Copy(Var Source,Destination:Cluster.Connections);
begin
  Destination:=Source;
end;

class procedure Cluster.Copy(Var Source,Destination:Cluster.Memory);
begin
  Destination:=Source;
end;

class procedure Cluster.Copy(Var Source,Destination:Cluster.Uptime);
begin
  Destination:=Source;
end;

class Function  Cluster.IndexOf(Var List:Cluster.Items; ID:QWord): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=Length(List); iLcv:=0; Result:=-1;
  While (iLcv<iCount) and (Result=-1) do begin
    if (List[iLcv]<>nil) and (List[iLcv]^.ID=ID) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class Function  Cluster.AliasOf(Var List:Cluster.Items; ID:QWord):Core.Strings.VarString;
var
  iIndex:LongInt;
begin
  if ID=0 then begin
    Result:=Cluster.DB.Keys.All;
  end else begin
    SetLength(Result,0);
    iIndex:=IndexOf(List,ID);
    if iIndex<>-1 then
      Result:=List[iIndex]^.Group;
  end;
end;

class procedure Cluster.Empty(Var Entry:Cluster.Item);
begin
  Entry.ID:=0;
  Entry.ReportingIP:=0;
  Entry.ReportingSocket:=0;
  Entry.BytesSent:=0;
  Entry.BytesReceived:=0;
  Entry.Transactions:=0;
  Entry.TXFiltered:=0;
  Entry.PTXSent:=0;
  Entry.PTXRecvd:=0;
  Empty(Entry.Connections);
  Empty(Entry.Memory);
  Empty(Entry.Uptime);
  Empty(Entry.Location);
  SetLength(Entry.Group,0);
end;

class procedure Cluster.Done(Var Entry:Cluster.Item);
begin
  Done(Entry.Connections);
  Done(Entry.Memory);
  Done(Entry.Uptime);
  Done(Entry.Location);
  Finalize(Entry.Group,0);
  Finalize(Entry);
end;

class procedure Cluster.Empty(Var Entry:Cluster.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entry) do begin
    if (Entry[iLcv]<>nil) then begin
      Done(Entry[iLcv]^);
      Dispose(Entry[iLcv]);
    end;
  end;
  SetLength(Entry,0);
end;

class procedure Cluster.Empty(Var Entry:Cluster.Connections);
begin
  Entry.Streams:=0;
  Entry.Sessions:=0;
end;

class procedure Cluster.Empty(Var Entry:Cluster.Memory);
begin
  Entry.Total:=0;
  Entry.Free:=0;
  Entry.Load:=0;
end;

class procedure Cluster.Empty(Var Entry:Cluster.Uptime);
begin
  Entry.Years:=0;
  Entry.Months:=0;
  Entry.Weeks:=0;
  Entry.Days:=0;
  Entry.Hours:=0;
  Entry.Minutes:=0;
  Entry.Seconds:=0;
end;

class procedure Cluster.Done(Var Entry:Cluster.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entry) do begin
    if (Entry[iLcv]<>nil) then begin
      Done(Entry[iLcv]^);
      Dispose(Entry[iLcv]);
    end;
  end;
  Finalize(Entry);
end;

class procedure Cluster.Done(Var Entry:Cluster.Connections);
begin
  Finalize(Entry);
end;

class procedure Cluster.Done(Var Entry:Cluster.Memory);
begin
  Finalize(Entry);
end;

class procedure Cluster.Done(Var Entry:Cluster.Uptime);
begin
  Finalize(Entry);
end;

class procedure Cluster.Empty(Var Entry:Cluster.Location);
begin
  SetLength(Entry.Country,0);
  SetLength(Entry.Region,0);
  SetLength(Entry.Locality,0);
  SetLength(Entry.Area,0);
  SetLength(Entry.Street,0);
  SetLength(Entry.Building,0);
  SetLength(Entry.Floor,0);
  SetLength(Entry.Room,0);
  SetLength(Entry.Zip,0);
  SetLength(Entry.Description,0);
end;

class procedure Cluster.Done(Var Entry:Cluster.Location);
begin
  Finalize(Entry.Country);
  Finalize(Entry.Region);
  Finalize(Entry.Locality);
  Finalize(Entry.Area);
  Finalize(Entry.Street);
  Finalize(Entry.Building);
  Finalize(Entry.Floor);
  Finalize(Entry.Room);
  Finalize(Entry.Zip);
  Finalize(Entry.Description);
  Finalize(Entry);
end;


class procedure Cluster.Init(Var Entry:Cluster.Memory);
begin
  Entry.Free:=0;
  Entry.Load:=0;
  Entry.Total:=0;
end;

class procedure Cluster.Init(Var Entry:Cluster.Connections);
begin
  Entry.Sessions:=0;
  Entry.Streams:=0;
end;


class procedure Cluster.Init(Var Entry:Cluster.Uptime);
begin
  With Entry do begin
    Years:=0;
    Months:=0;
    Weeks:=0;
    Days:=0;
    Hours:=0;
    Minutes:=0;
  end;
end;

class procedure Cluster.Init(Var Entry:Cluster.Location);
begin
  With Entry do begin
    SetLength(Country,0);
    SetLength(Region,0);
    SetLength(Locality,0);
    SetLength(Area,0);
    SetLength(Street,0);
    SetLength(Building,0);
    SetLength(Floor,0);
    SetLength(Room,0);
    SetLength(Zip,0);
    SetLength(Description,0);
  end;
end;

class procedure Cluster.Init(Var Entry:Cluster.Item);
begin
  With Entry do begin
    ID:=0;
    ReportingIP:=0;
    ReportingSocket:=0;
    BytesSent:=0;
    BytesReceived:=0;
    Transactions:=0;
    TXFiltered:=0;
    PTXSent:=0;
    PTXRecvd:=0;
    Init(Connections);
    Init(Memory);
    Init(Uptime);
    Init(Location);
    SetLength(Group,0);
  end;
end;

initialization
  RegisterDB;
end.


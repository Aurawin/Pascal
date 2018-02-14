unit Storage.MatrixServices;
{
unit Storage.MatrixServices.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is protected under the Aurawin Public Release License
http://www.aurawin.com/aprl.html


Matrix Services Tell The Node:
       What Services To Run
       The Scale To Run It At
       The IP which the Service is be run
       The Port

}


interface

uses
  Classes,
  SysUtils,
  RSR,
  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Storage.Domains,
  Storage.MatrixClusters,
  Storage.MatrixNodes,
  Storage.MatrixResources,
  Storage.MatrixQueue,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.Arrays.VarString,
  Core.Strings,
  Core.Timer;


Type
  Default=class
  const
    Settings                     : QWord = 0;
    Cluster                      : QWord = 0;
    Node                         : QWord = 0;
    Resource                     : QWord = 0;
    NoScale                      : Byte  = 0;
    Enabled                      : Boolean = true;
    Disabled                     : Boolean = false;
    Domain                       : QWord = 0;
  end;

  Items=class
  Const
    FMT_CLUSTER_AND_ID = '%s=%s';

    // NOTICE: ONLY APPEND TO THIS LIST
    mkXMPPCToS                     = 0;
    mkXMPPSToS                     = 1;
    mkPOP3                         = 2;
    mkPOP3S                        = 3;
    mkSMTP                         = 4;
    mkSMTPS                        = 5;
    mkSMTPSO                       = 6;
    mkHTTP                         = 7;
    mkHTTPS                        = 8;
    mkDNS                          = 9;
    mkLDAP                         = 10;
    mkIMAP                         = 11;
    mkIMAPS                        = 12;
    mkBMail                        = 13;
    mkKeywordScanner               = 14;
    mkRealTimeStreaming            = 15;
    mkAuDisk                       = 16;
    mkAuDiskS                      = 17;

    MK_MAX = 17;

  Type
    Kind=(
      skXMPPCToS                   = 0,
      skXMPPSToS                   = 1,
      skPOP3                       = 2,
      skPOP3S                      = 3,
      skSMTP                       = 4,
      skSMTPS                      = 5,
      skSMTPSO                     = 6,
      skHTTP                       = 7,
      skHTTPS                      = 8,
      skDNS                        = 9,
      skLDAP                       = 10,
      skIMAP                       = 11,
      skIMAPS                      = 12,
      skBMail                      = 13,
      skKeywordScanner             = 14,
      skRealTimeStreaming          = 15,
      skAuDisk                     = 16,
      skAuDiskS                    = 17
    );
    Kinds=set of Kind;
    State=class
    const
      None                       = 1;
      Created                    = 2;
      Terminated                 = 3;
    end;
    (*
    MS_STATE_NONE                    = 0;
    MS_STATE_CREATED                 = 1;
    MS_STATE_TERMINATED              = 2;
    *)
  const
    RunInProcess:Kinds = [skXMPPCToS,skXMPPSToS,skPOP3,skPOP3S,skSMTP,skSMTPS,skSMTPSO,skHTTP,skHTTPS,skIMAP,skIMAPS,skAuDisk,skAuDiskS];
    RunInProcessValues:Array[0..12] of QWord=(0,1,2,3,4,5,6,7,8,11,12,16,17);
    DefaultPort:Array[0..MK_MAX] of WORD=(
      5222,  // 0
      5223,  // 1
      110,   // 2
      995,   // 3
      25,    // 4
      465,   // 5
      587,   // 6
      80,    // 7
      443,   // 8
      53,    // 9
      389,   // 10
      143,   // 11
      993,   // 12
      25,    // 13
      0,     // 14
      554,   // 15
      1121,  // 16
      1122   // 17
    );
    DefaultTimeOut:Array[0..MK_MAX] of DWORD=
      (
      1000*60*2,    // 0
      1000*60*2,    // 1
      1000*60*5,    // 2
      1000*60*5,    // 3
      1000*60*1,    // 4
      1000*60*3,    // 5
      1000*60*2,    // 6
      1000*60*2,    // 7
      1000*60*2,    // 8
      1000*60*2,    // 9
      1000*60*2,    // 10
      1000*60*2,    // 11
      1000*60*2,    // 12
      1000*60*2,    // 13
      1000*60*2,    // 14
      1000*60*2,    // 15
      1000*60*2,    // 16
      1000*60*2     // 17
    );
    mkList:Array[0..MK_MAX] of String=(
      'XMPP C2S',
      'XMPP S2S',
      'POP3',
      'POP3S',
      'SMTP',
      'SMTPS',
      'SMTPSO',
      'HTTP',
      'HTTPS',
      'DNS',
      'LDAP',
      'IMAP',
      'IMAPS',
      'BMAIL',
      'KEYWORDS',
      'RTSP',
      'AUDISK',
      'AUDISKS'
    );
    mkLongNames:Array[0..MK_MAX] of String=(
      'XMPP',
      'XMPP S2S',
      'POP3',
      'POP3 SSL',
      'SMTP',
      'SMTP SSL',
      'SMTP SSL Out',
      'HTTP',
      'HTTPS',
      'DNS',
      'LDAP',
      'IMAP',
      'IMAPS',
      'BMAIL',
      'KEYWORDS',
      'STREAMING',
      'AUDISK',
      'AUDISK SSL'
    );
    mkDescriptions:Array[0..MK_MAX] of String=(
      'Enables real-time messaging and streaming.',
      'Enables federated services between other servers.',
      'Enables post office for mail.',
      'Enables post office with security for mail.',
      'Enables e-mail message transfer server.',
      'Enables secure e-mail message transfer server.',
      'Enables secure e-mail message transfer outbound server.',
      'Enables content for the world wide web.',
      'Enables secure content for the world wide web.',
      'Enables Domain Name Server service.',
      'Enables Directory Access service.',
      'Enables email management.',
      'Enables email managment with security.',
      'Enables broadcast e-mail marketing communications.',
      'Enables high speed keyword parsing for files.',
      'Enables real-time streaming protocol.',
      'Enabled required disk services.',
      'Enabled required disk services with encryption.'
    );
    msNone                           = 0;
    msEnabled                        = 1 shl 0;
    msHung                           = 1 shl 1;
    msSuspended                      = 1 shl 2;




  Type
    // DomainID=0 is the default domain and represents the defaults for all domains.
    //   if a particular domain provides the DomainID in the query it will retrieve
    //   all services for that domain.
    // ClusterID=0 is the default value and represents the defaults for all clusters
    //   A cluster can have custom values for scale and the like by binding values
    //   to the ClusterID and again for the Domain ID.
    // NodeID=0 is the default node and represents the scale and service for each supported service in the matrix
    //   A node can have custom values for scale and the like by binding values to
    //   the ClusterID and NodeID along with the Domain too.
    // Any Service Discovery with an ID=0 will cause the system to create default settings
    //   for each of the services currently supported in mkList
    // A record in table for each DomainID,ClusterID,NodeID,Kind,IP,Port,State,Scale
    // If a node is unknown it can call "IsNodeRegistered(ClusterID,NodeID,out IP)
    //    function to see if the node is registered and the IP matches the registerd IP
    // If a
    FillOption=(foUseID,foUseCRN,foUseCNK,foUseCRNDK);
    (*
    TMatrixServiceProperty=(
      ID,
      InsertID,
      ClusterID,
      ResourceID,
      NodeID,
      DomainID,
      Kind,
      Port,
      State,
      Scale,
      Enabled,
      Ping,
      Limit
    );
    *)
    Item=Record
      ID                         : QWord;     //
      ClusterID                  : QWord;     //
      ResourceID                 : QWord;     // Resource Group
      NodeID                     : QWord;     //
      DomainID                   : QWord;     //
      Kind                       : QWord;     //
      Port                       : LongInt;   //
      State                      : Byte;      //
      Scale                      : Byte;      //
      Enabled                    : Boolean;   //
      Ping                       : Double;    // Ping Check
      // Runtime Variables
      Started                    : Boolean;
      servicePath                : Core.Strings.VarString;
      serviceParams              : Core.Arrays.Types.VarString;
      Service                    : Pointer;
      Validated                  : boolean;
    end;
    PItem=^Item;
    Items=Array of PItem;
    PItems=^Items.Items;
    Defaults=Array[0..MK_MAX] of Item;

    Manifest=Record
      Cluster                    : Storage.MatrixClusters.Cluster.Item;       // Each node as an instance of this structure
      Resource                   : Storage.MatrixResources.Resource.Item;      // Hardware/virtual Resource asset
      Node                       : Storage.MatrixNodes.Node.Item;          // Runtime
      List                       : Items.Items;   // Services hosted on this node.
    end;
    PManifest=^Manifest;
    DB=class
    type
      IDS=class
      const
        ID                       : LongInt= 0;
        InsertID                 : LongInt= 1;
        ClusterID                : LongInt= 2;
        ResourceID               : LongInt= 3;
        NodeID                   : LongInt= 4;
        DomainID                 : LongInt= 5;
        Kind                     : LongInt= 6;
        Port                     : LongInt= 7;
        State                    : LongInt= 8;
        Scale                    : LongInt= 9;
        Enabled                  : LongInt= 10;
        Ping                     : LongInt= 11;
        Limit                    : LongInt= 12;
      end;
      Keys=class
      const
        ID                       : Core.Strings.VarString= 'MSID';
        InsertID                 : Core.Strings.VarString= 'MSIID';
        ClusterID                : Core.Strings.VarString= 'MSCID';
        ResourceID               : Core.Strings.VarString= 'MSRID';
        NodeID                   : Core.Strings.VarString= 'MSNID';
        DomainID                 : Core.Strings.VarString= 'MSDID';
        Kind                     : Core.Strings.VarString= 'MSKND';
        Port                     : Core.Strings.VarString= 'MSP';
        State                    : Core.Strings.VarString= 'MSS';
        Scale                    : Core.Strings.VarString= 'MSL';
        Enabled                  : Core.Strings.VarString= 'MSE';
        Ping                     : Core.Strings.VarString= 'MSPNG';
        Limit                    : Core.Strings.VarString= 'MLMT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Clustering';
        Name                     : 'Services';
        Value                    : 'scs_mtx_svcs';
        Hint                     : 'Matrix Services storage for domains';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..12] of Core.Database.Types.Field= (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.ClusterID; KeyP: @Keys.ClusterID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.ResourceID; KeyP: @Keys.ResourceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.NodeID; KeyP: @Keys.NodeID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull),
        (IDP: @IDs.Port; KeyP: @Keys.Port; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull),
        (IDP: @IDs.State; KeyP: @Keys.State; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Scale; KeyP: @Keys.Scale; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Enabled; KeyP: @Keys.Enabled; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Ping; KeyP: @Keys.Ping; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone),
        (IDP: @IDs.Limit; KeyP: @Keys.Limit; DataType: dftInteger; AutoCreate: false; Verified: False; Precision: 0; Flags: cfNone)
      );

      class Function  Create(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID,Kind:QWord; Port:LongInt; Scale:Byte; Enabled:boolean; out ID:QWord):Boolean; overload;
      class Function  Create(Task:Core.Database.Types.TTask; Var MS:Item):Boolean; overload;

      class Function  Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID:QWord):Boolean;
      class Function  Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID:QWord; out Count:Int64):Boolean;

      class Function  Delete(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; ID:QWord):Boolean;

      class Function  GetState(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
      class Function  SetState(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;

      class Function  Save(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
      class Function  GetRandomInstance(Task:Core.Database.Types.TTask; Kind,DomainID:QWord; Var MS:Item):Boolean;

      class Function  SetScale(Task:Core.Database.Types.TTask; Var MS:Item):Boolean; overload;
      class Function  SetScale(Task:Core.Database.Types.TTask; NodeID,Kind:QWord; Scale:Byte):Boolean; overload;

      class Function  SetScale(Task:Core.Database.Types.TTask; DomainID:QWord; Var MS:Item):Boolean; overload;
      class Function  SetPort(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;

      class Function  SetEnabled(Task:Core.Database.Types.TTask; Var MS:Item):Boolean; overload;
      class Function  SetEnabled(Task:Core.Database.Types.TTask; DomainID,Kind:QWord):Boolean; overload;
      class Function  SetDisabled(Task:Core.Database.Types.TTask; DomainID,Kind:QWord):Boolean;

      class Function  SetDomainID(Task:Core.Database.Types.TTask; NodeID,Kind,DomainID:QWord):Boolean; overload;
      class Function  SetDomainID(Task:Core.Database.Types.TTask; NodeID,DomainID:QWord):Boolean; overload;

      class procedure Verify(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; var Domains:Core.Arrays.Types.LargeWord; Var Entries:Defaults); overload;
      class procedure Verify(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; Var Entries:Defaults); overload;
      class procedure Verify(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID,Kind:QWord; Port:LongInt; Scale:Byte; Enabled:boolean); overload;

      class Function  Ping(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
      class function  Runable(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; Var Entries:Items.Items):boolean; overload;

      class Function  Fill(Task:Core.Database.Types.TTask; Var Entry:Manifest; Option:FillOption):boolean; overload;
      class function  Fill(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; Var Entries:Items.Items):boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; Var Entries:Defaults; DomainID:QWord=0):boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; Var Entry:Items.Item; Option:FillOption):Boolean; overload; // All Matrix Services for All Nodes and All Domains
      class Function  Fill(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; DomainID,NodeID:QWord):Boolean; overload; // All Matrix services for a particular node.
      class Function  Fill(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; DomainID:QWord):boolean; overload; // All Matrix serives for all nodes on a domain

      class Function  GetDomains(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; Var Domains:Core.Arrays.Types.VarString):Boolean;
      class Function  Count_Clusters(Task:Core.Database.Types.TTask; DomainID:QWord):QWord;
      class Function  Count_Nodes(Task:Core.Database.Types.TTask; DomainID:QWord):QWord;

    end;

    class Function  FromLongName(sName:String): LongInt;


    class procedure getParams(var MS:Item; var List:Core.Arrays.Types.VarString);
    class procedure Copy(Var Source,Destination:Item); overload;
    class procedure Copy(Var Source,Destination:Defaults); overload;

    class procedure Invalidate(var Entries:Items.Items);

    class procedure Empty(Var Entries:Items.Items); overload;
    class procedure Empty(Var Entry:Manifest); overload;
    class procedure Empty(Var Entry:Item); overload;

    class procedure Init(Var Entries:Defaults); overload;
    class procedure Init(var Entry:Manifest); overload;
    class procedure Init(var Entry:Item); overload;
    class procedure Init(var Entries:Items.Items); overload;

    class procedure Done(var Entry:Item); overload;
    class procedure Done(var Entry:Manifest); overload;
    class procedure Done(var Entries:Items.Items); overload;
    class procedure Done(var Entries:Defaults); overload;

    class Function  IndexOf(var Entries:Items.Items; ID:QWord): LongInt; overload;
    class Function  IndexOfServiceID(Var Entry:Manifest; ID:QWord): LongInt;
    class Function  IndexOfKind(Var Entry:Manifest; DomainID,aKind:QWord): LongInt; overload;
    class Function  IndexOfDefaultService(Var Entry:Manifest; DomainID,aKind:QWord): LongInt;
    class Function  IndexOf(Var Entry:Manifest; DomainID,aKind:QWord): LongInt; overload;
    class Function  Find(Var Entry:Items.Manifest; DomainID,aKind:QWord):Items.PItem; overload;
  end;
var
  Current                          : Items.Item;

implementation
uses
  db,
  DateUtils;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    with Items.DB do begin
      if ItemP=MonitorP then begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        Finally
          Core.Database.Done(Commands);
        End;
      end;
    end;
  end;

  procedure PushClusterDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    with Items.DB do begin
      if ItemP=MonitorP then begin
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
    with Items.DB do begin
      if ItemP=MonitorP then begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,ItemID,Commands);
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
    with Items.DB do begin
      if ItemP=MonitorP then begin
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
    Core.Database.Monitor.Notify.DOMAIN_DELETED           : PushDomainDeleted;
    Core.Database.Monitor.Notify.CLUSTER_DELETED          : PushClusterDeleted;
    Core.Database.Monitor.Notify.CLUSTER_NODE_DELETED     : PushNodeDeleted;
    Core.Database.Monitor.Notify.CLUSTER_RESOURCE_DELETED : PushResourceDeleted;
  end;
end;

procedure RegisterDB;
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
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


class Function  Items.DB.Create(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID,Kind:QWord; Port:LongInt; Scale:Byte; Enabled:boolean; out ID:QWord):Boolean;
var
  iCount                         : LongInt;
  InsertID,iReset                : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; InsertID:=Random(High(Integer)); ID:=0;
    // Set Table
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Add attributes
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ClusterID,poNone,oNone,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ResourceID,poNone,oNone,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.NodeID,poNone,oNone,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Port,poNone,oNone,Port,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Scale,poNone,oNone,Scale,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Enabled,poNone,oNone,Enabled,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Create(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
begin
  Result:=Create(Task,MS.ClusterID,MS.ResourceID,MS.NodeID,MS.DomainID,MS.Kind,MS.Port,MS.Scale,MS.Enabled,MS.ID);
end;

class Function  Items.DB.Save(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MS.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Kind,poNone,oNone,MS.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Port,poNone,oNone,MS.Port,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.State,poNone,oNone,MS.State,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Scale,poNone,oNone,MS.Scale,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,MS.Enabled,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Fill_IDS(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.LargeWord.Add(Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt,Core.Arrays.Types.PLargeWord(DataP)^);
end;

procedure CB_Fill_Item(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ServiceP:Items.PItem;
begin
  ServiceP:=DataP;
  With ServiceP^ do begin
    ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    DomainID:=Fields.FieldByName(Items.DB.Keys.DomainID).AsLargeInt;
    ClusterID:=Fields.FieldByName(Items.DB.Keys.ClusterID).AsLargeInt;
    ResourceID:=Fields.FieldByName(Items.DB.Keys.ResourceID).AsLargeInt;
    NodeID:=Fields.FieldByName(Items.DB.Keys.NodeID).AsLargeInt;
    Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsLargeInt;
    Port:=Fields.FieldByName(Items.DB.Keys.Port).AsInteger;
    State:=Fields.FieldByName(Items.DB.Keys.State).AsInteger;
    Scale:=Fields.FieldByName(Items.DB.Keys.Scale).AsInteger;
    Enabled:=Fields.FieldByName(Items.DB.Keys.Enabled).AsBoolean;
    Ping:=Fields.FieldByName(Items.DB.Keys.Ping).AsFloat;
  end;
  ServiceP^.Validated:=True;
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entry:Items.Item; Option:FillOption):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushLoadClusterResourceNode;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Entry.ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,Entry.ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Entry.NodeID,Commands);
  end;

  procedure PushLoadClusterNodeKind;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Entry.ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Entry.NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Entry.Kind,Commands);
  end;

  procedure PushLoadClusterNodeDomainKind;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Entry.ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,Entry.ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Entry.NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Entry.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,Entry.DomainID,Commands);
  end;

  procedure PushLoadUseID;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
  end;

begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    case Option of
      foUseID    : PushLoadUseID;
      foUseCRN   : PushLoadClusterResourceNode;
      foUseCNK   : PushLoadClusterNodeKind;
      foUseCRNDK : PushLoadClusterNodeDomainKind;
    end;
    {$i Storage.MatrixServices.Fill.Select.inc}

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Item,@Entry);
  finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entries:Defaults; DomainID:QWord=0):boolean;
var
  iCount,iIndex                  : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Try
    for iLcv:=Low(Defaults) to High(Entries) do begin
      iCount:=0;
      Core.Database.Empty(Commands);
      Entries[iLcv].Kind:=iLcv;
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Default.Cluster,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Default.Node,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,Default.Resource,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Entries[iLcv].Kind,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);

      {$i Storage.MatrixServices.Fill.Select.inc}
      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Item,@Entries[iLcv]);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_Fill_Manifest(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iLcv                           : QWord;
  iItemIndex                     : LongInt;
  iItemID                        : QWord;
  DomainP                        : Storage.Domains.Items.PDomain;
  ServicesP                      : Items.PManifest;
  ServiceP                       : Items.PItem;
begin
  ServicesP:=DataP;
  iItemID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iItemIndex:=Items.IndexOfServiceID(ServicesP^,iItemID);
  If iItemIndex=-1 then begin
    iItemIndex:=System.Length(ServicesP^.List);
    SetLength(ServicesP^.List,iItemIndex+1);
    New(ServiceP);
    Items.Init(ServiceP^);
    ServiceP^.NodeID:=ServicesP^.Node.ID;
    ServiceP^.ID:=iItemID;
    ServicesP^.List[iItemIndex]:=ServiceP;
  end else
    ServiceP:=ServicesP^.List[iItemIndex];
  With ServiceP^ do begin
    ResourceID:=Fields.FieldByName(Items.DB.Keys.ResourceID).AsLargeInt;
    DomainID:=Fields.FieldByName(Items.DB.Keys.DomainID).AsLargeInt;
    ClusterID:=Fields.FieldByName(Items.DB.Keys.ClusterID).AsLargeInt;
    NodeID:=Fields.FieldByName(Items.DB.Keys.NodeID).AsLargeInt;
    Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsLargeInt;
    Port:=Fields.FieldByName(Items.DB.Keys.Port).AsInteger;
    State:=Fields.FieldByName(Items.DB.Keys.State).AsInteger;
    Scale:=Fields.FieldByName(Items.DB.Keys.Scale).AsInteger;
    Enabled:=Fields.FieldByName(Items.DB.Keys.Enabled).AsBoolean;
    Ping:=Fields.FieldByName(Items.DB.Keys.Ping).AsFloat;
  end;
end;

procedure CB_Fill_Items(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iLcv                           : QWord;
  iItemIndex                     : LongInt;
  iItemID                        : QWord;
  DomainP                        : Storage.Domains.Items.PDomain;
  ListP                          : Items.PItems;
  ServiceP                       : Items.PItem;
begin
  ListP:=DataP;
  iItemID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iItemIndex:=Items.IndexOf(ListP^,iItemID);
  If iItemIndex=-1 then begin
    iItemIndex:=System.Length(ListP^);
    SetLength(ListP^,iItemIndex+1);
    New(ServiceP);
    Items.Init(ServiceP^);
    ServiceP^.ID:=iItemID;
    ListP^[iItemIndex]:=ServiceP;
  end else
    ServiceP:=ListP^[iItemIndex];
  With ServiceP^ do begin
    Validated:=true;
    ResourceID:=Fields.FieldByName(Items.DB.Keys.ResourceID).AsLargeInt;
    DomainID:=Fields.FieldByName(Items.DB.Keys.DomainID).AsLargeInt;
    ClusterID:=Fields.FieldByName(Items.DB.Keys.ClusterID).AsLargeInt;
    NodeID:=Fields.FieldByName(Items.DB.Keys.NodeID).AsLargeInt;
    Kind:=Fields.FieldByName(Items.DB.Keys.Kind).AsLargeInt;
    Port:=Fields.FieldByName(Items.DB.Keys.Port).AsInteger;
    State:=Fields.FieldByName(Items.DB.Keys.State).AsInteger;
    Scale:=Fields.FieldByName(Items.DB.Keys.Scale).AsInteger;
    Enabled:=Fields.FieldByName(Items.DB.Keys.Enabled).AsBoolean;
    Ping:=Fields.FieldByName(Items.DB.Keys.Ping).AsFloat;
  end;
end;


class function  Items.DB.Fill(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; Var Entries:Items.Items):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Invalidate(Entries);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP, useForOrderBy,IDs.Kind,poNone,oAscending,Commands);
    {$i Storage.MatrixServices.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Items,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.DB.Runable(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; Var Entries:Items.Items):boolean;
var
  iCount                         : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oNotEqual,Default.Domain,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Enabled,poAnd,oEqual,Default.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Scale,poAnd,oGreaterThan,Default.NoScale,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poNone,oEqual,RunInProcessValues[0],Commands);

    for iLcv:=1 to High(RunInProcessValues) do
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poOr,oEqual,RunInProcessValues[iLcv],Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);

    {$i Storage.MatrixServices.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Items,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

//
class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entry:Manifest; Option:FillOption):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushLoadDefaults;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Default.Cluster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Default.Node,Commands);
  end;

  procedure PushClusterNodeKind;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Entry.Cluster.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Entry.Node.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Enabled,poAnd,oEqual,Default.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Scale,poAnd,oNotEqual,Default.NoScale,Commands);
  end;

  procedure PushClusterNodeDomainKind;
  const ENABLED:Boolean=true;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,Entry.Cluster.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,Entry.Resource.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Entry.Node.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,Entry.Node.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Enabled,poAnd,oEqual,Default.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Scale,poAnd,oNotEqual,Default.NoScale,Commands);
  end;

  procedure PushLoadUseID;
  begin
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,Entry.Node.ID,Commands);
  end;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    case Option of
      foUseID    : PushLoadUseID;
      foUseCNK   : PushClusterNodeKind;
      foUseCRNDK : PushClusterNodeDomainKind;
    end;
    {$i Storage.MatrixServices.Fill.Select.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Manifest,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; DomainID,NodeID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Invalidate(Entry.List);
  iCount:=0;

  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poAnd,oEqual,Default.Cluster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,Default.Resource,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,Default.Node,Commands);

    Core.Database.AddCommand(iCount,TableP, useForOrderBy,IDs.Kind,poNone,oAscending,Commands);


    {$i Storage.MatrixServices.Fill.Select.inc}
    iCount:=System.Length(Entry.List);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Manifest,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; DomainID:QWord):boolean; overload;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Invalidate(Entry.List);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    {$i Storage.MatrixServices.Fill.Select.inc}
    iCount:=System.Length(Entry.List);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Manifest,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.DB.Verify(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; Var Entries:Defaults);
var
  msItem:Item;
  iLcv:LongInt;
begin
  Init(msItem);
  Try
    for iLcv:=Low(Entries) to High(Entries) do begin
      Empty(msItem);
      msItem.NodeID:=NodeID;
      msItem.ClusterID:=ClusterID;
      msItem.ResourceID:=ResourceID;
      msItem.Kind:=Entries[iLcv].Kind;
      msItem.Port:=Entries[iLcv].Port;
      msItem.Scale:=Entries[iLcv].Scale;
      msItem.Enabled:=Entries[iLcv].Enabled;
      Fill(Task,msItem,foUseCNK);
      if (msItem.ID=0) then begin
        Create(
          Task,
          ClusterID,
          ResourceID,
          NodeID,
          Default.Domain,
          msItem.Kind,
          msItem.Port,
          msItem.Scale,
          msItem.Enabled,
          msItem.ID
        );
      end;
    end;
  finally
    Done(msItem);
  end;
end;

class procedure Items.DB.Verify(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID:QWord; var Domains:Core.Arrays.Types.LargeWord; Var Entries:Defaults);

  procedure PushProcessDomain(Const DomainID:QWord);
  var
    msItem:Item;
    iLcv:LongInt;
  begin
    Init(msItem);
    Try
      for iLcv:=Low(Entries) to High(Entries) do begin
        Empty(msItem);
        msItem.NodeID:=NodeID;
        msItem.ClusterID:=ClusterID;
        msItem.ResourceID:=ResourceID;
        msItem.DomainID:=DomainID;
        msItem.Kind:=Entries[iLcv].Kind;
        Fill(Task,msItem,foUseCRNDK);
        Copy(msItem,Entries[iLcv]);
      end;
    finally
      Done(msItem);
    end;
  end;

  procedure PushProcessList;
  var
    iLcv:LongInt;
  begin
    for iLcv:=0 to High(Domains) do
      PushProcessDomain(Domains[iLcv]);
  end;

begin
 PushProcessList;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; ID:QWord):Boolean;
var
  iCount                         : LongInt;
  Index                          : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    If Result then begin
      iCount:=Length(Entry.List);
      Index:=IndexOfServiceID(Entry,ID);
      for iLcv:=Index to iCount-2 do
        Entry.List[iLcv]:=Entry.List[iLcv+1];
      SetLength(Entry.List,iCount-1);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;



procedure CB_GetState(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Items.PItem(DataP)^.State:=Fields.FieldByName(Items.DB.Keys.State).AsInteger;
end;

class Function  Items.DB.GetState(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;

  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,MS.NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.State,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_GetState,@MS);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetEnabled(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MS.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,MS.Enabled,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.SetEnabled(Task:Core.Database.Types.TTask; DomainID,Kind:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Kind,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,Default.Enabled,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetDomainID(Task:Core.Database.Types.TTask; NodeID,Kind,DomainID:QWord):Boolean; overload;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetDomainID(Task:Core.Database.Types.TTask; NodeID,DomainID:QWord):Boolean; overload;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetDisabled(Task:Core.Database.Types.TTask; DomainID,Kind:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Kind,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,Default.Disabled,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.DB.Verify(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID,Kind:QWord; Port:LongInt; Scale:Byte; Enabled:boolean);
var
  iCount                         : LongInt;
  ID                             : QWord;
  Count                          : Int64;
  Commands                       : Core.Database.Types.Commands;
begin
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Kind,Commands);
    if (Core.Database.SQL.Count(Task,@Commands,Count) and (Count=0)) then begin
      Create(Task,ClusterID,ResourceID,NodeID,DomainID,Kind,Port,Scale,Enabled,ID);
    end else begin
      SetDomainID(Task,NodeID,Kind,DomainID);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;



procedure CB_Count_Clusters(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iID:QWord;
begin
  iID:=Fields.FieldByName(Items.DB.Keys.ClusterID).AsLargeInt;
  if Core.Arrays.LargeWord.IndexOf(iID,Core.Arrays.Types.PLargeWord(DataP)^)=-1 then
    Core.Arrays.LargeWord.Add(iID,Core.Arrays.Types.PLargeWord(DataP)^);
end;

class Function  Items.DB.Count_Clusters(Task:Core.Database.Types.TTask; DomainID:QWord):QWord;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  iaIDs                          : Core.Arrays.Types.LargeWord;
begin
  Result:=0;
  Try
    iCount:=0; Core.Arrays.LargeWord.Empty(iaIDs);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ClusterID,poNone,oNone,Commands);
    Try
      Core.Database.SQL.Select(Task,@Commands,@CB_Count_Clusters,@iaIDs);
      Result:=Length(iaIDs);
    finally
      Core.Arrays.LargeWord.Empty(iaIDs);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function   Items.DB.Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID:QWord):Boolean;
var
  iCount                         : LongInt;
  Count                          : Int64;
  Commands                       : Core.Database.Types.Commands;
begin
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NodeID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Allocated(Task:Core.Database.Types.TTask; ClusterID,ResourceID,NodeID,DomainID:QWord; out Count:Int64):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ClusterID,poNone,oEqual,ClusterID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poAnd,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NodeID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,Count) and (Count>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Count_Nodes(Task:Core.Database.Types.TTask; DomainID:QWord):QWord;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NodeID,poNone,oNone,Commands);
    Core.Database.SQL.Count(Task,@Commands,Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetState(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MS.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.State,poNone,oNone,MS.State,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Ping(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    MS.Ping:=Core.Timer.dtUT;
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MS.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Ping,poNone,oNone,MS.Ping,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.GetRandomInstance(Task:Core.Database.Types.TTask; Kind, DomainID:QWord; Var MS:Item):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  Index:LongInt;
  List:Core.Arrays.Types.LargeWord;
begin
  Result := False;
  try
    Core.Arrays.LargeWord.Init(List);
    Try
      iCount := 0;
      MS.Validated:=false;
      MS.ID:=0;
      MS.Enabled:=True;
      MS.Kind:=Kind;
      MS.Ping:=DateUtils.IncSecond(Core.Timer.dtUT,-30);
      MS.DomainID:=DomainID;
      MS.NodeID:=0;
      Core.Database.AddCommand(iCount, TableP,@Commands);

      Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Kind, poNone, oEqual, Kind, Commands);
      Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Ping, poAnd, oGreaterThan, MS.Ping, Commands);
      Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poAnd, oEqual, MS.DomainID, Commands);
      // enable for production
      // Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Enabled, poAnd, oEqual, MS.Enabled, Commands);
      // Disabled - Core.Database.AddCommand(iCount, TableP, useForLimit, Integer(matxSrvLimit), poNone, oNone, hDatabase.Singleton, Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Core.Database.SQL.Select(Task, @Commands, @CB_Fill_IDS, @List);

      iCount:=Length(List);
      if iCount>0 then begin
        Index:=Random(iCount);
        iCount:=0;
        Core.Database.Empty(Commands);
        Core.Database.AddCommand(iCount, TableP,@Commands);
        Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poNone, oEqual, List[Index], Commands);
        {$i Storage.MatrixServices.Fill.Select.inc}
        Result:=((Core.Database.SQL.Select(Task, @Commands, @CB_Fill_Item, @MS)=true) and (MS.Validated=true));
      end;
    finally
      Core.Arrays.LargeWord.Done(List);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;


class Function  Items.DB.SetScale(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MS.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Scale,poNone,oNone,MS.Scale,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetScale(Task:Core.Database.Types.TTask; DomainID:QWord; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,MS.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Scale,poNone,oNone,MS.Scale,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetScale(Task:Core.Database.Types.TTask; NodeID,Kind:QWord; Scale:Byte):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.NodeID,poNone,oEqual,NodeID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Kind,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Scale,poNone,oNone,Scale,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetPort(Task:Core.Database.Types.TTask; Var MS:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,MS.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Port,poNone,oNone,MS.Port,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_GetDomains(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.VarString.Add(Core.Arrays.Types.PVarString(DataP),Fields.FieldByName(Storage.Domains.Items.DB.Keys.Name).AsString);
end;

class Function  Items.DB.GetDomains(Task:Core.Database.Types.TTask; Var Entry:Items.Manifest; Var Domains:Core.Arrays.Types.VarString):Boolean;
var
  iCount                         : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
  ia:Core.Arrays.Types.LargeWord;
begin
  Result:=False; iCount:=0; SetLength(ia,0);
  Try
    For iLcv:=0 to High(Entry.List) do
      Core.Arrays.LargeWord.Add(Entry.List[iLcv]^.DomainID,ia);
    Try
      With Storage.Domains.Items.DB do begin
        For iLcv:=0 to High(ia) do begin
          iCount:=0;
          Core.Database.Empty(Commands);
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ia[iLcv],Commands);
          Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
          Core.Database.SQL.Select(Task,@Commands,@CB_GetDomains,@Domains);
        end;
      end;
    Finally
      Core.Database.Done(Commands);
    End;
  Finally
    Core.Arrays.LargeWord.Done(ia);
  End;
end;

class procedure Items.getParams(var MS:Item; var List:Core.Arrays.Types.VarString);
begin
  Core.Arrays.VarString.Empty(List);
  Core.Arrays.VarString.Add(@List,Concat('service=',IntToStr(MS.ID)));
  // Optional Values
  Core.Arrays.VarString.Add(@List,Concat('node=',IntToStr(MS.NodeID)));

  {
  These parameters are no longer needed
  Core.Arrays.VarString.Add(@List,Concat('cluster=',IntToStr(MS.ClusterID)));
  Core.Arrays.VarString.Add(@List,Concat('resource=',IntToStr(MS.ResourceID)));
  Core.Arrays.VarString.Add(@List,Concat('node=',IntToStr(MS.NodeID)));

  Core.Arrays.VarString.Add(@List,Concat('kind=',IntToStr(MS.Kind)));
  Core.Arrays.VarString.Add(@List,Concat('scale=',IntToStr(MS.Scale)));
  }
end;

class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.ClusterID:=Source.ClusterID;
  Destination.ResourceID:=Source.ResourceID;
  Destination.NodeID:=Source.NodeID;
  Destination.DomainID:=Source.DomainID;
  Destination.Kind:=Source.Kind;
  Destination.Port:=Source.Port;
  Destination.State:=Source.State;
  Destination.Scale:=Source.Scale;
  Destination.Enabled:=Source.Enabled;
  Destination.Started:=Source.Started;
  Destination.servicePath:=Source.servicePath;
  Core.Arrays.VarString.Copy(Source.serviceParams,Destination.serviceParams);
  Destination.Service:=Source.Service;
end;


class procedure Items.Copy(Var Source,Destination:Defaults);
var
  iLcv:LongInt;
begin
  // Fixed Length defaults
  for iLcv:=Low(Source) to High(Source) do
    Copy(Source[iLcv],Destination[iLcv]);
end;

class Function  Items.IndexOfKind(Var Entry:Manifest; DomainID,aKind:QWord): LongInt;
var
  iLcv:LongInt;
begin
  iLcv:=0; Result:=-1;
  While (iLcv<Length(Entry.List)) and (Result=-1) do begin
    If (Entry.List[iLcv]<>nil) and (Entry.List[iLcv]^.Kind=aKind) and (Entry.List[iLcv]^.DomainID=DomainID)  then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class Function  Items.IndexOfServiceID(Var Entry:Manifest; ID:QWord): LongInt;
var
  iLcv:LongInt;
begin
  iLcv:=0; Result:=-1;
  While (iLcv<Length(Entry.List)) and (Result=-1) do begin
    If (Entry.List[iLcv]<>nil) and (Entry.List[iLcv]^.ID=ID) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class procedure Items.Empty(Var Entry:Items.Manifest);
begin
  Storage.MatrixClusters.Cluster.Empty(Entry.Cluster);
  Storage.MatrixResources.Resource.Empty(Entry.Resource);
  Storage.MatrixNodes.Node.Empty(Entry.Node);
  Empty(Entry.List);
end;

class procedure Items.Empty(Var Entry:Item);
begin
  with Entry do begin
    Enabled:=false;
    ID:=0;
    ClusterID:=0;
    ResourceID:=0;
    NodeID:=0;
    DomainID:=0;
    Kind:=0;

    Port:=0;
    Scale:=0;
    Started:=false;

    If Service<>Nil then
      TRSRServer(Service).Free;

    SetLength(servicePath,0);
    SetLength(serviceParams,0);
    Service:=nil;
  end;
end;

class procedure Items.Init(var Entry:Item);
begin
  With Entry do begin
    Enabled:=false;
    ID:=0;
    ClusterID:=0;
    ResourceID:=0;
    NodeID:=0;
    DomainID:=0;
    Kind:=0;

    Port:=0;
    Scale:=0;
    Service:=nil;
    Started:=false;
    SetLength(servicePath,0);
    SetLength(serviceParams,0);
  end;
end;

class procedure Items.Init(var Entry:Manifest);
begin
  Storage.MatrixResources.Resource.Init(Entry.Resource);
  Storage.MatrixClusters.Cluster.Init(Entry.Cluster);
  Storage.MatrixNodes.Node.Init(Entry.Node);
  Init(Entry.List);
end;

class procedure Items.Init(var Entries:Items.Items);
begin
  Empty(Entries);
end;

class procedure Items.Init(Var Entries:Defaults);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    Init(Entries[iLcv]);
    Entries[iLcv].Kind:=iLcv;
    Entries[iLcv].Port:=DefaultPort[iLcv];
  end;
end;

class procedure Items.Done(var Entry:Items.Manifest);
begin
  Storage.MatrixResources.Resource.Done(Entry.Resource);
  Storage.MatrixClusters.Cluster.Done(Entry.Cluster);
  Storage.MatrixNodes.Node.Done(Entry.Node);
  Done(Entry.List);
  Finalize(Entry);
end;

class procedure Items.Done(var Entries:Defaults);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(Entries) to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Items.Done(var Entries:Items.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do begin
    if Entries[iLcv]<>nil then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
    end;
  end;
  Finalize(Entries);
end;

class procedure Items.Done(Var Entry:Item);
begin
  If Entry.Service<>nil then
    TObject(Entry.Service).Free;
  Finalize(Entry.servicePath);
  Finalize(Entry.serviceParams);
  Finalize(Entry);
end;

class procedure Items.Empty(Var Entries:Items.Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]<>nil) then begin
      Done(Entries[iLcv]^);
      Dispose(Entries[iLcv]);
    end;
  end;
  SetLength(Entries,0);
end;

class Function  Items.FromLongName(sName:String): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1; iLcv:=0;
  while (iLcv<=MK_MAX) and (Result=-1) do begin
    if SameText(sName,mkLongNames[iLcv]) then
      Result:=iLcv;
    inc(iLcv);
  end;
end;

class Function  Items.IndexOfDefaultService(Var Entry:Manifest; DomainID,aKind:QWord): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1; iLcv:=0; iCount:=Length(Entry.List);
  while (iLcv<iCount) and (Result=-1) do begin
    if ( (Entry.List<>nil) and
         (Entry.List[iLcv]^.ClusterID=Default.Cluster) and
         (Entry.List[iLcv]^.NodeID=Default.Node) and
         (Entry.List[iLcv]^.DomainID=DomainID) and
         (Entry.List[iLcv]^.Kind=aKind)
    ) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class function  Items.IndexOf(var Entries:Items.Items; ID:QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entries) do begin
    if (Entries[iLcv]<>nil) and (Entries[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class Function  Items.IndexOf(Var Entry:Manifest; DomainID,aKind:QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  For iLcv:=0 to High(Entry.List) do begin
    if (Entry.List[iLcv]<>nil) and (Entry.List[iLcv]^.DomainID=DomainID) and (Entry.List[iLcv]^.Kind=aKind) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class Function  Items.Find(Var Entry:Items.Manifest; DomainID,aKind:QWord):Items.PItem;
var
  iLcv:LongInt;
begin
  Result:=nil;
  For iLcv:=0 to High(Entry.List) do begin
    if (Entry.List[iLcv]<>nil) and (Entry.List[iLcv]^.DomainID=DomainID) and (Entry.List[iLcv]^.Kind=aKind) then begin
      Result:=Entry.List[iLcv];
      Break;
    end;
  end;
end;

class procedure Items.Invalidate(var Entries:Items.Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Entries[iLcv]^.Validated:=false;
end;

initialization
  RegisterDB;
end.


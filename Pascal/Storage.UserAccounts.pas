unit Storage.UserAccounts;
{
  Based on One Table

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}

interface

uses
  Core.Strings,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Arrays.Types,
  Core.Arrays.LargeWord,
  Core.Arrays.Bytes,

  Core.Streams,
  Core.XML,
  Core.Timer,

  Storage.Main,
  Storage.Domains,
  Storage.Roster,
  Storage.MatrixNodes,

  MD5,
  XMLRead,
  DOM,
  Classes,
  SysUtils,
  sqldb;


Const
  Megabyte                        = 1000*1000;
  Gigabyte                        = 1000*1000*1000;
  Terabyte                        = 1000*1000*1000*1000;
  UA_ANY_ACCOUNT : QWord          = 0;
  UA_ANY_DOMAIN  : QWord          = 0;
  UA_DEFAULT_ACCOUNT:Core.Strings.VarString       = 'default';

Type
  Items=class
  Type
    Kind=(akNormal,akTemporary);
    Item=record
      ID                        : Core.Database.Types.LargeWord;
      ResourceID                : Core.Database.Types.LargeWord;
      DomainID                  : Core.Database.Types.LargeWord;
      Enabled                   : Core.Database.Types.Bool;
      Modified                  : Core.Database.Types.Bool;
      Forwarding                : Core.Database.Types.Bool;
      User                      : Core.Strings.VarString;
      Password                  : Core.Strings.VarString;
      Forward                   : Core.Strings.VarString;
      Auth                      : Core.Strings.VarString;
      First                     : Core.Strings.VarString;
      Last                      : Core.Strings.VarString;
      Telephone                 : Core.Strings.VarString;
      Quota                     : Core.Database.Types.LargeWord;
      Consumption               : Core.Database.Types.LargeWord;
      Throttle                  : Core.Database.Types.LargeWord;
      FirstIP                   : Core.Database.Types.LargeInt;
      LastIP                    : Core.Database.Types.LargeInt;

      LastAccessed              : Core.Database.Types.Double;
      LastQuotaEnforce          : Core.Database.Types.Double;
      LastMessage               : Core.Database.Types.LargeWord;
      LockoutCount              : Core.Database.Types.Integer;


      Trash                     : Core.Database.Types.LargeWord;
      Inbox                     : Core.Database.Types.LargeWord;
      SpamBox                   : Core.Database.Types.LargeWord;
      OutBox                    : Core.Database.Types.LargeWord;
      SentBox                   : Core.Database.Types.LargeWord;
      ArchiveBox                : Core.Database.Types.LargeWord;
      TrashBox                  : Core.Database.Types.LargeWord;
      Devices                   : Core.Database.Types.LargeWord;
      Contact                   : Core.Database.Types.LargeWord;

      Kind                      : Kind;

      aclCoreObjects            : Core.Database.Types.LargeWordArray;
      aclCoreCommands           : Core.Database.Types.LargeWordArray;

      // Runtime
      AuraNode                  : Storage.MatrixNodes.Node.Item;
      Roster                    : System.Pointer;
      NetworkP                  : System.Pointer;
    end;
    PItem=^Item;
    UAPList=Array of PItem;
    UAList=Array of Item;
    Event=procedure(var Entry:Item) of object;
    AquireMode=(amUser,amAuth);
    TList=class;
    XML=class
    type
      Stanzas=Class
      const
        Accounts               = 'accounts';
        Account                = 'account';
      end;
      Fields=class
      const
        ID                         : Core.Database.Types.VarString = 'id';
        User                       : Core.Database.Types.VarString = 'usrn';
        FirstName                  : Core.Database.Types.VarString = 'fname';
        LastName                   : Core.Database.Types.VarString = 'lname';
        Telephone                  : Core.Database.Types.VarString = 'tel';
        Password                   : Core.Database.Types.VarString = 'pwd';
        DomainID                   : Core.Database.Types.VarString = 'did';
        ForwardEmail               : Core.Database.Types.VarString = 'fwe';
        Auth                       : Core.Database.Types.VarString = 'auth';
        Kind                       : Core.Database.Types.VarString = 'kind';
        LastAccessed               : Core.Database.Types.VarString = 'lsta';
        LastQuotaEnforce           : Core.Database.Types.VarString = 'lqe';
        LastMessage                : Core.Database.Types.VarString = 'lstm';
        FirstIP                    : Core.Database.Types.VarString = 'fip';
        LastIP                     : Core.Database.Types.VarString = 'lip';
        Quota                      : Core.Database.Types.VarString = 'qta';
        Consumption                : Core.Database.Types.VarString = 'cspn';
        Throttle                   : Core.Database.Types.VarString = 'trtl';
        SMTPForwarding             : Core.Database.Types.VarString = 'smf';
        Enabled                    : Core.Database.Types.VarString = 'end';
        LockOutCount               : Core.Database.Types.VarString = 'loc';
        Trash                      : Core.Database.Types.VarString = 'trsh';
        Inbox                      : Core.Database.Types.VarString = 'ibox';
        Spambox                    : Core.Database.Types.VarString = 'jbox';
        Outbox                     : Core.Database.Types.VarString = 'obox';
        Sentbox                    : Core.Database.Types.VarString = 'sbox';
        ArchiveBox                 : Core.Database.Types.VarString = 'abox';
        Delivered                  : Core.Database.Types.VarString = 'dbox';
        Trashbox                   : Core.Database.Types.VarString = 'tbox';
        Attachments                : Core.Database.Types.VarString = 'match';
        Devices                    : Core.Database.Types.VarString = 'devcs';
        Contact                    : Core.Database.Types.VarString = 'cid';
        coACL                      : Core.Strings.VarString = 'coACL';
        ccACL                      : Core.Strings.VarString = 'ccACL';
      end;
    end;
    DB=class
    type
      IDs=class
      const
        ID                         : Core.Database.Types.Integer = 0;
        InsertID                   : Core.Database.Types.Integer = 1;
        DomainID                   : Core.Database.Types.Integer = 2;
        UserName                   : Core.Database.Types.Integer = 3;
        Password                   : Core.Database.Types.Integer = 4;
        FirstName                  : Core.Database.Types.Integer = 5;
        LastName                   : Core.Database.Types.Integer = 6;
        Telephone                  : Core.Database.Types.Integer = 7;
        ForwardAddress             : Core.Database.Types.Integer = 8;
        LastAccessed               : Core.Database.Types.Integer = 9;
        LastQuotaEnforce           : Core.Database.Types.Integer = 10;
        LastMessage                : Core.Database.Types.Integer = 11;
        FirstIP                    : Core.Database.Types.Integer = 12;
        LastIP                     : Core.Database.Types.Integer = 13;
        Quota                      : Core.Database.Types.Integer = 14;
        Consumption                : Core.Database.Types.Integer = 15;
        Throttle                   : Core.Database.Types.Integer = 16;
        Forwarding                 : Core.Database.Types.Integer = 17;
        Enabled                    : Core.Database.Types.Integer = 18;
        LockoutCount               : Core.Database.Types.Integer = 19;
        Auth                       : Core.Database.Types.Integer = 20;
        Kind                       : Core.Database.Types.Integer = 21;
        Trash                      : Core.Database.Types.Integer = 22;
        Inbox                      : Core.Database.Types.Integer = 23;
        Trashbox                   : Core.Database.Types.Integer = 24;
        SpamBox                    : Core.Database.Types.Integer = 25;
        OutBox                     : Core.Database.Types.Integer = 26;
        SentBox                    : Core.Database.Types.Integer = 27;
        ArchiveBox                 : Core.Database.Types.Integer = 28;
        Devices                    : Core.Database.Types.Integer = 29;
        Contact                    : Core.Database.Types.Integer = 30;
        ACLCoreObjects             : Core.Database.Types.Integer = 31;
        ACLCoreCommands            : Core.Database.Types.Integer = 32;
      end;
      Keys=class
      const
        ID                         : Core.Database.Types.VarString = 'ITMID';
        InsertID                   : Core.Database.Types.VarString = 'ITMIID';
        DomainID                   : Core.Database.Types.VarString = 'DMID';
        UserName                   : Core.Database.Types.VarString = 'ITMU';
        Password                   : Core.Database.Types.VarString = 'PSWD';
        FirstName                  : Core.Database.Types.VarString = 'ITMF';
        LastName                   : Core.Database.Types.VarString = 'ITML';
        Telephone                  : Core.Database.Types.VarString = 'ITMT';
        ForwardAddress             : Core.Database.Types.VarString = 'FWDADR';
        LastAccessed               : Core.Database.Types.VarString = 'USRLA';
        LastQuotaEnforce           : Core.Database.Types.VarString = 'ULQE';
        LastMessage                : Core.Database.Types.VarString = 'USRLM';
        FirstIP                    : Core.Database.Types.VarString = 'USRFIP';
        LastIP                     : Core.Database.Types.VarString = 'USRLIP';
        Quota                      : Core.Database.Types.VarString = 'USRQTA';
        Consumption                : Core.Database.Types.VarString = 'USRCSP';
        Throttle                   : Core.Database.Types.VarString = 'USRTRL';
        Forwarding                 : Core.Database.Types.VarString = 'USRFWD';
        Enabled                    : Core.Database.Types.VarString = 'USRENA';
        LockoutCount               : Core.Database.Types.VarString = 'LOCT';
        Auth                       : Core.Database.Types.VarString = 'USRATH';
        Kind                       : Core.Database.Types.VarString = 'USKIND';
        Trash                      : Core.Database.Types.VarString = 'TRASH';
        Inbox                      : Core.Database.Types.VarString = 'INBOX';
        Trashbox                   : Core.Database.Types.VarString = 'TRSBOX';
        SpamBox                    : Core.Database.Types.VarString = 'SPBOX';
        OutBox                     : Core.Database.Types.VarString = 'OTBOX';
        SentBox                    : Core.Database.Types.VarString = 'STBOX';
        ArchiveBox                 : Core.Database.Types.VarString = 'ARBOX';
        Devices                    : Core.Database.Types.VarString = 'DEVCS';
        Contact                    : Core.Database.Types.VarString = 'UICTID';
        ACLCoreObjects             : Core.Database.Types.VarString = 'ACLCOS';
        ACLCoreCommands            : Core.Database.Types.VarString = 'ACLCDS';
      end;
    Const
      TableP                 : Core.Database.Types.PTable = nil;
      MonitorP               : Core.Database.Monitor.Types.PItem = nil;
      Startup                : Core.Database.Types.TableIni=(
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Domains/Users/Accounts';
        Name                 : 'Registered';
        Value                : 'scs_accts';
        Hint                 : 'Storage of user accounts';
        PrimaryKeyP          : @Keys.ID;
      );
      Fields:Array [0..32] of Core.Database.Types.Field=(
        (IDP:@IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP:@IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull; ),
        (IDP:@IDs.UserName; KeyP: @Keys.UserName; DataType:dftString; AutoCreate:True; Verified:False; Precision:50; Flags: cfNotNull; ),
        (IDP:@IDs.Password; KeyP: @Keys.Password; DataType:dftString; AutoCreate:True; Verified:False; Precision:20; Flags: cfNone; ),
        (IDP:@IDs.FirstName; KeyP: @Keys.FirstName; DataType:dftString; AutoCreate:True; Verified:False; Precision:20; Flags: cfNone; ),
        (IDP:@IDs.LastName; KeyP: @Keys.LastName; DataType:dftString; AutoCreate:True; Verified:False; Precision:20; Flags: cfNone; ),
        (IDP:@IDs.Telephone; KeyP: @Keys.Telephone; DataType:dftString; AutoCreate:True; Verified:False; Precision:20; Flags: cfNone; ),
        (IDP:@IDs.ForwardAddress; KeyP: @Keys.ForwardAddress; DataType:dftString; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP:@IDs.LastAccessed; KeyP: @Keys.LastAccessed; DataType:dftDateTime; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.LastQuotaEnforce; KeyP: @Keys.LastQuotaEnforce; DataType:dftDateTime; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.LastMessage; KeyP: @Keys.LastMessage; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.FirstIP; KeyP: @Keys.FirstIP; DataType:dftInt64; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.LastIP; KeyP: @Keys.LastIP; DataType:dftInt64; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Quota; KeyP: @Keys.Quota; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Consumption; KeyP: @Keys.Consumption; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Throttle; KeyP: @Keys.Throttle; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Forwarding; KeyP: @Keys.Forwarding; DataType:dftBoolean; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Enabled; KeyP: @Keys.Enabled; DataType:dftBoolean; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.LockoutCount; KeyP: @Keys.LockoutCount; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Auth; KeyP: @Keys.Auth; DataType:dftString; AutoCreate:True; Verified:False; Precision:32; Flags: cfNone; ),
        (IDP:@IDs.Kind; KeyP: @Keys.Kind; DataType:dftInteger; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Trash; KeyP: @Keys.Trash; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Inbox; KeyP: @Keys.Inbox; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Trashbox; KeyP: @Keys.Trashbox; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Spambox; KeyP: @Keys.Spambox; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Outbox; KeyP: @Keys.Outbox; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Sentbox; KeyP: @Keys.Sentbox; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.ArchiveBox; KeyP: @Keys.ArchiveBox; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Devices; KeyP: @Keys.Devices; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.Contact; KeyP: @Keys.Contact; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP:@IDs.ACLCoreObjects; KeyP: @Keys.ACLCoreObjects; DataType:dftQWordArray; AutoCreate:True; Verified:False; Precision:1024*1024*4; Flags: cfNone; ),
        (IDP:@IDs.ACLCoreCommands; KeyP: @Keys.ACLCoreCommands; DataType:dftQWordArray; AutoCreate:True; Verified:False; Precision:1024*1024*4; Flags: cfNone; )
      );

      class Function  Find(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; Var Account:Core.Strings.VarString):Boolean; overload;
      class Function  Find(Task:Core.Database.Types.TTask; Accounts:TList; DomainID:QWord; sLike:Core.Strings.VarString):boolean; overload;
      class Function  Init_Root(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
      class Function  Throttle(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; out Limit:QWord):boolean;

      class Function  Delete(Task:Core.Database.Types.TTask; DomainID,UserID:QWord):Boolean;
      class Function  Exists(Task:Core.Database.Types.TTask; Var UA:Item):Boolean; overload;
      class Function  Exists(Task:Core.Database.Types.TTask; Var User:Core.Strings.VarString; DomainID:QWord):Boolean; overload;
      class Function  Exists(Task:Core.Database.Types.TTask; Var User,Domain:Core.Strings.VarString) : Boolean; overload;
      class Function  Authenticates(Task:Core.Database.Types.TTask; Var Auth:Core.Strings.VarString; DomainID:QWord; Var UA:Item):Boolean; overload;
      class Function  Authenticates(Task:Core.Database.Types.TTask; Var User,Password:Core.Strings.VarString; DomainID:QWord):Boolean; overload;
      class Function  LastIPMatches(Task:Core.Database.Types.TTask; IP:QWord; Var User:Core.Strings.VarString; DomainID:QWord):Boolean;
      class Function  UpdateConsumption(Task:Core.Database.Types.TTask; var DomainID,UserID,Consumption:QWord):Boolean;
      class Function  Authorized(Task:Core.Database.Types.TTask; var DomainID:QWord; var UAP:PItem; Accounts:TList; var AuthCount:LongInt; var Username,Auth:Core.Strings.VarString; out Credentials:Pointer):Boolean;

      class Function  UpdateMods(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
      class Function  UpdateACLs(Task:Core.Database.Types.TTask; var UA:Item):Boolean;

      class Function  SetLastIP(Task:Core.Database.Types.TTask; ID,LastIP:QWord):Boolean;
      class Function  SetAuth(Task:Core.Database.Types.TTask; Var UA:Item):Boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; ID:QWord; Var UA:Item):Boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; Name:Core.Strings.VarString; Var UA:Item):Boolean; overload;
      class Function  Fill_Auth(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
      class Function  Count(Task:Core.Database.Types.TTask; DomainID:QWord):QWord;
      class Function  Save(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
      class Function  SetLockOutCount(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; LockCount:LongInt):Boolean;
      class Function  Write(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
      class Function  SetContact(Task:Core.Database.Types.TTask; DomainID,UserID,ContactID:QWord):Boolean;
      class Function  SetTrash(Task:Core.Database.Types.TTask; DomainID,UserID,Trash:QWord):Boolean;
      class Function  SetSpamBox(Task:Core.Database.Types.TTask; DomainID,UserID,SpamBox:QWord):Boolean;
      class Function  SetInBox(Task:Core.Database.Types.TTask; DomainID,UserID,InBox:QWord):Boolean;
      class Function  SetOutBox(Task:Core.Database.Types.TTask; DomainID,UserID,OutBox:QWord):Boolean;
      class Function  SetSentBox(Task:Core.Database.Types.TTask; DomainID,UserID,SentBox:QWord):Boolean;
      class Function  SetTrashBox(Task:Core.Database.Types.TTask; DomainID,UserID,TrashBox:QWord):Boolean;
      class Function  SetArchiveBox(Task:Core.Database.Types.TTask; DomainID,UserID,ArchiveBox:QWord):Boolean;
      class Function  Create(Task:Core.Database.Types.TTask; DomainID:QWord; Var UA:Item; var Contact:Storage.Roster.Items.Item):Boolean; overload;
      class Function  Create(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain):Boolean; overload; // Create Root Account
      class Function  CreateDefault(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain):Boolean;
    end;
    Cost=record
      Limit                    : Core.Database.Types.LargeWord;
      Price                    : Core.Database.Types.Double;
    end;
    PUAACLs=^TUAACLs;
    TUAACLs=record
      Objects                    : Core.Database.Types.PLargeWordArray;
      Commands                   : Core.Database.Types.PLargeWordArray;
    end;
    Throttle=class
    const
      Level1    : Cost=(Limit : 28*Megabyte; Price: 0.0;);
      Level2    : Cost=(Limit : 56*Megabyte; Price: 0.50;);
      Level3    : Cost=(Limit : 128*Megabyte; Price: 0.75;);
      Level4    : Cost=(Limit : 256*Megabyte; Price: 1.00;);
      Level5    : Cost=(Limit : 512*Megabyte; Price: 2.00;);
      Level6    : Cost=(Limit : 1 * Gigabyte; Price: 4.00;);
      Level7    : Cost=(Limit : 2 * Gigabyte; Price: 8.00;);
    end;
    TList=class
    private
      Service      : Core.Strings.VarString;
      FDomainP     : Storage.Domains.Items.PDomain;
      FList        : TThreadList;
    public
      Function  Acquire(Data:Core.Strings.VarString; Const Mode:AquireMode=amUser):PItem; overload;
      Function  Acquire(ID:QWord):PItem; overload;
    public
      Function  Add(Data:Core.Strings.VarString; Const Mode:AquireMode=amUser):PItem; overload;
      Function  Add(ID:QWord):PItem; overload;
      Function  Find(Data:Core.Strings.VarString; Const Mode:AquireMode=amUser):PItem; overload;
      Function  Find(ID:QWord):PItem; overload;
      Function  List(Var Entries:UAPList): LongInt;
      procedure Reset(aDomainP:Storage.Domains.Items.PDomain);
      procedure Clear();
    public
      Constructor Create(ADomainP:Storage.Domains.Items.PDomain; AService:Core.Strings.VarString);  Reintroduce;
      Destructor  Destroy; Override;
    end;

    class procedure SetAuth(var UA:Item); overload;
    class procedure Init(Var Entry:Item); overload;

    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:UAList); overload;
    class procedure Empty(Var Entries:UAPList); overload;

    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:UAList); overload;
    class procedure Done(Var Entries:UAPList); overload;

    class function  IndexOf(Var Entries:UAList; ID:QWord): LongInt; overload;
    class procedure Copy(Var Source,Destination:Item); overload;

    class function fromXML(xDoc:TXMLDocument; var Entry:Item; DomainID,UserID:QWord):boolean;
    class function toXML(var Entry:Item; Output:TMemoryStream; Header:boolean):boolean;
  end;


implementation
uses

  RSR.Core,
  Storage.CoreObjects,
  Storage.AuraDisks,
  Storage.UserStorage,
  Storage.Social,
  Storage.Social.Network,

  db;


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
        SetLength(Commands,0);
      End;
    end;
  end;

  procedure PushUserDeleted;
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
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted;
  end;
end;

procedure cbDestroyAccounts(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDBM;
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
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyAccounts,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Items.Init(Var Entry:Item);
begin
  Entry.ResourceID:=0;
  Entry.ID:=0;
  Entry.DomainID:=0;
  Entry.Enabled:=False;
  Entry.Modified:=False;
  Entry.Forwarding:=False;
  Core.Strings.Init(Entry.User);
  Core.Strings.Init(Entry.Password);
  Core.Strings.Init(Entry.Forward);
  Core.Strings.Init(Entry.Auth);
  Core.Strings.Init(Entry.First);
  Core.Strings.Init(Entry.Last);
  Core.Strings.Init(Entry.Telephone);
  Entry.Quota:=0;
  Entry.Consumption:=0;
  Entry.Throttle:=0;
  Entry.LastIP:=0;
  Entry.LastAccessed:=0;
  Entry.LastMessage:=0;
  Entry.LockoutCount:=0;
  Entry.Inbox:=0;
  Entry.Spambox:=0;
  Entry.Outbox:=0;
  Entry.SentBox:=0;
  Entry.ArchiveBox:=0;

  Entry.Trash:=0;
  Entry.Trashbox:=0;

  Entry.Devices:=0;
  Entry.Contact:=0;
  Core.Arrays.LargeWord.Init(Entry.aclCoreCommands);
  Core.Arrays.LargeWord.Init(Entry.aclCoreObjects);
  Storage.MatrixNodes.Node.Init(Entry.AuraNode);
  Entry.Roster:=Nil;
  Entry.NetworkP:=Nil;
end;


class procedure Items.Empty(Var Entry:Item);
begin
  Entry.ResourceID:=0;
  Entry.ID:=0;
  Entry.DomainID:=0;
  Entry.Enabled:=False;
  Entry.Modified:=False;
  Entry.Forwarding:=False;
  Core.Strings.Empty(Entry.User);
  Core.Strings.Empty(Entry.Password);
  Core.Strings.Empty(Entry.Forward);
  Core.Strings.Empty(Entry.Auth);
  Core.Strings.Empty(Entry.First);
  Core.Strings.Empty(Entry.Last);
  Core.Strings.Empty(Entry.Telephone);
  Entry.Quota:=0;
  Entry.Consumption:=0;
  Entry.Throttle:=0;
  Entry.LastIP:=0;
  Entry.LastAccessed:=0;
  Entry.LastMessage:=0;
  Entry.LockoutCount:=0;
  Entry.Inbox:=0;
  Entry.Spambox:=0;
  Entry.Outbox:=0;
  Entry.SentBox:=0;
  Entry.ArchiveBox:=0;

  Entry.Trash:=0;
  Entry.Trashbox:=0;

  Entry.Contact:=0;
  Entry.Devices:=0;
  If (Entry.Roster<>Nil) then
    Storage.Roster.Items.Empty(Storage.Roster.Items.PItem(Entry.Roster)^);
  Storage.MatrixNodes.Node.Empty(Entry.AuraNode);
  Core.Arrays.LargeWord.Empty(Entry.aclCoreObjects);
  Core.Arrays.LargeWord.Empty(Entry.aclCoreCommands);

  Entry.NetworkP:=Nil;
end;

class procedure Items.Done(Var Entry:Item);
begin
  Finalize(Entry.User);
  Finalize(Entry.Password);
  Finalize(Entry.Forward);
  Finalize(Entry.Auth);
  Finalize(Entry.First);
  Finalize(Entry.Last);
  Finalize(Entry.Telephone);
  If Entry.Roster<>Nil then begin
    Storage.Roster.Items.Done(Storage.Roster.Items.PItem(Entry.Roster)^);
    Dispose(Storage.Roster.Items.PItem(Entry.Roster));
    Entry.Roster:=nil;
  end;
  Storage.MatrixNodes.Node.Done(Entry.AuraNode);
  Core.Arrays.LargeWord.Done(Entry.aclCoreObjects);
  Core.Arrays.LargeWord.Done(Entry.aclCoreCommands);
  Finalize(Entry);
end;

class procedure Items.Done(Var Entries:UAList);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Items.Done(var Entries:UAPList);
begin
  Finalize(Entries);
end;

class procedure Items.Empty(Var Entries:UAList);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Empty(Var Entries:UAPList);
begin
  SetLength(Entries,0);
end;

class function  Items.IndexOf(Var Entries:UAList; ID:QWord): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1; iCount:=Length(Entries); iLcv:=0;
  while (iLcv<iCount) and (Result=-1) do begin
    if (Entries[iLcv].ID=ID) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Constructor Items.TList.Create(ADomainP:Storage.Domains.Items.PDomain; AService:Core.Strings.VarString);
begin
  Service:=AService;
  FDomainP:=ADomainP;
  FList:=TThreadList.Create();
  Inherited Create;
end;

Destructor  Items.TList.Destroy;
begin
  Clear();
  FreeAndNil(FList);
  Inherited Destroy;
end;

procedure   Items.TList.Clear;
var
  Lst:Classes.TList;
  iLcv:LongInt;
  UAP:PItem;
begin
  Lst:=FList.LockList();
  Try
    For iLcv:=0 to Lst.Count-1 do begin
      UAP:=Lst[iLcv];
      Done(UAP^);
      Dispose(UAP);
    end;
    Lst.Clear();
  finally
    FList.UnlockList();
  end;
end;

Function  Items.TList.Add(Data:Core.Strings.VarString; Const Mode:AquireMode=amUser):PItem;
Var
  UAP:PItem;
  Lst:Classes.TList;
begin
  Result:=Nil;
  UAP:=nil;
  New(UAP);
  Init(UAP^);
  Lst:=FList.LockList();
  Try
    Lst.Add(UAP);
  finally
    FList.UnlockList();
  end;
  Empty(UAP^);
  UAP^.ID:=0;
  UAP^.DomainID:=FDomainP^.ID;
  Case Mode of
    amUser   : UAP^.User:=Data;
    amAuth   : UAP^.Auth:=Data;
  End;
  Result:=UAP;
end;

Function    Items.TList.Add(ID:QWord):PItem;
Var
  UAP:PItem;
  Lst:Classes.TList;
begin
  Result:=Nil;
  UAP:=nil;
  New(UAP);
  Init(UAP^);
  Lst:=FList.LockList();
  Try
    Lst.Add(UAP);
  finally
    FList.UnlockList();
  end;
  Empty(UAP^);
  UAP^.ID:=ID;
  UAP^.DomainID:=FDomainP^.ID;
  Result:=UAP;
end;

Function    Items.TList.Find(Data:Core.Strings.VarString; Const Mode:AquireMode=amUser):PItem;
var
  lst:Classes.TList;

  procedure PushUserMode;
  var
    UAP:PItem;
    iLcv:LongInt;
  begin
    for iLcv:=0 to lst.Count-1 do begin
      If PItem(lst[iLcv])^.User=Data then begin
        Result:=lst[iLcv];
        Break;
      end;
    end;
  end;

  procedure PushAuthMode;
  var
    UAP:PItem;
    iLcv:LongInt;
  begin
    for iLcv:=0 to lst.Count-1 do begin
      If PItem(lst[iLcv])^.Auth=Data then begin
        Result:=lst[iLcv];
        Break;
      end;
    end;
  end;
begin
  Result:=Nil;
  lst:=FList.LockList();
  Try
    Case Mode of
      amUser : PushUserMode;
      amAuth : PushAuthMode;
    End;
  finally
    FList.UnlockList();
  end;
end;

Function    Items.TList.Find(ID:QWord):PItem;
var
  iLcv:LongInt;
  lst:Classes.TList;
begin
  Result:=Nil;
  lst:=FList.LockList();
  Try
    for iLcv:=0 to lst.Count-1 do begin
      If PItem(lst[iLcv])^.ID=ID then begin
        Result:=lst[iLcv];
        Break;
      end;
    end;
  finally
    FList.UnlockList();
  end;
end;

Function    Items.TList.List(Var Entries:UAPList): LongInt;
var
  lst:Classes.TList;
  iLcv:LongInt;
begin
  SetLength(Entries,0);
  Result:=0;
  lst:=FList.LockList();
  Try
    SetLength(Entries,lst.Count);
    for iLcv:=0 to lst.Count-1 do
      Entries[iLcv]:=lst[iLcv];
  finally
    FList.UnlockList();
  end;
end;

procedure   Items.TList.Reset(aDomainP:Storage.Domains.Items.PDomain);
begin
  If FDomainP<>aDomainP then begin
    Clear();
    FDomainP:=aDomainP;
  end;
end;

Function    Items.TList.Acquire(ID:QWord):PItem;
begin
  Result:=Find(ID);
  if Result=nil then
    Result:=Add(ID);
  Result^.ID:=ID;
  Result^.DomainID:=FDomainP^.ID;
end;

Function    Items.TList.Acquire(Data:Core.Strings.VarString; Const Mode:AquireMode=amUser):PItem;
  procedure PushAuthMode;
  begin
    Result:=Find(Data,Mode);
    if Result=Nil then
      Result:=Add(Data,Mode);
    Result^.DomainID:=FDomainP^.ID;
  end;

  procedure PushUserMode;
  begin
    Result:=Find(Data,Mode);
    if Result=Nil then
      Result:=Add(Data,Mode);
    Result^.DomainID:=FDomainP^.ID;
  end;
begin
  Case Mode of
    amUser   : PushUserMode;
    amAuth   : PushAuthMode;
  End;
end;

class Function Items.DB.Create(Task:Core.Database.Types.TTask; DomainID:QWord; Var UA:Item; var Contact:Storage.Roster.Items.Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  InsertID,Reset:QWord;
  Route:Storage.AuraDisks.Router.TItem;
begin
  Result:=False;
  iCount:=0; Reset:=0; UA.Inbox:=0; UA.Trash:=0; UA.Trashbox:=0;
  UA.Quota:=Storage.AuraDisks.Quota.Strata.Level1.Limit;
  UA.Throttle:=Storage.UserAccounts.Items.Throttle.Level1.Limit;
  Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreObjects);
  Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreCommands);

  InsertID:=Random(High(Integer));
  UA.DomainID:=DomainID;
  Items.SetAuth(UA);
  Try
    // First Setup Table, Reset and Primary IDs
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,UA.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,Reset,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,UA.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserName,poNone,oNone,UA.User,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Password,poNone,oNone,UA.Password,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Auth,poNone,oNone,UA.Auth,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.FirstName,poNone,oNone,UA.First,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastName,poNone,oNone,UA.Last,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Telephone,poNone,oNone,UA.Telephone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ForwardAddress,poNone,oNone,UA.Forward,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastAccessed,poNone,oNone,UA.LastAccessed,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastMessage,poNone,oNone,UA.LastMessage,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastIP,poNone,oNone,UA.LastIP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Quota,poNone,oNone,UA.Quota,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Consumption,poNone,oNone,UA.Consumption,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Forwarding,poNone,oNone,UA.Forwarding,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Enabled,poNone,oNone,UA.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LockoutCount,poNone,oNone,UA.LockoutCount,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Throttle,poNone,oNone,UA.Throttle,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ACLCoreObjects,poNone,oNone,UA.aclCoreObjects,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ACLCoreCommands,poNone,oNone,UA.aclCoreCommands,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

    if Result then begin
      Contact.UserID:=UA.ID;
      Contact.DomainID:=DomainID;

      Storage.Roster.Items.DB.Add(Task,DomainID,UA.ID,UA.ID,Contact);
      Storage.AuraDisks.Router.Allocate(Task,DomainID,UA.ID,Storage.AuraDisks.Kinds.User,UA.AuraNode,Route);
      Try
        if Storage.UserStorage.Folders.DB.CreateDefaults(Task,DomainID,UA.ID,UA.Trash,UA.Inbox,UA.Spambox,UA.Outbox,UA.Sentbox,UA.ArchiveBox,UA.Trashbox,UA.Devices) then begin
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UA.ID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trash,poNone,oNone,UA.Trash,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Inbox,poNone,oNone,UA.Inbox,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Spambox,poNone,oNone,UA.Spambox,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Outbox,poNone,oNone,UA.Outbox,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Sentbox,poNone,oNone,UA.Sentbox,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trashbox,poNone,oNone,UA.Trashbox,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Devices,poNone,oNone,UA.Devices,Commands);
          Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Contact,poNone,oNone,Contact.ID,Commands);
          Result:=Core.Database.SQL.Update(Task,@Commands);
        end;
      finally
        Storage.AuraDisks.Router.Done(Route);
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Create(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain):Boolean; // Create Root Account
Const
  YES:boolean=true;
  NO:boolean=false;
  ZERO:LongInt=0;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  TrashID,InboxID,SpamboxID,OutBoxID,SentBoxID,DeliveredID,TrashboxID,
  ArchiveBoxID,AttachID,DevicesID,RootID,InsertID,Reset:QWord;
  User:Item;
  Route:Storage.AuraDisks.Router.TItem;
  Contact:Storage.Roster.Items.Item;
begin
  Result:=False;
  Try
    iCount:=0; Reset:=0; InboxID:=0; TrashID:=0; TrashboxID:=0; AttachID:=0; SpamboxID:=0; DeliveredID:=0;
    InsertID:=Random(High(Integer));
    // First Setup Table, Reset and Primary IDs
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainP^.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,Reset,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserName,poNone,oNone,DomainP^.Root,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Enabled,poNone,oNone,YES,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LockoutCount,poNone,oNone,ZERO,Commands);

    Init(User);
    Try
      Grant(CoreObjectItems,@User); // to do add command defaults here too!!!
      Core.Arrays.LargeWord.RemoveAll(0,User.aclCoreObjects);
      Core.Arrays.LargeWord.RemoveAll(0,User.aclCoreCommands);

      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ACLCoreObjects,poNone,oNone,User.aclCoreObjects,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ACLCoreCommands,poNone,oNone,User.aclCoreCommands,Commands);
      Result:=Core.Database.SQL.Insert(Task,@Commands);
      If Result then begin
        Storage.Roster.Items.Init(Contact);
        Try
          with Contact do begin
            UserID:=RootID;
            DomainID:=DomainP^.ID;
            ResourceID:=0;
            AccountID:=RootID;
            Email1:=Concat(DomainP^.Root,'@',DomainP^.Name);
          end;
          Storage.Roster.Items.DB.Add(Task,DomainP^.ID,RootID,RootID,Contact);
          Storage.AuraDisks.Router.Allocate(Task,DomainP^.ID,RootID,Storage.AuraDisks.Kinds.User,User.AuraNode,Route);
          Try
            if Storage.UserStorage.Folders.DB.CreateDefaults(Task,DomainP^.ID,RootID,TrashID,InboxID,SpamboxID,OutboxID,SentBoxID,ArchiveBoxID,TrashboxID,DevicesID) then begin
              iCount:=0;
              Core.Database.AddCommand(iCount,TableP,@Commands);
              Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainP^.ID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,RootID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trash,poNone,oNone,TrashID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Inbox,poNone,oNone,InboxID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Spambox,poNone,oNone,SpamBoxID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Outbox,poNone,oNone,OutBoxID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Sentbox,poNone,oNone,SentBoxID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trashbox,poNone,oNone,TrashboxID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Devices,poNone,oNone,DevicesID,Commands);
              Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Contact,poNone,oNone,Contact.ID,Commands);
              Result:=Core.Database.SQL.Update(Task,@Commands);
            end;
          finally
            Storage.AuraDisks.Router.Done(Route);
          end;
        finally
          Storage.Roster.Items.Done(Contact);
        end;
      end;
    finally
      Done(User);
    end;

  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.CreateDefault(Task:Core.Database.Types.TTask; DomainP:Storage.Domains.Items.PDomain):Boolean;
Const
  YES:boolean=true;
  NO:boolean=false;
  ZERO:LongInt=0;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  Route:Storage.AuraDisks.Router.TItem;
  UserID,InsertID,Reset:QWord;
  User:Item;
begin
  Result:=False;
  Try
    iCount:=0; Reset:=0;
    Init(User);
    Try
      User.User:=UA_DEFAULT_ACCOUNT;
      InsertID:=Random(High(Integer));
      // First Setup Table, Reset and Primary IDs
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainP^.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,InsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,InsertID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,Reset,Commands);
      Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,User.ID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserName,poNone,oNone,User.User,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Enabled,poNone,oNone,NO,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LockoutCount,poNone,oNone,ZERO,Commands);

      Grant(CoreObjectItems,@User);
      Core.Arrays.LargeWord.RemoveAll(0,User.aclCoreObjects);
      Core.Arrays.LargeWord.RemoveAll(0,User.aclCoreCommands);

      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ACLCoreObjects,poNone,oNone,User.aclCoreObjects,Commands);
      Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ACLCoreCommands,poNone,oNone,User.aclCoreCommands,Commands);
      Result:=Core.Database.SQL.Insert(Task,@Commands);

      If Result then begin
        Storage.AuraDisks.Router.Allocate(Task,DomainP^.ID,User.ID,Storage.AuraDisks.Kinds.User,User.AuraNode,Route);
        Try
          if Storage.UserStorage.Folders.DB.CreateDefaults(Task,DomainP^.ID,User.ID,User.Trash,User.Inbox,User.Spambox,User.OutBox,User.SentBox,User.ArchiveBox,User.Trashbox,User.Devices) then begin
            iCount:=0;
            Core.Database.AddCommand(iCount,TableP,@Commands);
            Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainP^.ID,Commands);
            Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,User.ID,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trash,poNone,oNone,User.Trash,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Inbox,poNone,oNone,User.Inbox,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trashbox,poNone,oNone,User.Trashbox,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Spambox,poNone,oNone,User.SpamBox,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Outbox,poNone,oNone,User.Outbox,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Sentbox,poNone,oNone,User.Sentbox,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Archivebox,poNone,oNone,User.Archivebox,Commands);
            Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Devices,poNone,oNone,User.Devices,Commands);
            Result:=Core.Database.SQL.Update(Task,@Commands);
          end;
        finally
          Storage.AuraDisks.Router.Done(Route);
        end;
      end;
    finally
      Done(User);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID,UserID:QWord):Boolean;
begin
  Result:=True;
  Core.Database.Monitor.Cascade(Task,TableP,UserID,Core.Database.Monitor.Notify.USER_DELETED);
end;

class Function  Items.DB.Init_Root(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
begin
  With UA do begin
    User:='postmaster';
    SetLength(Password,0);
    SetLength(Forward,0);
    Enabled:=True;
    Modified:=True;
    Forwarding:=False;
    Quota:=0;
    Throttle:=Items.Throttle.Level1.Limit;
    Consumption:=0;
    LastIP:=0;
    LastAccessed:=Core.Timer.dtUT;
    LastMessage:=0;
    LockoutCount:=0;
  end;
  Result:=True;
end;

procedure CB_Fill_Auth(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  With Items.PItem(DataP)^ do begin
    ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
    LastAccessed:=Fields.FieldByName(Items.DB.Keys.LastAccessed).AsFloat;
    LastQuotaEnforce:=Fields.FieldByName(Items.DB.Keys.LastQuotaEnforce).AsFloat;
    LastMessage:=Fields.FieldByName(Items.DB.Keys.LastMessage).AsInteger;
    LastIP:=Fields.FieldByName(Items.DB.Keys.LastIP).AsLargeInt;
    Inbox:=Fields.FieldByName(Items.DB.Keys.Inbox).AsLargeInt;
    Spambox:=Fields.FieldByName(Items.DB.Keys.Spambox).AsLargeInt;
    Outbox:=Fields.FieldByName(Items.DB.Keys.Outbox).AsLargeInt;
    Sentbox:=Fields.FieldByName(Items.DB.Keys.Sentbox).AsLargeInt;
    Devices:=Fields.FieldByName(Items.DB.Keys.Devices).AsLargeInt;
    Throttle:=Fields.FieldByName(Items.DB.Keys.Throttle).AsLargeInt;
    Quota:=Fields.FieldByName(Items.DB.Keys.Quota).AsLargeInt;
    Consumption:=Fields.FieldByName(Items.DB.Keys.Consumption).AsLargeInt;
    Contact:=Fields.FieldByName(Items.DB.Keys.Contact).AsLargeInt;
    Enabled:=Fields.FieldByName(Items.DB.Keys.Enabled).AsBoolean;
    LockoutCount:=Fields.FieldByName(Items.DB.Keys.LockoutCount).AsInteger;
    Password:=Fields.FieldByName(Items.DB.Keys.Password).AsString;
    Auth:=Fields.FieldByName(Items.DB.Keys.Auth).AsString;
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.ACLCoreObjects).AsString,aclCoreObjects,',');
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(Items.DB.Keys.ACLCoreCommands).AsString,aclCoreCommands,',');
    Core.Arrays.LargeWord.RemoveAll(0,aclCoreObjects);
    Core.Arrays.LargeWord.RemoveAll(0,aclCoreCommands);
  end;
end;

class Function Items.DB.Fill_Auth(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  sCheck:Core.Strings.VarString;
  qID :QWord;
  Route:Storage.AuraDisks.Router.TItem;
 begin
  Result:=False;
  Try
    iCount:=0; UA.ID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,UA.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,UA.User,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.InsertID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Password,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.FirstName,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastName,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Telephone,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Auth,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastAccessed,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastQuotaEnforce,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastMessage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastIP,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Inbox,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Spambox,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Outbox,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Sentbox,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Devices,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Throttle,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Quota,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Contact,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Consumption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Enabled,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LockoutCount,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ACLCoreObjects,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ACLCoreCommands,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Fill_Auth,@UA);

    If (UA.ID<>0) then begin
      Storage.AuraDisks.Router.Verify(Task,UA.DomainID,UA.ID,Storage.AuraDisks.Kinds.User,UA.AuraNode,Route);
      Try
        if (Length(UA.Auth)=0) then
          SetAuth(Task,UA);

        sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Archive);
        Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkArchivebox,UA.ArchiveBox);

        sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Spam);
        Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkSpambox,UA.SpamBox);

        sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Outbox);
        Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkOutbox,UA.OutBox);

        sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Sent);
        Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkSentbox,UA.Sentbox);
      finally
        Storage.AuraDisks.Router.Done(Route);
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_UserAccountFill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  UAP:Items.PItem;
  iID:QWord;
begin
  UAP:=DataP;
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  {$i Storage.UserAccounts.UserAccount.Fill.Callback.inc}
end;

class Function Items.DB.Fill(Task:Core.Database.Types.TTask; ID:QWord; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  sCheck:Core.Strings.VarString;
  Route:Storage.AuraDisks.Router.TItem;
begin
  Result:=False;
  Try
    iCount:=0; UA.ID:=ID;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,UA.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ID,Commands);
    {$i Storage.UserAccounts.UserAccount.Fill.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_UserAccountFill,@UA) and (UA.ID>0);

    If Result then begin
      Storage.AuraDisks.Router.Verify(Task,UA.DomainID,UA.ID,Storage.AuraDisks.Kinds.User,UA.AuraNode,Route);
      if (Length(UA.Auth)=0) then
        SetAuth(Task,UA);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Archive);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkArchiveBox,UA.ArchiveBox);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Spam);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkSpambox,UA.SpamBox);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Outbox);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkOutbox,UA.OutBox);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Sent);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkSentBox,UA.Sentbox);
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_UserAccountThrottle(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Database.Types.PLargeWord(DataP)^:=Fields.FieldByName(Items.DB.Keys.Throttle).AsLargeInt;
end;

class Function Items.DB.Throttle(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Out Limit:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Limit:=Items.Throttle.Level1.Limit;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Throttle,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_UserAccountThrottle,@Limit);

  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function Items.DB.Fill(Task:Core.Database.Types.TTask; Name:Core.Strings.VarString; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  Route:Storage.AuraDisks.Router.TItem;
  sCheck:Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0; UA.ID:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,UA.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,Name,Commands);
    {$i Storage.UserAccounts.UserAccount.Fill.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_UserAccountFill,@UA) and (UA.ID>0);
    If Result then begin
      Storage.AuraDisks.Router.Verify(Task,UA.DomainID,UA.ID,Storage.AuraDisks.Kinds.User,UA.AuraNode,Route);
      if (Length(UA.Auth)=0) then
       SetAuth(Task,UA);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Archive);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkArchiveBox,UA.ArchiveBox);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Spam);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkSpambox,UA.SpamBox);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Outbox);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkOutbox,UA.OutBox);

      sCheck:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Storage.UserStorage.Folders.Defaults.Mail.Sent);
      Storage.UserStorage.Folders.DB.Verify(Task,UA.DomainID,UA.ID,sCheck,fvkSentBox,UA.Sentbox);
     end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_UserAccountFind_Auth(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Database.Types.PLargeWord(DataP)^:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

procedure CB_UserAccountFind_Account(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  UAP:Items.PItem;
begin
  UAP:=DataP;
  UAP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  UAP^.Password:=Fields.FieldByName(Items.DB.Keys.Password).AsString;
end;

procedure CB_UserAccountFind_Account_Auth(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  UAP:Items.PItem;
begin
  UAP:=DataP;
  UAP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  UAP^.User:=Fields.FieldByName(Items.DB.Keys.UserName).AsString;
end;


procedure CB_UserAccountFind(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  Accounts:Items.TList;
  UAP:Items.PItem;
  iID:QWord;
begin
  Accounts:=Items.TList(DataP);
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  UAP:=Accounts.Acquire(iID);
  {$i Storage.UserAccounts.UserAccount.Fill.Callback.inc}
end;

class Function  Items.DB.Authorized(Task:Core.Database.Types.TTask; var DomainID:QWord; var UAP:PItem; Accounts:TList; var AuthCount:LongInt; var Username,Auth:Core.Strings.VarString; out Credentials:Pointer):Boolean;
var
  iID:QWord;
begin
  if (System.Length(Auth)>0) and (AuthCount<5) then begin
    iID:=0;
    Inc(AuthCount);
    Credentials:=nil;
    if Find(Task,DomainID,iID,Username) then begin
      AuthCount:=0;
      if (UAP=nil) then begin
        UAP:=Accounts.Acquire(iID);
        Fill(Task,iID,UAP^);
      end else if (UAP^.ID<>iID) then begin
        Fill(Task,iID,UAP^);
      end;
      Credentials:=UAP;
    end;
  end;
  Result:=(Credentials<>nil) and (Auth=UAP^.Auth);
end;


class Function  Items.DB.Find(Task:Core.Database.Types.TTask; Var DomainID,ID:QWord; Var Account:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  UA:Item;
  dgMD5:TMD5Digest;
begin
  Result:=False;
  Try
    iCount:=0;
    Init(UA);
    Try
      Core.Database.AddCommand(iCount,TableP,@Commands);
      if ID=0 then
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poNone,oEqual,Account,Commands)
      else
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);


      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.UserName,poNone,oNone,Commands);
      Result:=(Core.Database.SQL.Select(Task,@Commands,@CB_UserAccountFind_Account_Auth,@UA) and (UA.ID<>0));
      if Result then begin
        ID:=UA.ID;
        Account:=UA.User;
      end;
    finally
      Done(UA);
    end;
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.Find(Task:Core.Database.Types.TTask; Accounts:TList; DomainID:QWord; sLike:Core.Strings.VarString):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,useForCriteriaBracket,oOpenBracket,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poNone,oLike,sLike,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FirstName,poOr,oLike,sLike,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.LastName,poOr,oLike,sLike,Commands);
    Core.Database.AddCommand(iCount,useForCriteriaBracket,oCloseBracket,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.ID,poNone,oDescending,Commands);
    {$i Storage.UserAccounts.UserAccount.Fill.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_UserAccountFind,Accounts);
  Finally
    Core.Database.Done(Commands);
  end;
end;

procedure CB_Exists(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Items.PItem(DataP)^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

class Function  Items.DB.Exists(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; UA.ID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,UA.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,UA.User,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Exists,@UA) and (UA.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Count(Task:Core.Database.Types.TTask; DomainID:QWord):QWord;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=0;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.SQL.Count(Task,@Commands,Result);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Exists(Task:Core.Database.Types.TTask; Var User,Domain:Core.Strings.VarString):Boolean;
var
  DomainID: QWord;
begin
  Result:=Storage.Domains.Items.DB.GetID(Task,Domain,DomainID);
  If Result then
    Result:=Exists(Task,User,DomainID);
end;

class Function Items.DB.Exists(Task:Core.Database.Types.TTask; Var User:Core.Strings.VarString; DomainID:QWord):Boolean;
var
  iCount:LongInt;
  rCount:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    rCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,User,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,rCount) and (rCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Authenticates(Task:Core.Database.Types.TTask; Var User,Password:Core.Strings.VarString; DomainID:QWord):Boolean;
var
  iCount:LongInt;
  rCount:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    rCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,User,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Password,poAnd,oEqual,Password,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,rCount) and (rCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Authenticates(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  With Items.PItem(DataP)^ do begin
    ID:=Fields.FieldByName(Items.DB.Keys.ID).asLargeInt;
    Password:=Fields.FieldByName(Items.DB.Keys.Password).AsString;
    User:=Fields.FieldByName(Items.DB.Keys.UserName).AsString;
  end;
end;

class Function  Items.DB.Authenticates(Task:Core.Database.Types.TTask; Var Auth:Core.Strings.VarString; DomainID:QWord; Var UA:Item):Boolean; overload;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Empty(UA);
    UA.Auth:=Auth;
    UA.DomainID:=DomainID;
    iCount:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,UA.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Auth,poAnd,oEqual,UA.Auth,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.UserName,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Password,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Authenticates,@UA) and (UA.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.LastIPMatches(Task:Core.Database.Types.TTask; IP:QWord; Var User:Core.Strings.VarString; DomainID:QWord):Boolean;
var
  iCount:LongInt;
  rCount:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    rCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,User,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.LastIP,poAnd,oEqual,IP,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,rCount) and (rCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Save(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreObjects);
    Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreCommands);
    Items.SetAuth(UA);

    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UA.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Password,poNone,oNone,UA.Password,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.FirstName,poNone,oNone,UA.First,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastName,poNone,oNone,UA.Last,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Telephone,poNone,oNone,UA.Telephone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Auth,poNone,oNone,UA.Auth,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ForwardAddress,poNone,oNone,UA.Forward,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Quota,poNone,oNone,UA.Quota,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Forwarding,poNone,oNone,UA.Forwarding,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,UA.Enabled,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LockoutCount,poNone,oNone,UA.LockoutCount,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ACLCoreObjects,poNone,oNone,UA.aclCoreObjects,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ACLCoreCommands,poNone,oNone,UA.aclCoreCommands,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetLockOutCount(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; LockCount:LongInt):Boolean;
Const
  Yes:boolean=true;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LockoutCount,poNone,oNone,LockCount,Commands);
    if LockCount=0 then
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,YES,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Write(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreObjects);
    Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreCommands);
    Items.SetAuth(UA);

    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UA.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Password,poNone,oNone,UA.Password,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.FirstName,poNone,oNone,UA.First,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastName,poNone,oNone,UA.Last,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Telephone,poNone,oNone,UA.Telephone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Auth,poNone,oNone,UA.Auth,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ForwardAddress,poNone,oNone,UA.Forward,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Forwarding,poNone,oNone,UA.Forwarding,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Enabled,poNone,oNone,UA.Enabled,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetContact(Task:Core.Database.Types.TTask; DomainID,UserID,ContactID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Contact,poNone,oNone,ContactID,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetTrash(Task:Core.Database.Types.TTask; DomainID,UserID,Trash:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Trash,poNone,oNone,Trash,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetSpamBox(Task:Core.Database.Types.TTask; DomainID,UserID,SpamBox:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Spambox,poNone,oNone,SpamBox,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetOutBox(Task:Core.Database.Types.TTask; DomainID,UserID,OutBox:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Outbox,poNone,oNone,OutBox,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetInBox(Task:Core.Database.Types.TTask; DomainID,UserID,InBox:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Inbox,poNone,oNone,InBox,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function  Items.DB.SetSentBox(Task:Core.Database.Types.TTask; DomainID,UserID,SentBox:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Sentbox,poNone,oNone,SentBox,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetTrashBox(Task:Core.Database.Types.TTask; DomainID,UserID,TrashBox:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.TrashBox,poNone,oNone,TrashBox,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetArchiveBox(Task:Core.Database.Types.TTask; DomainID,UserID,ArchiveBox:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ArchiveBox,poNone,oNone,ArchiveBox,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.UpdateMods(Task:Core.Database.Types.TTask; Var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UA.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastIP,poNone,oNone,UA.LastIP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastAccessed,poNone,oNone,UA.LastAccessed,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastMessage,poNone,oNone,UA.LastMessage,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LockoutCount,poNone,oNone,UA.LockoutCount,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.SetLastIP(Task:Core.Database.Types.TTask; ID,LastIP:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  dtNow:Double;
begin
  Result:=False;
  Try
    iCount:=0; dtNow:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastIP,poNone,oNone,LastIP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastAccessed,poNone,oNone,dtNow,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.UpdateACLs(Task:Core.Database.Types.TTask; var UA:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreObjects);
    Core.Arrays.LargeWord.RemoveAll(0,UA.aclCoreCommands);
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UA.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ACLCoreObjects,poNone,oNone,UA.aclCoreObjects,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.ACLCoreCommands,poNone,oNone,UA.aclCoreCommands,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.SetAuth(Var UA:Item);
var
  dgMD5:TMD5Digest;
begin
  dgMD5:=MD5.MD5String(Concat(UA.User,UA.Password));
  UA.Auth:=MD5.MD5Print(dgMD5);
end;

class Function Items.DB.SetAuth(Task:Core.Database.Types.TTask; Var UA:Item):Boolean; overload;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Items.SetAuth(UA);
    iCount:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UA.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Auth,poNone,oNone,UA.Auth,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.UpdateConsumption(Task:Core.Database.Types.TTask; var DomainID,UserID,Consumption:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  dtNow:double;
  personalUse:QWord;
  socialUse:QWord;
begin
  Result:=False;
  Try
    Storage.UserStorage.Consumption(Task,DomainID,UserID,personalUse);
    Storage.Social.Network.Consumption(Task,DomainID,UserID,socialUse);

    Consumption:=personalUse+socialUse;

    dtNow:=Core.Timer.dtUT;
    iCount:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastQuotaEnforce,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Consumption,poNone,oNone,Consumption,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.Enabled:=Source.Enabled;
  Destination.Modified:=Source.Modified;
  Destination.Forwarding:=Source.Forwarding;

  Destination.User:=Source.User;
  Destination.Password:=Source.Password;
  Destination.Forward:=Source.Forward;
  Destination.Auth:=Source.Auth;
  Destination.First:=Source.First;
  Destination.Last:=Source.Last;
  Destination.Telephone:=Source.Telephone;

  Destination.Throttle:=Source.Throttle;
  Destination.Quota:=Source.Quota;
  Destination.Consumption:=Source.Consumption;
  Destination.LastIP:=Source.LastIP;
  Destination.LastAccessed:=Source.LastAccessed;
  Destination.LastMessage:=Source.LastMessage;
  Destination.LockoutCount:=Source.LockoutCount;
  Destination.LastQuotaEnforce:=Source.LastQuotaEnforce;

  Destination.Trash:=Source.Trash;
  Destination.Inbox:=Source.Inbox;
  Destination.Trashbox:=Source.Trashbox;

  Destination.Devices:=Source.Devices;
  Destination.Contact:=Source.Contact;

  Core.Arrays.LargeWord.Copy(Source.aclCoreObjects,Destination.aclCoreObjects);
  Core.Arrays.LargeWord.Copy(Source.aclCoreCommands,Destination.aclCoreCommands);

  Storage.MatrixNodes.Node.Copy(Source.AuraNode,Destination.AuraNode);
  Destination.Roster:=Source.Roster;
  Destination.NetworkP:=Source.NetworkP;
end;

class Function  Items.fromXML(xDoc:TXMLDocument; var Entry:Item; DomainID,UserID:QWord):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Entry.DomainID:=DomainID;
  Entry.ID:=UserID;
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Account);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Entry.ID              :=toQWord(xItem,XML.Fields.ID,UserID);
      Entry.DomainID        :=toQWord(xItem,XML.Fields.DomainID,DomainID);
      Entry.Enabled         :=toBoolean(xItem,XML.Fields.Enabled,false);
      Entry.Forwarding      :=toBoolean(xItem,XML.Fields.SMTPForwarding,false);
      Entry.User            :=toString(xItem,XML.Fields.User);
      Entry.Password        :=toString(xItem,XML.Fields.Password);
      Entry.Forward         :=toString(xItem,XML.Fields.ForwardEmail);
      Entry.Auth            :=toString(xItem,XML.Fields.Auth);
      Entry.First           :=toString(xItem,XML.Fields.FirstName);
      Entry.Last            :=toString(xItem,XML.Fields.LastName);
      Entry.Telephone       :=toString(xItem,XML.Fields.Telephone);
      Entry.Throttle        :=toQWord(xItem,XML.Fields.Throttle);
      Entry.Quota           :=toQWord(xItem,XML.Fields.Quota);
      Entry.Consumption     :=toQWord(xItem,XML.Fields.Consumption);
      Entry.Contact         :=toQWord(xItem,XML.Fields.Contact);
      Entry.FirstIP         :=toQWord(xItem,XML.Fields.FirstIP);
      Entry.LastIP          :=toQWord(xItem,XML.Fields.LastIP);
      Entry.LastAccessed    :=toDouble(xItem,XML.Fields.LastAccessed);
      Entry.LastQuotaEnforce:=toDouble(xItem,XML.Fields.LastQuotaEnforce);
      Entry.LastMessage     :=toInteger(xItem,XML.Fields.LastMessage);

      Entry.LockoutCount    :=toInteger(xItem,XML.Fields.LockOutCount);
      Entry.Kind            :=Kind(toByte(xItem,XML.Fields.Kind));

      toQWordArray(xItem,XML.Fields.coACL,Entry.aclCoreObjects);
      toQWordArray(xItem,XML.Fields.ccACL,Entry.aclCoreCommands);
      Result:=True;
    end;
  end;
end;

class Function  Items.toXML(var Entry:Item; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Account,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.DomainID,Entry.DomainID),Output);
    Core.Streams.Write(Print(XML.Fields.User,Entry.User),Output);
    Core.Streams.Write(Print(XML.Fields.Password,Entry.Password),Output);
    Core.Streams.Write(Print(XML.Fields.Enabled,Entry.Enabled),Output);
    Core.Streams.Write(Print(XML.Fields.SMTPForwarding,Entry.Forwarding),Output);
    Core.Streams.Write(Print(XML.Fields.ForwardEmail,Entry.Forward),Output);
    Core.Streams.Write(Print(XML.Fields.Auth,Entry.Auth),Output);
    Core.Streams.Write(Print(XML.Fields.FirstName,Entry.First),Output);
    Core.Streams.Write(Print(XML.Fields.LastName,Entry.Last),Output);
    Core.Streams.Write(Print(XML.Fields.Telephone,Entry.Telephone),Output);
    Core.Streams.Write(Print(XML.Fields.Throttle,Entry.Throttle),Output);
    Core.Streams.Write(Print(XML.Fields.Quota,Entry.Quota),Output);
    Core.Streams.Write(Print(XML.Fields.Consumption,Entry.Consumption),Output);
    Core.Streams.Write(Print(XML.Fields.FirstIP,Entry.FirstIP),Output);
    Core.Streams.Write(Print(XML.Fields.LastIP,Entry.LastIP),Output);
    Core.Streams.Write(Print(XML.Fields.LastAccessed,Entry.LastAccessed),Output);
    Core.Streams.Write(Print(XML.Fields.LastQuotaEnforce,Entry.LastQuotaEnforce),Output);
    Core.Streams.Write(Print(XML.Fields.LastMessage,Entry.LastMessage),Output);
    Core.Streams.Write(Print(XML.Fields.LockOutCount,Entry.LockoutCount),Output);

    Core.Streams.Write(Print(XML.Fields.Trash,Entry.Trash),Output);
    Core.Streams.Write(Print(XML.Fields.Inbox,Entry.Inbox),Output);
    Core.Streams.Write(Print(XML.Fields.Spambox,Entry.Spambox),Output);
    Core.Streams.Write(Print(XML.Fields.Outbox,Entry.Outbox),Output);
    Core.Streams.Write(Print(XML.Fields.Sentbox,Entry.Sentbox),Output);
    Core.Streams.Write(Print(XML.Fields.ArchiveBox,Entry.Archivebox),Output);

    Core.Streams.Write(Print(XML.Fields.Trashbox,Entry.Trashbox),Output);

    Core.Streams.Write(Print(XML.Fields.Devices,Entry.Devices),Output);
    Core.Streams.Write(Print(XML.Fields.Contact,Entry.Contact),Output);

    Core.Streams.Write(Print(XML.Fields.Kind,Byte(Entry.Kind)),Output);

    Core.Streams.Write(Print(XML.Fields.coACL,Entry.aclCoreObjects),Output);
    Core.Streams.Write(Print(XML.Fields.ccACL,Entry.aclCoreCommands),Output);
    Result:=true;
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Account,Output);
  Core.Streams.Write('>',1,Output);
end;


Initialization
  RegisterDBM;
end.


unit Storage.Social;

{
  unit Storage.Social.pas

  Social Networking Database Module

  DBMS facilities to handle Social Networking specific requests

  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html

}

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

  Storage.Social.Folders,
  Storage.Social.Files,
  Storage.Social.Network,
  Storage.Social.Sync.Pipes,

  DOM,
  MD5,
  XMLRead,
  Classes,
  SysUtils;
type
  Connection = class
  type
    XML=class
    type
      Stanzas=class
      const
        Connection               : Core.Database.Types.VarString = 'connection';
        Connections              : Core.Database.Types.VarString = 'connections';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        OwnerID                  : Core.Database.Types.VarString = 'oid';
        NetworkID                : Core.Database.Types.VarString = 'nid';
        Created                  : Core.Database.Types.VarString = 'ctd';
        Accepted                 : Core.Database.Types.VarString = 'atd';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        OwnerID                  : Core.Database.Types.Integer = 3;
        NetworkID                : Core.Database.Types.Integer = 4;
        Created                  : Core.Database.Types.Integer = 5;
        Accepted                 : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        OwnerID                  : Core.Database.Types.VarString = 'IOID';
        NetworkID                : Core.Database.Types.VarString = 'INID';
        Created                  : Core.Database.Types.VarString = 'ICTD';
        Accepted                 : Core.Database.Types.VarString = 'IATD';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit                   : True;
        Group        : 'System/Applications/Social';
        Name         : 'Connection';
        Value        : 'scs_soc_con';
        Hint         : 'Social Connection storage';
        PrimaryKeyP  : @Keys.ID;
      );
      Fields: array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID;  KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created;  KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.Accepted; KeyP: @Keys.Accepted; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PConnection=^TConnection;
    TConnection=record
      ID                       : QWord;
      OwnerID                  : QWord;
      NetworkID                : QWord;
      Created                  : Double;
      Accepted                 : Double;
      Verified                 : Boolean;
    end;
    TConnections=Array of PConnection;
    PConnections=^TConnections;
    TConnectionsEvent=procedure(var Items:TConnections; var Networks:TNetworks) of object;

    class function  Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var Item:TConnection):boolean;
    class function  Delete(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TConnection):boolean;
    class function  Read(Task:Core.Database.Types.TTask; DomainID,OwnerID,NetworkID,ItemID:QWord; var Item:TConnection):boolean;
    class function  SetAccepted(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var Item:TConnection):boolean;
    class function  List(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TConnections):boolean;

    class function  fromXML(xNode:TDOMNode; var Item:TConnection):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TConnection):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TConnections):boolean; overload;

    class function  Get(ID:QWord; var Items:TConnections):PConnection;

    class function  toXML(var Item:TConnections; var Nets:TNetworks; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TConnections; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TConnection; Output:TMemoryStream; Header:Boolean):boolean; overload;

    class procedure Invalidate(var Items:TConnections);
    class procedure Purge(var Items:TConnections);

    class procedure Empty(Var Item:TConnection); overload;
    class procedure Empty(Var Item:TConnections); overload;

    class procedure Init(Var Item:TConnection); overload;
    class procedure Init(Var Item:TConnections); overload;

    class procedure Done(Var Item:TConnection); overload;
    class procedure Done(Var Item:TConnections); overload;
  end;
  Conversation=class
  type
    XML=class
    type
      Stanzas=class
      const
        Conversation             : Core.Database.Types.VarString = 'conversation';
        Conversations            : Core.Database.Types.VarString = 'conversations';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        UserID                   : Core.Database.Types.VarString = 'uid';
        NetworkID                : Core.Database.Types.VarString = 'nid';
        ParentID                 : Core.Database.Types.VarString = 'pid';
        Posted                   : Core.Database.Types.VarString = 'posted';
        Content                  : Core.Database.Types.VarString = 'content';
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
        UserID                   : Core.Database.Types.Integer = 4;
        ParentID                 : Core.Database.Types.Integer = 5;
        Posted                   : Core.Database.Types.Integer = 6;
        Content                  : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        NetworkID                : Core.Database.Types.VarString = 'INID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        ParentID                 : Core.Database.Types.VarString = 'IPID';
        Posted                   : Core.Database.Types.VarString = 'IPST';
        Content                  : Core.Database.Types.VarString = 'ICNT';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Social';
        Name                     : 'Conversation';
        Value                    : 'scs_soc_cvs';
        Hint                     : 'Social Conversations';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ParentID; KeyP: @Keys.ParentID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Posted; KeyP: @Keys.Posted; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Content;  KeyP: @Keys.Content; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 10240; Flags: cfNone;)
      );
    end;
    PConversation=^TConversation;
    PConversations=^TConversations;
    TConversations=Array of PConversation;
    TConversation=record
      ID                       : QWord;
      NetworkID                : QWord;
      UserID                   : QWord;
      ParentID                 : QWord;
      Posted                   : double;
      Content                  : Core.Strings.VarString;
    end;
    class function Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,UserID:QWord; var Item:TConversation):boolean;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TConversations):boolean;
    class function Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TConversation):boolean;
    class function Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TConversation):boolean;

    class function  fromXML(xNode:TDOMNode; var Item:TConversation):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TConversation):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TConversations):boolean; overload;

    class function  toXML(var Item:TConversations; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TConversation; Output:TMemoryStream; Header:Boolean):boolean; overload;

    class procedure Empty(var Item:TConversations); overload;
    class procedure Empty(Var Item:TConversation); overload;
    class procedure Done(Var Item:TConversations); overload;
    class procedure Done(Var Item:TConversation); overload;
    class procedure Init(var Item:TConversation); overload;
    class procedure Init(var Item:TConversations); overload;
  end;
  Flags=class
  type
    XML=class
    type
      Stanzas=class
      const
        Flag                     : Core.Database.Types.VarString = 'flag';
        Flags                    : Core.Database.Types.VarString = 'flags';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        NetworkID                : Core.Database.Types.VarString = 'nid';
        UserID                   : Core.Database.Types.VarString = 'uid';
        TargetID                 : Core.Database.Types.VarString = 'tid';
        Kind                     : Core.Database.Types.VarString = 'kind';
        Posted                   : Core.Database.Types.VarString = 'posted';
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
        UserID                   : Core.Database.Types.Integer = 4;
        TargetID                 : Core.Database.Types.Integer = 5;
        Kind                     : Core.Database.Types.Integer = 6;
        Posted                   : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        NetworkID                : Core.Database.Types.VarString = 'INID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        TargetID                 : Core.Database.Types.VarString = 'TGID';
        Kind                     : Core.Database.Types.VarString = 'IKND';
        Posted                   : Core.Database.Types.VarString = 'IPST';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Social';
        Name                     : 'Flags';
        Value                    : 'scs_soc_flgs';
        Hint                     : 'Content flags for social media';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.TargetID; KeyP: @Keys.TargetID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.Posted; KeyP: @Keys.Posted; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PFlag=^TFlag;
    PFlags=^TFlags;
    TFlags=Array of PFlag;
    TFlag=record
      ID                       : QWord;
      NetworkID                : QWord;
      UserID                   : QWord;
      TargetID                 : QWord;
      Kind                     : QWord;
      Posted                   : double;
    end;
    class function Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,UserID,TargetID,Kind:QWord; var Item:TFlag):boolean;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TFlags):boolean; overload;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID,Kind:QWord; var Item:TFlags):boolean; overload;
    class function Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):boolean;
    class function Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TFlag):boolean;

    class function  fromXML(xNode:TDOMNode; var Item:TFlag):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TFlag):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TFlags):boolean; overload;

    class function  toXML(var Item:TFlags; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TFlag; Output:TMemoryStream; Header:Boolean):boolean; overload;

    class procedure Empty(var Item:TFlags); overload;
    class procedure Empty(Var Item:TFlag); overload;
    class procedure Done(Var Item:TFlags); overload;
    class procedure Done(Var Item:TFlag); overload;
    class procedure Init(var Item:TFlag); overload;
    class procedure Init(var Item:TFlags); overload;
  end;
  Kind=class
  type
    XML=class
    type
      Stanzas=class
      const
        Kind                     : Core.Database.Types.VarString = 'kind';
        Kinds                    : Core.Database.Types.VarString = 'kinds';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        NameSpace                : Core.Database.Types.VarString = 'ns';
      end;
    end;
    NameSpace=class
    Const
      Text                       : Core.Database.Types.VarString = 'text';
      Picture                    : Core.Database.Types.VarString = 'picture';
      Music                      : Core.Database.Types.VarString = 'music';
      Video                      : Core.Database.Types.VarString = 'video';
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        NameSpace                : Core.Database.Types.Integer = 3;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        NameSpace                : Core.Database.Types.VarString = 'INSP';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Social';
        Name                     : 'Kinds';
        Value                    : 'scs_soc_kds';
        Hint                     : 'Registery of social media types';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..3] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NameSpace; KeyP: @Keys.NameSpace;  DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
    end;
    PKind=^TKind;
    PKinds=^TKinds;
    TKinds=Array of PKind;
    TKind=record
      ID                       : QWord;
      NameSpace                : Core.Strings.VarString;
    end;
    class function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TKind):boolean;
    class function  List(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TKinds):boolean;
    class function  Verify(Task:Core.Database.Types.TTask; DomainID:QWord; var Items:TKinds):boolean;
    class function  Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
    class function  Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TKind):boolean;

    class function  fromXML(xNode:TDOMNode; var Item:TKind):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TKind):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TKinds):boolean; overload;

    class function  toXML(var Item:TKinds; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TKind; Output:TMemoryStream; Header:Boolean):boolean; overload;

    class procedure Empty(var Item:TKinds); overload;
    class procedure Empty(Var Item:TKind); overload;
    class procedure Done(Var Item:TKinds); overload;
    class procedure Done(Var Item:TKind); overload;
    class procedure Init(var Item:TKind); overload;
    class procedure Init(var Item:TKinds); overload;

    class function  IndexOf(aNameSpace:Core.Strings.VarString; var Items:TKinds): LongInt;
    class function  getItem(aNameSpace:Core.Strings.VarString; var Items:TKinds):PKind;
  end;
  Bann=class
  type
    XML=class
    type
      Stanzas=class
      const
        Bann                     : Core.Database.Types.VarString = 'bann';
        Banns                    : Core.Database.Types.VarString = 'banns';
      end;
      Fields=class
      const
        ID                       : Core.Database.Types.VarString = 'id';
        NetworkID                : Core.Database.Types.VarString = 'nid';
        AdminID                  : Core.Database.Types.VarString = 'aid';
        UserID                   : Core.Database.Types.VarString = 'uid';
        Days                     : Core.Database.Types.VarString = 'days';
        Posted                   : Core.Database.Types.VarString = 'posted';
        Expires                  : Core.Database.Types.VarString = 'ttl';
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
        AdminID                  : Core.Database.Types.Integer = 4;
        UserID                   : Core.Database.Types.Integer = 5;
        Days                     : Core.Database.Types.Integer = 6;
        Posted                   : Core.Database.Types.Integer = 7;
        Expires                  : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        NetworkID                : Core.Database.Types.VarString = 'INID';
        AdminID                  : Core.Database.Types.VarString = 'IAID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        Days                     : Core.Database.Types.VarString = 'IDYS';
        Posted                   : Core.Database.Types.VarString = 'IPST';
        Expires                  : Core.Database.Types.VarString = 'ITTL';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Social';
        Name                     : 'Banned';
        Value                    : 'scs_soc_bann';
        Hint                     : 'Membership banns for social networks';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.AdminID; KeyP: @Keys.AdminID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Days; KeyP: @Keys.Days; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Posted; KeyP: @Keys.Posted; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Expires; KeyP: @Keys.Expires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;)
      );
    end;
    PBann=^TBann;
    PBanns=^TBanns;
    TBanns=Array of PBann;
    TBann=record
      ID                       : QWord;
      NetworkID                : QWord;
      AdminID                  : QWord;
      UserID                   : QWord;
      Days                     : LongInt;
      Posted                   : double;
      Expires                  : double;
    end;
    class function Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,AdminID,UserID:QWord; Days:LongInt; var Item:TBann):boolean;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TBanns):boolean;
    class function Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):boolean;
    class function Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TBann):boolean;

    class function  fromXML(xNode:TDOMNode; var Item:TBann):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TBann):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TBanns):boolean; overload;

    class function  toXML(var Item:TBanns; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Item:TBann; Output:TMemoryStream; Header:Boolean):boolean; overload;

    class procedure Empty(var Item:TBanns); overload;
    class procedure Empty(Var Item:TBann); overload;
    class procedure Done(Var Item:TBanns); overload;
    class procedure Done(Var Item:TBann); overload;
    class procedure Init(var Item:TBann); overload;
    class procedure Init(var Item:TBanns); overload;
  end;

implementation
uses
  DB,
  sqldb,
  DateUtils;

procedure cbDestroyConversation(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Conversation.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyFlags(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Flags.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyBann(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Bann.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyKind(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Kind.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyConnection(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with Connection.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Conversation.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Conversation.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Conversation.DB.TableP, useForCriteria, Conversation.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP=Flags.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Flags.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Flags.DB.TableP,useForCriteria,Flags.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Kind.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Kind.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Kind.DB.TableP,useForCriteria,Kind.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Bann.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Kind.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Kind.DB.TableP,useForCriteria,Bann.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Connection.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Kind.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Kind.DB.TableP,useForCriteria,Connection.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Conversation.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Conversation.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Conversation.DB.TableP, useForCriteria, Conversation.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if (ItemP=Bann.DB.MonitorP) then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Bann.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Bann.DB.TableP,useForCriteria,Bann.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if (ItemP=Connection.DB.MonitorP) then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Connection.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Connection.DB.TableP,useForCriteria,Connection.DB.IDs.OwnerID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
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

  with Connection.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyConnection, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Conversation.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyConversation, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Kind.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyKind, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Bann.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyBann, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Flags.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFlags, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

class function Conversation.Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,UserID:QWord; var Item:TConversation):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Item.NetworkID:=NetworkID;
    Item.UserID:=UserID;
    Item.Posted:=Core.Timer.dtUT;

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.NetworkID),poNone,oNone,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ParentID),poNone,oNone,Item.ParentID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Posted),poNone,oNone,Item.Posted,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Content),poNone,oNone,Item.Content,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbReadConversation(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Conversation.PConversation;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadConversation.Fields.inc}
end;

class function Conversation.Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TConversation):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);
    Item.ID:=ItemID;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Social.Conversation.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadConversation, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListConversations(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Conversation.PConversations;
  ItmP:Conversation.PConversation;
  iIndex:LongInt;
begin
  New(ItmP);
  Conversation.Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadConversation.Fields.inc}
end;

class function Conversation.List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TConversations):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poNone, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForOrderBy,  DB.IDs.Posted,poNone,oNone,Commands);
    {$i Storage.Social.Conversation.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbListConversations, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Conversation.Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TConversation):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Conversation.Empty(var Item:TConversations);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Conversation.Init(var Item:TConversation);
begin
  With Item do begin
    ID:=0;
    UserID:=0;
    NetworkID:=0;
    ParentID:=0;
    Posted:=0;
    SetLength(Content,0);
  end;
end;

class procedure Conversation.Init(var Item:TConversations);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Conversation.Empty(Var Item:TConversation);
begin
  With Item do begin
    ID:=0;
    UserID:=0;
    NetworkID:=0;
    ParentID:=0;
    Posted:=0;
    SetLength(Content,0);
  end;
end;

class procedure Conversation.Done(Var Item:TConversations);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Conversation.Done(Var Item:TConversation);
begin
  With Item do
    Finalize(Content);
  Finalize(Item);
end;

class function  Conversation.fromXML(xNode:TDOMNode; var Item:TConversation):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.UserID:=toQWord(xNode,XML.Fields.UserID);
      Item.NetworkID:=toQWord(xNode,XML.Fields.NetworkID);
      Item.ParentID:=toQWord(xNode,XML.Fields.ParentID);
      Item.Posted:=toByte(xNode,XML.Fields.Posted);
      Item.Content:=toString(xNode,XML.Fields.Content);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function  Conversation.fromXML(xDoc:TXMLDocument; var Item:TConversation):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Conversation),Item);
end;

class function  Conversation.fromXML(xDoc:TXMLDocument; var Items:TConversations):boolean;
var
  xCons:TDOMNode;
  xCon:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PConversation;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Empty(Items);
  with Core.XML.DB do begin
    xCons:=getNode(xDoc,XML.Stanzas.Conversations);
    if xCons<>nil then begin
      for iLcv:=0 to xCons.ChildNodes.Count-1 do begin
        xCon:=xCons.ChildNodes[iLcv];
        if SameText(xCon.NodeName,XML.Stanzas.Conversation) then begin
          New(itmP);
          Init(itmP^);
          SetLength(Items,iCount+1);
          Items[iCount]:=itmP;
          Inc(iCount);

          fromXML(xCon,itmP^);
        end;
      end;
      Result:=True;
    end else
      Result:=false;
  end;
end;

class Function  Conversation.toXML(var Item:TConversations; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Conversations,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Conversations,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Conversation.toXML(var Item:TConversation; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Conversation,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.UserID,Item.UserID),Output);
    Core.Streams.Write(Print(XML.Fields.ParentID,Item.ParentID),Output);
    Core.Streams.Write(Print(XML.Fields.Posted,Item.Posted),Output);
    Core.Streams.Write(Print(XML.Fields.Content,Item.Content,CDATA_ON),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Conversation,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;


class procedure Flags.Empty(var Item:TFlags);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Flags.Init(var Item:TFlag);
begin
  With Item do begin
    ID:=0;
    NetworkID:=0;
    UserID:=0;
    TargetID:=0;
    Kind:=0;
    Posted:=0.0;
  end;
end;

class procedure Flags.Init(var Item:TFlags);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Flags.Empty(Var Item:TFlag);
begin
  With Item do begin
    ID:=0;
    NetworkID:=0;
    UserID:=0;
    TargetID:=0;
    Kind:=0;
    Posted:=0.0;
  end;
end;

class procedure Flags.Done(Var Item:TFlags);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Flags.Done(Var Item:TFlag);
begin
  With Item do
    Finalize(Item);
  Finalize(Item);
end;

class function  Flags.fromXML(xNode:TDOMNode; var Item:TFlag):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.NetworkID:=toQWord(xNode,XML.Fields.NetworkID);
      Item.UserID:=toQWord(xNode,XML.Fields.UserID);
      Item.TargetID:=toQWord(xNode,XML.Fields.TargetID);
      Item.Kind:=toQWord(xNode,XML.Fields.Kind);
      Item.Posted:=toDouble(xNode,XML.Fields.Posted);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function  Flags.fromXML(xDoc:TXMLDocument; var Item:TFlag):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Flag),Item);
end;

class function  Flags.fromXML(xDoc:TXMLDocument; var Items:TFlags):boolean;
var
  xItms:TDOMNode;
  xItm:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PFlag;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Empty(Items);
  with Core.XML.DB do begin
    xItms:=getNode(xDoc,XML.Stanzas.Flags);
    if xItms<>nil then begin
      for iLcv:=0 to xItms.ChildNodes.Count-1 do begin
        xItm:=xItms.ChildNodes[iLcv];
        if SameText(xItm.NodeName,XML.Stanzas.Flag) then begin
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

class Function  Flags.toXML(var Item:TFlags; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Flags,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Flags,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Flags.toXML(var Item:TFlag; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Flag,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.UserID,Item.UserID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.TargetID,Item.TargetID),Output);
    Core.Streams.Write(Print(XML.Fields.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(XML.Fields.Posted,Item.Posted),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Flag,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function Flags.Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,UserID,TargetID,Kind:QWord; var Item:TFlag):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));

    Item.NetworkID:=NetworkID;
    Item.UserID:=UserID;
    Item.TargetID:=TargetID;
    Item.Kind:=Kind;
    Item.Posted:=Core.Timer.dtUT;

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.NetworkID),poNone,oNone,Item.NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,Item.UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.TargetID),poNone,oNone,Item.TargetID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Kind),poNone,oNone,Item.Kind,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Posted),poNone,oNone,Item.Posted,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbReadFlag(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Flags.PFlag;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadFlag.Fields.inc}
end;

class function Flags.Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TFlag):boolean;
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

    {$i Storage.Social.Flags.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadFlag, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListFlags(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Flags.PFlags;
  ItmP:Flags.PFlag;
  iIndex:LongInt;
begin
  New(ItmP);
  Flags.Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadFlag.Fields.inc}
end;

class function Flags.List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TFlags):boolean;
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

    {$i Storage.Social.Flags.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListFlags, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Flags.List(Task:Core.Database.Types.TTask; DomainID,NetworkID,Kind:QWord; var Item:TFlags):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Kind, poAnd, oEqual, Kind, Commands);

    {$i Storage.Social.Flags.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListFlags, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Flags.Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):boolean;
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

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Kind.Empty(var Item:TKinds);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Kind.Init(var Item:TKind);
begin
  With Item do begin
    ID:=0;
    SetLength(NameSpace,0);
  end;
end;

class function  Kind.IndexOf(aNameSpace:Core.Strings.VarString; var Items:TKinds): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if SameText(Items[iLcv]^.NameSpace,aNameSpace) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class function  Kind.getItem(aNameSpace:Core.Strings.VarString; var Items:TKinds):PKind;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Items) do begin
    if SameText(Items[iLcv]^.NameSpace,aNameSpace) then begin
      Result:=Items[iLcv];
      Break;
    end;
  end;
end;

class procedure Kind.Init(var Item:TKinds);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Kind.Empty(Var Item:TKind);
begin
  With Item do begin
    ID:=0;
    SetLength(NameSpace,0);
  end;
end;

class procedure Kind.Done(Var Item:TKinds);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Kind.Done(Var Item:TKind);
begin
  With Item do
    Finalize(NameSpace);
  Finalize(Item);
end;

class function  Kind.fromXML(xNode:TDOMNode; var Item:TKind):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.NameSpace:=toString(xNode,XML.Fields.NameSpace);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function  Kind.fromXML(xDoc:TXMLDocument; var Item:TKind):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Kind),Item);
end;

class function  Kind.fromXML(xDoc:TXMLDocument; var Items:TKinds):boolean;
var
  xItms:TDOMNode;
  xItm:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PKind;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Empty(Items);
  with Core.XML.DB do begin
    xItms:=getNode(xDoc,XML.Stanzas.Kinds);
    if xItms<>nil then begin
      for iLcv:=0 to xItms.ChildNodes.Count-1 do begin
        xItm:=xItms.ChildNodes[iLcv];
        if SameText(xItm.NodeName,XML.Stanzas.Kind) then begin
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

class Function  Kind.toXML(var Item:TKinds; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Kinds,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Kinds,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Kind.toXML(var Item:TKind; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Kind,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.NameSpace,Item.NameSpace),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Kind,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function Kind.Add(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TKind):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.NameSpace),poNone,oNone,Item.NameSpace,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbReadKind(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Kind.PKind;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadKind.Fields.inc}
end;

class function Kind.Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TKind):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Social.Kinds.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadKind, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListKinds(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Kind.PKinds;
  ItmP:Kind.PKind;
  iIndex:LongInt;
begin
  New(ItmP);
  Kind.Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadKind.Fields.inc}
end;

class function  Kind.Verify(Task:Core.Database.Types.TTask; DomainID:QWord; var Items:TKinds):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  itmP:PKind;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Items);
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    {$i Storage.Social.Kinds.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbListKinds, @Items);
  finally
    Core.Database.Done(Commands);
  end;

  itmP:=getItem(NameSpace.Text,Items);
  if itmP=nil then begin
    new(itmP);
    Init(itmP^);
    itmP^.NameSpace:=NameSpace.Text;
    Add(Task,DomainID,itmP^);
  end;
  itmP:=getItem(NameSpace.Picture,Items);
  if itmP=nil then begin
    new(itmP);
    Init(itmP^);
    itmP^.NameSpace:=NameSpace.Picture;
    Add(Task,DomainID,itmP^);
  end;
  itmP:=getItem(NameSpace.Music,Items);
  if itmP=nil then begin
    new(itmP);
    Init(itmP^);
    itmP^.NameSpace:=NameSpace.Music;
    Add(Task,DomainID,itmP^);
  end;
  itmP:=getItem(NameSpace.Video,Items);
  if itmP=nil then begin
    new(itmP);
    Init(itmP^);
    itmP^.NameSpace:=NameSpace.Video;
    Add(Task,DomainID,itmP^);
  end;

end;

class function Kind.List(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TKinds):boolean;
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
    {$i Storage.Social.Kinds.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbListKinds, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Kind.Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Bann.Empty(var Item:TBanns);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Bann.Init(var Item:TBann);
begin
  With Item do begin
    ID:=0;
    NetworkID:=0;
    AdminID:=0;
    UserID:=0;
    Posted:=0.0;
    Expires:=0.0;
  end;
end;

class procedure Bann.Init(var Item:TBanns);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Bann.Empty(Var Item:TBann);
begin
  With Item do begin
    ID:=0;
    NetworkID:=0;
    AdminID:=0;
    UserID:=0;
    Posted:=0.0;
    Expires:=0.0;
  end;
end;

class procedure Bann.Done(Var Item:TBanns);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class procedure Bann.Done(Var Item:TBann);
begin
  Finalize(Item);
end;

class function  Bann.fromXML(xNode:TDOMNode; var Item:TBann):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.NetworkID:=toQWord(xNode,XML.Fields.NetworkID);
      Item.AdminID:=toQWord(xNode,XML.Fields.AdminID);
      Item.UserID:=toQWord(xNode,XML.Fields.UserID);
      Item.Posted:=toQWord(xNode,XML.Fields.Posted);
      Item.Expires:=toQWord(xNode,XML.Fields.Expires);
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function  Bann.fromXML(xDoc:TXMLDocument; var Item:TBann):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Bann),Item);
end;

class function  Bann.fromXML(xDoc:TXMLDocument; var Items:TBanns):boolean;
var
  xItms:TDOMNode;
  xItm:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PBann;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Empty(Items);
  with Core.XML.DB do begin
    xItms:=getNode(xDoc,XML.Stanzas.Banns);
    if xItms<>nil then begin
      for iLcv:=0 to xItms.ChildNodes.Count-1 do begin
        xItm:=xItms.ChildNodes[iLcv];
        if SameText(xItm.NodeName,XML.Stanzas.Bann) then begin
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

class Function  Bann.toXML(var Item:TBanns; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Banns,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Banns,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Bann.toXML(var Item:TBann; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Bann,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.AdminID,Item.AdminID),Output);
    Core.Streams.Write(Print(XML.Fields.UserID,Item.UserID),Output);
    Core.Streams.Write(Print(XML.Fields.Posted,Item.Posted),Output);
    Core.Streams.Write(Print(XML.Fields.Expires,Item.Expires),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Bann,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function Bann.Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,AdminID,UserID:QWord; Days:LongInt; var Item:TBann):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Item.NetworkID:=NetworkID;
    Item.AdminID:=AdminID;
    Item.UserID:=UserID;
    Item.Posted:=Core.Timer.dtUT;
    Item.Days:=Days;
    Item.Expires:=DateUtils.IncDay(Item.Posted,Days);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.NetworkID),poNone,oNone,Item.NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.AdminID),poNone,oNone,Item.AdminID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,Item.UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Posted),poNone,oNone,Item.Posted,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Expires),poNone,oNone,Item.Expires,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cbReadBann(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Bann.PBann;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadBann.Fields.inc}
end;

class function Bann.Read(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord; var Item:TBann):boolean;
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

    {$i Storage.Social.Banns.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadBann, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbListBanns(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Bann.PBanns;
  ItmP:Bann.PBann;
  iIndex:LongInt;
begin
  New(ItmP);
  Bann.Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadBann.Fields.inc}
end;

class function Bann.List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Item:TBanns):boolean;
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
    {$i Storage.Social.Banns.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbListBanns, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Bann.Delete(Task:Core.Database.Types.TTask; DomainID,NetworkID,ItemID:QWord):boolean;
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

    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Connection.Add(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var Item:TConnection):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Item.NetworkID:=NetworkID;
    Item.OwnerID:=OwnerID;
    Item.Created:=Core.Timer.dtUT;
    Item.Accepted:=Core.Timer.dtUT;

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.NetworkID),poNone,oNone,Item.NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.OwnerID),poNone,oNone,Item.OwnerID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Created),poNone,oNone,Item.Created,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Accepted),poNone,oNone,Item.Accepted,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Connection.Delete(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TConnection):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, Item.NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.OwnerID, poAnd, oEqual, OwnerID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadConnection(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:Connection.PConnection;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadConnection.Fields.inc}
end;

class function Connection.Read(Task:Core.Database.Types.TTask; DomainID,OwnerID,NetworkID,ItemID:QWord; var Item:TConnection):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.OwnerID, poAnd, oEqual, OwnerID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Social.Connection.Read.Fields.inc}

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadConnection, @Item) and  (Item.ID<>0) );
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Connection.SetAccepted(Task:Core.Database.Types.TTask; DomainID,NetworkID,OwnerID:QWord; var Item:TConnection):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; Item.Accepted:=Core.Timer.dtUT;
  Try
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.OwnerID,poAnd,oEqual,OwnerID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.ID,poAnd,oEqual,Item.ID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,DB.IDs.Accepted,poNone,oNone,Item.Accepted,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbListConnections(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Connection.PConnections;
  ItmP:Connection.PConnection;
  iIndex:LongInt;
begin
  New(ItmP);
  Connection.Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadConnection.Fields.inc}
end;

class function Connection.List(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TConnections):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.OwnerID, poAnd, oEqual, OwnerID, Commands);
    {$i Storage.Social.Connection.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbListConnections, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Connection.fromXML(xNode:TDOMNode; var Item:TConnection):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xNode,XML.Fields.ID);
      Item.NetworkID:=toQWord(xNode,XML.Fields.NetworkID);
      Item.OwnerID:=toQWord(xNode,XML.Fields.OwnerID);
      Item.Created:=toQWord(xNode,XML.Fields.Created);
      Item.Accepted:=toQWord(xNode,XML.Fields.Accepted);
      Item.Verified:=true;
    end;
    Result:=True;
  end else
    Result:=false;
end;

class function Connection.fromXML(xDoc:TXMLDocument; var Item:TConnection):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Connection),Item);
end;


class function Connection.fromXML(xDoc:TXMLDocument; var Items:TConnections):boolean;
var
  xItms:TDOMNode;
  xItm:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PConnection;
  iID:QWord;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Invalidate(Items);
  with Core.XML.DB do begin
    xItms:=getNode(xDoc,XML.Stanzas.Connections);
    if xItms<>nil then begin
      for iLcv:=0 to xItms.ChildNodes.Count-1 do begin
        xItm:=xItms.ChildNodes[iLcv];
        if SameText(xItm.NodeName,XML.Stanzas.Connection) then begin
          iID:=toQWord(xItm,XML.Fields.ID);
          itmP:=Get(iID,Items);
          if (itmP=nil) then begin
            New(itmP);
            Init(itmP^);
            SetLength(Items,iCount+1);
            Items[iCount]:=itmP;
            Inc(iCount);
          end;
          fromXML(xItm,itmP^);
        end;
      end;
      Result:=True;
    end else
      Result:=false;
  end;
  Purge(Items);
end;

class function Connection.toXML(var Item:TConnections; var Nets:TNetworks; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Connections,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);

  Storage.Social.Network.toXML(Nets,Output,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Connections,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function Connection.toXML(var Item:TConnections; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Connections,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Connections,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function Connection.toXML(var Item:TConnection; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Connection,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.NetworkID,Item.NetworkID),Output);
    Core.Streams.Write(Print(XML.Fields.OwnerID,Item.OwnerID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Accepted,Item.Accepted),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Connection,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class procedure Connection.Invalidate(var Items:TConnections);
var
  iLcv:LongInt;
  itmP:PConnection;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    if (itmP<>nil) then
      itmP^.Verified:=false;
  end;
end;

class procedure Connection.Purge(var Items:TConnections);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iStartCt:LongInt;
  iCt:LongInt;
  itmP:PConnection;
begin
  iStartCt:=System.Length(Items);
  iCt:=iStartCt;
  for iLcv:=0 to iCt-1 do begin
    itmP:=Items[iLcv];
    if (itmP<>nil) and (itmP^.Verified=false) then begin
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
  while (iLcv<iCt) do begin
    itmP:=Items[iLcv];
    if itmP=nil then begin
      for jLcv:=iLcv to iCt-2 do
        Items[jLcv]:=Items[jLcv+1];
      Dec(iCt);
    end else
      inc(iLcv);
  end;
  if (iStartCt<>iCt) then
    SetLength(Items,iCt);
end;

class function  Connection.Get(ID:QWord; var Items:TConnections):PConnection;
var
  iLcv:LongInt;
begin
  Result:=nil;
  For iLCv:=0 to High(Items) do begin
    if Items[iLcv]^.ID=ID then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;


class procedure Connection.Empty(Var Item:TConnection);
begin
  Item.Verified:=false;
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.OwnerID:=0;
  Item.Created:=0;
  Item.Accepted:=0;
end;

class procedure Connection.Empty(Var Item:TConnections);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

class procedure Connection.Init(Var Item:TConnection);
begin
  Item.Verified:=false;
  Item.ID:=0;
  Item.NetworkID:=0;
  Item.OwnerID:=0;
  Item.Created:=0;
  Item.Accepted:=0;
end;

class procedure Connection.Init(Var Item:TConnections);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

class procedure Connection.Done(Var Item:TConnection);
begin
  Finalize(Item);
end;

class procedure Connection.Done(Var Item:TConnections);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

initialization
  RegisterDB;
end.


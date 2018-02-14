unit Storage.Social.Network;


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
  Encryption.Base64,

  Storage,
  Storage.Social.Network.Requests,


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
Type
  PNetwork=^TNetwork;
  TNetwork=record
    ID                       : QWord;
    OwnerID                  : QWord;
    AvatarID                 : QWord;

    Created                  : double;
    Modified                 : double;

    MemberCount              : LongInt;
    RequestCount             : LongInt;

    DocumentsID              : QWord;
    TrashID                  : QWord;
    MusicID                  : QWord;
    PicturesID               : QWord;
    VideosID                 : QWord;

    Privacy                  : Byte;
    Tag                      : TMD5Digest;

    Title                    : Core.Strings.VarString;
    Description              : Core.Strings.VarString;

    PublicMembers            : Core.Arrays.Types.LargeWord;
    PrivateMembers           : Core.Arrays.Types.LargeWord;
    Admins                   : Core.Arrays.Types.LargeWord;

    Node                     : Storage.MatrixNodes.Node.Item;
    Verified                 : boolean;
  end;
  TNetworks=Array of PNetwork;
  PNetworks=^TNetworks;
  TNetworksEvent=procedure(var Items:TNetworks) of object;

  Admins = class
  type
    XML=class
    Type
      Stanza=class
      const
        Admins                 : Core.Database.Types.VarString = 'admins';
        Admin                  : Core.Database.Types.VarString = 'admin';
      end;
      Fields=class
      const
        DomainID               : Core.Database.Types.VarString = 'did';
        UserID                 : Core.Database.Types.VarString = 'uid';
        NetworkID              : Core.Database.Types.VarString = 'nid';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        DomainID               : Core.Database.Types.Integer = 0;
        UserID                 : Core.Database.Types.Integer = 1;
        NetworkID              : Core.Database.Types.Integer = 2;
      end;
      Keys=class
      const
        DomainID               : Core.Database.Types.VarString = 'IDID';
        UserID                 : Core.Database.Types.VarString = 'IUID';
        NetworkID              : Core.Database.Types.VarString = 'NIRD';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit   : True;
        Group        : 'System/Applications/Social/Network';
        Name         : 'Admins';
        Value        : 'scs_soc_net_adm';
        Hint         : 'Social Networking Administrators';
        PrimaryKeyP  : nil;
      );
      Fields: array [0..2] of Core.Database.Types.Field = (
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; )
      );
    end;
    class function Count(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; out Items:QWord):boolean;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
    class function Remove(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord):boolean;
    class function Add(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord):boolean; overload;
    class function Add(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Users:Core.Arrays.Types.LargeWord):boolean; overload;
  end;
  Members = class
  type
    Face=class
    Const
      Open                     : Core.Database.Types.Integer= 0;
      Closed                   : Core.Database.Types.Integer= 1;
    end;
    XML=class
    Type
      Stanza=class
      const
        Admins                 : Core.Database.Types.VarString = 'adms';
        Admin                  : Core.Database.Types.VarString = 'adm';
        Member                 : Core.Database.Types.VarString = 'mbr';
        Members                : Core.Database.Types.VarString = 'mbrs';
      end;
      Fields=class
      const
        DomainID               : Core.Database.Types.VarString = 'did';
        UserID                 : Core.Database.Types.VarString = 'uid';
        NetworkID              : Core.Database.Types.VarString = 'nid';
        Face                   : Core.Database.Types.VarString = 'fce';
        ID                     : Core.Database.Types.VarString = 'id';
        AccountID              : Core.Database.Types.VarString = 'auid';
        AvatarID               : Core.Database.Types.VarString = 'aid';
        First                  : Core.Database.Types.VarString = 'fst';
        Nick                   : Core.Database.Types.VarString = 'nck';
        Last                   : Core.Database.Types.VarString = 'lst';
        City                   : Core.Database.Types.VarString = 'cty';
        State                  : Core.Database.Types.VarString = 'ste';
        Post                   : Core.Database.Types.VarString = 'pst';
        Country                : Core.Database.Types.VarString = 'cnt';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        DomainID               : Core.Database.Types.Integer = 0;
        UserID                 : Core.Database.Types.Integer = 1;
        NetworkID              : Core.Database.Types.Integer = 2;
        Face                   : Core.Database.Types.Integer = 3;
      end;
      Keys=class
      const
        DomainID               : Core.Database.Types.VarString = 'IDID';
        UserID                 : Core.Database.Types.VarString = 'IUID';
        NetworkID              : Core.Database.Types.VarString = 'NIRD';
        Face                   : Core.Database.Types.VarString = 'MFCE';
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate   : True;
        AutoCommit                   : True;
        Group        : 'System/Applications/Social/Network';
        Name         : 'Members';
        Value        : 'scs_soc_net_mbrs';
        Hint         : 'Social Networking Members';
        PrimaryKeyP  : nil;
      );
      Fields: array [0..3] of Core.Database.Types.Field = (
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.NetworkID; KeyP: @Keys.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull;  ),
        (IDP: @IDs.Face; KeyP: @Keys.Face; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull;)
      );
    end;
    class function Count(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; out Items:QWord):boolean; overload;
    class function Count(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Visibility:byte; out Items:QWord):boolean; overload;

    class function  List(Task:Core.Database.Types.TTask; DomainID:QWord; var Networks:TNetworks; var Items:Storage.Roster.Items.List):boolean; overload;
    class function  List(Task:Core.Database.Types.TTask; DomainID:QWord; var Requests:TRequests; var Items:Storage.Roster.Items.List):boolean; overload;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean; overload;
    class function List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord; Visibility:Byte):boolean; overload;
    class function Remove(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord):boolean;
    class function Add(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord; Visibility:byte):boolean; overload;
    class function Add(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Users:Core.Arrays.Types.LargeWord; Visibility:byte):boolean; overload;
    class function  toXML(var Entries:Storage.Roster.Items.List; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Entry:Storage.Roster.Items.Item; Output:TMemoryStream; Header:Boolean):boolean; overload;
  end;

  XML=class
  type
    Stanzas=class
    const
      Network                  : Core.Database.Types.VarString = 'network';
      Networks                 : Core.Database.Types.VarString = 'networks';
    end;
    Fields=class
    const
      ID                       : Core.Database.Types.VarString = 'id';
      OwnerID                  : Core.Database.Types.VarString = 'oid';
      AvatarID                 : Core.Database.Types.VarString = 'aid';
      Created                  : Core.Database.Types.VarString = 'ctd';
      Modified                 : Core.Database.Types.VarString = 'mtd';
      MemberCount              : Core.Database.Types.VarString = 'memct';
      RequestCount             : Core.Database.Types.VarString = 'reqct';
      // Root Folder IDs
      DocumentsID              : Core.Database.Types.VarString = 'dcid';
      MusicID                  : Core.Database.Types.VarString = 'muid';
      PicturesID               : Core.Database.Types.VarString = 'pcid';
      TrashID                  : Core.Database.Types.VarString = 'trid';
      VideosID                 : Core.Database.Types.VarString = 'vdid';
      // *** End of Root Folder IDs
      Privacy                   : Core.Database.Types.VarString = 'privacy';
      Tag                       : Core.Database.Types.VarString = 'tag';
      Title                     : Core.Database.Types.VarString = 'title';
      Description               : Core.Database.Types.VarString = 'desc';
      PublicMembers             : Core.Database.Types.VarString = 'pum';
      PrivateMembers            : Core.Database.Types.VarString = 'prm';
      Admins                    : Core.Database.Types.VarString = 'admins';
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
      AvatarID                 : Core.Database.Types.Integer = 4;
      Created                  : Core.Database.Types.Integer = 5;
      Modified                 : Core.Database.Types.Integer = 6;
      MemberCount              : Core.Database.Types.Integer = 7;
      RequestCount             : Core.Database.Types.Integer = 8;

      DocumentsID              : Core.Database.Types.Integer = 9;
      PicturesID               : Core.Database.Types.Integer = 10;
      MusicID                  : Core.Database.Types.Integer = 11;
      TrashID                  : Core.Database.Types.Integer = 12;
      VideosID                 : Core.Database.Types.Integer = 13;

      Privacy                  : Core.Database.Types.Integer = 14;
      Tag                      : Core.Database.Types.Integer = 15;

      Title                    : Core.Database.Types.Integer = 16;
      Description              : Core.Database.Types.Integer = 17;

      IDs                      : Core.Database.Types.Integer = 18; // runtime
      Limit                    : Core.Database.Types.Integer = 19; // runtime
    end;
    Keys=class
    const
      ID                       : Core.Database.Types.VarString = 'ITID';
      InsertID                 : Core.Database.Types.VarString = 'IIID';
      DomainID                 : Core.Database.Types.VarString = 'IDID';
      OwnerID                  : Core.Database.Types.VarString = 'IOID';
      AvatarID                 : Core.Database.Types.VarString = 'TARD';
      Created                  : Core.Database.Types.VarString = 'ICTD';
      Modified                 : Core.Database.Types.VarString = 'IMTD';
      MemberCount              : Core.Database.Types.VarString = 'MMCT';
      RequestCount             : Core.Database.Types.VarString = 'RQCT';

      // Folders
      DocumentsID              : Core.Database.Types.VarString = 'DCSID';
      MusicID                  : Core.Database.Types.VarString = 'MUSID';
      PicturesID               : Core.Database.Types.VarString = 'PCSID';
      TrashID                  : Core.Database.Types.VarString = 'TRHID';
      VideosID                 : Core.Database.Types.VarString = 'VIDID';

      Privacy                  : Core.Database.Types.VarString = 'FLGS';
      Tag                      : Core.Database.Types.VarString = 'ITAG';

      Title                    : Core.Database.Types.VarString = 'ITIT';
      Description              : Core.Database.Types.VarString = 'IDSC';

      IDs                      : Core.Database.Types.VarString = 'ITID';
      Limit                    : Core.Database.Types.VarString = 'LIMIT';
    end;
  const
    TableP   : Core.Database.Types.PTable = nil;
    MonitorP : Core.Database.Monitor.Types.PItem = nil;
    Startup  : Core.Database.Types.TableIni = (
      AutoCreate   : True;
      AutoCommit                   : True;
      Group        : 'System/Applications/Social';
      Name         : 'Network';
      Value        : 'scs_soc_net';
      Hint         : 'Social Networking storage';
      PrimaryKeyP  : @Keys.ID;
    );
    Fields: array [0..19] of Core.Database.Types.Field = (
      (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
      (IDP: @IDs.InsertID;  KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.OwnerID;  KeyP: @Keys.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
      (IDP: @IDs.AvatarID;  KeyP: @Keys.AvatarID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.Created;  KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
      (IDP: @IDs.Modified;  KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.MemberCount; KeyP: @Keys.MemberCount; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.RequestCount;  KeyP: @Keys.RequestCount; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),

      (IDP: @IDs.DocumentsID; KeyP: @Keys.DocumentsID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.MusicID; KeyP: @Keys.MusicID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.TrashID; KeyP: @Keys.TrashID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
      (IDP: @IDs.PicturesID; KeyP: @Keys.PicturesID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.VideosID; KeyP: @Keys.VideosID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),

      (IDP: @IDs.Privacy; KeyP: @Keys.Privacy; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Tag; KeyP: @Keys.Tag; DataType: dftMD5Digest; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),

      (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
      (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),

      // Runtime Fields
      (IDP: @IDs.IDs;  KeyP: @Keys.IDs; DataType: dftQWordArray; AutoCreate: false; Verified: true; Precision: 1024*1024*4; Flags: cfNone; ),
      (IDP: @IDs.Limit; KeyP: @Keys.Limit; DataType: dftInteger; AutoCreate: false; Verified: true; Precision: 0; Flags: cfNone; )
    );
  end;
  Privacy=class
  const
    Hidden                   : byte = 0;
    Visible                  : byte = 1;
  end;
  Defaults=class
  const
    TimeToLive               : word = 120;
    MaxSearchResults         : LongInt = 1000;
  end;

  function Search(Task:Core.Database.Types.TTask; DomainID:QWord; var Criteria:Core.Arrays.Types.KeyStrings; var Item:TNetworks):boolean;
  function Add(Task:Core.Database.Types.TTask; Refactor:TMemoryStream; var OwnerNode:Storage.MatrixNodes.Node.Item; DomainID,OwnerID:QWord; var Item:TNetwork):boolean;
  function Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID:QWord; var Item:TNetwork):boolean;
  function Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TNetwork):boolean;
  function Write(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TNetwork):boolean;
  function List(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TNetworks):boolean;  overload;
  function List(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TNetworks):boolean;  overload;
  function List(Task:Core.Database.Types.TTask; var IDs:Core.Arrays.Types.LargeWord; var Item:TNetworks):boolean; overload;

  function setMemberCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Count:LongInt):Boolean;

  function getAvatarID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var AvatarID:QWord):Boolean;
  function setAvatarID(Task:Core.Database.Types.TTask; DomainID,ItemID,AvatarID:QWord):Boolean;

  function getTrashID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
  function setTrashID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;

  function getMusicID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
  function setMusicID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;

  function getPicturesID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
  function setPicturesID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;

  function getVideosID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
  function setVideosID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;

  function incRequestCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; Count:LongInt=1):Boolean;
  function decRequestCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; Count:LongInt=1):Boolean;
  function getRequestCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; out Count:LongInt):Boolean;

  procedure Convert(Task:Core.Database.Types.TTask; DomainID:QWord);
  function Consumption(Task:Core.Database.Types.TTask; Var DomainID,NetworkID,Value:QWord):boolean;
  procedure setIDs(var Items:TNetworks; var IDs:Core.Arrays.Types.LargeWord);

  function isMember(ID:QWord; var Item:TNetwork):Boolean;
  function isAdmin(ID:QWord; var Item:TNetwork):Boolean;

  function  Get(ID:QWord; var Items:TNetworks):PNetwork;


  function  fromXML(xNode:TDOMNode; var Item:TNetwork):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Item:TNetwork):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Items:TNetworks):boolean; overload;

  function  toXML(var Item:TNetworks; Output:TMemoryStream; Header:Boolean):boolean; overload;
  function  toXML(var Item:TNetwork; Output:TMemoryStream; Header:Boolean):boolean; overload;

  procedure Invalidate(var Items:TNetworks);
  procedure Purge(var Items:TNetworks);

  procedure Empty(Var Item:TNetwork); overload;
  procedure Empty(Var Item:TNetworks); overload;

  procedure Init(Var Item:TNetwork); overload;
  procedure Init(Var Item:TNetworks); overload;

  procedure Done(Var Item:TNetwork); overload;
  procedure Done(Var Item:TNetworks); overload;

  procedure Copy(var Source,Dest:TNetwork);


implementation
uses
  Storage.Social,
  Storage.Social.Folders,
  Storage.Social.Files;


procedure cbDestroyNetworkAdmins(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Admins.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyNetworkMembers(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Members.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyNetwork(ItemP: Core.Database.Monitor.Types.PItem);
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
    end else if ItemP = Admins.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Admins.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Admins.DB.TableP, useForCriteria, Admins.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Members.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Members.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Members.DB.TableP, useForCriteria, Members.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
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
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.OwnerID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Admins.DB.MonitorP then begin
        try
          iCount := 0;
          Core.Database.AddCommand(iCount, Admins.DB.TableP,@Commands);
          Core.Database.AddCommand(iCount, Admins.DB.TableP, useForCriteria, Admins.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
          Result := Core.Database.SQL.Delete(Task, @Commands);
        finally
          Empty(Commands);
        end;
    end else if ItemP = Members.DB.MonitorP then begin
        try
          iCount := 0;
          Core.Database.AddCommand(iCount, Members.DB.TableP,@Commands);
          Core.Database.AddCommand(iCount, Members.DB.TableP, useForCriteria, Members.DB.IDs.UserID, poNone, oEqual, ItemID, Commands);
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
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyNetwork, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Admins.DB do begin
      if TableP = nil then begin
        New(TableP);
        Core.Database.Init(TableP^, Startup);
        for iLcv := 0 to High(Fields) do
          Core.Database.AddField(@Fields[iLcv], TableP);
        if MonitorP = nil then begin
          New(MonitorP);
          Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyNetworkAdmins, @cbDBMonitorNotified);
          Core.Database.Monitor.Add(MonitorP);
        end;
      end;
  end;
  with Members.DB do begin
      if TableP = nil then begin
        New(TableP);
        Core.Database.Init(TableP^, Startup);
        for iLcv := 0 to High(Fields) do
          Core.Database.AddField(@Fields[iLcv], TableP);
        if MonitorP = nil then begin
          New(MonitorP);
          Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyNetworkMembers, @cbDBMonitorNotified);
          Core.Database.Monitor.Add(MonitorP);
        end;
      end;
  end;
end;

function Add(Task:Core.Database.Types.TTask; Refactor:TMemoryStream; var OwnerNode:Storage.MatrixNodes.Node.Item; DomainID,OwnerID:QWord; var Item:TNetwork):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
  Avatar:Storage.Avatars.Items.TItem;
  Context:TMD5Context;
  Route:Storage.AuraDisks.Router.TItem;
  usrFile:Storage.UserStorage.Files.TItem;
  fsAvatar:TFileStream;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Item.OwnerID:=OwnerID;
    Item.Created:=Core.Timer.dtUT;
    Item.Modified:=Core.Timer.dtUT;
    Item.RequestCount:=0;

    Core.Arrays.LargeWord.Add(OwnerID,Item.Admins,aoCheckForDuplicates);
    Core.Arrays.LargeWord.Add(OwnerID,Item.PublicMembers,aoCheckForDuplicates);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);

    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.OwnerID),poNone,oNone,OwnerID,Commands);

    //Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.AvatarID),poNone,oNone,Item.AvatarID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Created,poNone,oNone,Item.Created,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Modified,poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Privacy,poNone,oNone,Item.Privacy,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Title,poNone,oNone,Item.Title,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.Description,poNone,oNone,Item.Description,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.RequestCount,poNone,oNone,Item.RequestCount,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (Item.ID<>0);

    if Result then begin
      Members.Add(Task,DomainID,Item.ID,Item.PublicMembers,Members.Face.Open);
      Members.Add(Task,DomainID,Item.ID,Item.PrivateMembers,Members.Face.Closed);
      Admins.Add(Task,DomainID,Item.ID,Item.Admins);

      Storage.AuraDisks.Router.Allocate(Task,DomainID,Item.ID,Storage.AuraDisks.Kinds.Social,Item.Node,Route);
      Try
        Storage.Social.Folders.CreateDefaults(Task,DomainID,Item.ID,OwnerID,Item.TrashID,Item.DocumentsID,Item.MusicID,Item.PicturesID,Item.VideosID);
        if (Item.AvatarID<>0) then begin
          Try
            usrFile.ID:=Item.AvatarID;
            if Storage.UserStorage.Files.DB.Data(Task,OwnerNode,DomainID,OwnerID,usrFile,fsAvatar) then begin
              Try
                Core.Streams.Copy(fsAvatar,Refactor);
                Storage.Avatars.Items.DB.Add(Task,DomainID,OwneriD,Storage.Avatars.Items.Kinds.Network,Core.Utils.Files.Extract(usrFile.Name,efeoNone),Refactor,Avatar);
                Refactor.Size:=0;
                Item.AvatarID:=Avatar.ID;
              finally
                FreeAndNil(fsAvatar);
                Storage.UserStorage.Files.Done(usrFile);
              end;
            end;
          finally
            Storage.Avatars.Items.Done(Avatar);
          end;
        end;
      finally
        Storage.AuraDisks.Router.Done(Route);
      end;
      MD5.MD5Init(Context);
      MD5.MD5Update(Context,OwnerID,SizeOf(OwnerID));
      MD5.MD5Update(Context,Item.ID,SizeOf(Item.ID));
      MD5.MD5Final(Context,Item.Tag);

      iCount := 0;
      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

      Item.Modified:=Core.Timer.dtUT;

      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified,poNone, oNone, Item.Modified, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Tag,poNone, oNone, Item.Tag, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.AvatarID, poNone, oNone, Item.AvatarID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.DocumentsID, poNone, oNone, Item.DocumentsID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.TrashID, poNone, oNone, Item.TrashID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.MusicID, poNone, oNone, Item.MusicID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.PicturesID, poNone, oNone, Item.PicturesID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.VideosID, poNone, oNone, Item.VideosID, Commands);

      Result:=Core.Database.SQL.Update(Task,@Commands);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

function Write(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TNetwork):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.AvatarID, poNone, oNone, Item.AvatarID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Privacy, poNone, oNone, Item.Privacy, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Title, poNone, oNone, Item.Title, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Description, poNone, oNone, Item.Description, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function setMemberCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Count:LongInt):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.MemberCount, poNone, oNone, Count, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Core.Timer.dtUT,Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function setAvatarID(Task:Core.Database.Types.TTask; DomainID,ItemID,AvatarID:QWord):boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.AvatarID, poNone, oNone, AvatarID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Core.Timer.dtUT,Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function incRequestCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; Count:LongInt=1):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForIncrement, DB.IDs.RequestCount, poNone, oNone, Count, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

function decRequestCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; Count:LongInt=1):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForDecrement, DB.IDs.RequestCount, poNone, oNone, Count, Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbReadNetwork(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItmP:PNetwork;
begin
  ItmP:=DataP;
  {$i Storage.Social.cbReadNetwork.Fields.inc}
  ItmP^.RequestCount:=Fields.FieldByName(DB.Keys.RequestCount).AsInteger;
end;

function Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TNetwork):boolean;
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

    {$i Storage.Social.Network.Read.Fields.inc}
    // Ownership Fields
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.RequestCount, poNone, oNone, Commands);

    Result := (Core.Database.SQL.Select(Task, @Commands, @cbReadNetwork, @Item) and  (Item.ID<>0) );
    if Result then begin
      Storage.AuraDisks.Router.Verify(Task,DomainID,Item.ID,Storage.AuraDisks.Kinds.Social,Item.Node);
      Members.List(Task,DomainID,Item.ID,Item.PublicMembers,Members.Face.Open);
      Members.List(Task,DomainID,Item.ID,Item.PrivateMembers,Members.Face.Closed);
      Admins.List(Task,DomainID,Item.ID,Item.Admins);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbNetworkAvatarID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(DB.Keys.AvatarID).AsLargeInt;
end;

function getAvatarID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var AvatarID:QWord):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; AvatarID:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.AvatarID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbNetworkAvatarID, @AvatarID);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbNetworkTrashID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(DB.Keys.TrashID).AsLargeInt;
end;

function getTrashID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; FolderID:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, FolderID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.TrashID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbNetworkTrashID, @FolderID);
  finally
    Core.Database.Done(Commands);
  end;
end;

function setTrashID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.TrashID, poNone, oNone, FolderID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Core.Timer.dtUT,Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbNetworkMusicID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(DB.Keys.MusicID).AsLargeInt;
end;

function getMusicID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; FolderID:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.MusicID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbNetworkMusicID, @FolderID);
  finally
    Core.Database.Done(Commands);
  end;
end;

function setMusicID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.MusicID, poNone, oNone, FolderID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Core.Timer.dtUT,Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbNetworkPicturesID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(DB.Keys.PicturesID).AsLargeInt;
end;

function getPicturesID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; FolderID:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.PicturesID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbNetworkPicturesID, @FolderID);
  finally
    Core.Database.Done(Commands);
  end;
end;

function setPicturesID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.PicturesID, poNone, oNone, FolderID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Core.Timer.dtUT,Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbNetworkVideosID(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(DB.Keys.VideosID).AsLargeInt;
end;

function getVideosID(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var FolderID:QWord):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; FolderID:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.VideosID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbNetworkVideosID, @FolderID);
  finally
    Core.Database.Done(Commands);
  end;
end;

function setVideosID(Task:Core.Database.Types.TTask; DomainID,ItemID,FolderID:QWord):Boolean;
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

    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.VideosID, poNone, oNone, FolderID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForUpdates, DB.IDs.Modified,poNone,oNone,Core.Timer.dtUT,Commands);
    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbNetworkRequestCount(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  PInteger(DataP)^:=Fields.FieldByName(DB.Keys.RequestCount).AsInteger;
end;

function getRequestCount(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; out Count:LongInt):Boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0; Count:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.RequestCount, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbNetworkRequestCount, @Count);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure Convert(Task:Core.Database.Types.TTask; DomainID:QWord);
var
  Items:TNetworks;
  iLcv,iConLcv:LongInt;
  NetP:PNetwork;
begin
  List(Task,DomainID,Items);
  Try
    for iLcv:=0 to High(Items) do begin
      NetP:=Items[iLcv];
      for iConLcv:=0 to High(NetP^.Admins) do
        Admins.Add(Task,DomainID,NetP^.Admins[iConLcv],NetP^.ID);
      for iConLcv:=0 to High(NetP^.PublicMembers) do
        Members.Add(Task,DomainID,NetP^.PublicMembers[iConLcv],NetP^.ID,Members.Face.Open);
      for iConLcv:=0 to High(NetP^.PrivateMembers) do
        Members.Add(Task,DomainID,NetP^.PrivateMembers[iConLcv],NetP^.ID,Members.Face.Closed);
    end;
  finally
    Done(Items);
  end;
end;


function isMember(ID:QWord; var Item:TNetwork):Boolean;
begin
  Result:=(
    (Item.OwnerID=ID) or
    (Core.Arrays.LargeWord.IndexOf(ID,Item.Admins)<>-1) or
    (Core.Arrays.LargeWord.IndexOf(ID,Item.PublicMembers)<>-1) or
    (Core.Arrays.LargeWord.IndexOf(ID,Item.PrivateMembers)<>-1)
  );
end;

procedure setIDs(var Items:TNetworks; var IDs:Core.Arrays.Types.LargeWord);
var
  iLcv:LongInt;
begin
  Core.Arrays.LargeWord.Empty(IDs);
  for iLcv:=0 to High(Items) do
    Core.Arrays.LargeWord.Add(Items[iLcv]^.ID,IDs);
end;

function isAdmin(ID:QWord; var Item:TNetwork):Boolean;
begin
  Result:=(
    (Item.OwnerID=ID) or
    (Core.Arrays.LargeWord.IndexOf(ID,Item.Admins)<>-1)
  );
end;

function  Get(ID:QWord; var Items:TNetworks):PNetwork;
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

procedure cbListNetworksAsOwner(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:PNetworks;
  ItmP:PNetwork;
  iIndex:LongInt;
begin
  New(ItmP);
  Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadNetwork.Fields.inc}
  ItmP^.RequestCount:=Fields.FieldByName(DB.Keys.RequestCount).AsInteger;
end;

procedure cbListNetworks(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:PNetworks;
  ItmP:PNetwork;
  iIndex:LongInt;
begin
  New(ItmP);
  Init(ItmP^);

  ListP:=DataP;
  iIndex:=System.Length(ListP^);
  SetLength(ListP^,iIndex+1);
  ListP^[iIndex]:=ItmP;

  {$i Storage.Social.cbReadNetwork.Fields.inc}
end;

function List(Task:Core.Database.Types.TTask; DomainID,OwnerID:QWord; var Item:TNetworks):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  ItmP:PNetwork;
  iLcv:LongInt;

begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.OwnerID, poAnd, oEqual, OwnerID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForOrderBy,  DB.IDs.Modified,poNone,oNone,Commands);

    {$i Storage.Social.Network.Read.Fields.inc}
    // Ownership Fields
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.RequestCount, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListNetworksAsOwner, @Item);

    for iLcv:=0 to High(Item) do begin
      ItmP:=Item[iLcv];
      Members.List(Task,DomainID,ItmP^.ID,ItmP^.PublicMembers,Members.Face.Open);
      Members.List(Task,DomainID,ItmP^.ID,ItmP^.PrivateMembers,Members.Face.Closed);
      Admins.List(Task,DomainID,ItmP^.ID,ItmP^.Admins);
      iCount:=System.Length(ItmP^.PrivateMembers)+System.Length(ItmP^.PublicMembers);
      if ItmP^.MemberCount<>iCount then begin
        ItmP^.MemberCount:=iCount;
        setMemberCount(Task,DomainID,ItmP^.ID,ItmP^.MemberCount);
      end;
    end;

  finally
    Core.Database.Done(Commands);
  end;
end;

function List(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TNetworks):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  ItmP:PNetwork;
  iLcv:LongInt;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForOrderBy,  DB.IDs.Modified,poNone,oNone,Commands);

    {$i Storage.Social.Network.Read.Fields.inc}
    // Ownership Fields
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID, poNone, oNone, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.RequestCount, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListNetworksAsOwner, @Item);

    For iLcv:=0 to High(Item) do begin
      ItmP:=Item[iLcv];
      Members.List(Task,DomainID,ItmP^.ID,ItmP^.PublicMembers,Members.Face.Open);
      Members.List(Task,DomainID,ItmP^.ID,ItmP^.PrivateMembers,Members.Face.Closed);
      Admins.List(Task,DomainID,ItmP^.ID,ItmP^.Admins);
    end;
  finally
    Core.Database.Done(Commands);
  end;
end;

function List(Task:Core.Database.Types.TTask; var IDs:Core.Arrays.Types.LargeWord; var Item:TNetworks):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  Empty(Item);
  if (System.Length(IDs)>0) then begin
    iCount := 0;
    try
      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.IDs, poNone, oIn, IDs, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForOrderBy,  DB.IDs.Modified,poNone,oNone,Commands);
      {$i Storage.Social.Network.Read.Fields.inc}
      Result := Core.Database.SQL.Select(Task, @Commands, @cbListNetworks, @Item);
    finally
      Core.Database.Done(Commands);
    end;
  end else
    Result:=true;
end;

function  Consumption(Task:Core.Database.Types.TTask; Var DomainID,NetworkID,Value:QWord):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    with Files do begin
      Core.Database.AddCommand(iCount,Storage.Social.Files.DB.TableP,@Commands);
      Core.Database.AddCommand(iCount,Storage.Social.Files.DB.TableP,useForCriteria,Storage.Social.Files.DB.IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,Storage.Social.Files.DB.TableP,useForCriteria,Storage.Social.Files.DB.IDs.NetworkID,poAnd,oEqual,NetworkID,Commands);
      Core.Database.AddCommand(iCount,Storage.Social.Files.DB.TableP,useForFields,Storage.Social.Files.DB.IDs.Size,poNone,oNone,Commands);
    end;
    Result:=Core.Database.SQL.Sum(Task,@Commands,Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

function Search(Task:Core.Database.Types.TTask; DomainID:QWord; var Criteria:Core.Arrays.Types.KeyStrings; var Item:TNetworks):boolean;
const
  PreOp:array[boolean] of SQLPreOperator = (poOr,poNone);
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
  iCriCt,iLcv:LongInt;
begin
  Result := False;
  try
    iCount := 0; Empty(Item);

    iCriCt:=System.Length(Criteria);
    for iLcv:=0 to iCriCt-1 do
      Criteria[iLcv]^.Value:=Like_Prep(Criteria[iLcv]^.Value);


    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Privacy, poAnd, oGreaterThan, Privacy.Hidden, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForLimit, DB.IDs.Limit, poAnd, oNone, Defaults.MaxSearchResults,Commands);

    if iCriCt>1 then begin
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, PROPERTY_ID_VOID, poAnd, oOpenBracket,Commands);
      for iLcv:=0 to iCriCt-1 do begin
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, PROPERTY_ID_VOID, PreOp[iLcv=0], oOpenBracket,Commands);
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Title, poNone, oILike, Criteria[iLcv]^.Value, Commands);
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Description, poOr, oILike, Criteria[iLcv]^.Value, Commands);
        Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, PROPERTY_ID_VOID, poNone, oCloseBracket,Commands);
      end;
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, PROPERTY_ID_VOID, poNone, oCloseBracket,Commands);
    end else begin
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, PROPERTY_ID_VOID, poAnd, oOpenBracket,Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Title, poNone, oILike, Criteria[iLcv]^.Value, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Description, poOr, oILike, Criteria[iLcv]^.Value, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, PROPERTY_ID_VOID, poNone, oCloseBracket,Commands);
    end;

    {$i Storage.Social.Network.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListNetworks, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

function Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID:QWord; var Item:TNetwork):boolean;
var
  iCount:LongInt;
  iLcv:LongInt;
  Commands: Core.Database.Types.Commands;
  IDS:Core.Arrays.Types.LargeWord;

begin
  Result := False;

  Result:=List(Task,DomainID,Item.ID,IDS);
  if Result then begin
    for iLcv:=0 to High(IDS) do
      Storage.Social.Folders.Delete(Task,Node,DomainID,Item.ID,IDs[iLcv]);
    try
      iCount := 0;

      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

      Result := Core.Database.SQL.Delete(Task, @Commands);
    finally
      Core.Database.Empty(Commands);
    end;

    try
      iCount := 0;
      Core.Database.AddCommand(iCount, Conversation.DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, Conversation.DB.TableP, useForCriteria, Conversation.DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
      Core.Database.AddCommand(iCount, Conversation.DB.TableP, useForCriteria, Conversation.DB.IDs.NetworkID, poAnd, oEqual, Item.ID, Commands);

      Result := Core.Database.SQL.Delete(Task, @Commands);
    finally
      Core.Database.Empty(Commands);
    end;

    try
      With Storage.Avatars.Items.DB do begin
        iCount := 0;
        Core.Database.AddCommand(iCount, TableP,@Commands);
        Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
        Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.OwnerID, poAnd, oEqual, Item.ID, Commands);
        Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.Kind, poAnd, oEqual, Storage.Avatars.Items.Kinds.Network, Commands);

        Result := Core.Database.SQL.Delete(Task, @Commands);
      end;
    finally
      Core.Database.Empty(Commands);
    end;

    Core.Database.Monitor.Cascade(Task,DB.TableP,Item.ID,Core.Database.Monitor.Notify.SOCIAL_NETWORK_DELETED);
  end;
end;

procedure Empty(var Item:TNetworks);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Init(var Item:TNetwork);
begin
  With Item do begin
    ID:=0;
    OwnerID:=0;
    AvatarID:=0;
    DocumentsID:=0;
    MusicID:=0;
    PicturesID:=0;
    TrashID:=0;
    VideosID:=0;
    Privacy:=0;
    SetLength(Title,0);
    SetLength(Description,0);
    Core.Arrays.LargeWord.Init(PublicMembers);
    Core.Arrays.LargeWord.Init(PrivateMembers);
    Core.Arrays.LargeWord.Init(Admins);
    Storage.MatrixNodes.Node.Init(Node);
  end;
end;

procedure Init(var Item:TNetworks);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Copy(var Source,Dest:TNetwork);
begin
  Dest.ID:=Source.ID;
  Dest.OwnerID:=Source.OwnerID;
  Dest.AvatarID:=Source.AvatarID;
  Dest.DocumentsID:=Source.DocumentsID;
  Dest.MusicID:=Source.MusicID;
  Dest.PicturesID:=Source.PicturesID;
  Dest.TrashID:=Source.TrashID;
  Dest.VideosID:=Source.VideosID;
  Dest.Privacy:=Source.Privacy;
  Dest.Title:=Source.Title;
  Dest.Description:=Source.Description;

  Core.Arrays.Bytes.Copy(Source.Tag,Dest.Tag);
  Core.Arrays.LargeWord.Copy(Source.PublicMembers,Dest.PublicMembers);
  Core.Arrays.LargeWord.Copy(Source.PrivateMembers,Dest.PrivateMembers);
  Core.Arrays.LargeWord.Copy(Source.Admins,Dest.Admins);

  Storage.MatrixNodes.Node.Copy(Source.Node,Dest.Node);
end;

procedure Invalidate(var Items:TNetworks);
var
  iLcv:LongInt;
  itmP:PNetwork;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    if (itmP<>nil) then
      itmP^.Verified:=false;
  end;
end;

procedure Purge(var Items:TNetworks);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iStartCt:LongInt;
  iCt:LongInt;
  itmP:PNetwork;
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

procedure Empty(Var Item:TNetwork);
begin
  With Item do begin
    ID:=0;
    OwnerID:=0;
    AvatarID:=0;

    DocumentsID:=0;
    MusicID:=0;
    PicturesID:=0;
    TrashID:=0;
    VideosID:=0;


    Privacy:=0;
    FillChar(Tag[0],SizeOf(Tag),0);

    SetLength(Title,0);
    SetLength(Description,0);

    Core.Arrays.LargeWord.Init(PublicMembers);
    Core.Arrays.LargeWord.Init(PrivateMembers);
    Core.Arrays.LargeWord.Init(Admins);

    Storage.MatrixNodes.Node.Empty(Node);

  end;
end;

procedure Done(Var Item:TNetworks);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure Done(Var Item:TNetwork);
begin
  With Item do begin
    Finalize(Title);
    Finalize(Description);
    Finalize(Tag);

    Core.Arrays.LargeWord.Done(PublicMembers);
    Core.Arrays.LargeWord.Done(PrivateMembers);
    Core.Arrays.LargeWord.Done(Admins);

    Storage.MatrixNodes.Node.Done(Node);
  end;
  Finalize(Item);
end;

function  fromXML(xNode:TDOMNode; var Item:TNetwork):boolean;
begin
  Empty(Item);
  if xNode<>nil then begin
    with Core.XML.DB do begin
      Item.ID       :=toQWord(xNode,XML.Fields.ID);
      Item.OwnerID  :=toQWord(xNode,XML.Fields.OwnerID);
      Item.AvatarID :=toQWord(xNode,XML.Fields.AvatarID);
      Item.MemberCount :=toInteger(xNode,XML.Fields.MemberCount);
      Item.RequestCount :=toInteger(xNode,XML.Fields.RequestCount);

      Item.Created :=toDouble(xNode,XML.Fields.Created);
      Item.Modified :=toDouble(xNode,XML.Fields.Modified);


      Item.DocumentsID   :=toQWord(xNode,XML.Fields.DocumentsID);
      Item.TrashID       :=toQWord(xNode,XML.Fields.TrashID);
      Item.PicturesID    :=toQWord(xNode,XML.Fields.PicturesID);
      Item.VideosID      :=toQWord(xNode,XML.Fields.VideosID);
      Item.MusicID       :=toQWord(xNode,XML.Fields.MusicID);

      Item.Privacy:=toByte(xNode,XML.Fields.Privacy);
      Item.Title:=toString(xNode,XML.Fields.Title);
      Item.Description:=toString(xNode,XML.Fields.Description);
      toMD5Digest(xNode,XML.Fields.Tag,Item.Tag);

      toQWordArray(xNode,XML.Fields.PublicMembers,Item.PublicMembers);
      toQWordArray(xNode,XML.Fields.PrivateMembers,Item.PrivateMembers);
      toQWordArray(xNode,XML.Fields.Admins,Item.Admins);

      Item.Verified:=True;
    end;
    Result:=True;
  end else
    Result:=false;
end;

function  fromXML(xDoc:TXMLDocument; var Item:TNetwork):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Network),Item);
end;

function  fromXML(xDoc:TXMLDocument; var Items:TNetworks):boolean;
var
  xNets:TDOMNode;
  xNet:TDOMNode;
  iLcv,iCount:LongInt;
  itmP:PNetwork;
  iID:QWord;
begin
  Result:=False; iLcv:=0; iCount:=0;
  Invalidate(Items);
  with Core.XML.DB do begin
    xNets:=getNode(xDoc,XML.Stanzas.Networks);
    if xNets<>nil then begin
      for iLcv:=0 to xNets.ChildNodes.Count-1 do begin
        xNet:=xNets.ChildNodes[iLcv];
        if SameText(xNet.NodeName,XML.Stanzas.Network) then begin
          iID:=toQWord(xNet,XML.Fields.ID);
          itmP:=Get(iID,Items);
          if (itmP=nil) then begin
            New(itmP);
            Init(itmP^);
            SetLength(Items,iCount+1);
            Items[iCount]:=itmP;
            Inc(iCount);
          end;
          fromXML(xNet,itmP^);
        end;
      end;
      Result:=True;
    end else
      Result:=false;
  end;
  Purge(Items);
end;

Function  toXML(var Item:TNetworks; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Networks,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Networks,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

function  toXML(var Item:TNetwork; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Network,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.OwnerID,Item.OwnerID),Output);
    Core.Streams.Write(Print(XML.Fields.AvatarID,Item.AvatarID),Output);
    Core.Streams.Write(Print(XML.Fields.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.MemberCount,Item.MemberCount),Output);
    Core.Streams.Write(Print(XML.Fields.RequestCount,Item.RequestCount),Output);

    Core.Streams.Write(Print(XML.Fields.DocumentsID,Item.DocumentsID),Output);
    Core.Streams.Write(Print(XML.Fields.TrashID,Item.TrashID),Output);
    Core.Streams.Write(Print(XML.Fields.PicturesID,Item.PicturesID),Output);
    Core.Streams.Write(Print(XML.Fields.VideosID,Item.VideosID),Output);
    Core.Streams.Write(Print(XML.Fields.MusicID,Item.MusicID),Output);

    Core.Streams.Write(Print(XML.Fields.Privacy,Item.Privacy),Output);
    Core.Streams.Write(Print(XML.Fields.Tag,Item.Tag),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Item.Title,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Item.Description,CDATA_ON),Output);

    Core.Streams.Write(Print(XML.Fields.PublicMembers,Item.PublicMembers),Output);
    Core.Streams.Write(Print(XML.Fields.PrivateMembers,Item.PrivateMembers),Output);
    Core.Streams.Write(Print(XML.Fields.Admins,Item.Admins),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Network,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;


procedure cbReadAdmins(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Core.Database.Types.PLargeWordArray;
begin
  ListP:=DataP;
  Core.Arrays.LargeWord.Add(Fields.FieldByName(Admins.DB.Keys.UserID).AsLargeInt,ListP^,aoCheckForDuplicates);
End;

class function Admins.List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Arrays.LargeWord.Empty(Items);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadAdmins, @Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Admins.Add(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Users:Core.Arrays.Types.LargeWord):boolean;
var
  iLcv:LongInt;
begin
  Result:=True;
  for iLcv:=0 to High(Users) do
    Add(Task,DomainID,Users[iLcv],NetworkID);
end;

class function Admins.Add(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord):boolean;
var
  iCount:LongInt;
  usrCount:QWord;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;  usrCount:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone,Commands);
    Core.Database.SQL.Count(Task, @Commands,usrCount);
    if usrCount=0 then begin
      iCount:=0;
      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.UserID,poNone,oNone,UserID,Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.NetworkID,poNone,oNone,NetworkID,Commands);
      Result := Core.Database.SQL.Insert(Task, @Commands);
    end else begin
      Result:=true;
    end;

  finally
    Core.Database.Done(Commands);
  end;
end;

class function Admins.Count(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; out Items:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;  Items:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone,Commands);
    Result:=Core.Database.SQL.Count(Task, @Commands,Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Admins.Remove(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID,poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);

    Result:=Core.Database.SQL.Delete(Task, @Commands);

  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadMembers(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Core.Database.Types.PLargeWordArray;
begin
  ListP:=DataP;
  Core.Arrays.LargeWord.Add(Fields.FieldByName(Members.DB.Keys.UserID).AsLargeInt,ListP^,aoCheckForDuplicates);
End;

class function Members.List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Arrays.LargeWord.Empty(Items);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadMembers, @Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Members.List(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Items:Core.Arrays.Types.LargeWord; Visibility:Byte):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Arrays.LargeWord.Empty(Items);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Face, poAnd, oEqual, Visibility, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadMembers, @Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Members.Add(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; var Users:Core.Arrays.Types.LargeWord; Visibility:byte):boolean;
var
  iLcv:LongInt;
begin
  Result:=true;
  for iLcv:=0 to High(Users) do
    Add(Task,DomainID,Users[iLcv],NetworkID,Visibility);
end;

class function Members.Add(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord; Visibility:byte):boolean;
var
  iCount:LongInt;
  usrCount:QWord;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;  usrCount:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone,Commands);

    Core.Database.SQL.Count(Task, @Commands,usrCount);
    if usrCount=0 then begin
      iCount:=0;
      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.DomainID,poNone,oNone,DomainID,Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.UserID,poNone,oNone,UserID,Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.NetworkID,poNone,oNone,NetworkID,Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForInsert, DB.IDs.Face,poNone,oNone,Visibility,Commands);

      Result := Core.Database.SQL.Insert(Task, @Commands);
    end else begin
      iCount:=0;
      Core.Database.AddCommand(iCount, DB.TableP,@Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
      Core.Database.AddCommand(iCount, DB.TableP, useForUpdates,   DB.IDs.Face, poNone, oNone, Visibility, Commands);

      Result:=Core.Database.SQL.Update(Task,@Commands);
    end;

  finally
    Core.Database.Done(Commands);
  end;
end;

class function Members.Count(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; out Items:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;  Items:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone, Commands);
    Result:=Core.Database.SQL.Count(Task, @Commands,Items);
  finally
    Core.Database.Done(Commands);
  end;
end;


class function Members.Count(Task:Core.Database.Types.TTask; DomainID,NetworkID:QWord; Visibility:Byte; out Items:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;  Items:=0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.Face, poAnd, oEqual, Visibility, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.UserID, poNone, oNone, Commands);
    Result:=Core.Database.SQL.Count(Task, @Commands,Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Members.Remove(Task:Core.Database.Types.TTask; DomainID,UserID,NetworkID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.NetworkID, poAnd, oEqual, NetworkID, Commands);

    Result:=Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

(*
Fields=class
const
  ID                     = 'id';
  First                  = 'fst';
  Nick                   = 'nck';
  Last                   = 'lst';
  City                   = 'cty';
  State                  = 'ste';
  Post                   = 'pst';
  AvatarID               = 'aid';
end;
*)

class function  Members.List(Task:Core.Database.Types.TTask; DomainID:QWord; var Networks:TNetworks; var Items:Storage.Roster.Items.List):boolean;
var
  IDs:Core.Arrays.Types.LargeWord;
  iLcv:LongInt;
begin
  Core.Arrays.LargeWord.Init(IDs);
  Try
    for iLcv:=0 to High(Networks) do
      Core.Arrays.LargeWord.Add(Networks[iLcv]^.OwnerID,IDs,aoCheckForDuplicates);
    Result:=Storage.Roster.Items.DB.List(Task,DomainID,IDs,Items);
  finally
    Core.Arrays.LargeWord.Done(IDs);
  end;
end;

class function  Members.List(Task:Core.Database.Types.TTask; DomainID:QWord; var Requests:Storage.Social.Network.Requests.TRequests; var Items:Storage.Roster.Items.List):boolean;
var
  IDs:Core.Arrays.Types.LargeWord;
  iLcv:LongInt;
begin
  Core.Arrays.LargeWord.Init(IDs);
  Storage.Roster.Items.Empty(Items);
  Try
    for iLcv:=0 to High(Requests) do begin
      if (Requests[iLcv]^.QueryID<>0) then Core.Arrays.LargeWord.Add(Requests[iLcv]^.QueryID,IDs,aoCheckForDuplicates);
      if (Requests[iLcv]^.ResponseID<>0) then Core.Arrays.LargeWord.Add(Requests[iLcv]^.ResponseID,IDs,aoCheckForDuplicates);
    end;
    Result:= (Length(IDs)>0) and Storage.Roster.Items.DB.List(Task,DomainID,IDs,Items);
  finally
    Core.Arrays.LargeWord.Done(IDs);
  end;
end;

class function  Members.toXML(var Entries:Storage.Roster.Items.List; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanza.Members,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to High(Entries) do
    toXML(Entries[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanza.Members,Output);

  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Members.toXML(var Entry:Storage.Roster.Items.Item; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanza.Member,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.AccountID,Entry.AccountID),Output);
    Core.Streams.Write(Print(XML.Fields.AvatarID,Entry.AvatarID),Output);

    Core.Streams.Write(Print(XML.Fields.First,Entry.FirstName,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Nick,Entry.NickName,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Last,Entry.LastName,CDATA_ON),Output);

    Core.Streams.Write(Print(XML.Fields.City,Entry.City,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.State,Entry.State,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Post,Entry.Post,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Country,Entry.Country,CDATA_ON),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanza.Member,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

initialization
  RegisterDB;

end.


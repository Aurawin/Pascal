unit Storage.Roster;

{
  Storage.Roster.pas is the place where all user related contact information is stored

  Based on One Table

  Copyright Aurawin LLC 2003-2011
  Written by: Andrew Thomas Brunner

  This code is protected under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}


interface

uses

  Core.Strings,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.XML,
  Core.Streams,
  Core.Timer,

  XMLRead,
  DOM,


  Classes,
  SysUtils;

type
  Items = class
  type
    Defaults=Class
    const
      NoAccount                : QWord = 0;
    end;
    Item=record
      ID                       : QWord;
      UserID                   : QWord;
      DomainID                 : QWord;
      ResourceID               : QWord;
      AccountID                : QWord;
      AvatarID                 : QWord;
      Subscription             : LongInt;
      Modified                 : Double;
      URL                      : Core.Strings.VarString;
      Email1                   : Core.Strings.VarString;
      Email2                   : Core.Strings.VarString;
      Email3                   : Core.Strings.VarString;
      Emails                   : Core.Strings.VarString;
      Text1                    : Core.Strings.VarString;
      Text2                    : Core.Strings.VarString;
      Text3                    : Core.Strings.VarString;
      Texts                    : Core.Strings.VarString;
      Folder                   : Core.Strings.VarString;
      NickName                 : Core.Strings.VarString;
      FirstName                : Core.Strings.VarString;
      MiddleName               : Core.Strings.VarString;
      LastName                 : Core.Strings.VarString;
      Address1                 : Core.Strings.VarString;
      Address2                 : Core.Strings.VarString;
      City                     : Core.Strings.VarString;
      State                    : Core.Strings.VarString;
      Post                     : Core.Strings.VarString;
      Country                  : Core.Strings.VarString;
      Phone1                   : Core.Strings.VarString;
      Phone2                   : Core.Strings.VarString;
      Phone3                   : Core.Strings.VarString;
      Phones                   : Core.Strings.VarString;
      Field1                   : Core.Strings.VarString;
      Field2                   : Core.Strings.VarString;
      Field3                   : Core.Strings.VarString;
      Field4                   : Core.Strings.VarString;
      Field5                   : Core.Strings.VarString;
      Fields                   : Core.Strings.VarString;
    end;
    PItem=^Item;
    List=Array of PItem;
    PList=^List;

    XML=class
    type
      Stanzas=Class
      const
        Contacts               = 'contacts';
        Contact                = 'contact';
      end;
      Fields=class
      const
        ID                     = 'id';
        AccountID              = 'auid';
        AvatarID               = 'avid';
        Modified               = 'modified';
        Subscription           = 'ssptn';
        ResourceID             = 'rcid';
        URL                    = 'url';
        Email1                 = 'eml1';
        Email2                 = 'eml2';
        Email3                 = 'eml3';
        Emails                 = 'emls';
        Text1                  = 'txt1';
        Text2                  = 'txt2';
        Text3                  = 'txt3';
        Texts                  = 'txts';
        Folder                 = 'fldr';
        NickName               = 'name-nick';
        FirstName              = 'name-first';
        MiddleName             = 'name-middle';
        LastName               = 'name-family';
        Address1               = 'addr1';
        Address2               = 'addr2';
        City                   = 'city';
        State                  = 'state';
        Post                   = 'zip';
        Country                = 'cntry';
        Phone1                 = 'phn1';
        Phone2                 = 'phn2';
        Phone3                 = 'phn3';
        Phones                 = 'phns';
        Field1                 = 'fld1';
        Field2                 = 'fld2';
        Field3                 = 'fld3';
        Field4                 = 'fld4';
        Field5                 = 'fld5';
        Fields                 = 'flds';
      end;
    end;
    DB = class
    type
      IDs = class
      const
        ID                     : Core.Database.Types.Integer = 0;
        InsertID               : Core.Database.Types.Integer = 1;
        DomainID               : Core.Database.Types.Integer = 2;
        UserID                 : Core.Database.Types.Integer = 3;
        AccountID              : Core.Database.Types.Integer = 4;
        AvatarID               : Core.Database.Types.Integer = 5;
        Modified               : Core.Database.Types.Integer = 6;
        Subscription           : Core.Database.Types.Integer = 7;
        ResourceID             : Core.Database.Types.Integer = 8;
        URL                    : Core.Database.Types.Integer = 9;
        Email1                 : Core.Database.Types.Integer = 10;
        Email2                 : Core.Database.Types.Integer = 11;
        Email3                 : Core.Database.Types.Integer = 12;
        Emails                 : Core.Database.Types.Integer = 13;
        Text1                  : Core.Database.Types.Integer = 14;
        Text2                  : Core.Database.Types.Integer = 15;
        Text3                  : Core.Database.Types.Integer = 16;
        Texts                  : Core.Database.Types.Integer = 17;
        Folder                 : Core.Database.Types.Integer = 18;
        NickName               : Core.Database.Types.Integer = 19;
        FirstName              : Core.Database.Types.Integer = 20;
        MiddleName             : Core.Database.Types.Integer = 21;
        LastName               : Core.Database.Types.Integer = 22;
        Address1               : Core.Database.Types.Integer = 23;
        Address2               : Core.Database.Types.Integer = 24;
        City                   : Core.Database.Types.Integer = 25;
        State                  : Core.Database.Types.Integer = 26;
        Post                   : Core.Database.Types.Integer = 27;
        Country                : Core.Database.Types.Integer = 28;
        Phone1                 : Core.Database.Types.Integer = 29;
        Phone2                 : Core.Database.Types.Integer = 30;
        Phone3                 : Core.Database.Types.Integer = 31;
        Phones                 : Core.Database.Types.Integer = 32;
        Field1                 : Core.Database.Types.Integer = 33;
        Field2                 : Core.Database.Types.Integer = 34;
        Field3                 : Core.Database.Types.Integer = 35;
        Field4                 : Core.Database.Types.Integer = 36;
        Field5                 : Core.Database.Types.Integer = 37;
        Fields                 : Core.Database.Types.Integer = 38;

        // Runtime Fields
        AccountIDs             : Core.Database.Types.Integer = 39;
      end;
      Keys = class
      const
        ID                     : Core.Database.Types.VarString = 'TID';
        InsertID               : Core.Database.Types.VarString = 'TIID';
        DomainID               : Core.Database.Types.VarString = 'TDID';
        UserID                 : Core.Database.Types.VarString = 'TUID';
        AccountID              : Core.Database.Types.VarString = 'AUID';
        AvatarID               : Core.Database.Types.VarString = 'AVID';
        Modified               : Core.Database.Types.VarString = 'MDFD';
        Subscription           : Core.Database.Types.VarString = 'RSUB';
        ResourceID             : Core.Database.Types.VarString = 'RCID';
        URL                    : Core.Database.Types.VarString = 'RURL';
        Email1                 : Core.Database.Types.VarString = 'EML1';
        Email2                 : Core.Database.Types.VarString = 'EML2';
        Email3                 : Core.Database.Types.VarString = 'EML3';
        Emails                 : Core.Database.Types.VarString = 'EMLS';
        Text1                  : Core.Database.Types.VarString = 'TXT1';
        Text2                  : Core.Database.Types.VarString = 'TXT2';
        Text3                  : Core.Database.Types.VarString = 'TXT3';
        Texts                  : Core.Database.Types.VarString = 'TXTS';
        Folder                 : Core.Database.Types.VarString = 'RPTH';
        NickName               : Core.Database.Types.VarString = 'NKNM';
        FirstName              : Core.Database.Types.VarString = 'FNME';
        MiddleName             : Core.Database.Types.VarString = 'MNME';
        LastName               : Core.Database.Types.VarString = 'LNME';
        Address1               : Core.Database.Types.VarString = 'ADR1';
        Address2               : Core.Database.Types.VarString = 'ADR2';
        City                   : Core.Database.Types.VarString = 'RCTY';
        State                  : Core.Database.Types.VarString = 'RPRV';
        Post                   : Core.Database.Types.VarString = 'POST';
        Country                : Core.Database.Types.VarString = 'CTRY';
        Phone1                 : Core.Database.Types.VarString = 'PHN1';
        Phone2                 : Core.Database.Types.VarString = 'PHN2';
        Phone3                 : Core.Database.Types.VarString = 'PHN3';
        Phones                 : Core.Database.Types.VarString = 'PHNS';
        Field1                 : Core.Database.Types.VarString = 'FLD1';
        Field2                 : Core.Database.Types.VarString = 'FLD2';
        Field3                 : Core.Database.Types.VarString = 'FLD3';
        Field4                 : Core.Database.Types.VarString = 'FLD4';
        Field5                 : Core.Database.Types.VarString = 'FLD5';
        Fields                 : Core.Database.Types.VarString = 'FLDS';
        // Runtime Fields
        AccountIDs             : Core.Database.Types.VarString = 'AUID';
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate: True;
        AutoCommit  : True;
        Group: 'System/Applications';
        Name: 'Contacts';
        Value: 'scs_rstr';
        Hint: 'Contacts storage';
        PrimaryKeyP: @Keys.ID;
        );
      Fields: array [0..39] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.AccountID; KeyP: @Keys.AccountID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.AvatarID; KeyP: @Keys.AvatarID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Subscription; KeyP: @Keys.Subscription; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ResourceID; KeyP: @Keys.ResourceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.URL; KeyP: @Keys.URL; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Email1; KeyP: @Keys.Email1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Email2; KeyP: @Keys.Email2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Email3; KeyP: @Keys.Email3; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Emails; KeyP: @Keys.Emails; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Text1; KeyP: @Keys.Text1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Text2; KeyP: @Keys.Text2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Text3; KeyP: @Keys.Text3; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Texts; KeyP: @Keys.Texts; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Folder; KeyP: @Keys.Folder; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.NickName; KeyP: @Keys.NickName; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.FirstName; KeyP: @Keys.FirstName; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.MiddleName; KeyP: @Keys.MiddleName; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.LastName; KeyP: @Keys.LastName; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Address1; KeyP: @Keys.Address1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Address2; KeyP: @Keys.Address2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.City; KeyP: @Keys.City; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.State; KeyP: @Keys.State; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Post; KeyP: @Keys.Post; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Country; KeyP: @Keys.Country; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Phone1; KeyP: @Keys.Phone1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Phone2; KeyP: @Keys.Phone2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Phone3; KeyP: @Keys.Phone3; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Phones; KeyP: @Keys.Phones; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Field1; KeyP: @Keys.Field1; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Field2; KeyP: @Keys.Field2; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Field3; KeyP: @Keys.Field3; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Field4; KeyP: @Keys.Field4; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Field5; KeyP: @Keys.Field5; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Fields; KeyP: @Keys.Fields; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.AccountIDs; KeyP: @Keys.AccountIDs; DataType: dftQWordArray; AutoCreate: False; Verified: True; Precision: 1024*1024*4; Flags: cfNone;  )
      );
      class Function  Fill(Task:Core.Database.Types.TTask; DomainID, UserID, ResourceID:QWord; ItemsP:PList):Boolean;
      class Function  Update(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;
      class Function  Refresh(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; DomainID,UserID,AccountID:QWord; Var Entry:Item):Boolean; overload;
      class Function  Add(Task:Core.Database.Types.TTask; DomainID,UserID,AccountID:QWord; Var Entries:List; Var Entry:Item):Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):Boolean;
      class Function  List(Task:Core.Database.Types.TTask; DomainID:QWord; Accounts:Core.Arrays.Types.LargeWord; var Entries:List):Boolean;
      class Function  setAvatarID(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID,AvatarID:QWord; Var Modified:Double):Boolean;
    end;

    class procedure Init(Var Entry:Item); overload;
    class procedure Init(Var Entries:List); overload;

    class procedure Copy(Var Source,Destination:Item); overload;
    class procedure Copy(Var Source,Destination:List); overload;

    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:List); overload;

    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:List); overload;

    class Function  IndexOf(Var Entries:List; iID:QWord): LongInt; overload;
    class Function  IndexOf(Var Entries:List; Var jID:Core.Strings.VarString): LongInt; overload;

    class Function  fromXML(xDoc:TXMLDocument; var Entry:Item; DomainID,UserID:QWord):boolean; overload;
    class Function  fromXML(xItem:TDOMNode; var Entry:Item; DomainID,UserID:QWord):boolean; overload;
    class Function  toXML(var Entries:List; Output:TMemoryStream):boolean; overload;
    class Function  toXML(var Entry:Item; Output:TMemoryStream):boolean; overload;

    class Function  SetSize(Entries:List; Count:LongInt): LongInt; overload;

  end;


implementation
uses db;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

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
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
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
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyTable,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

procedure CB_FillRoster(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iLcv,iItemIndex,iItemID:LongInt;
  RosterP:Items.PList;
  ItemP:Items.PItem;
begin
  RosterP:=DataP;
  iItemID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iItemIndex:=Items.IndexOf(RosterP^,iItemID);
  If iItemIndex=-1 then begin
    New(ItemP);
    Items.Init(ItemP^);
    ItemP^.ID:=iItemID;
    iItemIndex:=Length(RosterP^);
    SetLength(RosterP^,iItemIndex+1);
    RosterP^[iItemIndex]:=ItemP;
  end;
  {$i Storage.Roster.Fill.inc}
end;

class function  Items.DB.Fill(Task:Core.Database.Types.TTask; DomainID, UserID, ResourceID:QWord; ItemsP:PList):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Empty(ItemsP^);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    if ResourceID<>0 then
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ResourceID,poAnd,oEqual,ResourceID,Commands);
    // Fields
    {$i Storage.Roster.Fields.inc}

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillRoster,ItemsP);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.setAvatarID(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID,AvatarID:QWord; Var Modified:Double):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.AvatarID,poNone,oNone,AvatarID,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Items.DB.Update(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Entry.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,Entry.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,Entry.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Subscription,poNone,oNone,Entry.Subscription,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.URL,poNone,oNone,Entry.URL,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Email1,poNone,oNone,Entry.Email1,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Email2,poNone,oNone,Entry.Email2,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Email3,poNone,oNone,Entry.Email3,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Emails,poNone,oNone,Entry.Emails,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Text1,poNone,oNone,Entry.Text1,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Text2,poNone,oNone,Entry.Text2,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Text3,poNone,oNone,Entry.Text3,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Texts,poNone,oNone,Entry.Texts,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Folder,poNone,oNone,Entry.Folder,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.NickName,poNone,oNone,Entry.NickName,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.FirstName,poNone,oNone,Entry.FirstName,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.MiddleName,poNone,oNone,Entry.MiddleName,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.LastName,poNone,oNone,Entry.LastName,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Address1,poNone,oNone,Entry.Address1,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Address2,poNone,oNone,Entry.Address2,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.City,poNone,oNone,Entry.City,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.State,poNone,oNone,Entry.State,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Post,poNone,oNone,Entry.Post,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Country,poNone,oNone,Entry.Country,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Phone1,poNone,oNone,Entry.Phone1,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Phone2,poNone,oNone,Entry.Phone2,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Phone3,poNone,oNone,Entry.Phone3,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Phones,poNone,oNone,Entry.Phones,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Field1,poNone,oNone,Entry.Field1,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Field2,poNone,oNone,Entry.Field2,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Field3,poNone,oNone,Entry.Field3,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Field4,poNone,oNone,Entry.Field4,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Field5,poNone,oNone,Entry.Field5,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Fields,poNone,oNone,Entry.Fields,Commands);


    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_RefreshItem(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemP:Items.PItem;
begin
  ItemP:=DataP;
  {$i Storage.Roster.Fill.inc}
end;

class Function  Items.DB.Refresh(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,Entry.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Modified,poAnd,oNotEqual,Entry.Modified,Commands);
    {$i Storage.Roster.Fields.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_RefreshItem,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,Entry.UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,Entry.ID,Commands);
    {$i Storage.Roster.Fields.inc}
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_RefreshItem,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.DB.Add(Task:Core.Database.Types.TTask; DomainID,UserID,AccountID:QWord; Var Entries:List; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
  ItemP                          : PItem;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    iInsertID:=Random(High(Int64));
    Entry.ID:=0;
    Entry.DomainID:=DomainID;
    Entry.AccountID:=AccountID;
    Entry.UserID:=UserID;
    Entry.Modified:=Core.Timer.dtUT;

    {$i Storage.Roster.Add.inc}

    Result:=( Core.Database.SQL.Insert(Task,@Commands) and (Entry.ID<>0) );
    If Result then begin
      New(ItemP);
      Copy(Entry,ItemP^);
      iCount:=Length(Entries);
      SetLength(Entries,iCount+1);
      Entries[iCount]:=ItemP;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.DB.Add(Task:Core.Database.Types.TTask; DomainID,UserID,AccountID:QWord; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    iInsertID:=Random(High(Int64));
    Entry.ID:=0;
    Entry.Modified:=Core.Timer.dtUT;
    Entry.DomainID:=DomainID;
    Entry.AccountID:=AccountID;
    Entry.UserID:=UserID;
    {$i Storage.Roster.Add.inc}

    Result:=( Core.Database.SQL.Insert(Task,@Commands) and (Entry.ID<>0) );
  Finally
    Core.Database.Done(Commands);
  End;
end;


class function  Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure cb_ListItems(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  idx:LongInt;
  auID:QWord;
  RosterP:Items.PList;
  ItemP:Items.PItem;
begin
  RosterP:=DataP;
  New(ItemP);
  Items.Init(ItemP^);

  idx:=Length(RosterP^);
  SetLength(RosterP^,idx+1);
  RosterP^[idx]:=ItemP;

  ItemP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  ItemP^.AccountID:=Fields.FieldByName(Items.DB.Keys.AccountID).AsLargeInt;
  ItemP^.AvatarID:=Fields.FieldByName(Items.DB.Keys.AvatarID).AsLargeInt;
  ItemP^.URL:=Fields.FieldByName(Items.DB.Keys.URL).AsString;
  ItemP^.NickName:=Fields.FieldByName(Items.DB.Keys.NickName).AsString;
  ItemP^.FirstName:=Fields.FieldByName(Items.DB.Keys.FirstName).AsString;
  ItemP^.LastName:=Fields.FieldByName(Items.DB.Keys.LastName).AsString;
  ItemP^.City:=Fields.FieldByName(Items.DB.Keys.City).AsString;
  ItemP^.State:=Fields.FieldByName(Items.DB.Keys.State).AsString;
  ItemP^.Post:=Fields.FieldByName(Items.DB.Keys.Post).AsString;
  ItemP^.Country:=Fields.FieldByName(Items.DB.Keys.Country).AsString;
end;


class Function  Items.DB.List(Task:Core.Database.Types.TTask; DomainID:QWord; Accounts:Core.Arrays.Types.LargeWord; var Entries:List):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  sCriteria:Core.Strings.VarString;
begin
  Try
    //sCriteria:=toInCriteria(Accounts);
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.AccountIDs,poAnd,oIn,Accounts,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.AccountID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.AvatarID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.URL,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.NickName,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.FirstName,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastName,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.City,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.State,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Post,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Country,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_ListItems,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.fromXML(xItem:TDOMNode; var Entry:Item; DomainID,UserID:QWord):boolean;
begin
  Entry.DomainID:=DomainID;
  Entry.UserID:=UserID;
  with Core.XML.DB do begin
    if (xItem<>nil) then begin
      Entry.ID:=toQWord(xItem,XML.Fields.ID);
      Entry.ResourceID:=toQWord(xItem,XML.Fields.ResourceID);
      Entry.AccountID:=toQWord(xItem,XML.Fields.AccountID);
      Entry.AvatarID:=toQWord(xItem,XML.Fields.AvatarID);
      Entry.Subscription:=toInteger(xItem,XML.Fields.Subscription);
      Entry.URL:=toString(xItem,Items.XML.Fields.URL);
      Entry.Email1:=toString(xItem,Items.XML.Fields.Email1);
      Entry.Email2:=toString(xItem,Items.XML.Fields.Email2);
      Entry.Email3:=toString(xItem,Items.XML.Fields.Email3);
      Entry.Emails:=toString(xItem,Items.XML.Fields.Emails);
      Entry.Text1:=toString(xItem,Items.XML.Fields.Text1);
      Entry.Text2:=toString(xItem,Items.XML.Fields.Text2);
      Entry.Text3:=toString(xItem,Items.XML.Fields.Text3);
      Entry.Texts:=toString(xItem,Items.XML.Fields.Texts);
      Entry.Folder:=toString(xItem,Items.XML.Fields.Folder);
      Entry.NickName:=toString(xItem,Items.XML.Fields.NickName);
      Entry.FirstName:=toString(xItem,Items.XML.Fields.FirstName);
      Entry.MiddleName:=toString(xItem,Items.XML.Fields.MiddleName);
      Entry.LastName:=toString(xItem,Items.XML.Fields.LastName);
      Entry.Address1:=toString(xItem,Items.XML.Fields.Address1);
      Entry.Address2:=toString(xItem,Items.XML.Fields.Address2);
      Entry.City:=toString(xItem,Items.XML.Fields.City);
      Entry.State:=toString(xItem,Items.XML.Fields.State);
      Entry.Post:=toString(xItem,Items.XML.Fields.Post);
      Entry.Country:=toString(xItem,Items.XML.Fields.Country);
      Entry.Phone1:=toString(xItem,Items.XML.Fields.Phone1);
      Entry.Phone2:=toString(xItem,Items.XML.Fields.Phone2);
      Entry.Phone3:=toString(xItem,Items.XML.Fields.Phone3);
      Entry.Phones:=toString(xItem,Items.XML.Fields.Phones);
      Entry.Field1:=toString(xItem,Items.XML.Fields.Field1);
      Entry.Field2:=toString(xItem,Items.XML.Fields.Field2);
      Entry.Field3:=toString(xItem,Items.XML.Fields.Field3);
      Entry.Field4:=toString(xItem,Items.XML.Fields.Field4);
      Entry.Field5:=toString(xItem,Items.XML.Fields.Field5);
      Entry.Fields:=toString(xItem,Items.XML.Fields.Fields);
      Result:=True;
    end;
  end;
end;

class function  Items.fromXML(xDoc:TXMLDocument; var Entry:Item; DomainID,UserID:QWord):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Contact),Entry,DomainID,UserID);
end;

class procedure Items.Init(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    UserID:=0;
    DomainID:=0;
    ResourceID:=0;
    AccountID:=0;
    AvatarID:=0;
    Subscription:=0;
    Modified:=0.0;
    SetLength(URL,0);
    SetLength(Email1,0);
    SetLength(Email2,0);
    SetLength(Email3,0);
    SetLength(Emails,0);
    SetLength(Text1,0);
    SetLength(Text2,0);
    SetLength(Text3,0);
    SetLength(Texts,0);
    SetLength(Folder,0);
    SetLength(NickName,0);
    SetLength(MiddleName,0);
    SetLength(LastName,0);
    SetLength(Address1,0);
    SetLength(Address2,0);
    SetLength(City,0);
    SetLength(State,0);
    SetLength(Post,0);
    SetLength(Country,0);
    SetLength(Phone1,0);
    SetLength(Phone2,0);
    SetLength(Phone3,0);
    SetLength(Phones,0);
    SetLength(Field1,0);
    SetLength(Field2,0);
    SetLength(Field3,0);
    SetLength(Field4,0);
    SetLength(Field5,0);
    SetLength(Fields,0);
  end;
end;

class procedure Items.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    UserID:=0;
    DomainID:=0;
    ResourceID:=0;
    AccountID:=0;
    AvatarID:=0;
    Subscription:=0;
    Modified:=0.0;
    SetLength(URL,0);
    SetLength(Email1,0);
    SetLength(Email2,0);
    SetLength(Email3,0);
    SetLength(Emails,0);
    SetLength(Text1,0);
    SetLength(Text2,0);
    SetLength(Text3,0);
    SetLength(Texts,0);
    SetLength(Folder,0);
    SetLength(NickName,0);
    SetLength(MiddleName,0);
    SetLength(LastName,0);
    SetLength(Address1,0);
    SetLength(Address2,0);
    SetLength(City,0);
    SetLength(State,0);
    SetLength(Post,0);
    SetLength(Country,0);
    SetLength(Phone1,0);
    SetLength(Phone2,0);
    SetLength(Phone3,0);
    SetLength(Phones,0);
    SetLength(Field1,0);
    SetLength(Field2,0);
    SetLength(Field3,0);
    SetLength(Field4,0);
    SetLength(Field5,0);
    SetLength(Fields,0);
  end;
end;

class procedure Items.Done(Var Entry:Item);
begin
  With Entry do begin
    Finalize(URL);
    Finalize(Email1);
    Finalize(Email2);
    Finalize(Email3);
    Finalize(Emails);
    Finalize(Text1);
    Finalize(Text2);
    Finalize(Text3);
    Finalize(Texts);
    Finalize(Folder);
    Finalize(NickName);
    Finalize(MiddleName);
    Finalize(LastName);
    Finalize(Address1);
    Finalize(Address2);
    Finalize(City);
    Finalize(State);
    Finalize(Post);
    Finalize(Country);
    Finalize(Phone1);
    Finalize(Phone2);
    Finalize(Phone3);
    Finalize(Phones);
    Finalize(Field1);
    Finalize(Field2);
    Finalize(Field3);
    Finalize(Field4);
    Finalize(Field5);
    Finalize(Fields);
  end;
  Finalize(Entry);
end;

class procedure Items.Init(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Empty(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  SetLength(Entries,0);
end;

class procedure Items.Done(Var Entries:List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do begin
    Done(Entries[iLcv]^);
    Dispose(Entries[iLcv]);
  end;
  Finalize(Entries);
end;

class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.UserID:=Source.UserID;
  Destination.DomainID:=Source.DomainID;
  Destination.ResourceID:=Source.ResourceID;
  Destination.AccountID:=Source.AccountID;
  Destination.AvatarID:=Source.AvatarID;
  Destination.Subscription:=Source.Subscription;
  Destination.Modified:=Source.Modified;
  Destination.URL:=Source.URL;
  Destination.Email1:=Source.Email1;
  Destination.Email2:=Source.Email2;
  Destination.Email3:=Source.Email3;
  Destination.Emails:=Source.Emails;
  Destination.Text1:=Source.Text1;
  Destination.Text2:=Source.Text2;
  Destination.Text3:=Source.Text3;
  Destination.Texts:=Source.Texts;
  Destination.Folder:=Source.Folder;
  Destination.NickName:=Source.NickName;
  Destination.FirstName:=Source.FirstName;
  Destination.MiddleName:=Source.MiddleName;
  Destination.LastName:=Source.LastName;
  Destination.Address1:=Source.Address1;
  Destination.Address2:=Source.Address2;
  Destination.City:=Source.City;
  Destination.State:=Source.State;
  Destination.Post:=Source.Post;
  Destination.Country:=Source.Country;
  Destination.Phone1:=Source.Phone1;
  Destination.Phone2:=Source.Phone2;
  Destination.Phone3:=Source.Phone3;
  Destination.Phones:=Source.Phones;
  Destination.Field1:=Source.Field1;
  Destination.Field2:=Source.Field2;
  Destination.Field3:=Source.Field3;
  Destination.Field4:=Source.Field4;
  Destination.Field5:=Source.Field5;
  Destination.Fields:=Source.Fields;
end;

class Function  Items.toXML(var Entries:List; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Contacts,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to High(Entries) do
    toXML(Entries[iLcv]^,Output);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Contacts,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

class Function  Items.toXML(var Entry:Item; Output:TMemoryStream):boolean;
begin
  Result:=true;
  Output.Seek(0,soFromEnd);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Contact,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Entry.ID),Output);
    Core.Streams.Write(Print(XML.Fields.ResourceID,Entry.ResourceID),Output);
    Core.Streams.Write(Print(XML.Fields.AccountID,Entry.AccountID),Output);
    Core.Streams.Write(Print(XML.Fields.AvatarID,Entry.AvatarID),Output);
    Core.Streams.Write(Print(XML.Fields.Subscription,Entry.Subscription),Output);
    Core.Streams.Write(Print(XML.Fields.Modified,Entry.Modified),Output);
    Core.Streams.Write(Print(XML.Fields.URL,Entry.URL,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Email1,Entry.Email1,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Email2,Entry.Email2,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Email3,Entry.Email3,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Emails,Entry.Emails,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Text1,Entry.Text1,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Text2,Entry.Text2,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Text3,Entry.Text3,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Texts,Entry.Texts,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Folder,Entry.Folder,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.NickName,Entry.NickName,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.FirstName,Entry.FirstName,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.MiddleName,Entry.MiddleName,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.LastName,Entry.LastName,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Address1,Entry.Address1,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Address2,Entry.Address2,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.City,Entry.City,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.State,Entry.State,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Post,Entry.Post,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Country,Entry.Country,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Phone1,Entry.Phone1,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Phone2,Entry.Phone2,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Phone3,Entry.Phone3,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.Phones,Entry.Phones,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Field1,Entry.Field1,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Field2,Entry.Field2,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Field3,Entry.Field3,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Field4,Entry.Field4,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Field5,Entry.Field5,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Fields,Entry.Fields,CDATA_ON),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Contact,Output);
  Core.Streams.Write('>',1,Output);
end;


class procedure Items.Copy(Var Source,Destination:List);
var
  iLen,iLcv:LongInt;
begin
  iLen:=Length(Source);
  SetSize(Destination,iLen);
  for iLcv:=0 to iLen-1 do
    Copy(Source[iLcv]^,Destination[iLcv]^);
end;

class function  Items.IndexOf(Var Entries:List; iID:QWord): LongInt;
var
 iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Entries) do begin
    If Entries[iLcv]^.ID=iID then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class function  Items.IndexOf(Var Entries:List; Var jID:Core.Strings.VarString): LongInt;
var
 iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Entries) do begin
    If SameText(Entries[iLcv]^.Text1,jID) or
       SameText(Entries[iLcv]^.Text2,jID) or
       SameText(Entries[iLcv]^.Text3,jID)
    then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class function  Items.SetSize(Entries:List; Count:LongInt): LongInt;
var
  iCount:LongInt;
  iLcv:LongInt;
begin
  iCount:=System.Length(Entries);
  iLcv:=iCount;
  While (iLcv<Count) and (iLcv>0) do begin // shrink
    Done(Entries[iLcv-1]^);
    Dispose(Entries[iLcv-1]);
    Dec(iLcv);
    Dec(iCount);
  end;
  iLcv:=iCount;
  SetLength(Entries,Count);
  While (iLcv<Count) do begin // grow
    New(Entries[iLcv]);
    Init(Entries[iLcv]^);
    Inc(iLcv);
    Inc(iCount);
  end;
  Result:=iCount;
end;

initialization
  RegisterDB;
end.


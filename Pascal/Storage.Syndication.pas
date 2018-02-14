{
unit Storage.Syndication.pas

Offers Syndication Services to System

  The Syndication Data Store
  Admin is where all admin data is stored.
  Feeds are where the actual feeds are stored.
  Items are where the individual items are stored.
  Streams are where the actual compiled versions are stored.

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

}
unit Storage.Syndication;

interface

uses
  Classes,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,
  Core.Arrays.Bytes,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  SysUtils;
Type
  Feed=class
  type
    Item=record
      ID                         : QWord;
      DomainID                   : QWord;
      RootID                     : QWord;

      PubDate                    : Double;
      LastBuildDate              : Double;
      DefaultExpires             : Double;
      Title                      : Core.Strings.VarString;
      Link                       : Core.Strings.VarString;
      Description                : Core.Strings.VarString;
      Webmaster                  : Core.Strings.VarString;
      Editor                     : Core.Strings.VarString;
      Docs                       : Core.Strings.VarString;
    end;
    PItem=^Item;
    Items=Array of Item;
    PItems=^Items;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        RootID                   : Core.Database.Types.Integer = 3;
        DefaultExpires           : Core.Database.Types.Integer = 4;
        Webmaster                : Core.Database.Types.Integer = 5;
        Editor                   : Core.Database.Types.Integer = 6;
        Title                    : Core.Database.Types.Integer = 7;
        Link                     : Core.Database.Types.Integer = 8;
        Description              : Core.Database.Types.Integer = 9;
        PubDate                  : Core.Database.Types.Integer = 10;
        LastBuildDate            : Core.Database.Types.Integer = 11;
        Docs                     : Core.Database.Types.Integer = 12;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        RootID                   : Core.Database.Types.VarString = 'ITMRID';
        DefaultExpires           : Core.Database.Types.VarString = 'ITMEXP';
        Webmaster                : Core.Database.Types.VarString = 'ITMWMR';
        Editor                   : Core.Database.Types.VarString = 'ITMEDR';
        Title                    : Core.Database.Types.VarString = 'ITMTIT';
        Link                     : Core.Database.Types.VarString = 'ITMLNK';
        Description              : Core.Database.Types.VarString = 'ITMDES';
        PubDate                  : Core.Database.Types.VarString = 'ITMPDT';
        LastBuildDate            : Core.Database.Types.VarString = 'ITMBDT';
        Docs                     : Core.Database.Types.VarString = 'ITMDCS';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Users/Syndication';
        Name                     : 'Feeds';
        Value                    : 'scs_syn_feeds';
        Hint                     : 'Syndication feeds storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..12] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.RootID; KeyP: @Keys.RootID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.DefaultExpires; KeyP: @Keys.DefaultExpires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Webmaster; KeyP: @Keys.Webmaster; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Editor; KeyP: @Keys.Editor; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Link; KeyP: @Keys.Link; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*65; Flags: cfNone;  ),
        (IDP: @IDs.PubDate; KeyP: @Keys.PubDate; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.LastBuildDate; KeyP: @Keys.LastBuildDate; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Docs; KeyP: @Keys.Docs; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  )
      );
      class Function  Retrieve(Task:Core.Database.Types.TTask; FeedID:QWord; Var Entry:Item):Boolean; overload;
      class Function  Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean; overload;
      class Function  Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; Var Entries:Items):Boolean; overload;
      class Function  Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Create(Task:Core.Database.Types.TTask; RootID:QWord; Var Entry:Item):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
    end;
    class Function  ToString(var Entry:Item; Const Delim:Core.Strings.VarString=#2):Core.Strings.VarString;
    class procedure Init(Var Entry:Item); overload;
    class procedure Init(Var Entries:Items); overload;
    class procedure Empty(Var Entry:Item); overload;
    class procedure Empty(Var Entries:Items); overload;
    class procedure Done(Var Entry:Item); overload;
    class procedure Done(Var Entries:Items); overload;
    class procedure Copy(Var Source,Destination:Item);
  end;
  Admin=class
  type
    Item=record
      ID                         : QWord;
      DomainID                   : QWord;
      RootID                     : QWord;
      DefaultExpires             : Double;
      Webmaster                  : Core.Strings.VarString;
      Editor                     : Core.Strings.VarString;
      DefaultDocs                : Core.Strings.VarString;
      Feeds                      : Core.Arrays.Types.LargeWord;
    end;
    PItem=^Item;

    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        RootID                   : Core.Database.Types.Integer = 3;
        Webmaster                : Core.Database.Types.Integer = 4;
        Editor                   : Core.Database.Types.Integer = 5;
        DefaultDocs              : Core.Database.Types.Integer = 6;
        DefaultExpires           : Core.Database.Types.Integer = 7;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        RootID                   : Core.Database.Types.VarString = 'ITMRID';
        Webmaster                : Core.Database.Types.VarString = 'ITMWMSTR';
        Editor                   : Core.Database.Types.VarString = 'ITMEDITR';
        DefaultDocs              : Core.Database.Types.VarString = 'ITMDDOCS';
        DefaultExpires           : Core.Database.Types.VarString = 'ITMDEXP';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Users/Syndication';
        Name                     : 'Administration';
        Value                    : 'scs_syn_admin';
        Hint                     : 'Syndication administration storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..7] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.RootID; KeyP: @Keys.RootID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Webmaster; KeyP: @Keys.Webmaster; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Editor; KeyP: @Keys.Editor; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.DefaultDocs; KeyP: @Keys.DefaultDocs; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.DefaultExpires; KeyP: @Keys.DefaultExpires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class Function  Retrieve(Task:Core.Database.Types.TTask; UserID: QWord; Var Entry:Item):Boolean;
      class Function  Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
    end;
    class procedure Init(Var Entry:Item);
    class procedure Empty(Var Entry:Item);
    class procedure Done(Var Entry:Item);
    class procedure Copy(Var Source,Destination:Item);
  end;
  Items=class
  type
    Item=record
      ID                         : QWord;
      DomainID                   : QWord;
      RootID                     : QWord;
      FeedID                     : QWord;
      Expires                    : Double;
      PubDate                    : Double;
      Title                      : Core.Strings.VarString;
      Link                       : Core.Strings.VarString;
      Description                : Core.Strings.VarString;
      GUID                       : Core.Strings.VarString;
    end;
    PItem=^Item;
    Items=Array of Item;
    PItems=^Items;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        RootID                   : Core.Database.Types.Integer = 3;
        FeedID                   : Core.Database.Types.Integer = 4;
        Expires                  : Core.Database.Types.Integer = 5;
        PubDate                  : Core.Database.Types.Integer = 6;
        Title                    : Core.Database.Types.Integer = 7;
        Link                     : Core.Database.Types.Integer = 8;
        GUID                     : Core.Database.Types.Integer = 9;
        Description              : Core.Database.Types.Integer = 10;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        RootID                   : Core.Database.Types.VarString = 'ITMRID';
        FeedID                   : Core.Database.Types.VarString = 'ITMFID';
        Expires                  : Core.Database.Types.VarString = 'ITMEXP';
        PubDate                  : Core.Database.Types.VarString = 'ITMPDT';
        Title                    : Core.Database.Types.VarString = 'ITMTIT';
        Link                     : Core.Database.Types.VarString = 'ITMLNK';
        GUID                     : Core.Database.Types.VarString = 'ITMGID';
        Description              : Core.Database.Types.VarString = 'ITMDSC';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Users/Syndication';
        Name                     : 'Items';
        Value                    : 'scs_syn_itms';
        Hint                     : 'Syndication item storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..10] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.RootID; KeyP: @Keys.RootID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.FeedID; KeyP: @Keys.FeedID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Expires; KeyP: @Keys.Expires; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.PubDate; KeyP: @Keys.PubDate; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Link; KeyP: @Keys.Link; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.GUID; KeyP: @Keys.GUID; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*65; Flags: cfNone; )
      );
      class Function  Create(Task:Core.Database.Types.TTask; RootID,FeedID:QWord; Var Entry:Item):Boolean;
      class Function  Retrieve(Task:Core.Database.Types.TTask; ItemID:QWord; Var Entry:Item):Boolean; overload;
      class Function  Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean; overload;
      class Function  Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; dtCutoff:Double; Var Entries:Core.Arrays.Types.LargeWord):Boolean; overload;
      class Function  Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
    end;
    class procedure Empty(Var Entry:Item);
    class procedure Copy(Var Source,Destination:Item);
  end;
  Stream=class
  type
    Item=record
      ID                         : QWord;
      DomainID                   : QWord;
      RootID                     : QWord;
      FeedID                     : QWord;
      Data                       : Core.Arrays.Types.Bytes;
    end;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        RootID                   : Core.Database.Types.Integer = 3;
        FeedID                   : Core.Database.Types.Integer = 4;
        Data                     : Core.Database.Types.Integer = 5;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        RootID                   : Core.Database.Types.VarString = 'ITMRID';
        FeedID                   : Core.Database.Types.VarString = 'ITMFID';
        Data                     : Core.Database.Types.VarString = 'ITMFDT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Users/Syndication';
        Name                     : 'Streams';
        Value                    : 'scs_syn_strms';
        Hint                     : 'Syndication stream storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..5] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.RootID; KeyP: @Keys.RootID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.FeedID; KeyP: @Keys.FeedID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*1024*1; Flags: cfNone; )
      );
    end;
    class procedure Empty(Var Entry:Item);
    class procedure Copy(Var Source,Destination:Item);
  end;

implementation
uses
  db;

procedure cbDestroyAdmin(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Admin.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyFeeds(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Feed.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyItems(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyStreams(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Stream.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Admin.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Admin.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Admin.DB.TableP,useForCriteria,Admin.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Feed.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Feed.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Feed.DB.TableP,useForCriteria,Feed.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Stream.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Stream.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Stream.DB.TableP,useForCriteria,Stream.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

  procedure PushUserDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Admin.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Admin.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Admin.DB.TableP,useForCriteria,Admin.DB.IDs.RootID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Feed.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Feed.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Feed.DB.TableP,useForCriteria,Feed.DB.IDs.RootID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.RootID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Stream.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Stream.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Stream.DB.TableP,useForCriteria,Items.DB.IDs.RootID,poNone,oEqual,ItemID,Commands);
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

procedure RegisterDBM;
var
  iLcv                           : LongInt;
begin
  With Admin.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyAdmin, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  With Feed.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFeeds, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  With Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyItems, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  With Stream.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyStreams, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Admin.Init(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    DomainID:=0;
    RootID:=0;
    DefaultExpires:=0;
    SetLength(Webmaster,0);
    SetLength(Editor,0);
    SetLength(DefaultDocs,0);
    SetLength(Feeds,0);
  end;
end;

class procedure Admin.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    DomainID:=0;
    RootID:=0;
    DefaultExpires:=0;
    Core.Strings.Empty(Webmaster);
    Core.Strings.Empty(Editor);
    Core.Strings.Empty(DefaultDocs);
    Core.Arrays.LargeWord.Empty(Feeds);
  end;
end;

class procedure Admin.Done(Var Entry:Item);
begin
  with Entry do begin
    Core.Strings.Done(Webmaster);
    Core.Strings.Done(Editor);
    Core.Strings.Done(DefaultDocs);
    Core.Arrays.LargeWord.Done(Feeds);  // Feeds he's managing must do query to get this data...
  end;
  Finalize(Entry);
end;

class Function  Feed.ToString(var Entry:Item; Const Delim:Core.Strings.VarString=#2):Core.Strings.VarString;
begin
  Result:=Concat(
    IntToStr(Entry.ID),Delim,
    FloatToStr(Entry.PubDate),Delim,
    FloatToStr(Entry.LastBuildDate),Delim,
    FloatToStr(Entry.DefaultExpires),Delim,
    Entry.Editor,Delim,
    Entry.Webmaster,Delim,
    Entry.Title,Delim,
    Entry.Link,Delim,
    Entry.Description,Delim,
    Entry.Docs
  );
end;

class procedure Feed.Init(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    DomainID:=0;
    RootID:=0;
    PubDate:=0;
    LastBuildDate:=0;
    DefaultExpires:=0;
    SetLength(Title,0);
    SetLength(Link,0);
    SetLength(Description,0);
    SetLength(Webmaster,0);
    SetLength(Editor,0);
    SetLength(Docs,0);
  end;
end;

class procedure Feed.Empty(Var Entry:Item);
begin
  with Entry do begin
    ID:=0;
    DomainID:=0;
    RootID:=0;
    PubDate:=0;
    LastBuildDate:=0;
    DefaultExpires:=0;
    Core.Strings.Empty(Editor);
    Core.Strings.Empty(Webmaster);
    Core.Strings.Empty(Title);
    Core.Strings.Empty(Link);
    Core.Strings.Empty(Description);
    Core.Strings.Empty(Docs);
  end;
end;

class procedure Feed.Done(Var Entry:Item);
begin
  with Entry do begin
    Core.Strings.Done(Editor);
    Core.Strings.Done(Webmaster);
    Core.Strings.Done(Title);
    Core.Strings.Done(Link);
    Core.Strings.Done(Description);
    Core.Strings.Done(Docs);
  end;
  Finalize(Entry);
end;

class procedure Feed.Init(Var Entries:Items);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Feed.Empty(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Feed.Done(Var Entries:Items);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Items.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    DomainID:=0;
    RootID:=0;
    FeedID:=0;
    Expires:=0;
    PubDate:=0;
    Core.Strings.Empty(Title);
    Core.Strings.Empty(Link);
    Core.Strings.Empty(Description);
    Core.Strings.Empty(GUID);
  end;
end;

class procedure Stream.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    DomainID:=0;
    RootID:=0;
    FeedID:=0;
    Core.Arrays.Bytes.Empty(Data);
  end;
end;


class procedure Admin.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.RootID:=Source.RootID;
  Destination.DefaultExpires:=Source.DefaultExpires;
  Destination.Webmaster:=Source.WebMaster;
  Destination.WebMaster:=Source.Editor;
  Destination.DefaultDocs:=Source.DefaultDocs;
  Core.Arrays.LargeWord.Copy(Source.Feeds,Destination.Feeds);
end;

class procedure Feed.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.RootID:=Source.RootID;
  Destination.PubDate:=Source.PubDate;
  Destination.LastBuildDate:=Source.LastBuildDate;
  Destination.Title:=Source.Title;
  Destination.Link:=Source.Link;
  Destination.Description:=Source.Description;
  Destination.Docs:=Source.Docs;
end;

class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.RootID:=Source.RootID;
  Destination.FeedID:=Source.FeedID;
  Destination.Expires:=Source.FeedID;
  Destination.PubDate:=Source.PubDate;
  Destination.Title:=Source.Title;
  Destination.Link:=Source.Link;
  Destination.GUID:=Source.GUID;
end;

class procedure Stream.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.RootID:=Source.RootID;
  Destination.FeedID:=Source.FeedID;
  Destination.Data:=Source.Data;
end;

Procedure CB_Syndication_Admin_Retrieve(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  AdminP:Admin.PItem;
begin
  AdminP:=DataP;
  AdminP^.ID:=Fields.FieldByName(Admin.DB.Keys.ID).AsLargeInt;
  AdminP^.DomainID:=Fields.FieldByName(Admin.DB.Keys.DomainID).AsLargeInt;
  AdminP^.RootID:=Fields.FieldByName(Admin.DB.Keys.RootID).AsLargeInt;
  AdminP^.DefaultExpires:=Fields.FieldByName(Admin.DB.Keys.DefaultExpires).AsDateTime;
  AdminP^.Webmaster:=Fields.FieldByName(Admin.DB.Keys.Webmaster).AsString;
  AdminP^.Editor:=Fields.FieldByName(Admin.DB.Keys.Editor).AsString;
  AdminP^.DefaultDocs:=Fields.FieldByName(Admin.DB.Keys.DefaultDocs).AsString;
end;

class Function  Admin.DB.Retrieve(Task:Core.Database.Types.TTask; UserID: QWord; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; Entry.RootID:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.RootID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.RootID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Webmaster,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Editor,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultDocs,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultExpires,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Admin_Retrieve,@Entry) and (Entry.RootID=UserID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Admin.DB.Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.RootID,poAnd,oEqual,Entry.RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Webmaster,poNone,oNone,Entry.Webmaster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Editor,poNone,oNone,Entry.Editor,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultDocs,poNone,oNone,Entry.DefaultDocs,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultExpires,poNone,oNone,Entry.DefaultExpires,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Admin.DB.Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iReset,iInsertID:QWord;
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iReset:=0;  iInsertID:=Random(High(Integer));
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.RootID,poNone,oNone,Entry.RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Webmaster,poNone,oNone,Entry.Webmaster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Editor,poNone,oNone,Entry.Editor,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DefaultDocs,poNone,oNone,Entry.DefaultDocs,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DefaultExpires,poNone,oNone,Entry.DefaultExpires,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Procedure CB_Syndication_Feed_Retrieve(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  FeedP:Feed.PItem;
begin
  FeedP:=DataP;
  {$i Storage.Syndication.Feeds.Assign.inc}
end;

class Function  Feed.DB.Retrieve(Task:Core.Database.Types.TTask; FeedID:QWord; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; Entry.ID:=0; Entry.RootID:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,FeedID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.RootID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.PubDate,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastBuildDate,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Docs,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultExpires,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Webmaster,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Editor,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Feed_Retrieve,@Entry) and (Entry.ID=FeedID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Procedure CB_Syndication_Feed_Retrieve_Feeds(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP:Feed.PItems;
  FeedP:Feed.PItem;
  iIndex:LongInt;
begin
  ListP:=DataP;
  iIndex:=Length(ListP^);
  SetLength(ListP^,iIndex+1);
  FeedP:=@ListP^[iIndex];
  {$i Storage.Syndication.Feeds.Assign.inc}
end;

class Function  Feed.DB.Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; Var Entries:Items):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; Empty(Entries);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.RootID,poNone,oEqual,RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.RootID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.PubDate,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastBuildDate,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Docs,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultExpires,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Webmaster,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Editor,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Feed_Retrieve_Feeds,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Procedure CB_Syndication_Feed_Retrieve_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ArrayP:Core.Arrays.Types.PLargeWord;
  iIndex:LongInt;
begin
  ArrayP:=DataP;
  iIndex:=Length(ArrayP^);
  SetLength(ArrayP^,iIndex+1);
  ArrayP^[iIndex]:=Fields.FieldByName(Feed.DB.Keys.ID).AsLargeInt;
end;

class Function  Feed.DB.Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Core.Arrays.LargeWord.Empty(Entries);
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.RootID,poNone,oEqual,RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Feed_Retrieve_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Feed.DB.Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastBuildDate,poNone,oNone,Entry.LastBuildDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Entry.Link,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Docs,poNone,oNone,Entry.Docs,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DefaultExpires,poNone,oNone,Entry.DefaultExpires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Webmaster,poNone,oNone,Entry.Webmaster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Editor,poNone,oNone,Entry.Editor,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Feed.DB.Create(Task:Core.Database.Types.TTask; RootID:QWord; Var Entry:Item):Boolean;
var
  iReset,iInsertID:QWord;
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  iReset:=0;
  iInsertID:=Random(High(Integer));

  Entry.RootID:=RootID;
  Entry.ID:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.RootID,poNone,oNone,Entry.RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DefaultExpires,poNone,oNone,Entry.DefaultExpires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Webmaster,poNone,oNone,Entry.Webmaster,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Editor,poNone,oNone,Entry.Editor,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Link,poNone,oNone,Entry.Link,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.PubDate,poNone,oNone,Entry.PubDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.LastBuildDate,poNone,oNone,Entry.LastBuildDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Docs,poNone,oNone,Entry.Docs,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands) and (Entry.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Feed.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Create(Task:Core.Database.Types.TTask; RootID,FeedID:QWord; Var Entry:Item):Boolean;
var
  iReset,iInsertID:QWord;
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0; iReset:=0; iInsertID:=Random(High(Integer));
  Entry.RootID:=RootID; Entry.FeedID:=FeedID;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.RootID,poNone,oNone,Entry.RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.FeedID,poNone,oNone,Entry.FeedID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Expires,poNone,oNone,Entry.Expires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Link,poNone,oNone,Entry.Link,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.PubDate,poNone,oNone,Entry.PubDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.GUID,poNone,oNone,Entry.GUID,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Procedure CB_Syndication_Item_Retrieve(CommansP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemP:Items.PItem;
begin
  ItemP:=DataP;
  ItemP^.RootID:=Fields.FieldByName(Items.DB.Keys.RootID).AsLargeInt;
  ItemP^.DomainID:=Fields.FieldByName(Items.DB.Keys.DomainID).AsLargeInt;
  ItemP^.FeedID:=Fields.FieldByName(Items.DB.Keys.FeedID).AsLargeInt;
  ItemP^.PubDate:=Fields.FieldByName(Items.DB.Keys.PubDate).AsDateTime;
  ItemP^.Expires:=Fields.FieldByName(Items.DB.Keys.Expires).AsDateTime;
  ItemP^.Title:=Fields.FieldByName(Items.DB.Keys.Title).AsString;
  ItemP^.Link:=Fields.FieldByName(Items.DB.Keys.Link).AsString;
  ItemP^.Description:=Fields.FieldByName(Items.DB.Keys.Description).AsString;
  ItemP^.GUID:=Fields.FieldByName(Items.DB.Keys.GUID).AsString;
end;

class Function  Items.DB.Retrieve(Task:Core.Database.Types.TTask; ItemID:QWord; Var Entry:Item):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Entry.ID:=ItemID;
  Entry.RootID:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.RootID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.FeedID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Expires,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.GUID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Item_Retrieve,@Entry) and (Entry.RootID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Procedure CB_Syndication_Item_Retrieve_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ArrayP:Core.Arrays.Types.PLargeWord;
  iIndex:LongInt;
begin
  ArrayP:=DataP;
  iIndex:=Length(ArrayP^);
  SetLength(ArrayP^,iIndex+1);
  ArrayP^[iIndex]:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
end;

class Function  Items.DB.Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.RootID,poNone,oEqual,RootID,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Item_Retrieve_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Retrieve(Task:Core.Database.Types.TTask; RootID:QWord; dtCutoff:Double; Var Entries:Core.Arrays.Types.LargeWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.RootID,poNone,oEqual,RootID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Expires,poAnd,oGreaterThan,dtCutoff,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Syndication_Item_Retrieve_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Edit(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Expires,poNone,oNone,Entry.Expires,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Link,poNone,oNone,Entry.Link,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Description,poNone,oNone,Entry.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.PubDate,poNone,oNone,Entry.PubDate,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.GUID,poNone,oNone,Entry.GUID,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDBM;
end.


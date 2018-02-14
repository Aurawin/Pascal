unit Storage.RSS;

{
  uRSS.pas is the place where all Syndication information is stored

  Based on Three Tables :

    * One for Channels
    * One for Items
    * One for Cache

  Copyright Aurawin LLC 2003-2010
  Written by: Andrew Thomas Brunner

  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}


interface

uses

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Timer,
  Core.Strings,

  Classes,
  SysUtils;


Const
  RC_FN_ID                       : Core.Strings.VarString = 'ITMID';
  RC_FN_InsertID                 : Core.Strings.VarString = 'ITMIID';
  RC_FN_DOMAINID                 : Core.Strings.VarString = 'ITMDID';
  RC_FN_TITLE                    : Core.Strings.VarString = 'ITMTIT';
  RC_FN_Description              : Core.Strings.VarString = 'ITMDES';
  RC_FN_Link                     : Core.Strings.VarString = 'Link';
  RC_FN_Copyright                : Core.Strings.VarString = 'ITMCPY';
  RC_FN_Language                 : Core.Strings.VarString = 'ITMLAN';
  RC_FN_LastBuildDate            : Core.Strings.VarString = 'ITMLBD';
  RC_FN_Editor                   : Core.Strings.VarString = 'ITMED';
  RC_FN_Webmaster                : Core.Strings.VarString = 'ITMWM';
  RC_FN_Sub_ICO_URL              : Core.Strings.VarString = 'ITMSUL';
  RC_FN_IMG_URL                  : Core.Strings.VarString = 'ITMISUL';
  RC_FN_IMG_TITLE                : Core.Strings.VarString = 'ITMITIT';
  RC_FN_IMG_Link                 : Core.Strings.VarString = 'ITMILNK';
  RC_FN_TC                       : Core.Strings.VarString = 'TC';
  RC_FN_THC                      : Core.Strings.VarString = 'THC';
  RC_FN_TRC                      : Core.Strings.VarString = 'TRC';
  RC_FN_TDC                      : Core.Strings.VarString = 'TDC';
  RC_FN_TSC                      : Core.Strings.VarString = 'TSC';
  RC_FN_TLC                      : Core.Strings.VarString = 'TLC';
  RC_FN_TIC                      : Core.Strings.VarString = 'TIC';
  RC_FN_TFC                      : Core.Strings.VarString = 'TFC';

  RI_FN_ID                       : Core.Strings.VarString = 'ITMID';
  RI_FN_InsertID                 : Core.Strings.VarString = 'ITMIID';
  RI_FN_CFID                     : Core.Strings.VarString = 'ITMCID';
  RI_FN_DOMAINID                 : Core.Strings.VarString = 'ITMDID';
  RI_FN_TITLE                    : Core.Strings.VarString = 'ITMTIT';
  RI_FN_Description              : Core.Strings.VarString = 'ITMDES';
  RI_FN_Link                     : Core.Strings.VarString = 'ITMLNK';
  RI_FN_PubDate                  : Core.Strings.VarString = 'ITMPDT';
  RC_FN_Author                   : Core.Strings.VarString = 'ITMATH';

  RC_FN_DATA                     : Core.Strings.VarString = 'ITMDAT';

Type
  TRSSItem=record
    ID                           : Core.Database.Types.LargeWord;
    DomainID                     : Core.Database.Types.LargeWord;
    ChannelID                    : Core.Database.Types.LargeWord;
    Verified                     : Core.Database.Types.Bool;
    PubDate                      : Core.Database.Types.DateTime;
    Title                        : Core.Database.Types.VarString;
    Link                         : Core.Database.Types.VarString;
    Description                  : Core.Database.Types.VarString;
    Author                       : Core.Database.Types.VarString;
  end;
  PRSSItem=^TRSSItem;
  TRSSItems=Array of PRSSItem;
  TRSSImage=record
    URL                          : Core.Database.Types.VarString;
    Title                        : Core.Database.Types.VarString;
    Link                         : Core.Database.Types.VarString;
  end;
  TRSSChannel=record
    ID                           : Core.Database.Types.LargeWord;
    DomainID                     : Core.Database.Types.LargeWord;
    Verified                     : Core.Database.Types.Bool;
    LastBuildDate                : Core.Database.Types.DateTime;

    Title                        : Core.Database.Types.VarString;
    Description                  : Core.Database.Types.VarString;
    Link                         : Core.Database.Types.VarString;
    Copyright                    : Core.Database.Types.VarString;
    Language                     : Core.Database.Types.VarString;
    Editor                       : Core.Database.Types.VarString;
    Webmaster                    : Core.Database.Types.VarString;
    SubscribeIconUrl             : Core.Database.Types.VarString;
    TC                           : Core.Database.Types.VarString;
    THC                          : Core.Database.Types.VarString;
    TRC                          : Core.Database.Types.VarString;
    TSC                          : Core.Database.Types.VarString;
    TDC                          : Core.Database.Types.VarString;
    TLC                          : Core.Database.Types.VarString;
    TFC                          : Core.Database.Types.VarString;
    TIC                          : Core.Database.Types.VarString;
    Image                        : TRSSImage;
    Items                        : TRSSItems;
  end;
  PRSSChannels=^TRSSChannels;
  PRSSChannel=^TRSSChannel;
  TRSSChannels=Array of PRSSChannel;
  TRSSCache=record
    ID                           : Core.Database.Types.LargeWord;
    ChannelID                    : Core.Database.Types.LargeWord;
    DomainID                     : Core.Database.Types.LargeWord;
    Data                         : Core.Database.Types.VarString;
  end;
  Channels=class
  Type
    DB=class
    Type
      IDs=class
      const
        ID                       : LongInt = 0;
        InsertID                 : LongInt = 1;
        DomainID                 : LongInt = 2;
        LastBuildDate            : LongInt = 3;
        Title                    : LongInt = 4;
        Description              : LongInt = 5;
        Link                     : LongInt = 6;
        Copyright                : LongInt = 7;
        Language                 : LongInt = 8;
        Editor                   : LongInt = 9;
        Webmaster                : LongInt = 10;
        SubIcon                  : LongInt = 11;
        ImageURL                 : LongInt = 12;
        ImageTitle               : LongInt = 13;
        ImageLink                : LongInt = 14;
        TC                       : LongInt = 15;
        THC                      : LongInt = 16;
        TRC                      : LongInt = 17;
        TSC                      : LongInt = 18;
        TDC                      : LongInt = 19;
        TLC                      : LongInt = 20;
        TFC                      : LongInt = 21;
        TIC                      : LongInt = 22;
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Domains/Syndication/RSS';
        Name                 : 'Channels';
        Value                : 'scs_rss_c';
        Hint                 : 'Storage of domain RSS Channels';
        PrimaryKeyP          : @RC_FN_ID;
      );
      Fields: array [0..22] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP:@RC_FN_ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP:@RC_FN_InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP:@RC_FN_DOMAINID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.LastBuildDate; KeyP:@RC_FN_LastBuildDate; DataType: dftDateTime; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP:@RC_FN_TITLE; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP:@RC_FN_DESCRIPTION; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*32; Flags: cfNone; ),
        (IDP: @IDs.Link; KeyP:@RC_FN_Link; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Copyright; KeyP:@RC_FN_Copyright; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Language; KeyP:@RC_FN_Language; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Editor; KeyP:@RC_FN_Editor; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Webmaster; KeyP:@RC_FN_Webmaster; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.SubIcon; KeyP:@RC_FN_Sub_ICO_URL; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.ImageURL; KeyP:@RC_FN_IMG_URL; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.ImageTitle; KeyP:@RC_FN_IMG_TITLE; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.ImageLink; KeyP:@RC_FN_IMG_Link; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.TC; KeyP:@RC_FN_TC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.THC; KeyP:@RC_FN_THC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.TRC; KeyP:@RC_FN_TRC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.TSC; KeyP:@RC_FN_TSC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.TDC; KeyP:@RC_FN_TDC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.TLC; KeyP:@RC_FN_TLC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.TFC; KeyP:@RC_FN_TFC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; ),
        (IDP: @IDs.TIC; KeyP:@RC_FN_TIC; DataType: dftString; AutoCreate: True; Verified: False; Precision: 50; Flags: cfNone; )
      );
      class Function  Fill(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entries:TRSSChannels):Boolean; overload;
      class Function  Fill(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entry:TRSSChannel):Boolean; overload;
      class Function  Delete(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entry:TRSSChannel; Var Entries:TRSSChannels):Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Channel:TRSSChannel):Boolean;
      class Function  Edit(Task:Core.Database.Types.TTask; DomainID:QWord; Var Channel:TRSSChannel):Boolean;
    end;
    class procedure Init(Var Item:TRSSChannels); overload;
    class procedure Init(Var Item:TRSSChannel); overload;
    class procedure Empty(Var Item:TRSSChannel); overload;
    class procedure Empty(Var Item:TRSSChannels); overload;
    class procedure Copy(Var Source,Destination:TRSSChannel);
    class Function  IndexOf(Var Entries:TRSSChannels; iID:System.QWord): LongInt;
    class procedure Done(Var Item:TRSSChannels); overload;
    class procedure Done(Var Item:TRSSChannel); overload;
  end;
  Items=class
  type
    DB=Class
    type
      IDs=class
      const
        ID                       : LongInt = 0;
        InsertID                 : LongInt = 1;
        ChannelID                : LongInt = 2;
        DomainID                 : LongInt = 3;
        PubDate                  : LongInt = 4;
        Title                    : LongInt = 5;
        Link                     : LongInt = 6;
        Description              : LongInt = 7;
        Author                   : LongInt = 8;
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Domains/Syndication/RSS';
        Name                 : 'Items';
        Value                : 'scs_rss_i';
        Hint                 : 'Storage of domain RSS Channel Items';
        PrimaryKeyP          : @RI_FN_ID;
      );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP:@RC_FN_ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP:@RC_FN_InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ChannelID; KeyP:@RI_FN_CFID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.DomainID; KeyP:@RC_FN_DOMAINID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.PubDate; KeyP:@RI_FN_PubDate; DataType: dftDateTime; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP:@RC_FN_TITLE; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Link; KeyP:@RI_FN_Link; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone; ),
        (IDP: @IDs.Description; KeyP:@RC_FN_DESCRIPTION; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*32; Flags: cfNone; ),
        (IDP: @IDs.Author; KeyP:@RC_FN_Author; DataType: dftString; AutoCreate: True; Verified: False; Precision: 150; Flags: cfNone; )
      );
      class Function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Channel:TRSSChannel; Var Item:TRSSItem):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TRSSItem):Boolean;
      class Function  Edit(Task:Core.Database.Types.TTask; DomainID:QWord; Var Channel:TRSSChannel; Var Item:TRSSItem):Boolean;

    end;
    class procedure Init(var Item:TRSSImage); overload;
    class procedure Empty(Var Item:TRSSImage); overload;
    class procedure Done(var Item:TRSSImage); overload;
    class procedure Copy(Var Source,Destination:TRSSImage); overload;
    class procedure Copy(Var Source,Destination:TRSSItem); overload;
    class procedure Copy(Var Source,Destination:TRSSItems); overload;
    class procedure Init(Var Item:TRSSItem); overload;
    class procedure Init(Var Item:TRSSItems); overload;

    class function  IndexOf(Var List:TRSSItems; ID:QWord): LongInt;
    class Function  IndexOf(Var Entries:TRSSChannels; iChannelID,iItemID:System.QWord): LongInt; overload;
    class Function  IndexOf(Var Entry:TRSSChannel; iItemID:System.QWord): LongInt; overload;
    class procedure Empty(Var Item:TRSSItem); overload;
    class procedure Empty(Var Item:TRSSItems); overload;

    class procedure Done(var Item:TRSSItems); overload;
    class procedure Done(var Item:TRSSItem); overload;
  End;

  Cache=class
  Type
    DB=class
    Type
      IDs=class
      const
        ID                       : LongInt = 0;
        InsertID                 : LongInt = 1;
        ChannelID                : LongInt = 2;
        DomainID                 : LongInt = 3;
        Data                     : LongInt = 4;
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Domains/Syndication/RSS';
        Name                 : 'Cache';
        Value                : 'scs_rss_e';
        Hint                 : 'Storage of domain RSS Cache';
        PrimaryKeyP          : @RC_FN_ID;
      );
      Fields: array [0..4] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP:@RC_FN_ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity),
        (IDP: @IDs.InsertID; KeyP:@RC_FN_InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ChannelID; KeyP:@RI_FN_CFID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.DomainID; KeyP:@RC_FN_DOMAINID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull; ),
        (IDP: @IDs.Data; KeyP:@RC_FN_DATA; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*32; Flags: cfNone; )
      );
    end;
  end;


implementation
uses
  db, sqldb;


function cbDBMonitorNotifyied(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; DomainID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP=Channels.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Channels.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Channels.DB.TableP,useForCriteria,Channels.DB.IDS.DomainID,poNone,oEqual,DomainID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        SetLength(Commands,0);
      End;
    end else if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDS.DomainID,poNone,oEqual,DomainID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        SetLength(Commands,0);
      End;
    end else If ItemP=Cache.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Cache.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Cache.DB.TableP,useForCriteria,Items.DB.IDS.DomainID,poNone,oEqual,DomainID,Commands);
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
  end;
end;

procedure cbDestroyChannels(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Channels.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyItems(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyCache(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Cache.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDB;
var
  iCount                         : LongInt;
  iLcv                           : LongInt;
begin
  if Channels.DB.TableP = nil then begin
    New(Channels.DB.TableP);
    Init(Channels.DB.TableP^, Channels.DB.Startup);
    for iLcv := 0 to High(Channels.DB.Fields) do
      Core.Database.AddField(@Channels.DB.Fields[iLcv], Channels.DB.TableP);
  end;
  if Channels.DB.MonitorP = nil then begin
    New(Channels.DB.MonitorP);
    Init(Channels.DB.MonitorP^, Channels.DB.TableP^, @cbDestroyChannels, @cbDBMonitorNotifyied);
    Core.Database.Monitor.Add(Channels.DB.MonitorP);
  end;
  if Items.DB.TableP = nil then begin
    New(Items.DB.TableP);
    Init(Items.DB.TableP^, Items.DB.Startup);
    for iLcv := 0 to High(Items.DB.Fields) do
      Core.Database.AddField(@Items.DB.Fields[iLcv], Items.DB.TableP);
  end;
  if Items.DB.MonitorP = nil then begin
    New(Items.DB.MonitorP);
    Init(Items.DB.MonitorP^, Items.DB.TableP^, @cbDestroyItems, @cbDBMonitorNotifyied);
    Core.Database.Monitor.Add(Items.DB.MonitorP);
  end;
  if Cache.DB.TableP = nil then begin
    New(Cache.DB.TableP);
    Init(Cache.DB.TableP^, Cache.DB.Startup);
    for iLcv := 0 to High(Cache.DB.Fields) do
      Core.Database.AddField(@Cache.DB.Fields[iLcv], Cache.DB.TableP);
  end;
  if Cache.DB.MonitorP = nil then begin
    New(Cache.DB.MonitorP);
    Init(Cache.DB.MonitorP^, Cache.DB.TableP^, @cbDestroyCache, @cbDBMonitorNotifyied);
    Core.Database.Monitor.Add(Cache.DB.MonitorP);
  end;
end;

class procedure Items.Init(Var Item:TRSSItem);
begin
  Item.ID:=0;
  Item.DomainID:=0;
  Item.ChannelID:=0;
  Item.Verified:=false;
  Item.PubDate:=0;

  SetLength(Item.Title,0);
  SetLength(Item.Link,0);
  SetLength(Item.Description,0);
  SetLength(Item.Author,0);
end;

class procedure Items.Empty(Var Item:TRSSItem);
begin
  Item.ID:=0;
  Item.DomainID:=0;
  Item.ChannelID:=0;
  Item.Verified:=false;
  Item.PubDate:=0;

  SetLength(Item.Title,0);
  SetLength(Item.Link,0);
  SetLength(Item.Description,0);
  SetLength(Item.Author,0);
end;

class procedure Items.Init(Var Item:TRSSItems);
var
  iLcv : LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    if ( Item[iLcv]<>nil) then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      Item[iLcv]:=nil;
    end;
  end;
  SetLength(Item,0);
end;

class procedure Items.Empty(Var Item:TRSSItems);
var
  iLcv : LongInt;
begin
  For iLcv:=0 to High(Item) do begin
    if ( Item[iLcv]<>nil) then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      Item[iLcv]:=nil;
    end;
  end;
  SetLength(Item,0);
end;

class procedure Items.Done(var Item:TRSSItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    if (Item[iLcv]<>nil) then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      Item[iLcv]:=nil;
    end;
  end;
  Finalize(Item);
end;

class procedure Items.Done(var Item:TRSSItem);
begin
  with Item do begin
    Finalize(Title);
    Finalize(Link);
    Finalize(Description);
    Finalize(Author);
  end;
  Finalize(Item);
end;

class Function Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TRSSItem):System.Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDS.ID,poNone,oEqual,Item.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class Function Items.DB.Add(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Channel:TRSSChannel; Var Item:TRSSItem):System.Boolean;
var
  iCount                         : LongInt;
  iReset                         : System.QWord;
  InsertID                       : System.QWord;
  itmP                           : PRSSItem;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; InsertID:=Random(High(Int64));

    Item.DomainID:=DomainID;
    Item.ChannelID:=Channel.ID;
    Item.PubDate:=Core.Timer.dtUT;

    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.InsertID,poNone,oNone,InsertID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDS.InsertID,poNone,oEqual,InsertID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForPrimaryID,Items.DB.IDS.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForResetInsertID,Items.DB.IDS.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.ChannelID,poNone,oNone,Channel.ID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.PubDate,poNone,oNone,Item.PubDate,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.Title,poNone,oNone,Item.Title,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.Link,poNone,oNone,Item.Link,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.Description,poNone,oNone,Item.Description,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForInsert,Items.DB.IDS.Author,poNone,oNone,Item.Author,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    If Result then begin
      New(itmP);
      Init(itmP^);
      Copy(Item,itmP^);
      iCount:=Length(Channel.Items);
      SetLength(Channel.Items,iCount+1);
      Channel.Items[iCount]:=itmP;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Items.DB.Edit(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Channel:TRSSChannel; Var Item:TRSSItem):System.Boolean;
var
  iCount,iIndex                  : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDS.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDS.PubDate,poNone,oNone,Item.PubDate,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDS.Title,poNone,oNone,Item.Title,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDS.Link,poNone,oNone,Item.Link,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDS.Description,poNone,oNone,Item.Description,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForUpdates,Items.DB.IDS.Author,poNone,oNone,Item.Author,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_FillItem(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iLcv,iItemIndex,iItemID:LongInt;
  ChannelP:PRSSChannel;
  itmP:PRSSItem;
begin
  ChannelP:=DataP;
  iItemID:=Fields.FieldByName(RI_FN_ID).AsLargeInt;
  iItemIndex:=Items.IndexOf(ChannelP^,iItemID);
  If iItemIndex=-1 then begin
    iItemIndex:=Length(ChannelP^.Items);
    new(itmP);
    Items.Init(itmP^);

    SetLength(ChannelP^.Items,iItemIndex+1);
    ChannelP^.Items[iItemIndex]:=itmP;
    itmP^.DomainID:=ChannelP^.DomainID;
    itmP^.ID:=iItemID;
    itmP^.ChannelID:=ChannelP^.ID;
  end else
    itmP:=ChannelP^.Items[iItemIndex];

  itmP^.Verified:=true;
  itmP^.PubDate:=Fields.FieldByName(RI_FN_PubDate).AsDateTime;
  itmP^.Title:=Fields.FieldByName(RI_FN_TITLE).AsString;
  itmP^.Link:=Fields.FieldByName(RI_FN_LINK).AsString;
  itmP^.Description:=Fields.FieldByName(RI_FN_Description).AsString;
  itmP^.Author:=Fields.FieldByName(RC_FN_Author).AsString;
end;

class Function  Channels.DB.Fill(Task:Core.Database.Types.TTask; DomainID:QWord; Var Entry:TRSSChannel):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Channels.DB.IDS.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Channels.DB.IDS.DomainID,poAnd,oEqual,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.Link,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.Description,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.Author,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForFields,Items.DB.IDS.PubDate,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillItem,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Procedure CB_FillChannels(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iChannelID:System.QWord;
  iLcv:LongInt;
  iDX:LongInt;
  ChannelsP:PRSSChannels;
  ChannelP:PRSSChannel;
begin
  ChannelsP:=DataP;
  iChannelID:=Fields.FieldByName(RC_FN_ID).AsLargeInt;
  iDX:=Channels.IndexOf(ChannelsP^,iChannelID);
  If iDX=-1 then begin
    New(ChannelP);
    Channels.Init(ChannelP^);
    iDX:=Length(ChannelsP^);
    SetLength(ChannelsP^,iDX+1);
    ChannelsP^[iDX]:=ChannelP;
  end else
    ChannelP:=ChannelsP^[iDX];
  ChannelP^.ID:=iChannelID;
  ChannelP^.DomainID:=Fields.FieldByName(RC_FN_ID).AsLargeInt;
  ChannelP^.LastBuildDate:=Fields.FieldByName(RC_FN_LastBuildDate).AsDateTime;
  ChannelP^.Title:=Fields.FieldByName(RC_FN_TITLE).AsString;
  ChannelP^.Description:=Fields.FieldByName(RC_FN_DESCRIPTION).AsString;
  ChannelP^.Link:=Fields.FieldByName(RC_FN_Link).AsString;
  ChannelP^.Copyright:=Fields.FieldByName(RC_FN_Copyright).AsString;
  ChannelP^.Language:=Fields.FieldByName(RC_FN_Language).AsString;
  ChannelP^.Editor:=Fields.FieldByName(RC_FN_Editor).AsString;
  ChannelP^.Webmaster:=Fields.FieldByName(RC_FN_Webmaster).AsString;
  ChannelP^.SubscribeIconUrl:=Fields.FieldByName(RC_FN_Sub_ICO_URL).AsString;
  ChannelP^.Image.URL:=Fields.FieldByName(RC_FN_IMG_URL).AsString;
  ChannelP^.Image.Title:=Fields.FieldByName(RC_FN_IMG_TITLE).AsString;
  ChannelP^.Image.Link:=Fields.FieldByName(RC_FN_IMG_Link).AsString;

  ChannelP^.TC:=Fields.FieldByName(RC_FN_TC).AsString;
  ChannelP^.THC:=Fields.FieldByName(RC_FN_THC).AsString;
  ChannelP^.TRC:=Fields.FieldByName(RC_FN_TRC).AsString;
  ChannelP^.TSC:=Fields.FieldByName(RC_FN_TSC).AsString;
  ChannelP^.TDC:=Fields.FieldByName(RC_FN_TDC).AsString;
  ChannelP^.TLC:=Fields.FieldByName(RC_FN_TLC).AsString;
  ChannelP^.TFC:=Fields.FieldByName(RC_FN_TFC).AsString;
  ChannelP^.TIC:=Fields.FieldByName(RC_FN_TIC).AsString;
end;

class Function Channels.DB.Fill(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Entries:TRSSChannels):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Channels.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForCriteria,IDS.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Description,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Link,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Copyright,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Language,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Editor,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.Webmaster,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.SubIcon,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.ImageURL,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.ImageTitle,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.ImageLink,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.THC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TRC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TSC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TDC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TLC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TFC,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForFields,IDS.TIC,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillChannels,@Entries);
    If Result then begin
      For iLcv:=0 to High(Entries) do
        if Entries[iLcv]<>nil then
          Fill(Task,DomainID,Entries[iLcv]^);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Items.Copy(Var Source,Destination:TRSSItem);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.Verified:=Source.Verified;
  Destination.PubDate:=Source.PubDate;
  Destination.Title:=Source.Title;
  Destination.Link:=Source.Link;
  Destination.Description:=Source.Description;
  Destination.Author:=Source.Author;
end;

class procedure Items.Copy(Var Source,Destination:TRSSItems);
var
  iLcv:LongInt;
  iSourceLen:LongInt;
  itmP:PRSSItem;
begin
  Empty(Destination);
  iSourceLen:=Length(Source);
  SetLength(Destination,iSourceLen);
  for iLcv:=0 to iSourceLen-1 do begin
    New(itmP);
    Init(itmP^);
    Copy(Source[iLcv]^,itmP^);
    Destination[iLcv]:=itmP;
  end;
end;

class Function  Items.IndexOf(Var List:TRSSItems; ID:System.QWord): LongInt;
var
  iLcv:LongInt;
begin
  iLcv:=0; Result:=-1;
  While (iLcv<Length(List)) and (Result=-1) do begin
    If ((List[iLcv]<>nil) and (List[iLcv]^.ID=ID)) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

class procedure Channels.Copy(Var Source,Destination:TRSSChannel);
begin
  Destination.ID:=Source.ID;
  Destination.DomainID:=Source.DomainID;
  Destination.Verified:=Source.Verified;
  Destination.LastBuildDate:=Source.LastBuildDate;
  Items.Copy(Source.Image,Destination.Image);
  Items.Copy(Source.Items,Destination.Items);
  Destination.Title:=Source.Title;
  Destination.Description:=Source.Description;
  Destination.Link:=Source.Link;
  Destination.Copyright:=Source.Copyright;
  Destination.Language:=Source.Language;
  Destination.Editor:=Source.Editor;
  Destination.Webmaster:=Source.Webmaster;
  Destination.SubscribeIconUrl:=Source.SubscribeIconUrl;
  Destination.TC:=Source.TC;
  Destination.THC:=Source.THC;
  Destination.TRC:=Source.TRC;
  Destination.TSC:=Source.TSC;
  Destination.TDC:=Source.TDC;
  Destination.TLC:=Source.TLC;
  Destination.TFC:=Source.TFC;
  Destination.TIC:=Source.TIC;
end;

class procedure Items.Copy(Var Source,Destination:TRSSImage);
begin
  Destination.URL:=Source.URL;
  Destination.Title:=Source.Title;
  Destination.Link:=Source.Link;
end;

class Function  Channels.IndexOf(Var Entries:TRSSChannels; iID:System.QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Entries) do begin
    If ((Entries[iLcv]<>nil) and (Entries[iLcv]^.ID=iID)) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class procedure Items.Empty(Var Item:TRSSImage);
begin
  SetLength(Item.URL,0);
  SetLength(Item.Title,0);
  SetLength(Item.Link,0);
end;

class procedure Channels.Empty(Var Item:TRSSChannel);
begin
  With Item do begin
    ID:=0;
    DomainID:=0;
    Verified:=false;
    LastBuildDate:=0;

    System.SetLength(Title,0);
    System.SetLength(Description,0);
    System.SetLength(Link,0);
    System.SetLength(Copyright,0);
    System.SetLength(Language,0);
    System.SetLength(Editor,0);
    System.SetLength(Webmaster,0);
    System.SetLength(SubscribeIconUrl,0);
    System.SetLength(TC,0);
    System.SetLength(THC,0);
    System.SetLength(TRC,0);
    System.SetLength(TSC,0);
    System.SetLength(TDC,0);
    System.SetLength(TLC,0);
    System.SetLength(TFC,0);
    System.SetLength(TIC,0);
  end;
  Items.Empty(Item.Image);
  Items.Empty(Item.Items);
end;

class procedure Channels.Empty(Var Item:TRSSChannels);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do begin
    if Item[iLcv]<>nil then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      Item[iLcv]:=nil;
    end;
  end;
  SetLength(Item,0);
end;

class procedure Items.Init(Var Item:TRSSImage);
begin
  SetLength(Item.URL,0);
  SetLength(Item.Title,0);
  SetLength(Item.Link,0);
end;

class procedure Channels.Init(Var Item:TRSSChannel);
begin
  With Item do begin
    ID:=0;
    DomainID:=0;
    Verified:=false;
    LastBuildDate:=0;

    System.SetLength(Title,0);
    System.SetLength(Description,0);
    System.SetLength(Link,0);
    System.SetLength(Copyright,0);
    System.SetLength(Language,0);
    System.SetLength(Editor,0);
    System.SetLength(Webmaster,0);
    System.SetLength(SubscribeIconUrl,0);
    System.SetLength(TC,0);
    System.SetLength(THC,0);
    System.SetLength(TRC,0);
    System.SetLength(TSC,0);
    System.SetLength(TDC,0);
    System.SetLength(TLC,0);
    System.SetLength(TFC,0);
    System.SetLength(TIC,0);

  end;
  Items.Init(Item.Image);
  Items.Init(Item.Items);
end;

class procedure Channels.Init(Var Item:TRSSChannels);
begin
  Empty(Item);
end;


class procedure Channels.Done(Var Item:TRSSChannels);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    if Item[iLcv]<>nil then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      Item[iLcv]:=nil;
    end;
  end;
  Finalize(Item);
end;

class procedure Channels.Done(Var Item:TRSSChannel);
begin
  With Item do begin
    Finalize(Title);
    Finalize(Description);
    Finalize(Link);
    Finalize(Copyright);
    Finalize(Language);
    Finalize(Editor);
    Finalize(Webmaster);
    Finalize(SubscribeIconUrl);
    Finalize(TC);
    Finalize(THC);
    Finalize(TRC);
    Finalize(TSC);
    Finalize(TDC);
    Finalize(TLC);
    Finalize(TFC);
    Finalize(TIC);
  end;
  Items.Done(Item.Image);
  Items.Done(Item.Items);
  Finalize(Item);
end;

class procedure Items.Done(Var Item:TRSSImage);
begin
  With Item do begin
    Finalize(URL);
    Finalize(Title);
    Finalize(Link);
  end;
  Finalize(Item);
end;


class Function Channels.DB.Edit(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Channel:TRSSChannel):System.Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Channels.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForCriteria,IDs.ID,poNone,oEqual,Channel.ID,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Title,poNone,oNone,Channel.Title,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Description,poNone,oNone,Channel.Description,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Link,poNone,oNone,Channel.Link,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Copyright,poNone,oNone,Channel.Copyright,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Language,poNone,oNone,Channel.Language,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Editor,poNone,oNone,Channel.Editor,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.Webmaster,poNone,oNone,Channel.Webmaster,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.SubIcon,poNone,oNone,Channel.SubscribeIconURL,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.ImageURL,poNone,oNone,Channel.Image.URL,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.ImageTitle,poNone,oNone,Channel.Image.Title,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.ImageLink,poNone,oNone,Channel.Image.Link,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TC,poNone,oNone,Channel.TC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.THC,poNone,oNone,Channel.THC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TRC,poNone,oNone,Channel.TRC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TSC,poNone,oNone,Channel.TSC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TDC,poNone,oNone,Channel.TDC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TLC,poNone,oNone,Channel.TLC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TFC,poNone,oNone,Channel.TFC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForUpdates,IDs.TIC,poNone,oNone,Channel.TIC,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Channels.DB.Add(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Channel:TRSSChannel):System.Boolean;
var
  iCount                         : LongInt;
  iReset                         : System.QWord;
  iInsertID                      : System.QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Int64));
    Core.Database.AddCommand(iCount,Channels.DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForPrimaryID,IDs.ID,poNone,oNone,Channel.ID,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,Channels.DB.IDS.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.LastBuildDate,poNone,oNone,Channel.LastBuildDate,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Title,poNone,oNone,Channel.Title,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Description,poNone,oNone,Channel.Description,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Link,poNone,oNone,Channel.Link,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Copyright,poNone,oNone,Channel.Copyright,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Language,poNone,oNone,Channel.Language,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Editor,poNone,oNone,Channel.Editor,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.Webmaster,poNone,oNone,Channel.Webmaster,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.SubIcon,poNone,oNone,Channel.SubscribeIconUrl,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.ImageURL,poNone,oNone,Channel.Image.Url,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.ImageTitle,poNone,oNone,Channel.Image.Title,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.ImageLink,poNone,oNone,Channel.Image.Link,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TC,poNone,oNone,Channel.TC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.THC,poNone,oNone,Channel.THC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TRC,poNone,oNone,Channel.TRC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TSC,poNone,oNone,Channel.TSC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TDC,poNone,oNone,Channel.TDC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TLC,poNone,oNone,Channel.TLC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TFC,poNone,oNone,Channel.TFC,Commands);
    Core.Database.AddCommand(iCount,Channels.DB.TableP,useForInsert,IDs.TIC,poNone,oNone,Channel.TIC,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
    If Result then begin
      Channel.Link:=Concat('/core/rss.co?feed&',IntToStr(Channel.ID));
      Channel.Image.Link:=Concat('/core/rss.co?view&',IntToStr(Channel.ID));
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function Channels.DB.Delete(Task:Core.Database.Types.TTask; DomainID:System.QWord; Var Entry:TRSSChannel; Var Entries:TRSSChannels):System.Boolean;
var
  iIndex,iCount,iLcv             : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDS.ChannelID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    If Result then begin
      iCount:=0;
      Core.Database.Empty(Commands);

      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDS.ID,poNone,oEqual,Entry.ID,Commands);
      Result:=Core.Database.SQL.Delete(Task,@Commands);
      If Result then begin
        iIndex:=IndexOf(Entries,Entry.ID);
        if iIndex<>-1 then begin
          Done(Entries[iIndex]^);
          Dispose(Entries[iIndex]);
          Entries[iIndex]:=nil;
          iCount:=Length(Entries);
          for iLcv:=iIndex to iCount-2 do
            Entries[iLcv]:=Entries[iLcv+1];
          Dec(iCount);
          SetLength(Entries,iCount);
        end;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.IndexOf(Var Entries:TRSSChannels; iChannelID,iItemID:System.QWord): LongInt;
var
  jLcv,iLcv:LongInt;
  chP:PRSSChannel;
begin
  Result:=-1;
  for iLcv:=0 to High(Entries) do begin
    chP:=Entries[iLcv];
    if ((chP<>nil) and (chP^.ID=iChannelID) )then begin
      for jLcv:=0 to High(chP^.Items) do begin
        If (chP^.Items[jLcv]<>nil) and (chP^.Items[jLcv]^.ID=iItemID) then begin
          Result:=jLcv;
          exit;
        end;
      end;
    end;
  end;
end;

class Function  Items.IndexOf(Var Entry:TRSSChannel; iItemID:System.QWord): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Entry.Items) do begin
    If ((Entry.Items[iLcv]<>nil) and (Entry.Items[iLcv]^.ID=iItemID)) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

initialization
  RegisterDB;
end.


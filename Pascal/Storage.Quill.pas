unit Storage.Quill;

{
unit Storage.Quill.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html


This Database Module is for Blogging

}

interface

uses
  Classes,

  Core.Database,
  Core.Database.SQL,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,

  Core.Strings,

  SysUtils;


Type
  Blogs=class
  Type
    Item=record
      ID                         : QWord;
      OwnerID                    : QWord;
      DomainID                   : QWord;
      Title                      : Core.Strings.VarString;
      Items                      : Core.Arrays.Types.LargeWord;
    end;
    PItem=^Item;

    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        OwnerID                  : Core.Database.Types.Integer = 2;
        DomainID                 : Core.Database.Types.Integer = 3;
        Title                    : Core.Database.Types.Integer = 4;
        Items                    : Core.Database.Types.Integer = 5;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'BID';
        InsertID                 : Core.Database.Types.VarString = 'BIID';
        OwnerID                  : Core.Database.Types.VarString = 'BOID';
        DomainID                 : Core.Database.Types.VarString = 'BDID';
        Title                    : Core.Database.Types.VarString = 'BTLE';
        Items                    : Core.Database.Types.VarString = 'BITMS';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications';
        Name                     : 'Quill';
        Value                    : 'scs_qblgs';
        Hint                     : 'Storage of blogs for domain users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..5] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType:dftString; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP: @IDs.Items; KeyP: @Keys.Items; DataType:dftQWordArray; AutoCreate:True; Verified:False; Precision:1024*1024*4; Flags: cfNone; )
      );
      class function  Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  Write(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
    end;
    class procedure Done(Var Entry:Item);
    class procedure Empty(Var Entry:Item);
    class procedure Copy(Var Source,Destination:Item);
  end;
  Items=class
  Type
    Item=record
      ID                         : QWord;
      OwnerID                    : QWord;
      DomainID                   : QWord;
      BlogID                     : QWord;
      Posted                     : Double;
      Modified                   : Double;
      Title                      : Core.Strings.VarString;
      Body                       : Core.Strings.VarString;
    end;
    PItem=^Item;
    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        OwnerID                  : Core.Database.Types.Integer = 2;
        DomainID                 : Core.Database.Types.Integer = 3;
        BlogID                   : Core.Database.Types.Integer = 4;
        Posted                   : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        Title                    : Core.Database.Types.Integer = 7;
        Body                     : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'QIID';
        InsertID                 : Core.Database.Types.VarString = 'QIIID';
        OwnerID                  : Core.Database.Types.VarString = 'QIOID';
        DomainID                 : Core.Database.Types.VarString = 'QIDID';
        BlogID                   : Core.Database.Types.VarString = 'QIBID';
        Posted                   : Core.Database.Types.VarString = 'QIDTS';
        Modified                 : Core.Database.Types.VarString = 'QIMOD';
        Title                    : Core.Database.Types.VarString = 'QITLE';
        Body                     : Core.Database.Types.VarString = 'QIBDY';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Quill';
        Name                     : 'Items';
        Value                    : 'scs_qitms';
        Hint                     : 'Storage of entries for blogs';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..8] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.OwnerID; KeyP: @Keys.OwnerID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.BlogID; KeyP: @Keys.BlogID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Posted; KeyP: @Keys.Posted; DataType:dftDouble; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType:dftDouble; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType:dftString; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; ),
        (IDP: @IDs.Body; KeyP: @Keys.Body; DataType:dftMemo; AutoCreate:True; Verified:False; Precision:1024*4; Flags: cfNone; )
      );
      class function  Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  Write(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
    end;
    class procedure Done(var Entry:Item);
    class procedure Empty(Var Entry:Item);
    class procedure Copy(Var Source,Destination:Item);
  end;
  InterActions=class
  Type
    Item=record
      ID                         : QWord;
      AuthID                     : QWord;
      DomainID                   : QWord;
      BlogID                     : QWord;
      ItemID                     : QWord;
      Actions                    : Byte;
    end;
    Items=Array of Item;
    Request=record
      ManifestP                  : Core.Arrays.Types.PBytes;
      IDsP                       : Core.Arrays.Types.PLargeWord;
    end;
    PRequest=^Request;

    DB=class
    Type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        AuthID                   : Core.Database.Types.Integer = 2;
        DomainID                 : Core.Database.Types.Integer = 3;
        BlogID                   : Core.Database.Types.Integer = 4;
        ItemID                   : Core.Database.Types.Integer = 5;
        Actions                  : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'QIAID';
        InsertID                 : Core.Database.Types.VarString = 'QIAIID';
        AuthID                   : Core.Database.Types.VarString = 'QIAAID';
        DomainID                 : Core.Database.Types.VarString = 'QIADID';
        BlogID                   : Core.Database.Types.VarString = 'QIABID';
        ItemID                   : Core.Database.Types.VarString = 'QIAITD';
        Actions                  : Core.Database.Types.VarString = 'QIANS';
      end;
    Const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni=(
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Applications/Quill';
        Name                     : 'Actions';
        Value                    : 'scs_qias';
        Hint                     : 'Storage of quill actions for users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : Array [0..6] of Core.Database.Types.Field=(
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.AuthID; KeyP: @Keys.AuthID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.BlogID; KeyP: @Keys.BlogID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.ItemID; KeyP: @Keys.ItemID; DataType:dftQWord; AutoCreate:True; Verified:False; Precision:0; Flags: cfNone; ),
        (IDP: @IDs.Actions; KeyP: @Keys.Actions; DataType:dftByte; AutoCreate:True; Verified:False; Precision:255; Flags: cfNone; )
      );
      class function  Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
      class function  List(Task:Core.Database.Types.TTask; AuthID,BlogID:QWord; Var Entries:Core.Arrays.Types.LargeWord; Var Actions:Core.Arrays.Types.Bytes):Boolean;
    end;
    class procedure Done(Var Entry:Item);
    class procedure Empty(Var Entry:Item);
    class procedure Copy(Var Source,Destination:Item);
  Const
    qsNone                       : Byte = 0;
    qsRead                       : Byte = 1 shl 0;
    qsUnRead                     : Byte = 1 shl 1;
    qsDelete                     : Byte = 1 shl 2;
  end;

implementation
uses db;


function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Blogs.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Blogs.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Blogs.DB.TableP,useForCriteria,Blogs.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
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
    end else if ItemP=Interactions.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,useForCriteria,Interactions.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
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
    if ItemP=Blogs.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Blogs.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Blogs.DB.TableP,useForCriteria,Blogs.DB.IDs.OwnerID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Items.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Items.DB.TableP,useForCriteria,Items.DB.IDs.OwnerID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Interactions.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Interactions.DB.TableP,useForCriteria,Interactions.DB.IDs.AuthID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;

begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted();
  end;
end;

procedure cbDestroyBlogs(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Blogs.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyItems(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyActions(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Interactions.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
begin
  with Blogs.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyBlogs,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Items.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyItems,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with InterActions.DB do begin
    if TableP=nil then begin
      New(TableP);
      Core.Database.Init(TableP^,Startup);
      for iLcv:=0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv],TableP);
    end;
    If MonitorP=nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^,TableP^,@cbDestroyActions,@cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class procedure Blogs.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    OwnerID:=0;
    DomainID:=0;
    SetLength(Title,0);
    Core.Arrays.LargeWord.Empty(Items);
  end;
end;

class procedure Items.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    OwnerID:=0;
    DomainID:=0;
    BlogID:=0;
    Posted:=0;
    Modified:=0;
    SetLength(Title,0);
    SetLength(Body,0);
  end;
end;

class procedure InterActions.Empty(Var Entry:Item);
begin
  With Entry do begin
    ID:=0;
    AuthID:=0;
    DomainID:=0;
    BlogID:=0;
    ItemID:=0;
    Actions:=qsNone;
  end;
end;

class procedure Blogs.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.OwnerID:=Source.OwnerID;
  Destination.DomainID:=Source.DomainID;
  Destination.Title:=Source.Title;
  Core.Arrays.LargeWord.Copy(Source.Items,Destination.Items);
end;

class procedure Items.Copy(Var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.OwnerID:=Source.OwnerID;
  Destination.DomainID:=Source.DomainID;
  Destination.BlogID:=Source.BlogID;
  Destination.Posted:=Source.Posted;
  Destination.Modified:=Source.Modified;
  Destination.Title:=Source.Title;
  Destination.Body:=Source.Body;
end;

class procedure InterActions.Copy(var Source,Destination:Item);
begin
  Destination.ID:=Source.ID;
  Destination.AuthID:=Source.AuthID;
  Destination.DomainID:=Source.DomainID;
  Destination.BlogID:=Source.BlogID;
  Destination.ItemID:=Source.ItemID;
  Destination.Actions:=Source.Actions;
end;

class procedure Blogs.Done(Var Entry:Item);
begin
  With Entry do begin
    Finalize(Title);
    Core.Arrays.LargeWord.Done(Items);
  end;
  Finalize(Entry);
end;

class procedure Items.Done(var Entry:Item);
begin
  With Entry do begin
    Finalize(Title);
    Finalize(Body);
  end;
  Finalize(Entry);
end;

class procedure Interactions.Done(var Entry:Item);
begin
  Finalize(Entry);
end;

class function  Blogs.DB.Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Data Properties
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.OwnerID,poNone,oNone,Entry.OwnerID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Items,poNone,oNone,Entry.Items,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Quill_Read_Blog(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:Pointer=Nil);
var
  ItemP                          : Blogs.PItem;
begin
  ItemP:=DataP;
  ItemP^.ID:=Fields.FieldByName(Blogs.DB.Keys.ID).AsLargeInt;
  ItemP^.OwnerID:=Fields.FieldByName(Blogs.DB.Keys.OwnerID).AsLargeInt;
  ItemP^.DomainID:=Fields.FieldByName(Blogs.DB.Keys.DomainID).AsLargeInt;
  ItemP^.Title:=Fields.FieldByName(Blogs.DB.Keys.Title).AsString;
  Core.Arrays.LargeWord.fromString(Fields.FieldByName(Blogs.DB.Keys.Items).AsString,ItemP^.Items,',');
end;

class function  Blogs.DB.Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.OwnerID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Items,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Quill_Read_Blog,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Blogs.DB.Write(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.OwnerID,poNone,oNone,Entry.OwnerID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Items,poNone,oNone,Entry.Items,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Blogs.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    iCount:=0;
    Core.Database.AddCommand(iCount,Items.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Items.DB.TableP,UseForCriteria,Items.DB.IDs.BlogID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    iCount:=0;
    Core.Database.AddCommand(iCount,Interactions.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Interactions.DB.TableP,UseForCriteria,Interactions.DB.IDs.BlogID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.DB.Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Int64));
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Data Properties
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.OwnerID,poNone,oNone,Entry.OwnerID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Posted,poNone,oNone,Entry.Posted,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Body,poNone,oNone,Entry.Body,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Quill_Read_Item(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:Pointer=Nil);
var
  ItemP                          : Items.PItem;
begin
  ItemP:=DataP;
  ItemP^.ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  ItemP^.OwnerID:=Fields.FieldByName(Items.DB.Keys.OwnerID).AsLargeInt;
  ItemP^.DomainID:=Fields.FieldByName(Items.DB.Keys.DomainID).AsLargeInt;
  ItemP^.BlogID:=Fields.FieldByName(Items.DB.Keys.BlogID).AsLargeInt;
  ItemP^.Posted:=Fields.FieldByName(Items.DB.Keys.Posted).AsFloat;
  ItemP^.Modified:=Fields.FieldByName(Items.DB.Keys.Modified).AsFloat;
  ItemP^.Title:=Fields.FieldByName(Items.DB.Keys.Title).AsString;
  ItemP^.Body:=Fields.FieldByName(Items.DB.Keys.Body).AsString;
end;

class function  Items.DB.Read(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.OwnerID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.DomainID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.BlogID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Posted,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Title,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Body,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Quill_Read_Item,@Entry);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class function  Items.DB.Write(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.OwnerID,poNone,oNone,Entry.OwnerID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.BlogID,poNone,oNone,Entry.BlogID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Posted,poNone,oNone,Entry.Posted,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Entry.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Title,poNone,oNone,Entry.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Body,poNone,oNone,Entry.Body,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.DB.Delete(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poNone,oEqual,Entry.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    iCount:=0;
    Core.Database.AddCommand(iCount,InterActions.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,InterActions.DB.TableP,UseForCriteria,InterActions.DB.IDs.ItemID,poNone,oEqual,Entry.ID,Commands);

    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Interactions.DB.Create(Task:Core.Database.Types.TTask; Var Entry:Item):Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Int64));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Entry.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Data Properties
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.AuthID,poNone,oNone,Entry.AuthID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,Entry.DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.BlogID,poNone,oNone,Entry.BlogID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.ItemID,poNone,oNone,Entry.ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Actions,poNone,oNone,Entry.Actions,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Quill_Action_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:Pointer=Nil);
var
  ItemP                          : InterActions.PRequest;
  iIndex                         : LongInt;
begin
  ItemP:=DataP;
  iIndex:=Core.Arrays.LargeWord.IndexOf(Fields.FieldByName(InterActions.DB.Keys.ItemID).AsLargeInt,ItemP^.IDsP^);
  if iIndex<>-1 then
    Core.Arrays.Bytes.fromString(Fields.FieldByName(InterActions.DB.Keys.Actions).AsString,ItemP^.ManifestP^[iIndex]);
end;

class function  InterActions.DB.List(Task:Core.Database.Types.TTask; AuthID,BlogID:QWord; Var Entries:Core.Arrays.Types.LargeWord; Var Actions:Core.Arrays.Types.Bytes):Boolean;
var
  iCount                         : LongInt;
  iIDCount                       : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Request                        : InterActions.Request;
begin
  Result:=False;
  iCount:=0;
  Try
    iIDCount:=Length(Entries);
    if iIDCount>0 then begin
      Request.IDsP:=@Entries;
      Request.ManifestP:=@Actions;
      SetLength(Actions,iIDCount);

      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ItemID,poNone,oEqual,Entries[0],Commands);
      for iLcv:=1 to iIDCount-1 do
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ItemID,poOr,oEqual,Entries[iLcv],Commands);

      Core.Database.AddCommand(iCount,TableP,useForCriteria,PROPERTY_ID_VOID,poAnd,oOpenBracket,Commands);

        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.AuthID,poNone,oEqual,AuthID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForCriteria,PROPERTY_ID_VOID,poNone,oCloseBracket,Commands);

      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ItemID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Actions,poNone,oNone,Commands);

      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Quill_Action_List,@Request);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;
initialization
  RegisterDBM;
end.


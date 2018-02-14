unit Storage.Commerce;

{
  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,
  Core.Database.SQL,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,
  Core.Strings,
  Core.Streams,
  Core.XML,
  Core.Timer,

  Storage.Main,

  XMLRead,
  DOM,
  SysUtils;


type
  Payments=class
  type
    Recurrance=class
    const
      Off          : byte = 0;
      Day1         : byte = 1;
      Day2         : byte = 2;
      Day3         : byte = 3;
      Day4         : byte = 4;
      Day5         : byte = 5;
      Day6         : byte = 6;
      Day7         : byte = 7;
      Day8         : byte = 8;
      Day9         : byte = 9;
      Day10        : byte = 10;
      Day11        : byte = 11;
      Day12        : byte = 12;
      Day13        : byte = 13;
      Day14        : byte = 14;
      Day15        : byte = 15;
      Day16        : byte = 16;
      Day17        : byte = 17;
      Day18        : byte = 18;
      Day19        : byte = 19;
      Day20        : byte = 20;
      Day21        : byte = 21;
      Day22        : byte = 22;
      Day23        : byte = 23;
      Day24        : byte = 24;
      Day25        : byte = 25;
      Day26        : byte = 26;
      Day27        : byte = 27;
      Day28        : byte = 28;
    end;
  end;
  Address=class
  type
    XML=class
    type
      Stanzas=class
      const
        Item      : Core.Strings.VarString = 'address';
        Items     : Core.Strings.VarString = 'addresses';
      end;
      Fields=class
      const
        Address1    : Core.Strings.VarString = 'ad1';
        Address2    : Core.Strings.VarString = 'ad2';
        City        : Core.Strings.VarString = 'cty';
        State       : Core.Strings.VarString = 'ste';
        Post        : Core.Strings.VarString = 'pst';
      end;
    end;
    TItem=record
      Address1    : Core.Strings.VarString;
      Address2    : Core.Strings.VarString;
      City        : Core.Strings.VarString;
      State       : Core.Strings.VarString;
      Post        : Core.Strings.VarString;
    end;
    class procedure Empty(var Item:TItem);
    class procedure Init(var Item:TItem);
    class procedure Done(var Item:TItem);

    class function  fromXML(xItem:TDOMNode; var Item:TItem):boolean;
    class function  toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
  end;
  Card=class
  type
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        Number                   : Core.Database.Types.Integer = 4;
        ExpMonth                 : Core.Database.Types.Integer = 5;
        ExpYear                  : Core.Database.Types.Integer = 6;
        Code                     : Core.Database.Types.Integer = 7;
        Active                   : Core.Database.Types.Integer = 8;
        Alias                    : Core.Database.Types.Integer = 9;
        Holder                   : Core.Database.Types.Integer = 10;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        Number                   : Core.Database.Types.VarString = 'INOR';
        ExpMonth                 : Core.Database.Types.VarString = 'IXMH';
        ExpYear                  : Core.Database.Types.VarString = 'IXYR';
        Code                     : Core.Database.Types.VarString = 'ICOE';
        Active                   : Core.Database.Types.VarString = 'IACT';
        Alias                    : Core.Database.Types.VarString = 'IALS';
        Holder                   : Core.Database.Types.VarString = 'IHDR';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Commerce';
        Name                     : 'Card';
        Value                    : 'scs_com_crd';
        Hint                     : 'Payment card storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..10] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Number; KeyP: @Keys.Number; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ExpMonth; KeyP: @Keys.ExpMonth; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ExpYear; KeyP: @Keys.ExpYear; DataType: dftWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Code; KeyP: @Keys.Code; DataType: dftWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Active; KeyP: @Keys.Active; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Alias; KeyP: @Keys.Alias; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Holder; KeyP: @Keys.Holder; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
    end;
    XML=class
    type
      Stanzas=class
      const
        Item      : Core.Strings.VarString = 'card';
        Items     : Core.Strings.VarString = 'cards';
      end;
      Fields=class
      const
        ID        : Core.Strings.VarString = 'id';
        Number    : Core.Strings.VarString = 'num';
        ExpMonth  : Core.Strings.VarString = 'em';
        ExpYear   : Core.Strings.VarString = 'ey';
        Code      : Core.Strings.VarString = 'cd';
        Active    : Core.Strings.VarString = 'ac';
        Alias     : Core.Strings.VarString = 'al';
        Holder    : Core.Strings.VarString = 'ch';
      end;
    end;
    TItem=record
      ID          : QWord;
      Number      : QWord;
      ExpMonth    : Byte;
      ExpYear     : Word;
      Code        : Word;
      Active      : Boolean;
      Alias       : Core.Strings.VarString;
      Holder      : Core.Strings.VarString;
      Valid       : Boolean;
    end;
    PItem=^TItem;
    TItems=Array of PItem;
    PItems=^TItems;

    class procedure Empty(var Item:TItem); overload;
    class procedure Empty(var Item:TItems); overload;
    class procedure Init(var Item:TItem); overload;
    class procedure Init(var Item:TItems); overload;
    class procedure Done(var Item:TItem); overload;
    class procedure Done(var Item:TItems); overload;
    class function  IndexOf(ID:QWord; var List:TItems): LongInt;
    class procedure Invalidate(Var List:TItems);
    class procedure Purge(Var Item:TItems);

    class function  Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Item:TItem):boolean;
    class function  Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
    class function  Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
    class function  Read(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; var Item:TItem):boolean;
    class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Items:TItems):boolean;

    class function  fromXML(xItem:TDOMNode; var Item:TItem):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;

    class function  toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean; overload;
    class function  toXML(var Items:TItems; Output:TMemoryStream; Header:Boolean):boolean; overload;
  end;
  Tax=class
  type
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        NameShort                : Core.Database.Types.Integer = 3;
        NameLong                 : Core.Database.Types.Integer = 4;
        Collect                  : Core.Database.Types.Integer = 5;
        Rate                     : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        NameShort                : Core.Database.Types.VarString = 'INST';
        NameLong                 : Core.Database.Types.VarString = 'INLG';
        Collect                  : Core.Database.Types.VarString = 'ICOT';
        Rate                     : Core.Database.Types.VarString = 'IRAE';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'System/Commerce';
        Name                     : 'Tax';
        Value                    : 'scs_com_tax';
        Hint                     : 'Tax table storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields                     : array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NameShort; KeyP: @Keys.NameShort; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NameLong; KeyP: @Keys.NameLong; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Collect; KeyP: @Keys.Collect; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Rate; KeyP: @Keys.Rate; DataType: dftFloat; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
    end;
    XML=class
    type
       Stanzas=class
       const
         Item      : Core.Strings.VarString = 'tax';
         Items     : Core.Strings.VarString = 'taxes';
       end;
       Fields=class
       const
         NameLong  : Core.Strings.VarString = 'nl';
         NameShort : Core.Strings.VarString = 'ns';
         Collect   : Core.Strings.VarString = 'cl';
         Rate      : Core.Strings.VarString = 'rt';
       end;
    end;
    TItem=record
      NameLong    : Core.Strings.VarString;
      NameShort   : Core.Strings.VarString;
      Collect     : boolean;
      Rate        : Single;
    end;
    class procedure Empty(var Item:TItem);
    class procedure Init(var Item:TItem);
    class procedure Done(var Item:TItem);
    class function  fromXML(xItem:TDOMNode; var Item:TItem):boolean;
    class function  toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
  end;
  Purchase=class
  type
    Kind=class
    const
      Membership   : byte = 0;
      Subscription : byte = 1;
      Item         : byte = 2;
      Refund       : byte = 3;
      List         : array[0..3] of Core.Strings.VarString = (
        'Membership',
        'Subscription',
        'Item',
        'Refund'
      );
      asString='Membership,Subscription,Item,Refund';
    end;
    Log=class
    type
      DB = class
      type
        IDs = class
        const
          ID                     : Core.Database.Types.Integer = 0;
          InsertID               : Core.Database.Types.Integer = 1;
          DomainID               : Core.Database.Types.Integer = 2;
          Customer               : Core.Database.Types.Integer = 3;
          Purchases              : Core.Database.Types.Integer = 4;
          Date                   : Core.Database.Types.Integer = 5;
          Tax                    : Core.Database.Types.Integer = 6;
          Total                  : Core.Database.Types.Integer = 7;
          Confirmation           : Core.Database.Types.Integer = 8;
        end;
        Keys=class
        const
          ID                     : Core.Database.Types.VarString = 'ITID';
          InsertID               : Core.Database.Types.VarString = 'IIID';
          DomainID               : Core.Database.Types.VarString = 'IDID';
          Customer               : Core.Database.Types.VarString = 'ICUD';
          Purchases              : Core.Database.Types.VarString = 'IPUE';
          Date                   : Core.Database.Types.VarString = 'IDAE';
          Tax                    : Core.Database.Types.VarString = 'ITAX';
          Total                  : Core.Database.Types.VarString = 'ITOL';
          Confirmation           : Core.Database.Types.VarString = 'ICNF';
        end;
      const
        TableP                   : Core.Database.Types.PTable = nil;
        MonitorP                 : Core.Database.Monitor.Types.PItem = nil;
        Startup                  : Core.Database.Types.TableIni = (
          AutoCreate             : True;
          AutoCommit             : True;
          Group                  : 'System/Commerce';
          Name                   : 'Purchase Log';
          Value                  : 'scs_com_plog';
          Hint                   : 'Log of all purchases';
          PrimaryKeyP            : @Keys.ID;
          );
        Fields: array [0..8] of Core.Database.Types.Field = (
          (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
          (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Customer; KeyP: @Keys.Customer; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Purchases; KeyP: @Keys.Purchases; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone; ),
          (IDP: @IDs.Date; KeyP: @Keys.Date; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Tax; KeyP: @Keys.Tax; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Total; KeyP: @Keys.Total; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Confirmation; KeyP: @Keys.Confirmation; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
        );
      end;
      XML=class
      type
         Stanzas=class
         const
           Item         : Core.Strings.VarString = 'log';
           Items        : Core.Strings.VarString = 'logs';
         end;
         Fields=class
         const
           ID           : Core.Strings.VarString = 'id';
           Customer     : Core.Strings.VarString = 'cs';
           Purchases    : Core.Strings.VarString = 'pc';
           Date         : Core.Strings.VarString = 'de';
           Tax          : Core.Strings.VarString = 'tx';
           Total        : Core.Strings.VarString = 'tl';
           Confirmation : Core.Strings.VarString = 'cf';
         end;
      end;
      TItem=record
        ID           : QWord;
        Customer     : QWord;
        Purchases    : Core.Arrays.Types.LargeWord;
        Date         : Double;
        Tax          : Double;
        Total        : Double;
        Confirmation : Core.Strings.VarString;
      end;
      class procedure Empty(var Item:TItem);
      class procedure Init(var Item:TItem);
      class procedure Done(var Item:TItem);
      class function  fromXML(xItem:TDOMNode; var Item:TItem):boolean;
      class function  toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
    end;
    Item=class
    type
      TItem=record
        ID             : QWord;
        Title          : Core.Strings.VarString;
        Description    : Core.Strings.VarString;
        Kind           : Byte;
        Price          : Double;
        Taxable        : boolean;
        Available      : Int64;
        Inventory      : Int64;
        Backorder      : Int64;
        Enabled        : boolean;
        Valid          : boolean;
      end;
      PItem=^TItem;
      PItems=^TItems;
      TItems=array of PItem;
      DB = class
      type
        IDs = class
        const
          ID                     : Core.Database.Types.Integer = 0;
          InsertID               : Core.Database.Types.Integer = 1;
          DomainID               : Core.Database.Types.Integer = 2;
          Title                  : Core.Database.Types.Integer = 3;
          Description            : Core.Database.Types.Integer = 4;
          Kind                   : Core.Database.Types.Integer = 5;
          Price                  : Core.Database.Types.Integer = 6;
          Taxable                : Core.Database.Types.Integer = 7;
          Available              : Core.Database.Types.Integer = 8;
          Inventory              : Core.Database.Types.Integer = 9;
          Backorder              : Core.Database.Types.Integer = 10;
          Enabled                : Core.Database.Types.Integer = 11;
        end;
        Keys=class
        const
          ID                     : Core.Database.Types.VarString = 'ITID';
          InsertID               : Core.Database.Types.VarString = 'IIID';
          DomainID               : Core.Database.Types.VarString = 'IDID';
          Title                  : Core.Database.Types.VarString = 'ITIE';
          Description            : Core.Database.Types.VarString = 'IDIN';
          Kind                   : Core.Database.Types.VarString = 'IKID';
          Price                  : Core.Database.Types.VarString = 'IPRE';
          Taxable                : Core.Database.Types.VarString = 'ITAE';
          Available              : Core.Database.Types.VarString = 'IAVE';
          Inventory              : Core.Database.Types.VarString = 'IINY';
          Backorder              : Core.Database.Types.VarString = 'IBOR';
          Enabled                : Core.Database.Types.VarString = 'IEND';
        end;
      const
        TableP                   : Core.Database.Types.PTable = nil;
        MonitorP                 : Core.Database.Monitor.Types.PItem = nil;
        Startup                  : Core.Database.Types.TableIni = (
          AutoCreate             : True;
          AutoCommit             : True;
          Group                  : 'System/Commerce';
          Name                   : 'Purchases';
          Value                  : 'scs_com_ptms';
          Hint                   : 'Items that can be purchased';
          PrimaryKeyP            : @Keys.ID;
        );
        Fields                   : array [0..11] of Core.Database.Types.Field = (
          (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; ),
          (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.Price; KeyP: @Keys.Price; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Taxable; KeyP: @Keys.Taxable; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.Available; KeyP: @Keys.Available; DataType: dftInt64; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Inventory; KeyP: @Keys.Inventory; DataType: dftInt64; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
          (IDP: @IDs.Backorder; KeyP: @Keys.Backorder; DataType: dftInt64; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
          (IDP: @IDs.Enabled; KeyP: @Keys.Enabled; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
        );
        class function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TItem):boolean;
        class function  Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
        class function  Write(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TItem):boolean;
        class function  Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; var Item:TItem):boolean;
        class function  List(Task:Core.Database.Types.TTask; DomainID:QWord; var Items:TItems):boolean;
      end;
      XML=class
      type
         Stanzas=class
         const
           Item        : Core.Strings.VarString = 'pc';
           Items       : Core.Strings.VarString = 'pcs';
         end;
         Fields=class
         const
           ID          : Core.Strings.VarString = 'id';
           Title       : Core.Strings.VarString = 'tl';
           Description : Core.Strings.VarString = 'dn';
           Kind        : Core.Strings.VarString = 'kd';
           Price       : Core.Strings.VarString = 'pe';
           Taxable     : Core.Strings.VarString = 'te';
           Available   : Core.Strings.VarString = 'ae';
           Inventory   : Core.Strings.VarString = 'iy';
           Backorder   : Core.Strings.VarString = 'br';
           Enabled     : Core.Strings.VarString = 'ed';
         end;
      end;
      class procedure Empty(var Item:TItems); overload;
      class procedure Init(var Item:TItems); overload;
      class procedure Done(var Item:TItems); overload;
      class procedure Invalidate(var Item:TItems);
      class procedure Purge(var Item:TItems);
      class function  getPrice(var Item:TItem):Core.Strings.VarString;

      class procedure Empty(var Item:TItem); overload;
      class procedure Init(var Item:TItem); overload;
      class procedure Done(var Item:TItem); overload;

      class function  IndexOf(ID:QWord; var List:TItems): LongInt;
      class function  IndexOf(Title:Core.Strings.VarString; var List:TItems): LongInt;

      class function  fromXML(xItem:TDOMNode; var Item:TItem):boolean; overload;
      class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;
      class function  fromXML(xDoc:TXMLDocument; var Item:TItems):boolean; overload;

      class function  toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean; overload;
      class function  toXML(var Items:TItems; Output:TMemoryStream; Header:Boolean):boolean; overload;
    end;
  end;

implementation
uses DB;


procedure cbDestroyPurchaseItem(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Purchase.Item.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyPurchaseLog(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Purchase.Log.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyTax(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Tax.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyCard(ItemP: Core.Database.Monitor.Types.PItem);
begin
  with Card.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Purchase.Item.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Purchase.Item.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Purchase.Item.DB.TableP, useForCriteria, Purchase.Item.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Purchase.Log.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Purchase.Log.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Purchase.Log.DB.TableP, useForCriteria, Purchase.Log.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Card.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Card.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Card.DB.TableP, useForCriteria, Card.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end else if ItemP = Tax.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Tax.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Tax.DB.TableP, useForCriteria, Tax.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

  procedure PushUserDeleted;
  begin
    if ItemP = Purchase.Log.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Purchase.Log.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Purchase.Log.DB.TableP, useForCriteria, Purchase.Log.DB.IDs.Customer, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;

begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED      : PushDomainDeleted();
    Core.Database.Monitor.Notify.USER_DELETED        : PushUserDeleted();
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Purchase.Item.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyPurchaseItem, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Purchase.Log.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyPurchaseLog, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Tax.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTax, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
  with Card.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyCard, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;


class procedure Purchase.Item.Empty(var Item:TItem);
begin
  Item.ID:=0;
  SetLength(Item.Title,0);
  SetLength(Item.Description,0);

  Item.Kind:=0;
  Item.Price:=0;
  Item.Taxable:=false;
  Item.Available:=0;
  Item.Inventory:=0;
  Item.Backorder:=0;
  Item.Enabled:=false;
  Item.Valid:=false;
end;

class procedure Purchase.Item.Empty(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Purchase.Item.Init(var Item:TItem);
begin
  Item.ID:=0;
  SetLength(Item.Title,0);
  SetLength(Item.Description,0);

  Item.Kind:=0;
  Item.Price:=0;
  Item.Taxable:=false;
  Item.Available:=0;
  Item.Inventory:=0;
  Item.Backorder:=0;
  Item.Enabled:=false;
  Item.Valid:=false;
end;

class procedure Purchase.Item.Init(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Purchase.Item.Done(var Item:TItem);
begin
  Finalize(Item.Title);
  Finalize(Item.Description);
  Finalize(Item);
end;

class procedure Purchase.Item.Done(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

class function  Purchase.Item.IndexOf(ID:QWord; var List:TItems): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(List) do begin
    if (List[iLcv]<>nil) and (List[iLcv]^.ID=ID) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function  Purchase.Item.IndexOf(Title:Core.Strings.VarString; var List:TItems): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(List) do begin
    if (List[iLcv]<>nil) and SameText(List[iLcv]^.Title,Title) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class procedure Purchase.Item.Invalidate(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    if (Item[iLcv]<>nil) then
      Item[iLcv]^.Valid:=false;
  end;
end;

class function  Purchase.Item.getPrice(var Item:TItem):Core.Strings.VarString;
begin
  Result:=Format('%.2F',[Item.Price]);
end;

class procedure Purchase.Item.Purge(var Item:TItems);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iLength:LongInt;
begin
  iLcv:=0;
  iLength:=System.Length(Item);
  while (iLcv<iLength) do begin
    if (Item[iLcv]^.Valid=false) then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      for jLcv:=iLcv to High(Item)-1 do
        Item[jLcv]:=Item[jLcv+1];
      Dec(iLength);
      SetLength(Item,iLength);
    end else
      Inc(iLcv);
  end;
end;

class function  Purchase.Item.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.Title:=toString(xItem,XML.Fields.Title);
      Item.Description:=toString(xItem,XML.Fields.Description);
      Item.Kind:=toByte(xItem,XML.Fields.Kind);
      Item.Price:=toDouble(xItem,XML.Fields.Price);
      Item.Taxable:=toBoolean(xItem,XML.Fields.Taxable);
      Item.Available:=toInt64(xItem,XML.Fields.Available);
      Item.Inventory:=toInt64(xItem,XML.Fields.Inventory);
      Item.Backorder:=toInt64(xItem,XML.Fields.Backorder);
      Item.Enabled:=toBoolean(xItem,XML.Fields.Enabled);
      Item.Valid:=true;
      Result:=True;
    end;
  end else
    Result:=false;
end;

class function  Purchase.Item.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Item),Item);
end;

class function  Purchase.Item.fromXML(xDoc:TXMLDocument; var Item:TItems):boolean;
var
  xItems : TDOMNode;
  xItem  : TDOMNode;
  xID    : TDOMNode;
  iLcv   : LongInt;
  ID     : QWord;
  idx    : LongInt;
  itmP   : PItem;
begin
  Result:=false;
  Invalidate(Item);
  xItems:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Items);
  if (xItems<>nil) then begin
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      xItem:=xItems.ChildNodes[iLcv];
      if SameText(xItem.NodeName,XML.Stanzas.Item) then begin
        xID:=Core.XML.DB.getChildNode(xItem,XML.Fields.ID);
        if (xID<>nil) then begin
          ID:=Core.XML.DB.toQWord(xItem,XML.Fields.ID);
          idx:=IndexOf(ID,Item);
          if idx=-1 then begin
            idx:=Length(Item);
            New(itmP);
            Init(itmP^);
            SetLength(Item,idx+1);
            Item[idx]:=itmP;
          end else begin
            itmP:=Item[idx];
          end;
          fromXML(xItem,itmP^);
        end;
      end;
    end;
  end;
  Purge(Item);
  Result:=True;
end;

class function  Purchase.Item.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Title,Item.Title,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Description,Item.Description,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(XML.Fields.Price,Item.Price),Output);
    Core.Streams.Write(Print(XML.Fields.Taxable,Item.Taxable),Output);
    Core.Streams.Write(Print(XML.Fields.Available,Item.Available),Output);
    Core.Streams.Write(Print(XML.Fields.Inventory,Item.Inventory),Output);
    Core.Streams.Write(Print(XML.Fields.Backorder,Item.Backorder),Output);
    Core.Streams.Write(Print(XML.Fields.Enabled,Item.Enabled),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;

end;

class function  Purchase.Item.toXML(var Items:TItems; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to High(Items) do
    if Items[iLcv]^.Valid=true then
      toXML(Items[iLcv]^,Output,false);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;

end;

class function  Purchase.Item.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TItem):boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Title,poNone,oNone,Item.Title,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Description,poNone,oNone,Item.Description,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,Item.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Price,poNone,oNone,Item.Price,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Taxable,poNone,oNone,Item.Taxable,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Available,poNone,oNone,Item.Available,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Inventory,poNone,oNone,Item.Inventory,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Backorder,poNone,oNone,Item.Backorder,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Enabled,poNone,oNone,Item.Enabled,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Purchase.Item.DB.Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Purchase.Item.DB.Write(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Title, poNone, oNone, Item.Title, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Description, poNone, oNone, Item.Description, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Kind, poNone, oNone, Item.Kind, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Price, poNone, oNone, Item.Price, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Taxable, poNone, oNone, Item.Taxable, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Available, poNone, oNone, Item.Available, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Inventory, poNone, oNone, Item.Inventory, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Backorder, poNone, oNone, Item.Backorder, Commands);
    Core.Database.AddCommand(iCount, TableP, useForValues, IDs.Enabled, poNone, oNone, Item.Enabled, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadPurchaseItem(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItemP:Purchase.Item.PItem;
begin
  ItemP:=DataP;
  {$i Storage.Commerce.Purchase.Item.Read.inc}
  ItemP^.Valid:=True;
end;

class function Purchase.Item.DB.Read(Task:Core.Database.Types.TTask;  DomainID,ItemID:QWord; var Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.ID, poAnd, oEqual, ItemID, Commands);

    {$i Storage.Commerce.Purchase.Item.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadPurchaseItem, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbListPurchaseItems(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Purchase.Item.PItems;
  ItemP:Purchase.Item.PItem;
  iIndex:LongInt;
  ID:QWord;
begin
  ListP:=DataP;
  ID:=Fields.FieldByName(Purchase.Item.DB.Keys.ID).AsLargeInt;
  iIndex:=Purchase.Item.IndexOf(ID,ListP^);
  if (iIndex=-1) then begin
    iIndex:=System.Length(ListP^);
    SetLength(ListP^,iIndex+1);
    New(ItemP);
    Purchase.Item.Init(ItemP^);
    ItemP^.ID:=ID;
    ListP^[iIndex]:=ItemP;
  end else
    ItemP:=ListP^[iIndex];
  {$i Storage.Commerce.Purchase.Item.Read.inc}
  ItemP^.Valid:=true;
end;


class function Purchase.Item.DB.List(Task:Core.Database.Types.TTask; DomainID:QWord; var Items:TItems):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Invalidate(Items);

    Core.Database.AddCommand(iCount, TableP,@Commands);
    Core.Database.AddCommand(iCount, TableP, useForCriteria, IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, TableP, useForOrderBy, IDs.ID,poNone,oNone,Commands);

    {$i Storage.Commerce.Purchase.Item.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListPurchaseItems, @Items);

    Purge(Items);

  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure Address.Empty(var Item:TItem);
begin
  SetLength(Item.Address1,0);
  SetLength(Item.Address2,0);
  SetLength(Item.City,0);
  SetLength(Item.State,0);
  SetLength(Item.Post,0);
end;

class procedure Address.Init(var Item:TItem);
begin
  SetLength(Item.Address1,0);
  SetLength(Item.Address2,0);
  SetLength(Item.City,0);
  SetLength(Item.State,0);
  SetLength(Item.Post,0);
end;

class procedure Address.Done(var Item:TItem);
begin
  Finalize(Item.Address1);
  Finalize(Item.Address2);
  Finalize(Item.City);
  Finalize(Item.State);
  Finalize(Item.Post);
  Finalize(Item);
end;

class function  Address.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.Address1:=toString(xItem,XML.Fields.Address1);
      Item.Address2:=toString(xItem,XML.Fields.Address2);
      Item.City:=toString(xItem,XML.Fields.City);
      Item.State:=toString(xItem,XML.Fields.State);
      Item.Post:=toString(xItem,XML.Fields.Post);
      Result:=True;
    end;
  end else
    Result:=false;
end;

class function  Address.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.Address1,Item.Address1,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Address2,Item.Address2,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.City,Item.City,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.State,Item.State,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Post,Item.Post,CDATA_ON),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class procedure Card.Empty(var Item:TItem);
begin
  Item.ID:=0;
  Item.Number:=0;
  Item.ExpMonth:=0;
  Item.ExpYear:=0;
  Item.Code:=0;
  Item.Active:=false;
  SetLength(Item.Alias,0);
  SetLength(Item.Holder,0);
end;

class procedure Card.Empty(var Item:TItems);
var
  iLcv:LongInt;
  ItmP:PItem;
begin
  for iLcv:=0 to High(Item) do begin
    ItmP:=Item[iLcv];
    Done(ItmP^);
    Dispose(ItmP);
  end;
  System.SetLength(Item,0);
end;

class procedure Card.Init(var Item:TItem);
begin
  Item.ID:=0;
  Item.Number:=0;
  Item.ExpMonth:=0;
  Item.ExpYear:=0;
  Item.Code:=0;
  Item.Active:=false;
  SetLength(Item.Alias,0);
  SetLength(Item.Holder,0);
end;

class procedure Card.Init(var Item:TItems);
var
  iLcv:LongInt;
  ItmP:PItem;
begin
  for iLcv:=0 to High(Item) do begin
    ItmP:=Item[iLcv];
    Done(ItmP^);
    Dispose(ItmP);
  end;
  System.SetLength(Item,0);
end;

class procedure Card.Done(var Item:TItem);
begin
  Finalize(Item.Alias);
  Finalize(Item.Holder);
  Finalize(Item);
end;

class procedure Card.Done(var Item:TItems);
var
  iLcv:LongInt;
  ItmP:PItem;
begin
  for iLcv:=0 to High(Item) do begin
    ItmP:=Item[iLcv];
    Done(ItmP^);
    Dispose(ItmP);
  end;
  System.SetLength(Item,0);
  Finalize(Item);
end;

class function  Card.IndexOf(ID:QWord; var List:TItems): LongInt;
var
  iLcv:LongInt;
  itmP:PItem;
begin
  Result:=-1;
  for iLcv:=0 to High(List) do begin
    itmP:=List[iLcv];
    if itmP^.ID=ID then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

class procedure Card.Purge(var Item:TItems);
var
  iLcv:LongInt;
  jLcv:LongInt;
  iLength:LongInt;
begin
  iLcv:=0;
  iLength:=System.Length(Item);
  while (iLcv<iLength) do begin
    if (Item[iLcv]^.Valid=false) then begin
      Done(Item[iLcv]^);
      Dispose(Item[iLcv]);
      for jLcv:=iLcv to High(Item)-1 do
        Item[jLcv]:=Item[jLcv+1];
      Dec(iLength);
      SetLength(Item,iLength);
    end else
      Inc(iLcv);
  end;
end;

class procedure Card.Invalidate(var List:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(List) do
    List[iLcv]^.Valid:=false;
end;

class function  Card.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.Number:=toQWord(xItem,XML.Fields.Number);
      Item.ExpMonth:=toByte(xItem,XML.Fields.ExpMonth);
      Item.ExpYear:=toWord(xItem,XML.Fields.ExpYear);
      Item.Active:=toBoolean(xItem,XML.Fields.Active);
      Item.Code:=toWord(xItem,XML.Fields.Code);
      Item.Alias:=toString(xItem,XML.Fields.Alias);
      Item.Holder:=toString(xItem,XML.Fields.Holder);
      Result:=True;
    end;
  end else
    Result:=false;
end;

class function  Card.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Stanzas.Item),Item);
end;

class function  Card.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Number,Item.Number),Output);
    Core.Streams.Write(Print(XML.Fields.ExpMonth,Item.ExpMonth),Output);
    Core.Streams.Write(Print(XML.Fields.ExpYear,Item.ExpYear),Output);
    Core.Streams.Write(Print(XML.Fields.Active,Item.Active),Output);
    Core.Streams.Write(Print(XML.Fields.Code,Item.Code),Output);
    Core.Streams.Write(Print(XML.Fields.Alias,Item.Alias),Output);
    Core.Streams.Write(Print(XML.Fields.Holder,Item.Holder),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Card.toXML(var Items:TItems; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to High(Items) do
    Card.toXML(Items[iLcv]^,Output,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  Result:=false;
end;

class function  Card.Add(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Item:TItem):boolean;
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
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,DB.IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,DB.IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,DB.IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,DB.IDs.InsertID,poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Alias),poNone,oNone,Item.Alias,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Holder),poNone,oNone,Item.Holder,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Number),poNone,oNone,Item.Number,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ExpMonth),poNone,oNone,Item.ExpMonth,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ExpYear),poNone,oNone,Item.ExpYear,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Code),poNone,oNone,Item.Code,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Active),poNone,oNone,Item.Active,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Card.Delete(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function Card.Write(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Item:TItem):boolean;
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
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Alias, poNone, oNone, Item.Alias, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Holder, poNone, oNone, Item.Holder, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Number, poNone, oNone, Item.Number, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.ExpMonth, poNone, oNone, Item.ExpMonth, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.ExpYear, poNone, oNone, Item.ExpYear, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Code, poNone, oNone, Item.Code, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Active, poNone, oNone, Item.Active, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadCardItem(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ItemP:Card.PItem;
begin
  ItemP:=DataP;
  ItemP^.ID:=Fields.FieldByName(Card.DB.Keys.ID).AsLargeInt;
  {$i Storage.Commerce.Card.Item.Read.inc}
  ItemP^.Valid:=True;
end;

class function Card.Read(Task:Core.Database.Types.TTask;  DomainID,UserID,ItemID:QWord; var Item:TItem):boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;
begin
  Result := False;
  try

    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);
    {$i Storage.Commerce.Card.Item.Read.Fields.inc}
    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadPurchaseItem, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;


procedure cbListCardItems(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
var
  ListP:Card.PItems;
  ItemP:Card.PItem;
  iIndex:LongInt;
  ID:QWord;
begin
  ListP:=DataP;
  ID:=Fields.FieldByName(Card.DB.Keys.ID).AsLargeInt;
  iIndex:=Card.IndexOf(ID,ListP^);
  if (iIndex=-1) then begin
    iIndex:=System.Length(ListP^);
    SetLength(ListP^,iIndex+1);
    New(ItemP);
    Card.Init(ItemP^);
    ItemP^.ID:=ID;
    ListP^[iIndex]:=ItemP;
  end else
    ItemP:=ListP^[iIndex];
  {$i Storage.Commerce.Card.Item.Read.inc}
  ItemP^.Valid:=true;
end;


class function Card.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Items:TItems):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Invalidate(Items);

    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForOrderBy, DB.IDs.ID,poNone,oNone,Commands);

    {$i Storage.Commerce.Card.Item.Read.Fields.inc}

    Result := Core.Database.SQL.Select(Task, @Commands, @cbListCardItems, @Items);

    Purge(Items);
  finally
    Core.Database.Done(Commands);
  end;
end;


class procedure Tax.Empty(var Item:TItem);
begin
  SetLength(Item.NameLong,0);
  SetLength(Item.NameShort,0);
  Item.Collect:=false;
  Item.Rate:=0;
end;

class procedure Tax.Init(var Item:TItem);
begin
  SetLength(Item.NameLong,0);
  SetLength(Item.NameShort,0);
  Item.Collect:=false;
  Item.Rate:=0;
end;

class procedure Tax.Done(var Item:TItem);
begin
  Finalize(Item.NameLong);
  Finalize(Item.NameShort);
  Finalize(Item);
end;

class function  Tax.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.NameLong:=toString(xItem,XML.Fields.NameLong);
      Item.NameShort:=toString(xItem,XML.Fields.NameShort);
      Item.Collect:=toBoolean(xItem,XML.Fields.Collect);
      Item.Rate:=toSingle(xItem,XML.Fields.Rate);
      Result:=True;
    end;
  end else
    Result:=false;
end;

class function  Tax.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.NameLong,Item.NameLong,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.NameShort,Item.NameShort,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Collect,Item.Collect),Output);
    Core.Streams.Write(Print(XML.Fields.Rate,Item.Rate),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class procedure Purchase.Log.Empty(var Item:TItem);
begin
  Item.ID:=0;
  Item.Customer:=0;
  Core.Arrays.LargeWord.Empty(Item.Purchases);
  Item.Date:=0;
  Item.Tax:=0;
  Item.Total:=0;
  SetLength(Item.Confirmation,0);
end;

class procedure Purchase.Log.Init(var Item:TItem);
begin
  Item.ID:=0;
  Item.Customer:=0;
  Core.Arrays.LargeWord.Init(Item.Purchases);
  Item.Date:=0;
  Item.Tax:=0;
  Item.Total:=0;
  SetLength(Item.Confirmation,0);
end;

class procedure Purchase.Log.Done(var Item:TItem);
begin
  Item.ID:=0;
  Item.Customer:=0;
  Core.Arrays.LargeWord.Done(Item.Purchases);
  Item.Date:=0;
  Item.Tax:=0;
  Item.Total:=0;
  Finalize(Item.Confirmation);
  Finalize(Item);
end;

class function  Purchase.Log.fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Empty(Item);
  if xItem<>nil then begin
    with Core.XML.DB do begin
      Item.ID:=toQWord(xItem,XML.Fields.ID);
      Item.Customer:=toQWord(xItem,XML.Fields.Customer);
      toQWordArray(xItem,XML.Fields.Purchases,Item.Purchases);
      Item.Date:=toDouble(xItem,XML.Fields.Date);
      Item.Tax:=toDouble(xItem,XML.Fields.Tax);
      Item.Total:=toDouble(xItem,XML.Fields.Total);
      Item.Confirmation:=toString(xItem,XML.Fields.Confirmation);
      Result:=True;
    end;
  end else
    Result:=false;
end;

class function  Purchase.Log.toXML(var Item:TItem; Output:TMemoryStream; Header:Boolean):boolean;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Fields.Customer,Item.Customer),Output);
    Core.Streams.Write(Print(XML.Fields.Purchases,Item.Purchases),Output);
    Core.Streams.Write(Print(XML.Fields.Date,Item.Date),Output);
    Core.Streams.Write(Print(XML.Fields.Tax,Item.Tax),Output);
    Core.Streams.Write(Print(XML.Fields.Total,Item.Total),Output);
    Core.Streams.Write(Print(XML.Fields.Confirmation,Item.Confirmation,CDATA_ON),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

initialization
  RegisterDB;

end.


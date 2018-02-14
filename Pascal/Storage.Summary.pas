unit Storage.Summary;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Strings,
  Core.Streams,

  Storage,
  Storage.Main,

  Core.XML,

  DOM,
  SysUtils;

Type
  Music=class
  Type
    XML=class
    const
      Stanza                 = 's';
      Tags                   = 'tags';
      Tag                    = 'tag';
    Type
      Fields=class
      const
        Inspected            = 'ip';
        Kind                 = 'k';
        Name                 = 'n';
        Value                = 'v';
        // New Values
        Album                = 'al';
        Song                 = 's';
        Details              = 'd';
        Artist               = 'a';
        Accompaniment        = 'ac';
        Composer             = 'co';
        TrackNumber          = 'tn';
        Year                 = 'yr';
        Genre                = 'ge';
        Group                = 'gp';
      end;
    end;
    TItem=record
      Kind                   : LongInt;
      Name                   : Core.Strings.VarString;
      Value                  : Core.Strings.VarString;
    end;
    PItem=^TItem;
    TItems=record
      Inspected              : boolean;
      Tags                   : Array of PItem;

      Album                  : Core.Strings.VarString;
      Song                   : Core.Strings.VarString;
      Details                : Core.Strings.VarString;
      Artist                 : Core.Strings.VarString;
      Accompaniment          : Core.Strings.VarString;
      Composer               : Core.Strings.VarString;
      TrackNumber            : Core.Strings.VarString;
      Year                   : Core.Strings.VarString;
      Genre                  : Core.Strings.VarString;
      Group                  : Core.Strings.VarString;
    end;
    class procedure Copy(var Source,Dest:TItem);

    class procedure Init(var Items:TItems); overload;
    class procedure Init(var Item:TItem); overload;

    class procedure Empty(Var Items:TItems); overload;
    class procedure Empty(var Item:TItem); overload;

    class procedure Done(var Item:TItem); overload;
    class procedure Done(var Items:TItems); overload;

    class function  Add(itmP:PItem; var Items:TItems): LongInt;
    class function  Add(var Item:TItem; var Items:TItems): LongInt;

    class function  Wrap(var Summary:Core.Strings.VarString):Core.Strings.VarString;

    class function toXML(var Items:TItems; Refactor:TStream):Core.Strings.VarString; overload;
    class function toXML(var Item:TItem; Output:TStream):boolean; overload;
    class function fromXML(xNode:TDOMNode; var Items:TItems):boolean; overload;
    class function fromXML(xNode:TDOMNode; var Item:TItem):boolean; overload;
  end;
  Video=class
  Type
    XML=class
    const
      Stanza                 = 's';
      People                 = 'ppl';
    Type
      Fields=class
      const
        Status               = 'st';
        Duration             = 'd';
        Time                 = 't';
        Location             = 'l';
        Person               = 'p';
      end;
    end;
    Status=class
    const
      None                   = 0;
      Playing                = 1;
      Encoding               = 2;
    end;

    PItem=^TItem;
    TItem=record
      Status                 : Byte;
      Duration               : Double;
      Time                   : Double;
      Location               : Double;
      People                 : Core.Arrays.Types.VarString;
    end;
    class procedure Copy(var Source,Dest:TItem);

    class procedure Init(var Item:TItem);
    class procedure Empty(var Item:TItem);
    class procedure Done(var Item:TItem);

    class function  Wrap(var Summary:Core.Strings.VarString):Core.Strings.VarString;

    class function toXML(var Item:TItem; Output:TStream):boolean;
    class function fromXML(xNode:TDOMNode; var Item:TItem):boolean;
  end;

implementation


class function Music.toXML(var Item:TItem; Output:TStream):boolean;
begin
  with Core.XML.DB do begin
    Core.Streams.Write(
      Concat(
        '<',XML.Tag,'>',
        Print(XML.Fields.Kind,Item.Kind),
        Print(XML.Fields.Name,Item.Name,CDATA_OFF),
        Print(XML.Fields.Value,Item.Value,CDATA_ON),
        '</',XML.Tag,'>'
      ),
      Output
    );
  end;
  Result:=true;
end;

class function Music.toXML(var Items:TItems; Refactor:TStream):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  Refactor.Size:=0;
  Try
    with Core.XML.DB do begin
      Core.Streams.Write(Print(XML.Fields.Inspected,Items.Inspected),Refactor);
      Core.Streams.Write(Print(XML.Fields.Album,Items.Album,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Song,Items.Song,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Details,Items.Details,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Artist,Items.Artist,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Accompaniment,Items.Accompaniment,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Composer,Items.Composer,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.TrackNumber,Items.TrackNumber,CDATA_OFF),Refactor);
      Core.Streams.Write(Print(XML.Fields.Year,Items.Year,CDATA_OFF),Refactor);
      Core.Streams.Write(Print(XML.Fields.Genre,Items.Genre,CDATA_ON),Refactor);
      Core.Streams.Write(Print(XML.Fields.Group,Items.Group,CDATA_ON),Refactor);
      Core.Streams.Write(Concat('<',XML.Tags,'>'),Refactor);
      for iLcv:=0 to High(Items.Tags) do
        Music.toXML(Items.Tags[iLcv]^,Refactor);
      Core.Streams.Write(Concat('</',XML.Tags,'>'),Refactor);
    end;
    Result:=Core.Streams.toString(Refactor);
  finally
    Refactor.Size:=0;
  end;
end;

class function Music.fromXML(xNode:TDOMNode; var Item:TItem):boolean;
begin
  with Core.XML.DB do begin
    Item.Kind:=toInteger(xNode,XML.Fields.Kind);
    Item.Name:=toString(xNode,XML.Fields.Name);
    Item.Value:=toString(xNode,XML.Fields.Value);
  end;
  Result:=True;
end;

class function Music.fromXML(xNode:TDOMNode; var Items:TItems):boolean;
var
  xTags:TDOMNode;
  xTag:TDOMNode;
  itmP:PItem;
  iLcv:LongInt;
begin
  Result:=false; Empty(Items);

  Items.Inspected:=Core.XML.DB.toBoolean(xNode,XML.Fields.Inspected);
  Items.Album:=Core.XML.DB.toString(xNode,XML.Fields.Album);
  Items.Song:=Core.XML.DB.toString(xNode,XML.Fields.Song);
  Items.Details:=Core.XML.DB.toString(xNode,XML.Fields.Details);
  Items.Artist:=Core.XML.DB.toString(xNode,XML.Fields.Artist);
  Items.Accompaniment:=Core.XML.DB.toString(xNode,XML.Fields.Accompaniment);
  Items.Composer:=Core.XML.DB.toString(xNode,XML.Fields.Composer);
  Items.TrackNumber:=Core.XML.DB.toString(xNode,XML.Fields.TrackNumber);
  Items.Year:=Core.XML.DB.toString(xNode,XML.Fields.Year);
  Items.Genre:=Core.XML.DB.toString(xNode,XML.Fields.Genre);
  Items.Group:=Core.XML.DB.toString(xNode,XML.Fields.Group);

  xTags:=Core.XML.DB.getChildNode(xNode,XML.Tags);
  if xTags<>nil then begin
    for iLcv:=0 To xTags.ChildNodes.Count-1 do begin
      xTag:=xTags.ChildNodes[iLcv];
      if SysUtils.SameText(xTag.NodeName,XML.Tag) then begin
        new(itmP);
        Init(itmP^);
        Add(itmP,Items);
        fromXML(xTag,itmP^);
      end;
    end;
    Result:=True;
  end;
end;

class procedure Music.Init(var Items:TItems);
var
  iLcv:LongInt;
begin
  Items.Inspected:=False;
  for iLcv:=0 to High(Items.Tags) do begin
    Done(Items.Tags[iLcv]^);
    Dispose(Items.Tags[iLcv]);
  end;
  SetLength(Items.Tags,0);

  SetLength(Items.Album,0);
  SetLength(Items.Song,0);
  SetLength(Items.Artist,0);
  SetLength(Items.Details,0);
  SetLength(Items.Accompaniment,0);
  SetLength(Items.Composer,0);
  SetLength(Items.TrackNumber,0);
  SetLength(Items.Year,0);
  SetLength(Items.Genre,0);
  SetLength(Items.Group,0);
end;

class procedure Music.Init(var Item:TItem);
begin
  Item.Kind:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
end;

class procedure Music.Empty(Var Items:TItems);
var
  iLcv:LongInt;
begin
  Items.Inspected:=false;
  for iLcv:=0 to High(Items.Tags) do begin
    Done(Items.Tags[iLcv]^);
    Dispose(Items.Tags[iLcv]);
  end;
  SetLength(Items.Tags,0);

  SetLength(Items.Album,0);
  SetLength(Items.Song,0);
  SetLength(Items.Details,0);
  SetLength(Items.Artist,0);
  SetLength(Items.Accompaniment,0);
  SetLength(Items.Composer,0);
  SetLength(Items.TrackNumber,0);
  SetLength(Items.Year,0);
  SetLength(Items.Genre,0);
  SetLength(Items.Group,0);
end;

class procedure Music.Empty(var Item:TItem);
begin
  Item.Kind:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
end;

class procedure Music.Done(var Item:TItem);
begin
  Finalize(Item.Name);
  Finalize(Item.Value);
  Finalize(Item);
end;

class procedure Music.Done(var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items.Tags) do begin
    Done(Items.Tags[iLcv]^);
    Dispose(Items.Tags[iLcv]);
  end;

  Finalize(Items.Album);
  Finalize(Items.Song);
  Finalize(Items.Details);
  Finalize(Items.Artist);
  Finalize(Items.Accompaniment);
  Finalize(Items.Composer);
  Finalize(Items.TrackNumber);
  Finalize(Items.Year);
  Finalize(Items.Genre);
  Finalize(Items.Group);

  Finalize(Items);
end;

class procedure Music.Copy(var Source,Dest:TItem);
begin
  Dest.Kind:=Source.Kind;
  Dest.Name:=Source.Name;
  Dest.Value:=Source.Value;
end;

class function  Music.Add(itmP:PItem; var Items:TItems): LongInt;
begin
  Result:=System.Length(Items.Tags);
  System.SetLength(Items.Tags,Result+1);
  Items.Tags[Result]:=itmP;
end;

class function  Music.Add(var Item:TItem; var Items:TItems): LongInt;
var
  itmP:PItem;
begin
  new(itmP);
  Init(itmP^);
  Copy(Item,itmP^);
  Result:=Add(itmP,Items);
end;

class function  Music.Wrap(var Summary:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat(Core.XML.DB.Header(Storage.Main.Header.Encoding),'<',XML.Stanza,'>',Summary,'</',XML.Stanza,'>');
end;


class procedure Video.Copy(var Source,Dest:TItem);
begin
  Dest.Duration:=Source.Duration;
  Dest.Location:=Source.Location;
  Dest.Status:=Source.Status;
  Dest.Time:=Source.Time;
  Core.Arrays.VarString.Copy(Source.People,Dest.People);
end;

class procedure Video.Init(var Item:TItem);
begin
  Item.Time:=0;
  Item.Status:=0;
  Item.Location:=0;
  Item.Duration:=0;
  Core.Arrays.VarString.Init(Item.People);
end;

class procedure Video.Empty(var Item:TItem);
begin
  Item.Time:=0;
  Item.Status:=0;
  Item.Location:=0;
  Item.Duration:=0;
  Core.Arrays.VarString.Empty(Item.People);
end;

class procedure Video.Done(var Item:TItem);
begin
  Core.Arrays.VarString.Done(Item.People);
  Finalize(Item);
end;

class function  Video.Wrap(var Summary:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat(Core.XML.DB.Header(Storage.Main.Header.Encoding),'<',XML.Stanza,'>',Summary,'</',XML.Stanza,'>');
end;

class function  Video.toXML(var Item:TItem; Output:TStream):boolean;
var
  iLcv:LongInt;
begin
  Result:=false;
  with Core.XML.DB do begin
    Core.Streams.Write(
      Concat(
        Print(XML.Fields.Status,Item.Status),
        Print(XML.Fields.Location,Item.Location),
        Print(XML.Fields.Duration,Item.Duration),
        Print(XML.Fields.Time,Item.Time)
      ),
      Output
    );
    Core.Streams.Write(Concat('<',XML.People,'>'),Output);
    for iLcv:=0 to High(Item.People) do
      Core.Streams.Write(Print(XML.Fields.Person,Item.People[iLcv],CDATA_ON),Output);
    Core.Streams.Write(Concat('</',XML.People,'>'),Output);
  end;
  Result:=true;
end;

class function  Video.fromXML(xNode:TDOMNode; var Item:TItem):boolean;
var
  xPeople:TDOMNode;
  iLcv:LongInt;
begin
  Result:=false;
  if xNode<>nil then begin
    xPeople:=Core.XML.DB.getChildNode(xNode,XML.People);
    Core.Arrays.VarString.Empty(Item.People);
    with Core.XML.DB do begin
      Item.Status:=toByte(xNode,XML.Fields.Status);
      Item.Location:=toDouble(xNode,XML.Fields.Location);
      Item.Duration:=toDouble(xNode,XML.Fields.Duration);
      Item.Time:=toDouble(xNode,XML.Fields.Time);
    end;
    if xPeople<>nil then begin
      for iLcv:=0 to xPeople.ChildNodes.Count-1 do
        Core.Arrays.VarString.Add(@Item.People,xPeople.ChildNodes[iLcv].TextContent,[]);
    end;
    Result:=True;
  end;
end;

end.


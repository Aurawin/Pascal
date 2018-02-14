unit Storage.CMS;

{
  unit dbmCMS.pas

  CMS Storage Database Module

  DBMS facilities to handle Content Management System

  Copyright Aurawin LLC 2014
  Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  RSR,
  RSR.HTTP,

  Core.Database,
  Core.Database.Types,

  Core.Timer,
  Core.XML,

  Encryption.Base64,

  Multimedia.Image,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Core.Strings,
  Core.Streams,

  Storage,
  Storage.Main,
  Storage.ContentTypes,

  XMLRead,
  DOM,
  SysUtils;

type
  Items = class
  type
    XML=class
    type
      Stanzas=class
      const
        PagePoint              = 'pp';
      end;
      Fields=class
      const
        FileID                 = 'fild';
        FolderID               = 'fold';
        URI                    = 'uri';
        Space                  = 'spc';
        Data                   = 'data';
      end;
    end;
    PPagePoint=^TPagePoint;
    TPagePoint=record
      FileID                   : QWord;
      FolderID                 : QWord;
      URI                      : Core.Strings.VarString;
      Space                    : Core.Strings.VarString;
      Data                     : Core.Strings.VarString;
    end;
    class function  toXML(var Item:TPagePoint; Output:TMemoryStream; Header:boolean):boolean;
    class function  fromXML(xDoc:TXMLDocument; var Item:TPagePoint):boolean; overload;

    class procedure Empty(Var Item:TPagePoint); overload;
    class procedure Init(Var Item:TPagePoint); overload;
    class procedure Done(Var Item:TPagePoint); overload;
  end;

implementation
uses DB, sqldb;

class function  Items.fromXML(xDoc:TXMLDocument; var Item:TPagePoint):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  Empty(Item);
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.PagePoint);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.FileID:=toQword(xItem,XML.Fields.FileID);
      Item.FolderID:=toQword(xItem,XML.Fields.FolderID);
      Item.Space:=toString(xItem,XML.Fields.Space);
      Item.URI:=toString(xItem,XML.Fields.URI);
      Item.Data:=toString(xItem,XML.Fields.Data);
      Result:=True;
    end;
  end;
end;

class function  Items.toXML(var Item:TPagePoint; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=False;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Output.Position:=Output.Size;
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.PagePoint,Output);
  Core.Streams.Write('>',1,Output);

  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.FileID,Item.FileID),Output);
    Core.Streams.Write(Print(XML.Fields.FolderID,Item.FolderID),Output);
    Core.Streams.Write(Print(XML.Fields.Space,Item.Space,CDATA_OFF),Output);
    Core.Streams.Write(Print(XML.Fields.URI,Item.URI,CDATA_ON),Output);
    Core.Streams.Write(Print(XML.Fields.Data,Item.Data,CDATA_OFF),Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.PagePoint,Output);
  Core.Streams.Write('>',1,Output);

  Result:=True;
end;

class procedure Items.Empty(Var Item:TPagePoint);
begin
  Item.FileID:=0;
  Item.FolderID:=0;
  SetLength(Item.Space,0);
  SetLength(Item.URI,0);
  SetLength(Item.Data,0);
end;

class procedure Items.Init(Var Item:TPagePoint);
begin
  Item.FileID:=0;
  Item.FolderID:=0;
  SetLength(Item.Space,0);
  SetLength(Item.URI,0);
  SetLength(Item.Data,0);
end;

class procedure Items.Done(Var Item:TPagePoint);
begin
  Finalize(Item.Space);
  Finalize(Item.URI);
  Finalize(Item.Data);
  Finalize(Item);
end;

end.


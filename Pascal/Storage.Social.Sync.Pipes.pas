unit Storage.Social.Sync.Pipes;


interface

uses
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
  Storage.Main,


  DOM,
  MD5,
  XMLRead,
  Classes,
  SysUtils;

Type
  XML=class
  const
    Items                    : Core.Database.Types.VarString = 'items';
    Item                     : Core.Database.Types.VarString = 'item';
    Name                     : Core.Database.Types.VarString = 'name';
    Path                     : Core.Database.Types.VarString = 'path';
    Direction                : Core.Database.Types.VarString = 'direction';
  end;
  Direction=class
  const
    Off                      =       0;
    Download                 = 1 shl 0;
    Upload                   = 1 shl 1;
    Both                     = Download or Upload;
  end;
  PItem=^TItem;
  PItems=^TItems;
  TItem=record
    Direction                : Byte;
    Name                     : Core.Strings.VarString;
    Path                     : Core.Strings.VarString;
    // Runtime Variable
    scannerUpload            : TThread;
    scannerDownload          : TThread;
  end;
  TItems=Array of PItem;

  function  fromXML(xItem:TDOMNode; var Item:TItem):boolean; overload;
  function  fromXML(xItems:TDOMNode; var Items:TItems):boolean; overload;
  function  fromXML(xDoc:TXMLDocument; var Items:TItems):boolean; overload;

  function  toXML(var Item:TItem; Output:TMemoryStream; Header:boolean):boolean; overload;
  function  toXML(var Items:TItems; Output:TMemoryStream; Header:boolean):boolean; overload;

  function  getItem(Name:Core.Strings.VarString; var Items:TItems):PItem;
  function  Force(Name:Core.Strings.VarString; var Items:TItems):PItem;

  procedure VerifyDefaults(var Items:TItems);

  procedure Copy(Var Source,Dest:TItem);

  procedure Empty(Var Item:TItem); overload;
  procedure Empty(Var Items:TItems); overload;

  procedure Done(Var Items:TItems); overload;
  procedure Done(Var Item:TItem); overload;

  procedure Init(var Items:TItems); overload;
  procedure Init(var Item:TItem); overload;

  function  Add(Name,Path:Core.Strings.VarString; aDirection,aMode:Byte; var Items:TItems):PItem;
  function  IndexOf(ItemP:PItem; var Items:TItems): LongInt;


implementation
uses
  Storage.Social.Folders;

procedure Init(var Item:TItem);
begin
  (*
  Item.Verified:=false;
  Item.ManifestLoaded:=false;
  Item.FoldersLoaded:=false;
  Item.FilesLoaded:=false;


  Item.Manifest:=nil;

  Item.NetworkP:=nil;
  Item.ConnectionP:=nil;

  Folders.Init(Item.Folders);
  *)
  Item.scannerUpload:=nil;
  Item.scannerDownload:=nil;
  Item.Direction:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Path,0);
end;

procedure Init(var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    Done(Items[iLcv]^);
    Dispose(Items[iLcv]);
  end;
  SetLength(Items,0);
end;

procedure Copy(Var Source,Dest:TItem);
begin
  Dest.Direction:=Source.Direction;
  Dest.Name:=Source.Name;
  Dest.Path:=Source.Path;
  //Dest.Manifest:=Source.Manifest;
  //Folders.Empty(Dest.Folders);
end;

procedure Empty(Var Item:TItem);
begin
  (*
  Item.Verified:=false;
  Item.ManifestLoaded:=false;
  Item.FoldersLoaded:=false;
  Item.FilesLoaded:=false;

  Item.NetworkP:=nil;
  Item.ConnectionP:=nil;

  Item.Manifest:=nil;
  Folders.Empty(Item.Folders);
  *)
  Item.scannerUpload:=nil;
  Item.scannerDownload:=nil;
  Item.Direction:=0;
  SetLength(Item.Name,0);
  SetLength(Item.Path,0);
end;

function  IndexOf(ItemP:PItem; var Items:TItems): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if (Items[iLcv]=ItemP) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

procedure Empty(Var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    Done(Items[iLcv]^);
    Dispose(Items[iLcv]);
  end;
  SetLength(Items,0);
end;

procedure Done(Var Item:TItem);
begin
  with Item do begin
    Finalize(Name);
    Finalize(Path);
  end;
  Finalize(Item);
end;

procedure Done(Var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    Done(Items[iLcv]^);
    Dispose(Items[iLcv]);
  end;
  Finalize(Items);
end;

function  fromXML(xItem:TDOMNode; var Item:TItem):boolean;
begin
  Result:=False;
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.Direction:=toByte(xItem,XML.Direction);
      Item.Path:=toString(xItem,XML.Path);
      Item.Name:=toString(xItem,XML.Name);
      Result:=True;
    end;
  end;
end;

function  fromXML(xItems:TDOMNode; var Items:TItems):boolean;
var
  xItem:TDOMNode;
  xName:TDOMNode;
  iLcv,iCt:LongInt;
  itmP:PItem;
begin
  Result:=False; iCt:=0;
  if (xItems<>nil) then begin
    with Core.XML.DB do begin
      for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
        xItem:=xItems.ChildNodes[iLcv];
        if SameText(xItem.NodeName,XML.Item) then begin
          xName:=Core.XML.DB.getChildNode(xItem,XML.Name);
          if (xName<>nil) then begin
            itmP:=getItem(xName.NodeValue,Items);
            if (itmP=nil) then begin
              new(itmP);
              Init(itmP^);
              SetLength(Items,iCt+1);
              Items[iCt]:=itmP;
              Inc(iCt);
            end;
            fromXML(xItem,itmP^);
            Result:=True;
          end;
        end;
      end;
    end;
  end;
end;

function  fromXML(xDoc:TXMLDocument; var Items:TItems):boolean;
begin
  Result:=fromXML(Core.XML.DB.getNode(xDoc,XML.Items),Items);
end;

function  toXML(var Item:TItem; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Item,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Direction,Item.Direction),Output);
    Core.Streams.Write(Print(XML.Name,Item.Name),Output);
    Core.Streams.Write(Print(XML.Path,Item.Path),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=true;
end;

function  toXML(var Items:TItems; Output:TMemoryStream; Header:boolean):boolean;
var
  iLcv:LongInt;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Items,Output);
  Core.Streams.Write('>',1,Output);
  for iLcv:=0 to High(Items) do
    toXML(Items[iLcv]^,Output,XML_HEADER_OFF);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Items,Output);
  Core.Streams.Write('>',1,Output);
  Result:=true;
end;

function  Add(Name,Path:Core.Strings.VarString; aDirection,aMode:Byte; var Items:TItems):PItem;
var
  iCt:LongInt;
  itmP:PItem;
begin
  iCt:=System.Length(Items);
  New(itmP);
  Init(itmP^);
  SetLength(Items,iCt+1);
  Items[iCt]:=itmP;
  itmP^.Name:=Name;
  itmP^.Path:=Path;
  itmP^.Direction:=aDirection;
  Result:=itmP;
end;

function  getItem(Name:Core.Strings.VarString; var Items:TItems):PItem;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Items) do begin
    if SameText(Name,Items[iLcv]^.Name) then begin
      Result:=Items[iLcv];
      Break;
    end;
  end;
end;

function  Force(Name:Core.Strings.VarString; var Items:TItems):PItem;
var
  itmP:PItem;
begin
  Result:=nil;
  itmP:=getItem(Name,Items);
  if itmP=nil then
    itmP:=Add(Name,'',0,0,Items);
  Result:=itmP;
end;

procedure VerifyDefaults(var Items:TItems);
begin
  Force(Defaults.Documents,Items);
  Force(Defaults.Music,Items);
  Force(Defaults.Pictures,Items);
  Force(Defaults.Videos,Items);
end;

end.


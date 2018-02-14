{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit hHTTPd;


interface
uses
  Classes,

  RSR,

  Core.Timer,

  Storage,
  Storage.Main,

  Encryption.Base64,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  App.Build,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.Boolean,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Strings,

  Core.Streams,
  RSR.HTTP,
  Core.Utils.Time,
  Core.XML,
  Storage.UserAccounts,
  Storage.CoreObjects,
  Storage.FAT,


  Sockets,
  HTTPDefs,
  DOM,
  XMLRead,
  md5,
  sha1;
Type
  THTTP=record
    WebSocket                    : Boolean;
    UAP                          : Storage.UserAccounts.Items.PItem;    // A user is authenticated here...
    AuthCount                    : LongInt;
    LastAuth                     : double;
    Media                        : PHTTPMedia;
    CoreData                     : PCoreData;
    Auth                         : Core.Strings.VarString;
    Account                      : Core.Strings.VarString;
  end;
  PHTTP=^THTTP;
  TFileData=record
    Name                         : Core.Strings.VarString;
    HasKeywords                  : Boolean;
    Data                         : Core.Arrays.Types.Bytes;
    FileAge                      : LongInt;
    FileSize                     : LongInt;
    RFCTime                      : Core.Strings.VarString;
  end;
  TFileArray=Array of TFileData;
  TTypeArray=Array of TBooleanArray;  // HTML Data=True False=DataFile
  Proxy=class
  Type
    XML=class
      const
        Stanza                 : Core.Strings.VarString = 'p';
      type
        Fields=class
        const
          Secure               : Core.Strings.VarString = 's';
          URL                  : Core.Strings.VarString = 'u';
          Data                 : Core.Strings.VarString = 'd';
        end;
    end;
    TGet=record
      Secure : Boolean;
      Host   : Core.Strings.VarString;
      URL    : Core.Strings.VarString;
      Data   : Core.Strings.VarString; // place response here
    end;
    class procedure Empty(Var Item:TGet); overload;
    class procedure Done(Var Item:TGet); overload;
    class procedure Init(Var Item:TGet); overload;

    class function  toXML(var Item:TGet):Core.Strings.VarString; overload;
    class function fromXML(xDoc:TXMLDocument; var Item:TGet):boolean; overload;
  end;

  procedure Init(var Item:THTTP); overload;
  procedure Empty(Var Item:THTTP); overload;
  procedure Done(Var Item:THTTP); overload;

  function  ErrorPage(const sTitle, sDetail, sDescription: Core.Strings.VarString): Core.Strings.VarString;

var
  Server_Header                  : Core.Strings.VarString;


implementation
uses SysUtils,DateUtils;

procedure Init(var Item:THTTP);
begin
  Item.UAP:=nil;
  Item.WebSocket:=False;
  Item.AuthCount:=0;
  Item.LastAuth:=0.0;
  Item.Media:=nil;
  Item.CoreData:=nil;
  SetLength(Item.Auth,0);
  SetLength(Item.Account,0);
end;

procedure Empty(Var Item:THTTP);
begin
  Item.UAP:=Nil;
  Item.WebSocket:=False;
  Item.AuthCount:=0;
  Item.LastAuth:=0.0;
  if Item.Media<>nil then
    Empty(Item.Media^);
  if Item.CoreData<>nil then
    Empty(Item.CoreData^);
  Empty(Item.Auth);
  Empty(Item.Account);
end;

procedure Done(Var Item:THTTP);
begin
  Item.UAP:=Nil;
  Done(Item.Auth);
  Done(Item.Account);
  if Item.Media<>nil then begin
    Done(Item.Media^);
    Dispose(Item.Media);
    Item.Media:=nil;
  end;
  if Item.CoreData<>nil then begin
    Done(Item.CoreData^);
    Dispose(Item.CoreData);
    Item.CoreData:=nil;
  end;
  Finalize(Item);
end;

function  ErrorPage(const sTitle, sDetail, sDescription: Core.Strings.VarString): Core.Strings.VarString;
begin
  Result :=Concat(
    '<html><head>'#13#10,
      '<title>',sTitle,'</title>'#13#10,
    '</head><body>',#13#10,
      '<h1>',sTitle,'</h1>',#13#10,
      '<b>',sDetail,'</b><p><pre>',sDescription,'</pre></p>'#13#10,
      '<hr>',#13#10,
      '<font size="2">Generated ',Core.Utils.Time.TimeZoneTime,' by ',Server_Header,'.</font>'#13#10,
    '</body></html>'
  );

end;

class procedure Proxy.Empty(Var Item:TGet);
begin
  Item.Secure:=false;
  SetLength(Item.Data,0);
  SetLength(Item.URL,0);
end;

class procedure Proxy.Done(Var Item:TGet);
begin
  Finalize(Item.Data);
  Finalize(Item.URL);
  Finalize(Item);
end;

class procedure Proxy.Init(Var Item:TGet);
begin
  Item.Secure:=false;
  SetLength(Item.Data,0);
  SetLength(Item.URL,0);
end;

class function  Proxy.fromXML(xDoc:TXMLDocument; var Item:TGet):boolean;
var
  xNode:TDOMNode;
begin
  Result:=false;
  Empty(Item);
  with Core.XML.DB do begin
    xNode:=getNode(xDOC,XML.Stanza);
    if (xNode<>nil) then begin
      with Item do begin
        Secure                 := toBoolean(xNode,XML.Fields.Secure);
        URL                    := toString(xNode,XML.Fields.URL);
        Data                   := toString(xNode,XML.Fields.Data);
      end;
      Result:=true;
    end;
  end;
end;

class function Proxy.toXML(var Item:TGet):Core.Strings.VarString;
begin
  SetLength(Result,0);
  with Core.XML.DB do begin
    Result:=Concat(
      Print(XML.Fields.Secure,Item.Secure),
      Print(XML.Fields.URL,Item.URL),
      Print(XML.Fields.Data,Item.Data,CDATA_OFF)
    );
  end;
end;

initialization
  Server_Header:=Concat(App.Build.Title,' ',App.Build.Edition,' Version ',App.Build.Version,' RSR Build ',IntToStr(RSR_Build_Number));


end.

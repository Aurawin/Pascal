{
 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}


unit Core.XML;

interface

uses Classes,SysUtils,


  Core.Strings,
  Core.Streams,
  Core.Logging,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeInt,
  Core.Arrays.LargeWord,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Bytes,

  Encryption.Base64,

  MD5,DOM, XMLRead;
Type
  TStanzaAddMode=(tsmAdd,tsmInsert,tsmDontAdd);
  TTagKind=(tStanza,tNormal);
  PXMLStanza=^TXMLStanza;
  PXMLTag=^TXMLTag;

  TXMLTag=record
    Name                         : Core.Strings.VarString;
    Value                        : Core.Strings.VarString;
    Parameters                   : Core.Arrays.Types.KeyStrings;
    Parent                       : PXMLTag;
  end;
  TXMLTags=record
    Parent                       : PXMLTag;
    Items                        : Array of PXMLTag;
  end;
  TXMLStanza=Record
    Data                         : Core.Strings.VarString;
    Root                         : TXMLTag;
    Tags                         : TXMLTags;
  end;
  TXMLStanzas=Array of PXMLStanza;
  TXMLParser=Class(TObject)
  private
    AddMode                      : TStanzaAddMode;
    sDebugData                   : Core.Strings.VarString;
    sData                        : Core.Strings.VarString;
    iStanzaLcv                   : LongInt;
    iTagLcv                      : LongInt;
    FTag                         : PXMLTag;
  private
    procedure  Parse_Tag(Kind:TTagKind);
    procedure  DecodeTagParameters(Tag:Core.Strings.VarString; Var XMLTag:TXMLTag);
    Function   ParseStanzas:LongInt;
    procedure  Perform_Parse;
    Function   ValueParse(Value:Core.Strings.VarString):Core.Strings.VarString;
  public
    Stanzas                      : TXMLStanzas;
  public
    EndTags                      : Core.Arrays.Types.VarString;
    StanzaBreaks                 : Core.Arrays.Types.VarString;
    ReplaceValues                : Core.Arrays.Types.KeyStrings;
  public
    Function   Parameter(Const Index:LongInt; sParameter:Core.Strings.VarString):Core.Strings.VarString;
    Function   StanzaIndex(sTag:Core.Strings.VarString): LongInt;
    Function   getStanza(sTag:Core.Strings.VarString):PXMLStanza;
    // Working with Tags
    Function   getTag(sName:Core.Strings.VarString; var Tags:TXMLTags):PXMLTag;
    Function   setTag(sName:Core.Strings.VarString; var Tags:TXMLTags):PXMLTag;
  public
    function   getValueAsString(Tag:PXMLTag):Core.Strings.VarString; overload;
    function   getValueAsInteger(Tag:PXMLTag): LongInt; overload;
    function   getValueAsDouble(Tag:PXMLTag):Double; overload;
    function   getValueAsInt64(Tag:PXMLTag):Int64; overload;
    procedure  getValueAsInt64Array(Tag:PXMLTag; var List:Core.Arrays.Types.LargeInt); overload;
  public
    function   getValueAsString(var Tags:TXMLTags; Name:Core.Strings.VarString):Core.Strings.VarString; overload;
    function   getValueAsInteger(var Tags:TXMLTags; Name:Core.Strings.VarString): LongInt; overload;
    function   getValueAsDouble(var Tags:TXMLTags; Name:Core.Strings.VarString):Double; overload;
    function   getValueAsInt64(var Tags:TXMLTags; Name:Core.Strings.VarString):Int64; overload;
    procedure  getValueAsInt64Array(var Tags:TXMLTags; Name:Core.Strings.VarString; var List:Core.Arrays.Types.LargeInt); overload;
  public
    Function   Tag(Const Index,TagIndexStart,Depth:LongInt; sTag:Core.Strings.VarString):Core.Strings.VarString; overload;
    Function   Tag(Const Index:LongInt; sTag:Core.Strings.VarString):Core.Strings.VarString; overload;
    Function   Tag(Const Index,TagIndex:LongInt):Core.Strings.VarString; overload;
  public
    Function   TagIndex(Const Index:LongInt; sTag:Core.Strings.VarString): LongInt; overload;
    Function   TagIndex(Const Index:LongInt; sTag,sParameter,sParamValue:Core.Strings.VarString): LongInt; overload;
  public
    Function   TagParameter(Const Index:LongInt; sTag,sParameter:Core.Strings.VarString):Core.Strings.VarString; overload;
    Function   TagParameter(Const Index,iTagIndex:LongInt; sParameter:Core.Strings.VarString):Core.Strings.VarString; overload;
    procedure  TagParameters(var List:Core.Arrays.Types.VarString; Const Index:LongInt; sTag,sParameter:Core.Strings.VarString);
  public
    procedure  Tags(Var List:Core.Arrays.Types.VarString; Const Index:LongInt; sTag:Core.Strings.VarString);
  public
    procedure  Clear;
    procedure  Parse(var Data:Core.Strings.VarString);
  public
    constructor Create; reIntroduce;
    Destructor  Destroy; override;
  end;

  const
    XML_HEADER_ON                          = true;
    XML_HEADER_OFF                         = false;

    XML_STANZA_ON                          = true;
    XML_STANZA_OFF                         = false;

  type
    DB=class
    const
      CDATA_OFF : boolean = false;
      CDATA_ON  : boolean = true;
    type
      class procedure Stamp(Encoding:Core.Strings.VarString; Stream:TStream);
      class function Header(var Encoding:Core.Strings.VarString):Core.Strings.VarString;

      class function  IsInvalid(var Value:Byte):boolean;
      class procedure Prepare(var sInput:Core.Strings.VarString; Output:TStringStream); overload;
      class function  Prepare(var sInput:Core.Strings.VarString; Refactor:TStream):Core.Strings.VarString; overload;
      class function  Extract(var Tag:Core.Strings.VarString; var Data:Core.Strings.VarString):Core.Strings.VarString; overload;

      class function toInt64(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Int64=0):Int64;
      class function toQword(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Qword=0):Qword;
      class function toWord(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Word=0):Word;
      class function toString(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Core.Strings.VarString=''):Core.Strings.VarString;
      class function toXML(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Core.Strings.VarString=''):Core.Strings.VarString; overload;
      class function toXML(Node:TDOMNode; aDefault:Core.Strings.VarString=''):Core.Strings.VarString; overload;
      class function toBoolean(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:boolean=false):boolean;
      class function toDouble(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:double=0.0):Double;
      class function toSingle(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:single=0.0):Single;

      class function toInteger(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:LongInt=0): LongInt;
      class function toByte(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:byte=0):byte;
      class procedure toInt64Array(Node:TDOMNode; Name:Core.Strings.VarString; Var Value:Core.Arrays.Types.LargeInt);
      class procedure toQWordArray(Node:TDOMNode; Name:Core.Strings.VarString; Var Value:Core.Arrays.Types.LargeWord);
      class procedure toByteArray(Node:TDOMNode; Name:Core.Strings.VarString; var Value:Core.Arrays.Types.Bytes);
      class procedure toMD5Digest(Node:TDOMNode; Name:Core.Strings.VarString; var Value:TMD5Digest);

      class function Print(Name:Core.Strings.VarString; var Value:Core.Strings.VarString; const CDATA:boolean=true):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:shortstring; const CDATA:boolean=true):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:shortstring):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; Value:byte):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:word):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:LongInt):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:DWord):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Int64):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Qword):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Double):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Single):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Boolean):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:TMD5Digest):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeInt):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeWord):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.VarString):Core.Strings.VarString; overload;
      class function Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.KeyStrings):Core.Strings.VarString; overload;

      class procedure Stream(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.Bytes; Output,Refactor:TStream); overload;

      class function getNode(xDoc:TXMLDocument; Name:Core.Strings.VarString):TDOMNode;
      class function getChildNode(xNode:TDOMNode; Name:Core.Strings.VarString):TDOMNode;
      class function getNodeValue(xNode:TDOMNode):Core.Strings.VarString;
      class function getNodeWrapped(xNode:TDOMNode):Core.Strings.VarString;
      class function getNodeText(xNode:TDOMNode):Core.Strings.VarString;
      class function getNodeTextWrapped(xNode:TDOMNode):Core.Strings.VarString;
      class function wrapCDATA(const sData:Core.Strings.VarString):Core.Strings.VarString;
      class procedure Wrap(DocType,Tag:Core.Strings.VarString; var Data:Core.Strings.VarString);
      class function Insert(var Entry,Data:Core.Strings.VarString; Tag:Core.Strings.VarString):boolean;
    end;


  Function  XMLStanzaToString(Var Stanza:TXMLStanza):Core.Strings.VarString;

  procedure Empty(Var Item:TXMLStanzas); overload;
  procedure Empty(Var Item:TXMLStanza); overload;
  procedure Empty(Var Item:TXMLTags); overload;
  procedure Empty(Var Item:TXMLTag); overload;
  
  procedure Copy(Var Source,Destination:TXMLStanza); overload;
  procedure Copy(Var Source,Destination:TXMLTags); overload;
  procedure Copy(Var Source,Destination:TXMLTag); overload;

  procedure Done(Var Item:TXMLTag); overload;
  procedure Done(Var Item:TXMLTags); overload;
  procedure Done(Var Item:TXMLStanza); overload;

  procedure Init(Var Item:TXMLTag; Parent:PXMLTag); overload;
  procedure Init(Var Item:TXMLTags; Parent:PXMLTag); overload;
  procedure Init(Var Item:TXMLStanza); overload;

  function Print(Name:Core.Strings.VarString; var Value:Core.Strings.VarString):Core.Strings.VarString; overload;
  function Print(Name:Core.Strings.VarString; var Value:shortstring):Core.Strings.VarString; overload;
  function Print(Name:Core.Strings.VarString; var Value:LongInt):Core.Strings.VarString; overload;
  function Print(Name:Core.Strings.VarString; var Value:Int64):Core.Strings.VarString; overload;
  function Print(Name:Core.Strings.VarString; var Value:Double):Core.Strings.VarString; overload;
  function Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeInt):Core.Strings.VarString; overload;

  function  SetSize(Var Item:TXMLTags; Count:LongInt): LongInt; overload;

  function  Add(Var Item:TXMLStanzas): LongInt;
  function  Add(Var Item:TXMLTags): LongInt;

implementation
uses StrUtils;

procedure Init(Var Item:TXMLStanza);
begin
  SetLength(Item.Data,0);
  Init(Item.Root,nil);
  Init(Item.Tags,@Item.Root);
end;

procedure Init(Var Item:TXMLTag; Parent:PXMLTag);
begin
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
  Empty(Item.Parameters);
  Item.Parent:=Parent;
end;

procedure Init(Var Item:TXMLTags; Parent:PXMLTag);
begin
  Item.Parent:=Parent;
  SetLength(Item.Items,0);
end;

procedure Copy(Var Source,Destination:TXMLStanza);
begin
  Destination.Data:=Source.Data;
  Copy(Source.Tags,Destination.Tags);
end;

procedure Copy(Var Source,Destination:TXMLTags);
var
  iLength:LongInt;
  iLcv:LongInt;
begin
  iLength:=Length(Source.Items);
  SetSize(Destination,iLength);
  For iLcv:=0 to iLength-1 do
    Copy(Source.Items[iLcv]^,Destination.Items[iLcv]^);
end;

function SetSize(Var Item:TXMLTags; Count:LongInt): LongInt;
var
  iLcv:LongInt;
begin
  Result:=Count;
  // Check to Shrink
  iLcv:=System.Length(Item.Items);
  While (iLcv>Count) do begin
    Done(Item.Items[iLcv-1]^);
    Dispose(Item.Items[iLcv-1]);
    Dec(iLcv);
  end;
  SetLength(Item.Items,Count);
  // Check to fill grown items
  While (iLcv<Count) do begin
    New(Item.Items[iLcv]);
    Init(Item.Items[iLcv]^,Item.Parent);
    Inc(iLcv);
  end;
end;

procedure Copy(Var Source,Destination:TXMLTag);
begin
  Destination.Name:=Source.Name;
  Destination.Value:=Source.Value;
  Core.Arrays.KeyString.Copy(Source.Parameters,Destination.Parameters);
end;

function  Add(Var Item:TXMLStanzas): LongInt;
var
  iIndex:LongInt;
begin
  iIndex:=Length(Item);
  SetLength(Item,iIndex+1);
  New(Item[iIndex]);
  Init(Item[iIndex]^);
  Result:=iIndex;
end;

function  Add(Var Item:TXMLTags): LongInt;
var
  iIndex:LongInt;
begin
  iIndex:=Length(Item.Items);
  SetLength(Item.Items,iIndex+1);
  New(Item.Items[iIndex]);
  Init(Item.Items[iIndex]^,Item.Parent);
  Result:=iIndex;
end;

procedure Empty(Var Item:TXMLStanzas);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Empty(Var Item:TXMLStanza);
begin
  SetLength(Item.Data,0);
  Empty(Item.Tags);
end;

procedure Empty(Var Item:TXMLTags);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item.Items) do begin
    Done(Item.Items[iLcv]^);
    Dispose(Item.Items[iLcv]);
  end;
  SetLength(Item.Items,0);
end;

procedure Empty(Var Item:TXMLTag);
begin
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
  Empty(Item.Parameters);
end;

procedure Done(Var Item:TXMLTag);
begin
  Finalize(Item.Name);
  Finalize(Item.Value);
  Done(Item.Parameters);
  Finalize(Item);
end;

procedure Done(var Item:TXMLStanza);
begin
  Finalize(Item.Data);
  Done(Item.Root);
  Done(Item.Tags);
  Finalize(Item);
end;

procedure Done(var Item:TXMLTags);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to high(Item.Items) do begin
    Done(Item.Items[iLcv]^);
    Dispose(Item.Items[iLcv]);
  end;
  Finalize(Item.Items);
  Finalize(Item);
end;

function Print(Name:Core.Strings.VarString; Var Value:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',Value,'</',Name,'>');
end;

function Print(Name:Core.Strings.VarString; Var Value:shortstring):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',Value,'</',Name,'>');
end;

function Print(Name:Core.Strings.VarString; Var Value:LongInt):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

function Print(Name:Core.Strings.VarString; Var Value:Int64):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

function Print(Name:Core.Strings.VarString; Var Value:Double):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',FloatToStr(Value),'</',Name,'>');
end;

function Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeInt):Core.Strings.VarString;
begin
  Core.Arrays.LargeInt.toString(Value,',',Result);
  Result:=Concat('<',Name,'>', Result,'</',Name,'>');
end;

Function  XMLStanzaToString(Var Stanza:TXMLStanza):Core.Strings.VarString;
begin
  Result:=Concat('Core.XML.DBStanzaToString (NOT IMPLEMENTED)');
end;

Constructor TXMLParser.Create;
begin
  iStanzaLcv:=0;
  iTagLcv:=0;
  Inherited Create;
end;

Destructor TXMLParser.Destroy;
begin
  Clear;
  Empty(StanzaBreaks);
  Empty(ReplaceValues);
  Empty(EndTags);
  Inherited Destroy;
end;

procedure  TXMLParser.Clear;
Var
  ijLcv,iLcv:LongInt;
begin
  Empty(sData);
  Empty(sDebugData);
  Empty(Stanzas);
  iTagLcv:=0;
  iStanzaLcv:=-1;
  AddMode:=tsmAdd;
end;

Function   TXMLParser.ValueParse(Value:Core.Strings.VarString):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  Result:=Value;
  For iLcv:=0 to High(ReplaceValues) do
    Result:=StringReplace(Result,ReplaceValues[iLcv]^.Key,ReplaceValues[iLcv]^.Value,[rfReplaceAll]);
end;

Function   TXMLParser.Parameter(Const Index:LongInt; sParameter:Core.Strings.VarString):Core.Strings.VarString;
var
  iResult,iLcv:LongInt;
begin
  Result:=''; iLcv:=0; iResult:=-1;
  If (Length(Stanzas[Index]^.Tags.Items)>0) Then  begin
    // This is the one.  Loop through parameters.
    While (iLcv<Length(Stanzas[Index]^.Tags.Items[0]^.Parameters)) and (iResult=-1) do begin
      If Stanzas[Index]^.Tags.Items[0]^.Parameters[iLcv]^.Key=sParameter then begin
        Result:=Stanzas[Index]^.Tags.Items[0]^.Parameters[iLcv]^.Value;
        iResult:=iLcv;
      end;
      Inc(iLcv);
    end;
  end;
end;

Function   TXMLParser.getStanza(sTag:Core.Strings.VarString):PXMLStanza;
var
  iLcv:LongInt;
begin
  Result:=nil;
  For iLcv:=0 to High(Stanzas) do begin
    If SameText(Stanzas[iLcv]^.Root.Name,sTag) then begin
      Result:=Stanzas[iLcv];
      Break;
    end;
  end;
end;

procedure  TXMLParser.Tags(var List:Core.Arrays.Types.VarString; Const Index:LongInt; sTag:Core.Strings.VarString);
var
  jLcv,iLcv:LongInt;
begin
  SetLength(List,0);
  For iLcv:=0 to High(Stanzas[Index]^.Tags.Items) do begin
    If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Name,sTag) then
        Core.Arrays.VarString.Add(List,Stanzas[Index]^.Tags.Items[iLcv]^.Value);
  end;
end;

procedure  TXMLParser.TagParameters(Var List:Core.Arrays.Types.VarString; Const Index:LongInt; sTag,sParameter:Core.Strings.VarString);
var
  jLcv,iLcv:LongInt;
begin
  SetLength(List,0);
  For iLcv:=0 to High(Stanzas[Index]^.Tags.Items) do begin
    For jLcv:=0 to High(Stanzas[Index]^.Tags.Items[iLcv]^.Parameters) do
      If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Parameters[jLcv]^.Key,sParameter) then
        Core.Arrays.VarString.Add(List,Stanzas[Index]^.Tags.Items[iLcv]^.Parameters[jLcv]^.Value);
  end;
end;

Function   TXMLParser.TagParameter(Const Index,iTagIndex:LongInt; sParameter:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Core.Arrays.KeyString.GetItemByKey(Stanzas[Index]^.Tags.Items[iTagIndex]^.Parameters,sParameter);
end;

Function   TXMLParser.TagParameter(Const Index:LongInt; sTag,sParameter:Core.Strings.VarString):Core.Strings.VarString;
var
  iResult,jLcv,iLcv:LongInt;
  iTagCount:LongInt;
  iParamCount:LongInt;
begin
  Result:=''; jLcv:=0; iLcv:=0; iResult:=-1;
  iTagCount:=System.Length(Stanzas[Index]^.Tags.Items);
  While (iLcv<iTagCount) and (iResult=-1) do begin
    If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Name,sTag) then begin
      // This is the one.  Loop through parameters.
      with Stanzas[Index]^.Tags.Items[iLcv]^ do begin
        iParamCount:=System.Length(Parameters);
        While (jLcv<iParamCount) and (iResult=-1) do begin
          If Parameters[jLcv]^.Key=sParameter then begin
            Result:=Parameters[jLcv]^.Value;
            iResult:=jLcv;
          end;
          Inc(jLcv);
        end;
      end;
    end;
    Inc(iLcv);
  end;
end;

Function   TXMLParser.TagIndex(Const Index:LongInt; sTag,sParameter,sParamValue:Core.Strings.VarString): LongInt;
var
  iResult,jLcv,iLcv:LongInt;
  iTagCount:LongInt;
  iParamCount:LongInt;
begin
  Result:=-1; jLcv:=0; iLcv:=0; iResult:=-1;
  iTagCount:=System.Length(Stanzas[Index]^.Tags.Items);
  While (iLcv<iTagCount) and (iResult=-1) do begin
    If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Name,sTag) then begin
      // This is the one.  Loop through parameters.
      With Stanzas[Index]^.Tags.Items[iLcv]^ do begin
        iParamCount:=System.Length(Parameters);
        While (jLcv<iParamCount) and (iResult=-1) do begin
          If SameText(Parameters[jLcv]^.Key,sParameter) and SameText(Parameters[jLcv]^.Value,sParamValue) then begin
            Result:=iLcv;
            iResult:=jLcv;
          end;
          Inc(jLcv);
        end;
      end;
    end;
    Inc(iLcv);
  end;
end;



Function   TXMLParser.TagIndex(Const Index:LongInt; sTag:Core.Strings.VarString): LongInt;
var
  iLcv:LongInt;
  iTagCount:LongInt;
begin
  iLcv:=0; Result:=-1;
  iTagCount:=Length(Stanzas[Index]^.Tags.Items);
  While (iLcv<iTagCount) and (Result=-1) do begin
    If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Name,sTag) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function   TXMLParser.Tag(Const Index,TagIndex:LongInt):Core.Strings.VarString;
begin
  Result:='';
  If TagIndex<Length(Stanzas[Index]^.Tags.Items) then
    Result:=Stanzas[Index]^.Tags.Items[TagIndex]^.Value;
end;

Function   TXMLParser.getTag(sName:Core.Strings.VarString; var Tags:TXMLTags):PXMLTag;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(Tags.Items) do begin
    If SameText(Tags.Items[iLcv]^.Name,sName) then begin
      Result:=Tags.Items[iLcv];
      Break;
    end;
  end;
end;

Function   TXMLParser.setTag(sName:Core.Strings.VarString; var Tags:TXMLTags):PXMLTag;
begin
  Result:=getTag(sName,Tags);
  FTag:=Result;
end;

Function   TXMLParser.Tag(Const Index,TagIndexStart,Depth:LongInt; sTag:Core.Strings.VarString):Core.Strings.VarString;
var
  iDepthLcv,iResult,iLcv:LongInt;
  iTagCount:LongInt;
begin
  iDepthLcv:=0; iLcv:=TagIndexStart; iResult:=-1; Result:='';
  iTagCount:=Length(Stanzas[Index]^.Tags.Items);
  While (iLcv<iTagCount) and (iResult=-1) and (iDepthLcv<Depth) do begin
    If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Name ,sTag ) then begin
      Result:=Stanzas[Index]^.Tags.Items[iLcv]^.Value;
      iResult:=iLcv;
    end;
    Inc(iLcv);
    Inc(iDepthLcv);
  end;
end;

function   TXMLParser.getValueAsString(Tag:PXMLTag):Core.Strings.VarString;
begin
  Result:=Tag^.Value;
end;

function   TXMLParser.getValueAsInteger(Tag:PXMLTag): LongInt;
begin
  Result:=StrToIntDef(Tag^.Value,0);
end;

function   TXMLParser.getValueAsDouble(Tag:PXMLTag):Double;
begin
  Result:=StrToFloatDef(Tag^.Value,0.0);
end;

function   TXMLParser.getValueAsInt64(Tag:PXMLTag):Int64;
begin
  Result:=StrToIntDef(Tag^.Value,0);
end;

procedure  TXMLParser.getValueAsInt64Array(Tag:PXMLTag; var List:Core.Arrays.Types.LargeInt);
var
  saItems:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  fromString(saItems,Tag^.Value,',');
  Try
    for iLcv:=0 to High(saItems) do begin
      if (Length(saItems[iLcv])>0) then
        Core.Arrays.LargeInt.Add(StrToIntDef(saItems[iLcv],0),List);
    end;
  finally
    Done(saItems);
  end;
end;

function   TXMLParser.getValueAsString(var Tags:TXMLTags; Name:Core.Strings.VarString):Core.Strings.VarString;
var
  xTag:PXMLTag;
begin
  SetLength(Result,0);
  xTag:=getTag(Name,Tags);
  if xTag<>nil then
    Result:=getValueAsString(xTag);
end;

function   TXMLParser.getValueAsInteger(var Tags:TXMLTags; Name:Core.Strings.VarString): LongInt;
var
  xTag:PXMLTag;
begin
  Result:=0;
  xTag:=getTag(Name,Tags);
  if xTag<>nil then
    Result:=getValueAsInteger(xTag);
end;

function   TXMLParser.getValueAsDouble(var Tags:TXMLTags; Name:Core.Strings.VarString):Double;
var
  xTag:PXMLTag;
begin
  Result:=0.0;
  xTag:=getTag(Name,Tags);
  if xTag<>nil then
    Result:=getValueAsDouble(xTag);
end;

function   TXMLParser.getValueAsInt64(var Tags:TXMLTags; Name:Core.Strings.VarString):Int64;
var
  xTag:PXMLTag;
begin
  Result:=0;
  xTag:=getTag(Name,Tags);
  if xTag<>nil then
    Result:=getValueAsInt64(xTag);
end;

procedure  TXMLParser.getValueAsInt64Array(var Tags:TXMLTags; Name:Core.Strings.VarString; var List:Core.Arrays.Types.LargeInt);
var
  xTag:PXMLTag;
begin
  xTag:=getTag(Name,Tags);
  if xTag<>nil then
    getValueAsInt64Array(xTag,List);
end;

Function   TXMLParser.Tag(Const Index:LongInt; sTag:Core.Strings.VarString):Core.Strings.VarString;
var
  iResult,iLcv:LongInt;
  iTagCount:LongInt;
begin
  iLcv:=0; iResult:=-1; Result:='';
  iTagCount:=Length(Stanzas[Index]^.Tags.Items);
  While (iLcv<iTagCount) and (iResult=-1) do begin
    If SameText(Stanzas[Index]^.Tags.Items[iLcv]^.Name,sTag) then begin
      Result:=Stanzas[Index]^.Tags.Items[iLcv]^.Value;
      iResult:=iLcv;
    end;
    Inc(iLcv);
  end;
end;


procedure  TXMLParser.Parse(Var Data:Core.Strings.VarString);
var
  iLcv:LongInt;
begin
  Clear;
  sDebugData:=Data;
  Try
    sData:=Data;
    If ParseStanzas>0 then begin
      iLcv:=0;
      While iLcv<Length(Stanzas) do begin
        AddMode:=tsmDontAdd;
        sData:=Stanzas[iLcv]^.Data;
        iStanzaLcv:=iLcv;
        Perform_Parse;
        Inc(iLcv);
      end;
    end else begin
      Perform_Parse;
    end;
  Except

  End;
end;

procedure  TXMLParser.DecodeTagParameters(Tag:Core.Strings.VarString; Var XMLTag:TXMLTag);
Const
  STRING_VALUE_CHARS=#39'"';
  KEY_COMPLETE='= ';
Type  TState=(sKey,sValue,sStringValue);
Var
  Key:Core.Strings.VarString;
  Value:Core.Strings.VarString;
  BlankValue:Core.Strings.VarString;
  iTagLen,iLcv:LongInt;
  State:TState;

  procedure PushAddKeyBlankValue;
  begin
    If Key<>'' then begin
      Core.Arrays.KeyString.Add(XMLTag.Parameters,Key,BlankValue);
      Key:='';
    end;
  end;

  procedure PushAddOnEquals;
  begin
    If (System.Pos(Tag[iLcv+1],STRING_VALUE_CHARS)>0) then begin
      Inc(iLcv);
      State:=sStringValue;
    end else
      State:=sValue;
  end;

  procedure PushKey;
  begin
    If (System.Pos(Tag[ilcv],KEY_COMPLETE)>0) then begin
      // Key is complete
      Case Tag[iLcv] of
        #32: PushAddKeyBlankValue;
        '=': PushAddOnEquals;
      end;
    end else
      Key:=Concat(Key,Tag[iLcv]);
  end;

  procedure PushValue;
  begin
    If (System.Pos(Tag[iLcv],STRING_VALUE_CHARS)>0) then begin
      // Parsing is Over
      If Key<>'' then
        Core.Arrays.KeyString.Add(XMLTag.Parameters,Key,Value);
      State:=sKey;
      SetLength(Key,0);
      SetLength(Value,0);
    end else
      Value:=Concat(Value,Tag[iLcv]);
  end;

  procedure PushStringValue;
  begin
    If (System.Pos(Tag[iLcv],STRING_VALUE_CHARS)>0) then begin
      State:=sKey;
      // Parsing is Over
      If Key<>'' then
        Core.Arrays.KeyString.Add(XMLTag.Parameters,Key,Value);
      SetLength(Key,0);
      SetLength(Value,0);
    end else
      Value:=Concat(Value,Tag[iLcv]);
  end;

begin
  SetLength(BlankValue,0);
  If (System.Pos(' ',Tag)>0) or (System.Pos('=',Tag)>0) then begin
    //'version="1.0" encoding="UTF-8"?'
    Core.Arrays.KeyString.fromString(XMLTag.Parameters,Tag,'=',' ',[soMakeKeyLowercase,soRemoveQuotes]);
    {
    SetLength(Key,0); SetLength(Value,0); iLcv:=1; iTagLen:=Length(Tag); State:=sKey;
    While iLcv<iTagLen do begin
      Case State Of
        sKey: PushKey;
        sValue: PushValue;
        sStringValue:PushStringValue;
      end;
      Inc(iLcv);
    end;
    If Key<>'' then
      ccUtils.AddKeyPair(Lowercase(Key),Value,XMLTag.Parameters);
    }
  end;
end;

Const
  TagStart='<';
  TagEnd='>';
  CDATAStart='<![CDATA[';
  CDATAEnd=']]>';
Var
  TagNameEnd:Core.Arrays.Types.VarString;

procedure  TXMLParser.Parse_Tag(Kind:TTagKind);
var
  iTagStart,iDataLength,iTagEnd,iTagLength,iAltTagClose,iTagClose:LongInt;
  iLcv:LongInt;
  sTag:Core.Strings.VarString;

  Function Singleton:Boolean;
  begin
    Result:=(iTagLength<2) or (sTag[iTagLength]='/');
  end;

  Function GetTagName:Core.Strings.VarString;
  Var
    iLcv,iLen:LongInt;
    iTagEnd:LongInt;
  begin
    Result:=''; iLcv:=1;
    iTagEnd:=Core.Arrays.VarString.Pos(sTag,TagNameEnd);
    If iTagEnd>0 then begin
      Result:=System.Copy(sTag,1,iTagEnd-1);
      System.Delete(sTag,1,iTagEnd);
    end else begin
      Result:=sTag; // Entire Tag is here...
    end;
    iLen:=Length(Result);
    If (iLen>0) and (Result[iLen]='/') then
      SetLength(Result,iLen-1);
  end;

  Function  IsEndTagPresent:Boolean;
  begin
    Result:=iTagEnd<>0;
  end;

  Function  GetTagValue:Core.Strings.VarString;
  var
    iLen,iCDATAEnd,iCDATAStart,iTagStart,iTagEnd,iLcv:LongInt;
    bNextTagIsCDATA:Boolean;
  begin
    Result:='';
    // Check to see if next tag is CDATA...
    iTagStart:=System.Pos(TagStart,sData);
    iCDATAStart:=System.Pos(CDataStart,sData);
    If iCDATAStart>0 then
      iCDATAEnd:=StrUtils.PosEx(CDataEnd,sData,iCDATAStart);
    If (iCDATAStart>0) and (iTagSTart=iCDATAStart) then begin
      Inc(iCDATAStart,Length(CDATAStart));
      Result:=System.Copy(sData,iCDATAStart,iCDATAEnd-iCDATAStart);
      System.Delete(sData,1,iCDATAEnd+Length(CDATAEnd)-1);
      iDataLength:=Length(sData);
    end else begin
      iLcv:=1; iLen:=iTagStart-1;
      If (iTagStart>0) then begin
        If iLen>0 then begin
          Result:=System.Copy(sData,1,iLen);
          System.Delete(sData,1,iTagStart-1);
        end;
      end else begin
        // No Next Tag Found.
        Result:=sData;
        SetLength(sData,0);  
      end;
      Core.Strings.Trim(sData);
      iDataLength:=Length(sData);
      Core.Strings.Trim(Result);
    end;
  end;

begin
  {
       <stanza start=something etc. /><more tags here>blah bla halbha
       <stanza />bla ahdsf asdiweasld aldiowl blah
       <stanza start=somthjing> blah etc. </stanza>
       <stanza>blah blah blah</stanza>
       <stanza>#13#10<tag>Blah</stanza>
  }
  iTagStart:=System.Pos('<',sData);
  iAltTagClose:=StrUtils.PosEx('/>',sData,iTagStart);
  iTagClose:=StrUtils.PosEx('>',sData,iTagStart);
  If iAltTagClose+1=iTagClose then
    iTagClose:=iAltTagClose;
  sTag:=SysUtils.Trim(System.Copy(sData,iTagStart+1,iTagClose-2));
  System.Delete(sData,1,iTagClose);
  iDataLength:=Length(sData);
  iTagLength:=Length(sTag);

  Case Kind of
    tStanza: begin
        Case AddMode of
          tsmAdd: iStanzaLcv:=Add(Stanzas);
          tsmInsert: begin
            Add(Stanzas);
            For iLcv:=High(Stanzas) downto iStanzaLcv+1 do
              Copy(Stanzas[iLcv-1]^,Stanzas[iLcv]^);
            Empty(Stanzas[iStanzaLcv]^);
          end;
          tsmDontAdd: begin
            AddMode:=tsmInsert;
          end;
        End;
        iTagLcv:=0;
        Stanzas[iStanzaLcv]^.Root.Name:=GetTagName;
        Stanzas[iStanzaLcv]^.Root.Value:=GetTagValue;
        DecodeTagParameters(sTag,Stanzas[iStanzaLcv]^.Root);
      end;
    tNormal: begin
        If Length(Stanzas)=0 then begin
          iStanzaLcv:=Add(Stanzas);
          iTagLcv:=0;
          Stanzas[iStanzaLcv]^.Root.Name:=GetTagName;
          Stanzas[iStanzaLcv]^.Root.Value:=GetTagValue;
          DecodeTagParameters(sTag,Stanzas[iStanzaLcv]^.Root);
        end else begin
          iTagLcv:=Add(Stanzas[iStanzaLcv]^.Tags);
          Stanzas[iStanzaLcv]^.Tags.Items[iTagLcv]^.Name:=GetTagName;
          Stanzas[iStanzaLcv]^.Tags.Items[iTagLcv]^.Value:=GetTagValue;
        end;
      end;
  end;
end;

Function  TXMLParser.StanzaIndex(sTag:Core.Strings.VarString): LongInt;
Var
  iCount,iLcv:LongInt;
begin
  Result:=-1; iLcv:=0; iCount:=Length(Stanzas);
  While (iLcv<iCount) and (Result=-1) do begin
    If SameText(Stanzas[iLcv]^.Root.Name,sTag) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function  TXMLParser.ParseStanzas:LongInt;
var

  iStanzaIndex:LongInt;

    // Start Of Stanza

  iStartOfStanza:LongInt;
  iEndOfStanza:LongInt;
  // End Of Stanza
  // General Info
  iStanzaStart:LongInt;
  iStanzaEnd:LongInt;

  iLcv:LongInt;
  sStanza:Core.Strings.VarString;


  procedure PushStanzaParse;
  begin
    sStanza:=Concat('<',StanzaBreaks[iLcv]);
    iStanzaStart:=System.Pos(sStanza,sData);
    If iStanzaStart>0 then begin
      iStanzaEnd:=PosEx('>',sData,iStanzaStart);
      If iStanzaEnd>0 then begin
        iStartOfStanza:=iStanzaStart;
        sStanza:=Concat('</',StanzaBreaks[iLcv]);
        iStanzaStart:=PosEx(sStanza,sData,iStartOfStanza);
        If iStanzaStart>0 then begin
          iStanzaEnd:=PosEx('>',sData,iStanzaStart);
          If iStanzaEnd>0 then begin
            iEndOfStanza:=iStanzaEnd;
            iStanzaIndex:=Add(Stanzas);
            Stanzas[iStanzaIndex]^.Data:=System.Copy(sData,iStartOfStanza,iStanzaStart-iStartOfStanza);
            System.Delete(sData,1,iStanzaEnd);
            Inc(Result);
            Inc(iStanzaIndex);
          end;
        end;
      end;
    end;
  end;

begin
  Result:=0;  iStanzaIndex:=0;
  For iLcv:=0 to High(StanzaBreaks) do begin
    Repeat
      PushStanzaParse;
    Until iStanzaStart=0;
  end;
end;

procedure  TXMLParser.Perform_Parse;
var
  bFinished:Boolean;
  iTagStart,iAltTagClose,iTagEndOffset,iTagClose,iTagEnd:LongInt;
  sTag:Core.Strings.VarString;
begin
  // <tag stuff>...</tag>
  // <tag .../> means no end tag.
  // <item>something</item>
  bFinished:=False;
  Core.Strings.Trim(sData);
  iTagStart:=System.Pos('<',sData);
  If iTagStart>0 then begin
    System.Delete(sData,1,iTagStart-1);
    iTagStart:=1;
  end;
  If (Length(sData)=0) or (iTagStart=0) Then begin
    Exit;
  end;
  Inc(iTagStart);
  iTagEndOffset:=2;
  iTagEnd:=StrUtils.PosEx(' ',sData,iTagStart);
  iTagClose:=StrUtils.PosEx('/>',sData,iTagStart);
  iAltTagClose:=StrUtils.PosEx('>',sData,iTagStart);
  If iAltTagClose=0 then begin
    iAltTagClose:=Length(sData);
    bFinished:=True;
  end;
  If iAltTagClose<>iTagClose+1 then
    iTagClose:=iAltTagClose;
  If (iTagEnd>iTagClose) or (iTagEnd=0) then begin
    iTagEnd:=iTagClose-1;
    iTagEndOffset:=1;
  end;
  sTag:=System.Copy(sData,iTagStart,iTagEnd-iTagEndOffset);
  // Check alternate tag...
  If Not bFinished then begin
    If Core.Arrays.VarString.IndexOf(StanzaBreaks,sTag)<>-1 then begin
      // This is the start of an entire stanza
      Parse_Tag(tStanza);
      Perform_Parse;
    end else begin
      If (Length(sTag)>1) and (sTag[1]='/') and (Core.Arrays.VarString.IndexOf(EndTags,sTag)=-1) then begin
        System.Delete(sData,1,iAltTagClose);
        Perform_Parse;
      end else begin
        Parse_Tag(tNormal);
        Perform_Parse;
      end;
    end;
  end;
end;


class function DB.Header(var Encoding:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat('<?xml version="1.0" encoding="',Encoding,'"?>');
end;

class procedure DB.Stamp(Encoding:Core.Strings.VarString; Stream:TStream);
var
  sHeader:Core.Strings.VarString;
begin
  sHeader:=Header(Encoding);
  Core.Streams.Write(sHeader,Length(sHeader),Stream);
  Finalize(sHeader);
end;

class function DB.toQWord(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:QWord=0):QWord;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToQWordDef(xNode.TextContent,aDefault);
end;

class function DB.toWord(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Word=0):Word;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToIntDef(xNode.TextContent,aDefault);
end;

class function DB.IsInvalid(var Value:Byte):boolean;
begin
  Result:=(Value<9) or (Value=11) or (Value=12) or ( (Value>13) and (Value<32));
end;

class function  DB.Prepare(var sInput:Core.Strings.VarString; Refactor:TStream):Core.Strings.VarString;
var
  bChar:byte;
  iLcv:cardinal;
  iLen:Int64;
  sReplace:Core.Strings.VarString;
begin
  Refactor.Size:=0;
  for iLcv:=1 to System.Length(sInput) do begin
    bChar:=Byte(sInput[iLcv]);
    if IsInvalid(bChar) then begin
      sReplace:=Concat('&#',IntToStr(bChar),';');
      iLen:=System.Length(sReplace);
      Refactor.Write(sReplace[1],iLen);
    end else
      Refactor.Write(bChar,1);
  end;
  System.SetLength(Result,Refactor.Size);
  if Refactor.Size>0 then begin
    Refactor.Position:=0;
    Refactor.Read(Result[1],Refactor.Size);
  end;
  Refactor.Size:=0;
end;

class function  DB.Extract(var Tag:Core.Strings.VarString; var Data:Core.Strings.VarString):Core.Strings.VarString;
var
  idxStart      : LongInt;
  idxEnd        : LongInt;
  idxDataStart  : LongInt;
  idxDataEnd    : LongInt;
  sTagStart     : Core.Strings.VarString;
  sTagEnd       : Core.Strings.VarString;
begin
  SetLength(Result,0);
  sTagStart:=Concat('<',Tag);
  sTagEnd:=Concat('</',Tag);
  idxStart:=Core.Strings.Search(Data,sTagStart);
  if (idxStart>0) then begin
    idxDataStart:=Core.Strings.Search(Data,'>',idxStart);
    if (idxDataStart>0) then begin
      idxEnd:=Core.Strings.Search(Data,sTagEnd,idxDataStart+1);
      if (idxDataEnd>0) then begin
        Result:=System.Copy(Data,idxDataStart+1,idxDataEnd-idxDataStart)
      end;
    end;
  end;

end;

class procedure DB.Prepare(var sInput:Core.Strings.VarString; Output:TStringStream);
var
  bChar:byte;
  iLcv:cardinal;
  iLen:Int64;
  sReplace:Core.Strings.VarString;
begin
  for iLcv:=1 to System.Length(sInput) do begin
    bChar:=Byte(sInput[iLcv]);
    if IsInvalid(bChar) then begin
      sReplace:=Concat('&#',IntToStr(bChar),';');
      iLen:=System.Length(sReplace);
      Output.Write(sReplace[1],iLen);
    end else
      Output.Write(bChar,1);
  end;
end;

class function DB.toInt64(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Int64=0):Int64;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToIntDef(xNode.TextContent,aDefault);
end;

class function DB.toDouble(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:double=0.0):Double;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToFloatDef(xNode.TextContent,aDefault);
end;

class function DB.toSingle(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Single=0.0):Single;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToFloatDef(xNode.TextContent,aDefault);
end;

class function DB.toString(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Core.Strings.VarString=''):Core.Strings.VarString;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then begin
    Result:=xNode.TextContent;
  end;
end;

class function DB.toXML(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:Core.Strings.VarString=''):Core.Strings.VarString;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
      Result:=getNodeValue(xNode);
end;

class function DB.toXML(Node:TDOMNode; aDefault:Core.Strings.VarString=''):Core.Strings.VarString;
begin
  Result:=aDefault;
  if Node<>nil then
      Result:=getNodeValue(Node);
end;

class function DB.toBoolean(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:boolean=false):boolean;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=( (xNode.TextContent='-1') or (xNode.TextContent='1') or (Core.Strings.SameText(xNode.TextContent,Core.Strings.Defaults.True)) or (Core.Strings.SameText(xNode.TextContent,Core.Strings.Defaults.Yes)) )
end;

class function DB.toInteger(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:LongInt=0 ): LongInt;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToIntDef(xNode.TextContent,aDefault);
end;

class function DB.toByte(Node:TDOMNode; Name:Core.Strings.VarString; aDefault:byte=0):byte;
var
  xNode:TDOMNode;
begin
  Result:=aDefault;
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then
    Result:=StrToIntDef(xNode.TextContent,aDefault);
end;

class procedure DB.toQWordArray(Node:TDOMNode; Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeWord);
var
  xNode:TDOMNode;
  saItems:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  Core.Arrays.LargeWord.Empty(Value);
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then begin
    Core.Arrays.VarString.Init(saItems);
    Try
      Core.Arrays.VarString.fromString(@saItems,xNode.TextContent,',');
      for iLcv:=0 to High(saItems) do begin
        if (Length(saItems[iLcv])>0) then
          Core.Arrays.LargeWord.Add(StrToQWordDef(saItems[iLcv],0),Value);
      end;
    finally
      Core.Arrays.VarString.Done(saItems);
    end;
  end;
end;

class procedure DB.toInt64Array(Node:TDOMNode; Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeInt);
var
  xNode:TDOMNode;
  saItems:Core.Arrays.Types.VarString;
  iLcv:LongInt;
begin
  Core.Arrays.LargeInt.Empty(Value);
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then begin
    Core.Arrays.VarString.fromString(@saItems,xNode.TextContent,',');
    Try
      for iLcv:=0 to High(saItems) do begin
        if (Length(saItems[iLcv])>0) then
          Core.Arrays.LargeInt.Add(StrToInt64Def(saItems[iLcv],0),Value);
      end;
    finally
      Core.Arrays.VarString.Done(saItems);
    end;
  end;
end;

class procedure DB.toByteArray(Node:TDOMNode; Name:Core.Strings.VarString; var Value:Core.Arrays.Types.Bytes);
var
  xNode:TDOMNode;
begin
  Core.Arrays.Bytes.Empty(Value);
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then begin
    Encryption.Base64.Decode(xNode.TextContent,Value);
  end;
end;

class procedure DB.toMD5Digest(Node:TDOMNode; Name:Core.Strings.VarString; var Value:TMD5Digest);
var
  xNode:TDOMNode;
begin
  System.FillByte(Value,16,0);
  xNode:=getChildNode(Node,Name);
  if xNode<>nil then begin
    Encryption.Base64.Decode(xNode.TextContent,Value);
  end;
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Core.Strings.VarString; const CDATA:boolean=true):Core.Strings.VarString;
const
  FMT_OUT:Array[boolean] of Core.Strings.VarString=('<%0:s>%1:s</%0:s>','<%0:s><![CDATA[%1:s]]></%0:s>');
begin
  Result:=Format(FMT_OUT[CDATA],[Name,Value]);
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:shortstring; const CDATA:boolean=true):Core.Strings.VarString;
const
  FMT_OUT:Array[boolean] of Core.Strings.VarString=('<%0:s>%1:s</%0:s>','<%0:s><![CDATA[%1:s]]></%0:s>');
begin
  Result:=Format(FMT_OUT[CDATA],[Name,Value]);
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:shortstring):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'><![CDATA[',Value,']]></',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; Value:byte):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:word):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:DWord):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:LongInt):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Boolean):Core.Strings.VarString;
const
  YN:array[boolean] of Core.Strings.VarString=('no','yes');
begin
  Result:=Concat('<',Name,'>',YN[Value],'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Int64):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Qword):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',IntToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Double):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',FloatToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Single):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>',FloatToStr(Value),'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeInt):Core.Strings.VarString;
begin
  Core.Arrays.LargeInt.toString(Value,',',Result);
  Result:=Concat('<',Name,'>', Result,'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.LargeWord):Core.Strings.VarString;
begin
  Core.Arrays.LargeWord.toString(Value,',',Result);
  Result:=Concat('<',Name,'>', Result,'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.KeyStrings):Core.Strings.VarString;
begin
  Core.Arrays.KeyString.toString(Value,',',Result);
  Result:=Concat('<',Name,'>', Result,'</',Name,'>');
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.VarString):Core.Strings.VarString;
begin
  Core.Arrays.VarString.toString(Value);
  Result:=Concat('<',Name,'>', Result,'</',Name,'>');
end;

class procedure DB.Stream(Name:Core.Strings.VarString; var Value:Core.Arrays.Types.Bytes; Output,Refactor:TStream);
begin
  Core.Streams.Write(Concat('<',Name,'>'),Output);
  Encryption.Base64.Encode(Value,Output,Refactor);
  Core.Streams.Write(Concat('</',Name,'>'),Output);
end;

class function DB.Insert(var Entry,Data:Core.Strings.VarString; Tag:Core.Strings.VarString):boolean;
var
  idxStart:LongInt;
begin
  Result:=false;
  Tag:=Concat('<',Tag,'>');
  idxStart:=Core.Strings.Search(Data,Tag);
  if (idxStart>0) then begin
    idxStart+=Length(Tag)+2;
    System.Insert(Entry,Data,idxStart);
    Result:=true;
  end;
end;

class function DB.Print(Name:Core.Strings.VarString; var Value:TMD5Digest):Core.Strings.VarString;
begin
  Result:=Concat('<',Name,'>', Encryption.Base64.Encode(Value),'</',Name,'>');
end;

class function DB.getNode(xDoc:TXMLDocument; Name:Core.Strings.VarString):TDOMNode;

  function ScanChildren(Parent:TDOMNode):TDOMNode;
  var
    iLcv:LongInt;
    nChild:TDOMNode;
  begin
    Result:=nil;
    if Core.Strings.SameText(Parent.NodeName,Name) then
      Result:=Parent
    else begin
      for iLcv:=0 to Parent.ChildNodes.Count-1 do begin
        nChild:=Parent.ChildNodes[iLcv];
        if SameText(nChild.NodeName,Name) then begin
          Result:=nChild;
          break;
        end else
          Result:=ScanChildren(nChild);
      end;
    end;
  end;

begin
  Result:=ScanChildren(xDoc.DocumentElement);
end;

class function DB.getChildNode(xNode:TDOMNode; Name:Core.Strings.VarString):TDOMNode;
var
  iLcv:LongInt;
begin
  Result:=nil;
  For iLcv:=0 to xNode.ChildNodes.Count-1 do begin
    if SameText(xNode.ChildNodes[iLcv].NodeName,Name) then begin
      Result:=xNode.ChildNodes[iLcv];
      break;
    end;
  end;
end;

class function DB.getNodeWrapped(xNode:TDOMNode):Core.Strings.VarString;
var
  iLcv:LongInt;
  xChild:TDOMNode;
begin
  SetLength(Result,0);
  if (xNode.NodeType=TEXT_NODE) or (xNode.NodeType=CDATA_SECTION_NODE) then begin
    Result:=xNode.NodeValue;
  end else if (xNode.ChildNodes.Count=1) then begin
    Result:=Concat('<',xNode.NodeName,'>',getNodeWrapped(xNode.ChildNodes[0]),'</',xNode.NodeName,'>');
  end else if (xNode.ChildNodes.Count>1) then begin
    Result:=Concat('<',xNode.NodeName,'>');
    for iLcv:=0 to xNode.ChildNodes.Count-1 do begin
      xChild:=xNode.ChildNodes[iLcv];
      if  (xChild.NodeType=TEXT_NODE) or (xChild.NodeType=CDATA_SECTION_NODE) then begin
        Result:=Concat(Result,'<',xChild.NodeName,'>',xChild.NodeValue,'</',xChild.NodeName,'>');
      end else begin
        Result:=Concat(Result,getNodeWrapped(xChild));
      end;
    end;
    Result:=Concat(Result,'</',xNode.NodeName,'>');
  end else begin
    Result:=xNode.TextContent;
  end;
  //SetCodePage(Result,CP_UTF8,true);
end;

class function DB.getNodeValue(xNode:TDOMNode):Core.Strings.VarString;
var
  iLcv:LongInt;
  xChild:TDOMNode;
begin
  SetLength(Result,0);
  if (xNode.NodeType=TEXT_NODE) or (xNode.NodeType=CDATA_SECTION_NODE) then begin
    Result:=xNode.NodeValue;
  end else if (xNode.ChildNodes.Count=1) then begin
    Result:=getNodeWrapped(xNode.ChildNodes[0]);
  end else if (xNode.ChildNodes.Count>1) then begin
    for iLcv:=0 to xNode.ChildNodes.Count-1 do begin
      xChild:=xNode.ChildNodes[iLcv];
      if  (xChild.NodeType=TEXT_NODE) or (xChild.NodeType=CDATA_SECTION_NODE) then begin
        Result:=Concat(Result,'<',xChild.NodeName,'>',xChild.NodeValue,'</',xChild.NodeName,'>');
      end else begin
        Result:=Concat(Result,getNodeWrapped(xChild));
      end;
    end;
  end else begin
    Result:=xNode.TextContent;
  end;
  //SetCodePage(Result,CP_UTF8,true);
end;

class function DB.getNodeText(xNode:TDOMNode):Core.Strings.VarString;
var
  iLcv:LongInt;
  xChild:TDOMNode;
begin
  SetLength(Result,0);
  if (xNode=nil) then exit;
  if (xNode.NodeType=TEXT_NODE) then
    Result:=xNode.NodeValue
  else if (xNode.NodeType=CDATA_SECTION_NODE) then
    Result:='<![CDATA['+xNode.NodeValue+']]>'
  else if (xNode.ChildNodes.Count=1) then
    Result:=getNodeTextWrapped(xNode.ChildNodes[0])
  else if (xNode.ChildNodes.Count>1) then begin
    for iLcv:=0 to xNode.ChildNodes.Count-1 do begin
      xChild:=xNode.ChildNodes[iLcv];
      if (xChild.NodeType=TEXT_NODE) then
        Result:=Concat(Result,'<',xChild.NodeName,'>',xChild.NodeValue,'</',xChild.NodeName,'>')
      else if (xNode.NodeType=CDATA_SECTION_NODE) then
        Result:=Concat(Result,'<',xChild.NodeName,'><![CDATA[',xChild.NodeValue,']]></',xChild.NodeName,'>')
      else
        Result:=Concat(Result,getNodeTextWrapped(xChild));
    end;
  end else
    Result:=xNode.TextContent;

  //SetCodePage(Result,CP_UTF8,true);
end;

class function DB.wrapCDATA(const sData:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat('<![CDATA[',sData,']]>');
end;

class procedure DB.Wrap(DocType,Tag:Core.Strings.VarString; var Data:Core.Strings.VarString);
begin
  Data:=Concat(DocType,'<',Tag,'>',Data,'</',Tag,'>');
end;

class function DB.getNodeTextWrapped(xNode:TDOMNode):Core.Strings.VarString;
var
  iLcv:LongInt;
  xChild:TDOMNode;
begin
  SetLength(Result,0);
  if (xNode=nil) then exit;
  if (xNode.NodeType=TEXT_NODE) then
    Result:=xNode.NodeValue
  else if (xNode.NodeType=CDATA_SECTION_NODE) then
    Result:='<![CDATA['+xNode.NodeValue+']]>'
  else if (xNode.ChildNodes.Count=1) then
    Result:=Concat('<',xNode.NodeName,'>',getNodeTextWrapped(xNode.ChildNodes[0]),'</',xNode.NodeName,'>')
  else if (xNode.ChildNodes.Count>1) then begin
    Result:=Concat('<',xNode.NodeName,'>');
    for iLcv:=0 to xNode.ChildNodes.Count-1 do begin
      xChild:=xNode.ChildNodes[iLcv];
      if  (xChild.NodeType=TEXT_NODE) then
        Result:=Concat(Result,'<',xChild.NodeName,'>',xChild.NodeValue,'</',xChild.NodeName,'>')
      else if  (xChild.NodeType=CDATA_SECTION_NODE) then
        Result:=Concat(Result,'<',xChild.NodeName,'><![CDATA[',xChild.NodeValue,']]></',xChild.NodeName,'>')
      else
        Result:=Concat(Result,getNodeTextWrapped(xChild));
    end;
    Result:=Concat(Result,'</',xNode.NodeName,'>');
  end else
    Result:=xNode.TextContent;

  //SetCodePage(Result,CP_UTF8,true);
end;

Initialization
 Core.Arrays.VarString.Add(@TagNameEnd,' ');
 Core.Arrays.VarString.Add(@TagNameEnd,'>');

Finalization
 Core.Arrays.VarString.Empty(TagNameEnd);

end.

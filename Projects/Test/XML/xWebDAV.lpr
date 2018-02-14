program xWebDAV;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,XMLRead,DOM, uWebDAV;

  function GenerateTestStream():TMemoryStream;
  var
    Buffer:AnsiString;
  begin
    Result:=TMemoryStream.Create();
    Buffer:=Concat(
      '<?xml version="1.0" encoding="UTF-8"?>',
      '<D:propfind xmlns:D="DAV:">',
        '<D:prop><D:supportedlock/></D:prop>',
      '</D:propfind>'
    );
    Result.Write(Buffer[1],Length(Buffer));
  end;

  function  GetAttribute(Node:TDOMNode; Name:UTF8String):UTF8String;
  var
    Attrib:TDOMNode;
  begin
    SetLength(Result,0);
    Attrib:=Node.Attributes.GetNamedItem(Name);
    If Assigned(Attrib) then
      Result:=Attrib.NodeValue;
  end;

  function  Test:boolean;
  var
    FXMLParser    : TDOMParser;
    FXMLDocument  : TXMLDocument;
    FXMLSource    : TXMLInputSource;
    FDOMNode      : TDOMNode;
    FData         : TMemoryStream;
    Output        : UTF8String;
  begin
    FXMLParser:=TDOMParser.Create();
    Try
      FData:=GenerateTestStream();
      FData.Position:=0;
      FXMLSource:=TXMLInputSource.Create(FData);
      Try
        FXMLParser.Parse(FXMLSource,FXMLDocument);

        FDOMNode:=FXMLDocument.FirstChild;
        Output:=Concat(
          ' NodeName=',FDOMNode.NodeName,
          ' NodeValue=',FDOMNode.NodeValue,
          ' NamespaceURI=',FDOMNode.NamespaceURI,
          ' XMLNS=',GetAttribute(FDOMNode,'xmlns:D')
        );
        FData.Free();
      Finally
        FXMLDocument.Free();
      end;
    Finally
      FXMLParser.Free();
    End;
  end;
begin
  Test();
end.


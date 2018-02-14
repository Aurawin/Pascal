program xLimit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,XMLRead,DOM;

  function GenerateTestStream(MAX_GIGS:integer):TMemoryStream;
  const
    BUFFER_SIZE=1024*1024; // 1MB
  var
    Buffer:string;
    iGig:integer;
    iLcv:integer;
  begin
    Result:=TMemoryStream.Create();
    Buffer:=Concat('<?xml version="1.0" encoding="LATIN1"?>');
    Result.Write(Buffer[1],Length(Buffer));
    Buffer:='<file><name>test</name><mime>';
    Result.Write(Buffer[1],Length(Buffer));
    SetLength(Buffer,BUFFER_SIZE);
    for iLcv:=1 to Length(Buffer) do
      Buffer[iLcv]:=Char(97+random(26));
    for iGig:=1 to MAX_GIGS do begin
      for iLcv:=1 to 1024 do
        Result.Write(Buffer[1],BUFFER_SIZE);
    end;
    Buffer:='</mime></file>';
    Result.Write(Buffer[1],Length(Buffer));
  end;

  function  Test:boolean;
  const
    ONE_GIGABYTE=1;
    TWO_GIGABYTES=2;
  var
    FXMLParser    : TDOMParser;
    FXMLDocument  : TXMLDocument;
    FXMLSource    : TXMLInputSource;
    FData         : TMemoryStream;
  begin
    FXMLParser:=TDOMParser.Create();
    Try
      FData:=GenerateTestStream(ONE_GIGABYTE);
      FData.Position:=0;
      FXMLSource:=TXMLInputSource.Create(FData);
      Try
        FXMLParser.Parse(FXMLSource,FXMLDocument);
        FData.Free();
      Finally
        FXMLDocument.Free();
      end;
      // Pause Here look at memory usage
      FData:=GenerateTestStream(TWO_GIGABYTES);
      FData.Position:=0;
      FXMLSource:=TXMLInputSource.Create(FData);
      Try
        FXMLParser.Parse(FXMLSource,FXMLDocument);
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


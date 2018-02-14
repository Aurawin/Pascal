function   TAuSocketMan.ParseXML(var Data:Core.Strings.VarString):boolean;
var
  FXMLSource                     : TXMLInputSource;
  iDataLen                       : LongInt;
begin
  Result:=False;
  iDataLen:=Length(Data);
  if (iDataLen>0) then begin
    Try
      FXMLSource:=TXMLInputSource.Create(Data);
      Try
        Try
          FXMLParser.Parse(FXMLSource,FXMLDocument);
          Result:=True;
        Except
          if FXMLDocument<>nil then begin
            FXMLDocument.Free();
            FXMLDocument:=nil;
          end;
        end;
      Finally
        FXMLSource.Free();
      end;
    Except

    end;
  end else begin

  end;
end;

function   TAuSocketMan.ParseXML(Data:TMemoryStream; ClearAfter:Boolean):boolean;
var
  FXMLSource                     : TXMLInputSource;
begin
  Result:=False;
  EntryPoint:='TAuSocketMan.ParseXML(Data:TMemoryStream; ClearAfter:Boolean)';
  if (Data.Size>0) then begin
    Data.Position:=0;
    Try
      EntryPoint:='TAuSocketMan.ParseXML(Data:TMemoryStream; ClearAfter:Boolean).FXMLSource';
      FXMLSource:=TXMLInputSource.Create(Data);
      Try
        Try
          EntryPoint:='TAuSocketMan.ParseXML(Data:TMemoryStream; ClearAfter:Boolean).Parse';
          FXMLParser.Parse(FXMLSource,FXMLDocument);
          Result:=True;
          {$ifdef Buffer_Debug}
            if FXMLDocument=nil then begin
              Core.Logging.Native.WriteLogEntry(
                SYSTEM_LOG,
                EntryPoint,
                'XML Document returned null!'
              );
            end else begin
              Core.Logging.Native.WriteLogEntry(
                  SYSTEM_LOG,
                  EntryPoint,
                  'XML Document Parsed'
              );
            end;
          {$endif}
        Except
          On E:Exception do begin
              Core.Streams.toFile(Data,SYSTEM_DUMP);
              Core.Logging.Native.WriteLogEntry(
                SYSTEM_LOG,
                EntryPoint,
                E.Message
              );

            if (FXMLDocument<>nil) then begin
              FXMLDocument.Free;
              FXMLDocument:=nil;
            end;

          end;
        end;
      Finally
        FXMLSource.Free();
      end;
    Except
      on E:Exception do begin
        {$ifdef Buffer_Debug}
          Core.Logging.Native.WriteLogEntry(
            SYSTEM_LOG,
            EntryPoint,
            E.Message
          );
        {$endif}
      end;
    end;
    If ClearAfter then
      Data.Size:=0;
  end else begin
    Core.Logging.Native.WriteLogEntry(
      SYSTEM_LOG,
      EntryPoint,
      'Null XML Document'
    );
  end;
end;

function   TAuSocketMan.ParseXML(var Data:Core.Strings.VarString; xHeader:boolean):boolean;
var
  FXMLSource                     : TXMLInputSource;
  iDataLen                       : LongInt;
begin
  Result:=False;
  iDataLen:=System.Length(Data);
  if iDataLen>0 then begin
    Refactor.Size:=0;
    Core.XML.DB.Stamp(Settings.Encoding,Refactor);
    Core.Streams.Write(Data,Refactor);
    Refactor.Position:=0;
    Try
      FXMLSource:=TXMLInputSource.Create(Refactor);
      Try
        Try
          FXMLParser.Parse(FXMLSource,FXMLDocument);
          Result:=true;
        except
          if FXMLDocument<>nil then begin

            FXMLDocument.Free();
            FXMLDocument:=nil;
          end;
        end;
      Finally
        FXMLSource.Free();
      end;
    except

    end;
    Refactor.Size:=0;
  end;
end;


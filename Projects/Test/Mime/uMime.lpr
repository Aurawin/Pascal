program uMime;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,uMime,uStreams;

  procedure PushTest();
  var
    SS:TStringStream;
    sData:string;
  begin
    SS:=TStringStream.Create('');
    Try
      uStreams.fromFile('/Developer/mime.txt',SS);
      SS.Position:=0;
      sData:=uMime.Base64Decode(SS.DataString);
    finally
      SS.Free();
    end;
  end;

begin
  PushTest();
end.


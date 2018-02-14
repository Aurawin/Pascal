program TProcessParams;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}

  Classes,process,sysUtils;

var
  sKeyFile:string;
  sDerkeyFile:string;
  sKeyFileCheck:string;
  sDerkeyFileCheck:string;
  iValue:integer;


  function Test1:boolean;
  var
    procGen:TProcess;
  begin
    procGen:=TProcess.Create(nil);
    procGen.Options:=procGen.Options+[poWaitOnExit];
    Try
      procGen.CommandLine:=Concat(
        'openssl genrsa -out ',
        sKeyFile,
        ' ',IntToStr(iValue)
      );
      procGen.Execute();

      procGen.CommandLine:=Concat(
        'openssl rsa -in ',
        sKeyFile,' ',
        '-inform pem ',
        '-out ',
        sDerKeyFile,' ',
        '-outform der'
      );
      procGen.Execute();

      Result:=FileExists(sDerkeyFileCheck) and FileExists(sKeyFileCheck);

      SysUtils.DeleteFile(sKeyFileCheck);
      SysUtils.DeleteFile(sDerkeyFileCheck);

    finally
      procGen.Free();
    end;
  end;

  function Test2:boolean;
  var
    procGen:TProcess;
    iValue:integer;
  begin
    iValue:=2048;
    procGen:=TProcess.Create(nil);
    procGen.Options:=procGen.Options+[poWaitOnExit];
    Try
      procGen.Executable:='openssl';
      procGen.Parameters.Clear();

      procGen.Parameters.Add('genrsa');
      procGen.Parameters.Add('-out');
      procGen.Parameters.Add(sKeyFile);
      procGen.Parameters.Add(IntToStr(iValue));

      procGen.Execute();


      procGen.Parameters.Clear();
      procGen.Parameters.Add('rsa');
      procGen.Parameters.Add('-in');
      procGen.Parameters.Add(sKeyFile);
      procGen.Parameters.Add('-inform');
      procGen.Parameters.Add('pem');
      procGen.Parameters.Add('-out');
      procGen.Parameters.Add(sDerKeyFile);
      procGen.Parameters.Add('-outform');
      procGen.Parameters.Add('der');

      Result:=FileExists(sDerkeyFile) and FileExists(sKeyFile);

      SysUtils.DeleteFile(sKeyFileCheck);
      SysUtils.DeleteFile(sDerkeyFileCheck);
    finally
      procGen.Free();
    end;
  end;

begin
  iValue:=2048;
  sKeyFileCheck:='/tmp/key.txt';
  sDerKeyFileCheck:='/tmp/durkey.txt';
  sKeyFile:='"/tmp/key.txt"';
  sDerKeyFile:='"/tmp/durkey.txt"';

  if Test1()=false then
    raise Exception.Create('Test program failed.  Either openssl not present or commmandline problem!');
  if Test2()=false then
    raise Exception.Create('Test program failed.  Commmandline problem!');

end.


program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, BESEN, Core.Social.Folders, Core.Social.Folders.Defaults, Core.Social.Folders.DB, Core.Social.Folders.XML, Core.Social.Folders.DB.IDS,
  Core.Social.Folders.DB.Types, Core.Database, Core.Database.Types, Core.Arrays, Core.Arrays.Int64, Core.Strings, Core.Arrays.Types, Core.Arrays.Byte,
  Core.Database.Monitor, Core.Database.Monitor.Notify, Core.Database.Monitor.Types, Core.Streams, Core.Streams.Types, Core.Arrays.VarString,
  Core.Arrays.KeyString, Core.Besen, Core.Data, Core.Data.Types;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.


program NameSpace;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, uNameSpace1,uNameSpace2;


Type
  TMyClass=class
  private
   FItem1   : uNameSpace1.Storage.Folders.Pipe.TItem;
   FItem2   : uNameSpace2.Storage.Folders.Pipe.TItem;
  protected
   procedure Test1(var Item:uNameSpace1.Storage.Folders.Pipe.TItem); overload;
   procedure Test2(var Item:uNameSpace2.Storage.Folders.Pipe.TItem); overload;
  public
   property Item1:uNameSpace1.Storage.Folders.Pipe.TItem read FItem1;
   property Item2:uNameSpace2.Storage.Folders.Pipe.TItem read FItem2;
  end;

procedure TMyClass.Test1(var Item:uNameSpace1.Storage.Folders.Pipe.TItem);
begin
end;

procedure TMyClass.Test2(var Item:uNameSpace2.Storage.Folders.Pipe.TItem);
begin
end;

{$R *.res}

var MC:TMyClass;

begin
  MC:=TMyClass.Create();


end.


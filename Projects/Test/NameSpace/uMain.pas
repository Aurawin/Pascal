unit uMain; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  uNameSpace;

type

  (*remove comment on next line to crash Linking*)
  //TMyEvent=procedure(var Items:Storage.UserStorage.Folders.TItem) of Object;
  TMyClass=class
  private
    FFolders : uNameSpace.Storage.UserStorage.Folders.TItems;
    FOnChange : uNameSpace.Storage.UserStorage.Folders.TItemsEvent;
    //FMyEvent : TMyEvent;

  public
    property OnChange:Storage.UserStorage.Folders.TItemsEvent read FOnChange write FOnChange;
    //property MyEvent:TMyEvent read FOnMyEvent write FOnMyEvent;
  end;


  TForm1 = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
    function  SomeMethod(Var ItemP:uNameSpace.Storage.UserStorage.Folders.PItem):uNameSpace.Storage.UserStorage.Folders.PItem;
    procedure MyEvent(var Item:uNameSpace.Storage.UserStorage.Folders.TItem); overload;
    procedure MyEvent(var Item:uNameSPace.Storage.UserStorage.Files.TItem); overload;
    procedure MyEvent(var Item:uNameSPace.Storage.Resources.TItem); overload;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

procedure TForm1.MyEvent(var Item:uNameSpace.Storage.Resources.TItem);
begin

end;

procedure TForm1.MyEvent(var Item:uNameSpace.Storage.UserStorage.Folders.TItem);
begin

end;


procedure TForm1.MyEvent(var Item:uNameSpace.Storage.UserStorage.Files.TItem);
begin

end;


function TForm1.SomeMethod(Var ItemP:uNameSpace.Storage.UserStorage.Folders.PItem):uNameSpace.Storage.UserStorage.Folders.PItem;
begin

end;

end.


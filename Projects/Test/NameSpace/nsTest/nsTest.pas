program nsTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, uNameSpace;

Type
  TMyEvent=procedure(var Items:Storage.UserStorage.Folders.TItem) of Object;
  TMyClass=class
  private
    FFolders : uNameSpace.Storage.UserStorage.Folders.TItems;
    FOnChange : uNameSpace.Storage.UserStorage.Folders.TItemsEvent;
    FMyEvent : TMyEvent;
  public
    property OnChange:Storage.UserStorage.Folders.TItemsEvent read FOnChange write FOnChange;
    //property MyEvent:TMyEvent read FOnMyEvent write FOnMyEvent;
  end;

  TOtherClass=class
    function  SomeMethod(Var ItemP:uNameSpace.Storage.UserStorage.Folders.PItem):uNameSpace.Storage.UserStorage.Folders.PItem;
    procedure MyEvent(var Item:uNameSpace.Storage.Resources.TItem); overload;
    {remove comment} procedure MyEvent(var Item:uNameSpace.Storage.UserStorage.Folders.TItem); overload;
    {remove comment} procedure MyEvent(var Item:uNameSpace.Storage.UserStorage.Files.TItem); overload;
  end;

  function TOtherClass.SomeMethod(Var ItemP:uNameSpace.Storage.UserStorage.Folders.PItem):uNameSpace.Storage.UserStorage.Folders.PItem;
  begin

  end;

  procedure TOtherClass.MyEvent(var Item:uNameSpace.Storage.Resources.TItem);
  begin

  end;
  procedure TOtherClass.MyEvent(var Item:uNameSpace.Storage.UserStorage.Folders.TItem);
  begin

  end;


  procedure TOtherClass.MyEvent(var Item:uNameSpace.Storage.UserStorage.Files.TItem);
  begin

  end;

begin


end.


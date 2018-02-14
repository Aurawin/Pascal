unit uNameSpace; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  Storage=class
  type
    Folders=class
    Const
      FLAG_REFRESH = 1;
      FLAG_DELETE  = 2;
    Type
      TItem=record
        ID:Int64;
        Path:String;
      end;
      PItem=^TItem;
      TItems=array of PItem;
      PItems=^TItems;

      TItemsEvent=procedure(Var Items:TItems) of Object;
    end;
    Files=class
    Type
      PItem=^TItem;
      TItem=record
        Name :String;
        Created:TDateTime;
      end;
      TItems=Array of PItem;
      TItemsEvent=procedure(Var Items:TItems) of Object;
    end;
    Resources=class
    Type
      PItem=^TItem;
      TItem=record
        Name :String;
        Description:String;
        Manifest:String;
      end;
      TItems=Array of PItem;
      TItemsEvent=procedure(Var Items:TItems) of Object;
    end;
  end;

implementation

end.


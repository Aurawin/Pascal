unit uNameSpace2;

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
      Pipe=class
      Type
        TItem=record
          Path:String;
        end;
      end;
    end;
  end;


implementation

end.


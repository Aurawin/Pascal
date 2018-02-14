unit Storage.Social.Folders.Defaults;

{$mode objfpc}{$H+}

interface
uses Core.Strings;

  Type
    Home=record
      Documents: Core.Strings.VarString;
      Music    : Core.Strings.VarString;
      Pictures : Core.Strings.VarString;
      Videos   : Core.Strings.VarString;
      Trash    : Core.Strings.VarString;
    end;
implementation

end.


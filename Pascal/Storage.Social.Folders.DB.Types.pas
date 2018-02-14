unit Core.Social.Folders.DB.Types;

interface

uses
  Core.Strings;

Type
  Keys=record
    ID                           : Core.Strings.VarString;
    InsertID                     : Core.Strings.VarString;
    DomainID                     : Core.Strings.VarString;
    NetworkID                    : Core.Strings.VarString;
    OwnerID                      : Core.Strings.VarString;
    Path                         : Core.Strings.VarString;
  end;
  IDs=record
    ID                           : LongInt;
    InsertID                     : LongInt;
    DomainID                     : LongInt;
    NetworkID                    : LongInt;
    OwnerID                      : LongInt;
    Path                         : LongInt;
  end;

implementation

end.


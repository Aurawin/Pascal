unit Core.Social.Folders.DB;
interface

uses
  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor.Types,
  Core.Social.Folders.DB.Types;


Const
    Keys:Core.Social.Folders.DB.Types.Keys=(
      ID                       : 'ITMID';
      InsertID                 : 'ITMIID';
      DomainID                 : 'ITMDID';
      NetworkID                : 'ITMNID';
      OwnerID                  : 'ITMOID';
      Path                     : 'ITMPTH';
    );
    IDS:Core.Social.Folders.DB.Types.IDs=(
      ID                       : 0;
      InsertID                 : 1;
      DomainID                 : 2;
      NetworkID                : 3;
      OwnerID                  : 4;
      Path                     : 5;
    );

    TableP: Core.Database.Types.Core.Database.Types.PTable = nil;
    MonitorP: Core.Database.Monitor.Types.PItem = nil;
    Startup: Core.Database.Types.TableIni = (
      AutoCreate           : True;
      AutoCommit           : True;
      Group                : 'System/Applications/Social/Storage';
      Name                 : 'Folders';
      Value                : 'scs_soc_flds';
      Hint                 : 'Storage of folders structures for social networks';
      PrimaryKey           : @Keys.ID;
    );
    Fields: array [0..5] of Core.Database.Types.DatabaseField = (
      (IDP: @IDs.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity; KeyP: @Keys.ID; ),
      (IDP: @IDs.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; KeyP: @Keys.InsertID; ),
      (IDP: @IDs.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; KeyP: @Keys.DomainID; ),
      (IDP: @IDs.NetworkID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; KeyP: @Keys.NetworkID; ),
      (IDP: @IDs.OwnerID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; KeyP: @Keys.OwnerID; ),
      (IDP: @IDs.Path; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; KeyP: @Keys.Path; )
    );

implementation

end.


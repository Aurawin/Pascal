unit coSocial;
{
 Copyright Aurawin LLC 2003-2016
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface
uses
  Classes,

  App.Consts,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  uHTTPd,
  hHTTPd,



  Storage.VDM,
  Storage.CoreObjects,

  coSearch,

  Encryption.Base64,

  Core.XML,
  Core.Streams,
  Core.Keywords,
  Core.Strings,
  Core.Timer,
  Core.Logging,



  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,

  Core.Utils.Files,

  Multimedia.MPEG,
  Multimedia.Image,

  Storage.UserAccounts,
  Storage.UserStorage,

  Storage.Social,
  Storage.Social.Network,
  Storage.Social.Network.Requests,
  Storage.Social.Folders,
  Storage.Social.Files,
  Storage.Social.Sync,

  Storage.ContentTypes,
  Storage.Roster,
  Storage.Search,
  Storage.Summary,

  MD5,
  DOM,
  XMLRead,
  SysUtils;


type
  Social=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'Social';
      NameSpace                  : '/core/soc';
      Caption                    : 'Social Networking Core Object';
      Prompt                     : 'User can access the Social Networking system';
      Description                : 'System wide application for social networking';
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TSocialCore';
      Location                   : 'coSocial.pas';
    );
    Header:TCoreObjectInfo=(
      ID                         : 0;
      ProviderID                 : 0;
      Enabled                    : true;
      Anonymous                  : true;
      Scale                      : 0;
      CLSInfo                    : @CLSInf;
      ACLInfo                    : @ACLInf;
    );
  type
    Network=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      type
        Search=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Search';
            NameSpace            : '/n/s';
            Caption              : 'Search Networks';
            Prompt               : 'User can search for social networks';
            Description          : 'Search social networks from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/n/r';
            Caption              : 'Read Network';
            Prompt               : 'User can read network properties';
            Description          : 'Network retrieval from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Write=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Write';
            NameSpace            : '/n/w';
            Caption              : 'Write Network';
            Prompt               : 'User can write their network properties';
            Description          : 'Write network properties to Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Add=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Add';
            NameSpace            : '/n/a';
            Caption              : 'Add Network';
            Prompt               : 'User can add a social network';
            Description          : 'Add social network to Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method               : nil;
            Resource             : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/n/d';
            Caption              : 'Delete Network';
            Prompt               : 'User can delete their social networks';
            Description          : 'Delete social network from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method               : nil;
            Resource             : nil;
          );
        end;
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/n/l';
            Caption              : 'List Networks';
            Prompt               : 'User can list social networks they created';
            Description          : 'List social network from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method               : nil;
            Resource             : nil;
          );
        end;
    end;
    Connection=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/c/r';
            Caption              : 'Read Connection';
            Prompt               : 'User can read connection properties';
            Description          : 'Connection retrieval from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method               : nil;
            Resource             : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/c/d';
            Caption              : 'Delete Connection';
            Prompt               : 'User can delete connections to social networks';
            Description          : 'Delete social network connections from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/c/l';
            Caption              : 'List Connections';
            Prompt               : 'User can list social network connections';
            Description          : 'List social network connections from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
      end;
    Folders=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      type
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/fldrs/l';
            Caption              : 'List of Folders';
            Prompt               : 'User can obtain a list folders';
            Description          : 'Retrieve folders from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Add=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Add';
            NameSpace            : '/fldrs/a';
            Caption              : 'Add a new folder';
            Prompt               : 'User can create folders';
            Description          : 'Add folders to Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/fldrs/d';
            Caption              : 'Delete a folder';
            Prompt               : 'User can delete folders';
            Description          : 'Delete folders from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Clear=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Clear';
            NameSpace            : '/fldrs/clr';
            Caption              : 'Clear all contents in a folder';
            Prompt               : 'User can delete all files from folders';
            Description          : 'Delete all files in a folder from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Rename=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Rename';
            NameSpace            : '/fldrs/r';
            Caption              : 'Rename a folder';
            Prompt               : 'User can rename folders';
            Description          : 'Rename folders in Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
    end;
    Files=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      NoXMLInf:TXMLInfo=(
       Enabled                   : false;
      );
    type
      List=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List';
          NameSpace              : '/fls/l';
          Caption                : 'List of Files';
          Prompt                 : 'User can obtain a list files';
          Description            : 'Retrieve files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      ListWith=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List With';
          NameSpace              : '/fls/lw';
          Caption                : 'List Files within Folders';
          Prompt                 : 'User can obtain a list of files';
          Description            : 'Retrieve user files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      ListAll=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List All';
          NameSpace              : '/fls/la';
          Caption                : 'List All Files';
          Prompt                 : 'User can obtain a list of all files';
          Description            : 'Retrieve all files from folders in Database for Synchronization';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Add=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Add';
          NameSpace              : '/fls/a';
          Caption                : 'Add a new file';
          Prompt                 : 'User can create files';
          Description            : 'Add files to folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Download=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Download';
          NameSpace              : '/fls/dl';
          Caption                : 'Download a file';
          Prompt                 : 'User can download files';
          Description            : 'Download files from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Get=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Get';
          NameSpace              : '/fls/get';
          Caption                : 'Access a file via http get';
          Prompt                 : 'User can access a file over the web';
          Description            : 'Host files from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : true;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      GetData=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Get Data';
          NameSpace              : '/fls/ged';
          Caption                : 'Retrieve raw file data';
          Prompt                 : 'User can access raw file data';
          Description            : 'Retrieves network files from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      SetData=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Set Data';
          NameSpace              : '/fls/sed';
          Caption                : 'Write raw file data';
          Prompt                 : 'User can write raw file data';
          Description            : 'Writes network files from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Stream=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Stream';
          NameSpace              : '/fls/strm';
          Caption                : 'Stream a file via http get';
          Prompt                 : 'User can stream a file over the web';
          Description            : 'Stream files from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : true;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Rotate=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Rotate';
          NameSpace            : '/fls/rot';
          Caption              : 'Rotate images';
          Prompt               : 'User can rotate pictures for display';
          Description          : 'Rotates pictures';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Transform=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Transform';
          NameSpace              : '/fls/tfm';
          Caption                : 'Transform certain media to fit display';
          Prompt                 : 'User can transform media for their current display';
          Description            : 'Transforms media';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : true;
          Cache                  : true;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      PalmPrint=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'PalmPrint';
          NameSpace              : '/fls/plp';
          Caption                : 'Palm Print certain media to fit display';
          Prompt                 : 'User can down scale media for their current display';
          Description            : 'Downscales media';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : true;
          Cache                  : true;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @NoXMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      SetCreated=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Set Created';
          NameSpace              : '/fls/sc';
          Caption                : 'Set created date time stamp';
          Prompt                 : 'User can change the value of when a file was created';
          Description            : 'Changes date and time of when a file was created';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/fls/r';
          Caption                : 'Read a file';
          Prompt                 : 'User can read files';
          Description            : 'Read files from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Write=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Write';
          NameSpace              : '/fls/w';
          Caption                : 'Write a file';
          Prompt                 : 'User can write files';
          Description            : 'Write files to Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Delete=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Delete';
          NameSpace              : '/fls/d';
          Caption                : 'Delete a file';
          Prompt                 : 'User can delete files';
          Description            : 'Delete files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Rename=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Rename';
          NameSpace              : '/fls/rn';
          Caption                : 'Rename a file';
          Prompt                 : 'User can rename files';
          Description            : 'Rename files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Copy=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Copy';
          NameSpace              : '/fls/c';
          Caption                : 'Copy a file';
          Prompt                 : 'User can copy files';
          Description            : 'Copy files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Move=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Move';
          NameSpace              : '/fls/m';
          Caption                : 'Move a file';
          Prompt                 : 'User can move files';
          Description            : 'Move files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Import=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Import';
          NameSpace              : '/fls/p';
          Caption                : 'Import a file';
          Prompt                 : 'User can copy file from their cabinet to social networks';
          Description            : 'Copy files from personal folders so social network folders';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Flag=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Flag';
          NameSpace              : '/fls/f';
          Caption                : 'Flag a file';
          Prompt                 : 'User can flag files';
          Description            : 'Flag files from folders in Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
    end;
    Sync=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/sync/r';
          Caption                : 'Read Synchronization data';
          Prompt                 : 'Users must be able to read synchronization data';
          Description            : 'Synchronization data retrieval from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Write=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Write';
          NameSpace              : '/sync/w';
          Caption                : 'Write Synchronization data';
          Prompt                 : 'Users must be able to write synchronization data';
          Description            : 'Synchronization data storage';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
    end;
    Conversation=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/t/r';
            Caption              : 'Read Conversation';
            Prompt               : 'User can read conversations';
            Description          : 'Conversation retrieval for a Network';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Add=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Add';
            NameSpace            : '/t/a';
            Caption              : 'Add to a Conversation';
            Prompt               : 'User can add text to conversations';
            Description          : 'Add to conversations for a Network';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/t/d';
            Caption              : 'Delete Conversation';
            Prompt               : 'User can delete conversations';
            Description          : 'Delete conversations for a Network';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/t/l';
            Caption              : 'List Conversations';
            Prompt               : 'User can list conversations';
            Description          : 'List conversations for a Network';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Flag=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Flag';
            NameSpace            : '/t/f';
            Caption              : 'Flag Conversations';
            Prompt               : 'User can flag conversations';
            Description          : 'Flag conversations for a Network';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Ban=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Ban';
            NameSpace            : '/t/b';
            Caption              : 'Ban Conversationalists';
            Prompt               : 'User can ban conversationalists';
            Description          : 'Ban conversationalists for a Network';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
    end;
    Request=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/r/r';
            Caption              : 'Read Request';
            Prompt               : 'User can read network requests';
            Description          : 'Request retrieval for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Write=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Write';
            NameSpace            : '/r/w';
            Caption              : 'Write Requests';
            Prompt               : 'User can write requests';
            Description          : 'Write requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/r/d';
            Caption              : 'Delete Requests';
            Prompt               : 'User can delete requests';
            Description          : 'Delete requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/r/l';
            Caption              : 'List Requests';
            Prompt               : 'User can list requests';
            Description          : 'List requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Make=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Make';
            NameSpace            : '/r/m';
            Caption              : 'Make Requests';
            Prompt               : 'User can make requests';
            Description          : 'Make requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Invite=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Invite';
            NameSpace            : '/r/i';
            Caption              : 'Invite Requests';
            Prompt               : 'User can make invitational requests';
            Description          : 'Make invitational requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Accept=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Accept';
            NameSpace            : '/r/acc';
            Caption              : 'Accept Requests';
            Prompt               : 'User can accept requests';
            Description          : 'Accept requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Reject=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Reject';
            NameSpace            : '/r/rej';
            Caption              : 'Reject Requests';
            Prompt               : 'User can reject requests';
            Description          : 'Reject requests for Network memberships';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
    end;
    Kind=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
      type
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List Kinds';
            NameSpace            : '/kind/l';
            Caption              : 'List Social Media Kinds';
            Prompt               : 'User can obtain list of supported social media types';
            Description          : 'Lists social media types presently supported';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
    end;
    Search=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Music=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Music';
          NameSpace              : '/srch/music';
          Caption                : 'Search Music Collection';
          Prompt                 : 'User can search music collection';
          Description            : 'Provides music listings from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      type
        Terms=class
        const
          Artist                 = 'a';
          Album                  = 'm';
          Song                   = 's';
          Genre                  = 'gre';
          Group                  = 'grp';
          Tags                   = 't';
        end;
      end;
      Files=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Files';
          NameSpace              : '/srch/files';
          Caption                : 'Search User Files';
          Prompt                 : 'User can search virtual file system';
          Description            : 'Provides file listings from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
    end;
  end;
  TSocialCore=Class(TCoreObject)
  type
    {$i coSocial.TSocialCore.cmTransformVideo.Type.inc}
  private
    FSearchCore                  : TSearchCore;


    FLength                      : LongInt;
    FAuthLength                  : LongInt;
    FAngle                       : double;
    FExt                         : Core.Strings.VarString;
    FContentType                 : Core.Strings.VarString;
    FSourceKind                  : Core.Strings.VarString;
    FContentKind                 : Core.Strings.VarString;
    FETag                        : Core.Strings.VarString;
    FRange                       : Core.Strings.VarString;

    FWrite                       : boolean;
    FCacheExpired                : boolean;
    DataP                        : PHTTP;
    FX                           : LongInt;
    FY                           : LongInt;


    FCR                          : TDateTime;
    FBias                        : Int64;
    FMediaP                      : PHTTPMedia;


    FKPList                      : Core.Arrays.Types.KeyStrings;

    FFolder                      : Storage.Social.Folders.TSFolder;
    FFolders                     : Storage.Social.Folders.TSFolders;
    FUserFile                    : Storage.UserStorage.Files.TItem;
    FRosterItems                 : Storage.Roster.Items.List;

    FFile                        : Storage.Social.Files.TSFile;
    FVideo                       : Storage.Social.Files.TSFile;
    FFiles                       : Storage.Social.Files.TSFiles;
    FFileList                    : Storage.Social.Files.TSFiles;
    FNetwork                     : Storage.Social.Network.TNetwork;
    FNetwork2                    : Storage.Social.Network.TNetwork;
    FNetworks                    : Storage.Social.Network.TNetworks;
    FConnection                  : Storage.Social.Connection.TConnection;
    FConnections                 : Storage.Social.Connection.TConnections;
    FConversation                : Storage.Social.Conversation.TConversation;
    FConversation2               : Storage.Social.Conversation.TConversation;
    FConversations               : Storage.Social.Conversation.TConversations;

    FSync                        : Storage.Social.Sync.THeader;
    FRequest                     : Storage.Social.Network.Requests.TRequest;
    FRequest2                    : Storage.Social.Network.Requests.TRequest;
    FRequests                    : Storage.Social.Network.Requests.TRequests;
    FMedia                       : Storage.Social.Kind.TKinds;

    FFlag                        : Storage.Social.Flags.TFlag;
    FKindP                       : Storage.Social.Kind.PKind;
    FKinds                       : Storage.Social.Kind.TKinds;

    FBann                        : Storage.Social.Bann.TBann;

    FIDList                      : Core.Arrays.Types.LargeWord;

    FMP3Reader                   : Multimedia.MPEG.TReader;
    FMP3Tags                     : Storage.Summary.Music.TItems;
    FMP3Tag                      : Storage.Summary.Music.TItem;

    FGenres                      : Core.Arrays.Types.VarString;
    FArtists                     : Core.Arrays.Types.VarString;
    FGroups                      : Core.Arrays.Types.VarString;
    FAlbums                      : Core.Arrays.Types.VarString;
    FTags                        : Core.Arrays.Types.VarString;

    FPattern                     : Core.Arrays.Types.VarString;



    FFolderID                    : QWord;
    FFileID                      : QWord;
    FNetworkID                   : QWord;

    FPath                        : Core.Strings.VarString;
    FLen                         : Int64;
    FDepth                       : LongInt;
    FUpdateStamp                 : Boolean;

    FTagsAggregate               : Core.Strings.VarString;

    FMP3Summary                  : Storage.Summary.Music.TItems;

    FXMLParser                   : TDOMParser;
    FDocument                    : TXMLDocument;
    FSource                      : TXMLInputSource;
    FNode                        : TDOMNode;
    FExtraRefactor               : TMemoryStream;
    FSFile                       : TFileStream;
    FSVideo                      : TFileStream;
  private
    procedure OnID3TagFrame(ID3:TFrame; Item:TFrame; Stream:TStream; var Handled:Boolean);
  private
    // Kinds
    function  cmdKindList      (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Search Capabilities
    function  cmdSearchFiles(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdSearchMusic(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Synchronization
    function  cmdSyncRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdSyncWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Network
    function  cmdNetworkSearch  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdNetworkRead    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdNetworkWrite   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdNetworkList    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdNetworkAdd     (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdNetworkDelete  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Connections
    function  cmdConnectionRead    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdConnectionList    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdConnectionDelete  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Conversations
    function  cmdTextRead       (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdTextAdd        (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdTextList       (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdTextDelete     (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdTextFlag       (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdTextBan        (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Requests
    function  cmdRequestRead    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestSetResponse (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestList    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestDelete  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestInvite  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestMake    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestAccept  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdRequestReject  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Folders
    function  cmdFoldersList    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersAdd     (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersDelete  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersRename  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersClear   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Files
    function  cmdFilesRead      (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesRotate    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesTransform (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesPalmPrint (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesWrite     (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesSetCreated(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesListWith  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesList      (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesListAll   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesAdd       (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesGetData   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesSetData   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesDelete    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesMove      (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesCopy      (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesRename    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesDownload  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesGet       (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesStream    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesFlag      (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    procedure Started; override;
  protected
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  public
    function  Find(Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Provider:Provider.TItem; var Query:Query.TItem; var Results:Storage.Search.Cache.TItem):WORD; override;
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses DateUtils,Process;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TSocialCore.Install(Task);
end;

class procedure TSocialCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TSocialCore);
  with Social do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    with Kind do begin
      COREOBJECT_VerifyID(Task,List.cmd);
    end;
    with Sync do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
    end;
    with Network do begin
      COREOBJECT_VerifyID(Task,Search.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
    end;
    with Connection do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
    end;
    with Folders do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Rename.cmd);
      COREOBJECT_VerifyID(Task,Clear.cmd);
    end;
    with Files do begin
      COREOBJECT_VerifyID(Task,Rotate.cmd);
      COREOBJECT_VerifyID(Task,Transform.cmd);
      COREOBJECT_VerifyID(Task,PalmPrint.cmd);
      COREOBJECT_VerifyID(Task,Stream.cmd);
      COREOBJECT_VerifyID(Task,Get.cmd);
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,GetData.cmd);
      COREOBJECT_VerifyID(Task,SetData.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,ListWith.cmd);
      COREOBJECT_VerifyID(Task,ListAll.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Move.cmd);
      COREOBJECT_VerifyID(Task,Copy.cmd);
      COREOBJECT_VerifyID(Task,Rename.cmd);
      COREOBJECT_VerifyID(Task,Import.cmd);
      COREOBJECT_VerifyID(Task,Download.cmd);
      COREOBJECT_VerifyID(Task,Flag.cmd);
      COREOBJECT_VerifyID(Task,SetCreated.cmd);
    end;
    with Conversation do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Flag.cmd);
      COREOBJECT_VerifyID(Task,Ban.cmd);
    end;
    with Request do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Make.cmd);
      COREOBJECT_VerifyID(Task,Invite.cmd);
      COREOBJECT_VerifyID(Task,Accept.cmd);
      COREOBJECT_VerifyID(Task,Reject.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
    end;
  end;
end;

class procedure TSocialCore.UnInstall;
begin
  UnRegisterClass(TSocialCore);
end;

procedure TSocialCore.Initialize;
begin
  FSFile:=nil;

  FSearchCore:=TSearchCore(CoreObjects.Find(coSearch.Search.ACLInf.NameSpace));
  FExtraRefactor:=TMemoryStream.Create();
  FXMLParser:=TDOMParser.Create();
  FXMLParser.Options.Validate:=False;
  //FXMLParser.Options.ConformanceLevel:=clFragment;
  FDocument:=nil;
  FSource:=nil;

  Storage.Summary.Music.Init(FMP3Summary);

  Core.Arrays.VarString.Init(FGenres);
  Core.Arrays.VarString.Init(FArtists);
  Core.Arrays.VarString.Init(FGroups);
  Core.Arrays.VarString.Init(FAlbums);
  Core.Arrays.VarString.Init(FTags);

  Core.Arrays.VarString.Init(FPattern);

  FMP3Reader:=Multimedia.MPEG.TReader.Create();
  FMP3Reader.OnTagFrame:=@OnID3TagFrame;

  with Social do begin
    With Kind do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdNetworkList);
    end;
    With Sync do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdSyncRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdSyncWrite);
    end;
    With Network do begin
      Storage.CoreObjects.Add(Search.cmd,FCommands,Header,@cmdNetworkSearch);
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdNetworkRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdNetworkWrite);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdNetworkList);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdNetworkAdd);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdNetworkDelete);
    end;
    With Connection do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdConnectionRead);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdConnectionList);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdConnectionDelete);
    end;
    with Folders do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdFoldersList);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdFoldersAdd);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdFoldersDelete);
      Storage.CoreObjects.Add(Rename.cmd,FCommands,Header,@cmdFoldersRename);
      Storage.CoreObjects.Add(Clear.cmd,FCommands,Header,@cmdFoldersClear);
    end;
    with Files do begin
      Storage.CoreObjects.Add(Rotate.cmd,FCommands,Header,@cmdFilesRotate);
      Storage.CoreObjects.Add(Transform.cmd,FCommands,Header,@cmdFilesTransform);
      Storage.CoreObjects.Add(PalmPrint.cmd,FCommands,Header,@cmdFilesPalmPrint);
      Storage.CoreObjects.Add(Stream.cmd,FCommands,Header,@cmdFilesStream);
      Storage.CoreObjects.Add(Get.cmd,FCommands,Header,@cmdFilesGet);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdFilesList);
      Storage.CoreObjects.Add(ListWith.cmd,FCommands,Header,@cmdFilesListWith);
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdFilesRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdFilesWrite);
      Storage.CoreObjects.Add(GetData.cmd,FCommands,Header,@cmdFilesGetData);
      Storage.CoreObjects.Add(SetData.cmd,FCommands,Header,@cmdFilesSetData);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdFilesAdd);
      Storage.CoreObjects.Add(Rename.cmd,FCommands,Header,@cmdFilesRename);
      Storage.CoreObjects.Add(Move.cmd,FCommands,Header,@cmdFilesMove);
      Storage.CoreObjects.Add(Copy.cmd,FCommands,Header,@cmdFilesCopy);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdFilesDelete);
      Storage.CoreObjects.Add(Download.cmd,FCommands,Header,@cmdFilesDownload);
      Storage.CoreObjects.Add(ListAll.cmd,FCommands,Header,@cmdFilesListAll);
      Storage.CoreObjects.Add(Flag.cmd,FCommands,Header,@cmdFilesFlag);
      Storage.CoreObjects.Add(SetCreated.cmd,FCommands,Header,@cmdFilesSetCreated);
    end;
    with Conversation do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdTextRead);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdTextAdd);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdTextList);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdTextDelete);
      Storage.CoreObjects.Add(Flag.cmd,FCommands,Header,@cmdTextFlag);
      Storage.CoreObjects.Add(Ban.cmd,FCommands,Header,@cmdTextBan);
    end;
    with Request do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdRequestRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdRequestSetResponse);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdRequestList);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdRequestDelete);
      Storage.CoreObjects.Add(Invite.cmd,FCommands,Header,@cmdRequestInvite);
      Storage.CoreObjects.Add(Make.cmd,FCommands,Header,@cmdRequestMake);
      Storage.CoreObjects.Add(Accept.cmd,FCommands,Header,@cmdRequestAccept);
      Storage.CoreObjects.Add(Reject.cmd,FCommands,Header,@cmdRequestReject);
    end;
  end;
end;

procedure TSocialCore.Finalize;
begin
  FreeAndNil(FExtraRefactor);
  FreeAndNil(FXMLParser);
  FreeAndNil(FSource);
  FreeAndNil(FDocument);

  FreeAndNil(FMP3Reader);
  Storage.Summary.Music.Done(FMP3Summary);
  Storage.Summary.Music.Done(FMP3Tags);
  Storage.Summary.Music.Done(FMP3Tag);

  Core.Arrays.VarString.Done(FGenres);
  Core.Arrays.VarString.Done(FArtists);
  Core.Arrays.VarString.Done(FGroups);
  Core.Arrays.VarString.Done(FAlbums);
  Core.Arrays.VarString.Done(FTags);

   Core.Arrays.VarString.Done(FPattern);
end;

procedure TSocialCore.Started;
begin
  Storage.Social.Kind.Verify(FTask,OwnerP^.DomainP^.ID,FMedia);
end;

function TSocialCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Handled:=True;
  DataP:=SR.Info.DataP;
  Result:=CO_STATUS_OK;
  Storage.Social.Sync.Empty(FSync);
  Storage.Social.Files.Empty(FFiles);
  Storage.Social.Files.Empty(FFile);
  Storage.Roster.Items.Empty(FRosterItems);
  Storage.Social.Network.Requests.Empty(FRequests);
  Storage.Social.Folders.Empty(FFolder,Storage.Social.Folders.FREE_FILES);
  Storage.Social.Folders.Empty(FFolders,Storage.Social.Folders.FREE_FILES);
end;

function  TSocialCore.Find(Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Provider:Provider.TItem; var Query:Query.TItem; var Results:Storage.Search.Cache.TItem):WORD;

  procedure PushNetworkSearch;
  var
    iResLen:integer;
  begin
    iResLen:=System.Length(Results.ResultsAsXML);
    If (Results.ID=0) or (Core.Timer.dtUT>Results.Expires)  then begin
      if Storage.Social.Network.Search(FTask,UAP(SR)^.DomainID,FKPList,FNetworks) then begin
        Storage.Social.Network.setIDs(FNetworks,Results.Results);
        Storage.Social.Network.toXML(FNetworks,Transport(SR).Output,XML_HEADER_ON);
        Results.ResultsAsXML:=Core.Streams.toString(Transport(SR).Output);
        Results.WriteBack:=true;
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;

    end else begin
      Core.Streams.Append(Results.ResultsAsXML,iResLen,Transport(SR).Output);
      Result:=CO_STATUS_OK;
      {
      // The search has already been performed and still has valid results that don't require re-query
      if Storage.Social.Network.List(FTask,Criteria.Results,FNetworks) then begin
        Storage.Social.Network.setIDs(FNetworks,Criteria.Results);
        Storage.Social.Network.toXML(FNetworks,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      };
    end;
  end;

begin
  Core.Arrays.KeyString.fromString(FKPList,Query.Term,'=',' ',[soClearList]);

  Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  case Provider.Namespace2 of
    '/n/s' : PushNetworkSearch;
  end;
end;

procedure TSocialCore.OnID3TagFrame(ID3:TFrame; Item:TFrame; Stream:TStream; var Handled:Boolean);
begin
  Storage.Summary.Music.Empty(FMP3Tag);
  FMP3Tag.Kind:=Integer(Item.Kind);
  FMP3Tag.Name:=Item.Header.Name;
  case Item.Kind of
    TFrameKind.fEncodedBy        : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fComments         : FMP3Tag.Value:=Concat( PCommentPayload(Item.Payload.Data)^.Description,' ',PCommentPayload(Item.Payload.Data)^.Text);
    TFrameKind.fYear             : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Year:=FMP3Tag.Value;
    end;
    TFrameKind.fISRC             : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fRecordingDates   : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fTrackNumber      : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.TrackNumber:=FMP3Tag.Value;
    end;
    TFrameKind.fLength           : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fSize             : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fEncodingParams   : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fTitle            : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Album:=FMP3Tag.Value;
    end;
    TFrameKind.fMediaType        : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fOriginalArtist   : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fOriginalfilename : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fOriginalWriter   : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fOriginalReleaseYear : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fFileLicense      : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fComposer         : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Composer:=FMP3Tag.Value;
    end;
    TFrameKind.fConductor        : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fContentType      : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FLength:=System.Length(FMP3Tag.Value);
      if (FLength>2) then begin
        If (FMP3Tag.Value[1]='(') and (FMP3Tag.Value[FLength]=')') then
          FMP3Tag.Value:=System.Copy(FMP3Tag.Value,2,FLength-2);
      end;
      FMP3Tags.Genre:=FMP3Tag.Value;
    end;
    TFrameKind.fCopyrightMessage : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fCompilation      : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fLeadArtist       : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Artist:=FMP3Tag.Value;
    end;
    TFrameKind.fAccompaniment    : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Accompaniment:=FMP3Tag.Value;
    end;
    TFrameKind.fPerformerRefinement : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fModifiedBy          : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fRadioStationOwner   : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPartOfaSet          : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPublisher           : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fContentGroupDescription : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Group:=FMP3Tag.Value;
    end;
    TFrameKind.fTitleDescription    : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Song:=FMP3Tag.Value;
    end;
    TFrameKind.fSubTitleDescription : begin
      FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
      FMP3Tags.Details:=FMP3Tag.Value;
    end;
    TFrameKind.fTextWriter       : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fURL              : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fCommericalInfo   : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fCopyrightInfo    : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fOfficialFileWebpage : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fOfficialArtistWebpage : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fOfficialAudioSourceWebpage : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fStationURL       : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fPaymentURL       : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fPublishersOfficialWebpage   : FMP3Tag.Value:=PURLPayload(Item.Payload.Data)^.URL;
    TFrameKind.fUserDefinedTextInformation  : FMP3Tag.Value:=Concat(PUserURLPayload(Item.Payload.Data)^.Description,' ',PUserURLPayload(Item.Payload.Data)^.URL);
    TFrameKind.fReleaseTime                 : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;

    TFrameKind.fPodcast                     : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPodcastDescription          : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPodcastKeywords             : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPodcastFeed                 : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPodcastID                   : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;

    TFrameKind.fEncodingTime                : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fOriginalReleaseTime         : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fRecordingTime               : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fTaggingTime                 : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fInvolvedPeopleList          : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fMusicianCreditsList         : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fMood                        : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fProducedNotice              : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fAlbumSortOrder              : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fPerformerSortOrder          : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fTitleSortOrder              : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
    TFrameKind.fSetSubTitle                 : FMP3Tag.Value:=PTextPayload(Item.Payload.Data)^.Information;
  end;
  Storage.Summary.Music.Add(FMP3Tag,FMP3Tags);
end;

function  TSocialCore.cmdKindList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Social.Kind.List(FTask,UAP(SR)^.DomainID, FKinds) then begin
      Storage.Social.Kind.toXML(FKinds,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdSearchFiles(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin

  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdSearchMusic(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  procedure  Push_Genre_List;
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and (System.Length(FMP3Summary.Genre)>0) then
              Core.Arrays.VarString.Add(FGenres,FMP3Summary.Genre,[aoCheckForDuplicates]);
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FSearchCore.srchResults.ResultsAsXML:=Core.XML.DB.wrapCDATA(Core.Arrays.VarString.toString(FGenres));
    Result:=CO_STATUS_OK;
  end;

  procedure  Push_ListBy_Genre(var sGenre:Core.Strings.VarString);
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and Core.Strings.SameText(FMP3Summary.Genre,sGenre) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.Social.Files.Add(FFiles[iLcv]^,FFileList);
            end;
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FRefactor.Size:=0;;
    Storage.Social.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
    FRefactor.Position:=0;
    FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);

    FRefactor.Size:=0;
    Result:=CO_STATUS_OK;
  end;


  procedure  Push_Group_List;
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and (System.Length(FMP3Summary.Group)>0) then
              Core.Arrays.VarString.Add(FGroups,FMP3Summary.Group,[aoCheckForDuplicates]);
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FSearchCore.srchResults.ResultsAsXML:=Core.XML.DB.wrapCDATA(Core.Arrays.VarString.toString(FGroups));
    Result:=CO_STATUS_OK;
  end;

  procedure  Push_ListBy_Group(var sGroup:Core.Strings.VarString);
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and Core.Strings.SameText(FMP3Summary.Group,sGroup) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.Social.Files.Add(FFiles[iLcv]^,FFileList);
            end;
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FRefactor.Size:=0;;
    Storage.Social.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
    FRefactor.Position:=0;
    FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);
    FRefactor.Size:=0;
    Result:=CO_STATUS_OK;
  end;


  procedure  Push_Artist_List;
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and (System.Length(FMP3Summary.Artist)>0) then
              Core.Arrays.VarString.Add(FArtists,FMP3Summary.Artist,[aoCheckForDuplicates]);
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FSearchCore.srchResults.ResultsAsXML:=Core.XML.DB.wrapCDATA(Core.Arrays.VarString.toString(FArtists));
    Result:=CO_STATUS_OK;
  end;

  procedure  Push_ListBy_Artist(var sArtist:Core.Strings.VarString);
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and Core.Strings.SameText(FMP3Summary.Artist,sArtist) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.Social.Files.Add(FFiles[iLcv]^,FFileList);
            end;
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FRefactor.Size:=0;;
    Storage.Social.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
    FRefactor.Position:=0;
    FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);
    FRefactor.Size:=0;
    Result:=CO_STATUS_OK;
  end;

  procedure  Push_Album_List;
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and (System.Length(FMP3Summary.Album)>0) then
              Core.Arrays.VarString.Add(FAlbums,FMP3Summary.Album,[aoCheckForDuplicates]);
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FSearchCore.srchResults.ResultsAsXML:=Core.XML.DB.wrapCDATA(Core.Arrays.VarString.toString(FAlbums));
    Result:=CO_STATUS_OK;
  end;

  procedure  Push_ListBy_Album(var sAlbum:Core.Strings.VarString);
  var
    iLcv:integer;
  begin
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        Try
          FSource:=TXMLInputSource.Create(Storage.Summary.Music.Wrap(FFiles[iLcv]^.Summary));
          try
            FXMLParser.Parse(FSource,FDocument);
            FNode:=Core.XML.DB.getNode(FDocument,Storage.Summary.Music.XML.Stanza);
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and Core.Strings.SameText(FMP3Summary.Album,sAlbum) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.Social.Files.Add(FFiles[iLcv]^,FFileList);
            end;
          Finally
            FreeAndNil(FSource);
          end;
        Finally
          FreeAndNil(FDocument);
        end;
      end;
    end;
    FRefactor.Size:=0;;
    Storage.Social.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
    FRefactor.Position:=0;
    FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);
    FRefactor.Size:=0;
    Result:=CO_STATUS_OK;
  end;

  procedure  Push_ListBy_Tags(sTags:Core.Strings.VarString);
  var
    iLcv,tLcv:integer;
  begin
    sTags:=Lowercase(sTags);
    Core.Arrays.VarString.fromString(FTags,sTags,#32,[soClearList]);
    for iLcv:=0 to High(FFiles) do begin
      if System.Length(FFiles[iLcv]^.Summary)>0 then begin
        FTagsAggregate:=Lowercase(FFiles[iLcv]^.Summary);
        for tLcv:=0 to High(FTags) do begin
          if Core.Strings.Pos(FTags[tLcv],FTagsAggregate)>0 then begin
            Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
            Storage.Social.Files.Add(FFiles[iLcv]^,FFileList);
            break;
          end;
        end;
      end;
    end;
    FRefactor.Size:=0;;
    Storage.Social.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
    FRefactor.Position:=0;
    FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);
    FRefactor.Size:=0;
    Result:=CO_STATUS_OK;
  end;

  procedure  ProcessTermAsSingleton;
  begin
    case FSearchCore.srchQuery.Term of
      Social.Search.Music.Terms.Genre  : Push_Genre_List;
      Social.Search.Music.Terms.Group  : Push_Group_List;
      Social.Search.Music.Terms.Artist : Push_Artist_List;
      Social.Search.Music.Terms.Album  : Push_Album_List;
      else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PROPERTY;
    end;
  end;

  procedure ProcessTermAsCriteria;
  begin
    case FSearchCore.srchCriteria[0]^.Key of
      Social.Search.Music.Terms.Genre  : Push_ListBy_Genre(FSearchCore.srchCriteria[0]^.Value);
      Social.Search.Music.Terms.Group  : Push_ListBy_Group(FSearchCore.srchCriteria[0]^.Value);
      Social.Search.Music.Terms.Artist : Push_ListBy_Artist(FSearchCore.srchCriteria[0]^.Value);
      Social.Search.Music.Terms.Album  : Push_ListBy_Album(FSearchCore.srchCriteria[0]^.Value);
      Social.Search.Music.Terms.Tags   : Push_ListBy_Tags(FSearchCore.srchCriteria[0]^.Value);
      else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PROPERTY;
    end;
  end;

begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if FSearchCore.srchQuery.ID<>0 then begin
      FLength:=System.Length(FSearchCore.srchCriteria);
      Core.Arrays.VarString.SetSize(FPattern,3);
      FPattern[0]:='%.mp3';
      FPattern[1]:='%.mp4';
      FPattern[2]:='%.mpeg';
      System.SetLength(FFileList,0);
      Try
        Try
          Storage.Social.Folders.List(FTask, UAP(SR)^.DomainID,FSearchCore.srchQuery.NetworkID,Folders.Defaults.Home.Music,FIDList);
          Storage.Social.Files.List(FTask,UAP(SR)^.DomainID,FSearchCore.srchQuery.NetworkID, FIDList,FFiles,FPattern,Storage.Social.Files.DB.IDs.Modified);
          case FLength of
            0: ProcessTermAsSingleton;
            1: ProcessTermAsCriteria;
          end;
        Finally
          Storage.Social.Files.Empty(FFiles);
        end;
      Finally
        System.SetLength(FFileList,0);
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_EXECUTION;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdSyncRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Sync.fromXML(FXMLDocument,FSync,nil) and (FSync.ResourceID<>0) and (FSync.NetworkID<>0) then begin
      if  Storage.Social.Sync.Read(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FSync.ResourceID,FSync.NetworkID,FSync) then begin
        Storage.Social.Sync.toXML(FSync,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdSyncWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Sync.fromXML(FXMLDocument,FSync,nil) and (FSync.ID<>0) and (FSync.ResourceID<>0) and (FSync.NetworkID<>0) then begin
      if Storage.Social.Sync.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FSync) then begin
        Storage.Social.Sync.toXML(FSync,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdNetworkRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iID:QWord;
  iUID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    iID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (iID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,iID,FNetwork) then begin
        // This is where we should do an acl test for membership
        iUID:=UAP(SR)^.ID;
        if Storage.Social.Network.isMember(iUID,FNetwork) then begin
          Storage.Social.Network.toXML(FNetwork,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdNetworkWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.Social.Network.fromXML(FXMLDocument,FNetwork) and (FNetwork.ID<>0) ) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,FNetwork2) then begin
        if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork2) then begin
          if Storage.Social.Network.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FNetwork) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdNetworkAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.Social.Network.fromXML(FXMLDocument,FNetwork) ) then begin
      if Storage.Social.Network.Add(FTask,FRefactor,UAP(SR)^.AuraNode, UAP(SR)^.DomainID,UAP(SR)^.ID,FNetwork) then begin
        Storage.Social.Network.toXML(FNetwork,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdNetworkSearch(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if FSearchCore.srchQuery.ID<>0 then begin
      FLength:=System.Length(FSearchCore.srchCriteria);

      if Storage.Social.Network.Search(FTask,UAP(SR)^.DomainID,FSearchCore.srchCriteria,FNetworks) then begin
        for iLcv:=0 to High(FNetworks) do
          Core.Arrays.LargeWord.Add(FNetworks[iLcv]^.ID,FSearchCore.srchResults.Results);

        FRefactor.Size:=0;
        Storage.Social.Network.toXML(FNetworks,FRefactor,XML_HEADER_OFF);
        FRefactor.Position:=0;
        FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);
        FRefactor.Size:=0;
        Result:=CO_STATUS_OK;

      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_EXECUTION;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;
{
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    sSearch:=Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldSearch);
    if Storage.Social.Network.Search(FTask,UAP(SR)^.DomainID,sSearch,FNetworks) then begin
      Storage.Social.Network.toXML(FNetworks,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
}

function  TSocialCore.cmdNetworkList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Social.Network.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FNetworks) then begin
      Storage.Social.Network.toXML(FNetworks,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdNetworkDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.fromXML(FXMLDocument,FNetwork) then begin
      if (FNetwork.ID<>0) then begin                                 // DomainID , OwnerID
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,FNetwork2) then begin
          if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork2) then begin
            if Storage.Social.Network.Delete(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork) then begin
              Result:=CO_STATUS_OK
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdConnectionRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.Social.Connection.fromXML(FXMLDocument,FConnection) and (FConnection.ID<>0) and (FConnection.NetworkID<>0) ) then begin
      if Storage.Social.Connection.Read(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FConnection.NetworkID,FConnection.ID,FConnection) then begin
        Storage.Social.Connection.toXML(FConnection,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdConnectionList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Social.Connection.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FConnections) then begin
      Core.Arrays.LargeWord.Empty(FIDList);
      for iLcv:=0 to High(FConnections) do
        Core.Arrays.LargeWord.Add(FConnections[iLcv]^.NetworkID,FIDList);
      if Storage.Social.Network.List(FTask,FIDList,FNetworks) then begin
        Storage.Social.Connection.toXML(FConnections,FNetworks,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdConnectionDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.Social.Connection.fromXML(FXMLDocument,FConnection) and (FConnection.NetworkID<>0) and (FConnection.ID<>0) ) then begin
      if Storage.Social.Connection.Delete(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FConnection) then begin
        Result:=CO_STATUS_OK
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFoldersList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FNetworkID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    FPath:=Core.Arrays.KeyString.GetItemAsString(recvHeaders,fieldNameSpace);
    FDepth:=StrToIntDef(Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldDepth),-1);
    if Length(FPath)>0 then begin
      // List sub folders with starting path
      if (FNetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            // Ok to search system for subitems
            if Storage.Social.Folders.List(FTask,UAP(SR)^.DomainID, FNetworkID,FPath,FFolders) then begin
              if FDepth>-1 then
                Storage.Social.Folders.Purge(FFolders,FDepth);
              Storage.Social.Folders.toXML(FFolders,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
    end else begin
      if (FNetworkID<>0) then begin// network id
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Folders.List(FTask,UAP(SR)^.DomainID, FNetworkID,FFolders) then begin
              if FDepth>-1 then
                Storage.Social.Folders.Purge(FFolders,FDepth);
              Storage.Social.Folders.toXML(FFolders,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFoldersAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Social.Folders.fromXML(FXMLDocument,FFolder) then begin
      if (FFolder.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            FFolder.OwnerID:=UAP(SR)^.ID;
            if Storage.Social.Folders.Fill(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FFolder.Path,FFolder) then begin
              Storage.Social.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else if Storage.Social.Folders.Create(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,UAP(SR)^.ID,FFolder.ID,FFolder.Path) then begin
              Storage.Social.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFoldersDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Folders.fromXML(FXMLDocument,FFolder) and (FFolder.ID<>0) and (FFolder.NetworkID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Folders.Delete(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID,FFolder.ID) then begin
            Result:=CO_STATUS_OK
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFoldersRename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Folders.fromXML(FXMLDocument,FFolder) then begin
      if (FFolder.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Folders.Rename(FTask,UAP(SR)^.DomainID, FNetwork.ID,FFolder) then begin
              Storage.Social.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFoldersClear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Social.Folders.fromXML(FXMLDocument,FFolder) then begin
      if (FFolder.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Clear(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFolder.ID) then begin
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    iID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (iID<>0) then begin // Folder id
      if Storage.Social.Folders.Fill(FTask,UAP(SR)^.DomainID,iID,FFolder) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.List(FTask,UAP(SR)^.DomainID,FFolder.NetworkID,FFolder.ID,FFiles,Storage.Social.Files.DB.IDs.Name) then begin
              Storage.Social.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesListWith(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  Folders:Integer;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    Folders:=Core.Arrays.KeyString.GetItemAsQWordArray(recvHeaders,fieldSearch,FIDList);
    if (Folders>0) then begin
      if ( Storage.Social.Network.fromXML(FXMLDocument,FNetwork) and (FNetwork.ID<>0) ) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.List(FTask,UAP(SR)^.DomainID,FNetwork.ID,FIDList,FFiles,Storage.Social.Files.DB.IDs.Created) then begin
              Folders:=System.Length(FFiles);
              Storage.Social.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
              Storage.Social.Files.Empty(FFiles);
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
      Empty(FIDList);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesListAll(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    iID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (iID<>0) then begin// network id
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,iID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Files.ListAll(FTask,UAP(SR)^.DomainID,iID,FFiles) then begin
            Storage.Social.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
 end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesSetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?----2---?--3---|
  // Parameters from /core/soc?fls/sed?NetworkID?FolderID?FileId
  if SR.Credentials<>nil then begin
    OwnerP^.Manager.RenewCycle();
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      OwnerP^.Manager.RenewCycle();
      FLen:=System.Length(Parameters);
      if (FLen>3) then begin
        FFile.NetworkID:=StrToQWordDef(Parameters[1],0);
        FFile.FolderID:=StrToQWordDef(Parameters[2],0);
        FFile.ID:=StrToQWordDef(Parameters[3],0);
        if ((FFile.NetworkID<>0) and (FFile.FolderID<>0) and (FFile.ID<>0)) then begin
          if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
            if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
              Try
                if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFile.NetworkID,FFile.FolderID,FFile.ID,FFile,FSFile) then begin
                  // Update Stamp if Already Allocated
                  FUpdateStamp:=(FFile.Allocated>Storage.Social.Files.Allocate_Base);
                  if FUpdateStamp=false then
                    FFile.Modified:=FFile.Created;

                  Storage.Social.Files.SetAllocatedStamp(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FFile.ID,Storage.Social.Files.Allocate_Writing);

                  OwnerP^.Manager.RenewCycle();

                  FFile.Allocated:=Core.Timer.dtUT;

                  OwnerP^.Manager.EntryPoint:=Concat('coSocial.cmdFilesSetData.Copy[',IntToStr(FFile.ID),']');
                  Core.Streams.Copy(Transport(SR).Input,FSFile);

                  OwnerP^.Manager.RenewCycle();
                  OwnerP^.Manager.EntryPoint:=Concat('coSocial.cmdFilesSetData.CheckSum[',IntToStr(FFile.ID),']');

                  Core.Streams.CheckSum(Transport(SR).Input,FFile.Digest);
                  OwnerP^.Manager.RenewCycle();
                  Transport(SR).Input.Position:=0;
                  FFile.Size:=FSFile.Size;
                  FExt:=Lowercase(Core.Utils.Files.Extract(FFile.Name,efeoNone));
                  FWrite:=false;
                  case (FExt) of
                    'mov'  : FFile.Kind:=Storage.Social.Files.Kind.Video;
                    'mp4'  : FFile.Kind:=Storage.Social.Files.Kind.Video;
                    'mpg'  : FFile.Kind:=Storage.Social.Files.Kind.Video;
                    'mpeg' : FFile.Kind:=Storage.Social.Files.Kind.Video;
                    'mp3' : begin
                      FFile.Kind:=Storage.Social.Files.Kind.Music;
                      OwnerP^.Manager.EntryPoint:=Concat('coSocial.cmdFilesSetData.Inspect.Music[',IntToStr(FFile.ID),']');
                      Try
                        Storage.Summary.Music.Empty(FMP3Tags);
                        OwnerP^.Manager.EntryPoint:=Concat('coSocial.cmdFilesSetData.Inspect.Music[',IntToStr(FFile.ID),'].LoadFirst');
                        try
                          FMP3Reader.LoadFirst(Transport(SR).Input);
                          FMP3Tags.Inspected:=true;
                        except
                          on E:Exception do Core.Logging.Native.WriteLogEntry(OwnerP^.DomainP^.Name,OwnerP^.Manager.Service,Concat(OwnerP^.Manager.EntryPoint,' Exception: ',E.Message));
                        end;
                        OwnerP^.Manager.EntryPoint:=Concat('coSocial.cmdFilesSetData.Inspect.Music[',IntToStr(FFile.ID),'].toXML');
                        FFile.Summary:=Storage.Summary.Music.toXML(FMP3Tags,FRefactor);
                      Finally
                        OwnerP^.Manager.EntryPoint:=Concat('coSocial.cmdFilesSetData.Inspect.Music[',IntToStr(FFile.ID),'].Empty');
                        Storage.Summary.Music.Empty(FMP3Tags);
                      end;
                    end;
                  end;
                  OwnerP^.Manager.RenewCycle();

                  Storage.Social.Files.Write(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFile.NetworkID,FFile,FUpdateStamp);
                  Storage.Social.Sync.setModified(FTask,UAP(SR)^.DomainID,FFile.NetworkID,dtUT);

                  Transport(SR).ContentType:=ctXML;
                  Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
                  Result:=CO_STATUS_OK;
                end else begin
                  Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
                  Transport(SR).ContentType:=ctXML;
                  Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
                end;
              finally
                FSFile.Free();
              end;
            end else begin
              Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
              Transport(SR).ContentType:=ctXML;
              Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
            end;
          end else begin
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            Transport(SR).ContentType:=ctXML;
            Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          end;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
          Transport(SR).ContentType:=ctXML;
          Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesGetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLen       : LongInt;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0--?----1----?---2----? 3
  // Parameters from /core/soc?fls/ged?NetworkID?FolderID?FileId
  if SR.Credentials<>nil then begin
    iLen:=System.Length(Parameters);
    if (iLen>3) then begin
      Storage.Social.Files.Empty(FFile);
      FFile.NetworkID:=StrToQWordDef(Parameters[1],0);
      FFile.FolderID:=StrToQWordDef(Parameters[2],0);
      FFile.ID:=StrToQWordDef(Parameters[3],0);
      if (FFile.FolderID<>0) and (FFile.ID<>0)  and (FFile.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFile.FolderID,FFile.ID,FFile,FSFile) then begin
              Try
                Transport(SR).ContentType:=ctStream;
                Core.Streams.Copy(FSFile,Transport(SR).Output);
                Storage.Social.Files.Empty(FFile);
                Result:=CO_STATUS_OK;
              finally
                FSFile.Free();
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;
function  TSocialCore.cmdFilesAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      if ( Storage.Social.Files.fromXML(FXMLDocument,FFile) and (FFile.FolderID<>0) and (FFile.NetworkID<>0) ) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if ( Storage.Social.Files.Exists(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FFile.FolderID,FFile.Name)=true) then begin
              if Storage.Social.Files.Fill(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FFile.FolderID,FFile.Name,FFile) then begin
                Result:=CO_STATUS_OK;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else begin
              if Storage.Social.Files.Add(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFile.NetworkID,UAP(SR)^.ID,FFile,nil) then begin
                Result:=CO_STATUS_OK;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end;
            Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TSocialCore.cmdFilesDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
  FileP:Storage.Social.Files.PSFile;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if  (Storage.Social.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
      Storage.Social.Network.Empty(FNetwork);
      for iLcv:=0 to High(FFiles) do begin
        FileP:=FFiles[iLcv];
        if (FNetwork.ID<>FileP^.NetworkID)  then
          Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FileP^.NetworkID,FNetwork);
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          Storage.Social.Files.Move(FTask,FNetwork.Node,UAP(SR)^.DomainID, FileP^.NetworkID, FileP^.ID,FileP^.FolderID, FNetwork.TrashID);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end;
      Result:=CO_STATUS_OK;
      Storage.Social.Files.Empty(FFiles);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesDownload(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLen       : LongInt;
  sVal       : Core.Strings.VarString;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0--?----1----?---2----? 3
  // Parameters from /core/soc?fls/dl?NetworkID?FolderID?FileId
  if SR.Credentials<>nil then begin
    iLen:=System.Length(Parameters);
    if (iLen>3) then begin
      Storage.Social.Files.Empty(FFile);
      FFile.NetworkID:=StrToQWordDef(Parameters[1],0);
      FFile.FolderID:=StrToQWordDef(Parameters[2],0);
      FFile.ID:=StrToQWordDef(Parameters[3],0);
      if (FFile.FolderID<>0) and (FFile.ID<>0)  and (FFile.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFile.FolderID,FFile.ID,FFile,FSFile) then begin
              Try
                Transport(SR).CacheResponse:=FFile.Modified;
                if Transport(SR).CacheRequest<>FFile.Modified then begin
                  Transport(SR).CacheDate:=FFile.Modified;
                  Transport(SR).CacheTag:=MD5Print(FFile.Digest);
                  Transport(SR).CacheExposure:=PRIVATE_CACHE;
                  Transport(SR).ContentType:=ctStream;

                  sVal:=Concat('attachment; filename=',FFile.Name);
                  Core.Arrays.KeyString.Update(respHeaders,fieldContentDisposition,sVal);

                  Core.Streams.Copy(FSFile,Transport(SR).Output);
                end;
                Storage.Social.Files.Empty(FFile);
                Result:=CO_STATUS_OK;
              Finally
                FSFile.Free();
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure OutputFile;
  begin
    Transport(SR).CacheResponse:=FFile.Modified;
    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=MDPrint(FFile.Digest);

    FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));

    FCR:=Transport(SR).CacheRequest;
    FBias:=DateUtils.MilliSecondsBetween(FCR,FFile.Modified);

    if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
      FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
      FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
      if RSR.HTTP.IndexOf(FContentType,ctMedia)<>-1 then begin
        if FMediaP=nil then begin
          new(FMediaP);
          Init(FMediaP^);
          DataP^.Media:=FMediaP;
        end else
          Empty(FMediaP^);

        FMediaP^.ContentType:=FContentTYpe;
        FMediaP^.ETag:=Transport(SR).CacheTag;
        FMediaP^.Modified:=FFile.Modified;
        FMediaP^.ContentLength:=FSFile.Size;
        Core.Streams.Copy(FSFile,FMediaP^.Content);

        Transport(SR).ContentType:=FContentType;
        Transport(SR).Media:=FMediaP;
        Result:=CO_STATUS_OK;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
      end;
    end else begin
      Result:=CO_STATUS_OK;
    end;
    Storage.Social.Files.Empty(FFile);
  end;

  procedure OutputMedia;
  begin
    Transport(SR).Media:=FMediaP;
    Transport(SR).ContentType:=FMediaP^.ContentType;
    Transport(SR).CacheResponse:=FMediaP^.Modified;
    Transport(SR).CacheDate:=FMediaP^.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=FMediaP^.ETag;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?----2---?--3---?-4--|
  // Parameters from /core/soc?fls/get&NetworkID&FolderID&FileId&Auth|
  FLength:=System.Length(Parameters);
  if ( System.Length(DataP^.Auth)=0) then begin
    if (FLength>4) then begin
      DataP^.Auth:=Parameters[4];
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end;
  if ( (System.Length(DataP^.Auth)>0) and  (SR.Credentials<>nil)) then begin
    FMediaP:=DataP^.Media;
    if (FLength>3) then begin
      FNetworkID:=StrToQwordDef(Parameters[1],0);
      FFolderID:=StrToQwordDef(Parameters[2],0);
      FFileID:=StrToQwordDef(Parameters[3],0);
      FETag:=Core.Arrays.KeyString.GetItemAsString(recvHeaders,fieldIfRange);
      if (FFolderID<>0) and (FFileID<>0)  and (FNetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetworkID,FNetwork) then begin
          if (FNetwork.AvatarID=FFileID) or Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if (Length(FETag)>0) then begin
              if (FMediaP<>nil) and (FMediaP^.ETag=FETag) then begin
                OutputMedia();
              end else if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFolderID,FFileID,FFile,FSFile) then begin
                Try
                  If (FFile.Allocated>Storage.Social.Files.Allocate_Base) then begin
                    OutputFile();
                  end else begin
                    Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                  end;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else begin
              if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFolderID,FFileID,FFile,FSFile) then begin
                Try
                  If (FFile.Allocated>Storage.Social.Files.Allocate_Base) then begin
                    OutputFile();
                  end else begin
                    Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                  end;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TSocialCore.cmdFilesStream(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure OutputFile;
  begin
    Transport(SR).CacheResponse:=FFile.Modified;
    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=MDPrint(FFile.Digest);

    FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
    FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);

    if RSR.HTTP.IndexOf(FContentType,ctMedia)<>-1 then begin
      if FMediaP=nil then begin
        new(FMediaP);
        Init(FMediaP^);
        DataP^.Media:=FMediaP;
      end else
        Empty(FMediaP^);
      FMediaP^.ContentType:=FContentType;
      FMediaP^.ETag:=Transport(SR).CacheTag;
      FMediaP^.Modified:=FFile.Modified;
      FMediaP^.ContentLength:=FSFile.Size;
      Core.Streams.Copy(FSFile,FMediaP^.Content);
      Transport(SR).ContentType:=FContentType;
      Transport(SR).Media:=FMediaP;

      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
    end;
    Storage.Social.Files.Empty(FFile);
  end;

  procedure OutputMedia;
  begin
    Transport(SR).Media:=FMediaP;
    Transport(SR).CacheResponse:=FMediaP^.Modified;
    Transport(SR).ContentType:=FMediaP^.ContentType;
    Transport(SR).CacheResponse:=FMediaP^.Modified;
    Transport(SR).CacheDate:=FMediaP^.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=FMediaP^.ETag;

    Result:=CO_STATUS_OK;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0----?----1----?---2----?--3---?-4--?
  // Parameters from /core/soc?fls/strm?NetworkID?FolderID?FileId?Auth?
  // Parameters from /core/soc?fls/strm?NetworkID?FolderID?FileId?
  FLength:=System.Length(Parameters);
  if ( System.Length(DataP^.Auth)=0) then begin
    if (FLength>4) then begin
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        DataP^.Auth:=Parameters[4];
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end;
  if (System.Length(DataP^.Auth)>0) then begin
    if (FLength>3) then begin
      FMediaP:=DataP^.Media;
      FRange:=Core.Arrays.KeyString.GetItemAsString(recvHeaders,fieldRange);
      FETag:=Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldETag);
      FNetworkID:=StrToQwordDef(Parameters[1],0);
      FFolderID:=StrToQwordDef(Parameters[2],0);
      FFileID:=StrToQwordDef(Parameters[3],0);
      if (FFolderID<>0) and (FFileID<>0)  and (FNetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetworkID,FNetwork) then begin
          if (FNetwork.AvatarID=FFileID) or Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if ( (Length(FRange)>0) or (Length(FETag)>0) ) then begin
              if (FMediaP<>nil) and (FMediaP^.ETag=FETag) then begin
                OutputMedia();
              end else if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFolderID,FFileID,FFile,FSFile) then begin
                Try
                  If (FFile.Allocated>Storage.Social.Files.Allocate_Base) then begin
                    OutputFile();
                  end else begin
                    Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                  end;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else begin
              if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFolderID,FFileID,FFile,FSFile) then begin
                Try
                  If (FFile.Allocated>Storage.Social.Files.Allocate_Base) then begin
                    OutputFile();
                  end else begin
                    Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                  end;
                finally
                  FSFile.Free();
                end;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TSocialCore.cmdFilesRotate (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure ConvertFile;
  begin
    Core.Streams.Copy(FSFile,FRefactor);
    If Multimedia.Image.Tool.Rotate(FRefactor,FAngle) then begin
      Core.Streams.Copy(FRefactor,FSFile);
      Core.Streams.CheckSum(FRefactor,FFile.Digest);
      FFile.Size:=FRefactor.Size;
      Storage.Social.Files.SetDigest(FTask,UAP(SR)^.DomainID, FFile.NetworkID, FFile.ID,FFile.Size,FFile.Digest);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
    FRefactor.Size:=0;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?---2----?--3---?-4--?
  // Parameters from /core/vdm?fls/rot?NetworkID?FolderID?FileId?angle // what a
  if SR.Credentials<>nil then begin
    if Length(Parameters)>4 then begin
      Storage.Social.Files.Empty(FFile);
      FAngle:=StrToFloatDef(Parameters[4],90);
      FFile.NetworkID:=StrToQwordDef(Parameters[1],0);
      FFile.FolderID:=StrToQwordDef(Parameters[2],0);
      FFile.ID:=StrToQwordDef(Parameters[3],0);
      if (FFile.FolderID<>0) and (FFile.ID<>0)  and (FFile.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFile.FolderID,FFile.ID,FFile,FSFile) then begin
              Try
                If (FFile.Allocated>Storage.Social.Files.Allocate_Base) then begin
                  FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
                  FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
                  if RSR.HTTP.IndexOf(FContentType,ctImage)<>-1 then
                    ConvertFile()
                  else
                    Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
                end else begin
                  Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                end;
              finally
                if FSFile<>nil then
                  FSFile.Free();
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesTransform (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure OutputPicture;
  begin
    Transport(SR).CacheResponse:=FFile.Modified;
    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheValidation:=MUST_REVALIDATE;
    Transport(SR).CacheTag:=MDPrint(FFile.Digest);

    FCacheExpired:=( (Transport(SR).ETagRequested=false) or (Transport(SR).ETagRequest<>Transport(SR).CacheTag) );

    FCR:=Transport(SR).CacheRequest;
    FBias:=DateUtils.MilliSecondsBetween(FCR,FFile.Modified);

    if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
      Core.Streams.Copy(FSFile,FRefactor);
      if (RSR.HTTP.IndexOf(FContentType,ctImage)<>-1) then begin
        FSourceKind:=Multimedia.Image.Tool.Kind.fromString(FExt);
        FX:=1024;
        FY:=768;
        If Multimedia.Image.Tool.Transform(FRefactor,FX,FY) then begin
          Transport(SR).ContentType:=FContentType;
          Core.Streams.Copy(FRefactor,Transport(SR).Output);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
      FRefactor.Size:=0;
    end else begin
      Result:=CO_STATUS_OK;
    end;
    Storage.Social.Files.Empty(FFile);
  end;

  procedure ConvertVideo;
  begin
    Storage.Social.Files.Empty(FVideo);
    FVideo.Name:=SysUtils.ChangeFileExt(FFile.Name,'.mp4');
    if Storage.Social.Files.Force(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, FNetwork.ID, FFile.FolderID,UAP(SR)^.ID,Storage.Social.Files.Kind.Video,FFile.Created,FVideo.Name,FVideo,FSVideo) then begin
      Try
        OwnerP^.Manager.AddMethod(cmTransformVideo.Create(OwnerP^.Manager, @SR, FNetwork,UAP(SR)^.DomainID, UAP(SR)^.ID,FFile,FVideo,FSFile.FileName,FSVideo.FileName));
        Storage.Social.Files.toXML(FVideo,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      finally
        FSVideo.Free();
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    Storage.Social.Files.Empty(FVideo);
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---&----1----&----2---&--3---&-4--|
  // Parameters from /core/soc?fls/get?NetworkID?FolderID?FileId?Auth|
  FLength:=System.Length(Parameters);
  FAuthLength:=System.Length(DataP^.Auth);
  if ( FAuthLength=0) then begin
    if (FLength>4) then begin
      DataP^.Auth:=Parameters[4];
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
        FAuthLength:=System.Length(DataP^.Auth);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        exit;
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
      exit;
    end;
  end;
  if ( (FAuthLength>0) and  (SR.Credentials<>nil)) then begin
    if (FLength>3) then begin
      Storage.Social.Files.Empty(FFile);
      FFile.NetworkID:=StrToQwordDef(Parameters[1],0);
      FFile.FolderID:=StrToQwordDef(Parameters[2],0);
      FFile.ID:=StrToQwordDef(Parameters[3],0);
      if (FFile.FolderID<>0) and (FFile.ID<>0)  and (FFile.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if (FNetwork.AvatarID=FFile.ID) or Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFile.FolderID,FFile.ID,FFile,FSFile) then begin
              Try
                If (FFile.Allocated>Storage.Social.Files.Allocate_Base) then begin
                  FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
                  FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
                  if RSR.HTTP.IndexOf(FContentType,ctImage)<>-1 then begin
                    OutputPicture();
                  end else if RSR.HTTP.IndexOf(FContentType,ctVideoTransforms)<>-1 then begin
                    ConvertVideo();
                  end else begin
                    Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
                  end;
                end else begin
                  Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
                end;
              finally
                if FSFile<>nil then
                  FSFile.Free();
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TSocialCore.cmdFilesPalmPrint (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure OutputPicture;
  begin
    Transport(SR).CacheResponse:=FFile.Modified;
    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=MDPrint(FFile.Digest);

    FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));

    FCR:=Transport(SR).CacheRequest;
    FBias:=DateUtils.MilliSecondsBetween(FCR,FFile.Modified);

    if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
      Core.Streams.Copy(FSFile,FRefactor);
      if RSR.HTTP.IndexOf(FContentType,ctImage)<>-1 then begin
        FSourceKind:=Multimedia.Image.Tool.Kind.fromString(FExt);
        FContentKind:=FSourceKind;
        FX:=512;
        FY:=384;
        If Multimedia.Image.Tool.Transform(FRefactor,FX,FY) then begin
          Transport(SR).ContentType:=FContentType;
          Core.Streams.Copy(FRefactor,Transport(SR).Output);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
      FRefactor.Size:=0;
    end else begin
      Result:=CO_STATUS_OK;
    end;
    Storage.Social.Files.Empty(FFile);
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?----2---?--3---?-4--|
  // Parameters from /core/soc?fls/get?NetworkID?FolderID?FileId?Auth|
  FLength:=System.Length(Parameters);
  FAuthLength:=System.Length(DataP^.Auth);
  if ( FAuthLength=0) then begin
    if (FLength>4) then begin
      DataP^.Auth:=Parameters[4];
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
        FAuthLength:=System.Length(DataP^.Auth);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        exit;
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
      exit;
    end;
  end;
  if ( (FAuthLength>0) and  (SR.Credentials<>nil)) then begin
    if (FLength>3) then begin
      Storage.Social.Files.Empty(FFile);
      FFile.NetworkID:=StrToQwordDef(Parameters[1],0);
      FFile.FolderID:=StrToQwordDef(Parameters[2],0);
      FFile.ID:=StrToQwordDef(Parameters[3],0);
      if (FFile.FolderID<>0) and (FFile.ID<>0)  and (FFile.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if (FNetwork.AvatarID=FFile.ID) or Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFile.FolderID,FFile.ID,FFile,FSFile) then begin
              Try
                FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
                FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
                if RSR.HTTP.IndexOf(FContentType,ctImage)<>-1 then begin
                  OutputPicture();
                end else begin
                  Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
                end;
              finally
                if FSFile<>nil then
                  FSFile.Free();
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;


function  TSocialCore.cmdFilesFlag(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin

  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    Storage.Social.Files.Empty(FFile);
    if  ( Storage.Social.Files.fromXML(FXMLDocument,FFile) and (FFile.ID<>0) and (FFile.NetworkID<>0) and (FFile.FolderID<>0)) then begin
      Storage.Social.Network.Empty(FNetwork);
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) and  (FNetwork.ID<>0) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Files.Fill(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID,FFile.FolderID,FFile.ID,FFile,FSFile) then begin
            FSFile.Free();
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
    Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
    Storage.Social.Files.Empty(FFile);
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesSetCreated(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  bDenied,bFiles:Boolean;
  iLcv:integer;
  FileP:Storage.Social.Files.PSFile;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  ( Storage.Social.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
      bDenied:=false;
      bFiles:=false; Storage.Social.Network.Empty(FNetwork);
      for iLcv:=0 to High(FFiles) do begin
        FileP:=FFiles[iLcv];
        if (FileP^.ID<>0) and (FileP^.NetworkID<>0) and (FileP^.FolderID<>0) then begin
          if (FNetwork.ID<>FileP^.NetworkID) then
            Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FileP^.NetworkID,FNetwork);
          if (FNetwork.ID<>0) then begin
            if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
              Storage.Social.Files.SetCreatedStamp(FTask,UAP(SR)^.DomainID, FNetwork.ID,FileP^.ID,FileP^.Created);
              bFiles:=true;
            end else
              bDenied:=true;
          end;
        end;
      end;
      if bFiles then begin
        Storage.Social.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else if bDenied then
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED
      else
        Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    OwnerP^.Manager.RenewCycle();
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      if ( Storage.Social.Files.fromXML(FXMLDocument,FFile) and (FFile.NetworkID<>0) and (FFile.FolderID<>0) and (FFile.ID<>0)) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if (SR.Throttle.Enabled) and (SR.Throttle.Consumption>=SR.Throttle.Limit) and (SR.RecvBuffer.posWrite>BUFFER_THRESHOLD)then begin
              Result:=CO_STATUS_BUFFERS_EXEEDED;
              Transport(SR).ContentType:=ctXML;
              Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Exit;
            end;
            FFile.Allocated:=2; // Refreshed content - good for re-writes
            FUpdateStamp:=true;
            if Storage.Social.Files.Write(FTask,FNetwork.Node,UAP(SR)^.DomainID, FNetwork.ID, FFile,FUpdateStamp) then begin
              Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else begin
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
              Transport(SR).ContentType:=ctXML;
              Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
            end;
          end else begin
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
            Transport(SR).ContentType:=ctXML;
            Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          end;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          Transport(SR).ContentType:=ctXML;
          Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesMove(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  bMoved   : boolean;
  bDenied  : boolean;
  iLcv     : LongInt;
  FolderID : QWord;
  FileP    : Storage.Social.Files.PSFile;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FolderID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FolderID>0) then begin
      if  ( Storage.Social.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
        Storage.Social.Network.Empty(FNetwork);
        bMoved:=false; bDenied:=false;
        for iLcv:=0 to High(FFiles) do begin
          FileP:=FFiles[iLcv];
          if (FNetwork.ID<>FileP^.NetworkID) and (FileP^.NetworkID<>0) then
            Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FileP^.NetworkID,FNetwork);
          if (FNetwork.ID<>0) then begin
            if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
              bMoved:=true;
              Storage.Social.Files.Move(FTask,FNetwork.Node,UAP(SR)^.DomainID,FNetwork.ID,FFiles[iLcv]^.ID,FFiles[iLcv]^.FolderID,FolderID);
            end else
              bDenied:=true;
          end;
        end;
        if bMoved then begin
          Result:=CO_STATUS_OK;
        end else if bDenied then
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED
        else
          Result:=CO_STATUS_OK;
        Storage.Social.Files.Empty(FFiles);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesCopy(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Files.fromXML(FXMLDocument,FFile) then begin
      if (FFile.NetworkID<>0) and (FFile.FolderID<>0) and (FFile.ID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Copy(FTask,FNetwork.Node,UAP(SR)^.DomainID,FFile.NetworkID,UAP(SR)^.ID,FFile.FolderID,FFile.ID,FFile.ID) then begin
              Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdFilesRename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Files.fromXML(FXMLDocument,FFile) then begin
      if (FFile.NetworkID<>0) then begin
        if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FNetwork) then begin
          if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
            if Storage.Social.Files.Rename(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FFile) then begin
              Storage.Social.Files.Fill(FTask,UAP(SR)^.DomainID,FFile.NetworkID,FFile.ID,FFile);
              Storage.Social.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TSocialCore.cmdTextRead   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Conversation.fromXML(FXMLDocument,FConversation) and (FConversation.ID<>0) and (FConversation.NetworkID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Conversation.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FConversation.ID,FConversation) then begin
            Storage.Social.Conversation.toXML(FConversation,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdTextAdd  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Conversation.fromXML(FXMLDocument,FConversation) and (FConversation.NetworkID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Conversation.Add(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,UAP(SR)^.ID,FConversation) then begin
            Storage.Social.Conversation.toXML(FConversation,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdTextList   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    iID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if iID<>0 then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,iID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Conversation.List(FTask,UAP(SR)^.DomainID,iID,FConversations) then begin
            Storage.Social.Conversation.toXML(FConversations,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdTextDelete (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Conversation.fromXML(FXMLDocument,FConversation) and (FConversation.NetworkID<>0) and (FConversation.ID<>0) then begin
      if Storage.Social.Conversation.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FConversation.ID,FConversation2) then begin
        if (FConversation2.ID=FConversation.ID) and (FConversation2.NetworkID=FConversation.NetworkID) then begin
          if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FNetwork) then begin
            if ( (FConversation2.UserID=UAP(SR)^.ID) or Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork)  ) then begin
                if Storage.Social.Conversation.Delete(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FConversation) then begin
                  Storage.Social.Conversation.toXML(FConversation,Transport(SR).Output,XML_HEADER_ON);
                  Result:=CO_STATUS_OK;
                end else
                  Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdTextFlag   (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Conversation.fromXML(FXMLDocument,FConversation) and (FConversation.NetworkID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isMember(UAP(SR)^.ID,FNetwork) then begin
          // We need to set MediaP
          FKindP:=Storage.Social.Kind.getItem(Storage.Social.Kind.NameSpace.Text,FMedia);
          if FKindP<>nil then begin
            if Storage.Social.Flags.Add(FTask,UAP(SR)^.DomainID,FNetwork.ID,UAP(SR)^.ID,FConversation.ID,FKindP^.ID,FFlag) then
              Result:=CO_STATUS_OK
            else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_MEDIA_NOT_FOUND;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdTextBan    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Bann.fromXML(FXMLDocument,FBann) and (FBann.NetworkID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FBann.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Bann.Add(FTask,UAP(SR)^.DomainID,FBann.NetworkID,UAP(SR)^.ID,FBann.UserID,FBann.Days,FBann) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestRead    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) and (FRequest.ID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FNetwork) then begin
        FRequest.QueryID:=Storage.Social.Network.Requests.getQueryID(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FRequest.ID);
        if (FRequest.QueryID=UAP(SR)^.ID) or Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Network.Requests.Read(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FRequest.ID,FRequest) then begin
            Storage.Social.Network.Requests.toXML(FRequest,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestSetResponse (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) and (FRequest.ID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
          FRequest.Flags:=FRequest.Flags or Storage.Social.Network.Requests.getFlags(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.ID);
          if Storage.Social.Network.Requests.SetResponse(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.ID,UAP(SR)^.ID,FRequest.Flags,FRequest.Response) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestList    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FNetwork.ID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FNetwork.ID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,FNetwork) then begin
        if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Network.Requests.List(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequests) then begin
            if System.Length(FRequests)>0 then begin
              if Storage.Social.Network.Members.List(FTask,UAP(SR)^.DomainID,FRequests,FRosterItems) then begin
                Storage.Social.Network.Requests.toXML(FRequests,FRosterItems,Transport(SR).Output,XML_HEADER_ON);
                Result:=CO_STATUS_OK;
              end else
                Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end else begin
              Result:=CO_STATUS_OK;
              Storage.Social.Network.Requests.toXML(FRequests,FRosterItems,Transport(SR).Output,XML_HEADER_ON);
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestDelete  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) and (FRequest.ID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FNetwork) then begin
        FRequest.QueryID:=Storage.Social.Network.Requests.getQueryID(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.ID);
        if (FRequest.QueryID=UAP(SR)^.ID) or Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Network.Requests.Delete(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.ID) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestMake    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FNetwork) then begin
        if Storage.Social.Network.Requests.Make(FTask,UAP(SR)^.DomainID,FNetwork.ID,UAP(SR)^.ID,FRequest) then begin
          Storage.Social.Network.Requests.toXML(FRequest,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestInvite    (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) and (FRequest.QueryID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FConversation.NetworkID,FNetwork) then begin
        if Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) then begin
          if Storage.Social.Network.Requests.Invite(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.QueryID,UAP(SR)^.ID,FRequest) then begin
            Storage.Social.Network.Requests.toXML(FRequest,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSocialCore.cmdRequestAccept  (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) and (FRequest.ID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FNetwork) then begin
        if Storage.Social.Network.Requests.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.ID,FRequest2) then begin
          If (
            ( FRequest2.QueryID=FRequest.QueryID) and
            ( Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) )
            ) then begin
            if Storage.Social.Network.Requests.Accept(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FRequest) then begin
              Storage.Social.Network.Requests.toXML(FRequest,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TSocialCore.cmdRequestReject (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Social.Network.Requests.fromXML(FXMLDocument,FRequest) and (FRequest.NetworkID<>0) and (FRequest.ID<>0) then begin
      if Storage.Social.Network.Read(FTask,UAP(SR)^.DomainID,FRequest.NetworkID,FNetwork) then begin
        if Storage.Social.Network.Requests.Read(FTask,UAP(SR)^.DomainID,FNetwork.ID,FRequest.ID,FRequest2) then begin
          If (
            ( FRequest2.QueryID=FRequest.QueryID) and
            ( Storage.Social.Network.isAdmin(UAP(SR)^.ID,FNetwork) )
            ) then begin
            if Storage.Social.Network.Requests.Reject(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FRequest) then begin
              Storage.Social.Network.Requests.toXML(FRequest,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

{$i coSocial.TSocialCore.cmTransformVideo.Code.inc}
end.

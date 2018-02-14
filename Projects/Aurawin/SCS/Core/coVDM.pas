unit coVDM;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface
uses
  Classes,

  App.Consts,

  RSR,
  RSR.HTTP,
  RSR.Core,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  uHTTPd,
  hHTTPd,

  Core.Timer,
  Core.XML,
  Core.Keywords,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,

  Core.Logging,
  Core.Streams,
  Core.Utils.Files,


  Multimedia.Image,
  Multimedia.MPEG,

  coSearch,

  Encryption.Base64,

  Storage,
  Storage.Main,
  Storage.VDM,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.UserStorage,
  Storage.ContentTypes,
  Storage.Search,
  Storage.FAT,
  Storage.Summary,

  md5,
  DOM,
  XMLRead,
  SysUtils;

type
  VDM=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'VDM';
      NameSpace                  : '/core/vdm';
      Caption                    : 'Virtual Desktop Manager Core Object';
      Prompt                     : 'User can access the VDM system';
      Description                : 'System wide application to host virtual desktops'
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TVDMCore';
      Location                   : 'coVDM.pas';
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
    State=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : false;
      );
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/ste/r';
            Caption              : 'Read VDM State';
            Prompt               : 'User can read the vdm state';
            Description          : 'VDM state retrieval from Database';
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
        Write=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Write';
            NameSpace            : '/ste/w';
            Caption              : 'Write VDM State';
            Prompt               : 'User can write their vdm state';
            Description          : 'Write VDM state to Database for document reloading';
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
            Description          : 'Retrieve user folders from Database';
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
            Description          : 'Add user folders to Database';
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
            NameSpace            : '/fldrs/d';
            Caption              : 'Delete a folder';
            Prompt               : 'User can delete folders';
            Description          : 'Delete user folders from Database';
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
            Description          : 'Delete all user files in a folder from Database';
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
            Description          : 'Rename user folders in Database';
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
            Name                 : 'List';
            NameSpace            : '/fls/l';
            Caption              : 'List of Files';
            Prompt               : 'User can obtain a list files';
            Description          : 'Retrieve user files from folders in Database';
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
        ListAll=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List All';
            NameSpace            : '/fls/la';
            Caption              : 'List All Files';
            Prompt               : 'User can obtain a list of all files';
            Description          : 'Retrieve all user files from folders in Database';
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
        ListWith=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List With';
            NameSpace            : '/fls/lw';
            Caption              : 'List Files within Folders';
            Prompt               : 'User can obtain a list of files';
            Description          : 'Retrieve user files from folders in Database';
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
            NameSpace            : '/fls/a';
            Caption              : 'Add a new file';
            Prompt               : 'User can create files';
            Description          : 'Add user files to folders in Database';
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
        SetCreated=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Set Created';
            NameSpace            : '/fls/sc';
            Caption              : 'Set created date time stamp';
            Prompt               : 'User can change the value of when a file was created';
            Description          : 'Changes date and time of when a file was created';
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
        Download=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Download';
            NameSpace            : '/fls/dl';
            Caption              : 'Download a file';
            Prompt               : 'User can download files';
            Description          : 'Download user files from Database';
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
        Get=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Get';
            NameSpace            : '/fls/get';
            Caption              : 'Access a file via http get';
            Prompt               : 'User can access a file over the web';
            Description          : 'Host user files from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : true;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        GetData=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Get Data';
            NameSpace            : '/fls/ged';
            Caption              : 'Retrieve raw file data';
            Prompt               : 'User can access raw file data';
            Description          : 'Retrieves user files from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        SetData=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Set Data';
            NameSpace            : '/fls/sed';
            Caption              : 'Write raw file data';
            Prompt               : 'User can write raw file data';
            Description          : 'Writes user files from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Stream=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Stream';
            NameSpace            : '/fls/strm';
            Caption              : 'Stream a file via http get';
            Prompt               : 'User can stream a file over the web';
            Description          : 'Stream user files from Database';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : true;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
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
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Transform=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Transform';
            NameSpace            : '/fls/tfm';
            Caption              : 'Transform certain media to fit display';
            Prompt               : 'User can transform media for their current display';
            Description          : 'Transforms media';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : true;
            Cache                : true;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        PalmPrint=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Palm Print';
            NameSpace            : '/fls/plp';
            Caption              : 'Downscale certain media to fit display';
            Prompt               : 'User can scale media for their current display';
            Description          : 'Downscales media';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : true;
            Cache                : true;
            Compress             : false;
            Secure               : false;
            XMLInfo              : @NoXMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/fls/r';
            Caption              : 'Read a file';
            Prompt               : 'User can read files';
            Description          : 'Read user files from Database';
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
        Inspect=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Inspect';
            NameSpace            : '/fls/i';
            Caption              : 'Inspect a file';
            Prompt               : 'User can inspect files';
            Description          : 'Inspect user files to (re)create manifest data';
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
            NameSpace            : '/fls/w';
            Caption              : 'Write a file';
            Prompt               : 'User can write files';
            Description          : 'Write user files to Database';
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
            NameSpace            : '/fls/d';
            Caption              : 'Delete a file';
            Prompt               : 'User can delete files';
            Description          : 'Delete user files from folders in Database';
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
        Rename=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Rename';
            NameSpace            : '/fls/rn';
            Caption              : 'Rename a file';
            Prompt               : 'User can rename files';
            Description          : 'Rename user files from folders in Database';
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
        Copy=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Copy';
            NameSpace            : '/fls/c';
            Caption              : 'Copy a file';
            Prompt               : 'User can copy files';
            Description          : 'Copy user files from folders in Database';
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
        Move=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Move';
            NameSpace            : '/fls/m';
            Caption              : 'Move a file';
            Prompt               : 'User can move files';
            Description          : 'Move user files from folders in Database';
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
    Resources=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      List=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List';
          NameSpace              : '/res/l';
          Caption                : 'List resources available to user';
          Prompt                 : 'User can obtain a list of their resources';
          Description            : 'Obtain list of resources managed by user';
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
          NameSpace              : '/res/a';
          Caption                : 'Add a resource for a user';
          Prompt                 : 'User can add to their list of resources';
          Description            : 'Adds a resource to the list of resources';
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
          NameSpace              : '/res/d';
          Caption                : 'Delete a resource for a user';
          Prompt                 : 'User can delete resources from their list of resources';
          Description            : 'Deletes a resource from their list of resources';
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
          NameSpace              : '/res/w';
          Caption                : 'Write a resource for a user';
          Prompt                 : 'User can save a resource';
          Description            : 'Saves a resource from their list of resources';
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
      Refresh=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Refresh';
          NameSpace              : '/res/h';
          Caption                : 'Refresh a resource for a user';
          Prompt                 : 'User can refresh information of a resource';
          Description            : 'Refreshes a resource from their list of resources';
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
    Proxy=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Get=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Get';
          NameSpace              : '/proxy/g';
          Caption                : 'Get internet resource';
          Prompt                 : 'User should be able to perform an HTTP get';
          Description            : 'Process redirect HTTP Get statement to obtain data';
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
    Manifest=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/mfst/r';
          Caption                : 'Read VDM manifest file';
          Prompt                 : 'User must be able to read manifest file';
          Description            : 'VDM manifest retrieval from Database';
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
          NameSpace              : '/mfst/w';
          Caption                : 'Write VDM manifest file';
          Prompt                 : 'User must be able to save VDM manifest file';
          Description            : 'Manifest storage in Database';
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

    WallPaper=class
    const
      Folder                     : Core.Strings.VarString = '/core/vdm/imgs/wpr';
      Tiles                      : Core.Strings.VarString = '/core/vdm/imgs/wpr/tl';
      Scenes                     : Core.Strings.VarString = '/core/vdm/imgs/wpr/sc';
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      ListTiles=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List Tiles';
          NameSpace              : '/wlpr/lt';
          Caption                : 'List stock VDM wallpaper tiles';
          Prompt                 : 'User should be able to list wallpaper tiles';
          Description            : 'Wallpaper tile listing from Database';
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
      ListScenes=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List Scenes';
          NameSpace              : '/wlpr/ls';
          Caption                : 'List stock VDM wallpaper scenes';
          Prompt                 : 'User should be able to list wallpaper scenes';
          Description            : 'Wallpaper scene listing from Database';
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
          NameSpace              : '/wlpr/g';
          Caption                : 'Get VDM wallpaper';
          Prompt                 : 'User should be able to retrieve wallpaper';
          Description            : 'Wallpaper data from Database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : true;
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
      GetServerBuffer=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Get Server Buffer';
          NameSpace              : '/sync/gsb';
          Caption                : 'Get size of data waiting to be read by server';
          Prompt                 : 'Users must be able to retrieve server buffer size';
          Description            : 'Server buffer size of incoming data waiting to be processed';
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
      SetClientBuffer=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Set Client Buffer';
          NameSpace              : '/sync/scb';
          Caption                : 'Set size of data waiting to be sent by client';
          Prompt                 : 'Users must be able to set client buffer size';
          Description            : 'Client buffer size of outgoing data waiting to be processed';
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
      OpenChannel=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Open Channel';
          NameSpace              : '/sync/oc';
          Caption                : 'Open channel to synchronize with server';
          Prompt                 : 'Users must be able to open a synchronization channel';
          Description            : 'Opens a channel to the server for device synchronization';
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
      CloseChannel=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Close Channel';
          NameSpace              : '/sync/cc';
          Caption                : 'Close a synchronize channel with server';
          Prompt                 : 'Users must be able to close a synchronization channel';
          Description            : 'Closes a channel to the server for device synchronization';
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
    Groups=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/grps/r';
          Caption                : 'Read Application Group Name';
          Prompt                 : 'User can read group names';
          Description            : 'Application group retrieval from Database';
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
          NameSpace              : '/grps/w';
          Caption                : 'Write Application Group Name';
          Prompt                 : 'User can rename Application group names';
          Description            : 'Write their own Application group names to Database';
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
      List=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List';
          NameSpace              : '/grps/l';
          Caption                : 'List Application Groups';
          Prompt                 : 'User can obtain a list of Application groups';
          Description            : 'List their own Application groups from Database';
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
          NameSpace              : '/grps/a';
          Caption                : 'Add Application Groups';
          Prompt                 : 'User can add their own Application groups';
          Description            : 'Add their own Application groups to the Database';
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
          NameSpace              : '/grps/d';
          Caption                : 'Delete Application Groups';
          Prompt                 : 'User can delete Application groups';
          Description            : 'Delete their own Application groups from Database';
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
          Artist                 = 'tst';
          Album                  = 'alb';
          Song                   = 'sng';
          Genre                  = 'gre';
          Group                  = 'grp';
          Tags                   = 'tgs';
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
  TVDMCore=Class(TCoreObject)
  type
    {$i coVDM.TVDMCore.cmTransformVideo.Type.inc}
  private
    FSearchCore                  : TSearchCore;
    FWrite                       : boolean;
    FExt                         : Core.Strings.VarString;
    FPath                        : Core.Strings.VarString;
    FDepth                       : LongInt;

    FX                           : LongInt;
    FY                           : LongInt;
    FServer                      : THTTPServer;
    FManager                     : THTTPManager;
    FCR                          : TDateTime;
    FBias                        : Int64;
    FCacheExpired                : boolean;
    FLength                      : LongInt;
    FAuthLength                  : LongInt;
    FContentType                 : Core.Strings.VarString;
    FContentKind                 : Core.Strings.VarString;
    FSourceKind                  : Core.Strings.VarString;
    FETag                        : Core.Strings.VarString;
    FRange                       : Core.Strings.VarString;
    FAngle                       : Double;
    FFileID                      : QWord;
    FFolderID                    : QWord;
    FCount                       : Int64;
    FUpdateStamp                 : Boolean;

    DataP                        : PHTTP;
    FMediaP                      : PHTTPMedia;
    FFile                        : Storage.UserStorage.Files.TItem;
    FVideo                       : Storage.UserStorage.Files.TItem;
    FFiles                       : Storage.UserStorage.Files.TItems;
    FFileList                    : Storage.UserStorage.Files.TItems;
    FFolder                      : Storage.UserStorage.Folders.TFolder;
    FFolders                     : Storage.UserStorage.Folders.TFolders;
    FResources                   : Resources.TResources;
    FResource                    : Resources.TResource;

    FGet                         : Proxy.TGet;
    FAppGroup                    : Groups.TItem;
    FAppGroups                   : Groups.TItems;
    FManifest                    : Manifest.TItem;
    FManifest2                   : Manifest.TItem;

    FExtraRefactor               : TMemoryStream;
    FMP3Reader                   : Multimedia.MPEG.TReader;
    FMP3Tags                     : Storage.Summary.Music.TItems;
    FMP3Tag                      : Storage.Summary.Music.TItem;

    FWallPaper                   : TDSFolder;
    FWallPaperFile               : TDSFile;
    FWallPaperTiles              : TDSFolder;
    FWallPaperScenes             : TDSFolder;

    FMP3Summary                  : Storage.Summary.Music.TItems;

    FIDList                      : Core.Arrays.Types.LargeWord;
    FPattern                     : Core.Arrays.Types.VarString;

    FGenres                      : Core.Arrays.Types.VarString;
    FArtists                     : Core.Arrays.Types.VarString;
    FGroups                      : Core.Arrays.Types.VarString;
    FAlbums                      : Core.Arrays.Types.VarString;
    FTags                        : Core.Arrays.Types.VarString;
    FTagsAggregate               : Core.Strings.VarString;

    FSFile                       : TFileStream;
    FSVideo                      : TFileStream;

    FXMLParser                   : TDOMParser;
    FDocument                    : TXMLDocument;
    FSource                      : TXMLInputSource;
    FNode                        : TDOMNode;
  private
    procedure OnID3TagFrame(ID3:TFrame; Item:TFrame; Stream:TStream; var Handled:Boolean);
  private
    // VDM State
    function  cmdStateRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdStateWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Proxy
    function  cmdProxyGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Search Capabilities
    function  cmdSearchFiles(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdSearchMusic(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // WallPaper
    function  cmdTilesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdScenesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdWallPaperGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Folders
    function  cmdFoldersList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersRename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFoldersClear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Files
    function  cmdFilesRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesSetCreatedStamp(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesSetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesGetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesRotate (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesTransform (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesPalmPrint (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesListWith(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesListAll(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesMove(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesCopy(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesRename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesDownload(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFilesStream(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // VDM Resources
    function  cmdResourcesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdResourcesAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdResourcesDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdResourcesRefresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdResourcesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // VDM Application Groups
    function  cmdAppGroupRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdAppGroupWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdAppGroupsList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdAppGroupAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdAppGroupVerify(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdAppGroupDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // VDM Manifest
    function  cmdManifestRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdManifestWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
  protected
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  protected
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses DateUtils,Process;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TVDMCore.Install(Task);
end;

class procedure TVDMCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TVDMCore);
  with VDM do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    with State do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
    end;
    with Proxy do begin
      COREOBJECT_VerifyID(Task,Get.cmd);
    end;
    with Sync do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,GetServerBuffer.cmd);
      COREOBJECT_VerifyID(Task,SetClientBuffer.cmd);
      COREOBJECT_VerifyID(Task,OpenChannel.cmd);
      COREOBJECT_VerifyID(Task,CloseChannel.cmd);
    end;
    with Resources do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Refresh.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
    end;
    with Manifest do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
    end;
    with Groups do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
    end;
    with WallPaper do begin
      COREOBJECT_VerifyID(Task,ListTiles.cmd);
      COREOBJECT_VerifyID(Task,ListScenes.cmd);
      COREOBJECT_VerifyID(Task,Get.cmd);
    end;
    with Folders do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Rename.cmd);
      COREOBJECT_VerifyID(Task,Clear.cmd);
    end;
    with Files do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,GetData.cmd);
      COREOBJECT_VerifyID(Task,SetData.cmd);
      COREOBJECT_VerifyID(Task,Rotate.cmd);
      COREOBJECT_VerifyID(Task,Transform.cmd);
      COREOBJECT_VerifyID(Task,PalmPrint.cmd);
      COREOBJECT_VerifyID(Task,Get.cmd);
      COREOBJECT_VerifyID(Task,Stream.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Rename.cmd);
      COREOBJECT_VerifyID(Task,Move.cmd);
      COREOBJECT_VerifyID(Task,Copy.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Download.cmd);
      COREOBJECT_VerifyID(Task,ListAll.cmd);
      COREOBJECT_VerifyID(Task,ListWith.cmd);
      COREOBJECT_VerifyID(Task,Inspect.cmd);
      COREOBJECT_VerifyID(Task,SetCreated.cmd);
    end;
    With Search do begin
      COREOBJECT_VerifyID(Task,Music.cmd);
      COREOBJECT_VerifyID(Task,Files.cmd);
    end;
  end;
end;

class procedure TVDMCore.UnInstall;
begin
  UnRegisterClass(TVDMCore);
end;

procedure TVDMCore.Initialize;
begin
  FManager:=THTTPManager(OwnerP^.Manager);
  FServer:=THTTPServer(OwnerP^.Server);

  FSFile:=nil;
  FExtraRefactor:=TMemoryStream.Create();

  FXMLParser:=TDOMParser.Create();
  FXMLParser.Options.Validate:=False;
  //FXMLParser.Options.ConformanceLevel:=clFragment;
  FDocument:=nil;
  FSource:=nil;

  Storage.Summary.Music.Init(FMP3Summary);

  FWallPaper:=nil;
  FWallPaperTiles:=nil;
  FWallPaperScenes:=nil;


  Core.Arrays.VarString.Init(FGenres);
  Core.Arrays.VarString.Init(FArtists);
  Core.Arrays.VarString.Init(FGroups);
  Core.Arrays.VarString.Init(FAlbums);
  Core.Arrays.VarString.Init(FTags);

  FSearchCore:=TSearchCore(CoreObjects.Find(coSearch.Search.ACLInf.NameSpace));

  FMP3Reader:=Multimedia.MPEG.TReader.Create();
  FMP3Reader.OnTagFrame:=@OnID3TagFrame;

  Groups.Init(FAppGroup);
  Groups.Init(FAppGroups);
  with VDM do begin
    // Desktop State
    With State do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdStateRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdStateWrite);
    end;
    With Proxy do begin
      Storage.CoreObjects.Add(Get.cmd,FCommands,Header,@cmdProxyGet);
    end;
    with Manifest do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdManifestRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdManifestwrite);
    end;
    With Resources do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdResourcesList);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdResourcesAdd);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdResourcesDelete);
      Storage.CoreObjects.Add(Refresh.cmd,FCommands,Header,@cmdResourcesRefresh);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdResourcesWrite);
    end;
    With Groups do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdAppGroupsList);
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdAppGroupRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdAppGroupWrite);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdAppGroupAdd);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdAppGroupDelete);
    end;
    with WallPaper do begin
      Storage.CoreObjects.Add(ListTiles.cmd,FCommands,Header,@cmdTilesList);
      Storage.CoreObjects.Add(ListScenes.cmd,FCommands,Header,@cmdScenesList);
      Storage.CoreObjects.Add(Get.cmd,FCommands,Header,@cmdWallPaperGet);
    end;
    with Folders do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdFoldersList);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdFoldersAdd);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdFoldersDelete);
      Storage.CoreObjects.Add(Rename.cmd,FCommands,Header,@cmdFoldersRename);
      Storage.CoreObjects.Add(Clear.cmd,FCommands,Header,@cmdFoldersClear);
    end;
    with Files do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@cmdFilesList);
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdFilesRead);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdFilesWrite);
      Storage.CoreObjects.Add(GetData.cmd,FCommands,Header,@cmdFilesGetData);
      Storage.CoreObjects.Add(SetData.cmd,FCommands,Header,@cmdFilesSetData);
      Storage.CoreObjects.Add(Rotate.cmd,FCommands,Header,@cmdFilesRotate);
      Storage.CoreObjects.Add(Transform.cmd,FCommands,Header,@cmdFilesTransform);
      Storage.CoreObjects.Add(PalmPrint.cmd,FCommands,Header,@cmdFilesPalmPrint);
      Storage.CoreObjects.Add(Get.cmd,FCommands,Header,@cmdFilesGet);
      Storage.CoreObjects.Add(Stream.cmd,FCommands,Header,@cmdFilesStream);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@cmdFilesAdd);

      Storage.CoreObjects.Add(Rename.cmd,FCommands,Header,@cmdFilesRename);
      Storage.CoreObjects.Add(Move.cmd,FCommands,Header,@cmdFilesMove);
      Storage.CoreObjects.Add(Copy.cmd,FCommands,Header,@cmdFilesCopy);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdFilesDelete);
      Storage.CoreObjects.Add(Download.cmd,FCommands,Header,@cmdFilesDownload);
      Storage.CoreObjects.Add(ListAll.cmd,FCommands,Header,@cmdFilesListAll);
      Storage.CoreObjects.Add(ListWith.cmd,FCommands,Header,@cmdFilesListWith);
      Storage.CoreObjects.Add(SetCreated.cmd,FCommands,Header,@cmdFilesSetCreatedStamp);
    end;
    with Search do begin
      Storage.CoreObjects.Add(Music.cmd,FCommands,Header,@cmdSearchMusic);
      Storage.CoreObjects.Add(Files.cmd,FCommands,Header,@cmdSearchFiles);
    end;
  end;
end;

procedure TVDMCore.Finalize;
begin
  FreeAndNil(FExtraRefactor);
  FreeAndNil(FXMLParser);
  FreeAndNil(FSource);
  FreeAndNil(FDocument);

  FreeAndNil(FMP3Reader);
  Storage.Summary.Music.Done(FMP3Summary);
  Storage.Summary.Music.Done(FMP3Tags);
  Storage.Summary.Music.Done(FMP3Tag);

  Groups.Done(FAppGroup);
  Groups.Done(FAppGroups);
  Storage.UserStorage.Files.Done(FFile);
  Storage.UserStorage.Files.Done(FFiles);
  Storage.UserStorage.Folders.Done(FFolder,Storage.UserStorage.Folders.FREE_FILES);
  Storage.UserStorage.Folders.Done(FFolders,Storage.UserStorage.Folders.FREE_FILES);
  Resources.Done(FResources);
  Resources.Done(FResource);

  Manifest.Done(FManifest);
  Manifest.Done(FManifest2);

  Core.Arrays.VarString.Done(FGenres);
  Core.Arrays.VarString.Done(FArtists);
  Core.Arrays.VarString.Done(FGroups);
  Core.Arrays.VarString.Done(FAlbums);
  Core.Arrays.VarString.Done(FTags);

end;

procedure TVDMCore.OnID3TagFrame(ID3:TFrame; Item:TFrame; Stream:TStream; var Handled:Boolean);
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

function TVDMCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Handled:=True;
  DataP:=SR.Info.DataP;
  Result:=CO_STATUS_OK;
  Storage.UserStorage.Files.Empty(FFile);
  Storage.UserStorage.Files.Empty(FFiles);
  Storage.UserStorage.Files.Empty(FFile);
  Storage.UserStorage.Folders.Empty(FFolder,Storage.UserStorage.Folders.FREE_FILES);
  Storage.UserStorage.Folders.Empty(FFolders,Storage.UserStorage.Folders.FREE_FILES);
  FFolderID:=0;
  FFileID:=0;
  FWallPaperFile:=nil;
  if (FWallPaper=nil) then
    FWallPaper:=OwnerP^.Fat.Acquire(VDM.WallPaper.Folder,FRefactor);
  if (FWallPaperTiles=nil) then
    FWallPaperTiles:=OwnerP^.Fat.Acquire(VDM.WallPaper.Tiles,FRefactor);
  if (FWallPaperScenes=nil) then
    FWallPaperScenes:=OwnerP^.Fat.Acquire(VDM.WallPaper.Scenes,FRefactor);
end;

function  TVDMCore.cmdStateRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_OK;
end;

function  TVDMCore.cmdStateWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_OK;
end;

function  TVDMCore.cmdProxyGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;

end;

function  TVDMCore.cmdSearchFiles(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin

  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdSearchMusic(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and SameText(FMP3Summary.Genre,sGenre) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.UserStorage.Files.Add(FFiles[iLcv]^,FFileList);
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
    Storage.UserStorage.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
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
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and SameText(FMP3Summary.Group,sGroup) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.UserStorage.Files.Add(FFiles[iLcv]^,FFileList);
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
    Storage.UserStorage.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
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
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and SameText(FMP3Summary.Artist,sArtist) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.UserStorage.Files.Add(FFiles[iLcv]^,FFileList);
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
    Storage.UserStorage.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
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
            if (FNode<>nil) and Storage.Summary.Music.fromXML(FNode,FMP3Summary) and SameText(FMP3Summary.Album,sAlbum) then begin
              Core.Arrays.LargeWord.Add(FFiles[iLcv]^.ID,FSearchCore.srchResults.Results);
              Storage.UserStorage.Files.Add(FFiles[iLcv]^,FFileList);
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
    Storage.UserStorage.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
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
            Storage.UserStorage.Files.Add(FFiles[iLcv]^,FFileList);
            break;
          end;
        end;
      end;
    end;
    FRefactor.Size:=0;;
    Storage.UserStorage.Files.toXML(FFileList,FRefactor,FExtraRefactor,XML_HEADER_OFF);
    FRefactor.Position:=0;
    FSearchCore.srchResults.ResultsAsXML:=Core.Streams.toString(FRefactor);
    FRefactor.Size:=0;
    Result:=CO_STATUS_OK;
  end;

  procedure  ProcessTermAsSingleton;
  begin
    case FSearchCore.srchQuery.Term of
      VDM.Search.Music.Terms.Genre  : Push_Genre_List;
      VDM.Search.Music.Terms.Group  : Push_Group_List;
      VDM.Search.Music.Terms.Artist : Push_Artist_List;
      VDM.Search.Music.Terms.Album  : Push_Album_List;
      else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PROPERTY;
    end;
  end;

  procedure ProcessTermAsCriteria;
  begin
    case FSearchCore.srchCriteria[0]^.Key of
      VDM.Search.Music.Terms.Genre  : Push_ListBy_Genre(FSearchCore.srchCriteria[0]^.Value);
      VDM.Search.Music.Terms.Group  : Push_ListBy_Group(FSearchCore.srchCriteria[0]^.Value);
      VDM.Search.Music.Terms.Artist : Push_ListBy_Artist(FSearchCore.srchCriteria[0]^.Value);
      VDM.Search.Music.Terms.Album  : Push_ListBy_Album(FSearchCore.srchCriteria[0]^.Value);
      VDM.Search.Music.Terms.Tags   : Push_ListBy_Tags(FSearchCore.srchCriteria[0]^.Value);
      else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PROPERTY;
    end;
  end;

begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if FSearchCore.srchQuery.ID<>0 then begin
      FLength:=System.Length(FSearchCore.srchCriteria);
      Core.Arrays.VarString.SetSize(FPattern,4);
      FPattern[0]:='%.ogg';
      FPattern[1]:='%.mp3';
      FPattern[2]:='%.mp4';
      FPattern[3]:='%.mpeg';
      System.SetLength(FFileList,0);
      Try
        Try
          Storage.UserStorage.Folders.DB.List(FTask, UAP(SR)^.DomainID,UAP(SR)^.ID,Storage.UserStorage.Folders.Defaults.Home.Music,FIDList);
          Storage.UserStorage.Files.DB.List(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID, FIDList,FFiles,FPattern,Storage.UserStorage.Files.DB.IDs.Modified);
          case FLength of
            0: ProcessTermAsSingleton;
            1: ProcessTermAsCriteria;
          end;
        Finally
          Storage.UserStorage.Files.Empty(FFiles);
        end;
      Finally
        System.SetLength(FFileList,0);
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_EXECUTION;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdResourcesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Resources.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FResources) then begin
      Resources.toXML(FResources,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdResourcesAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Resources.fromXML(FXMLDocument,FResource) then begin
      if Resources.Add(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FResource) then begin
        Resources.toXML(FResource,Transport(SR).Output,XML_HEADER_ON);
        FFolder.Path:=Concat(Storage.UserStorage.Folders.Defaults.Home.Devices,'/',FResource.Name);
        Storage.UserStorage.Folders.DB.Create(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFolder.ID,FFolder.Path);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdResourcesRefresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Resources.fromXML(FXMLDocument,FResource) and (FResource.ID<>0) then begin
      if Resources.Read(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FResource.ID,FResource) then begin
        Resources.toXML(FResource,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdResourcesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Resources.fromXML(FXMLDocument,FResource) and (FResource.ID<>0) then begin
      if Resources.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FResource) then begin
        Resources.toXML(FResource,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdResourcesDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Resources.fromXML(FXMLDocument,FResource) and (FResource.ID<>0) then begin
      if Resources.Delete(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FResource.ID) then begin
        Resources.toXML(FResource,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFoldersList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FPath:=Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldSearch);
    FDepth:=StrToIntDef(Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldDepth),-1);
    if (Length(FPath)>0) then begin
      if Storage.UserStorage.Folders.DB.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FPath,FFolders) then begin
        if (FDepth>-1) then
          Storage.UserStorage.Folders.Purge(FFolders,FDepth);
        Storage.UserStorage.Folders.toXML(FFolders,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else begin
      if Storage.UserStorage.Folders.DB.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FFolders) then begin
        if (FDepth>-1) then
          Storage.UserStorage.Folders.Purge(FFolders,FDepth);
        Storage.UserStorage.Folders.toXML(FFolders,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFoldersAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserStorage.Folders.fromXML(FXMLDocument,FFolder) then begin
      if Storage.UserStorage.Folders.DB.ID(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFolder.Path,FFolder.ID) then begin
        Storage.UserStorage.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else if Storage.UserStorage.Folders.DB.Create(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFolder.ID,FFolder.Path) then begin
        Storage.UserStorage.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFoldersDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Folders.fromXML(FXMLDocument,FFolder) then begin
      if Storage.UserStorage.Folders.DB.Delete(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID,FFolder.ID) then begin
        Storage.UserStorage.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFoldersRename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  sPath:Core.Strings.VarString;
  iLcv:integer;
  iLen:integer;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Folders.fromXML(FXMLDocument,FFolder) then begin
      sPath:=Storage.UserStorage.Folders.DB.getPath(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FFolder.ID);
      iLen:=System.Length(sPath);
      // existing path
      if Storage.UserStorage.Folders.DB.List(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,sPath,FFolders) then begin
        for iLcv:=0 to High(FFolders) do begin
          System.Delete(FFolders[iLcv]^.Path,1,iLen);
          System.Insert(FFolder.Path,FFolders[iLcv]^.Path,1);
          if Storage.UserStorage.Folders.DB.Rename(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FFolders[iLcv]^) then begin
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end;
        Storage.UserStorage.Folders.Empty(FFolders,Storage.UserStorage.Folders.FREE_FILES);
        Storage.UserStorage.Folders.Empty(FFolder,Storage.UserStorage.Folders.FREE_FILES);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFoldersClear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FFolderID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FFolderID>0) then begin
      if Storage.UserStorage.Files.DB.Clear(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFolderID) then begin
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdTilesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if (FWallPaper<>nil) and (FWallPaperTiles<>nil) then begin
      FWallPaperTiles.Files.toXML(Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TVDMCore.cmdScenesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if (FWallPaper<>nil) and (FWallPaperScenes<>nil) then begin
      FWallPaperScenes.Files.toXML(Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdWallPaperGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if FWallPaper<>nil then begin
      FCount:=Length(Parameters);
      case FCount of
        1: FFileID:=StrToQwordDef(Core.Arrays.VarString.Parameter(Parameters, 1),0);
        2: FFileID:=StrToQwordDef(Core.Arrays.VarString.Parameter(Parameters, 2),0);
        else begin
          FFileID:=0;
        end;
      end;
      if (FFileID<>0) then begin
        if FWallPaper.Find(FFileID,FWallPaperFile) then begin
          FSFile:=FWallPaperFile.AcquireData();
          Try
            if (FSFile<>nil) then begin
              FExt:=Core.Utils.Files.Extract(FWallPaperFile.Name,efeoNone);
              FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);

              Transport(SR).CacheDate:=FWallPaperFile.Modified;
              Transport(SR).CacheResponse:=FWallPaperFile.Modified;
              Transport(SR).ContentType:=FContentType;
              Transport(SR).CacheExposure:=PRIVATE_CACHE;
              Transport(SR).CacheTag:=MD5Print(FWallPaperFile.Digest);

              FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));
              FCR:=Transport(SR).CacheRequest;
              FBias:=DateUtils.MilliSecondsBetween(FCR,FWallPaperFile.Modified);
              if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
                Core.Streams.Copy(FSFile,Transport(SR).Output);
                Result:=CO_STATUS_OK;
              end else begin
                Result:=CO_STATUS_OK;
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
          finally
            FSFile.Free();
          end;
        end else
          Result:=CO_STATUS_ERR_CO_RESOURCE_NOT_FOUND;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TVDMCore.cmdFilesList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FFolderID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FFolderID>0) then begin
      if Storage.UserStorage.Files.DB.List(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFolderID,FFiles,Storage.UserStorage.Files.DB.IDs.Created) then begin
        Storage.UserStorage.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesListWith(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FCount:=Core.Arrays.KeyString.GetItemAsQWordArray(recvHeaders,fieldSearch,FIDList);
    if (FCount>0) then begin
      if Storage.UserStorage.Files.DB.List(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FIDList,FFiles,Storage.UserStorage.Files.DB.IDs.Created) then begin
        Storage.UserStorage.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Storage.UserStorage.Files.Empty(FFiles);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      Empty(FIDList);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesListAll(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserStorage.Files.DB.ListAll(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFiles) then begin
      Storage.UserStorage.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesGetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLen:integer;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?-2--|
  // Parameters from /core/vdm?fls/sed?FolderID?FileId
  if SR.Credentials<>nil then begin
    iLen:=System.Length(Parameters);
    if (iLen>2) then begin
      Storage.UserStorage.Files.Empty(FFile);
      FFile.FolderID:=StrToQWordDef(Parameters[1],0);
      FFile.ID:=StrToQWordDef(Parameters[2],0);
      OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesGetData User (',IntToStr(UAP(SR)^.ID),') File (',IntToStr(FFile.ID),')');
      if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFile.ID,FFile,FSFile) then begin
        If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
          Try
            Core.Arrays.KeyString.Update(respHeaders,fieldContentType,ctStream);
            Core.Streams.Copy(FSFile,Transport(SR).Output);
            Storage.UserStorage.Files.Empty(FFile);
            Result:=CO_STATUS_OK;
          finally
            FSFile.Free();
          end;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesSetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLen:integer;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?----1----?-2--|
  // Parameters from /core/vdm?fls/sed?FolderID?FileId
  if SR.Credentials<>nil then begin
    OwnerP^.Manager.RenewCycle();
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      Result:=CO_STATUS_OK;
      iLen:=System.Length(Parameters);
      if (iLen>2) then begin
        Storage.UserStorage.Files.Empty(FFile);
        FFile.FolderID:=StrToQWordDef(Parameters[1],0);
        FFile.ID:=StrToQWordDef(Parameters[2],0);
        if (FFile.FolderID<>0) and (FFile.ID<>0) then begin
          Try
            if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile.ID,FFile,FSFile) then begin
              FUpdateStamp:=(FFile.Allocated>Storage.UserStorage.Files.Allocate_Base);
              if FUpdateStamp=false then
                FFile.Modified:=FFile.Created;

              Storage.UserStorage.Files.DB.SetAllocatedStamp(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile.ID,Storage.UserStorage.Files.Allocate_Writing);
              FFile.Allocated:=Core.Timer.dtUT;
              OwnerP^.Manager.RenewCycle();

              OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesSetData.Copy[',IntToStr(FFile.ID),']');
              Core.Streams.Copy(Transport(SR).Input,FSFile);

              OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesSetData.CheckSum[',IntToStr(FFile.ID),']');
              Core.Streams.CheckSum(Transport(SR).Input,FFile.Digest);

              OwnerP^.Manager.RenewCycle();
              FFile.Size:=FSFile.Size;
              Transport(SR).Input.Position:=0;
              FExt:=Lowercase(Core.Utils.Files.Extract(FFile.Name,efeoNone));
              case (FExt) of
                //  Handlers must empty FFile.Data buffer
                'mov'  : FFile.Kind:=Storage.UserStorage.Kind.Video;
                'mp4'  : FFile.Kind:=Storage.UserStorage.Kind.Video;
                'mpg'  : FFile.Kind:=Storage.UserStorage.Kind.Video;
                'mpeg' : FFile.Kind:=Storage.UserStorage.Kind.Video;
                'mp3'  : begin
                    OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesInspect.File.Music[',IntToStr(FFile.ID),']');
                    Try
                      FFile.Kind:=Storage.UserStorage.Kind.Music;
                      Storage.Summary.Music.Empty(FMP3Tags);
                      OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesInspect.File.Music[',IntToStr(FFile.ID),'].LoadFirst');
                      try
                        FMP3Reader.LoadFirst(Transport(SR).Input);
                        FMP3Tags.Inspected:=true;
                        FWrite:=true;
                      except
                        on e:exception do Core.Logging.Native.WriteLogEntry(OwnerP^.DomainP^.Name,OwnerP^.Manager.Service,Concat(OwnerP^.Manager.EntryPoint,' Exception: ',E.Message));
                      end;
                      OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesInspect.File.Music[',IntToStr(FFile.ID),'].toXML');
                      FFile.Summary:=Storage.Summary.Music.toXML(FMP3Tags,FRefactor);
                    Finally
                      OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesInspect.File.Music[',IntToStr(FFile.ID),'].Empty');
                      Storage.Summary.Music.Empty(FMP3Tags);
                    end;
                end;
              end;

              OwnerP^.Manager.RenewCycle();
              Storage.UserStorage.Files.DB.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile,FUpdateStamp);
              Result:=CO_STATUS_OK;

              Transport(SR).ContentType:=ctXML;
              Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Storage.UserStorage.Files.Empty(FFile);
            end else begin
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
              Transport(SR).ContentType:=ctXML;
              Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
              Storage.UserStorage.Files.Empty(FFile);
            end;
          Finally
            FreeAndNil(FSFile);
          end;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.UserStorage.Files.fromXML(FXMLDocument,FFile) and (FFile.FolderID<>0) and (Length(FFile.Name)>0) ) then begin
      if ( Storage.UserStorage.Files.DB.Exists(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile.FolderID,FFile.Name)=true) then begin
        if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile.FolderID,FFile.Name,FFile) then begin
           Result:=CO_STATUS_OK;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end;
      end else begin
        if Storage.UserStorage.Files.DB.Add(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile.FolderID,FFile,nil) then begin
          Result:=CO_STATUS_OK;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end;
      end;
      Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
      Storage.UserStorage.Files.Empty(FFile);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if  (Storage.UserStorage.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
      for iLcv:=0 to High(FFiles) do
        Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFiles[iLcv]^.ID,FFiles[iLcv]^.FolderID,UAP(SR)^.Trash);
      Result:=CO_STATUS_OK;
      Storage.UserStorage.Files.Empty(FFiles);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesDownload(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  // Parameters from /core/vdm?fls/get?FileId
  if SR.Credentials<>nil then begin
    FLength:=System.Length(Parameters);
    if (FLength>1) then begin
      FFileID:=StrToIntDef(Parameters[1],0);
      OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesDownload User (',IntToStr(UAP(SR)^.ID),') File (',IntToStr(FFileID),')');
      if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
        Try
          OwnerP^.Manager.RenewCycle();
          Transport(SR).CacheResponse:=FFile.Modified;
          Transport(SR).CacheTag:=MD5Print(FFile.Digest);
          FBias:=DateUtils.MillisecondsBetween(FFile.Modified,Transport(SR).CacheRequest);
          FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));
          if (FBias>MODIFIED_THRESHOLD) then begin
            if (Transport(SR).CacheRequest<>FFile.Modified) then begin
              Core.Arrays.KeyString.Add(@respHeaders,fieldContentDisposition,Concat('attachment; filename=',FFile.Name));

              Transport(SR).ContentType:=ctStream;
              Transport(SR).CacheDate:=FFile.Modified;
              Transport(SR).CacheResponse:=FFile.Modified;
              Transport(SR).CacheExposure:=PRIVATE_CACHE;

              Core.Streams.Copy(FSFile,Transport(SR).Output);
              Storage.UserStorage.Files.Empty(FFile);
            end;
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_OK;
        Finally
          FSFile.Free();
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  procedure OutputFile;
  begin
    FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
    FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);

    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheResponse:=FFile.Modified;

    Transport(SR).ContentType:=FContentType;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=MD5Print(FFile.Digest);
    FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));

    FCR:=Transport(SR).CacheRequest;
    FBias:=DateUtils.MilliSecondsBetween(FCR,FFile.Modified);

    if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
      if RSR.HTTP.IndexOf(FContentType,ctMedia)<>-1 then begin
        if FMediaP=nil then begin
          new(FMediaP);
          RSR.HTTP.Init(FMediaP^);
          DataP^.Media:=FMediaP;
        end else
          Empty(FMediaP^);

        FMediaP^.ContentType:=FContentType;
        FMediaP^.ETag:=Transport(SR).CacheTag;
        FMediaP^.Modified:=FFile.Modified;
        FMediaP^.ContentLength:=FFile.Size;
        Transport(SR).Media:=FMediaP;

        Core.Streams.Copy(FSFile,FMediaP^.Content);
        Result:=CO_STATUS_OK;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
      end;
    end else begin
      Result:=CO_STATUS_OK;
    end;
  end;

  procedure OutputMedia;
  begin
    Transport(SR).Media:=FMediaP;
    Transport(SR).CacheDate:=FMediaP^.Modified;
    Transport(SR).CacheResponse:=FMediaP^.Modified;
    Transport(SR).ContentType:=FMediaP^.ContentType;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=FMediaP^.ETag;
    Result:=CO_STATUS_OK;
  end;

begin
  Result:=CO_STATUS_FAIL;
  // Parameters from /core/vdm?fls/get?FileId
  if SR.Credentials<>nil then begin
    FMediaP:=DataP^.Media;
    FLength:=System.Length(Parameters);
    if (FLength>1) then begin
      FFileID:=StrToIntDef(Parameters[1],0);
      FETag:=Core.Arrays.KeyString.GetItemAsString(recvHeaders,fieldIfRange);
      if (Length(FETag)>0) then begin
        if (FMediaP<>nil) and (FMediaP^.ETag=FETag) then begin
          OutputMedia();
        end else begin
          OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesGet eTag User (',IntToStr(UAP(SR)^.ID),') File (',IntToStr(FFileID),')');
          if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
            Try
              If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
                OutputFile();
                Storage.UserStorage.Files.Empty(FFile);
              end else begin
                Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
              end;
            finally
              FSFile.Free();
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end;
      end else begin
        OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesGet User (',IntToStr(UAP(SR)^.ID),') File (',IntToStr(FFileID),')');
        if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
          try
            If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
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
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesStream(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
        RSR.HTTP.Init(FMediaP^);
        DataP^.Media:=FMediaP;
      end else
        RSR.HTTP.Empty(FMediaP^);
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

begin
  Result:=CO_STATUS_FAIL; //  ?----0---?--1---?-2-|
  // Parameters from /core/vdm?fls/strm?FileId?Auth
  FLength:=System.Length(Parameters);
  if ( System.Length(DataP^.Auth)=0) then begin
    if (FLength>2) then begin
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        DataP^.Auth:=Parameters[2];
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end;
  if (System.Length(DataP^.Auth)>0) then begin
    if (FLength>1) then begin
      FFileID:=StrToQWordDef(Parameters[1],0);
      if  (FFileID<>0) then begin
        FMediaP:=DataP^.Media;
        FRange:=Core.Arrays.KeyString.GetItemAsString(recvHeaders,fieldRange);
        FETag:=Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldETag);
        if ( (Length(FRange)>0) or (Length(FETag)>0) ) then begin
          if (FMediaP<>nil) and (FMediaP^.ETag=FETag) then begin
            OutputMedia();
          end else begin
            OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesStream eTag User (',IntToStr(UAP(SR)^.ID),') File (',IntToStr(FFileID),')');
            if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
              Try
                If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
                  OwnerP^.Manager.RenewCycle();
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
        end else begin
          OwnerP^.Manager.EntryPoint:=Concat('coVDM.cmdFilesStream User (',IntToStr(UAP(SR)^.ID),') File (',IntToStr(FFileID),')');
          if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
            try
              If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
                OwnerP^.Manager.RenewCycle();
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
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TVDMCore.cmdFilesTransform (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure OutputFilePicture;
  begin
    Transport(SR).CacheResponse:=FFile.Modified;
    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheValidation:=MUST_REVALIDATE;
    Transport(SR).CacheTag:=MDPrint(FFile.Digest);

    FCacheExpired:=( (Transport(SR).ETagRequested=false) or (Transport(SR).ETagRequest<>Transport(SR).CacheTag));

    FCR:=Transport(SR).CacheRequest;
    FBias:=DateUtils.MilliSecondsBetween(FCR,FFile.Modified);

    if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
      if RSR.HTTP.IndexOf(FContentType,ctImageTransforms)<>-1 then begin
        Core.Streams.Copy(FSFile,FRefactor);
        FSourceKind:=Multimedia.Image.Tool.Kind.fromString(FExt);
        FContentKind:=FSourceKind;
        FX:=1024;
        FY:=768;
        If Multimedia.Image.Tool.Transform(FRefactor,FX,FY) then begin
          Transport(SR).ContentType:=FContentType;
          Core.Streams.Copy(FRefactor,Transport(SR).Output);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
        FRefactor.Size:=0;
      end else begin
        Result:=CO_STATUS_OK;
      end;
    end else begin
      Result:=CO_STATUS_OK;
    end;
  end;

  procedure ConvertVideo;
  begin
    FVideo.Name:=SysUtils.ChangeFileExt(FFile.Name,'.mp4');
    if Storage.UserStorage.Files.DB.Force(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFile.FolderID,Storage.UserStorage.Kind.Video,FFile.Created,FVideo.Name,FVideo,FSVideo) then begin
      Try
        OwnerP^.Manager.AddMethod(cmTransformVideo.Create(OwnerP^.Manager, @SR, UAP(SR)^.DomainID, UAP(SR)^.ID,FFile,FVideo,FSFile.FileName,FSVideo.FileName));
        Storage.UserStorage.Files.toXML(FVideo,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      finally
        FSVideo.Free();
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?---1--?--2-?
  // Parameters from /core/vdm?fls/tfm?FileId?Auth|
  FLength:=System.Length(Parameters);
  FAuthLength:=System.Length(DataP^.Auth);
  if ( FAuthLength=0) then begin
    if (FLength>2) then begin
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        DataP^.Auth:=Parameters[2];
        FAuthLength:=System.Length(DataP^.Auth);
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        exit;
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
      Exit;
    end;
  end;
  if (FAuthLength>0) then begin
    if (FLength>1) then begin
      FFileID:=StrToQWordDef(Parameters[1],0);
      if (FFileID<>0)  then begin
        if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
          Try
            If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
              FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
              FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
              if (RSR.HTTP.IndexOf(FContentType,ctImage)<>-1) then begin
                OutputFilePicture();
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
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TVDMCore.cmdFilesPalmPrint (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure OutputFilePicture;
  begin
    Transport(SR).CacheResponse:=FFile.Modified;
    Transport(SR).CacheDate:=FFile.Modified;
    Transport(SR).CacheExposure:=PRIVATE_CACHE;
    Transport(SR).CacheTag:=MDPrint(FFile.Digest);

    FCacheExpired:=( (Transport(SR).ETagRequested=false) or ((Transport(SR).ETagRequested=true) and (Transport(SR).ETagRequest<>Transport(SR).CacheTag)));

    FCR:=Transport(SR).CacheRequest;
    FBias:=DateUtils.MilliSecondsBetween(FCR,FFile.Modified);

    if (FBias>MODIFIED_THRESHOLD) or (FCacheExpired=true) then begin
      if RSR.HTTP.IndexOf(FContentType,ctImageTransforms)<>-1 then begin
        Core.Streams.Copy(FSFile,FRefactor);
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
        FRefactor.Size:=0;
      end;
    end else begin
      Result:=CO_STATUS_OK;
    end;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?---1--?--2-?
  // Parameters from /core/vdm?fls/tfm?FileId?Auth|
  FLength:=System.Length(Parameters);
  FAuthLength:=System.Length(DataP^.Auth);
  if ( FAuthLength=0) then begin
    if (FLength>2) then begin
      Result:=Transport(SR).OnCoreObjectCheckCredentials(CommandP,SR,recvHeaders,respHeaders,Data);
      if (Result=CO_STATUS_OK) then begin
        DataP^.Auth:=Parameters[2];
        FAuthLength:=System.Length(DataP^.Auth);
        Core.Arrays.KeyString.Update(recvHeaders,fieldAuth,DataP^.Auth);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        exit;
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
      Exit;
    end;
  end;
  if (FAuthLength>0) then begin
    if (FLength>1) then begin
      FFileID:=StrToQWordDef(Parameters[1],0);
      if (FFileID<>0)  then begin
        if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
          Try
            If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
              FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
              FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
              if (RSR.HTTP.IndexOf(FContentType,ctImage)<>-1) then begin
                OutputFilePicture();
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
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
end;

function  TVDMCore.cmdFilesRotate (CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  procedure ConvertFile;
  begin
    Core.Streams.Copy(FSFile,FRefactor);
    If Multimedia.Image.Tool.Rotate(FRefactor,FAngle) then begin
      Core.Streams.Copy(FRefactor,FSFile);
      Core.Streams.CheckSum(FRefactor,FFile.Digest);
      FFile.Size:=FRefactor.Size;
      Storage.UserStorage.Files.DB.SetDigest(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID, FFile.ID,FFile.Size,FFile.Digest);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
    FRefactor.Size:=0;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---?---1--?--2-?
  // Parameters from /core/vdm?fls/rot?FileId?angle // what a
  if SR.Credentials<>nil then begin
    if Length(Parameters)>3 then begin
        FFileID:=StrToQWordDef(Parameters[1],0);
        FAngle:=StrToFloatDef(Parameters[2],90);
        if (FFileID<>0)  then begin
          if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFileID,FFile,FSFile) then begin
            Try
              If (FFile.Allocated>Storage.UserStorage.Files.Allocate_Base) then begin
                FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
                FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
                if (RSR.HTTP.IndexOf(FContentType,ctImage)<>-1) then begin
                  ConvertFile();
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
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesSetCreatedStamp(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  ( Storage.UserStorage.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
      for iLcv:=0 to High(FFiles) do begin
        if FFiles[iLcv]^.ID<>0 then
          Storage.UserStorage.Files.DB.SetCreatedStamp(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID, FFiles[iLcv]^.ID,FFiles[iLcv]^.Created);
        OwnerP^.Manager.RenewCycle();
      end;
      Storage.UserStorage.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
      Storage.UserStorage.Files.Empty(FFiles);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    OwnerP^.Manager.RenewCycle();
    if (SR.Throttle.Enabled) and (SR.Throttle.Consumption>=SR.Throttle.Limit) and (SR.RecvBuffer.posWrite>BUFFER_THRESHOLD) then
      Result:=CO_STATUS_BUFFERS_EXEEDED;
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      OwnerP^.Manager.RenewCycle();
      if  ( Storage.UserStorage.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
        if Result<>CO_STATUS_BUFFERS_EXEEDED then begin
          for iLcv:=0 to High(FFiles) do begin
            if FFiles[iLcv]^.ID<>0 then begin
              if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFiles[iLcv]^.ID,FFiles[iLcv]^,FSFile) then
                FSFile.Free();
            end;
            OwnerP^.Manager.RenewCycle();
          end;
          Result:=CO_STATUS_OK;
        end;
        Storage.UserStorage.Files.toXML(FFiles,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Storage.UserStorage.Files.Empty(FFiles);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    OwnerP^.Manager.RenewCycle();
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      if ( Storage.UserStorage.Files.fromXML(FXMLDocument,FFile) and (FFile.FolderID<>0) and (FFile.ID<>0) ) then begin
        if (SR.Throttle.Enabled) and (SR.Throttle.Consumption>=SR.Throttle.Limit) and (SR.RecvBuffer.posWrite>BUFFER_THRESHOLD)then begin
          Result:=CO_STATUS_BUFFERS_EXEEDED;
          Transport(SR).ContentType:=ctXML;
          Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          Exit;
        end;
        FFile.Allocated:=2; // Refreshed content - good for re-writes
        if Storage.UserStorage.Files.DB.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile,true) then begin
          Transport(SR).ContentType:=ctXML;
          Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          Transport(SR).ContentType:=ctXML;
          Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        end;
      end else
       Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesMove(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
  FolderID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FolderID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FolderID>0) then begin
      if  ( Storage.UserStorage.Files.fromXML(FXMLDocument,FFiles) and (System.Length(FFiles)>0) ) then begin
        for iLcv:=0 to High(FFiles) do begin
          Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID, UAP(SR)^.ID, FFiles[iLcv]^.ID,FFiles[iLcv]^.FolderID,FolderID);
        end;
        Result:=CO_STATUS_OK;
        Storage.UserStorage.Files.Empty(FFiles);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesCopy(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Files.fromXML(FXMLDocument,FFile) then begin
      if (FFile.ID<>0) then begin
        if Storage.UserStorage.Files.DB.Copy(FTask,UAP(SR)^.AuraNode,UAP(SR)^.DomainID,UAP(SR)^.ID,FFile.ID,FFile.ID) then begin
          Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdFilesRename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserStorage.Files.fromXML(FXMLDocument,FFile) then begin
      if Storage.UserStorage.Files.DB.Rename(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FFile) then begin
        Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.DomainID, UAP(SR)^.ID,FFile.ID,FFile);
        Storage.UserStorage.Files.toXML(FFile,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdAppGroupRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Groups.fromXML(FXMLDocument,FAppGroup) and (FAppGroup.ID>0) then begin
      if Groups.Read(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAppGroup) then begin
        Result:=CO_STATUS_OK;
        Groups.toXML(FAppGroup,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdAppGroupWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Groups.fromXML(FXMLDocument,FAppGroup) and (FAppGroup.ID>0) then begin
      if Groups.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAppGroup) then begin
        Result:=CO_STATUS_OK;
        Groups.toXML(FAppGroup,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdAppGroupVerify(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Groups.fromXML(FXMLDocument,FAppGroup) then begin
      if Groups.Verify(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAppGroup) then begin
        Result:=CO_STATUS_OK;
        Groups.toXML(FAppGroup,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TVDMCore.cmdManifestRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if UAP(SR)^.ResourceID<>0 then begin
      Manifest.Read(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,UAP(SR)^.ResourceID,FManifest);
      if FManifest.ID=0 then
        Manifest.Add(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,UAP(SR)^.ResourceID,FManifest);
      if FManifest.ID<>0 then begin
        Result:=CO_STATUS_OK;
        Manifest.toXML(FManifest,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdManifestWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if UAP(SR)^.ResourceID=0 then
      UAP(SR)^.ResourceID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldResourceID);
    if (UAP(SR)^.ResourceID<>0) then begin
      if (FXMLDocument<>nil) and Manifest.fromXML(FXMLDocument,FManifest) then begin
        if (FManifest.ID=0) then begin
          if Manifest.Read(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,UAP(SR)^.ResourceID,FManifest2) then begin
            FManifest.ID:=FManifest2.ID;
            if Manifest.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,UAP(SR)^.ResourceID,FManifest) then begin
              Result:=CO_STATUS_OK;
              Manifest.toXML(FManifest,Transport(SR).Output);
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else if Manifest.Write(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,UAP(SR)^.ResourceID,FManifest) then begin
          Result:=CO_STATUS_OK;
          Manifest.toXML(FManifest,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_NO_DEVICE_ID;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdAppGroupsList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Groups.List(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAppGroups) then begin
      Result:=CO_STATUS_OK;
      Groups.toXML(FAppGroups,Transport(SR).Output);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdAppGroupAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Groups.fromXML(FXMLDocument,FAppGroup) and (Length(FAppGroup.Name)>0) then begin
      if Groups.Add(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAppGroup) then begin
        Result:=CO_STATUS_OK;
        Groups.toXML(FAppGroup,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TVDMCore.cmdAppGroupDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Groups.fromXML(FXMLDocument,FAppGroup) and (FAppGroup.ID>0) then begin
      if Groups.Delete(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,FAppGroup) then begin
        Result:=CO_STATUS_OK;
        Groups.toXML(FAppGroup,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

{$i coVDM.TVDMCore.cmTransformVideo.Code.inc}
end.

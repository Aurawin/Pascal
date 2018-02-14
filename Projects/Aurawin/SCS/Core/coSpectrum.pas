{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected by the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit coSpectrum;

interface
uses
  Classes,

  App.Consts,
  hHttpd,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Database,
  Core.Database.Types,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.Bytes,
  Core.Arrays.KeyString,
  Core.Timer,
  Core.Keywords,
  Core.Streams,
  Core.XML,
  Core.Strings,

  Encryption.Base64,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.Assemblies,
  Storage.Domains,
  Storage.Tasks,
  Storage.UserAccounts,
  Storage.VDM,
  Storage.Roster,
  Storage.Projects,
  Storage.Signatures,
  Storage.UserStorage,
  Storage.Security,
  Storage.Intrusion,
  Storage.ContentTypes,

  Core.Utils.Time,
  Core.Utils.Sockets,
  Core.Utils.Files,

  DOM,
  XMLRead;

Type
  Spectrum=class
  const
    ACLInf:TACLInfo=(
      Name                       : 'Spectrum';
      NameSpace                  : '/core/spc';
      Caption                    : 'Spectrum Core Object';
      Prompt                     : 'User access to spectrum';
      Description                : 'Provides a single point access to messaging, contacts, and more';
    );
    CLSInf:TCLSInfo=(
      Name                       : 'TSpectrumCore';
      Location                   : 'coSpectrum.pas';
    );
    Header:TCoreObjectInfo=(
      ID                         : 0;
      ProviderID                 : 0;
      Enabled                    : true;
      Anonymous                  : false;
      Scale                      : 0;
      CLSInfo                    : @CLSInf;
      ACLInfo                    : @ACLInf;
    );
  type
    Contacts=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Contact=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : true;
        );
      type
        Add=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Add';
            NameSpace            : '/cts/ctc/a';
            Caption              : 'Add New Contact';
            Prompt               : 'Users can add new contacts';
            Description          : 'Provides users the ability to add new contacts';
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
          ACLInf:TACLinfo=(
            Name                 : 'Delete';
            NameSpace            : '/cts/ctc/d';
            Caption              : 'Delete Contact';
            Prompt               : 'Users can delete contacts';
            Description          : 'Provides users the ability to remove contacts';
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
        Read=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Read';
            NameSpace            : '/cts/ctc/r';
            Caption              : 'Read Contact';
            Prompt               : 'Users can read contacts';
            Description          : 'Provides users the ability to read contacts';
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
        Write=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Write';
            NameSpace            : '/cts/ctc/w';
            Caption              : 'Write Contact';
            Prompt               : 'Users can edit contacts';
            Description          : 'Provides users the ability to edit contacts';
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
        SetAvatar=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Set Avatar';
            NameSpace            : '/cts/ctc/sa';
            Caption              : 'Set Avatar';
            Prompt               : 'Users can set a custom avatar for contacts';
            Description          : 'Provides users the ability to set custom avatars';
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
        Refresh=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Refresh';
            NameSpace            : '/cts/ctc/h';
            Caption              : 'Refresh Contact';
            Prompt               : 'Users can read contacts';
            Description          : 'Provides users the ability to refresh a contact';
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
      List=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'List';
          NameSpace              : '/cts/l';
          Caption                : 'List Contacts';
          Prompt                 : 'Users can list contacts';
          Description            : 'Provides users the ability to list contacts';
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
          Method               : nil;
          Resource             : nil;
        );
      end;
    end;
    Signatures=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Signature=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : true;
        );
      type
        Add=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Add';
            NameSpace            : '/sigs/sig/a';
            Caption              : 'Add a Signature';
            Prompt               : 'Users can add signatures';
            Description          : 'Provides users the ability to create a signature';
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
        List=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'List';
            NameSpace            : '/sigs/l';
            Caption              : 'List signatures';
            Prompt               : 'Users can list signatures';
            Description          : 'Provides users the ability list signatures';
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
          ACLInf:TACLinfo=(
            Name                 : 'Delete';
            NameSpace            : '/sigs/sig/d';
            Caption              : 'Delete a signature';
            Prompt               : 'Users can delete a signature';
            Description          : 'Provides users the ability delete a signature';
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
        Write=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Write';
            NameSpace            : '/sigs/sig/w';
            Caption              : 'Write a signature';
            Prompt               : 'Users can save a signature';
            Description          : 'Provides users the ability save a signature';
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
        Read=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Read';
            NameSpace            : '/sigs/sig/r';
            Caption              : 'Read a signature';
            Prompt               : 'Users can read a signature';
            Description          : 'Provides users the ability read a signature';
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
        Refresh=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Refresh';
            NameSpace            : '/sigs/sig/rf';
            Caption              : 'Refresh a signature';
            Prompt               : 'Users can refresh a signature';
            Description          : 'Provides users the ability refresh a signature';
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
    end;
    Email=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Add=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Add';
          NameSpace              : '/eml/a';
          Caption                : 'Create a New Email';
          Prompt                 : 'Users can create email messages';
          Description            : 'Provides users the ability to create new email messages';
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
          Method               : nil;
          Resource             : nil;
        );
      end;
      Delete=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Delete';
          NameSpace              : '/eml/del';
          Caption                : 'Delete Email';
          Prompt                 : 'Users can delete email';
          Description            : 'Provides users the ability to delete emails';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Move=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Move';
          NameSpace              : '/eml/mv';
          Caption                : 'Move Email';
          Prompt                 : 'Users can move email';
          Description            : 'Provides users the ability to move emails';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Data=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Data';
          NameSpace              : '/eml/dat';
          Caption                : 'Read Email Message Body';
          Prompt                 : 'Users can read email message body';
          Description            : 'Provides users the ability to seperately download message body';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Read=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Read';
          NameSpace              : '/eml/r';
          Caption                : 'Read Email Message';
          Prompt                 : 'Users can read email messages';
          Description            : 'Provides users the ability to read messages';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Count=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Count';
          NameSpace              : '/eml/cnt';
          Caption                : 'Count Email Messages';
          Prompt                 : 'Users can count email messages';
          Description            : 'Provides users the ability to count messages';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Write=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Write';
          NameSpace              : '/eml/w';
          Caption                : 'Write Email Message';
          Prompt                 : 'Users can save email messages';
          Description            : 'Provides users the ability to save email messages';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Mime=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Mime';
          NameSpace              : '/eml/m';
          Caption                : 'Read Mime Sections in Email Message';
          Prompt                 : 'Users can read sections in email messages';
          Description            : 'Provides users the ability to read parts of messages';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      MGet=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'MGet';
          NameSpace              : '/eml/getm';
          Caption                : 'Get Mime Attachment in Email Message';
          Prompt                 : 'Users can download sections of email messages';
          Description            : 'Provides users the ability to get parts of messages';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      UpdateSummary=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Update Summary';
          NameSpace              : '/eml/us';
          Caption                : 'Update summary information for Email';
          Prompt                 : 'Users can update summary information for messages';
          Description            : 'Provides users the ability update summary information for messages';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Refresh=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Refresh';
          NameSpace              : '/eml/h';
          Caption                : 'Refresh Email Message';
          Prompt                 : 'Users can refresh email messages';
          Description            : 'Provides users the ability to refresh an email message';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Stat=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Stat';
          NameSpace              : '/eml/st';
          Caption                : 'Status of Inbox';
          Prompt                 : 'Users can retrieve Inbox statistics';
          Description            : 'Provides users with real-time Inbox statistics';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      List=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'List';
          NameSpace              : '/eml/l';
          Caption                : 'List Email Messages';
          Prompt                 : 'Users can list email messages in a folder';
          Description            : 'Provides users the ability to list email messages';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Clear=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Clear';
          NameSpace              : '/eml/clr';
          Caption                : 'Clear Email Messages';
          Prompt                 : 'Users can erase email messages in a folder';
          Description            : 'Provides users the ability to erase all email messages';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Archive=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'Archive';
          NameSpace              : '/eml/arc';
          Caption                : 'Archive Email Messages';
          Prompt                 : 'Users can archive email messages in a folder';
          Description            : 'Provides users the ability to archive all email messages';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : false;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
    Assemblies=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Assembly=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : true;
        );
      type
        Add=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Add';
            NameSpace            : '/asms/asm/a';
            Caption              : 'Add an Assembly';
            Prompt               : 'Users can create an assembly';
            Description          : 'Provides users the ability to create an assembly';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure                 : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Delete';
            NameSpace            : '/asms/asm/d';
            Caption              : 'Delete an Assembly';
            Prompt               : 'Users can delete an assembly';
            Description          : 'Provides users the ability to delete an assembly';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Read=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Read';
            NameSpace            : '/asms/asm/r';
            Caption              : 'Read an Assembly';
            Prompt               : 'Users can read an assembly';
            Description          : 'Provides users the ability to read an assembly';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Write=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Write';
            NameSpace            : '/asms/asm/w';
            Caption              : 'Write an Assembly';
            Prompt               : 'Users can write an assembly';
            Description          : 'Provides users the ability to write an assembly';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Refresh=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Refresh';
            NameSpace            : '/asms/asm/h';
            Caption              : 'Refresh an Assembly';
            Prompt               : 'Users can refresh an assembly';
            Description          : 'Provides users the ability to refresh an assembly';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Admins=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/adns/r';
              Caption            : 'Get list of administrators';
              Prompt             : 'User can obtain a list of administrators for an assembly';
              Description        : 'Provides users the ability to obtain administrators an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/adns/w';
              Caption            : 'Set list of administrators';
              Prompt             : 'User can set the list of administrators for an assembly';
              Description        : 'Provides users the ability to set administrators for an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Files=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/fls/r';
              Caption            : 'Get list of files';
              Prompt             : 'User can obtain a list of files attached to an assembly';
              Description        : 'Provides users the ability to obtain files attached to an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/fls/w';
              Caption            : 'Set list of files';
              Prompt             : 'User can set the list of files attached to an assembly';
              Description        : 'Provides users the ability to set files attached to an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Comments=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/cmnts/r';
              Caption            : 'Get list of comments';
              Prompt             : 'User can obtain a list of comments associated with an assembly';
              Description        : 'Provides users the ability to obtain comments associated with an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/cmnts/w';
              Caption            : 'Set list of comments';
              Prompt             : 'User can set the list of comments associated with an assembly';
              Description        : 'Provides users the ability to comment on an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Assemblies=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/subs/r';
              Caption            : 'Get list of sub assemblies';
              Prompt             : 'User can obtain a list of sub assemblies associated with an assembly';
              Description        : 'Provides users the ability to obtain sub assemblies associated with an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/subs/w';
              Caption            : 'Set list of sub assemblies';
              Prompt             : 'User can set the list of sub assemblies associated with an assembly';
              Description        : 'Provides users the ability to set list of sub assemblies associated with an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        State=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/ste/r';
              Caption            : 'Get the state of an assembly';
              Prompt             : 'User can obtain the state of an assembly';
              Description        : 'Provides users the ability to obtain the state of an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : false;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/ste/w';
              Caption            : 'Set the state of administrators';
              Prompt             : 'User can set the state of an assembly';
              Description        : 'Provides users the ability to set the state of an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Privacy=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/prcy/r';
              Caption            : 'Get the privacy of an assembly';
              Prompt             : 'User can obtain the privacy of an assembly';
              Description        : 'Provides users the ability to obtain the privacy of an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/prcy/w';
              Caption            : 'Set privacy of an assembly';
              Prompt             : 'User can set the privacy of an assembly';
              Description        : 'Provides users the ability to set the privacy of an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Schedule=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/asms/asm/sched/r';
              Caption            : 'Get the schedule date of an assembly';
              Prompt             : 'User can obtain the schedule date of an assembly';
              Description        : 'Provides users the ability to obtain the schedule date of an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/asms/asm/sched/w';
              Caption            : 'Set schedule date of an assembly';
              Prompt             : 'User can set the schedule date of an assembly';
              Description        : 'Provides users the ability to set the schedule date of an assembly';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Find=Class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Find';
            NameSpace            : '/asms/f';
            Caption              : 'Find Assemblies';
            Prompt               : 'Users can search for assemblies';
            Description          : 'Provides users the ability to search for assemblies';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
      end;
    end;
    Projects=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Project=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : true;
        );
      type
        Add=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Add';
            NameSpace            : '/pjs/pj/a';
            Caption              : 'Add a Project';
            Prompt               : 'Users can create a project';
            Description          : 'Provides users the ability to create a project';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Delete';
            NameSpace            : '/pjs/pj/d';
            Caption              : 'Delete a Project';
            Prompt               : 'Users can delete a project';
            Description          : 'Provides users the ability to delete a project';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Read=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Read';
            NameSpace            : '/pjs/pj/r';
            Caption              : 'Read a Project';
            Prompt               : 'Users can read a project';
            Description          : 'Provides users the ability to read a project';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Write=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Write';
            NameSpace            : '/pjs/pj/w';
            Caption              : 'Write a Project';
            Prompt               : 'Users can write a project';
            Description          : 'Provides users the ability to write a project';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Refresh=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Refresh';
            NameSpace            : '/pjs/pj/h';
            Caption              : 'Refresh a Project';
            Prompt               : 'Users can refresh a project';
            Description          : 'Provides users the ability to refresh a project';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Admins=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/pjs/pj/adms/r';
              Caption            : 'Obtain administrators for a Project';
              Prompt             : 'Users can obtain a list of administrators a project';
              Description        : 'Provides users the ability to list administrators for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/pjs/pj/adms/w';
              Caption            : 'Add administrators to a Project';
              Prompt             : 'Users can add administrators to a project';
              Description        : 'Provides users the ability to add administrators for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Comments=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/pjs/pj/cmts/r';
              Caption            : 'Obtain comments for a Project';
              Prompt             : 'Users can obtain a list of comments a project';
              Description        : 'Provides users the ability to list comments for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/pjs/pj/cmts/w';
              Caption            : 'Add comments to a Project';
              Prompt             : 'Users can add comments to a project';
              Description        : 'Provides users the ability to add comments to a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Issues=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/pjs/pj/iss/r';
              Caption            : 'Obtain issues for a Project';
              Prompt             : 'Users can obtain a list of issues a project';
              Description        : 'Provides users the ability to list issues for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/pjs/pj/iss/w';
              Caption            : 'Add issues to a Project';
              Prompt             : 'Users can add issues to a project';
              Description        : 'Provides users the ability to add issues to a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Caveats=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/pjs/pj/cvts/r';
              Caption            : 'Obtain caveats for a Project';
              Prompt             : 'Users can obtain a list of caveats for a project';
              Description        : 'Provides users the ability to list caveats for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/pjs/pj/cvts/w';
              Caption            : 'Add caveats for a Project';
              Prompt             : 'Users can add caveats for a project';
              Description        : 'Provides users the ability to add caveats for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Skills=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/pjs/pj/skls/r';
              Caption            : 'Obtain required skills for a Project';
              Prompt             : 'Users can obtain a list of skills required for a project';
              Description        : 'Provides users the ability to list skills required for a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/pjs/pj/skls/w';
              Caption            : 'Add required skills to a Project';
              Prompt             : 'Users can add required skills to a project';
              Description        : 'Provides users the ability to add skill requirements to a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
        Status=class
        type
          Read=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Read';
              NameSpace          : '/pjs/pj/stat/r';
              Caption            : 'Obtain status of a Project';
              Prompt             : 'Users can obtain status of a project';
              Description        : 'Provides users the ability to obtain the status of a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
          Write=class
          const
            ACLInf:TACLinfo=(
              Name               : 'Write';
              NameSpace          : '/pjs/pj/stat/w';
              Caption            : 'Set status of a Project';
              Prompt             : 'Users can set the status of a project';
              Description        : 'Provides users the ability to set the status of a project';
            );
            cmd:TCoreCommand=(
              HeaderP            : @Header;
              ID                 : 0;
              Enabled            : true;
              Anonymous          : false;
              Cache              : false;
              Compress           : true;
              Secure             : false;
              XMLInfo            : @XMLInf;
              ACLInfo            : @ACLInf;
              Method                 : nil;
              Resource               : nil;
            );
          end;
        end;
      end;
      List=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'List';
          NameSpace              : '/pjs/l';
          Caption                : 'List Projects';
          Prompt                 : 'Users can list projects';
          Description            : 'Provides users the ability to list projects';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
    Tasks=class
    const
      XMLInf:TXMLInfo=(
        Enabled                  : true;
      );
    type
      Task=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : true;
        );
      type
        Add=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Add';
            NameSpace            : '/tsks/tsk/a';
            Caption              : 'Add New Task';
            Prompt               : 'Users can add new tasks';
            Description          : 'Provides users the ability to add new tasks';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Delete=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Delete';
            NameSpace            : '/tsks/tsk/d';
            Caption              : 'Delete Task';
            Prompt               : 'Users can delete tasks';
            Description          : 'Provides users the ability to remove tasks';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Read=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Read';
            NameSpace            : '/tsks/tsk/r';
            Caption              : 'Read Tasks';
            Prompt               : 'Users can read tasks';
            Description          : 'Provides users the ability to read tasks';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Write=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Write';
            NameSpace            : '/tsks/tsk/w';
            Caption              : 'Write Tasks';
            Prompt               : 'Users can edit tasks';
            Description          : 'Provides users the ability to edit tasks';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Refresh=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Refresh';
            NameSpace            : '/tsks/tsk/h';
            Caption              : 'Refresh Task';
            Prompt               : 'Users can refresh a task';
            Description          : 'Provides users the ability to refresh a task';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
        Delegate=class
        const
          ACLInf:TACLinfo=(
            Name                 : 'Delegate';
            NameSpace            : '/tsks/tsk/dg';
            Caption              : 'Delegate a Task';
            Prompt               : 'Users can delegate a task';
            Description          : 'Provides users the ability to delegate a task';
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
            Method                 : nil;
            Resource               : nil;
          );
        end;
      end;
      List=class
      const
        ACLInf:TACLinfo=(
          Name                   : 'List';
          NameSpace              : '/tsks/l';
          Caption                : 'List Tasks';
          Prompt                 : 'Users can list tasks';
          Description            : 'Provides users the ability to list tasks';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
  end;
  TSpectrumCore=Class(TCoreObject)
  private
    iRemoteIP                    : QWord;
    FID                          : QWord;
    FLID                         : QWord;
    FItemsPerPage                : Word;
    FPage                        : Word;
    FRecvHeadersCount            : Word;
    FYear                        : Word;
    FLastYear                    : Word;
    FLength                      : LongInt;
    FAuthLength                  : LongInt;
    FSData                       : TFileStream;
    DataP                        : PHTTP;
    FFileFlags                   : LongInt;
    FMime                        : Storage.UserStorage.Items.SMTP.TMime;
    FEmail                       : Storage.UserStorage.Files.TItem;
    FEmails                      : Storage.UserStorage.Files.TItems;
    FEmailRefactor               : Storage.UserStorage.Files.TItems;
    FEmailStats                  : Storage.UserStorage.Items.SMTP.TStats;
    FFolder                      : Storage.UserStorage.Folders.TFolder;
    FFolders                     : Storage.UserStorage.Folders.TFolders;
    FSummary                     : Core.Strings.VarString;
    FEntry                       : Core.Strings.VarString;
    FEntry2                      : Core.Strings.VarString;
    FSMTPSummary                 : Storage.UserStorage.Items.SMTP.TSummary;
    FSignature                   : Storage.Signatures.Items.Item;
    FSignatures                  : Storage.Signatures.Items.List;
    FSpamFilter                  : Storage.Security.Filter.Item;
    FContact                     : Storage.Roster.Items.Item;
    FRoster                      : Storage.Roster.Items.List;
    RCP                          : Storage.VDM.Resources.PResource;
    FTasks                       : Storage.Tasks.Items.List;
    FTaskItem                    : Storage.Tasks.Items.Item;
    FProject                     : Storage.Projects.Items.TProject;
    FProjects                    : Storage.Projects.Items.TProjects;
    FAssemblies                  : Storage.Assemblies.Items.List;
    FAssembly                    : Storage.Assemblies.Items.Item;
    FSubscriptions               : Storage.Assemblies.Subscription.List;
    FSubscription                : Storage.Assemblies.Subscription.Item;
    FIMAPRecords                 : Storage.UserStorage.Items.IMAP.TRecordCount;
    FXMLParser                   : TDOMParser;
    FDocument                    : TXMLDocument;
    FSource                      : TXMLInputSource;
    FNode                        : TDOMNode;

  private
    // Contact Management
    function  Contact_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Contact_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Contact_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Contact_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Contact_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Contacts_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Contact_SetAvatar(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Folder
    function  Folder_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Folder_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Folder_Rename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Folder_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    // Emails
    function  Email_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Move(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Clear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Archive(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Stat(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Data(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Mime(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_MGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_Count(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    function  Email_UpdateSummary(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
    // Signature Files
    function  Signature_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Signature_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Signature_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Signature_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Signature_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Signature_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    // Task Managment
    function  Task_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Task_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Task_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Task_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Task_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Tasks_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    // Projects
    function  Project_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Project_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Project_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Project_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Project_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Projects_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    Function  Project_Admins_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Admins_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Caveats_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Caveats_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Comments_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Comments_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Issues_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Issues_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Skills_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Skills_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Status_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    Function  Project_Status_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;


    // Assembly Management
    function  Assembly_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    function  Assembly_Admins_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Admins_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Files_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Files_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Comments_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Comments_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Assemblies_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Assemblies_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_State_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_State_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Privacy_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Privacy_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Scheduled_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Assembly_Scheduled_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    function  Assembly_Find(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    // Subscriptions
    function  Subscriptions_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Subscription_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Subscription_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Subscription_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Subscription_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Subscription_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses SysUtils,DateUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TSpectrumCore.Install(Task);
end;

class procedure TSpectrumCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TSpectrumCore);
  with Spectrum do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    with Contacts do begin
      with Contact do begin
        COREOBJECT_VerifyID(Task,Add.cmd);
        COREOBJECT_VerifyID(Task,Delete.cmd);
        COREOBJECT_VerifyID(Task,Read.cmd);
        COREOBJECT_VerifyID(Task,Write.cmd);
        COREOBJECT_VerifyID(Task,Refresh.cmd);
        COREOBJECT_VerifyID(Task,SetAvatar.cmd);
      end;
      COREOBJECT_VerifyID(Task,List.cmd);
    end;
    with Email do begin
      COREOBJECT_VerifyID(Task,Count.cmd);
      COREOBJECT_VerifyID(Task,Stat.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Move.cmd);
      COREOBJECT_VerifyID(Task,Data.cmd);
      COREOBJECT_VerifyID(Task,Mime.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Refresh.cmd);
      COREOBJECT_VerifyID(Task,UpdateSummary.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Archive.cmd);
      COREOBJECT_VerifyID(Task,Clear.cmd);
      COREOBJECT_VerifyID(Task,MGet.cmd);
    end;
    with Signatures do begin
      with Signature do begin
        COREOBJECT_VerifyID(Task,Add.cmd);
        COREOBJECT_VerifyID(Task,Read.cmd);
        COREOBJECT_VerifyID(Task,Write.cmd);
        COREOBJECT_VerifyID(Task,Delete.cmd);
        COREOBJECT_VerifyID(Task,Refresh.cmd);
        COREOBJECT_VerifyID(Task,List.cmd);
      end;
    end;
    // Tasks
    COREOBJECT_VerifyID(Task,Tasks.Task.Add.cmd);
    COREOBJECT_VerifyID(Task,Tasks.Task.Delete.cmd);
    COREOBJECT_VerifyID(Task,Tasks.Task.Read.cmd);
    COREOBJECT_VerifyID(Task,Tasks.Task.Write.cmd);
    COREOBJECT_VerifyID(Task,Tasks.Task.Refresh.cmd);
    COREOBJECT_VerifyID(Task,Tasks.List.cmd);
    // Projects
    with Projects do begin
      with Project do begin
        COREOBJECT_VerifyID(Task,Add.cmd);
        COREOBJECT_VerifyID(Task,Delete.cmd);
        COREOBJECT_VerifyID(Task,Read.cmd);
        COREOBJECT_VerifyID(Task,Write.cmd);
        COREOBJECT_VerifyID(Task,Refresh.cmd);
        with Admins do begin
          COREOBJECT_VerifyID(Task,Read.cmd);
          COREOBJECT_VerifyID(Task,Write.cmd);
        end;
        with Caveats do begin
          COREOBJECT_VerifyID(Task,Read.cmd);
          COREOBJECT_VerifyID(Task,Write.cmd);
        end;
        with Comments do begin
          COREOBJECT_VerifyID(Task,Read.cmd);
          COREOBJECT_VerifyID(Task,Write.cmd);
        end;
        with Issues do begin
          COREOBJECT_VerifyID(Task,Read.cmd);
          COREOBJECT_VerifyID(Task,Write.cmd);
        end;
        with Skills do begin
          COREOBJECT_VerifyID(Task,Read.cmd);
          COREOBJECT_VerifyID(Task,Write.cmd);
        end;
        with Status do begin
          COREOBJECT_VerifyID(Task,Read.cmd);
          COREOBJECT_VerifyID(Task,Write.cmd);
        end;
      end;
      COREOBJECT_VerifyID(Task,List.cmd);
    end;
    with Assemblies do begin
      with Assembly do begin
        COREOBJECT_VerifyID(Task,Add.cmd);
        COREOBJECT_VerifyID(Task,Delete.cmd);
        COREOBJECT_VerifyID(Task,Read.cmd);
        COREOBJECT_VerifyID(Task,Write.cmd);
        COREOBJECT_VerifyID(Task,Refresh.cmd);
        COREOBJECT_VerifyID(Task,Find.cmd);
      end;
    end;
  end;
end;

class procedure TSpectrumCore.UnInstall;
begin
  UnRegisterClass(TSpectrumCore);
end;

procedure TSpectrumCore.Initialize;
begin
  FXMLParser:=TDOMParser.Create();
  FXMLParser.Options.Validate:=False;
  //FXMLParser.Options.ConformanceLevel:=clFragment;
  FDocument:=nil;
  FSource:=nil;
  with Spectrum do begin
    // Contacts
    with Contacts do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@Contacts_List);
      with Contact do begin
        Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Contact_Add);
        Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Contact_Delete);
        Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Contact_Read);
        Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Contact_Write);
        Storage.CoreObjects.Add(Refresh.cmd,FCommands,Header,@Contact_Refresh);
        Storage.CoreObjects.Add(SetAvatar.cmd,FCommands,Header,@Contact_SetAvatar);
      end;
    end;
    with Email do begin
      Storage.CoreObjects.Add(Count.cmd,FCommands,Header,@Email_Count);
      Storage.CoreObjects.Add(Stat.cmd,FCommands,Header,@Email_Stat);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Email_Add);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@Email_List);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Email_Delete);
      Storage.CoreObjects.Add(Move.cmd,FCommands,Header,@Email_Move);
      Storage.CoreObjects.Add(Data.cmd,FCommands,Header,@Email_Data);
      Storage.CoreObjects.Add(Mime.cmd,FCommands,Header,@Email_Mime);
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Email_Read);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Email_Write);
      Storage.CoreObjects.Add(Refresh.cmd,FCommands,Header,@Email_Refresh);
      Storage.CoreObjects.Add(UpdateSummary.cmd,FCommands,Header,@Email_UpdateSummary);
      Storage.CoreObjects.Add(Clear.cmd,FCommands,Header,@Email_Clear);
      Storage.CoreObjects.Add(Archive.cmd,FCommands,Header,@Email_Archive);
      Storage.CoreObjects.Add(MGet.cmd,FCommands,Header,@Email_MGet);
    end;
    with Signatures do begin
      with Signature do begin
        Storage.CoreObjects.Add(List.cmd,FCommands,Header,@Signature_List);
        Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Signature_Add);
        Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Signature_Delete);
        Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Signature_Read);
        Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Signature_Write);
        Storage.CoreObjects.Add(Refresh.cmd,FCommands,Header,@Signature_Refresh);
      end;
    end;
    Storage.CoreObjects.Add(Tasks.List.cmd,FCommands,Header,@Tasks_List);
    Storage.CoreObjects.Add(Tasks.Task.Add.cmd,FCommands,Header,@Task_Add);
    Storage.CoreObjects.Add(Tasks.Task.Delete.cmd,FCommands,Header,@Task_Delete);
    Storage.CoreObjects.Add(Tasks.Task.Read.cmd,FCommands,Header,@Task_Read);
    Storage.CoreObjects.Add(Tasks.Task.Write.cmd,FCommands,Header,@Task_Write);
    Storage.CoreObjects.Add(Tasks.Task.Refresh.cmd,FCommands,Header,@Task_Refresh);
    with Projects do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@Projects_List);
      with Project do begin
        Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Project_Add);
        Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Project_Delete);
        Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Read);
        Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Write);
        Storage.CoreObjects.Add(Refresh.cmd,FCommands,Header,@Project_Refresh);
        with Admins do begin
          Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Admins_Read);
          Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Admins_Write);
        end;
        with Caveats do begin
          Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Caveats_Read);
          Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Caveats_Write);
        end;
        with Comments do begin
          Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Comments_Read);
          Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Comments_Write);
        end;
        with Issues do begin
          Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Issues_Read);
          Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Issues_Write);
        end;
        with Skills do begin
          Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Skills_Read);
          Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Skills_Write);
        end;
        with Status do begin
          Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Project_Status_Read);
          Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Project_Status_Write);
        end;
      end;
    end;
    with Assemblies do begin
      with Assembly do begin
        Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@Assembly_Add);
        Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@Assembly_Delete);
        Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@Assembly_Read);
        Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@Assembly_Write);
        Storage.CoreObjects.Add(Refresh.cmd,FCommands,Header,@Assembly_Refresh);
        Storage.CoreObjects.Add(Find.cmd,FCommands,Header,@Assembly_Find);
      end;
    end;
  end;
end;

procedure TSpectrumCore.Finalize;
begin
  FreeAndNil(FXMLParser);
  FreeAndNil(FSource);
  FreeAndNil(FDocument);

  Storage.UserStorage.Files.Done(FEmail);
  Storage.UserStorage.Files.Done(FEmails);
  Storage.UserStorage.Folders.Done(FFolder,Storage.UserStorage.Folders.FREE_FILES);
  Storage.UserStorage.Folders.Done(FFolders,Storage.UserStorage.Folders.FREE_FILES);



  Storage.Roster.Items.Done(FContact);
  Storage.Tasks.Items.Done(FTaskItem);
  Storage.Tasks.Items.Done(FTasks);
  Storage.Projects.Items.Done(FProjects);
  Storage.Projects.Items.Done(FProject);
  Storage.Roster.Items.Done(FRoster);
  Storage.Assemblies.Items.Done(FAssemblies);
  Storage.Assemblies.Items.Done(FAssembly);
  Storage.Assemblies.Subscription.Done(FSubscriptions);
  Storage.Assemblies.Subscription.Done(FSubscription);
end;

function  TSpectrumCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  FRecvHeadersCount:=Length(srcHeaders);
  FItemsPerPage:=0;
  FLID:=0;

  FFileFlags:=0;
  Result:=CO_STATUS_OK;
  DataP:=SR.Info.DataP;
  RCP:=SR.Resource;
  FSData:=nil;
  FID:=0;

  Storage.UserStorage.Items.IMAP.Empty(FIMAPRecords);
  Storage.Security.Filter.Empty(FSpamFilter);

  Core.Strings.Empty(FSummary);
  Core.Strings.Empty(FEntry);
  Core.Strings.Empty(FEntry2);

  Storage.UserStorage.Files.Empty(FEmail);
  Storage.UserStorage.Files.Empty(FEmails);
  Storage.UserStorage.Folders.Empty(FFolder,Storage.UserStorage.Folders.FREE_FILES);
  Storage.UserStorage.Folders.Empty(FFolders,Storage.UserStorage.Folders.FREE_FILES);
end;

function  TSpectrumCore.Contact_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Roster.Items.fromXML(FXMLDocument,FContact,OwnerP^.DomainP^.ID,UAP(SR)^.ID) then begin
      if RCP<>nil then
        FContact.ResourceID:=RCP^.ID;
      if Storage.Roster.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,Storage.Roster.Items.Defaults.NoAccount,FContact) then begin
        Storage.Roster.Items.toXML(FContact,Transport(SR).Output);
        Result:=CO_STATUS_OK
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Contact_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Roster.Items.fromXML(FXMLDocument,FContact, OwnerP^.DomainP^.ID,UAP(SR)^.ID) then begin
      if RCP<>nil then
        FContact.ResourceID:=RCP^.ID;
      if Storage.Roster.Items.DB.Refresh(FTask,FContact) then begin
        Result:=CO_STATUS_OK;
        Storage.Roster.Items.toXML(FContact,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Contact_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil)  then begin
    if Storage.Roster.Items.fromXML(FXMLDocument,FContact, OwnerP^.DomainP^.ID,UAP(SR)^.ID) then begin
      if RCP<>nil then
        FContact.ResourceID:=RCP^.ID;
      if Storage.Roster.Items.DB.Read(FTask,FContact) then begin
        Result:=CO_STATUS_OK;
        Storage.Roster.Items.toXML(FContact,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Contact_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Roster.Items.fromXML(FXMLDocument,FContact,OwnerP^.DomainP^.ID,UAP(SR)^.ID) then begin
      if RCP<>nil then
        FContact.ResourceID:=RCP^.ID;
      if Storage.Roster.Items.DB.Update(FTask,FContact) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Contact_SetAvatar(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Roster.Items.fromXML(FXMLDocument,FContact,OwnerP^.DomainP^.ID,UAP(SR)^.ID) and (FContact.ID<>0) and (FContact.AvatarID<>0) then begin
      if Storage.Roster.Items.DB.setAvatarID(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FContact.ID,FContact.AvatarID,FContact.Modified) then begin
        Storage.Roster.Items.toXML(FContact,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;


function  TSpectrumCore.Contact_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Roster.Items.fromXML(FXMLDocument,FContact,OwnerP^.DomainP^.ID,UAP(SR)^.ID) then begin
      if RCP<>nil then
        FContact.ResourceID:=RCP^.ID;
      if Storage.Roster.Items.DB.Delete(FTask,FContact.DomainID,FContact.UserID,FContact.ID) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Contacts_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Roster.Items.DB.Fill(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, Resources.getID(RCP),@FRoster) then begin
      Storage.Roster.Items.toXML(FRoster,Transport(SR).Output);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.UserStorage.Files.fromXML(FXMLDocument,FEmail) and (FEmail.FolderID<>0) ) then begin
      // there is no data here
      if Storage.UserStorage.Files.DB.Add(FTask, UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FEmail.FolderID,FEmail,nil) then begin
        Storage.UserStorage.Files.toXML(FEmail,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;

    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if  ((Storage.UserStorage.Files.fromXML(FXMLDocument,FEmails)) and (System.Length(FEmails)>0) ) then begin
      FItemsPerPage:=Core.Arrays.KeyString.GetItemAsWord(recvHeaders,fieldScale,FRecvHeadersCount,50);
      for iLcv:=0 to High(FEmails) do begin
        FID:=FEmails[iLcv]^.FolderID;
        if (FID=UAP(SR)^.Trashbox) then begin
          Storage.UserStorage.Files.DB.Delete(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FID,FEmails[iLcv]^.ID);
        end else begin
          Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FID,UAP(SR)^.Trashbox);
        end;
        Result:=CO_STATUS_OK;
      end;
      Storage.UserStorage.Files.Empty(FEmails);
      Storage.UserStorage.Files.DB.Count(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FID,FIMAPRecords);
      Storage.UserStorage.Items.IMAP.Paginate(FIMAPRecords,FItemsPerPage);
      Storage.UserStorage.Items.IMAP.toXML(FIMAPRecords,Transport(SR).Output,XML_HEADER_ON);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Move(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iLcv:integer;

  procedure PushStatus();
  begin
    if FLID<>0 then begin
      Storage.UserStorage.Files.Empty(FEmails);
      Storage.UserStorage.Files.DB.Count(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FLID,FIMAPRecords);
      Storage.UserStorage.Items.IMAP.Paginate(FIMAPRecords,FItemsPerPage);
      Storage.UserStorage.Items.IMAP.toXML(FIMAPRecords,Transport(SR).Output,XML_HEADER_ON);
    end;
  end;

  procedure SetupEntries();
  begin
    SetLength(FEntry,0);
    SetLength(FEntry2,0);
    SetLength(FEntry,0);
    Core.XML.DB.Wrap(Core.XML.DB.Header(Storage.Main.Header.Encoding),Storage.UserStorage.Items.SMTP.XML.Stanza,FSummary);
    Try
      FSource:=TXMLInputSource.Create(FSummary);
      try
        FXMLParser.Parse(FSource,FDocument);
        if Storage.UserStorage.Items.SMTP.fromXML(FDocument,FSMTPSummary) then begin
          FEntry:=Storage.Security.Filter.getTopLevel(FSMTPSummary.RemoteDomain);
          FEntry2:=FSMTPSummary.RemoteIP;
        end;
      Finally
        FreeAndNil(FSource);
      end;
    Finally
      FreeAndNil(FDocument);
    end;
    SetLength(FSummary,0);
  end;

  procedure BlacklistEntries();
  begin
    if (System.Length(FEntry)>0) then begin
      FEntry:=Lowercase(FEntry);
      Storage.Security.Filter.Empty(FSpamFilter);
      FSpamFilter.Counter:=1;
      FSpamFilter.Enabled:=True;
      FSpamFilter.Value:=FEntry;
      Storage.Security.Filter.DB.Identify(FTask,secBlacklist,FSpamFilter);
      Storage.Security.Filter.Empty(FSpamFilter);
    end;
    if (System.Length(FEntry2)>0) then begin
      Storage.Security.Filter.Empty(FSpamFilter);
      FSpamFilter.Counter:=1;
      FSpamFilter.Enabled:=True;
      FSpamFilter.Value:=Core.Utils.Sockets.MaskClassC(FEntry2);
      Storage.Security.Filter.DB.Identify(FTask,secViolatorIP,FSpamFilter);
      Storage.Security.Filter.Empty(FSpamFilter);
    end;
  end;

  procedure WhitelistEntries();
  begin
    if (System.Length(FEntry)>0) then begin
      FEntry:=Lowercase(FEntry);
      Storage.Security.Filter.Empty(FSpamFilter);
      FSpamFilter.Counter:=1;
      FSpamFilter.Enabled:=True;
      FSpamFilter.Value:=FEntry;
      Storage.Security.Filter.DB.Blacklist_Delist(FTask,FEntry);
      Storage.Security.Filter.DB.Identify(FTask,secWhitelist,FSpamFilter);
      Storage.Security.Filter.Empty(FSpamFilter);
    end;
    if (System.Length(FEntry2)>0) then begin
      iRemoteIP:=Core.Utils.Sockets.InAddrFromStr(FEntry2);
      Storage.Intrusion.Intruder.DB.Delete(FTask,OwnerP^.DomainP^.ID,iRemoteIP);
      Storage.Security.Filter.DB.Violator_Delist(FTask,FEntry2);
    end;
  end;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch,FRecvHeadersCount,0);
    FItemsPerPage:=Core.Arrays.KeyString.GetItemAsWord(recvHeaders,fieldScale,FRecvHeadersCount,50);
    if (FID>0) then begin
      if  ( Storage.UserStorage.Files.fromXML(FXMLDocument,FEmails) and (System.Length(FEmails)>0) ) then begin
        if (FID=UAP(SR)^.SpamBox) then begin
          for iLcv:=0 to High(FEmails) do begin
            if Storage.UserStorage.Files.DB.GetSummary(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FSummary) then begin
              SetupEntries();
              BlacklistEntries();
            end;
            Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FEmails[iLcv]^.FolderID,FID);
            FLID:=FEmails[iLcv]^.FolderID;
            Result:=CO_STATUS_OK;
          end;
        end else if (FID=UAP(SR)^.Inbox) then begin
          for iLcv:=0 to High(FEmails) do begin
            If (FEmails[iLcv]^.FolderID=UAP(SR)^.Spambox) then begin
              if Storage.UserStorage.Files.DB.GetSummary(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FSummary) then begin
                SetupEntries();
                WhitelistEntries();
              end;
            end;
            Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FEmails[iLcv]^.FolderID,FID);
            FLID:=FEmails[iLcv]^.FolderID;
            Result:=CO_STATUS_OK;
          end;
        end else if (FID<>UAP(SR)^.Trashbox) then begin
          for iLcv:=0 to High(FEmails) do begin
            if (FEmails[iLcv]^.FolderID=UAP(SR)^.SpamBox) then begin
              if Storage.UserStorage.Files.DB.GetSummary(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FSummary) then begin
                SetupEntries();
                WhitelistEntries();
              end;
            end;
            Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FEmails[iLcv]^.FolderID,FID);
            FLID:=FEmails[iLcv]^.FolderID;
            Result:=CO_STATUS_OK;
          end;
        end else begin
          for iLcv:=0 to High(FEmails) do begin
            Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FEmails[iLcv]^.FolderID,FID);
            FLID:=FEmails[iLcv]^.FolderID;
          end;
          Result:=CO_STATUS_OK;
        end;
        PushStatus();
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Stat(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  FolderID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FolderID:=UAP(SR)^.Inbox;
    if (FolderID>0) then begin
      if Storage.UserStorage.Items.SMTP.Stats(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FolderID,FEmailStats) then begin
        Storage.UserStorage.Items.SMTP.toXML(FEmailStats,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    FItemsPerPage:=Core.Arrays.KeyString.GetItemAsWord(recvHeaders,fieldScale,FRecvHeadersCount,50);
    FPage:=Core.Arrays.KeyString.GetItemAsWord(recvHeaders,fieldDepth,FRecvHeadersCount,1);
    if (FID>0) then begin
      if Storage.UserStorage.Files.DB.List(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FID, FEmails,Storage.UserStorage.Files.DB.IDs.Created) then begin
        Try
          Storage.UserStorage.Files.Paginate(FEmails,FEmailRefactor,FItemsPerPage,FPage);
          Storage.UserStorage.Files.toXML(FEmailRefactor,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        finally
          SetLength(FEmailRefactor,0); // pointer copies to FEMails.
          Storage.UserStorage.Files.Empty(FEmails);
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Clear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FID>0) then begin
      if Storage.UserStorage.Files.DB.Clear(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FID) then begin
        Storage.UserStorage.Items.IMAP.Empty(FIMAPRecords);
        Storage.UserStorage.Items.IMAP.toXML(FIMAPRecords,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Archive(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iLcv:integer;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch);
    if (FID>0) then begin
      if Storage.UserStorage.Files.DB.List(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FID, FEmails,Storage.UserStorage.Files.DB.IDs.Created) then begin
        FID:=UAP(SR)^.ArchiveBox;
        FLastYear:=0;
        For iLcv:=0 to High(FEmails) do begin
          FYear:=DateUtils.YearOf(FEmails[iLcv]^.Created);
          if (FLastYear<>FYear) then begin
            FEntry:=Concat(Folders.Defaults.Home.Mail+'/'+Folders.Defaults.Mail.Archive,'/',IntToStr(FYear));
            Storage.UserStorage.Folders.DB.Force(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FEntry,FID);
          end;
          Storage.UserStorage.Files.DB.Move(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmails[iLcv]^.ID,FEmails[iLcv]^.FolderID,FID);
          FLastYear:=FYear;
        end;
        Storage.UserStorage.Items.IMAP.Empty(FIMAPRecords);
        Storage.UserStorage.Items.IMAP.toXML(FIMAPRecords,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Count(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch,FRecvHeadersCount,0);
    FItemsPerPage:=Core.Arrays.KeyString.GetItemAsWord(recvHeaders,fieldScale,FRecvHeadersCount,50);
    if (FID>0) then begin
      if Storage.UserStorage.Files.DB.Count(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FID,FIMAPRecords) then begin
        Storage.UserStorage.Items.IMAP.Paginate(FIMAPRecords,FItemsPerPage);
        Storage.UserStorage.Items.IMAP.toXML(FIMAPRecords,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Files.fromXML(FXMLDocument,FEmail) then begin
      if Storage.UserStorage.Files.DB.Refresh(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmail) then begin
        Storage.UserStorage.Files.toXML(FEmail,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Files.fromXML(FXMLDocument,FEmail) and (FEmail.ID<>0) then begin
      if Storage.UserStorage.Files.DB.Fill(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmail.ID,FEmail,FSData) then begin
        Try
          Storage.UserStorage.Files.toXML(FEmail,FSData,Transport(SR).Output,FRefactor,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        finally
          FSData.Free();
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Data(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Files.fromXML(FXMLDocument,FEmail) then begin
      if Storage.UserStorage.Files.DB.Data(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmail,FSData) then begin
        Try
          Core.Streams.Copy(FSData,Transport(SR).Output);
        finally
          FSData.Free();
        end;
        Core.Arrays.KeyString.Update(respHeaders,fieldContentType,ctStream);
        //Storage.UserStorage.Files.toXML(FEmail,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_Mime(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch,0,0);
    if (FID<>0) then begin
      if Storage.UserStorage.Items.SMTP.fromXML(FXMLDocument,FMime) then begin
        FEmail.ID:=FID;
        if Storage.UserStorage.Files.DB.Data(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FEmail,FMime,FRefactor,Transport(SR).Output) then begin
          Core.Arrays.KeyString.Update(respHeaders,fieldContentType,ctMIME);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_MGet(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?----0---&---1--&--2--&-3-&----4---&-----5-----&-6-&
  // Parameters from /core/spc?eml/getm&FileId&start&end&encoding&disposition&name
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
    if (FLength>6) then begin
      FEmail.ID:=StrToQWordDef(Parameters[1],0);  // File ID
      FMime.idxContentStart:=StrToIntDef(Parameters[2],0);
      FMime.idxContentEnd:=StrToIntDef(Parameters[3],0);
      FMime.cntEncoding:=StrToIntDef(Parameters[4],0);
      FMime.cntDisposition:=StrToIntDef(Parameters[5],0);
      FMime.cntName:=Parameters[6];
      if ( (FEmail.ID<>0) and (FMime.idxContentStart<>0) and (FMime.idxContentEnd<>0) and (FMime.cntEncoding<>0) and (Length(FMime.cntName)>0) ) then begin
        if (SR.Credentials<>nil) then begin
          FEntry:=Core.Utils.Files.Extract(FMime.cntName,efeoNone);
          if Storage.UserStorage.Files.DB.Data(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FEmail,FMime,FRefactor,Transport(SR).Output) then begin
            Encryption.Base64.Decode(Transport(SR).Output,FRefactor,FRefactor);
            Core.Streams.Copy(FRefactor,Transport(SR).Output);
            FRefactor.Size:=0;
            Core.Arrays.KeyString.Update(respHeaders,fieldContentEncoding,Storage.UserStorage.Items.SMTP.Encoding.Value[Storage.UserStorage.Items.SMTP.Encoding.emBinary]);
            Core.Arrays.KeyString.Update(respHeaders,fieldContentTransferEncoding,Storage.UserStorage.Items.SMTP.Encoding.Value[Storage.UserStorage.Items.SMTP.Encoding.emBinary]);
            Core.Arrays.KeyString.Update(respHeaders,fieldContentType,ContentTypeFromFile(Storage.ContentTypes.List,FEntry));
            Core.Arrays.KeyString.Update(
              respHeaders,
              fieldContentDisposition,
              Concat(
                Storage.UserStorage.Items.SMTP.Content.Disposition.Value[FMime.cntDisposition],'; ',
                'filename="',FMime.cntName,'"'
              )
            );
            Core.Arrays.VarString.Empty(Parameters);
            Core.Strings.Empty(FEntry);
            Core.Strings.Empty(FEntry2);
            Result:=CO_STATUS_OK;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end;
  end;
end;

function  TSpectrumCore.Email_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if ( Storage.UserStorage.Files.fromXML(FXMLDocument,FEmail) and (FEmail.FolderID<>0) and (FEMail.ID<>0)) then begin
      if Storage.UserStorage.Files.DB.Write(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FEmail, true) then begin
        Storage.UserStorage.Files.toXML(FEmail,Transport(SR).Output,FRefactor,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Email_UpdateSummary(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserStorage.Items.SMTP.fromXML(FXMLDocument,FSMTPSummary) and (FSMTPSummary.ID<>0) then begin
      Storage.UserStorage.Files.DB.getFlags(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID,FSMTPSummary.ID,FFileFlags);
      FFileFlags:=Storage.UserStorage.Items.SMTP.getFlags(FSMTPSummary,FFileFlags);
      if Storage.UserStorage.Items.SMTP.Write(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FSMTPSummary,FFileFlags) then begin
        Storage.UserStorage.Items.SMTP.toXML(FSMTPSummary,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Folder_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.UserStorage.Folders.fromXML(FXMLDocument,FFolder) then begin
      if Storage.UserStorage.Folders.DB.Create(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FFolder.ID,FFolder.Path) then begin
        Storage.UserStorage.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TSpectrumCore.Folder_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  sPath:Core.Strings.VarString;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    sPath:=Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldSearch);
    if (Length(sPath)>0) then begin
      if Storage.UserStorage.Folders.DB.List(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID,sPath,FFolders) then begin
        Storage.UserStorage.Folders.toXML(FFolders,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else begin
      if Storage.UserStorage.Folders.DB.List(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID,FFolders) then begin
        Storage.UserStorage.Folders.toXML(FFolders,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Folder_Rename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Folders.fromXML(FXMLDocument,FFolder) then begin
      if Storage.UserStorage.Folders.DB.Rename(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID,FFolder) then begin
        Storage.UserStorage.Folders.toXML(FFolder,Transport(SR).Output,XML_HEADER_ON);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Folder_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.UserStorage.Folders.fromXML(FXMLDocument,FFolder) then begin
      if Storage.UserStorage.Folders.DB.Delete(FTask,UAP(SR)^.AuraNode,OwnerP^.DomainP^.ID, UAP(SR)^.ID,FFolder.ID) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Signature_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if Storage.Signatures.Items.fromXML(FXMLDocument,FSignature) then begin
      if Storage.Signatures.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSignature) then begin
        Storage.Signatures.Items.toXML(FSignature,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Signature_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Signatures.Items.DB.List(FTask,OwnerP^.DomainP^.ID, UAP(SR)^.ID, FSignatures) then begin
      Storage.Signatures.Items.toXML(FSignatures,Transport(SR).Output);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Signature_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Signatures.Items.fromXML(FXMLDocument,FSignature) and (FSignature.ID>0) then begin
      if Storage.Signatures.Items.DB.Write(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSignature) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Signature_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Signatures.Items.fromXML(FXMLDocument,FSignature) and (FSignature.ID>0) then begin
      if Storage.Signatures.Items.DB.Refresh(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSignature) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


function  TSpectrumCore.Signature_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Signatures.Items.fromXML(FXMLDocument,FSignature) then begin
      if Storage.Signatures.Items.DB.Delete(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSignature.ID) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Signature_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Signatures.Items.fromXML(FXMLDocument,FSignature) then begin
      if Storage.Signatures.Items.DB.Read(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSignature) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Find(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  sSearch:Core.Strings.VarString;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    sSearch:=Core.Arrays.KeyString.GetItemByKey(recvHeaders,fieldSearch);
    if (System.Length(sSearch)>0) then begin
      if Storage.Assemblies.Items.DB.Find(FTask,OwnerP^.DomainP^.ID,sSearch,FAssemblies) then begin
        Storage.Assemblies.Items.toXML(FAssemblies,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Assembly_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) then begin
      if Storage.Assemblies.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FAssembly) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Assembly_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Read(FTask,FAssembly) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Write(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FAssembly) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Delete(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FAssembly.ID) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Assembly_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Refresh(FTask,FAssembly) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Assembly_Admins_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getAdmins(FTask,FAssembly.ID,FAssembly.Admins) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Admins_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setAdmins(FTask,FAssembly.ID,FAssembly.Admins) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Files_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getFiles(FTask,FAssembly.ID,FAssembly.Files) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Files_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setFiles(FTask,FAssembly.ID,FAssembly.Files) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Comments_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getComments(FTask,FAssembly.ID,FAssembly.Comments) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Comments_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setComments(FTask,FAssembly.ID,FAssembly.Comments) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Assemblies_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getAssemblies(FTask,FAssembly.ID,FAssembly.Assemblies) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Assemblies_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setAssemblies(FTask,FAssembly.ID,FAssembly.Assemblies) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Assembly_State_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getState(FTask,FAssembly.ID,FAssembly.State) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;

end;

function  TSpectrumCore.Assembly_State_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setState(FTask,FAssembly.ID,FAssembly.State) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Privacy_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getPrivacy(FTask,FAssembly.ID,FAssembly.Privacy) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Privacy_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setPrivacy(FTask,FAssembly.ID,FAssembly.Privacy) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Scheduled_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.getScheduled(FTask,FAssembly.ID,FAssembly.Scheduled) then begin
        Result:=CO_STATUS_OK;
        Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Assembly_Scheduled_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Items.fromXML(FXMLDocument,FAssembly) and (FAssembly.ID>0) then begin
      if Storage.Assemblies.Items.DB.Granted(FTask,UAP(SR)^.ID,FAssembly.ID) then begin
        if Storage.Assemblies.Items.DB.setScheduled(FTask,FAssembly.ID,FAssembly.Scheduled) then begin
          Result:=CO_STATUS_OK;
          Storage.Assemblies.Items.toXML(FAssembly,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Tasks_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iKind:integer;
  iID:QWord;
begin
  if SR.Credentials<>nil then begin
    iKind:=Core.Arrays.KeyString.GetItemAsInteger(recvHeaders,fieldKind,0,Storage.Tasks.Items.Kind.Unknown);
    iID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch,0,0);
    if (iKind>=Storage.Tasks.Items.Kind.Low) and (iKind<=Storage.Tasks.Items.Kind.High) then begin
      if (iID<>0) then begin
        if Storage.Tasks.Items.DB.List(FTask,iID,iKind,FTasks) then begin
          Result:=CO_STATUS_OK;
          Storage.Tasks.Items.toXML(FTasks,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Task_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Tasks.Items.fromXML(FXMLDocument,FTaskItem) then begin
      if Storage.Tasks.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FTaskItem) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Task_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Tasks.Items.fromXML(FXMLDocument,FTaskItem) then begin
      if Storage.Tasks.Items.DB.Read(FTask,FTaskItem) then begin
        Result:=CO_STATUS_OK;
        Storage.Tasks.Items.toXML(FTaskItem,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Task_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Tasks.Items.fromXML(FXMLDocument,FTaskItem) then begin
      if Storage.Tasks.Items.DB.Write(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FTaskItem) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Task_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Tasks.Items.fromXML(FXMLDocument,FTaskItem) and (FTaskItem.ID>0) then begin
      if Storage.Tasks.Items.DB.Delete(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FTaskItem.ID) then begin
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Task_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Tasks.Items.fromXML(FXMLDocument,FTaskItem) and (FTaskItem.ID>0) then begin
      if Storage.Tasks.Items.DB.Refresh(FTask,FTaskItem) then begin
        Result:=CO_STATUS_OK;
        Storage.Tasks.Items.toXML(FTaskItem,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Projects_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iID:QWord;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    iID:=Core.Arrays.KeyString.GetItemAsQWord(recvHeaders,fieldSearch,0,0);
    if iID<>0 then begin
      if Storage.Projects.Items.DB.List(FTask,OwnerP^.DomainP^.ID, iID, FProjects) then begin
        Storage.Projects.Items.toXML(FProjects,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FProject) then begin
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Read(FTask,FProject) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.Write(FTask,FProject) then begin
          Result:=CO_STATUS_OK;
          Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.Delete(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FProject.ID) then begin
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Refresh(FTask,FProject) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Admins_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.getAdmins(FTask,FProject.ID,FProject.Admins) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Admins_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.setAdmins(FTask,FProject.ID,FProject.Admins) then begin
          Result:=CO_STATUS_OK;
          Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Comments_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.getComments(FTask,FProject.ID,FProject.Comments) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Comments_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.setComments(FTask,FProject.ID,FProject.Comments) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Issues_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.getIssues(FTask,FProject.ID,FProject.Issues) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Issues_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.setIssues(FTask,FProject.ID,FProject.Issues) then begin
          Result:=CO_STATUS_OK;
          Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Skills_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.getSkills(FTask,FProject.ID,FProject.Skills) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Skills_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.setSkills(FTask,FProject.ID,FProject.Skills) then begin
          Result:=CO_STATUS_OK;
          Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Caveats_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.getCaveats(FTask,FProject.ID,FProject.Caveats) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Caveats_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.setCaveats(FTask,FProject.ID,FProject.Caveats) then begin
          Result:=CO_STATUS_OK;
          Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Status_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.getStatus(FTask,FProject.ID,FProject.Status) then begin
        Result:=CO_STATUS_OK;
        Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Project_Status_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Projects.Items.fromXML(FXMLDocument,FProject) and (FProject.ID>0) then begin
      if Storage.Projects.Items.DB.Granted(FTask,UAP(SR)^.ID,FProject.ID) then begin
        if Storage.Projects.Items.DB.setStatus(FTask,FProject.ID,FProject.Status) then begin
          Result:=CO_STATUS_OK;
          Storage.Projects.Items.toXML(FProject,Transport(SR).Output);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Subscriptions_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Subscription.DB.List(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSubscriptions) then begin
        Storage.Assemblies.Subscription.toXML(FSubscriptions,Transport(SR).Output);
        Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Subscription_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Subscription.fromXML(FXMLDocument,FSubscription) then begin
      if Storage.Assemblies.Subscription.DB.Add(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSubscription) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Subscription_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Subscription.fromXML(FXMLDocument,FSubscription) and (FSubscription.ID>0) then begin
      if Storage.Assemblies.Subscription.DB.Delete(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSubscription.ID) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Subscription_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Subscription.fromXML(FXMLDocument,FSubscription) and (FSubscription.ID<>0) then begin
      if Storage.Assemblies.Subscription.DB.Read(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSubscription) then begin
        Storage.Assemblies.Subscription.toXML(FSubscription,Transport(SR).Output);
        Result:=CO_STATUS_OK;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Subscription_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Subscription.fromXML(FXMLDocument,FSubscription) and (FSubscription.ID<>0) then begin
      if Storage.Assemblies.Subscription.DB.Write(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSubscription) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TSpectrumCore.Subscription_Refresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var recvHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Assemblies.Subscription.fromXML(FXMLDocument,FSubscription) and (FSubscription.ID<>0) then begin
      if Storage.Assemblies.Subscription.DB.Refresh(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,FSubscription) then
        Result:=CO_STATUS_OK
      else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

end.

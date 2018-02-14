unit auLang;

interface

uses
  App.Lang,
  Core.Strings,

  Classes,
  SysUtils;

Type
  Table=class(App.Lang.Table)
  Type
    Size=class
    const                                //  k   m    g    t    p     x
      Exabyte                    : QWord = 1024*1024*1024*1024*1024*1024;
      Petabyte                   : QWord = 1024*1024*1024*1024*1024;
      Terabyte                   : QWord = 1024*1024*1024*1024;
      Gigabyte                   : QWord = 1024*1024*1024;
      Megabyte                   : QWord = 1024*1024;
      Kilobyte                   : QWord = 1024;
    end;
    Symbol=class
    const
      Reload                     : Core.Strings.VarString = '↻';
    type
      Chevron=class
      const
        Right                    : Core.Strings.VarString = '❱';
        Left                     : Core.Strings.VarString = '❰';
      end;
    end;

    Input=class
    const
      Invalid                    : Core.Strings.VarString = #33+#34+#36+#37+#38+#39+#42+#58+#59+#60+#61+#62+#63+#96;
    end;
    Format=class
    Const
      MemoryConsumption          : Core.Strings.VarString = '%0:s %1:.2fMiB';
      CoreError                  : Core.Strings.VarString = 'NS_CO:%0:s'#9'NS_CC:%1:s'#9'Code:%2d';
      AccountParseErrror         : Core.Strings.VarString = 'Exception: %0s'#9'ID:%1d';
      CoreData                   : Core.Strings.VarString = 'CO:%0s'#9'CC:%1:s'#9'Code:%2d'#9'%3:s';
      DebugData                  : Core.Strings.VarString = '%0:s';
      SizeCalculation            : Core.Strings.VarString = '%0:.2f %1:s';
    end;

    Labels=class
    Const
      On                         : Core.Strings.VarString = 'On';
      Off                        : Core.Strings.VarString = 'Off';
      Browse                     : Core.Strings.VarString = 'Browse';
      Connected                  : Core.Strings.VarString = 'Connected';
      Connecting                 : Core.Strings.VarString = 'Connecting';
      Disconnected               : Core.Strings.VarString = 'Disconnected';
      Encoding                   : Core.Strings.VarString = 'Encoding';
      Importing                  : Core.Strings.VarString = 'Importing';
      Exporting                  : Core.Strings.VarString = 'Exporting';
      Scanning                   : Core.Strings.VarString = 'Scanning';
      Synchronizing              : Core.Strings.VarString = 'Synchronizing';
      Synchronize                : Core.Strings.VarString = 'Synchronize';
      SyncUpload                 : Core.Strings.VarString = 'Sync Upload';
      SyncCreate                 : Core.Strings.VarString = 'Sync Create';
      SyncDownload               : Core.Strings.VarString = 'Sync Download';
      SyncWrite                  : Core.Strings.VarString = 'Sync Write';
      SyncUpCheck                : Core.Strings.VarString = 'SyncUp check';
      SyncDownCheck              : Core.Strings.VarString = 'SyncDown check';
      Files                      : Core.Strings.VarString = 'Files';
      Folders                    : Core.Strings.VarString = 'Folders';
      Bytes                      : Core.Strings.VarString = 'Bytes';
      KiloBytes                  : Core.Strings.VarString = 'Kilobytes';
      MegaBytes                  : Core.Strings.VarString = 'Megabytes';
      GigaBytes                  : Core.Strings.VarString = 'Gigabytes';
      TeraBytes                  : Core.Strings.VarString = 'Terabytes';
      PetaBytes                  : Core.Strings.VarString = 'Petabytes';
      ExaBytes                   : Core.Strings.VarString = 'Exabytes';
      Name                       : Core.Strings.VarString = 'Name';
      Account                    : Core.Strings.VarString = 'Account';
      Password                   : Core.Strings.VarString = 'Password';
      Path                       : Core.Strings.VarString = 'Path';
      Resources                  : Core.Strings.VarString = 'Resources';
      Sync                       : Core.Strings.VarString = 'Sync';
      Status                     : Core.Strings.VarString = 'Status';
      Settings                   : Core.Strings.VarString = 'Settings';
      Total                      : Core.Strings.VarString = 'Total';
      Direction                  : Core.Strings.VarString = 'Direction';
      Placement                  : Core.Strings.VarString = 'Placement';
      Devices                    : Core.Strings.VarString = 'Devices';
      Yes                        : Core.Strings.VarString = 'Yes';
      No                         : Core.Strings.VarString = 'No';
      Edit                       : Core.Strings.VarString = 'Edit';
      Description                : Core.Strings.VarString = 'Description';
      AddNew                     : Core.Strings.VarString = 'Add New';
      Available                  : Core.Strings.VarString = 'Available';
      Select                     : Core.Strings.VarString = 'Select';
      Upload                     : Core.Strings.VarString = 'Upload';
      Used                       : Core.Strings.VarString = 'Used';
      Download                   : Core.Strings.VarString = 'Download';
    end;
    Folders=class
    const
      Documents                  : Core.Strings.VarString = 'Documents';
    Type
      Mail=class
      const
        Root                     : Core.Strings.VarString = 'Mail';
        Attachments              : Core.Strings.VarString = 'Attachments';
        Inbox                    : Core.Strings.VarString = 'Inbox';
        Outbox                   : Core.Strings.VarString = 'Outbox';
        Sent                     : Core.Strings.VarString = 'Sent';
        Trash                    : Core.Strings.VarString = 'Trash';
      end;
      class function isRestricted(sPath:Core.Strings.VarString):boolean;
    const
      Music                      : Core.Strings.VarString = 'Music';
      Pictures                   : Core.Strings.VarString = 'Pictures';
      Trash                      : Core.Strings.VarString = 'Trash';
      Videos                     : Core.Strings.VarString = 'Videos';
      Devices                    : Core.Strings.VarString = 'Devices';
    end;
    Hints=class
    const
      Folder_Count               : Core.Strings.VarString = 'Number of Folders';
      File_Count                 : Core.Strings.VarString = 'Number of Files';
    end;
    Options=class
    const
      AutoRunSync                : Core.Strings.VarString = 'Autorun synchronization';
      SaveLoginInfo              : Core.Strings.VarString = 'Save login information';
      YesNo                      : Array[boolean] of Core.Strings.VarString = ('No','Yes');
      PipeMode                   : Array[0..3] of Core.Strings.VarString = ('Off','Cabinet','Devices','Both');
      PipeFlow                   : Array[0..3] of Core.Strings.VarString = ('Off','Download','Upload','Both');
    end;
    Messages=class
    type
      Delete=class
      type
        Folder=class
        const
          Caption                : Core.Strings.VarString = 'Confirm folder deletion';
          Message                : Core.Strings.VarString = 'Are you sure you want to delete [$folder]?';
        type
          class function getMessage(sFolder:Core.Strings.VarString):Core.Strings.VarString;
        end;
        Resource=class
        const
          Caption                : Core.Strings.VarString = 'Confirm resource deletion';
          Message                : Core.Strings.VarString = 'Are you sure you want to delete resource [$resource]?';
        type
          class function getMessage(sResource:Core.Strings.VarString):Core.Strings.VarString;
        end;
      end;
    end;
    Splash=class
    const
      Files                      : Core.Strings.VarString = 'Aurawin Cloud Files';
      Resource                   : Core.Strings.VarString = '$device - $description';
      ProvideCredentials         : Core.Strings.VarString = 'Please provide your username and password.';
      AccessDenied               : Core.Strings.VarString = 'Invalid username and or password.';
      ProvideResourceInfo        : Core.Strings.VarString = 'Please provide the following information for this device.';
    type
      class function getResource(sDevice,sDescription:Core.Strings.VarString):Core.Strings.VarString;
    end;
  end;

implementation

class function Table.Splash.getResource(sDevice,sDescription:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=StringReplace(Resource,'$device',sDevice,[rfReplaceAll]);
  Result:=StringReplace(Result,'$description',sDescription,[rfReplaceAll]);
end;

class function Table.Folders.isRestricted(sPath:Core.Strings.VarString):boolean;
begin
  Result:=(
    (sPath=Documents) or
    (sPath=Mail.Root) or
    (sPath=Mail.Root+'/'+Mail.Attachments) or
    (sPath=Mail.Root+'/'+Mail.Inbox) or
    (sPath=Mail.Root+'/'+Mail.Outbox) or
    (sPath=Mail.Root+'/'+Mail.Sent) or
    (sPath=Mail.Root+'/'+Mail.Trash) or
    (sPath=Music) or
    (sPath=Pictures) or
    (sPath=Videos) or
    (sPath=Devices) or
    (Pos(Devices+'/',sPath)=1)
  );
end;

class function Table.Messages.Delete.Folder.getMessage(sFolder:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=StringReplace(Message,'$folder',sFolder,[rfReplaceAll]);
end;

class function Table.Messages.Delete.Resource.getMessage(sResource:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=StringReplace(Message,'$resource',sResource,[rfReplaceAll]);
end;

end.


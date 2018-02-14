unit coCMS;

{
 unit coCMS.pas

 This core object provides domain users the ability to have
 access to edit website content.

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}


interface

uses
  Classes,

  hHttpd,

  RSR,
  RSR.Core,
  RSR.HTTP,

  App.Consts,


  Core.Timer,
  Core.Keywords,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,
  Core.XML,

  Core.Database,
  Core.Database.Types,

  Encryption.Base64,
  Encryption.SHA,

  Core.Streams,
  Core.Strings,
  Core.Utils.Time,

  Multimedia.Image,

  Core.Utils.Files,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.CMS,
  Storage.Domains,
  Storage.FAT,
  Storage.AuraDisks,
  Storage.ContentTypes,

  SysUtils;

type
  CMS=class
  const
    ACLInf:TACLInfo=(
      Name                     : 'Content Managment';
      NameSpace                : '/core/cms';
      Caption                  : 'Content Managment Core Object';
      Prompt                   : 'User can access to content managment system';
      Description              : 'Provides domain level content managment'
    );
    CLSInf:TCLSInfo=(
      Name                     : 'TCMSCore';
      Location                 : 'coCMS.pas';
    );
    Header:TCoreObjectInfo=(
      ID                       : 0;
      ProviderID               : 0;
      Enabled                  : true;
      Anonymous                : false;
      Scale                    : 0;
      CLSInfo                  : @CLSInf;
      ACLInfo                  : @ACLInf;
    );
    XMLInf:TXMLInfo=(
      Enabled                  : true;
    );
  type
    ID=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'ID';
        NameSpace              : '/id';
        Caption                : 'Identify File';
        Prompt                 : 'User can obtain identity of resources';
        Description            : 'Provides access to identify file allocation table (files/folders)';
      );
      cmd:TCoreCommand=(
        HeaderP                : @Header;
        ID                     : 0;
        Enabled                : true;
        Anonymous              : true;
        Cache                  : false;
        Compress               : true;
        Secure                 : true;
        XMLInfo                : @XMLInf;
        ACLInfo                : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Read=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Read';
        NameSpace              : '/r';
        Caption                : 'Read Files or Folders';
        Prompt                 : 'User can read domain level resources';
        Description            : 'Provides access to read file allocation table (files/folders)';
      );
      cmd:TCoreCommand=(
        HeaderP                : @Header;
        ID                     : 0;
        Enabled                : true;
        Anonymous              : true;
        Cache                  : false;
        Compress               : true;
        Secure                 : true;
        XMLInfo                : @XMLInf;
        ACLInfo                : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Write=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Write';
        NameSpace              : '/w';
        Caption                : 'Write Files or Folders';
        Prompt                 : 'User can write domain level resources';
        Description            : 'Provides access to write to the file allocation table (files/folders)';
      );
      cmd:TCoreCommand=(
        HeaderP                : @Header;
        ID                     : 0;
        Enabled                : true;
        Anonymous              : false;
        Cache                  : false;
        Compress               : true;
        Secure                 : true;
        XMLInfo                : @XMLInf;
        ACLInfo                : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Editor=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Editor';
        NameSpace              : '/edtr';
        Caption                : 'Display Editor';
        Prompt                 : 'User can use wysiwyg editor screen';
        Description            : 'Provides access to pop-up editor screen';
      );
      cmd:TCoreCommand=(
        HeaderP                : @Header;
        ID                     : 0;
        Enabled                : true;
        Anonymous              : false;
        Cache                  : false;
        Compress               : true;
        Secure                 : true;
        XMLInfo                : @XMLInf;
        ACLInfo                : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Domain=class
    type
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/dm/r';
          Caption                : 'Read Domain';
          Prompt                 : 'User can read Domain defaults';
          Description            : 'Provides access read domain defaults from database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : true;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Write=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Write';
          NameSpace              : '/dm/w';
          Caption                : 'Write Domain';
          Prompt                 : 'User can write Domain defaults';
          Description            : 'Provides write access to domain defaults to database';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : true;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
    Templates=class
    type
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Read';
          NameSpace            : '/tp8/r';
          Caption              : 'Read Templates';
          Prompt               : 'User can read domain level template resources';
          Description          : 'Provides access to read file allocation table (files/folders) for templates';
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
      Write=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Write';
          NameSpace            : 'tp8/w';
          Caption              : 'Write templates';
          Prompt               : 'User can write domain level template resources';
          Description          : 'Provides access to write templates to the file allocation table (files/folders)';
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
      List=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'List';
          NameSpace            : 'tp8/l';
          Caption              : 'List templates';
          Prompt               : 'User can list domain level template resources';
          Description          : 'Provides access to list templates from the file allocation table (files/folders)';
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
        ACLInf:TACLInfo=(
          Name                 : 'Delete';
          NameSpace            : 'tp8/d';
          Caption              : 'Delete templates';
          Prompt               : 'User can delete domain level template resources';
          Description          : 'Provides access to delete templates from the file allocation table (files/folders)';
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
      New=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'New';
          NameSpace            : 'tp8/n';
          Caption              : 'Create templates';
          Prompt               : 'User can create domain level template resources';
          Description          : 'Provides access to create templates from the file allocation table (files/folders)';
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
          NameSpace            : '/fld/l';
          Caption              : 'List of Folders';
          Prompt               : 'User can obtain a list folders';
          Description          : 'Retrieve domain folders from Database';
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
          NameSpace            : '/fld/a';
          Caption              : 'Add a new folder';
          Prompt               : 'User can create folders';
          Description          : 'Add domain folders to Database';
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
          NameSpace            : '/fldr/d';
          Caption              : 'Delete a folder';
          Prompt               : 'User can delete folders';
          Description          : 'Delete domain folders from Database';
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
          NameSpace            : '/fldr/n';
          Caption              : 'Rename a folder';
          Prompt               : 'User can change folder name';
          Description          : 'Set name of domain folders from Database';
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
          Name                 : 'List';
          NameSpace            : '/fls/l';
          Caption              : 'List of Files';
          Prompt               : 'User can obtain a list files';
          Description          : 'Retrieve domain files in Database';
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
          Name                 : 'List';
          NameSpace            : '/fls/a';
          Caption              : 'Add a new file';
          Prompt               : 'User can create a file';
          Description          : 'Add domain files in Database';
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
          Name                 : 'List';
          NameSpace            : '/fls/d';
          Caption              : 'Delete a file';
          Prompt               : 'User can delete a file';
          Description          : 'Delete domain files in Database';
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
          Name                 : 'List';
          NameSpace            : '/fls/r';
          Caption              : 'Rename a file';
          Prompt               : 'User can rename a file';
          Description          : 'Rename domain files in Database';
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
      GetData=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'Get Data';
          NameSpace            : '/fls/ged';
          Caption              : 'Get file contents';
          Prompt               : 'User can get file contents';
          Description          : 'Retrieve domain file content';
        );
        cmd:TCoreCommand=(
          HeaderP              : @Header;
          ID                   : 0;
          Enabled              : true;
          Anonymous            : false;
          Cache                : false;
          Compress             : true;
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
          Caption              : 'Set file contents';
          Prompt               : 'User can set file contents';
          Description          : 'Write domain file content';
        );
        cmd:TCoreCommand=(
          HeaderP              : @Header;
          ID                   : 0;
          Enabled              : true;
          Anonymous            : false;
          Cache                : false;
          Compress             : true;
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
          Caption              : 'Rotate media';
          Prompt               : 'User can rotate media';
          Description          : 'Rotate domain media files';
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
      SetAttributes=class
      const
        ACLInf:TACLInfo=(
          Name                 : 'SetAtttributes';
          NameSpace            : '/fls/sfa';
          Caption              : 'Set file attributes';
          Prompt               : 'User can set file attributes';
          Description          : 'Write domain file attributes';
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
  end;

TCMSCore=Class(TCoreObject)
private
  FPagePoint                     : Storage.CMS.Items.TPagePoint;
  FURI                           : Core.Strings.VarString;
  FPath                          : Core.Strings.VarString;
  FContentType                   : Core.Strings.VarString;
  FExt                           : Core.Strings.VarString;
  FFolder                        : TDSFolder;
  FFile                          : TDSFile;
  FTempFile                      : TDSFile;
  FDomain                        : Storage.Domains.Items.TDomain;
  FDepth                         : LongInt;
  FID                            : QWord;
  FFolderID                      : QWord;
  FFileID                        : QWord;
  FAngle                         : Double;
  FBB                            : Core.Arrays.Types.Bytes;
  FLList                         : TList;  // used for LockList();
  FList                          : TList;
  FS                             : TFileStream;
private
  function  CMD_Folders_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Folders_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Folders_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Folders_Rename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
private
  function  CMD_Files_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_Rename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_GetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_SetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_Rotate(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Files_SetAttributes(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
private
  function  CMD_ID(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Editor(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
private
  function  CMD_Domain_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Domain_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
private
  // Template files store general info on template.  Domain based.
  function  CMD_Template_New(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Template_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Template_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Template_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  function  CMD_Template_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

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

uses DateUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TCMSCore.Install(Task);
end;

class procedure TCMSCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TCMSCore);
  Storage.CoreObjects.Add(CMS.Header,CoreObjectItems);
  with CMS do begin
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,ID.cmd);
    COREOBJECT_VerifyID(Task,Read.cmd);
    COREOBJECT_VerifyID(Task,Write.cmd);
    COREOBJECT_VerifyID(Task,Editor.cmd);
    with Folders do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Rename.cmd);
    end;
    with Files do begin
      COREOBJECT_VerifyID(Task,List.cmd);
      COREOBJECT_VerifyID(Task,Add.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,Rename.cmd);
      COREOBJECT_VerifyID(Task,GetData.cmd);
      COREOBJECT_VerifyID(Task,SetData.cmd);
      COREOBJECT_VerifyID(Task,Rotate.cmd);
      COREOBJECT_VerifyID(Task,SetAttributes.cmd);
    end;
    with Templates do begin
      COREOBJECT_VerifyID(Task,New.cmd);
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
      COREOBJECT_VerifyID(Task,Delete.cmd);
      COREOBJECT_VerifyID(Task,List.cmd);
    end;
    with Domain do begin
      COREOBJECT_VerifyID(Task,Read.cmd);
      COREOBJECT_VerifyID(Task,Write.cmd);
    end;
  end;

end;

class procedure TCMSCore.UnInstall;
begin
  UnRegisterClass(TCMSCore);
end;

procedure TCMSCore.Initialize;
begin
  FTempFile:=TDSFile.Create(nil,0,0,0,0,false,false,0,0,'');
  Storage.Domains.Items.Init(FDomain);
  FList:=TList.Create();
  Storage.CMS.Items.Init(FPagePoint);
  With CMS do begin
    Storage.CoreObjects.Add(ID.cmd,FCommands,Header,@CMD_ID);
    Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@CMD_Read);
    Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@CMD_Write);
    Storage.CoreObjects.Add(Editor.cmd,FCommands,Header,@CMD_Editor);
    With Folders do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@CMD_Folders_List);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@CMD_Folders_Add);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@CMD_Folders_Delete);
      Storage.CoreObjects.Add(Rename.cmd,FCommands,Header,@CMD_Folders_Rename);
    end;
    With Files do begin
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@CMD_Files_List);
      Storage.CoreObjects.Add(Add.cmd,FCommands,Header,@CMD_Files_Add);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@CMD_Files_Delete);
      Storage.CoreObjects.Add(Rename.cmd,FCommands,Header,@CMD_Files_Rename);
      Storage.CoreObjects.Add(GetData.cmd,FCommands,Header,@CMD_Files_GetData);
      Storage.CoreObjects.Add(SetData.cmd,FCommands,Header,@CMD_Files_SetData);
      Storage.CoreObjects.Add(Rotate.cmd,FCommands,Header,@CMD_Files_Rotate);
      Storage.CoreObjects.Add(SetAttributes.cmd,FCommands,Header,@CMD_Files_SetAttributes);
    end;
    With Templates do begin
      Storage.CoreObjects.Add(New.cmd,FCommands,Header,@CMD_Template_New);
      Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@CMD_Template_Delete);
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@CMD_Template_Read);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@CMD_Template_Write);
      Storage.CoreObjects.Add(List.cmd,FCommands,Header,@CMD_Template_List);
    end;
    With Domain do begin
      Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@CMD_Domain_Read);
      Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@CMD_Domain_Write);
    end;
  end;
end;

procedure TCMSCore.Finalize;
begin
  FTempFile.Free();
  FList.Free();
  Storage.Domains.Items.Done(FDomain);
  Storage.CMS.Items.Done(FPagePoint);
end;

function  TCMSCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  FS:=nil;
  FFolder:=nil;
  FList.Clear();
  Storage.CMS.Items.Empty(FPagePoint);
  Result:=CO_STATUS_OK;
  SetLength(FURI,0);
  SetLength(FBB,0);
end;

function  TCMSCore.CMD_ID(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  if Storage.CMS.Items.fromXML(FXMLDocument,FPagePoint) and (Length(FPagePoint.URI)>0) then begin
    if (Length(FPagePoint.Space)>0) then
      FURI:=Concat('/',FPagePoint.Space,FPagePoint.URI)
    else
      FURI:=FPagePoint.URI;
    Storage.FAT.Files.DB.Verify(FTask,OwnerP^.DomainP^.ID,FURI,FPagePoint.FolderID,FPagePoint.FileID);
    Storage.CMS.Items.toXML(FPagePoint,Transport(SR).Output,XML_HEADER_ON);
    Result:=CO_STATUS_OK;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
end;

function  TCMSCore.CMD_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  if Storage.CMS.Items.fromXML(FXMLDocument,FPagePoint) and (FPagePoint.FolderID>0) and (FPagePoint.FileID>0) then begin
    if Storage.AuraDisks.Files.Acquire(OwnerP^.Fat.AuraDiskNode,OwnerP^.DomainP^.ID,Use.Global,FPagePoint.FolderID,FPagePoint.FileID,Kinds.Domain,FS) then begin
      FPagePoint.Data:=Encryption.Base64.Encode(FS,FRefactor);
      Storage.CMS.Items.toXML(FPagePoint,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
end;

function  TCMSCore.CMD_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  if (SR.Credentials<>nil) then begin
    if Storage.CMS.Items.fromXML(FXMLDocument,FPagePoint) and (FPagePoint.FolderID>0) and (FPagePoint.FileID>0) then begin
      if Storage.AuraDisks.Files.Acquire(OwnerP^.Fat.AuraDiskNode,OwnerP^.DomainP^.ID,Use.Global,FPagePoint.FolderID,FPagePoint.FileID,Kinds.Domain,FS) then begin
        Encryption.Base64.Decode(FPagePoint.Data,FBB);
        Try
          SetLength(FPagePoint.Data,0);
          Storage.FAT.Files.DB.Write(FTask,OwnerP^.Fat.AuraDiskNode, OwnerP^.DomainP^.ID,FPagePoint.FolderID,FPagePoint.FileID,FBB);
          Storage.CMS.Items.toXML(FPagePoint,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        finally
          SetLength(FBB,0);
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DISK_DATA_MISSING;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Editor(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  if (SR.Credentials<>nil) then begin
    Result:=CO_STATUS_OK;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Domain_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  if (SR.Credentials<>nil) then begin
    Storage.Domains.Items.toXML(OwnerP^.DomainP^,Transport(SR).Output,XML_HEADER_ON);
    Result:=CO_STATUS_OK;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Domain_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  if (SR.Credentials<>nil) then begin
    if Storage.Domains.Items.fromXML(FXMLDocument,FDomain) and (FDomain.ID=OwnerP^.DomainP^.ID) and (Length(FDomain.Name)>0) then begin
      Storage.Domains.Items.DB.Update(FTask,FDomain);
      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Template_New(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Template_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Template_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Template_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Template_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Folders_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FPath:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldSearch);
    FDepth:=StrToIntDef(Core.Arrays.KeyString.GetItemByKey(srcHeaders,fieldDepth),-1);
    if (Length(FPath)>0) and (FDepth>0) then begin
      OwnerP^.Fat.Search(FList,FPath,FDepth,FRefactor);
      Storage.FAT.Folders.toXML(FList,Transport(SR).Output,XML_HEADER_ON);
      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Folders_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Folders_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Folders_Rename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TCMSCore.CMD_Files_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    FID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,fieldSearch);
    if (FID>0) then begin
      If OwnerP^.Fat.Folders.Find(FFolder,FID) then begin
        FLList:=FFolder.Files.LockList();
        Try
          Storage.FAT.Files.toXML(FLList,Transport(SR).Output,XML_HEADER_ON);
        finally
          FFolder.Files.UnlockList();
        end;
        Result:=CO_STATUS_OK;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
    end;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_Rotate(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  procedure ConvertFile;
  begin
    Core.Streams.Copy(FS,FRefactor);
    If Multimedia.Image.Tool.Rotate(FRefactor,FAngle) then begin
      Core.Streams.Copy(FRefactor,FS);
      Core.Streams.CheckSum(FRefactor,FFile.Digest);
      FFile.Size:=FRefactor.Size;
      Storage.FAT.Files.DB.Write(FTask,FFile,dsFileSaveDigest);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
    FRefactor.Size:=0;
  end;

begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?---0---&---1----&--2---&-3--|
  // Parameters from /core/cms?fls/rot&FolderID&FileId&angle // what a
  if SR.Credentials<>nil then begin
    if Length(Parameters)>3 then begin
        FFolderID:=StrToQWordDef(Parameters[1],0);
        FFileID:=StrToQWordDef(Parameters[2],0);
        FAngle:=StrToFloatDef(Parameters[3],90);
        if (FFileID<>0) and (FFolderID<>0) then begin
          If OwnerP^.Fat.Folders.Find(FFolder,FFolderID) then begin
            if FFolder.Find(FFileID,FFile) then begin
              FExt:=Core.Utils.Files.Extract(FFile.Name,efeoNone);
              FContentType:=ContentTypeFromFile(Storage.ContentTypes.List,FExt);
              if (RSR.HTTP.IndexOf(FContentType,ctImage)<>-1) then begin
                FS:=FFile.AcquireData();
                Try
                  ConvertFile();
                finally
                  FreeAndNil(FS);
                end;
              end else begin
                Result:=CO_STATUS_ERR_CO_CMD_INVALID_MEDIA;
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if FTempFile.fromXML(FXMLDocument) and (FTempFile.ID=0) and (FTempFile.FolderID>0)then begin
      if OwnerP^.Fat.Folders.Find(FFolder,FTempFile.FolderID) then begin
        FFile:=FFolder.Files.New(FTask,FTempFile.Attributes,FTempFile.HasKeywords,FTempFile.Deflate,FTempFile.Cache,FTempFile.Name,'');
        if Assigned(FFile) then begin
          Transport(SR).ContentType:=ctXML;
          Storage.Fat.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
          Result:=CO_STATUS_OK;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DUPLICATE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if FTempFile.fromXML(FXMLDocument) and (FTempFile.ID>0) and (FTempFile.FolderID>0)then begin
      If OwnerP^.Fat.Folders.Find(FFolder,FTempFile.FolderID) then begin
        if FFolder.Find(FTempFile.ID,FFile) then begin
          // Looking to delete this file
          if ((FFile.Attributes or FS_ATTR_COREOBJECT)<>FFile.Attributes) then begin
            Transport(SR).ContentType:=ctXML;
            Storage.Fat.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
            if Storage.FAT.Files.DB.Delete(FTask,FFolder.Owner.FAT.AuraDiskNode,OwnerP^.DomainP^.ID,FFolder.ID,FFile.ID) then begin
              OwnerP^.FAT.Refresh(DOMAIN_FAT_REFRESH_DELETE);
              Result:=CO_STATUS_OK;
            end else begin
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
            end;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_Rename(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if FTempFile.fromXML(FXMLDocument) and (FTempFile.ID>0) and (FTempFile.FolderID>0) and (Length(FTempFile.Name)>0)then begin
      If OwnerP^.FAT.Folders.Find(FFolder,FTempFile.FolderID) then begin
        if FFolder.Find(FTempFile.ID,FFile) then begin
          FFile.Name:=FTempFile.Name;
          Transport(SR).ContentType:=ctXML;
          if Storage.FAT.Files.DB.Write(FTask,FFile,dsfileSaveInfo) then begin
            Storage.FAT.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else begin
            Storage.FAT.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_GetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?-0-&---1----&--2---|
  // Parameters from /core/cms?ged&FolderID&FileId
  if SR.Credentials<>nil then begin
    if Length(Parameters)>2 then begin
        FFolderID:=StrToQWordDef(Parameters[1],0);
        FFileID:=StrToQWordDef(Parameters[2],0);
        if (FFileID<>0) and (FFolderID<>0) then begin
          If OwnerP^.Fat.Folders.Find(FFolder,FFolderID) then begin
            if FFolder.Find(FFileID,FFile) then begin
              FS:=FFile.AcquireData();
              Try
                Core.Arrays.KeyString.Update(respHeaders,fieldContentType,ctStream);
                Core.Streams.Copy(FS,Transport(SR).Output);
                Result:=CO_STATUS_OK;
              finally
                FreeAndNil(FS);
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_SetData(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin                     //  URL Parameters for Core gateway HTTP access
  Result:=CO_STATUS_FAIL; //  ?-0-&---1----&--2---|
  // Parameters from /core/cms?ged&FolderID&FileId
  if SR.Credentials<>nil then begin
    if Length(Parameters)>2 then begin
        FFolderID:=StrToQWordDef(Parameters[1],0);
        FFileID:=StrToQWordDef(Parameters[2],0);
        if (FFileID<>0) and (FFolderID<>0) then begin
          If OwnerP^.Fat.Folders.Find(FFolder,FFolderID) then begin
            if FFolder.Find(FFileID,FFile) then begin
              FS:=FFile.AcquireData();
              Try
                OwnerP^.Manager.RenewCycle();
                OwnerP^.Manager.EntryPoint:=Concat('coCMS.cmdFilesSetData.Copy[',IntToStr(FFile.ID),']');
                Core.Streams.Copy(Transport(SR).Input,FS);
                Core.Streams.CheckSum(Transport(SR).Input,FFile.Digest);
                FFile.Size:=FS.Size;
                Transport(SR).ContentType:=ctXML;
                Storage.FAT.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
                if Storage.FAT.Files.DB.Write(FTask,FFile,dsFileSaveDigest) then begin
                  Result:=CO_STATUS_OK;
                end else begin
                  Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
                end;
              finally
                FreeAndNil(FS);
              end;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_INVALID_PARAMETER;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_MISSING_PARAMETER;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function  TCMSCore.CMD_Files_SetAttributes(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if (SR.Credentials<>nil) then begin
    if FTempFile.fromXML(FXMLDocument) and (FTempFile.ID>0) and (FTempFile.FolderID>0)then begin
      If OwnerP^.Fat.Folders.Find(FFolder,FTempFile.FolderID) then begin
        if FFolder.Find(FTempFile.ID,FFile) then begin
          FFile.Assign(FTempFile); // will only set typical attributes
          Transport(SR).ContentType:=ctXML;
          if Storage.FAT.Files.DB.Write(FTask,FFile,dsFileSaveAttribs) then begin
            Storage.FAT.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_OK;
          end else begin
            Storage.FAT.Files.toXML(FFile,Transport(SR).Output,XML_HEADER_ON);
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_RESOURCE_NOT_FOUND;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_FOLDER_NOT_FOUND;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


end.


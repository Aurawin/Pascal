unit coApp;

{
 unit coApp.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html


 Application Framework API


 coApp offers back-end access to
   Application :
     Registry :
       Universal ID
       Copyright
       Patent(s)
       Title
       Description
     Storage for :
       Language Tables
       User based Settings
       Application based Settings

}

interface
uses
  Classes,

  hHTTPd,

  App.Consts,

  RSR,
  RSR.HTTP,
  RSR.Core,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,

  Core.Keywords,
  Core.Timer,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,

  Core.Utils.Time,


  Storage,
  Storage.Main,
  Storage.Apps,
  Storage.Domains,
  Storage.CoreObjects,
  Storage.UserAccounts,

  MD5,
  SysUtils;

Type
  ns=class
  Type
    App=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Application';
        NameSpace                : '/core/app';
        Caption                  : 'Application Core Object';
        Prompt                   : 'User can create and install applications';
        Description              : 'Back-end system application handler'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TAppCore';
        Location                 : 'coApp.pas';
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
      Register=class
      type
        Add=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Register';
            NameSpace            : '/register/add';
            Caption              : 'Application Registration';
            Prompt               : 'User can register applications for the system';
            Description          : 'System-wide application registration';
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
        Defines=class
        type
          Write=class
          const
            ACLInf:TACLInfo=(
              Name               : 'Write';
              NameSpace          : '/register/defs/w';
              Caption            : 'Application Definitions';
              Prompt             : 'User can modify definitions in application registry';
              Description        : 'Provides write access to application registry';
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
              Method               : nil;
              Resource             : nil;
            );
          end;
        end;
        Declares=class
        type
          Write=class
          const
            ACLInf:TACLInfo=(
              Name               : 'Write';
              NameSpace          : '/register/decs/w';
              Caption            : 'Application Declarations';
              Prompt             : 'User can modify declarations in application registry';
              Description        : 'Provides write access to application registry';
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
              Method             : nil;
              Resource           : nil;
            );
          end;
        end;
        Depends=class
        type
          Write=class
          const
            ACLInf:TACLInfo=(
              Name               : 'Write';
              NameSpace          : '/register/deps/w';
              Caption            : 'Application Dependencies';
              Prompt             : 'User can modify dependencies in application registry';
              Description        : 'Provides write access to application registry';
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
              Method             : nil;
              Resource           : nil;
            );
          end;
        end;
        LangTable=class
        type
          Write=class
          const
            ACLInf:TACLInfo=(
              Name               : 'Write';
              NameSpace          : '/register/lt/w';
              Caption            : 'Application Language Table';
              Prompt             : 'User can modify language tables in application registry';
              Description        : 'Provides write access to application registry';
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
              Method             : nil;
              Resource           : nil;
            );
          end;
        end;
      end;
      Read=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Read';
          NameSpace              : '/r';
          Caption                : 'Application Storage';
          Prompt                 : 'User can read application based data';
          Description            : 'Read access to system-wide application data storage field';
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
          NameSpace              : '/w';
          Caption                : 'Application Storage';
          Prompt                 : 'User can write application based data';
          Description            : 'Write access to system-wide application data storage field';
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
          Name                   : 'Remove';
          NameSpace              : '/remove';
          Caption                : 'Application Storage';
          Prompt                 : 'User can remove applications from the system';
          Description            : 'System-wide application removal';
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
      Boot=class
      type
        Kit=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Boot Kit';
            NameSpace            : '/boot/kit';
            Caption              : 'Application Boot Kit';
            Prompt               : 'User can boot applications';
            Description          : 'Boot installed system applications';
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
        LangTable=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Language Table';
            NameSpace            : '/boot/lt';
            Caption              : 'Application Language Table';
            Prompt               : 'User retrieve Language Tables';
            Description          : 'Read access to multiple languages for applications';
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
        Declares=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Declarations';
            NameSpace            : '/boot/decs';
            Caption              : 'Application Declarations';
            Prompt               : 'User can retrieve application declarations';
            Description          : 'Read access to installed application declarations';
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
        Defines=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Definitions';
            NameSpace            : '/boot/defs';
            Caption              : 'Application Definitions';
            Prompt               : 'User can retrieve application definitions';
            Description          : 'Read access to installed application definitions';
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
        Depends=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Dependencies';
            NameSpace            : '/boot/deps';
            Caption              : 'Application Dependencies';
            Prompt               : 'User can retrieve application dependencies';
            Description          : 'Read access to installed application dependencies';
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
        Manifest=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Manifest';
            NameSpace            : '/boot/mfst';
            Caption              : 'Application Manifest';
            Prompt               : 'User can retrieve Applications manifest';
            Description          : 'Read access to installed applications manifest';
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
      Data=class
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/data/r';
            Caption              : 'Read Data';
            Prompt               : 'User can read declared items in storage';
            Description          : 'Read access to application items list';
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
            NameSpace            : '/data/w';
            Caption              : 'Write Data';
            Prompt               : 'User can write declared items in storage';
            Description          : 'Write access to application items list';
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
            NameSpace            : '/data/d';
            Caption              : 'Delete Data';
            Prompt               : 'User can delete declared items in storage';
            Description          : 'Delete access to application items list';
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
        Refresh=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Refresh';
            NameSpace            : '/data/f';
            Caption              : 'Refresh Data';
            Prompt               : 'User can refresh declared items in storage';
            Description          : 'Read access to refresh application items list';
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
  end;
  Type
    TAppCore=Class(TCoreObject)
    private
      FUserAccounts              : Storage.UserAccounts.Items.TList;
      FPacket                    : Core.Arrays.Types.KeyStrings;
      DataP                      : PHTTP;
    private

    private
       // RSR Core Object Commands for Application DevelopmentAdministration
      function  cmdRegisterAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdRegisterDefinesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdRegisterDeclaresWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdRegisterDependsWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdRegisterLanguageTableWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdRemove(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdBootKit(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdBootLanguageTable(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdBootDeclarations(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdBootDefinitions(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdBootDependencies(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdBootManifest(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdDataRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdDataWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdDataDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
      function  cmdDataRefresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    private
    protected
       // RSR Core Object Initialization Call happens on entry
       class procedure Install(Task:Core.Database.Types.TTask); override;
       class procedure UnInstall; override;
       procedure Initialize; override;
       procedure Finalize; override;
       function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; Var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
    End;

    procedure Install(Task:Core.Database.Types.TTask);

implementation
uses DateUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TAppCore.Install(Task);
end;

class procedure TAppCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TAppCore);
  with ns.App do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Register.Add.cmd);
    COREOBJECT_VerifyID(Task,Register.Defines.Write.cmd);
    COREOBJECT_VerifyID(Task,Register.Declares.Write.cmd);
    COREOBJECT_VerifyID(Task,Register.Depends.Write.cmd);
    COREOBJECT_VerifyID(Task,Register.LangTable.Write.cmd);
    COREOBJECT_VerifyID(Task,Read.cmd);
    COREOBJECT_VerifyID(Task,Write.cmd);
    COREOBJECT_VerifyID(Task,Delete.cmd);
    COREOBJECT_VerifyID(Task,Boot.Kit.cmd);
    COREOBJECT_VerifyID(Task,Boot.LangTable.cmd);
    COREOBJECT_VerifyID(Task,Boot.Declares.cmd);
    COREOBJECT_VerifyID(Task,Boot.Defines.cmd);
    COREOBJECT_VerifyID(Task,Boot.Depends.cmd);
    COREOBJECT_VerifyID(Task,Boot.Manifest.cmd);
    COREOBJECT_VerifyID(Task,Data.Read.cmd);
    COREOBJECT_VerifyID(Task,Data.Write.cmd);
    COREOBJECT_VerifyID(Task,Data.Delete.cmd);
    COREOBJECT_VerifyID(Task,Data.Refresh.cmd);
  end;
end;

class procedure TAppCore.UnInstall;
begin
  UnRegisterClass(TAppCore);
end;

procedure TAppCore.Initialize;
begin
  With ns.App do begin
    Storage.CoreObjects.Add(Register.Add.cmd,FCommands,Header,@cmdRegisterAdd);
    Storage.CoreObjects.Add(Register.Defines.Write.cmd,FCommands,Header,@cmdRegisterDefinesWrite);
    Storage.CoreObjects.Add(Register.Declares.Write.cmd,FCommands,Header,@cmdRegisterDeclaresWrite);
    Storage.CoreObjects.Add(Register.Depends.Write.cmd,FCommands,Header,@cmdRegisterDependsWrite);
    Storage.CoreObjects.Add(Register.LangTable.Write.cmd,FCommands,Header,@cmdRegisterLanguageTableWrite);
    Storage.CoreObjects.Add(Read.cmd,FCommands,Header,@cmdRead);
    Storage.CoreObjects.Add(Write.cmd,FCommands,Header,@cmdWrite);
    Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@cmdRemove);
    Storage.CoreObjects.Add(Boot.Kit.cmd,FCommands,Header,@cmdBootKit);
    Storage.CoreObjects.Add(Boot.LangTable.cmd,FCommands,Header,@cmdBootLanguageTable);
    Storage.CoreObjects.Add(Boot.Declares.cmd,FCommands,Header,@cmdBootDeclarations);
    Storage.CoreObjects.Add(Boot.Defines.cmd,FCommands,Header,@cmdBootDefinitions);
    Storage.CoreObjects.Add(Boot.Depends.cmd,FCommands,Header,@cmdBootDependencies);
    Storage.CoreObjects.Add(Boot.Manifest.cmd,FCommands,Header,@cmdBootManifest);
    Storage.CoreObjects.Add(Data.Read.cmd,FCommands,Header,@cmdDataRead);
    Storage.CoreObjects.Add(Data.Write.cmd,FCommands,Header,@cmdDataWrite);
    Storage.CoreObjects.Add(Data.Delete.cmd,FCommands,Header,@cmdDataDelete);
    Storage.CoreObjects.Add(Data.Refresh.cmd,FCommands,Header,@cmdDataRefresh);
  end;
end;

procedure TAppCore.Finalize;
begin
  Core.Arrays.KeyString.Done(FPacket);
end;

function  TAppCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; Var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  FUserAccounts:=OwnerP^.Accounts;
  DataP:=SR.Info.DataP;
end;

function  TAppCore.cmdRegisterAdd(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iHdrCount                      : LongInt;
  Digest                         : TMD5Digest;
  Context                        : TMD5Context;
  iVendorID                      : QWord;
  App                            : Storage.Apps.Items.App;
begin
  Result:=CO_STATUS_FAIL;
  iHdrCount:=System.Length(srcHeaders);
  iVendorID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,Storage.Apps.Items.DB.Keys.VendorID,iHdrCount);
  if (SR.Credentials<>nil) and Storage.CoreObjects.Granted(CommandP^,UAP(SR)) then begin
    if (iVendorID<>0) then begin
      Storage.Apps.Items.Init(App);
      Try
        App.Created:=Core.Timer.dtNow;
        App.Modified:=App.Created;
        App.Data:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Data,iHdrCount);
        App.Defines:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Defines,iHdrCount);
        App.Declares:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Declares,iHdrCount);
        App.Dependencies:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Dependencies,iHdrCount);
        App.Manifest:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Manifest,iHdrCount);
        App.LanguageTable:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.LanguageTable,iHdrCount);
        App.TitleLong:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.TitleLong,iHdrCount);
        App.TitleShort:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.TitleShort,iHdrCount);
        App.Description:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Description,iHdrCount);
        App.Copyright:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Copyright,iHdrCount);
        App.Patents:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,Storage.Apps.Items.DB.Keys.Patents,iHdrCount);

        App.Visibility:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Apps.Items.DB.Keys.Visibility,iHdrCount,Storage.Apps.Items.Defaults.Visibility);
        App.VersionMajor:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Apps.Items.DB.Keys.VersionMajor,iHdrCount);
        App.VersionMinor:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Apps.Items.DB.Keys.VersionMinor,iHdrCount);
        App.VersionMicro:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Apps.Items.DB.Keys.VersionMicro,iHdrCount);
        App.VersionBuild:=Core.Arrays.KeyString.GetItemAsInteger(srcHeaders,Storage.Apps.Items.DB.Keys.VersionBuild,iHdrCount);

        md5.MD5Init(Context);
        md5.MD5Update(Context,OwnerP^.DomainP^.ID,SizeOf(OwnerP^.DomainP^.ID));
        md5.MD5Update(Context,UAP(SR)^.ID,SizeOf(UAP(SR)^.ID));
        md5.MD5Update(Context,iVendorID,SizeOf(iVendorID));
        md5.MD5Update(Context,App.Created,SizeOf(App.Created));

        md5.MD5Final(Context,Digest);
        App.Auth:=md5.MD5Print(Digest);

        Empty(respHeaders);

        Core.Arrays.KeyString.Add(@respHeaders,Storage.Apps.Items.DB.Keys.Auth,App.Auth);
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Apps.Items.DB.Keys.Created,FloatToStr(App.Created));
        Core.Arrays.KeyString.Add(@respHeaders,Storage.Apps.Items.DB.Keys.Modified,FloatToStr(App.Modified));

        if Storage.Apps.Items.DB.Install(FTask,OwnerP^.DomainP^.ID,UAP(SR)^.ID,App) then begin
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
        end;
      Finally
        Storage.Apps.Items.Done(App);
      end;
    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
      TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_ACCESS_DENIED);
  end;

end;

function  TAppCore.cmdRegisterDefinesWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdRegisterDeclaresWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdRegisterDependsWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdRegisterLanguageTableWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;
function  TAppCore.cmdWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdRemove(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdBootKit(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdBootLanguageTable(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdBootDeclarations(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdBootDefinitions(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdBootDependencies(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdBootManifest(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdDataRead(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdDataWrite(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdDataDelete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function  TAppCore.cmdDataRefresh(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

end.


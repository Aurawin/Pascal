{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}
unit coQuill;

interface

uses
  Classes,

  HTTPDefs,
  hHttpd,

  App.Consts,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Keywords,

  Core.Database,
  Core.Database.Types,

  Core.Timer,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,

  Storage,
  Storage.Main,
  Storage.CoreObjects,

  SysUtils;

type
  ns=class
  type
    Quill=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Quill';
        NameSpace                : '/core/quill';
        Caption                  : 'Quill Core Object';
        Prompt                   : 'User can access blogging system';
        Description              : 'Back-end system to facilitate blogging'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TQuillCore';
        Location                 : 'coQuill.pas';
      );
      Header:TCoreObjectInfo=(
        ID                       : 0;
        ProviderID               : 0;
        Enabled                  : true;
        Anonymous                : true;
        Scale                    : 0;
        CLSInfo                  : @CLSInf;
        ACLInfo                  : @ACLInf;
      );
    Type
      Blog=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : false;
        );
      type
        New=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'New';
            NameSpace            : '/bgn';
            Caption              : 'Create Blogs';
            Prompt               : 'User can create blogs';
            Description          : 'Quill access to create blogs';
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
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/bgr';
            Caption              : 'Blog Reader';
            Prompt               : 'User can access to read blogs';
            Description          : 'Quill access to read blogs from database';
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
          ACLInf:TACLInfo=(
            Name                 : 'Write';
            NameSpace            : '/bgw';
            Caption              : 'Blog Writer';
            Prompt               : 'User can access to write blogs';
            Description          : 'Quill access to write blogs from database';
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
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/bgd';
            Caption              : 'Delete Blogs';
            Prompt               : 'User can delete blogs';
            Description          : 'Quill access to delete blogs from database';
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
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/bgl';
            Caption              : 'List Blogs';
            Prompt               : 'User can list blogs';
            Description          : 'Quill access to list blogs from database';
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
      type
        Item=class
        type
          Read=class
          const
            ACLInf:TACLInfo=(
              Name               : 'Read';
              NameSpace          : '/itm/r';
              Caption            : 'Read Item';
              Prompt             : 'User can read blog items';
              Description        : 'Provides access to read blog items';
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
            ACLInf:TACLInfo=(
              Name               : 'Write';
              NameSpace          : '/itm/w';
              Caption            : 'Write Item';
              Prompt             : 'User can write blog items';
              Description        : 'Provides access to write blog items';
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
          Delete=class
          const
            ACLInf:TACLInfo=(
              Name               : 'Delete';
              NameSpace          : '/itm/d';
              Caption            : 'Delete Item';
              Prompt             : 'User can delete blog items';
              Description        : 'Provides access to delete blog items';
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
          Action=class
          type
            Add=class
            const
              ACLInf:TACLInfo=(
                Name             : 'Add';
                NameSpace        : '/itm/ia/a';
                Caption          : 'Add Interaction';
                Prompt           : 'User can interact with blog items';
                Description      : 'Provides access to interact with blog items';
              );
              cmd:TCoreCommand=(
                HeaderP          : @Header;
                ID               : 0;
                Enabled          : true;
                Anonymous        : false;
                Cache            : false;
                Compress         : true;
                Secure           : false;
                XMLInfo          : @XMLInf;
                ACLInfo          : @ACLInf;
                Method                 : nil;
                Resource               : nil;
              );
            end;
            List=class
            const
              ACLInf:TACLInfo=(
                Name             : 'List';
                NameSpace        : '/itm/ia/l';
                Caption          : 'List Interactions';
                Prompt           : 'User can list interactions with blog items';
                Description      : 'Provides access to list of interactions with blog items';
              );
              cmd:TCoreCommand=(
                HeaderP          : @Header;
                ID               : 0;
                Enabled          : true;
                Anonymous        : false;
                Cache            : false;
                Compress         : true;
                Secure           : false;
                XMLInfo          : @XMLInf;
                ACLInfo          : @ACLInf;
                Method                 : nil;
                Resource               : nil;
              );
            end;
            Clear=class
            const
              ACLInf:TACLInfo=(
                Name             : 'Clear';
                NameSpace        : '/itm/ia/c';
                Caption          : 'Clear Interactions';
                Prompt           : 'User can clear list of interactions with blog items';
                Description      : 'Provides access to clear list of interactions with blog items';
              );
              cmd:TCoreCommand=(
                HeaderP          : @Header;
                ID               : 0;
                Enabled          : true;
                Anonymous        : false;
                Cache            : false;
                Compress         : true;
                Secure           : false;
                XMLInfo          : @XMLInf;
                ACLInfo          : @ACLInf;
                Method                 : nil;
                Resource               : nil;
              );
            end;
          end;
        end;
      end;
    end;
  end;
  TQuillCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    RSRP                         : PRSR;
  private
    function  Perform_Blog_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Blog_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Blog_Create(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Blog_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Blog_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    function  Perform_Item_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Item_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Item_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    function  Perform_Action_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Action_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Perform_Action_Clear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  private
    function  Request:THTTPRequest;
    function  Response:THTTPResponse;
  protected
    // RSR Core Object Initialization Call happens on entry
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function
BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  end;

  procedure Install(Task:Core.Database.Types.TTask);
implementation

procedure Install(Task:Core.Database.Types.TTask);
begin
  TQuillCore.Install(Task);
end;

class procedure TQuillCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TQuillCore);
  with ns.Quill do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Blog.New.cmd);
    COREOBJECT_VerifyID(Task,Blog.Read.cmd);
    COREOBJECT_VerifyID(Task,Blog.Write.cmd);
    COREOBJECT_VerifyID(Task,Blog.Delete.cmd);
    COREOBJECT_VerifyID(Task,Blog.List.cmd);
    COREOBJECT_VerifyID(Task,Blog.Item.Read.cmd);
    COREOBJECT_VerifyID(Task,Blog.Item.Write.cmd);
    COREOBJECT_VerifyID(Task,Blog.Item.Delete.cmd);
    COREOBJECT_VerifyID(Task,Blog.Item.Action.Add.cmd);
    COREOBJECT_VerifyID(Task,Blog.Item.Action.List.cmd);
    COREOBJECT_VerifyID(Task,Blog.Item.Action.Clear.cmd);
  end;
end;

class procedure TQuillCore.UnInstall;
begin
  UnRegisterClass(TQuillCore);
end;

procedure TQuillCore.Initialize;
begin
  with ns.Quill do begin
    Storage.CoreObjects.Add(Blog.New.cmd,FCommands,Header,@Perform_Blog_Create);
    Storage.CoreObjects.Add(Blog.Read.cmd,FCommands,Header,@Perform_Blog_Read);
    Storage.CoreObjects.Add(Blog.Write.cmd,FCommands,Header,@Perform_Blog_Write);
    Storage.CoreObjects.Add(Blog.Delete.cmd,FCommands,Header,@Perform_Blog_Delete);
    Storage.CoreObjects.Add(Blog.List.cmd,FCommands,Header,@Perform_Blog_List);
    Storage.CoreObjects.Add(Blog.Item.Read.cmd,FCommands,Header,@Perform_Item_Read);
    Storage.CoreObjects.Add(Blog.Item.Write.cmd,FCommands,Header,@Perform_Item_Write);
    Storage.CoreObjects.Add(Blog.Item.Delete.cmd,FCommands,Header,@Perform_Item_Delete);
    Storage.CoreObjects.Add(Blog.Item.Action.Add.cmd,FCommands,Header,@Perform_Action_Add);
    Storage.CoreObjects.Add(Blog.Item.Action.List.cmd,FCommands,Header,@Perform_Action_List);
    Storage.CoreObjects.Add(Blog.Item.Action.Clear.cmd,FCommands,Header,@Perform_Action_Clear);
  end;
end;

procedure TQuillCore.Finalize;
begin

end;

function  TQuillCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
end;


function  TQuillCore.Request:THTTPRequest;
begin
  Result:=nil;
end;

function  TQuillCore.Response:THTTPResponse;
begin
  Result:=nil;
end;


function  TQuillCore.Perform_Blog_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Blog_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Blog_Create(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Blog_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Blog_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Item_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Item_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Item_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  //
end;

function  TQuillCore.Perform_Action_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Action_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TQuillCore.Perform_Action_Clear(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

end.


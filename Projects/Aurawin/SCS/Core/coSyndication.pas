{
 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}


unit coSyndication;

interface
  uses
    Classes,

    App.Consts,

    RSR,
    RSR.HTTP,
    RSR.Core,

    hHttpd,

    Core.Database,
    Core.Database.Types,
    Core.Database.SQL,


    Core.Strings,

    Core.Timer,

    Core.Arrays,
    Core.Arrays.Types,
    Core.Arrays.KeyString,
    Core.Arrays.LargeWord,
    Core.Arrays.VarString,
    Core.Arrays.Bytes,

    Core.Keywords,

    Storage,
    Storage.Main,
    Storage.CoreObjects,
    Storage.UserAccounts,
    Storage.Syndication,

    sha1;

type
  ns=class
  type
    Synd=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Syndication';
        NameSpace                : '/core/synd';
        Caption                  : 'Syndication Core Object';
        Prompt                   : 'User can access to syndication system';
        Description              : 'Back-end syndication system'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TSyndicationCore';
        Location                 : 'coSyndication.pas';
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
    Type
      Feeds=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : false;
        );
      type
        List=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'List';
            NameSpace            : '/fds/l';
            Caption              : 'Feeds List';
            Prompt               : 'User can access list of feeds';
            Description          : 'Provides access to the list of feeds';
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
        Feed=class
        type
          New=Class
          const
            ACLInf:TACLInfo=(
              Name               : 'New';
              NameSpace          : '/fds/fd/n';
              Caption            : 'New Feed';
              Prompt             : 'User can create a new feed';
              Description        : 'Provides access to create a new feed';
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
          Read=Class
          const
            ACLInf:TACLInfo=(
              Name               : 'Read';
              NameSpace          : '/fds/fd/r';
              Caption            : 'Read Feed';
              Prompt             : 'User can read feed';
              Description        : 'Provides read access to a feed';
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
          Write=Class
          const
            ACLInf:TACLInfo=(
              Name               : 'Write';
              NameSpace          : '/fds/fd/w';
              Caption            : 'Write Feed';
              Prompt             : 'User can save a feed';
              Description        : 'Provides write access to a feed';
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
          Delete=Class
          const
            ACLInf:TACLInfo=(
              Name               : 'Delete';
              NameSpace          : '/fds/fd/d';
              Caption            : 'Delete Feed';
              Prompt             : 'User can delete a feed';
              Description        : 'Provides delete access to a feed';
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
          Items=class
          const
            XMLInf:TXMLInfo=(
              Enabled            : false;
            );
          type
            List=class
            const
              ACLInf:TACLInfo=(
                Name             : 'List';
                NameSpace        : '/fds/fd/itms/l';
                Caption          : 'List Feed Items';
                Prompt           : 'User can list items in a feed';
                Description      : 'Provides list of items to a feed';
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
      Profile=class
      const
        XMLInf:TXMLInfo=(
          Enabled                : false;
        );
      type
        Read=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Read';
            NameSpace            : '/prfl/r';
            Caption              : 'Read profile of syndicator';
            Prompt               : 'User can view profile of syndicator';
            Description          : 'Provides read access to syndicaiton profiles';
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
            NameSpace            : '/prfl/w';
            Caption              : 'Write profile of syndicator';
            Prompt               : 'Users can edit their profile';
            Description          : 'Provides access to edit syndication profiles';
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
  end;
Type

  TSyndicationCore=Class(TCoreObject)
  private
    FRequest                     : THTTPRequest;
    saContent                    : Core.Arrays.Types.VarString;
    saQuery                      : Core.Arrays.Types.VarString;
    saFeed                       : Core.Arrays.Types.VarString;
    synFeed                      : Storage.Syndication.Feed.Item;
    synFeeds                     : Storage.Syndication.Feed.Items;
    synAdmin                     : Storage.Syndication.Admin.Item;
    sFeed                        : Core.Strings.VarString;
    DataP                        : PHTTP;
    msBuffer                     : TMemoryStream;
  private
    function  Push_CMD_Feed_Items(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Feed_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Feed_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Feed_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Feed_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Feed_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;

    function  Push_CMD_Profile_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Profile_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation
uses SysUtils,DateUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TSyndicationCore.Install(Task);
end;

class procedure TSyndicationCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TSyndicationCore);
  with ns.Synd do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Feeds.List.cmd);
    COREOBJECT_VerifyID(Task,Feeds.Feed.New.cmd);
    COREOBJECT_VerifyID(Task,Feeds.Feed.Read.cmd);
    COREOBJECT_VerifyID(Task,Feeds.Feed.Write.cmd);
    COREOBJECT_VerifyID(Task,Feeds.Feed.Delete.cmd);
    COREOBJECT_VerifyID(Task,Feeds.Feed.Items.List.cmd);
    COREOBJECT_VerifyID(Task,Profile.Read.cmd);
    COREOBJECT_VerifyID(Task,Profile.Write.cmd);
  end;
end;

class procedure TSyndicationCore.UnInstall;
begin
  UnRegisterClass(TSyndicationCore);
end;

procedure TSyndicationCore.Initialize;
begin
  msBuffer:=TMemoryStream.Create();
  Storage.Syndication.Admin.Init(synAdmin);
  Storage.Syndication.Feed.Init(synFeed);
  Storage.Syndication.Feed.Init(synFeeds);
  Core.Arrays.VarString.Init(saFeed);
  Core.Arrays.VarString.Init(saContent);
  Core.Arrays.VarString.Init(saQuery);
  Core.Arrays.VarString.Init(saFeed);

  with ns.Synd do begin
    Storage.CoreObjects.Add(Feeds.List.cmd,FCommands,Header,@Push_CMD_Feed_List);
    Storage.CoreObjects.Add(Feeds.Feed.New.cmd,FCommands,Header,@Push_CMD_Feed_Add);
    Storage.CoreObjects.Add(Feeds.Feed.Read.cmd,FCommands,Header,@Push_CMD_Feed_Read);
    Storage.CoreObjects.Add(Feeds.Feed.Delete.cmd,FCommands,Header,@Push_CMD_Feed_Delete);
    Storage.CoreObjects.Add(Feeds.Feed.Items.List.cmd,FCommands,Header,@Push_CMD_Feed_Items);
    Storage.CoreObjects.Add(Profile.Read.cmd,FCommands,Header,@Push_CMD_Profile_Read);
    Storage.CoreObjects.Add(Profile.Write.cmd,FCommands,Header,@Push_CMD_Profile_Write);
  end;
end;

procedure TSyndicationCore.Finalize;
begin
  FreeAndNil(msBuffer);

  Storage.Syndication.Admin.Done(synAdmin);
  Storage.Syndication.Feed.Done(synFeed);
  Storage.Syndication.Feed.Done(synFeeds);

  Core.Arrays.VarString.Done(saFeed);
  Core.Arrays.VarString.Done(saContent);
  Core.Arrays.VarString.Done(saQuery);
  Core.Arrays.VarString.Done(saFeed);
end;

function  TSyndicationCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
end;


function  TSyndicationCore.Push_CMD_Feed_Add(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Core.Arrays.VarString.fromStream(saFeed,FRequest.Data,#2);
  Try
    If (Length(saFeed)>3) then begin
      Storage.Syndication.Feed.Empty(synFeed);
      Try
        If (DataP^.UAP<>Nil) and (DataP^.UAP^.Auth=DataP^.Auth) then begin
          synFeed.RootID:=DataP^.UAP^.ID;
          synFeed.PubDate:=Core.Timer.dtNow;
          synFeed.LastBuildDate:=0;
          synFeed.Title:=saFeed[0];
          synFeed.Editor:=saFeed[1];
          synFeed.Webmaster:=saFeed[2];
          synFeed.DefaultExpires:=StrToFloatDef(saFeed[3],0);
          synFeed.Description:=saFeed[4];
          If Storage.Syndication.Feed.DB.Create(FTask,synFeed.RootID,synFeed) then begin
            Result:=CO_STATUS_OK;
          end else begin
            Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end;
        end else begin
          Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
        end;
      Finally
        Storage.Syndication.Feed.Empty(synFeed);
      end;
    end;
  Finally
    Empty(saFeed);
  end;
end;

function  TSyndicationCore.Push_CMD_Feed_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TSyndicationCore.Push_CMD_Feed_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TSyndicationCore.Push_CMD_Feed_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function  TSyndicationCore.Push_CMD_Feed_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
  iLength:integer;
begin
  If (DataP^.UAP<>Nil) and (DataP^.UaP^.Auth=DataP^.Auth) then begin
    If Storage.Syndication.Feed.DB.Retrieve(FTask,DataP^.UaP^.ID,synFeeds) then begin
      Empty(sFeed);
      For iLcv:=0 to High(synFeeds) do
        sFeed:=Concat(sFeed,Feed.ToString(synFeeds[iLcv],#2),#1);
      iLength:=Length(sFeed);
      If iLength>0 then
        SetLength(sFeed,iLength-1);

    end else begin
      Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end;
  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
  end;
end;

function TSyndicationCore.Push_CMD_Feed_Items(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_ERR_CO_CMD_NOT_IMPLEMENTED;
end;

function TSyndicationCore.Push_CMD_Profile_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  If DataP^.UAP<>Nil then begin
    If (DataP^.UAP^.Auth=DataP^.Auth) then begin
      Admin.DB.Retrieve(FTask,DataP^.UAP^.ID,synAdmin);
      Result:=CO_STATUS_OK;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;

function TSyndicationCore.Push_CMD_Profile_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  If DataP^.UAP<>Nil then begin
    If (DataP^.UAP^.Auth=DataP^.Auth) then begin
      // Parse In Profile and Update Accordingly.
      synAdmin.RootID:=DataP^.UAP^.ID;
      If (synAdmin.ID<>0) and Admin.DB.Edit(FTask,synAdmin) then begin
        Result:=CO_STATUS_OK;
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end
    end else
      Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_AUTH_REQD;
end;


end.


unit coPostInfo;

{
  Copyright Aurawin LLC 2003-2015
  Written by: Andrew Thomas Brunner

  unit coPostInfo.pas

  This core object provides website developers the ability to have
  instant access to post information to the email engine and database.


  This code is issued under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,

  hHttpd,

  App.Consts,

  RSR,
  RSR.HTTP,
  RSR.Core,

  Core.Database,
  Core.Database.Types,

  Core.Utils.Time,
  Core.Timer,
  Core.Keywords,
  Core.Strings,
  Encryption.SHA,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.PostInfo,
  Storage.UserStorage,
  Storage.Domains,

  Core.Utils.Mail,
  Core.Utils.Sockets,

  HTTPDefs,
  SysUtils;

type
  PostInfo=class
  const
    ACLInf:TACLInfo=(
      Name                     : 'PostInfo';
      NameSpace                : '/core/pstinf';
      Caption                  : 'Post Info Core Object';
      Prompt                   : 'User has access to post information to system';
      Description              : 'Provides instant access to post data to email engine and database'
    );
    CLSInf:TCLSInfo=(
      Name                     : 'TPostInfoCore';
      Location                 : 'coPostInfo.pas';
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
      Enabled                  : false;
    );
  Type
    Email=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Email';
        NameSpace              : '/eml';
        Caption                : 'Post to Email';
        Prompt                 : 'User can access email system to collect form data';
        Description            : 'Provides capabilities to convert form data to email messages';
      );
      cmd:TCoreCommand=(
        HeaderP                : @Header;
        ID                     : 0;
        Enabled                : true;
        Anonymous              : true;
        Cache                  : false;
        Compress               : true;
        Secure                 : false;
        XMLInfo                : @XMLInf;
        ACLInfo                : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    DB=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Database';
        NameSpace              : '/db';
        Caption                : 'Post to Database';
        Prompt                 : 'User can access database system to collect form data';
        Description            : 'Provides capabilities to post form data to database tables';
      );
      cmd:TCoreCommand=(
        HeaderP                : @Header;
        ID                     : 0;
        Enabled                : true;
        Anonymous              : true;
        Cache                  : false;
        Compress               : true;
        Secure                 : false;
        XMLInfo                : @XMLInf;
        ACLInfo                : @ACLInf;
        Method                   : nil;
        Resource                 : nil;
      );
    end;
  end;
  TPostInfoCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
    FFolderID                    : QWord;
    FFileID                      : QWord;
    FKP                          : Core.Strings.PKeyString;
    iQueryCount                  : LongInt;
    iRedirect                    : LongInt;
    iRecipient                   : LongInt;
    iFrom                        : LongInt;
    iHello                       : LongInt;
    iStringFields                : LongInt;
    iNumericFields               : LongInt;
    iTable                       : LongInt;
    kplFormData                  : Core.Arrays.Types.KeyStrings;

    FFormData                    : Core.Strings.VarString;
    FLines                       : Core.Arrays.Types.VarString;
    saRecipient                  : Core.Arrays.Types.VarString;
    FContent                     : Core.Arrays.Types.VarString;
    FNameSpace                   : Core.Strings.VarString;
    FEmail                       : Core.Strings.VarString;
    FSubject                     : Core.Strings.VarString;
    FBody                        : Core.Strings.VarString;
    FSummary                     : Storage.UserStorage.Items.SMTP.TSummary;
    FAccount                     : Storage.UserAccounts.Items.Item;
    FMessage                     : Storage.UserStorage.Items.SMTP.TRecvMessage;
  private
    function  Push_CMD_Email(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_DB(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
uses StrUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TPostInfoCore.Install(Task);
end;

class procedure TPostInfoCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TPostInfoCore);
  with PostInfo do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Email.cmd);
    COREOBJECT_VerifyID(Task,DB.cmd);
  end;
end;

class procedure TPostInfoCore.UnInstall;
begin
  UnRegisterClass(TPostInfoCore);
end;

procedure TPostInfoCore.Initialize;
begin
  DataP:=nil;
  iQueryCount:=-1;
  iRedirect:=-1;
  iRecipient:=-1;
  iFrom:=-1;
  iHello:=-1;
  iStringFields:=-1;
  iNumericFields:=-1;
  iTable:=-1;
  Core.Arrays.VarString.Init(FLines);
  Storage.UserStorage.Items.SMTP.Init(FSummary);
  Storage.UserAccounts.Items.Init(FAccount);
  Core.Arrays.KeyString.Init(kplFormData);

  Core.Arrays.VarString.Init(saRecipient);
  Core.Arrays.VarString.Init(FContent);
  Storage.UserStorage.Items.SMTP.Init(FMessage);
  with PostInfo do begin
    Storage.CoreObjects.Add(Email.cmd,FCommands,Header,@Push_CMD_Email);
    Storage.CoreObjects.Add(DB.cmd,FCommands,Header,@Push_CMD_DB);
  end;

end;

procedure TPostInfoCore.Finalize;
begin
  Core.Arrays.VarString.Done(FLines);
  Storage.UserStorage.Items.SMTP.Done(FSummary);
  Storage.UserStorage.Items.SMTP.Done(FMessage);

  Core.Arrays.VarString.Done(FContent);
  Storage.UserAccounts.Items.Done(FAccount);
  Core.Strings.Done(FFormData);

  Core.Arrays.KeyString.Done(kplFormData);
  Core.Arrays.VarString.Done(saRecipient);
end;

function  TPostInfoCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
  Empty(FFormData);
  Empty(FNameSpace);
  Storage.UserAccounts.Items.Empty(FAccount);
  Storage.UserStorage.Items.SMTP.Empty(FSummary);
  Storage.UserStorage.Items.SMTP.Empty(FMessage);
  Core.Arrays.KeyString.Empty(kplFormData);

  Core.Arrays.VarString.Empty(FLines);
  Core.Arrays.VarString.Empty(FContent);
  Core.Arrays.VarString.Empty(saRecipient);
  Empty(FEmail);
  Result:=CO_STATUS_OK;
end;

function  TPostInfoCore.Push_CMD_Email(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
end;

function  TPostInfoCore.Push_CMD_DB(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv:integer;
begin
  Core.Arrays.KeyString.fromStream(kplFormData,Data,'=','&');
  for iLcv:=0 to High(kplFormData) do begin
    FKP:=kplFormData[iLcv];
    FKP^.Key:=HTTPDefs.HTTPDecode(FKP^.Key);
    FKP^.Value:=HTTPDefs.HTTPDecode(FKP^.Value);
  end;
  FNameSpace:=Core.Arrays.KeyString.GetItemAsString(kplFormData,Storage.PostInfo.Items.Email.NameSpace);
  FEmail:=Core.Arrays.KeyString.GetItemAsString(kplFormData,Storage.PostInfo.Items.Email.Address);
  if Length(FEmail)>0 then begin
    FSubject:=Core.Arrays.KeyString.GetItemAsString(kplFormData,Storage.PostInfo.Items.Email.Subject);
    FBody:=Core.Arrays.KeyString.GetItemAsString(kplFormData,Storage.PostInfo.Items.Email.Body);
  end;
  Core.Arrays.KeyString.Update(kplFormData,Storage.PostInfo.Items.Email.Address,false);
  Core.Arrays.KeyString.Update(kplFormData,Storage.PostInfo.Items.Email.Subject,false);
  Core.Arrays.KeyString.Update(kplFormData,Storage.PostInfo.Items.Email.Body,false);
  Core.Arrays.KeyString.Update(kplFormData,Storage.PostInfo.Items.Email.NameSpace,false);

  FFormData:=Core.Arrays.KeyString.toString(kplFormData,'=',#13#10);

  if (Length(FEmail)>0) then begin
    if Length(FBody)>0 then
      FBody:=SysUtils.StringReplace(FBody,'{$i formdata}',FFormData,[rfReplaceAll])
    else
      FBody:=FFormData;
    Core.Arrays.VarString.fromString(FContent,FBody);
    With FSummary do begin
      Kind:=Storage.UserStorage.Kind.SMTP;
      Sender:=Concat(OwnerP^.RootP^.User,'@',OwnerP^.DomainP^.Name);
      &To:=FEmail;
      From:=Sender;
      Subject:=FSubject;
      Domain:=ExtractDomain(FEmail);
      User:=ExtractUserName(FEmail);
      RemoteIP:=Core.Utils.Sockets.InAddrToStr(SR.Address.sin_addr.S_addr);
      RemoteDomain:=Concat(OwnerP^.NodeP^.Alias,'.',OwnerP^.DomainP^.Name);
      RemoteFrom:=Sender;
      Exchanger:=RemoteDomain;
      MessageId:=Concat('<',Storage.UserStorage.GenerateUID(),'.',Sender,'.',RemoteDomain,'>');
      UTC:=Core.Timer.dtUT;
      Date:=Core.Timer.dtNow;
      tzBias:=BiasMinutes;
      cntType:=Storage.UserStorage.Items.SMTP.Content.ctTextPlain;
    end;


    if Storage.Domains.Items.DB.GetID(FTask,FSummary.Domain,FAccount.DomainID) then begin
      Core.Arrays.KeyString.Update(FMessage.Headers,'From',FSummary.From);
      Core.Arrays.KeyString.Update(FMessage.Headers,'To',FSummary.&To);
      Core.Arrays.KeyString.Update(FMessage.Headers,'Subject',FSummary.Subject);
      Core.Arrays.KeyString.Update(FMessage.Headers,'Date',Core.Utils.Time.TimeZoneTime);
      Core.Arrays.KeyString.Update(FMessage.Headers,'Message-Id',FSummary.MessageId);
      Core.Arrays.KeyString.Update(FMessage.Headers,'MIME-Version','1.0');
      Core.Arrays.KeyString.Update(FMessage.Headers,'Content-Type','text/plain; charset=utf-8');
      Core.Arrays.KeyString.Update(FMessage.Headers,'Content-Transfer-Encoding',Storage.UserStorage.Items.SMTP.Encoding.Value[Storage.UserStorage.Items.SMTP.Encoding.em8Bit]);

      FRefactor.Size:=0;
      Core.Arrays.KeyString.toStream(FMessage.Headers,FRefactor,': ',#13#10);
      FRefactor.Position:=0;
      Core.Arrays.VarString.fromStream(FMessage.Content,FRefactor);
      Core.Arrays.VarString.Add(@FMessage.Content,'',[aoCheckForDuplicates]);
      Core.Arrays.VarString.Add(FContent,FMessage.Content);
      FRefactor.Size:=0;


      Storage.UserStorage.Items.SMTP.Stamp(
        FSummary,
        FMessage.Headers,
        FMessage.Content,
        FRefactor
      );
      FRefactor.Size:=0;
      Storage.UserStorage.Mimes(FMessage.Headers,FMessage.Content,FSummary,FRefactor);
      if Storage.UserAccounts.Items.DB.Fill(FTask,FSummary.User,FAccount) then begin
        FFolderID:=FAccount.Inbox;
        FFileID:=0;
        if Storage.UserStorage.Items.SMTP.Write (
          FTask,FAccount.AuraNode,
          FAccount.SpamBox,FAccount.ID,FAccount.DomainID,
          FSummary,
          FMessage.Content,
          Storage.UserStorage.Items.IMAP.Flags.Recent,
          FRefactor,
          Storage.UserStorage.Items.SMTP.BypassFilters,
          FFolderID,
          FFileID
        ) then begin

        end;
      end;
    end;
  end;
  if Storage.PostInfo.Items.DB.Post(FTask,OwnerP^.DomainP^.ID,SR.Address.sin_addr.S_addr,FNameSpace,FFormData) then begin
    Result:=CO_STATUS_OK;

  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end;

  Empty(FLines);
  Empty(FContent);
  Empty(FBody);
  Storage.UserStorage.Items.SMTP.Empty(FSummary);
  Storage.UserStorage.Items.SMTP.Empty(FMessage);
end;

end.


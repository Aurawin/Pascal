
{
 unit coData.pas

 This core object provides developers the ability to have
 instant access to SQL and NOSQL data.

 Copyright Aurawin LLC 2003-2014
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/core/vdm/license.txt
}

unit coData;

interface

uses
  uRSR,Classes,uCoreObjects,RSR,uKeywords,uStorage,HSRConsts,hHttpd,
  RSR.HTTP,hDatabase,Core.Utils.Time,Core.Timer,sha1,uByteArray,Core.Arrays.VarString,uInt64Array,Core.Strings,
  HTTPDefs,Core.Arrays.KeyString,Storage.CoreObjects,Storage.UserAccounts,dbmPostInfo,dbmUserStorage,dbmDomains,
  uMsgUtils,Core.Utils.Sockets;

type
  Data=class
  const
    ACLInf:TACLInfo=(
      Name                     : 'Data';
      NameSpace                : '/core/d';
      Caption                  : 'Data Core Object';
      Prompt                   : 'User has access to data';
      Description              : 'Provides applications with SQL and NOSQL data'
    );
    CLSInf:TCLSInfo=(
      Name                     : 'TDataCore';
      Location                 : 'coData.pas';
    );
    Header:TCoreObjectInfo=(
      ID                       : 0;
      ProviderID               : 0;
      Enabled                  : true;
      Anonymous                : false;
      NotifyOnBuffersChanged   : false;
      Scale                    : 0;
      CLSInfo                  : @CLSInf;
      ACLInfo                  : @ACLInf;
    );
    XMLInf:TXMLInfo=(
      Enabled                  : false;
    );
  Type
    Read=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Read';
        NameSpace              : '/r';
        Caption                : 'Read Data';
        Prompt                 : 'User can access read data items';
        Description            : 'Provides read access to data';
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
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    Write=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Write';
        NameSpace              : '/w';
        Caption                : 'Write Data';
        Prompt                 : 'User can write data items';
        Description            : 'Provides write access to data';
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
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    Delete=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'Delete';
        NameSpace              : '/d';
        Caption                : 'Delete Data';
        Prompt                 : 'User can delete data items';
        Description            : 'Provides delete access to data';
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
        Method                   : nil;
        Resource                 : nil;
      );
    end;
    List=class
    const
      ACLInf:TACLInfo=(
        Name                   : 'List';
        NameSpace              : '/l';
        Caption                : 'List Data';
        Prompt                 : 'User can list data items';
        Description            : 'Provides list access to data';
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
        Method                   : nil;
        Resource                 : nil;
      );
    end;
  end;
  TDataCore=Class(TCoreObject)
  private
    DataP                        : PHTTP;
  private
    function  CMD_Read(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  CMD_Write(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  CMD_List(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  CMD_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
uses SysUtils,StrUtils;

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
  Init(FLines);
  Storage.Items.SMTP.Init(FSummary);
  Init(FAccount);
  Init(kplFormData);

  Init(saRecipient);
  Init(FContent);
  Storage.Items.SMTP.Init(FMessage);
  with PostInfo do begin
    Storage.CoreObjects.Add(Email.cmd,FCommands,Header,@Push_CMD_Email);
    Storage.CoreObjects.Add(DB.cmd,FCommands,Header,@Push_CMD_DB);
  end;

end;

procedure TPostInfoCore.Finalize;
begin
  Done(FLines);
  Storage.Items.SMTP.Done(FSummary);
  Storage.Items.SMTP.Done(FMessage);

  Done(FContent);
  Done(FAccount);
  Done(FFormData);

  Done(kplFormData);
  Done(saRecipient);
end;

function  TPostInfoCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
  Empty(FFormData);
  Empty(FNameSpace);
  Empty(FAccount);
  Storage.Items.SMTP.Empty(FSummary);
  Storage.Items.SMTP.Empty(FMessage);
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
  FNameSpace:=Core.Arrays.KeyString.GetItemAsString(kplFormData,dbmPostInfo.PostInfo.Email.NameSpace);
  FEmail:=Core.Arrays.KeyString.GetItemAsString(kplFormData,dbmPostInfo.PostInfo.Email.Address);
  if Length(FEmail)>0 then begin
    FSubject:=Core.Arrays.KeyString.GetItemAsString(kplFormData,dbmPostInfo.PostInfo.Email.Subject);
    FBody:=Core.Arrays.KeyString.GetItemAsString(kplFormData,dbmPostInfo.PostInfo.Email.Body);
  end;
  Core.Arrays.KeyString.Update(kplFormData,dbmPostInfo.PostInfo.Email.Address,false);
  Core.Arrays.KeyString.Update(kplFormData,dbmPostInfo.PostInfo.Email.Subject,false);
  Core.Arrays.KeyString.Update(kplFormData,dbmPostInfo.PostInfo.Email.Body,false);
  Core.Arrays.KeyString.Update(kplFormData,dbmPostInfo.PostInfo.Email.NameSpace,false);

  FFormData:=Core.Arrays.KeyString.toString(kplFormData,'=',#13#10);

  if (Length(FEmail)>0) then begin
    if Length(FBody)>0 then
      FBody:=SysUtils.StringReplace(FBody,'{$i formdata}',FFormData,[rfReplaceAll])
    else
      FBody:=FFormData;
    Core.Arrays.VarString.fromString(FContent,FBody);
    With FSummary do begin
      Kind:=Storage.Kind.SMTP;
      Sender:=Concat(OwnerP^.RootP^.User,'@',OwnerP^.DomainP^.Name);
      Recipients:=FEmail;
      From:=Sender;
      Subject:=FSubject;
      Domain:=ExtractDomain(FEmail);
      User:=ExtractUserName(FEmail);
      RemoteIP:=Core.Utils.Sockets.InAddrToStr(SR.Address.sin_addr.S_addr);
      RemoteDomain:=Concat(OwnerP^.NodeP^.Alias,'.',OwnerP^.DomainP^.Name);
      RemoteFrom:=Sender;
      Exchanger:=RemoteDomain;
      MessageId:=Concat('<',dbmUserStorage.GenerateUID(),'.',Sender,'>');
      UTC:=Core.Timer.dtUT;
      Date:=Core.Timer.dtNow;
      tzBias:=Core.Utils.Time.SystemBias;
      cntType:=Storage.Items.SMTP.Content.ctTextPlain;
    end;


    if dbmDomains.Domain_GetID(FModuleP^,FTaskP^,FSummary.Domain,FAccount.DomainID) then begin
      Core.Arrays.KeyString.Update(FMessage.Headers,'From',FSummary.From);
      Core.Arrays.KeyString.Update(FMessage.Headers,'To',FSummary.Recipients);
      Core.Arrays.KeyString.Update(FMessage.Headers,'Subject',FSummary.Subject);
      Core.Arrays.KeyString.Update(FMessage.Headers,'Date',Core.Utils.Time.TimeZoneTime);
      Core.Arrays.KeyString.Update(FMessage.Headers,'Message-Id',FSummary.MessageId);
      Core.Arrays.KeyString.Update(FMessage.Headers,'MIME-Version','1.0');
      Core.Arrays.KeyString.Update(FMessage.Headers,'Content-Type','text/plain; charset=utf-8');
      Core.Arrays.KeyString.Update(FMessage.Headers,'Content-Transfer-Encoding',Storage.Items.SMTP.Encoding.Value[Storage.Items.SMTP.Encoding.em8Bit]);

      FRefactor.Size:=0;
      Core.Arrays.KeyString.toStream(FMessage.Headers,FRefactor,': ',#13#10);
      FRefactor.Position:=0;
      Core.Arrays.VarString.fromStream(FMessage.Content,FRefactor);
      Core.Arrays.VarString.Add(@FMessage.Content,'',[aoCheckForDuplicates]);
      Core.Arrays.VarString.Add(FContent,FMessage.Content);

      FRefactor.Size:=0;

      Storage.Items.SMTP.Stamp(
        FSummary,
        FMessage.Headers,
        FMessage.Content,
        FRefactor
      );
      FRefactor.Size:=0;
      dbmUserStorage.Mimes(FMessage,FSummary,FRefactor);
      if Storage.UserAccounts.Items.DB.Fill(FModuleP^,FTaskP^,FSummary.User,FAccount) then begin
        if Storage.Items.SMTP.Write (
          FModuleP^,FTaskP^,FAccount.AuraNode,
          FAccount.Attachments,FAccount.Inbox,FAccount.SpamBox,FAccount.ID,FAccount.DomainID,
          FSummary,
          FMessage.Content,
          Storage.Items.IMAP.Flags.Recent,
          FRefactor
        ) then begin

        end;
      end;
    end;
  end;
  if dbmPostInfo.PostInfo.Post(FModuleP^,FTaskP^,OwnerP^.DomainP^.ID,SR.Address.sin_addr.S_addr,FNameSpace,FFormData) then begin
    Result:=CO_STATUS_OK;

  end else begin
    Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
  end;

  Empty(FLines);
  Empty(FContent);
  Empty(FBody);
  Storage.Items.SMTP.Empty(FSummary);
  Storage.Items.SMTP.Empty(FMessage);
end;

end.


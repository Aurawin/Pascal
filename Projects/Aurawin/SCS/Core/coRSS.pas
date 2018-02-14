unit coRSS;

{
 unit coRSS.pas

 This core object provides website visitors the ability to have
 access to rss streams or view items via http.

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

  Core.Keywords,

  App.Consts,

  Core.Database,
  Core.Database.Types,

  Core.Timer,
  Encryption.SHA,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,

  Core.Strings,
  Core.Utils.Time,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.RSS,

  SysUtils;

type
  ns=class
  type
    RSS=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'RSS';
        NameSpace                : '/core/rss';
        Caption                  : 'RSS Core Object';
        Prompt                   : 'User can access to rss system';
        Description              : 'Provides domain level streaming and rendering of RSS feeds'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TRSSCore';
        Location                 : 'coRSS.pas';
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
      Stream=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Stream';
          NameSpace              : '/s';
          Caption                : 'RSS Streams';
          Prompt                 : 'User can read RSS streams';
          Description            : 'Provides streaming capabilities for RSS feed';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
      Viewer=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Viewer';
          NameSpace              : '/v';
          Caption                : 'RSS Viewer';
          Prompt                 : 'User can view RSS feeds';
          Description            : 'Renders domain RSS feeds to HTTP';
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
          Method                 : nil;
          Resource               : nil;
        );
      end;
    end;
  end;
  TRSSCore=Class(TCoreObject)
  private
    ChannelsP                    : PRSSChannels;
    DataP                        : PHTTP;
    Request                      : THTTPRequest;
    Response                     : THTTPResponse;
    iParamCount                  : LongInt;
    saParameters                 : Core.Arrays.Types.VarString;
  private
    function  Push_CMD_Stream(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Viewer(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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


procedure Install(Task:Core.Database.Types.TTask);
begin
  TRSSCore.Install(Task);
end;

class procedure TRSSCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TRSSCore);
  with ns.RSS do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Stream.cmd);
    COREOBJECT_VerifyID(Task,Viewer.cmd);
  end;
end;

class procedure TRSSCore.UnInstall;
begin
  UnRegisterClass(TRSSCore);
end;

procedure TRSSCore.Initialize;
begin
  if OwnerP^.DomainP<>nil then
    ChannelsP:=OwnerP^.DomainP^.FeedsP;
  Request:=nil;
  Response:=nil;
  Core.Arrays.VarString.Init(saParameters);
  with ns.RSS do begin
    Storage.CoreObjects.Add(Stream.cmd,FCommands,Header,@Push_CMD_Stream);
    Storage.CoreObjects.Add(Viewer.cmd,FCommands,Header,@Push_CMD_Viewer);
  end;
end;

procedure TRSSCore.Finalize;
begin
  Core.Arrays.VarString.Done(saParameters);
end;

function  TRSSCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  DataP:=SR.Info.DataP;
  Core.Arrays.VarString.fromString(saParameters,Request.Query,'?');
end;
{
procedure TRSSCore.PushBuildPage;
begin
  RSR.cntrTX.Increment;
  FResponse.Close:=FRequest.Close;
  FResponse.Code:=HTTP_SC_OK;
  FResponse.Status:='OK';
  FResponse.ContentType:=ctHtml;
  if Resource.HasKeywords then begin
    FResponse.Content:=Resource.toString;
    FResponse.Send(RSRP);
  end else begin
    FResponse.Send(RSRP,Resource);
  end;
  FResponse.Empty;
end;
}
function  TRSSCore.Push_CMD_Stream(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv,iIndex:Integer;
begin
  {$if defined(cpu64)}
    InterlockedIncrement64(RSR.cntrTX);
  {$else}
    Inc(RSR.cntrTX);
  {$endif}
  iIndex:=Storage.RSS.Channels.IndexOf(ChannelsP^,StrToIntDef(saParameters[1],-1));
  with Transport(SR).Output do begin
    Seek(0,soFromEnd);
    WriteAnsiString('<?xml version="1.0" encoding="iso-8859-1" ?><rss version="2.0">'#13#10'<channel>'#13#10);
    If (iIndex>-1) and (iIndex<Length(ChannelsP^)) then with ChannelsP^[iIndex]^ do begin
      WriteAnsiString(
        Concat(
          '<title>',Title,'</title>'#13#10,
          '<description>',Description,'</description>'#13#10,
          '<link>',Link,'</link>'#13#10,
          '<copyright>',Copyright,'</copyright>'#13#10,
          '<language>',Language,'</language>'#13#10,
          '<lastbuilddate>',DateTimeToRFCDateTime(LastBuildDate),'</lastbuilddate>'#13#10,
          '<managingeditor>',Editor,'</managingeditor>'#13#10,
          '<webmaster>',Webmaster,'</webmaster>'#13#10,
          '<image>'#13#10,
            '<url>',Image.URL,'</url>'#13#10,
            '<title>',Image.Title,'</title>'#13#10,
            '<link>',Image.Link,'</link>'#13#10,
          '</image>'#13#10
        )
      );
      For iLcv:=0 to High(Items) do begin
        { <item>
            <title>911 Tapes Show Lake George Horror</title>
            <link>http://www.foxnews.com/story/0,2933,171251,00.html</link>
            <description>Newly released 911 tapes show the desperation of witnesses to summon help after a tour boat capsized in the Adirondack Mountains, killing 20 elderly people.</description>
            <author>foxnewsonline@foxnews.com</author>
            <pubDate>Wed, 05 Oct 2005 04:55:48 EST</pubDate>
          </item>  }
        WriteAnsiString(
          Concat(
          '<item>'#13#10,
            '<title>',Items[iLcv]^.Title,'</title>'#13#10,
            '<link>',Items[iLcv]^.Link,'</link>'#13#10,
            '<description>',Items[iLcv]^.Description,'</description>'#13#10,
            '<author>',Items[iLcv]^.Author,'</author>'#13#10,
            '<pubdate>',DateTimeToRFCDateTime(Items[iLcv]^.PubDate),'</pubdate>'#13#10,
          '</item>'
          )
        );
      end;
      WriteAnsiString(#13#10'</channel>'#13#10'</rss>');
      Response.ContentType:=ctXML;
      Result:=CO_STATUS_OK;
    end else begin
      Result:=CO_STATUS_ERR_CO_RESOURCE_NOT_FOUND;
    end;
  end;
end;

function  TRSSCore.Push_CMD_Viewer(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv,iIndex:Integer;
begin
  iIndex:=Storage.RSS.Channels.IndexOf(ChannelsP^,StrToIntDef(saParameters[1],-1));
  If (iIndex>-1) and (iIndex<Length(ChannelsP^)) then with ChannelsP^[iIndex]^ do begin
    {
    sTitle:=Title;
    sDescription:=Description;
    sOutput:=Concat(
      '<table class="',TC,'">',
      '<tr class="',TRC,'"><th colspan=2 class="',THC,'">',Title,'</th></tr>',
      '<tr class="',TRC,'">',
        '<td colspan=2 class="',TDC,'"><a href="',Image.Link,'" class="',TLC,'"><img src="',Image.URL,'" alt="',Image.Title,'" class="',TLC,'"></a>',
        Description,'</td>',
      '</tr>',
      '<tr class="',TRC,'"><td colspan=2 class="',TDC,'"><a href="',Link,'" class="',TLC,'"><img src="',SubscribeIconUrl,'" alt="Feed" class="',TLC,'">',Link,'</a></td></tr>'
    );
    For iLcv:=0 to High(Items) do begin
      sOutput:=Concat(sOutput,
        '<tr clss="',TRC,'"><th colspan=2 class="',TSC,'">',Items[iLcv].Title,'</th></tr>',
        '<tr class="',TRC,'"><td class="',TDC,'">Author</td><td class="',TDC,'">',Items[iLcv].Author,'</td></tr>',
        '<tr class="',TRC,'"><td class="',TDC,'">Date</td><td class="',TDC,'">',DateTimeToRFCDateTime(Items[iLcv].PubDate),'</td></tr>',
        '<tr class="',TRC,'"><td class="',TDC,'">Link</td><td class="',TDC,'"><a href="',Items[iLcv].Link,'" class="',TLC,'">',Items[iLcv].Link,'</a></td></tr>',
        '<tr class="',TRC,'"><td class="',TDC,'">Description</td><td class="',TDC,'">',Items[iLcv].Description,'</td></tr>'
      );
    end;
    sOutput:=Concat(sOutput,'<tr class="',TRC,'"><th colspan=2 class="',TFC,'">',Copyright,'</th></tr>','</table>');
    }
    Result:=CO_STATUS_OK;
  end else begin
    //sTitle:='Channel Error';
    //sOutput:='<b><h1>Channel Not Found!</h1></b>';
    Result:=CO_STATUS_ERR_CO_RESOURCE_NOT_FOUND;
  end;
end;



end.


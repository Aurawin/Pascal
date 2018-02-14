unit coCalendar;

{
 unit coCalendar.pas

 This core object provides website visitors the ability to have
 access to calendar events or view events via http.

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}



interface

uses
  Classes,
  hHttpd,

  App.Consts,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Core.Keywords,
  Core.Strings,
  Core.Utils.Time,
  Core.Timer,

  Core.Database,
  Core.Database.Types,

  Encryption.SHA,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,

  Storage,
  Storage.Main,
  Storage.CoreObjects,
  Storage.UserAccounts,
  Storage.RSS,
  Storage.Calendaring,

  SysUtils;


type
  ns=class
  type
    Calendar=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Calendar';
        NameSpace                : '/core/calendar';
        Caption                  : 'Calendar Core Object';
        Prompt                   : 'User has access to View calendar events';
        Description              : 'Provides domain level calendar events'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TCalendarCore';
        Location                 : 'coCalendar.pas';
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
      Events=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List';
          NameSpace              : '/l';
          Caption                : 'Events List';
          Prompt                 : 'User can access list of calendar events';
          Description            : 'Provides capabilities for reading calendar events';
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
          Method             : nil;
          Resource           : nil;
        );
      end;
      Event=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Event';
          NameSpace              : '/r';
          Caption                : 'Read Event';
          Prompt                 : 'User can read a calendar event';
          Description            : 'Provides capabilities for reading a calendar event';
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
          Method             : nil;
          Resource           : nil;
        );
      end;
    end;
  end;
  TCalendarCore=Class(TCoreObject)
  private
    iParamCount                  : LongInt;
    ChannelsP                    : PRSSChannels;
    EventsP                      : Storage.Calendaring.Items.PEvents;
    ConfigsP                     : Storage.Calendaring.Items.PConfig;
    ConfigP                      : Storage.Calendaring.Items.PConfigItem;
    Request                      : THTTPRequest;
    Response                     : THTTPResponse;
  private
    function  Push_CMD_Events(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  Push_CMD_Event(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
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
  TCalendarCore.Install(Task);
end;

class procedure TCalendarCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TCalendarCore);
  with ns.Calendar do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,Events.cmd);
    COREOBJECT_VerifyID(Task,Event.cmd);
  end;
end;

class procedure TCalendarCore.UnInstall;
begin
  UnRegisterClass(TCalendarCore);
end;

procedure TCalendarCore.Initialize;
begin
  if OwnerP^.DomainP<>nil then begin
    ChannelsP:=OwnerP^.DomainP^.FeedsP;
    EventsP:=OwnerP^.DomainP^.EventsP;
  end;
  Request:=nil;
  Response:=nil;
  with ns.Calendar do begin
    Storage.CoreObjects.Add(Events.cmd,FCommands,Header,@Push_CMD_Events);
    Storage.CoreObjects.Add(Event.cmd,FCommands,Header,@Push_CMD_Event);
  end;
end;


procedure TCalendarCore.Finalize;
begin

end;

function  TCalendarCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  iParamCount:=Length(Parameters);
end;

function TCalendarCore.Push_CMD_Event(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iIndex,iCount:Integer;
begin
  {
  saParameters[1] // Year
  saParameters[2] // Month
  saParameters[3] // Day
  }
  iIndex:=Storage.RSS.Channels.IndexOf(ChannelsP^,StrToIntDef(Parameters[1],-1));
  iCount:=Length(EventsP^);
  If (iIndex<>-1) and (iIndex<iCount) then begin
    With EventsP^[iIndex] do begin
      { html formatted output... deprecated
      sOutput:=Concat(sOutput,'<table class="',TC,'">',#13#10,
        '<tr class="',TRC,'"><th colspan=2 class="',THC,'">',Core.Utils.Time.Day_Long[DayOfWeek(dtDateTime)],', ',Core.Utils.Time.Month_Long[MonthOf(dtDateTime)],' ',IntToStr(DayOf(dtDateTime)),' ',IntToStr(YearOf(dtDateTime)),'</th></tr>',#13#10,
        '<tr class="',TRC,'"><td colspan=2 class="',TDC,'">',Title,'</td></tr>',#13#10,
        '<tr class="',TRC,'"><td class="',TDC,'">Time:</td><td class="',TDC,'">',HTTP_DateTime(dtDateTime),'</td></tr>',#13#10,
        '<tr class="',TRC,'"><td class="',TDC,'">Description:</td><td class="',TDC,'">',Description,'</td></tr>',#13#10,
        '<tr class="',TRC,'"><td colspan=2 class="',TSC,'">Coordinator</td></tr>',#13#10,
        '<tr class="',TRC,'"><td class="',TDC,'">Name:</td><td class="',TDC,'">',Coordinator,'</td></tr>',#13#10,
        '<tr class="',TRC,'"><td class="',TDC,'">Phone:</td><td class="',TDC,'">',Coordinator_Phone,'</td></tr>',#13#10,
        '<tr class="',TRC,'"><td class="',TDC,'">Email:</td><td class="',TDC,'"><a class="',TLC,'" href="mailto:',Coordinator_EM,'">',Coordinator_EM,'</a></td></tr>',#13#10,
        '<tr class="',TRC,'"><td class="',TDC,'">IM:</td><td class="',TDC,'"><a class="',TLC,'" href="im:',Coordinator_IM,'">',Coordinator_IM,'</a></td></tr>',#13#10,
        '<tr class="',TRC,'"><td colspan=2 class="',TSC,'">Notes</td></tr>',#13#10,
        '<tr class="',TRC,'"><td colspan=2 class="',TDC,'">',SysUtils.StringReplace(Notes,#13#10,'<br>',[rfReplaceAll]),'</td></tr>',#13#10,
        '<tr class="',TRC,'"><th colspan=2 class="',TFC,'">&nbsp;</th></tr>',#13#10
      );
      }
      {
      sTitle:=Concat(OwnerP^.DomainP^.FriendlyName,' Event ',Title);
      sDescription:=Concat(OwnerP^.DomainP^.Name,' Event ',Title);
      Case iCount of
        0: sBlurb:='There are no events planned for today.';
        1: sBlurb:='There is only one event planned for today.';
      else
        sBlurb:=Concat('There are ',IntToStr(iCount),' events planned for today.');
      end;
      }
    end;
    Result:=CO_STATUS_OK;
  end else begin
  {
    sTitle:='Event Not Found!';
    sDescription:='The event requested was not found.';
    }
    Result:=CO_STATUS_ERR_CO_RESOURCE_NOT_FOUND;
  end;
end;

function TCalendarCore.Push_CMD_Events(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  QueryDate:TDateTime;
  iCount,iLcv:Integer;
begin
  {
  saParameters[1] // Year
  saParameters[2] // Month
  saParameters[3] // Day
  }
  If Core.Strings.isAllDigits(Parameters[1]) and Core.Strings.isAllDigits(Parameters[2]) and Core.Strings.isAllDigits(Parameters[3]) then begin
    ConfigP:=@ConfigsP^[Storage.Calendaring.Items.Index.Events];
    QueryDate:=EncodeDate(StrToInt(Parameters[1]),StrToInt(Parameters[2]),StrToInt(Parameters[3]));
    iCount:=0;
    For iLcv:=0 to High(EventsP^) do begin
      With EventsP^[iLcv] do begin
        If DateUtils.IsSameDay(Core.Timer.dtNow,QueryDate) then begin
          { html formatted output deprecated.
          sOutput:=Concat('<table class="',TC,'">',
            '<tr class="',TRC,'"><th colspan=2 class="',THC,'">',Title,'</th></tr>',#13#10,
            '<tr class="',TRC,'"><td class="',TDC,'">Time:</td><td class="',TDC,'">',HTTP_DateTime(dtDateTime),'</td></tr>',#13#10,
            '<tr class="',TRC,'"><td class="',TDC,'">Description:</td><td class="',TDC,'">',Description,'</td></tr>',#13#10,
            '<tr class="',TRC,'"><td colspan=2 class="',TSC,'">Coordinator</td></tr>',#13#10,
            '<tr class="',TRC,'"><td class="',TDC,'">Name:</td><td class="',TDC,'">',Coordinator,'</td></tr>',#13#10,
            '<tr class="',TRC,'"><td class="',TDC,'">Phone:</td><td class="',TDC,'">',Coordinator_Phone,'</td></tr>',#13#10,
            '<tr class="',TRC,'"><td class="',TDC,'">Email:</td><td class="',TDC,'"><a class="',TLC,'" href="mailto:',Coordinator_EM,'">',Coordinator_EM,'</a></td></tr>',#13#10,
            '<tr class="',TRC,'"><td class="',TDC,'">IM:</td><td class="',TDC,'"><a class="',TLC,'" href="im:',Coordinator_IM,'">',Coordinator_IM,'</a></td></tr>',#13#10,
            '<tr class="',TRC,'"><td colspan=2 class="',TSC,'">Notes</td></tr>',#13#10,
            '<tr class="',TRC,'"><td colspan=2 class="',TDC,'">',SysUtils.StringReplace(Notes,#13#10,'<br>',[rfReplaceAll]),'</td></tr>',#13#10,
            '<tr class="',TRC,'"><th colspan=2 class="',TFC,'">&nbsp;</th></tr></table>',#13#10
          );
          }
          Inc(iCount);
        end;
      end;
    end;
    {
    sDescription:=Concat(OwnerP^.DomainP^.FriendlyName,' Events for ',Core.Utils.Time.Day_Long[DayOfWeek(QueryDate)],', ',Core.Utils.Time.Month_Long[MonthOf(QueryDate)],' ',IntToStr(DayOf(QueryDate)),' ',IntToStr(YearOf(QueryDate)));
    sTitle:=Concat(OwnerP^.DomainP^.FriendlyName,' Events for ',Core.Utils.Time.Day_Long[DayOfWeek(QueryDate)],', ',Core.Utils.Time.Month_Long[MonthOf(QueryDate)],' ',IntToStr(DayOf(QueryDate)),' ',IntToStr(YearOf(QueryDate)));
    Case iCount of
      0: sBlurb:='There are no events planned for today.';
      1: sBlurb:='There is only one event planned for today.';
    else
      sBlurb:=Concat('There are ',IntToStr(iCount),' events planned for today.');
    end;
    }
    Result:=CO_STATUS_OK;
  end else begin
    {
    sTitle:='Invalid calendar date!';
    sDescription:='The date specified for retrieving events was not valid.';
    }
    Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
  end;
end;

end.


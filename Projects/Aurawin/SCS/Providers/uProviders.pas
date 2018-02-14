{
unit uProviders.pas

Copyright Aurawin LLC 2003-2015
Written by: Andrew Thomas Brunner

This code is issued under the Aurawin Public Release License
http://www.aurawin.com/aprl.html

Search Provider Engine for Search System
}

unit uProviders;

interface
uses
  Classes,

  RSR,
  RSR.HTTP,
  RSR.DNS,

  App.Consts,

  Core.Timer,


  hHTTPd,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.LargeWord,
  Core.Arrays.Pointers,

  Core.Strings,

  Core.Utils.Sockets,

  Core.Database,
  Core.Database.Types,

  Core.Logging,
  Encryption.Base64,
  Core.Streams,
  Core.Arrays.VarString,

  Storage,
  Storage.Main,
  Storage.DNS,
  Storage.SrchQueries,
  Storage.SrchResults,
  Storage.SrchTmpUserInteractions,
  Storage.SrchProviders,
  Storage.SrchUserLayer,
  Storage.KeepAlive,

  Sockets;
Const
  PROVIDER_STACK_SIZE            = 1024*10;
  MAX_QRLIST_SIZE                = 100;
  CACHE_TIMEOUT                  = 1000*60*4; // 4 minutes
  MAX_ERROR_COUNT                = 4;
Type
  TPSKind=(pskNone,pskProviderData,pskProviderEndUserCache,pskProviderDNS);
  PProviderSocketInfo=^TProviderSocketInfo;
  TResultParsed=procedure(InfoP:PProviderSocketInfo; Var Title,Link,Description:Core.Strings.VarString) of object;
  TOnProviderContent=procedure(InfoP:PProviderSocketInfo; Var Content:Core.Strings.VarString) of object;
  TInitializeResultProvider=procedure (RSRP:PRSR; Port,MaxCount:Integer; Var SearchTerm,Domain:Core.Strings.VarString) of Object;
  TProviderSocketInfo=record
    Sent                         : Boolean;
    Exhaustive                   : Boolean;

    byTry                        : Byte;
    byLevelQ                     : Byte;
    byLevelR                     : Byte;
    PageNumber                   : LongInt;
    FirstReturnOn                : LongInt;
    ErrorCount                   : LongInt;
    Total                        : LongInt;
    Current                      : LongInt;
    Max                          : LongInt;
    Requested                    : LongInt;
    BindIP                       : QWord;
    RemoteIP                     : QWord;
    DNS_IP                       : QWord;
    EU_ID                        : QWord;
    StreamResultIndex            : LongInt;
    Kind                         : TPSKind;
    SenderP                      : PRSR;
    CacheP                       : PProviderSocketInfo;
    RequestP                     : PProviderSocketInfo;

    ResultSet                    : Storage.SrchQueries.Results.Item;
    Results                      : Storage.SrchResults.Store.Items;
    Interactions                 : Storage.SrchTmpUserInteractions.Interaction.Manifest;

    OutputFormat                 : Core.Strings.VarString;
    Query                        : Core.Strings.VarString;
    SearchTerm                   : Core.Strings.VarString;
    NextPage                     : Core.Strings.VarString;
    Cookies                      : Core.Arrays.Types.KeyStrings;
    QueryID                      : QWord;
    QueryResultsID               : QWord;
    SocketID                     : QWord;
  end;

  TProviderManager=Class(TRSRManager)
  private
    FRSRP                        : PRSR;

    iIPLcv                       : LongInt;
    IPList                       : Core.Arrays.Types.LargeWord;
    sPage                        : Core.Strings.VarString;

  private
    function    GetNextProviderIP:QWord;
  private
    procedure   OnResultParsed(InfoP:PProviderSocketInfo; Var Title,Link,Description:Core.Strings.VarString);
    procedure   OnProviderError(InfoP:PProviderSocketInfo; Var sContent:Core.Strings.VarString);
    procedure   SendResultsBack(InfoP:PProviderSocketInfo);
    procedure   PushHTTPError(RSRP:PRSR; sCode,sMessage:Core.Strings.VarString);
    procedure   PushContentError(RSRP:PRSR; sContent:Core.Strings.VarString);
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  public
    Constructor Create(AHeader:Core.Database.Types.THeader; AProviderP:Storage.SrchProviders.Items.PItem); reintroduce;
    Destructor  Destroy; override;
  public
    ProviderP                    : Storage.SrchProviders.Items.PItem;
  public
    procedure   SubmitQuery(aBindIP:QWord; aBindPort:Word; sQuery:Core.Strings.VarString; EUID:QWord; SenderP:PRSR; MaxResults,FirstReturnOn:Integer; Var OutputFormat:Core.Strings.VarString);
    procedure   ObtainMoreResults(SenderP:PRSR; CacheSocket:QWord; CurrentCount:Integer; QueryID,EUID:QWord); // Cancels the old query if not found...
  End;
  procedure Empty(Var Item:TProviderSocketInfo); overload;
  procedure Done(Var Item:TProviderSocketInfo); overload;

implementation
uses
  SysUtils,uGoogle,StrUtils,DateUtils;

procedure Done(Var Item:TProviderSocketInfo);
begin
  Finalize(Item.Query);
  Finalize(Item.SearchTerm);
  Finalize(Item.NextPage);
  Finalize(Item.OutputFormat);
  Core.Arrays.KeyString.Done(Item.Cookies);
  Storage.SrchQueries.Results.Done(Item.ResultSet);
  Storage.SrchResults.Store.Done(Item.Results);
  Storage.SrchTmpUserInteractions.Interaction.Done(Item.Interactions);
  Finalize(Item);
end;

procedure Empty(Var Item:TProviderSocketInfo);
begin
  Item.CacheP:=Nil;
  Item.RequestP:=Nil;
  Item.QueryResultsID:=0;
  Item.QueryID:=0;
  Item.BindIP:=0;
  Item.RemoteIP:=0;
  Item.DNS_IP:=0;
  Item.EU_ID:=0;
  Item.PageNumber:=0;
  Item.ErrorCount:=0;
  Item.Exhaustive:=True;
  Item.Sent:=False;
  Item.Max:=0;
  Item.FirstReturnOn:=0;
  Item.Requested:=0;
  Item.StreamResultIndex:=0;
  Item.SocketID:=0;
  Item.Total:=0;
  Item.Current:=0;
  Item.byTry:=0;
  Item.byLevelQ:=0;
  Item.byLevelR:=0;
  Item.Kind:=pskNone;
  Item.SenderP:=Nil;
  SetLength(Item.Query,0);
  SetLength(Item.SearchTerm,0);
  SetLength(Item.NextPage,0);
  SetLength(Item.OutputFormat,0);
  Core.Arrays.KeyString.Empty(Item.Cookies);
  Storage.SrchQueries.Results.Empty(Item.ResultSet);
  Storage.SrchResults.Store.Empty(Item.Results);
  Storage.SrchTmpUserInteractions.Interaction.Empty(Item.Interactions);
end;

Constructor TProviderManager.Create(AHeader:Core.Database.Types.THeader; AProviderP:Storage.SrchProviders.Items.PItem);
begin
  iIPLcv:=0;
  ProviderP:=AProviderP;

  Task:=Core.Database.Types.TTask.Create(AHeader,'Providers');

  Inherited Create(nil,NO_SSL,SSL_OFF,THREAD_METHODS_OFF,PROVIDER_STACK_SIZE);

  Core.Arrays.Pointers.Add(ProviderP^.Managers,Self);
end;

Destructor  TProviderManager.Destroy;
begin
  Task.Free();
  Core.Arrays.LargeWord.Done(IPList);
  Core.Arrays.Pointers.Remove(ProviderP^.Managers,Self);
  Inherited Destroy;
end;

procedure   TProviderManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(ProviderP^.Domain,SERVICE_PROVIDER,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure  TProviderManager.OnQueue(RSRP:PRSR);
begin

end;

procedure  TProviderManager.OnConnect(RSRP:PRSR);
var
  InfoP:PProviderSocketInfo;
  sHeader:Core.Strings.VarString;
begin
  InfoP:=RSRP^.Info.DataP;
  If InfoP<>Nil then begin
    InfoP^.byLevelR:=0;
    InfoP^.byLevelQ:=1;
    Empty(InfoP^.Cookies);
    sHeader:=Concat(
      'GET ',InfoP^.Query,' HTTP/1.1',#13#10,
      'Host: ',RSRP^.Info.Server,#13#10,
      'Connection: Keep-Alive'#13#10,      
      'Accept-Language: en',#13#10,
      #13#10
    );
    Send(
      RSRP,
      sHeader
    );
  end;
end;

(*
procedure  OnNewProviderSocket(AManager:Pointer; ItemP:PRSRAddInfo); stdcall;
var
  Manager:TProviderManager;
  InfoP:PProviderSocketInfo;
begin
  // The socket is now useable...
  Manager:=TProviderManager(AManager);
  InfoP:=ItemP^.DataP;
  InfoP^.SocketID:=ItemP^.Socket;
  SocketMap[ItemP^.Socket].sktOPS:=ItemP^.Socket;
  SocketMap[ItemP^.Socket].dwExpires:=Core.Timer.dwTick+Manager.TimeOut;
  If Manager.iIPLcv>=Length(Manager.IPList) then
    Manager.iIPLcv:=0;
  Manager.Connect(@SocketMap[ItemP^.Socket],Manager.IPList[Manager.iIPLcv],ItemP^.Port);
  Inc(Manager.iIPLcv);
end;
*)

procedure  TProviderManager.OnDisconnect(RSRP:PRSR);
var
  iLcv:Integer;
  InfoP:PProviderSocketInfo;
begin
  // Could be that the remote server has disconnected the connection.
  InfoP:=RSRP^.Info.DataP;
  If (InfoP<>Nil) then begin
    If not (InfoP^.CacheP^.Sent) and (InfoP^.Query<>'') then begin
      If (InfoP^.ErrorCount<3) and (InfoP^.byLevelQ<>InfoP^.byLevelR) and (InfoP^.CacheP^.Total<InfoP^.CacheP^.Max) then begin
        // Add New Socket... Pass this info... Set this info to nil... And Resubmit Query...
        Inc(InfoP^.ErrorCount);
        Inc(InfoP^.byTry);
      end else if (InfoP^.CacheP^.Total>0) and (InfoP^.CacheP^.StreamResultIndex<InfoP^.CacheP^.Total) then begin  // We are giving up...  Were these results Sent
        SendResultsBack(InfoP);
      end;
    end;
  end;
end;

procedure  TProviderManager.OnError(RSRP:PRSR);
  procedure PushTimeOutDNS;
  begin
    If Length(IPList)=0 then begin
      OnException('uProviders.TProviderManager.OnTimeOut','Retrieving IPList',Format(SRCH_PVDR_FMT_DNS_FAILURE,[ProviderP^.Caption,ProviderP^.Domain]));
      PProviderSocketInfo(RSRP^.Info.DataP)^.DNS_IP:=Storage.DNS.Native.GetNextHost(dnskRegular);
      DNSLookup(RSRP,RSRP^.Info.Server,[dnsCache,dnsIP],PProviderSocketInfo(RSRP^.Info.DataP)^.DNS_IP);
    end;
  end;

  procedure PushTimeOutCache;
  begin
    // Allow removal... Finalize will clean up anything going on...
  end;

  procedure PushTimeOutProviderData;
  begin
    Close(RSRP);
  end;

begin
  If RSRP^.Kind=rsrsDNS then begin
    RSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,TimeOut);
  end else begin
    Case PProviderSocketInfo(RSRP^.Info.DataP)^.Kind of
      pskProviderDNS          : PushTimeOutDNS;
      pskProviderData         : PushTimeOutProviderData;
      pskProviderEndUserCache : PushTimeOutCache;
    End;
  end;
end;

procedure  TProviderManager.OnDNSResult(RSRP:PRSR);
var
  iLcv,iipLen,isaLen:Integer;
  saIPs:Core.Arrays.Types.VarString;
begin
  Copy(RSRP^.DNS.Answers,saIPs);
  Try
    isaLen:=Length(saIPs);
    iipLen:=Length(IPList);
    If (isaLen<>0) and (isaLen<>iipLen) then
      SetLength(IPList,isaLen);
    For iLcv:=0 to isaLen-1 do
      IPList[iLcv]:=Core.Utils.Sockets.InAddrFromStr(saIPs[iLcv]);
  Finally
    Empty(saIPs);
  End;
end;

function   TProviderManager.GetNextProviderIP:QWord;
var
  iLen:integer;
begin
  Result:=0;
  iLen:=System.Length(IPList);
  if iIPLcv>=iLen then
    iIPLcv:=0;
  if (iIPLcv<iLen) then begin
    Result:=IPList[iIPLcv];
    Inc(iIPLcv);
  end;
end;

procedure  TProviderManager.OnInitialize(RSRP:PRSR);
begin

end;

procedure  TProviderManager.OnFinalize(RSRP:PRSR);
var
  InfoP:PProviderSocketInfo;

  procedure PushFinalizeDNS;
  begin
    RSRP^.Info.DataP:=Nil;
    Done(InfoP^);
    Dispose(InfoP);
  end;

  procedure PushFinalizeProviderData;
  begin
    RSRP^.Info.DataP:=Nil;
    If (InfoP^.CacheP<>Nil) then 
      InfoP^.CacheP^.RequestP:=Nil;
    Done(InfoP^);
    Dispose(InfoP);
  end;

  procedure PushFinalizeCache;
  begin
    RSRP^.Info.DataP:=Nil;
    {$ifdef cpu64}
      InterlockedDecrement64(RSR_Stream_Count);
    {$else}
      Dec(RSR_Stream_Count);
    {$endif}
    Done(InfoP^);
    Dispose(InfoP);
  end;

begin
  InfoP:=RSRP^.Info.DataP;
  If InfoP<>Nil then begin
    Case InfoP^.Kind of
      pskProviderDNS          : PushFinalizeDNS;
      pskProviderData         : PushFinalizeProviderData;
      pskProviderEndUserCache : PushFinalizeCache;
    End;
  end;
end;

procedure  TProviderManager.OnDataReceived(RSRP:PRSR; var Handled:boolean);
var
  iLcv,iLastCount:Integer;
  iHeaderLength,iHeaderStart,iHeaderEnd,iHeaderLoc:Integer;
  sLastNextPage,sHeaderName,sHeaderValue,sLowerHeader,sHeader:Core.Strings.VarString;
  saHeaders:Core.Arrays.Types.VarString;
  bRedirect:Boolean;
  InfoP:PProviderSocketInfo;

  Function PushHeaderAdd(Var List:Core.Arrays.Types.KeyStrings):Integer;
  begin
    sHeader:=Trim(System.Copy(sHeader,iHeaderStart,(iHeaderEnd-iHeaderStart)+1));
    iHeaderEnd:=Pos(';',sHeader);
    sHeader:=System.Copy(sHeader,1,iHeaderEnd);
    iHeaderLength:=Length(sHeader);
    iHeaderEnd:=Pos('=',sHeader);
    sHeaderName:=System.Copy(sHeader,1,iHeaderEnd-1);
    sHeaderValue:=System.Copy(sHeader,iHeaderEnd+1,iHeaderLength-iHeaderEnd-1);
    Result:=Core.Arrays.KeyString.Add(List,sHeaderName,sHeaderValue);
  end;

begin
  Handled:=true;
  FRSRP:=RSRP; bRedirect:=False;
  InfoP:=RSRP^.Info.DataP;
  If (InfoP<>Nil) then begin
    If uGoogle.IsContentValid(RSRP^.RecvBuffer.Stream) then begin
      Inc(InfoP^.byLevelR);
      iHeaderLoc:=Core.Streams.Pos(RSRP^.RecvBuffer.Stream,#13#10#13#10);
      System.Pos(#13#10#13#10,sPage);
      If (iHeaderLoc>0) then begin
        sHeader:=Core.Streams.Extract(RSRP^.RecvBuffer.Stream,0,iHeaderLoc);
        Core.Arrays.VarString.fromString(saHeaders,sHeader,#13#10);
        Try
          For iLcv:=0 to High(saHeaders) do begin
            sHeader:=saHeaders[iLcv];
            sLowerHeader:=Lowercase(sHeader);
            iHeaderStart:=Pos('set-cookie:',sLowerHeader);
            If iHeaderStart>0 then begin
              iHeaderStart:=iHeaderStart+11;
              iHeaderEnd:=Length(sHeader);
              PushHeaderAdd(InfoP^.Cookies);
            end else begin
              iHeaderStart:=Pos('location:',sLowerHeader);
              If iHeaderStart>0 then begin
                bRedirect:=True;
                iHeaderStart:=iHeaderStart+9;
                iHeaderEnd:=Length(sHeader);
                InfoP^.NextPage:=Trim(System.Copy(sHeader,iHeaderStart,(iHeaderEnd-iHeaderStart)+1));
                /// Ok this is a redirect...  Check status... If Status is ok then that's good otherwise throw back results even if nothing...
                ///
              end;
            end;
          end;
        Finally
          SetLength(sHeader,0);
          SetLength(sHeaderName,0);
          SetLength(sHeaderValue,0);
          Empty(saHeaders);
        End;
      end;
      Inc(InfoP^.PageNumber);
      If Not bRedirect then begin
        sLastNextPage:=InfoP^.NextPage; iLastCount:=InfoP^.Total;
        Try
          uGoogle.ParseContent(InfoP,ProviderP^.MaxResults,InfoP^.Total,InfoP^.NextPage,sPage,@OnResultParsed,@OnProviderError);
        Except
          On E:Exception do OnException('uProvider.ParseContent','uGoogle.ParseContent',E.Message);
        End;
        If iLastCount<>InfoP^.Total then
          Results.DB.Update(Task,InfoP^.QueryResultsID,InfoP^.ResultSet.Results);
      end;
      SetLength(sPage,0);
      If (InfoP^.Total<ProviderP^.MaxResults) and (sLastNextPage<>InfoP^.NextPage) and (InfoP^.NextPage<>'') then begin
        InfoP^.Query:=InfoP^.NextPage;
        sHeader:=Concat(
          'GET ',InfoP^.Query,' HTTP/1.1',#13#10,
          'Host: ',RSRP^.Info.Server,#13#10,
          'Connection: Keep-Alive'#13#10
        );
        If Length(InfoP^.Cookies)>0 then
          sHeader:=Concat(sHeader,
            'Cookie: ',Core.Arrays.KeyString.toString(InfoP^.Cookies,Refactor,'=',''),#13#10
          );
        sHeader:=Concat(sHeader,
          'Accept-Language: en',#13#10,
          #13#10
        );
        Inc(InfoP^.byLevelQ);
        Send(
          RSRP,
          sHeader
        );
      end;
      if not (InfoP^.CacheP^.Sent or bRedirect) and ((InfoP^.CacheP^.Total=0) or (InfoP^.CacheP^.StreamResultIndex<InfoP^.CacheP^.Total)) then begin  // We are giving up...  Were these results Sent
        SendResultsBack(InfoP);
      end;
      RSR.Empty(RSRP^.RecvBuffer);
    end;
  end;
end;

(*
procedure  OnProviderSocket(AManager:Pointer; ItemP:PRSRAddInfo); stdcall;
var
  Manager:TProviderManager;
  iIPLen:Integer;
  InfoP:PProviderSocketInfo;
begin
  // The socket is now useable...
  Manager:=TProviderManager(AManager);
  Try
    InfoP:=ItemP^.DataP;
    InfoP^.SocketID:=ItemP^.Socket;
    SocketMap[ItemP^.Socket].sktOPS:=ItemP^.Socket;
    SocketMap[ItemP^.Socket].dwExpires:=Core.Timer.dwTick+Manager.TimeOut;
    iIPLen:=Length(Manager.IPList);
    If iIPLen>0 then begin
      If Manager.iIPLcv>=iIPLen then
        Manager.iIPLcv:=0;
      Manager.Connect(@SocketMap[ItemP^.Socket],Manager.IPList[Manager.iIPLcv],ItemP^.Port);
      Inc(Manager.iIPLcv);
    end else begin
      Manager.OnException('uProviders.OnProviderSocket','IPList',Format('There are no IPs for provider %s',[VarStringToString(Manager.ProviderP^.Domain)]));
    end;
  Except
    On E: Exception do Manager.OnException('uProviders.OnProviderSocket','After Assign',E.Message);
  End;
end;
*)
(*
procedure  OnUserQueryCacheSocket(AManager:Pointer; ItemP:PRSRAddInfo); stdcall;
var
  CacheInfoP:PProviderSocketInfo;
  InfoP:PProviderSocketInfo;
  Item:TRSRAddInfo;
  Manager:TProviderManager;
  iLcv:Integer;
  sQuery,sFormat:Core.Strings.VarString;
begin
  Manager:=AManager;
  CacheInfoP:=ItemP^.DataP;
  CacheInfoP^.SocketID:=ItemP^.Socket;
  InterlockedIncrement(RSR_Stream_Count);
  SocketMap[ItemP^.Socket].sktOPS:=ItemP^.Socket;
  SocketMap[ItemP^.Socket].dwExpires:=Core.Timer.dwTick+CACHE_TIMEOUT;
  If (CacheInfoP^.SenderP<>Nil) and (ManagerMap[CacheInfoP^.SenderP^.sktOPS]<>Nil) and (SocketMap[CacheInfoP^.SenderP^.sktOPS].sktOPS=CacheInfoP^.SenderP^.sktOPS) then
    CacheInfoP^.SenderP^.AddInfo.Cache:=ItemP^.Socket;  // Provide RSR Pointer with Cache Socket ID
  If CacheInfoP^.Total>0 then begin
    Manager.SendResultsBack(CacheInfoP);
  end else begin
    Empty(Item);
    New(InfoP);
    Try
      Empty(InfoP^);
      // Parent Child Member Setup
      InfoP^.CacheP:=CacheInfoP;
      CacheInfoP^.RequestP:=InfoP;

      sQuery:=SysUtils.StringReplace(CacheInfoP^.SearchTerm,' ','+',[rfReplaceAll]);
      sFormat:=ccUtils.VarStringToString(Manager.ProviderP^.QS_WEB);

      InfoP^.Query:=sysUtils.StringReplace(sFormat,'$search_term$',sQuery,[rfReplaceAll]);
      InfoP^.QueryID:=CacheInfoP^.QueryID;
      InfoP^.QueryResultsID:=CacheInfoP^.QueryResultsID;

      InfoP^.Total:=0;
      InfoP^.BindIP:=CacheInfoP^.BindIP;
      InfoP^.Max:=CacheInfoP^.Max;
      InfoP^.Requested:=CacheInfoP^.Requested;
      InfoP^.SenderP:=CacheInfoP^.SenderP;
      InfoP^.OutputFormat:=CacheInfoP^.OutputFormat;
      InfoP^.FirstReturnOn:=CacheInfoP^.FirstReturnOn;

      Item.Server:=VarStringToString(Manager.ProviderP^.Domain);
      Item.Port:=Manager.ProviderP^.PORT;
      Item.DataP:=InfoP;
      Item.BindIP:=CacheInfoP^.BindIP;
      Item.OnAdded:=OnProviderSocket;
      Item.Location:='uProviders.OnProviderSocket';
      Item.Kind:=Integer(pskProviderData);
      Manager.NewSocket(Item,AF_INET,SOCK_STREAM,IPPROTO_TCP,Nil,0,0,WM_SOCKETACTIVITY);
    Except
      CacheInfoP^.RequestP:=Nil;
      Dispose(InfoP);
    End;
  end;
end;
*)

procedure  TProviderManager.SubmitQuery(aBindIP:QWord; aBindPort:WORD; sQuery:Core.Strings.VarString; EUID:QWord; SenderP:PRSR; MaxResults,FirstReturnOn:Integer; Var OutputFormat:Core.Strings.VarString);
var
  QueryResultsID,QueryID:QWord;
  CacheInfoP:PProviderSocketInfo;
  CacheRSRP:PRSR;
  Manager:TProviderManager;

  procedure PushCacheAssignments;
  var
    iLcv:Integer;
  begin
    CacheInfoP^.QueryID:=QueryID;
    CacheInfoP^.QueryResultsID:=QueryResultsID;
    CacheInfoP^.EU_ID:= EUID; // PHTTP(SenderP^.AddInfo.DataP)^.UserID;
    CacheInfoP^.ResultSet.ProviderID:=ProviderP^.ID;
    CacheInfoP^.ResultSet.QueryID:=QueryID;
    CacheInfoP^.FirstReturnOn:=FirstReturnOn;
    CacheInfoP^.ResultSet.QueryID:=QueryID;
    CacheInfoP^.SenderP:=SenderP;
    CacheInfoP^.Max:=MaxResults;
    CacheInfoP^.Requested:=MaxResults;
    CacheInfoP^.RequestP:=CacheInfoP;
    CacheInfoP^.CacheP:=CacheInfoP;
    CacheInfoP^.SearchTerm:=sQuery;
    CacheInfoP^.OutputFormat:=OutputFormat;
    CacheInfoP^.BindIP:=aBindIP;
    CacheInfoP^.DNS_IP:=Storage.DNS.Native.GetNextHost(dnskRegular);
    CacheInfoP^.StreamResultIndex:=0;
    Interaction.Empty(CacheInfoP^.Interactions);
    CacheInfoP^.Total:=Store.DB.List(Task,CacheInfoP^.ResultSet.Results,CacheInfoP^.Results);
    SetLength(CacheInfoP^.Interactions,CacheInfoP^.Total);
    For iLcv:=0 to CacheInfoP^.Total-1 do
      Storage.SrchTmpUserInteractions.Interaction.DB.Retrieve(Task,CacheInfoP^.EU_ID,CacheInfoP^.ResultSet.Results[iLcv], CacheInfoP^.Interactions[iLcv]);
  end;
begin
  CacheInfoP:=Nil;
  // identify Query...
  QueryID:=Storage.SrchQueries.Items.DB.Find(Task,ProviderP^.ID, sQuery);
  If QueryID=0 then
    Storage.SrchQueries.Items.DB.Add(Task,QueryID,ProviderP^.ID,sQuery);
  If (QueryID<>0) then begin
    If (Length(IPList)=0) then begin
      SenderP^.DNS.Address.sin_addr.s_addr:=Storage.DNS.Native.GetNextHost(dnskRegular);
      DNSLookup(SenderP,ProviderP^.Domain,[dnsCache,dnsIP],SenderP^.DNS.Address.sin_addr.s_addr);
      PushHTTPError(SenderP,HTTP_SCS_PRECONDITIONFAILED,'Waiting for DNS entries to populate.');
    end else begin
      New(CacheInfoP);
      Empty(CacheInfoP^);
      QueryResultsID:=Storage.SrchQueries.Results.DB.Find(Task,ProviderP^.ID,QueryID,CacheInfoP^.ResultSet.Results);
      CacheInfoP^.QueryID:=QueryID;
      CacheInfoP^.QueryResultsID:=QueryResultsID;
      CacheInfoP^.Total:=Length(CacheInfoP^.ResultSet.Results);
      If (CacheInfoP^.Total=0) then begin
        Empty(CacheInfoP^);
        CacheInfoP^.RemoteIP:=GetNextProviderIP;
        PushCacheAssignments;
        CacheRSRP:=Queue(rsrsTCP,aBindIP,aBindPort,CacheInfoP);
        CacheInfoP^.SocketID:=CacheRSRP^.Info.Socket;
      end else begin
        SendResultsBack(CacheInfoP);
      end;
    end;
  end else begin
    PushHTTPError(SenderP,HTTP_SCS_SERVICEUNAVAILABLE,'DMBS is unavailable.');
  end;
end;

procedure  TProviderManager.ObtainMoreResults(SenderP:PRSR; CacheSocket:QWord; CurrentCount:Integer; QueryID,EUID:QWord); // Cancels the old query if not found...
var
  HTTPDP         : PHTTP;
  CacheRSRP      : PRSR;
  CacheInfoP     : PProviderSocketInfo;
  Manager        : TProviderManager;
  iLcv           : LongInt;
begin
  HTTPDP:=SenderP^.Info.DataP;
  {
  If (CacheSocket=0) and (HTTPDP^.CacheID<>0) then
    CacheSocket:=HTTPDP^.CacheID;
  }
  If (CacheSocket>0) and (CacheSocket<RSR_MAX_SOCKETS) and (MAN_MAP[CacheSocket]<>Nil) and (MAN_MAP[CacheSocket] is TProviderManager) then begin
    Manager:=TProviderManager(MAN_MAP[CacheSocket]);
    CacheRSRP:=RSR_MAP[CacheSocket];
    CacheRSRP^.dtExpires:=IncMillisecond(Core.Timer.dtNow,CACHE_TIMEOUT);
    CacheInfoP:=CacheRSRP^.Info.DataP;
    If (CacheInfoP<>nil) then begin
      If (EUID=0) and (CacheInfoP^.EU_ID<>EUID) and (HTTPDP<>Nil) and (HTTPDP^.UAP<>nil) then begin
        EUID:=HTTPDP^.UAP^.ID;
        CacheInfoP^.EU_ID:=EUID;
      end;
      If (CacheInfoP^.EU_ID=EUID) then begin
          If (CacheInfoP^.QueryID=QueryID) then begin
            CacheInfoP^.SenderP:=SenderP;
            CacheInfoP^.StreamResultIndex:=CurrentCount;
            If (CacheInfoP^.Total<CacheInfoP^.CacheP^.Total) then begin
              SetLength(CacheInfoP^.ResultSet.Results,CacheInfoP^.CacheP^.Total);
              SetLength(CacheInfoP^.Results,CacheInfoP^.CacheP^.Total);
              SetLength(CacheInfoP^.Interactions,CacheInfoP^.CacheP^.Total);
              For iLcv:=CurrentCount to CacheInfoP^.CacheP^.Total-1 do begin
                CacheInfoP^.ResultSet.Results[iLcv]:=CacheInfoP^.CacheP^.ResultSet.Results[iLcv];
                Storage.SrchResults.Store.Copy(CacheInfoP^.CacheP^.Results[iLcv],CacheInfoP^.Results[iLcv]);
                Storage.SrchTmpUserInteractions.Interaction.Copy(CacheInfoP^.CacheP^.Interactions[iLcv],CacheInfoP^.Interactions[iLcv]);
              end;
            end;
            For iLcv:=0 to CacheInfoP^.Total-1 do
              Storage.SrchTmpUserInteractions.Interaction.DB.Retrieve(Task,CacheInfoP^.EU_ID,CacheInfoP^.ResultSet.Results[iLcv], CacheInfoP^.Interactions[iLcv]);
            Manager.SendResultsBack(CacheInfoP);  // Probably will send no result packet
          end else begin
            //Push 404 Result Set is no longer available.
            Manager.PushHTTPError(SenderP,HTTP_SCS_SERVICEUNAVAILABLE,'Result set no longer available.');
          end;
      end else begin
        //Push 404 Result Set is no longer available.
        Manager.PushHTTPError(SenderP,HTPP_SCS_UNAUTHORIZED,'Credentials no longer valid.');
      end;
    end else begin
      Manager.PushHTTPError(SenderP,HTTP_SCS_SERVICEUNAVAILABLE,'Result set cache no longer available.');
    end;
  end else begin
    PushHTTPError(SenderP,HTTP_SCS_BADREQUEST,'Invalid session identifier.');
  end;
end;

procedure  TProviderManager.SendResultsBack(InfoP:PProviderSocketInfo);
var
  iPageLength,iPageStart:Integer;

  Function PushConvertInteractions(Index:Integer):Core.Strings.VarString;
  var
    iLength,iLcv:Integer;
  begin
    Result:='';
    For iLcv:=0 to High(InfoP^.Interactions[Index]) do
      Result:=Concat(
        Result,
          IntToStr(InfoP^.Interactions[Index][iLcv].Kind),#4,
          Encryption.Base64.Encode(InfoP^.Interactions[Index][iLcv].Data),
        #3
       );
    iLength:=Length(Result);
    If iLength>0 then
      SetLength(Result,iLength-1);  // Remove last #3
  end;

  procedure PushConvertResult(Index:Integer);
  begin
    Core.Streams.Write(
      Concat(
        IntToStr(InfoP^.Results[Index].ID),#2,
        InfoP^.Results[Index].Title,#2,
        InfoP^.Results[Index].Link,#2,
        InfoP^.Results[Index].Description,#2,
        PushConvertInteractions(Index),
        #1
      ),
      Refactor
    );
  end;

  procedure PushResultIteration;
  var
    iConverted,iCount,iLcv:Integer;
  begin
    iConverted:=0; iCount:=Length(InfoP^.Results); iLcv:=InfoP^.StreamResultIndex;
    While (iLcv<iCount) and (iConverted<InfoP^.Requested) do begin
      //if Not uStorage.IsResultDeleted(InfoP^.Interactions,iLcv) then begin
        PushConvertResult(iLcv);
        Inc(iConverted);
      //end;
      Inc(iLcv);
      Inc(InfoP^.StreamResultIndex);
    end;
  end;

  procedure WriteHeader;
  begin
    Refactor.Size:=0;
    Core.Streams.Write(
      Concat(
        IntToStr(ProviderP^.MaxResults),#2,
        IntToStr(InfoP^.Total),#2,
        IntToStr(InfoP^.QueryID),#2,
        IntToStr(InfoP^.EU_ID),#2,
        IntToStr(InfoP^.CacheP^.SocketID),#2,
        #1
      ),
      Refactor
    );
  end;


begin
  InfoP^.Sent:=True;
  WriteHeader;
  PushResultIteration;
  sPage:=StringReplace(InfoP^.OutputFormat,'{$i srs_page_output}',Core.Streams.toString(Refactor),[rfReplaceAll]);
  iPageStart:=System.Pos(#13#10#13#10,sPage);
  If iPageStart>0 then
    iPageLength:=Length(sPage)-iPageStart-4
  else
    iPageLength:=Refactor.Size;
  sPage:=StringReplace(spage,'{$i srs_page_length}',IntToStr(iPageLength),[rfReplaceAll]);
  If (InfoP^.SenderP<>Nil) then begin
    If (MAN_MAP[InfoP^.SenderP^.Info.Socket]<>nil) then begin
      MAN_MAP[InfoP^.SenderP^.Info.Socket].Send(InfoP^.SenderP,sPage);
    //ManagerMap[InfoP^.SenderP^.sktOPS].Close(InfoP^.SenderP);
    end else
      InfoP^.SenderP:=Nil;
  end;
  SetLength(sPage,0);
  Refactor.Size:=0;
end;

procedure TProviderManager.PushHTTPError(RSRP:PRSR; sCode,sMessage: Core.Strings.VarString);
begin
  sPage:=Concat(
    'HTTP/1.1 ',sCode,' ',sMessage,#13#10,
    'Date: ',HTTP_DateTime(Core.Timer.dtNow,True),#13#10,
    'Server: ',Server_Header,#13#10,
    'Content-Length: 0',#13#10,
    'Connection: Keeep-Alive',#13#10#13#10
  );
  MAN_MAP[RSRP^.Info.Socket].Send(RSRP,sPage);
  MAN_MAP[RSRP^.Info.Socket].Close(RSRP);
  SetLength(sPage,0);
end;


procedure TProviderManager.PushContentError(RSRP:PRSR; sContent:Core.Strings.VarString);
begin
  // Log to provider error log
end;

procedure TProviderManager.OnProviderError(InfoP:PProviderSocketInfo; Var sContent:Core.Strings.VarString);
begin

end;

procedure TProviderManager.OnResultParsed(InfoP:PProviderSocketInfo; Var Title,Link,Description:Core.Strings.VarString);
var
  iLcv:Integer;
begin
  SetLength(InfoP^.Results,InfoP^.Total+1);
  SetLength(InfoP^.Interactions,InfoP^.Total+1);
  SetLength(InfoP^.ResultSet.Results,InfoP^.Total+1);
  Try
    InfoP^.Results[InfoP^.Total].DateTime:=Core.Timer.dtNow;
    InfoP^.Results[InfoP^.Total].Title:=Title;
    InfoP^.Results[InfoP^.Total].Link:=Link;
    InfoP^.Results[InfoP^.Total].Description:=Description;
    Storage.SrchResults.Store.DB.IsResultInStore(Task,ProviderP^.ID, InfoP^.Results[InfoP^.Total].ID,InfoP^.Results[InfoP^.Total].DateTime,InfoP^.Results[InfoP^.Total].Title,InfoP^.Results[InfoP^.Total].Link,InfoP^.Results[InfoP^.Total].Description);
    InfoP^.ResultSet.Results[InfoP^.Total]:=InfoP^.Results[InfoP^.Total].ID;
    //uStorage.TempUserInteraction_Retrieve(Module,InfoP^.EU_ID,InfoP^.Results[InfoP^.Total].ID,InfoP^.Interactions[InfoP^.Total]);
  Finally
    Inc(InfoP^.Total);
  End;
  If (InfoP^.CacheP<>Nil) and (InfoP^.CacheP^.QueryID=InfoP^.QueryID) then begin
    InfoP^.CacheP^.Total:=InfoP^.Total;
    SetLength(InfoP^.CacheP^.Results,InfoP^.Total);
    Storage.SrchResults.Store.Copy(InfoP^.Results[InfoP^.Total-1],InfoP^.CacheP^.Results[InfoP^.Total-1]);
    SetLength(InfoP^.CacheP^.Interactions,InfoP^.Total);
    Storage.SrchTmpUserInteractions.Interaction.Copy(InfoP^.Interactions[InfoP^.Total-1],InfoP^.CacheP^.Interactions[InfoP^.Total-1]);
    SetLength(InfoP^.CacheP^.ResultSet.Results,InfoP^.Total);
    InfoP^.CacheP^.ResultSet.Results[InfoP^.Total-1]:=InfoP^.ResultSet.Results[InfoP^.Total-1];
    if (InfoP^.CacheP^.Total>0) and not (InfoP^.CacheP^.Sent) and (InfoP^.CacheP^.Total>=InfoP^.CacheP^.FirstReturnOn) then begin
      For iLcv:=InfoP^.CacheP^.StreamResultIndex to InfoP^.CacheP^.Total-1 do
        Storage.SrchTmpUserInteractions.Interaction.DB.Retrieve(Task,InfoP^.CacheP^.EU_ID,InfoP^.CacheP^.Results[iLcv].ID, InfoP^.CacheP^.Interactions[iLcv]);
      SendResultsBack(InfoP^.CacheP);
    end;
  end;
end;

end.

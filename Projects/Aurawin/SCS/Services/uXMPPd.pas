unit uXMPPd;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface

uses
  Classes,
  Sockets,

  RSR,
  RSR.DNS,
  RSR.XMPP,

  App.Build,
  App.Consts,

  Core.Strings,
  Core.Utils.Sockets,
  Core.Logging,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.LargeWord,

  Core.XML,
  Core.Timer,

  Core.Database,
  Core.Database.Types,
  Core.Database.Timer,

  Core.Generics,
  Core.Streams,
  Core.Arrays.VarString,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.Roster,
  Storage.VDM,
  Storage.KeepAlive,

  hXMPPd,
  SysUtils;
Const
   XMPP_STACK_SIZE=1024*20;
Type
  TXMPPRemoteSessionManager=Class;
  TXMPPNetwork=Class;
  PXMPPClientSession=^TXMPPClientSession;
  PXMPPServerSession=^TXMPPServerSession;
  TXMPPNetworks=specialize GObjectList<TXMPPNetwork>;
  TXMPPNetwork=class
  private
    iSessionMax        : LongInt;              // Scale.  Max number of Sessions to open
    iSessionThreshold  : LongInt;              // Max number of Users before opening another session.
    FDomain            : Core.Strings.VarString;
    IPs                : Core.Arrays.Types.LargeWord;          // Array of DNS Resolved IPs returned back from DNS Lookups...
    dtIPsExpire        : TDateTime;            // System Time whereby the IPs cached will be re-downloaded from DNS Server.
  public
    Constructor Create(aDomain:Core.Strings.VarString); reIntroduce;
    Destructor Destroy; override;
  public
    property Domain:Core.Strings.VarString read FDomain;
  end;

  TXMPPServerSession=Record
    StreamID           : Core.Strings.VarString;
    State              : TSessionStates;
    Network            : TXMPPNetwork;
    bIPLcv             : Byte;
  end;


  TXMPPClientSession=Record
    UAP                : Storage.UserAccounts.Items.PItem;
    State              : TSessionStates;
    Presence           : TPresence;
    StreamID           : Core.Strings.VarString;
    Resource           : Pointer;
  end;
  TXMPPClientSessionServer=Class;

  TXMPPClientSessionManager=Class(TRSRManager)
  private
    Owner                        : TXMPPClientSessionServer;
    FXMLParser                   : TXMLParser;

    Disco_Item_Functions         : TXMPPFunctions;
    UserAccounts                 : Storage.UserAccounts.Items.TList;

    FS_Query                     : Core.Strings.VarString;
    FS_Message                   : TMessage;
    FS_IQ                        : TIQ;
    FS_Presence                  : TPresence;

    procedure    PushStreamError(Error:TStreamError; Message:Core.Strings.VarString; RSRP:PRSR);
    procedure    PushSessionError(Error:TStanzaError; aKind:TErrorType; sMessage,sPre,sPost:Core.Strings.VarString; RSRP:PRSR);

    procedure    PushProcessStanzas(RSRP:PRSR);

    Function     Disco_Items_Root_Node_List(RSRP:Pointer; iStanzaIndex:LongInt):Boolean;
    Function     Disco_Items_UnImplemented(RSRP:Pointer; iStanzaIndex:LongInt):Boolean;

  protected
    procedure    OnError(RSRP:PRSR); override;
    procedure    OnDisconnect(RSRP:PRSR); override;
    procedure    OnConnect(RSRP:PRSR); override;
    procedure    OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;

    procedure    OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
    procedure    OnDNSResult(RSRP:PRSR); override;
    procedure    OnInitialize(RSRP:PRSR); override;
    procedure    OnFinalize(RSRP:PRSR); override;
  public
    Constructor  Create(AOwner:TXMPPClientSessionServer); reintroduce;
    Destructor   Destroy; override;
  end;

  TXMPPClientSessionServer=Class(TRSRServer)
  private
    FClusterID         : LongInt;
    RootDomain         : Storage.Domains.Items.TDomain;
    FPrivate_Conf      : TAgent;
    FPublic_Conf       : TAgent;
    FDirectory         : TAgent;
    FDomains           : Core.Arrays.Types.VarString;
    FManagers          : TRSRManagers;
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    Constructor Create(Const ADomain:ShortString; Const aIP:Int64; Const aPort,aScale:Word); ReIntroduce;
    Destructor  Destroy; override;
  end;

  TXMPPServerToServerServer=Class(TRSRServer)
  private
    FClusterID         : LongInt;
    FManagers          : TRSRManagers;
    RootDomain         : Storage.Domains.Items.TDomain;
    FDomains           : Core.Arrays.Types.VarString;
    FNetworks          : TXMPPNetworks;
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    Constructor Create(Const aDomain:ShortString; Const aIP:Int64; Const aPort,aScale:Word); ReIntroduce;
    Destructor  Destroy; override;
  end;

  TXMPPRemoteSessionManager=Class(TRSRManager)
  private
    FNetwork           : TXMPPNetwork;
    Owner              : TXMPPServerToServerServer;
    FXMLParser         : TXMLParser;

    FS_Query           : Core.Strings.VarString;
    FS_Message         : TMessage;
    FS_IQ              : TIQ;
    FS_Presence        : TPresence;
  private
    Function    FindNetwork(Domain:Core.Strings.VarString):TXMPPNetwork;
    Function    Aquire_Network(Domain:Core.Strings.VarString):TXMPPNetwork;
    procedure   AquireSessionConnection(Domain:Core.Strings.VarString);
  private
    procedure   PushStreamError(Error:TStreamError; Message:Core.Strings.VarString; RSRP:PRSR);
    procedure   PushSessionError(Error:TStanzaError; aKind:TErrorType; sMessage,sPre,sPost:Core.Strings.VarString; RSRP:PRSR);
    procedure   PushAquireStream(RSRP:PRSR);
    procedure   PushProcessStanzas(RSRP:PRSR);
  protected
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  public
    Constructor Create(AOwner:TXMPPServerToServerServer); reintroduce;
    Destructor  Destroy; override;
  end;

  procedure Empty(var Item:TXMPPServerSession); overload;
  procedure Init(var Item:TXMPPServerSession); overload;
  procedure Done(var Item:TXMPPServerSession); overload;

implementation
uses DateUtils;

Procedure  Empty(var Item:TXMPPServerSession);
begin
  With Item do begin
    SetLength(StreamID,0);
    State:=[];
    Network:=Nil;
    bIPLcv:=0;
  end;
end;

Procedure  Done(Var Item:TXMPPServerSession);
begin
  With Item do begin
    Finalize(StreamID);
    Network:=Nil;
    State:=[];
  end;
  Finalize(Item);
end;

procedure Init(Var Item:TXMPPServerSession);
begin
  Empty(Item);
end;

Constructor TXMPPNetwork.Create(aDomain:Core.Strings.VarString);
begin
  FDomain:=aDomain;
  Core.Arrays.LargeWord.Init(IPs);
  iSessionMax:=0;          // Scale.  Max number of Sessions to open
  iSessionThreshold:=100;  // Max number of Users before opening another session.
  Inherited Create;
end;

Destructor TXMPPNetwork.Destroy;
begin
  Core.Arrays.LargeWord.Done(IPs);
  Inherited Destroy;
end;

Constructor   TXMPPClientSessionServer.Create(Const ADomain:ShortString; Const aIP:Int64; Const aPort,aScale:Word);
var
  iCount,iLcv:LongInt;
begin
  Inherited Create(@FManagers,tTCP,aIP,aPort,aScale,SSL_OFF,CREATE_SUSPENDED);

  FService:=SERVICE_XMPP;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Client Session Server'));

  RootDomain.Name:=ADomain;

  Storage.Domains.Items.DB.Fill(Task,RootDomain);
  Storage.Domains.Items.DB.List(Task,FDomains);

  FreeOnTerminate:=True;

  FPrivate_Conf.ID:=Concat(_Private,'.',ADomain);
  FPrivate_Conf.Name:='Private Conferencing';
  FPrivate_Conf.Service:=_Private;
  FPrivate_Conf.Description:='Private Conferencing Rooms';
  FPrivate_Conf.Features:='';

  FPublic_Conf.ID:=Concat(_Public,'.',ADomain);
  FPublic_Conf.Name:='Public Conferencing';
  FPublic_Conf.Service:=_Public;
  FPublic_Conf.Description:='Public Conferencing Rooms';
  FPublic_Conf.Features:='';

  FDirectory.ID:=Concat(_Directory,'.',ADomain);
  FDirectory.Name:='System User Directory';
  FDirectory.Service:=_Directory;
  FDirectory.Description:='System User Directory';



  iCount:=aScale;
  iLcv:=0;
  While iLcv<iCount do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TXMPPClientSessionManager.Create(Self);
    Except
      iLcv:=iCount;
    end;
    Inc(iLcv);
  end;
end;

Destructor    TXMPPClientSessionServer.Destroy;
begin
  RSR.Done(FManagers);
  Storage.Domains.Items.Done(RootDomain);
  Core.Arrays.VarString.Done(FDomains);
  Done(FPrivate_Conf);
  Done(FPublic_Conf);
  Done(FDirectory);
  Inherited Destroy;
end;

procedure   TXMPPClientSessionServer.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure   TXMPPClientSessionServer.OnError(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  //Core.Logging.Native.WriteLogEntry(RootDomainP^.Domain,Concat(sProcedure,'.',sLocation,':',sError));
end;

Constructor   TXMPPClientSessionManager.Create(AOwner:TXMPPClientSessionServer);
begin
  FService:=SERVICE_XMPP;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Client Session Manager'));

  Owner:=AOwner;
  UserAccounts:=Storage.UserAccounts.Items.TList.Create(@Owner.RootDomain,FService);
  FXMLParser:=TXMLParser.Create;
  RSR.XMPP.Init(FXMLParser);
  Inherited Create(aOwner,@AOwner.FSSLInfo,SSL_OFF,THREAD_METHODS_OFF,XMPP_STACK_SIZE);

  RSR.XMPP.AddFunctionToList(Owner.RootDomain.Name,@Disco_Items_Root_Node_List,Disco_Item_Functions);

  TimeOut:=600000*10;
end;

Destructor   TXMPPClientSessionManager.Destroy;
begin
  Empty(Disco_Item_Functions);
  FreeAndNil(UserAccounts);
  FreeAndNil(FXMLParser);

  Inherited Destroy;
end;

procedure    TXMPPClientSessionManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure    TXMPPClientSessionManager.OnConnect(RSRP:PRSR);
begin

end;

procedure    TXMPPClientSessionManager.OnDisconnect(RSRP:PRSR);
begin

end;

procedure    TXMPPClientSessionManager.OnError(RSRP:PRSR);
begin
end;

procedure    TXMPPClientSessionManager.OnDNSResult(RSRP:PRSR);
begin
  // I have no idea...
end;

Function     TXMPPClientSessionManager.Disco_Items_UnImplemented(RSRP:Pointer; iStanzaIndex:LongInt):Boolean;
var
  sResponse:Core.Strings.VarString;
begin
  sResponse:=Concat('<iq type="result" from="',FS_IQ.sTO,'" to="',FS_IQ.sFrom,'" id="',FS_IQ.sID,'"><query xmlns="',NS_DISCO_ITEMS,'"/></iq>');
end;

Function     TXMPPClientSessionManager.Disco_Items_Root_Node_List(RSRP:Pointer; iStanzaIndex:LongInt):Boolean;
var
  sResponse:Core.Strings.VarString;
begin
  // Just return all nodes for this domain...
  // i.e. developer.aurawin.com
  // Or don't return anything...
  sResponse:=Concat(
    '<iq type="result" from="',FS_IQ.sTo,'" to="',FS_IQ.sFrom,'" id="',FS_IQ.sID,'"><query xmlns="',NS_DISCO_ITEMS,'">',
      '<item jid="conferencing.',FS_IQ.sTo,'" name="',Owner.RootDomain.FriendlyName,' Conference Rooms"/>',
    '</iq>'
  );
  Result:=True;
  Send(RSRP,sResponse);

  {
     <iq type='result' from='confence.aura.com' to='$from' id='$id'>
      <query xmlns='disco-items>
         <item jid='conference.developer.aurawin.com' name='Aurawin Conference Rooms'/>
         <item jid='streams.developer.aurawin.com' name='Aurawin Streams'/>'
      </query>
     </iq>
  }
end;

procedure    TXMPPClientSessionManager.OnDataReceived(RSRP:PRSR; var Handled:boolean);
var
  iLoc:LongInt;
  sXML:Core.Strings.VarString;
begin
  Handled:=true;
  if RSR.EndOf(RSRP^.RecvBuffer,'>') then begin
    sXML:=Core.Streams.toString(RSRP^.RecvBuffer.Stream);
    FXMLParser.Parse(sXML);
    PushProcessStanzas(RSRP);
    Empty(RSRP^.RecvBuffer);
  end;
end;

procedure    TXMPPClientSessionManager.PushProcessStanzas(RSRP:PRSR);
var
  csDataP   : PXMPPClientSession;
  RcP       : Resources.PResource;
  iLcv      : LongInt;
  saData    : Core.Arrays.Types.VarString;
  sResponse : Core.Strings.VarString;

  procedure PushSetUserAccount;
  var
    sPassword,sUserName:Core.Strings.VarString;
  begin
    If (ssAuth in csDataP^.State) then begin
      PushSessionError(szeForbidden,etCancel,'You are already authorized.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
    end else begin
      sUserName:=Lowercase(FXMLParser.Tag(iLcv,'username'));
      csDataP^.UAP:=UserAccounts.Acquire(sUserName);
      If (csDataP^.UAP<>Nil) then begin
        If Storage.UserAccounts.Items.DB.Fill_Auth(Task,csDataP^.UAP^) then begin
          If (csDataP^.UAP^.LockoutCount<=Max_LockCount) or (MinutesBetween(csDataP^.UAP^.LastAccessed,Core.Timer.dtUT)>=2) then begin
            csDataP^.UAP^.LockoutCount:=0;
            csDataP^.UAP^.LastAccessed:=Core.Timer.dtUT;
            if GetSHAPassword(csDataP^.StreamID,csDataP^.UAP^.Password)=FXMLParser.Tag(iLcv,'digest') then begin
              Include(csDataP^.State,ssAuth);
              Resources.PResource(csDataP^.Resource)^.Name:=FXMLParser.Tag(iLcv,'resource');
              sResponse:=Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="result"/>');
              Send(RSRP,sResponse);
            end else begin
              Inc(csDataP^.UAP^.LockoutCount);
              PushSessionError(szeNotAuthorized,etAuth,'Password Incorrect.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
            end;
          end else
            PushSessionError(szeNotAuthorized,etAuth,'Too many failed login attempts.  Please wait a few minutes before trying again.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
        end else
          PushSessionError(szeNotAuthorized,etAuth,'Authicantion Lookup Service Failed.  Please wait a few minutes before trying again.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
      end else begin
        PushSessionError(szeSubscriptionRequired,etAuth,'Username Not Found.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
      end;
    end;
  end;

  procedure PushLookForUserAccount;
  var
    LocalP:Storage.UserAccounts.Items.PItem;
    sUserName:Core.Strings.VarString;
  begin
    If (ssAuth in csDataP^.State) then begin
      PushSessionError(szeForbidden,etCancel,'You are already authorized.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
    end else begin
      sUserName:=Lowercase(FXMLParser.Tag(iLcv,'username'));
      LocalP:=UserAccounts.Acquire(sUserName);
      If LocalP<>Nil then begin
        sResponse:=Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="result"><query xmlns="',NS_AUTH,'"><username>',sUsername,'</username><password/><digest/><resource/></query></iq>');
        Send(RSRP,sResponse);
      end else begin
        PushSessionError(szeSubscriptionRequired,etAuth,'Username Not Found.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
      end;
    end;
  end;

  procedure PushSendDiscoInfo;
  begin
  end;

  procedure PushSendAgents;
  begin
    If (ssAuth in csDataP^.State) then begin
      sResponse:=Concat(
        '<iq ',Clean_ID_Tag(FS_IQ.sID),'type="result" from="',Owner.RootDomain.Name,'"><query xmlns="',NS_AGENTS,'">',
        hXMPPD.AgentToXML(Owner.FPrivate_Conf),
        hXMPPD.AgentToXML(Owner.FPublic_Conf),
        hXMPPD.AgentToXML(Owner.FDirectory),
        '</query></iq>'
      );
      Send(RSRP,sResponse);
    end else
      PushSessionError(szeNotAuthorized,etAuth,'You are not logged in.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
  end;

  procedure PushSendRosterItems;
  var
    iLcv:LongInt;
    StorageP:Storage.Roster.Items.PList;
  begin
    StorageP:=csDataP^.UAP^.Roster;
    If StorageP=Nil then begin
      New(StorageP);
      Storage.Roster.Items.Init(StorageP^);
      csDataP^.UAP^.Roster:=StorageP;
    end;
    If (ssAuth in csDataP^.State) then begin
      If Storage.Roster.Items.DB.Fill(Task,Owner.RootDomain.ID,csDataP^.UAP^.ID,RcP^.ID,csDataP^.UAP^.Roster) then begin
        sResponse:=Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="result" from="',csDataP^.UAP^.User,'@',Owner.RootDomain.Name,'/',RcP^.Name,'"><query xmlns="',NS_JR,'">');
        Send(RSRP,sResponse);
        For iLcv:=0 to High(StorageP^) do With StorageP^[iLcv]^ do begin
          sResponse:=Concat('<item jid="',Text1,'" name="',NickName,'" First="',FirstName,'" Last="',LastName,'" subscription="',SubscriptionToString(TSubscription(Subscription)),'"><group>',Folder,'</group></item>');
          Send(RSRP,sResponse);
        end;
        sResponse:='</query></iq>';
        Send(RSRP,sResponse);
      end else
        PushSessionError(szeInternalServerError,etContinue,'Unable to retrieve roster.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
    end else
      PushSessionError(szeNotAuthorized,etAuth,'You are not logged in.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
  end;

  procedure PushSetRosterItems(iStanza:LongInt);
  var
    iLcv,iIndex:LongInt;
    RI:Storage.Roster.Items.Item;
    StorageP:Storage.Roster.Items.PList;
    RcP:Resources.PResource;
  begin
    StorageP:=csDataP^.UAP^.Roster;
    If (ssAuth in csDataP^.State) then begin
      For iLcv:=0 to High(FXMLParser.Stanzas[iStanza]^.Tags.Items) do begin
        If FXMLParser.Stanzas[iStanza]^.Tags.Items[iLcv]^.Name='item' then begin
          Try
            RI.Folder:=FXMLParser.Tag(iStanza,iLcv,Length(FXMLParser.Stanzas[iStanza]^.Tags.Items),'group');
            RI.Text1:=FXMLParser.TagParameter(iStanza,iLcv,'jid');
            RI.NickName:=FXMLParser.TagParameter(iStanza,iLcv,'name');
            RI.FirstName:=FXMLParser.TagParameter(iStanza,iLcv,'first');
            RI.LastName:=FXMLParser.TagParameter(iStanza,iLcv,'last');
            RI.ResourceID:=RcP^.ID;
            RI.UserID:=csDataP^.UAP^.ID;
            RI.DomainID:=csDataP^.UAP^.DomainID;
            RI.Subscription:=LongInt(pSubscribe);
            iIndex:=Storage.Roster.Items.IndexOf(StorageP^,RI.Text1);
            If iIndex=-1 then begin
              If Storage.Roster.Items.DB.Add(Task,csDataP^.UAP^.DomainID,csDataP^.UAP^.ID,Storage.Roster.Items.Defaults.NoAccount,RI) then begin
                // Now we need to dispatch a query to determine if member is subscribed on the other end.
              end else
                PushSessionError(szeInternalServerError,etContinue,'Unable to add roster item.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
            end else begin
              Storage.Roster.Items.Copy(RI,StorageP^[iIndex]^);
              If Not Storage.Roster.Items.DB.Update(Task,StorageP^[iIndex]^) then
                PushSessionError(szeInternalServerError,etContinue,'Unable to update roster item.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
            end;
          Finally
            Storage.Roster.Items.Done(RI);
          end;
        end;
      end;
    end else
      PushSessionError(szeNotAuthorized,etAuth,'You are not logged in.',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
  end;

  procedure PushPresence(iStanza:LongInt);
  var
    saData   : Core.Arrays.Types.VarString;
    PProbe   : TPresence;
  begin
    If FS_Presence.sTo='' then begin
      // This is to the entire roster list
      PProbe:=FS_Presence;
      PProbe.Kind:=RSR.XMPP.pProbe;
    end else begin
      // Send to just this one JID.
      Core.Arrays.VarString.fromString(saData,FS_Presence.sTo,'/');
      Try
        If Length(saData)=2 then begin
          FS_Presence.sTo:=saData[0];
          FS_Presence.sToResource:=saData[1];
        end;
      Finally
        SetLength(saData,0);
      end;
      Core.Arrays.VarString.fromString(saData,FS_Presence.sTo,'@');
      Try
        If Length(saData)=2 then begin
          If SameText(saData[1],Owner.RootDomain.Name) then begin

          end else If Find(Owner.FDomains,saData[1]) then begin
            // Look for nodal information and re-transmit presence stanza to that node.
          end else begin
            //  Send Presence Info to Outside Domain
          end;
        end else begin
          PushSessionError(szeJIDMalFormed,etModify,'Malformed to field.',Concat('<presence ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</presence>',RSRP);
        end;
      Finally
        SetLength(saData,0);
      end;
    end;

  end;
  procedure PushSetupStream;
  var
    sNS:Core.Strings.VarString;
    sTo:Core.Strings.VarString;
    sVersion:Core.Strings.VarString;
  begin
    If Not (ssStream in csDataP^.State) then begin
      sNS:=FXMLParser.Parameter(0,'xmlns');
      sTo:=Lowercase(FXMLParser.Parameter(0,'to'));
      sVersion:=Lowercase(FXMLParser.Parameter(0,'version'));
      If (sNS=NS_Client) then begin
        If SameText(sTo,Owner.RootDomain.Name) then begin
          Inc(RSR.XMPP.Sessions_Lcv);
          csDataP^.StreamID:=RSR.XMPP.GenerateStreamID;
          sResponse:=Concat('<',Stream_Stream,' xmlns="',NS_Client,'" id="',csDataP^.StreamID,'" from="',sTo,'" xmlns:stream="',NS_Streams,'">');
          Send(RSRP,sResponse);
          If sVersion<>'' then begin
            sResponse:=Concat('<',Stream_Features,'><mechanisms xmlns="',NS_SASL,'"><mechanism>DIGEST-MD5</mechanism><mechanism>PLAIN</mechanism></mechanisms>');
            Send(RSRP,sResponse);
          end;
          Include(csDataP^.State,ssStream);
        end else
          PushStreamError(seSeeOtherHost,'Cannot establish session stream to server other than this domain.',RSRP);
      end else
        PushStreamError(seInvalidNamespace,'Namespace not acceptable in this stanza.',RSRP);
    end else
      PushStreamError(seConflict,'Stream already established.',RSRP);
  end;
  procedure PushCancelMessage;
  begin
    sResponse:=Concat('<message from="',FS_Message.sTo,'" to="',FS_Message.sFrom,'" ',Clean_ID_Tag(FS_IQ.sID),'type="error"><body>',FS_Message.sBody,'</body>',hXMPPD.ERROR_CANCEL,'</message>');
    Send(RSRP,sResponse);
  end;

  Procedure PushMessage;
  var
    saData:Core.Arrays.Types.VarString;
    UserP:Storage.UserAccounts.Items.PItem;
  begin
    FS_Message.sTo:=Lowercase(FXMLParser.Parameter(0,'to'));
    FS_Message.sFrom:=Lowercase(FXMLParser.Parameter(0,'from'));
    If (FS_Message.sFrom='') then begin
      FS_Message.sFrom:=Concat(csDataP^.UAP^.User,'@',Owner.RootDomain.Name);
      FS_Message.sFromResource:=RcP^.Name;
    end;
    If FS_Message.sFromResource='' then
      FS_Message.sFromResource:=RcP^.Name;

    FS_Message.Kind:=RSR.XMPP.MessageKindFromString(Lowercase(FXMLParser.Parameter(0,'type')));
    FS_Message.sSubject:=FXMLParser.Tag(iLcv,'subject');
    FS_Message.sThread:=FXMLParser.Tag(iLcv,'thread');
    FS_Message.sBody:=FXMLParser.Tag(iLcv,'body');
    FS_Message.Stamp:=Core.Timer.dtNow;
    Core.Arrays.VarString.fromString(saData,FS_Message.sTo,'/');
    Try
      If Length(saData)=2 then begin
        FS_Message.sTo:=saData[0];
        FS_Message.sToResource:=saData[1];
      end;
    Finally
      SetLength(saData,0);
    end;
    // Check to see if user in on this system...
    Core.Arrays.VarString.fromString(saData,FS_Message.sTo,'@');
    Try
      If Length(saData)=2 then begin
        If SameText(saData[1],Owner.RootDomain.Name) then begin
          // This domain is local
          UserP:=UserAccounts.Acquire(saData[0]);
          If csDataP^.UAP=UserP then begin
            sResponse:=MessageToString(FS_Message);
            Send(RSRP,sResponse);
          end else If Storage.UserAccounts.Items.DB.Exists(Task,UserP^) then begin
            // Push Drop Message in Storage?  Or relay to other node?
          end else begin
            PushCancelMessage;
          end;
        end else If Find(Owner.FDomains,saData[1]) then begin


        end else begin
          PushCancelMessage;
        end;
      end else begin
        PushStreamError(seInvalidID,'Invalid To Field.',RSRP);
      end;
    Finally
      SetLength(saData,0);
    end;
  end;

  procedure PushProcessDisco_Items(iStanzaIndex:LongInt);
  var
    iFunctionIndex:LongInt;
  begin
    iFunctionIndex:=RSR.XMPP.IndexOfFunctionData(FS_IQ.sTo,Disco_Item_Functions);
    If iFunctionIndex=-1 then
      Disco_Items_UnImplemented(RSRP,iStanzaIndex)
    else
      Disco_Item_Functions[iFunctionIndex].Execute(RSRP,iStanzaIndex);
  end;
  
begin
  csDataP:=RSRP^.Info.DataP;
  RcP:=csDataP^.Resource;
  For iLcv:=0 to High(FXMLParser.Stanzas) do begin
    If FXMLParser.Stanzas[iLcv]^.Tags.Items[0]^.Name=Stream_Stream then begin
      PushSetupStream;
    end else If FXMLParser.Stanzas[iLcv]^.Tags.Items[0]^.Name='iq' then begin
      FS_IQ.iqType:=RSR.XMPP.IQTypeFromString(FXMLParser.Parameter(iLcv,'type'));
      FS_IQ.sID:=FXMLParser.Parameter(iLcv,'id');
      FS_IQ.sFrom:=FXMLParser.Parameter(iLcv,'from');
      FS_IQ.sTo:=FXMLParser.Parameter(iLcv,'to');
    end else if (FXMLParser.Stanzas[iLcv]^.Tags.Items[0]^.Name='presence') then begin
      FS_Presence.sTo:=FXMLParser.Parameter(iLcv,'to');
      FS_Presence.sFrom:=FXMLParser.Parameter(iLcv,'from');
      Core.Arrays.VarString.fromString(saData,FS_Presence.sFrom,'/');
      Try
        If Length(saData)=2 then
          FS_Presence.sFrom:=saData[0];
        FS_Presence.sFromResource:=RcP^.Name;
      Finally
        SetLength(saData,0);
      end;
      FS_Presence.sMessage:=FXMLParser.Tag(iLcv,'status');
      FS_Presence.Kind:=RSR.XMPP.PresenceKindFromString(FXMLParser.Parameter(iLcv,'type'));
      PushPresence(iLcv);
    end else if (FXMLParser.Stanzas[iLcv]^.Tags.Items[0]^.Name='query') then begin
      // Check Namespace for command
      FS_Query:=FXMLParser.Parameter(iLcv,'xmlns');
      If FS_Query=NS_AUTH then begin
        Case FS_IQ.iqType of
          iqGet: PushLookForUserAccount;
          iqSet: PushSetUserAccount;
        else
          PushSessionError(szeFeatureNotImplemented,etCancel,'Unknown/Invalid IQ type',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
        end;
      end else if FS_Query=NS_AGENTS then begin
        PushSendAgents;
      end else if FS_Query=NS_JR then begin
        Case FS_IQ.iqType of
          iqGet: PushSendRosterItems;
          iqSet: PushSetRosterItems(iLcv);
        else
          PushSessionError(szeFeatureNotImplemented,etCancel,'Unknown/Invalid IQ type',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
        end;
      end else if FS_Query=NS_DISCO_ITEMS then begin
        PushProcessDisco_Items(iLcv);
      end else if FS_Query=NS_DISCO_INFO then begin

      end else
        PushSessionError(szeFeatureNotImplemented,etCancel,'Unimplemented Query type',Concat('<iq ',Clean_ID_Tag(FS_IQ.sID),'type="error">'),'</iq>',RSRP);
    end else if (FXMLParser.Stanzas[iLcv]^.Tags.Items[0]^.Name='message') then begin
      PushMessage;
    end;
  end;
end;

procedure    TXMPPClientSessionManager.OnInitialize(RSRP:PRSR);
var
 CSDataP:PXMPPClientSession;
 RcP:Resources.PResource;
begin
  New(CSDataP);
  With csDataP^ do begin
    UAP:=Nil;
    State:=[];
    SetLength(StreamID,0);
    New(RcP);
    Resources.Init(RcP^);
    Resource:=RcP;
  end;
  RSRP^.Info.DataP:=CSDataP;
  {$ifdef cpu64}
    InterlockedIncrement64(RSR_Stream_Count);
  {$else}
    Inc(RSR_Stream_Count);
  {$endif}
end;

procedure    TXMPPClientSessionManager.OnFinalize(RSRP:PRSR);
Var
  StorageP : Storage.Roster.Items.PList;
  CSDataP  : PXMPPClientSession;
  RcP      : Resources.PResource;
begin
  {$ifdef cpu64}
    InterlockedDecrement64(RSR_Stream_Count);
  {$else}
    Dec(RSR_Stream_Count);
  {$endif}
  csDataP:=RSRP^.Info.DataP;
  If (csDataP<>Nil) then begin
    With csDataP^ do begin
      If (UAP<>Nil) then begin
        StorageP:=csDataP^.UAP^.Roster;
        If StorageP<>Nil then begin
          Storage.Roster.Items.Done(StorageP^);
          Dispose(StorageP);
          csDataP^.UAP^.Roster:=Nil;
        end;
      end;
      UAP:=Nil;
      State:=[];
      SetLength(StreamID,0);
      RcP:=Resource;
      Resources.Done(RcP^);
      Dispose(RcP);
      Resource:=nil;
    end;
    Dispose(csDataP);
    RSRP^.Info.DataP:=nil;
  end;
end;

procedure    TXMPPClientSessionManager.PushStreamError(Error:TStreamError; Message:Core.Strings.VarString; RSRP:PRSR);
var
  sResponse:Core.Strings.VarString;
  CSDataP:PXMPPClientSession;
begin
  csDataP:=RSRP^.Info.DataP;
  if (csDataP<>Nil) then begin
    sResponse:=Concat('<',Stream_Error,'><',StreamErrorMessages[Error],' xmlns="',NS_Streams,'"/>',
      '<text xml:lang="en" xmlns="',NS_Streams,'">',Message,'</text>',
      '</',Stream_Error,'></',Stream_Stream,'>'
    );
    Send(RSRP,sResponse);
    Include(csDataP^.State,ssClose);
  end;
  //Close(FRSRP);
end;

procedure    TXMPPClientSessionManager.PushSessionError(Error:TStanzaError; aKind:TErrorType; sMessage,sPre,sPost:Core.Strings.VarString; RSRP:PRSR);
var
  sResponse:Core.Strings.VarString;
begin
  sResponse:=Concat(sPre,'<error type="',SessionErrors[aKind],'"><',StanzaErrorMessages[Error],' xmlns="',NS_Stanzas,'"/>',
    '<text xml:lang="en" xmlns="',NS_Stanzas,'">',sMessage,'</text>','</error>',sPost
  );
  Send(RSRP,sResponse);
end;


Constructor  TXMPPServerToServerServer.Create(Const aDomain:ShortString; Const aIP:Int64; Const aPort,aScale:Word);
var
  iCount  : LongInt;
  iLcv    : LongInt;
begin
  Inherited Create(@FManagers,tTCP,aIP,aPort,aScale,SSL_OFF,CREATE_SUSPENDED);

  FService:=SERVICE_XMPPSTOS;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Server To Server'));

  RootDomain.Name:=ADomain;
  Storage.Domains.Items.DB.Fill(Task,RootDomain);
  Storage.Domains.Items.DB.List(Task,FDomains);

  iCount:=aScale;

  iLcv:=0;
  While iLcv<iCount do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TXMPPRemoteSessionManager.Create(Self);
    Except
      iLcv:=iCount;
    end;
    Inc(iLcv);
  end;

end;

Destructor  TXMPPServerToServerServer.Destroy;
begin
  Core.Arrays.VarString.Done(FDomains);
  Inherited Destroy;
end;

procedure   TXMPPServerToServerServer.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


procedure   TXMPPServerToServerServer.OnError(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
end;

Constructor   TXMPPRemoteSessionManager.Create(AOwner:TXMPPServerToServerServer);
begin
  FService:=SERVICE_XMPPSTOS;
  Owner:=AOwner;
  FXMLParser:=TXMLParser.Create;
  RSR.XMPP.Init(FXMLParser);
  Inherited Create(AOwner,@AOwner.FSSLInfo,SSL_OFF,THREAD_METHODS_OFF,XMPP_STACK_SIZE);

  TimeOut:=600000*60;
end;

Destructor   TXMPPRemoteSessionManager.Destroy;
begin
  FreeAndNil(FXMLParser);
  Inherited Destroy;
end;

Function   TXMPPRemoteSessionManager.Aquire_Network(Domain:Core.Strings.VarString):TXMPPNetwork;
var
  iLen    : LongInt;
  RSRP    : PRSR;
  ssDataP : PXMPPServerSession;
begin
  Result:=FindNetwork(Domain);
  If Result=Nil then begin
    Result:=TXMPPNetwork.Create(Domain);
    Owner.FNetworks.Add(Result);
  end;
end;

Function    TXMPPRemoteSessionManager.FindNetwork(Domain:Core.Strings.VarString):TXMPPNetwork;
var
  iLen,iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to Owner.FNetworks.Count-1 do begin
    if SameText(Owner.FNetworks.Items[iLcv].FDomain,Domain) then begin
      Result:=Owner.FNetworks.Items[iLcv];
      break;
    end;
  end;
end;

procedure    TXMPPRemoteSessionManager.AquireSessionConnection(Domain:Core.Strings.VarString);
var
  Network:TXMPPNetwork;
begin
  Network:=Aquire_Network(Domain);
  {
  If Length(NetworkP^.IPs)=0 then begin
    ServerSessionMap[NetworkP^.Sessions[0]^.SktOps].State:=[ssResolveDNS];
    DNSLookup(NetworkP^.Sessions[0],Domain,[dnsCache,dnsIP],GetNextDNSServer);
  end else With ServerSessionMap[NetworkP^.Sessions[0]^.SktOps] do begin
    State:=[ssAquireStream];
    Connect(NetworkP^.Sessions[0],NetworkP^.IPs[bIPLcv],5269);
  end;
  }
end;


procedure    TXMPPRemoteSessionManager.PushStreamError(Error:TStreamError; Message:Core.Strings.VarString; RSRP:PRSR);
var
  sResponse:Core.Strings.VarString;
  SessionP:PXMPPServerSession;
begin
  SessionP:=RSRP^.Info.DataP;
  If (SessionP<>Nil) then begin
    sResponse:=Concat('<',Stream_Error,'><',StreamErrorMessages[Error],' xmlns="',NS_Streams,'"/>',
      '<text xml:lang="en" xmlns="',NS_Streams,'">',Message,'</text>',
      '</',Stream_Error,'></',Stream_Stream,'>'
    );
    Send(RSRP,sResponse);
    Include(SessionP^.State,ssClose);
  end;
  //Close(FRSRP);
end;

procedure    TXMPPRemoteSessionManager.PushSessionError(Error:TStanzaError; aKind:TErrorType; sMessage,sPre,sPost:Core.Strings.VarString; RSRP:PRSR);
var
  sResponse:Core.Strings.VarString;
begin
  sResponse:=Concat(sPre,'<error type="',SessionErrors[aKind],'"><',StanzaErrorMessages[Error],' xmlns="',NS_Stanzas,'"/>',
    '<text xml:lang="en" xmlns="',NS_Stanzas,'">',sMessage,'</text>','</error>',sPost
  );
  Send(RSRP,sResponse);
end;

procedure    TXMPPRemoteSessionManager.PushAquireStream(RSRP:PRSR);
var
  sResponse:Core.Strings.VarString;
begin
  sResponse:=Concat('<',Stream_Stream,' to="',FNetwork.Domain,'" xmlns="',NS_Server,'" xmlns:stream="',NS_Streams,'">');
  Send(RSRP,sResponse);
end;

procedure    TXMPPRemoteSessionManager.PushProcessStanzas(RSRP:PRSR);
var
  iLcv:LongInt;
  sResponse:Core.Strings.VarString;
  UAP:Storage.UserAccounts.Items.PItem;
  SessionP:PXMPPServerSession;
  
  Procedure PushRecvStreamRequest;
  var
    sNS:Core.Strings.VarString;
    sTo:Core.Strings.VarString;
    sVersion:Core.Strings.VarString;
  begin
    If Length(FXMLParser.Stanzas)=1 then begin
      sNS:=FXMLParser.Parameter(0,'xmlns');
      sTo:=Lowercase(FXMLParser.Parameter(0,'to'));
      sVersion:=Lowercase(FXMLParser.Parameter(0,'version'));
      If (sNS=NS_Server) then begin
        If SameText(sTo,Owner.RootDomain.Name) then begin
          Inc(RSR.XMPP.Sessions_Lcv);
          SessionP^.StreamID:=RSR.XMPP.GenerateStreamID;
          sResponse:=Concat('<',Stream_Stream,' xmlns="',NS_Server,'" id="',SessionP^.StreamID,'" from="',sTo,'" xmlns:stream="',NS_Streams,'">');
          Send(RSRP,sResponse);
          If sVersion<>'' then begin
            sResponse:=Concat('<',Stream_Features,'><mechanisms xmlns="',NS_SASL,'"><mechanism>DIGEST-MD5</mechanism><mechanism>PLAIN</mechanism></mechanisms>');
            Send(RSRP,sResponse);
          end;
          Include(SessionP^.State,ssStream);
        end else
          PushStreamError(seSeeOtherHost,'Cannot establish session stream to server other than this domain.',RSRP);
      end else
        PushStreamError(seInvalidNamespace,'Namespace unrecognized.',RSRP);
    end else
      PushStreamError(seUnsupportedStanzaType,'Unrecoginized xml code.',RSRP);
  end;

  procedure PushCancelMessage;
  begin
    sResponse:=Concat('<message from="',FS_Message.sTo,'" to="',FS_Message.sFrom,'" ',Clean_ID_Tag(FS_IQ.sID),'type="error"><body>',FS_Message.sBody,'</body>',hXMPPD.ERROR_CANCEL,'</message>');
    Send(RSRP,sResponse);
  end;

  Procedure PushMessage;
  var
    saData:Core.Arrays.Types.VarString;
  begin
    FS_Message.sTo:=Lowercase(FXMLParser.Parameter(0,'to'));
    FS_Message.sFrom:=Lowercase(FXMLParser.Parameter(0,'from'));
    FS_Message.Kind:=RSR.XMPP.MessageKindFromString(Lowercase(FXMLParser.Parameter(0,'type')));
    FS_Message.sSubject:=FXMLParser.Tag(iLcv,'subject');
    FS_Message.sThread:=FXMLParser.Tag(iLcv,'thread');
    FS_Message.sBody:=FXMLParser.Tag(iLcv,'body');
    Core.Arrays.VarString.fromString(saData,FS_Message.sTo,'/');
    Try
      If Length(saData)=2 then begin
        FS_Message.sTo:=saData[0];
        FS_Message.sToResource:=saData[1];
      end;
    Finally
      SetLength(saData,0);
    end;
    // Check to see if user in on this system...
    Core.Arrays.VarString.fromString(saData,FS_Message.sTo,'@');
    Try
      If Length(saData)=2 then begin
        If (SameText(saData[1],Owner.RootDomain.Name)) or Find(Owner.FDomains,saData[1]) then begin
          // This domain is local
          {
          todo send message
          If Find(FOwner.FDomains,saData[0],saData[1]) then begin


          end else begin
            PushCancelMessage;
          end;
          }
        end else begin
          PushCancelMessage;
        end;
      end else begin
        PushStreamError(seInvalidID,'Invalid To Field.',RSRP);
      end;
    Finally
      Core.Arrays.VarString.Done(saData);
    end;
  end;

begin
  SessionP:=RSRP^.Info.DataP;
  For iLcv:=0 to High(FXMLParser.Stanzas) do With FXMLParser.Stanzas[iLcv]^.Tags.Items[0]^ do begin
    If Name=Stream_Stream then begin
      PushRecvStreamRequest;
    end else If  Name=MESSAGE then begin
      PushMessage;
    end;
  end;
end;

procedure    TXMPPRemoteSessionManager.OnDisconnect(RSRP:PRSR);
begin

end;

procedure    TXMPPRemoteSessionManager.OnConnect(RSRP:PRSR);
var
  ssDataP:PXMPPServerSession;
begin
  ssDataP:=RSRP^.Info.DataP;
  If ssDataP<>nil then begin
    If ssAquireStream in ssDataP^.State then begin

    end;
  end;
end;

procedure    TXMPPRemoteSessionManager.OnError(RSRP:PRSR);
begin

end;

procedure    TXMPPRemoteSessionManager.OnQueue(RSRP:PRSR);
begin

end;

procedure    TXMPPRemoteSessionManager.OnDataReceived(RSRP:PRSR; var Handled:boolean);
var
  iLoc:LongInt;
  sXML:Core.Strings.VarString;
begin
  Handled:=true;
  if RSR.EndOf(RSRP^.RecvBuffer,'>') then begin
    sXML:=Core.Streams.toString(RSRP^.RecvBuffer.Stream);
    FXMLParser.Parse(sXML);
    PushProcessStanzas(RSRP);
    Empty(RSRP^.RecvBuffer);
  end;
end;


procedure    TXMPPRemoteSessionManager.OnDNSResult(RSRP:PRSR);
var
  iLcv,iLen:LongInt;
  SessionP:PXMPPServerSession;
begin
  SessionP:=RSRP^.Info.DataP;
  If RSRP^.DNS.Questions[0].Q_Type=QT_IP then begin
    iLen:=Length(RSRP^.DNS.Answers);
    SetLength(SessionP^.Network.IPs,iLen);
    For iLcv:=0 to iLen-1 do
      SessionP^.Network.IPs[iLcv]:=Core.Utils.Sockets.InAddrFromStr(RSRP^.DNS.Answers[iLcv]);
  end;
  RSRP^.DNS.Reset();
end;

procedure    TXMPPRemoteSessionManager.OnInitialize(RSRP:PRSR);
var
  SessionP:PXMPPServerSession;
begin
  New(SessionP);
  RSRP^.Info.DataP:=SessionP;
  Init(SessionP^);
  {$ifdef cpu64}
    InterlockedIncrement64(RSR_Stream_Count);
  {$else}
    Inc(RSR_Stream_Count);
  {$endif}
end;

procedure    TXMPPRemoteSessionManager.OnFinalize(RSRP:PRSR);
var
  SessionP:PXMPPServerSession;
begin
  SessionP:=RSRP^.Info.DataP;
  Done(SessionP^);
  {$ifdef cpu64}
    InterlockedDecrement64(RSR_Stream_Count);
  {$else}
    Dec(RSR_Stream_Count);
  {$endif}
end;

procedure    TXMPPRemoteSessionManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;


end.



{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit uIMAPd;


interface
uses
  Types,
  Classes,
  SysUtils,

  Core.Utils.Time,

  App,
  App.Consts,
  App.Build,

  RSR,
  RSR.DNS,
  RSR.IMAP,
  RSR.HTTP,

  Core.Timer,
  Core.Streams,
  Core.XML,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Core.Logging,
  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.Bytes,
  Core.Arrays.Boolean,
  Core.Utils.Sockets,
  Core.Utils.Mail,

  Storage,
  Storage.Main,
  Storage.UserAccounts,
  Storage.UserStorage,
  Storage.Domains,
  Storage.CoreObjects,
  Storage.KeepAlive,
  Storage.Intrusion,
  Storage.ContentTypes,
  Storage.Certs,
  Storage.Security,
  Encryption.Base64,

  XmlReader,
  XMLRead,
  DOM,
  DateUtils;

Type
  Commands=Class
  Type
    Response=class
    Const
      SpacePadding : Array[boolean] of Core.Strings.VarString = ('',' ');
    end;

    Authenticate=class
    Const
      Name       : Core.Strings.VarString = 'Authenticate';
      Plain      : Core.Strings.VarString = 'Plain';
      Login      : Core.Strings.VarString = 'Login';
      DigestMD5  : Core.Strings.VarSTring = 'DIGEST-MD5';
    end;

  Const
    Capability   : Core.Strings.VarString = 'Capability';
    Noop         : Core.Strings.VarString = 'Noop';
    Fetch        : Core.Strings.VarString = 'Fetch';
    UID          : Core.Strings.VarString = 'UID';
    Search       : Core.Strings.VarString = 'Search';
    Store        : Core.Strings.VarString = 'Store';
    Close        : Core.Strings.VarString = 'Close';
    Check        : Core.Strings.VarString = 'Check';
    Expunge      : Core.Strings.VarString = 'Expunge';
    Logout       : Core.Strings.VarString = 'Logout';
    Login        : Core.Strings.VarString = 'Login';
    StartTLS     : Core.Strings.VarString = 'StartTLS';
    Select       : Core.Strings.VarString = 'Select';
    Examine      : Core.Strings.VarString = 'Examine';
    Created      : Core.Strings.VarString = 'Create';
    CopyUID      : Core.Strings.VarString = 'CopyUID';
    Delete       : Core.Strings.VarString = 'Delete';
    Rename       : Core.Strings.VarString = 'Rename';
    Subscribe    : Core.Strings.VarString = 'Subscribe';
    UnSubscribe  : Core.Strings.VarString = 'UnSubscribe';
    Status       : Core.Strings.VarString = 'Status';
    List         : Core.Strings.VarString = 'List';
    LSub         : Core.Strings.VarString = 'LSub';
    Append       : Core.Strings.VarString = 'Append';
    ID           : Core.Strings.VarString = 'ID';
  end;

  TIMAPServer=Class(TRSRServer)
  private
    FManagers            : TRSRManagers;
    RootDomain           : Storage.Domains.Items.TDomain;
  protected
    procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
  public
    Constructor Create(aDomain:Core.Strings.VarString; Const aIP:Int64; aPort, aScale:WORD; aSuspended,aSSL:boolean); ReIntroduce;
    Destructor  Destroy; override;
  end;

  TIMAPManager=Class(TRSRManager)
  private
    Owner                        : TIMAPServer;
    UserAccounts                 : Storage.UserAccounts.Items.TList;
    FCommands                    : Core.Arrays.Types.VarString;

    IMAPP                        : PIMAP;


    FDestinationFolderP          : Storage.UserStorage.Folders.PFolder;
    FFolderP                     : Storage.UserStorage.Folders.PFolder;
    FFileP                       : Storage.UserStorage.Files.PItem;
    FFile                        : Storage.UserStorage.Files.TItem;
    FFolderPath                  : Core.Arrays.Types.VarString;
    FUIDSet                      : Core.Arrays.Types.LargeWord;
    FStoreFlags                  : LongInt;
    FNewID                       : QWord;
    FBody                        : Storage.UserStorage.Items.IMAP.TBodyElement;
    FMessageP                    : PIMAPMessage;
    FRefPath                     : Core.Arrays.Types.VarString;
    FMbxPath                     : Core.Arrays.Types.VarString;

    FPathLen                     : LongInt;
    FPathSkip                    : LongInt;
    FIntrusions                  : QWord;
    FRemoteIP                    : QWord;
    CMDS                         : Core.Strings.VarString;
    CMD                          : Core.Strings.VarString;
    CMDP1                        : Core.Strings.VarString;
    CMDP2                        : Core.Strings.VarString;
    CMDP3                        : Core.Strings.VarString;
    CMDP4                        : Core.Strings.VarString;
    CMDP5                        : Core.Strings.VarString;

    FSData                       : TFileStream;
    FRangeTerm                   : Core.Arrays.Types.VarString;
    FRangePair                   : Core.Arrays.Types.VarString;

    FStreamStart                 : Int64;
    FStreamEnd                   : Int64;

    FBodyStream                  : TMemoryStream;

    FFetchKinds                  : TIMAPFetchKinds;
    FRangeKinds                  : TIMAPRangeKinds;
    FRangeKind                   : TIMAPRangeKind;
    FStoreFlagMode               : TIMAPStoreFlagMode;

    FContent                     : Core.Arrays.Types.VarString;
    FLines                       : Core.Arrays.Types.VarString;
    FBodyItems                   : Core.Arrays.Types.VarString;
    FHeaderFields                : Core.Arrays.Types.VarString;
    FHeaderItems                 : Core.Arrays.Types.KeyStrings;


    FMimeP                       : Storage.UserStorage.Items.SMTP.PMIME;
    FMimesP                      : Storage.UserStorage.Items.SMTP.PMIMES;
    FMimeLen                     : LongInt;
    FFlagIndex                   : LongInt;
    FFLags                       : LongInt;
    FCommandCount                : LongInt;

    FSetLen                      : LongInt;

    FRangeStart                  : Int64;
    FRangeLen                    : Int64;
    FRangeEnd                    : Int64;

    FByteRangeStart              : LongInt;
    FByteRangeSize               : LongInt;

    FStoreIdx                    : LongInt;
    FStoreId                     : QWord;

    FFetchSections               : Core.Arrays.Types.VarString;
    FFetchSection                : Core.Strings.VarString;
    FFetchSectionPartial         : Core.Strings.VarString;
    FFetchSectionPartials        : Core.Arrays.Types.VarString;

    FFetchIdx                    : LongInt;
    FFetchPart                   : LongInt;
    FFetchLen                    : Int64;
    FFetchLcv                    : QWord;
    FSearchSize                  : QWord;
    FFileID                      : QWord;
    FMailboxWildChar             : boolean;
    FFileCount                   : LongInt;
    FLength                      : Int64;
    FHeaderBreak                 : LongInt;
    FBodyCount                   : Int64;
    FBodySize                    : Int64;
    FStamp                       : Double;

    FRecent                      : Cardinal;
    FDeleted                     : Cardinal;
    FUnseen                      : Cardinal;
    FUNSEENID                    : QWord;
    FNEXTID                      : QWord;
    FFilesWithFlags              : QWord;
    FFolderID                    : QWord;
    FHeaderCount                 : LongInt;

    FStatus                      : Core.Strings.VarString;
    FSelected                    : Core.Strings.VarString;
    FExamine                     : Core.Strings.VarString;


    FResponse                    : Core.Strings.VarString;
    FPath                        : Core.Strings.VarString;
    FPathNew                     : Core.Strings.VarString;
    FPathLoop                    : Core.Strings.VarString;
    FMailbox                     : Core.Strings.VarString;
    FReference                   : Core.Strings.VarString;

    FSrchArg                     : TSrchArg;


    FSrchArgs                    : TSrchArgs;
    FSrchTerms                   : TSrchTerms;
    FSrchDate                    : Double;
    FSrchFileDate                : Double;
    FSrchResult                  : QWord;

    FSrchYear                    : Word;
    FSrchMonth                   : Word;
    FSrchDay                     : Word;
    FSrchHour                    : Word;
    FSrchMin                     : Word;
    FSrchSec                     : Word;
    FSrchMs                      : Word;
    FSrchFileYear                : Word;
    FSrchFileMonth               : Word;
    FSrchFileDay                 : Word;
    FSrchFileHour                : Word;
    FSrchFileMin                 : Word;
    FSrchFileSec                 : Word;
    FSrchFileMs                  : Word;

    FSrchArgCount                : LongInt;
    FSrchError                   : LongInt;

    FSrchTerm                    : Core.Strings.VarString;
    FSrchResults                 : Core.Arrays.Types.LargeWord;

    FSummary                     : Storage.UserStorage.Items.SMTP.TSummary;

    FClientAuth                  : Core.Arrays.Types.KeyStrings;
    FSEQ                         : Core.Strings.VarString;

    FData                        : Core.Strings.VarString;
    FEntry                       : Core.Strings.VarString;
    FEntry2                      : Core.Strings.VarString;
    FHeadersConfirmed            : Core.Strings.VarString;
    FHeaders                     : Core.Strings.VarString;
    FHeadersOut                  : Core.Strings.VarString;
    FBodyFileName                : Core.Strings.VarString;
    FReadAhead                   : Boolean;
    FUTF8                        : UTF8String;
    FFolder                      : Core.Strings.VarString;
    FAuth                        : Core.Strings.VarString;
    FBodyPeek                    : Core.Strings.VarString;
    FBodySection                 : Core.Strings.VarString;
    FHeader                      : Core.Strings.VarString;
    FHeaderP                     : Core.Strings.PKeyString;
    FBodyFull                    : Core.Strings.VarString;
    FTempRange                   : Core.Strings.VarString;
    FDestination                 : Core.Strings.VarString;
    FBoxFlags                    : Core.Strings.VarString;
    FRenameFolders               : Storage.UserStorage.Folders.TFolders;

    FSpamFilter                  : Storage.Security.Filter.Item;
    FConnectionFilter            : Storage.Security.Filter.Item;
    FIntruder                    : Boolean;
    FHacker                      : Boolean;
    FFetch                       : Core.Arrays.Types.VarString;
    //FQueryLog                    : Core.Arrays.Types.VarString;

    FXMLParser                   : TDOMParser;
    FXMLDocument                 : TXMLDocument;
    FXMLSource                   : TXMLInputSource;

    FEnvelope                    : Storage.UserStorage.Items.IMAP.TEnvelope;

  private
    function    ID_Search_Execute():QWord;
    function    ID_Search_Directive(var Term:TSrchTerm):QWord;
    function    ID_Search_Single(var Term:TSrchTerm):QWord;
    function    ID_Search_Double(var Term:TSrchTerm):QWord;
    function    ID_Search_Tripple(var Term:TSrchTerm):QWord;
    function    ID_Search_Single_Not(var Term:TSrchTerm):QWord;
    function    ID_Search_Double_Not(var Term:TSrchTerm):QWord;
    function    ID_Search_Tripple_Not(var Term:TSrchTerm):QWord;
    function    ID_Search_Not(var Directive:TSrchTermDirective):QWord;
    function    ID_Search_Or(var Directive:TSrchTermDirective):QWord;


    procedure   ID_Search(RSRP:PRSR);
    procedure   ID_Fetch(RSRP:PRSR);
    procedure   ID_Store(RSRP:PRSR);
    procedure   ID_Copy(RSRP:PRSR);
    procedure   ID_Expunge(RSRP:PRSR);

    function    ParseSummary(Data:Core.Strings.VarString; var Summary:Storage.UserStorage.Items.SMTP.TSummary):Boolean;
    function    GetSection(var Data:Core.Strings.VarString; var Summary:Storage.UserStorage.Items.SMTP.TSummary; Index:LongInt):Core.Strings.VarString;
    procedure   saFromStringProgress(Index,Total:Int64);

    function    GetTextMime(var Summary:Storage.UserStorage.Items.SMTP.TSummary):Storage.UserStorage.Items.SMTP.PMIME;
    function    CacheMessage(var &File:Storage.UserStorage.Files.TItem; out HeaderIndex,HeaderCount:LongInt; out sHeaders:Core.Strings.VarString; out kplHeaders:Core.Arrays.Types.KeyStrings):Boolean;
    function    SetRange():LongInt;
    function    Capabilities:Core.Strings.VarString;
    function    TransState:Boolean;
  private
    procedure   OutputFetchFull(RSRP:PRSR; const Index:LongInt; var &File:Storage.UserStorage.Files.TItem; var Envelope:Storage.UserStorage.Items.IMAP.TEnvelope; var Body:Storage.UserStorage.Items.IMAP.TBodyElement);
    procedure   OutputFetchAll(RSRP:PRSR; const Index:LongInt; var &File:Storage.UserStorage.Files.TItem; var Envelope:Storage.UserStorage.Items.IMAP.TEnvelope);
    procedure   OutputFetchFast(RSRP:PRSR; const Index:LongInt; var &File:Storage.UserStorage.Files.TItem);
    procedure   OutputFetchBodyStructure(RSRP:PRSR; const Index:LongInt; var Body:Storage.UserStorage.Items.IMAP.TBodyElement);
    procedure   OutputFetchFlags(RSRP:PRSR; Range:TIMAPRangeKinds; var Folder:Storage.UserStorage.Folders.TFolder; var Files:Storage.UserStorage.Files.TItems);
    procedure   OutputFetchDeep(RSRP:PRSR; Range:TIMAPRangeKinds; var Folder:Storage.UserStorage.Folders.TFolder; var Files:Storage.UserStorage.Files.TItems);
    procedure   OutputFetchFlag(RSRP:PRSR; Index:LongInt; ID:QWord; Flags:LongInt);
  private
    procedure   ID(RSRP:PRSR);
    procedure   Search(RSRP:PRSR);
    procedure   Status(RSRP:PRSR; Path:Core.Strings.VarString); overload;
    procedure   Status(RSRP:PRSR);
    procedure   Status(RSRP:PRSR; var Folder:Storage.UserStorage.Folders.TFolder; Const AsCommand:boolean); overload;
    procedure   Select(RSRP:PRSR; Path:Core.Strings.VarString);

    procedure   Noop(RSRP:PRSR);
    procedure   Check(RSRP:PRSR);
    procedure   Created(RSRP:PRSR; Folder:string); overload;
    procedure   CopyUID(RSRP:PRSR; FolderID:QWord);
  private
    procedure   Empty(); overload;
  private
    function    Expunge(RSRP:PRSR; var Folder:Storage.UserStorage.Folders.TFolder; StatusMessages:boolean=false):boolean;
    procedure   ReloadFolders(sPath:Core.Strings.VarString; aKind:TIMAPFolderReloadKind);
    procedure   ReleaseMessage(var Message:PIMAPMessage);
    function    Append(RSRP:PRSR):boolean; overload;
    function    Respond(sSequence,sCode,sResult:Core.Strings.VarString):Core.Strings.VarString; overload;
    function    Respond(sResult:Core.Strings.VarString):Core.Strings.VarString; overload;
    procedure   Process(RSRP:PRSR; var InputN:Core.Strings.VarString);
    procedure   Process_Message(RSRP:PRSR);
    procedure   Process_UID(RSRP:PRSR);
    procedure   ClipBuffer(Const CRLFCount:LongInt; Var Buffer:Core.Strings.VarString);
  protected
    procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
    procedure   OnQueue(RSRP:PRSR); override;
    procedure   OnError(RSRP:PRSR); override;
    procedure   OnDisconnect(RSRP:PRSR); override;
    procedure   OnConnect(RSRP:PRSR); override;
    procedure   OnDataReceived(RSRP:PRSR; Var Handled:Boolean); override;
    procedure   OnDNSResult(RSRP:PRSR); override;
    procedure   OnInitialize(RSRP:PRSR); override;
    procedure   OnFinalize(RSRP:PRSR); override;
  public
    Constructor Create(AOwner:TIMAPServer); reintroduce; overload; virtual;
    Destructor  Destroy; override;
  end;

implementation
uses Math;

const
  S_SSL:Array[boolean] of Core.Strings.VarString=(' ',' SSL ');
  S_SVC:Array[boolean] of Core.Strings.PVarString=(@SERVICE_IMAP,@SERVICE_IMAP_SSL);

Constructor TIMAPServer.Create(aDomain:Core.Strings.VarString; Const aIP:Int64; aPort,aScale:Word; aSuspended,aSSL:boolean);
var
  iLcv:LongInt;
begin
  Inherited Create(@FManagers,tTCP,aIP,aPort,aScale,aSSL,aSuspended);

  FService:=S_SVC[aSSL]^;

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Server'));

  RootDomain.Name:=aDomain;

  Storage.Domains.Items.DB.Fill(Storage.Main.Task,RootDomain);

  if RootDomain.CertID<>0 then
    Storage.Certs.Items.DB.Read(Task,RootDomain.ID,RootDomain.CertID,Cert);


  FSSLInfo.keyLen:=Core.Arrays.Bytes.Copy(Cert.DerKey,FSSLInfo.keyData);
  Load(Cert,FSSLInfo.Manifest);

  iLcv:=0;
  While iLcv<aScale do begin
    SetLength(FManagers,iLcv+1);
    Try
      FManagers[iLcv]:=TIMAPManager.Create(Self);
    Except
      iLcv:=aScale;
    end;
    Inc(iLcv);
  end;

end;

Destructor  TIMAPServer.Destroy;
begin
  Inherited Destroy;
end;

{$i uIMAPServer.Callbacks.inc}

Constructor TIMAPManager.Create(AOwner:TIMAPServer);
begin
  FSData:=nil;
  FBodyStream:=TMemoryStream.Create();

  FService:=S_SVC[AOwner.FSecure]^;
  FXMLParser:=TDOMParser.Create;
  FXMLParser.Options.Validate:=False;
  FXMLParser.Options.ConformanceLevel:=clFragment;

  Storage.Security.Filter.Init(FSpamFilter);
  Storage.Security.Filter.Init(FConnectionFilter);

  Storage.UserStorage.Items.SMTP.Init(FSummary);
  Storage.UserStorage.Items.IMAP.Init(FEnvelope);
  Storage.UserStorage.Items.IMAP.Init(FBody);

  Task:=Core.Database.Types.TTask.Create(Storage.Main.Header,Concat(FService,' Manager'));

  Owner:=AOwner;
  UserAccounts:=Storage.UserAccounts.Items.TList.Create(@AOwner.RootDomain,FService);

  Inherited Create(aOwner,@AOwner.FSSLInfo,AOwner.FSecure,THREAD_METHODS_OFF,IMAP_STACK_SIZE);

  Storage.UserStorage.Files.Init(FFile);

  TimeOut:=120000;
end;

Destructor  TIMAPManager.Destroy;
begin
  Storage.UserStorage.Files.Done(FFile);
  Storage.UserStorage.Items.SMTP.Done(FSummary);
  Storage.UserStorage.Items.IMAP.Done(FEnvelope);
  Storage.UserStorage.Items.IMAP.Done(FBody);

  Storage.Security.Filter.Done(FSpamFilter);
  Storage.Security.Filter.Done(FConnectionFilter);

  //Done(FQueryLog);


  FreeAndNil(FBodyStream);
  FreeAndNil(FXMLParser);
  FreeAndNil(UserAccounts);

  Inherited Destroy;
end;

{$i uIMAPManager.ClipBuffer.inc}

procedure  TIMAPManager.OnQueue(RSRP:PRSR);
begin
  IMAPP:=RSRP^.Info.DataP;
  If (IMAPP<>Nil) then begin
    IMAPP^.RemoteIP:=Core.Utils.Sockets.InAddrToStr(RSRP^.Address.sin_addr.S_addr);
    Storage.Security.Filter.Empty(FConnectionFilter);
    Try
        FIntruder:=Storage.Intrusion.Intruder.DB.Recent(Task,Owner.RootDomain.ID,RSRP^.Address.sin_addr.s_addr)>0;
        FHacker:=Storage.Intrusion.Account.DB.Recent(Task,Owner.RootDomain.ID,RSRP^.Address.sin_addr.s_addr)>0;
        if (FHacker=false) and (FIntruder=false) then begin
          IMAPP:=RSRP^.Info.DataP;
          IMAPP^.State:=RS_NONE;
          IMAPP^.ErrorCount:=0;
          IMAPP^.DeleteIndex:=-1;
          FResponse:=Concat('* OK [CAPABILITY ',Capabilities,'] ',Owner.RootDomain.Name,' running ',App.Build.Title,' ',App.Build.Edition,' Build (',App.Build.Version,') RSR Build (',App.Build.RSR,'). AURA IMAP Service Ready.',#13#10);
          Send(RSRP,FResponse);
        end else begin
          FResponse:=Concat('* NO ',Owner.RootDomain.Name,'IP (',IMAPP^.RemoteIP,') blocked by ',App.Build.Title,' ',App.Build.Edition,' Build (',App.Build.Version,') RSR Build (',App.Build.RSR,'). AURA IMAP Service denied.',#13#10);
          Send(RSRP,FResponse);
          Close(RSRP);
          Core.Logging.Native.WriteLogEntry(
            Owner.RootDomain.Name,
            FService,
            Concat(
              'Security alert : Rejected ',
              App.Consts.Hacker[FHacker],' ',
              App.Consts.Intruder[FIntruder],' ',
              'IP ',
              IntToStr(RSRP^.Address.sin_addr.s_addr),
              ' (',IMAPP^.RemoteIP,')'
            )
          );
        end;

    finally
      Storage.Security.Filter.Empty(FConnectionFilter);
    end;
  end else begin
    FResponse:=Concat('* NO ',Owner.RootDomain.Name,' Aura IMAP Service Not Ready.',#13#10);
    Send(RSRP,FResponse);
    Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TIMAPManager.OnQueue Error:','Null pointer to IMAP Data'));
    Close(RSRP);
  end;
end;

procedure  TIMAPManager.OnConnect(RSRP:PRSR);
begin
end;

procedure   TIMAPManager.OnDNSResult(RSRP:PRSR);
begin

end;

procedure  TIMAPManager.OnDisconnect(RSRP:PRSR);
begin
end;

procedure   TIMAPManager.Process_UID(RSRP:PRSR);
begin
  {$i uIMAPManager.Process.UID.inc}
end;

procedure   TIMAPManager.Process_Message(RSRP:PRSR);
begin
  {$i uIMAPManager.ProcessMessage.inc}
end;

procedure  TIMAPManager.OnInitialize(RSRP:PRSR);
begin
  New(IMAPP);
  RSR.IMAP.Init(IMAPP^);
  RSRP^.Info.DataP:=IMAPP;
end;

procedure  TIMAPManager.OnFinalize(RSRP:PRSR);
begin
  IMAPP:=RSRP^.Info.DataP;
  If (IMAPP<>Nil) then begin
    Try
      EntryPoint:='TIMAPManager.OnFinalize.Done';
      RSR.IMAP.Done(IMAPP^);
      EntryPoint:='TIMAPManager.OnFinalize.Dispose';
      Dispose(IMAPP);
    Finally
      RSRP^.Info.DataP:=Nil;
    end;
  end;
end;

function TIMAPManager.ID_Search_Execute:QWord;
var
  iLcv:LongInt;
  TermP:PSrchTerm;
  Results:TBooleanArray;
  iCt:LongInt;
begin
  EntryPoint:='TIMAPManager.ID_Search_Execute';
  Result:=0;
  iCt:=Length(FSrchTerms);
  Core.Arrays.Boolean.SetSize(Results,iCt);
  Try
    Fill(Results,False);
    for iLcv:=0 to High(FSrchTerms) do begin
      TermP:=FSrchTerms[iLcv];
      Case TermP^.Kind of
        srchTermKindSingle    : Results[iLcv]:=ID_Search_Single(TermP^)<>0;
        srchTermKindDouble    : Results[iLcv]:=ID_Search_Double(TermP^)<>0;
        srchTermKindTripple   : Results[iLcv]:=ID_Search_Tripple(TermP^)<>0;
        srchTermKindDirective : Results[iLcv]:=ID_Search_Directive(TermP^)<>0;
      end;
    end;
    If Core.Arrays.Boolean.All(true,Results) then
      Result:=FFileP^.ID;

  finally
    Core.Arrays.Boolean.Done(Results);
  end;
end;

function TIMAPManager.ID_Search_Single(var Term:TSrchTerm):QWord;
var
  SingleP  : PSrchTermSingle;
begin
  EntryPoint:='TIMAPManager.ID_Search_Single';
  Result:=0;
  SingleP:=Term.Data;
  case SingleP^.Value of
    srchArgAnswered: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Answered=FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgDeleted: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgDraft: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Draft=FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgFlagged: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Flagged=FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgNew : if (
      (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) and
      (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags)
    ) then
      Result:=FFileP^.ID;
    srchArgOld : if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent<>FFileP^.Flags) then
      Result:=FFileP^.ID;
    srchArgRecent: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgSeen: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen=FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgUnAnwered: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Answered<>FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgUnDeleted: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted<>FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgUnDraft: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Draft<>FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgUnFlagged:If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Flagged<>FFileP^.Flags then
      Result:=FFileP^.ID;
    srchArgUnseen: If FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags then
      Result:=FFileP^.ID;
  end;
end;

function TIMAPManager.ID_Search_Single_Not(var Term:TSrchTerm):QWord;
var
  SingleP                 : PSrchTermSingle;
begin
  EntryPoint:='TIMAPManager.ID_Search_Single_Not';
  Result:=0;
  SingleP:=Term.Data;
  case SingleP^.Value of
    srchArgAnswered: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Answered=FFileP^.Flags) then
      Result:=FFileP^.ID;
    srchArgDeleted: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags) then
      Result:=FFileP^.ID;
    srchArgDraft: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Draft=FFileP^.Flags) then
      Result:=FFileP^.ID;
    srchArgFlagged: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Flagged=FFileP^.Flags) then
      Result:=FFileP^.ID;
    srchArgNew : if ( Not (
      (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) and
      (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags)
    ) ) then
      Result:=FFileP^.ID;
    srchArgOld : if Not ((FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent<>FFileP^.Flags) ) then
      Result:=FFileP^.ID;
    srchArgRecent: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags ) then
      Result:=FFileP^.ID;
    srchArgSeen: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen=FFileP^.Flags ) then
      Result:=FFileP^.ID;
    srchArgUnAnwered: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Answered<>FFileP^.Flags ) then
      Result:=FFileP^.ID;
    srchArgUnDeleted: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted<>FFileP^.Flags ) then
      Result:=FFileP^.ID;
    srchArgUnDraft: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Draft<>FFileP^.Flags ) then
      Result:=FFileP^.ID;
    srchArgUnFlagged:If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Flagged<>FFileP^.Flags ) then
      Result:=FFileP^.ID;
    srchArgUnseen: If Not (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags) then
      Result:=FFileP^.ID;
  end;
end;

function TIMAPManager.ID_Search_Tripple(var Term:TSrchTerm):QWord;
var
  TrippleP : PSrchTermTripple;
begin
  EntryPoint:='TIMAPManager.ID_Search_Tripple';
  Result:=0;
  TrippleP:=Term.Data;

  Case TrippleP^.Value of
    srchArgHeader : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,TrippleP^.Criteria1,FHeaderCount);
      if ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,TrippleP^.Criteria2)>0) ) then
        Result:=FFileP^.ID;
    end;
  end;

end;

function TIMAPManager.ID_Search_Tripple_NOT(var Term:TSrchTerm):QWord;
var
  TrippleP : PSrchTermTripple;
begin
  EntryPoint:='TIMAPManager.ID_Search_Tripple_NOT';
  Result:=0;
  TrippleP:=Term.Data;

  Case TrippleP^.Value of
    srchArgHeader : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,TrippleP^.Criteria1,FHeaderCount);
      if not ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,TrippleP^.Criteria2)>0) ) then
        Result:=FFileP^.ID;
    end;
  end;

end;


function TIMAPManager.ID_Search_Double(var Term:TSrchTerm):QWord;
var
  DoubleP : PSrchTermDouble;
begin
  EntryPoint:='TIMAPManager.ID_Search_Double';
  Result:=0;
  DoubleP:=Term.Data;
  case DoubleP^.Value of
    srchArgBcc         : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'BCC',FHeaderCount);
      if ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0) )then
        Result:=FFileP^.ID;
    end;
    srchArgBefore      : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) and (FFileP^.Created<FSrchDate) then
        Result:=FFileP^.ID;
    end;
    srchArgBody        : begin
      if ( (Length(FHeader)>0) and (Core.Streams.Pos(FBodyStream,DoubleP^.Criteria)>-1) ) then
        Result:=FFileP^.ID;
    end;
    srchArgCC          : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'CC',FHeaderCount);
      if ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0) )then
        Result:=FFileP^.ID;
    end;
    srchArgFrom        : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'From',FHeaderCount);
      if ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0)) then
        Result:=FFileP^.ID;
    end;
    srchArgKeyword     : begin

    end;
    srchArgLarger      : begin
      FSearchSize:=SysUtils.StrToIntDef(DoubleP^.Criteria,0);
      if (FFileP^.Size>FSearchSize) then
        Result:=FFileP^.ID;
    end;
    srchArgOn          : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
        DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
        DateUtils.DecodeDateDay(FFileP^.Created,FSrchFileYear,FSrchFileDay);
        if (FSrchYear=FSrchFileYear) and (FSrchDay=FSrchFileDay) then
         Result:=FFileP^.ID;
      end;
    end;
    srchArgSentBefore  : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
        FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Date',FHeaderCount);
        FSrchFileDate:=RFCDateTimeToDateTime(FHeader);
        DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
        DateUtils.DecodeDateDay(FSrchFileDate,FSrchFileYear,FSrchFileDay);
        if (FSrchFileYear<=FSrchYear) and (FSrchFileDay<=FSrchDay) then
          Result:=FFileP^.ID;
      end;
    end;
    srchArgSentOn      : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
          FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Date',FHeaderCount);
          FSrchFileDate:=RFCDateTimeToDateTime(FHeader);
          DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
          DateUtils.DecodeDateDay(FSrchFileDate,FSrchFileYear,FSrchFileDay);
          if (FSrchYear=FSrchFileYear) and (FSrchDay=FSrchFileDay) then
            Result:=FFileP^.ID;
      end;
    end;
    srchArgSentSince   : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
          FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Date',FHeaderCount);
          FSrchFileDate:=RFCDateTimeToDateTime(FHeader);
          DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
          DateUtils.DecodeDateDay(FSrchFileDate,FSrchFileYear,FSrchFileDay);
          if (FSrchFileYear>=FSrchYear) and (FSrchFileDay>=FSrchDay) then
            Result:=FFileP^.ID;
      end;
    end;
    srchArgSince       : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
        DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
        DateUtils.DecodeDateDay(FFileP^.Created,FSrchFileYear,FSrchFileDay);
        if (FSrchFileYear>=FSrchYear) and (FSrchFileDay>=FSrchDay) then
          Result:=FFileP^.ID;
      end;
    end;
    srchArgSmaller     : begin
      FSearchSize:=SysUtils.StrToQWordDef(DoubleP^.Criteria,0);
      if (FFileP^.Size<FSearchSize) then
        Result:=FFileP^.ID;
    end;
    srchArgHeader     : begin
      if ( Core.Arrays.KeyString.Search(FHeaderItems,DoubleP^.Criteria)<>-1)  then
        Result:=FFileP^.ID;
    end;
    srchArgSubject     : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Subject',FHeaderCount);
      if ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0) ) then
        Result:=FFileP^.ID;
    end;
    srchArgText        : begin
      if ( (Length(FHeader)>0) and (Core.Streams.Pos(FBodyStream,DoubleP^.Criteria)>-1)) then
       Result:=FFileP^.ID;
    end;
    srchArgTo          : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'To',FHeaderCount);
      if( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0)) then
        Result:=FFileP^.ID;
    end;
    srchArgUID         : begin
      Core.Arrays.LargeWord.fromString(DoubleP^.Criteria,FUIDSet,',');
      if Core.Arrays.LargeWord.IndexOf(FFileP^.ID,FUIDSet)<>-1 then
        Result:=FFileP^.ID;
    end;
    srchArgUnKeyword   : begin

    end;
  end;
end;

function TIMAPManager.ID_Search_Double_Not(var Term:TSrchTerm):QWord;
var
  DoubleP : PSrchTermDouble;
begin
  EntryPoint:='TIMAPManager.ID_Search_Double_Not';
  Result:=0;
  DoubleP:=Term.Data;
  case DoubleP^.Value of
    srchArgBcc         : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'BCC',FHeaderCount);
      if Not( ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0)))then
        Result:=FFileP^.ID;
    end;
    srchArgBefore      : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if Not( (FSrchDate<>0) and (FFileP^.Created<FSrchDate) ) then
        Result:=FFileP^.ID;
    end;
    srchArgBody        : begin
      if Not(  ( (Length(FHeader)>0) and (Core.Streams.Pos(FBodyStream,DoubleP^.Criteria)>-1) )) then
        Result:=FFileP^.ID;
    end;
    srchArgCC          : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'CC',FHeaderCount);
      if Not( ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0)))then
        Result:=FFileP^.ID;
    end;
    srchArgFrom        : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'From',FHeaderCount);
      if Not( ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0))) then
        Result:=FFileP^.ID;
    end;
    srchArgKeyword     : begin

    end;
    srchArgLarger      : begin
      FSearchSize:=SysUtils.StrToIntDef(DoubleP^.Criteria,0);
      if Not( (FFileP^.Size>FSearchSize)) then
        Result:=FFileP^.ID;
    end;
    srchArgOn          : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
        DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
        DateUtils.DecodeDateDay(FFileP^.Created,FSrchFileYear,FSrchFileDay);
        if Not( (FSrchYear=FSrchFileYear) and (FSrchDay=FSrchFileDay)) then
         Result:=FFileP^.ID;
      end;
    end;
    srchArgSentBefore  : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin

          FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Date',FHeaderCount);
          FSrchFileDate:=RFCDateTimeToDateTime(FHeader);
          DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
          DateUtils.DecodeDateDay(FSrchFileDate,FSrchFileYear,FSrchFileDay);
          if Not(  (FSrchFileYear<=FSrchYear) and (FSrchFileDay<=FSrchDay)) then
            Result:=FFileP^.ID;
      end;
    end;
    srchArgSentOn      : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin

          FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Date',FHeaderCount);
          FSrchFileDate:=RFCDateTimeToDateTime(FHeader);
          DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
          DateUtils.DecodeDateDay(FSrchFileDate,FSrchFileYear,FSrchFileDay);
          if Not( (FSrchYear=FSrchFileYear) and (FSrchDay=FSrchFileDay)) then
            Result:=FFileP^.ID;

      end;
    end;
    srchArgSentSince   : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin

          FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Date',FHeaderCount);
          FSrchFileDate:=RFCDateTimeToDateTime(FHeader);
          DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
          DateUtils.DecodeDateDay(FSrchFileDate,FSrchFileYear,FSrchFileDay);
          if Not( (FSrchFileYear>=FSrchYear) and (FSrchFileDay>=FSrchDay)) then
            Result:=FFileP^.ID;

      end;
    end;
    srchArgSince       : begin
      FSrchDate:=RFCDateTimeToDateTime(DoubleP^.Criteria);
      if (FSrchDate<>0) then begin
        DateUtils.DecodeDateDay(FSrchDate,FSrchYear,FSrchDay);
        DateUtils.DecodeDateDay(FFileP^.Created,FSrchFileYear,FSrchFileDay);
        if Not( (FSrchFileYear>=FSrchYear) and (FSrchFileDay>=FSrchDay)) then
          Result:=FFileP^.ID;
      end;
    end;
    srchArgSmaller     : begin
      FSearchSize:=SysUtils.StrToQWordDef(DoubleP^.Criteria,0);
      if Not( (FFileP^.Size<FSearchSize) ) then
        Result:=FFileP^.ID;
    end;
    srchArgSubject     : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'Subject',FHeaderCount);
      if Not(  ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0) )) then
        Result:=FFileP^.ID;
    end;
    srchArgText        : begin
      if Not( ( (Length(FHeader)>0) and (Core.Streams.Pos(FBodyStream,DoubleP^.Criteria)>-1))) then
       Result:=FFileP^.ID;
    end;
    srchArgTo          : begin
      FHeader:=Core.Arrays.KeyString.GetItemAsString(FHeaderItems,'To',FHeaderCount);
      if Not( ( (Length(FHeader)>0) and (Core.Strings.Search(FHeader,DoubleP^.Criteria)>0))) then
        Result:=FFileP^.ID;
    end;
    srchArgUID         : begin
      Core.Arrays.LargeWord.fromString(DoubleP^.Criteria,FUIDSet,',');
      if Not( Core.Arrays.LargeWord.IndexOf(FFileP^.ID,FUIDSet)<>-1) then
        Result:=FFileP^.ID;
    end;
    srchArgUnKeyword   : begin

    end;
  end;
end;

function TIMAPManager.ID_Search_Not(var Directive:TSrchTermDirective):QWord;
var
  TermP:PSrchTerm;
begin
  EntryPoint:='TIMAPManager.ID_Search_Not';
  Result:=0;
  TermP:=@Directive.Key1;
  Case TermP^.Kind of
    srchTermKindSingle    : Result:=ID_Search_Single_NOT(TermP^);
    srchTermKindDouble    : Result:=ID_Search_Double_NOT(TermP^);
    srchTermKindTripple   : Result:=ID_Search_Tripple_NOT(TermP^);
    srchTermKindDirective : Result:=ID_Search_Directive(TermP^);
  end;
end;

function TIMAPManager.ID_Search_Or(var Directive:TSrchTermDirective):QWord;
var
  Term1P:PSrchTerm;
  Term2P:PSrchTerm;
  ID1:QWord;
  ID2:QWord;
begin
  EntryPoint:='TIMAPManager.ID_Search_Or';
  Result:=0;
  ID1:=0;
  ID2:=0;
  Term1P:=@Directive.Key1;
  Term2P:=@Directive.Key2;
  Case Term1P^.Kind of
    srchTermKindSingle    : ID1:=ID_Search_Single(Term1P^);
    srchTermKindDouble    : ID1:=ID_Search_Double(Term1P^);
    srchTermKindTripple   : ID1:=ID_Search_Tripple(Term1P^);
    srchTermKindDirective : ID1:=ID_Search_Directive(Term1P^);
  end;
  if (ID1<>FFileP^.ID) then begin
    Case Term2P^.Kind of
      srchTermKindSingle    : ID2:=ID_Search_Single(Term2P^);
      srchTermKindDouble    : ID2:=ID_Search_Double(Term2P^);
      srchTermKindTripple   : ID2:=ID_Search_Tripple(Term2P^);
      srchTermKindDirective : ID2:=ID_Search_Directive(Term2P^);
    end;
    if (ID2=FFileP^.ID) then
      Result:=FFileP^.ID;
  end else
    Result:=FFileP^.ID;
end;

function TIMAPManager.ID_Search_Directive(var Term:TSrchTerm):QWord;
var
  DirectiveP : PSrchTermDirective;
begin
  EntryPoint:='TIMAPManager.ID_Search_Directive';
  Result:=0;
  DirectiveP:=Term.Data;
  Case DirectiveP^.Value of
    srchArgOr  : Result:=ID_Search_Or(DirectiveP^);
    srchArgNot : Result:=ID_Search_Not(DirectiveP^);
  end;
end;

function TIMAPManager.ParseSummary(Data:Core.Strings.VarString; var Summary:Storage.UserStorage.Items.SMTP.TSummary):Boolean;
begin
  EntryPoint:='TIMAPManager.ParseSummary(Data, Summary)';
  Storage.UserStorage.Items.SMTP.Empty(Summary);
  Data:=Concat('<',Storage.UserStorage.Files.XML.Summary,'>',Data,'</',Storage.UserStorage.Files.XML.Summary,'>');

  Result:=False;
  Try
    FXMLSource:=TXMLInputSource.Create(Data);
    Try
      FXMLParser.Parse(FXMLSource,FXMLDocument);
      Try
        Result:=Storage.UserStorage.Items.SMTP.fromXML(FXMLDocument,Summary);
      finally
        FreeAndNil(FXMLDocument);
      end;
    Finally
      FreeAndNil(FXMLSource);
    end;
  Except
    on E: Exception do begin
      Result:=False;
    end;
  end;
end;

function  TIMAPManager.GetSection(var Data:Core.Strings.VarString; var Summary:Storage.UserStorage.Items.SMTP.TSummary; Index:LongInt):Core.Strings.VarString;
var
  mimeP:Storage.UserStorage.Items.SMTP.PMIME;
  iLcv:LongInt;
  iCt:LongInt;
begin
  EntryPoint:='TIMAPManager.GetSection(Data,Summary,Index)';
  Refactor.Size:=0;
  Core.Streams.Write(#13#10,2,Refactor);
  SetLength(Result,0);
  Core.Arrays.VarString.fromString(FLines,Data,#13#10,[soClearList]);
  mimeP:=@Summary.Mime;
  iCt:=Length(mimeP^.Mimes);
  if (iCt>0) and (Index<iCt) then begin
    mimeP:=mimeP^.Mimes[Index];
    for iLcv:=mimeP^.idxContentStart to mimeP^.idxContentEnd do begin
      Core.Streams.Write(FLines[iLcv],Refactor);
      Core.Streams.Write(#13#10,2,Refactor);
    end;
  end;
  If (Refactor.Size>0) then
    Refactor.Size:=Refactor.Size-2;
  Result:=Core.Streams.toString(Refactor);
  Refactor.Size:=0;
end;

function  TIMAPManager.GetTextMime(var Summary:Storage.UserStorage.Items.SMTP.TSummary):Storage.UserStorage.Items.SMTP.PMIME;
begin
  Result:=Storage.UserStorage.Items.SMTP.getMime(Summary.Mime,Storage.UserStorage.Items.SMTP.Content.ctTextHTML);
  if Result=nil then
    Result:=Storage.UserStorage.Items.SMTP.getMime(Summary.Mime,Storage.UserStorage.Items.SMTP.Content.ctTextPlain);
end;

procedure TIMAPManager.Select(RSRP:PRSR; Path:Core.Strings.VarString);
var
  iLcv:LongInt;
begin
  EntryPoint:='TIMAPManager.Select(RSRP, Path)';
  if (TransState()=true) then begin
    FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',Path);
    If ( (IMAPP^.Selected<>nil) and (SameText(FPath,IMAPP^.Selected^.Path)=false) ) then begin
      Storage.UserStorage.Files.Empty(IMAPP^.Selected^.Files);
      IMAPP^.Selected:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    end;
    if (IMAPP^.Selected=nil) then
      ReloadFolders(FPath,ifrkSelected);
    If (IMAPP^.Selected<>nil) then begin
      FSelected:=FPath;
      FFolderP:=IMAPP^.Selected;
      Expunge(RSRP,FFolderP^);

      FUNSEENID:=0;
      FRecent:=0;
      FUnseen:=0;
      FNextID:=0;
      FFLags:=0;
      FBodyCount:=0;
      for iLcv:=0 to High(FFolderP^.Files) do begin
        FFileP:=FFolderP^.Files[iLcv];
        FFlags:=FFlags or FFileP^.Flags;

        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted<>FFileP^.Flags) then begin
          if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) then
            Inc(FRecent);
          if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags) then begin
            Inc(FUnseen);
            FUNSEENID:=FFileP^.ID;
          end;
          if FFileP^.ID>FNextID then
            FNextID:=FFileP^.ID;
          Inc(FBodyCount);
        end;
      end;
      FResponse:=Respond(Concat(IntToStr(FBodyCount),' ',SELECT_RESP_EXISTS));
      Send(RSRP,FResponse);
      FResponse:=Respond(Concat(IntToStr(FRecent),' ',SELECT_RESP_RECENT));
      Send(RSRP,FResponse);

      if FUNSEENID<>0 then
        FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UNSEEN,' ',IntToStr(FUnseen),'] Message ',IntToStr(FUNSEENID),' is first unseen'))
      else
        FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UNSEEN,' ',IntToStr(FUnseen),'] Messages'));
      Send(RSRP,FResponse);

      FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UIDVALIDITY,' ',IntToStr(fFolderP^.ID),'] UIDs valid'));
      Send(RSRP,FResponse);

      if FNextID<>0 then begin
        FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('[',SELECT_RESP_UIDNEXT,' ',IntToStr(FNextID),'] next estimated UID'));
        Send(RSRP,FResponse);
      end;
      if FFlags<>0 then begin
        FResponse:=Respond(Storage.UserStorage.Items.IMAP.toString(FFlags));
        Send(RSRP,FResponse);
      end;
      FResponse:=Respond(
        SEQ_ANY,
        SSC_OK,
        Concat(
          '[',SELECT_RESP_PERMANENTFLAGS,' ',
           ' (',
              FLAG_ANSWERED,' ',
              FLAG_DELETED,' ',
              FLAG_SEEN,' ',
              FLAG_ANY,
            ')',
          '] Limited'
        )
      );
      Send(RSRP,FResponse);

      FResponse:=Respond(FSEQ,SSC_OK,'[READ-WRITE] SELECT completed.');
      Send(RSRP,FResponse);

      FFolderP^.Inspected:=Core.Timer.dtUT;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('SELECT failed: ',FPath,' does not exist.'));
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('SELECT failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
end;

procedure TIMAPManager.ID(RSRP:PRSR);
begin
  FResponse:=Respond(
   Concat(
      'ID (',
      '"name" "AuraIMAP" ',
      '"vendor" "Aurawin LLC" ',
      '"support-url" "https://aurawin.com/support/" ',
      '"edition" "',App.Build.Edition,'" ',
      '"version" "',App.Build.Version,'" ',
      '"build" "',App.Build.RSR,'" ',
      '"remote-host" "',Core.Utils.Sockets.InAddrToStr(RSRP^.Address.sin_addr.s_addr),'"',
      ')'
    )
  );
  Send(RSRP,FResponse);
  FResponse:=Respond(
    FSEQ,
    SSC_OK,
    'ID command completed.'
  );
  Send(RSRP,FResponse);
end;

procedure TIMAPManager.Search(RSRP:PRSR);
var
  iLcv:LongInt;
  iLen:LongInt;

  procedure SearchItem();
  begin
    EntryPoint:='TIMAPManager.Search(RSRP, idxCommand).SearchItem.Search_ID_Search_Execute';
    FSrchResult:=ID_Search_Execute();
    if FSrchResult<>0 then
      Core.Arrays.LargeWord.Add(iLcv,FSrchResults,aoNone);
  end;

begin
  EntryPoint:='TIMAPManager.Search(RSRP)';
  if TransState()=true then begin
    If (IMAPP^.Selected<>nil) then begin
      iLen:=Length(FCommands);
      FFolderP:=IMAPP^.Selected;
      iLcv:=SetRange();
      RSR.IMAP.Empty(FSrchTerms);

      Parse(FReadAhead,iLcv,FSrchError,FCommands,FSrchTerms);

      if (FSrchError<>-1) then begin
        FResponse:=Respond(FSEQ,SSC_NO,Concat('SEARCH misunderstood argument near the [',IntToStr(FSrchError+1),'] element of entire statement.'));
        Send(RSRP,FResponse);
        Exit();
      end;
      if Length(FSrchTerms)>0 then begin
        for iLcv:=0 to High(FFolderP^.Files) do begin
          RenewCycle();
          FFileP:=FFolderP^.Files[iLcv];
          if ( (FFileP^.ID<=FRangeEnd) and (FFileP^.ID>=FRangeStart)) then begin
            if (FFileP^.Valid=true) then begin
              if (FReadAhead=true) then begin
                if CacheMessage(FFileP^,FHeaderBreak, FHeaderCount, FHeaders, FHeaderItems) then begin
                  SearchItem();
                end;
              end else begin
                SearchItem();
              end;
            end;
          end;
        end;
        if Length(FSrchResults)>0 then begin
          FResponse:=Respond(Concat('SEARCH ',Core.Arrays.LargeWord.toString(FSrchResults,' ',Refactor)));
          Send(RSRP,FResponse);
        end;
        FResponse:=Respond(FSEQ,SSC_OK,'SEARCH completed.');
        Send(RSRP,FResponse);
      end else begin
        FResponse:=Respond(FSEQ,SSC_NO,Concat('SEARCH statement contains no parsable directives,terms, or arguments.'));
        Send(RSRP,FResponse);
        Exit();
      end;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('SEARCH failed: no folder was selected.'));
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('SEARCH failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
end;

procedure TIMAPManager.Status(RSRP:PRSR);
var
  iLcv:LongInt;
begin
  EntryPoint:='TIMAPManager.Status(RSRP)';
  if (TransState()=true) then begin
    FFolderP:=IMAPP^.Selected;
    if (FFolderP<>nil) then begin
      if Storage.UserStorage.Files.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID,FFolderP^.Files) then begin
        FFolderP^.Inspected:=Core.Timer.dtUT;
        FUNSEENID:=0;
        FRecent:=0;
        FUnseen:=0;
        FNextID:=0;
        FDeleted:=0;
        FFilesWithFlags:=0;
        FFileCount:=Length(FFolderP^.Files);
        FFlags:=0;
        for iLcv:=0 to FFileCount-1 do begin
          FFileP:=FFolderP^.Files[iLcv];
          if FFileP^.Flags<>0 then begin
            Inc(FFilesWithFlags);
            FFlags:=FFlags or FFileP^.Flags;
            if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags) then begin
              Inc(FDeleted);
            end else begin
              if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) then
                Inc(FRecent);
              if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags) then begin
                Inc(FUnseen);
                FUNSEENID:=FFileP^.ID;
              end;
            end;
          end;
          if FFileP^.ID>FNextID then
            FNextID:=FFileP^.ID+1;
        end;
        if (IMAPP^.LastDeleted=-1) or (IMAPP^.LastDeleted<>FDeleted) then begin
          FResponse:=Respond(Concat(IntToStr(FDeleted),' ',STATUS_DATA_DELETED));
          Send(RSRP,FResponse);
          IMAPP^.LastDeleted:=FDeleted;
        end;
        if (IMAPP^.LastExists=-1) or (IMAPP^.LastExists<>FFileCount) then begin
          FResponse:=Respond(Concat(IntToStr(FFileCount),' ',STATUS_DATA_EXISTS));
          Send(RSRP,FResponse);
          IMAPP^.LastExists:=FFileCount;
        end;
        if (IMAPP^.LastRecent=-1) or (IMAPP^.LastRecent<>FRecent) then begin
          FResponse:=Respond(Concat(IntToStr(FRecent),' ',STATUS_DATA_RECENT));
          Send(RSRP,FResponse);
          IMAPP^.LastRecent:=FRecent;
        end;
      end;
    end;
  end;
  SetLength(FResponse,0);
end;

procedure TIMAPManager.Empty();
begin
  RSR.IMAP.Empty(FSrchTerms);

  Core.Arrays.VarString.Empty(FRangeTerm);
  Core.Arrays.VarString.Empty(FRangePair);
  Core.Arrays.VarString.Empty(FFetch);
  Core.Arrays.VarString.Empty(FLines);
  Core.Arrays.VarString.Empty(FCommands);


  Storage.UserStorage.Items.SMTP.Empty(FSummary);
  Storage.UserStorage.Items.IMAP.Empty(FEnvelope);
  Storage.UserStorage.Items.IMAP.Empty(FBody);

  Storage.UserStorage.Folders.Empty(FRenameFolders,Storage.UserStorage.Folders.FREE_FILES);

  Core.Arrays.LargeWord.Empty(FUIDSet);
  Core.Arrays.LargeWord.Empty(FSrchResults);
  Core.Arrays.KeyString.Empty(FHeaderItems);
  Core.Arrays.KeyString.Empty(FClientAuth);
  Core.Arrays.VarString.Empty(FBodyItems);
  Core.Arrays.VarString.Empty(FHeaderFields);
  SetLength(FBodyPeek,0);
  SetLength(FBodySection,0);
  SetLength(FHeaders,0);
  SetLength(FHeader,0);
  SetLength(FHeadersConfirmed,0);
  SetLength(FHeadersOut,0);
  SetLength(FBodyFull,0);
  SetLength(FBoxFlags,0);
  SetLength(FMbxPath,0);
  SetLength(FTempRange,0);
  SetLength(FMailbox,0);
  SetLength(FDestination,0);
  SetLength(FSrchTerm,0);
  SetLength(FResponse,0);
  SetLength(FFolder,0);
  SetLength(FEntry,0);
  SetLength(FEntry2,0);
  SetLength(FData,0);

  Core.Arrays.VarString.Empty(FContent);
  FPathSkip:=0;
  FHeaderCount:=0;
  FRangeLen:=0;
  FSetLen:=0;

  FByteRangeStart:=0;
  FByteRangeSize:=0;

  FRangeKinds:=[];
  FFetchKinds:=[];
  SetLength(FFetchSection,0);
  SetLength(FFetchSectionPartial,0);
  Core.Arrays.VarString.Empty(FFetchSections);
  Core.Arrays.VarString.Empty(FFetchSectionPartials);

  FFetchPart:=0;
  FStoreIdx:=0;
  FStoreId:=0;
  FStoreFlags:=0;
  FStoreFlagMode:=isfNone;
  FDestinationFolderP:=nil;
  FNewID:=0;
  FMailboxWildChar:=false;
  FFileP:=nil;
  FFolderP:=nil;
  FMimeLen:=0;
  FMimeP:=nil;
  FMimesP:=nil;
  FSrchArgs:=[];
  FSrchArg:=srchArgUnknown;
  FSrchArgCount:=0;
  FSrchError:=-1;
  FSrchDate:=0;
  FSrchFileDate:=0;
  FSrchResult:=0;

  FSrchYear:=0;
  FSrchMonth:=0;
  FSrchDay:=0;
  FSrchHour:=0;
  FSrchMin:=0;
  FSrchSec:=0;
  FSrchMs:=0;

  FSrchFileYear:=0;
  FSrchFileMonth:=0;
  FSrchFileDay:=0;
  FSrchFileHour:=0;
  FSrchFileMin:=0;
  FSrchFileSec:=0;
  FSrchFileMs:=0;

  FBodyStream.Size:=0;
end;

procedure TIMAPManager.Noop(RSRP:PRSR);
var
  iLcv:LongInt;
begin
  if TransState()=true then begin
    FFolderP:=IMAPP^.Selected;
    if (FFolderP<>nil) then begin
      if Core.Utils.Time.DifferInMilliseconds(Core.Timer.dtUT,FFolderP^.Inspected)>APP.Consts.FOLDER_MS_REFRESH then begin
        Storage.UserStorage.Files.DB.Refresh(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID,FFolderP^.Files,Storage.UserStorage.Files.DB.IDs.ID);
        FFolderP^.Inspected:=Core.Timer.dtUT;
      end;

      FUNSEENID:=0;
      FRecent:=0;
      FUnseen:=0;
      FNextID:=0;
      FDeleted:=0;
      FFilesWithFlags:=0;
      FFileCount:=Length(FFolderP^.Files);
      FFlags:=0;
      for iLcv:=0 to FFileCount-1 do begin
        FFileP:=FFolderP^.Files[iLcv];
        if FFileP^.Flags<>0 then begin
          Inc(FFilesWithFlags);
          FFlags:=FFlags or FFileP^.Flags;
          if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags) then begin
            Inc(FDeleted);
          end else begin
            if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) then
              Inc(FRecent);
            if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags) then begin
              Inc(FUnseen);
              FUNSEENID:=FFileP^.ID;
            end;
          end;
        end;
        if FFileP^.ID>FNextID then
          FNextID:=FFileP^.ID+1;
      end;
      if (IMAPP^.LastDeleted=-1) or (IMAPP^.LastDeleted<>FDeleted) then begin
        FResponse:=Respond(Concat(IntToStr(FDeleted),' ',STATUS_DATA_DELETED));
        Send(RSRP,FResponse);
        IMAPP^.LastDeleted:=FDeleted;
      end;
      if (IMAPP^.LastExists=-1) or (IMAPP^.LastExists<>FFileCount) then begin
        FResponse:=Respond(Concat(IntToStr(FFileCount),' ',STATUS_DATA_EXISTS));
        Send(RSRP,FResponse);
        IMAPP^.LastExists:=FFileCount;
      end;
      if (IMAPP^.LastRecent=-1) or (IMAPP^.LastRecent<>FRecent) then begin
        FResponse:=Respond(Concat(IntToStr(FRecent),' ',STATUS_DATA_RECENT));
        Send(RSRP,FResponse);
        IMAPP^.LastRecent:=FRecent;
      end;
    end;
  end;
  FResponse:=Respond(FSEQ,SSC_OK,'NOOP Command complete.');
  Send(RSRP,FResponse);
  SetLength(FResponse,0);
end;


procedure TIMAPManager.Check(RSRP:PRSR);
begin
  if TransState()=true then begin
    FFolderP:=IMAPP^.Selected;
    if (FFolderP<>nil) then
      Status(RSRP,FFolderP^,false);
  end;
  FResponse:=Respond(FSEQ,SSC_OK,'Check Command complete.');
  Send(RSRP,FResponse);
  SetLength(FResponse,0);
end;

procedure TIMAPManager.CopyUID(RSRP:PRSR; FolderID:QWord);
var
  iLcv:LongInt;
  iCount:LongInt;

  procedure ProcessFile(FileID:QWord);
  begin
    if Storage.UserStorage.Files.Data(
      IMAPP^.UAP^.AuraNode,
      IMAPP^.UAP^.DomainID,
      IMAPP^.UAP^.ID,
      FolderID,
      FileID,
      FSData
    ) then begin
      Try
        FSData.Position:=0;
        If Storage.UserStorage.Files.DB.Add(
          Task,
          IMAPP^.UAP^.AuraNode,
          IMAPP^.UAP^.DomainID,
          IMAPP^.UAP^.ID,
          FolderID,
          FFile,
          FSData
        ) then begin
          if (FolderID=IMAPP^.UAP^.SpamBox) then begin
            // Process as spam
            SetLength(FEntry,0);
            FData:=FFileP^.Summary;
            Core.XML.DB.Wrap(Core.XML.DB.Header(Storage.Main.Header.Encoding),Storage.UserStorage.Items.SMTP.XML.Stanza,FData);
            Try
              FXMLSource:=TXMLInputSource.Create(FData);
              try
                FXMLParser.Parse(FXMLSource,FXMLDocument);
                if Storage.UserStorage.Items.SMTP.fromXML(FXMLDocument,FSummary) then begin
                  FEntry:=Storage.Security.Filter.getTopLevel(FSummary.RemoteDomain);
                  FEntry2:=FSummary.RemoteIP;
                end;
              Finally
                FreeAndNil(FXMLSource);
              end;
            Finally
              FreeAndNil(FXMLDocument);
            end;
            SetLength(FData,0);
            if (System.Length(FEntry)>0) then begin
              FEntry:=Lowercase(FEntry);
              Storage.Security.Filter.Empty(FSpamFilter);
              FSpamFilter.Counter:=1;
              FSpamFilter.Enabled:=True;
              FSpamFilter.Value:=FEntry;
              Storage.Security.Filter.DB.Identify(Task,secBlackList,FSpamFilter);
              Storage.Security.Filter.Empty(FSpamFilter);
            end;
            if (System.Length(FEntry2)>0) then begin
              Storage.Security.Filter.Empty(FSpamFilter);
              FSpamFilter.Counter:=1;
              FSpamFilter.Enabled:=True;
              FSpamFilter.Value:=FEntry2;
              Storage.Security.Filter.DB.Identify(Task,secViolatorIP,FSpamFilter);
              Storage.Security.Filter.Empty(FSpamFilter);
            end;
          end;
          FResponse:=Respond(FSEQ,SSC_OK,Concat('COPYUID [',IntToStr(FileID),'] copied.'));
          Send(RSRP,FResponse);
        end else begin
          FResponse:=Respond(FSEQ,SSC_NO,Concat('COPYUID [',IntToStr(FileID),'] failed.'));
          Send(RSRP,FResponse);
        end;
      finally
        FreeAndNil(FSData);
      end;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('COPYUID [',IntToStr(FileID),'] failed.'));
      Send(RSRP,FResponse);
    end;
  end;

begin
  EntryPoint:='TIMAPManager.CopyUID(RSRP, FolderID)';
  if TransState()=true then begin
    SetRange();
    FFolderP:=Storage.UserStorage.Folders.getFolder(FolderID,IMAPP^.Folders);
    if FFolderP=nil then begin
      ReloadFolders('',ifrkStatus);
      FFolderP:=Storage.UserStorage.Folders.getFolder(FolderID,IMAPP^.Folders);
    end;
    if FFolderP<>nil then begin
      Storage.UserStorage.Files.DB.List(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID,FFolderP^.Files);
      if irkRange in FRangeKinds then begin
        For iLcv:=0 to High(FFolderP^.Files) do begin
          FFileP:=FFolderP^.Files[iLcv];
          ProcessFile(FFileP^.ID);
        end;
      end;
      if irkSet in FRangeKinds then begin
        For iLcv:=0 to High(FFolderP^.Files) do begin
          FFileP:=FFolderP^.Files[iLcv];
          if (Core.Arrays.LargeWord.IndexOf(FFileP^.ID,FUIDSet)<>-1) then begin
            FFileP^.Flags:=FStoreFlags;
            ProcessFile(FFileP^.ID);
          end;
        end;
      end;
      FResponse:=Respond(FSEQ,SSC_OK,'COPYUID complete.');
      Send(RSRP,FResponse);
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,'COPYUID failed: UIDVALIDITY does not exist.');
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,'COPYUID failed: you must first login.');
    Send(RSRP,FResponse);
  end;
  SetLength(FResponse,0);
end;

procedure TIMAPManager.Created(RSRP:PRSR; Folder:string);
begin
  EntryPoint:='TIMAPManager.Created(RSRP Folder)';
  if TransState()=true then begin
    //Storage.UserStorage.Folders.Defaults.Mail.Inbox;
    FFolder:=Folder;
    if Core.Strings.SameText(Folder,Storage.UserStorage.Folders.Defaults.Mail.Junk) then
      FFolder:=Storage.UserStorage.Folders.Defaults.Mail.Spam;

    FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',FFolder);
    FFolderP:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    if (FFolderP=nil) then begin
      // Will reset list with files too.
      FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail);
      ReloadFolders(FPath,ifrkStatus);
      FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',FFolder);
      FFolderP:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    end;
    If (FFolderP=nil) then begin
      FFolderID:=0;
      if Storage.UserStorage.Folders.DB.Create(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderID,FPath) then begin
        ReloadFolders(FPath,ifrkStatus);
        FResponse:=Respond(FSEQ,SSC_OK,'CREATE completed.');
        Send(RSRP,FResponse);
     end else begin
        FResponse:=Respond(FSEQ,SSC_NO,Concat('CREATE was not able to create [',Folder,']'));
        Send(RSRP,FResponse);
      end;
    end else begin
      FResponse:=Respond(FSEQ,SSC_OK,Concat('CREATE ',Folder,' already exists.'));
      Send(RSRP,FResponse);
      FResponse:=Respond(Concat('STATUS ',Folder,' (',STATUS_DATA_UIDVALIDITY,' ',IntToStr(FFolderP^.ID),')'));
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('CREATE failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
  SetLength(FResponse,0);
end;

procedure TIMAPManager.Status(RSRP:PRSR; Path:Core.Strings.VarString);
begin
  EntryPoint:='TIMAPManager.Status(RSRP, Path)';
  if TransState()=true then begin
    IMAPP^.Status:=Storage.UserStorage.Folders.getFolder(Path,IMAPP^.Folders);
    if (IMAPP^.Status=nil) then
      ReloadFolders(Path,ifrkStatus);
    if IMAPP^.Status<>nil then
      Status(RSRP,IMAPP^.Status^,true);
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('STATUS failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
end;

procedure TIMAPManager.Status(RSRP:PRSR; var Folder:Storage.UserStorage.Folders.TFolder; Const AsCommand:boolean);
var
  iLcv:LongInt;
begin
  if TransState()=true then begin
    If (IMAPP^.Status<>nil) and (IMAPP^.Status<>@Folder) then begin
      Storage.UserStorage.Files.Empty(IMAPP^.Status^.Files);
      IMAPP^.Status:=nil;
    end;
    FStatus:=Folder.Path;
    IMAPP^.Status:=@Folder;
    FFolderP:=IMAPP^.Status;
    FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/');
    FPathSkip:=System.Length(FPath);
    FPathLen:=Length(FFolderP^.Path);
    FPath:=System.Copy(FFolderP^.Path,FPathSkip+1,FPathLen-FPathSkip);

    if Storage.UserStorage.Files.DB.List(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFolderP^.ID,FFolderP^.Files) then begin
      FUNSEENID:=0;
      FRecent:=0;
      FUnseen:=0;
      FNextID:=0;
      FDeleted:=0;
      for iLcv:=0 to High(FFolderP^.Files) do begin
        FFileP:=FFolderP^.Files[iLcv];
        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted=FFileP^.Flags) then begin
          Inc(FDeleted);
        end else begin
          if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags) then
            Inc(FRecent);
          if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>FFileP^.Flags) then begin
            Inc(FUnseen);
            FUNSEENID:=FFileP^.ID;
          end;
        end;
        if FFileP^.ID>FNextID then
          FNextID:=FFileP^.ID+1;
      end;
      FResponse:=Concat(SEQ_ANY,' STATUS "',FPath,'" (');
      FResponse:=Concat(FResponse,STATUS_DATA_MESSAGES,' ',IntToStr(Length(IMAPP^.Status^.Files)),' ');
      FResponse:=Concat(FResponse,STATUS_DATA_RECENT,' ',IntToStr(FRecent),' ');
      if FNextID>0 then
        FResponse:=Concat(FResponse,STATUS_DATA_UIDNEXT,' ',IntToStr(FNextID),' ');
      FResponse:=Concat(FResponse,STATUS_DATA_UIDVALIDITY,' ',IntToStr(FFolderP^.ID),' ');
      FResponse:=Concat(FResponse,STATUS_DATA_UNSEEN,' ',IntToStr(FUnseen));
      FResponse:=Concat(FResponse,')',#13#10);
      Send(RSRP,FResponse);
      if AsCommand=true then begin
        FResponse:=Respond(FSEQ,SSC_OK,'STATUS completed.');
        Send(RSRP,FResponse);
      end;
      //Storage.UserStorage.Files.Empty(FFolderP^.Files);
    end else if AsCommand=true then begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('STATUS failed: ','cannot retrieve files in ',FPath));
      Send(RSRP,FResponse);
    end;
  end else if AsCommand=true then begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('STATUS failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
end;

function TIMAPManager.SetRange():LongInt;
var
  iLcv:LongInt;
  qStart:QWord;
  qEnd:QWord;
  qLcv:QWord;
  iLen:LongInt;
  sTerm:Core.Strings.VarString;

  idxUID:LongInt;
  idxFetch:LongInt;
  idxStore:LongInt;
  idxSearch:LongInt;
  idxBefore:LongInt;
  idxSince:LongInt;
  idxCopy:LongInt;
begin
  EntryPoint:='TIMAPManager.SetRange()';
  Result:=-1;
  SetLength(sTerm,0);
  idxBefore:=Core.Arrays.VarString.IndexOf(@FCommands,'before');
  idxSince:=Core.Arrays.VarString.IndexOf(@FCommands,'since');
  idxUID:=Core.Arrays.VarString.IndexOf(@FCommands,'uid');
  idxFetch:=Core.Arrays.VarString.IndexOf(@FCommands,'fetch');
  idxSearch:=Core.Arrays.VarString.IndexOf(@FCommands,'search');
  idxStore:=Core.Arrays.VarString.IndexOf(@FCommands,'store');
  idxCopy:=Core.Arrays.VarString.IndexOf(@FCommands,'copy');
  if (idxUID<>-1) then begin
    if (idxFetch<>-1) then begin
      sTerm:=FCommands[idxFetch+1];
      Result:=idxFetch+2;
    end else if (idxSearch<>-1) then begin
      if Core.Strings.SameText(FCommands[idxSearch+1],RSR.IMAP.Command.UID) then
        idxSearch+=1;
      if ( (idxBefore<>-1) and (idxBefore+1<FCommandCount) ) then begin
        sTerm:=FCommands[idxBefore+1];
        Result:=idxBefore;
      end else if ((idxSince<>-1) and (idxSince+1<FCommandCount)) then begin
        sTerm:=FCommands[idxSince+1];
        Result:=idxSince;
      end else begin
        sTerm:=FCommands[idxSearch+1];
        Result:=idxSearch+2;
      end;
    end else if (idxStore<>-1) then begin
      sTerm:=FCommands[idxStore+1];
      Result:=idxStore+2;
    end else if (idxCopy<>-1) then begin
      sTerm:=FCommands[idxCopy+1];
      Result:=idxCopy+2;
    end;
  end else begin
    if (idxSearch<>-1) then begin
      sTerm:=FCommands[idxSearch+1];
      Result:=idxSearch+2;
    end else if (idxSearch<>-1) then begin
      sTerm:=FCommands[idxSearch+1];
      Result:=idxFetch+2;
    end else if (idxStore<>-1) then begin
      sTerm:=FCommands[idxStore+1];
      Result:=idxStore+2;
    end else if (idxCopy<>-1) then begin
      sTerm:=FCommands[idxCopy+1];
      Result:=idxCopy+2;
    end;
  end;
  if Core.Strings.Pos(',',sTerm)>0 then begin
    FRangeKinds+=[irkSet];
    If Core.Strings.Pos(':',sTerm)>0 then begin
      Core.Arrays.VarString.fromString(FRangeTerm,sTerm,',',[soClearList]);
      repeat
        FStoreIdx:=Core.Arrays.VarString.Search(FRangeTerm,':');
        if FStoreIdx<>-1 then begin
          FTempRange:=FRangeTerm[FStoreIdx];
          Core.Arrays.VarString.Remove(FRangeTerm,FStoreIdx);
          iLen:=Core.Arrays.VarString.fromString(FRangePair,FTempRange,':',[soClearList]);
          if iLen=2 then begin
            qStart:=StrToQWordDef(FRangePair[0],0);
            qEnd:=StrToQWordDef(FRangePair[1],0);
            for qLcv:=qStart to qEnd do
              Core.Arrays.LargeWord.Add(qLcv,FUIDSet);
          end;
        end;
      until (FStoreIdx=-1);

      for iLcv:=0 to High(FRangeTerm) do begin
        FStoreId:=StrToQWordDef(FRangeTerm[iLcv],0);
        if FStoreId<>0 then
          Core.Arrays.LargeWord.Add(FStoreId,FUIDSet);
      end;
    end else
      Core.Arrays.LargeWord.fromString(sTerm,FUIDSet,',');
    FSetLen:=Length(FUIDSet);
  end else if Core.Strings.Pos(':',sTerm)>0 then begin
    Core.Arrays.VarString.fromString(FRangeTerm,sTerm,':',[soClearList]);
    FRangeKinds+=[irkRange];
    FRangeLen:=Length(FRangeTerm);
  end else if StrToQWordDef(sTerm,0)>0 then begin
    SetLength(FRangeTerm,1);
    FRangeTerm[0]:=sTerm;
    FRangeKinds+=[irkRange];
    FRangeLen:=1;
  end else begin
    SetLength(FRangeTerm,2);
    FRangeTerm[0]:='1';
    FRangeKinds+=[irkRange];
    FRangeLen:=2;
  end;
  case FRangeLen of
    1: begin
         FRangeStart:=StrToQWordDef(FRangeTerm[0],1);
         FRangeEnd:=FRangeStart;
       end;
    2: begin
         FRangeStart:=StrToQWordDef(FRangeTerm[0],1);
         FRangeEnd:=StrToQWordDef(FRangeTerm[1],High(FRangeEnd));
       end;
  end;
end;

function  TIMAPManager.CacheMessage(var &File:Storage.UserStorage.Files.TItem; out HeaderIndex,HeaderCount:LongInt; out sHeaders:Core.Strings.VarString; out kplHeaders:Core.Arrays.Types.KeyStrings):Boolean;
begin
  EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders)';
  Try
    SetLength(FHeadersOut,0);

    RenewCycle();
    EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders) Getting Data';
    Try
      Storage.UserStorage.Files.Data(IMAPP^.UAP^.AuraNode,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,&File.FolderID,&File.ID,FSData);
      EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders) Assigning Data';
      FBodyFileName:=FSData.FileName;
      Core.Streams.Copy(FSData,FBodyStream);
    Finally
      FreeAndNil(FSData);
    end;
    EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders) Header Index';
    HeaderIndex:=Core.Streams.Pos(FBodyStream,Storage.UserStorage.Items.SMTP.HeaderBreak);
    EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders) Header Extract';
    Core.Streams.Extract(FBodyStream,0,HeaderIndex-1,sHeaders);
    EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders) fromString';
    HeaderCount:=Core.Arrays.KeyString.fromString(kplHeaders,sHeaders,': ',#13#10,[soClearList]);
    sHeaders:=Concat(sHeaders,#13#10#13#10);
    Result:=(HeaderCount>0);
  Except
    On E:Exception do begin
      Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(EntryPoint,' Exception:',E.Message));
    end;
  end;
  EntryPoint:='TIMAPManager.CacheMessage(File HeaderIndex,HeaderCount,sHeaders,kplHeaders) Done.';
end;

procedure TIMAPManager.OutputFetchFull(RSRP:PRSR; const Index:LongInt; var &File:Storage.UserStorage.Files.TItem; var Envelope:Storage.UserStorage.Items.IMAP.TEnvelope; var Body:Storage.UserStorage.Items.IMAP.TBodyElement);
begin
  FResponse:=Respond(
    Concat(
      IntToStr(Index),' ',
      'FETCH (',
        Storage.UserStorage.Items.IMAP.toString(&File.Flags),' ',
        Storage.UserStorage.Items.IMAP.toString(&File.Created),' ',
        Storage.UserStorage.Items.IMAP.toString(&File.Size),' ',
        Storage.UserStorage.Items.IMAP.toString(Envelope,Refactor),
        'BODY ',Storage.UserStorage.Items.IMAP.toString(Body),
      ')'
    )
  );
  Send(RSRP,FResponse);
  SetLength(FResponse,0);
end;

procedure TIMAPManager.OutputFetchAll(RSRP:PRSR; const Index:LongInt; var &File:Storage.UserStorage.Files.TItem; var Envelope:Storage.UserStorage.Items.IMAP.TEnvelope);
begin
  FResponse:=Respond(
    Concat(
      IntToStr(Index),' ',
      'FETCH (',
        Storage.UserStorage.Items.IMAP.toString(&File.Flags),' ',
        Storage.UserStorage.Items.IMAP.toString(&File.Created),' ',
        Storage.UserStorage.Items.IMAP.toString(&File.Size),' ',
        Storage.UserStorage.Items.IMAP.toString(Envelope,Refactor),
      ')'
    )
  );
  Send(RSRP,FResponse);
  SetLength(FResponse,0);
end;

procedure TIMAPManager.OutputFetchFast(RSRP:PRSR; const Index:LongInt; var &File:Storage.UserStorage.Files.TItem);
begin
  FResponse:=Respond(
    Concat(
      IntToStr(Index),' ',
      'FETCH (',
        Storage.UserStorage.Items.IMAP.toString(&File.Flags),' ',
        Storage.UserStorage.Items.IMAP.toString(&File.Created),' ',
        Storage.UserStorage.Items.IMAP.toString(&File.Size),
      ')'
    )
  );
  Send(RSRP,FResponse);
  SetLength(FResponse,0);
end;

procedure TIMAPManager.OutputFetchBodyStructure(RSRP:PRSR; const Index:LongInt; var Body:Storage.UserStorage.Items.IMAP.TBodyElement);
begin
  FResponse:=Respond(
    Concat(
      IntToStr(Index),' ',
      'FETCH BODYSTRUCTURE (', Storage.UserStorage.Items.IMAP.toString(Body,true),')'
    )
  );
  Send(RSRP,FResponse);
  SetLength(FResponse,0);
end;

procedure TIMAPManager.OutputFetchFlag(RSRP:PRSR; Index:LongInt; ID:QWord; Flags:LongInt);
begin
  FResponse:=Concat(
      SEQ_ANY,' ',
      IntToStr(Index),' ',
      'FETCH (',
      'UID ',IntToStr(ID),' ',
      Storage.UserStorage.Items.IMAP.toString(Flags),
      ')',#13#10
  );
  Send(RSRP,FResponse);
end;

procedure TIMAPManager.OutputFetchFlags(RSRP:PRSR; Range:TIMAPRangeKinds; var Folder:Storage.UserStorage.Folders.TFolder; var Files:Storage.UserStorage.Files.TItems);
var
  iLcv:LongInt;
  FileIndex:LongInt;
  StatusUpdateNeeded:Boolean;
begin
  EntryPoint:='TIMAPManager.OutputFetchFlags(RSRP, Range, Folder, Files)';
  StatusUpdateNeeded:=false;
  if irkSet in Range then begin
    for iLcv:=0 to High(FUIDSet) do begin
      FFileID:=FUIDSet[iLcv];
      FileIndex:=Storage.UserStorage.Files.IndexOf(FFileID,Files);
      if FileIndex<>-1 then begin
        FFileP:=Files[FileIndex];
        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted)<>FFileP^.Flags then
          OutputFetchFlag(RSRP,FileIndex+1,FFileID,FFileP^.Flags);
      end else begin
        StatusUpdateNeeded:=True;
      end;
    end;
  end;
  if irkRange in Range then begin
    for iLcv:=0 to High(Files) do begin
      FFileID:=Files[iLcv]^.ID;
      StatusUpdateNeeded:=true;
      If (FFileID>=FRangeStart) and (FFileID<=FRangeEnd) then begin
        FFileP:=Files[iLcv];
        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted)<>FFileP^.Flags then
          OutputFetchFlag(RSRP,iLcv+1,FFileID,FFileP^.Flags);
        StatusUpdateNeeded:=false;
      end;
    end;
  end;
  FResponse:=Respond(FSEQ,SSC_OK,'FETCH completed.');
  Send(RSRP,FResponse);

  if (StatusUpdateNeeded=true) then
    Status(RSRP);
end;

procedure TIMAPManager.saFromStringProgress(Index,Total:Int64);
begin
   RenewCycle();
end;

procedure TIMAPManager.OutputFetchDeep(RSRP:PRSR; Range:TIMAPRangeKinds; var Folder:Storage.UserStorage.Folders.TFolder; var Files:Storage.UserStorage.Files.TItems);
var
  iLcv : LongInt;
  FileIndex:LongInt;
  StatusUpdateNeeded:boolean;

  procedure ProcessFileFlags();
  begin
    FFileP^.Flags:=FFileP^.Flags and not Storage.UserStorage.Items.IMAP.Flags.Recent;
    FFileP^.Flags:=FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen;
    Storage.UserStorage.Files.DB.SetFlags(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FFileP^.Flags);
  end;

  procedure ProcessFileRecentFlag();
  begin
    if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent=FFileP^.Flags)=false then begin
      FFileP^.Flags:=FFileP^.Flags and not Storage.UserStorage.Items.IMAP.Flags.Recent;
      Storage.UserStorage.Files.DB.SetFlags(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FFileP^.Flags);
    end;
  end;

  procedure PushOutput();
  var
    jLcv : LongInt;
    SpaceNeeded:Boolean;
  begin
    EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP,Range,Folder,Files)';
    Storage.UserStorage.Items.SMTP.Empty(FSummary);
    Storage.UserStorage.Items.IMAP.Empty(FEnvelope);
    Storage.UserStorage.Items.IMAP.Empty(FBody);
    SpaceNeeded:=False;
    if CacheMessage(FFileP^,FHeaderBreak, FHeaderCount, FHeaders, FHeaderItems) then begin
      EntryPoint:=Concat('TIMAPManager.OutputFetchDeep(RSRP,Range,Folder,Files) FContent.fromString(',FFileP^.Name,')');
      RenewCycle();
      FLength:=Core.Arrays.VarString.fromStream(FContent,FBodyStream,#13#10,[soClearList]);


      EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP,Range,Folder,Files) Parse Summary';
      ParseSummary(FFileP^.Summary,FSummary);
      EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP,Range,Folder,Files) Parse Envelope';
      ParseEnvelope(FLines,FSummary,FEnvelope);
      EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP,Range,Folder,Files) Parse Body';
      ParseBody(FSummary,FBody);

      EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP,Range,Folder,Files) Building Fetch Result...';

      FResponse:=Concat(SEQ_ANY,' ',IntToStr(FileIndex+1),' FETCH (');

      if ifkUID in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          'UID ',IntToStr(FFileP^.ID)
        );
        SpaceNeeded:=True;
      end;
      if ifkFlags in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          Storage.UserStorage.Items.IMAP.toString(FFileP^.Flags)
        );
        SpaceNeeded:=True;
      end;
      if ifkRFC822Size in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          Storage.UserStorage.Items.IMAP.toString(FFileP^.Size)
        );
        SpaceNeeded:=True;
      end;
      if ifkInternalDate in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          Storage.UserStorage.Items.IMAP.toString(FFileP^.Created)
        );
        SpaceNeeded:=True;
      end;
      if ifkBodyStructure in FFetchKinds then begin
        FBodyPeek:=Storage.UserStorage.Items.IMAP.toString(FBody,true);
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          'BODYSTRUCTURE ',FBodyPeek
        );
        SpaceNeeded:=True;
        ProcessFileRecentFlag();
      end;
      if ifkBodyPeekHeader in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          'BODY[HEADER] {',IntToStr(Length(FHeaders)),'}'#13#10,
          FHeaders
        );
        SpaceNeeded:=False;
      end;
      if ifkBodyHeader in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          'BODY[HEADER] {',IntToStr(Length(FHeaders)),'}'#13#10,
          FHeaders
        );
        SpaceNeeded:=False;
        ProcessFileRecentFlag();
      end;
      if ifkRFC822Header in FFetchKinds then begin
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          'RFC822.HEADER {',IntToStr(Length(FHeaders)),'}'#13#10,
          FHeaders
        );
        SpaceNeeded:=False;
        ProcessFileRecentFlag();
      end;
      if ifkBodyHeaderFields in FFetchKinds then begin
        SetLength(FHeadersConfirmed,0);
        SetLength(FHeadersOut,0);
        for jLcv:=0 to High(FHeaderFields) do begin
          FHeaderP:=Core.Arrays.KeyString.GetItem(FHeaderItems,FHeaderFields[jLcv],FHeaderCount);
          if FHeaderP<>nil then begin
            FHeadersOut:=Concat(FHeadersOut,FHeaderP^.Key,': ',FHeaderP^.Value,#13#10);
            FHeadersConfirmed:=Concat(FHeadersConfirmed,FHeaderP^.Key,#32);
          end;
        end;
        Core.Strings.TrimRight(FHeadersConfirmed);
        FHeadersOut:=Concat(FHeadersOut,#13#10);
        FResponse:=Concat(
          FResponse,
          Commands.Response.SpacePadding[SpaceNeeded],
          'BODY[HEADER.FIELDS (',FHeadersConfirmed,')] ',
          '{',IntToStr(Length(FHeadersOut)),'}'#13#10,
          FHeadersOut
        );
        SpaceNeeded:=False;
        ProcessFileRecentFlag();
      end;
      if ifkBodySection in FFetchKinds then begin
        FFetchLcv:=0;
        FFetchLen:=Length(FFetchSections);
        FMimeP:=@FSummary.Mime;
        FMimesP:=@FMimeP^.Mimes;
        FMimeLen:=Length(FMimesP^);
        if FFetchLen>0 then begin
          While (FFetchLcv<FFetchLen) and (FMimesP<>nil) do begin
            FFetchPart:=StrToIntDef(FFetchSections[FFetchLcv],1)-1;
            if (FFetchPart>-1) then begin
              if FFetchPart<Length(FMimesP^) then begin
                FMimeP:=FMimesP^[FFetchPart];
                FMimesP:=@FMimeP^.Mimes;
              end else begin
                FMimeP:=nil;
                FMimesP:=nil;
              end;
            end;
            Inc(FFetchLcv);
          end;
        end;
        if (FMimeP=nil) then begin
          FMimeP:=@FSummary.Mime;
          FMimesP:=@FMimeP^.Mimes;
          FMimeLen:=Length(FMimesP^);
        end;

        if (FMimeP^.idxContentEnd<FLength) then begin
          SetLength(FBodyPeek,0);
          Storage.UserStorage.Examine(FMimeP^,FBodyStream,FStreamStart,FStreamEnd,FBodySize);
          if ifkBodySectionPartial in FFetchKinds then begin
            FResponse:=Concat(
              FResponse,
              Commands.Response.SpacePadding[SpaceNeeded],
              'BODY[',FFetchSection,'] {',IntToStr(FBodySize),'}'#13#10
            );
          end else begin
            FResponse:=Concat(
              FResponse,
              Commands.Response.SpacePadding[SpaceNeeded],
              'BODY[',FFetchSection,'] {',IntToStr(FBodySize),'}'#13#10
            );
          end;
          SpaceNeeded:=True;
          Send(RSRP,FResponse);
          SetLength(FResponse,0);
          try
            Send(RSRP,FBodyStream,FStreamStart,FBodySize);
          except
            On E:Exception do begin
              Core.Logging.Native.WriteLogEntry(
                Owner.RootDomain.Name,
                FService,
                Concat(
                  'TIMAPManager.OutputFetchDeep: ',
                  'Query=[',CMDS,'] ',
                  'Path=',FBodyFileName,' ',
                  'FStreamStart=',IntToStr(FStreamStart),' ',
                  'FBodyCount=',IntToStr(FBodyCount),' ',
                  'FBodyStream.Size=',IntToStr(FBodyStream.Size)
                )
              );
            end;
          end;

          ProcessFileRecentFlag();
        end;
      end;
      if ifkBody in FFetchKinds then begin
        SetLength(FBodyPeek,0);
        FResponse:=Concat(
            FResponse,
            Commands.Response.SpacePadding[SpaceNeeded],
            'BODY[] {',IntToStr(FBodyStream.Size),'}'#13#10
        );
        Send(RSRP,FResponse);
        Send(RSRP,FBodyStream);
        SetLength(FResponse,0);
        SpaceNeeded:=True;
        if (ifkBodyPeek in FFetchKinds)=false then
          ProcessFileFlags();
      end;
      if ifkBodyText in FFetchKinds then begin
        FMimeP:=@FSummary.Mime;
        if (FMimeP^.idxContentEnd<FLength) then begin
          SetLength(FBodyPeek,0);
          Storage.UserStorage.Examine(FMimep^,FBodyStream,FStreamStart,FStreamEnd,FBodySize);
          FResponse:=Concat(
            FResponse,
            Commands.Response.SpacePadding[SpaceNeeded],
            'BODY[TEXT] {',IntToStr(FBodySize),'}'#13#10
          );
          Send(RSRP,FResponse);
          Send(RSRP,FBodyStream,FStreamStart,FBodySize);
          FResponse:='';
          SpaceNeeded:=False;
          if (ifkBodyPeekText in FFetchKinds=false) then
            ProcessFileFlags();
        end;
      end;
      FResponse:=Concat(FResponse,')'#13#10);
      Send(RSRP,FResponse);
      SetLength(FResponse,0);
    end;
  end;

begin
  EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP, Range, Folder, Files)';
  StatusUpdateNeeded:=false;

  if irkSet in Range then begin
    for iLcv:=0 to High(FUIDSet) do begin
      FFileID:=FUIDSet[iLcv];
      FileIndex:=Storage.UserStorage.Files.IndexOf(FFileID,Files);
      if FileIndex<>-1 then begin
        FFileP:=Files[FileIndex];
        if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted)<>FFileP^.Flags then begin
          PushOutput();
        end;
      end else begin
        StatusUpdateNeeded:=true;
      end;
    end;
    SetLength(FResponse,0);
  end;
  if irkRange in FRangeKinds then begin
    if irkRange in Range then begin
      for iLcv:=0 to High(Files) do begin
        FFileID:=Files[iLcv]^.ID;
        FileIndex:=iLcv;
        StatusUpdateNeeded:=true;
        If (FFileID>=FRangeStart) and (FFileID<=FRangeEnd) then begin
          FFileP:=Files[FileIndex];
          if (FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted)<>FFileP^.Flags then begin
            PushOutput();
            StatusUpdateNeeded:=false;
          end;
        end;
      end;
    end;
  end;
  FResponse:=Respond(FSEQ,SSC_OK,'FETCH completed.');
  Send(RSRP,FResponse);
  Core.Streams.toFile(RSRP^.SendBuffer.Stream,'/home/atbrunner/buffer.txt');
  CMDS:=CMDS;
  if (StatusUpdateNeeded=true) then
    Status(RSRP);
  EntryPoint:='TIMAPManager.OutputFetchDeep(RSRP, Range, Folder, Files).Done';
end;

Function TIMAPManager.TransState:Boolean;
begin
  Result:=(IMAPP^.State or RS_AUTHENTICATED)=IMAPP^.State;
end;

function TIMAPManager.Capabilities:Core.Strings.VarString;
begin
  Result:=Concat(
    'IMAP4rev1',' ',
    'STARTTLS',' ',
    'ID',' ',
    'AUTH=PLAIN',' ',
    'AUTH=LOGIN',' ',
    'LITERAL+',' ',
    'LOGIN',' ',
    'UIDPLUS'
  );
  //if (Owner.Secure=false) then
    //Result:=Concat(Result,' ','LOGINDISABLED');
end;

procedure TIMAPManager.ID_Expunge(RSRP:PRSR);
var
  iLcv:LongInt;
begin
  EntryPoint:='TIMAPManager.ID_Expunge(RSRP)';
  FFetchKinds:=[];
  SetRange();
  if irkSet in FRangeKinds then begin
    for iLcv:=0 to High(FFolderP^.Files) do begin
      FFileP:=FFolderp^.Files[iLcv];
      if (Core.Arrays.LargeWord.IndexOf(FFileP^.ID,FUIDSet)<>-1) then begin
        {$i uIMAPManager.Process.UID.Expunge.Respond.inc}
      end;
    end;
  end;
  if irkRange in FRangeKinds then begin
    for iLcv:=0 to High(FFolderP^.Files) do begin
      FFileP:=FFolderp^.Files[iLcv];
      if (FFileP^.ID<=FRangeEnd) and (FFileP^.ID>=FRangeStart) then begin
        {$i uIMAPManager.Process.UID.Expunge.Respond.inc}
      end;
    end;
  end;
  FResponse:=Respond(FSEQ,SSC_OK,Concat('EXPUNGE completed.'));
  Send(RSRP,FResponse);
end;

procedure TIMAPManager.ID_Copy(RSRP:PRSR);
var
  iLcv:LongInt;


  procedure SetupEntries();
  begin
    SetLength(FEntry,0);
    SetLength(FEntry2,0);
    FData:=FFileP^.Summary;
    Core.XML.DB.Wrap(Core.XML.DB.Header(Storage.Main.Header.Encoding),Storage.UserStorage.Items.SMTP.XML.Stanza,FData);
    Try
      FXMLSource:=TXMLInputSource.Create(FData);
      try
        FXMLParser.Parse(FXMLSource,FXMLDocument);
        if Storage.UserStorage.Items.SMTP.fromXML(FXMLDocument,FSummary) then begin
          FEntry:=Storage.Security.Filter.getTopLevel(FSummary.RemoteDomain);
          FEntry2:=FSummary.RemoteIP;
        end;
      Finally
        FreeAndNil(FXMLSource);
      end;
    Finally
      FreeAndNil(FXMLDocument);
    end;
    SetLength(FData,0);
  end;

  procedure BlacklistEntries();
  begin
    if (System.Length(FEntry)>0) then begin
      FEntry:=Lowercase(FEntry);
      Storage.Security.Filter.Empty(FSpamFilter);
      FSpamFilter.Counter:=1;
      FSpamFilter.Enabled:=True;
      FSpamFilter.Value:=FEntry;
      Storage.Security.Filter.DB.Identify(Task,secBlacklist,FSpamFilter);
      Storage.Security.Filter.Empty(FSpamFilter);
    end;
    if (System.Length(FEntry2)>0) then begin
      Storage.Security.Filter.Empty(FSpamFilter);
      FSpamFilter.Counter:=1;
      FSpamFilter.Enabled:=True;
      FSpamFilter.Value:=Core.Utils.Sockets.MaskClassC(FEntry2);
      Storage.Security.Filter.DB.Identify(Task,secViolatorIP,FSpamFilter);
      Storage.Security.Filter.Empty(FSpamFilter);
    end;
  end;

  procedure WhitelistEntries();
  begin
    if (System.Length(FEntry)>0) then begin
      FEntry:=Lowercase(FEntry);
      Storage.Security.Filter.Empty(FSpamFilter);
      FSpamFilter.Counter:=1;
      FSpamFilter.Enabled:=True;
      FSpamFilter.Value:=FEntry;
      Storage.Security.Filter.DB.Blacklist_Delist(Task,FEntry);
      Storage.Security.Filter.DB.Identify(Task,secWhitelist,FSpamFilter);
      Storage.Security.Filter.Empty(FSpamFilter);
    end;
    if (System.Length(FEntry2)>0) then begin
      FRemoteIP:=Core.Utils.Sockets.InAddrFromStr(FEntry2);
      Storage.Intrusion.Intruder.DB.Delete(Task,Owner.RootDomain.ID,FRemoteIP);
      Storage.Security.Filter.DB.Violator_Delist(Task,FEntry2);
    end;
  end;

begin
  EntryPoint:='TIMAPManager.ID_Copy(RSRP)';
  SetRange();

  FDestination:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP3);
  FDestinationFolderP:=Storage.UserStorage.Folders.getFolder(FDestination,IMAPP^.Folders);
  if (FDestinationFolderP=nil) then begin
    ReloadFolders(Storage.UserStorage.Folders.Defaults.Home.Mail,ifrkStatus);
    FDestinationFolderP:=Storage.UserStorage.Folders.getFolder(FDestination,IMAPP^.Folders);
  end;

  if (FDestinationFolderP<>nil) then begin
    if irkRange in FRangeKinds then begin
      For iLcv:=0 to High(FFolderP^.Files) do begin
        FFileP:=FFolderP^.Files[iLcv];
        if ( (FFileP^.ID<=FRangeEnd) and (FFileP^.ID>=FRangeStart)) then begin
          Storage.UserStorage.Files.DB.Copy(Task,IMAPP^.UAP^.AuraNode,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FDestinationFolderP^.ID,FNewID);
          if FDestinationFolderP^.ID=IMAPP^.UAP^.TrashBox then begin
            FFileP^.Flags:=FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted;
            Storage.UserStorage.Files.DB.SetFlags(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FFileP^.Flags);
          end else if (FDestinationFolderP^.ID=IMAPP^.UAP^.SpamBox) then begin
            // Process as spam
            SetLength(FEntry,0);
            SetupEntries();
            BlacklistEntries();
          end else if (FDestinationFolderP^.ID=IMAPP^.UAP^.InBox) and (FFolderP^.ID=IMAPP^.UAP^.SpamBox) then begin
            SetupEntries();
            WhitelistEntries();
          end;
        end;
      end;

    end;
    if irkSet in FRangeKinds then begin
      For iLcv:=0 to High(FFolderP^.Files) do begin
        FFileP:=FFolderP^.Files[iLcv];
        if (Core.Arrays.LargeWord.IndexOf(FFileP^.ID,FUIDSet)<>-1) then begin
          Storage.UserStorage.Files.DB.Copy(Task,IMAPP^.UAP^.AuraNode,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FDestinationFolderP^.ID,FNewID);
          if FDestinationFolderP^.ID=IMAPP^.UAP^.TrashBox then begin
            FFileP^.Flags:=FFileP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Deleted;
            Storage.UserStorage.Files.DB.SetFlags(Task,IMAPP^.UAP^.DomainID,IMAPP^.UAP^.ID,FFileP^.ID,FFileP^.Flags);
          end else if (FDestinationFolderP^.ID=IMAPP^.UAP^.SpamBox) then begin
            SetupEntries();
            BlacklistEntries();
          end else if (FDestinationFolderP^.ID=IMAPP^.UAP^.InBox) and (FFolderP^.ID=IMAPP^.UAP^.SpamBox) then begin
            SetupEntries();
            WhitelistEntries();
          end;
        end;
      end;
    end;
    FResponse:=Respond(FSEQ,SSC_OK,'COPY completed.');
    Send(RSRP,FResponse);
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('COPY failed: unable to find folder "',CMDP3,'"'));
    Send(RSRP,FResponse);
  end;
end;

procedure TIMAPManager.ID_Store(RSRP:PRSR);
var
  iLcv:LongInt;
begin
  EntryPoint:='TIMAPManager.ID_Store(RSRP)';
  SetRange();

  if Core.Strings.SameText(CMDP3,RSR.IMAP.Command.Parameter.FlagsSilentOn) then
    FStoreFlagMode:=isfPlusSilent
  else if Core.Strings.SameText(CMDP3,RSR.IMAP.Command.Parameter.FlagsSilentOff) then
    FStoreFlagMode:=isfMinusSilent
  else if SameText(CMDP3,RSR.IMAP.Command.Parameter.FlagsOn) then
    FStoreFlagMode:=isfPlus
  else if SameText(CMDP3,RSR.IMAP.Command.Parameter.FlagsOff)then
    FStoreFlagMode:=isfMinus
  else if SameText(CMDP3,RSR.IMAP.Command.Parameter.FlagsSilent) then
    FStoreFlagMode:=isfAllSilent
  else if SameText(CMDP3,RSR.IMAP.Command.Parameter.Flags) then
    FStoreFlagMode:=isfAll;

  FStoreFlags:=RSR.IMAP.GetFlags(CMDP4);

  Case FStoreFlagMode of
    isfAll         : begin
                      {$i uIMAPManager.Process.UID.Store.SetFlags.All.inc}
                     end;
    isfAllSilent   : begin
                       {$i uIMAPManager.Process.UID.Store.SetFlags.AllSlient.inc}
                     end;
    isfPlus        : begin
                       {$i uIMAPManager.Process.UID.Store.SetFlags.Plus.inc}
                     end;
    isfPlusSilent  : begin
                       {$i uIMAPManager.Process.UID.Store.SetFlags.PlusSilent.inc}
                     end;
    isfMinus       : begin
                       {$i uIMAPManager.Process.UID.Store.SetFlags.Minus.inc}
                     end;
    isfMinusSilent : begin
                       {$i uIMAPManager.Process.UID.Store.SetFlags.MinusSilent.inc}
                     end;
    else begin
      {$i uIMAPManager.Process.UID.Store.SetFlags.None.inc}
    end;
  end;
end;

procedure TIMAPManager.ID_Search(RSRP:PRSR);
var
  iLcv:LongInt;
  iLen:LongInt;

  procedure SearchItem();
  begin
    EntryPoint:='TIMAPManager.ID_Search(RSRP, idxCommand).SearchItem.ID_Search_Execute';
    FSrchResult:=ID_Search_Execute();
    if FSrchResult<>0 then
      Core.Arrays.LargeWord.Add(FSrchResult,FSrchResults,aoNone);
  end;

begin
  EntryPoint:='TIMAPManager.ID_Search(RSRP)';
  if TransState()=true then begin
    If (IMAPP^.Selected<>nil) then begin
      FFolderP:=IMAPP^.Selected;
      iLcv:=SetRange();
      iLen:=Length(FCommands);
      RSR.IMAP.Empty(FSrchTerms);
      Try
        Parse(FReadAhead,iLcv,FSrchError,FCommands,FSrchTerms);
        if (FSrchError<>-1) then begin
          FEntry:=Concat('UID SEARCH misunderstood argument near the [',IntToStr(FSrchError+1),'] element of entire statement [',CMDS,'].');
          FResponse:=Respond(FSEQ,SSC_NO,FEntry);
          Send(RSRP,FResponse);
          Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TIMAPManager.ID_Search:',FEntry));
          Exit();
        end;
        if (Length(FSrchTerms)>0) then begin
          for iLcv:=0 to High(FFolderP^.Files) do begin
            RenewCycle();
            FFileP:=FFolderP^.Files[iLcv];
            if ( (FFileP^.ID<=FRangeEnd) and (FFileP^.ID>=FRangeStart)) then begin
              if (FFileP^.Valid=true) then begin
                if FReadAhead then begin
                  if CacheMessage(FFileP^,FHeaderBreak, FHeaderCount, FHeaders, FHeaderItems) then begin
                    SearchItem();
                  end;
                end else begin
                  SearchItem();
                end;
              end;
            end;
          end;
          if Length(FSrchResults)>0 then begin
            FResponse:=Concat('* SEARCH ',Core.Arrays.LargeWord.toString(FSrchResults,' ',Refactor),#13#10);
            Send(RSRP,FResponse);
          end;
          FResponse:=Respond(FSEQ,SSC_OK,'SEARCH completed.');
          Send(RSRP,FResponse);
        end else begin
          FResponse:=Respond(FSEQ,SSC_NO,Concat('UID SEARCH statement contains no parsable directives,terms, or arguments.'));
          Send(RSRP,FResponse);
          Exit();
        end;
      finally
        RSR.IMAP.Empty(FSrchTerms);
      end;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('UID SEARCH failed: no folder was selected.'));
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('UID SEARCH failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
end;

procedure TIMAPManager.ID_Fetch(RSRP:PRSR);

procedure PushPartials;
begin
  FFetchSection:=Core.Strings.Extract(FFetch[FFetchIdx],'[',']'); // multi-part
  if Length(FFetchSection)>0 then begin
    Core.Arrays.VarString.fromString(FFetchSections,FFetchSection,'.',[soClearList]);
    FFetchKinds+=[ifkFlags,ifkBodySection];
  end;
  FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'<');
  if FFetchIdx<>-1 then begin
    FFetchSectionPartial:=Core.Strings.Extract(FFetch[FFetchIdx],'<','>');// Byte Range;
    if Length(FFetchSectionPartial)>0 then begin
      Core.Arrays.VarString.fromString(FFetchSectionPartials,FFetchSectionPartial,'.',[soClearList]);
      if Length(FFetchSectionPartials)=2 then begin
        FByteRangeStart:=StrToIntDef(FFetchSectionPartials[0],0);
        FByteRangeSize:=StrToIntDef(FFetchSectionPartials[1],0);
      end else begin
        FByteRangeStart:=StrToIntDef(FFetchSectionPartials[0],0);
        FByteRangeSize:=0;
      end;
      FFetchKinds+=[ifkFlags,ifkBodySectionPartial];
    end;
  end;
end;

begin
  EntryPoint:='TIMAPManager.ID_Fetch(RSRP)';
  FFetchKinds:=[];
  SetRange();
  Core.Arrays.VarString.fromString(FFetch,CMDP3,' ',[soClearList,soParenWraps,soBracketWraps]);
  FFetchLen:=Length(FFetch);
  FFetchKinds+=[ifkUID];
  if Core.Arrays.VarString.Search(FFetch,'flags')<>-1 then
    FFetchKinds+=[ifkFlags];
  if Core.Arrays.VarString.Search(FFetch,'rfc822.size')<>-1 then
    FFetchKinds+=[ifkRFC822Size];
  if Core.Arrays.VarString.SearcH(FFetch,'rfc822.header')<>-1 then
    FFetchKinds+=[ifkRFC822Header];
  if Core.Arrays.VarString.Search(FFetch,'internaldate')<>-1 then
    FFetchKinds+=[ifkInternalDate];

  FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body[header]');
  if FFetchIdx<>-1 then
    FFetchKinds+=[ifkFlags,ifkBodyHeader];

  FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body.peek[]');
  if FFetchIdx<>-1 then begin
    FFetchKinds+=[ifkFlags,ifkBodyPeek,ifkBody];
    PushPartials();
  end else begin
    FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'header.fields');
    if FFetchIdx<>-1 then begin
      FFetchKinds+=[ifkFlags,ifkBodyHeaderFields];
      FFetchSection:=Core.Strings.Extract(FFetch[FFetchIdx],'[',']');
      FBodyPeek:=Core.Strings.Extract(FFetchSection,'(',')');
      Core.Arrays.VarString.fromString(FHeaderFields,FBodyPeek,' ',[soClearList]);
    end;
    FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body.peek');
    if FFetchIdx<>-1 then begin
      FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body.peek[header]');
      if FFetchIdx<>-1 then
        FFetchKinds+=[ifkFlags,ifkBodyPeekHeader];
      FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body.peek[text]');
      if FFetchIdx<>-1 then
        FFetchKinds+=[ifkFlags,ifkBodyText,ifkBodyPeekText];

      FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body.peek[');
      if(
        ((ifkBodyPeek in FFetchKinds)=false) and
        ((ifkBodyPeekHeader in FFetchKinds)=false) and
        ((ifkBodyHeaderFields in FFetchKinds)=false) and
        ((ifkBodyPeekText in FFetchKinds)=false) and
        ((ifkBodySection in FFetchKinds)=false) and
        (FFetchIdx<>-1)) then begin
        PushPartials();
      end;
    end;
  end;
  FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'bodystructure');
  if FFetchIdx<>-1 then begin
    FFetchKinds+=[ifkFlags,ifkBodyStructure];
  end;
  FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body[text]');
  if FFetchIdx<>-1 then begin
    FFetchKinds+=[ifkFlags,ifkBodyText];
  end;
  FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body[]');
  if FFetchIdx<>-1 then begin
    FFetchKinds+=[ifkFlags,ifkBody];
    PushPartials();
  end else begin
    FFetchIdx:=Core.Arrays.VarString.Search(FFetch,'body[');
    if FFetchIdx<>-1 then
      PushPartials();
  end;
  FFileCount:=Length(FFolderP^.Files);
  if (
    (FFetchKinds=[ifkFlags]) or
    (FFetchKinds=[ifkUID,ifkFlags])
  ) then begin
    OutputFetchFlags(RSRP,FRangeKinds,IMAPP^.Selected^,IMAPP^.Selected^.Files);
  end else if FFetchKinds<>[] then begin
    OutputFetchDeep(RSRP,FRangeKinds,IMAPP^.Selected^,IMAPP^.Selected^.Files);
  end else begin
    FResponse:=Respond(FSEQ,SSC_BAD,Concat('FETCH failed: "',CMDP3,'" not understood.'));
    Send(RSRP,FResponse);
  end;
  Storage.UserStorage.Items.SMTP.Empty(FSummary);
  Storage.UserStorage.Items.IMAP.Empty(FEnvelope);
  Storage.UserStorage.Items.IMAP.Empty(FBody);
end;

{$i uIMAPManager.OnDataReceived.inc}

{$i uIMAPManager.Process.inc}
{$i uIMAPManager.Respond.inc}
{$i uIMAPManager.Append.inc}
{$i uIMAPManager.Expunge.inc}
{$i uIMAPManager.ReloadFolders.inc}

{$i uIMAPManager.Exceptions.inc}
{$i uIMAPManager.Errors.inc}


end.

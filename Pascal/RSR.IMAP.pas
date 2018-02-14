unit RSR.IMAP;

interface

uses
  Classes,

  Core.Strings,

  Core.Arrays,
  Core.Arrays.Types,

  Core.Arrays.VarString,

  Storage.UserAccounts,
  Storage.UserStorage,

  SysUtils;

Const
  IMAP_PORT = 143;
  IMAP_STACK_SIZE      : LongInt= 1024*5;


  MAX_IMAP_ERRORS = 5;

  RS_NONE              =0 shl 0;
  RS_AUTHENTICATED     =1 shl 1;
  RS_SELECTED          =1 shl 2;
  RS_STARTTLS          =1 shl 3;
  RS_LOGOUT            =1 shl 4;
  RS_EndSession        =1 shl 5;
  RS_LOGIN_DISABLED    =1 shl 6;
  RS_MESSAGE_DATA      =1 shl 7;
  RS_AUTHENTICATE      =1 shl 8;

  STATUS_TAGS_OFF      = false;
  STATUS_TAGS_ON       = true;
  SSC_OK          = 'OK';
  SSC_NO          = 'NO';
  SSC_BAD         = 'BAD';
  SSC_BYE         = 'BYE';
  SEQ_ANY         = '*';

  STATUS_DATA_MESSAGES           = 'MESSAGES';
  STATUS_DATA_RECENT             = 'RECENT';
  STATUS_DATA_UIDNEXT            = 'UIDNEXT';
  STATUS_DATA_UIDVALIDITY        = 'UIDVALIDITY';
  STATUS_DATA_UNSEEN             = 'UNSEEN';
  STATUS_DATA_DELETED            = 'EXPUNGE';
  STATUS_DATA_EXISTS             = 'EXISTS';

  SELECT_RESP_REQUIRED           = 'REQUIRED';
  SELECT_RESP_FLAGS              = 'FLAGS';
  SELECT_RESP_EXISTS             = 'EXISTS';
  SELECT_RESP_RECENT             = 'RECENT';
  SELECT_RESP_UNSEEN             = 'UNSEEN';
  SELECT_RESP_PERMANENTFLAGS     = 'PERMANENTFLAGS';
  SELECT_RESP_UIDNEXT            = 'UIDNEXT';
  SELECT_RESP_UIDVALIDITY        = 'UIDVALIDITY';

  FLAG_ANSWERED                  = '\Answered';
  FLAG_FLAGGED                   = '\Flagged';
  FLAG_DELETED                   = '\Deleted';
  FLAG_SEEN                      = '\Seen';
  FLAG_ANY                       = '\*';
  FLAG_DRAFT                     = '\Draft';
  FLAG_NOSELECT                  = '\Noselect';


  SRCH_MAX_BYTES                 : LongInt = 10240;


  SRCH_ARG_ALL                    = 'ALL';
  SRCH_ARG_ANSWERED               = 'ANSWERED';
  SRCH_ARG_BCC                    = 'BCC';
  SRCH_ARG_BEFORE                 = 'BEFORE';
  SRCH_ARG_BODY                   = 'BODY';
  SRCH_ARG_CC                     = 'CC';
  SRCH_ARG_DELETED                = 'DELETED';
  SRCH_ARG_DRAFT                  = 'DRAFT';
  SRCH_ARG_FLAGGED                = 'FLAGGED';
  SRCH_ARG_FROM                   = 'FROM';
  SRCH_ARG_HEADER                 = 'HEADER';
  SRCH_ARG_KEYWORD                = 'KEYWORD';
  SRCH_ARG_LARGER                 = 'LARGER';
  SRCH_ARG_NEW                    = 'NEW';
  SRCH_ARG_NOT                    = 'NOT';
  SRCH_ARG_OLD                    = 'OLD';
  SRCH_ARG_ON                     = 'ON';
  SRCH_ARG_OR                     = 'OR';
  SRCH_ARG_RECENT                 = 'RECENT';
  SRCH_ARG_SEEN                   = 'SEEN';
  SRCH_ARG_SENTBEFORE             = 'SENTBEFORE';
  SRCH_ARG_SENTON                 = 'SENTON';
  SRCH_ARG_SENTSINCE              = 'SENTSINCE';
  SRCH_ARG_SINCE                  = 'SINCE';
  SRCH_ARG_SMALLER                = 'SMALLER';
  SRCH_ARG_SUBJECT                = 'SUBJECT';
  SRCH_ARG_MESSAGEID              = 'MESSAGEID';
  SRCH_ARG_TEXT                   = 'TEXT';
  SRCH_ARG_TO                     = 'TO';
  SRCH_ARG_UID                    = 'UID';
  SRCH_ARG_UNANSWERED             = 'UNANSWERED';
  SRCH_ARG_UNDELETED              = 'UNDELETED';
  SRCH_ARG_UNDRAFT                = 'UNDRAFT';
  SRCH_ARG_UNFLAGGED              = 'UNFLAGGED';
  SRCH_ARG_UNKEYWORD              = 'UNKEYWORD';
  SRCH_ARG_UNSEEN                 = 'UNSEEN';
type
  TSrchArg=(
    srchArgUnknown,
    srchArgAll,
    srchArgAnswered,     // 1
    srchArgBcc,          // 2
    srchArgBefore,       // 2
    srchArgBody,         // 2
    srchArgCC,           // 2
    srchArgDeleted,      // 1
    srchArgDraft,        // 1
    srchArgFlagged,      // 1
    srchArgFrom,         // 2
    srchArgHeader,       // 3
    srchArgKeyword,
    srchArgLarger,
    srchArgNew,
    srchArgNot,
    srchArgOld,
    srchArgOn,
    srchArgOr,
    srchArgRecent,
    srchArgSeen,
    srchArgSentBefore,
    srchArgSentOn,
    srchArgSentSince,
    srchArgSince,
    srchArgSmaller,
    srchArgSubject,
    srchArgMessageID,
    srchArgText,
    srchArgTo,
    srchArgUID,
    srchArgUnAnwered,
    srchArgUnDeleted,
    srchArgUnDraft,
    srchArgUnFlagged,
    srchArgUnKeyword,
    srchArgUnseen
  );
  TSrchDirective=(srchDirectiveNot,srchDirectiveOr);
  TSrchArgs=set of TSrchArg;
  TSrchTermKind=(srchTermKindUnknown,srchTermKindSingle,srchTermKindDouble,srchTermKindTripple,srchTermKindDirective);

  TSrchTermSingle=record
    Value : TSrchArg;
  end;
  TSrchTermDouble=record
    Value    : TSrchArg;
    Criteria : Core.Strings.VarString;
  end;
  TSrchTermTripple=record
    Value     : TSrchArg;
    Criteria1 : Core.Strings.VarString;
    Criteria2 : Core.Strings.VarString;
  end;
  TSrchTerm=record
    Kind : TSrchTermKind;
    Data : Pointer;
  end;
  TSrchTermDirective=record
    Value     : TSrchArg;   // OR or NOT
    Keys      : LongInt;
    Key1      : TSrchTerm;
    Key2      : TSrchTerm;
  end;
  PSrchTermSingle=^TSrchTermSingle;
  PSrchTermDouble=^TSrchTermDouble;
  PSrchTermTripple=^TSrchTermTripple;
  PSrchTermDirective=^TSrchTermDirective;

  PSrchTerm=^TSrchTerm;
  TSrchTerms=Array of PSrchTerm;

Type
  TIMAPFetchKind=(ifkUID,ifkFlags,ifkRFC822Size,ifkRFC822Header,
    ifkInternalDate, ifkBodyHeaderFields,ifkBodyStructure,ifkBody,ifkBodyHeader,
    ifkBodyPeek,ifkBodyPeekHeader,ifkBodyText,ifkBodyPeekText,
    ifkBodySection,ifkBodySectionPartial
  );
  TIMAPRangeKind=(irkRange,irkSet);
  TIMAPStoreFlagMode=(isfNone,isfAll,isfAllSilent,isfPlus,isfPlusSilent,isfMinus,isfMinusSilent);
  TIMAPFetchKinds=set of TIMAPFetchKind;
  TIMAPRangeKinds=set of TIMAPRangeKind;
  TIMAPFolderReloadKind=(ifrkStatus,ifrkSelected,ifrkExamine);
  PIMAPMessage=^TIMAPMessage;
  Command=class
  const
    Fetch                        : Core.Strings.VarString = 'fetch';
    Store                        : Core.Strings.VarString = 'store';
    Copy                         : Core.Strings.VarString = 'copy';
    Search                       : Core.Strings.VarString = 'search';
    Expunge                      : Core.Strings.VarString = 'expunge';
    UID                          : Core.Strings.VarString = 'uid';
  Type
    Parameter=class
    const
      Flags                      : Core.Strings.VarString = 'flags';
      FlagsSilent                : Core.Strings.VarString = 'flags.silent';
      FlagsSilentOn              : Core.Strings.VarString = '+flags.silent';
      FlagsSilentOff             : Core.Strings.VarString = '-flags.silent';
      FlagsOn                    : Core.Strings.VarString = '+flags';
      FlagsOff                   : Core.Strings.VarString = '-flags';
    end;
  end;

  TIMAPMessage=record
    ID               : QWord;
    Flags            : LongInt;
    Size             : Cardinal;
    Plus             : Boolean;
    Folder           : Storage.UserStorage.Folders.PFolder;
    Message          : TMemoryStream;
  end;

  TIMAP=record
    UAP              : Storage.UserAccounts.Items.PItem;
    Folders          : Storage.UserStorage.Folders.TFolders;
    Selected         : Storage.UserStorage.Folders.PFolder;
    Status           : Storage.UserStorage.Folders.PFolder;
    Examine          : Storage.UserStorage.Folders.PFolder;

    sUser            : Core.Strings.VarString;
    sPass            : Core.Strings.VarString;
    LastSequence     : Core.Strings.VarString;
    Nonce            : Core.Strings.VarString;
    CNonce           : Core.Strings.VarString;
    RemoteIP         : Core.Strings.VarString;

    MessageP         : PIMAPMessage;
    LastDeleted      : LongInt;
    LastExists       : LongInt;
    LastRecent       : LongInt;
    ErrorCount       : Byte;
    DeleteIndex      : LongInt;
    State            : LongInt;
  end;
  PIMAP=^TIMAP;

  const SEARCH_DIRECTIVE_ARG : TSrchArgs=[
    srchArgOr,srchArgNot
  ];
  const ReadAheadArg:TSrchArgs=[
    srchArgBcc,
    srchArgBefore,
    srchArgBody,
    srchArgCC,
    srchArgFrom,
    srchArgKeyword,
    srchArgSubject,
    srchArgText,
    srchArgTo,
    srchArgUID,
    srchArgMessageID,
    srchArgUnKeyword,
    srchArgHeader
  ];
  const SEARCH_SINGLE_ARG : TSrchArgs=[
    srchArgAnswered,
    srchArgDeleted,
    srchArgDraft,
    srchArgFlagged,
    srchArgNew,
    srchArgOld,
    srchArgRecent,
    srchArgSeen,
    srchArgUnAnwered,
    srchArgUnDeleted,
    srchArgUnDraft,
    srchArgUnFlagged,
    srchArgUnseen
  ];
  const SEARCH_DOUBLE_ARG : TSrchArgs=[
    srchArgBcc,
    srchArgBefore,
    srchArgBody,
    srchArgCC,
    srchArgFrom,
    srchArgKeyword,
    srchArgLarger,
    srchArgOn,
    srchArgSentBefore,
    srchArgSentOn,
    srchArgSentSince,
    srchArgSince,
    srchArgSmaller,
    srchArgSubject,
    srchArgText,
    srchArgTo,
    srchArgUID,
    srchArgMessageID,
    srchArgUnKeyword
  ];
  const SEARCH_TRIPPLE_ARG : TSrchArgs=[
    srchArgHeader
  ];
  procedure Empty(var Item:TSrchTermSingle); overload;
  procedure Empty(var Item:TSrchTermDouble); overload;
  procedure Empty(var Item:TSrchTermTripple); overload;
  procedure Empty(var Item:TSrchTermDirective); overload;

  procedure Empty(var Item:TSrchTerms); overload;
  procedure Empty(var Item:TSrchTerm); overload;
  procedure Empty(Var Item:TIMAPMessage); overload;
  procedure Empty(Var Item:TIMAP); overload;

  procedure Init(Var Item:TIMAPMessage); overload;
  procedure Init(Var Item:TIMAP); overload;
  procedure Init(var Item:TSrchTerms); overload;
  procedure Init(aKind:TSrchTermKind; var Item:TSrchTerm); overload;
  procedure Init(var Item:TSrchTermSingle; aArg:TSrchArg); overload;
  procedure Init(var Item:TSrchTermDouble; aArg:TSrchArg); overload;
  procedure Init(var Item:TSrchTermTripple; aArg:TSrchArg); overload;
  procedure Init(var Item:TSrchTermDirective; aArg:TSrchArg; aKey1Kind,aKey2Kind:TSrchTermKind); overload;


  procedure Done(Var Item:TIMAPMessage); overload;
  procedure Done(Var Item:TIMAP); overload;
  procedure Done(var Item:TSrchTerms); overload;
  procedure Done(var Item:TSrchTerm); overload;
  procedure Done(var Item:TSrchTermSingle); overload;
  procedure Done(var Item:TSrchTermDouble); overload;
  procedure Done(var Item:TSrchTermTripple); overload;
  procedure Done(var Item:TSrchTermDirective); overload;

  procedure fromString(sData:Core.Strings.VarString; out Arg:TSrchArg); overload;

  function  Parse(Var ReadAheadNeeded:boolean; var Index,Error:LongInt; var Input:Core.Arrays.Types.VarString; var Items:TSrchTerms):boolean; overload;

  function  GetFlags(sFlags:Core.Strings.VarString): LongInt;

implementation


procedure fromString(sData:Core.Strings.VarString; out Arg:TSrchArg);
begin
  if Sysutils.SameText(sData,SRCH_ARG_ALL) then
    Arg:=srchArgAll
  else if Sysutils.SameText(sData,SRCH_ARG_ANSWERED) then
    Arg:=srchArgAnswered
  else if Sysutils.SameText(sData,SRCH_ARG_BCC) then
    Arg:=srchArgBcc
  else if Sysutils.SameText(sData,SRCH_ARG_BEFORE) then
    Arg:=srchArgBefore
  else if Sysutils.SameText(sData,SRCH_ARG_BODY) then
    Arg:=srchArgBody
  else if Sysutils.SameText(sData,SRCH_ARG_CC) then
    Arg:=srchArgCC
  else if Sysutils.SameText(sData,SRCH_ARG_DELETED) then
    Arg:=srchArgDeleted
  else if Sysutils.SameText(sData,SRCH_ARG_DRAFT) then
    Arg:=srchArgDraft
  else if Sysutils.SameText(sData,SRCH_ARG_FLAGGED) then
    Arg:=srchArgFlagged
  else if Sysutils.SameText(sData,SRCH_ARG_FROM) then
    Arg:=srchArgFrom
  else if Sysutils.SameText(sData,SRCH_ARG_HEADER) then
    Arg:=srchArgHeader
  else if Sysutils.SameText(sData,SRCH_ARG_KEYWORD) then
    Arg:=srchArgKeyword
  else if Sysutils.SameText(sData,SRCH_ARG_LARGER) then
    Arg:=srchArgLarger
  else if Sysutils.SameText(sData,SRCH_ARG_NEW) then
    Arg:=srchArgNew
  else if Sysutils.SameText(sData,SRCH_ARG_NOT) then
    Arg:=srchArgNot
  else if Sysutils.SameText(sData,SRCH_ARG_OLD) then
    Arg:=srchArgOld
  else if Sysutils.SameText(sData,SRCH_ARG_ON) then
    Arg:=srchArgOn
  else if Sysutils.SameText(sData,SRCH_ARG_OR) then
    Arg:=srchArgOr
  else if Sysutils.SameText(sData,SRCH_ARG_RECENT) then
    Arg:=srchArgRecent
  else if Sysutils.SameText(sData,SRCH_ARG_SEEN) then
    Arg:=srchArgSeen
  else if Sysutils.SameText(sData,SRCH_ARG_SENTBEFORE) then
    Arg:=srchArgSentBefore
  else if Sysutils.SameText(sData,SRCH_ARG_SENTON) then
    Arg:=srchArgSentOn
  else if Sysutils.SameText(sData,SRCH_ARG_SENTSINCE) then
    Arg:=srchArgSentSince
  else if Sysutils.SameText(sData,SRCH_ARG_SINCE) then
    Arg:=srchArgSince
  else if Sysutils.SameText(sData,SRCH_ARG_SMALLER) then
    Arg:=srchArgSmaller
  else if Sysutils.SameText(sData,SRCH_ARG_SUBJECT) then
    Arg:=srchArgSubject
  else if Sysutils.SameText(sData,SRCH_ARG_MESSAGEID) then
    Arg:=srchArgMessageId
  else if Sysutils.SameText(sData,SRCH_ARG_TEXT) then
    Arg:=srchArgText
  else if Sysutils.SameText(sData,SRCH_ARG_TO) then
    Arg:=srchArgTo
  else if Sysutils.SameText(sData,SRCH_ARG_UID) then
    Arg:=srchArgUID
  else if Sysutils.SameText(sData,SRCH_ARG_UNANSWERED) then
    Arg:=srchArgUnAnwered
  else if Sysutils.SameText(sData,SRCH_ARG_UNDELETED) then
    Arg:=srchArgUnDeleted
  else if Sysutils.SameText(sData,SRCH_ARG_UNDRAFT) then
    Arg:=srchArgUnDraft
  else if Sysutils.SameText(sData,SRCH_ARG_UNFLAGGED) then
    Arg:=srchArgUnFlagged
  else if Sysutils.SameText(sData,SRCH_ARG_UNKEYWORD) then
    Arg:=srchArgUnKeyword
  else if Sysutils.SameText(sData,SRCH_ARG_UNSEEN) then
    Arg:=srchArgUnseen
  else
    Arg:=srchArgUnknown;
end;

procedure Empty(var Item:TSrchTerms);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Done(var Item:TSrchTerm);

  procedure Push_Single();
  var
    TermP:PSrchTermSingle;
  begin
    TermP:=Item.Data;
    Done(TermP^);
    Dispose(TermP);
  end;

  procedure Push_Double();
  var
    TermP:PSrchTermDouble;
  begin
    TermP:=Item.Data;
    Done(TermP^);
    Dispose(TermP);
  end;

  procedure Push_Directive();
  var
    TermP:PSrchTermDirective;
  begin
    TermP:=Item.Data;
    Done(TermP^);
    Dispose(TermP);
  end;

begin
  case Item.Kind of
    srchTermKindSingle    : Push_Single();
    srchTermKindDouble    : Push_Double();
    srchTermKindDirective : Push_Directive();
  end;
  Item.Data:=nil;
  Finalize(Item);
end;

procedure Done(var Item:TSrchTermSingle);
begin
  Item.Value:=srchArgUnknown;
  Finalize(Item);
end;

procedure Done(var Item:TSrchTermDouble);
begin
  Item.Value:=srchArgUnknown;
  Finalize(Item.Criteria);
  Finalize(Item);
end;

procedure Done(var Item:TSrchTermTripple);
begin
  Item.Value:=srchArgUnknown;
  Finalize(Item.Criteria1);
  Finalize(Item.Criteria2);
  Finalize(Item);
end;

procedure Done(var Item:TSrchTermDirective);
begin
  Item.Value:=srchArgUnknown;
  Done(Item.Key1);
  Done(Item.Key2);
  Finalize(Item);
end;

procedure Empty(var Item:TSrchTerm);
  procedure Push_Single();
  var
    TermP:PSrchTermSingle;
  begin
    TermP:=Item.Data;
    Empty(TermP^);
  end;

  procedure Push_Double();
  var
    TermP:PSrchTermDouble;
  begin
    TermP:=Item.Data;
    Empty(TermP^);
  end;

  procedure Push_Directive();
  var
    TermP:PSrchTermDirective;
  begin
    TermP:=Item.Data;
    Empty(TermP^);
  end;

begin
  case Item.Kind of
    srchTermKindSingle    : Push_Single();
    srchTermKindDouble    : Push_Double();
    srchTermKindDirective : Push_Directive();
  end;
end;

procedure Empty(var Item:TSrchTermSingle);
begin
  Item.Value:=srchArgUnknown;
end;

procedure Empty(var Item:TSrchTermDouble);
begin
  Item.Value:=srchArgUnknown;
  SetLength(Item.Criteria,0);
end;

procedure Empty(var Item:TSrchTermTripple);
begin
  Item.Value:=srchArgUnknown;
  SetLength(Item.Criteria1,0);
  SetLength(Item.Criteria2,0);
end;

procedure Empty(var Item:TSrchTermDirective);
begin
  Item.Value:=srchArgUnknown;
  Item.Keys:=-1;
  Empty(Item.Key1);
  Empty(Item.Key2);
end;

procedure Empty(Var Item:TIMAPMessage);
begin
  Item.ID:=0;
  Item.Size:=0;
  Item.Flags:=0;
  Item.Plus:=false;
  Item.Folder:=nil;
  Item.Message.Size:=0;
end;

procedure Empty(Var Item:TIMAP);
begin
  Item.LastExists:=-1;
  Item.LastRecent:=-1;
  Item.LastDeleted:=-1;

  Item.UAP:=nil;
  Item.Selected:=nil;
  Item.Status:=nil;
  Item.Examine:=nil;

  Storage.UserStorage.Folders.Init(Item.Folders);
  SetLength(Item.sUser,0);
  SetLength(Item.sPass,0);
  SetLength(Item.LastSequence,0);
  SetLength(Item.Nonce,0);
  SetLength(Item.CNonce,0);
  SetLength(Item.RemoteIP,0);

  Item.ErrorCount:=0;
  Item.DeleteIndex:=-1;
  If Item.MessageP<>nil then begin
    Done(Item.MessageP^);
    Dispose(Item.MessageP);
    Item.MessageP:=nil;
  end;
  Item.State:=RS_NONE;
end;

procedure Init(var Item:TSrchTermSingle; aArg:TSrchArg);
begin
  Item.Value:=aArg;
end;

procedure Init(var Item:TSrchTermDouble; aArg:TSrchArg);
begin
  Item.Value:=aArg;
  SetLength(Item.Criteria,0);
end;

procedure Init(var Item:TSrchTermTripple; aArg:TSrchArg);
begin
  Item.Value:=aArg;
  SetLength(Item.Criteria1,0);
  SetLength(Item.Criteria2,0);
end;

procedure Init(var Item:TSrchTermDirective; aArg:TSrchArg; aKey1Kind,aKey2Kind:TSrchTermKind);
begin
  Item.Value:=aArg;
  Item.Keys:=0;
  Init(aKey1Kind,Item.Key1);
  Init(aKey2Kind,Item.Key2);
end;

procedure Init(Var Item:TIMAPMessage);
begin
  Item.ID:=0;
  Item.Flags:=0;
  Item.Folder:=nil;
  Item.Size:=0;
  Item.Plus:=false;
  Item.Message:=TMemoryStream.Create();
end;

procedure Init(Var Item:TSrchTerms);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Init(aKind:TSrchTermKind; var Item:TSrchTerm);

  procedure Push_Single();
  var
    TermP:PSrchTermSingle;
  begin
    New(TermP);
    Init(TermP^,srchArgUnknown);
    Item.Data:=TermP;
  end;

  procedure Push_Double();
  var
    TermP:PSrchTermDouble;
  begin
    New(TermP);
    Init(TermP^,srchArgUnknown);
    Item.Data:=TermP;
  end;

  procedure Push_Tripple();
  var
    TermP:PSrchTermTripple;
  begin
    New(TermP);
    Init(TermP^,srchArgUnknown);
    Item.Data:=TermP;
  end;

  procedure Push_Directive();
  var
    TermP:PSrchTermDirective;
  begin
    New(TermP);
    Init(TermP^,srchArgUnknown,srchTermKindUnknown,srchTermKindUnknown);
    Item.Data:=TermP;
  end;

  procedure Push_Unknown();
  begin
    Item.Data:=nil;
  end;

begin
  Item.Kind:=aKind;
  case Item.Kind of
    srchTermKindUnknown   : Push_Unknown();
    srchTermKindSingle    : Push_Single();
    srchTermKindDouble    : Push_Double();
    srchTermKindTripple   : Push_Tripple();
    srchTermKindDirective : Push_Directive();
  end;
end;


procedure Init(Var Item:TIMAP);
begin
  Item.UAP:=nil;
  Item.Selected:=nil;
  Item.Status:=nil;
  Item.Examine:=nil;

  Item.MessageP:=nil;

  Storage.UserStorage.Folders.Init(Item.Folders);
  SetLength(Item.sUser,0);
  SetLength(Item.sPass,0);
  SetLength(Item.LastSequence,0);
  SetLength(Item.Nonce,0);
  SetLength(Item.CNonce,0);
  SetLength(Item.RemoteIP,0);

  Item.ErrorCount:=0;
  Item.DeleteIndex:=-1;
  Item.LastExists:=-1;
  Item.LastRecent:=-1;
  Item.LastDeleted:=-1;

  Item.State:=RS_NONE;

end;

procedure Done(Var Item:TIMAP);
begin
  Storage.UserStorage.Folders.Done(Item.Folders,Storage.UserStorage.Folders.FREE_FILES);
  Finalize(Item.sUser);
  Finalize(Item.sPass);
  Finalize(Item.LastSequence);
  Finalize(Item.Nonce);
  Finalize(Item.CNonce);
  Finalize(Item.RemoteIP);

  If (Item.MessageP<>nil) then begin
    Done(Item.MessageP^);
    Dispose(Item.MessageP);
    Item.MessageP:=nil;
  end;
  Finalize(Item);
end;

procedure Done(var Item:TSrchTerms);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

procedure Done(Var Item:TIMAPMessage);
begin
  Item.Message.Free();
  Item.Message:=nil;
  Finalize(Item);
end;

function GetFlags(sFlags:Core.Strings.VarString): LongInt;
begin
  Result:=Storage.UserStorage.Items.IMAP.Flags.None;
  if Core.Strings.Search(sFlags,FLAG_ANSWERED)>0 then
    Result:=Result or Storage.UserStorage.Items.IMAP.Flags.Answered;
  if Core.Strings.Search(sFlags,FLAG_FLAGGED)>0 then
    Result:=Result or Storage.UserStorage.Items.IMAP.Flags.Flagged;
  if Core.Strings.Search(sFlags,FLAG_DELETED)>0 then
    Result:=Result or Storage.UserStorage.Items.IMAP.Flags.Deleted;
  if Core.Strings.Search(sFlags,FLAG_SEEN)>0 then
    Result:=Result or Storage.UserStorage.Items.IMAP.Flags.Seen;
  if Core.Strings.Search(sFlags,FLAG_DRAFT)>0 then
    Result:=Result or Storage.UserStorage.Items.IMAP.Flags.Draft;
  if Core.Strings.Search(sFlags,FLAG_NOSELECT)>0 then
    Result:=Result or Storage.UserStorage.Items.IMAP.Flags.NoSelect;
end;

function Parse(Var ReadAheadNeeded:boolean; var Index,Error:LongInt; var Input:Core.Arrays.Types.VarString; var Items:TSrchTerms):boolean;
var
  iLen  : LongInt;

  function Add():PSrchTerm;
  var
    ItemP:PSrchTerm;
    iCt:LongInt;
  begin
    New(ItemP);
    iCt:=Length(Items);
    SetLength(Items,iCt+1);
    Items[iCt]:=ItemP;
    Result:=ItemP;
  end;

  function ParseSingleTerm(DirectiveP:PSrchTerm; Arg:TSrchArg):PSrchTerm;
  var
    ItemP:PSrchTerm;
    DataP:PSrchTermSingle;
    DirP:PSrchTermDirective;
  begin
    ItemP:=DirectiveP;
    ReadAheadNeeded:=( (ReadAheadNeeded=true) or (Arg in ReadAheadArg) );
    if (ItemP=nil) then begin
      ItemP:=Add();
      Init(srchTermKindSingle,ItemP^);
      DataP:=ItemP^.Data;
      DataP^.Value:=Arg;
    end else begin
      DirP:=DirectiveP^.Data;
      Inc(DirP^.Keys);
      New(DataP);
      Init(DataP^,Arg);
      Case DirP^.Keys of
        1: begin
          DirP^.Key1.Kind:=srchTermKindSingle;
          DirP^.Key1.Data:=DataP;
        end;
        2: begin
          DirP^.Key2.Kind:=srchTermKindSingle;
          DirP^.Key2.Data:=DataP;
        end;
      end;
      DataP:=DirP^.Key1.Data;
    end;
    Result:=ItemP;
  end;

  procedure ParseDoubleTerm(DirectiveP:PSrchTerm; Arg:TSrchArg);
  var
    ItemP:PSrchTerm;
    DataP:PSrchTermDouble;
    DirP:PSrchTermDirective;
  begin
    ReadAheadNeeded:=( (ReadAheadNeeded=true) or (Arg in ReadAheadArg) );
    ItemP:=DirectiveP;
    if (ItemP=nil) then begin
      ItemP:=Add();
      Init(srchTermKindDouble,ItemP^);
      DataP:=ItemP^.Data;
      DataP^.Value:=Arg;
    end else begin
      DirP:=DirectiveP^.Data;
      Inc(DirP^.Keys);
      New(DataP);
      Init(DataP^,Arg);
      Case DirP^.Keys of
        1: begin
            DirP^.Key1.Kind:=srchTermKindDouble;
            DirP^.Key1.Data:=DataP;
        end;
        2: begin
          DirP^.Key2.Kind:=srchTermKindDouble;
          DirP^.Key2.Data:=DataP;
        end;
      end;
      DataP:=DirP^.Key1.Data;
    end;
    DataP^.Criteria:=Input[Index];
    Inc(Index);
  end;

  procedure ParseTrippleTerm(DirectiveP:PSrchTerm; Arg:TSrchArg);
  var
    ItemP:PSrchTerm;
    DataP:PSrchTermTripple;
    DirP:PSrchTermDirective;
  begin
    ReadAheadNeeded:=( (ReadAheadNeeded=true) or (Arg in ReadAheadArg) );
    ItemP:=DirectiveP;
    if (ItemP=nil) then begin
      ItemP:=Add();
      Init(srchTermKindTripple,ItemP^);
      DataP:=ItemP^.Data;
      DataP^.Value:=Arg;
    end else begin
      DirP:=DirectiveP^.Data;
      Inc(DirP^.Keys);
      New(DataP);
      Init(DataP^,Arg);
      Case DirP^.Keys of
        1: begin
            DirP^.Key1.Kind:=srchTermKindTripple;
            DirP^.Key1.Data:=DataP;
        end;
        2: begin
          DirP^.Key2.Kind:=srchTermKindTripple;
          DirP^.Key2.Data:=DataP;
        end;
      end;
      DataP:=DirP^.Key1.Data;
    end;
    DataP^.Criteria1:=Input[Index];
    Inc(Index);
    DataP^.Criteria2:=Input[Index];
    Inc(Index);
  end;


  procedure ParseDirective(Arg:TSrchArg);
  var
    RootP:PSrchTerm;
    ItemP:PSrchTerm;
    DirP:PSrchTermDirective;
    ArgNext:TSrchArg;
    kLcv:LongInt;
  begin
    ItemP:=Add();
    Init(srchTermKindDirective,ItemP^);
    DirP:=ItemP^.Data;
    DirP^.Value:=Arg;
    Case Arg of
      srchArgOr  : begin
        if Index<iLen then begin
          {$i RSR.IMAP.Parse.ParseDirective.inc}
          ItemP:=@DirP^.Key2;
          {$i RSR.IMAP.Parse.ParseDirective.inc}
        end else begin
          Error:=Index;
          Exit;
        end;
      end;
      srchArgNot : begin
         if Index<iLen then begin
          {$i RSR.IMAP.Parse.ParseDirective.inc}
        end else begin
          Error:=Index;
          Exit;
        end;
      end;
    end;
  end;

  procedure ParseNextItem();
  var
    Arg   : TSrchArg;
  begin
    fromString(Input[Index],Arg);
    Inc(Index);
    if Arg = srchArgHeader then begin
      // The last index if the Actual Search Term
     Index:=iLen-1;
     ParseDoubleTerm(nil,Arg);
    end else if Arg in SEARCH_DIRECTIVE_ARG then begin
      ParseDirective(Arg);
    end else if Arg in SEARCH_SINGLE_ARG then begin
      ParseSingleTerm(nil,Arg);
    end else if Arg in SEARCH_DOUBLE_ARG then begin
      ParseDoubleTerm(nil,Arg)
    end else if Arg in SEARCH_TRIPPLE_ARG then begin
      ParseTrippleTerm(nil,Arg)
    end else begin
      Error:=Index;
    end;
  end;

begin
  ReadAheadNeeded:=false;
  Result:=false;
  Error:=-1;
  iLen:=Length(Input);
  While (Index<iLen) and (Error=-1) do
    ParseNextItem();
end;

end.


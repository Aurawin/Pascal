unit RSR.XMPP;

{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

interface
uses
  classes,

  Core.XML,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Timer,
  Core.Utils.Time,
  Core.Options,
  Core.Arrays.VarString,

  Encryption.SHA,

  SysUtils;
Type
  TRolePrivilege=(rpPresent,rpRecvMessages,rpChangeStatus,rpChangeRoomNickname,
    rpSendPrivateMessages,rpInviteOtherUsers,rpSendMessages,rpModifySubject,
    rpKick,rpGrantVoice,rpRevokeVoice
  );
  TRolePrivileges=Set of TRolePrivilege;
  TConferenceFlag=(cPrivate,cPublic,cPersistent,cOpen,cMembersOnly,cTemporary,cPasswordRequired,cUnsecure,cNonAnonymous,cSemiAnonymous,cModerated,cUnModerated);
  TConferenceFlags=Set of TConferenceFlag;
  TChatAffiliation=(caOwner,caAdmin,caMember,caOutcast,caNone,caInvalid);
  TChatRole=(crModerator,crParticipant,crVisitor,crNone);
  TMessageKind=(mChat,mError,mGroupChat,mHeadline,mNormal);
  TPresenceKind=(pNotify,pAvailable,pUnavailable,pSubscribe,pSubscribed,pUnsubscribe,pUnsubscribed,pError,pProbe,pNone);
  TSubscription=(sNone,sTo,sFrom,sBoth);
  TStatus=(jOnline,jChat,jAway,jExtendedAway,jDND,jUnavailable);
  TIQRequire=(iqrUsername,iqrPassword,iqrDigest,iqrResource);
  TIQRequires=Set of TIQRequire;
  TError=(eAuthenticate,eLogin,eStream,eTime);
  TErrors=Set of TError;
  TIQType=(iqUnknown,iqGet,iqSet,iqResult,iqError);
  TXMPPFunction=function(RSRP:Pointer; iStanzaIndex:LongInt):boolean of Object;
  TXMPPFunctionData=Record
    Name     : Core.Strings.VarString;
    Execute  : TXMPPFunction;
  end;
  TXMPPFunctions=Array of TXMPPFunctionData;

  TAffiliationItem=record
    sID:Core.Strings.VarString;
    sNick:Core.Strings.VarString;
    Affiliation:TChatAffiliation;
  end;
  TAffiliationList=Array of TAffiliationItem;
  TXMPPTime=record
    sUTC:Core.Strings.VarString;
    sTimeZone:Core.Strings.VarString;
    sDisplay:Core.Strings.VarString;
  end;
  TVersion=record
    sName:Core.Strings.VarString;
    sVersion:Core.Strings.VarString;
    sOS:Core.Strings.VarString;
  end;
  TIQ=record
    iqType        : TIQType;
    sID           : Core.Strings.VarString;
    sFrom         : Core.Strings.VarString;
    sFromResource : Core.Strings.VarString;
    sTo           : Core.Strings.VarString;
    sToResource   : Core.Strings.VarString;
  end;
  TRosterItem=Record
    ID            : Int64;
    Group         : Core.Strings.VarString;
    JID           : Core.Strings.VarString;
    Alias         : Core.Strings.VarString;
    First         : Core.Strings.VarString;
    Last          : Core.Strings.VarString;
    Subscription  : TSubscription;
  end;
  TRosterItems=Array of TRosterItem;
  TPresence=record
    Kind:TPresenceKind;
    sMessage:Core.Strings.VarString;
    sFrom:Core.Strings.VarString;
    sFromResource:Core.Strings.VarString;
    sTo:Core.Strings.VarString;
    sToResource:Core.Strings.VarString;
    Status:TStatus;
  end;
  TMessage=record
    sFrom:Core.Strings.VarString;
    sFromResource:Core.Strings.VarString;
    sTo:Core.Strings.VarString;
    sToResource:Core.Strings.VarString;
    sSubject:Core.Strings.VarString;
    sThread:Core.Strings.VarString;
    sBody:Core.Strings.VarString;
    Kind:TMessageKind;
    Stamp:TDateTime;
  end;
  TVcard=record
    sVersion:Core.Strings.VarString;
    sFrom:Core.Strings.VarString;
    sFullName:Core.Strings.VarString;
    SGiven:Core.Strings.VarString;
    sFamily:Core.Strings.VarString;
    sMiddle:Core.Strings.VarString;
    sNickname:Core.Strings.VarString;
    sURL:Core.Strings.VarString;
    sBDay:Core.Strings.VarString;
    sOrganization_Name:Core.Strings.VarString;
    sOrganization_Unit:Core.Strings.VarString;
    sTitle:Core.Strings.VarString;
    sRole:Core.Strings.VarString;
    sVoice:Core.Strings.VarString;
  end;
  TChatRoomInfo=record
    sID           : Core.Strings.VarString;
    sJID          : Core.Strings.VarString;
    sCategory     : Core.Strings.VarString;
    sType         : Core.Strings.VarString;
    sName         : Core.Strings.VarString;
    sNickName     : Core.Strings.VarString;
    sOwner        : Core.Strings.VarString;
    sSubject      : Core.Strings.VarString;
    sDescription  : Core.Strings.VarString;
    sOccupants    : Core.Strings.VarString;
    sLanguage     : Core.Strings.VarString;
    Flags         : TConferenceFlags;
  end;
  TChatRoom=record
    sService      : Core.Strings.VarString;
    sName         : Core.Strings.VarString;
    sJID          : Core.Strings.VarString;
    sNickname     : Core.Strings.VarString;
  end;
  TChatRoomParticipant=record
    sJID          : Core.Strings.VarString;
    sNickname     : Core.Strings.VarString;
  end;

  TXMPPFFKind=(ftBoolean,ftFixed,ftHidden,ftSingleID,ftMultiID,ftSingleList,
    ftMultiList,ftTextMulti,ftPrivateText,ftSingleText);
  TXMPPFormField=record
    Kind          : TXMPPFFKind;
    sVar          : Core.Strings.VarString;
    sLabel        : Core.Strings.VarString;
    sDescription  : Core.Strings.VarString;
    Required      : Boolean;
    Options       : TOptions;
    Values        : Core.Arrays.Types.VarString;
  end;
  TXMPPFormFields=Array of TXMPPFormField;
  TXMPPForm=record
    Title         : Core.Strings.VarString;
    Instructions  : Core.Strings.VarString;
    Fields        : TXMPPFormFields;
  end;

  Function  ChatRoleToString(Role:TChatRole):Core.Strings.VarString;
  Function  ChatRoleFromString(Role:Core.Strings.VarString):TChatRole;
  Function  AffiliationToString(Affiliation:TChatAffiliation):Core.Strings.VarString;
  Function  AffiliationFromString(Affiliation:Core.Strings.VarString):TChatAffiliation;
  Function  FormKindToString(Kind:TXMPPFFKind):Core.Strings.VarString;
  Function  FormKindFromString(Kind:Core.Strings.VarString):TXMPPFFKind;
  Function  StringFromConferenceFlag(Flags:TConferenceFlags):Core.Strings.VarString;
  Function  StringFromError(Errors:TErrors):Core.Strings.VarString;
  Function  IQTypeFromString(sString:Core.Strings.VarString):TIQType;
  Function  SubscriptionFromString(sString:Core.Strings.VarString):TSubscription;
  Function  SubscriptionFromByte(Value:Byte):TSubscription;
  Function  SubscriptionToByte(Value:TSubscription):Byte;
  Function  SubscriptionToString(Subscription:TSubscription):Core.Strings.VarString;
  Function  StatusFromString(sString:Core.Strings.VarString):TStatus;
  Function  StatusToString(Status:TStatus):Core.Strings.VarString;
  Function  PrepareTag(sName,sValue:Core.Strings.VarString):Core.Strings.VarString;
  Function  MessageKindFromString(sString:Core.Strings.VarString):TMessageKind;
  Function  MessageKindToString(Message:TMessageKind):Core.Strings.VarString;
  Function  PresenceKindToString(Presence:TPresenceKind):Core.Strings.VarString;
  Function  PresenceKindFromString(sString:Core.Strings.VarString):TPresenceKind;

  Function  MessageToString(Var Message:TMessage):Core.Strings.VarString;

  procedure DateTime(Var xTime:TXMPPTime); overload;

  Function  toDateTime(var xTime:TXMPPTime):TDateTime; overload;
  Function  IndexOfRosterItems(Var JID:Core.Strings.VarString; Var List:TRosterItems): LongInt;

  Function  IndexOfFunctionData(Name:Core.Strings.VarString; Var List:TXMPPFunctions): LongInt;
  Function  AddFunctionToList(Name:Core.Strings.VarString; Callback:TXMPPFunction; Var List:TXMPPFunctions): LongInt;



  Function  ConferenceFlagsFromStringArray(Var saData:Core.Arrays.Types.VarString):TConferenceFlags;
  procedure AddFormField(Var Form:TXMPPForm; Var Field:TXMPPFormField);

  procedure Init(Parser:TXMLParser); overload;
  procedure Init(Var Version:TVersion); overload;
  procedure Init(Var Field:TXMPPFormField); overload;

  procedure Done(Var Item:TXMPPFormField); overload;
  procedure Done(Var Item:TXMPPFormFields); overload;
  procedure Done(Var Item:TXMPPForm); overload;


  procedure Empty(Var Item:TXMPPFunctionData); overload;
  procedure Empty(Var Item:TXMPPFunctions); overload;
  procedure Empty(var Item:TXMPPFormFields); overload;
  procedure Empty(var Item:TXMPPFormField); overload;
  procedure Empty(var Item:TXMPPForm); overload;

  procedure Copy(Var Source,Destination:TXMPPForm); overload;

  Function  GetSHAPassword(FSessionID,FPassword:Core.Strings.VarString):Core.Strings.VarString;
  Function  JIDFromStrings(Name,Resource:Core.Strings.VarString):Core.Strings.VarString;
  Function  Clean_ID_Tag(ID:Core.Strings.VarString):Core.Strings.VarString;
  Function  GenerateStreamID:Core.Strings.VarString;
  procedure ModifyFormVariable(Var Form:TXMPPForm; Variable,Value:Core.Strings.VarString);
const
  XMPP_MUFS_HIDDEN:Core.Strings.VarString='muc_hidden';
  XMPP_MUFS_PUBLIC:Core.Strings.VarString='muc_public';
  XMPP_MUFS_UNSECURE:Core.Strings.VarString='muc_unsecure';
  XMPP_MUFS_PWDPROTECTED:Core.Strings.VarString='muc_passwordprotected';
  XMPP_MUFS_PERSISTENT:Core.Strings.VarString='muc_persitent';
  XMPP_MUFS_TEMPORARY:Core.Strings.VarString='muc_temporary';
  XMPP_MUFS_OPEN:Core.Strings.VarString='muc_open';
  XMPP_MUFS_MEMBERSONLY:Core.Strings.VarString='muc_membersonly';
  XMPP_MUFS_MODERATED:Core.Strings.VarString='muc_moderated';
  XMPP_MUFS_UNMODERATED:Core.Strings.VarString='muc_unmoderated';
  XMPP_MUFS_NONANONYMOUS:Core.Strings.VarString='muc_nonanonymous';
  XMPP_MUFS_SEMIANONYMOUS:Core.Strings.VarString='muc_semianonymous';

  NS_Stanzas='urn:ietf:params:xml:ns:xmpp-stanzas';
  NS_Dialback='jabber:server:dialback';
  NS_Bind='urn:ietf:params:xml:ns:xmpp-bind';
  NS_Streams='http://etherx.jabber.org/streams';
  NS_TLS='urn:ietf:params:xml:ns:xmpp-tls';
  NS_SASL='urn:ietf:params:xml:ns:xmpp-sasl';
  //NS_Streams='urn:ietf:params:xml:ns:xmpp-streams';
  NS_Client='jabber:client';
  NS_Server='jabber:server';
  Stream_Error='stream:error';
  Stream_Stream='stream:stream';
  Stream_Features='stream:features';
  NS_AUTH='jabber:iq:auth';
  MESSAGE='message';
  NS_REG='jabber:iq:register';
  NS_VER='jabber:iq:version';
  NS_DELAY='jabber:x:delay';
  NS_DATA='jabber:x:data';
  NS_TIME='jabber:iq:time';
  NS_AGENTS='jabber:iq:agents';
  NS_JR='jabber:iq:roster';
  NS_MUC='http://jabber.org/protocol/muc';
  NS_MUC_ROOM_OWNER='muc#roominfo_owner';
  NS_MUC_ROOM_LANG='muc#roominfo_lang';
  NS_MUC_ROOM_OCCUPANTS='muc#roominfo_occupants';
  NS_MUC_ROOM_SUBJECT='muc#roominfo_subject';
  NS_MUC_ROOM_DESCRIPTION='muc#roominfo_description';
  NS_MUC_INFO='http://jabber.org/protocol/muc#roominfo';
  NS_MUC_ADMIN='http://jabber.org/protocol/muc#admin';
  NS_MUC_USER='http://jabber.org/protocol/muc#user';
  NS_MUC_OWNER='http://jabber.org/protocol/muc#owner';
  NS_DISCO_INFO='http://jabber.org/protocol/disco#info';
  NS_DISCO_ITEMS='http://jabber.org/protocol/disco#items';


Const
  Sessions_Lcv:Cardinal=103129;

implementation

uses DateUtils;

Function  GenerateStreamID:Core.Strings.VarString;
begin
  Result:=Concat(IntToHex(Sessions_Lcv,5),IntToHex(Random(1024),4));
end;

Function  Clean_ID_Tag(ID:Core.Strings.VarString):Core.Strings.VarString;
begin
  If ID='' then Result:='' else Result:=Concat('id="',ID,'" ');
end;

Function  AddFunctionToList(Name:Core.Strings.VarString; Callback:TXMPPFunction; Var List:TXMPPFunctions): LongInt;
begin
  Result:=Length(List);
  SetLength(List,Result+1);
  List[Result].Name:=Name;
  List[Result].Execute:=Callback;
end;

Function  IndexOfFunctionData(Name:Core.Strings.VarString; Var List:TXMPPFunctions): LongInt;
var
  iCount,iLcv:LongInt;
begin
  Result:=-1; iCount:=Length(List); iLcv:=0;
  While (iLCv<iCount) and (Result=-1) do begin
    If AnsiCompareText(Name,List[iLcv].Name)=0 then
      Result:=iLcv;
    Inc(iLCv);
  end;
end;

Function  IndexOfRosterItems(Var JID:Core.Strings.VarString; Var List:TRosterItems): LongInt;
var
  iCount,iLcv:LongInt;
begin
  Result:=-1; iCount:=Length(List);  iLcv:=0;
  While (iLcv<iCount) and (Result=-1) do begin
    If List[iLcv].JID=JID then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function GetSHAPassword(FSessionID,FPassword:Core.Strings.VarString):Core.Strings.VarString;
var
  Context : TSHA1Context;
  Digest  : TSHA1Digest;
  sData   : Core.Strings.VarString;
begin
  sData:=Concat(FSessionID,FPassword);
  Encryption.SHA.Init(Context);
  Encryption.SHA.Update(Context,sData);
  Encryption.SHA.Final(Context,Digest);
  Result:=Lowercase(toHexString(@Digest,SizeOf(Digest)));
end;



procedure Init(Var Field:TXMPPFormField);
begin
  Field.Kind:=ftSingleText;
  SetLength(Field.sVar,0);
  SetLength(Field.sLabel,0);
  SetLength(Field.sDescription,0);
  Field.Required:=False;
  Core.Options.Init(Field.Options);
  Core.Arrays.VarString.Init(Field.Values);
end;

procedure Copy(Var Source,Destination:TXMPPForm);
var
  iTag,jLcv,iLcv:LongInt;
begin
  Empty(Destination);
  Destination.Title:=Source.Title;
  Destination.Instructions:=Source.Instructions;
  SetLength(Destination.Fields,Length(Source.Fields));
  For iLcv:=0 to High(Source.Fields) do begin
    Destination.Fields[iLcv].Kind:=Source.Fields[iLcv].Kind;
    Destination.Fields[iLcv].sVar:=Source.Fields[iLcv].sVar;
    Destination.Fields[iLcv].sLabel:=Source.Fields[iLcv].sLabel;
    Destination.Fields[iLcv].sDescription:=Source.Fields[iLcv].sDescription;
    Destination.Fields[iLcv].Required:=Source.Fields[iLcv].Required;
    SetLength(Destination.Fields[iLcv].Options,Length(Source.Fields[iLcv].Options));
    For jLcv:=0 to High(Source.Fields[iLcv].Options) do begin
      Destination.Fields[iLcv].Options[jLcv].sLabel:=Source.Fields[iLcv].Options[jLcv].sLabel;
      Destination.Fields[iLcv].Options[jLcv].sLabel:=Source.Fields[iLcv].Options[jLcv].sLabel;
    end;
    SetLength(Destination.Fields[iLcv].Values,Length(Source.Fields[iLcv].Values));
    For jLcv:=0 to High(Source.Fields[iLcv].Values) do
      Destination.Fields[iLcv].Values[jLcv]:=Source.Fields[iLcv].Values[jLcv];
  end;
end;

procedure ModifyFormVariable(Var Form:TXMPPForm; Variable,Value:Core.Strings.VarString);
Const
  YesNo:Array[Boolean] of Core.Strings.VarString=('no','yes');
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Form.Fields) do begin
    If Form.Fields[iLcv].sVar=Variable then begin
      If Length(Form.Fields[iLcv].Values)=0 then
        SetLength(Form.Fields[iLcv].Values,1);
      Case Form.Fields[iLcv].Kind of
        ftBoolean: begin
            Value:=Lowercase(Value);
            If (Value='no') or (Value='0') then
              Form.Fields[iLcv].Values[0]:='0'
            else
              Form.Fields[iLcv].Values[0]:='1';
          end;
        ftMultiList,ftTextMulti,ftMultiID: begin
            SetLength(Form.Fields[iLcv].Values,0);
            Core.Arrays.VarString.fromString(Form.Fields[iLcv].Values,Value);
          end;
        ftFixed,ftHidden,ftSingleID,ftSingleList,ftPrivateText,ftSingleText: Form.FIelds[iLcv].Values[0]:=Value;
      end;
    end;
  end;
end;

procedure AddFormField(Var Form:TXMPPForm; Var Field:TXMPPFormField);
var
  iCount:LongInt;
begin
  iCount:=Length(Form.Fields);
  SetLength(Form.Fields,iCount+1);
  Form.Fields[iCount]:=Field;
end;

procedure Done(Var Item:TXMPPFormField);
begin
  Finalize(Item.sVar);
  Finalize(Item.sLabel);
  Finalize(Item.sDescription);
  Done(Item.Options);
  Done(Item.Values);
  Finalize(Item);
end;

procedure Done(Var Item:TXMPPFormFields);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure Done(Var Item:TXMPPForm);
begin
  Done(Item.Fields);
  Finalize(Item.Instructions);
  Finalize(Item.Title);
  Finalize(Item);
end;

procedure Empty(var Item:TXMPPFormFields);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do Done(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Empty(var Item:TXMPPFormField);
begin
  Item.Kind:=ftBoolean;
  SetLength(Item.sVar,0);
  SetLength(Item.sLabel,0);
  SetLength(Item.sDescription,0);
  Item.Required:=false;
  Empty(Item.Options);
  Empty(Item.Values);
end;

procedure Empty(var Item:TXMPPForm);
begin
  SetLength(Item.Title,0);
  SetLength(Item.Instructions,0);
  Empty(Item.Fields);
end;

Function  PrepareTag(sName,sValue:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:='';
  If sValue<>'' then
    Result:=Concat('<',sName,'>',sValue,'</',sName,'>');
end;

Function  JIDFromStrings(Name,Resource:Core.Strings.VarString):Core.Strings.VarString;
begin
  If Resource='' then
    Result:=Name
  else
    Result:=Concat(Name,'/',Resource);
end;

Function  ChatRoleToString(Role:TChatRole):Core.Strings.VarString;
begin
  Case Role of
   crModerator:Result:='moderator';
   crParticipant:Result:='participant';
   crVisitor:Result:='visitor';
  else
    Result:='none';
  end;
end;

Function  ChatRoleFromString(Role:Core.Strings.VarString):TChatRole;
begin
  If Role='moderator' then
    Result:=crModerator
  else If Role='participant' then
    Result:=crParticipant
  else If Role='visitor' then
    Result:=crVisitor
  else
    Result:=crNone;
end;

Function  AffiliationToString(Affiliation:TChatAffiliation):Core.Strings.VarString;
begin
  Case Affiliation of
    caOwner:Result:='owner';
    caAdmin:Result:='admin';
    caMember:Result:='member';
    caOutcast:Result:='outcast';
  else
    Result:='none';
  end;
end;

Function  AffiliationFromString(Affiliation:Core.Strings.VarString):TChatAffiliation;
begin
  If Affiliation='owner' then
    Result:=caOwner
  else if Affiliation='admin' then
    Result:=caAdmin
  else if Affiliation='member' then
    Result:=caMember
  else if Affiliation='outcast' then
    Result:=caOutcast
  else if Affiliation='none' then
    Result:=caNone
  else
    Result:=caInvalid;
end;

Function  FormKindToString(Kind:TXMPPFFKind):Core.Strings.VarString;
begin
  // http://www.jabber.org/jeps/jep-0004.html
  Case Kind of
    ftBoolean:Result:='boolean';
    ftFixed:Result:='fixed';
    ftHidden:Result:='hidden';
    ftSingleID:Result:='jid-single';
    ftMultiID:Result:='jid-multi';
    ftSingleList:Result:='list-single';
    ftMultiList:Result:='list-multi';
    ftTextMulti:Result:='text-multi';
    ftPrivateText:Result:='text-private';
    ftSingleText:Result:='text-single';
  else
    Result:='';
  end;
end;

Function  FormKindFromString(Kind:Core.Strings.VarString):TXMPPFFKind;
begin
  // http://www.jabber.org/jeps/jep-0004.html

  If Kind='boolean' then
    Result:=ftBoolean
  else if Kind='fixed' then
    Result:=ftFixed
  else if Kind='hidden' then
    Result:=ftHidden
  else if Kind='jid-multi' then
    Result:=ftMultiID
  else if Kind='jid-single' then
    Result:=ftSingleID
  else if Kind='list-multi' then
    Result:=ftMultiList
  else if Kind='list-single' then
    Result:=ftSingleList
  else if Kind='text-multi' then
    Result:=ftTextMulti
  else if Kind='text-private' then
    Result:=ftPrivateText
  else if Kind='text-single' then
    Result:=ftSingleText;
end;

Function  StringFromConferenceFlag(Flags:TConferenceFlags):Core.Strings.VarString;
var
  saData:Core.Arrays.Types.VarString;
begin
  Try
    Core.Arrays.VarString.Init(saData);
    If cPrivate in Flags then
      Core.Arrays.VarString.Add(@saData,'hidden');
    If cPublic in Flags then
      Core.Arrays.VarString.Add(@saData,'public');
    If cUnsecure in Flags then
      Core.Arrays.VarString.Add(@saData,'unsecure');
    If cPasswordRequired in Flags then
      Core.Arrays.VarString.Add(@saData,'passwordprotected');
    If cPersistent in Flags then
      Core.Arrays.VarString.Add(@saData,'persitent');
    If cTemporary in Flags then
      Core.Arrays.VarString.Add(@saData,'temporary');
    If cOpen in Flags then
      Core.Arrays.VarString.Add(@saData,'open');
    If cMembersOnly in Flags then
      Core.Arrays.VarString.Add(@saData,'membersonly');
    If cUnModerated in Flags then
      Core.Arrays.VarString.Add(@saData,'unmoderated');
    If cModerated in Flags then
      Core.Arrays.VarString.Add(@saData,'moderated');
    If cNonAnonymous in Flags then
      Core.Arrays.VarString.Add(@saData,'nonanonymous');
    If cSemiAnonymous in Flags then
      Core.Arrays.VarString.Add(@saData,'semianonymous');
    Result:=Core.Arrays.VarString.toString(saData,',');
  Finally
    Done(saData);
  end;
end;

Function  ConferenceFlagsFromStringArray(Var saData:Core.Arrays.Types.VarString):TConferenceFlags;
begin
  // TConferenceFlag=(cPrivate,cPublic,cPersistent,cTemporary,cPasswordRequired,
  // cUnsecure,cOpen,cMembersOnly,cNonAnonymous,cSemiAnonymous,cModerated,cUnModerated);
  Result:=[];
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_HIDDEN)>-1 then
    Include(Result,cPrivate);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_PUBLIC)>-1 then
    Include(Result,cPublic);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_PUBLIC)>-1 then
    Include(Result,cUnsecure);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_PWDPROTECTED)>-1 then
    Include(Result,cPasswordRequired);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_PERSISTENT)>-1 then
    Include(Result,cPersistent);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_TEMPORARY)>-1 then
    Include(Result,cTemporary);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_OPEN)>-1 then
    Include(Result,cOpen);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_MEMBERSONLY)>-1 then
    Include(Result,cMembersOnly);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_UNMODERATED)>-1 then
    Include(Result,cUnModerated);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_MODERATED)>-1 then
    Include(Result,cModerated);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_NONANONYMOUS)>-1 then
    Include(Result,cNonAnonymous);
  if Core.Arrays.VarString.IndexOf(saData,XMPP_MUFS_SEMIANONYMOUS)>-1 then
    Include(Result,cSemiAnonymous);
end;

procedure Init(Parser:TXMLParser);
begin
  Core.Arrays.KeyString.Add(@Parser.ReplaceValues,'&apos;','&');
  Core.Arrays.VarString.Add(@Parser.EndTags,'/field');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'starttls');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'stream:stream');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'stream:error');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'iq');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'query');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'text');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'agent');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'presence');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'error');
  //ccUtils.AddString(Parser.StanzaBreaks,'item');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'message');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'bad-format');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'bad-namespace-prefix');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'conflict');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'connection-timeout');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'host-gone');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'host-unknown');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'improper-addressing');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'internal-server-error');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'invalid-from');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'invalid-id');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'invalid-namespace');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'invalid-xml');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'not-authorized');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'policy-violation');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'remote-connection-failed');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'resource-constraint');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'restricted-xml');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'see-other-host');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'system-shutdown');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'undefined-condition');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'unsupported-encoding');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'unsupported-stanza-type');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'unsupported-version');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'xml-not-well-formed');
  Core.Arrays.VarString.Add(@Parser.StanzaBreaks,'vcard');
end;

Function  toDateTime(var xTime:TXMPPTime):TDateTime;
var
  stValue:TSystemTime;
  iBias:LongInt;
begin
   {
   20020910T17:58:35
            11111111
   12345678901234567
   |YR|M|D|T|| || ||
            H  M  S
   }
  If xTime.sUTC='' then begin
    Result:=Now;
  end else begin
    iBias:=GMTBiasfromString(xTime.sTimeZone);

    stValue.Year:=StrToIntDef(System.Copy(xTime.sUTC,1,4),stLocalTime.Year);
    stValue.Month:=StrToIntDef(System.Copy(xTime.sUTC,5,2),stLocalTime.Month);
    stValue.Day:=StrToIntDef(System.Copy(xTime.sUTC,8,2),stLocalTime.Day);
    stValue.Hour:=StrToIntDef(System.Copy(xTime.sUTC,10,2),stLocalTime.Hour);
    stValue.Minute:=StrToIntDef(System.Copy(xTime.sUTC,13,2),stLocalTime.Minute);
    stValue.Second:=StrToIntDef(System.Copy(xTime.sUTC,16,2),stLocalTime.Second);
    stValue.MilliSecond:=0;

    Result:=Core.Utils.Time.toDateTime(stValue.Hour,stValue.Minute,stValue.Second,stValue.Millisecond,stValue.Day,stValue.Month,stValue.Year,iBias);
  end;
end;

procedure DateTime(Var xTime:TXMPPTime);
Var
  stValue:TSystemTime;
begin
  stValue:=Core.Utils.Time.stUniversal;
  //<utc>20020910T17:58:35</utc>
  //<display>Tue Sep 10 12:58:35 2002</display>
  xTime.sUTC:=Concat(
     Format('%.4d',[stValue.Year]),
     Format('%.2d',[stValue.Month]),
     Format('%.2d',[stValue.Day]),
     'T',
     Format('%.2d',[stValue.Hour]),
     Format('%.2d',[stValue.Minute]),
     Format('%.2d',[stValue.Second])
  );
  xTime.sTimeZone:=Core.Utils.Time.TZIBias;
  xTime.sDisplay:=Concat(
     Day_Short[DayOfWeek(Core.Timer.dtNow)],' ',
     Month_Short[MonthOf(Core.Timer.dtNow)],' ',
     Format('%.2d',[Core.Utils.Time.stLocalTime.Day]),' ',
     Format('%.2d',[Core.Utils.Time.stLocalTime.Hour]),':',
     Format('%.2d',[Core.Utils.Time.stLocalTime.Minute]),':',
     Format('%.2d',[Core.Utils.Time.stLocalTime.Second]),' ',
     Format('%.4d',[Core.Utils.Time.stLocalTime.Year])
   );
end;

procedure Init(Var Version:TVersion);
begin
  SetLength(Version.sName,0);
  SetLength(Version.sVersion,0);
  SetLength(Version.sOS,0);
end;

Function StringFromError(Errors:TErrors):Core.Strings.VarString;
var
  saResult:Core.Arrays.Types.VarString;
begin
  Try
    If eAuthenticate in Errors then
      Core.Arrays.VarString.Add(@saResult,'eAuthenticate');
    If eLogin in Errors then
      Core.Arrays.VarString.Add(@saResult,'eLogin');
    If eStream in Errors then
      Core.Arrays.VarString.Add(@saResult,'eStream');
    Result:=Core.Arrays.VarString.toString(saResult,' ');
  Finally
    SetLength(saResult,0);
  end;
end;

Function  PresenceKindToString(Presence:TPresenceKind):Core.Strings.VarString;
begin
  Case Presence of
    pSubscribe:Result:='subscribe';
    pSubscribed:Result:='subscribed';
    pUnsubscribe:Result:='unsubscribe';
    pUnsubscribed:Result:='unsubscribed';
    pProbe:Result:='probe';
    pError:Result:='error';
    pNone:Result:='';
  else
    Result:='';
  end;
end;

Function  PresenceKindFromString(sString:Core.Strings.VarString):TPresenceKind;
begin
  If sString='subscribe' then
    Result:=pSubscribe
  else if sString='subscribed' then
    Result:=pSubscribed
  else if sString='available' then
    Result:=pAvailable
  else if sString='unavailable' then
    Result:=pUnavailable
  else if sString='unsubscribe' then
    Result:=pUnsubscribe
  else if sString='unsubscribed' then
    Result:=pUnsubscribed
  else if sString='error' then
    Result:=pError
  else if sString='probe' then
    Result:=pProbe
  else if sString='' then
    Result:=pNone
  else
    Result:=pNotify;
end;


Function IQTypeFromString(sString:Core.Strings.VarString):TIQType;
begin
  If sString='get' then
    Result:=iqGet
  else if sString='set' then
    Result:=iqSet
  else if sString='result' then
    Result:=iqResult
  else if sString='error' then
    Result:=iqError
  else
    Result:=iqUnknown;
end;

Function   MessageToString(Var Message:TMessage):Core.Strings.VarString;
begin
  Result:=Concat('<message from="',JIDFromStrings(Message.sFrom,Message.sFromResource),'" to="',JIDFromStrings(Message.sTo,Message.sToResource),'" type="',MessageKindToString(Message.Kind),'">',PrepareTag('subject',Message.sSubject),PrepareTag('thread',Message.sThread),'<body>',Message.sBody,'</body>','</message>');
end;


Function  MessageKindFromString(sString:Core.Strings.VarString):TMessageKind;
begin
  if sString='error' then
    Result:=mError
  else if sString='groupchat' then
    Result:=mGroupChat
  else if sString='headline' then
    Result:=mHeadline
  else if sString='normal' then
    Result:=mNormal
  else
    Result:=mChat;
end;

Function  MessageKindToString(Message:TMessageKind):Core.Strings.VarString;
begin
  Case Message of
    mChat      :Result:='chat';
    mError     :Result:='error';
    mGroupChat :Result:='groupchat';
    mHeadline  :Result:='headline';
    mNormal    :Result:='normal';
  end;
end;

Function  SubscriptionFromByte(Value:Byte):TSubscription;
begin
  Case Value of
    0:Result:=sNone;
    1:Result:=sTo;
    2:Result:=sFrom;
    3:Result:=sBoth;
  end;
end;

Function  SubscriptionToByte(Value:TSubscription):Byte;
begin
  Case Value of
    sNone:Result:=0;
    sTo:Result:=1;
    sFrom:Result:=2;
    sBoth:Result:=3;
  end;
end;

Function  SubscriptionFromString(sString:Core.Strings.VarString):TSubscription;
begin
  if sString='to' then
    Result:=sTo
  else if sString='from' then
    Result:=sFrom
  else if sString='both' then
    Result:=sBoth
  else
    Result:=sNone;
end;

Function  SubscriptionToString(Subscription:TSubscription):Core.Strings.VarString;
begin
  Case Subscription of
    sNone:Result:='none';
    sTo:Result:='to';
    sFrom:Result:='from';
    sBoth:Result:='both';
  end;
end;

Function  StatusFromString(sString:Core.Strings.VarString):TStatus;
begin
  // TStatus=(jNone,jChat,jAway,jExtendedAway,jDND,jUnavailable);
  If sString='chat' then
    Result:=jChat
  else if sString='away' then
    Result:=jAway
  else if sString='xa' then
    Result:=jExtendedAway
  else if sString='dnd' then
    Result:=jDND
  else if sString='unavailable' then
    Result:=jUnavailable
  else
    Result:=jOnline;
end;

Function  StatusToString(Status:TStatus):Core.Strings.VarString;
begin
  // TStatus=(jNone,jChat,jAway,jExtendedAway,jDND,jUnavailable);
  Case Status of
    jChat:Result:='chat';
    jAway:Result:='away';
    jExtendedAway:Result:='xa';
    jDnD:Result:='dnd';
    jOnline:Result:='online';
    jUnavailable:Result:='unavailable';
  else
    Result:='unknown';
  end;
end;

procedure Empty(Var Item:TXMPPFunctiondata);
begin
  SetLength(Item.Name,0);
  Item.Execute:=Nil;
end;

procedure Empty(Var Item:TXMPPFunctions);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    empty(Item[iLcv]);
  SetLength(Item,0);
end;



end.

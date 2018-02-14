unit hXMPPd;

interface

Type
  TStreamError=(seBadFormat,seBadNSPrefix,seConflict,seConnectionTimeout,
    seHostGone,seHostUnknown,seImproperAddressing,seInternalServerError,seInvalidFrom,
    seInvalidID,seInvalidNamespace,seInvalidXML,seNotAuthorized,sePolicyViolation,
    seRemoteConnectionFailed,seResourceConstraint,seRestrictedXML,seSeeOtherHost,
    seSystemShutdown,seUndefinedCondition,seUnsupportedEncoding,seUnsupportedStanzaType,
    seUnsupportedVersion,seMalFormedXML
  );
  TStanzaError=(szeBadRequest,szeConflict,szeFeatureNotImplemented,szeForbidden,szeGone,
    szeInternalServerError,szeItemNotFound,szeJIDMalFormed,szeNotAcceptable,szeNotAllowed,
    szeNotAuthorized,szePaymentRequired,szeRecipientUnavailable,szeRedirect,szeRegistrationRequired,
    szeRemoteServerNotFound,szeRemoteServerTimedOut,szeResourceConstraint,szeServiceUnavailable,
    szeSubscriptionRequired,szeUndefinedCondition,szeUnexpectedRequest
  );
  TErrorType=(etCancel,etContinue,etModify,etAuth,etWait);
  TSessionState=(ssResolveDNS,ssAquireStream,ssStream,ssAuth,ssClose);
  TSessionStates=Set of TSessionState;

  Const
    XMPP_SERVER_TO_CLIENT_PORT=5222;
    XMPP_SERVER_TO_SERVER_PORT=5223;

    _Public      ='public';
    _Private     ='private';
    _Directory   ='directory';
    ERROR_CANCEL ='<error type="cancel"><service-unavailable xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>';
    
    SessionErrors:Array[etCancel..etWait] of String=(
      'cancel',
      'continue',
      'modify',
      'auth',
      'wait'
    );
    StanzaErrorMessages:Array[szeBadRequest..szeUnexpectedRequest] of String=(
      'bad-request',
      'conflict',
      'feature-not-implemented',
      'forbidden',
      'gone',
      'internal-server-error',
      'item-not-found',
      'jid-malformed',
      'not-acceptable',
      'not-allowed',
      'not-authorized',
      'payment-required',
      'recipient-unavailable',
      'redirect',
      'registration-required',
      'remote-server-not-found',
      'remote-server-timeout',
      'resource-constraint',
      'service-unavailable',
      'subscription-required',
      'undefined-condition',
      'unexpected-request'
    );
    StreamErrorMessages:Array[seBadFormat..seMalFormedXML] of String=(
      'bad-format',
      'bad-namespace-prefix',
      'conflict',
      'connection-timeout',
      'host-gone',
      'host-unknown',
      'improper-addressing',
      'internal-server-error',
      'invalid-from',
      'invalid-id',
      'invalid-namespace',
      'invalid-xml',
      'not-authorized',
      'policy-violation',
      'remote-connection-failed',
      'resource-constraint',
      'restricted-xml',
      'see-other-host',
      'system-shutdown',
      'undefined-condition',
      'unsupported-encoding',
      'unsupported-stanza-type',
      'unsupported-version',
      'xml-not-well-formed'
    );

Type
  TAgent=record
    ID                           : String;
    Name                         : String;
    Service                      : String;
    Description                  : String;
    Features                     : String;
  end;

  Function  AgentToXML(Var Agent:TAgent):String;

  procedure Empty(Var Item:TAgent); overload;
  procedure Init(Var Item:TAgent); overload;
  procedure Done(Var Item:TAgent); overload;


implementation

Function AgentToXML(Var Agent:TAgent):String;
begin
  Result:=Concat('<agent jid="',Agent.ID,'"><name>',Agent.Name,'</name><service>',Agent.Service,'</service>',Agent.Features,'</agent>');
end;

procedure Empty(Var Item:TAgent);
begin
  With Item do begin
    SetLength(ID,0);
    SetLength(Name,0);
    SetLength(Service,0);
    SetLength(Description,0);
    SetLength(Features,0);
  end;
end;

procedure Init(Var Item:TAgent);
begin
  With Item do begin
    SetLength(ID,0);
    SetLength(Name,0);
    SetLength(Service,0);
    SetLength(Description,0);
    SetLength(Features,0);
  end;
end;

procedure Done(Var Item:TAgent);
begin
  With Item do begin
    Finalize(ID);
    Finalize(Name);
    Finalize(Service);
    Finalize(Description);
    Finalize(Features);
  end;
  Finalize(Item);
end;

end.

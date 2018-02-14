  TTransportBase=class
  private
    FRSRP                 : PRSR;
    FOwner                : TMailManager;
  protected
    procedure OnQueued(RSRP:PRSR); virtual; abstract;
    procedure OnDataReceived(RSRP:PRSR; var Handled:boolean); virtual; abstract;
    procedure OnErrored(RSRP:PRSR); virtual; abstract;
    procedure OnConnected(RSRP:PRSR); virtual; abstract;
    procedure OnDisconnected(RSRP:PRSR); virtual; abstract;
    procedure OnInitialized(RSRP:PRSR); virtual; abstract;
    procedure OnFinalized(RSRP:PRSR); virtual; abstract;
    procedure OnDNSResult(RSRP:PRSR); virtual; abstract;
  public
    constructor Create(aOwner:TMailManager);
  end;


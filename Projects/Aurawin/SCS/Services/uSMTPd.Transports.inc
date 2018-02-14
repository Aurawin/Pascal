Type
  TTransportBase=class
  protected
    procedure OnDataReceived(RSRP:PRSR; var Handled:boolean); virtual; abstract;
  end;

  TTransportReceive=class
  protected
    procedure OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
  end;

  TTransportRelay=class
  protected
    procedure OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
  end;
end;

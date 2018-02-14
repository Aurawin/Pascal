TTransportRelay=class(TTransportBase)
private
  FRelayP           : Storage.UserStorage.Items.SMTP.PRelayMail;
  FRelayService     : Storage.MatrixServices.Items.Item;
  FResponse         : Core.Strings.VarString;
  FMeta             : Core.Strings.VarString;
  FTTL              : Double;
  FNodeID           : QWord;
  FParameters       : Core.Arrays.Types.VarString;
  FFolderID         : QWord;
  FFileID           : QWord;
private
  procedure   Bounce(Task:Core.Database.Types.TTask; Refactor:TMemoryStream; RelayP:Storage.UserStorage.Items.SMTP.PRelayMail; Notice:Core.Strings.VarString);
  procedure   WriteConfirmation();
  procedure   Retry(Increment:boolean);
protected
  procedure OnInitialized(RSRP:PRSR); override;
  procedure OnConnected(RSRP:PRSR); override;
  procedure OnQueued(RSRP:PRSR); override;
  procedure OnDisconnected(RSRP:PRSR); override;
  procedure OnDataReceived(RSRP:PRSR; var Handled:boolean); override;
  procedure OnDNSResult(RSRP:PRSR); override;
  procedure OnFinalized(RSRP:PRSR); override;
  procedure OnErrored(RSRP:PRSR); override;
end;


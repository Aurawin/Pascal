unit uFTPD;

interface
uses classes,clFtpServer,uStorage;

type
  TFTPServer=class(TclFtpServer)
  private

  public
    Constructor Create(AOwner:TComponent); override;
  end;

implementation


Constructor TFTPServer.Create(AOwner:TComponent);
begin
  Inherited Create(AOwner);
end;
end.

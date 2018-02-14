constructor cmProcessSyncItems.Create(aManager:TRSRManager; aRSRP:PRSR);
begin
  FRequest:=THTTPRequest.Create();
  Inherited Create(aManager,aRSRP);
end;

destructor cmProcessSyncItems.Destroy();
begin
  FRequest.Free();
  inherited Destroy();
end;

procedure cmProcessSyncItems.Execute();
var
  Man:TAuSocketMan;
begin
  Man:=TAuSocketMan(FOwner);
  FProcessor.EntryPoint:='uEngine.cmProcessSyncItems.Execute';
  Man.FSyncQueue.Process(FProcessor,FRSRP,FRequest);
end;


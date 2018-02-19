TScanThread=class(CThread)
private
  FOwner   : TAuSocketMan;
  FPipeP   : Storage.Social.Sync.Pipes.PItem;
  FSyncP   : Storage.Social.Sync.PSync;
  FRequest : THTTPRequest;
  FSleepP  : PRTLEvent;
  FRSRP    : PRSR;
private
  function checkForFilesRefresh(RSRP:PRSR; Expires:Double):boolean;
  function checkForFoldersRefresh(RSRP:PRSR; Expires:Double):boolean;
protected
  procedure doTerminate();
public
  constructor Create(
    aOwner   : TAuSocketMan;
    aPipeP   : Storage.Social.Sync.Pipes.PItem;
    aSyncP   : Storage.Social.Sync.PSync;
    aRSRP    : PRSR
  ); virtual; reIntroduce;
  destructor Destroy; override;
protected
  procedure Execute(); override;
  function Process(RSRP:PRSR):DWORD; virtual; abstract;
end;

TScannerUploadThread=class(TScanThread)
protected
  function Process(RSRP:PRSR):DWORD; override;
end;

TScannerDownloadThread=class(TScanThread)
protected
  function Process(RSRP:PRSR):DWORD; override;
end;

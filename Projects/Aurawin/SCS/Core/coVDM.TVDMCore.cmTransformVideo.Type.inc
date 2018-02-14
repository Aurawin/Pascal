cmTransformVideo=class(TRSRMethod)
private
  FDomainID       : QWord;
  FUserID         : QWord;
  FSourceFile     : string;
  FDestFile       : string;
  FSource         : Storage.UserStorage.Files.TItem;
  FDest           : Storage.UserStorage.Files.TItem;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; aDomainID,aUserID:Qword; var aSource,aDest:Storage.UserStorage.Files.TItem; aSourceFile,aDestFile:string); reIntroduce;
  destructor Destroy; override;
end;


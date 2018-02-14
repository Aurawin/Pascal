cmTransformVideo=class(TRSRMethod)
private
  FDomainID       : QWord;
  FUserID         : QWord;
  FSourceFile     : string;
  FDestFile       : string;
  FNetwork        : Storage.Social.Network.TNetwork;
  FSource         : Storage.Social.Files.TSFile;
  FDest           : Storage.Social.Files.TSFile;
protected
  procedure Execute(); override;
public
  constructor Create(aManager:TRSRManager; aRSRP:PRSR; aNetwork:Storage.Social.Network.TNetwork; aDomainID,aUserID:Qword; var aSource,aDest:Storage.Social.Files.TSFile; aSourceFile,aDestFile:string); reIntroduce;
  destructor Destroy; override;
end;


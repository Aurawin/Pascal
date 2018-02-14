constructor TVDMCore.cmTransformVideo.Create(aManager:TRSRManager; aRSRP:PRSR; aDomainID,aUserID:Qword; var aSource,aDest:Storage.UserStorage.Files.TItem; aSourceFile,aDestFile:string);
begin
  FDomainID:=aDomainID;
  FUserID:=aUserID;
  FSourceFile:=aSourceFile;
  FDestFile:=aDestFile;
  Storage.UserStorage.Files.Copy(aSource,FSource);
  Storage.UserStorage.Files.Copy(aDest,FDest);
  Inherited Create(aManager,aRSRP);
end;

destructor TVDMCore.cmTransformVideo.Destroy;
begin
  Storage.UserStorage.Files.Done(FDest);
  Storage.UserStorage.Files.Done(FSource);
  inherited Destroy();
end;

procedure TVDMCore.cmTransformVideo.Execute();
var
  FProc:TProcess;
  FSFile:TFileStream;
  isXML:TXMLInputSource;
  xDocument:TXMLDocument;
  xNode:TDOMNode;
  FSummary:Storage.Summary.Video.TItem;
begin
  FProcessor.EntryPoint:='coVDM.TVDMCore.cmTransformVideo.Execute';
  Storage.Summary.Video.Init(FSummary);
  try
    if System.Length(FDest.Summary)>0 then begin
      isXML:=TXMLInputSource.Create(FDest.Summary);
      Try
       xDocument:=TXMLDocument.Create();
       Try
         xNode:=FProcessor.XMLParser.ParseWithContext(isXML,xDocument.DocumentElement,xaAppendAsChildren);
         Storage.Summary.Video.fromXML(xNode,FSummary);
       finally
         xDocument.Free();
       end;
      Finally
        isXML.Free();
      end;
    end;

    FSummary.Status:=Storage.Summary.Video.Status.Encoding;
    FProcessor.Refactor.Size:=0;
    Storage.Summary.Video.toXML(FSummary,FProcessor.Refactor);
    FDest.Summary:=Core.Streams.toString(FProcessor.Refactor);
    FProcessor.Refactor.Size:=0;
    Storage.UserStorage.Files.DB.SetSummary(FProcessor.Task,FDomainID,FUserID,FDest.ID,FDest.Summary,FDest.Flags);

    FProc:=TProcess.Create(nil);
    Try
      FProc.Executable:='avconv';
      FPRoc.Parameters.Add('-i');
      FPRoc.Parameters.Add(FSourceFile);
      FPRoc.Parameters.Add('-vcodec');
      FPRoc.Parameters.Add('libx264');

      FPRoc.Parameters.Add('-s');
      FPRoc.Parameters.Add('640x360');

      FPRoc.Parameters.Add('-b');
      FPRoc.Parameters.Add('120k');
      FPRoc.Parameters.Add('-preset');
      FPRoc.Parameters.Add('veryfast');
      FPRoc.Parameters.Add('-strict');
      FPRoc.Parameters.Add('experimental');
      FPRoc.Parameters.Add('-y');
      FPRoc.Parameters.Add('-acodec');
      FPRoc.Parameters.Add('aac');
      FPRoc.Parameters.Add('-r');
      FPRoc.Parameters.Add('24');
      FPRoc.Parameters.Add('-crf');
      FPRoc.Parameters.Add('24');
      FPRoc.Parameters.Add('-f');
      FPRoc.Parameters.Add('mp4');
      FPRoc.Parameters.Add(FDestFile);

      FProc.Options:=FProc.Options + [poWaitOnExit];
      FProc.Execute();

      FSFile:=TFileStream.Create(FDestFile,fmOpenReadWrite or fmShareDenyNone);
      try
        FDest.Size:=FSFile.Size;
        Core.Streams.CheckSum(FSFile,FDest.Digest);
        FDest.Kind:=Storage.UserStorage.Kind.Video;
        FSummary.Status:=Storage.Summary.Video.Status.None;
        Storage.Summary.Video.toXML(FSummary,FProcessor.Refactor);
        FDest.Summary:=Core.Streams.toString(FProcessor.Refactor);
        FProcessor.Refactor.Size:=0;
        FDest.Allocated:=Core.Timer.dtUT;
        Storage.UserStorage.Files.DB.Write(FProcessor.Task,FDomainID,FUserID,FDest,true);
        FProcessor.Refactor.Size:=0;
      finally
        FSFile.Free();
      end;
    Finally
      FProc.Free();
    end;
  finally
    Storage.Summary.Video.Done(FSummary);
  end;
end;


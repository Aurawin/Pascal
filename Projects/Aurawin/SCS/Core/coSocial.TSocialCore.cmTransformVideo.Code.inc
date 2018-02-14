constructor TSocialCore.cmTransformVideo.Create(aManager:TRSRManager; aRSRP:PRSR; aNetwork:Storage.Social.Network.TNetwork; aDomainID,aUserID:Qword; var aSource,aDest:Storage.Social.Files.TSFile; aSourceFile,aDestFile:string);
begin
  FDomainID:=aDomainID;
  FUserID:=aUserID;
  FSourceFile:=aSourceFile;
  FDestFile:=aDestFile;
  Storage.Social.Network.Copy(aNetwork,FNetwork);
  Storage.Social.Files.Copy(aSource,FSource);
  Storage.Social.Files.Copy(aDest,FDest);
  Inherited Create(aManager,aRSRP);
end;

destructor TSocialCore.cmTransformVideo.Destroy;
begin
  Storage.Social.Network.Done(FNetwork);
  Storage.Social.Files.Done(FDest);
  Storage.Social.Files.Done(FSource);
  inherited Destroy();
end;

procedure TSocialCore.cmTransformVideo.Execute();
var
  FProc:TProcess;
  FSFile:TFileStream;
  isXML:TXMLInputSource;
  xDocument:TXMLDocument;
  xSummary:TDOMNode;
  xNode:TDOMNode;
  FSummary:Storage.Summary.Video.TItem;
begin
  FProcessor.EntryPoint:='coSocial.TSocialCore.cmTransformVideo.Execute';
  Storage.Summary.Video.Init(FSummary);
  try
    if Length(FDest.Summary)>0 then begin
      isXML:=TXMLInputSource.Create(FDest.Summary);
      Try
       xDocument:=TXMLDocument.Create();
       Try
         xDocument.XMLVersion:='1.0';
         xSummary:=xDocument.CreateElement(Storage.Summary.Video.XML.Stanza);
         xNode:=FProcessor.XMLParser.ParseWithContext(isXML,xSummary,xaAppendAsChildren);
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

    Storage.Social.Files.SetSummary(FProcessor.Task,FDomainID,FNetwork.ID,FDest);

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
        FDest.Allocated:=Core.Timer.dtUT;
        FDest.Kind:=Storage.UserStorage.Kind.Video;
        FSummary.Status:=Storage.Summary.Video.Status.None;
        FProcessor.Refactor.Size:=0;
        Storage.Summary.Video.toXML(FSummary,FProcessor.Refactor);
        FDest.Summary:=Core.Streams.toString(FProcessor.Refactor);
        FProcessor.Refactor.Size:=0;

        Storage.Social.Files.Write(FProcessor.Task, FNetwork.Node,FDomainID,FNetwork.ID,FDest,true);
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


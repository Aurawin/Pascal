constructor TMPEGFrame.Create(aReader:TReader);
begin
  FHeader:=TMPEGHeader.Create(aReader);
  FPayload:=TMPEGPayload.Create(aReader);
  Inherited Create(fMPEG,aReader);
end;

procedure TMPEGFrame.Reset();
begin

end;

function TMPEGFrame.Load(Stream:TStream; var Handled:Boolean):boolean;
begin
  Result:=false;
end;

function  TMPEGPayload.Header:TMPEGHeader;
begin
  Result:=TMPEGHeader(FReader.FMPEGFrame.FHeader);
end;

procedure TMPEGPayload.Reset();
begin
  Inherited Reset();
end;

procedure TMPEGPayload.Load(Stream:TStream; var Handled:Boolean);
begin

end;


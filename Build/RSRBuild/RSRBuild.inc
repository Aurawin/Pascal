procedure TRSRBuild.DoRun;
var
  ErrorMsg: String;
  Code,Value,Len,FIO:Integer;
  FName:TFileName;
  Buf:AnsiString;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }
  // Insert user code here
  FName:=ChangeFileExt(ParamStr(0),'.inc');
  If Not FileExists(FName) then
    FileClose(FileCreate(FName));
  FIO:=FileOpen(FName,fmOpenReadWrite or fmShareDenyNone);
  If FIO>0 then begin
    Len:=FileSeek(FIO,0,2);
    If Len>0 then begin
      SetLength(Buf,Len);
      FileSeek(FIO,0,0);
      FileRead(FIO,Buf[1],Len);
    end;
    If Buf='' then
      Buf:=Concat(#39,'47777',#39);
    FileSeek(FIO,0,0);
    FileTruncate(FIO,0);
    System.Delete(Buf,1,1);
    Code:=Length(Buf);
    SetLength(Buf,Code-1);
    Val(Buf,Value,Code);
    If Code<>0 then
      Value:=47777;
    Inc(Value);
    Buf:=Concat(#39,IntToStr(Value),#39);
    FileWrite(FIO,Buf[1],Length(Buf));
    FileClose(FIO);

    FName:=ChangeFileExt(ParamStr(0),'.num');
    If Not FileExists(FName) then
    FileClose(FileCreate(FName));
    FIO:=FileOpen(FName,fmOpenReadWrite or fmShareDenyNone);
    if FIO>0 then begin
      FileSeek(FIO,0,0);
      FileTruncate(FIO,0);
      Buf:=IntToStr(Value);
      FileWrite(FIO,Buf[1],Length(Buf));
      FileClose(FIO);
    end;
  end;

  // stop program loop
  Terminate;
end;

constructor TRSRBuild.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TRSRBuild.Destroy;
begin
  inherited Destroy;
end;

procedure TRSRBuild.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h');
end;



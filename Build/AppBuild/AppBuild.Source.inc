  // Insert user code here
  Version:=FormatDateTime('yyyy"."m"."d',Now);
  FName:=ChangeFileExt(ParamStr(0),'.inc');
  If Not FileExists(FName) then
    FileClose(FileCreate(FName));
  FIO:=FileOpen(FName,fmOpenReadWrite or fmShareDenyNone);
  If FIO>0 then begin
    FileSeek(FIO,0,0);
    FileTruncate(FIO,0);   // 2001.10.29
    Buf:=Concat('''',Version,'''');
    FileWrite(FIO,Buf[1],Length(Buf));
    FileClose(FIO);
  end;

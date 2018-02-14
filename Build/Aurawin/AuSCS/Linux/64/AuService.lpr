Program AuService;

Uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  DaemonApp,
  lazdaemonapp,
  Classes,
  Interfaces,

  App,
  App.Consts,
  App.Build,
  Core.Logging,

  SysUtils;

{$R *.res}
  Procedure ProcessException(Obj : TObject; Addr : Pointer; FrameCount:Longint; Frame: PPointer);
  var
    sMessage: string;
    iLcv: LongInt;
  begin
    sMessage:=Concat('Address:',HexStr(PtrUInt(Addr), SizeOf(PtrUInt) * 2),' ');
    if Obj is exception then
      sMessage:=Concat(sMessage,Exception(Obj).ClassName,' : ',Exception(Obj).Message,' ')
    else
      sMessage:=Concat(sMessage,'Object ', Obj.ClassName);
    sMessage:=Concat(sMessage,BackTraceStrFunc(Addr),' ');

    for iLcv := 0 to FrameCount - 1 do
      sMessage:=Concat(sMessage,BackTraceStrFunc(Frame[iLcv]),' ');
    Core.Logging.Native.WriteLogEntry(SERVICE_PROCESS,'AuService.Exception',sMessage);
  end;

begin
  System.ExceptProc:=@ProcessException;
  DefaultSystemCodePage:=CP_UTF8;
  App.Build.Title:='AuService';
  App.Build.Caption:='Aurawin SCS Service';
  Application.Title:=App.Build.Caption;

  Application.Initialize;
  Application.Run;
end.

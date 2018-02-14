Program AuService;

Uses
{$IFDEF UNIX}
  CThreads,
{$ENDIF}
  DaemonApp, lazdaemonapp, Classes,Interfaces, SysUtils,uDaemon, uService, hSRConsts, uAppBuild,
  uLogging;

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
    SystemLog.WriteLogEntry(SERVICE_PROCESS,'AuraProcess.Exception',sMessage);
  end;

begin
  System.ExceptProc:=@ProcessException;
  Application.Initialize;
  Application.Title:='Aurawin SCS Service';

  Application.Run;
end.

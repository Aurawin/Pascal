procedure SocketEventsInitQueues(var qRecv,qWrite,qConn,qDiscon:TSocketEventQueue);
begin
  qRecv:=0;
  qWrite:=0;
  qConn:=0;
  qDiscon:=0;
end;

procedure SocketEventsCreateWindow(CallBack:TSocketEventsCallBack);
var
  cls: TWndClass;
  isreg : Boolean;
  clsType:ATOM;
  iError:LongInt;
begin
  if (hwndCallback=0) then begin
    hwndCBClass.lpfnWndProc:=Callback;
    hwndCBClass.hInstance := GetModuleHandle(nil);
    hwndCBClass.lpszClassName:='SocketEvents';
    hwndCBClass.cbSize:=SizeOf(hwndCBClass);
    clsType:=Windows.RegisterClassEx(hwndCBClass);
    if clsType<>0 then begin
      hwndCallback:=CreateWindowEx(WS_EX_TOOLWINDOW or WS_EX_TRANSPARENT or WS_EX_NOACTIVATE,hwndCBClass.lpszClassName,nil,0, 0, 0, 0, 0,0, 0, hwndCBClass.hInstance,nil);
      if hwndCallback=0 then begin
        iError:=GetLastError;
        raise Exception.Create(format('Cannot Create "%s" Handle.  Code[%.d]',['SocketEvents',iError]));
      end;
    end else begin
      iError:=GetLastError;
      raise Exception.Create(format('Cannot register Class "%s".  Code[%.d]',['SocketEvents',iError]));
    end;
  end;
end;

procedure SocketEventsAttach(Const Socket:Sockets.TSocket; const Filters:TSocketFilters);
var
  Flags:Cardinal;
  iLcv:TSocketFilter;
begin
  Flags:=0;
  for iLcv:=Low(TSocketFilter) to High(TSocketFilter) do begin
    if iLcv in Filters then
      Flags:=Flags or SelectFlags[iLcv];
  end;
  WSAAsyncSelect(Socket,hwndCallback,WM_SOCKETACTIVITY,Flags);
end;

procedure SocketEventsDetach(Const Socket:Sockets.TSocket);
begin
   WSAAsyncSelect(Socket,hwndCallback,0,0);
end;

function SocketEvent(HWindow: HWND; Message: UINT; WParam:WPARAM; LParam: LPARAM): LRESULT; stdcall;
var
  Manager : TRSRManager;
  RSRP    : PRSR;

  function Push_Socket_Activity:Longint;
  var
    FD:Word;
  begin
    Result:=0;
    Manager:=MAN_MAP[WParam];
    RSRP:=RSR_MAP[WParam];
    If (Manager<>Nil) and (RSRP<>nil) then begin
      With RSRP^ do begin
        TaskError:=Windows.HIWORD(LParam);
        FD:=Windows.LOWORD(LParam);
        Case FD of
          RSR.FD_READ    : Operations:=Operations or RSR_OP_READ;
          RSR.FD_WRITE   : Operations:=Operations or RSR_OP_WRITABLE;
          RSR.FD_CONNECT : Operations:=Operations or RSR_OP_CONNECTED;
          RSR.FD_CLOSE   : Operations:=Operations or RSR_OP_DISCONNECTED;
        end;
      end;
    end;
  end;

begin
  // Msg.LParamLo  EVENT... Ie.) FD_OPEN... FD_CLOSE etc
  // Msg.LParamHi  WINSOCK ERROR...
  Try
    case Message of
      WM_SOCKETACTIVITY        : Result:=Push_Socket_Activity();
      WM_CLOSE                 : Result:=0;//
      WM_GETMINMAXINFO         : Result:=0;//
      WM_NCCREATE              : Result:=LResult(true);

    else
        Result:=DefWindowProc(hwndCallback, Message, WParam, LParam);
    end;
  Except
    On E:Exception do Manager.OnException('uRSR.SocketEvents.Windows','Exception',E.Message);
  end;
end;


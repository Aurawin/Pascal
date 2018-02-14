  type TSocketEventsCallBack=function(HWindow: HWND; Message: UINT; WParam:WPARAM; LParam: LPARAM): LRESULT; stdcall;
  type TSocketEventQueue=LongInt;
  const
    NoPollQueue     = 0;
    FD_READ         = $01;
    FD_WRITE        = $02;
    FD_OOB          = $04;
    FD_ACCEPT       = $08;
    FD_CONNECT      = $10;
    FD_CLOSE        = $20;
    FD_QOS          = $40;
    FD_GROUP_QOS    = $80;
    FIONREAD     = IOC_OUT or { get # bytes to read }
      ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
      (Longint(Byte('f')) shl 8) or 127;
    FIONBIO       = IOC_IN or { set/clear non-blocking i/o }
      ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
      (Longint(Byte('f')) shl 8) or 126;
    FIOASYNC     = IOC_IN or { set/clear async i/o }
      ((Longint(SizeOf(Longint)) and IOCPARM_MASK) shl 16) or
      (Longint(Byte('f')) shl 8) or 125;
  const SelectFlags : Array[TSocketFilter] of longint=(
     RSR.FD_READ,    {sfkRead}
     RSR.FD_WRITE,   {sfkWrite}
     RSR.FD_CONNECT, {sfkConnect}
     RSR.FD_CLOSE    {sfkDisconnect}
  );
  var
    hwndCallback:THandle;
    hwndCBClass: TWNDCLASSEX = (
      cbSize         : 0;
      style          : CS_NOCLOSE or CS_GLOBALCLASS;
      lpfnWndProc    : nil;
      cbClsExtra     : 0;
      cbWndExtra     : 0;
      hInstance      : 0;
      hIcon          : 0;
      hCursor        : 0;
      hbrBackground  : 0;
      lpszMenuName   : nil;
      lpszClassName  : nil
    );
    procedure SocketEventsInitQueues(var qRecv,qWrite,qConn,qDiscon:TSocketEventQueue);
    procedure SocketEventsCreateWindow(CallBack:TSocketEventsCallBack);
    procedure SocketEventsAttach(Const Socket:Sockets.TSocket; const Filters:TSocketFilters);
    procedure SocketEventsDetach(Const Socket:Sockets.TSocket);


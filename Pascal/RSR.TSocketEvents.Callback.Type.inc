{$ifdef Windows}
  type
    TSocketEventsCallBack=function(HWindow: THandle; Message, WParam, LParam: Longint): Longint; stdcall;
  var
    hwndCallback:THandle;
    hwndCBClass: TWndClass = (
      style          : WS_POPUPWINDOW;
      lpfnWndProc    : nil;
      cbClsExtra     : 0;
      cbWndExtra     : 0;
      hInstance      : 0;
      hIcon          : 0;
      hCursor        : 0;
      hbrBackground  : 0;
      lpszMenuName   : nil;
      lpszClassName  : 'SocketEvents'
  );
{$endif}


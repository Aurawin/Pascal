{$if defined(Windows)}
  {$i RSR.SocketEvents.Type.Windows.inc}
{$else if defined(Unix)}
  {$if defined(Darwin)}
    {$i RSR.SocketEvents.Type.Darwin.inc}
  {$else}
    {$i RSR.SocketEvents.Type.Linux.inc}
  {$endif}
  {$define useEventQueues}
  procedure cb_Socket_Event_Open(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
  procedure cb_Socket_Event_Close(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
  procedure cb_Socket_Event_Read(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
  procedure cb_Socket_Event_Write(Socket:Sockets.TSocket; var Event:TSocketPollEvent);

{$endif}



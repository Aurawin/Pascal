{$if defined(Windows) }
  {$i RSR.SocketEvents.Windows.inc}
{$else if defined(Unix) }
  {$if defined(Darwin) }
    {$i RSR.SocketEvents.Darwin.inc}
  {$else}
    {$i RSR.SocketEvents.Linux.inc}
  {$endif}
{$endif}


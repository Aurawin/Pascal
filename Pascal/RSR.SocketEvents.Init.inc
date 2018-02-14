{$if defined(Windows)}
  SocketEventsCreateWindow(@SocketEvent);
{$elseif defined(Unix)}
  {$if defined(Darwin)}
    // Darwin
  {$else}
    // Linux
  {$endif}

{$endif}
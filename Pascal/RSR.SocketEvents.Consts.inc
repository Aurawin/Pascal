Const
{$if defined(Windows)}
  HWND_MESSAGE                 = -3;
  WM_MSBASE                    = 1024+7979;
  WM_HOSTFOUND                 = WM_MSBASE+1;
  WM_SERVICEFOUND              = WM_MSBASE+2;
  WM_SOCKETACTIVITY            = WM_MSBASE+3;  // Generic
  WM_CONNECT_ACTIVITY          = WM_MSBASE+4;  // Socket Connect Message
  WM_GETHOSTBYNAME_ACTIVITY    = WM_MSBASE+5;  // HOST BY NAME Message
  WM_GETHOSTBYADDR_ACTIVITY    = WM_MSBASE+6;  // HOST BY Address Message
  WM_GETSERVICE_ACTIVITY       = WM_MSBASE+7;  // SERVICE Message
  WM_RSR_READ_ACTIVITY         = WM_MSBASE+8;  // Data Received
  WM_RSR_WRITE_ACTIVITY        = WM_MSBASE+9;  // Data Writable
  WM_RSR_CONNECT_ACTIVITY      = WM_MSBASE+10;  // Data Writable
  WM_RSR_CLOSE_ACTIVITY        = WM_MSBASE+11;  // Data Writable

  WM_DNS_ACTIVITY              = WM_MSBASE+12;  // DNS Activity

  evtFilterDisconnect          = FD_CLOSE;
  evtFilterRead                = FD_READ or FD_CLOSE;
  evtFilterWrite               = FD_WRITE or FD_CLOSE;
  evtFilterReadWrite           = evtFilterRead or evtFilterWrite;
{$else if defined(Unix)}
  O_ASYNC                      = $020000;
  F_SETSIG                     = 10;
  F_GETSIG                     = 11;
  SIOCSPGRP                    = $8902;
  SIOCGPGRP                    = $8904;
  QUEUE_POLL_SIZE              = 100;
  {$if defined(Darwin)}
    evtFilterRead              = EVFILT_READ;
    evtFilterWrite             = EVFILT_WRITE;
    evtFilterDisconnect        = 0;
    tsPoll                     :TTimeSpec=( tv_sec:0; tv_nsec:0);
  {$else}
    evtFilterRead              = EPOLLIN;
    evtFilterWrite             = EPOLLOUT;
    evtFliterDisconnect        = EPOLLHUP or EPOLLERR;
    evtFilters : Array[TSocketFilter] of cInt =(EPOLLIN,EPOLLOUT,EPOLLIN or EPOLLOUT,EPOLLHUP or EPOLLERR);
  {$endif}

  (*
  BASE_SIZE                    = 100;
  // bug in fpc 2.0.4-
  EPOLL_CTL_ADD                = 1;
  EPOLL_CTL_DEL                = 2;
  EPOLL_CTL_MOD                = 3;

  EPOLLIN                      = $01; { The associated file is available for read(2) operations. }
  EPOLLPRI                     = $02; { There is urgent data available for read(2) operations. }
  EPOLLOUT                     = $04; { The associated file is available for write(2) operations. }
  EPOLLERR                     = $08; { Error condition happened on the associated file descriptor. }
  EPOLLHUP                     = $10; { Hang up happened on the associated file descriptor. }
  EPOLLONESHOT                 = 1 shl 30;
  EPOLLET                      = 1 shl 31; { Sets  the  Edge  Triggered  behaviour  for  the  associated file descriptor. }
  *)
{$endif}

procedure SocketEventsInitQueues(var qRecv,qWrite,qConn,qDiscon:TPollQueue);
begin
  qRecv:=0;
  qWrite:=0;
  qConn:=0;
  qDiscon:=0;
end;

procedure SocketEventsCreateQueues(var qRecv,qWrite,qConn,qDiscon:TPollQueue);
begin
  qRecv:=epoll_create(QUEUE_POLL_SIZE);
  qWrite:=epoll_create(QUEUE_POLL_SIZE);
  qConn:=epoll_create(QUEUE_POLL_SIZE);
  qDiscon:=epoll_create(QUEUE_POLL_SIZE);
end;

procedure SocketEventsAttach(qPoll:TPollQueue; Const Socket:Sockets.TSocket; Filter:TSocketFilter);
var
  peEvent : TEPoll_Event;
  iRes:LongInt;
begin
  peEvent.Events:=evtFilters[Filter];
  peEvent.Data.fd:=Socket;
  iRes:=epoll_ctl(qPoll,EPOLL_CTL_ADD,Socket,@peEvent);
  if iRes<>0 then
    iRes:=errno;
end;

procedure SocketEventsDetach(qPoll:TPollQueue; Const Socket:Sockets.TSocket);
begin
  epoll_ctl(qPoll,EPOLL_CTL_DEL,Socket,nil);
end;

procedure SocketEventsPoll(qPoll:TPollQueue; qEvents:TSocketPollEvents; Callback:TSocketEventCallback);
var
  iCount : LongInt;
  iLcv   : LongInt;
begin
  iCount:=epoll_wait(qPoll, @qEvents[0], QUEUE_POLL_SIZE,0);
  for iLcv:=0 to iCount-1 do begin
    if qEvents[iLcv].Data.ptr<>nil then
      Callback(qEvents[iLcv].Data.fd,qEvents[iLcv]);
  end;
end;

procedure cb_Socket_Event_Close(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager:TRSRManager;
  RSRP    : PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) then begin
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
    RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
  end;
end;

procedure cb_Socket_Event_Open(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager:TRSRManager;
  RSRP    : PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) then begin
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_CONNECTED;
  end else begin
    Sockets.fpshutdown(Socket,SHUT_RDWR);
    Sockets.CloseSocket(Socket);
  end;
end;


procedure cb_Socket_Event_Read(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager:TRSRManager;
  RSRP    : PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) and ((RSRP^.State or RSR_STATE_ERROR)<>RSRP^.State ) then begin
    RSRP^.State:=RSRP^.State or RSR_STATE_POLLED_RCVD;
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_READ;
    if (RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) then begin
      RSRP^.Operations:=RSRP^.Operations and not RSR_OP_READ;
      RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
      SocketEventsDetach(Manager.FRcvQueue,Socket);
    end;
  end else begin
    Sockets.fpshutdown(Socket,SHUT_RDWR);
    Sockets.CloseSocket(Socket);
  end;
end;


procedure cb_Socket_Event_Write(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager:TRSRManager;
  RSRP    : PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) and ((RSRP^.State or RSR_STATE_ERROR)<>RSRP^.State ) then begin
    RSRP^.State:=RSRP^.State or RSR_STATE_POLLED_RCVD;
    RSRP^.State:=RSRP^.State or RSR_STATE_WRITE;
    SocketEventsDetach(Manager.FWriteQueue,Socket);
  end else begin
    Sockets.fpshutdown(Socket,SHUT_RDWR);
    Sockets.CloseSocket(Socket);
  end;
end;

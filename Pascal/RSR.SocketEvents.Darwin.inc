procedure SocketEventsInitQueues(var qRecv,qWrite,qConn,qDiscon:TKQueue);
begin
  qRecv:=0;
  qWrite:=0;
  qConn:=0;
  qDiscon:=0;
end;

procedure SocketEventsCreateQueues(var qRecv,qWrite,qConn,qDiscon:TKQueue);
begin
  qRecv:=BSD.kqueue();
  qWrite:=BSD.kqueue();
  qConn:=BSD.kqueue();
  qDiscon:=BSD.kqueue();
end;

procedure SocketEventsAttach(kQueue:TKQueue; Const Socket:Sockets.TSocket; Flags:TSocketFilter);
var
  kEvent:TKEvent;
begin
  //Flags EVFILT_READ
  FillByte(kEvent,SizeOf(kEvent),0);
  case Flags of
    sfkRead : begin
      BSD.EV_SET(@kEvent,Socket,EVFILT_READ,EV_ADD or EV_CLEAR or EV_ENABLE,0,0,nil);
      BSD.kevent(kQueue,@kEvent, 1,nil,0,nil);
    end;
    sfkWrite :  begin
      BSD.EV_SET(@kEvent,Socket,EVFILT_WRITE,EV_ADD or EV_CLEAR or EV_ENABLE,0,0,nil);
      BSD.kevent(kQueue,@kEvent, 1,nil,0,nil);
    end;
    sfkConnect :  begin
      BSD.EV_SET(@kEvent,Socket,EVFILT_READ,EV_ONESHOT or EV_ADD or EV_ENABLE,0,0,nil);
      BSD.kevent(kQueue,@kEvent, 1,nil,0,nil);
      BSD.EV_SET(@kEvent,Socket,EVFILT_WRITE,EV_ADD or EV_ENABLE,0,0,nil);
      BSD.kevent(kQueue,@kEvent, 1,nil,0,nil);
    end;
  end;
end;

procedure SocketEventsDetach(kQueue:TKQueue; Const Socket:Sockets.TSocket);
var
  kEvent:TKEvent;
begin
  FillByte(kEvent,SizeOf(kEvent),0);
  BSD.EV_SET(@kEvent,Socket,EVFILT_READ,EV_DELETE,0,0,nil);
  BSD.kevent(kQueue,@kEvent, 1,nil,0,nil);
  BSD.EV_SET(@kEvent,Socket,EVFILT_WRITE,EV_DELETE,0,0,nil);
  BSD.kevent(kQueue,@kEvent, 1,nil,0,nil);
end;

procedure SocketEventsPoll(kQueue:TKQueue; qEvents:TSocketPollEvents; Callback:TSocketEventCallback);
var
  iCount : LongInt;
  iLcv   : LongInt;
begin
  iCount:=kevent(kQueue, nil, 0, @qEvents[0], hRSR.QUEUE_POLL_SIZE, @hRSR.tsPoll);
  for iLcv:=0 to iCount-1 do begin
    if qEvents[iLcv].Ident<>0 then
      Callback(qEvents[iLcv].Ident,qEvents[iLcv]);
  end;
end;

function SocketEventsCreateQueue():TKQueue;
begin
  Result:=BSD.kqueue();
end;

procedure cb_Socket_Event_Close(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager:TRSRManager;
  RSRP:PRSR;
begin
  // This should not occur because there is no signal that occurs due to the fact taht there is no
  // filter event for close or error.  We have to read states on each call during read and connects
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) then begin
    if (Event.Flags=EV_EOF) or (Event.Flags=EV_ERROR) then begin
      RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
      if (Event.Flags=EV_EOF) then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
      if (Event.Flags=EV_ERROR) then
        RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
    end;
  end;
end;

procedure cb_Socket_Event_Open(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager:TRSRManager;
  RSRP:PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) then begin
    RSRP^.Operations:=RSRP^.Operations or RSR_OP_CONNECTED;
    if (Event.Flags=EV_EOF) then begin
      RSRP^.Operations:=RSRP^.Operations or RSR_OP_DISCONNECTED;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    end;
    if (Event.Flags=EV_ERROR) then begin
      RSRP^.Operations:=RSRP^.Operations or RSR_OP_ERROR;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    end;
  end;
end;

procedure cb_Socket_Event_Read(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager : TRSRManager;
  RSRP    : PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) then begin
    if (Event.Flags=EV_EOF) then begin
      RSRP^.Operations:=RSRP^.State or RSR_OP_DISCONNECTED;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    end;
    if (Event.Flags=EV_ERROR) then begin
      RSRP^.Operations:=RSRP^.State or RSR_OP_ERROR;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    end;
    if  ((RSRP^.State or RSR_STATE_POLLED_RCVD) <> RSRP^.State) and ((RSRP^.State or RSR_OP_ERROR)<>RSRP^.State ) then begin
      RSRP^.State:=RSRP^.State or RSR_STATE_POLLED_RCVD;
      RSRP^.Operations:=RSRP^.Operations or RSR_OP_READ;
    end;
  end;
end;

procedure cb_Socket_Event_Write(Socket:Sockets.TSocket; var Event:TSocketPollEvent);
var
  Manager : TRSRManager;
  RSRP    : PRSR;
begin
  Manager:=MAN_MAP[Socket];
  RSRP:=RSR_MAP[Socket];
  If (Manager<>Nil) and (RSRP<>nil) then begin
    if (Event.Flags=EV_EOF) then begin
      RSRP^.Operations:=RSRP^.State or RSR_OP_DISCONNECTED;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    end;
    if (Event.Flags=EV_ERROR) then begin
      RSRP^.Operations:=RSRP^.State or RSR_OP_ERROR;
      RSRP^.State:=RSRP^.State and not RSR_STATE_OPEN;
    end;
    if  ((RSRP^.State or RSR_STATE_POLLED_RCVD) <> RSRP^.State) and ((RSRP^.State or RSR_OP_ERROR)<>RSRP^.State ) then
      RSRP^.State:=RSRP^.State or RSR_STATE_WRITE;
  end;
end;

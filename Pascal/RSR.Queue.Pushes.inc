procedure Push_Queue_TCP;
begin
  {$ifdef cpu64}
    System.InterlockedIncrement64(RSR_Connection_Count);
  {$else}
    System.Inc(RSR_Connection_Count);
  {$endif}
  SetEvents(RSRP);
end;

procedure Push_Queue_UDP;
begin
  {$ifdef cpu64}
    System.InterlockedIncrement64(RSR_Stream_Count);
  {$else}
    System.Inc(RSR_Stream_Count);
  {$endif}
  RSRP^.SendEvent:=@cb_RSR_Write_UDP;
  RSRP^.RecvEvent:=@cb_RSR_Read_UDP;
  {$ifdef Unix}
    SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
    SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
  {$endif}
end;


procedure Push_Queue_DNS;
begin
  {$ifdef cpu64}
    System.InterlockedIncrement64(RSR_Stream_Count);
  {$else}
    System.Inc(RSR_Stream_Count);
  {$endif}
  RSRP^.SendEvent:=@cb_RSR_Write_UDP;
  RSRP^.RecvEvent:=@cb_RSR_Read_UDP;
  {$ifdef Unix}
  SocketEventsAttach(FRcvQueue,RSRP^.Info.Socket,sfkRead);
  SocketEventsAttach(FWriteQueue,RSRP^.Info.Socket,sfkWrite);
  {$endif}
end;


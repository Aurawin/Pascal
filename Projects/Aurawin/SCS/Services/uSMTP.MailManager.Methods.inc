
procedure   TMailManager.CopyAntiSpamData;
begin
  EnterCriticalSection(Owner.FAntiSpamLock);
  Try
    Copy(Owner.saDNSBlackLists,saDNSBlackLists);
    Storage.Security.Filter.Copy(Owner.FFilters[secContentProfiles],ContentProfiles);
    Storage.Security.Filter.Copy(Owner.FFilters[secContentFilter],ContentPhrases);
    Storage.Security.Filter.Copy(Owner.FFilters[secBlackList],BlackList);
    Storage.Security.Filter.Copy(Owner.FFilters[secWhiteList],WhiteList);
  Finally
    LeaveCriticalSection(Owner.FAntiSpamLock);
  End;
  dtNextAntiSpamUpdate:=IncSecond(Core.Timer.dtNow,60);
end;

procedure   TMailManager.OnException(sProcedure,sLocation,sError:Core.Strings.VarString);
begin
  Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat(sProcedure,'.',sLocation,':',sError));
end;

procedure   TMailManager.OnError(RSRP:PRSR);
begin
  TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
  TTransportBase(RSRP^.Transport).OnErrored(RSRP);
  if (RSR.IsAcceptableError(RSRP^.Errors)=false) then begin
    Core.Logging.Native.WriteLogEntry(
      Owner.RootDomain.Name,
      FService,
      Concat(
        #9'Network Error(s): ',RSRErrorToString(RSRP^.Errors),
        #9'Remote IP: ',Core.Utils.Sockets.InAddrToStr(RSRP^.Address.sin_addr.s_addr),
        #9'Socket: ',IntToStr(RSRP^.Info.Socket),
        #9'Socket Error(s): ',IntToStr(RSRP^.TaskError)
      )
    );
  end;
end;

procedure   TMailManager.OnDisconnect(RSRP:PRSR);
begin
  if (RSRP^.Transport<>nil) then begin
    TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
    TTransportBase(RSRP^.Transport).OnDisconnected(RSRP);
  end;
end;

procedure   TMailManager.OnDNSResult(RSRP:PRSR);
begin
  TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
  TTransportBase(RSRP^.Transport).OnDNSResult(RSRP);
end;

procedure   TMailManager.OnQueue(RSRP:PRSR);
begin
  If RSRP^.Transport=nil then
    RSRP^.Transport:=FTransportRecv;

  TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
  TTransportBase(RSRP^.Transport).OnQueued(RSRP);
end;

procedure   TMailManager.OnConnect(RSRP:PRSR);
begin
  TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
  TTransportBase(RSRP^.Transport).OnConnected(RSRP);
end;

procedure   TMailManager.OnDataReceived(RSRP:PRSR; Var Handled:Boolean);
begin
  EntryPoint:='TMailManager.OnDataReceived';
  if ( (RSRP^.Operations or RSR_OP_CLOSE)<>RSRP^.Operations) then begin
    TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
    TTransportBase(RSRP^.Transport).OnDataReceived(RSRP,Handled);
  end else
    Handled:=true;
end;

procedure   TMailManager.OnInitialize(RSRP:PRSR);
begin
  EntryPoint:='TMailManager.OnInitialize';
  If RSRP^.Transport=nil then
    RSRP^.Transport:=FTransportRecv;
  TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
  TTransportBase(RSRP^.Transport).OnInitialized(RSRP);
end;

procedure   TMailManager.OnFinalize(RSRP:PRSR);
begin
  EntryPoint:='TMailManager.OnFinalize';
  if (RSRP^.Transport<>nil) then begin
    TTransportBase(RSRP^.Transport).FRSRP:=RSRP;
    TTransportBase(RSRP^.Transport).OnFinalized(RSRP);
  end;
end;


procedure   TPOPManager.OnDataReceived(RSRP:PRSR; var Handled:Boolean);
var
  iLoopControl                   : LongInt;
  iEndOfMessage                  : Int64;
  sInput                         : Core.Strings.VarString;

  Function CloseSession:Boolean;
  begin
    Result:=(RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) or (POP3P^.ErrorCount>5) or (POP3P^.State or RS_EndSession=POP3P^.State);
  end;

begin
  Handled:=true;
  POP3P:=RSRP^.Info.DataP;
  If POP3P<>Nil then begin
    If RSR.EndOfLine(RSRP^.RecvBuffer) then begin
      iLoopControl:=0;
      While (RSRP^.RecvBuffer.posRead<RSRP^.RecvBuffer.Stream.Size) and Not (CloseSession or Terminated) and (iLoopControl<50) and (POP3P^.ErrorCount<=MAX_POP3_ERRORS) do begin
        sInput:=RSR.ReadLine(RSRP^.RecvBuffer,Refactor);
        Try
          RSR.Clean(sInput);
          Process(RSRP,sInput);
        Except
          On E:Exception do begin
            Inc(POP3P^.ErrorCount);
            Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TPOPManager.OnDataReceived.Process Exception:',E.Message));
          end;
        End;
        Inc(iLoopControl);
      end;
      RSRP^.RecvBuffer.posRead:=0;
      RSRP^.RecvBuffer.posWrite:=0;
      RSRP^.RecvBuffer.Stream.Size:=0;
      If iLoopControl>50 then
        Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TPOPManager.OnDataReceived','Loop Control Variable Exceeded'));
    end;
    If CloseSession then
      Close(RSRP);
    If (POP3P^.UAP<>Nil) and (POP3P^.UAP^.Modified) then begin
      EntryPoint:='TPOPManager.OnDataReceived.UserAccount_UpdateMods';
      Storage.UserAccounts.Items.DB.UpdateMods(Task,POP3P^.UAP^);
      POP3P^.UAP^.Modified:=False;
    end;
    If CloseSession then
      Close(RSRP);
  end else begin
    Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TPOPManager.OnDataReceived.Process Error:','Null pointer to POP3 Data'));
    Close(RSRP);
  end;
end;


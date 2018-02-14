procedure   TIMAPManager.OnDataReceived(RSRP:PRSR; var Handled:Boolean);
var
  iLoopControl                   : LongInt;
  iEndOfMessage                  : Int64;
  sInput                         : Core.Strings.VarString;

  Function CloseSession:Boolean;
  begin
    Result:=(RSRP^.State or RSR_STATE_OPEN<>RSRP^.State) or (IMAPP^.ErrorCount>5) or (IMAPP^.State or RS_EndSession=IMAPP^.State);
  end;

begin
  Handled:=true;
  IMAPP:=RSRP^.Info.DataP;
  If IMAPP<>Nil then begin
    If RSR.EndOfLine(RSRP^.RecvBuffer) then begin
      if ((IMAPP^.State or RS_MESSAGE_DATA)=IMAPP^.State) then begin
        Try
          Process_Message(RSRP);
        finally
          Empty();
        end;
      end else begin
        iLoopControl:=0;
        While (RSRP^.RecvBuffer.posRead<RSRP^.RecvBuffer.Stream.Size) and (IMAPP^.State or RS_MESSAGE_DATA<>IMAPP^.State) and Not (CloseSession or Terminated) and (iLoopControl<50) and (IMAPP^.ErrorCount<=MAX_IMAP_ERRORS) do begin
          sInput:=RSR.ReadLine(RSRP^.RecvBuffer,Refactor);
          Try
            RSR.Clean(sInput);
            Try
              Process(RSRP,sInput);
            finally
              Empty();
            end;
          Except
            On E:Exception do begin
              Inc(IMAPP^.ErrorCount);
              Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TIMAPManager.OnDataReceived.Process Exception:',E.Message));
            end;
          End;
          Inc(iLoopControl);
        end;
        If iLoopControl>50 then
          Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TIMAPManager.OnDataReceived','Loop Control Variable Exceeded'));
      end;
    end;
    If CloseSession then
      Close(RSRP);
    If (IMAPP^.UAP<>Nil) and (IMAPP^.UAP^.Modified) and (IMAPP^.UAP^.ID<>0) then begin
      EntryPoint:='TIMAPManager.OnDataReceived.UserAccount_UpdateMods';
      Storage.UserAccounts.Items.DB.UpdateMods(Task,IMAPP^.UAP^);
      IMAPP^.UAP^.Modified:=False;
    end;
    If CloseSession then
      Close(RSRP);
  end else begin
    Core.Logging.Native.WriteLogEntry(Owner.RootDomain.Name,FService,Concat('TIMAPManager.OnDataReceived.Process Error:','Null pointer to IMAP Data'));
    Close(RSRP);
  end;
end;


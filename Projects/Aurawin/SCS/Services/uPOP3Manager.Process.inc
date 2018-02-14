Procedure   TPOPManager.Process(RSRP:PRSR; var InputN:Core.Strings.VarString);
var
  saCommands                     : Core.Arrays.Types.VarString;
  CMD                            : Core.Strings.VarString;
  CMDP1                          : Core.Strings.VarString;
  CMDP2                          : Core.Strings.VarString;

  iLcv,iCount                    : LongInt;
  iOffset                        : Int64;
  iIndex                         : LongInt;
  iSize                          : Int64;


  DataLcv                        : LongInt;
  Code                           : LongInt;

  procedure PushSend;
  begin
    Send(RSRP,FResponse);
  end;

  Procedure PushError;
  begin
    Inc(POP3P^.ErrorCount);
    If POP3P^.ErrorCount>Max_Error then begin
      Send(RSRP,FResponse);
      POP3P^.State:=RS_EndSession;
    end else begin
      PushSend;
    end;
  end;

  Function TransState:Boolean;
  begin
    Result:=(POP3P^.State or RS_TRANSACTION)=POP3P^.State;
  end;

begin
  POP3P:=RSRP^.Info.DataP;
  If POP3P<>Nil then begin
    Core.Arrays.VarString.fromString(saCommands,InputN,#32);
    Try
      CMD:=Uppercase(Core.Arrays.VarString.Parameter(saCommands,1));
      CMDP1:=Core.Arrays.VarString.Parameter(saCommands,2);
      CMDP2:=Core.Arrays.VarString.Parameter(saCommands,3);
    Finally
      SetLength(saCommands,0);
    end;
    If (CMD='QUIT') then begin
      {$i uPOP3Manager.Process.QUIT.inc}
    end else if (CMD='USER') then begin
      {$i uPOP3Manager.Process.USER.inc}
    end else if (CMD='PASS') then begin
      {$i uPOP3Manager.Process.PASS.inc}
    end else if (CMD='LAST') then begin
      {$i uPOP3Manager.Process.LAST.inc}
    end else if (CMD='STAT') then begin
      {$i uPOP3Manager.Process.STAT.inc}
    end else if (CMD='LIST') then begin
      {$i uPOP3Manager.Process.LIST.inc}
    end else if (CMD='TOP') then begin
      {$i uPOP3Manager.Process.TOP.inc}
    end else if (CMD='UIDL') then begin
      {$i uPOP3Manager.Process.UIDL.inc}
    end else if (CMD='RETR') then begin
      {$i uPOP3Manager.Process.RETR.inc}
    end else if (CMD='DELE') then begin
      {$i uPOP3Manager.Process.DELE.inc}
    end else if (CMD='RSET') then begin
      {$i uPOP3Manager.Process.RSET.inc}
    end else if (CMD='NOOP') then begin
      {$i uPOP3Manager.Process.NOOP.inc}
    end else if (CMD='CLEAR') then begin
      {$i uPOP3Manager.Process.CLEAR.inc}
    end else if (CMD='VERS') then begin
      {$i uPOP3Manager.Process.VERS.inc}
    end else if (CMD='ID') then begin
      {$i uPOP3Manager.Process.ID.inc}
    end else if (CMD='STLS') then begin
      {$i uPOP3Manager.Process.STLS.inc}
    end else if (CMD='STARTTLS') then begin
      {$i uPOP3Manager.Process.STLS.inc}
    end else if (CMD='CAPA') then begin
      {$i uPOP3Manager.Process.CAPA.inc}
    end else begin
      If TransState then begin
        FResponse:=Concat('-ERR We do not understand "',CMD,'" during Transaction state.'#13#10);
        PushError;
      end else begin
        FResponse:=Concat('-ERR We do not understand "',CMD,'" during the Authorization state.'#13#10);
        PushError;
      end;
    end;
  end;
end;


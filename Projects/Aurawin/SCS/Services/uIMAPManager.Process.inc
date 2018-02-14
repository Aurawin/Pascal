Procedure   TIMAPManager.Process(RSRP:PRSR; var InputN:Core.Strings.VarString);
var
  FSrchLcv                       : LongInt;

  iLen                           : LongInt;
  iLcv                           : LongInt;
  jLcv                           : LongInt;
  fLcv                           : LongInt;
  iCount                         : LongInt;
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
    Inc(IMAPP^.ErrorCount);
    If IMAPP^.ErrorCount>Max_Error then begin
      Send(RSRP,FResponse);
      IMAPP^.State:=RS_EndSession;
    end else begin
      Send(RSRP,FResponse);
    end;
  end;

  {$i uIMAPManager.Process.PushLoginAttempt.inc}

begin
  EntryPoint:='TIMAPManager.Process(RSRP, InputN)';
  IMAPP:=RSRP^.Info.DataP;
  If (IMAPP<>Nil) then begin
    if IMAPP^.State or RS_AUTHENTICATE=IMAPP^.State then begin
      {$i uIMAPManager.Process.AUTHENTICATE.Data.inc}
    end else begin
      Core.Arrays.VarString.fromString(FCommands,InputN,#32,[soClearList,soRemoveQuotes,soParenWraps,soBraceWraps]);
      Try
        FCommandCount:=Length(FCommands);
        IMAPP^.LastSequence:=Core.Arrays.VarString.Parameter(FCommands,1);
        FSEQ:=IMAPP^.LastSequence;
        CMDS:=InputN;
        CMD:=Uppercase(Core.Arrays.VarString.Parameter(FCommands,2));
        CMDP1:=Core.Arrays.VarString.Parameter(FCommands,3);
        CMDP2:=Core.Arrays.VarString.Parameter(FCommands,4);
        CMDP3:=Core.Arrays.VarString.Parameter(FCommands,5);
        CMDP4:=Core.Arrays.VarString.Parameter(FCommands,6);
        CMDP5:=Core.Arrays.VarString.Parameter(FCommands,7);
        if Core.Strings.SameText(CMD,Commands.Capability) then begin
          {$i uIMAPManager.Process.CAPABILITY.inc}
        end else if Core.Strings.SameText(CMD,Commands.Noop)then begin
          Noop(RSRP);
        end else if Core.Strings.SameText(CMD,Commands.Fetch)then begin
          {$i uIMAPManager.Process.FETCH.inc}
        end else if Core.Strings.SameText(CMD,Commands.UID)then begin
          Process_UID(RSRP);
        end else if Core.Strings.SameText(CMD,Commands.Search)then begin
          Search(RSRP);
        end else if Core.Strings.SameText(CMD,Commands.Store)then begin
          {$i uIMAPManager.Process.STORE.inc}
        end else if Core.Strings.SameText(CMD,Commands.Close)then begin
          {$i uIMAPManager.Process.CLOSE.inc}
        end else if Core.Strings.SameText(CMD,Commands.Check)then begin
          Check(RSRP);
        end else if Core.Strings.SameText(CMD,Commands.Expunge)then begin
          {$i uIMAPManager.Process.EXPUNGE.inc}
        end else if Core.Strings.SameText(CMD,Commands.Logout)then begin
          {$i uIMAPManager.Process.LOGOUT.inc}
        end else if Core.Strings.SameText(CMD,Commands.Login)then begin
          {$i uIMAPManager.Process.LOGIN.inc}
        end else if Core.Strings.SameText(CMD,Commands.Authenticate.Name )then begin
          {$i uIMAPManager.Process.AUTHENTICATE.inc}
        end else if Core.Strings.SameText(CMD,Commands.StartTLS)then begin
          {$i uIMAPManager.Process.STARTTLS.inc}
        end else if Core.Strings.SameText(CMD,Commands.Select)then begin
          Select(RSRP,CMDP1);
        end else if Core.Strings.SameText(CMD,Commands.Examine)then begin
          {$i uIMAPManager.Process.EXAMINE.inc}
        end else if Core.Strings.SameText(CMD,Commands.Created)then begin
          Created(RSRP,CMDP1);
        end else if Core.Strings.SameText(CMD,Commands.CopyUID)then begin
          CopyUID(RSRP,StrToQWordDef(CMDP1,0));
        end else if Core.Strings.SameText(CMD,Commands.Delete)then begin
          {$i uIMAPManager.Process.DELETE.inc}
        end else if Core.Strings.SameText(CMD,Commands.Rename)then begin
          {$i uIMAPManager.Process.RENAME.inc}
        end else if Core.Strings.SameText(CMD,Commands.Subscribe)then begin
          {$i uIMAPManager.Process.SUBSCRIBE.inc}
        end else if Core.Strings.SameText(CMD,Commands.UnSubscribe)then begin
          {$i uIMAPManager.Process.UNSUBSCRIBE.inc}
        end else if Core.Strings.SameText(CMD,Commands.Status)then begin
          Status(RSRP,Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP1));
        end else if Core.Strings.SameText(CMD,Commands.List)then begin
          {$i uIMAPManager.Process.LIST.inc}
        end else if Core.Strings.SameText(CMD,Commands.LSub)then begin
          {$i uIMAPManager.Process.LSUB.inc}
        end else if Core.Strings.SameText(CMD,Commands.Append)then begin
          Append(RSRP);
        end else if Core.Strings.SameText(CMD,Commands.ID)then begin
          ID(RSRP);
        end else begin
          If TransState then begin
            FResponse:=Respond(FSEQ,SSC_NO,Concat(CMD,' failed: during transaction state.'));
            Send(RSRP,FResponse);
          end else begin
            FResponse:=Respond(FSEQ,SSC_NO,Concat(CMD,' failed: during authorization state.'));
            Send(RSRP,FResponse);
          end;
        end;
        //Core.Streams.toFile(RSRP^.SendBuffer.Stream,'/home/atbrunner/Desktop/Output.txt');
      Finally
        SetLength(FCommands,0);
      end;
    end;
  end;
end;


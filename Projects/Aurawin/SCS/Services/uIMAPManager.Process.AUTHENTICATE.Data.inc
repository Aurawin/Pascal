FSEQ:=IMAPP^.LastSequence;

If (Length(IMAPP^.Nonce)=0) then begin
  FUTF8:=Encryption.Base64.Decode(InputN);
  Try
    Core.Arrays.VarString.fromString(@FCommands,FUTF8,#0,[soClearList,soIgnoreDelimAtStart]);
    Try
      if (Length(FCommands)=2) then begin
        IMAPP^.sUser:=Core.Strings.Extract(FCommands[0],'@');
        IMAPP^.sPass:=FCommands[1];
        PushLoginAttempt();
        IMAPP^.State:=IMAPP^.State and not RS_AUTHENTICATE;
      end else begin
        if (Length(IMAPP^.sUser)=0) then begin
          IMAPP^.sUser:=Core.Strings.Extract(FCommands[0],'@');
          FResponse:=Concat('+',#13#10);
          Send(RSRP,FResponse);
        end else begin
          IMAPP^.sPass:=FCommands[0];
          PushLoginAttempt();
          IMAPP^.State:=IMAPP^.State and not RS_AUTHENTICATE;
        end;
      end;
    Finally
      Core.Arrays.VarString.Empty(FCommands);
    end;
  finally
    SetLength(FUTF8,0);
  end;
end else begin
  FAuth:=Encryption.Base64.Decode(FUTF8);
  FHeaderCount:=Core.Arrays.KeyString.fromString(FClientAuth,FAuth,':',',',[soClearList,soRemoveQuotes]);
  IMAPP^.CNonce:=Core.Arrays.KeyString.GetItemAsString(FClientAuth,'cnonce',FHeaderCount,'');
end;


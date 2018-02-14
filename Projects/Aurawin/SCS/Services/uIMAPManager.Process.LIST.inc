if TransState()=true then begin
  If ( (Length(CMDP1)=0) and (Length(CMDP2)=0)) then begin
    FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('LIST (',FLAG_NOSELECT,') "/" ""'));
    PushSend();
    FResponse:=Respond(FSEQ,SSC_OK,Concat('LIST Completed.'));
    PushSend();
  end else if (CMDP2='%') or (CMDP2='%/%')then begin
    if (CMDP1=#34) then
      SetLength(CMDP1,0);
    if Length(CMDP1)>0 then
      FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail)
    else
      FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',CMDP1);

    {$i uIMAPManager.Process.List.Output.inc}

  end else if (CMDP2='') then begin
    FResponse:=Respond(SEQ_ANY,SSC_OK,Concat('LIST (',FLAG_NOSELECT,') "/" ""'));
    PushSend();
    FResponse:=Respond(FSEQ,SSC_OK,Concat('LIST Completed.'));
    PushSend();
  end else begin
    if (CMDP1=#34) then
      SetLength(CMDP1,0);

    FReference:=CMDP1;
    FMailbox:=CMDP2; // possibly /Inbox/*
    FMailboxWildChar:=EndsWith(FMailbox,'*');
    if (FMailboxWildChar=true) then
      ClipEnd(FMailbox,'*');
    ClipEnd(FMailbox,'/');

    if Core.Strings.SameText(FMailbox,Storage.UserStorage.Folders.Defaults.Mail.Inbox) then begin
      FMailbox:=Storage.UserStorage.Folders.Defaults.Mail.Inbox;
    end else if Core.Strings.SameText(FMailbox,Storage.UserStorage.Folders.Defaults.Mail.Trash) then begin
      FMailbox:=Storage.UserStorage.Folders.Defaults.Mail.Trash;
    end else if (
      Core.Strings.SameText(FMailbox,Storage.UserStorage.Folders.Defaults.Mail.Drafts) or
      Core.Strings.SameText(FMailbox,Storage.UserStorage.Folders.Defaults.Mail.Outbox)
    ) then begin
      FMailbox:=Storage.UserStorage.Folders.Defaults.Mail.Outbox;
    end else if (
      Core.Strings.SameText(FMailbox,Storage.UserStorage.Folders.Defaults.Mail.Spam) or
      Core.Strings.SameText(FMailbox,Storage.UserStorage.Folders.Defaults.Mail.Junk)
    ) then begin
      FMailbox:=Storage.UserStorage.Folders.Defaults.Mail.Spam;
    end;

    Core.Arrays.VarString.fromString(FRefPath,FReference,'/',[soClearList,soIgnoreDelimAtStart]);
    Core.Arrays.VarString.fromString(FMbxPath,FMailbox,'/',[soClearList,soIgnoreDelimAtStart]);

    {$i uIMAPManager.Process.List.Output.inc}

  end;
end else begin
  FResponse:=Respond(FSEQ,SSC_NO,Concat('LIST failed: you must first login.'));
  PushSend();
end;

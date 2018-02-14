
procedure  TIMAPManager.ReleaseMessage(var Message:PIMAPMessage);
begin
  RSR.IMAP.Done(Message^);
  Dispose(Message);
  Message:=nil;
end;

function  TIMAPManager.Append(RSRP:PRSR):boolean;
begin
  EntryPoint:='TIMAPManager.Append(RSRP)';
  Result:=false;
  if TransState()=true then begin
    FMessageP:=IMAPP^.MessageP;
    if (FMessageP<>nil) then begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('APPEND failed: you must first complete prior APPEND.'));
      Send(RSRP,FResponse);
      Exit;
    end else begin
      New(FMessageP);
      RSR.IMAP.Init(FMessageP^);
      IMAPP^.MessageP:=FMessageP;
    end;
    FMessageP^.Plus:=Core.Strings.EndsWith(CMDP3,'+');
    if FMessageP^.Plus=true then
      Core.Strings.ClipEnd(CMDP3,'+');
    FMessageP^.Size:=StrToQWordDef(CMDP3,0);
    FMessageP^.Flags:=RSR.IMAP.GetFlags(CMDP2);
    FMessageP^.Flags:=FMessageP^.Flags or Storage.UserStorage.Items.IMAP.Flags.Recent;
    FPath:=CMDP1;
    FPath:=Concat(Storage.UserStorage.Folders.Defaults.Home.Mail,'/',FPath);
    FMessageP^.Folder:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    if (FMessageP^.Folder=nil) then begin
      ReloadFolders(FPath,ifrkStatus);
      FMessageP^.Folder:=Storage.UserStorage.Folders.getFolder(FPath,IMAPP^.Folders);
    end;
    if (FMessageP^.Folder<>nil) then begin
      If (FMessageP^.Size>0) then begin
        Result:=true;
        IMAPP^.State:=IMAPP^.State or RS_MESSAGE_DATA;
        if (FMessageP^.Plus=false) then begin
          FResponse:='+ APPEND Ready for literal data.'#13#10;
          Send(RSRP,FResponse);
        end;
      end else begin
        ReleaseMessage(IMAPP^.MessageP);
        FResponse:=Respond(FSEQ,SSC_BAD,Concat('APPEND failed: unable parse size parameter "',CMDP3,'".'));
        Send(RSRP,FResponse);
      end;
    end else begin
      FResponse:=Respond(FSEQ,SSC_NO,Concat('APPEND failed: folder [',CMDP1,'] does not exist.'));
      Send(RSRP,FResponse);
    end;
  end else begin
    FResponse:=Respond(FSEQ,SSC_NO,Concat('APPEND failed: you must first login.'));
    Send(RSRP,FResponse);
  end;
end;

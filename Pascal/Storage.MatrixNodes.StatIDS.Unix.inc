class function Node.StatIDS(Var Entry:Item; var sGroup,sID,sError:Core.Strings.VarString):boolean;
var
  Term:TProcess;
  sPath:Core.Strings.VarString;
  sOutput:Core.Strings.VarString;
  iDX:QWord;
begin
  System.SetLength(sError,0);
  Result:=False;
  sPath:=Format(FMT_STAT_ID_GROUP,[sGroup]);
  Term:=TProcess.Create(nil);
  try
    Term.Options:=[poWaitOnExit,poUsePipes];
    Term.Parameters.Delimiter:=#32;
    Term.Parameters.DelimitedText:=sPath;
    Term.Executable:=ID_STAT;
    Term.Execute();
    Term.Output.Position:=0;
    if Term.Stderr.NumBytesAvailable>0 then begin
      SetLength(sError,Term.Stderr.NumBytesAvailable);
      Term.Stderr.Read(sError[1],Term.Stderr.NumBytesAvailable);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
      sError:=Concat('Group ID Error :',sError);
      Exit;
    end;
    if Term.Output.NumBytesAvailable>0 then begin
      SetLength(sOutput,Term.Output.NumBytesAvailable);
      Term.Output.Read(sOutput[1],Term.Output.NumBytesAvailable);
    end;
    iDX:=System.Pos(#10,sOutput);
    if iDX>0 then
      System.SetLength(sOutput,iDX-1);
    Entry.GroupID:=StrToQWordDef(sOutput,0);
    SetLength(sOutput,0);
  finally
    FreeAndNil(Term);
  end;
  sPath:=Format(FMT_STAT_ID_USER,[sID]);
  Term:=TProcess.Create(nil);
  try
    Term.Options:=[poWaitOnExit,poUsePipes];
    Term.Parameters.Delimiter:=#32;
    Term.Parameters.DelimitedText:=sPath;
    Term.Executable:=ID_STAT;
    Term.Execute();
    Term.Output.Position:=0;
    if Term.Stderr.NumBytesAvailable>0 then begin
      SetLength(sError,Term.Stderr.NumBytesAvailable);
      Term.Stderr.Read(sError[1],Term.Stderr.NumBytesAvailable);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
      sError:=Concat('User ID Error :',sError);
      Exit;
    end;
    if Term.Output.NumBytesAvailable>0 then begin
      SetLength(sOutput,Term.Output.NumBytesAvailable);
      Term.Output.Read(sOutput[1],Term.Output.NumBytesAvailable);
    end;
    iDX:=System.Pos(#10,sOutput);
    if iDX>0 then
      System.SetLength(sOutput,iDX-1);
    Entry.UserID:=StrToQWordDef(sOutput,0);
    SetLength(sOutput,0);
  finally
    FreeAndNil(Term);
  end;
end;


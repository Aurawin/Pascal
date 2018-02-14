class function Node.IsMounted(Var Entry:Item; out sError:Core.Strings.VarString):boolean;
var
  Term:TProcess;
  sParams:Core.Strings.VarString;
  sOutput:Core.Strings.VarString;
  sPath:Core.Strings.VarString;
  sRemote:Core.Strings.VarString;
  iDX:QWord;
begin
  System.SetLength(sError,0);
  Result:=False;

  sRemote:=Concat(Core.Utils.Sockets.InAddrToStr(Entry.IP),':',Entry.Disk.Device);
  sPath:=storage.MatrixNodes.Node.FMT_MOUNT_TARGET;
  sPath:=StringReplace(sPath,'$nid',IntToStr(Entry.ID),[rfReplaceAll,rfIgnoreCase]);
  sPath:=StringReplace(sPath,'$rid',IntToStr(Entry.ResourceID),[rfReplaceAll,rfIgnoreCase]);
  sPath:=StringReplace(sPath,'$cid',IntToStr(Entry.ClusterID),[rfReplaceAll,rfIgnoreCase]);

  sParams:=Format(FMT_GREP_MOUNTED,[sRemote,sPath,MOUNTS]);

  Term:=TProcess.Create(nil);
  try
    Term.Options:=[poWaitOnExit,poUsePipes];
    Term.Parameters.Delimiter:=#32;
    Term.Parameters.DelimitedText:=sParams;
    Term.Executable:=GREP;
    Term.Execute();
    Term.Output.Position:=0;
    if Term.Stderr.NumBytesAvailable>0 then begin
      SetLength(sError,Term.Stderr.NumBytesAvailable);
      Term.Stderr.Read(sError[1],Term.Stderr.NumBytesAvailable);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
      sError:=Concat('Is Mounted Error :',sError);
      Exit;
    end;
    if Term.Output.NumBytesAvailable>0 then begin
      SetLength(sOutput,Term.Output.NumBytesAvailable);
      Term.Output.Read(sOutput[1],Term.Output.NumBytesAvailable);
      Entry.Disk.Live:=True;
      Result:=true;
    end;
  finally
    FreeAndNil(Term);
  end;
end;


class function Node.StatDisk(Var Entry:Storage.MatrixNodes.Node.Item; out sError:Core.Strings.VarString):boolean;
var
  Term:TProcess;
  sPath:Core.Strings.VarString;
  sOutput:Core.Strings.VarString;
  saLines:Core.Arrays.Types.VarString;
  saInput:Core.Arrays.Types.VarString;
  iBias:QWord;
begin
  // This method if for finding out the capacity of a mounted drive
  System.SetLength(sError,0);
  Result:=Entry.Disk.Enabled and IsMounted(Entry,sError);
  If Result then begin
    Entry.Disk.Live:=True;
    sPath:=FMT_STAT_PARAMS;
    sPath:=StringReplace(sPath,'$nid',IntToStr(Entry.ID),[rfReplaceAll,rfIgnoreCase]);
    sPath:=StringReplace(sPath,'$rid',IntToStr(Entry.ResourceID),[rfReplaceAll,rfIgnoreCase]);
    sPath:=StringReplace(sPath,'$cid',IntToStr(Entry.ClusterID),[rfReplaceAll,rfIgnoreCase]);
    Term:=TProcess.Create(nil);
    try
      Term.Options:=[poWaitOnExit,poUsePipes];
      Term.Parameters.Delimiter:=#32;
      Term.Parameters.DelimitedText:=sPath;
      Term.Executable:=DISK_STAT;
      Term.Execute();
      Term.Output.Position:=0;
      if Term.Stderr.NumBytesAvailable>0 then begin
        SetLength(sOutput,Term.Stderr.NumBytesAvailable);
        Term.Stderr.Read(sOutput[1],Term.Stderr.NumBytesAvailable);
      end;
      if Term.Output.NumBytesAvailable>0 then begin
        SetLength(sOutput,Term.Output.NumBytesAvailable);
        Term.Output.Read(sOutput[1],Term.Output.NumBytesAvailable);
        Core.Arrays.VarString.fromString(saLines,sOutput,#10,[soClearList]);
        Try
          if (System.Length(saLines)>1) then begin
            Core.Strings.Deduplicate(saLines[0],#32);
            {
            saLines[0]:=StringReplace(saLines[0],#32#32,#9,[rfReplaceAll]);
            saLines[0]:=StringReplace(saLines[0],#9#9,#9,[rfReplaceAll]);
            Core.Arrays.VarString.fromString(saInput,saLines[0],#9,[soClearList]);
            }
            Core.Arrays.VarString.fromString(saInput,saLines[0],#32,[soClearList]);
            iBias:=1024*1024;
            Try
              if (System.Length(saInput)>1) then begin
                saInput[1]:=Lowercase(saInput[1]);
                if (saInput[1]='1k-blocks') then
                  iBias:=1024*1024
                else if (saInput[1]='512-blocks') then
                  iBias:=1024*1024*512;
              end;
            finally
              Core.Arrays.VarString.Empty(saInput);
            end;
            Core.Strings.Deduplicate(saLines[1],#32);
            {
            saLines[1]:=StringReplace(saLines[1],#32#32,#9,[rfReplaceAll]);
            saLines[1]:=StringReplace(saLines[1],#9#9,#9,[rfReplaceAll]);
            saLines[1]:=StringReplace(saLines[1],#9,#32,[rfReplaceAll]);
            }
            Core.Arrays.VarString.fromString(saInput,saLines[1],#32,[soClearList]);
            Try
              if (System.Length(saInput)>4) then begin
                //  0                             1           2        3      4       5
                // Filesystem                 512-blocks      Used  Available Use% Mounted on
                // Filesystem                 1K-blocks      Used  Available Use% Mounted on
                // 192.168.0.100:/media/raid 1951131648 271534080 1581941760  15% /auDisks/1/1/2
                Entry.Disk.Available:=StrToQWordDef(saInput[3],0);
                Entry.Disk.Available:=Entry.Disk.Available div (iBias);

                Entry.Disk.Capacity:=StrToQWordDef(saInput[1],0);
                Entry.Disk.Capacity:=Entry.Disk.Capacity div (iBias);

                Entry.Disk.Consumption:=StrToQWordDef(saInput[2],0);
                Entry.Disk.Consumption:=Entry.Disk.Consumption div (iBias);

                Result:=True;
              end;
            finally
              Core.Arrays.VarString.Done(saInput);
            end;
          end;
        Finally
          Core.Arrays.VarString.Done(saLines);
        end;
        // Parse Response
      end;
    finally
      FreeAndNil(Term);
    end;
  end else begin
    Entry.Disk.Live:=false;
  end;
end;


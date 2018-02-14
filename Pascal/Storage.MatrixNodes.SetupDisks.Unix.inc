class procedure Node.DB.SetupDisks(Task:Core.Database.Types.TTask; out Nodes:Storage.MatrixNodes.Node.Items; out sErrors:Core.Strings.VarString);
var
  Term:TProcess;
  iLcv:LongInt;
  iDX:LongInt;
  sPath:Core.Strings.VarString;
  sError:Core.Strings.VarString;
begin
  System.SetLength(sErrors,0);
  System.SetLength(sError,0);
  if Disks(Task,Nodes) then begin
    for iLcv:=0 to High(Nodes) do begin
      if Nodes[iLcv]^.Disk.Enabled and (System.Length(Nodes[iLcv]^.Disk.Device)>0) then begin
        sPath:=FMT_MOUNT_TARGET;
        sPath:=StringReplace(sPath,'$nid',IntToStr(Nodes[iLcv]^.ID),[rfReplaceAll,rfIgnoreCase]);
        sPath:=StringReplace(sPath,'$rid',IntToStr(Nodes[iLcv]^.ResourceID),[rfReplaceAll,rfIgnoreCase]);
        sPath:=StringReplace(sPath,'$cid',IntToStr(Nodes[iLcv]^.ClusterID),[rfReplaceAll,rfIgnoreCase]);
        Core.Utils.Files.ForceDirectories(sPath,Process.UserID,Process.GroupID,Process.Permissions);

        sPath:=FMT_MOUNT_TARGET;
        sPath:=StringReplace(sPath,'$nid',IntToStr(Nodes[iLcv]^.ID),[rfReplaceAll,rfIgnoreCase]);
        sPath:=StringReplace(sPath,'$rid',IntToStr(Nodes[iLcv]^.ResourceID),[rfReplaceAll,rfIgnoreCase]);
        sPath:=StringReplace(sPath,'$cid',IntToStr(Nodes[iLcv]^.ClusterID),[rfReplaceAll,rfIgnoreCase]);
        sPath:=Concat(sPath,AuDisk);

        if FileExists(sPath)=false then begin
          if IsMounted(Nodes[iLcv]^,sError)=false then begin
            sPath:=FMT_MOUNT_PARAMS;
            sPath:=StringReplace(sPath,'$nid',IntToStr(Nodes[iLcv]^.ID),[rfReplaceAll,rfIgnoreCase]);
            sPath:=StringReplace(sPath,'$rid',IntToStr(Nodes[iLcv]^.ResourceID),[rfReplaceAll,rfIgnoreCase]);
            sPath:=StringReplace(sPath,'$cid',IntToStr(Nodes[iLcv]^.ClusterID),[rfReplaceAll,rfIgnoreCase]);
            sPath:=StringReplace(sPath,'$ip',Core.Utils.Sockets.InAddrToStr(Nodes[iLcv]^.IP),[rfReplaceAll,rfIgnoreCase]);
            sPath:=StringReplace(sPath,'$disk',Nodes[iLcv]^.Disk.Device,[rfReplaceAll,rfIgnoreCase]);
            Term:=TProcess.Create(nil);
            try
              Term.Options:=[poWaitOnExit,poUsePipes];
              Term.Parameters.Delimiter:=#32;
              Term.Parameters.DelimitedText:=sPath;
              Term.Executable:=MOUNT_EXE;
              Term.Execute();
              if Term.Stderr.NumBytesAvailable>0 then begin
                SetLength(sError,Term.Stderr.NumBytesAvailable);
                Term.Stderr.Read(sError[1],Term.Stderr.NumBytesAvailable);
                iDX:=System.Pos(#10,sError);
                if iDX>0 then
                  System.SetLength(sError,iDX-1);
                sErrors:=Concat(sErrors,' ',sError);

                sPath:=FMT_MOUNT_TARGET;
                sPath:=StringReplace(sPath,'$nid',IntToStr(Nodes[iLcv]^.ID),[rfReplaceAll,rfIgnoreCase]);
                sPath:=StringReplace(sPath,'$rid',IntToStr(Nodes[iLcv]^.ResourceID),[rfReplaceAll,rfIgnoreCase]);
                sPath:=StringReplace(sPath,'$cid',IntToStr(Nodes[iLcv]^.ClusterID),[rfReplaceAll,rfIgnoreCase]);
                sPath:=Concat(sPath,AuDisk);
                if Not FileExists(sPath) then
                  sErrors:=Concat(sErrors,'Error mounting AuDisk : missing signature ',sPath);
              end;
            finally
              FreeAndNil(Term);
            end;
          end else begin
            sPath:=FMT_MOUNT_TARGET;
            sPath:=StringReplace(sPath,'$nid',IntToStr(Nodes[iLcv]^.ID),[rfReplaceAll,rfIgnoreCase]);
            sPath:=StringReplace(sPath,'$rid',IntToStr(Nodes[iLcv]^.ResourceID),[rfReplaceAll,rfIgnoreCase]);
            sPath:=StringReplace(sPath,'$cid',IntToStr(Nodes[iLcv]^.ClusterID),[rfReplaceAll,rfIgnoreCase]);
            sPath:=Concat(sPath,AuDisk);
            if Not FileExists(sPath) then
              sErrors:=Concat(sErrors,'Error in mounted AuDisk : missing signature ',sPath);
          end;
        end;
      end;
    end;
  end;
end;


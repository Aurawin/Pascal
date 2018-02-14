  Core.Arrays.KeyString.fromFile(kpFile,'/etc/resolv.conf',#32,#10);
  Try
    for iLcv:=0 to High(kpFile) do begin
      if Core.Strings.SameText(kpFile[iLcv]^.Key,RSR.DNS.NameServer) then
        Core.Arrays.LargeWord.Add(Core.Utils.Sockets.InAddrFromStr(kpFile[iLcv]^.Value),List);
    end;
  finally
    Empty(kpFile);
  end;

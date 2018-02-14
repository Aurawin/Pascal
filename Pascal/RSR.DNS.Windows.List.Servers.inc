    iCount:=0;
    OutLen := System.SizeOf(TFixedInfo);
    GetMem(pFI, System.SizeOf(TFixedInfo));
    try
      if GetNetworkParams(pFI, @OutLen) = ERROR_BUFFER_OVERFLOW then begin
        ReallocMem(pFI, OutLen);
        if GetNetworkParams(pFI, @OutLen) = NO_ERROR then
          PushAdd;
      end else
        PushAdd;
    finally
      FreeMem(pFI);
    end;

dftBoolean      : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftByte         : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftWord         : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftInteger      : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftInt64        : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftQword        : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftInt64Array   : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,Core.Database.toInCriteria(Core.Database.Types.PLargeIntArray(CommandP^.Value)^),[]);
dftQWordArray   : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,Core.Database.toInCriteria(Core.Database.Types.PLargeWordArray(CommandP^.Value)^),[]);
dftDateTime     : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftFloat        : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftDouble       : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftMD5Digest    : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo
dftSmallString  : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,Core.Database.Types.PSmallString(CommandP^.Value)^,[]);
dftString       : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,Core.Database.Types.PVarString(CommandP^.Value)^,[]);
dftMemo         : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,Core.Database.Types.PVarString(CommandP^.Value)^,[]);
dftByteBuffer   : sCriteria:=SysUtils.StringReplace(sCriteria,sParamIDX,'',[]); // todo


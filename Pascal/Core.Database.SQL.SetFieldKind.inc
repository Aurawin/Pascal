Case TableP^.Fields[iLcv].DataType of
  dftBoolean      : sFieldKind:='BOOLEAN';
  dftByte         : sFieldKind:=Core.Database.Types.BYTE_FIELD[ModeP^];
  dftWord         : sFieldKind:='INTEGER';
  dftInteger      : sFieldKind:='INTEGER';
  dftInt64        : sFieldKind:='BIGINT';
  dftQword        : sFieldKind:='BIGINT';
  dftInt64Array   : sFieldKind:=Core.Database.Types.LONG_TEXT_FIELD[ModeP^];
  dftQWordArray   : sFieldKind:=Core.Database.Types.LONG_TEXT_FIELD[ModeP^];
  dftDateTime     : sFieldKind:=Core.Database.Types.DOUBLE_FIELD[ModeP^];
  dftFloat        : sFieldKind:='FLOAT';
  dftDouble       : sFieldKind:=Core.Database.Types.DOUBLE_FIELD[ModeP^];
  dftMD5Digest    : sFieldKind:=Format(FMT_VARCHAR_FIELD[TableP^.Fields[iLcv].Precision>0],[IntToStr(TableP^.Fields[iLcv].Precision)]);
  dftSmallString  : sFieldKind:=SMALLSTRING_FIELD[ModeP^];
  dftString       : sFieldKind:=Format(FMT_VARCHAR_FIELD[TableP^.Fields[iLcv].Precision>0],[IntToStr(TableP^.Fields[iLcv].Precision)]);
  dftMemo         : sFieldKind:=Core.Database.Types.LONG_TEXT_FIELD[ModeP^];
  dftByteBuffer   : sFieldKind:=Core.Database.Types.LONG_TEXT_FIELD[ModeP^];
else
  sFieldKind:='UNKNOWN';
end;


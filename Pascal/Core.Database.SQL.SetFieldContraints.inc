// $NOT_NULL$ $UNIQUE$ $AUTO_INCREMENT$ $PRIMARY_KEY$
if ( (TableP^.Fields[iLcv].Flags or cfNotNull) = TableP^.Fields[iLcv].Flags) then
  sFieldNotNull:='NOT NULL';
if ( (TableP^.Fields[iLcv].Flags or cfUnique) = TableP^.Fields[iLcv].Flags) then
  sFieldUnique:='UNIQUE';
if ( (TableP^.Fields[iLcv].Flags or cfPrimaryKey) = TableP^.Fields[iLcv].Flags) then
  sFieldKey:='PRIMARY KEY';
if ( (TableP^.Fields[iLcv].Flags or cfIdentity) = TableP^.Fields[iLcv].Flags) then begin
  sFieldID:=Core.Database.Types.AUTO_INCREMENT[ModeP^];
  if (ModeP^=PostgreSQL) then begin
    if (
        (TableP^.Fields[iLcv].DataType=dftInt64) or
        (TableP^.Fields[iLcv].DataType=dftQWord)
    ) then begin
      sFieldKind:='BIGSERIAL';
      sFieldID:='';
    end else if (TableP^.Fields[iLcv].DataType=dftInteger) then begin
      sFieldKind:='SERIAL';
      sFieldID:='';
    end;
  end;
end;

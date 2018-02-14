var
  TableP                : Core.Database.Types.PTable;
  iLcv                  : LongInt;
  iLength               : LongInt;
  iRetries              : LongInt;
  sTable                : Core.Strings.VarString;
  sCriteria             : Core.Strings.VarString;
  sField                : Core.Strings.VarString;
  sCommand              : Core.Strings.VarString;
  Parameters            : Core.Database.Types.CommandParameters;

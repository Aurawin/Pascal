var
  TableP                : Core.Database.Types.PTable;
  iResetIndex           : LongInt;
  iPrimaryIDIndex       : LongInt;
  iCriteriaIndex        : LongInt;
  iTableIndex           : LongInt;

  iRetries              : LongInt;
  iLength               : LongInt;

  sPrimaryField         : Core.Strings.VarString;
  sTable                : Core.Strings.VarString;
  sFields               : Core.Strings.VarString;
  sValues               : Core.Strings.VarString;
  sCriteria             : Core.Strings.VarString;
  sCommand              : Core.Strings.VarString;

  Parameters            : Core.Database.Types.CommandParameters;

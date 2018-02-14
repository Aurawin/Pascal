var
  TableP                : Core.Database.Types.PTable;
  iLcv                  : LongInt;
  iLength               : LongInt;
  sTable                : Core.Strings.VarString;
  sView                 : Core.Strings.VarString;
  sFields               : Core.Strings.VarString;
  sNewFields            : Core.Strings.VarString;
  sCriteria             : Core.Strings.VarString;
  sCommand              : Core.Strings.VarString;
  sDataLengthStr        : Core.Strings.VarString;
  Parameters            : Core.Database.Types.CommandParameters;

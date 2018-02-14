unit Core.Arrays;

interface

uses
  Classes, SysUtils,

  Core.Strings;

Const

  Stream_Off = false;
  Stream_On  = true;
Type
  SplitOption=(soClearList,soMakeKeyLowercase,soTrimValue,soMakeLowercase,soRemoveQuotes,soIgnoreDelimAtStart,soSingleton,soTrimLines,soParenWraps,soBraceWraps,soBracketWraps,soQuoteWraps);
  SplitOptions=set of SplitOption;

  AddOption=(aoNone,aoCheckForDuplicates,aoOverwriteDuplicate);
  AddOptions=set of AddOption;

  SearchOption=(soAnywhere,soStart);

  SaveOption=(soNone,soUTF8);

  ParameterOption=(poZeroBased,poOneBased);

implementation



end.


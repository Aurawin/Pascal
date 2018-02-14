sFieldStatement:=StringReplace(sFieldStatement,'$KIND$',sFieldKind,[rfReplaceAll]);
sFieldStatement:=StringReplace(sFieldStatement,'$NOT_NULL$',sFieldNotNull,[rfReplaceAll]);
sFieldStatement:=StringReplace(sFieldStatement,'$UNIQUE$',sFieldUnique,[rfReplaceAll]);
sFieldStatement:=StringReplace(sFieldStatement,'$AUTO_INCREMENT$',sFieldID,[rfReplaceAll]);
sFieldStatement:=StringReplace(sFieldStatement,'$PRIMARY_KEY$',sFieldKey,[rfReplaceAll]);

sFieldStatement:=SysUtils.Trim(StringReplace(sFieldStatement,'  ',' ',[rfReplaceAll]));

sFieldsCollection:=Concat(sFieldsCollection,sFieldStatement,', ');

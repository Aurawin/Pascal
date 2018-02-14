procedure PushSetupStatement();
var
  iLcv:LongInt;
begin
  sCommand:=SQL_INSERT_STATEMENT;
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForInsert:ODBC_PushUsage_Inserts(sFields,sValues,@CommandsP^[iLcv],Parameters);
      useForValues:PushUsage_Values(sValues,@CommandsP^[iLcv],Parameters);
      useForFields:PushUsage_Fields(sFields,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sTable);
  If (iLength>0) Then SetLength(sTable,iLength-1);
  iLength:=Length(sFields);
  If (iLength>0) Then SetLength(sFields,iLength-1);
  iLength:=Length(sValues);
  If (iLength>0) Then SetLength(sValues,iLength-1);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$FIELDS$',sFields,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$VALUES$',sValues,[]);
end;


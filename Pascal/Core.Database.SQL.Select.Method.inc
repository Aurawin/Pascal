Result:=False; iRetries:=0;
Try
  sDataLengthStr:=Core.Database.Types.SQL_DATALENGTH_STRING[Task.Header.Mode];
  sCommand:=Core.Database.Types.SQL_SELECT_STATEMENT; SetLength(Parameters,0);
  sCriteria:=''; sTable:=''; sFields:=''; sOrderBy:=''; sLimit:='';
  TableP:=nil;
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForCriteria:PushUsage_Criteria(Task,'SQL_SELECT',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
      useForFields:PushUsage_Fields(sFields,@CommandsP^[iLcv]);
      useForOrderBy: PushUsage_OrderBy(sOrderBy,@CommandsP^[iLcv]);
      useForLimit : PushUsage_Limit(sLimit,@CommandsP^[iLcv]);
      useForDataLength: PushUsage_DataLength(sDataLengthStr,sFields,@CommandsP^[iLcv]);
      useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sTable);
  If (iLength>0) Then SetLength(sTable,iLength-1);
  iLength:=Length(sFields);
  If (iLength>0) Then SetLength(sFields,iLength-1);
  sCommand:=SysUtils.StringReplace(sCommand,'$FIELDS$',sFields,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$ORDERBY$',sOrderBy,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$LIMIT$',sLimit,[]);
  if sCriteria='' then
    sCommand:=SysUtils.StringReplace(sCommand,'WHERE','',[]);
  sCommand:=SysUtils.StringReplace(sCommand,#32#32,#32,[rfReplaceAll]);
  // Now we go through parameters and assign locations...
  Result:=Push_Select;
Finally
  Done(Parameters);
End;

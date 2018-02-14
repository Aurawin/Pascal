Result:=False; iRetries:=0;
Try
  Empty(Parameters); TableP:=nil;
  sCommand:=Core.Database.Types.SQL_SUM[Task.Header.Mode];
  sCriteria:=''; sTable:=''; Count:=0; sField:='';
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForCriteria:PushUsage_Criteria(Task,'SQL_Sum',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
      useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
      useForFields:PushUsage_Sum(sField,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sTable);

  If (iLength>0) Then SetLength(sTable,iLength-1);
  sCommand:=StringReplace(sCommand,'$TYPE$',Core.Database.Types.SQL_BIG_INT[Task.Header.Mode],[]);
  sCommand:=StringReplace(sCommand,'$FIELD$',sField,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);
  if sCriteria='' then
    sCommand:=SysUtils.StringReplace(sCommand,'WHERE','',[]);

  Result:=PushSum;

Finally
  Done(Parameters);
End;


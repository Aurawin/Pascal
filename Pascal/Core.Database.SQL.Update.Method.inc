
Result:=False; iRetries:=0;
Try
  sCommand:=SQL_UPDATE_STATEMENT; TableP:=nil;
  sCriteria:=''; sUpdates:=''; sTable:=''; Empty(Parameters);
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForCriteria:PushUsage_Criteria(Task,'SQL_Update',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
      useForUpdates,useForFields,useForValues:PushUsage_Updates(sUpdates,@CommandsP^[iLcv],Parameters);
      useForIncrement : PushUsage_Increment(sUpdates,@CommandsP^[iLcv],Parameters);
      useForDecrement : PushUsage_Decrement(sUpdates,@CommandsP^[iLcv],Parameters);
      useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sTable);
  If (iLength>0) Then SetLength(sTable,iLength-1);
  iLength:=Length(sUpdates);
  If (iLength>0) Then SetLength(sUpdates,iLength-1);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$UPDATES$',sUpdates,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);
  if sCriteria='' then
    sCommand:=SysUtils.StringReplace(sCommand,'WHERE','',[]);

  Result:=PushUpdate;
Finally
  Done(Parameters);
End;


Result:=false;
if Task.Connection.Connected=false then
  Task.Connection.Connected:=true;
Task.LastUsed:=Core.Timer.dtNow;
Try
  sCommand:=Core.Database.Types.SQL_LENGTH[Task.Header.Mode];
  Empty(Parameters);
  TableP:=nil;
  sCriteria:=''; sTable:=''; sField:='';
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForCriteria:PushUsage_Criteria(Task,'SQL_DataLength',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
      useForFields:PushUsage_Fields(sField,@CommandsP^[iLcv]);
      useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sTable);
  If (iLength>0) Then SetLength(sTable,iLength-1);
  iLength:=Length(sField);
  If (iLength>0) Then SetLength(sField,iLength-1);
  sCommand:=SysUtils.StringReplace(sCommand,'$FIELD$',sField,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);
  // Now we go through parameters and assign locations...
  Task.Query.SQL.Text:=sCommand;
  Try
    For iLcv:=0 to Length(Parameters)-1 do
      BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
    Task.Query.Prepare;
    Try
      Task.Query.Open;
      If Not Task.Query.EOF then begin
        Value:=Task.Query.Fields[0].AsLargeInt;
        Result:=True;
      end;
    finally
      Task.Query.Close;
    end;
  Except
    On E:Exception do begin
      If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'SQL_Count.Execute',sTable,Task.Name,Format(SQL_DATALENGTH_EXCEPTION_MESSAGE,[sCommand,E.Message]));
    end;
  End;
Finally
  Done(Parameters);
End;


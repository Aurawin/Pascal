Result:=False;

if (Task.Connection.Connected=false) then
  Task.Connection.Connected:=true;


Task.LastUsed:=Core.Timer.dtNow;
Try
  sCommand:=Core.Database.Types.SQL_COUNT_STATEMENT; Empty(Parameters);
  sCriteria:=''; sTable:=''; Output:=0; TableP:=nil;
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForCriteria:PushUsage_Criteria(Task,'SQL_Count',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
      useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
      //useForFields:PushUsage_Fields(sField,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sTable);
  If (iLength>0) Then SetLength(sTable,iLength-1);
  //iLength:=Length(sField);
  //If (iLength>0) Then SetLength(sField,iLength-1);
  //sCommand:=StringReplace(sCommand,'$FIELD$',sField,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);
  if sCriteria='' then
    sCommand:=SysUtils.StringReplace(sCommand,'WHERE','',[]);
  // Now we go through parameters and assign locations...
  Task.Query.SQL.Text:=sCommand;
  Try
    For iLcv:=0 to Length(Parameters)-1 do
      BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
    Task.Query.Prepare;
    Try
      Task.Query.Open;
      If (Task.Query.EOF=false) then begin
        with Task.Query.Fields[0] do begin
          case DataType of
            ftInteger  : Output:=AsInteger;
            ftLargeint : Output:=AsLargeInt;
          end;
        end;
        Result:=True;
      end;
    finally
      Task.Query.Close;
    end;
  Except
    On E:Exception do begin
      If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Databse.SQL.Count.Method.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
    end;
  End;
Finally
  Done(Parameters);
End;

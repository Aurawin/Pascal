Result:=False;
if Task.Connection.Connected=false then
  Task.Connection.Connected:=true;
Task.LastUsed:=Core.Timer.dtNow;
Try
  Empty(Parameters);
  sDataLengthStr:=Core.Database.Types.SQL_SUM_LENGTH[Task.Header.Mode];
  sCommand:=Core.Database.Types.SQL_CREATE_VIEW[Task.Header.Mode];
  sView:=''; sCriteria:=''; sTable:=''; sFields:=''; sNewFields:='';
  TableP:=nil;
  For iLcv:=0 to Length(CommandsP^)-1 do begin
    Case CommandsP^[iLcv].Useage of
      useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
      useForView: PushUsage_View(sView,@CommandsP^[iLcv]);
      useForCriteria:PushUsage_Criteria(Task,'SQL_CreateView',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
      useForFields:PushUsage_View_Fields(sFields,sNewFields,@CommandsP^[iLcv],sDataLengthStr);
      useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
    End;
  end;
  iLength:=Length(sView);
  If (iLength>0) Then SetLength(sView,iLength-1);
  iLength:=Length(sTable);
  If (iLength>0) Then SetLength(sTable,iLength-1);
  iLength:=Length(sFields);
  If (iLength>0) Then SetLength(sFields,iLength-1);
  iLength:=Length(sNewFields);
  If (iLength>0) Then SetLength(sNewFields,iLength-1);

  sCommand:=SysUtils.StringReplace(sCommand,'$FIELDS$',sFields,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$NEWFIELDS$',sNewFields,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$VIEW$',sView,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
  sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);
  // Now we go through parameters and assign locations...
  Task.Query.SQL.Text:=sCommand;
  Try
    If Task.Transaction.Active=false then
      Task.Transaction.StartTransaction;
    For iLcv:=0 to Length(Parameters)-1 do
      BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
    Task.Query.Prepare;
    Try
      Task.Query.ExecSQL;
      Task.Transaction.Commit();
    finally
      Task.Query.Close;
    end;
    Result:=True;
  Except
    On E:Exception do begin
      If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.CreateView.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
    end;
  End;
Finally
  Done(Parameters);
End;

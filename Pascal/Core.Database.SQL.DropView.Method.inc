Result:=False;
if Task.Connection.Connected=false then
  Task.Connection.Connected:=true;
Task.LastUsed:=Core.Timer.dtNow;

sCommand:=Core.Database.Types.SQL_DROPVIEW_STATEMENT;
sView:='';
For iLcv:=0 to Length(CommandsP^)-1 do begin
  Case CommandsP^[iLcv].Useage of
    useAsTable,useForView:PushUsage_View(sView,@CommandsP^[iLcv]);
  End;
end;
iLength:=Length(sView);
If (iLength>0) Then SetLength(sView,iLength-1);
sCommand:=SysUtils.StringReplace(sCommand,'$VIEW$',sView,[]);
// Now we go through parameters and assign locations...
Task.Query.SQL.Text:=sCommand;
Try
  if Task.Transaction.Active then Task.Transaction.Commit();
  Task.Transaction.StartTransaction;
  Task.Query.Prepare;
  Try
    Task.Query.ExecSQL;
    Task.Transaction.Commit();
  finally
    Task.Query.Close;
  end;
Except
  On E:Exception do begin
    If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.DropView.Execute',sView,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
  end;
End;

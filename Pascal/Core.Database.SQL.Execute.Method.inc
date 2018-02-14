Result:=False;
Task.Query.SQL.Text:=SQL;
Try
  Task.Query.ExecSQL();
  Result:=True;
Except
  On E:Exception do begin
    If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Execute.Method','SQL',Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[SQL,E.Message]));
  end;
end;


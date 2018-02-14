function PushUpdate:Boolean;
var
  iLcv                : LongInt;
begin
  Result:=false;

  Task.LastUsed:=Core.Timer.dtNow;

  if Task.Connection.Connected=false then
    Task.Connection.Connected:=True;

  if (Task.TransactionLock=false) then begin
    if Task.Transaction.Active then  Task.Transaction.Commit();
    Task.Transaction.StartTransaction();
  end;

  Task.Query.SQL.Text:=sCommand;
  //DBMSP^.Query.
  For iLcv:=0 to Length(Parameters)-1 do
    BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
  Try
    Task.Query.Prepare;
    Task.Query.ExecSQL;
    if TableP^.StartupP^.AutoCommit then Task.Transaction.Commit();
    Result:=True;
  Except
     on E:Exception do begin
       Task.Query.Close;
       Case Task.Header.Mode of
         PostgreSQL : begin
           If Core.Strings.Search('no connection',E.Message)>0 then begin
             Task.Connection.Connected:=False;
             Inc(iRetries);
             If iRetries<3 then
               Result:=PushUpdate()
             else if Assigned(Task.Header.OnException) then
               Task.Header.OnException(Task.Header.ModuleName,'SQL_Update.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
           end else If Assigned(Task.Header.OnException) then begin
             Task.Header.OnException(Task.Header.ModuleName,'SQL_Update.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
           end;
         end;
         MySQL51: begin
           If Core.Strings.Search('gone away',E.Message)>0 then begin
             Task.Connection.Connected:=False;
             Inc(iRetries);
             If iRetries<3 then
               Result:=PushUpdate()
             else if Assigned(Task.Header.OnException) then
               Task.Header.OnException(Task.Header.ModuleName,'SQL_Update.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
           end else If Assigned(Task.Header.OnException) then begin
             Task.Header.OnException(Task.Header.ModuleName,'SQL_Update.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
           end;
         end;
       else
         If Assigned(Task.Header.OnException) then
           Task.Header.OnException(Task.Header.ModuleName,'SQL_Update.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
       end;
     end;
  end;
  Task.Query.Close;
end;


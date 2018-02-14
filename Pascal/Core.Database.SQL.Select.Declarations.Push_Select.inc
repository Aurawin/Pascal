function Push_Select:System.Boolean;
var
  iLcv                : LongInt;
begin
  Result:=false;
  Task.LastUsed:=Core.Timer.dtNow;
  if Task.Connection.Connected=false then
    Task.Connection.Connected:=True;
  Try
    if (Task.TransactionLock=false) then begin
      if Task.Transaction.Active then Task.Transaction.Commit();
      Task.Transaction.StartTransaction();
    end;
    Task.Query.SQL.Text:=sCommand;
    For iLcv:=0 to Length(Parameters)-1 do
      BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
    Try
      Task.Query.Prepare;
      Task.Query.Open;
      Result:=True;
      Try
        While Not Task.Query.EOF do begin
          Try
            Callback(CommandsP,Task.Query.Fields,DataP);
          Except
            On E:Exception do  If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Callback',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
          End;
          Task.Query.Next;
        end;
      finally
        Task.Query.Close();
      end;
      if (Task.TransactionLock=false) then begin
        if Task.Transaction.Active then Task.Transaction.Commit();
      end;
    except
      On E:Exception do  begin
        If Assigned(Task.Header.OnException) then
          Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Prepare',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
        Task.Connection.Connected:=false;
        Inc(iRetries);
        If iRetries<3 then
          Result:=Push_Select();
      end;
    end;
  Except
    on E:Exception do begin
      Task.Connection.Connected:=False;
      Case Task.Header.Mode of
        PostgreSQL : begin
          If (
            (Core.Strings.Search(E.Message,'no connection')>0) or
            (Core.Strings.Search(E.Message,'ssl error')>0) or
            (Core.Strings.Search(E.Message,'query failed')>0)

          ) then begin
            Inc(iRetries);
            If iRetries<3 then
              Result:=Push_Select()
            else if Assigned(Task.Header.OnException) then
              Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
          end else If Assigned(Task.Header.OnException) then begin
            Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
          end;
          if (Task.TransactionLock=false) then begin
            if Task.Transaction.Active then
              Task.Transaction.EndTransaction;
          end;
        end;
        MySQL51: begin
          If (Core.Strings.Search(E.Message,'gone away')>0) then begin
            Inc(iRetries);
            If iRetries<3 then
              Result:=Push_Select()
            else if Assigned(Task.Header.OnException) then
              Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
          end else If Assigned(Task.Header.OnException) then begin
            Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
          end;
        end;
      else
        If Assigned(Task.Header.OnException) then
          Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Select.Declarations.Push_Select.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
      end;
    end;
  end;
end;


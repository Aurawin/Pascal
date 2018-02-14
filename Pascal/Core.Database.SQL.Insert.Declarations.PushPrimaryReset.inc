procedure PushPrimaryReset();
var
  iLcv:LongInt;
begin
  iPrimaryIDIndex:=Core.Database.IndexOf(CommandsP,useForPrimaryID);
  If (iPrimaryIDIndex<>-1) then begin
    If (CommandsP^[iPrimaryIDIndex].Value<>Nil) then begin
      sPrimaryField:=CommandsP^[iPrimaryIDIndex].Operation;
      iCriteriaIndex:=Core.Database.IndexOf(CommandsP,useForCriteria);
      If (iCriteriaIndex<>-1) then begin
        If (CommandsP^[iCriteriaIndex].Value<>Nil) then begin
          For iLcv:=0 to Length(CommandsP^)-1 do begin
            If CommandsP^[iLcv].Useage=useForCriteria then begin
              PushUsage_Criteria(Task,'SQL_Insert',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
            end;
          end;
          sCommand:=SQL_SELECT_PRIMARYID;
          sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
          sCommand:=SysUtils.StringReplace(sCommand,'$ITEM_ID$',CommandsP^[iPrimaryIDIndex].Operation,[]);
          sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);

          Task.Query.SQL.Text:=sCommand;
          For iLcv:=0 to Length(Parameters)-1 do
            BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
          Task.Query.Prepare;
          Try
            Task.Query.Open;
            Try
              If Not Task.Query.EOF then begin
                iResetIndex:=Core.Database.IndexOf(CommandsP,useForResetInsertID);
                SetFieldCommandValue(Task.Query.Fields.FieldByName(sPrimaryField),CommandsP^[iPrimaryIDIndex],CommandsP);

                // Primary ID found check to see if we have to reset InsertID
                If iResetIndex<>-1 then begin
                  Task.Query.Close();
                  sCommand:=Core.Database.Types.SQL_RESET_INSERT_ID_STATEMENT;
                  sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
                  sCommand:=SysUtils.StringReplace(sCommand,'$INSERTID$',CommandsP^[iResetIndex].Operation,[]);
                  sCommand:=SysUtils.StringReplace(sCommand,'$PRIMARYID$',CommandsP^[iPrimaryIDIndex].Operation,[]);

                  Task.Query.SQL.Text:=sCommand;

                  case CommandsP^[iResetIndex].DataType of
                    dftInt64: Task.Query.Params.ParamByName('PARAM_1_').AsLargeInt:=PInt64(CommandsP^[iResetIndex].Value)^;
                    dftQWord: Task.Query.Params.ParamByName('PARAM_1_').AsLargeInt:=PQWord(CommandsP^[iResetIndex].Value)^;
                  end;
                  case CommandsP^[iPrimaryIDIndex].DataType of
                    dftInt64: Task.Query.Params.ParamByName('PARAM_2_').AsLargeInt:=PInt64(CommandsP^[iPrimaryIDIndex].Value)^;
                    dftQWord: Task.Query.Params.ParamByName('PARAM_2_').AsLargeInt:=PQWord(CommandsP^[iPrimaryIDIndex].Value)^;
                  end;

                  Task.Query.Prepare;
                  Try
                    Task.Query.ExecSQL;
                    if (Task.TransactionLock=false) then
                      Task.Transaction.Commit();
                    Result:=True;
                  Except
                    On E:Exception do begin
                      If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Insert.Declarations.PushPrimaryReset.Reset',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
                    end;
                  End;
                end;
              end;
            finally
              Task.Query.Close;
            end;
          Except
            On E:Exception do begin
              If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Insert.Declarations.PushPrimaryReset.GetPrimaryIndex',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
            end;
          End;
        end else
          If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Insert.Declarations.PushPrimaryReset.Criteria',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,Concat('The value assigned in the Criteria field ',CommandsP^[iCriteriaIndex].Operation,' is not set.')]));
      end else
        If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Insert.Declarations.PushPrimaryReset.CriteriaIndex',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,'A Criteria field was not set.']));
    end else
      If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'Core.Database.SQL.Insert.Declarations.PushPrimaryReset.PrimaryID',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,Concat('The value assigned in the PrimaryID field ',CommandsP^[iPrimaryIDIndex].Operation,' is not set.')]));
  end else begin
    if (Task.TransactionLock=false) then begin
      Task.Transaction.Commit();
      Task.Transaction.EndTransaction();
    end;
    Result:=True;
  end;
end;

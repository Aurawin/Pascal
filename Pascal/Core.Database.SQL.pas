unit Core.Database.SQL;

interface

uses
  DB,SQLDB,Core.Timer,Core.Strings,Core.Arrays.Types,Core.Arrays.LargeInt, Core.Arrays.LargeWord,Core.Arrays.VarString,
  Encryption.Base64,
  Core.Arrays.Bytes,Core.Database.Types;

procedure  StartTransaction(Task:Core.Database.Types.TTask);
procedure  CommitTransaction(Task:Core.Database.Types.TTask);

procedure  AddTable(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable);
Function   Delete(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
Function   Select(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Callback:Core.Database.Types.Callback; Const DataP:System.Pointer=Nil):System.Boolean; Overload;
Function   Select(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Callback:Core.Database.Types.Event; Const DataP:System.Pointer=Nil):System.Boolean; Overload;
Function   Update(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
Function   Insert(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
Function   Count(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Output:System.QWord):System.Boolean; overload;
Function   Count(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Output:System.Int64):System.Boolean; overload;
Function   Sum(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Count:System.Int64):System.Boolean; overload;
Function   Sum(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Count:System.QWord):System.Boolean; overload;
Function   CreateView(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
Function   DropView(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
Function   DataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Value:System.Int64):System.boolean overload;
Function   DataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Value:System.Qword):System.boolean overload;
Function   SumDataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Length:System.Int64):System.boolean; overload;
Function   SumDataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Length:System.QWord):System.boolean; overload;
Function   Execute(Task:Core.Database.Types.TTask; var SQL:Core.Strings.VarString):System.Boolean;


procedure BindParameter(Index:LongInt; var Parameter:Core.Database.Types.CommandParameter; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Bool; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Byte; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Word; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Integer; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeInt; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWord; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeIntArray; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWordArray; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Float; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Double; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.MD5Digest; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.VarString; Query:TSQLQuery; Kind:TParamType); overload;
procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Buffer; Query:TSQLQuery; Kind:TParamType); overload;


Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Bool; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Byte; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Integer; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeInt; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeWord; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeIntArray; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeWordArray; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Float; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.MD5Digest; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Double; Query:TSQLQuery); overload;
Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Buffer; Query:TSQLQuery); overload;


implementation
uses Core.Database,SysUtils;

procedure PushUsage_Increment(Var sUpdates:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand; Var Parameters:Core.Database.Types.CommandParameters);
begin
  sUpdates:=Concat(sUpdates,CommandP^.Operation,'=',CommandP^.Operation,' + :PARAM_',IntToStr(Length(Parameters)),'_,');
  Case CommandP^.DataType of
    {$i Core.Database.SQL.AssignCommandParameters.inc}
  End;
end;

procedure PushUsage_Decrement(Var sUpdates:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand; Var Parameters:Core.Database.Types.CommandParameters);
begin
  sUpdates:=Concat(sUpdates,CommandP^.Operation,'=',CommandP^.Operation,' - :PARAM_',IntToStr(Length(Parameters)),'_,');
  Case CommandP^.DataType of
    {$i Core.Database.SQL.AssignCommandParameters.inc}
  End;
end;

procedure PushUsage_Updates(Var sUpdates:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand; Var Parameters:Core.Database.Types.CommandParameters);
begin
  sUpdates:=Concat(sUpdates,CommandP^.Operation,'=:PARAM_',IntToStr(Length(Parameters)),'_,');
  Case CommandP^.DataType of
    {$i Core.Database.SQL.AssignCommandParameters.inc}
  End;
end;

procedure PushUsage_Values(Var sValues:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand; Var Parameters:Core.Database.Types.CommandParameters);
begin
  sValues:=Concat(sValues,':PARAM_',IntToStr(Length(Parameters)),'_,');
  Case CommandP^.DataType of
    {$i Core.Database.SQL.AssignCommandParameters.inc}
  End;
end;

procedure PushUsage_Criteria_Brackets(Task:Core.Database.Types.TTask; var sCriteria:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand);
begin
 sCriteria:=Concat(sCriteria,SQL_PREOPERATOR[CommandP^.SQLPreOperator],SQL_OPERATOR[CommandP^.SQLOperator]);
end;

procedure PushUsage_Criteria(Task:Core.Database.Types.TTask; sFunction,sTable:Core.Strings.VarString; Var sCriteria:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand; Var Parameters:Core.Database.Types.CommandParameters);
var
  sWorkingCriteria    : Core.Strings.VarString;
  sParamIDX : Core.Strings.VarString;
begin
  If (CommandP^.SQLPreOperator=poNone) and (CommandP^.SQLOperator=oNone) then begin
    If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,'PushUsage_Criteria',sTable,Task.Name,Format(SQL_CRITERIA_EXCEPTION_MESSAGE,[sFunction,sTable,CommandP^.Operation,IntToStr(CommandP^.PropertyID),'Nothing assigned to pre or main operators.']));
    exit;
  end;
  sParamIDX:=IntToStr(Length(Parameters));
  sWorkingCriteria:=SQL_SELECT_CRITERIA;
  sWorkingCriteria:=SysUtils.StringReplace(sWorkingCriteria,'$PREOPERATOR$',SQL_PREOPERATOR[CommandP^.SQLPreOperator],[]);
  sWorkingCriteria:=SysUtils.StringReplace(sWorkingCriteria,'$OPERATION$',CommandP^.Operation,[]);
  sWorkingCriteria:=SysUtils.StringReplace(sWorkingCriteria,'$OPERATOR$',SQL_OPERATOR[CommandP^.SQLOperator],[]);
  sWorkingCriteria:=SysUtils.StringReplace(sWorkingCriteria,'$PARAMETER$',sParamIDX,[]);
  If sWorkingCriteria<>'' then begin
    sCriteria:=Concat(sCriteria,sWorkingCriteria);
    if (CommandP^.SQLOperator = oIn) then begin
      sParamIDX:=Concat(':PARAM_',sParamIDX,'_');
      Case CommandP^.DataType of
        {$i Core.Database.SQL.AssignCommandInParameters.inc}
      end;
    end else if CommandP^.SQLOperator = oNotNull then begin
      // nothing to do yet
    end else begin
      Case CommandP^.DataType of
        {$i Core.Database.SQL.AssignCommandParameters.inc}
      end;
    end;
  end;
end;

procedure PushUsage_Table(Var sTable:Core.Strings.VarString; var TableP:Core.Database.Types.PTable; FieldP:Core.Database.Types.PCommand);
begin
  TableP:=Core.Database.Types.PTable(FieldP^.Value);
  sTable:=Concat(sTable,FieldP^.Operation,',');
end;

procedure PushUsage_View(Var sView:Core.Strings.VarString; FieldP:Core.Database.Types.PCommand);
begin
  sView:=Concat(sView,FieldP^.Operation,',');
end;

procedure PushUsage_Limit(var sLimit:Core.Strings.VarString; CommandsP:Core.Database.Types.PCommand);
begin
  case CommandsP^.DataType of
    dftBoolean                   : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',IntToStr(Integer(PBool(CommandsP^.Value)^)),[]);
    dftByte                      : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',IntToStr(PByte(CommandsP^.Value)^),[]);
    dftWord                      : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',IntToStr(PInteger(CommandsP^.Value)^),[]);
    dftInteger                   : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',IntToStr(PInteger(CommandsP^.Value)^),[]);
    dftInt64                     : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',IntToStr(PInt64(CommandsP^.Value)^),[]);
    dftQWord                     : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',IntToStr(PQword(CommandsP^.Value)^),[]);
    dftInt64Array                : sLimit:='';
    dftQWordArray                : sLimit:='';
    dftDateTime                  : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',FloatToStr(PDateTime(CommandsP^.Value)^),[]);
    dftFloat                     : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',FloatToStr(PFloat(CommandsP^.Value)^),[]);
    dftDouble                    : sLimit:=SysUtils.StringReplace(SQL_LIMIT,'$LIMIT$',FloatToStr(PDouble(CommandsP^.Value)^),[]);
    dftSmallString               : sLimit:='';
    dftString                    : sLimit:='';
    dftMemo                      : sLimit:='';
    dftByteBuffer                : sLimit:='';
  end;
end;

procedure PushUsage_OrderBy(Var sOrderBy:Core.Strings.VarString; CommandsP:Core.Database.Types.PCommand);
var
  sCriteria:Core.Strings.VarString;
begin
  SetLength(sCriteria,0);
  sOrderBy:=SysUtils.StringReplace(SQL_ORDER_BY,'$FIELD$',CommandsP^.Operation,[]);
  if CommandsP^.SQLOperator in [oAscending,oDescending] then
    sCriteria:=SQL_OPERATOR[CommandsP^.SQLOperator];
  sOrderBy:=SysUtils.StringReplace(sOrderBy,'$BY$',sCriteria,[]);
end;

procedure PushUsage_Fields(Var sFields:Core.Strings.VarString; FieldP:Core.Database.Types.PCommand);
begin
  sFields:=Concat(sFields,FieldP^.Operation,',');
end;

procedure PushUsage_Sum(Var sFields:Core.Strings.VarString; FieldP:Core.Database.Types.PCommand);
begin
  sFields:=FieldP^.Operation;
end;

procedure PushUsage_DataLength(DataLengthStr:Core.Strings.VarString; Var sFields:Core.Strings.VarString; FieldP:Core.Database.Types.PCommand);
begin
  sFields:=Concat(sFields,DataLengthStr,'(',FieldP^.Operation,'),');
end;

procedure PushUsage_View_Fields(Var sFields,sNewFields:Core.Strings.VarString; FieldP:Core.Database.Types.PCommand; Const DataLengthStr:Core.Strings.VarString='DATALENGTH');
begin
  Case FieldP^.SQLOperator of
    //oIdentity:         sNewFields:=Concat(sNewFields,'IDENTITY(int 1,1) as ',FieldP^.Operation,',');
    oIdentity: begin
      sFields:=Concat(sFields,FieldP^.Operation,' IDENTITY,');
    end;
    oDataLength: begin
      sFields:=Concat(sFields,FieldP^.Operation,',');
      sNewFields:=Concat(sNewFields,DataLengthStr,'(',PShortString(FieldP^.Value)^,'),');
    end;
    else begin
      sFields:=Concat(sFields,FieldP^.Operation,',');
      sNewFields:=Concat(sNewFields,FieldP^.Operation,',');
    End;
  End;
end;

procedure ODBC_PushUsage_Inserts(Var sFields,sValues:Core.Strings.VarString; CommandP:Core.Database.Types.PCommand; Var Parameters:Core.Database.Types.CommandParameters);
begin
  PushUsage_Fields(sFields,CommandP);
  PushUsage_Values(sValues,CommandP,Parameters);
end;

Function GetTables(Task:Core.Database.Types.TTask; Out List:Core.Arrays.Types.VarString):System.Boolean;
var
  sEntry:Core.Strings.VarString;
  sField:Core.Strings.VarString;
  sSQL:Core.Strings.VarString;
  iLcv:LongInt;
begin
  Result:=False;
  If not Task.Connection.Connected then exit;
  sSQL:=FORMAT(Core.Database.Types.SQL_LIST_TABLES[Task.Header.Mode],[Task.Header.Schema]);
  Task.Query.SQL.Text:=sSQL;
  Task.Query.Prepare;

  Task.Query.Open;
  Try
    Core.Arrays.VarString.Empty(List);
    While (Task.Query.EOF=False) do begin
      sEntry:=Task.Query.Fields[0].AsString;
      Core.Arrays.VarString.Add(List,sEntry);
      Task.Query.Next;
    end;
    Result:=True;
  finally
    Task.Query.Close;
  end;

end;


Function GetColumns(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; Out List:Core.Arrays.Types.VarString):System.Boolean;
var
  sEntry:Core.Strings.VarString;
  sField:Core.Strings.VarString;
  sSQL:Core.Strings.VarString;
  iLcv:LongInt;
begin
  Result:=False;
  If not Task.Connection.Connected then exit;
  sSQL:=Format(Core.Database.Types.SQL_LIST_COLUMNS[Task.Header.Mode],[Task.Header.Schema,TableP^.Name]);
  Task.Query.SQL.Text:=sSQL;
  Task.Query.Prepare;

  Task.Query.Open;
  Try
    Core.Arrays.VarString.Empty(List);
    While (Task.Query.EOF=False) do begin
      sEntry:=Task.Query.Fields[0].AsString;
      Core.Arrays.VarString.Add(List,sEntry);
      Task.Query.Next;
    end;
    Result:=True;
  finally
    Task.Query.Close;
  end;

end;


procedure   AddTable(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable);
var
  ModeP:Core.Database.Types.PDriver;
  saTables:Core.Arrays.Types.VarString;

  procedure PushCreateTable;
  var
    sCreateTableSQL:Core.Strings.VarString;
    sFieldStatement:Core.Strings.VarString;
    sFieldData:Core.Strings.VarString;
    sFieldKind:Core.Strings.VarString;
    sFieldNotNull:Core.Strings.VarString;
    sFieldUnique:Core.Strings.VarString;
    sFieldKey:Core.Strings.VarString;
    sFieldID:Core.Strings.VarString;
    sPrimaryKey:Core.Strings.VarString;
    sFieldsCollection:Core.Strings.VarString;
    iLcv:LongInt;
  begin
    sCreateTableSQL:=Core.Database.Types.SQL_CREATE_TABLE_STATEMENT;
    sCreateTableSQL:=StringReplace(sCreateTableSQL,'$TABLE$',TableP^.Name,[rfReplaceAll]);
    sFieldsCollection:=#13#10;
    for iLcv:=0 to High(TableP^.Fields) do begin
      if TableP^.Fields[iLcv].AutoCreate then begin
        sFieldStatement:=Core.Database.Types.SQL_CREATE_TABLE_FIELD_STATEMENT;
        sFieldStatement:=StringReplace(sFieldStatement,'$NAME$',TableP^.Fields[iLcv].KeyP^,[rfReplaceAll]);
        {$i Core.Database.SQL.ResetFieldStatement.inc}
        {$i Core.Database.SQL.SetFieldKind.inc}
        {$i Core.Database.SQL.SetFieldContraints.inc}
        {$i Core.Database.SQL.SetFieldStatement.inc}
      end;
    end;
    System.SetLength(sFieldsCollection,Length(sFieldsCollection)-2);
    sCreateTableSQL:=StringReplace(sCreateTableSQL,'$FIELDS$',sFieldsCollection,[rfReplaceAll]);
    sCreateTableSQL:=StringReplace(sCreateTableSQL,'  ',' ',[rfReplaceAll]);

    if (Task.TransactionLock=false) then begin
      if Not Task.Transaction.Active then
        Task.Transaction.StartTransaction();
    end;
    Try
      Task.Query.SQL.Text:=sCreateTableSQL;
      Try
        Task.Query.Prepare;
        Try
          Try
            Task.Query.ExecSQL();
            if (Task.TransactionLock=false) then
              Task.Transaction.Commit();
          finally
            Task.Query.Close();
          end;
        Except
          On E:Exception do If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,Task.Name,Format('SQL_AddTable.PushCreateTable.ExecSQL(%s)',[TableP^.Name]),TableP^.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCreateTableSQL,E.Message]));
        end;
      Except
        On E:Exception do If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,Task.Name,Format('SQL_AddTable.PushCreateTable.Prepare(%s)',[TableP^.Name]),TableP^.Name,E.Message);
      end;
    finally
      if (Task.TransactionLock=false) then
        Task.Transaction.EndTransaction();
    end;
  end;

  procedure AddUnVerifiedFields;
  var
    sAlterTableSQL:Core.Strings.VarString;
    sFieldStatement:Core.Strings.VarString;
    sFieldKind:Core.Strings.VarString;
    sFieldNotNull:Core.Strings.VarString;
    sFieldUnique:Core.Strings.VarString;
    sFieldKey:Core.Strings.VarString;
    sFieldID:Core.Strings.VarString;
    sPrimaryKey:Core.Strings.VarString;
    sFieldsCollection:Core.Strings.VarString;
    iLcv:LongInt;
  begin
    sAlterTableSQL:=SQL_ALTER_TABLE_STATEMENT;
    sAlterTableSQL:=StringReplace(sAlterTableSQL,'$TABLE$',TableP^.Name,[rfReplaceAll]);
    SetLength(sFieldsCollection,0);
    for iLcv:=0 to High(TableP^.Fields) do begin
      if ( (not TableP^.Fields[iLcv].Verified) and TableP^.Fields[iLcv].AutoCreate ) then begin
        sFieldStatement:=SQL_ALTER_ADD_FIELD;
        sFieldStatement:=StringReplace(sFieldStatement,'$FIELD$',Core.Database.Types.SQL_CREATE_TABLE_FIELD_STATEMENT,[rfReplaceAll]);
        sFieldStatement:=StringReplace(sFieldStatement,'$NAME$',TableP^.Fields[iLcv].KeyP^,[rfReplaceAll]);
        {$i Core.Database.SQL.ResetFieldStatement.inc}
        {$i Core.Database.SQL.SetFieldKind.inc}
        {$i Core.Database.SQL.SetFieldContraints.inc}
        {$i Core.Database.SQL.SetFieldStatement.inc}
      end;
    end;
    if Length(sFieldsCollection)>2 then begin
      System.SetLength(sFieldsCollection,Length(sFieldsCollection)-2);
      sAlterTableSQL:=StringReplace(sAlterTableSQL,'$FIELDS$',sFieldsCollection,[rfReplaceAll]);
      sAlterTableSQL:=StringReplace(sAlterTableSQL,'  ',' ',[rfReplaceAll]);
      if (Task.TransactionLock=false) then begin
        if Task.Transaction.Active=false then
          Task.Transaction.StartTransaction();
      end;
      Try
        Task.Query.SQL.Text:=sAlterTableSQL;
        Try
          Task.Query.Prepare;
          Try
            Try
              Task.Query.ExecSQL;
              if (Task.TransactionLock=false) then
                Task.Transaction.Commit;
            Finally
              Task.Query.Close();
            end;
          Except
            On E:Exception do If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,TableP^.Name,Task.Name,Format('SQL_AddTable.AddUnVerifiedFields.ExecSQL(%s)',[TableP^.Name]),Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sAlterTableSQL,E.Message]));
          end;
        Except
          On E:Exception do If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,TableP^.Name,Task.Name,Format('SQL_AddTable.AddUnVerifiedFields.Prepare(%s)',[TableP^.Name]),E.Message);
        end;
      finally
        if (Task.TransactionLock=false) then
          Task.Transaction.EndTransaction();
      end;
    end;
  end;

  procedure PushVerifyTable;
  var
    saColumns:Core.Arrays.Types.VarString;
    iLcv:LongInt;
  begin
    Try
      if GetColumns(Task,TableP,saColumns) then begin
        for iLcv:=0 to High(TableP^.Fields) do
          TableP^.Fields[iLcv].Verified:=(Core.Arrays.VarString.IndexOf(saColumns,TableP^.Fields[iLcv].KeyP^)>-1);
        Try
          AddUnVerifiedFields;
        Except
          On E:Exception do If Assigned(Task.Header.OnException) then Task.Header.OnException(Task.Header.ModuleName,TableP^.Name,Task.Name,Format('SQL_AddTable.PushVerifyTable.SetSchemaInfo(%s)',[TableP^.Name]),E.Message);
        end;
      end;
    finally
      Done(saColumns);
    end;
  end;

begin
  ModeP:=@Task.Header.Mode;
  Try
    If GetTables(Task,saTables) then begin
      if Core.Arrays.VarString.IndexOf(saTables,TableP^.Name)<>-1 then begin
        PushVerifyTable;
      end else begin
        If TableP^.StartupP^.AutoCreate then
          PushCreateTable;
      end;
    end;
  Finally
    Done(saTables);
  end;
end;


Function   Delete(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
var
  iLcv                  : LongInt;
  iRetries              : LongInt;
  iLength               : LongInt;
  sTable                : Core.Strings.VarString;
  sCriteria             : Core.Strings.VarString;
  sCommand              : Core.Strings.VarString;
  Parameters            : Core.Database.Types.CommandParameters;
  TableP                : Core.Database.Types.PTable;

  function PushDelete:System.Boolean;
  var
    iLcv                : LongInt;
  begin
    Result:=false;

    Task.LastUsed:=Core.Timer.dtNow;

    if Task.Connection.Connected=false then
      Task.Connection.Connected:=True;

    if Task.Transaction.Active=false then
       Task.Transaction.StartTransaction;

    Task.Query.SQL.Text:=sCommand;

    For iLcv:=0 to Length(Parameters)-1 do
      BindParameter(iLcv,Parameters[iLcv],Task.Query,ptInput);
    Try
      Task.Query.Prepare;
      Task.Query.ExecSQL;
      if (Task.TransactionLock=false) then
        Task.Transaction.Commit;
      Result:=True;
    Except
       on E:Exception do begin
         Task.Query.Close;
         Case Task.Header.Mode of
           PostgreSQL : begin
             If System.Pos('no connection',E.Message)>0 then begin
               Task.Connection.Connected:=False;
               Inc(iRetries);
               If iRetries<3 then
                 Result:=PushDelete()
               else if Assigned(Task.Header.OnException) then
                 Task.Header.OnException(Task.Header.ModuleName,'SQL_Delete.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
             end else If Assigned(Task.Header.OnException) then begin
               Task.Header.OnException(Task.Header.ModuleName,'SQL_Delete.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
             end;
           end;
           MySQL51: begin
             If System.Pos('gone away',E.Message)>0 then begin
               Task.Connection.Connected:=False;
               Inc(iRetries);
               If iRetries<3 then
                 Result:=PushDelete()
               else if Assigned(Task.Header.OnException) then
                 Task.Header.OnException(Task.Header.ModuleName,'SQL_Delete.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
             end else If Assigned(Task.Header.OnException) then begin
               Task.Header.OnException(Task.Header.ModuleName,'SQL_Delete.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
             end;
           end;
         else
           If Assigned(Task.Header.OnException) then
             Task.Header.OnException(Task.Header.ModuleName,'SQL_Delete.Execute',sTable,Task.Name,Format(SQL_EXECUTE_EXCEPTION_MESSAGE,[sCommand,E.Message]));
         end;
       end;
    end;
    Task.Query.Close;
    if (Task.TransactionLock=false) then
      Task.Transaction.EndTransaction;
  end;

begin
  Result:=False; iRetries:=0;
  Try
    sCommand:=SQL_DELETE_STATEMENT;
    sCriteria:=''; sTable:=''; Core.Database.Empty(Parameters);
    TableP:=nil;
    For iLcv:=0 to Length(CommandsP^)-1 do begin
      Case CommandsP^[iLcv].Useage of
        useAsTable:PushUsage_Table(sTable,TableP,@CommandsP^[iLcv]);
        useForCriteria:PushUsage_Criteria(Task,'SQL_Delete',sTable,sCriteria,@CommandsP^[iLcv],Parameters);
        useForCriteriaBracket:PushUsage_Criteria_Brackets(Task,sCriteria,@CommandsP^[iLcv]);
      End;
    end;
    iLength:=Length(sTable);
    If (iLength>0) Then SetLength(sTable,iLength-1);
    sCommand:=SysUtils.StringReplace(sCommand,'$TABLE$',sTable,[]);
    sCommand:=SysUtils.StringReplace(sCommand,'$CRITERIA$',sCriteria,[]);

    Result:=PushDelete;
  Finally
    Done(Parameters);
  End;
end;


procedure BindParameter(Index:LongInt; var Parameter:Core.Database.Types.CommandParameter; Query:TSQLQuery; Kind:TParamType);
begin
  Case Parameter.DataType of
    dftBoolean      : BindParameter(Index,Parameter.DataType,PBool(Parameter.Value)^,Query,Kind);
    dftByte         : BindParameter(Index,Parameter.DataType,PByte(Parameter.Value)^,Query,Kind);
    dftWord         : BindParameter(Index,Parameter.DataType,PWord(Parameter.Value)^,Query,Kind);
    dftInteger      : BindParameter(Index,Parameter.DataType,PInteger(Parameter.Value)^,Query,Kind);
    dftInt64        : BindParameter(Index,Parameter.DataType,PInt64(Parameter.Value)^,Query,Kind);
    dftQword        : BindParameter(Index,Parameter.DataType,PQword(Parameter.Value)^,Query,Kind);
    dftInt64Array   : BindParameter(Index,Parameter.DataType,PLargeIntArray(Parameter.Value)^,Query,Kind);
    dftQWordArray   : BindParameter(Index,Parameter.DataType,PLargeWordArray(Parameter.Value)^,Query,Kind);
    dftDateTime     : BindParameter(Index,Parameter.DataType,PDouble(Parameter.Value)^,Query,Kind);
    dftFloat        : BindParameter(Index,Parameter.DataType,PFloat(Parameter.Value)^,Query,Kind);
    dftDouble       : BindParameter(Index,Parameter.DataType,PDouble(Parameter.Value)^,Query,Kind);
    dftMD5Digest    : BindParameter(Index,Parameter.DataType,PMD5Digest(Parameter.Value)^,Query,Kind);
    dftSmallString  : BindParameter(Index,Parameter.DataType,PSmallString(Parameter.Value)^,Query,Kind);
    dftString       : BindParameter(Index,Parameter.DataType,PVarString(Parameter.Value)^,Query,Kind);
    dftMemo         : BindParameter(Index,Parameter.DataType,PVarString(Parameter.Value)^,Query,Kind);
    dftByteBuffer   : BindParameter(Index,Parameter.DataType,PBuffer(Parameter.Value)^,Query,Kind);
  end;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Bool; Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftBoolean) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftBoolean]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsBoolean:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Byte; Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftByte) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftByte]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsSmallInt:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Word;  Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftWord) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftWord]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsInteger:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Integer;  Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftInteger) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftInteger]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsInteger:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeInt; Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftInt64) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftInt64]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsLargeInt:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWord; Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftQword) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftQword]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsLargeInt:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeIntArray; Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftInt64Array) then Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftInt64Array]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsString:=Core.Arrays.LargeInt.toString(Value,',');
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWordArray; Query:TSQLQuery; Kind:TParamType);
var
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftQWordArray) then Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftQWordArray]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsString:=Core.Arrays.LargeWord.toString(Value,',');
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Float;Query:TSQLQuery; Kind:TParamType);
var
  Param:TParam;
  sName:Core.Strings.VarString;
begin
  If (DataType<>dftFloat) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftFloat]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsFloat:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Double;  Query:TSQLQuery; Kind:TParamType);
var
  Param:TParam;
  sName:Core.Strings.VarString;
begin
  If not ((DataType=dftDouble) or (DataType=dftDateTime)) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftDouble]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsFloat:=Value;
end;


procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.MD5Digest; Query:TSQLQuery; Kind:TParamType);
var
  Param:TParam;
  sName:Core.Strings.VarString;
  iLength:LongInt;
begin
  If  (DataType<>dftMD5Digest) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftMD5Digest]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  iLength:=Length(Value);
  Param:=Query.ParamByName(sName);
  Param.AsString:=Encryption.Base64.Encode(Value);
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.VarString; Query:TSQLQuery; Kind:TParamType);
var
  Param:TParam;
  sName:Core.Strings.VarString;
begin
  If  Not (DataType in [dftSmallString,dftString,dftMemo]) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],Concat(DB_FIELD[dftString],' or ',DB_FIELD[dftMemo])]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  Query.Params.ParamByName(sName).AsString:=Value;
end;

procedure BindParameter(Index:LongInt; DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Buffer; Query:TSQLQuery; Kind:TParamType);
var
  Param:TParam;
  sName:Core.Strings.VarString;
  iLength:LongInt;
begin
  If  (DataType<>dftByteBuffer) then Raise Exception.Create(Format(FMT_INVALID_TYPE,[IntToStr(Index),DB_FIELD[DataType],DB_FIELD[dftByteBuffer]]));
  sName:=Format(FMT_PARAM_FIELD,[IntToStr(Index)]);
  iLength:=Length(Value);
  Param:=Query.ParamByName(sName);
  Param.AsString:=Encryption.Base64.Encode(Value);
end;


Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Bool; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsBoolean:=Value;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Byte; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter-1].SetData(@Value);
  Query.Params.Items[iParameter-1].Size:=1;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Word; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsInteger:=Value;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Integer; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsInteger:=Value;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeInt; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsLargeInt:=Value;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeWord; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsLargeInt:=Value;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeIntArray; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsString:=Core.Arrays.LargeInt.toString(Value,',');
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.LargeWordArray; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsString:=Core.Arrays.LargeWord.toString(Value,',');
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Float; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsFloat:=Value;
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.MD5Digest; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsString:=Encryption.Base64.Encode(Value);
end;

Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Double; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsFloat:=Value;
end;


Procedure Bind(Const iParameter:LongInt; Var Value:Core.Database.Types.Buffer; Query:TSQLQuery);
begin
  Query.Params.Items[iParameter].AsString:=Encryption.Base64.Encode(Value);
end;


Function   DataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Value:System.Int64):System.Boolean;
  {$i Core.Database.SQL.DataLength.Declarations.inc}
begin
  {$i Core.Database.SQL.DataLength.Method.inc}
end;

Function   DataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Value:System.Qword):System.Boolean;
  {$i Core.Database.SQL.DataLength.Declarations.inc}
begin
  {$i Core.Database.SQL.DataLength.Method.inc}
end;

Function   SumDataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Length:System.Int64):System.Boolean;
  {$i Core.Database.SQL.SumDataLength.Declarations.inc}
begin
  {$i Core.Database.SQL.SumDataLength.Method.inc}
end;

Function   SumDataLength(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; var Length:System.QWord):System.Boolean;
  {$i Core.Database.SQL.SumDataLength.Declarations.inc}
begin
  {$i Core.Database.SQL.SumDataLength.Method.inc}
end;

Function   Select(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Callback:Core.Database.Types.Callback; Const DataP:System.Pointer=Nil):System.Boolean;
  {$i Core.Database.SQL.Select.Declarations.inc}
  {$i Core.Database.SQL.Select.Declarations.Push_Select.inc}
begin
  {$i Core.Database.SQL.Select.Method.inc}
end;

Function   Select(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Callback:Core.Database.Types.Event; Const DataP:System.Pointer=Nil):System.Boolean;
  {$i Core.Database.SQL.Select.Declarations.inc}
  {$i Core.Database.SQL.Select.Declarations.Push_Select.inc}
begin
  {$i Core.Database.SQL.Select.Method.inc}
end;

Function   Update(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
  {$i Core.Database.SQL.Update.Declarations.inc}
  {$i Core.Database.SQL.Update.Declarations.PushUpdate.inc}
begin
  {$i Core.Database.SQL.Update.Method.inc}
end;

Function   Insert(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
  {$i Core.Database.SQL.Insert.Declarations.inc}
  {$i Core.Database.SQL.Insert.Declarations.PushInsert.inc}
  {$i Core.Database.SQL.Insert.Declarations.PushPrimaryReset.inc}
  {$i Core.Database.SQL.Insert.Declarations.PushSetupStatement.inc}
begin
  {$i Core.Database.SQL.Insert.Method.inc}
end;


Function   Count(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Output:System.QWord):System.Boolean;
  {$i Core.Database.SQL.Count.Declarations.inc}
begin
  {$i Core.Database.SQL.Count.Method.inc}
end;

Function   Count(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Output:System.Int64):System.Boolean;
  {$i Core.Database.SQL.Count.Declarations.inc}
begin
  {$i Core.Database.SQL.Count.Method.inc}
end;

Function   Sum(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands; Var Count:System.Int64):System.Boolean;
  {$i Core.Database.SQL.Sum.Declarations.inc}
  {$i Core.Database.SQL.Sum.Declarations.PushSum.inc}
begin

end;

Function   Sum(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands;  Var Count:System.QWord):System.Boolean;
  {$i Core.Database.SQL.Sum.Declarations.inc}
  {$i Core.Database.SQL.Sum.Declarations.PushSum.inc}
begin
  {$i Core.Database.SQL.Sum.Method.inc}
end;

Function   CreateView(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
  {$i Core.Database.SQL.CreateView.Declarations.inc}
begin
  {$i Core.Database.SQL.CreateView.Method.inc}
end;

Function   DropView(Task:Core.Database.Types.TTask; CommandsP:Core.Database.Types.PCommands):System.Boolean;
  {$i Core.Database.SQL.DropView.Declarations.inc}
begin
  {$i Core.Database.SQL.DropView.Method.inc}
end;

Function   Execute(Task:Core.Database.Types.TTask; var SQL:Core.Strings.VarString):System.Boolean;
begin
  {$i Core.Database.SQL.Execute.Method.inc}
end;

procedure  StartTransaction(Task:Core.Database.Types.TTask);
begin
  {$i Core.Database.SQL.StartTransaction.Method.inc}
end;

procedure  CommitTransaction(Task:Core.Database.Types.TTask);
begin
  {$i Core.Database.SQL.CommitTransaction.Method.inc}
end;

end.


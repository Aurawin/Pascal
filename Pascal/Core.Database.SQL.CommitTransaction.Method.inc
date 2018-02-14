Task.TransactionLock:=False;
if Task.Transaction.Active then begin
  Task.Transaction.Commit();
  Task.Transaction.EndTransaction();
  Task.Transaction.Active:=false;
end;


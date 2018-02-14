Task.TransactionLock:=True;
if Task.Transaction.Active then begin
  Task.Transaction.Commit();
  Task.Transaction.EndTransaction();
end;
Task.Transaction.StartTransaction();


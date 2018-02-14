Result:=False;
// Check
if not Task.Connection.Connected then
  Task.Connection.Connected:=true;
iAutoNumberCount:=0; bError:=False;
For iLcv:=0 to High(TableP^.Fields) do begin
  If TableP^.Fields[iLcv].Flags or cfIdentity= TableP^.Fields[iLcv].Flags then begin
    If iAutoNumberCount<>0 then begin
      bError:=True;
      If Assigned(Task.Header.OnException) then begin
        Task.Header.OnException(
          Task.Header.ModuleName,
          Format('AddTable(%s)',[TableP^.Name]),
          TableP^.Name,
          Task.Name,
          Concat('Field ',TableP^.Fields[iLcv].KeyP^,' is a duplicate Autonumber field.')
        );
      end;
    end;
    Inc(iAutoNumberCount);
  end;
end;
iPrimaryIndex:=-1;
if TableP^.StartupP^.PrimaryKeyP<>nil then
  iPrimaryIndex:=Core.Database.IndexOf(TableP^.StartupP^.PrimaryKeyP^,@TableP^.Fields);
If iPrimaryIndex<>-1 then
  TableP^.Fields[iPrimaryIndex].Flags:=TableP^.Fields[iPrimaryIndex].Flags or cfIdentity or cfNotNull or cfPrimaryKey;

Core.Database.SQL.AddTable(Task,TableP);
Result:=True;


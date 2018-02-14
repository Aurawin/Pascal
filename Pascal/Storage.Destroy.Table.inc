Core.Database.Done(TableP^);
Dispose(TableP);
TableP := nil;
Core.Database.Monitor.Remove(MonitorP);
Core.Database.Monitor.Done(MonitorP^);
Dispose(MonitorP);
MonitorP := nil;


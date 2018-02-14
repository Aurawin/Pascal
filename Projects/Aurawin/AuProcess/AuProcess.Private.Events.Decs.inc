procedure OnDBMSConnected(Task:Core.Database.Types.TTask);
procedure OnDBMSDisconnected(Task:Core.Database.Types.TTask);
procedure OnDBMSException(sProcedure,sLocation,sTable,sTask,sError:Core.Strings.VarString);
procedure OnServerListening(Server:TRSRServer);
procedure OnServerEngineFailure(Server:TRSRServer);


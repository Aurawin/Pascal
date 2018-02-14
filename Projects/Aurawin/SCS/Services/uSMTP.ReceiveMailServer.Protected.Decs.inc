
procedure   OnTimer_FillAntiSpamData(TimerP:Core.Timer.PItem);
procedure   OnTimer_FillRootUserData(TimerP:Core.Timer.PItem);

procedure   OnException(sProcedure,sLocation,sError:Core.Strings.VarString); override;
procedure   OnError(sProcedure,sLocation,sError:Core.Strings.VarString); override;


FStoreFlags:=0;
if Core.Arrays.VarString.Search(FCommands,'+flags.silent')>0 then
  FStoreFlagMode:=isfPlusSilent
else if Core.Arrays.VarString.Search(FCommands,'-flags.silent')>0 then
  FStoreFlagMode:=isfMinusSilent
else if Core.Arrays.VarString.Search(FCommands,'flags.silent')>0 then
  FStoreFlagMode:=isfAllSilent
else if Core.Arrays.VarString.Search(FCommands,'+flags')>0 then
  FStoreFlagMode:=isfPlus
else if Core.Arrays.VarString.Search(FCommands,'-flags')>0 then
  FStoreFlagMode:=isfMinus
else if Core.Arrays.VarString.Search(FCommands,'flags')>0 then
  FStoreFlagMode:=isfAll;
FFlagIndex:=Core.Arrays.VarString.Search(FCommands,'\');
if FFlagIndex>0 then
  FStoreFlags:=RSR.IMAP.GetFlags(FCommands[FFlagIndex]);

Case FStoreFlagMode of
  isfAll         : begin
                    {$i uIMAPManager.Process.Store.SetFlags.All.inc}
                   end;
  isfAllSilent   : begin
                     {$i uIMAPManager.Process.Store.SetFlags.AllSlient.inc}
                   end;
  isfPlus        : begin
                     {$i uIMAPManager.Process.Store.SetFlags.Plus.inc}
                   end;
  isfPlusSilent  : begin
                     {$i uIMAPManager.Process.Store.SetFlags.PlusSilent.inc}
                   end;
  isfMinus       : begin
                     {$i uIMAPManager.Process.Store.SetFlags.Minus.inc}
                   end;
  isfMinusSilent : begin
                     {$i uIMAPManager.Process.Store.SetFlags.MinusSilent.inc}
                   end;
  else begin
    {$i uIMAPManager.Process.Store.SetFlags.None.inc}
  end;
end;


procedure  AuraProcess.LoadSearchProviders;
var
  iCt,iManLcv,iLcv:integer;
begin
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(FDomain.Name,'AuraProcess.LoadSearchProviders','Loading');
  {$endif}
  Storage.SrchProviders.Items.DB.Fill(Storage.Main.Task,Storage.SrchProviders.List);
  iCt:=System.Length(Storage.SrchProviders.List);
  for iLcv:=0 to iCt-1 do begin
    SetLength(Storage.SrchProviders.List[iLcv].Managers,Storage.SrchProviders.List[iLcv].Scale);
    for iManLcv:=0 to High(Storage.SrchProviders.List[iLcv].Managers) do
      Storage.SrchProviders.List[iLcv].Managers[iManLcv]:=uProviders.TProviderManager.Create(Storage.Main.Header,@Storage.SrchProviders.List[iLcv]);
  end;
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(FDomain.Name,'AuraProcess.LoadSearchProviders',Concat('Loaded (',IntToStr(ict),') providers'));
  {$endif}
end;

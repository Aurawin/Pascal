if GUI.Locked or (lvProviderService.Selected=nil) or (FSearchProviderP=nil) then exit;

frmProvider.ProviderForm.Show(lvProviderService.Selected.Caption,FSearchProviderP);

FDomainServiceP:=lvDomainServices.Selected.Data;
FDomainServiceP^.Enabled:=cbDomain_Service.Checked;
lvDomainServices.Selected.SubItems[0]:=Yes_No[FDomainServiceP^.Enabled];
Storage.MatrixServices.Items.DB.SetEnabled(Storage.Main.Task,FDomainServiceP^);
Storage.MatrixServices.Items.DB.SetEnabled(Storage.Main.Task,FDomainServiceP^.DomainID,FDomainServiceP^.Kind);


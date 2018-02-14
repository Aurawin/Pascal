FDomainServiceP:=lvDomainServices.Selected.Data;
FDomainServiceP^.Scale:=tbDomainScale.Position;
lvDomainServices.Selected.SubItems[1]:=IntToStr(FDomainServiceP^.Scale);
Storage.MatrixServices.Items.DB.SetScale(Storage.Main.Task,FDomainServiceP^);
Storage.MatrixServices.Items.DB.SetScale(Storage.Main.Task,FDomainP^.ID,FDomainServiceP^);


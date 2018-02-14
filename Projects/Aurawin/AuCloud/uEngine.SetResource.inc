procedure   TAuSocketMan.SetResource(var Item:Storage.VDM.Resources.TResource);
begin
  Storage.VDM.Resources.Copy(Item,FResource);
end;

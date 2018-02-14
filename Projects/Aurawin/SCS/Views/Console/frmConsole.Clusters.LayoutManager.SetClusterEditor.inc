if (tvClusterResources.Selected=nil) or (tvClusterResources.Selected.Data=nil) then Exit;
FClusterResourceP:=tvClusterResources.Selected.Data;
if FClusterResourceP<>nil then begin
  txtCRNResource.Text:=FClusterResourceP^.Name;
  gbCRNResource.Visible:=True;
  txtCRNResource.SetFocus;
end;

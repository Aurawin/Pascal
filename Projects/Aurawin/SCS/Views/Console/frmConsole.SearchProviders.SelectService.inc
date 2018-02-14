If GUI.Locked or (Item=nil) or (FSearchProviderP=nil) then exit;
GUI.Lock;
Try
  txtProviderQueryString.Text:=Storage.SrchProviders.Items.GetQueryString(Item.Caption,FSearchProviderP^);
Finally
  GUI.UnLock;
end;

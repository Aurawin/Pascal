for iLcv:=0 to High(FPurchases) do begin
  li:=lvPurchase.Items.Add();
  li.Caption:=IntToStr(FPurchases[iLcv]^.ID);
  li.Data:=FPurchases[iLcv];
  li.SubItems.Add(Yes_No[FPurchases[iLcv]^.Enabled]);
  li.SubItems.Add(Yes_No[FPurchases[iLcv]^.Taxable]);
  li.SubItems.Add(Purchase.Kind.List[FPurchases[iLcv]^.Kind]);
  li.SubItems.Add(Purchase.Item.getPrice(FPurchases[iLcv]^));
  li.SubItems.Add(FPurchases[iLcv]^.Title);
  li.SubItems.Add(FPurchases[iLcv]^.Description);
end;
FPurchaseP:=nil;
LoadPurchase();


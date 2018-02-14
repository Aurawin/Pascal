GUI.Lock;
Try
  if (Item<>nil) and Selected then begin
    FSearchProviderP:=Storage.SrchProviders.Items.Find(QWord(Item.Data),FSearchProviders);
    if FSearchProviderP<>nil then begin
      txtProviderName.Text:=FSearchProviderP^.Caption;
      txtProviderDomain.Text:=FSearchProviderP^.Domain;
      txtProviderPort.Text:=IntToStr(FSearchProviderP^.Port);
      tbSearchScale.Position:=FSearchProviderP^.Scale;
      txtProviderMaxResults.Text:=IntToStr(FSearchProviderP^.MaxResults);
      if (lvProviderService.Selected=nil) then begin
        lvProviderService.Items[0].Focused:=true;
        lvProviderService.Selected:=lvProviderService.Items[0];
        lvProviderService.Items[0].Selected:=true;
      end;
      txtProviderQueryString.Text:=Storage.SrchProviders.Items.GetQueryString(lvPRoviderService.Selected.Caption,FSearchProviderP^);
    end;
  end;
finally
  GUI.UnLock;
end;


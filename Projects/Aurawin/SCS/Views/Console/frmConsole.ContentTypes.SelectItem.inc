  if FContentTypeP=nil then begin
    GUI.Lock;
    Try
      txtContentTypeExtension.Clear;
      txtContentTypeKind.Clear;
    finally
      GUI.UnLock;
    end;
  end else begin
    GUI.Lock;
    Try
      txtContentTypeKind.Text:=FContentTypeP^.Kind;
      txtContentTypeExtension.Text:=FContentTypeP^.Ext;
    Finally
      GUI.UnLock;
    end;
  end;

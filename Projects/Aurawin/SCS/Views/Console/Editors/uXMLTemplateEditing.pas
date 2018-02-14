unit uXMLTemplateEditing;

interface
uses SysUtils,ccUtils,Classes,uItemScrollBox,uStorage,OCL,HSRConsts;

Type
  TXMLEditItem=Record
    XML_CFG                           : TCFG_XML;
    GI_DATA                           : TGroupItem;
  end;
  TXMLEditItems=Array of TXMLEditItem;

  Function  Load_XML(ISB:TItemScrollBox; Var Items:TXMLEditItems; DomainID:Int64):Boolean;
  Function  Add_XML(Var Item:TXMLEditItem):Boolean;
  Function  Delete_XML(Var Item:TXMLEditItem):Boolean;
  Function  Save_XML(ISB:TItemScrollBox; Var Items:TXMLEditItems):Boolean;
  Function  Exists_XML(DomainID:Int64; Name:String):Boolean;

implementation

Function  Load_XML(ISB:TItemScrollBox; Var Items:TXMLEditItems; DomainID:Int64):Boolean;
var
  Prop:TProperty;
  GP:TGroup;
  iLcv:Integer;
  List:Core.Arrays.VarString;
begin
  Result:=False;
  ISB.Caption:='Loading...';
  ISB.Clear;
  Try
    if uStorage.CFG_XML_List(DomainID,List) then begin
      SetLength(Items,Length(List));
      Prop.Name:='XML Source';
      Prop.Style:=psMemo;
      Prop.MaskValue:=False;
      for iLcv := 0 to High(Items) do begin
        Items[iLcv].XML_CFG.DomainID:=DomainID;
        StringToVarString(List[iLcv],Items[iLcv].XML_CFG.Name);
        If uStorage.CFG_XML_Get(Items[iLcv].XML_CFG) then begin
          GP:=ISB.AddGroup(List[iLcv]);
          Prop.Value:=ccUtils.VarStringToString(Items[iLcv].XML_CFG.Value);
          Items[iLcv].GI_DATA:=GP.AddPropertyItem(-1,Prop);
          Items[iLcv].GI_DATA.Tag:=iLcv;
          Result:=True;
        end;
      end;
    end;
  finally
    SetLength(List,0);
  end;
end;

Function  Add_XML(Var Item:TXMLEditItem):Boolean;
begin
  Result:=uStorage.CFG_XML_Add(Item.XML_CFG);
end;

Function  Delete_XML(Var Item:TXMLEditItem):Boolean;
begin
  Result:=uStorage.CFG_XML_Delete(Item.XML_CFG);
end;

Function  Save_XML(ISB:TItemScrollBox; Var Items:TXMLEditItems):Boolean;
var
  iLcv:Integer;
  GI:TGroupItem;
begin
  ISB.Caption:='Saving...';
  for iLcv := 0 to High(Items) do begin
    if GI<>Nil then begin
      StringToVarString(Items[iLcv].GI_DATA.Properties.Value,Items[iLcv].XML_CFG.Value);
      Result:=uStorage.CFG_XML_Set(Items[iLcv].XML_CFG);
    end;
  end;
end;

Function  Exists_XML(DomainID:Int64; Name:String):Boolean;
var
  vsXMLName:Core.Strings.VarString;
begin
  ccUtils.StringToVarString(Name,vsXMLName);
  Try
    Result:=uStorage.CFG_XML_Exists(DomainID,vsXMLName);
  Finally
    SetLength(vsXMLName,0);
  End;
end;

end.

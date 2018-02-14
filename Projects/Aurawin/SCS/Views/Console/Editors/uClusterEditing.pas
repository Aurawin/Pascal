unit uClusterEditing;

interface
  uses SysUtils,ccUtils,Classes,uItemScrollBox,uStorage,OCL,HSRConsts;

Type
  TClusterEditItem=record
    Cluster                            : TMatrixCluster;
    GI_Country                         : TGroupItem;
    GI_Region                          : TGroupItem;
    GI_Locality                        : TGroupItem;
    GI_Area                            : TGroupItem;
    GI_Street                          : TGroupItem;
    GI_Building                        : TGroupItem;
    GI_Floor                           : TGroupItem;
    GI_Room                            : TGroupItem;
    GI_ZIP                             : TGroupItem;
    GI_Description                     : TGroupItem;
  end;

  Function   Initialize_Cluster(Var Item:TClusterEditItem):Boolean;overload;
  Function   Initialize_ClusterWindow(ISB:TItemScrollBox; Var Item:TClusterEditItem):Boolean;
  Function   Save_Cluster(Var Item:TClusterEditItem):Boolean;
  Function   Add_Cluster(Var Item:TClusterEditItem):Boolean;
  Function   Delete_Cluster(Var Item:TClusterEditItem):Boolean;
var
  Cluster_Edit     : TClusterEditItem;
  
implementation

Function   Delete_Cluster(Var Item:TClusterEditItem):Boolean;
begin
  Result:=uStorage.Cluster_Delete(Item.Cluster.ID);
  if Result then
    Result:=uStorage.MatrixNode_Delete(Item.Cluster.ID);
end;

Function   Add_Cluster(Var Item:TClusterEditItem):Boolean;
begin
   Result:=uStorage.Cluster_Create(Item.Cluster);
end;

Function   Initialize_Cluster(Var Item:TClusterEditItem):Boolean;
begin
   Result:=uStorage.Cluster_Fill(Item.Cluster);
end;

Function   Initialize_ClusterWindow(ISB:TItemScrollBox; Var Item:TClusterEditItem):Boolean;
Var
  GP:TGroup;
  Prop:TProperty;
begin
  ISB.Clear;
  GP:=ISB.AddGroup(VarStringToString(Item.Cluster.Group));
  With Item do begin
    Prop.Style:=psString;
    Prop.MaskValue:=False;
    Prop.ItemCount:=0;
    Prop.Name:='Country';
    Prop.Value:=VarStringToString(Cluster.Location.Country);
    GI_Country:=GP.AddPropertyItem(-1,Prop);
    GI_Country.Hint:='The country where this cluster resides.';
    Prop.Name:='Region';
    Prop.Value:=VarStringToString(Cluster.Location.Region);
    GI_Region:=GP.AddPropertyItem(-1,Prop);
    GI_Region.Hint:='The region or state where this cluster resides.';
    Prop.Name:='Locality';
    Prop.Value:=VarStringToString(Cluster.Location.Locality);
    GI_Locality:=GP.AddPropertyItem(-1,Prop);
    GI_Locality.Hint:='The town where this cluster resides.';
    Prop.Name:='Area';
    Prop.Value:=VarStringToString(Cluster.Location.Area);
    GI_Area:=GP.AddPropertyItem(-1,Prop);
    GI_Area.Hint:='The area where this cluster resides.';
    Prop.Name:='Street';
    Prop.Value:=VarStringToString(Cluster.Location.Street);
    GI_Street:=GP.AddPropertyItem(-1,Prop);
    GI_Street.Hint:='The street where this cluster resides.';
    Prop.Name:='Building';
    Prop.Value:=VarStringToString(Cluster.Location.Building);
    GI_Building:=GP.AddPropertyItem(-1,Prop);
    GI_Building.Hint:='The building where this cluster resides.';
    Prop.Name:='Floor';
    Prop.Value:=VarStringToString(Cluster.Location.Floor);
    GI_Floor:=GP.AddPropertyItem(-1,Prop);
    GI_Floor.Hint:='The floor where this cluster resides.';
    Prop.Name:='Room';
    Prop.Value:=VarStringToString(Cluster.Location.Room);
    GI_Room:=GP.AddPropertyItem(-1,Prop);
    GI_Room.Hint:='The room where this cluster resides.';
    Prop.Name:='Postal Code';
    Prop.Value:=VarStringToString(Cluster.Location.Zip);
    GI_ZIP:=GP.AddPropertyItem(-1,Prop);
    GI_ZIP.Hint:='The postal code where this cluster resides.';
    Prop.Name:='Description';
    Prop.Value:=VarStringToString(Cluster.Location.Description);
    GI_Description:=GP.AddPropertyItem(-1,Prop);
    GI_Description.Hint:='A description of this cluster.';
  end;

end;

Function   Save_Cluster(Var Item:TClusterEditItem):Boolean;
begin
  With Item do begin
    StringToVarString(GI_Country.Properties.Value,Cluster.Location.Country);
    StringToVarString(GI_Region.Properties.Value,Cluster.Location.Region);
    StringToVarString(GI_Locality.Properties.Value,Cluster.Location.Locality);
    StringToVarString(GI_Area.Properties.Value,Cluster.Location.Area);
    StringToVarString(GI_Street.Properties.Value,Cluster.Location.Street);
    StringToVarString(GI_Building.Properties.Value,Cluster.Location.Building);
    StringToVarString(GI_Floor.Properties.Value,Cluster.Location.Floor);
    StringToVarString(GI_Room.Properties.Value,Cluster.Location.Room);
    StringToVarString(GI_ZIP.Properties.Value,Cluster.Location.Zip);
    StringToVarString(GI_Description.Properties.Value,Cluster.Location.Description);
  end;
  Result:=uStorage.Cluster_Save(Item.Cluster);
end;


end.

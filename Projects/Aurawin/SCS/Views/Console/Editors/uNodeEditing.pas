unit uNodeEditing;

interface
  uses ccWinsock,SysUtils,ccUtils,Classes,uItemScrollBox,uStorage,OCL,HSRConsts;

{
    Matrix Nodes are Physical Boxes that run the service
Each node can handle X number of services across Y number of IPs for Domains
installed in the matrix.

    The Node ID is the basis for all data stored that can return statistics
    or all the IPs for that node.
}

Type
  TNodeEditItem=Record
    Node                               : TMatrixNode;

    GP_Main                            : TGroup;
    GP_STATS                           : TGroup;

    GI_Alias                           : TGroupItem;
    GI_IP                              : TGroupItem;
    GI_IPS                             : TGroupItem;


    GI_Sessions                        : TGroupItem;
    GI_Streams                         : TGroupItem;
    GI_Filtered                        : TGroupItem;
    GI_Transactions                    : TGroupItem;
    GI_PTX_Sent                        : TGroupItem;
    GI_PTX_Received                    : TGroupItem;
    GI_MB_Sent                         : TGroupItem;
    GI_MB_Recvd                        : TGroupItem;
    GI_Received                        : TGroupItem;
    GI_Mem_Total                       : TGroupItem;
    GI_Mem_Free                        : TGroupItem;
    GI_CPU_Usage                       : TGroupItem;
  end;

  Function Initialize_Node(ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
  Function Initialize_Node_Window(ISB:TItemScrollBox; Var Item:TNodeEditItem):Boolean;
  Function Add_Node(ClusterID:Cardinal; var Item:TNodeEditItem):Boolean;
  Function Delete_Node(ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
  Function Save_Node(ISB:TItemScrollbox; ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
var
  Node_Edit        : TNodeEditItem;

implementation

Function Initialize_Node(ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
begin
  Item.Node.ID:=0;
  Item.Node.ClusterID:=ClusterID;
  Result:=MatrixNode_Fill(Item.Node);
  Result:=Result and (Item.Node.ID<>0);
end;

Function Initialize_Node_Window(ISB:TItemScrollBox; Var Item:TNodeEditItem):Boolean;
var
  Prop:TProperty;
  iLcv:Integer;
begin
  Result:=False;
  ISB.Caption:='Loading...';
  ISB.Clear;
  Item.GP_Main:=ISB.AddGroup('Main');
  Item.GP_Stats:=ISB.AddGroup('Statistics');

  Prop.Name:='Alias';
  Prop.Value:=VarStringToString(Item.Node.Alias);
  Prop.Style:=psString;
  Prop.MaskValue:=False;
  Item.GI_Alias:=Item.GP_Main.AddPropertyItem(-1,Prop);

  Prop.Name:='Root IP';
  Prop.Value:=ccWinsock.InAddrToStr(Item.Node.IP);
  Prop.Style:=psString;
  Prop.MaskValue:=False;
  Item.GI_IP:=Item.GP_Main.AddPropertyItem(-1,Prop);

  Prop.Name:='List';
  Prop.Value:=VarStringToString(Item.Node.IPList);
  Prop.Style:=psMemo;
  Prop.ItemCount:=0;
  Prop.MaskValue:=False;
  Item.GI_IPS:=Item.GP_Main.AddPropertyItem(-1,Prop);


  Prop.Style:=psString;
  Prop.ItemCount:=0;
  Prop.MaskValue:=False;
  Prop.Name:='Streams';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.Streams);
  Item.GI_Streams:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_Streams.Hint:='Number of concurrent streams.';

  Prop.Name:='Sessions';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.Sessions);
  Item.GI_Sessions:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_Sessions.Hint:='Number of sessions.';

  Prop.Name:='Transactions';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.Transactions);
  Item.GI_Transactions:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_Transactions.Hint:='Number of transactions.';

  Prop.Name:='Filtered';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.Filtered);
  Item.GI_Filtered:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_Filtered.Hint:='Number of filtered transactions.';

  Prop.Name:='Partial Sent Transactions';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.PTX_Sent);
  Item.GI_PTX_Sent:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_PTX_Sent.Hint:='Number of partial sent transactions.';

  Prop.Name:='Partial Recv Transactions';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.PTX_Received);
  Item.GI_PTX_Received:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_PTX_Received.Hint:='Number of partial received transactions.';

  Prop.Name:='Megabytes Sent';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.MB_Sent);
  Item.GI_MB_Sent:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_MB_Sent.Hint:='Number of megabytes sent.';

  Prop.Name:='Megabytes Received';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.MB_Received);
  Item.GI_MB_Recvd:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_MB_Recvd.Hint:='Number of megabytes received.';

  Prop.Name:='Total Memory';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.MEM_Total);
  Item.GI_Mem_Total:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_Mem_Total.Hint:='Number of megabytes of RAM.';

  Prop.Name:='Free Memory';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.MEM_Free);
  Item.GI_MB_Recvd:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_MB_Recvd.Hint:='Number of megabytes of Free RAM.';

  Prop.Name:='CPU Utilization';
  Prop.Value:=FormatFloat(RSR_STATS_FORMAT,Item.Node.Stats.CPU_Usage);
  Item.GI_MB_Recvd:=Item.GP_STATS.AddPropertyItem(-1,Prop);
  Item.GI_MB_Recvd.Hint:='Percentage of CPU utilized on this node.';

  Result:=True;
end;

Function Add_Node(ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
begin
  Item.Node.ClusterID:=ClusterID;
  Result:=uStorage.MatrixNode_Create(Item.Node);
end;

Function Delete_Node(ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
begin
  Item.Node.ClusterID:=ClusterID;
  Result:=uStorage.MatrixNode_Delete(Item.Node);
end;

Function Save_Node(ISB:TItemScrollbox; ClusterID:Cardinal; Var Item:TNodeEditItem):Boolean;
begin
  Item.Node.ClusterID:=ClusterID;
  StringToVarString(Item.GI_Alias.Properties.Value,Item.Node.Alias);
  Item.Node.IP:=ccWinsock.inet_addr(PChar(Item.GI_IP.Properties.Value));
  StringToVarString(Item.GI_IPS.Properties.Value,Item.Node.IPList);
  Result:=uStorage.MatrixNode_Set(Item.Node);
end;

end.

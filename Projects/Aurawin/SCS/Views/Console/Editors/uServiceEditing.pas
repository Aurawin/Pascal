unit uServiceEditing;

interface
  uses SysUtils,ccUtils,Classes,ccWinsock,uItemScrollBox,uStorage,OCL,HSRConsts,
       uClusterEditing,uNodeEditing,uDomainEditing;

Type

  TServiceEditGUIItems=Record
    GI_IP                              : TGroupItem;
    GI_PORT                            : TGroupItem;
    GI_Enabled                         : TGroupItem;
  end;
  TServiceEditGUIList=Array of TServiceEditGUIItems;

  TServiceEditItem=record
    DomainID                           : Int64;
    Services                           : TMatrixServices;

    GP_POP3                            : TGroup;
    GP_SMTP                            : TGroup;
    GP_XMPPC2S                         : TGroup;
    GP_XMPPS2S                         : TGroup;
    GP_HTTP                            : TGroup;
    GP_HTTPS                           : TGroup;
    GP_LDAP                            : TGroup;
    GP_DNS                             : TGroup;
    GP_IMAP                            : TGroup;
    GP_BMAIL                           : TGroup;

    GUI_Items                          : TServiceEditGUIList;
  end;

  Function Load_Domain(ISB:TItemScrollBox; Var Item:TServiceEditItem):Boolean;
  Function Save_Domain(ISB:TItemScrollBox; Var Item:TServiceEditItem):Boolean;
  Function Delete_Service(Var Item:TServiceEditItem; Selection:TGroupItem):Boolean;
  Function Add_Service(Var Item:TServiceEditItem; Kind:Int64; IP:String; Port:Word; Scale:Word):Boolean;
  Function Initialize(Var Item:TServiceEditItem):Boolean; overload;
  Function IndexOf(GI:TGroupItem; Var Item:TServiceEditItem):Integer; overload;
var
  Service_Edit     : TServiceEditItem;

implementation

Function Initialize(Var Item:TServiceEditItem):Boolean;
begin
  Copy(uClusterEditing.Cluster_Edit.Cluster,Item.Services.Cluster);
  Copy(uNodeEditing.Node_Edit.Node,Item.Services.Node);
  Result:=uStorage.MatrixServices_Fill(Item.Services,Item.DomainID);
end;

Function IndexOf(GI:TGroupItem; Var Item:TServiceEditItem):Integer; overload;
var
  iServiceCount,iServiceLcv:Integer;
begin
  iServiceLcv:=0; iServiceCount:=Length(Item.Services.List); Result:=-1;
  While (iServiceLcv<iServiceCount) and (Result=-1) do begin
    If Item.Services.List[iServiceLcv].ID=GI.Tag then
      Result:=iServiceLcv;
    Inc(iServiceLcv);
  end;

end;

Function Load_Domain(ISB:TItemScrollBox; Var Item:TServiceEditItem):Boolean;
var
  iLen,iLcv:Integer;

  Group:TGroup;

  pIP:TProperty;
  pPort:TProperty;
  pState:TProperty;

  procedure PushAddIP;
  begin
    pIP.Value:=ccWinsock.InAddrToStr(Item.Services.List[iLcv].IP);
    pIP.Name:='IP Address';
    pIP.Caption:='IP Address';
    Item.GUI_Items[iLen].GI_IP:=Group.AddPropertyItem(-1,pIP);
    Item.GUI_Items[iLen].GI_IP.Hint:='IP Address of service.';
    Item.GUI_Items[iLen].GI_IP.Tag:=Item.Services.List[iLcv].ID;
  end;

  procedure PushAddPort;
  begin
    pPort.Value:=IntToStr(Item.Services.List[iLcv].Port);
    pPort.Name:='Port';
    pPort.Caption:='Port';
    Item.GUI_Items[iLen].GI_PORT:=Group.AddPropertyItem(-1,pPort);
    Item.GUI_Items[iLen].GI_PORT.Hint:='Port to initiate service.';
    Item.GUI_Items[iLen].GI_PORT.Tag:=Item.Services.List[iLcv].ID;
  end;

  procedure PushAddEnabled;
  begin
    pState.Value:=uItemScrollBox.YesNo[Item.Services.List[iLcv].State or msEnabled=Item.Services.List[iLcv].State];
    pState.Name:='Enabled';
    pState.Caption:='Enabled';
    Item.GUI_Items[iLen].GI_Enabled:=Group.AddPropertyItem(-1,pState);
    Item.GUI_Items[iLen].GI_Enabled.Hint:='Click here to enable/disable this service.';
    Item.GUI_Items[iLen].GI_Enabled.Tag:=Item.Services.List[iLcv].ID;
  end;

begin
  ISB.Caption:='Loading Services...';

  pIP.Style:=psString;
  pIP.MaskValue:=False;
  pState.Style:=psYesNo;
  pState.MaskValue:=False;
  pPort.Style:=psString;
  pPort.MaskValue:=False;

  Item.GP_IMAP:=ISB.AddGroup('IMAP Mail');
  Item.GP_POP3:=ISB.AddGroup('POP3 Mail');
  Item.GP_SMTP:=ISB.AddGroup('SMTP Mail');
  Item.GP_BMAIL:=ISB.AddGroup('Broadcast Mail');
  Item.GP_HTTP:=ISB.AddGroup('HTTP Web');
  Item.GP_HTTPS:=ISB.AddGroup('HTTPS Secure Web');
  Item.GP_LDAP:=ISB.AddGroup('LDAP');
  Item.GP_DNS:=ISB.AddGroup('DNS');
  Item.GP_XMPPC2S:=ISB.AddGroup('XMPP Client To Server');
  Item.GP_XMPPS2S:=ISB.AddGroup('XMPP Server To Server');
  Empty(Item.Services.List);
  Item.DomainID:=Domain_Edit.Domain.ID;
  Copy(Cluster_Edit.Cluster,Item.Services.Cluster);
  Copy(Node_Edit.Node,Item.Services.Node);

  Result:=uStorage.MatrixServices_Fill(Item.Services);
  if Result then begin
    iLen:=0;
    for iLcv:= 0 to High(Item.Services.List) do begin
      if Item.Services.List[iLcv].DomainID=Item.DomainID then begin
        Group:=Nil;
        case Item.Services.List[iLcv].Kind of
          mkPOP3     : Group:=Item.GP_POP3;
          mkSMTP     : Group:=Item.GP_SMTP;
          mkXMPPCToS : Group:=Item.GP_XMPPC2S;
          mkXMPPSToS : Group:=Item.GP_XMPPS2S;
          mkHTTP     : Group:=Item.GP_HTTP;
          mkHTTPS    : Group:=Item.GP_HTTPS;
          mkLDAP     : Group:=Item.GP_DNS;
          mkDNS      : Group:=Item.GP_DNS;
          mkIMAP     : Group:=Item.GP_IMAP;
          mkBMAIL    : Group:=Item.GP_BMAIL;
        end;
        if Group<>Nil then begin
          SetLength(Item.GUI_Items,iLen+1);
          Try
            PushAddIP;
            PushAddPort;
            PushAddEnabled;
          Finally
            Inc(iLen);
          end;
        end;
      end;
    end;
  end;
end;


Function Save_Domain(ISB:TItemScrollBox; Var Item:TServiceEditItem):Boolean;
var
  iIndex,iLcv:Integer;
begin
  iLcv:=0; Result:=True;
  ISB.Caption:='Saving Services...';
  while (iLcv<Length(Item.GUI_Items)) and Result do begin
    // Save All Items...
    iIndex:=IndexOfServiceID(Item.Services,Item.GUI_Items[iLcv].GI_IP.Tag);
    Item.Services.List[iIndex].IP:=ccWinsock.inet_addr(PChar(Item.GUI_Items[iLcv].GI_IP.Properties.Value));
    Item.Services.List[iIndex].Port:=StrToIntDef(Item.GUI_Items[iLcv].GI_PORT.Properties.Value,0);
    if Item.GUI_Items[iLcv].GI_ENABLED.Properties.Value=YESNO[True] then
      Item.Services.List[iIndex].State:=Item.Services.List[iIndex].State or uStorage.msEnabled
    else
      Item.Services.List[iIndex].State:=Item.Services.List[iIndex].State and not uStorage.msEnabled;
    Result:=uStorage.MatrixService_Save(Item.Services.List[iIndex]);
    Inc(iLcv);
  end;
end;

Function Delete_Service(Var Item:TServiceEditItem; Selection:TGroupItem):Boolean;
var
  iTag:Integer;
  GI:TGroupItem;
  ISB:TItemScrollBox;
begin
  iTag:=Selection.Tag; ISB:=Selection.Scrollbox;
  Result:=uStorage.MatrixService_Delete(Item.Services,iTag);
  Repeat
    GI:=ISB.FindItemByTag(iTag);
    If GI<>Nil then
      ISB.RemoveSubItem(GI);
  Until GI=Nil;
end;

Function Add_Service(Var Item:TServiceEditItem; Kind:Int64; IP:String; Port:Word; Scale:Word):Boolean;
var
  MS:TMatrixService;
begin
  Copy(uClusterEditing.Cluster_Edit.Cluster,Item.Services.Cluster);
  Copy(uNodeEditing.Node_Edit.Node,Item.Services.Node);
  Item.DomainID:=Domain_Edit.Domain.ID;

  MS.Service:=Nil;
  MS.NodeID:=Item.Services.Node.ID;
  MS.DomainID:=Item.DomainID;
  MS.Kind:=Kind;
  MS.IP:=ccWinsock.inet_addr(PChar(IP));
  MS.Scale:=Scale;
  MS.Port:=Port;
  MS.State:=msEnabled;
  Result:=uStorage.MatrixService_Create(MS);
end;

end.

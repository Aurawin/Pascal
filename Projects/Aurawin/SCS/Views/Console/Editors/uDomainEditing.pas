unit uDomainEditing;

interface
uses SysUtils,ccUtils,Classes,uItemScrollBox,uStorage,OCL,HSRConsts;
Const
  Illegal_Characters='~!@#$%^&*()+{}[]|\:;"'',<>/? ';
Type
  TDomainEditItem=record
    Domain                            : TDomain;
    Root                              : TUserAccount;
    GP_Domain                         : TGroup;
    Directory                         : TDDirectory;

    GI_Root                           : TGroupItem;
    GI_FriendlyName                   : TGroupItem;
    GI_HTTP_Files                     : TGroupItem;
    GI_SMTP_AUX_Port                  : TGroupItem;
    GI_RELAY_Scale                    : TGroupItem;
    GI_SMTP_Scale                     : TGroupItem;
    GI_SMTP_AUX_Scale                 : TGroupItem;
    GI_POP3_Scale                     : TGroupItem;
    GI_HTTP_Scale                     : TGroupItem;
    GI_XMPP_Scale                     : TGroupItem;
    GI_DNS_Scale                      : TGroupItem;
    GI_XMPP_ClientToServer_Scale      : TGroupItem;
    GI_XMPP_ServerToServer_Scale      : TGroupItem;
    GI_LDAP_Scale                     : TGroupItem;
    GI_Default_Quota                  : TGroupItem;
    GI_Default_WebMail                : TGroupItem;
    GI_Default_SpamFiltering          : TGroupItem;
    GI_Default_SiteStats              : TGroupItem;
    GI_Default_RSSEdit                : TGroupItem;
    GI_Default_CalendarEdit           : TGroupItem;
    GI_Calendar_Config_P_TC           : TGroupItem;
    GI_Calendar_Config_P_THC          : TGroupItem;
    GI_Calendar_Config_P_TRC          : TGroupItem;
    GI_Calendar_Config_P_TSC          : TGroupItem;
    GI_Calendar_Config_P_TDC          : TGroupItem;
    GI_Calendar_Config_P_TLC          : TGroupItem;
    GI_Calendar_Config_P_TIC          : TGroupItem;
    GI_Calendar_Config_P_TFC          : TGroupItem;

    GI_Calendar_Config_C_TC           : TGroupItem;
    GI_Calendar_Config_C_THC          : TGroupItem;
    GI_Calendar_Config_C_TRC          : TGroupItem;
    GI_Calendar_Config_C_TSC          : TGroupItem;
    GI_Calendar_Config_C_TDC          : TGroupItem;
    GI_Calendar_Config_C_TLC          : TGroupItem;
    GI_Calendar_Config_C_TIC          : TGroupItem;
    GI_Calendar_Config_C_TFC          : TGroupItem;

    GI_Calendar_Config_N_TC           : TGroupItem;
    GI_Calendar_Config_N_THC          : TGroupItem;
    GI_Calendar_Config_N_TRC          : TGroupItem;
    GI_Calendar_Config_N_TSC          : TGroupItem;
    GI_Calendar_Config_N_TDC          : TGroupItem;
    GI_Calendar_Config_N_TLC          : TGroupItem;
    GI_Calendar_Config_N_TIC          : TGroupItem;
    GI_Calendar_Config_N_TFC          : TGroupItem;

    GI_Calendar_Config_D_TC           : TGroupItem;
    GI_Calendar_Config_D_THC          : TGroupItem;
    GI_Calendar_Config_D_TRC          : TGroupItem;
    GI_Calendar_Config_D_TSC          : TGroupItem;
    GI_Calendar_Config_D_TDC          : TGroupItem;
    GI_Calendar_Config_D_TLC          : TGroupItem;
    GI_Calendar_Config_D_TIC          : TGroupItem;
    GI_Calendar_Config_D_TFC          : TGroupItem;
    
    GI_ADMIN_iTextAreaCols            : TGroupItem;
    GI_ADMIN_iTextAreaRows            : TGroupItem;
    GI_ADMIN_sTableClass              : TGroupItem;
    GI_ADMIN_sTRClass                 : TGroupItem;
    GI_ADMIN_sTHClass                 : TGroupItem;
    GI_ADMIN_sTDClass                 : TGroupItem;
    GI_ADMIN_sTSClass                 : TGroupItem;
    GI_ADMIN_sTFClass                 : TGroupItem;
    GI_ADMIN_sTIClass                 : TGroupItem;
    GI_ADMIN_sComboClass              : TGroupItem;
    GI_ADMIN_sInputClass              : TGroupItem;
    GI_ADMIN_sRSSIconURL              : TGroupItem;
    GI_ADMIN_sCalIconURL              : TGroupItem;
    GI_ADMIN_sStatsIconURL            : TGroupItem;
    GI_ADMIN_sGoButton                : TGroupItem;
  end;

  Function  IsValidDomain(Domain:String):Boolean;
  Function  Initialize_DomainWindow(ISB:TItemScrollBox; Var Item:TDomainEditItem):Boolean;
  Function  Add_Domain(Var Item:TDomainEditItem; Domain:String):Boolean;
  Function  Delete_Domain(Var Item:TDomainEditItem):Boolean;
  Function  Save_DomainWindow(ISB:TItemScrollBox; Var Item:TDomainEditItem):Boolean;
var
  Domain_Edit      : TDomainEditItem;
implementation

Function IsValidDomain(Domain:String):Boolean;
var
  iLcv:Integer;
begin
  Result:=True;  iLcv:=1;
  while Result and (iLcv<=Length(Illegal_Characters)) do begin
    Result:=System.Pos(Illegal_Characters[iLcv],Domain)=0;
    Inc(iLcv);
  end;
end;

Function Initialize_DomainWindow(ISB:TItemScrollBox; Var Item:TDomainEditItem):Boolean;
var
  iLcv:Integer;
  Prop:TProperty;
begin
  Result:=False;
  if Length(Item.Domain.Domain)>0 then begin
    ISB.Clear;
    With Item do begin
      GP_Domain:=ISB.AddGroup('Properties');
      Prop.Name:='Root';
      Prop.Value:=ccUtils.VarStringToString(Item.Domain.Root);
      Prop.Style:=psString;
      Prop.MaskValue:=False;
      GI_Root:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Root.Hint:='Root User Account';

      Prop.Caption:='Friendly Name';
      Prop.Name:='Alias';
      Prop.Value:=ccUtils.VarStringToString(Item.Domain.FriendlyName);
      Prop.Style:=psString;
      Prop.MaskValue:=False;
      GI_FriendlyName:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_FriendlyName.Hint:='Friendly Name';


      Prop.Name:='Relay Scale';
      Prop.Value:=IntToStr(Item.Domain.RELAY_Scale);
      Prop.Style:=psNumber;
      GI_RELAY_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_RELAY_Scale.Hint:='Scale of Relay service for mail services';

      Prop.Name:='SMTP Scale';
      Prop.Value:=IntToStr(Item.Domain.SMTP_Scale);
      GI_SMTP_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_SMTP_Scale.Hint:='Scale of SMTP services';

      Prop.Name:='POP3 Scale';
      Prop.Value:=IntToStr(Item.Domain.POP3_Scale);
      GI_POP3_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_POP3_Scale.Hint:='Scale of POP3 services';

      Prop.Name:='HTTP Scale';
      Prop.Value:=IntToStr(Item.Domain.HTTP_Scale);
      GI_HTTP_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_HTTP_Scale.Hint:='Scale of HTTP services';

      Prop.Name:='DNS Scale';
      Prop.Value:=IntToStr(Item.Domain.DNS_Scale);
      GI_DNS_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_DNS_Scale.Hint:='Scale of DNS services';

      Prop.Name:='XMPP Client Scale';
      Prop.Value:=IntToStr(Item.Domain.XMPP_ClientToServer_Scale);
      Prop.Style:=psNumber;
      GI_XMPP_ClientToServer_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_XMPP_ClientToServer_Scale.Hint:='Scale of XMPP Client to Server services';

      Prop.Name:='XMPP Server Scale';
      Prop.Value:=IntToStr(Item.Domain.XMPP_ServerToServer_Scale);
      Prop.Style:=psNumber;
      GI_XMPP_ServerToServer_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_XMPP_ServerToServer_Scale.Hint:='Scale of XMPP Server to Server services';

      Prop.Name:='LDAP Scale';
      Prop.Value:=IntToStr(Item.Domain.LDAP_Scale);
      Prop.Style:=psNumber;
      GI_LDAP_Scale:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_LDAP_Scale.Hint:='Scale of LDAP services';

      Prop.Name:='Default Quota';
      Prop.Value:=IntToStr(Item.Domain.Default_Quota);
      Prop.Style:=psNumber;
      GI_Default_Quota:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Default_Quota.Hint:='Default Quota for Users of services';

      Prop.Name:='Default Webmail';
      Prop.Value:=YesNo[Item.Domain.Default_WebMail];
      Prop.Style:=psYesNo;
      GI_Default_WebMail:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Default_WebMail.Hint:='Default Access To Webmail for Users of services';

      Prop.Name:='Default Filter';
      Prop.Value:=YesNo[Item.Domain.Default_SpamFiltering];
      Prop.Style:=psYesNo;
      GI_Default_SpamFiltering:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Default_SpamFiltering.Hint:='Default Access To Spam Measures for Users of services';

      Prop.Name:='Default Stats';
      Prop.Value:=YesNo[Item.Domain.Default_SiteStats];
      Prop.Style:=psYesNo;
      GI_Default_SiteStats:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Default_SiteStats.Hint:='Default Access To Server Stats for Users of services';

      Prop.Name:='Default Edit Calendar';
      Prop.Value:=YesNo[Item.Domain.Default_Calendar];
      Prop.Style:=psYesNo;
      GI_Default_CalendarEdit:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Default_CalendarEdit.Hint:='Default Access To Manage Calendar Events for Users of services';

      Prop.Name:='Default Edit RSS';
      Prop.Value:=YesNo[Item.Domain.Default_RSS];
      Prop.Style:=psYesNo;
      GI_Default_RSSEdit:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Default_RSSEdit.Hint:='Default Access To Manage RSS Feeds for Users of services';

      Prop.Name:='Previous TC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TC.Hint:='Previous Month Table Class';

      Prop.Name:='Previous THC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].THC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_THC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_THC.Hint:='Previous Month Table Header Class';

      Prop.Name:='Previous TRC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TRC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TRC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TRC.Hint:='Previous Month Table Row Class';

      Prop.Name:='Previous TSC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TSC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TSC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TSC.Hint:='Previous Month Table Separator Class';

      Prop.Name:='Previous TDC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TDC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TDC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TDC.Hint:='Previous Month Table Cell Class';

      Prop.Name:='Previous TLC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TLC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TLC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TLC.Hint:='Previous Month Table Link Class';

      Prop.Name:='Previous TIC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TIC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TIC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TIC.Hint:='Previous Month Table Image Class';

      Prop.Name:='Previous TFC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TFC);
      Prop.Style:=psString;
      GI_Calendar_Config_P_TFC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_P_TFC.Hint:='Previous Month Table Footer Class';

      Prop.Name:='Current TC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TC.Hint:='Current Month Table Class';

      Prop.Name:='Current THC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].THC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_THC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_THC.Hint:='Current Month Table Header Class';

      Prop.Name:='Current TRC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TRC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TRC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TRC.Hint:='Current Month Table Row Class';

      Prop.Name:='Current TSC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TSC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TSC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TSC.Hint:='Current Month Table Separator Class';

      Prop.Name:='Current TDC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TDC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TDC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TDC.Hint:='Current Month Table Cell Class';

      Prop.Name:='Current TLC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TLC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TLC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TLC.Hint:='Current Month Table Link Class';

      Prop.Name:='Current TIC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TIC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TIC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TIC.Hint:='Current Month Table Image Class';

      Prop.Name:='Current TFC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TFC);
      Prop.Style:=psString;
      GI_Calendar_Config_C_TFC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_C_TFC.Hint:='Current Month Table Footer Class';

      Prop.Name:='Next TC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TC.Hint:='Next Month Table Class';

      Prop.Name:='Next THC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].THC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_THC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_THC.Hint:='Next Month Table Header Class';

      Prop.Name:='Next TRC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TRC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TRC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TRC.Hint:='Next Month Table Row Class';

      Prop.Name:='Next TSC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TSC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TSC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TSC.Hint:='Next Month Table Separator Class';

      Prop.Name:='Next TDC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TDC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TDC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TDC.Hint:='Next Month Table Cell Class';

      Prop.Name:='Next TLC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TLC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TLC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TLC.Hint:='Next Month Table Link Class';

      Prop.Name:='Next TIC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TIC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TIC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TIC.Hint:='Next Month Table Image Class';

      Prop.Name:='Next TFC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TFC);
      Prop.Style:=psString;
      GI_Calendar_Config_N_TFC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_N_TFC.Hint:='Next Month Table Footer Class';

      Prop.Name:='Display TC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TC.Hint:='Display Month Table Class';

      Prop.Name:='Display THC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].THC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_THC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_THC.Hint:='Display Month Table Header Class';

      Prop.Name:='Display TRC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TRC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TRC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TRC.Hint:='Display Month Table Row Class';

      Prop.Name:='Display TSC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TSC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TSC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TSC.Hint:='Display Month Table Separator Class';

      Prop.Name:='Display TDC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TDC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TDC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TDC.Hint:='Display Month Table Cell Class';

      Prop.Name:='Display TLC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TLC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TLC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TLC.Hint:='Display Month Table Link Class';

      Prop.Name:='Display TIC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TIC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TIC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TIC.Hint:='Display Month Table Image Class';

      Prop.Name:='Display TFC';
      Prop.Value:=ccUtils.VarStringToString(PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TFC);
      Prop.Style:=psString;
      GI_Calendar_Config_D_TFC:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_Calendar_Config_D_TFC.Hint:='Display Month Table Footer Class';

      Prop.Name:='Admin Cols';
      Prop.Value:=IntToStr(Item.Domain.Admin.iTextAreaCols);
      Prop.Style:=psNumber;
      GI_ADMIN_iTextAreaCols:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_iTextAreaCols.Hint:='Number of Columns in Text Areas for Site Administration';

      Prop.Name:='Admin Rows';
      Prop.Value:=IntToStr(Item.Domain.Admin.iTextAreaRows);
      Prop.Style:=psNumber;
      GI_ADMIN_iTextAreaRows:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_iTextAreaRows.Hint:='Number of Rows in Text Areas for Site Administration';

      Prop.Name:='Admin TC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.TableClass);
      Prop.Style:=psString;
      GI_ADMIN_sTableClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTableClass.Hint:='Table Class for Site Administration';

      Prop.Name:='Admin TRC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.TRClass);
      Prop.Style:=psString;
      GI_ADMIN_sTRClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTRClass.Hint:='Table Row Class for Site Administration';

      Prop.Name:='Admin THC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.THClass);
      Prop.Style:=psString;
      GI_ADMIN_sTHClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTHClass.Hint:='Table Header Class for Site Administration';

      Prop.Name:='Admin TDC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.TDClass);
      Prop.Style:=psString;
      GI_ADMIN_sTDClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTDClass.Hint:='Table Cell Class for Site Administration';

      Prop.Name:='Admin TSC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.TSClass);
      Prop.Style:=psString;
      GI_ADMIN_sTSClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTSClass.Hint:='Table Separator Class for Site Administration';

      Prop.Name:='Admin TFC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.TFClass);
      Prop.Style:=psString;
      GI_ADMIN_sTFClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTFClass.Hint:='Table Footer Class for Site Administration';

      Prop.Name:='Admin TIC';
      Prop.Value:=VarStringToString(Item.Domain.Admin.TIClass);
      Prop.Style:=psString;
      GI_ADMIN_sTIClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sTIClass.Hint:='Table Image Class for Site Administration';

      Prop.Name:='Admin Combo Class';
      Prop.Value:=VarStringToString(Item.Domain.Admin.ComboClass);
      Prop.Style:=psString;
      GI_ADMIN_sComboClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sComboClass.Hint:='ComboBox Class for Site Administration';

      Prop.Name:='Admin Input Class';
      Prop.Value:=VarStringToString(Item.Domain.Admin.InputClass);
      Prop.Style:=psString;
      GI_ADMIN_sInputClass:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sInputClass.Hint:='Input Class for Site Administration';

      Prop.Name:='RSS Icon URL';
      Prop.Value:=VarStringToString(Item.Domain.Admin.RSSIconURL);
      Prop.Style:=psString;
      GI_ADMIN_sRSSIconURL:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sRSSIconURL.Hint:='URL of icon for RSS Feeds';

      Prop.Name:='Cal Icon URL';
      Prop.Value:=VarStringToString(Item.Domain.Admin.CalIconURL);
      Prop.Style:=psString;
      GI_ADMIN_sCalIconURL:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sCalIconURL.Hint:='URL of icon for Calendar';

      Prop.Name:='Stats Icon URL';
      Prop.Value:=VarStringToString(Item.Domain.Admin.StatsIconURL);
      Prop.Style:=psString;
      GI_ADMIN_sStatsIconURL:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sStatsIconURL.Hint:='URL of icon for Service Statistics';

      Prop.Name:='Go Icon';
      Prop.Value:=VarStringToString(Item.Domain.Admin.GoButtonURL);
      Prop.Style:=psString;
      GI_ADMIN_sGoButton:=GP_Domain.AddPropertyItem(-1,Prop);
      GI_ADMIN_sGoButton.Hint:='URL of icon for Go Button for Site Administration';
      Result:=True;
    end;
  end;
end;

Function Save_DomainWindow(ISB:TItemScrollBox; Var Item:TDomainEditItem):Boolean;
begin
  Result:=False;
  ISB.Caption:='Saving...';
  With Item do begin
    ccUtils.StringToVarString(GI_Root.Properties.Value,Item.Domain.Root);
    ccUtils.StringToVarString(GI_FriendlyName.Properties.Value,Item.Domain.FriendlyName);
    Item.Domain.RELAY_Scale:=StrToIntDef(GI_RELAY_Scale.Properties.Value,0);
    Item.Domain.SMTP_Scale:=StrToIntDef(GI_SMTP_Scale.Properties.Value,0);
    Item.Domain.POP3_Scale:=StrToIntDef(GI_POP3_Scale.Properties.Value,0);
    Item.Domain.HTTP_Scale:=StrToIntDef(GI_HTTP_Scale.Properties.Value,0);
    Item.Domain.DNS_Scale:=StrToIntDef(GI_DNS_Scale.Properties.Value,0);
    Item.Domain.XMPP_ClientToServer_Scale:=StrToIntDef(GI_XMPP_ClientToServer_Scale.Properties.Value,0);
    Item.Domain.XMPP_ServerToServer_Scale:=StrToIntDef(GI_XMPP_ServerToServer_Scale.Properties.Value,0);
    Item.Domain.LDAP_Scale:=StrToIntDef(GI_LDAP_Scale.Properties.Value,0);
    Item.Domain.Default_Quota:=StrToIntDef(GI_Default_Quota.Properties.Value,0);
    Item.Domain.Default_WebMail:=GI_Default_WebMail.Properties.Value=YesNo[True];
    Item.Domain.Default_SpamFiltering:=GI_Default_SpamFiltering.Properties.Value=YesNo[True];
    Item.Domain.Default_SiteStats:=GI_Default_SiteStats.Properties.Value=YesNo[True];
    Item.Domain.Default_Calendar:=GI_Default_CalendarEdit.Properties.Value=YesNo[True];
    Item.Domain.Default_RSS:=GI_Default_RSSEdit.Properties.Value=YesNo[True];
    StringToVarString(GI_Calendar_Config_P_TC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TC);
    StringToVarString(GI_Calendar_Config_P_THC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].THC);
    StringToVarString(GI_Calendar_Config_P_TRC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TRC);
    StringToVarString(GI_Calendar_Config_P_TSC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TSC);
    StringToVarString(GI_Calendar_Config_P_TDC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TDC);
    StringToVarString(GI_Calendar_Config_P_TLC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TLC);
    StringToVarString(GI_Calendar_Config_P_TIC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TIC);
    StringToVarString(GI_Calendar_Config_P_TFC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[0].TFC);
    StringToVarString(GI_Calendar_Config_C_TC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TC);
    StringToVarString(GI_Calendar_Config_C_THC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].THC);
    StringToVarString(GI_Calendar_Config_C_TRC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TRC);
    StringToVarString(GI_Calendar_Config_C_TSC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TSC);
    StringToVarString(GI_Calendar_Config_C_TDC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TDC);
    StringToVarString(GI_Calendar_Config_C_TLC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TLC);
    StringToVarString(GI_Calendar_Config_C_TIC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TIC);
    StringToVarString(GI_Calendar_Config_C_TFC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[1].TFC);
    StringToVarString(GI_Calendar_Config_N_TC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TC);
    StringToVarString(GI_Calendar_Config_N_THC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].THC);
    StringToVarString(GI_Calendar_Config_N_TRC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TRC);
    StringToVarString(GI_Calendar_Config_N_TSC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TSC);
    StringToVarString(GI_Calendar_Config_N_TDC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TDC);
    StringToVarString(GI_Calendar_Config_N_TLC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TLC);
    StringToVarString(GI_Calendar_Config_N_TIC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TIC);
    StringToVarString(GI_Calendar_Config_N_TFC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[2].TFC);
    StringToVarString(GI_Calendar_Config_D_TC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TC);
    StringToVarString(GI_Calendar_Config_D_THC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].THC);
    StringToVarString(GI_Calendar_Config_D_TRC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TRC);
    StringToVarString(GI_Calendar_Config_D_TSC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TSC);
    StringToVarString(GI_Calendar_Config_D_TDC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TDC);
    StringToVarString(GI_Calendar_Config_D_TLC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TLC);
    StringToVarString(GI_Calendar_Config_D_TIC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TIC);
    StringToVarString(GI_Calendar_Config_D_TFC.Properties.Value,PCalendarConfig(Item.Domain.CalendarConfigP)^[3].TFC);
    Item.Domain.Admin.iTextAreaCols:=StrToIntDef(GI_ADMIN_iTextAreaCols.Properties.Value,40);
    Item.Domain.Admin.iTextAreaRows:=StrToIntDef(GI_ADMIN_iTextAreaRows.Properties.Value,20);
    StringToVarString(GI_ADMIN_sTableClass.Properties.Value,Item.Domain.Admin.TableClass);
    StringToVarString(GI_ADMIN_sTRClass.Properties.Value,Item.Domain.Admin.TRClass);
    StringToVarString(GI_ADMIN_sTHClass.Properties.Value,Item.Domain.Admin.THClass);
    StringToVarString(GI_ADMIN_sTDClass.Properties.Value,Item.Domain.Admin.TDClass);
    StringToVarString(GI_ADMIN_sTSClass.Properties.Value,Item.Domain.Admin.TSClass);
    StringToVarString(GI_ADMIN_sTFClass.Properties.Value,Item.Domain.Admin.TFClass);
    StringToVarString(GI_ADMIN_sTIClass.Properties.Value,Item.Domain.Admin.TIClass);
    StringToVarString(GI_ADMIN_sComboClass.Properties.Value,Item.Domain.Admin.ComboClass);
    StringToVarString(GI_ADMIN_sInputClass.Properties.Value,Item.Domain.Admin.InputClass);
    StringToVarString(GI_ADMIN_sRSSIconURL.Properties.Value,Item.Domain.Admin.RSSIconURL);
    StringToVarString(GI_ADMIN_sCalIconURL.Properties.Value,Item.Domain.Admin.CalIconURL);
    StringToVarString(GI_ADMIN_sStatsIconURL.Properties.Value,Item.Domain.Admin.StatsIconURL);
    StringToVarString(GI_ADMIN_sGoButton.Properties.Value,Item.Domain.Admin.GoButtonURL);
  end;
  Result:=uStorage.Domain_Update(uStorage.Module,Item.Domain);
end;

Function  Add_Domain(Var Item:TDomainEditItem; Domain:String):Boolean;
var
  iFolderID:Int64;
  vsCore:Core.Strings.VarString;
begin
  Result:=False;
  uStorage.UserAccount_Init_Root(uStorage.Module,Item.Root);
  uStorage.Domain_Initialize(Item.Domain);
  StringToVarString(Domain,Item.Domain.Domain);
  Copy(Item.Root.User,Item.Domain.Root);
  Copy(Item.Domain.Domain,Item.Domain.FriendlyName);
  If uStorage.Domain_Create(uStorage.Module,Item.Domain) then begin
    Result:=uStorage.UserAccount_Create(uStorage.Module,Item.Root,Item.Domain.ID);
    StringToVarString('www/core',vsCore);
    Try
      CreateFolder(uStorage.Module, Item.Directory, Item.Domain.ID, vsCore,iFolderID);
    Finally
      Empty(vsCore);
    End;
  end;
end;

Function  Delete_Domain(Var Item:TDomainEditItem):Boolean;
begin
  Result:=uStorage.Domain_Delete(uStorage.Module,Item.Domain);
end;

end.

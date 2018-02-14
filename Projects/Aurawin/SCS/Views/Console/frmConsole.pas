unit frmConsole;

interface

uses
  Classes,
  LCLIntf,
  LResources,
  Types,
  Process,
  DOM,
  XMLRead,
  FileUtil,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  ActnList,
  Menus,
  Buttons,
  Spin,
  CheckLst,
  PairSplitter,
  ValEdit,
  Grids,
  Interfaces,

  App,
  App.Build,
  App.Consts,

  Core.Timer,
  Core.Strings,
  Core.Threads.Workers,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Pointers,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,
  Core.Arrays.Boolean,
  Core.Arrays.VarString,


  Form.Exception,

  RSR,
  RSR.Core,
  RSR.HTTP,
  RSR.DNS,
  RSR.LDAP,
  RSR.IMAP,

  Storage,
  Storage.Apps,
  Storage.AuraDisks,
  Storage.CoreObjects,
  Storage.ContentTypes,
  Storage.Certs,
  Storage.Commerce,
  Storage.CSS,
  Storage.ConfigData,
  Storage.DNS,
  Storage.Domains,
  Storage.FAT,
  Storage.Intrusion,
  Storage.Keywords,
  Storage.Main,
  Storage.MatrixClusters,
  Storage.MatrixServices,
  Storage.MatrixNodes,
  Storage.MatrixResources,
  Storage.Quill,
  Storage.Roster,
  Storage.SrchGroupLayer,
  Storage.SrchProviders,
  Storage.Social,
  Storage.Security,
  Storage.UserAccounts,

  Core.XML,
  Core.Logging,
  Core.Streams,

  Encryption.Zip,
  Encryption.SSL,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,

  Core.Keywords,

  Core.Utils.Sockets,
  Core.Utils.TreeView,
  Core.Utils.Time,
  Core.Utils.ListView,
  Core.Utils.Files,
  Core.Utils.Forms,
  {$ifdef Unix}
    Core.Utils.Unix.Account,
  {$endif}
  Core.Lock.Boolean,
  SysUtils;

type

  { TConsoleForm }

  TConsoleForm = class(TForm)
    btnBLDNSItemSave: TButton;
    btnContentTypeSave: TButton;
    btnHSALAdd1: TButton;
    btnSecIPViolatorsReload: TButton;
    btnHSALRemove1: TButton;
    btnSecIPViolatorSave: TButton;
    btnSecIpViolatorSearch: TButton;
    btnHSCNSearch: TButton;
    btnHSBLAdd: TButton;
    btnHSALAdd: TButton;
    btnHSALReload: TButton;
    btnSecIPViolatorsToolClear: TButton;
    btnSecIPVoiolatorAdd: TButton;
    btnSecIPVRemove :TButton;
    btnHSALSearch: TButton;
    btnHSTLDAdd: TButton;
    btnHSTLDReload: TButton;
    btnHSBLRemove: TButton;
    btnHSBLReload: TButton;
    btnHSALRemove: TButton;
    btnHSTLDRemove: TButton;
    btnHSBLSave: TButton;
    btnHSALSave: TButton;
    btnHSTLDSave: TButton;
    btnHSTLDSearch: TButton;
    btnHSBLSearch: TButton;
    btnSecContentSearch: TButton;
    btnHSWLReload: TButton;
    btnHSWLAdd: TButton;
    btnHSWLRemove: TButton;
    btnHSWLSave: TButton;
    btnContentTypeAdd: TButton;
    btnContentTypeRemove: TButton;
    btnSecProfileSearch: TButton;
    btnSecProfileContentSave: TButton;
    btnWLDNSItemSave: TButton;
    btnClusterNewNode: TBitBtn;
    btnClusterNewNodeCancel: TBitBtn;
    btnDiskNodeRefresh: TBitBtn;
    btnBLDNSItemAdd: TButton;
    btnBLDNSItemRemove: TButton;
    btnProcessGroupChange: TButton;
    btnRaidChangeGroup: TButton;
    btnProcessUserChange: TButton;
    btnRaidUserChange: TButton;
    btnSecContentAdd: TButton;
    btnSecContentDel: TButton;
    btnSecContentSave: TButton;
    btnSecProfileAdd: TButton;
    btnSecProfileDel: TButton;
    btnUser_Edit_Cancel: TBitBtn;
    btnClusterCancel1: TBitBtn;
    btnClusterCancel2: TBitBtn;
    btnCRNResourceOk: TBitBtn;
    btnCRNResourceCancel: TBitBtn;
    btnCluster_Done: TBitBtn;
    btnDomain_User_Done: TBitBtn;
    btnKeywordEditingDone: TBitBtn;
    btnClusterNew: TBitBtn;
    btnClusterCancel: TBitBtn;
    btnDomainsNew_OK: TBitBtn;
    btnDNSItemAdd: TButton;
    btnDNSItemRemove: TButton;
    btnDomain_Done: TBitBtn;
    btnDomain_User_New_Done: TBitBtn;
    btnProviderLandingPageLoad: TButton;
    btnProviderLandingPageEdit: TButton;
    btnProviderLandingPageSaveAs: TButton;
    btnProviderLandingPageClear: TButton;
    btnDCAAllocate: TButton;
    btnDCADeallocate: TButton;
    cbClusterNodeDisk: TCheckBox;
    cbClusterNodeEnabled: TCheckBox;
    cbDCDomains: TComboBox;
    cbDomain_Host: TComboBox;
    cbSecIPViolator: TCheckBox;
    cbSecBLDomain: TCheckBox;
    cbSecALDomain: TCheckBox;
    cbSecWLDomain: TCheckBox;
    cbSecContentProfile: TCheckBox;
    cbSecContentPhrase: TCheckBox;
    cbUser_Edit_Settings: TCheckGroup;
    cmboDiskClusters: TComboBox;
    cmboDiskNodes: TComboBox;
    cmboDiskResources: TComboBox;
    cmboPurchaseKind: TComboBox;
    cmboPurchaseKind1: TComboBox;
    cmboSecIPViolator: TComboBox;
    cmboSecBLDomain: TComboBox;
    cmboSecALDomain: TComboBox;
    cmboSecCNDomain: TComboBox;
    cmboSecCNStatus: TComboBox;
    cmboSecIPViolatorSearch: TComboBox;
    cmboSecDmBLSC: TComboBox;
    cmboSecDmALSC: TComboBox;
    cmboSecWLDomain: TComboBox;
    cmboSecContentProfile: TComboBox;
    cmboSecContentPhrase: TComboBox;
    cmboSecDmWLSC: TComboBox;
    gbClusterNodeStats: TGroupBox;
    gbClusterNodeDefaults: TGroupBox;
    gbContentWordDetails: TGroupBox;
    gbContentWordDetails1: TGroupBox;
    gbDBMDBTable: TGroupBox;
    gbDBMTableDescription: TGroupBox;
    gbCRNResource: TGroupBox;
    gbDCProperties: TGroupBox;
    gbDCNI: TGroupBox;
    gbPurchaseEditor: TGroupBox;
    gbDomainLicenseEditor: TGroupBox;
    gbSecDBLSC: TGroupBox;
    gbSecDBLSC1: TGroupBox;
    gbSecDBLSC2: TGroupBox;
    gbSecDBLSC3: TGroupBox;
    gbSecCFSC: TGroupBox;
    gbSecDBLSC4: TGroupBox;
    GroupBox13: TGroupBox;
    GroupBox15: TGroupBox;
    GroupBox16: TGroupBox;
    GroupBox17: TGroupBox;
    GroupBox19: TGroupBox;
    GroupBox20: TGroupBox;
    GroupBox21: TGroupBox;
    GroupBox22: TGroupBox;
    GroupBox23: TGroupBox;
    GroupBox24: TGroupBox;
    GroupBox26: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox37: TGroupBox;
    GroupBox38: TGroupBox;
    gbSecDWLSC: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    GroupBox8: TGroupBox;
    imgLock: TImage;
    imgUnlock: TImage;
    Label1: TLabel;
    Label100: TLabel;
    Label101: TLabel;
    Label102: TLabel;
    Label103: TLabel;
    Label104: TLabel;
    Label105: TLabel;
    Label106: TLabel;
    Label107: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label25: TLabel;
    Label30: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label70: TLabel;
    Label74: TLabel;
    Label75: TLabel;
    Label76: TLabel;
    Label78: TLabel;
    Label79: TLabel;
    Label88: TLabel;
    Label92: TLabel;
    Label93: TLabel;
    Label97: TLabel;
    Label98: TLabel;
    Label99: TLabel;
    lbAuDisks: TLabel;
    lblBuildInfo: TLabel;
    lblStartupMessage: TLabel;
    Label72: TLabel;
    Label80: TLabel;
    Label82: TLabel;
    Label87: TLabel;
    Label90: TLabel;
    Label91: TLabel;
    Label94: TLabel;
    Label95: TLabel;
    Label96: TLabel;
    lbMNID3: TLabel;
    lbMNUserID: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label60: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    lbMNGroupID: TLabel;
    Label71: TLabel;
    lblDCMRCAvailable1: TLabel;
    lblDCMRCTotal1: TLabel;
    lblDCMRCUsed1: TLabel;
    lblDomainCert: TLabel;
    Label3: TLabel;
    Label63: TLabel;
    Label64: TLabel;
    Label65: TLabel;
    lbDCNIStatus: TLabel;
    lbDCNIStatusValue: TLabel;
    lblDCMRCUsed: TLabel;
    Label24: TLabel;
    lblDCMRCAvailable: TLabel;
    Label26: TLabel;
    lblDCMRCTotal: TLabel;
    Label31: TLabel;
    lbDCNIName: TLabel;
    lbDCNIIP: TLabel;
    lbDCNINValue: TLabel;
    lbDCNIIPValue: TLabel;
    lbConnection: TLabel;
    lblDSCStatus: TLabel;
    lblMNDSKAvail: TLabel;
    lblMNDSKTotal: TLabel;
    lblMNDSKUsed: TLabel;
    lblMNRAMAvail: TLabel;
    lblMNRAMTotal: TLabel;
    lblMNRAMUsed: TLabel;
    lbMNID: TLabel;
    lbMNDisk: TLabel;
    lbMNID2: TLabel;
    lbStatus: TLabel;
    lvCerts: TListView;
    lvClusterNodeService: TListView;
    lvContentFilters: TListView;
    lvHSBL: TListView;
    lvHSAL: TListView;
    lvHSTLD: TListView;
    lvDomainLicenses: TListView;
    lvSecCNDomain: TListView;
    lvHSWL: TListView;
    lvProfileFilters: TListView;
    lvPurchase: TListView;
    lvSecIPViolators: TListView;
    lvUserACLCoreCommands: TListView;
    MainMenu1: TMainMenu;
    miLoadServerIntermediateCert: TMenuItem;
    miDomainCertRequestNew: TMenuItem;
    miDomainCertImport: TMenuItem;
    miLoadRootServerIntCert: TMenuItem;
    miLoadCertIntRoot: TMenuItem;
    miDomainLicenseSep: TMenuItem;
    miDomainLicenseDelete: TMenuItem;
    miDomainLicenseEdit: TMenuItem;
    miDomainLicenseNew: TMenuItem;
    miSecCNDDelete: TMenuItem;
    miSecCNDSep2: TMenuItem;
    miSecCNDAL: TMenuItem;
    miSecCNDSep1: TMenuItem;
    miSecCNDWL: TMenuItem;
    miSecCNDBL: TMenuItem;
    miLoadCert: TMenuItem;
    miLoadCertInt: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    miCODenySelected: TMenuItem;
    MenuItem33: TMenuItem;
    miCOACLGrantAll: TMenuItem;
    miCOACLGrantChecked: TMenuItem;
    miCOACLGrantSelected: TMenuItem;
    miDenyAll: TMenuItem;
    miCODenyChecked: TMenuItem;
    MenuItem34: TMenuItem;
    miCOACLDenyAll: TMenuItem;
    miCOACLGrant: TMenuItem;
    miWindow: TMenuItem;
    miClusterResourceEdit1: TMenuItem;
    miClusterNodeEdit: TMenuItem;
    miClusterRNDelete1: TMenuItem;
    miClusterRNNew1: TMenuItem;
    miClusterRNNewNode1: TMenuItem;
    miClusterRNNewResource1: TMenuItem;
    miDomainKeywordsDelete: TMenuItem;
    miDomainKeywordsNew: TMenuItem;
    odCert: TOpenDialog;
    odKeyImport: TOpenDialog;
    odIntermediateAuthorityCert: TOpenDialog;
    odOPZFile: TOpenDialog;
    odRootAuthorityCert: TOpenDialog;
    odServerAuthorityCert: TOpenDialog;
    odSKWFile: TSaveDialog;
    odOKWFile: TOpenDialog;
    odSPFile: TSaveDialog;
    Panel34: TPanel;
    Panel35: TPanel;
    pnlAuDisks: TPanel;
    pnlDomainQuota: TPanel;
    Panel45: TPanel;
    Panel46: TPanel;
    Panel47: TPanel;
    pcSearch: TPageControl;
    pcPreferences: TPageControl;
    pcSecurity: TPageControl;
    pcManage: TPageControl;
    pcSecDomains: TPageControl;
    pcSecContent: TPageControl;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    pnlDefaultQuota1: TPanel;
    pnlSecContentEditor: TPanel;
    pnlSecContentEditor1: TPanel;
    pnlText: TPanel;
    puSSLNew: TPopupMenu;
    puDomainLicenses: TPopupMenu;
    puSecCND: TPopupMenu;
    puCertLoad: TPopupMenu;
    psCommerce: TPairSplitter;
    Panel20: TPanel;
    pnlCRNEditor: TPanel;
    pcRaid: TPageControl;
    Panel36: TPanel;
    Panel37: TPanel;
    Panel38: TPanel;
    Panel39: TPanel;
    Panel40: TPanel;
    Panel41: TPanel;
    Panel42: TPanel;
    Panel43: TPanel;
    pcService: TPageControl;
    pcUnifiedPermissions: TPageControl;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel30: TPanel;
    Panel31: TPanel;
    pcClusterNode: TPageControl;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    pbDCRAvailable1: TProgressBar;
    pbDCRTotal1: TProgressBar;
    pbDCRUsed1: TProgressBar;
    pbMNDSKAvailable: TProgressBar;
    pbMNDSKTotal: TProgressBar;
    pbMNDSKUsed: TProgressBar;
    pbMNRAMAvailable: TProgressBar;
    pbMNRAMTotal: TProgressBar;
    pbMNRAMUsed: TProgressBar;
    pnlButtons: TGroupBox;
    pnlCRNDisk: TPanel;
    pnlCRNIP: TPanel;
    pnlCRNIPO: TPanel;
    pnlCRNName: TPanel;
    pnlDCPRRAvailable2: TPanel;
    pnlDCPRRAvailable3: TPanel;
    pnlDCPRRAvailable4: TPanel;
    pnlDCPRRAvailable5: TPanel;
    pnlDCPRRAvailable6: TPanel;
    pnlDCPRRAvailable7: TPanel;
    pnlDCPRRUsed1: TPanel;
    pnlDCPRRUsed2: TPanel;
    pnlDCPRRUsed3: TPanel;
    pnlDiskNodeSelect: TPanel;
    pnlDiskOverallStats: TPanel;
    pnlDiskOverallStats1: TPanel;
    pnlDiskOverallStats2: TPanel;
    psUICore: TPairSplitter;
    pssUICObjects: TPairSplitterSide;
    pssUICCommands: TPairSplitterSide;
    pcUserSettings: TPageControl;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    pbDCRTotal: TProgressBar;
    pnlDCNIAddress1: TPanel;
    pnlDCPAllocate: TGroupBox;
    Label16: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbDBMTableDescription: TLabel;
    lvDBMTableFields: TListView;
    MenuItem26: TMenuItem;
    miClusterRNNewNode: TMenuItem;
    miClusterRNNewResource: TMenuItem;
    miClusterRNNew: TMenuItem;
    miClusterRNDelete: TMenuItem;
    miCRNNewResource: TMenuItem;
    miCRNNewNode: TMenuItem;
    pbDCRAvailable: TProgressBar;
    pnlDCPRRAvailable: TPanel;
    pbDCRUsed: TProgressBar;
    pnlDCPRRAvailable1: TPanel;
    pnlDCPRRUsed: TPanel;
    pnlDCNIName: TPanel;
    pnlDCNIAddress: TPanel;
    pnlDCPStatus: TGroupBox;
    pnlConnection: TPanel;
    pnlLogo: TPanel;
    pnlStatus: TPanel;
    procSelfSign: TProcess;
    puCOACL: TPopupMenu;
    puClusterR: TPopupMenu;
    puDomainKeywords: TPopupMenu;
    puClusterNewRN: TPopupMenu;
    psClusterRN: TPairSplitter;
    PairSplitterSide4: TPairSplitterSide;
    pssClusterContent: TPairSplitterSide;
    psDomainNodes: TPairSplitter;
    PairSplitterSide3: TPairSplitterSide;
    pssDomainNodesContent: TPairSplitterSide;
    puClusterRN: TPopupMenu;
    seDomain_Default_Quota: TSpinEdit;
    seDomain_Default_Quota1: TSpinEdit;
    seUser_Edit_Quota: TSpinEdit;
    sePurchasePrice: TFloatSpinEdit;
    seLicensePrice: TEdit;
    sgDomainLicense: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tsSecIpViolators: TTabSheet;
    tsDomainLicense: TTabSheet;
    tsSearch: TTabSheet;
    tsPreferences: TTabSheet;
    tsSecurity: TTabSheet;
    tsManage: TTabSheet;
    tsSecTLD: TTabSheet;
    tsSecDNA: TTabSheet;
    tsSecDNC: TTabSheet;
    tsSecWLDomains: TTabSheet;
    tsSecBLKDomains: TTabSheet;
    tsSecContent: TTabSheet;
    tsSecProfile: TTabSheet;
    tbbPZBackup: TToolButton;
    tbbPZRestore: TToolButton;
    tbbPurchaseDelete: TToolButton;
    tbbPurchaseEdit: TToolButton;
    tbbPurchaseNew: TToolButton;
    tbDomainClusterNodes1: TToolBar;
    tbPurchaseEnabled: TToggleBox;
    tbPurchaseTaxable: TToggleBox;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    tsCommerce: TTabSheet;
    tsRaidGroup: TTabSheet;
    tstsRaidUser: TTabSheet;
    tsService: TTabSheet;
    tsRaid: TTabSheet;
    tbClusterNodeScale: TTrackBar;
    tsClusterNodes: TTabSheet;
    tsProcessGroup: TTabSheet;
    tsServiceUser: TTabSheet;
    TSUICore: TTabSheet;
    TSUIO: TTabSheet;
    tsUSGI: TTabSheet;
    tbbManage1: TToolButton;
    tbbKWBackup: TToolButton;
    tbbKWRestore: TToolButton;
    tbDSCerts: TToolBar;
    btnDSRefresh: TToolButton;
    btnDSReq: TToolButton;
    btnDSNC: TToolButton;
    btnDSSSC: TToolButton;
    btnDSLC: TToolButton;
    btnDSDelete: TToolButton;
    ToolButton14: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    btnDSSet: TToolButton;
    ToolButton2: TToolButton;
    tvClusterResources: TTreeView;
    tvDomainClustering: TTreeView;
    tvDBMGroups: TTreeView;
    gbProviderServiceSettings: TGroupBox;
    gbProviderQueryStringLandingPage: TGroupBox;
    gbQueryString: TGroupBox;
    gbProviderLandingPage: TGroupBox;
    gbManagerResults: TGroupBox;
    GroupBox12: TGroupBox;
    gbDBMTableProperties: TGroupBox;
    gbDBMContent: TGroupBox;
    Label109: TLabel;
    Label110: TLabel;
    Label111: TLabel;
    Label112: TLabel;
    Label113: TLabel;
    Label114: TLabel;
    Label115: TLabel;
    Label116: TLabel;
    Label117: TLabel;
    Label118: TLabel;
    Label119: TLabel;
    Label81: TLabel;
    lvUserACLCoreObjects: TListView;
    lvClusters: TListView;
    lvProviderService: TListView;
    odFile: TOpenDialog;
    odFolder: TSelectDirectoryDialog;
    tsDatabaseModules: TTabSheet;
    tsStartup: TTabSheet;
    tbbKeywordLoad: TToolButton;
    tbbViewer: TToolButton;
    txtClusterNodeName: TEdit;
    txtCRNDisk: TEdit;
    txtCRNResource: TEdit;
    txtCluster_Building: TEdit;
    txtCluster_City: TEdit;
    txtCluster_Country: TEdit;
    txtCluster_Description: TEdit;
    txtCluster_Floor: TEdit;
    txtCluster_Name: TEdit;
    txtCluster_Room: TEdit;
    txtCluster_State: TEdit;
    txtCluster_Street: TEdit;
    txtCluster_Town: TEdit;
    txtCluster_Zipcode: TEdit;
    txtCstrIP0: TEdit;
    txtCstrIP1: TEdit;
    txtCstrIP2: TEdit;
    txtCstrIP3: TEdit;
    txtDBMTable: TEdit;
    txtDomainLicenseEdit: TEdit;
    txtDomain_FriendlyName: TEdit;
    txtDomain_Postmaster: TEdit;
    txtSecIPViolator: TEdit;
    txtHSBLHI: TEdit;
    txtHSALHI: TEdit;
    txtHSTLD: TEdit;
    txtLicenseDescription: TEdit;
    txtSecCNDomain: TEdit;
    txtHSWLHI: TEdit;
    txtKeywordName: TEdit;
    gbSeachProviderDetails: TGroupBox;
    GroupBox40: TGroupBox;
    gbKeywordEditor: TGroupBox;
    GroupBox9: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    lvKeywords: TListView;
    tbbKeywordDelete: TToolButton;
    tbbKeywordEdit: TToolButton;
    tbbKeywordNew: TToolButton;
    tbSearchScale: TTrackBar;
    TB_Clusters3: TToolBar;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    tsKeywords: TTabSheet;
    tsSearchProviders: TTabSheet;
    tsSecurityContentFilters: TTabSheet;
    tsSecurityDNSBlackLists: TTabSheet;
    tsSecurityDNSWhiteLists: TTabSheet;
    tsSecurityDomainRules: TTabSheet;
    txtContentTypeKind: TEdit;
    gbContentTypeDetails: TGroupBox;
    gbDomains: TGroupBox;
    GroupBox18: TGroupBox;
    GroupBox34: TGroupBox;
    GroupBox35: TGroupBox;
    Label77: TLabel;
    lvContentTypes: TListView;
    lvDomains: TListView;
    lvDomainServices: TListView;
    lvSearchProviders:TListView;
    lvUsers: TListView;
    MenuItem10: TMenuItem;
    MI_PU_Cluster_Delete: TMenuItem;
    MI_PU_Cluster_Edit: TMenuItem;
    MI_PU_Cluster_New: TMenuItem;
    MI_PU_Domain_User_Unlock: TMenuItem;
    MI_PU_Domain_User_Delete: TMenuItem;
    MenuItem9: TMenuItem;
    MI_PU_Domain_User_Edit: TMenuItem;
    MI_PU_Domain_User_New: TMenuItem;
    PU_Clusters: TPopupMenu;
    PU_Domain_Users: TPopupMenu;
    tsContentTypes: TTabSheet;
    tbbClusterNodeDelete: TToolButton;
    tbbClusterNodeEdit: TToolButton;
    tbbClustersNewNode: TToolButton;
    TB_Clusters2: TToolBar;
    ToolButton10: TToolButton;
    ToolButton9: TToolButton;
    tsDomainClustering: TTabSheet;
    txtCluster_New_Description: TEdit;
    txtContentTypeExtension: TEdit;
    txtKeywordValue: TEdit;
    txtProcessGroupID: TEdit;
    txtPurchaseDescription: TEdit;
    txtPurchaseTitle: TEdit;
    txtRaidGroupID: TEdit;
    txtProcessGroupName: TEdit;
    txtRaidGroupName: TEdit;
    txtProcessUserID: TEdit;
    txtRaidUserID: TEdit;
    txtProcessUserName: TEdit;
    txtRaidUserName: TEdit;
    txtProviderDomain: TEdit;
    txtProviderMaxResults: TEdit;
    txtProviderName: TEdit;
    txtProviderPort: TEdit;
    txtProviderQueryString: TEdit;
    Label89: TLabel;
    txtSecContent: TEdit;
    txtSecIPViolatorSearch: TEdit;
    txtSecDMBLS: TEdit;
    txtSecDMALS: TEdit;
    txtSecProfileSearch: TEdit;
    txtSecTLDS: TEdit;
    txtSecProfile: TEdit;
    txtSecDMWLS: TEdit;
    txtSecContentSearch: TEdit;
    txtUsers_New_Account: TEdit;
    txtUser_Edit_First: TEdit;
    txtUser_Edit_Last: TEdit;
    txtUsers_New_First: TEdit;
    txtUsers_New_Last: TEdit;
    GroupBox14: TGroupBox;
    gbDomain_Users_New: TGroupBox;
    Label66: TLabel;
    Label67: TLabel;
    Label84: TLabel;
    pnlDomainUserFind: TPanel;
    tbbDomain_User_Unlock: TToolButton;
    ToolButton6: TToolButton;
    tsUser: TTabSheet;
    tbbDomain_User_Delete: TToolButton;
    tbbDomain_User_Edit: TToolButton;
    tbbDomain_User_New: TToolButton;
    btnWLDNSItemAdd: TButton;
    btnWLDNSItemRemove: TButton;
    cbDomain_Service: TCheckBox;
    cbDomain_Defaults: TCheckListBox;
    gbDomainServiceSettings: TGroupBox;
    gbDomains_DomainInformation: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox31: TGroupBox;
    GroupBox32: TGroupBox;
    IL_24x24: TImageList;
    Label83: TLabel;
    Label85: TLabel;
    Label86: TLabel;
    pcDomain: TPageControl;
    tbUserAccounts: TToolBar;
    ToolButton1: TToolButton;
    ToolButton15: TToolButton;
    tbbDomain_Users_Find: TToolButton;
    tsDomainUsers: TTabSheet;
    tsDomainSecurity: TTabSheet;
    tsDomainDefaults: TTabSheet;
    tbDomainScale: TTrackBar;
    tsDomainScale: TTabSheet;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    txtDomains_Postmaster: TEdit;
    txtDNSIP1: TEdit;
    txtDNSIP2: TEdit;
    txtDNSIP3: TEdit;
    txtDNSIP4: TEdit;
    txtSecBlackListDNS: TEdit;
    txtDomains_FriendlyName: TEdit;
    txtCluster_New_Name: TEdit;
    txtSecWhiteListDNS: TEdit;
    txtDomains_Hostname: TEdit;
    gbClusters_ClusterInfo: TGroupBox;
    GroupBox25: TGroupBox;
    gbDNSEditor: TGroupBox;
    GroupBox27: TGroupBox;
    GroupBox28: TGroupBox;
    GroupBox29: TGroupBox;
    GroupBox30: TGroupBox;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label73: TLabel;
    lvDNSRegular: TListView;
    lvDNSBL: TListView;
    lvDNSWL: TListView;
    TabSheet10: TTabSheet;
    TabSheet13: TTabSheet;
    TabSheet21: TTabSheet;
    Image1: TImage;
    Label61: TLabel;
    Label62: TLabel;
    tsWelcome: TTabSheet;
    GroupBox1: TGroupBox;
    GroupBox10: TGroupBox;
    GroupBox11: TGroupBox;
    gbCRNode: TGroupBox;
    lvClusterNodes: TListView;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MI_File_New_Domain: TMenuItem;
    MI_File_New_Cluster: TMenuItem;
    MI_File_New_Node: TMenuItem;
    MI_File_New_User: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MI_File_New: TMenuItem;
    pcPages: TPageControl;
    Splitter1: TSplitter;
    tsDomains: TTabSheet;
    tsDomain: TTabSheet;
    tsPrefDNS: TTabSheet;
    tsClusters: TTabSheet;
    tsCluster: TTabSheet;
    TabSheet8: TTabSheet;
    TabSheet9: TTabSheet;
    ToolBar2: TToolBar;
    TB_Clusters: TToolBar;
    ToolButton11: TToolButton;
    btnDomainsNew: TToolButton;
    ToolButton3: TToolButton;
    btnDomainsDelete: TToolButton;
    btnDomainsEdit: TToolButton;
    tbbClustersNew: TToolButton;
    tbbClustersDelete: TToolButton;
    tbbClustersEdit: TToolButton;
    tvNavigation: TTreeView;
    txtUsersFind: TEdit;
    txtUsers_New_Password: TEdit;
    txtUser_Edit_Password: TEdit;
    txtUser_Edit_Telephone: TEdit;
    procedure btnBLDNSItemAddClick(Sender: TObject);
    procedure btnBLDNSItemSaveClick(Sender: TObject);
    procedure btnClusterCancel1Click(Sender: TObject);
    procedure btnClusterCancel2Click(Sender: TObject);
    procedure btnClusterCancelClick(Sender: TObject);
    procedure btnClusterNewClick(Sender: TObject);
    procedure btnClusterNewNodeCancelClick(Sender: TObject);
    procedure btnClusterNewNodeClick(Sender: TObject);
    procedure btnCluster_DoneClick(Sender: TObject);
    procedure btnContentTypeAddClick(Sender: TObject);
    procedure btnContentTypeRemoveClick(Sender: TObject);
    procedure btnContentTypeSaveClick(Sender: TObject);
    procedure btnCRNResourceCancelClick(Sender: TObject);
    procedure btnCRNResourceOkClick(Sender: TObject);
    procedure btnDCAAllocateClick(Sender: TObject);
    procedure btnDCADeallocateClick(Sender: TObject);
    procedure btnDiskNodeRefreshClick(Sender: TObject);
    procedure btnDSDeleteClick(Sender: TObject);
    procedure btnDSNCClick(Sender: TObject);
    procedure btnDSRefreshClick(Sender: TObject);
    procedure btnDSReqClick(Sender: TObject);
    procedure btnDSSetClick(Sender: TObject);
    procedure btnDSSSCClick(Sender: TObject);
    procedure btnHSALAdd1Click(Sender: TObject);
    procedure btnHSALAddClick(Sender: TObject);
    procedure btnSecIPViolatorSaveClick(Sender: TObject);
    procedure btnSecIpViolatorSearchClick(Sender: TObject);
    procedure btnSecIPViolatorsReloadClick(Sender: TObject);
    procedure btnHSALReloadClick(Sender: TObject);
    procedure btnHSALRemove1Click(Sender: TObject);
    procedure btnHSALRemoveClick(Sender: TObject);
    procedure btnHSALSaveClick(Sender: TObject);
    procedure btnHSALSearchClick(Sender: TObject);
    procedure btnHSBLAddClick(Sender: TObject);
    procedure btnHSBLReloadClick(Sender: TObject);
    procedure btnHSBLRemoveClick(Sender: TObject);
    procedure btnHSBLSaveClick(Sender: TObject);
    procedure btnHSBLSearchClick(Sender: TObject);
    procedure btnHSCNSearchClick(Sender: TObject);
    procedure btnHSTLDAddClick(Sender: TObject);
    procedure btnHSTLDReloadClick(Sender: TObject);
    procedure btnHSTLDRemoveClick(Sender: TObject);
    procedure btnHSTLDSaveClick(Sender: TObject);
    procedure btnHSTLDSearchClick(Sender: TObject);
    procedure btnHSWLAddClick(Sender: TObject);
    procedure btnHSWLReloadClick(Sender: TObject);
    procedure btnHSWLRemoveClick(Sender: TObject);
    procedure btnHSWLSaveClick(Sender: TObject);
    procedure btnKeywordEditingDoneClick(Sender: TObject);
    procedure btnDomainsDeleteClick(Sender: TObject);
    procedure btnDomain_User_DoneClick(Sender: TObject);
    procedure btnProviderLandingPageEditClick(Sender: TObject);
    procedure btnRaidChangeGroupClick(Sender: TObject);
    procedure btnRaidUserChangeClick(Sender: TObject);
    procedure btnSecContentSearchClick(Sender: TObject);
    procedure btnSecIPViolatorsToolClearClick(Sender: TObject);
    procedure btnSecIPVoiolatorAddClick(Sender: TObject);
    procedure btnSecIPVRemoveClick(Sender: TObject);
    procedure btnSecProfileAddClick(Sender: TObject);
    procedure btnSecProfileContentSaveClick(Sender: TObject);
    procedure btnSecContentSaveClick(Sender: TObject);
    procedure btnSecProfileDelClick(Sender: TObject);
    procedure btnSecProfileSearchClick(Sender: TObject);
    procedure btnUser_Edit_CancelClick(Sender: TObject);
    procedure btnDomainsNew_OKClick(Sender: TObject);
    procedure btnDomainsEditClick(Sender: TObject);
    procedure btnDomainsNewClick(Sender: TObject);
    procedure btnDomain_User_New_DoneClick(Sender: TObject);
    procedure btnWLDNSItemAddClick(Sender: TObject);
    procedure btnDNSItemAddClick(Sender: TObject);
    procedure btnBLDNSItemRemoveClick(Sender: TObject);
    procedure btnWLDNSItemRemoveClick(Sender: TObject);
    procedure btnDNSItemRemoveClick(Sender: TObject);
    procedure btnProcessGroupChangeClick(Sender: TObject);
    procedure btnProcessUserChangeClick(Sender: TObject);
    procedure btnSecContentAddClick(Sender: TObject);
    procedure btnSecContentDelClick(Sender: TObject);
    procedure btnWLDNSItemSaveClick(Sender: TObject);
    procedure cbClusterNodeEnabledClick(Sender: TObject);
    procedure cbDCDomainsChange(Sender: TObject);
    procedure cbDomain_DefaultsItemClick(Sender: TObject; Index: integer);
    procedure cbDomain_ServiceChange(Sender: TObject);
    procedure cmboDiskClustersChange(Sender: TObject);
    procedure cmboDiskResourcesChange(Sender: TObject);
    procedure cmboPurchaseKindChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lvCertsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvClusterNodeServiceSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvClusterNodesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvContentFiltersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvDNSRegularSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvHSALSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvHSBLSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvProfileFiltersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvPurchaseSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvSecCNDomainDblClick(Sender: TObject);
    procedure lvSecCNDomainSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvSecIPViolatorsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvSecIPViolatorsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvUserACLCoreCommandsDblClick(Sender: TObject);
    procedure lvUserACLCoreObjectsDblClick(Sender: TObject);
    procedure lvUserACLCoreObjectsItemChecked(Sender: TObject; Item: TListItem);
    procedure miDomainCertImportClick(Sender: TObject);
    procedure miLoadServerIntermediateCertClick(Sender: TObject);
    procedure miLoadRootServerIntCertClick(Sender: TObject);
    procedure miCOACLGrantAllClick(Sender: TObject);
    procedure miCODenyCheckedClick(Sender: TObject);
    procedure miCODenySelectedClick(Sender: TObject);
    procedure miClusterNodeEditClick(Sender: TObject);
    procedure miClusterResourceEdit1Click(Sender: TObject);
    procedure miClusterRNDelete1Click(Sender: TObject);
    procedure miClusterRNDeleteClick(Sender: TObject);
    procedure miCOACLGrantCheckedClick(Sender: TObject);
    procedure miCOACLGrantSelectedClick(Sender: TObject);
    procedure miDenyAllClick(Sender: TObject);
    procedure miCRNNewNodeClick(Sender: TObject);
    procedure miCRNNewResourceClick(Sender: TObject);
    procedure miLoadCertClick(Sender: TObject);
    procedure miLoadCertIntClick(Sender: TObject);
    procedure miLoadCertIntRootClick(Sender: TObject);
    procedure miSecCNDALClick(Sender: TObject);
    procedure miSecCNDBLClick(Sender: TObject);
    procedure miSecCNDDeleteClick(Sender: TObject);
    procedure miSecCNDWLClick(Sender: TObject);
    procedure pcDomainChange(Sender: TObject);
    procedure puClusterNewRNPopup(Sender: TObject);
    procedure sePurchasePriceChange(Sender: TObject);
    procedure sgDomainLicenseResize(Sender: TObject);
    procedure sgDomainLicenseSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
    procedure tbbClusterNodeDeleteClick(Sender: TObject);
    procedure tbbPZBackupClick(Sender: TObject);
    procedure tbbKWBackupClick(Sender: TObject);
    procedure tbbKWRestoreClick(Sender: TObject);
    procedure tbbManage1Click(Sender: TObject);
    procedure tbbPurchaseDeleteClick(Sender: TObject);
    procedure tbbPurchaseEditClick(Sender: TObject);
    procedure tbbPurchaseNewClick(Sender: TObject);
    procedure tbbPZRestoreClick(Sender: TObject);
    procedure tbbViewerClick(Sender: TObject);
    procedure tbClusterNodeScaleClick(Sender: TObject);
    procedure tbPurchaseEnabledChange(Sender: TObject);
    procedure tbPurchaseTaxableChange(Sender: TObject);
    procedure tsClustersShow(Sender: TObject);
    procedure tvClusterResourcesDblClick(Sender: TObject);
    procedure tvDomainClusteringSelectionChanged(Sender: TObject);
    procedure txtCRNResourceChange(Sender: TObject);
    procedure txtCstrIP0Change(Sender: TObject);
    procedure txtCstrIP0KeyPress(Sender: TObject; var Key: char);
    procedure txtCstrIP1Change(Sender: TObject);
    procedure txtCstrIP2Change(Sender: TObject);
    procedure txtCstrIP3Change(Sender: TObject);
    procedure txtDNSIP1Change(Sender: TObject);
    procedure txtDNSIP2Change(Sender: TObject);
    procedure txtDNSIP3Change(Sender: TObject);
    procedure txtDNSIP4Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvContentTypesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure lvDomainsColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvDomainsCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvDomainServicesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvHSWLSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvProviderServiceSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvSearchProvidersSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvUserACLCoreObjectsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lvUsersDblClick(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MI_File_NewClick(Sender: TObject);
    procedure pcDomainChanging(Sender: TObject; var AllowChange: Boolean);
    procedure PU_ClustersPopup(Sender: TObject);
    procedure PU_Domain_UsersPopup(Sender: TObject);
    procedure seDomain_Default_QuotaChange(Sender: TObject);
    procedure tbbClustersNewClick(Sender: TObject);
    procedure tbbClustersNewNodeClick(Sender: TObject);
    procedure tbbKeywordDeleteClick(Sender: TObject);
    procedure tbbKeywordEditClick(Sender: TObject);
    procedure tbbKeywordLoadClick(Sender: TObject);
    procedure tbbKeywordNewClick(Sender: TObject);
    procedure tbSearchScaleChange(Sender: TObject);
    procedure tbbClusterNodeEditClick(Sender: TObject);
    procedure tbbClustersEditClick(Sender: TObject);
    procedure tbbDomain_Users_FindClick(Sender: TObject);
    procedure tbbDomain_User_DeleteClick(Sender: TObject);
    procedure tbbDomain_User_NewClick(Sender: TObject);
    procedure tbbDomain_User_UnlockClick(Sender: TObject);
    procedure tbDomainScaleChange(Sender: TObject);
    procedure tvClusterResourcesSelectionChanged(Sender: TObject);
    procedure tvDBMGroupsSelectionChanged(Sender: TObject);
    procedure tvNavigationSelectionChanged(Sender: TObject);
    procedure txtClusterNodeNameChange(Sender: TObject);
    procedure txtCluster_BuildingChange(Sender: TObject);
    procedure txtCluster_CityChange(Sender: TObject);
    procedure txtCluster_CountryChange(Sender: TObject);
    procedure txtCluster_DescriptionChange(Sender: TObject);
    procedure txtCluster_FloorChange(Sender: TObject);
    procedure txtCluster_NameChange(Sender: TObject);
    procedure txtCluster_New_NameChange(Sender: TObject);
    procedure txtCluster_RoomChange(Sender: TObject);
    procedure txtCluster_StateChange(Sender: TObject);
    procedure txtCluster_StreetChange(Sender: TObject);
    procedure txtCluster_TownChange(Sender: TObject);
    procedure txtCluster_ZipcodeChange(Sender: TObject);
    procedure txtDomainLicenseEditKeyPress(Sender: TObject; var Key: char);
    procedure txtDomains_FriendlyNameChange(Sender: TObject);
    procedure txtDomains_HostnameChange(Sender: TObject);
    procedure txtDomains_PostmasterChange(Sender: TObject);
    procedure txtDomain_FriendlyNameChange(Sender: TObject);
    procedure cbDomain_HostChange(Sender: TObject);
    procedure txtDomain_PostmasterChange(Sender: TObject);
    procedure txtHSBLHIChange(Sender: TObject);
    procedure txtHSWLHIChange(Sender: TObject);
    procedure txtProviderDomainChange(Sender: TObject);
    procedure txtProviderMaxResultsChange(Sender: TObject);
    procedure txtProviderNameChange(Sender: TObject);
    procedure txtProviderPortChange(Sender: TObject);
    procedure txtProviderQueryStringChange(Sender: TObject);
    procedure txtPurchaseDescriptionChange(Sender: TObject);
    procedure txtPurchaseTitleChange(Sender: TObject);
    procedure txtSecBlackListDNSChange(Sender: TObject);
    procedure txtSecCNDomainEnter(Sender: TObject);
    procedure txtSecCNDomainKeyPress(Sender: TObject; var Key: char);
    procedure txtSecContentSearchKeyPress(Sender: TObject; var Key: char);
    procedure txtSecDMALSKeyPress(Sender: TObject; var Key: char);
    procedure txtSecDMBLSKeyPress(Sender: TObject; var Key: char);
    procedure txtSecDMWLSKeyPress(Sender: TObject; var Key: char);
    procedure txtSecIPViolatorChange(Sender: TObject);
    procedure txtSecIPViolatorSearchKeyPress(Sender: TObject; var Key: char);
    procedure txtSecProfileSearchKeyPress(Sender: TObject; var Key: char);
    procedure txtSecTLDSKeyPress(Sender: TObject; var Key: char);
    procedure txtSecWhiteListDNSChange(Sender: TObject);
    procedure txtUsersFindKeyPress(Sender: TObject; var Key: char);
    procedure txtUsers_New_AccountChange(Sender: TObject);
    procedure txtUser_Edit_FirstChange(Sender: TObject);
    procedure txtUser_Edit_LastChange(Sender: TObject);
    procedure txtUser_Edit_PasswordChange(Sender: TObject);
    procedure txtUser_Edit_TelephoneChange(Sender: TObject);
  private
    FDNS_HOST_IP                     : Core.Arrays.Types.VarString; // Preferences/DNS Servers
    FDNS_BL_Service                  : Core.Arrays.Types.VarString; // Security/DNS BlackLists
    FDNS_WL_Service                  : Core.Arrays.Types.VarString; // Security/DNS WhiteLists
    // Settings
    FProcessGroupID                  : Storage.ConfigData.Items.Item;
    FProcessGroupName                : Storage.ConfigData.Items.Item;
    FProcessUserID                   : Storage.ConfigData.Items.Item;
    FProcessUserName                 : Storage.ConfigData.Items.Item;

    FRaidUserID                      : Storage.ConfigData.Items.Item;
    FRaidGroupID                     : Storage.ConfigData.Items.Item;
    FRaidUserName                    : Storage.ConfigData.Items.Item;
    FRaidGroupName                   : Storage.ConfigData.Items.Item;

    {$ifdef Unix}
    FUserNameProcess                 : TNixUser;
    {$endif}
    // Security Features
    Security_TLD_List                : Storage.Security.Filter.Items;
    Security_Content_Filters         : Storage.Security.Filter.Items;
    Security_Block_List              : Storage.Security.Filter.Items;
    Security_Allow_List              : Storage.Security.Filter.Items;
    Security_IP_Violators_List       : Storage.Security.Filter.Items;
    Security_Acceptable_List         : Storage.Security.Filter.Items;
    Security_Content_Profiles        : Storage.Security.Filter.Items;
    Security_WL_Services             : Storage.Security.Filter.Items;
    Security_BL_Services             : Storage.Security.Filter.Items;
    Security_Connections             : Storage.Security.Filter.Items;
    Security_Item                    : Storage.Security.Filter.Item;
    Security_Profile_Search          : Storage.Security.Filter.Items;
    Security_IP_Violator_Search      : Storage.Security.Filter.Items;
    Security_WhiteList_Search        : Storage.Security.Filter.Items;
    Security_BlackList_Search        : Storage.Security.Filter.Items;
    Security_AcceptableList_Search   : Storage.Security.Filter.Items;
    Security_TLDS_Search             : Storage.Security.Filter.Items;
    Security_Content_Search          : Storage.Security.Filter.Items;

    // DNS Servers (Regular, BlackLists, WhiteLists)
    DNS_Hosts                        : Storage.DNS.Items.List;
    // Users
    FFindUsers                       : Storage.UserAccounts.Items.TList;
    FCoreObject                      : TCoreObject;
    FCoreCommand                     : PCoreCommand;
    // Domains
    Domains                          : Storage.Domains.Items.TDomains;
    FDomainP                         : Storage.Domains.Items.PDomain;
    FCertP                           : Encryption.SSL.PCertData;
    Root                             : Storage.UserAccounts.Items.Item;
    Default                          : Storage.UserAccounts.Items.Item;
    FContact                         : Storage.Roster.Items.Item;
    FClusterP                        : Storage.MatrixClusters.Cluster.PItem;
    FUAP                             : Storage.UserAccounts.Items.PItem;
    FUAPL                            : Storage.UserAccounts.Items.UAPList;
    FUserP                           : Storage.UserAccounts.Items.PItem;
    FDomainNodes                     : Storage.MatrixNodes.Node.Items; // Must use master node list
    FDomainDefaultServices           : Storage.MatrixServices.Items.Defaults;
    FDomainServices                  : Storage.MatrixServices.Items.Manifest;
    FDomainServiceP                  : Storage.MatrixServices.Items.PItem;
    FDomainNode                      : Storage.MatrixNodes.Node.Item;
    FDomainCluster                   : Storage.MatrixClusters.Cluster.Item;
    FDomainResource                  : Storage.MatrixResources.Resource.Item;
    FCoreObjects                     : TCoreObjects;
    FCerts                           : TCertList;

    TI_DomainServices                : Core.Timer.Item;
    TI_CertSelect                    : Core.Timer.Item;
    TI_ContentTypeSelect             : Core.Timer.Item;
    TI_DNSSelect                     : Core.Timer.Item;
    TI_SecContSelect                 : Core.Timer.Item;
    TI_SecProfileSelect              : Core.Timer.Item;
    TI_SecBLSSelect                  : Core.Timer.Item;
    TI_SecWLSSelect                  : Core.Timer.Item;
    TI_SecWLDSelect                  : Core.Timer.Item;
    TI_SecIPVSelect                  : Core.Timer.Item;
    TI_SecBLDSelect                  : Core.Timer.Item;
    TI_SecTLDSelect                  : Core.Timer.Item;
    TI_SecALDSelect                  : Core.Timer.Item;
    TI_DNS_Change                    : Core.Timer.Item;

    FContentTypeP                    : PContentType;

    // Purchases
    FPurchases                       : Purchase.Item.TItems;
    FPurchaseP                       : Purchase.Item.PItem;
    TI_Purchase                      : Core.Timer.Item;

    // Services
    Services                         : Storage.MatrixServices.Items.Items;
    FDefaultService                  : Storage.MatrixServices.Items.Item;
    FDefaultServices                 : Storage.MatrixServices.Items.Defaults;

    TI_NodeServiceSelection          : Core.Timer.Item;
    // Clusters
    FClusters                        : Storage.MatrixClusters.Cluster.Items;
    FClusterResources                : Storage.MatrixResources.Resource.Items;
    FClusterResourceP                : Storage.MatrixResources.Resource.PItem;
    FClusterNodes                    : Storage.MatrixNodes.Node.Items;
    FClusterNodeP                    : Storage.MatrixNodes.Node.PItem;
    FClusterNode_IP                  : Core.Arrays.Types.VarString;
    // Nodes
    Nodes                            : Storage.MatrixNodes.Node.Items;
    FDisks                           : Storage.MatrixNodes.Node.Items;
    FDiskResource                    : Storage.MatrixResources.Resource.Items;
    FAuraDisks                       : Storage.MatrixNodes.Node.Items;
    FNodeServices                    : Storage.MatrixServices.Items.Items;
    FNode_IP                         : Core.Arrays.Types.VarString;
    // Search
    FSearchProviders                 : Storage.SrchProviders.Items.List;
    FSearchProviderP                 : Storage.SrchProviders.Items.PItem;
    // Keywords
    FKeywords                        : Core.Keywords.TKeywords;
    FKeywordP                        : Core.Keywords.PKeyword;

    GUI                              : Core.Lock.Boolean.TItem;

    saLocateLevels                   : Core.Arrays.Types.VarString;
  private
    FManagerFat                      : TDSFAT;
    FInfoP                           : PFormInfo;
  private
    { private declarations }
    function  LocatePage(sString:Core.Strings.VarString):TTabsheet;
    function  ParseNodesForPath(Node:TTreeNode):Core.Strings.VarString;
    procedure UpdateSelectedDNSServer(Kind:Byte);
    procedure OnSelectPurchase(ItemP:Core.Timer.PItem);

    procedure OnNodeServiceSelection(ItemP:Core.Timer.PItem);
    procedure OnDomainServiceSelection(ItemP:Core.Timer.PItem);
    procedure OnDomainCertSelect(ItemP:Core.Timer.PItem);
    procedure OnContentTypeSelect(ItemP:Core.Timer.PItem);
    procedure OnDNSSelect(ItemP:Core.Timer.PItem);
    procedure OnDNS_Change(ItemP:Core.Timer.PItem);
    procedure OnSecContSelect(ItemP:Core.Timer.PItem);
    procedure OnSecProfileSelect(ItemP:Core.Timer.PItem);
    procedure OnSecWLSSelect(ItemP:Core.Timer.PItem);
    procedure OnSecBLSSelect(ItemP:Core.Timer.PItem);
    procedure OnSecWLDSelect(ItemP:Core.Timer.PItem);
    procedure OnSecIPVSelect(ItemP:Core.Timer.PItem);
    procedure OnSecTLDSelect(ItemP:Core.Timer.PItem);
    procedure OnSecBLDSelect(ItemP:Core.Timer.PItem);
    procedure OnSecALDSelect(ItemP:Core.Timer.PItem);


    procedure LoadUserACLCoreObjects(UserP:Storage.UserAccounts.Items.PItem);
    procedure TableValueChange(Sender:TObject);
    procedure LoadDBMSSettings;
    procedure ClearClusterLayoutManager;
    procedure LoadClusterLayoutManager;
    procedure LoadDomainClustering;
    function  cbKW_Loopback(ItemP:PKeyword):Core.Strings.VarString;

    {$i frmConsole.SecDomainFilter.Decs.inc}

  public
    { public declarations }
    procedure ProcessStartup;
    procedure LoadSettings;
    procedure LoadDomains; overload;
    procedure LoadFilterItems(Const Kind:TDBSecurityKind; var Items:Storage.Security.Filter.Items; ListView:TListView); overload;
    procedure LoadClusters;
    procedure LoadContentTypes;

    procedure LoadNodes;       // Master List
    procedure LoadSearchProviders;
    procedure LoadKeywords;
    procedure LoadSSLInfo;
    procedure LoadPurchase();
    procedure LoadPurchases();
    procedure LoadCertInfo(Var Cert:TCertData);
  public
    procedure Display_Welcome;
    procedure Display_Domain_Editor(DomainP:Storage.Domains.Items.PDomain);
    procedure Display_Cluster_Editor(ClusterP:Storage.MatrixClusters.Cluster.PItem);
  end;

var
  ConsoleForm: TConsoleForm;
const
  LI_IDX_DOMAINS_USERCOUNT = 0;
  LI_IDX_DOMAINS_NAME      = 1;

  LI_IDX_USER_NAME         = 1;

implementation
uses
  frmCertReq,
  frmRSA,
  frmServices,
  frmEdit,
  frmImage,
  frmLogin,
  Form.YesNo,
  frmProvider,

  {$i coList.Uses.inc},

  frmFileMan,
  frmKeyword,
  DateUtils;

{ TConsoleForm }

procedure TConsoleForm.Display_Welcome;
begin
  tvNavigation.Selected:=tvNavigation.Items.GetFirstNode;
end;

function TConsoleForm.ParseNodesForPath(Node:TTreeNode):Core.Strings.VarString;
begin
  SetLength(Result,0);
  if Node<>nil then
    Result:=Concat(ParseNodesForPath(Node.Parent),'/',Node.Text);
end;

function TConsoleForm.LocatePage(sString:Core.Strings.VarString):TTabsheet;
var
  iLcv:integer;
  jLcv:integer;
  iLevel:integer;
  iLevels:integer;
  pgsLcv:TPageControl;
  tsLcv:TTabSheet;
begin
  Result:=nil; iLevel:=0; tsLcv:=nil;
  iLevels:=Core.Arrays.VarString.fromString(saLocateLevels,sString,'/',[soClearList,soIgnoreDelimAtStart]);
  pgsLcv:=pcPages;
  While (iLevel<iLevels) and (Result=nil) do begin
    for iLcv:=0 to pgsLcv.PageCount-1 do begin
      if SameText(saLocateLevels[iLevel],pgsLcv.Pages[iLcv].Caption) then begin
        // This could be the page
        tsLcv:=pgsLcv.Pages[iLcv];
        pgsLcv.ActivePage:=tsLcv;
        if (iLevel+1>=iLevels) then begin
          Result:=tsLcv;
        end else begin
          // SubPages
          // Find the first subPages
          for jLcv:=0 to tsLcv.ControlCount-1 do begin
            if tsLcv.Controls[jLcv] is TPageControl then begin
              pgsLcv:=tsLcv.Controls[jLcv] as TPageControl;
              break;
            end;
          end;
        end;
        break;
      end;
    end;
    Inc(iLevel);
  end;
end;

procedure TConsoleForm.tvNavigationSelectionChanged(Sender: TObject);
var
  sPath:Core.Strings.VarString;
  tsPage:TTabsheet;
begin
  // Select propper page.
  if tvNavigation.Selected<>nil then begin
    sPath:=ParseNodesForPath(tvNavigation.Selected);
    tsPage:=LocatePage(sPath);
    if tsPage=nil then pcPages.ActivePage:=tsWelcome;
  end;
end;

procedure TConsoleForm.txtClusterNodeNameChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  btnClusterNewNode.Enabled:=Length(txtClusterNodeName.Text)>0;
end;

procedure TConsoleForm.txtCluster_BuildingChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Building:=txtCluster_Building.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_CityChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Area:=txtCluster_City.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_CountryChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Country:=txtCluster_Country.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_DescriptionChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Description:=txtCluster_Description.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_FloorChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Floor:=txtCluster_Floor.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_NameChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Group:=txtCluster_Name.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_New_NameChange(Sender: TObject);
begin
  btnClusterNew.Enabled:=(Length(txtCluster_New_Name.Text)>0);
end;

procedure TConsoleForm.txtCluster_RoomChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Room:=txtCluster_Room.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_StateChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Region:=txtCluster_State.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_StreetChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Street:=txtCluster_Street.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_TownChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Locality:=txtCluster_Town.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtCluster_ZipcodeChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FClusterP^.Location.Zip:=txtCluster_Zipcode.Text;
  Storage.MatrixClusters.Cluster.DB.Save(Storage.Main.Task,FClusterP^);
end;

procedure TConsoleForm.txtDomainLicenseEditKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#27) then begin
    Key:=#0;
    txtDomainLicenseEdit.Clear();
    txtDomainLicenseEdit.Visible:=false;
    sgDomainLicense.EditingDone();
    sgDomainLicense.SetFocus();
  end else if (Key=#9) or (Key=#13) then begin
    Key:=#0;
    sgDomainLicense.Cells[sgDomainLicense.Col,sgDomainLicense.Row]:=txtDomainLicenseEdit.Text;
    txtDomainLicenseEdit.Clear();
    txtDomainLicenseEdit.Visible:=false;
    sgDomainLicense.EditingDone();
    sgDomainLicense.SetFocus();
  end;
end;

procedure TConsoleForm.txtDomains_FriendlyNameChange(Sender: TObject);
begin
  {$i frmConsole.Domains.New.Input.Verify.inc}
end;

procedure TConsoleForm.txtDomains_HostnameChange(Sender: TObject);
begin
  {$i frmConsole.Domains.New.Input.Verify.inc}
end;

procedure TConsoleForm.txtDomains_PostmasterChange(Sender: TObject);
begin
  {$i frmConsole.Domains.New.Input.Verify.inc}
end;

procedure TConsoleForm.txtDomain_FriendlyNameChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FDomainP^.FriendlyName:=txtDomain_FriendlyName.Text;
  Storage.Domains.Items.DB.Update(Storage.Main.Task,FDomainP^);
end;

procedure TConsoleForm.cbDomain_HostChange(Sender: TObject);
var
  iIndex:Integer;
begin
  if GUI.Locked then exit;
  // Technically we're changing the current domain to another domain.
  // We can do lookups later but for now just a combo box.
  iIndex:=Storage.Domains.Items.IndexOf(Domains,cbDomain_Host.Text);
  if iIndex<>-1 then
    Display_Domain_Editor(@Domains[iIndex]);
end;

procedure TConsoleForm.txtDomain_PostmasterChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  FDomainP^.Root:=txtDomain_Postmaster.Text;
  Storage.Domains.Items.DB.Update(Storage.Main.Task,FDomainP^);
end;

procedure TConsoleForm.txtHSBLHIChange(Sender: TObject);
begin
  btnHSBLSave.Enabled:=Length(txtHSBLHI.Text)>0;
end;

procedure TConsoleForm.txtHSWLHIChange(Sender: TObject);
begin
  btnHSWLSave.Enabled:=Length(txtHSWLHI.Text)>0;
end;

procedure TConsoleForm.txtProviderDomainChange(Sender: TObject);
begin
  if GUI.Locked or (FSearchProviderP=nil) then exit;
  FSearchProviderP^.Domain:=txtProviderDomain.Text;
  Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FSearchProviderP^);
end;

procedure TConsoleForm.txtProviderMaxResultsChange(Sender: TObject);
begin
  if GUI.Locked or (FSearchProviderP=nil) then exit;
  FSearchProviderP^.MaxResults:=StrToIntDef(txtProviderMaxResults.Text,0);
  Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FSearchProviderP^);
end;

procedure TConsoleForm.txtProviderNameChange(Sender: TObject);
var
  liItem:TListItem;
begin
  if GUI.Locked or (FSearchProviderP=nil) then exit;
  FSearchProviderP^.Caption:=txtProviderName.Text;
  Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FSearchProviderP^);
  if Core.Utils.ListView.IndexOf(lvSearchProviders,Pointer(FSearchProviderP^.ID),liItem)<>-1 then
    liItem.Caption:=FSearchProviderP^.Caption;
end;

procedure TConsoleForm.txtProviderPortChange(Sender: TObject);
begin
  if GUI.Locked or (FSearchProviderP=nil) then exit;
  FSearchProviderP^.PORT:=StrToIntDef(txtProviderPort.Text,0);
  Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FSearchProviderP^);
end;

procedure TConsoleForm.txtProviderQueryStringChange(Sender: TObject);
begin
  if GUI.Locked or (FSearchProviderP=nil) then exit;
  Storage.SrchProviders.Items.SetQueryString(lvProviderService.Selected.Caption,txtProviderQueryString.Text,FSearchProviderP^);
  Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FSearchProviderP^);
end;

procedure TConsoleForm.txtPurchaseDescriptionChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  if FPurchaseP<>nil then begin
    FPurchaseP^.Description:=txtPurchaseDescription.Text;
    lvPurchase.Selected.SubItems[5]:=FPurchaseP^.Description;
    Storage.Commerce.Purchase.Item.DB.Write(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  end;
end;

procedure TConsoleForm.txtPurchaseTitleChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  if FPurchaseP<>nil then begin
    FPurchaseP^.Title:=txtPurchaseTitle.Text;
    lvPurchase.Selected.SubItems[4]:=FPurchaseP^.Title;
    Purchase.Item.DB.Write(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  end;
end;

procedure TConsoleForm.txtSecBlackListDNSChange(Sender: TObject);
begin
  btnBLDNSItemSave.Enabled:=(txtSecBlackListDNS.GetTextLen>0);
end;

procedure TConsoleForm.txtSecCNDomainEnter(Sender: TObject);
begin
  txtSecCNDomain.SelectAll();
end;

procedure TConsoleForm.txtSecCNDomainKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#13) then begin
    Key:=#0;
    btnHSCNSearchClick(Sender);
  end;
end;

procedure TConsoleForm.txtSecContentSearchKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#13) then begin
    Key:=#0;
    btnSecContentSearchClick(Sender);
  end;
end;

procedure TConsoleForm.txtSecDMALSKeyPress(Sender: TObject; var Key: char);

begin
  if (Key=#13) then begin
    Key:=#0;
    btnHSALSearchClick(Sender);
  end;
end;

procedure TConsoleForm.txtSecDMBLSKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#13) then begin
    Key:=#0;
    btnHSBLSearchClick(Sender);
  end;
end;

procedure TConsoleForm.txtSecDMWLSKeyPress(Sender: TObject; var Key: char);

  procedure Expires_Today;
  var
    dtSearch:double;
    iDayOfYear,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateDay(dtSearch,iYear,iDayOfYear);
    dtSearch:=DateUtils.EndOfADay(iYear,iDayOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secWhiteList,txtSecDMWLS.Text,dtSearch,Security_WhiteList_Search);
  end;

  procedure Expires_Week;
  var
    dtSearch:double;
    iWeekOfYear,iDayOfWeek,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateWeek(dtSearch,iYear,iWeekOfYear,iDayOfWeek);
    dtSearch:=DateUtils.EndOfAWeek(iYear,iWeekOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secWhiteList,txtSecDMWLS.Text,dtSearch,Security_WhiteList_Search);
  end;

  procedure Expires_Month;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAMonth(iYear,iMonth);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secWhiteList,txtSecDMWLS.Text,dtSearch,Security_WhiteList_Search);
  end;

  procedure Expires_Year;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAYear(iYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secWhiteList,txtSecDMWLS.Text,dtSearch,Security_WhiteList_Search);
  end;
begin
  if (Key=#13) then begin
    Key:=#0;
    txtSecDMWLS.Text:=Lowercase(txtSecDMWLS.Text);
    case cmboSecDmWLSC.ItemIndex of
      1 : Expires_Today();
      2 : Expires_Week();
      3 : Expires_Month();
      4 : Expires_Year();
      else begin
        Storage.Security.Filter.DB.Find(Storage.Main.Task,secWhiteList,txtSecDMWLS.Text,Security_WhiteList_Search);
      end;
    end;
    LoadFilterItems(secWhiteList,Security_WhiteList_Search,lvHSWL);
  end;
end;

procedure TConsoleForm.txtSecIPViolatorChange(Sender: TObject);
begin
  btnSecIPViolatorSave.Enabled:=Length(txtSecIPViolator.Text)>0;
end;

procedure TConsoleForm.txtSecIPViolatorSearchKeyPress(Sender: TObject;
  var Key: char);
begin
  if (Key=#13) then begin
    Key:=#0;
    btnSecIpViolatorSearch.Click();
  end;
end;

procedure TConsoleForm.txtSecProfileSearchKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#13) then begin
    Key:=#0;
    btnSecProfileSearchClick(Sender);
  end;
end;

procedure TConsoleForm.txtSecTLDSKeyPress(Sender: TObject; var Key: char);
begin
  if (Key=#13) then begin
    Key:=#0;
    btnHSTLDSearchClick(Sender);
  end;
end;

procedure TConsoleForm.txtSecWhiteListDNSChange(Sender: TObject);
begin
  btnWLDNSItemSave.Enabled:=(txtSecWhiteListDNS.GetTextLen>0);
end;

procedure TConsoleForm.txtUsersFindKeyPress(Sender: TObject; var Key: char);
begin
  if Key=#13 then
    tbbDomain_Users_Find.Click;
end;

procedure TConsoleForm.txtUsers_New_AccountChange(Sender: TObject);
var
  sAccount:Core.Strings.VarString;
begin
  if GUI.Locked then exit;
  sAccount:=txtUsers_New_Account.Text;
  Try
    btnDomain_User_New_Done.Enabled:=(Length(sAccount)>0) and (Not Storage.UserAccounts.Items.DB.Exists(Storage.Main.Task,sAccount,FDomainP^.ID));
  finally
    Empty(sAccount);
  end;
end;

procedure TConsoleForm.txtUser_Edit_FirstChange(Sender: TObject);
begin
  if GUI.Locked then Exit;
end;

procedure TConsoleForm.txtUser_Edit_LastChange(Sender: TObject);
begin
  if GUI.Locked then Exit;
end;

procedure TConsoleForm.txtUser_Edit_PasswordChange(Sender: TObject);
begin
  if GUI.Locked then Exit;
end;

procedure TConsoleForm.txtUser_Edit_TelephoneChange(Sender: TObject);
begin
  if GUI.Locked then exit;
end;

procedure TConsoleForm.MenuItem2Click(Sender: TObject);
begin
  Core.Utils.Forms.List.Shutdown;
end;

procedure TConsoleForm.cbSecDomainFilterOnInitItem(Thread:TWorkerThread; var Data:Pointer);
begin

end;

procedure TConsoleForm.cbSecDomainFilterOnEmptyItem(Thread:TWorkerThread; var Data:Pointer);
begin

end;

procedure TConsoleForm.cbSecDomainFilterOnDoneItem(Thread:TWorkerThread; var Data:Pointer);
begin

end;

procedure TConsoleForm.cbSecDomainFilterAddedWorkItem(Thread:TWorkerThread; var Data:Pointer);
begin

end;

procedure TConsoleForm.cbSecDomainFilterOnWorkItemVoid(Thread:TWorkerThread; var Data:Pointer);
begin

end;

procedure TConsoleForm.cbSecDomainFilterOnWorkItemWhite(Thread:TWorkerThread; var Data:Pointer);
var
  TaskP                          : Core.Database.Types.TTask;
  ItemP                          : Storage.Security.Filter.PItem;
begin
  ItemP:=Data;
  TaskP:=TTask(Thread.Context[0]);
  ItemP^.Stale:=Storage.Security.Filter.DB.Stale(TaskP,secWhiteList,ItemP^.Value);
end;

procedure TConsoleForm.cbSecDomainFilterOnWorkItemBlack(Thread:TWorkerThread; var Data:Pointer);
var
  TaskP                          : Core.Database.Types.TTask;
  ItemP                          : Storage.Security.Filter.PItem;
begin
  ItemP:=Data;
  TaskP:=TTask(Thread.Context[0]);
  ItemP^.Stale:=Storage.Security.Filter.DB.Stale(TaskP,secBlackList,ItemP^.Value);
end;

procedure TConsoleForm.cbSecDomainFilterOnWorkItemAcceptable(Thread:TWorkerThread; var Data:Pointer);
var
  TaskP                          : Core.Database.Types.TTask;
  ItemP                          : Storage.Security.Filter.PItem;
begin
  ItemP:=Data;
  TaskP:=TTask(Thread.Context[0]);
  ItemP^.Stale:=Storage.Security.Filter.DB.Stale(TaskP,secAcceptableList,ItemP^.Value);
end;

procedure TConsoleForm.cbSecDomainFilterOnWorkItemWBALists(Thread:TWorkerThread; var Data:Pointer);
var
  TaskP                          : Core.Database.Types.TTask;
  ItemP                          : Storage.Security.Filter.PItem;
begin
  ItemP:=Data;
  TaskP:=TTask(Thread.Context[0]);
  ItemP^.Stale:=Storage.Security.Filter.DB.Stale(TaskP,[secBlackList,secAcceptableList,secWhiteList],ItemP^.Value);
end;

procedure TConsoleForm.cbSecDomainFilterWorkerThreadInit(Thread:TWorkerThread);
var
  TaskP                          : Core.Database.Types.TTask;
begin
  TaskP:=Core.Database.Types.TTask.Create(Storage.Main.Header,'Security Worker Item');
  SetLength(Thread.Context,1);
  Thread.Context[0]:=TaskP;
end;

procedure TConsoleForm.cbSecDomainFilterWorkerThreadDone(Thread:TWorkerThread);
var
  TaskP                        : Core.Database.Types.TTask;
begin
  TaskP:=TTask(Thread.Context[0]);
  TaskP.Free();
  Finalize(Thread.Context);
end;

procedure TConsoleForm.FormCreate(Sender: TObject);
begin
  if (Encryption.SSL.IsSSLloaded=false) then begin
    if RSR.OpenSSL()=false then begin
    end;
  end;
  Caption:=App.Build.Caption;
  lblBuildInfo.Caption:=Concat(App.Build.Title, ' was built on ',App.Build.Version,' for ',App.Build.Edition,' (',App.Build.RSR,').');
  FInfoP:=Core.Utils.Forms.List.Load(Self,Self,MainMenu,miWindow);

  GUI:=Core.Lock.Boolean.TItem.Create;
  Visible:=false;
  GUI.Lock;
  pcPages.ShowTabs:=false;
  pcPages.ActivePage:=tsWelcome;

  pcManage.ShowTabs:=false;
  pcManage.ActivePage:=tsDomains;
  pcSecurity.ShowTabs:=false;
  pcSecurity.ActivePage:=tsSecurityContentFilters;
  pcSecDomains.ShowTabs:=false;
  pcSecDomains.ActivePage:=tsSecTLD;
  pcPreferences.ShowTabs:=false;
  pcPreferences.ActivePage:=tsContentTypes;
  pcSearch.ShowTabs:=false;
  pcSearch.ActivePage:=tsSearchProviders;
  pcSecContent.ShowTabs:=false;
  pcSecContent.ActivePage:=tsSecContent;


  GUI.Unlock;
  SetLength(FDNS_HOST_IP,4);

  tsUser.TabVisible:=false;
  Visible:=False;

  FClusterResourceP:=nil;
  FFindUsers:=nil;



  Storage.MatrixServices.Items.Init(FDefaultService);
  Storage.MatrixServices.Items.Init(FDefaultServices);
  Storage.MatrixServices.Items.Init(FDomainDefaultServices);

  Core.Timer.Init(TI_Purchase);
  TI_Purchase.Expires:=0;
  TI_Purchase.Mode:=temSynchronize;
  TI_Purchase.Location:='frmConsole.TConsoleForm.OnSelectPurchase';
  TI_Purchase.Event:=@OnSelectPurchase;
  Core.Timer.Background.RegisterEvent(TI_Purchase,LoadNoUpdate);

  Core.Timer.Init(TI_NodeServiceSelection);
  TI_NodeServiceSelection.Expires:=0;
  TI_NodeServiceSelection.Mode:=temSynchronize;
  TI_NodeServiceSelection.Location:='frmConsole.TConsoleForm.OnNodeServiceSelection';
  TI_NodeServiceSelection.Event:=@OnNodeServiceSelection;
  Core.Timer.Background.RegisterEvent(TI_NodeServiceSelection,LoadNoUpdate);

  Core.Timer.Init(TI_DomainServices);
  TI_DomainServices.Expires:=0;
  TI_DomainServices.Mode:=temSynchronize;
  TI_DomainServices.Location:='frmConsole.TConsoleForm.OnDomainServiceSelection';
  TI_DomainServices.Event:=@OnDomainServiceSelection;
  Core.Timer.Background.RegisterEvent(TI_DomainServices,LoadNoUpdate);

  Core.Timer.Init(TI_CertSelect);
  TI_CertSelect.Expires:=0;
  TI_CertSelect.Mode:=temSynchronize;
  TI_CertSelect.Location:='frmConsole.TConsoleForm.OnDomainCertSelect';
  TI_CertSelect.Event:=@OnDomainCertSelect;
  Core.Timer.Background.RegisterEvent(TI_CertSelect,LoadNoUpdate);

  Core.Timer.Init(TI_ContentTypeSelect);
  TI_ContentTypeSelect.Expires:=0;
  TI_ContentTypeSelect.Mode:=temSynchronize;
  TI_ContentTypeSelect.Location:='frmConsole.TConsoleForm.OnContentTypeSelect';
  TI_ContentTypeSelect.Event:=@OnContentTypeSelect;
  Core.Timer.Background.RegisterEvent(TI_ContentTypeSelect,LoadNoUpdate);

  Core.Timer.Init(TI_DNSSelect);
  TI_DNSSelect.Expires:=0;
  TI_DNSSelect.Mode:=temSynchronize;
  TI_DNSSelect.Location:='frmConsole.TConsoleForm.OnDNSSelect';
  TI_DNSSelect.Event:=@OnDNSSelect;
  Core.Timer.Background.RegisterEvent(TI_DNSSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecContSelect);
  TI_SecContSelect.Expires:=0;
  TI_SecContSelect.Mode:=temSynchronize;
  TI_SecContSelect.Location:='frmConsole.TConsoleForm.OnSecContSelect';
  TI_SecContSelect.Event:=@OnSecContSelect;
  Core.Timer.Background.RegisterEvent(TI_SecContSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecProfileSelect);
  TI_SecProfileSelect.Expires:=0;
  TI_SecProfileSelect.Mode:=temSynchronize;
  TI_SecProfileSelect.Location:='frmConsole.TConsoleForm.OnSecProfileSelect';
  TI_SecProfileSelect.Event:=@OnSecProfileSelect;
  Core.Timer.Background.RegisterEvent(TI_SecProfileSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecBLSSelect);
  TI_SecBLSSelect.Expires:=0;
  TI_SecBLSSelect.Mode:=temSynchronize;
  TI_SecBLSSelect.Location:='frmConsole.TConsoleForm.OnSecBLSSelect';
  TI_SecBLSSelect.Event:=@OnSecBLSSelect;
  Core.Timer.Background.RegisterEvent(TI_SecBLSSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecWLSSelect);
  TI_SecWLSSelect.Expires:=0;
  TI_SecWLSSelect.Mode:=temSynchronize;
  TI_SecWLSSelect.Location:='frmConsole.TConsoleForm.OnSecWLSSelect';
  TI_SecWLSSelect.Event:=@OnSecWLSSelect;
  Core.Timer.Background.RegisterEvent(TI_SecWLSSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecTLDSelect);
  TI_SecTLDSelect.Expires:=0;
  TI_SecTLDSelect.Mode:=temSynchronize;
  TI_SecTLDSelect.Location:='frmConsole.TConsoleForm.OnSecTLDSelect';
  TI_SecTLDSelect.Event:=@OnSecTLDSelect;
  Core.Timer.Background.RegisterEvent(TI_SecTLDSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecWLDSelect);
  TI_SecWLDSelect.Expires:=0;
  TI_SecWLDSelect.Mode:=temSynchronize;
  TI_SecWLDSelect.Location:='frmConsole.TConsoleForm.OnSecWLDSelect';
  TI_SecWLDSelect.Event:=@OnSecWLDSelect;
  Core.Timer.Background.RegisterEvent(TI_SecWLDSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecIPVSelect);
  TI_SecIPVSelect.Expires:=0;
  TI_SecIPVSelect.Mode:=temSynchronize;
  TI_SecIPVSelect.Location:='frmConsole.TConsoleForm.OnSecIPVSelect';
  TI_SecIPVSelect.Event:=@OnSecIPVSelect;
  Core.Timer.Background.RegisterEvent(TI_SecIPVSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecBLDSelect);
  TI_SecBLDSelect.Expires:=0;
  TI_SecBLDSelect.Mode:=temSynchronize;
  TI_SecBLDSelect.Location:='frmConsole.TConsoleForm.OnSecBLDSelect';
  TI_SecBLDSelect.Event:=@OnSecBLDSelect;
  Core.Timer.Background.RegisterEvent(TI_SecBLDSelect,LoadNoUpdate);

  Core.Timer.Init(TI_SecALDSelect);
  TI_SecALDSelect.Expires:=0;
  TI_SecALDSelect.Mode:=temSynchronize;
  TI_SecALDSelect.Location:='frmConsole.TConsoleForm.OnSecALDSelect';
  TI_SecALDSelect.Event:=@OnSecALDSelect;
  Core.Timer.Background.RegisterEvent(TI_SecALDSelect,LoadNoUpdate);

  Core.Timer.Init(TI_DNS_Change);
  TI_DNS_Change.Expires:=0;
  TI_DNS_Change.Mode:=temSynchronize;
  TI_DNS_Change.Location:='frmConsole.TConsoleForm.OnDNS_Change';
  TI_DNS_Change.Event:=@OnDNS_Change;
  Core.Timer.Background.RegisterEvent(TI_DNS_Change,LoadNoUpdate);

end;

procedure TConsoleForm.FormDestroy(Sender: TObject);
begin
  Core.Timer.Background.UnloadEvent(TI_Purchase,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_NodeServiceSelection,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_DomainServices,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_CertSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_CertSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_ContentTypeSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_DNSSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecContSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecProfileSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecBLSSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecWLSSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecTLDSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecWLDSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecIPVSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecBLDSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_SecALDSelect,UnloadNoExecute);
  Core.Timer.Background.UnloadEvent(TI_DNS_Change,UnloadNoExecute);



  Core.Timer.Done(TI_Purchase);
  Core.Timer.Done(TI_NodeServiceSelection);
  Core.Timer.Done(TI_DomainServices);
  Core.Timer.Done(TI_CertSelect);
  Core.Timer.Done(TI_ContentTypeSelect);
  Core.Timer.Done(TI_DNSSelect);
  Core.Timer.Done(TI_SecContSelect);
  Core.Timer.Done(TI_SecProfileSelect);
  Core.Timer.Done(TI_SecBLSSelect);
  Core.Timer.Done(TI_SecWLSSelect);
  Core.Timer.Done(TI_SecTLDSelect);
  Core.Timer.Done(TI_SecWLDSelect);
  Core.Timer.Done(TI_SecIPVSelect);
  Core.Timer.Done(TI_SecBLDSelect);
  Core.Timer.Done(TI_SecALDSelect);
  Core.Timer.Done(TI_DNS_Change);


  Storage.ConfigData.Items.Done(FProcessGroupID);
  Storage.ConfigData.Items.Done(FProcessGroupName);
  Storage.ConfigData.Items.Done(FProcessUserID);
  Storage.ConfigData.Items.Done(FProcessUserName);
  Storage.ConfigData.Items.Done(FRaidUserID);
  Storage.ConfigData.Items.Done(FRaidGroupID);
  Storage.ConfigData.Items.Done(FRaidUserName);
  Storage.ConfigData.Items.Done(FRaidGroupName);
  {$ifdef Unix}
  Core.Utils.Unix.Account.Done(FUserNameProcess);
  {$endif}

  Core.Arrays.varString.Done(FDNS_HOST_IP);
  Core.Arrays.varString.Done(FDNS_BL_Service);
  Core.Arrays.varString.Done(FDNS_WL_Service);
  Core.Arrays.varString.Done(FNode_IP);
  Core.Arrays.varString.Done(FClusterNode_IP);

  FreeAndNil(FManagerFat);

  Storage.Domains.Items.Done(Domains);
  Storage.MatrixClusters.Cluster.Done(FClusters);
  Storage.MatrixResources.Resource.Done(FClusterResources);
  Storage.MatrixNodes.Node.Done(Nodes);

  Storage.MatrixServices.Items.Done(Services);
  Storage.MatrixNodes.Node.Done(FDomainNode);
  Storage.MatrixNodes.Node.Done(FDomainNodes);
  Storage.MatrixClusters.Cluster.Done(FDomainCluster);

  Storage.MatrixResources.Resource.Done(FDomainResource);
  Storage.MatrixServices.Items.Done(FDomainServices);
  Storage.SrchProviders.Items.Done(FSearchProviders);
  Storage.UserAccounts.Items.Done(FUAPL);

  FSearchProviderP:=nil;

  Core.Keywords.Done(FKeywords);
  Storage.MatrixServices.Items.Done(FDefaultService);
  Storage.MatrixServices.Items.Done(FDefaultServices);
  Storage.MatrixServices.Items.Done(FDomainDefaultServices);

  FreeAndNil(FFindUsers);
  FreeAndNil(FCoreObjects);
  FreeAndNil(GUI);

end;

procedure TConsoleForm.OnDomainServiceSelection(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  GUI.Lock;
  Try
    {$i frmConsole.Domain.Service.SelectItem.inc}
  finally
    GUI.Unlock();
  end;
end;

procedure TConsoleForm.OnSelectPurchase(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  if lvPurchase.Selected<>nil then begin
    FPurchaseP:=lvPurchase.Selected.Data;
  end else begin
    FPurchaseP:=nil;
  end;
  LoadPurchase();
end;

procedure TConsoleForm.OnNodeServiceSelection(ItemP:Core.Timer.PItem);
var
  msP:Storage.MatrixServices.Items.PItem;
begin
  ItemP^.Expires:=0;
  if lvClusterNodeService.Selected<>nil then begin
    msP:=lvClusterNodeService.Selected.Data;
    GUI.Lock;
    Try
      cbClusterNodeEnabled.Checked:=msP^.Enabled;
      tbClusterNodeScale.Position:=msP^.Scale;
      tbClusterNodeScale.Enabled:=True;
      gbClusterNodeDefaults.Visible:=True;
    Finally
      GUI.UnLock;
    end;
  end else
    gbClusterNodeDefaults.Visible:=False;
end;

procedure TConsoleForm.OnContentTypeSelect(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  if lvContentTypes.Selected<>nil then
    FContentTypeP:=lvContentTypes.Selected.Data
  else
    FContentTypeP:=nil;
  {$i frmConsole.ContentTypes.SelectItem.inc}
end;

procedure TConsoleForm.OnDomainCertSelect(ItemP:Core.Timer.PItem);
var
  li:TListItem;
begin
  li:=lvCerts.Selected;
  if li<>nil then begin
    FCertP:=Encryption.SSL.getItem(StrToIntDef(li.Caption,0),FCerts);
    if FCertP<>nil then
      LoadCertInfo(FCertP^);
  end;
end;

procedure TConsoleForm.LoadUserACLCoreObjects(UserP:Storage.UserAccounts.Items.PItem);
var
  iLcv:integer;
  liItem:TListItem;
begin
  GUI.Lock;
  Try
    lvUserACLCoreCommands.Items.Clear;
    lvUserACLCoreObjects.Items.Clear;
    for iLcv:=0 to FCoreObjects.Count-1 do begin
      liItem:=lvUserACLCoreObjects.Items.Add;
      liItem.Data:=FCoreObjects.Items[iLcv];
      liItem.Checked:=Granted(FCoreObjects.Items[iLcv].Header,UserP);
      liItem.Caption:=YES_NO[liItem.Checked];
      liItem.SubItems.Add(FCoreObjects.Items[iLcv].Header.ACLInfo^.Caption);
      liItem.SubItems.Add(FCoreObjects.Items[iLcv].Header.ACLInfo^.Description);
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.lvContentTypesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if GUI.Locked then Exit;
  TI_ContentTypeSelect.Expires:=DateUtils.IncMillisecond(Now,500);
end;

procedure TConsoleForm.lvDomainsColumnClick(Sender: TObject; Column: TListColumn
  );
begin
  lvDomains.SortColumn:=Column.Index;
end;

procedure TConsoleForm.lvDomainsCompare(Sender: TObject; Item1,
  Item2: TListItem; Data: Integer; var Compare: Integer);
begin
  Compare:=CompareText(Item1.Caption,Item2.Caption);
end;

procedure TConsoleForm.TableValueChange(Sender:TObject);
var
  iIndex:integer;
  ItemP:Core.Database.Monitor.Types.PItem;
begin
  if (GUI.Locked) or (Sender is TEdit) then Exit;
  iIndex:=TEdit(Sender).Tag;
  ItemP:=Core.Database.Monitor.Retrieve(iIndex);
  if ItemP<>nil then begin
    ItemP^.TableP^.Name:=TEdit(Sender).Text;
    Storage.SaveINIFile;
  end;
end;

procedure TConsoleForm.LoadDBMSSettings;
var
  iLcv:integer;
  List:Core.Arrays.Types.Pointers;
  ItemP:Core.Database.Monitor.Types.PItem;
  tnGroup:TTreeNode;
  tnTable:TTreeNode;
begin
  GUI.Lock;
  Try
    {$i frmConsole.DBM.DisableGUI.inc}
    tvDBMGroups.Items.Clear;
    Core.Database.Monitor.Build(List);
    Try
      for iLcv:=0 to High(List) do begin
        ItemP:=List[iLcv];
        tnGroup:=Core.Utils.TreeView.Find(ItemP^.TableP^.StartupP^.Group,tvDBMGroups);
        if tnGroup=nil then
          tnGroup:=Core.Utils.TreeView.Add(ItemP^.TableP^.StartupP^.Group,tvDBMGroups,nil);
        tnTable:=Core.Utils.TreeView.Find(ItemP^.TableP^.StartupP^.Name,tnGroup);
        if tnTable=nil then
          tnTable:=tvDBMGroups.Items.AddChild(tnGroup,ItemP^.TableP^.StartupP^.Name);
        tnTable.Data:=ItemP;
      end;
      tvDBMGroups.AlphaSort;
    finally
      Core.Arrays.Pointers.Done(List);
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.btnDNSItemAddClick(Sender: TObject);
var
  iIndex:integer;
  liDNS:TListItem;
  itmP:Storage.DNS.Items.PItem;
begin
  new(itmP);
  Storage.DNS.Items.Init(itmP^);
  iIndex:=Length(DNS_Hosts);
  SetLength(DNS_Hosts,iIndex+1);
  DNS_Hosts[iIndex]:=itmP;

  itmP^.Kind:=Storage.DNS.dnskRegular;
  itmP^.ID:=0;
  itmP^.IP:=0;

  Storage.DNS.Items.DB.Add(Storage.Main.Task,itmP^);

  liDNS:=lvDNSRegular.Items.Add;
  liDNS.Data:=itmP;
  liDNS.Caption:=IntToStr(itmP^.ID);
  liDNS.SubItems.Add('0.0.0.0');
end;

procedure TConsoleForm.btnBLDNSItemRemoveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnBLDNSItemRemove.Enabled:=false;
    txtSecBlackListDNS.Clear();
    if (lvDNSBL.Selected<>nil) then begin
      itmP:=lvDNSBL.Selected.Data;
      lvDNSBL.Selected.Delete();
      if (itmP<>nil) then begin
        Storage.Security.Filter.DB.Delete(Storage.Main.Task,itmP^);
        itmP^.Stale:=True;
        Storage.Security.Filter.Cleanup(Security_BL_Services);
      end;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.btnWLDNSItemRemoveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnWLDNSItemRemove.Enabled:=false;
    txtSecWhiteListDNS.Clear();
    if (lvDNSWL.Selected<>nil) then begin
      itmP:=lvDNSWL.Selected.Data;
      lvDNSWL.Selected.Delete();
      if (itmP<>nil) then begin
        Storage.Security.Filter.DB.Delete(Storage.Main.Task,itmP^);
        itmP^.Stale:=True;
        Storage.Security.Filter.Cleanup(Security_WL_Services);
      end;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.btnBLDNSItemAddClick(Sender: TObject);
begin
  if lvDNSBL.Selected<>nil then
  lvDNSBL.Selected.Selected:=false;
  lvDNSBL.Selected:=nil;
  txtSecBlackListDNS.Clear();
  btnBLDNSItemRemove.Enabled:=false;
end;

procedure TConsoleForm.btnBLDNSItemSaveClick(Sender: TObject);
var
  iIndex:integer;
  liDNS:TListItem;
  itmP:Storage.Security.Filter.PItem;
begin
  liDNS:=lvDNSBL.Selected;
  if (liDNS<>nil) then begin
    itmP:=liDNS.Data;

    itmP^.Enabled:=true;
    itmP^.Value:=Lowercase(txtSecBlackListDNS.Text);
    liDNS.SubItems[0]:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task,secBLService,itmP^);
  end else begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);

    iIndex:=Length(Security_BL_Services);
    SetLength(Security_BL_Services,iIndex+1);
    Security_BL_Services[iIndex]:=itmP;

    itmP^.Enabled:=true;
    itmP^.Value:=Lowercase(txtSecBlackListDNS.Text);

    Storage.Security.Filter.DB.Add(Storage.Main.Task,secBLService,itmP^);

    liDNS:=lvDNSBL.Items.Insert(0);
    liDNS.Data:=itmP;
    liDNS.Caption:=IntToStr(itmP^.ID);
    liDNS.SubItems.Add(itmP^.Value);
    lvDNSBL.Selected:=liDNS;
    liDNS.MakeVisible(false);
  end;
end;


procedure TConsoleForm.btnClusterCancel1Click(Sender: TObject);
begin
  GUI.Lock;
  Try
    txtDomains_Hostname.Clear;
    txtDomains_FriendlyName.Clear;
    txtDomains_Postmaster.Clear;
    gbDomains_DomainInformation.Visible:=False;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.btnClusterCancel2Click(Sender: TObject);
begin
  GUI.Lock;
  Try
    {$i frmConsole.Domains.Users.Clear.inc}
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.btnClusterCancelClick(Sender: TObject);
begin
  gbClusters_ClusterInfo.Visible:=false;
  txtCluster_New_Name.Clear;
  txtCluster_New_Description.Clear;
end;

procedure TConsoleForm.btnClusterNewClick(Sender: TObject);
var
  iIndex:Integer;
begin
  New(FClusterP);
  Storage.MatrixClusters.Cluster.Init(FClusterP^);
  FClusterP^.Group:=txtCluster_New_Name.Text;
  FClusterP^.Location.Description:=txtCluster_New_Description.Text;
  if Storage.MatrixClusters.Cluster.DB.Create(Storage.Main.Task,FClusterP^) then begin
    txtCluster_Name.Clear;
    txtCluster_New_Description.Clear;
    iIndex:=Length(FClusters);
    SetLength(FClusters,iIndex+1);
    FClusters[iIndex]:=FClusterP;
    gbClusters_ClusterInfo.Visible:=false;
    // TO DO ADD Cluster to List View :-)
    Display_Cluster_Editor(FClusterP);
  end;
end;

procedure TConsoleForm.btnClusterNewNodeCancelClick(Sender: TObject);
begin
  {$i frmConsole.Clusters.LayoutManager.Hide.NodeEditor.inc}
end;

procedure TConsoleForm.btnClusterNewNodeClick(Sender: TObject);
var
  liNode:TListItem;
  iIndex:integer;
begin
  if (tvClusterResources.Selected=nil) then exit;
  GUI.Lock;
  Try
    liNode:=nil; FClusterNodeP:=nil;
    FClusterResourceP:=tvClusterResources.Selected.Data;
    if lvClusterNodes.Selected<>nil then
      FClusterNodeP:=lvClusterNodes.Selected.Data;
    if FClusterNodeP=nil then begin
       iIndex:=Length(FClusterNodes);
       SetLength(FClusterNodes,iIndex+1);
       New(FClusterNodeP);
       Storage.MatrixNodes.Node.Init(FClusterNodeP^);
       FClusterNodes[iIndex]:=FClusterNodeP;
       FClusterNodeP^.ClusterID:=FClusterP^.ID;
       FClusterNodeP^.ResourceID:=FClusterResourceP^.ID;
       FClusterNodeP^.DomainID:=Storage.MatrixServices.Default.Domain;
       FClusterNodeP^.IP:=Core.Utils.Sockets.InAddrFromStr(FClusterNode_IP);
       FClusterNodeP^.Alias:=txtClusterNodeName.Text;
       FClusterNodeP^.Disk.Device:=txtCRNDisk.Text;
       FClusterNodeP^.Disk.Enabled:=cbClusterNodeDisk.Checked;
       Storage.MatrixNodes.Node.DB.Create(Storage.Main.Task,FClusterNodeP^);
       liNode:=lvClusterNodes.Items.Add;
       liNode.Caption:=IntToStr(FClusterNodeP^.ID);
       liNode.SubItems.Add(IntToStr(FClusterNodeP^.ResourceID));
       liNode.SubItems.Add(IntToStr(FClusterNodeP^.ClusterID));
       liNode.SubItems.Add(Core.Utils.Sockets.InAddrToStr(FClusterNodeP^.IP));
       liNode.SubItems.Add(txtClusterNodeName.Text);
       liNode.SubItems.Add(Yes_No[cbClusterNodeDisk.Checked]);
       liNode.SubItems.Add(txtCRNDisk.Text);
       liNode.Data:=FClusterNodeP;
    end else begin;
      liNode:=lvClusterNodes.Selected;
      FClusterNodeP^.IP:=Core.Utils.Sockets.InAddrFromStr(FClusterNode_IP);
      FClusterNodeP^.Alias:=txtClusterNodeName.Text;
      FClusterNodeP^.Disk.Enabled:=cbClusterNodeDisk.Checked;
      FClusterNodeP^.Disk.Device:=txtCRNDisk.Text;
      Storage.MatrixNodes.Node.DB.Write(Storage.Main.Task,FClusterNodeP^);
      if (liNode<>nil) then begin
        liNode.SubItems[2]:=Core.Utils.Sockets.InAddrToStr(FClusterNodeP^.IP);
        liNode.SubItems[3]:=txtClusterNodeName.Text;
        liNode.SubItems[4]:=Yes_No[cbClusterNodeDisk.Checked];
        liNode.SubItems[5]:=txtCRNDisk.Text;
      end;
    end;
    {$i frmConsole.Clusters.LayoutManager.Hide.NodeEditor.inc}
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.btnCluster_DoneClick(Sender: TObject);
begin
  pcPages.ActivePage:=tsManage;
  pcManage.ActivePage:=tsClusters;
  lbStatus.Caption:=' Loading Clusters...';
  FClusterP:=nil;
  FClusterNodeP:=nil;
  FClusterResourceP:=nil;

  Application.ProcessMessages;
  LoadClusters;
end;

procedure TConsoleForm.btnContentTypeAddClick(Sender: TObject);
begin
  lvContentTypes.Selected:=nil;
  txtContentTypeExtension.Clear();
  txtContentTypeKind.Clear();
end;

procedure TConsoleForm.btnContentTypeRemoveClick(Sender: TObject);
var
  cntP : PContentType;
  li   : TListItem;
begin
  li:=lvContentTypes.Selected;
  if li <>nil then begin
    cntP:=li.Data;
    if Storage.ContentTypes.Items.DB.Delete(Storage.Main.Task,cntP^) then
      RSR.HTTP.Delete(Storage.ContentTypes.List,cntP);
    li.Delete();
  end;
end;

procedure TConsoleForm.btnContentTypeSaveClick(Sender: TObject);
var
  cntP : PContentType;
  li   : TListItem;
begin
  li:=lvContentTypes.Selected;
  if li<>nil then begin
    cntP:=lvContentTypes.Selected.Data;
    cntP^.Ext:=txtContentTypeExtension.Text;
    cntP^.Kind:=txtContentTypeKind.Text;
    if (RSR.HTTP.Validate(Storage.ContentTypes.List,txtContentTypeKind.Text,txtContentTypeExtension.Text,ctvEdit,cntP)) then begin
      Storage.ContentTypes.Items.DB.Write(Storage.Main.Task,cntP^);
      li.Caption:=cntP^.Ext;
      li.SubItems[0]:=cntP^.Kind;
    end;
  end else if (RSR.HTTP.Validate(Storage.ContentTypes.List,txtContentTypeKind.Text,txtContentTypeExtension.Text,ctvAdd,nil)) then begin
    new(cntP);
    RSR.HTTP.Init(cntP^);
    cntP^.Ext:=txtContentTypeExtension.Text;
    cntP^.Kind:=txtContentTypeKind.Text;
    if Storage.ContentTypes.Items.DB.Add(Storage.Main.Task,cntP^) then begin
      RSR.HTTP.Add(Storage.ContentTypes.List,cntP);
      li:=lvContentTypes.Items.Add;
      li.Caption:=txtContentTypeExtension.Text;
      li.SubItems.Add(txtContentTypeKind.Text);
      li.Data:=cntP;
    end;
  end;
end;

procedure TConsoleForm.btnCRNResourceCancelClick(Sender: TObject);
begin
  GUI.Lock;
  Try
    txtCRNResource.Text:='';
    gbCRNResource.Visible:=false;
    FClusterResourceP:=nil;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.btnCRNResourceOkClick(Sender: TObject);
var
  iIndex:integer;
  tnCluster:TTreeNode;
  tnItem:TTreeNode;
begin
  if FClusterResourceP=nil then begin
    iIndex:=Length(FClusterResources);
    SetLength(FClusterResources,iIndex+1);
    New(FClusterResourceP);
    Storage.MatrixResources.Resource.Init(FClusterResourceP^);
    FClusterResources[iIndex]:=FClusterResourceP;
    FClusterResourceP^.Name:=txtCRNResource.Text;
    FClusterResourceP^.ClusterID:=FClusterP^.ID;
    Storage.MatrixResources.Resource.DB.Create(Storage.Main.Task,FClusterResourceP^);
    tnCluster:=tvClusterResources.Selected;
    if (tnCluster<>nil) and (tnCluster.Data<>nil) then
      tnCluster:=tnCluster.Parent;
    tnItem:=tvClusterResources.Items.AddChild(tnCluster,FClusterResourceP^.Name);
    tnItem.Data:=FClusterResourceP;
    txtCRNResource.Clear;
    gbCRNResource.Visible:=false;
  end else begin
    // Existing ClusterResource... Change name and save
    if FClusterResourceP^.Name<>txtCRNResource.Text then begin
      FClusterResourceP^.Name:=txtCRNResource.Text;
      Storage.MatrixResources.Resource.DB.Write(Storage.Main.Task,FClusterResourceP^);
      tnItem:=Core.Utils.TreeView.Find(FClusterResourceP,tvClusterResources);
      if tnItem<>nil then
        tnItem.Text:=FClusterResourceP^.Name;
      txtCRNResource.Clear;
      gbCRNResource.Visible:=false;
    end;
  end;
end;

procedure TConsoleForm.btnDCAAllocateClick(Sender: TObject);
var
  tnAllocatedResource:TTreeNode;
  tnAvailableResource:TTreeNode;
  tnAvailableNode:TTreeNode;
  iLcv:integer;
begin
  //FMT_MN_DISPLAY_ALLOCATION_STATUS
  if (FDomainP=nil) and (tvDomainClustering.Selected=nil) and (tvDomainClustering.Selected.Data=nil) and (FDomainNode.ID=0) then exit;
  FDomainNode.DomainID:=Domains[cbDCDomains.ItemIndex].ID;
  Storage.MatrixNodes.Node.DB.SetDomainID(Storage.Main.Task,FDomainNode.ID,FDomainNode.DomainID);
  for iLcv:=0 to High(FDomainServices.List) do begin
    Storage.MatrixServices.Items.DB.Verify(
      Storage.Main.Task,
      FDomainCluster.ID,
      FDomainResource.ID,
      FDomainNode.ID,
      FDomainNode.DomainID,
      FDomainServices.List[iLcv]^.Kind,
      FDomainServices.List[iLcv]^.Port,
      FDomainServices.List[iLcv]^.Scale,
      FDomainServices.List[iLcv]^.Enabled
    );
    if FDomainServices.List[iLcv]^.Enabled then begin
      Storage.MatrixServices.Items.DB.SetEnabled(Storage.Main.Task,FDomainServices.List[iLcv]^.ID,FDomainServices.List[iLcv]^.Kind);
    end else begin
      Storage.MatrixServices.Items.DB.SetDisabled(Storage.Main.Task,FDomainServices.List[iLcv]^.ID,FDomainServices.List[iLcv]^.Kind);
    end;
  end;


  Storage.MatrixNodes.Node.DB.Write(Storage.Main.Task,FDomainNode);


  lbDCNIStatusValue.Caption:=Format(Storage.MatrixNodes.Node.FMT_MN_BOUND_STATUS[True],[Domains[cbDCDomains.ItemIndex].Name]);
  {$i frmConsole.Domain.Clustering.ResourceConsumption.inc}

  tnAllocatedResource:=Core.Utils.TreeView.Find(Concat('Allocated/',FDomainCluster.Group,'/',FDomainResource.Name),tvDomainClustering);
  tnAvailableResource:=Core.Utils.TreeView.Find(Concat('Available/',FDomainCluster.Group,'/',FDomainResource.Name),tvDomainClustering);
  tnAvailableNode:=Core.Utils.TreeView.Find(Pointer(FDomainNode.ID),tnAvailableResource);
  if tnAvailableNode<>nil then
    tnAvailableNode.MoveTo(tnAllocatedResource,naAddChild);
end;

procedure TConsoleForm.btnDCADeallocateClick(Sender: TObject);
var
  tnAllocatedResource:TTreeNode;
  tnAvailableResource:TTreeNode;
  tnAllocatedNode:TTreeNode;
begin
  // FMT_MN_DISPLAY_ALLOCATION_STATUS
  if (FDomainP=nil) and (tvDomainClustering.Selected=nil) and (tvDomainClustering.Selected.Data=nil) and (FDomainNode.ID=0) then exit;

  Storage.MatrixServices.Items.DB.SetDomainID(Storage.Main.Task,FDomainNode.ID,Storage.MatrixServices.Default.Domain);
  Storage.MatrixNodes.Node.DB.SetDomainID(Storage.Main.Task,FDomainNode.ID,Storage.MatrixServices.Default.Domain);

  lbDCNIStatusValue.Caption:=Format(Storage.MatrixNodes.Node.FMT_MN_BOUND_STATUS[false],[Storage.MatrixNodes.Node.FMT_MN_UNBOUND]);
  {$i frmConsole.Domain.Clustering.ResourceConsumption.inc}

  tnAllocatedResource:=Core.Utils.TreeView.Find(Concat('Allocated/',FDomainCluster.Group,'/',FDomainResource.Name),tvDomainClustering);
  tnAvailableResource:=Core.Utils.TreeView.Find(Concat('Available/',FDomainCluster.Group,'/',FDomainResource.Name),tvDomainClustering);
  tnAllocatedNode:=Core.Utils.TreeView.Find(Pointer(FDomainNode.ID),tnAllocatedResource);
  if tnAllocatedNode<>nil then
    tnAllocatedNode.MoveTo(tnAvailableResource,naAddChild);
end;

procedure TConsoleForm.btnDiskNodeRefreshClick(Sender: TObject);
var
  iDX:integer;
  iLcv:integer;
  li:TListItem;
  sError:Core.Strings.VarString;
begin
  iDX:=cmboDiskNodes.ItemIndex;
  if iDX<>-1 then begin
    Storage.MatrixServices.Items.Empty(FNodeServices);
    Storage.MatrixServices.Items.DB.Verify(Storage.Main.Task,FDisks[iDX]^.ClusterID,FDisks[iDX]^.ResourceID,FDisks[iDX]^.ID,FDefaultServices);
    Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FDisks[iDX]^.ClusterID,FDisks[iDX]^.ResourceID,FDisks[iDX]^.ID,FNodeServices);
    Storage.MatrixNodes.Node.DB.Stats(Storage.Main.Task,FDisks[iDX]^);
    lvClusterNodeService.Items.Clear;
    for iLcv:=0 to High(FNodeServices) do begin
      li:=lvClusterNodeService.Items.Add;
      li.Checked:=FNodeServices[iLcv]^.Enabled;
      li.Caption:=IntToStr(FNodeServices[iLcv]^.ID);
      li.SubItems.Add(YES_NO[FNodeServices[iLcv]^.Enabled]);
      li.SubItems.Add(Storage.MatrixServices.Items.mkList[FNodeServices[iLcv]^.Kind]);
      li.SubItems.Add(IntToStr(FNodeServices[iLcv]^.Scale));
      li.SubItems.Add(Storage.MatrixServices.Items.mkDescriptions[FNodeServices[iLcv]^.Kind]);
      li.Data:=FNodeServices[iLcv];
    end;
    if (FDisks[iDX]^.ID=Storage.NodeID) then begin
      Storage.MatrixNodes.Node.StatRAM(FDisks[iDX]^);
      if System.Length(FDisks[iDX]^.Disk.Device)>0 then
        Storage.MatrixNodes.Node.StatDisk(FDisks[iDX]^,sError);
      Storage.MatrixNodes.Node.StatIDs(FDisks[iDX]^,FProcessGroupName.Value,FProcessUserName.Value,sError);
    end;

    lbMNID.Caption:=IntToStr(FDisks[iDX]^.ID);
    lbMNGroupID.Caption:=IntToStr(FDisks[iDX]^.GroupID);
    lbMNUserID.Caption:=IntToStr(FDisks[iDX]^.UserID);
    lbMNDisk.Caption:=FDisks[iDX]^.Disk.Device;

    lblMNRAMUsed.Caption:=IntToStr(FDisks[iDX]^.Stats.MEM_Used);

    lblMNRAMAvail.Caption:=IntToStr(FDisks[iDX]^.Stats.MEM_Free);
    lblMNRAMTotal.Caption:=IntToStr(FDisks[iDX]^.Stats.MEM_Total);

    pbMNRAMTotal.Max:=FDisks[iDX]^.Stats.MEM_Total;
    pbMNRAMUsed.Max:=FDisks[iDX]^.Stats.MEM_Total;
    pbMNRAMAvailable.Max:=FDisks[iDX]^.Stats.MEM_Total;

    pbMNRAMTotal.Position:=FDisks[iDX]^.Stats.MEM_Total;
    pbMNRAMAvailable.Position:=FDisks[iDX]^.Stats.MEM_Free;
    pbMNRAMUsed.Position:=FDisks[iDX]^.Stats.MEM_Used;

    lblMNDSKUsed.Caption:=SysUtils.Format('%.0N',[FDisks[iDX]^.Disk.Consumption/1]);
    lblMNDSKAvail.Caption:=SysUtils.Format('%.0N',[FDisks[iDX]^.Disk.Available/1]);
    lblMNDSKTotal.Caption:=SysUtils.Format('%.0N',[FDisks[iDX]^.Disk.Capacity/1]);

    pbMNDSKAvailable.Max:=FDisks[iDX]^.Disk.Capacity;
    pbMNDSKUsed.Max:=FDisks[iDX]^.Disk.Capacity;
    pbMNDSKTotal.Max:=FDisks[iDX]^.Disk.Capacity;

    pbMNDSKAvailable.Position:=FDisks[iDX]^.Disk.Available;
    pbMNDSKUsed.Position:=FDisks[iDX]^.Disk.Consumption;
    pbMNDSKTotal.Position:=FDisks[iDX]^.Disk.Capacity;

    pcClusterNode.Visible:=true;

  end else begin
    lbMNID.Caption:='Unknown';

    lblMNRAMUsed.Caption:='0';
    lblMNRAMAvail.Caption:='0';
    lblMNRAMTotal.Caption:='0';
    pbMNRAMAvailable.Position:=0;
    pbMNRAMUsed.Position:=0;
    pbMNRAMTotal.Position:=0;

    lblMNDSKUsed.Caption:='0';
    lblMNDSKAvail.Caption:='0';
    lblMNDSKTotal.Caption:='0';
    pbMNDSKAvailable.Position:=0;
    pbMNDSKUsed.Position:=0;
    pbMNDSKTotal.Position:=0;

    pcClusterNode.Visible:=false;
  end;
end;

procedure TConsoleForm.btnDSDeleteClick(Sender: TObject);
begin
  if FCertP<>nil then begin
    Storage.Certs.Items.DB.Delete(Storage.Main.Task,FDomainP^.ID,FCertP^.ID);
    LoadSSLInfo;
  end;
end;

procedure TConsoleForm.btnDSNCClick(Sender: TObject);
var
  Cert:TCertData;
begin
  if frmRSA.RSAGen.Execute(App.Folders.UserSSL(),FDomainP^.Name)=mrOK then begin
    if Core.Utils.Files.FileSize(Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,No_Quotes))>0 then begin
      // New KeyFile is Ready
      Core.Utils.Files.Truncate(Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,No_Quotes));
      Core.Utils.Files.Truncate(Storage.Certs.GetRequestFile(App.Folders.UserSSL(),FDomainP^.Name,No_Quotes));
      if frmCertReq.CertReqForm.Execute(App.Folders.UserSSL(),FDomainP^.Name)=mrOK then begin
        if Core.Utils.Files.FileSize(Storage.Certs.GetRequestFile(App.Folders.UserSSL(),FDomainP^.Name,No_Quotes))>0 then begin
          Encryption.SSL.Init(Cert);
          Try
            Cert.Key:=Core.Utils.Files.toString(Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES));
            Core.Arrays.Bytes.fromFile(Storage.Certs.GetKeyFileAsDer(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES),Cert.DerKey);
            Cert.Date:=DateUtils.IncYear(dtUT,1);
            Cert.Request:=Core.Utils.Files.toString(Storage.Certs.GetRequestFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES));
            Storage.Certs.Items.DB.Add(Storage.Main.Task,FDomainP^.ID,Cert);
          finally
            Encryption.SSL.Done(Cert);
          end;
          LoadSSLInfo;
        end else
          Core.Utils.Files.Truncate(Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES));
      end else
        Core.Utils.Files.Truncate(Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES));
    end;
  end;
end;

procedure TConsoleForm.btnDSRefreshClick(Sender: TObject);
begin
  LoadSSLInfo;
end;

procedure TConsoleForm.btnDSReqClick(Sender: TObject);
begin
  if FCertP<>nil then begin
    CertReqForm.SetInfo(App.Folders.UserSSL(),FDomainP^.Name);
    CertReqForm.txtRequest.Lines.Text:=FCertP^.Request;
    CertReqForm.SetMode(cmViewRequest);
    CertReqForm.ShowModal;
  end;
end;

procedure TConsoleForm.btnDSSetClick(Sender: TObject);
begin
  if FCertP<>nil then begin
    if (FCertP^.Level>0) then begin
      FDomainP^.CertID:=FCertP^.ID;
      Storage.Domains.Items.DB.Update(Storage.Main.Task,FDomainP^);
      LoadSSLInfo;
    end else
      lblDSCStatus.Caption:=Concat('Cannot assign an invalid certificate to ',FDomainP^.Name);
  end;
end;

procedure TConsoleForm.btnDSSSCClick(Sender: TObject);
var
  sCertFile:Core.Strings.VarString;
  sDerCertFile:Core.Strings.VarString;
  sKeyFile:Core.Strings.VarString;
  sReqFile:Core.Strings.VarString;
  sException:Core.Strings.VarString;
begin
  if FCertP<>nil then begin
    sCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
    sDerCertFile:=Storage.Certs.GetCertFileAsDer(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
    sKeyFile:=Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES);
    sReqFile:=Storage.Certs.GetRequestFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES);
    Core.Utils.Files.Truncate(sCertFile);
    Core.Utils.Files.fromString(sKeyFile,FCertP^.Key);
    Core.Utils.Files.fromString(sReqFile,FCertP^.Request);
    procSelfSign.Executable:='openssl';
    procSelfSign.Parameters.Clear();
    procSelfSign.Parameters.Add('x509');
    procSelfSign.Parameters.Add('-req');
    procSelfSign.Parameters.Add('-days');
    procSelfSign.Parameters.Add('365');
    procSelfSign.Parameters.Add('-in');
    procSelfSign.Parameters.Add(Concat('"',sReqFile,'"'));
    procSelfSign.Parameters.Add('-signkey');
    procSelfSign.Parameters.Add(Concat('"',sKeyFile,'"'));
    procSelfSign.Parameters.Add('-out');
    procSelfSign.Parameters.Add(Concat('"',sCertFile,'"'));
    procSelfSign.Execute();
    (*
    procSelfSign.CommandLine:=Concat(
      'openssl x509 -req -days 365 -in ',
      sReqFile,
      ' -signkey ',
      sKeyFile,
      ' -out ',
      sCertFile
    );
    *)
    procSelfSign.Executable:='openssl';
    procSelfSign.Parameters.Clear();
    procSelfSign.Parameters.Add('x509');
    procSelfSign.Parameters.Add('-in');
    procSelfSign.Parameters.Add(Concat('"',sCertFile,'"'));
    procSelfSign.Parameters.Add('-inform');
    procSelfSign.Parameters.Add('PEM');
    procSelfSign.Parameters.Add('-out');
    procSelfSign.Parameters.Add(Concat('"',sDerCertFile,'"'));
    procSelfSign.Parameters.Add('-outform');
    procSelfSign.Parameters.Add('-DER');
    procSelfSign.Execute();

    (*
    procSelfSign.CommandLine:=Concat(
      'openssl x509 -in ',
      sCertFile,
      ' -inform PEM -out ',
      sDerCertFile,
      ' -outform DER'
    );
    *)
    if Core.Utils.Files.FileSize(sCertFile)>0 then begin
      FCertP^.Level:=1;
      Empty(FCertP^.Certs);
      Empty(FCertP^.DerCerts);
      SetLength(FCertP^.Certs,1);
      SetLength(FCertP^.DerCerts,1);

      FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
      Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);

      Storage.Certs.Items.DB.Write(Storage.Main.Task,FDomainP^.ID,FCertP^);

      LoadSSLInfo();
    end else begin
     sException:=Core.Streams.toMemo(procSelfSign.Output);
     lblDSCStatus.Caption:='Error self signing certificate.';
     Form.Exception.ShowException('frmConsole.pas','Self sign error',sException);
    end;

  end;
end;

procedure TConsoleForm.btnHSALAdd1Click(Sender: TObject);
begin
  lvSecIPViolators.Selected:=nil;
  txtSecIPViolator.Clear();
end;

procedure TConsoleForm.btnHSALAddClick(Sender: TObject);
begin
  lvHSAL.Selected:=nil;
  txtHSALHI.Clear();
end;

procedure TConsoleForm.btnSecIPViolatorSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if (lvSecIPViolators.Selected=nil) then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtSecIPViolator.Text);
    itmp^.Enabled:=cbSecIPViolator.Checked;
    case cmboSecIPViolator.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    Storage.Security.Filter.DB.Add(Storage.Main.Task, secViolatorIP,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_IP_Violators_List);

    li:=lvSecIPViolators.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(YES_NO[itmP^.Enabled]);
    li.SubItems.Add(IntToStr(itmP^.Counter));
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvSecIPViolators.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvSecIPViolators.Selected;
    itmP:=li.Data;
    itmp^.Enabled:=cbSecIPViolator.Checked;
    itmP^.Value:=LowerCase(txtSecIPViolator.Text);
    case cmboSecIPViolator.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4);  // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    li.SubItems[0]:=YES_NO[itmP^.Enabled];
    li.SubItems[1]:=IntToStr(itmP^.Counter);
    li.SubItems[2]:=Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF);
    li.SubItems[3]:=itmP^.Value;
    txtSecIPViolator.Text:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secViolatorIP,itmP^);
  end;
end;

procedure TConsoleForm.btnSecIpViolatorSearchClick(Sender: TObject);

  procedure Expires_Today;
  var
    dtSearch:double;
    iDayOfYear,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateDay(dtSearch,iYear,iDayOfYear);
    dtSearch:=DateUtils.EndOfADay(iYear,iDayOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secViolatorIP,txtSecIPViolatorSearch.Text,dtSearch,Security_IP_Violator_Search);
  end;

  procedure Expires_Week;
  var
    dtSearch:double;
    iWeekOfYear,iDayOfWeek,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateWeek(dtSearch,iYear,iWeekOfYear,iDayOfWeek);
    dtSearch:=DateUtils.EndOfAWeek(iYear,iWeekOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secViolatorIP,txtSecIPViolatorSearch.Text,dtSearch,Security_IP_Violator_Search);
  end;

  procedure Expires_Month;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAMonth(iYear,iMonth);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secViolatorIP,txtSecIPViolatorSearch.Text,dtSearch,Security_IP_Violator_Search);
  end;

  procedure Expires_Year;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAYear(iYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secViolatorIP,txtSecIPViolatorSearch.Text,dtSearch,Security_IP_Violator_Search);
  end;
begin
  txtSecIPViolatorSearch.Text:=Lowercase(txtSecIPViolatorSearch.Text);
  case cmboSecIPViolatorSearch.ItemIndex of
    1 : Expires_Today();
    2 : Expires_Week();
    3 : Expires_Month();
    4 : Expires_Year();
    else begin
      Storage.Security.Filter.DB.Find(Storage.Main.Task,secViolatorIP,txtSecIPViolatorSearch.Text,Security_IP_Violator_Search);
    end;
  end;
  LoadFilterItems(secViolatorIP,Security_IP_Violator_Search,lvSecIPViolators);
end;


procedure TConsoleForm.btnSecIPViolatorsReloadClick(Sender: TObject);
begin
  LoadFilterItems(secViolatorIP,Security_IP_Violators_List,lvSecIPViolators);
end;

procedure TConsoleForm.btnHSALReloadClick(Sender: TObject);
begin
  LoadFilterItems(secAcceptableList,Security_Acceptable_List,lvHSAL);
end;

procedure TConsoleForm.btnHSALRemove1Click(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
begin
  if lvSecIPViolators.Selected<>nil then begin
    ItmP:=lvSecIPViolators.Selected.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
    Storage.Security.Filter.Cleanup(Security_IP_Violators_List);
    lvSecIPViolators.Selected.Delete();
  end;
end;


procedure TConsoleForm.btnHSALRemoveClick(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
begin
  if lvHSAL.Selected<>nil then begin
    ItmP:=lvHSAL.Selected.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
    Storage.Security.Filter.Cleanup(Security_Acceptable_List);
    lvHSAL.Selected.Delete();
  end;
end;

procedure TConsoleForm.btnHSALSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if  lvHSAL.Selected=nil then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtHSALHI.Text);
    itmp^.Enabled:=cbSecALDomain.Checked;
    case cmboSecALDomain.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    Storage.Security.Filter.DB.Add(Storage.Main.Task, secAcceptableList,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_Acceptable_List);

    li:=lvHSAL.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(YES_NO[itmP^.Enabled]);
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvHSAL.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvHSAL.Selected;
    itmP:=li.Data;
    itmp^.Enabled:=cbSecALDomain.Checked;
    itmP^.Value:=LowerCase(txtHSALHI.Text);
    case cmboSecALDomain.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    li.SubItems[0]:=YES_NO[itmP^.Enabled];
    li.SubItems[1]:=Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF);
    li.SubItems[2]:=itmP^.Value;
    txtHSALHI.Text:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secAcceptableList,itmP^);
  end;
end;

procedure TConsoleForm.btnHSALSearchClick(Sender: TObject);
  procedure Expires_Today;
  var
    dtSearch:double;
    iDayOfYear,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateDay(dtSearch,iYear,iDayOfYear);
    dtSearch:=DateUtils.EndOfADay(iYear,iDayOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secAcceptableList,txtSecDMALS.Text,dtSearch,Security_AcceptableList_Search);
  end;

  procedure Expires_Week;
  var
    dtSearch:double;
    iWeekOfYear,iDayOfWeek,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateWeek(dtSearch,iYear,iWeekOfYear,iDayOfWeek);
    dtSearch:=DateUtils.EndOfAWeek(iYear,iWeekOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secAcceptableList,txtSecDMALS.Text,dtSearch,Security_AcceptableList_Search);
  end;

  procedure Expires_Month;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAMonth(iYear,iMonth);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secAcceptableList,txtSecDMALS.Text,dtSearch,Security_AcceptableList_Search);
  end;

  procedure Expires_Year;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAYear(iYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secAcceptableList,txtSecDMALS.Text,dtSearch,Security_AcceptableList_Search);
  end;
begin
  txtSecDMALS.Text:=Lowercase(txtSecDMALS.Text);
  case cmboSecDmALSC.ItemIndex of
    1 : Expires_Today();
    2 : Expires_Week();
    3 : Expires_Month();
    4 : Expires_Year();
    else begin
      Storage.Security.Filter.DB.Find(Storage.Main.Task,secAcceptableList,txtSecDMALS.Text,Security_AcceptableList_Search);
    end;
  end;
  LoadFilterItems(secAcceptableList,Security_AcceptableList_Search,lvHSAL);
end;


procedure TConsoleForm.btnHSBLAddClick(Sender: TObject);
begin
  lvHSBL.Selected:=nil;
  txtHSBLHI.Clear();
end;

procedure TConsoleForm.btnHSBLReloadClick(Sender: TObject);
begin
  LoadFilterItems(secBlackList,Security_Block_List,lvHSBL);
end;

procedure TConsoleForm.btnHSBLRemoveClick(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
begin
  if lvHSBL.Selected<>nil then begin
    ItmP:=lvHSBL.Selected.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
    Storage.Security.Filter.Cleanup(Security_Block_List);
    lvHSBL.Selected.Delete();
  end;
end;

procedure TConsoleForm.btnHSBLSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if  lvHSBL.Selected=nil then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtHSBLHI.Text);
    itmp^.Enabled:=cbSecBLDomain.Checked;
    case cmboSecBLDomain.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    Storage.Security.Filter.DB.Add(Storage.Main.Task, secBlackList,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_Block_List);

    li:=lvHSBL.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(YES_NO[itmP^.Enabled]);
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvHSBL.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvHSBL.Selected;
    itmP:=li.Data;
    itmp^.Enabled:=cbSecBLDomain.Checked;
    itmP^.Value:=LowerCase(txtHSBLHI.Text);
    case cmboSecBLDomain.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    li.SubItems[0]:=YES_NO[itmP^.Enabled];
    li.SubItems[1]:=Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF);
    li.SubItems[2]:=itmP^.Value;
    txtHSBLHI.Text:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secBlackList,itmP^);
  end;
end;

procedure TConsoleForm.btnHSBLSearchClick(Sender: TObject);
  procedure Expires_Today;
  var
    dtSearch:double;
    iDayOfYear,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateDay(dtSearch,iYear,iDayOfYear);
    dtSearch:=DateUtils.EndOfADay(iYear,iDayOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secBlackList,txtSecDMBLS.Text,dtSearch,Security_BlackList_Search);
  end;

  procedure Expires_Week;
  var
    dtSearch:double;
    iWeekOfYear,iDayOfWeek,iYear:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    DateUtils.DecodeDateWeek(dtSearch,iYear,iWeekOfYear,iDayOfWeek);
    dtSearch:=DateUtils.EndOfAWeek(iYear,iWeekOfYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secBlackList,txtSecDMBLS.Text,dtSearch,Security_BlackList_Search);
  end;

  procedure Expires_Month;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAMonth(iYear,iMonth);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secBlackList,txtSecDMBLS.Text,dtSearch,Security_BlackList_Search);
  end;

  procedure Expires_Year;
  var
    dtSearch:double;
    iMonth,iYear,iDay:Word;
  begin
    dtSearch:=Core.Timer.dtUT;
    SysUtils.DecodeDate(dtSearch,iYear,iMonth,iDay);
    dtSearch:=DateUtils.EndOfAYear(iYear);
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secBlackList,txtSecDMBLS.Text,dtSearch,Security_BlackList_Search);
  end;
begin
  txtSecDMBLS.Text:=Lowercase(txtSecDMBLS.Text);
  case cmboSecDmBLSC.ItemIndex of
    1 : Expires_Today();
    2 : Expires_Week();
    3 : Expires_Month();
    4 : Expires_Year();
    else begin
      Storage.Security.Filter.DB.Find(Storage.Main.Task,secBlackList,txtSecDMBLS.Text,Security_BlackList_Search);
    end;
  end;
  LoadFilterItems(secBlackList,Security_BlackList_Search,lvHSBL);
end;

procedure TConsoleForm.btnHSCNSearchClick(Sender: TObject);
var
 dtLow      : Double;
 dtHigh     : Double;
 wYear      : Word;
 wDayOfYear : Word;

  procedure FilterWhiteListed;
  begin
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secConnections,FILTER_INC_WHITES,FILTER_NON_WHITES,txtSecCNDomain.Text,dtLow,dtHigh,Security_Connections);
    LoadFilterItems(secConnections,Security_Connections,lvSecCNDomain);
  end;

  procedure FilterBlackListed;
  begin
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secConnections,FILTER_INC_BLACKS,FILTER_NON_BLACKS,txtSecCNDomain.Text,dtLow,dtHigh,Security_Connections);
    LoadFilterItems(secConnections,Security_Connections,lvSecCNDomain);
  end;

  procedure FilterAcceptableListed;
  begin
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secConnections,FILTER_INC_ACCEPTABLE,FILTER_NON_ACCEPTABLE,txtSecCNDomain.Text,dtLow,dtHigh,Security_Connections);
    LoadFilterItems(secConnections,Security_Connections,lvSecCNDomain);
  end;

  procedure FilterWhiteAndBlackAndAcceptableListed;
  begin
    Storage.Security.Filter.DB.Find(Storage.Main.Task,secConnections,FILTER_INC_LISTED,FILTER_NON_LISTED,txtSecCNDomain.Text,dtLow,dtHigh,Security_Connections);
    LoadFilterItems(secConnections,Security_Connections,lvSecCNDomain);
  end;
begin
  Storage.Security.Filter.Empty(Security_Connections);
  lvSecCNDomain.Items.Clear();

  dtHigh:=DateUtils.IncSecond(Core.Timer.dtUT,SEC_CONNECTION_SERVER_TTL);

  DateUtils.DecodeDateDay(dtHigh,wYear,wDayOfYear);
  dtLow:=DateUtils.StartOfADay(wYear,wDayOfYear);

  case cmboSecCNDomain.ItemIndex of
    0 : dtLow:=DateUtils.IncDay(dtLow,-1);       // 1 day
    1 : dtLow:=DateUtils.IncWeek(dtLow,-1);      // 1 week
    2 : dtLow:=DateUtils.IncWeek(dtLow,-4);      // 1 month;
    3 : dtLow:=DateUtils.IncYear(dtLow,-1);      // 1 Year;
  end;
  txtSecCNDomain.Text:=Lowercase(txtSecCNDomain.Text);
  case cmboSecCNStatus.ItemIndex of
    0: FilterWhiteListed();                      // 0 Not white
    1: FilterBlackListed();                      // 1 Not black
    2: FilterAcceptableListed();                 // 2 Not acceptable
    3: FilterWhiteAndBlackAndAcceptableListed(); // 3 Not listed
    else begin
      Storage.Security.Filter.DB.Find(Storage.Main.Task,secConnections,FILTER_INC_LISTED,[],txtSecCNDomain.Text,dtLow,dtHigh,Security_Connections);
      LoadFilterItems(secConnections,Security_Connections,lvSecCNDomain);
    end;
  end;
end;

procedure TConsoleForm.btnHSTLDAddClick(Sender: TObject);
begin
  lvHSTLD.Selected:=nil;
  txtHSTLD.Clear();
end;

procedure TConsoleForm.btnHSTLDReloadClick(Sender: TObject);
begin
  LoadFilterItems(secTopLevelDomains,Security_TLD_List,lvHSTLD);
end;

procedure TConsoleForm.btnHSTLDRemoveClick(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
begin
  if lvHSTLD.Selected<>nil then begin
    ItmP:=lvHSTLD.Selected.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
    Storage.Security.Filter.Cleanup(Security_TLD_List);
    lvHSTLD.Selected.Delete();
  end;
end;

procedure TConsoleForm.btnHSTLDSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if  lvHSTLD.Selected=nil then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtHSTLD.Text);
    itmP^.Enabled:=true;
    itmP^.Expires:=0;
    Storage.Security.Filter.DB.Add(Storage.Main.Task, secTopLevelDomains,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_TLD_List);

    li:=lvHSTLD.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvHSTLD.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvHSTLD.Selected;
    itmP:=li.Data;
    itmp^.Enabled:=true;
    itmP^.Value:=LowerCase(txtHSTLD.Text);
    itmP^.Expires:=0;
    li.SubItems[0]:=itmP^.Value;
    txtHSTLD.Text:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secTopLevelDomains,itmP^);
  end;
end;

procedure TConsoleForm.btnHSTLDSearchClick(Sender: TObject);
begin
  txtSecTLDS.Text:=Lowercase(txtSecTLDS.Text);
  Storage.Security.Filter.DB.Find(Storage.Main.Task,secTopLevelDomains,txtSecTLDS.Text,Security_TLDS_Search);
  LoadFilterItems(secTopLevelDomains,Security_TLDS_Search,lvHSTLD);
end;

procedure TConsoleForm.btnHSWLAddClick(Sender: TObject);
begin
  lvHSWL.Selected:=nil;
  txtHSWLHI.Clear();
end;

procedure TConsoleForm.btnHSWLReloadClick(Sender: TObject);
begin
  LoadFilterItems(secWhiteList,Security_Allow_List,lvHSWL);
end;

procedure TConsoleForm.btnHSWLRemoveClick(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
begin
  if lvHSWL.Selected<>nil then begin
    ItmP:=lvHSWL.Selected.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
    Storage.Security.Filter.Cleanup(Security_Allow_List);
    lvHSWL.Selected.Delete();
  end;
end;

procedure TConsoleForm.LoadFilterItems(const Kind: TDBSecurityKind; var Items: Storage.Security.Filter.Items; ListView: TListView);

  procedure List_ID_EN_EXP_HOST;
  var
    iLcv:integer;
    li:TListItem;
  begin
    for iLcv:=0 to High(Items) do begin
      li:=ListView.Items.Add;
      li.Data:=Items[iLcv];
      li.Caption:=IntToStr(Items[iLcv]^.ID);
      li.SubItems.Add(YES_NO[Items[iLcv]^.Enabled]);
      li.SubItems.Add(Core.Utils.Time.DateTimeToString(Items[iLcv]^.Expires,dtpoShort,BIAS_STAMP_OFF));
      li.SubItems.Add(Items[iLcv]^.Value);
    end;
  end;

  procedure List_ID_CNTR_EXP_HOST;
  var
    iLcv:integer;
    li:TListItem;
  begin
    for iLcv:=0 to High(Items) do begin
      li:=ListView.Items.Add;
      li.Data:=Items[iLcv];
      li.Caption:=IntToStr(Items[iLcv]^.ID);
      li.SubItems.Add(IntToStr(Items[iLcv]^.Counter));
      li.SubItems.Add(Core.Utils.Time.DateTimeToString(Items[iLcv]^.Expires,dtpoShort,BIAS_STAMP_OFF));
      li.SubItems.Add(Items[iLcv]^.Value);
    end;
  end;

  procedure List_ID_ENABLED_CNTR_EXP_HOST;
  var
    iLcv:integer;
    li:TListItem;
  begin
    for iLcv:=0 to High(Items) do begin
      li:=ListView.Items.Add;
      li.Data:=Items[iLcv];
      li.Caption:=IntToStr(Items[iLcv]^.ID);
      li.SubItems.Add(YES_NO[Items[iLcv]^.Enabled]);
      li.SubItems.Add(IntToStr(Items[iLcv]^.Counter));
      li.SubItems.Add(Core.Utils.Time.DateTimeToString(Items[iLcv]^.Expires,dtpoShort,BIAS_STAMP_OFF));
      li.SubItems.Add(Items[iLcv]^.Value);
    end;
  end;

  procedure List_ID_HOST;
  var
    iLcv:integer;
    li:TListItem;
  begin
    for iLcv:=0 to High(Items) do begin
      li:=ListView.Items.Add;
      li.Data:=Items[iLcv];
      li.Caption:=IntToStr(Items[iLcv]^.ID);
      li.SubItems.Add(Items[iLcv]^.Value);
    end;
  end;

begin
  ListView.Items.BeginUpdate();
  Try
    ListView.Items.Clear();
    //dbmSecurity.Security_List_Fill(Storage.Main.Task,Kind,Items);
    case Kind of
      secContentFilter   : List_ID_EN_EXP_HOST();
      secBlackList       : List_ID_EN_EXP_HOST();
      secWhiteList       : List_ID_EN_EXP_HOST();
      secViolatorIP      : List_ID_ENABLED_CNTR_EXP_HOST();
      secContentProfiles : List_ID_EN_EXP_HOST();
      secWLService       : List_ID_HOST();
      secBLService       : List_ID_HOST();
      secConnections     : List_ID_CNTR_EXP_HOST();
      secAcceptableList  : List_ID_EN_EXP_HOST();
      secTopLevelDomains : List_ID_HOST();
    end;
  finally
    ListView.Items.EndUpdate();
  end;
end;


procedure TConsoleForm.btnHSWLSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if  lvHSWL.Selected=nil then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtHSWLHI.Text);
    itmp^.Enabled:=cbSecWLDomain.Checked;
    case cmboSecWLDomain.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    Storage.Security.Filter.DB.Add(Storage.Main.Task, secWhiteList,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_Allow_List);

    li:=lvHSWL.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(YES_NO[itmP^.Enabled]);
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvHSWL.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvHSWL.Selected;
    itmP:=li.Data;
    itmp^.Enabled:=cbSecWLDomain.Checked;
    itmP^.Value:=LowerCase(txtHSWLHI.Text);
    case cmboSecWLDomain.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    li.SubItems[0]:=YES_NO[itmP^.Enabled];
    li.SubItems[1]:=Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF);
    li.SubItems[2]:=itmP^.Value;
    txtHSWLHI.Text:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secWhiteList,itmP^);
  end;
end;

procedure TConsoleForm.btnKeywordEditingDoneClick(Sender: TObject);
var
  liItem:TListItem;
  bModified:Boolean;
begin
  if FKeywordP<>nil then begin
    bModified:=txtKeywordName.Modified;
    if bModified then
      FKeywordP^.Name:=txtKeywordName.Text;
    if txtKeywordValue.Modified then begin
      FKeywordP^.Value:=txtKeywordValue.Text;
      bModified:=True;
    end;
    if bModified then begin
      Storage.Keywords.Items.DB.Save(Storage.Main.Task,FKeywordP^);
      if Core.Utils.ListView.IndexOf(lvKeywords,FKeywordP,liItem)<>-1 then begin
        liItem.Caption:=FKeywordP^.Name;
        liItem.SubItems[0]:=FKeywordP^.Value;
      end;
    end;
  end;
  {$i frmConsole.Domain.Keywords.EndEdit.inc}
end;

procedure TConsoleForm.btnDomainsDeleteClick(Sender: TObject);
var
  iIndex:integer;
begin
  if lvDomains.Selected=nil then exit;
  FDomainP:=lvDomains.Selected.Data;
  if Form.YesNo.ShowMessage(
    Format(FMT_CONFIRM_DOMAIN_DELETE_CAPTION,[FDomainP^.Name]),
    Format(FMT_CONFIRM_DOMAIN_DELETE_TITLE,[FDomainP^.Name]),
    Format(FMT_CONFIRM_DOMAIN_DELETE_MESSAGE,[FDomainP^.Name])
  )= mrYes then begin
    Storage.Domains.Items.DB.Delete(Storage.Main.Task,FDomainP^);
    iIndex:=cbDomain_Host.Items.IndexOf(FDomainP^.Name);
    if iIndex<>-1 then
      cbDomain_Host.Items.Delete(iIndex);
    lvDomains.Selected.Delete;
    FDomainP:=nil;
  end;
end;

procedure TConsoleForm.btnDomain_User_DoneClick(Sender: TObject);
var
 iLcv:integer;
 s1,s2:AnsiString;
begin
  // This account is edited.  Write settings and Save item...
  GUI.Lock;
  Try
    {$i frmConsole.Domain.Users.User.Write.inc}
    Storage.UserAccounts.Items.DB.Save(Storage.Main.Task,FUserP^);
    {$i frmConsole.Domain.Users.User.Clear.inc}
    if lvUsers.Selected<>nil then begin
      lvUsers.Selected.SubItems[1]:=FUserP^.First;
      lvUsers.Selected.SubItems[2]:=FUserP^.Last;
      lvUsers.Selected.SubItems[3]:=FUserP^.Telephone;
      lvUsers.Selected.SubItems[4]:=IntToStr(FUserP^.LockoutCount);
      lvUsers.Selected.SubItems[5]:=Core.Strings.toString(FUserP^.Quota);
      lvUsers.Selected.SubItems[6]:=Core.Strings.toString(FUserP^.Consumption);
    end;
    Storage.UserAccounts.Items.DB.Fill(Storage.Main.Task,FUserP^.ID,FUserP^);
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.btnProviderLandingPageEditClick(Sender: TObject);
begin
  {$i frmConsole.SearchProviders.Edit.inc}
end;

procedure TConsoleForm.btnRaidChangeGroupClick(Sender: TObject);
var
 sError:Core.Strings.VarString;
 iID:QWord;
begin
  SetLength(sError,0);
  FRaidGroupID.Value:=txtRaidGroupID.Text;
  FRaidGroupName.Value:=txtRaidGroupName.Text;

  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FRaidGroupID);
  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FRaidGroupName);
  {$ifdef Unix}
  iID:=Core.Utils.Unix.Account.GroupID(FRaidGroupName.Value,sError);
  If iID=0 then
    Core.Utils.Unix.Account.GroupAdd(FRaidGroupName.Value,FRaidGroupID.Value,sError)
  else
    Core.Utils.Unix.Account.GroupMod(FRaidGroupName.Value,FRaidGroupID.Value,sError);
  {$endif}
  if System.Length(sError)>0 then
    lblStartupMessage.Caption:=sError
  else
    lblStartupMessage.Caption:='Ok';
end;

procedure TConsoleForm.btnRaidUserChangeClick(Sender: TObject);
var
 sError:Core.Strings.VarString;
 iID:QWord;
begin
  SetLength(sError,0);
  FRaidUserID.Value:=txtRaidUserID.Text;
  FRaidUserName.Value:=txtRaidUserName.Text;

  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FRaidUserID);
  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FRaidUserName);
  {$ifdef Unix}
  iID:=Core.Utils.Unix.Account.UserID(FRaidUserName.Value,sError);
  If iID=0 then
    Core.Utils.Unix.Account.UserAdd(FRaidUserName.Value,FRaidGroupName.Value,'/dev/null',FRaidUserID.Value,sError)
  else
    Core.Utils.Unix.Account.UserMod(FRaidUserName.Value,FRaidUserID.Value,sError);
  {$endif}
  if System.Length(sError)>0 then
    lblStartupMessage.Caption:=sError
  else
    lblStartupMessage.Caption:='Ok';
end;

procedure TConsoleForm.btnSecContentSearchClick(Sender: TObject);
begin
  txtSecContentSearch.Text:=Lowercase(txtSecContentSearch.Text);
  Storage.Security.Filter.DB.Find(Storage.Main.Task,secContentFilter,txtSecContentSearch.Text,Security_Content_Search);
  LoadFilterItems(secContentFilter,Security_Content_Search,lvContentFilters);
end;

procedure TConsoleForm.btnSecIPViolatorsToolClearClick(Sender: TObject);
begin
  Storage.Security.Filter.DB.Clear(Storage.Main.Task,secViolatorIP);
  lvSecIPViolators.Clear();
  Storage.Security.Filter.Empty(Security_IP_Violators_List);

end;

procedure TConsoleForm.btnSecIPVoiolatorAddClick(Sender: TObject);
begin
  lvSecIPViolators.Selected:=nil;
  txtSecIPViolator.Clear();
end;

procedure TConsoleForm.btnSecIPVRemoveClick(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
  iCount:Integer;
  iLcv:integer;
begin
  iLcv:=0; iCount:=lvSecIPViolators.Items.Count;

  While (iLcv<iCount) do begin
    if (lvSecIPViolators.Items[iLcv].Selected=true) then begin
      ItmP:=lvSecIPViolators.Items[iLcv].Data;
      Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
      Storage.Intrusion.Intruder.DB.Delete(Storage.Main.Task,FDomainP^.ID,Core.Utils.Sockets.InAddrFromStr(ItmP^.Value));
      Storage.Security.Filter.Cleanup(Security_IP_Violators_List);
      lvSecIPViolators.Items[iLcv].Delete();
      Dec(iCount);
    end else
      Inc(iLcv);
  end;
end;

procedure TConsoleForm.btnSecProfileAddClick(Sender: TObject);
begin
  lvProfileFilters.Selected:=nil;
  txtSecProfile.Enabled:=true;
  btnSecProfileContentSave.Enabled:=true;
  txtSecProfile.Clear();
end;

procedure TConsoleForm.btnSecProfileContentSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if  lvProfileFilters.Selected=nil then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtSecProfile.Text);
    itmP^.Enabled:=cbSecContentProfile.Checked;
    case cmboSecContentProfile.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;
    Storage.Security.Filter.DB.Add(Storage.Main.Task, secContentProfiles,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_Content_Profiles);

    li:=lvProfileFilters.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(YES_NO[itmP^.Enabled]);
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvProfileFilters.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvProfileFilters.Selected;
    itmP:=li.Data;
    itmP^.Value:=LowerCase(txtSecProfile.Text);
    itmP^.Enabled:=cbSecContentProfile.Checked;
    case cmboSecContentProfile.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    txtSecProfile.Text:=itmP^.Value;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems[0]:=YES_NO[itmP^.Enabled];
    li.SubItems[1]:=Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF);
    li.SubItems[2]:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secContentProfiles,itmP^);
  end;
end;

procedure TConsoleForm.btnSecContentSaveClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if  lvContentFilters.Selected=nil then begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);
    itmP^.Value:=LowerCase(txtSecContent.Text);
    itmp^.Enabled:=cbSecContentPhrase.Checked;
    case cmboSecContentPhrase.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;

    Storage.Security.Filter.DB.Add(Storage.Main.Task, secContentFilter,itmP^);
    Storage.Security.Filter.Push(ItmP,Security_Content_Filters);

    li:=lvContentFilters.Items.Insert(0);
    li.Data:=itmP;
    li.Caption:=IntToStr(itmP^.ID);
    li.SubItems.Add(YES_NO[itmP^.Enabled]);
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(itmP^.Value);
    li.MakeVisible(false);

    lvContentFilters.Selected:=li;
    li.Selected:=true;
  end else begin
    li:=lvContentFilters.Selected;
    itmP:=li.Data;
    itmp^.Enabled:=cbSecContentPhrase.Checked;
    itmP^.Value:=LowerCase(txtSecContent.Text);
    case cmboSecContentPhrase.ItemIndex of
      0: itmP^.Expires:=DateUtils.IncDay(dtUT,1);   // 0 1 Day
      1: itmP^.Expires:=DateUtils.IncWeek(dtUT,1);  // 1 1 Week
      2: itmP^.Expires:=DateUtils.IncWeek(dtUT,4); // 2 1 Month
      3: itmP^.Expires:=DateUtils.IncYear(dtUT,1);  // 3 1 Year
    end;
    li.SubItems[0]:=YES_NO[itmP^.Enabled];
    li.SubItems[1]:=Core.Utils.Time.DateTimeToString(itmP^.Expires,dtpoShort,BIAS_STAMP_OFF);
    li.SubItems[2]:=itmP^.Value;
    txtSecContent.Text:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task, secContentFilter,itmP^);
  end;
end;

procedure TConsoleForm.btnSecProfileDelClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if lvProfileFilters.Selected=nil then exit;

  li:=lvProfileFilters.Selected;
  itmP:=li.Data;

  Storage.Security.Filter.DB.Delete(Storage.Main.Task,itmP^);

  itmP^.Stale:=true;
  Storage.Security.Filter.Cleanup(Security_Content_Profiles);

  txtSecProfile.Enabled:=false;
  btnSecProfileContentSave.Enabled:=false;
  txtSecProfile.Clear();

  li.Delete();
end;

procedure TConsoleForm.btnSecProfileSearchClick(Sender: TObject);
begin
  txtSecProfileSearch.Text:=Lowercase(txtSecProfileSearch.Text);
  Storage.Security.Filter.DB.Find(Storage.Main.Task,secContentProfiles,txtSecProfileSearch.Text,Security_Profile_Search);
  LoadFilterItems(secContentProfiles,Security_Profile_Search,lvProfileFilters);
end;

procedure TConsoleForm.btnUser_Edit_CancelClick(Sender: TObject);
var
  iLcv:integer;
begin
  {$i frmConsole.Domain.Users.User.Clear.inc}
end;

procedure TConsoleForm.btnDomainsNew_OKClick(Sender: TObject);
var
  iLen:Integer;
  Domain:Storage.Domains.Items.TDomain;
  liItem:TListItem;
  iaDomain:Core.Arrays.Types.LargeWord;
begin
  if Storage.Domains.Items.DB.IsInternal(Storage.Main.Task,txtDomains_Hostname.Text) then begin
    // Domain is already present.  Display miDomainLicenseEdit mode Yes No here
  end else begin
    // Ok do go ahead and add domain into system
    Storage.Domains.Items.Init(Domain);
    Try
      Domain.Name:=txtDomains_Hostname.Text;
      Domain.FriendlyName:=txtDomains_FriendlyName.Text;
      Domain.Root:=txtDomains_Postmaster.Text;

      if Storage.Domains.Items.DB.Create(Storage.Main.Task,Domain) then begin
        // First Create New Postmaster Account.
        Storage.UserAccounts.Items.DB.Create(Storage.Main.Task,@Domain);
        Storage.UserAccounts.Items.DB.CreateDefault(Storage.Main.Task,@Domain);
        // Close this little window and present them with the screen with
        // more advanced options.
        iLen:=Length(Domains);
        SetLength(Domains,iLen+1);
        Storage.Domains.Items.Copy(Domain,Domains[iLen]);
        liItem:=lvDomains.Items.Add;
        liItem.Caption:=IntToStr(Domain.ID);
        liItem.SubItems.Add('1');
        liItem.SubItems.Add(Domain.Name);
        liItem.Data:=@Domains[iLen];
        cbDomain_Host.Items.Add(Domain.Name);

        gbDomains_DomainInformation.Visible:=false;

        Core.Arrays.LargeWord.Init(iaDomain);
        Try
          Core.Arrays.LargeWord.Add(Domain.ID,iaDomain);
          Storage.MatrixServices.Items.DB.Verify(Storage.Main.Task,Storage.MatrixServices.Default.Cluster,Storage.MatrixServices.Default.Resource,Storage.MatrixServices.Default.Node,iaDomain,FDefaultServices);
        Finally
          Core.Arrays.LargeWord.Empty(iaDomain);
        end;

        Display_Domain_Editor(@Domains[iLen]);
      end;
    finally
      Storage.Domains.Items.Done(Domain);
    end;
  end;
end;

procedure TConsoleForm.btnDomainsEditClick(Sender: TObject);
begin
  if lvDomains.Selected<>nil then begin
    GUI.Lock;
    Try
      Display_Domain_Editor(lvDomains.Selected.Data);
    finally
      GUI.UnLock;
    end;
  end;
end;

procedure TConsoleForm.btnDomainsNewClick(Sender: TObject);
begin
  GUI.Lock;
  Try
    txtDomains_Hostname.Clear;
    txtDomains_FriendlyName.Clear;
    txtDomains_Postmaster.Text:='postmaster';
    gbDomains_DomainInformation.Visible:=true;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.btnDomain_User_New_DoneClick(Sender: TObject);
var
  liItem:TListItem;
  sUser:Core.Strings.VarString;
begin
  // Check to see if User is already present
  sUser:=txtUsers_New_Account.Text;
  GUI.Lock;
  Try
    if Storage.UserAccounts.Items.DB.Exists(Storage.Main.Task,sUser,FDomainP^.ID) then begin
      // Display Dialog Box showing that the user with that name can't be added again.
      txtUsers_New_Account.SetFocus;
    end else begin
      FUAP:=FFindUsers.Acquire(sUser);
      FUAP^.DomainID:=FDomainP^.ID;
      FUAP^.First:=txtUsers_New_First.Text;
      FUAP^.Last:=txtUsers_New_Last.Text;
      FUAP^.Password:=txtUsers_New_Password.Text;
      FUAP^.Enabled:=true;
      FUAP^.Quota:=FDomainP^.DefaultOptionQuota;
      FUAP^.Throttle:=Storage.UserAccounts.Items.Throttle.Level1.Limit;
      Core.Arrays.LargeWord.Copy(Default.aclCoreCommands,FUAP^.aclCoreCommands);
      Core.Arrays.LargeWord.Copy(Default.aclCoreObjects,FUAP^.aclCoreObjects);

      Storage.Roster.Items.Empty(FContact);
      FContact.Email1:=Concat(sUser,'@',FDomainP^.Name);
      FContact.FirstName:=FUAP^.First;
      FContact.LastName:=FUAP^.Last;

      if Storage.UserAccounts.Items.DB.Create(Storage.Main.Task,FDomainP^.ID,FUAP^,FContact) then begin
        liItem:=lvUsers.Items.Add;
        liItem.Data:=@FUAP^.User;
        liItem.Caption:=IntToStr(FUAP^.ID);
        liItem.SubItems.Add(sUser);
        liItem.SubItems.Add(FUAP^.First);
        liItem.SubItems.Add(FUAP^.Last);
        liItem.SubItems.Add('');
        liItem.SubItems.Add('0');
        liItem.SubItems.Add('0');
        liItem.SubItems.Add('0');
        {$i frmConsole.Domains.Users.Clear.inc}
        If lvDomains.Selected<>nil then
          lvDomains.Selected.SubItems[LI_IDX_DOMAINS_USERCOUNT]:=IntToStr(Storage.UserAccounts.Items.DB.Count(Storage.Main.Task,FDomainP^.ID));
      end;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.btnWLDNSItemAddClick(Sender: TObject);
begin
  if lvDNSWL.Selected<>nil then
    lvDNSWL.Selected.Selected:=false;
  lvDNSWL.Selected:=nil;
  txtSecWhiteListDNS.Clear();
  btnWLDNSItemRemove.Enabled:=false;
end;

procedure TConsoleForm.btnDNSItemRemoveClick(Sender: TObject);
var
  itmP:Storage.DNS.Items.PItem;
begin
  GUI.Lock;
  Try
    btnDNSItemRemove.Enabled:=false;
    btnDNSItemAdd.Enabled:=true;
    txtDNSIP1.Clear;
    txtDNSIP2.Clear;
    txtDNSIP3.Clear;
    txtDNSIP4.Clear;
    if lvDNSRegular.Selected<>nil then begin
      itmP:=lvDNSRegular.Selected.Data;
      Storage.DNS.Items.DB.Delete(Storage.Main.Task,itmP^);
      Storage.DNS.Items.Remove(DNS_Hosts,itmP);
      lvDNSRegular.Selected.Delete;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.btnProcessGroupChangeClick(Sender: TObject);
var
  sMessage:Core.Strings.VarString;
  iID:QWord;
begin
  SetLength(sMessage,0);
  FProcessGroupID.Value:=txtProcessGroupID.Text;
  Storage.MatrixNodes.Node.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);
  Storage.AuraDisks.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);
  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FProcessGroupID);
  {$ifdef Unix}
  iID:=Core.Utils.Unix.Account.GroupID(txtProcessGroupName.Text,sMessage);
  If iID=0 then
    Core.Utils.Unix.Account.GroupAdd(txtProcessGroupName.Text,FProcessGroupID.Value,sMessage)
  else
    Core.Utils.Unix.Account.GroupMod(txtProcessGroupName.Text,FProcessGroupID.Value,sMessage);
  {$endif}

  if System.Length(sMessage)>0 then
    lblStartupMessage.Caption:=sMessage
  else
    lblStartupMessage.Caption:='Ok';
end;

procedure TConsoleForm.btnProcessUserChangeClick(Sender: TObject);
var
  sError:Core.Strings.VarString;
  iID:QWord;
begin
  setLength(sError,0);

  FProcessUserID.Value:=txtProcessUserID.Text;

  Storage.MatrixNodes.Node.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);
  Storage.AuraDisks.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);

  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FProcessUserID);

  FProcessUserID.Value:=txtProcessUserID.Text;

  Storage.ConfigData.Items.DB.Write(Storage.Main.Task,FProcessUserID);
  {$ifdef Unix}
  iID:=Core.Utils.Unix.Account.UserID(txtProcessUserName.Text,sError);
  If iID=0 then
    Core.Utils.Unix.Account.UserAdd(txtProcessUserName.Text,txtProcessGroupName.Text,'/dev/null',FProcessUserID.Value,sError)
  else
    Core.Utils.Unix.Account.UserMod(txtProcessUserName.Text,FProcessUserID.Value,sError);
  {$endif}

  if System.Length(sError)>0 then
    lblStartupMessage.Caption:=sError
  else
    lblStartupMessage.Caption:='Ok';
end;

procedure TConsoleForm.btnSecContentAddClick(Sender: TObject);
begin
  lvContentFilters.Selected:=nil;
  txtSecContent.Enabled:=true;
  btnSecContentSave.Enabled:=true;
  txtSecContent.Clear();
end;

procedure TConsoleForm.btnSecContentDelClick(Sender: TObject);
var
  itmP:Storage.Security.Filter.PItem;
  li:TListItem;
begin
  if lvContentFilters.Selected=nil then exit;

  While lvContentFilters.SelCount>0 do begin;
    li:=lvContentFilters.Selected;
    itmP:=li.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task, itmP^);
    itmP^.Stale:=true;
    li.Delete();
  end;
  Storage.Security.Filter.Cleanup(Security_Content_Filters);

  txtSecContent.Enabled:=false;
  btnSecContentSave.Enabled:=false;
  txtSecContent.Clear();

  li.Delete();
end;

procedure TConsoleForm.btnWLDNSItemSaveClick(Sender: TObject);
var
  iIndex:integer;
  liDNS:TListItem;
  itmP:Storage.Security.Filter.PItem;
begin
  liDNS:=lvDNSWL.Selected;
  if (liDNS<>nil) then begin
    itmP:=liDNS.Data;
    itmP^.Enabled:=true;
    itmP^.Value:=Lowercase(txtSecWhiteListDNS.Text);
    liDNS.SubItems[0]:=itmP^.Value;
    Storage.Security.Filter.DB.Edit(Storage.Main.Task,secWLService,itmP^);
  end else begin
    new(itmP);
    Storage.Security.Filter.Init(itmP^);

    iIndex:=Length(Security_WL_Services);
    SetLength(Security_WL_Services,iIndex+1);
    Security_WL_Services[iIndex]:=itmP;

    itmP^.Enabled:=true;
    itmP^.Value:=Lowercase(txtSecWhiteListDNS.Text);

    Storage.Security.Filter.DB.Add(Storage.Main.Task,secWLService,itmP^);

    liDNS:=lvDNSWL.Items.Insert(0);
    liDNS.Data:=itmP;
    liDNS.Caption:=IntToStr(itmP^.ID);
    liDNS.SubItems.Add(itmP^.Value);
    lvDNSWL.Selected:=liDNS;
    liDNS.MakeVisible(false);
  end;
end;

procedure TConsoleForm.cbClusterNodeEnabledClick(Sender: TObject);
var
  iDX:integer;
  msP:Storage.MatrixServices.Items.PItem;
begin
  if GUI.Locked or (lvClusterNodeService.Selected=nil) then exit;
  msP:=lvClusterNodeService.Selected.Data;
  iDX:=cmboDiskNodes.ItemIndex;
  if iDX<>-1 then begin
    msP^.Enabled:=cbClusterNodeEnabled.Checked;
    lvClusterNodeService.Selected.Checked:=msP^.Enabled;
    lvClusterNodeService.Selected.SubItems[0]:=YES_NO[msP^.Enabled];
    Storage.MatrixServices.Items.DB.SetEnabled(Storage.Main.Task,msP^)
  end;
end;

procedure TConsoleForm.cbDCDomainsChange(Sender: TObject);
begin
  btnDCAAllocate.Enabled:=cbDCDomains.ItemIndex<>-1;
end;

procedure TConsoleForm.cbDomain_DefaultsItemClick(Sender: TObject;
  Index: integer);
begin
  if GUI.Locked then Exit;
  Case Index of
    0 : FDomainP^.DefaultOptionFiltering:=cbDomain_Defaults.State[Index]=cbChecked;
    1 : FDomainP^.DefaultOptionCatchAll:=cbDomain_Defaults.State[Index]=cbChecked;
  end;
  Storage.Domains.Items.DB.Update(Storage.Main.Task,FDomainP^);
end;

procedure TConsoleForm.cbDomain_ServiceChange(Sender: TObject);
begin
  if GUI.Locked then Exit;
  {$i frmConsole.Domain.Service.EnableChange.inc}
end;

procedure TConsoleForm.cmboDiskClustersChange(Sender: TObject);
var
  iLcv:integer;
begin
  cmboDiskResources.Clear();
  cmboDiskNodes.Clear();
  Storage.MatrixResources.Resource.Empty(FDiskResource);
  Storage.MatrixNodes.Node.Empty(FDisks);
  if cmboDiskClusters.ItemIndex<>-1 then begin
    Storage.MatrixResources.Resource.DB.List(Storage.Main.Task,FClusters[cmboDiskClusters.ItemIndex]^.ID,FDiskResource);
    for iLcv:=0 to High(FDiskResource) do
      cmboDiskResources.Items.Add(FDiskResource[iLcv]^.Name);
    {
    Storage.MatrixNodes.MatrixNodes_Disks(Storage.Main.Task,FClusters[cmboDiskClusters.ItemIndex]^.ID,FDisks);
    for iLcv:=0 to High(FDisks) do
      cmboDiskResources.Items.Add(FDisks[iLcv]^.Alias);
    }
  end;
  cmboDiskResources.Enabled:=(System.Length(FDiskResource)>0);
  cmboDiskNodes.Enabled:=(System.Length(FDisks)>0);
  btnDiskNodeRefresh.Enabled:=(System.Length(FDisks)>0);
end;

procedure TConsoleForm.cmboDiskResourcesChange(Sender: TObject);
var
  iLcv:integer;
begin
  Storage.MatrixNodes.Node.Empty(FDisks);
  cmboDiskNodes.Clear();
  if cmboDiskResources.ItemIndex<>-1 then begin
    Storage.MatrixNodes.Node.DB.Disks(
      Storage.Main.Task,
      FClusters[cmboDiskClusters.ItemIndex]^.ID,
      FDiskResource[cmboDiskResources.ItemIndex]^.ID,
      FDisks
    );
    for iLcv:=0 to High(FDisks) do
      cmboDiskNodes.Items.Add(FDisks[iLcv]^.Alias);
  end;
  cmboDiskResources.Enabled:=(System.Length(FDiskResource)>0);
  cmboDiskNodes.Enabled:=(System.Length(FDisks)>0);
  btnDiskNodeRefresh.Enabled:=(System.Length(FDisks)>0);
end;

procedure TConsoleForm.cmboPurchaseKindChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  if FPurchaseP<>nil then begin
    FPurchaseP^.Kind:=cmboPurchaseKind.ItemIndex;
    lvPurchase.Selected.SubItems[2]:=Purchase.Kind.List[FPurchaseP^.Kind];
    Storage.Commerce.Purchase.Item.DB.Write(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  end;
end;

procedure TConsoleForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:=caFree;
  Application.Terminate;
  Core.Utils.Forms.List.UnLoad(FInfoP^);
end;

procedure TConsoleForm.lvCertsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if GUI.Locked then Exit;
  TI_CertSelect.Expires:=IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvClusterNodeServiceSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if GUI.Locked then Exit;
  TI_NodeServiceSelection.Expires:=DateUtils.IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvClusterNodesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if (Item<>nil) and gbCRNode.Visible then begin
      GUI.Lock;
      Try
        If (Item.Selected) then begin
          FClusterNodeP:=Item.Data;
          {$i frmConsole.Clusters.LayoutManager.Set.NodeEditor.inc}
        end;
      finally
        GUI.UnLock;
      end;

  end else begin
    {$i frmConsole.Clusters.LayoutManager.Hide.NodeEditor.inc}
  end;
end;

procedure TConsoleForm.lvContentFiltersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  TI_SecContSelect.Expires:=DateUtils.IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvDNSRegularSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  TI_DNSSelect.Expires:=DateUtils.IncMillisecond(Now,500);
end;

procedure TConsoleForm.lvHSALSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  TI_SecALDSelect.Expires:=DateUtils.IncMillisecond(Now,500);
end;

procedure TConsoleForm.lvHSBLSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  TI_SecBLDSelect.Expires:=DateUtils.IncMillisecond(Now,500);
end;

procedure TConsoleForm.lvProfileFiltersSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  TI_SecProfileSelect.Expires:=DateUtils.IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvPurchaseSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if GUI.Locked then Exit;
  TI_Purchase.Expires:=IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvSecCNDomainDblClick(Sender: TObject);
begin
  if (lvSecCNDomain.Selected<>nil) then begin
     LCLIntf.OpenURL(Concat('http://',lvSecCNDomain.Selected.SubItems[2],'/'));
  end;
end;

procedure TConsoleForm.lvSecCNDomainSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  if Selected then begin
    txtSecCNDomain.Text:=Item.SubItems[2];
  end else
    txtSecCNDomain.Clear();
end;

procedure TConsoleForm.lvSecIPViolatorsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key=46) then begin
    Key:=0;
    btnSecIPVRemoveClick(Sender);
  end;
end;

procedure TConsoleForm.lvSecIPViolatorsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  TI_SecIPVSelect.Expires:=DateUtils.IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvUserACLCoreCommandsDblClick(Sender: TObject);
var
  Item:TListItem;
begin
  Item:=lvUserACLCoreCommands.Selected;
  if GUI.Locked or (Item=nil) or (Item.Data=nil) then exit;
  FCoreCommand:=Item.Data;
  Item.Checked:=not Item.Checked;
  FCoreCommand^.Enabled:=Item.Checked;
  Item.Caption:=Yes_No[FCoreCommand^.Enabled];
  If Item.Checked then
    Grant(FCoreCommand^,FUserP)
  else
    Deny(FCoreCommand^,FUserP);
  Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
end;

procedure TConsoleForm.lvUserACLCoreObjectsDblClick(Sender: TObject);
var
  Item:TListItem;
begin
  Item:=lvUserACLCoreObjects.Selected;
  if (Item=nil) or (Item.Data=nil) or (GUI.Locked) then exit;
  if Item.Selected then
    lvUserACLCoreObjectsSelectItem(Sender,Item,True);
end;

procedure TConsoleForm.lvUserACLCoreObjectsItemChecked(Sender: TObject;
  Item: TListItem);
begin
  FCoreObject:=TCoreObject(Item.Data);
  //Item.Checked:=Item.Checked;
  If (FCoreObject.Header.Enabled<>Item.Checked) then begin
    FCoreObject.Header.Enabled:=Item.Checked;
    Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
    // Enable all commands for user
    if FCoreObject.Header.Enabled then
      Grant(FCoreObject.Header,FUserP)
    else
      Deny(FCoreObject.Header,FUserP);
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end;
end;

procedure TConsoleForm.miDomainCertImportClick(Sender: TObject);
var
  Cert:TCertData;
begin
  if odKeyImport.Execute() then begin
    Core.Utils.Files.fromString(
      Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,No_Quotes),
      Core.Utils.Files.toString(odKeyImport.FileName)
    );
    frmRSA.RSAGen.ImportKey();
    Init(Cert);
    Try
      Cert.Key:=Core.Utils.Files.toString(Storage.Certs.GetKeyFile(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES));

      Core.Arrays.Bytes.fromFile(Storage.Certs.GetKeyFileAsDer(App.Folders.UserSSL(),FDomainP^.Name,NO_QUOTES),Cert.DerKey);
      Cert.Date:=DateUtils.IncYear(dtUT,1);
      Cert.Request:='Private key was supplied.';
      Storage.Certs.Items.DB.Add(Storage.Main.Task,FDomainP^.ID,Cert);
    finally
      Done(Cert);
    end;
    LoadSSLInfo;
  end;
end;

procedure TConsoleForm.miLoadServerIntermediateCertClick(Sender: TObject);
var
  sCertFile    : Core.Strings.VarString;
  sDerCertFile : Core.Strings.VarString;

  sException   : Core.Strings.VarString;
  iDerCertLen  : LongInt;
  X509         : PX509;
  bData        : Core.Arrays.Types.Bytes;

  {$i frmConsole.Process_DER_CERT.inc}
begin
  if (FCertP<>nil) then begin
    if odServerAuthorityCert.Execute() then begin
      if odIntermediateAuthorityCert.Execute() then begin
        if odCert.Execute() then begin
          FCertP^.Level:=3;
          Empty(FCertP^.DerCerts);
          Empty(FCertP^.Certs);

          SetLength(FCertP^.DerCerts,3);
          SetLength(FCertP^.Certs,3);

          // Certificate
          sCertFile:=odCert.FileName;
          sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
          Process_DER_CERT();
          FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
          Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);

          // Server CERT
          sCertFile:=odServerAuthorityCert.FileName;
          sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,2,NO_QUOTES);
          Process_DER_CERT();
          FCertP^.Certs[1]:=Core.Utils.Files.toString(sCertFile);
          Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[1]);

          // Intermediate CERT
          sCertFile:=odIntermediateAuthorityCert.FileName;
          sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,3,NO_QUOTES);
          Process_DER_CERT();
          FCertP^.Certs[2]:=Core.Utils.Files.toString(sCertFile);
          Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[2]);

          if Length(FCertP^.DerCerts[0])>0 then begin
            Try
              iDerCertLen:=Core.Arrays.Bytes.Copy(FCertP^.DerCerts[0],bData);
              X509:=Encryption.SSL.d2i_X509(nil,@bData,iDerCertLen);
              if X509<>nil then begin
                Try
                  FCertP^.Date:=Encryption.SSL.ToDateTime(X509^.cert_info^.validity^.notAfter);
                finally
                  Encryption.SSL.X509_free(X509);
                end;
              end;
            Finally
              Core.Arrays.Bytes.Done(bData);
            end;
            Storage.Certs.Items.DB.Write(Storage.Main.Task,FDomainP^.ID,FCertP^);
            LoadSSLInfo();
          end else begin
           sException:=Core.Streams.toMemo(procSelfSign.Output);
           lblDSCStatus.Caption:='Error loading certificate.';
           Form.Exception.ShowException('frmConsole.pas','Loading error',sException);
          end;
        end;
      end;
    end;
  end;
End;

procedure TConsoleForm.miLoadRootServerIntCertClick(Sender: TObject);
var
  sCertFile    : Core.Strings.VarString;
  sDerCertFile : Core.Strings.VarString;

  sException   : Core.Strings.VarString;
  iDerCertLen  : LongInt;
  X509         : PX509;
  bData        : Core.Arrays.Types.Bytes;

  {$i frmConsole.Process_DER_CERT.inc}

begin
  if (FCertP<>nil) then begin
    if odRootAuthorityCert.Execute() then begin
      if odServerAuthorityCert.Execute() then begin
        if odIntermediateAuthorityCert.Execute() then begin
          if odCert.Execute() then begin
            FCertP^.Level:=4;
            Empty(FCertP^.DerCerts);
            Empty(FCertP^.Certs);

            SetLength(FCertP^.DerCerts,4);
            SetLength(FCertP^.Certs,4);


            // Root CERT
            sCertFile:=odRootAuthorityCert.FileName;
            sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
            Process_DER_CERT();
            FCertP^.Certs[1]:=Core.Utils.Files.toString(sCertFile);
            Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[1]);

            // Server CERT
            sCertFile:=odServerAuthorityCert.FileName;
            sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,2,NO_QUOTES);
            Process_DER_CERT();
            FCertP^.Certs[2]:=Core.Utils.Files.toString(sCertFile);
            Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[2]);

            // Intermediate CERT
            sCertFile:=odIntermediateAuthorityCert.FileName;
            sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,3,NO_QUOTES);
            Process_DER_CERT();
            FCertP^.Certs[3]:=Core.Utils.Files.toString(sCertFile);
            Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[3]);

            // Certificate
            sCertFile:=odCert.FileName;
            sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,4,NO_QUOTES);
            Process_DER_CERT();
            FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
            Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);

            if Length(FCertP^.DerCerts[0])>0 then begin
              Try
                iDerCertLen:=Core.Arrays.Bytes.Copy(FCertP^.DerCerts[0],bData);
                X509:=Encryption.SSL.d2i_X509(nil,@bData,iDerCertLen);
                if X509<>nil then begin
                  Try
                    FCertP^.Date:=Encryption.SSL.ToDateTime(X509^.cert_info^.validity^.notAfter);
                  finally
                    Encryption.SSL.X509_free(X509);
                  end;
                end;
              Finally
                Core.Arrays.Bytes.Done(bData);
              end;
              Storage.Certs.Items.DB.Write(Storage.Main.Task,FDomainP^.ID,FCertP^);
              LoadSSLInfo();
            end else begin
             sException:=Core.Streams.toMemo(procSelfSign.Output);
             lblDSCStatus.Caption:='Error loading certificate.';
             Form.Exception.ShowException('frmConsole.pas','Loading error',sException);
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TConsoleForm.miCOACLGrantAllClick(Sender: TObject);
var
  Item:TListItem;
  iLcv:integer;
begin
  if GUI.Locked then exit;
  if puCOACL.PopupComponent=lvUserACLCoreObjects then begin
    for iLcv:=0 to lvUserACLCoreObjects.Items.Count-1 do begin
      Item:=lvUserACLCoreObjects.Items[iLcv];
      FCoreObject:=TCoreObject(Item.Data);
      FCoreObject.Header.Enabled:=True;
      Item.Checked:=true;
      Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
      Grant(FCoreObject.Header,FUserP)
    end;
  end else begin
    for iLcv:=0 to lvUserACLCoreCommands.Items.Count-1 do begin
      Item:=lvUserACLCoreCommands.Items[iLcv];
      FCoreCommand:=Item.Data;
      FCoreCommand^.Enabled:=True;
      Item.Checked:=true;
      Item.Caption:=Yes_No[FCoreCommand^.Enabled];
      Grant(FCoreCommand^,FUserP)
    end;
  end;
  Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);

end;

procedure TConsoleForm.miCODenyCheckedClick(Sender: TObject);
var
  Item:TListItem;
  iLcv:integer;
begin
  if GUI.Locked then exit;
  if puCOACL.PopupComponent=lvUserACLCoreObjects then begin
    for iLcv:=0 to lvUserACLCoreObjects.Items.Count-1 do begin
      Item:=lvUserACLCoreObjects.Items[iLcv];
      if Item.Checked then begin
        FCoreObject:=TCoreObject(Item.Data);
        FCoreObject.Header.Enabled:=False;
        Item.Checked:=false;
        Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
        Deny(FCoreObject.Header,FUserP)
      end;
    end;
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end else begin
    for iLcv:=0 to lvUserACLCoreCommands.Items.Count-1 do begin
      Item:=lvUserACLCoreCommands.Items[iLcv];
      if Item.Checked then begin
        FCoreCommand:=Item.Data;
        FCoreCommand^.Enabled:=False;
        Item.Checked:=false;
        Item.Caption:=Yes_No[FCoreCommand^.Enabled];
        Deny(FCoreCommand^,FUserP)
      end;
    end;
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end;
end;

procedure TConsoleForm.miCODenySelectedClick(Sender: TObject);
var
  Item:TListItem;
begin
  if GUI.Locked then exit;
  if puCOACL.PopupComponent=lvUserACLCoreObjects then begin
    Item:=lvUserACLCoreObjects.Selected;
    if Item=Nil then exit;
    FCoreObject:=TCoreObject(Item.Data);
    FCoreObject.Header.Enabled:=False;
    Item.Checked:=false;
    Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
    Deny(FCoreObject.Header,FUserP);
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end else begin
    Item:=lvUserACLCoreCommands.Selected;
    if Item=nil then exit;
    FCoreCommand:=Item.Data;
    FCoreCommand^.Enabled:=False;
    Item.Checked:=false;
    Item.Caption:=Yes_No[FCoreCommand^.Enabled];
    Deny(FCoreCommand^,FUserP);
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end;
end;

procedure TConsoleForm.miClusterNodeEditClick(Sender: TObject);
begin
  if (lvClusterNodes.Selected<>nil) then begin
    FClusterNodeP:=lvClusterNodes.Selected.Data;
    {$i frmConsole.Clusters.LayoutManager.Set.NodeEditor.inc}
  end;
end;

procedure TConsoleForm.miClusterResourceEdit1Click(Sender: TObject);
begin
  FClusterResourceP:=tvClusterResources.Selected.Data;
  {$i frmConsole.Clusters.LayoutManager.SetClusterEditor.inc}
end;

procedure TConsoleForm.miClusterRNDelete1Click(Sender: TObject);
begin
  if tvClusterResources.Selected<>nil then begin
    FClusterResourceP:=tvClusterResources.Selected.Data;
    Storage.MatrixResources.Resource.DB.Delete(Storage.Main.Task,FClusterResourceP^);
    Storage.MatrixResources.Resource.Remove(FClusterResourceP,FClusterResources);
    tvClusterResources.Selected.Delete;
  end;
end;

procedure TConsoleForm.miClusterRNDeleteClick(Sender: TObject);
begin
  if (lvClusterNodes.Selected<>nil) then begin
    FClusterNodeP:=lvClusterNodes.Selected.Data;
    Storage.MatrixNodes.Node.DB.Delete(Storage.Main.Task,FClusterNodeP^);
    Storage.MatrixNodes.Node.Remove(FClusterNodes,FClusterNodeP);
    lvClusterNodes.Selected.Delete;
  end;
end;

procedure TConsoleForm.miCOACLGrantCheckedClick(Sender: TObject);
var
  Item:TListItem;
  iLcv:integer;
begin
  if GUI.Locked then exit;
  if puCOACL.PopupComponent=lvUserACLCoreObjects then begin
    for iLcv:=0 to lvUserACLCoreObjects.Items.Count-1 do begin
      Item:=lvUserACLCoreObjects.Items[iLcv];
      if Item.Checked then begin
        FCoreObject:=TCoreObject(Item.Data);
        FCoreObject.Header.Enabled:=True;
        Item.Checked:=true;
        Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
        Grant(FCoreObject.Header,FUserP)
      end;
    end;
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end else begin
    for iLcv:=0 to lvUserACLCoreCommands.Items.Count-1 do begin
      Item:=lvUserACLCoreCommands.Items[iLcv];
      if Item.Checked then begin
        FCoreCommand:=Item.Data;
        FCoreCommand^.Enabled:=True;
        Item.Checked:=true;
        Item.Caption:=Yes_No[FCoreCommand^.Enabled];
        Grant(FCoreCommand^,FUserP)
      end;
    end;
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end;
end;


procedure TConsoleForm.miCOACLGrantSelectedClick(Sender: TObject);
var
  Item:TListItem;
begin
  if GUI.Locked then exit;
  if puCOACL.PopupComponent=lvUserACLCoreObjects then begin
    Item:=lvUserACLCoreObjects.Selected;
    if Item=Nil then exit;
    FCoreObject:=TCoreObject(Item.Data);
    FCoreObject.Header.Enabled:=True;
    Item.Checked:=true;
    Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
    Grant(FCoreObject.Header,FUserP);
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end else begin
    Item:=lvUserACLCoreCommands.Selected;
    if Item=nil then exit;
    FCoreCommand:=Item.Data;
    FCoreCommand^.Enabled:=True;
    Item.Checked:=true;
    Item.Caption:=Yes_No[FCoreCommand^.Enabled];
    Grant(FCoreCommand^,FUserP);
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end;
end;

procedure TConsoleForm.miDenyAllClick(Sender: TObject);
var
  Item:TListItem;
  iLcv:integer;
begin
  if GUI.Locked then exit;
  if puCOACL.PopupComponent=lvUserACLCoreObjects then begin
    for iLcv:=0 to lvUserACLCoreObjects.Items.Count-1 do begin
      Item:=lvUserACLCoreObjects.Items[iLcv];
      FCoreObject:=TCoreObject(Item.Data);
      FCoreObject.Header.Enabled:=False;
      Item.Checked:=false;
      Item.Caption:=Yes_No[FCoreObject.Header.Enabled];
      Deny(FCoreObject.Header,FUserP)
    end;
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end else begin
    for iLcv:=0 to lvUserACLCoreCommands.Items.Count-1 do begin
      Item:=lvUserACLCoreCommands.Items[iLcv];
      FCoreCommand:=Item.Data;
      FCoreCommand^.Enabled:=False;
      Item.Checked:=false;
      Item.Caption:=Yes_No[FCoreCommand^.Enabled];
      Deny(FCoreCommand^,FUserP)
    end;
    Storage.UserAccounts.Items.DB.UpdateACLs(Storage.Main.Task,FUserP^);
  end;
end;

procedure TConsoleForm.miCRNNewNodeClick(Sender: TObject);
begin
  if (tvClusterResources.Selected=nil) or (tvClusterResources.Selected.Data=nil)  then exit;
  Empty(FClusterNode_IP);
  SetLength(FClusterNode_IP,4);
  FClusterNodeP:=Nil;
  {$i frmConsole.Clusters.LayoutManager.Hide.ResourceEditor.inc}
  txtClusterNodeName.Clear;
  txtCstrIP0.Clear;
  txtCstrIP1.Clear;
  txtCstrIP2.Clear;
  txtCstrIP3.Clear;
  lvClusterNodes.Selected:=nil;
  gbCRNode.visible:=true;
end;

procedure TConsoleForm.miCRNNewResourceClick(Sender: TObject);
begin
  {$i frmConsole.Clusters.LayoutManager.Hide.NodeEditor.inc}
  FClusterResourceP:=Nil;
  txtCRNResource.Clear;
  gbCRNResource.visible:=true;
end;

procedure TConsoleForm.miLoadCertClick(Sender: TObject);
var
  sCertFile:Core.Strings.VarString;
  sDerCertFile:Core.Strings.VarString;
  sException:Core.Strings.VarString;

  iDerCertLen:integer;
  X509:PX509;
  bData:Core.Arrays.Types.Bytes;

  {$i frmConsole.Process_DER_CERT.inc}
begin
  if (FCertP<>nil) and odCert.Execute then begin
    FCertP^.Level:=1;
    Empty(FCertP^.Certs);
    Empty(FCertP^.DerCerts);
    SetLength(FCertP^.Certs,1);
    SetLength(FCertP^.DerCerts,1);

    sCertFile:=odCert.FileName;
    sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
    Process_DER_CERT();
    FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
    Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);

    if Core.Utils.Files.FileSize(sCertFile)>0 then begin
      FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
      Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);
      Core.Arrays.Bytes.fromFile(sDerCertFile,bData);
      Try
        iDerCertLen:=Length(bData);
        X509:=Encryption.SSL.d2i_X509(nil,@bData,iDerCertLen);
        if X509<>nil then begin
          Try
            FCertP^.Date:=Encryption.SSL.toDateTime(X509^.cert_info^.validity^.notAfter);
          finally
            Encryption.SSL.X509_free(X509);
          end;
        end;
        Storage.Certs.Items.DB.Write(Storage.Main.Task,FDomainP^.ID,FCertP^);
        LoadSSLInfo();
      finally
        Core.Arrays.Bytes.Done(bData);
      end;
    end else begin
     sException:=Core.Streams.toMemo(procSelfSign.Output);
     lblDSCStatus.Caption:='Error loading certificate.';
     Form.Exception.ShowException('frmConsole.pas','Loading error',sException);
    end;
  end;
end;

procedure TConsoleForm.miLoadCertIntClick(Sender: TObject);
var
  sDerCertFile:Core.Strings.VarString;
  sCertFile:Core.Strings.VarString;
  iDerCertLen:integer;
  X509:PX509;
  bData:Core.Arrays.Types.Bytes;


  sException:Core.Strings.VarString;

  {$i frmConsole.Process_DER_CERT.inc}

begin
  if (FCertP<>nil) then begin
    if odIntermediateAuthorityCert.Execute() then begin
      if odCert.Execute() then begin
        FCertP^.Level:=2;
        Empty(FCertP^.Certs);
        Empty(FCertP^.DerCerts);
        SetLength(FCertP^.Certs,2);
        SetLength(FCertP^.DerCerts,2);

        sCertFile:=odCert.FileName;
        sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
        Process_DER_CERT();
        FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
        Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);

        sCertFile:=odIntermediateAuthorityCert.FileName;
        sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,2,NO_QUOTES);
        Process_DER_CERT();
        FCertP^.Certs[1]:=Core.Utils.Files.toString(sCertFile);
        Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[1]);


        if Length(FCertP^.DerCerts[0])>0 then begin
          iDerCertLen:=Core.Arrays.Bytes.Copy(FCertP^.DerCerts[0],bData);
          Try
            X509:=Encryption.SSL.d2i_X509(nil,@bData,iDerCertLen);
            if X509<>nil then begin
              Try
                FCertP^.Date:=Encryption.SSL.ToDateTime(X509^.cert_info^.validity^.notAfter);
              finally
                Encryption.SSL.X509_free(X509);
              end;
            end;
          Finally
            Core.Arrays.Bytes.Done(bData);
          end;
          Storage.Certs.Items.DB.Write(Storage.Main.Task,FDomainP^.ID,FCertP^);
          LoadSSLInfo();
        end else begin
         sException:=Core.Streams.toMemo(procSelfSign.Output);
         lblDSCStatus.Caption:='Error loading certificate.';
         Form.Exception.ShowException('frmConsole.pas','Loading error',sException);
        end;
      end;
    end;
  end;
end;

procedure TConsoleForm.miLoadCertIntRootClick(Sender: TObject);
var
  sCertFile:Core.Strings.VarString;
  sDerCertFile:Core.Strings.VarString;

  iDerCertLen:integer;
  X509:PX509;
  bData:Core.Arrays.Types.Bytes;

  sException:Core.Strings.VarString;
  {$i frmConsole.Process_DER_CERT.inc}

begin
  if (FCertP<>nil) then begin
    if odRootAuthorityCert.Execute() then begin
      if odIntermediateAuthorityCert.Execute() then begin
        if odCert.Execute() then begin

          FCertP^.Level:=3;
          Empty(FCertP^.Certs);
          Empty(FCertP^.DerCerts);
          SetLength(FCertP^.Certs,3);
          SetLength(FCertP^.DerCerts,3);

          sCertFile:=odCert.FileName;
          sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,1,NO_QUOTES);
          Process_DER_CERT();
          FCertP^.Certs[0]:=Core.Utils.Files.toString(sCertFile);
          Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[0]);

          sCertFile:=odIntermediateAuthorityCert.FileName;
          sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,2,NO_QUOTES);
          Process_DER_CERT();
          FCertP^.Certs[1]:=Core.Utils.Files.toString(sCertFile);
          Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[1]);

          sCertFile:=odIntermediateAuthorityCert.FileName;
          sDerCertFile:=Storage.Certs.GetCertFile(App.Folders.UserSSL(),FDomainP^.Name,3,NO_QUOTES);
          Process_DER_CERT();
          FCertP^.Certs[2]:=Core.Utils.Files.toString(sCertFile);
          Core.Arrays.Bytes.fromFile(sDerCertFile,FCertP^.DerCerts[2]);

          if Length(FCertP^.DerCerts[0])>0 then begin
            iDerCertLen:=Copy(FCertP^.DerCerts[0],bData);
            Try
              X509:=Encryption.SSL.d2i_X509(nil,@bData,iDerCertLen);
              if X509<>nil then begin
                Try
                  FCertP^.Date:=Encryption.SSL.ToDateTime(X509^.cert_info^.validity^.notAfter);
                finally
                  Encryption.SSL.X509_free(X509);
                end;
              end;
            Finally
              Core.Arrays.Bytes.Done(bData);
            end;
            Storage.Certs.Items.DB.Write(Storage.Main.Task,FDomainP^.ID,FCertP^);
            LoadSSLInfo();
          end else begin
           sException:=Core.Streams.toMemo(procSelfSign.Output);
           lblDSCStatus.Caption:='Error loading certificate.';
           Form.Exception.ShowException('frmConsole.pas','Loading error',sException);
          end;
        end;
      end;
    end;
  end;
end;

procedure TConsoleForm.miSecCNDALClick(Sender: TObject);
var
  iLcv:integer;
  itmP:Storage.Security.Filter.PItem;
begin
   for iLcv:=0 to lvSecCNDomain.Items.Count-1 do begin
     if (lvSecCNDomain.Items[iLcv].Selected=true) then begin
       itmP:=lvSecCNDomain.Items[iLcv].Data;
       Storage.Security.Filter.Empty(Security_Item);
       Security_Item.Value:=itmP^.Value;
       Security_Item.Enabled:=true;
       Storage.Security.Filter.DB.Identify(Storage.Main.Task,secAcceptableList,Security_Item);
       Security_Item.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_ACCEPTABLE_SERVER_TTL);
       Storage.Security.Filter.DB.Edit(Storage.Main.Task,secAcceptableList,Security_Item);
     end;
   end;
end;

procedure TConsoleForm.miSecCNDBLClick(Sender: TObject);
var
  iLcv:integer;
  itmP:Storage.Security.Filter.PItem;
begin
   for iLcv:=0 to lvSecCNDomain.Items.Count-1 do begin
     if (lvSecCNDomain.Items[iLcv].Selected=true) then begin
       itmP:=lvSecCNDomain.Items[iLcv].Data;
       Storage.Security.Filter.Empty(Security_Item);
       Security_Item.Value:=itmP^.Value;
       Security_Item.Enabled:=true;
       Storage.Security.Filter.DB.Identify(Storage.Main.Task,secBlackList,Security_Item);
       Security_Item.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_BLACKLIST_SERVER_TTL);
       Storage.Security.Filter.DB.Edit(Storage.Main.Task,secBlackList,Security_Item);
     end;
   end;
end;

procedure TConsoleForm.miSecCNDDeleteClick(Sender: TObject);
var
  ItmP:Storage.Security.Filter.PItem;
begin
  if lvSecCNDomain.Selected<>nil then begin
    ItmP:=lvSecCNDomain.Selected.Data;
    Storage.Security.Filter.DB.Delete(Storage.Main.Task,ItmP^);
    Storage.Security.Filter.Cleanup(Security_Connections);
    lvSecCNDomain.Selected.Delete();
  end;
end;

procedure TConsoleForm.miSecCNDWLClick(Sender: TObject);
var
  iLcv:integer;
  itmP:Storage.Security.Filter.PItem;
begin
   for iLcv:=0 to lvSecCNDomain.Items.Count-1 do begin
     if (lvSecCNDomain.Items[iLcv].Selected=true) then begin
       itmP:=lvSecCNDomain.Items[iLcv].Data;
       Storage.Security.Filter.Empty(Security_Item);
       Security_Item.Value:=itmP^.Value;
       Security_Item.Enabled:=true;
       Storage.Security.Filter.DB.Identify(Storage.Main.Task,secWhiteList,Security_Item);
       Security_Item.Expires:=DateUtils.IncYear(Core.Timer.dtUT,SEC_WHITELIST_SERVER_TTL);
       Storage.Security.Filter.DB.Edit(Storage.Main.Task,secWhiteList,Security_Item);
     end;
   end;
end;

procedure TConsoleForm.pcDomainChange(Sender: TObject);
begin
  if pcDomain.ActivePage=tsDomainUsers then begin
    gbDomain_Users_New.Show();
    gbDomain_Users_New.Hide();
  end else if pcDomain.ActivePage=tsDomainScale then begin
    gbDomainServiceSettings.Show();
    gbDomainServiceSettings.Hide();
  end else if pcDomain.ActivePage=tsKeywords then begin
    gbKeywordEditor.Show();
    gbKeywordEditor.Hide();
  end else if pcDomain.ActivePage=tsDomainSecurity then begin
    LoadSSLInfo;
  end;
end;

procedure TConsoleForm.puClusterNewRNPopup(Sender: TObject);
begin
  miClusterRNNewNode.Enabled:=(tvClusterResources.Selected<>nil) and (tvClusterResources.Selected.Data<>nil);
  miClusterRNDelete.Enabled:=(tvClusterResources.Selected<>nil) and (tvClusterResources.Selected.Data<>nil);
end;

procedure TConsoleForm.sePurchasePriceChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  if FPurchaseP<>nil then begin
    FPurchaseP^.Price:=sePurchasePrice.Value;
    lvPurchase.Selected.SubItems[3]:=Purchase.Item.getPrice(FPurchaseP^);
    Storage.Commerce.Purchase.Item.DB.Write(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  end;
end;

procedure TConsoleForm.sgDomainLicenseResize(Sender: TObject);
begin
  sgDomainLicense.Columns[0].Width:=100;
  sgDomainLicense.Columns[1].Width:=sgDomainLicense.ClientWidth-102;
end;

procedure TConsoleForm.sgDomainLicenseSelectEditor(Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
var
  rcEditor:TRect;
begin
  Editor:=txtDomainLicenseEdit;
  rcEditor:=sgDomainLicense.CellRect(aCol,aRow);
  {$ifdef Unix}
    Dec(rcEditor.Bottom);
  {$endif}
  txtDomainLicenseEdit.BoundsRect:=rcEditor;
  txtDomainLicenseEdit.Text:=sgDomainLicense.Cells[sgDomainLicense.Col,sgDomainLicense.Row];
end;

procedure TConsoleForm.tbbClusterNodeDeleteClick(Sender: TObject);
begin
  if tvClusterResources.Focused then
    miClusterRNDeleteClick(Sender)
  else
    miClusterRNDeleteClick(Sender);
end;

procedure TConsoleForm.tbbPZBackupClick(Sender: TObject);
var
  fsXML:TFileStream;
  msXML:TMemoryStream;
begin
  if odSPFile.Execute() then begin
    fsXML:=TFileStream.Create(odSPFile.FileName,fmCreate);
    Try
      msXML:=TMemoryStream.Create();
      Try
        Storage.Commerce.Purchase.Item.toXML(FPurchases,msXML,XML_HEADER_ON);
        Core.Streams.Copy(msXML,fsXML);
      finally
        FreeAndNil(msXML);
      end;
    finally
      FreeAndNil(fsXML);
    end;
  end;
end;

procedure TConsoleForm.tbbKWBackupClick(Sender: TObject);
var
  zip:TZipper;
  iLcv:integer;
  kwP:PKeyword;
  Streams:Array of TStringStream;
begin
  if odSKWFile.Execute then begin
    zip:=TZipper.Create();
    Try
      zip.FileName:=odSKWFile.FileName;
      SetLength(Streams,lvKeywords.Items.Count);
      for iLcv:=0 to lvKeywords.Items.Count-1 do begin
        kwP:=lvKeywords.Items[iLcv].Data;
        Streams[iLcv]:=TStringStream.Create(kwP^.Value);
        zip.Entries.AddFileEntry(Streams[iLcv],kwp^.Name);
      end;
      zip.SaveToFile(odSKWFile.FileName);
      For iLcv:=0 to High(Streams) do
        Streams[iLcv].Free;
      SetLength(Streams,0);
    finally
      zip.Free;
    end;
  end;
end;

procedure TConsoleForm.tbbKWRestoreClick(Sender: TObject);
var
  zip:TUnZipper;
  iLcv:integer;
  iItemLcv:integer;
  liItem:TListItem;
  kwP:PKeyword;
  kwLcvP:PKeyword;
  sContent:Core.Strings.VarString;
  msContent:TMemoryStream;
begin
  if odOKWFile.Execute then begin
    zip:=TUnZipper.Create();
    Try
      zip.FileName:=odOKWFile.FileName;
      zip.Examine;
      msContent:=TMemoryStream.Create();
      Try
        for iLcv:=0 to zip.Entries.Count-1 do begin
          kwP:=nil;
          zip.Entries[iLcv].Stream:=msContent;
          zip.UnZip(zip.Entries[iLcv]);
          sContent:=Core.Streams.toString(msContent);
          zip.Entries[iLcv].Stream:=nil;
          for iItemLcv:=0 to lvKeywords.Items.Count-1 do begin
            kwLcvP:=lvKeywords.Items[iItemLcv].Data;
            if kwLcvP^.Name=zip.Entries[iLcv].ArchiveFileName then begin
              kwP:=kwLcvP;
              lvKeywords.Items[iItemLcv].SubItems[0]:=sContent;
              break;
            end;
          end;
          if kwP=nil then begin
            kwP:=Core.Keywords.Add(zip.Entries[iLcv].ArchiveFileName,sContent,FKeywords,KW_REFRESH_ON,NO_CALLBACK,NO_CALLBACK);
            Storage.Keywords.Items.DB.Create(Storage.Main.Task,FDomainP^.ID,kwP^);
            liItem:=lvKeywords.Items.Add;
            liItem.Caption:=kwP^.Name;
            liItem.SubItems.Add(kwP^.Value);
            liItem.Data:=kwP;
          end else begin
            kwP^.Value:=sContent;
            Storage.Keywords.Items.DB.Save(Storage.Main.Task,kwP^);
          end;
        end;
      finally
        msContent.Free;
      end;
    finally
      zip.Free;
    end;
  end;
end;

procedure TConsoleForm.tbbManage1Click(Sender: TObject);
var
  frmFileMan:TFileManager;
begin
  if lvDomains.Selected=nil then exit;
  if Core.Utils.Forms.List.Show(lvDomains.Selected.Data)=false then begin
    frmFileMan:=TFileManager.Create(nil);
    frmFileMan.Show(Storage.Domains.Items.PDomain(lvDomains.Selected.Data),@Root);
    lvDomains.Cursor:=crArrow;
  end;
end;

procedure TConsoleForm.tbbPurchaseDeleteClick(Sender: TObject);
begin
  If lvPurchase.Selected<>nil then begin
    FPurchaseP:=lvPurchase.Selected.Data;
    Storage.Commerce.Purchase.Item.DB.Delete(Storage.Main.Task,FDomainP^.ID,FPurchaseP^.ID);
    FPurchaseP^.Valid:=false;
    Purchase.Item.Purge(FPurchases);
    lvPurchase.Selected.Delete();
  end;
  FPurchaseP:=nil;
  LoadPurchase();
end;

procedure TConsoleForm.tbbPurchaseEditClick(Sender: TObject);
begin
  If lvPurchase.Selected<>nil then begin
    FPurchaseP:=lvPurchase.Selected.Data;
    LoadPurchase();
  end;
end;

procedure TConsoleForm.tbbPurchaseNewClick(Sender: TObject);
var
  li:TListItem;
  iIndex:integer;
begin
  iIndex:=System.Length(FPurchases);
  SetLength(FPurchases,iIndex+1);
  New(FPurchaseP);
  Purchase.Item.Init(FPurchaseP^);
  FPurchases[iIndex]:=FPurchaseP;
  Storage.Commerce.Purchase.Item.DB.Add(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  LoadPurchase();
  li:=lvPurchase.Items.Add();
  li.Data:=FPurchaseP;
  li.Caption:=IntToStr(FPurchaseP^.ID);
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.SubItems.Add('');
  li.Selected:=true;
end;

procedure TConsoleForm.tbbPZRestoreClick(Sender: TObject);
var
  fsXML   : TFileStream;
  xDoc    : TXMLDocument;
  xSource : TXMLInputSource;
  xParser : TDOMParser;
  Backup  : Storage.Commerce.Purchase.Item.TItems;
  iLcv    : LongInt;
  idx     : LongInt;
  itmP    : Storage.Commerce.Purchase.Item.PItem;
  bReload : boolean;
begin
  if odOPZFile.Execute() then begin
    bReload:=false;
    fsXML:=TFileStream.Create(odOPZFile.FileName,fmOpenRead);
    try
      fsXML.Position:=0;
      xParser:=TDOMParser.Create();
      Try
        xParser.Options.Validate:=False;
        xSource:=TXMLInputSource.Create(fsXML);
        Try
          xParser.Parse(xSource,xDoc);
          try
            Storage.Commerce.Purchase.Item.Init(Backup);
            Storage.Commerce.Purchase.Item.fromXML(xDoc,Backup);
            Try
              for iLcv:=0 to High(Backup) do begin
                itmP:=Backup[iLcv];
                idx:=Storage.Commerce.Purchase.Item.IndexOf(itmP^.ID,FPurchases);
                if (idx=-1) then
                  idx:=Storage.Commerce.Purchase.Item.IndexOf(itmP^.Title,FPurchases);
                if idx=-1 then begin
                  Storage.Commerce.Purchase.Item.DB.Add(Storage.Main.Task,FDomainP^.ID,itmP^);
                  bReload:=true;
                end;
              end;
              if (bReload=true) then
                LoadPurchases();
            Finally
              Storage.Commerce.Purchase.Item.Done(Backup);
            end;
          finally
            FreeAndNil(xDoc);
          end;
        finally
          FreeAndNil(xSource);
        end;
      finally
        FreeAndNil(xParser);
      end;
    finally
      FreeAndNil(fsXML);
    end;
  end;
end;

procedure TConsoleForm.tbbViewerClick(Sender: TObject);
begin
  if GUI.Locked or (lvKeywords.Selected=nil) then exit;
  FKeywordP:=lvKeywords.Selected.Data;
  if (FKeywordP<>nil) then begin
    if (FKeywordP^.Editor=nil) then
      FKeywordP^.Editor:=TKeywordForm.Create(nil);
    TKeywordForm(FKeywordP^.Editor).Show(FKeywordP);
  end;
end;

procedure TConsoleForm.tbClusterNodeScaleClick(Sender: TObject);
var
  iDX:integer;
  msP:Storage.MatrixServices.Items.PItem;
begin
  if GUI.Locked or (lvClusterNodeService.Selected=nil) then exit;
  msP:=lvClusterNodeService.Selected.Data;
  iDX:=cmboDiskNodes.ItemIndex;
  if iDX<>-1 then begin
    msP^.Scale:=tbClusterNodeScale.Position;
    lvClusterNodeService.Selected.SubItems[2]:=IntToStr(msP^.Scale);
    Storage.MatrixServices.Items.DB.SetScale(Storage.Main.Task,msP^);
  end;
end;

procedure TConsoleForm.tbPurchaseEnabledChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  if FPurchaseP<>nil then begin
    FPurchaseP^.Enabled:=tbPurchaseEnabled.Checked;
    lvPurchase.Selected.SubItems[0]:=Yes_No[FPurchaseP^.Enabled];
    Storage.Commerce.Purchase.Item.DB.Write(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  end;
end;

procedure TConsoleForm.tbPurchaseTaxableChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  if FPurchaseP<>nil then begin
    FPurchaseP^.Taxable:=tbPurchaseTaxable.Checked;
    lvPurchase.Selected.SubItems[1]:=Yes_No[FPurchaseP^.Taxable];
    Storage.Commerce.Purchase.Item.DB.Write(Storage.Main.Task,FDomainP^.ID,FPurchaseP^);
  end;
end;

procedure TConsoleForm.tsClustersShow(Sender: TObject);
begin
  lvClusters.Cursor:=crArrow;
end;

procedure TConsoleForm.tvClusterResourcesDblClick(Sender: TObject);
begin
  {$i frmConsole.Clusters.LayoutManager.SetClusterEditor.inc}
end;

procedure TConsoleForm.tvDomainClusteringSelectionChanged(Sender: TObject);
var
  iID:QWord;
  iDomainIndex:integer;
  sDomain:Core.Strings.VarString;
begin
  if GUI.Locked then exit;
  SetLength(sDomain,0);
  // {$i frmConsole.Domain.Node.SelectItem.inc}
  GUI.Lock;
  Try
    if (tvDomainClustering.Selected<>nil) and (tvDomainClustering.Selected.Data<>nil) then begin
      iID:=QWord(tvDomainClustering.Selected.Data);
      Storage.MatrixNodes.Node.DB.Fill(Storage.Main.Task,iID,FDomainNode);
      Storage.MatrixResources.Resource.Db.Fill(Storage.Main.Task,FDomainNode.ResourceID,FDomainResource);
      Storage.MatrixClusters.Cluster.DB.Fill(Storage.Main.Task,FDomainNode.ClusterID,FDomainCluster);
      pnlDCPAllocate.Enabled:=True;
      if FDomainNode.DomainID>0 then begin
        iDomainIndex:=Storage.Domains.Items.IndexOf(Domains,FDomainNode.DomainID);
        cbDCDomains.ItemIndex:=iDomainIndex;
        sDomain:=Domains[iDomainIndex].Name;
        lbDCNINValue.Caption:=FDomainNode.Alias;
        lbDCNIIPValue.Caption:=Core.Utils.Sockets.InAddrToStr(FDomainNode.IP);
        btnDCADeallocate.Enabled:=True;
        btnDCAAllocate.Enabled:=True;
        lbDCNIStatusValue.Caption:=Format(Storage.MatrixNodes.Node.FMT_MN_BOUND_STATUS[True],[sDomain]);
      end else begin
        iDomainIndex:=Storage.Domains.Items.IndexOf(Domains,FDomainP^.ID);
        lbDCNINValue.Caption:=FDomainNode.Alias;
        lbDCNIIPValue.Caption:=Core.Utils.Sockets.InAddrToStr(FDomainNode.IP);
        cbDCDomains.ItemIndex:=iDomainIndex;
        btnDCADeallocate.Enabled:=False;
        btnDCAAllocate.Enabled:=True;
        lbDCNIStatusValue.Caption:=Format(Storage.MatrixNodes.Node.FMT_MN_BOUND_STATUS[False],[Storage.MatrixNodes.Node.FMT_MN_UNBOUND]);
      end;
    end else begin
      lbDCNINValue.Caption:='Not a valid matrix node';
      lbDCNIIPValue.Caption:='Not a valid address';
      cbDCDomains.ItemIndex:=-1;
      pnlDCPAllocate.Enabled:=False;
      btnDCAAllocate.Enabled:=False;
      btnDCADeallocate.Enabled:=False;
      lbDCNIStatusValue.Caption:=Format(Storage.MatrixNodes.Node.FMT_MN_BOUND_STATUS[false],[Storage.MatrixNodes.Node.FMT_MN_UNKNOWN]);
      Storage.MatrixResources.Resource.Empty(FDomainResource);
      Storage.MatrixClusters.Cluster.Empty(FDomainCluster);
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.txtCRNResourceChange(Sender: TObject);
begin
  if GUI.Locked then exit;
  btnCRNResourceOk.Enabled:=Length(txtCRNResource.Text)>0;
end;

procedure TConsoleForm.txtCstrIP0Change(Sender: TObject);
begin
  If GUI.Locked then exit;
  FClusterNode_IP[0]:=txtCstrIP0.Text;
end;

procedure TConsoleForm.txtCstrIP0KeyPress(Sender: TObject; var Key: char);
begin
  if (System.Pos(Key,Core.Strings.Digits)=0) and (Key<>#8)then
    Key:=#0;
end;

procedure TConsoleForm.txtCstrIP1Change(Sender: TObject);
begin
  If GUI.Locked then exit;
  FClusterNode_IP[1]:=txtCstrIP1.Text;
end;

procedure TConsoleForm.txtCstrIP2Change(Sender: TObject);
begin
  If GUI.Locked then exit;
  FClusterNode_IP[2]:=txtCstrIP2.Text;
end;

procedure TConsoleForm.txtCstrIP3Change(Sender: TObject);
begin
  If GUI.Locked then exit;
  FClusterNode_IP[3]:=txtCstrIP3.Text;
end;

procedure TConsoleForm.txtDNSIP1Change(Sender: TObject);
begin
  If GUI.Locked then exit;
  FDNS_HOST_IP[0]:=IntToStr(StrToIntDef(txtDNSIP1.Text,0));
  TI_DNS_Change.Expires:=IncSecond(Core.Timer.dtNow,2);
end;

procedure TConsoleForm.txtDNSIP2Change(Sender: TObject);
begin
  if GUI.Locked then exit;
  FDNS_HOST_IP[1]:=IntToStr(StrToIntDef(txtDNSIP2.Text,0));
  TI_DNS_Change.Expires:=IncSecond(Core.Timer.dtNow,2);
end;

procedure TConsoleForm.txtDNSIP3Change(Sender: TObject);
begin
  if GUI.Locked then exit;
  FDNS_HOST_IP[2]:=IntToStr(StrToIntDef(txtDNSIP3.Text,0));
  TI_DNS_Change.Expires:=IncSecond(Core.Timer.dtNow,2);
end;

procedure TConsoleForm.txtDNSIP4Change(Sender: TObject);
begin
  if GUI.Locked then exit;
  FDNS_HOST_IP[3]:=IntToStr(StrToIntDef(txtDNSIP4.Text,0));
  TI_DNS_Change.Expires:=IncSecond(Core.Timer.dtNow,2);
end;

procedure TConsoleForm.lvDomainServicesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if GUI.Locked then Exit;
  TI_DomainServices.Expires:=IncMillisecond(Now,250);
end;

procedure TConsoleForm.OnSecContSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  ItemP^.Expires:=0;
  GUI.Lock;
  Try
    if lvContentFilters.Selected<>nil then begin
      itmP:=lvContentFilters.Selected.Data;
      txtSecContent.Enabled:=true;
      btnSecContentSave.Enabled:=true;
      cbSecContentPhrase.Checked:=itmP^.Enabled;
      txtSecContent.Text:=itmP^.Value;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.OnSecProfileSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  ItemP^.Expires:=0;
  GUI.Lock;
  Try
    if lvProfileFilters.Selected<>nil then begin
      itmP:=lvProfileFilters.Selected.Data;
      txtSecProfile.Enabled:=true;
      cbSecContentProfile.Checked:=itmP^.Enabled;
      btnSecProfileContentSave.Enabled:=true;
      txtSecProfile.Text:=itmP^.Value;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.OnSecBLSSelect(ItemP:Core.Timer.PItem);
begin
  GUI.Lock;
  Try
    if (lvDNSBL.Selected<>nil) then begin
      txtSecBlackListDNS.Text:=lvDNSBL.Selected.SubItems[0];
      btnBLDNSItemRemove.Enabled:=true;
    end else begin
      txtSecBlackListDNS.Clear;
      btnBLDNSItemRemove.Enabled:=false;
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.OnSecWLSSelect(ItemP:Core.Timer.PItem);
begin
  GUI.Lock;
  Try
    if (lvDNSWL.Selected<>nil) then begin
      txtSecWhiteListDNS.Text:=lvDNSWL.Selected.SubItems[0];
      btnWLDNSItemRemove.Enabled:=true;
    end else begin
      txtSecWhiteListDNS.Clear;
      btnWLDNSItemRemove.Enabled:=false;
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.OnSecWLDSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnHSWLRemove.Enabled:=(lvHSWL.Selected<>nil);
    if (lvHSWL.Selected<>nil) then begin
      itmP:=lvHSWL.Selected.Data;
      txtHSWLHI.Text:=itmP^.Value;
      cbSecWLDomain.Checked:=itmP^.Enabled;
    end else begin
      txtHSWLHI.Clear();
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.OnSecIPVSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnSecIPVRemove.Enabled:=(lvSecIPViolators.Selected<>nil);
    if (lvSecIPViolators.Selected<>nil) then begin
      itmP:=lvSecIPViolators.Selected.Data;
      txtSecIPViolator.Text:=itmP^.Value;
      cbSecIPViolator.Checked:=itmP^.Enabled;
    end else begin
      txtSecIPViolator.Clear();
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.OnSecTLDSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnHSTLDRemove.Enabled:=(lvHSTLD.Selected<>nil);
    if (lvHSTLD.Selected<>nil) then begin
      itmP:=lvHSTLD.Selected.Data;
      txtHSTLD.Text:=itmP^.Value;
    end else begin
      txtHSTLD.Clear();
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.OnSecBLDSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnHSBLRemove.Enabled:=(lvHSBL.Selected<>nil);
    if (lvHSBL.Selected<>nil) then begin
      itmP:=lvHSBL.Selected.Data;
      txtHSBLHI.Text:=itmP^.Value;
      cbSecBLDomain.Checked:=itmP^.Enabled;
    end else begin
      txtHSBLHI.Clear();
    end;
  finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.OnSecALDSelect(ItemP:Core.Timer.PItem);
var
  itmP:Storage.Security.Filter.PItem;
begin
  GUI.Lock;
  Try
    btnHSALRemove.Enabled:=(lvHSAL.Selected<>nil);
    if (lvHSAL.Selected<>nil) then begin
      itmP:=lvHSAL.Selected.Data;
      txtHSALHI.Text:=itmP^.Value;
      cbSecALDomain.Checked:=itmP^.Enabled;
    end else begin
      txtHSALHI.Clear();
    end;
  finally
    GUI.Unlock;
  end;
end;


procedure TConsoleForm.OnDNS_Change(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  GUI.Lock;
  Try
      UpdateSelectedDNSServer(dnskRegular);
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.OnDNSSelect(ItemP:Core.Timer.PItem);
begin
  ItemP^.Expires:=0;
  GUI.Lock;
  Try
    if lvDNSRegular.Selected <>nil then begin
      txtDNSIP4.Enabled:=true;
      txtDNSIP3.Enabled:=true;
      txtDNSIP2.Enabled:=true;
      txtDNSIP1.Enabled:=true;
      Core.Arrays.VarString.fromString(@FDNS_HOST_IP,lvDNSRegular.Selected.SubItems[0],'.');
      SetLength(FDNS_HOST_IP,4);// Add or truncate
      // Assign Editor
      txtDNSIP1.Text:=FDNS_HOST_IP[0];
      txtDNSIP2.Text:=FDNS_HOST_IP[1];
      txtDNSIP3.Text:=FDNS_HOST_IP[2];
      txtDNSIP4.Text:=FDNS_HOST_IP[3];

      btnDNSItemAdd.Enabled:=true;
      btnDNSItemRemove.Enabled:=true;
    end else begin
      txtDNSIP1.Clear;
      txtDNSIP2.Clear;
      txtDNSIP3.Clear;
      txtDNSIP4.Clear;
      txtDNSIP1.Enabled:=false;
      txtDNSIP2.Enabled:=false;
      txtDNSIP3.Enabled:=false;
      txtDNSIP4.Enabled:=false;
      btnDNSItemAdd.Enabled:=true;
      btnDNSItemRemove.Enabled:=false;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.lvHSWLSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  TI_SecWLDSelect.Expires:=DateUtils.IncMillisecond(Now,250);
end;

procedure TConsoleForm.lvProviderServiceSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  {$i frmConsole.SearchProviders.SelectService.inc}
end;

procedure TConsoleForm.lvSearchProvidersSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  {$i frmConsole.SearchProviders.DisableGUI.inc}
  {$i frmConsole.SearchProviders.SelectItem.inc}
  if (Item<>nil) and Selected then begin
    {$i frmConsole.SearchProviders.EnableGUI.inc};
  end;
end;

procedure TConsoleForm.lvUserACLCoreObjectsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
const
  dtLast:TDateTime=0;
var
  iLcv:integer;
  liCommand:TListItem;
  bGranted:Boolean;
begin
  if Selected=False then exit;
  if (MilliSecondsBetween(Now,dtLast)<100) then exit;
  dtLast:=Now;

  FCoreObject:=TCoreObject(Item.Data);
  {$i frmConsole.Domain.User.CoreObject.ListCommands.inc}
end;

procedure TConsoleForm.lvUsersDblClick(Sender: TObject);
begin
  FUserP:=nil;
  if lvUsers.Selected<>nil then begin
    FUserP:=lvUsers.Selected.Data;
    if FUserP<>nil then begin
      GUI.Lock;
      Try
       {$i frmConsole.Domain.User.Edit.inc}
      finally
        GUI.UnLock;
      end;
    end;
  end;
end;

procedure TConsoleForm.MenuItem17Click(Sender: TObject);
begin
  Storage.Main.Task.Connection.Connected:=false;
end;

procedure TConsoleForm.MI_File_NewClick(Sender: TObject);
begin
  MI_File_New_Node.Enabled:=(FClusterP<>nil);
  MI_File_New_User.Enabled:=(FDomainP<>nil);
end;

procedure TConsoleForm.pcDomainChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  AllowChange:=
  (
    (not ((pcDomain.ActivePage=tsUser) and tsUser.TabVisible)) and
    (not ((pcDomain.ActivePage=tsKeywords) and tsKeywords.TabVisible and gbKeywordEditor.Visible))
  );
end;

procedure TConsoleForm.PU_ClustersPopup(Sender: TObject);
var
  bEnabled:boolean;
begin
  bEnabled:=lvClusters.Selected<>nil;
  MI_PU_Cluster_Edit.Enabled:=bEnabled;
  MI_PU_Cluster_Delete.Enabled:=bEnabled;
end;

procedure TConsoleForm.PU_Domain_UsersPopup(Sender: TObject);
var
  bEnabled:boolean;
begin
  bEnabled:=(lvUsers.Selected<>Nil);
  MI_PU_Domain_User_Edit.Enabled:=bEnabled;
  MI_PU_Domain_User_Unlock.Enabled:=bEnabled;
  MI_PU_Domain_User_Delete.Enabled:=bEnabled;
end;

procedure TConsoleForm.seDomain_Default_QuotaChange(Sender: TObject);
begin
  if (FDomainP=nil) or GUI.Locked then Exit;
  FDomainP^.DefaultOptionQuota:=seDomain_Default_Quota.Value;
  Storage.Domains.Items.DB.Update(Storage.Main.Task,FDomainP^);
end;

procedure TConsoleForm.tbbClustersNewClick(Sender: TObject);
begin
  txtCluster_New_Name.Clear;
  txtCluster_New_Description.Clear;
  gbClusters_ClusterInfo.Visible:=True;
end;

procedure TConsoleForm.tbbClustersNewNodeClick(Sender: TObject);
begin
  if (tvClusterResources.Selected<>nil) and (tvClusterResources.Selected.Data<>nil) then
    miCRNNewNodeClick(Sender)
  else
    miCRNNewResourceClick(Sender);
end;

procedure TConsoleForm.tbbKeywordDeleteClick(Sender: TObject);
var
  liItem:TListItem;
begin
  if GUI.Locked or (lvKeywords.Selected=nil) then exit;
  FKeywordP:=lvKeywords.Selected.Data;
  if (FKeywordP<>nil) then begin
    Storage.Keywords.Items.DB.Delete(Storage.Main.Task,FKeywordP^.ID);
    if Core.Utils.ListView.IndexOf(lvKeywords,FKeywordP,liItem)<>-1 then
      liItem.Delete;
    Core.Keywords.Delete(FKeywordP^.ID,FKeywords);
    FKeywordP:=nil;
    {$i frmConsole.Domain.Keywords.EndEdit.inc}
  end;
end;

procedure TConsoleForm.tbbKeywordEditClick(Sender: TObject);
begin
  if lvKeywords.Selected=nil then exit;
  GUI.Lock;
  Try
    FKeywordP:=lvKeywords.Selected.Data;
    txtKeywordName.Text:=FKeywordP^.Name;
    txtKeywordValue.Text:=FKeywordP^.Value;
    txtKeywordName.Modified:=False;
    txtKeywordValue.Modified:=False;
    gbKeywordEditor.Visible:=True;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.tbbKeywordLoadClick(Sender: TObject);
begin
  if GUI.Locked then exit;
  if (FKeywordP<>nil) then begin
    odFile.Title:=Core.Keywords.FMT_FILE_CAPTION;
    odFile.Filter:=Core.Keywords.FMT_FILE_FILTER;
    odFile.FilterIndex:=Core.Keywords.FMT_FILE_INDEX;
    if odFile.Execute then begin
      FKeywordP^.Value:=Core.Utils.Files.toString(odFile.FileName);
      Storage.Keywords.Items.DB.Save(Storage.Main.Task,FKeywordP^);
    end;

  end;
end;

procedure TConsoleForm.tbbKeywordNewClick(Sender: TObject);
var
  liItem:TListItem;
  ItemP:PKeyword;
begin
  GUI.Lock;
  Try
    ItemP:=Core.Keywords.Add('New Keyword','',FKeywords,KW_REFRESH_ON,NO_CALLBACK,NO_CALLBACK);
    Storage.Keywords.Items.DB.Create(Storage.Main.Task,FDomainP^.ID,ItemP^);
    liItem:=lvKeywords.Items.Insert(0);
    liItem.Caption:=ItemP^.Name;
    liItem.SubItems.Add(ItemP^.Value);
    liItem.Data:=ItemP;
    liItem.MakeVisible(false);

    Application.ProcessMessages;
    liItem.MakeVisible(true);
    lvKeywords.Selected:=liItem;
    liItem.Selected:=true;
    liItem.Focused:=true;
    Application.ProcessMessages;
    FKeywordP:=ItemP;
    txtKeywordName.Text:=ItemP^.Name;
    txtKeywordValue.Text:=ItemP^.Value;
    txtKeywordName.Modified:=False;
    txtKeywordValue.Modified:=False;
    gbKeywordEditor.Visible:=True;
    lvKeywords.Enabled:=False;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.tbSearchScaleChange(Sender: TObject);
begin
  if GUI.Locked or (FSearchProviderP=nil) then exit;
  FSearchProviderP^.Scale:=tbSearchScale.Position;
  Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FSearchProviderP^);
end;

procedure TConsoleForm.tbbClusterNodeEditClick(Sender: TObject);
begin
  GUI.Lock;
  Try
    If (lvClusterNodes.Focused) then begin
      miClusterNodeEditClick(Sender);
    end else if (tvClusterResources.Selected<>nil) then begin
      miClusterResourceEdit1Click(Sender);
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.tbbClustersEditClick(Sender: TObject);
begin
  if lvClusters.Selected=nil then exit;
  FClusterP:=lvClusters.Selected.Data;
  Display_Cluster_Editor(FClusterP);
end;

{procedure TConsoleForm.tbbClustersNewNode(Sender: TObject);
begin
  Empty(FClusterNode_IP);
  SetLength(FClusterNode_IP,4);
  FClusterNodeP:=Nil;
  txtClusterNodeName.Clear;
  txtCstrIP0.Clear;
  txtCstrIP1.Clear;
  txtCstrIP2.Clear;
  txtCstrIP3.Clear;
  gbCRNode.visible:=true;
end;}

procedure TConsoleForm.tbbDomain_Users_FindClick(Sender: TObject);
var
  iLcv:integer;
  li:TListItem;
begin
  lvUsers.Clear;
  FFindUsers.Clear();
  Storage.UserAccounts.Items.DB.Find(Storage.Main.Task,FFindUsers,FDomainP^.ID,Like_Prep(txtUsersFind.Text));
  {$i frmConsole.Users.List.inc}
end;

procedure TConsoleForm.tbbDomain_User_DeleteClick(Sender: TObject);
begin
  if lvUsers.Selected=nil then exit;
  FUserP:=lvUsers.Selected.Data;

  if (FUserP^.ID=Default.ID) or (FUserP^.ID=Root.ID) then exit;
  Storage.UserAccounts.Items.DB.Delete(Storage.Main.Task,FDomainP^.ID,FUserP^.ID);
  FUAP:=FFindUsers.Find(FUserP^.ID);
  lvUsers.Selected.Delete;
  If lvDomains.Selected<>nil then
    lvDomains.Selected.SubItems[LI_IDX_DOMAINS_USERCOUNT]:=IntToStr(Storage.UserAccounts.Items.DB.Count(Storage.Main.Task,FDomainP^.ID));
end;

procedure TConsoleForm.tbbDomain_User_NewClick(Sender: TObject);
begin
  GUI.Lock;
  Try
    {$i frmConsole.Domains.Users.Clear.inc}
    gbDomain_Users_New.Visible:=true;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.tbbDomain_User_UnlockClick(Sender: TObject);
begin
  if (lvUsers.Selected<>nil) then begin
    FUserP:=lvUsers.Selected.Data;
    FUserP^.LockoutCount:=0;
    FUserP^.Enabled:=true;
    lvUsers.Selected.SubItems[4]:='0';
    Storage.UserAccounts.Items.DB.SetLockOutCount(Storage.Main.Task,FUserP^.DomainID,FUserP^.ID,0);
  end;
end;

procedure TConsoleForm.tbDomainScaleChange(Sender: TObject);
begin
  if GUI.Locked then Exit;
  GUI.Lock;
  Try
    {$i frmConsole.Domain.ScaleChange.inc}
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.tvClusterResourcesSelectionChanged(Sender: TObject);
var
  iLcv                           : LongInt;
  liItem                         : TListItem;
begin
  If GUI.Locked or (tvClusterResources.Selected=nil) or (tvClusterResources.Selected.Data=nil) then exit;
  Screen.Cursor:=crHourglass;
  Try
    Application.ProcessMessages;
    FClusterResourceP:=tvClusterResources.Selected.Data;
    GUI.Lock;
    Try
      Core.Utils.ListView.Clear(lvClusterNodes);
      {$i frmConsole.Clusters.LayoutManager.Hide.Editors.inc}

      Storage.MatrixNodes.Node.DB.List(Storage.Main.Task,FClusterP^.ID,FClusterResourceP^.ID,FClusterNodes);
      For iLcv:=0 to High(FClusterNodes) do begin
        liItem:=lvClusterNodes.Items.Add;
        liItem.Caption:=IntToStr(FClusterNodes[iLcv]^.ID);
        liItem.SubItems.Add(IntToStr(FClusterNodes[iLcv]^.ResourceID));
        liItem.SubItems.Add(IntToStr(FClusterNodes[iLcv]^.ClusterID));
        liItem.SubItems.Add(Core.Utils.Sockets.InAddrToStr(FClusterNodes[iLcv]^.IP));
        liItem.SubItems.Add(FClusterNodes[iLcv]^.Alias);
        liItem.SubItems.Add(Yes_No[FClusterNodes[iLcv]^.Disk.Enabled]);
        liItem.SubItems.Add(FClusterNodes[iLcv]^.Disk.Device);
        liItem.Data:=FClusterNodes[iLcv];
      end;
    finally
      GUI.Unlock;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;

end;

procedure TConsoleForm.tvDBMGroupsSelectionChanged(Sender: TObject);
var
  tnSelected:TTreeNode;
  ItemP:Core.Database.Monitor.Types.PItem;
  iLcv:integer;
  liField:TListItem;
begin
  tnSelected:=tvDBMGroups.Selected;
  GUI.Lock;
  Try
    if (tnSelected=nil) or (tnSelected.Data=nil) then begin
      {$i frmConsole.DBM.DisableGUI.inc}
    end else begin
      lvDBMTableFields.Clear;
      gbDBMDBTable.Enabled:=true;
      gbDBMTableDescription.Enabled:=true;
      lvDBMTableFields.Enabled:=true;
      ItemP:=tnSelected.Data;
      txtDBMTable.Text:=ItemP^.TableP^.Name;
      lbDBMTableDescription.Caption:=ItemP^.TableP^.StartupP^.Hint;
      for iLcv:=0 to High(ItemP^.TableP^.Fields) do begin
        liField:=lvDBMTableFields.Items.Add;
        liField.Caption:=ItemP^.TableP^.Fields[iLcv].KeyP^;
        liField.SubItems.Add(YES_NO[ItemP^.TableP^.Fields[iLcv].AutoCreate]);
        liField.SubItems.Add(YES_NO[ItemP^.TableP^.Fields[iLcv].Verified]);
        liField.SubItems.Add(DB_FIELD[ItemP^.TableP^.Fields[iLcv].DataType]);
        liField.SubItems.Add(IntToStr(ItemP^.TableP^.Fields[iLcv].Precision));
        liField.SubItems.Add(IntToStr(ItemP^.TableP^.Fields[iLcv].IDP^));
        liField.SubItems.Add(IntToStr(ItemP^.TableP^.Fields[iLcv].Flags));
      end;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.UpdateSelectedDNSServer(Kind:Byte);
var
  itmP:Storage.DNS.Items.PItem;

  procedure Push_Regular;
  begin
    If lvDNSRegular.Selected<>nil then begin
      itmP:=lvDNSRegular.Selected.Data;
      if itmP<>nil then begin
        lvDNSRegular.Selected.SubItems[0]:=Core.Arrays.VarString.toString(FDNS_HOST_IP,'.');
        itmP^.IP:=Core.Utils.Sockets.InAddrFromStr(lvDNSRegular.Selected.SubItems[0]);
        Storage.DNS.Items.DB.Write(Storage.Main.Task,itmP^);
      end;
    end;
  end;

begin
  Case Kind of
    dnskRegular : Push_Regular;
  end;
end;

procedure TConsoleForm.Display_Cluster_Editor(ClusterP:Storage.MatrixClusters.Cluster.PItem);
var
  iLcv:integer;
  tnCluster:TTreeNode;
  tnItem:TTreeNode;
begin
  GUI.Lock;
  Try
    tvClusterResources.Items.Clear;
    FClusterP:=ClusterP;
    Storage.MatrixNodes.Node.Empty(FClusterNodes);
    Storage.MatrixResources.Resource.Empty(FClusterResources);
    Storage.MatrixResources.Resource.DB.List(Storage.Main.Task,FClusterP^.ID,FClusterResources);

    tnCluster:=tvClusterResources.Items.Add(nil,FClusterP^.Group);
    for iLcv:=0 to High(FClusterResources) do begin
      tnItem:=tvClusterResources.Items.AddChild(tnCluster,FClusterResources[iLcv]^.Name);
      tnItem.Data:=FClusterResources[iLcv];
    end;


    txtCluster_Name.Text:=ClusterP^.Group;
    txtCluster_Town.Text:=ClusterP^.Location.Locality;
    txtCluster_City.Text:=ClusterP^.Location.Area;
    txtCluster_State.Text:=ClusterP^.Location.Region;
    txtCluster_Country.Text:=ClusterP^.Location.Country;
    txtCluster_Street.Text:=ClusterP^.Location.Street;
    txtCluster_Building.Text:=ClusterP^.Location.Building;
    txtCluster_Floor.Text:=ClusterP^.Location.Floor;
    txtCluster_Room.Text:=ClusterP^.Location.Room;
    txtCluster_Zipcode.Text:=ClusterP^.Location.Zip;
    txtCluster_Description.Text:=ClusterP^.Location.Description;

    lvClusterNodes.Clear;
    pcManage.ActivePage:=tsCluster;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.LoadDomainClustering;
var
  iLcv:integer;
  tnClusterAsAllocated:TTreeNode;
  tnClusterAsAvailable:TTreeNode;
  tnClusterAsAll:TTreeNode;
  tnResourceAsAllocated:TTreeNode;
  tnResourceAsAvailable:TTreeNode;
  tnResourceAsAll:TTreeNode;
  tnAllocated:TTreeNode;
  tnAvailable:TTreeNode;
  tnNode:TTreeNode;
  tnAll:TTreeNode;
  kplResources:Core.Arrays.Types.KeyStrings;
  iRLcv:integer;
  iNLcv:integer;
  iRID:QWord;
  iNID:QWord;
begin
  {$i frmConsole.Domain.Clustering.Load.inc}
  {$i frmConsole.Domain.Clustering.ResourceConsumption.inc}
end;

function  TConsoleForm.cbKW_Loopback(ItemP:PKeyword):Core.Strings.VarString;
begin
  Result:=ItemP^.Value;
end;

procedure TConsoleForm.Display_Domain_Editor(DomainP:Storage.Domains.Items.PDomain);
const
 S_Enabled:Array[Boolean] of TCheckBoxState=(cbUnChecked,cbChecked);
var
 iLcv    : LongInt;
 li      : TListItem;

  procedure AddDefaultMatrixService(Kind:Byte; Port:Integer);
  var
    iCount:integer;
  begin
     iCount:=Length(FDomainServices.List);
     New(FDomainServiceP);
     Storage.MatrixServices.Items.Init(FDomainServiceP^);
     SetLength(FDomainServices.List,iCount+1);
     FDomainServices.List[iCount]:=FDomainServiceP;
     // Add Defaults Now
     FDomainServiceP^.ClusterID:=Storage.MatrixServices.Default.Cluster;
     FDomainServiceP^.NodeID:=Storage.MatrixServices.Default.Node;
     FDomainServiceP^.DomainID:=FDomainP^.ID;
     FDomainServiceP^.Kind:=Kind;
     FDomainServiceP^.Port:=Port;
     FDomainServiceP^.Scale:=FDefaultServices[Kind].Scale;
     FDomainServiceP^.Enabled:=FDefaultServices[Kind].Enabled;
     Storage.MatrixServices.Items.DB.Create(Storage.Main.Task,FDomainServiceP^);
  end;
begin
  if FDomainP<>DomainP then begin
    GUI.Lock;
    Try
      FDomainP:=DomainP;
      pcManage.ActivePage:=tsDomain;
      pcDomain.ActivePage:=tsDomainScale;
      Screen.Cursor:=crHourglass;
      Application.ProcessMessages;

      cbDCDomains.Clear;
      for iLcv:=0 to High(Domains) do
        cbDCDomains.Items.Add(Domains[iLcv].Name);

      cbDomain_Host.Text:=DomainP^.Name;
      txtDomain_FriendlyName.Text:=DomainP^.FriendlyName;
      txtDomain_Postmaster.Text:=DomainP^.Root;
      Storage.MatrixServices.Items.Empty(FDomainServices);
      Core.Keywords.Empty(FKeywords);

      if Storage.MatrixServices.Items.DB.Fill(Storage.Main.Task,FDomainServices,FDomainP^.ID,Storage.MatrixServices.Default.Settings) then begin
        for iLcv:=0 to Storage.MatrixServices.Items.MK_MAX do begin
          FDomainServiceP:=Storage.MatrixServices.Items.Find(FDomainServices,FDomainP^.ID,iLcv);
          if (FDomainServiceP=nil) then
            AddDefaultMatrixService(iLcv,Storage.MatrixServices.Items.DefaultPort[iLcv]);
        end;
      end;
      seDomain_Default_Quota.Value:=FDomainP^.DefaultOptionQuota;
      cbDomain_Defaults.State[0]:=S_Enabled[FDomainP^.DefaultOptionFiltering];
      cbDomain_Defaults.State[1]:=S_Enabled[FDomainP^.DefaultOptionCatchAll];


      {$i frmConsole.Domain.Clustering.Clear.inc}

      Storage.UserAccounts.Items.Empty(Root);
      Storage.UserAccounts.Items.Empty(Default);
      Root.DomainID:=FDomainP^.ID;
      Root.User:=FDOmainP^.Root;
      Default.DomainID:=FDomainP^.ID;
      Default.User:=UA_DEFAULT_ACCOUNT;

      Storage.UserAccounts.Items.DB.Fill(Storage.Main.Task,FDomainP^.Root,Root);
      Storage.UserAccounts.Items.DB.Fill(Storage.Main.Task,'default',Default);
      if (Default.ID=0) then begin
        Storage.UserAccounts.Items.DB.CreateDefault(Storage.Main.Task,FDomainP);
        Storage.UserAccounts.Items.DB.Fill(Storage.Main.Task,UA_DEFAULT_ACCOUNT,Default);
      end;
      for iLcv:=0 to High(FDomainServices.List) do begin
        FDomainServiceP:=FDomainServices.List[iLcv];
        li:=lvDomainServices.Items.Add();
        li.Data:=FDomainServiceP;
        li.Caption:=IntToStr(FDomainServiceP^.ID);
        li.SubItems.Add(Yes_No[FDomainServiceP^.Enabled]);
        li.SubItems.Add(IntToStr(FDomainServiceP^.Scale));
        li.SubItems.Add(Storage.MatrixServices.Items.mkLongNames[FDomainServiceP^.Kind]);
        li.SubItems.Add(Storage.MatrixServices.Items.mkDescriptions[FDomainServiceP^.Kind]);
      end;
      tbDomainScale.Position:=0;
      cbDomain_Service.Checked:=false;

      LoadDomainClustering;

      // Now Load Any User Accounts with Like and Find Box
      FFindUsers.Reset(FDomainP);


      {$i frmConsole.Domain.Users.User.Clear.inc}
      if (Length(txtUsersFind.Text)>0) then
        Storage.UserAccounts.Items.DB.Find(Storage.Main.Task,FFindUsers,FDomainP^.ID,Like_Prep(txtUsersFind.Text))
      else if (Storage.UserAccounts.Items.DB.Count(Storage.Main.Task,FDomainP^.ID)<200) then
        Storage.UserAccounts.Items.DB.Find(Storage.Main.Task,FFindUsers,FDomainP^.ID,Like_Prep(''));
      // Populate Search Box
      {$i frmConsole.Users.List.inc}

      LoadKeywords();
      LoadSSLInfo();
      {$i frmConsole.Domain.LoadFolders.inc}

      LoadPurchases();


      // Display GUI
      Screen.Cursor:=crDefault;
      lvDomains.Cursor:=crArrow;
      Application.ProcessMessages;

      pcPages.ActivePage:=tsManage;
      pcManage.ActivePage:=tsDomain;
      pcDomain.ActivePage:=tsDomainUsers;

    finally
      GUI.UnLock;
    end;
  end else begin
    pcPages.ActivePage:=tsManage;
    pcManage.ActivePage:=tsDomain;
    pcDomain.ActivePage:=tsDomainUsers;
  end;
end;

procedure TConsoleForm.LoadDomains;
var
  liItem:TListItem;
  iLcv:integer;
begin
  GUI.Lock;
  Try
    lvDomains.Clear;
    cbDCDomains.Clear;
    cbDCDomains.Items.Clear;
    cbDomain_Host.Clear;
    cbDomain_Host.Items.Clear;
    Storage.Domains.Items.Empty(Domains);
    FDomainP:=nil;
    Storage.Domains.Items.DB.List(Storage.Main.Task,Domains);
    for iLcv:=0 to High(Domains) do begin
      liItem:=lvDomains.Items.Add;
      liItem.Data:=@Domains[iLcv];
      liItem.Caption:=IntToStr(Domains[iLcv].ID);
      liItem.SubItems.Add(Core.Strings.toString(Storage.UserAccounts.Items.DB.Count(Storage.Main.Task,Domains[iLcv].ID)));
      liItem.SubItems.Add(Domains[iLcv].Name);
      cbDCDomains.Items.Add(Domains[iLcv].Name);
      cbDomain_Host.Items.Add(Domains[iLcv].Name);
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.LoadNodes;
begin
  Storage.MatrixNodes.Node.DB.List(Storage.Main.Task,Nodes);
end;

procedure TConsoleForm.LoadCertInfo(var Cert: TCertData);
var
  bKeyPresent:Boolean;
  bReqPresent:Boolean;
  bCertPresent:Boolean;
begin
  btnDSNC.Visible:=true;
  frmCertReq.CertReqForm.SetInfo(App.Folders.UserSSL(),FDomainP^.Name);
  bKeyPresent:=(Length(Cert.Key)>0);
  bReqPresent:=(Length(Cert.Request)>0);
  bCertPresent:=(Cert.Level>0);
  if (bKeyPresent=false) then begin
    lblDSCStatus.Caption:='Key generation needed';
    btnDSSSC.Visible:=false;
    btnDSReq.Visible:=false;
    btnDSLC.Visible:=false;
  end else begin
    if (bReqPresent=true) then begin
      btnDSReq.Visible:=true;
      if (bCertPresent=false) then begin
        lblDSCStatus.Caption:='Certificate needs to be signed';
        btnDSSSC.Visible:=true;
        btnDSLC.Visible:=true;
      end else begin
        //btnDSSSC.Visible:=true;
        btnDSSSC.Visible:=false;
        btnDSLC.Visible:=true;
        if Cert.ID=FDomainP^.CertID then
          lblDSCStatus.Caption:=Concat('Certificate assigned to ',FDomainP^.Name)
        else
          lblDSCStatus.Caption:=Concat('Certificate stored but not assigned to ',FDomainP^.Name);
      end;
    end else begin
      btnDSReq.Visible:=false;
      btnDSSSC.Visible:=false;
      btnDSLC.Visible:=false;
      lblDSCStatus.Caption:='Certificate request needed';
    end;
  end;
end;

procedure TConsoleForm.LoadPurchases();
var
  iLcv:integer;
  li:TListItem;
begin
  cmboPurchaseKind.Items.Delimiter:=',';
  cmboPurchaseKind.Items.DelimitedText:=Storage.Commerce.Purchase.Kind.asString;
  cmboPurchaseKind.ItemIndex:=Storage.Commerce.Purchase.Kind.Item;
  lvPurchase.Items.Clear();
  Storage.Commerce.Purchase.Item.DB.List(Storage.Main.Task,FDomainP^.ID,FPurchases);
  {$i frmConsole.Domain.LoadPurchases.inc}
end;

procedure TConsoleForm.LoadPurchase;
begin
  GUI.Lock();
  Try
    if (FPurchaseP<>nil) then begin
      txtPurchaseTitle.Text:=FPurchaseP^.Title;
      txtPurchaseDescription.Text:=FPurchaseP^.Description;
      sePurchasePrice.Value:=FPurchaseP^.Price;
      cmboPurchaseKind.ItemIndex:=FPurchaseP^.Kind;
      tbPurchaseEnabled.Checked:=FPurchaseP^.Enabled;
      tbPurchaseTaxable.Checked:=FPurchaseP^.Taxable;
    end else begin
      txtPurchaseTitle.Clear();
      txtPurchaseDescription.Clear();
      sePurchasePrice.Value:=0.00;
      cmboPurchaseKind.ItemIndex:=Purchase.Kind.Item;
      tbPurchaseEnabled.Checked:=False;
      tbPurchaseTaxable.Checked:=False;
    end;
  finally
    GUI.Unlock();
  end;
end;

procedure TConsoleForm.LoadSSLInfo;
var
  iLcv:integer;
  li:TListItem;
begin
  lblDomainCert.Caption:=Concat('Server Certificate for ',FDomainP^.Name);

  btnDSLC.Visible:=false;
  btnDSSSC.Visible:=false;
  btnDSReq.Visible:=false;
  btnDSNC.Visible:=true;

  lvCerts.Items.Clear;
  Storage.Certs.Items.DB.List(Storage.Main.Task,FDomainP^.ID,FCerts);
  frmRSA.RSAGen.SetInfo(App.Folders.UserSSL(),FDomainP^.Name);
  FCertP:=Encryption.SSL.getItem(FDomainP^.CertID,FCerts);

  for iLcv:=0 to High(FCerts) do begin
    li:=lvCerts.Items.Add;
    li.Data:=FCerts[iLcv];
    li.Caption:=IntToStr(FCerts[iLcv]^.ID);
    li.SubItems.Add(Storage.Certs.Items.FMT_CERT_STATUS[Storage.Certs.Items.KeyStatus[Length(FCerts[iLcv]^.Key)>0]]);
    li.SubItems.Add(Storage.Certs.Items.FMT_CERT_STATUS[Storage.Certs.Items.RequestStatus[Length(FCerts[iLcv]^.Request)>0]]);
    li.SubItems.Add(Storage.Certs.Items.FMT_CERT_STATUS[Storage.Certs.Items.CertStatus[FCerts[iLcv]^.Level>0]]);
    li.SubItems.Add(IntToStr(FCerts[iLcv]^.Level));
    li.SubItems.Add(Core.Utils.Time.DateToString(FCerts[iLcv]^.Date,BiasMinutes));
    li.Selected:=FCerts[iLcv]=FCertP;
  end;
  if FCertP=nil then begin
    FDomainP^.CertID:=0;
    btnDSSet.ImageIndex:=6;
    btnDSSet.Caption:='Assign';
    lblDSCStatus.Caption:='Please create or assign a certificate';
    imgUnLock.Visible:=true;
    imgLock.Visible:=false;
  end else begin
    imgLock.Visible:=true;
    imgUnLock.Visible:=false;
    btnDSSet.ImageIndex:=24;
    btnDSSet.Caption:='Present';
  end;
end;

procedure TConsoleForm.LoadKeywords;
var
  liItem                         : TListItem;
  iLcv                           : LongInt;
  Request                        : Storage.Keywords.Items.DB.Request;
begin
  GUI.Lock;
  Try
    FKeywordP:=nil;
    Empty(FKeywords);
    if FDomainP<>nil then begin
      lvKeywords.BeginUpdate;
      Try
        lvKeywords.Clear;
        Request.cbDefault:=@cbKW_Loopback;
        Request.DomainID:=FDomainP^.ID;
        Request.KeywordsP:=@FKeywords;
        Storage.Keywords.Items.DB.Fill(Storage.Main.Task,Request);
        for iLcv:=0 to High(FKeywords) do begin
          liItem:=lvKeywords.Items.Add;
          liItem.Caption:=FKeywords[iLcv]^.Name;
          liItem.SubItems.Add(FKeywords[iLcv]^.Value);
          liItem.Data:=FKeywords[iLcv];
        end;
      finally
        lvKeywords.EndUpdate;
      end;
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.LoadSearchProviders;
var
  liItem:TListItem;
  iLcv:integer;
begin
  GUI.Lock;
  Try
    lvSearchProviders.Items.Clear;
    txtProviderName.Clear;
    txtProviderDomain.Clear;
    txtProviderPort.Clear;
    txtProviderMaxResults.Clear;
    txtProviderQueryString.Clear;
    Storage.SrchProviders.Items.Empty(FSearchProviders);
    Storage.SrchProviders.Items.DB.Fill(Storage.Main.Task,FSearchProviders);
    for iLcv:=0 to High(FSearchProviders) do begin
      liItem:=lvSearchProviders.Items.Add;
      liItem.Caption:=FSearchProviders[iLcv].Caption;
      liItem.Data:=Pointer(FSearchProviders[iLcv].ID);
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.LoadContentTypes;
var
  liItem:TListItem;
  iLcv:integer;
begin
  GUI.Lock;
  Try
    lvContentTypes.Clear;
    txtContentTypeExtension.Clear;
    txtContentTypeKind.Clear;
    Empty(Storage.ContentTypes.List);
    Storage.ContentTypes.Items.DB.Load(Storage.Main.Task,Storage.ContentTypes.List);
    for iLcv:=0 to High(Storage.ContentTypes.List) do begin
      liItem:=lvContentTypes.Items.Add;
      liItem.Caption:=Storage.ContentTypes.List[iLcv]^.Ext;
      liItem.SubItems.Add(Storage.ContentTypes.List[iLcv]^.Kind);
      liItem.Data:=Storage.ContentTypes.List[iLcv];
      liItem.Cut:=RSR.HTTP.IndexOf(ctDefaultExtensions,Storage.ContentTypes.List[iLcv]^.Ext)>-1;
    end;
  finally
    GUI.UnLock;
  end;
end;
procedure TConsoleForm.LoadClusterLayoutManager;
begin
  GUI.Lock;
  Try

  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.ClearClusterLayoutManager;
begin
  GUI.Lock;
  Try
    FClusterNodeP:=nil;
    FClusterResourceP:=nil;
    tvClusterResources.Items.Clear;
    lvClusterNodes.Clear;
    txtClusterNodeName.Clear;

    {$i frmConsole.Clusters.LayoutManager.Hide.NodeEditor.inc}
  Finally
    GUI.Unlock;
  end;
end;

procedure TConsoleForm.LoadClusters;
var
  liItem:TListItem;
  iLcv:integer;
begin
  GUI.Lock;
  Try
    lvClusters.Clear;
    cmboDiskClusters.Clear();
    ClearClusterLayoutManager;
    Storage.MatrixClusters.Cluster.Empty(FClusters);
    Storage.MatrixClusters.Cluster.DB.Fill(Storage.Main.Task,FClusters);
    For iLcv:=0 to High(FClusters) do begin
      liItem:=lvClusters.Items.Add;
      liItem.Caption:=FClusters[iLcv]^.Group;
      liItem.SubItems.Add(FClusters[iLcv]^.Location.Description);
      liItem.SubItems.Add(FClusters[iLcv]^.Location.Locality);
      liItem.SubItems.Add(FClusters[iLcv]^.Location.Area);
      liItem.SubItems.Add(FClusters[iLcv]^.Location.Region);
      liItem.SubItems.Add(FClusters[iLcv]^.Location.Country);
      liItem.Data:=FClusters[iLcv];
      cmboDiskClusters.Items.Add(FClusters[iLCv]^.Group);
    end;
  finally
    GUI.UnLock;
  end;
end;

procedure TConsoleForm.ProcessStartup;
var
  sError:Core.Strings.VarString;
begin
  FManagerFat:=TDSFAT.Create();
  FFindUsers:=Storage.UserAccounts.Items.TList.Create(nil,SERVICE_AUTH);
  FCoreObjects:=TCoreObjects.Create(Storage.Main.Header,nil,nil,nil, FDomainP,@Root,@Default,@FKeywords,FManagerFat,FFindUsers);

  lbStatus.Caption:=' Checking Database Tables...';
  Visible:=true;
  Application.ProcessMessages;
  Core.Database.Monitor.CheckTables(Storage.Main.Task);
  lbStatus.Caption:=' Loading System Settings...';
  Application.ProcessMessages;
  LoadSettings;

  if Storage.auDisks then begin
    lbStatus.Caption:=' Loading AuraDisks...';
    Application.ProcessMessages;
    Storage.MatrixNodes.Node.DB.SetupDisks(Storage.Main.Task,FAuraDisks,sError);
    if Length(sError)>0 then begin
      sError:=Concat('AuraDisks failed to start (You probably need sudo). Error: ',sError);
      Core.Logging.Native.WriteLogEntry(SYSTEM_LOG,SERVICE_AUDISK,sError);
      lbStatus.Caption:=sError;
      tvNavigation.Enabled:=false;
      Exit;
    end;
  end;
  lbAuDisks.Caption:=Concat(' Disks ',IntToStr(Length(FAuraDisks)));
  lbStatus.Caption:=' Loading Domains...';
  Application.ProcessMessages;
  LoadDomains;
  lbStatus.Caption:=' Loading Clusters...';
  Application.ProcessMessages;
  LoadClusters;
  lbStatus.Caption:=' Loading Nodes...';
  Application.ProcessMessages;
  LoadNodes;

  lbStatus.Caption:=' Loading Search Providers...';
  Application.ProcessMessages;
  LoadSearchProviders;
  lbStatus.Caption:=' Loading Content Types...';
  Application.ProcessMessages;
  LoadContentTypes;

  lbStatus.Caption:=' Installing core objects...';
  Application.ProcessMessages;
  {$i coList.Injections.inc}
  FCoreObjects.Load;

  if Storage.guiServices then begin
    lbStatus.Caption:=' Loading discovered services...';
    Application.ProcessMessages;
    if (frmServices.ServicesForm=nil) then
      Application.CreateForm(TfrmServices,frmServices.ServicesForm);
    frmServices.ServicesForm.Show;
  end;

  lbStatus.Caption:=' Welcome!';
  Enabled:=true;
end;

procedure TConsoleForm.LoadSettings;
var
  iLcv:integer;
  li:TListItem;
  sError:Core.Strings.VarString;
begin
  GUI.Lock;

  Storage.ConfigData.Items.Init(FProcessGroupName);
  FProcessGroupName.Name:=CFG_NS_OS_GROUP_NAME;
  FProcessGroupName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessGroupName);
  if FProcessGroupName.ID=0 then begin
    FProcessGroupName.Value:=txtProcessGroupName.Text;
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FProcessGroupName);
  end;
  txtProcessGroupName.Text:=FProcessGroupName.Value;


  Storage.ConfigData.Items.Init(FProcessGroupID);
  FProcessGroupID.Name:=CFG_NS_OS_GROUP_ID;
  FProcessGroupID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessGroupID);

  {$ifdef Unix}
  if FProcessGroupID.ID=0 then begin
    FProcessGroupID.Value:=IntToStr(Core.Utils.Unix.Account.GroupID(txtProcessGroupName.Text,sError));
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FProcessGroupID);
  end;
  {$endif}

  txtProcessGroupID.Text:=FProcessGroupID.Value;
  Storage.MatrixNodes.Node.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);
  Storage.AuraDisks.Process.GroupID:=StrToQWordDef(FProcessGroupID.Value,0);

  Storage.ConfigData.Items.Init(FProcessUserName);
  FProcessUserName.Name:=CFG_NS_OS_USER_NAME;
  FProcessUserName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessUserName);
  if FProcessUserName.ID=0 then begin
    FProcessUserName.Value:=txtProcessUserName.Text;
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FProcessUserName);
  end;
  txtProcessUserName.Text:=FProcessUserName.Value;

  Storage.ConfigData.Items.Init(FProcessUserID);
  FProcessUserID.Name:=CFG_NS_OS_USER_ID;
  FProcessUserID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FProcessUserID);
  {$ifdef Unix}
  if FProcessUserID.ID=0 then begin
    FProcessUserID.Value:=IntToStr(Core.Utils.Unix.Account.UserID(txtProcessUserName.Text,sError));
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FProcessUserID);
  end;
  {$endif}

  txtProcessUserID.Text:=FProcessUserID.Value;
  Storage.MatrixNodes.Node.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);
  Storage.AuraDisks.Process.UserID:=StrToQWordDef(FProcessUserID.Value,0);

  Storage.ConfigData.Items.Init(FRaidGroupName);
  FRaidGroupName.Name:=CFG_NS_OS_RAID_GROUP_NAME;
  FRaidGroupName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidGroupName);
  if FRaidGroupName.ID=0 then begin
    FRaidGroupName.Value:=txtRaidGroupName.Text;
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FRaidGroupName);
  end;
  txtRaidGroupName.Text:=FRaidGroupName.Value;

  Storage.ConfigData.Items.Init(FRaidGroupID);
  FRaidGroupID.Name:=CFG_NS_OS_RAID_GROUP_ID;
  FRaidGroupID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidGroupID);
  if FRaidGroupID.ID=0 then begin
    FRaidGroupID.Value:=txtRaidGroupID.Text;
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FRaidGroupID);
  end;
  txtRaidGroupID.Text:=FRaidGroupID.Value;

  Storage.ConfigData.Items.Init(FRaidUserName);
  FRaidUserName.Name:=CFG_NS_OS_RAID_USER_NAME;
  FRaidUserName.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidUserName);
  if FRaidUserName.ID=0 then begin
    FRaidUserName.Value:=txtRaidUserName.Text;
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FRaidUserName);
  end;
  txtRaidUserName.Text:=FRaidUserName.Value;

  Storage.ConfigData.Items.Init(FRaidUserID);
  FRaidUserID.Name:=CFG_NS_OS_RAID_USER_ID;
  FRaidUserID.DomainID:=Storage.ConfigData.Defaults.AllDomains;
  Storage.ConfigData.Items.DB.Read(Storage.Main.Task,FRaidUserID);
  if FRaidUserID.ID=0 then begin
    FRaidUserID.Value:=txtRaidUserID.Text;
    Storage.ConfigData.Items.DB.Add(Storage.Main.Task,FRaidUserID);
  end;
  txtRaidUserID.Text:=FRaidUserID.Value;


  // Reset Services and Scale for Domain Editor
  cbDomain_Service.Checked:=false;
  tbDomainScale.Position:=0;
  lvDomainServices.Clear;

  txtHSBLHI.Clear;
  txtHSWLHI.Clear;

  lvContentFilters.Clear;
  txtSecContent.Clear;
  lvProfileFilters.Clear();

  Storage.Security.Filter.DB.VerifyDefaultTopLevelDomains(Storage.Main.Task);

  Storage.Security.Filter.DB.Fill(Storage.Main.Task,secContentFilter,Security_Content_Filters);
  for iLcv:=0 to High(Security_Content_Filters) do begin
    li:=lvContentFilters.Items.Add;
    li.Data:=Security_Content_Filters[iLcv];
    li.Caption:=IntToStr(Security_Content_Filters[iLcv]^.ID);
    li.SubItems.Add(YES_NO[Security_Content_Filters[iLcv]^.Enabled]);
    li.SubItems.Add(Core.Utils.Time.DateTimeToString(Security_Content_Filters[iLcv]^.Expires,dtpoShort,BIAS_STAMP_OFF));
    li.SubItems.Add(Security_Content_Filters[iLcv]^.Value);
  end;

  Storage.Security.Filter.DB.Fill(Storage.Main.Task,secContentProfiles,Security_Content_Profiles);
  Storage.Security.Filter.DB.Fill(Storage.Main.Task,secTopLevelDomains,Security_TLD_List);
  Storage.Security.Filter.DB.Fill(Storage.Main.Task,secWLService,Security_WL_Services);
  Storage.Security.Filter.DB.Fill(Storage.Main.Task,secBLService,Security_BL_Services);


  LoadFilterItems(secContentProfiles,Security_Content_Profiles,lvProfileFilters);
  LoadFilterItems(secTopLevelDomains,Security_TLD_List,lvHSTLD);
  LoadFilterItems(secWLService,Security_WL_Services,lvDNSWL);
  LoadFilterItems(secBLService,Security_BL_Services,lvDNSBL);
  LoadFilterItems(secWhiteList,Security_Allow_List,lvHSWL);
  LoadFilterItems(secBlackList,Security_Block_List,lvHSBL);
  LoadFilterItems(secAcceptableList,Security_Acceptable_List,lvHSAL);
  LoadFilterItems(secViolatorIP,Security_IP_Violators_List,lvSecIPViolators);

  Storage.DNS.Items.Empty(DNS_Hosts);
  lvDNSRegular.Clear;
  Storage.DNS.Items.DB.Fill(Storage.Main.Task,DNS_Hosts,dnskRegular);
  for iLcv:=0 to High(DNS_Hosts) do begin
    li:=lvDNSRegular.Items.Add;
    li.Data:=DNS_Hosts[iLcv];
    li.Caption:=IntToStr(DNS_Hosts[iLcv]^.ID);
    li.SubItems.Add(Core.Utils.Sockets.InAddrToStr(DNS_Hosts[iLcv]^.IP));
  end;
  Storage.DNS.StartupDNS();
  Storage.DNS.Native.Load(Storage.Main.Task);
  LoadDBMSSettings;

  GUI.UnLock;
end;

initialization
  {$I frmConsole.lrs}
end.


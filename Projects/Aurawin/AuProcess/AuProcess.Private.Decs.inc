FServiceDefaults             : Storage.MatrixServices.Items.Defaults; // default services (not domain specific)
FService                     : Storage.MatrixServices.Items.Item;
FDomain                      : Storage.Domains.Items.TDomain;
FParams                      : Core.Arrays.Types.VarString;
FParamKeys                   : Core.Arrays.Types.KeyStrings;
FCluster                     : Storage.MatrixClusters.Cluster.Item;       // Each node as an instance of this structure
FResource                    : Storage.MatrixResources.Resource.Item;      // Hardware/virtual Resource asset
FNode                        : Storage.MatrixNodes.Node.Item;
FServer                      : TRSRServer;
FQueue                       : TSystemQueue;
TI_Ping                      : Core.Timer.Item;

FProcessGroupName            : Storage.ConfigData.Items.Item;
FProcessGroupID              : Storage.ConfigData.Items.Item;
FProcessUserName             : Storage.ConfigData.Items.Item;
FProcessUserID               : Storage.ConfigData.Items.Item;

FRaidGroupName               : Storage.ConfigData.Items.Item;
FRaidGroupID                 : Storage.ConfigData.Items.Item;

FRaidUserName                : Storage.ConfigData.Items.Item;
FRaidUserID                  : Storage.ConfigData.Items.Item;

FMatrixSetup                 : boolean;
FMatrixFail                  : boolean;


  TTransportHTTP=Class(TTransportBase)
  protected
    FRequest                     : THTTPRequest;
    FResponse                    : THTTPResponse;
    FAccounts                    : Storage.UserAccounts.Items.TList;
    FWebSocket                   : TWebSocketHandler;
    FID                          : int64;
    FUser                        : Core.Strings.VarString;
    FCacheExpired                : boolean;
  public
    procedure   OnCoreObjectRequestAuth(CommandP:PCoreCommand; Var SR:TRSR; sTitle,sPrompt:Core.Strings.VarString); override;
    procedure   OnCoreObjectError(CommandP:PCoreCommand; Var SR:TRSR; Error:WORD); override;
    procedure   OnCoreObjectSuccess(CommandP:PCoreCommand; Var SR:TRSR; Code:Word); override;
    procedure   OnCoreObjectRedirect(CommandP:PCoreCommand; Var SR:TRSR; URL:Core.Strings.VarString); override;
    function    OnCoreObjectCheckCredentials(CommandP:PCoreCommand; Var SR:TRSR; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD; override;
    procedure   OnCoreObjectNotFound(Var SR:TRSR; NameSpace:Core.Strings.VarString); override;
    procedure   OnCoreObjectCommandNotFound(Var SR:TRSR; nsObject,nsCommand:Core.Strings.VarString); override;
    procedure   OnBadRequest(Var SR:TRSR); override;
    procedure   OnInvalidMethod(Var SR:TRSR); override;
    procedure   OnNotFound(Var SR:TRSR; Title,Content:Core.Strings.VarString); override;
    procedure   OnDataReceived(Var SR:TRSR; var Handled:boolean); override;
    procedure   Redirect(Var SR:TRSR; URL:Core.Strings.VarString); override;
    function    ResolvePath(var URI: Core.Strings.VarString; var Folder:TDSFolder; var Resource:TDSFile; Var Path,FileName,Ext: Core.Strings.VarString; Refactor:TStream): TResolveResult; override;
    procedure   RequestPassword(Var SR:TRSR; Title,Prompt:Core.Strings.VarString);
    procedure   SendContent(Var SR:TRSR; Content:Core.Strings.VarString); override;
    procedure   SendFile(Var SR:TRSR; Folder:TDSFolder; Resource:TDSFile; Var sExt:Core.Strings.VarString);
    procedure   GetResponse(Var SR:TRSR); override;
  public
    procedure   Reset; override;
  public
    constructor Create(aManager:THTTPManager); reIntroduce;
    destructor  Destroy; override;
  end;


unit coSearch;

interface
  uses
    Classes,

    RSR,
    RSR.Core,

    Core.Keywords,
    Core.Timer,

    App.Consts,

    Core.Database,
    Core.Database.Timer,
    Core.Database.Types,
    Core.Database.Monitor,
    Core.Database.Monitor.Types,
    Core.Database.Monitor.Notify,
    Core.Database.SQL,

    Core.Strings,
    Core.Arrays,
    Core.Arrays.Types,
    Core.Arrays.KeyString,
    Core.Arrays.VarString,

    Storage,
    Storage.Main,
    Storage.CoreObjects,
    Storage.Search,

    Core.XML,

    uHTTPd,
    hHTTPd,


    DOM;

Type
  Search=class
  const
    ACLInf:TACLInfo=(
      Name        : 'Search';
      NameSpace   : '/core/srch';
      Caption     : 'Search System Core Object';
      Prompt      : 'User can access system level search';
      Description : 'Back-end router for search system'
    );
    CLSInf:TCLSInfo=(
      Name        : 'TSearchCore';
      Location    : 'coSearch.pas';
    );
    Header:TCoreObjectInfo=(
      ID          : 0;
      ProviderID  : 0;
      Enabled     : true;
      Anonymous   : false;
      Scale       : 0;
      CLSInfo     : @CLSInf;
      ACLInfo     : @ACLInf;
    );
    XMLInf:TXMLInfo=(
      Enabled                : true;
    );
  Type
    List=class
    const
      ACLInf:TACLInfo=(
        Name               : 'List';
        NameSpace          : '/l';
        Caption            : 'List results to a query';
        Prompt             : 'User can retrieve results';
        Description        : 'Download search results from Database';
      );
      cmd:TCoreCommand=(
        HeaderP            : @Header;
        ID                 : 0;
        Enabled            : true;
        Anonymous          : false;
        Cache              : false;
        Compress           : true;
        Secure             : false;
        XMLInfo            : @XMLInf;
        ACLInfo            : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Find=class
    const
      ACLInf:TACLInfo=(
        Name               : 'Find';
        NameSpace          : '/f';
        Caption            : 'Find results';
        Prompt             : 'User can search for registered items';
        Description        : 'Enables core object system searching of items from Database';
      );
      cmd:TCoreCommand=(
        HeaderP            : @Header;
        ID                 : 0;
        Enabled            : true;
        Anonymous          : false;
        Cache              : false;
        Compress           : true;
        Secure             : false;
        XMLInfo            : @XMLInf;
        ACLInfo            : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Discover=class
    const
      ACLInf:TACLInfo=(
        Name               : 'Discover';
        NameSpace          : '/d';
        Caption            : 'Discover search elements';
        Prompt             : 'User can discover elements of search';
        Description        : 'Enables users to discover providers, queries, or searches';
      );
      cmd:TCoreCommand=(
        HeaderP            : @Header;
        ID                 : 0;
        Enabled            : true;
        Anonymous          : false;
        Cache              : false;
        Compress           : true;
        Secure             : false;
        XMLInfo            : @XMLInf;
        ACLInfo            : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
    Query=class
    const
      ACLInf:TACLInfo=(
        Name               : 'Query';
        NameSpace          : '/q';
        Caption            : 'Query system core objects';
        Prompt             : 'User can access search mechanims of core objects';
        Description        : 'Enables core objects to connect with search system';
      );
      cmd:TCoreCommand=(
        HeaderP            : @Header;
        ID                 : 0;
        Enabled            : true;
        Anonymous          : false;
        Cache              : false;
        Compress           : true;
        Secure             : false;
        XMLInfo            : @XMLInf;
        ACLInfo            : @ACLInf;
        Method                 : nil;
        Resource               : nil;
      );
    end;
  end;

Type
  TSearchCore=Class(TCoreObject)
  private
    FIdxCO:integer;
    FidxCC:integer;
  protected
  public
    srchProvider                 : Storage.Search.Provider.TItem;
    srchQuery                    : Storage.Search.Query.TItem;
    srchResults                  : Storage.Search.Cache.TItem;
    srchCriteria                 : Core.Arrays.Types.KeyStrings;
  private
    function Execute(var Search:Storage.Search.Cache.TItem; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream):WORD;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
  protected
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; override;
  protected
    function  cmdDiscover(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdQuery(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  cmdFind(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  end;
  procedure Install(Task:Core.Database.Types.TTask);
implementation
uses
  SysUtils,
  DateUtils;

procedure Install(Task:Core.Database.Types.TTask);
begin
  TSearchCore.Install(Task);
end;

class procedure TSearchCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TSearchCore);
  Storage.CoreObjects.Add(coSearch.Search.Header,CoreObjectItems);
  COREOBJECT_VerifyID(Task,coSearch.Search.Header);
  COREOBJECT_VerifyID(Task,coSearch.Search.Query.cmd);
  COREOBJECT_VerifyID(Task,coSearch.Search.List.cmd);
  COREOBJECT_VerifyID(Task,coSearch.Search.Find.cmd);
  COREOBJECT_VerifyID(Task,coSearch.Search.Discover.cmd);
end;

class procedure TSearchCore.UnInstall;
begin
  UnRegisterClass(TSearchCore);
end;

procedure TSearchCore.Initialize;
begin
  Core.Arrays.KeyString.Init(srchCriteria);
  Storage.Search.Cache.Init(srchResults);
  Storage.Search.Provider.Init(srchProvider);
  Storage.Search.Query.Init(srchQuery);
  With Storage.Search.Cache do begin
    Storage.CoreObjects.Add(coSearch.Search.Query.cmd,FCommands,Header,@cmdQuery);
    Storage.CoreObjects.Add(coSearch.Search.List.cmd,FCommands,Header,@cmdList);
    Storage.CoreObjects.Add(coSearch.Search.Find.cmd,FCommands,Header,@cmdFind);
    Storage.CoreObjects.Add(coSearch.Search.Discover.cmd,FCommands,Header,@cmdDiscover);
  end;
end;

procedure TSearchCore.Finalize;
begin
  Core.Arrays.KeyString.Done(srchCriteria);
  Storage.Search.Cache.Done(srchResults);
  Storage.Search.Provider.Done(srchProvider);
  Storage.Search.Query.Done(srchQuery);
end;

function TSearchCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_OK;
end;

function TSearchCore.cmdList(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin

end;

function TSearchCore.Execute(var Search:Storage.Search.Cache.TItem; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream ):WORD;
begin
  if (
    Storage.Search.Query.DB.Read(
      FTask,
      UAP(SR)^.DomainID,
      Search.QueryID,
      srchQuery
    ) and
    (srchQuery.ID<>0)
  ) then begin
    FIdxCO:=CoreObjects.IndexOf(srchProvider.NameSpace1);
    if FIdxCO<>-1 then begin
      FIdxCC:=CoreObjects[FIdxCO].IndexOf(srchProvider.NameSpace2);
      if (FIdxCC<>-1) then begin
        CoreObjects[FIdxCO].Transport(SR).SetCoreObject(srchQuery.NameSpacePrimary);
        CoreObjects[FIdxCO].Transport(SR).SetCoreCommand(srchQuery.NameSpaceSecondary);
        CoreObjects.NestedExecute(SR,CoreObjects[FIdxCO].Transport(SR),Parameters,srcHeaders,respHeaders,Data);
        Result:=CoreObjects.Result;
        if (Result = CO_STATUS_OK) then begin
          srchResults.Executed:=Core.Timer.dtUT;
          srchResults.Expires:=DateUtils.IncSecond(Core.Timer.dtUT,srchQuery.TimeToLive);
          If srchResults.ID=0 then begin
            if Storage.Search.Cache.DB.Add(FTask,UAP(SR)^.DomainID,srchQuery.TimeToLive,srchResults) then begin
              //Storage.Search.Cache.toXML(srchResults,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else begin
            if Storage.Search.Cache.DB.Write(FTask,UAP(SR)^.DomainID,srchQuery.TimeToLive,srchResults) then begin
              //Storage.Search.Cache.toXML(srchResults,Transport(SR).Output,XML_HEADER_ON);
              Result:=CO_STATUS_OK;
            end else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end;
        end;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_NOT_FOUND;
    end else
      Result:=CO_STATUS_ERR_CO_NOT_FOUND;
  end;
end;

function TSearchCore.cmdDiscover(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  xNode:TDOMNode;

  procedure DISCO_Provider;
  begin
    if Storage.Search.Provider.fromXML(xNode,srchProvider) and (Length(srchProvider.NameSpace1)>0) and (Length(srchProvider.NameSpace2)>0) then begin
      if Storage.Search.Provider.DB.Identify(FTask,srchProvider)<>0 then begin
        Result:=CO_STATUS_OK;
        Storage.Search.Provider.toXML(srchProvider,Transport(SR).Output,Core.XML.XML_HEADER_ON);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end;

  procedure DISCO_Query;
  begin
    if Storage.Search.Query.fromXML(xNode,srchQuery) and (Length(srchQuery.NameSpacePrimary)>0) and (Length(srchQuery.NameSpaceSecondary)>0) then begin
      Core.Arrays.KeyString.fromString(srchCriteria,srchQuery.Term,'=',';');
      if Storage.Search.Query.DB.Identify(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,srchQuery)<>0 then begin
        Result:=CO_STATUS_OK;
        Storage.Search.Query.toXML(srchQuery,Transport(SR).Output,XML_HEADER_ON);
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end;

  procedure DISCO_Search;
  begin
    if Storage.Search.Cache.fromXML(xNode,srchResults) and (srchResults.ProviderID>0) and (srchResults.QueryID>0) then begin
      if Storage.Search.Query.DB.Read(FTask,UAP(SR)^.DomainID,srchResults.QueryID,srchQuery) then begin;
        Core.Arrays.KeyString.fromString(srchCriteria,srchQuery.Term,'=',';');

        if Storage.Search.Cache.DB.Identify(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,srchQuery.TimeToLive,srchResults)<>0 then begin
          if ((Core.Timer.dtUT>srchResults.Expires) or (srchResults.Executed=0.0)) and (srchResults.ReadDisco) then begin
            if Storage.Search.Provider.DB.Read(FTask,srchResults.ProviderID,srchProvider) then
              Result:=Execute(srchResults,SR,Parameters,srcHeaders,respHeaders,Data)
            else
              Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          end else
             Result:=CO_STATUS_OK;
          if Result=CO_STATUS_OK then
            Storage.Search.Cache.toXML(srchResults,Transport(SR).Output,XML_HEADER_ON);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end;

begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    xNode:=FXMLDocument.DocumentElement;
    if (xNode<>nil) then begin
      Core.Arrays.KeyString.Empty(srchCriteria);
      Storage.Search.Provider.Empty(srchProvider);
      Storage.Search.Cache.Empty(srchResults);
      case xNode.NodeName of
        Storage.Search.Provider.XML.Stanzas.Provider : DISCO_Provider;
        Storage.Search.Query.XML.Stanzas.Query       : DISCO_Query;
        Storage.Search.Cache.XML.Stanzas.Search     : DISCO_Search;
      end;
      Core.Arrays.KeyString.Empty(srchCriteria);
      Storage.Search.Provider.Empty(srchProvider);
      Storage.Search.Cache.Empty(srchResults);
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED
end;

function TSearchCore.cmdQuery(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    if Storage.Search.Query.fromXML(FXMLDocument,srchQuery) and (Length(srchQuery.NameSpacePrimary)>0) and (Length(srchQuery.NameSpaceSecondary)>0) then begin
      Core.Arrays.KeyString.fromString(srchCriteria,srchQuery.Term,'=',';');
      srchProvider.NameSpace1:=srchQuery.NameSpacePrimary;
      srchProvider.NameSpace2:=srchQuery.NameSpaceSecondary;
      Storage.Search.Provider.DB.Identify(FTask,srchProvider);
      if srchProvider.ID>0 then begin
        if Storage.Search.Query.DB.Identify(FTask,UAP(SR)^.DomainID,UAP(SR)^.ID,srchQuery)<>0 then begin
          Storage.Search.Cache.Empty(srchResults);

          // Now we need to identify query.
          srchResults.ProviderID:=srchProvider.ID;
          srchResults.NetworkID:=srchQuery.NetworkID;
          srchResults.GroupID:=srchQuery.GroupID;
          srchResults.SessionID:=srchQuery.SessionID;
          srchResults.UserID:=UAP(SR)^.ID;
          srchResults.QueryID:=srchQuery.ID;
          // Prefetch cached results
          Storage.Search.Cache.DB.Find(FTask,UAP(SR)^.DomainID,srchResults.ProviderID,srchResults.NetworkID,srchResults.UserID,srchResults.QueryID,srchResults);

          if (srchResults.ID=0) or (Core.Timer.dtUT>srchResults.Expires) then
            Result:=Execute(srchResults,SR,Parameters,srcHeaders,respHeaders,Data)
          else
            Result:=CO_STATUS_OK;
          if Result=CO_STATUS_OK then
            Storage.Search.Cache.toXML(srchResults,Transport(SR).Output,XML_HEADER_ON);
          Storage.Search.Cache.Empty(srchResults);
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED
end;

function TSearchCore.cmdFind(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString;  var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  idx:integer;
begin
  Result:=CO_STATUS_FAIL;
  if SR.Credentials<>nil then begin
    // this search re-requires query and provider identity
    if Storage.Search.Cache.fromXML(FXMLDocument,srchResults) and (srchResults.QueryID>0) and (srchResults.ProviderID<>0)  then begin
      if Storage.Search.Query.DB.Read(FTask,UAP(SR)^.DomainID,srchResults.QueryID,srchQuery) then begin
        if Storage.Search.Provider.DB.Read(FTask,srchResults.ProviderID,srchProvider) then begin
          if srchProvider.ID<>0 then begin
            idx:=CoreObjects.IndexOf(srchProvider.NameSpace1);
            if idx<>-1 then begin
              // prefetch result
              Storage.Search.Cache.DB.Find(FTask,UAP(SR)^.DomainID,srchProvider.ID,srchQuery.NetworkID,UAP(SR)^.ID,srchQuery.ID,srchResults);
              Result:=CoreObjects[idx].Find(SR,Parameters,srcHeaders,respHeaders,Data,srchProvider,srchQuery,srchResults);
              if (srchResults.ID=0) then
                Storage.Search.Cache.DB.Add(FTask,UAP(SR)^.DomainID,srchQuery.TimeToLive,srchResults)
              else if srchResults.WriteBack then
                Storage.Search.Cache.Db.Write(FTask,UAP(SR)^.DomainID,srchQuery.TimeToLive,srchResults);
            end else
              Result:=CO_STATUS_ERR_CO_NOT_FOUND;
          end else
            Result:=CO_STATUS_ERR_CO_CMD_INVALID_SEARCH;
        end else
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
      end else
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
    end else
      Result:=CO_STATUS_ERR_CO_CMD_MISSING_FIELDS;
  end else
    Result:=CO_STATUS_ERR_CO_CMD_ACCESS_DENIED
end;

end.

{
 Copyright Aurawin LLC 2003-2010
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit coSessions;

interface
  uses
    Classes,


    App.Consts,

    hHttpd,

    HTTPDefs,

    RSR,
    RSR.HTTP,
    RSR.Core,

    Core.Timer,
    Core.Strings,
    Core.Streams,
    Core.Keywords,

    Core.Database,
    Core.Database.Types,
    Core.Database.SQL,

    uProviders,
    uGoogle,

    Core.Arrays,
    Core.Arrays.Types,

    Core.Arrays.Bytes,
    Core.Arrays.VarString,
    Core.Arrays.KeyString,
    Core.Arrays.LargeWord,

    Storage,
    Storage.Main,
    Storage.CoreObjects,
    Storage.SrchResults,
    Storage.SrchSessions,
    Storage.UserAccounts,
    Storage.TmpAccounts,
    Storage.SrchProviders,

    SysUtils;

type
  ns=class
  type
    Session=class
    const
      ACLInf:TACLInfo=(
        Name                     : 'Sessions';
        NameSpace                : '/core/srch/snsl';
        Caption                  : 'Search Sessions Core Object';
        Prompt                   : 'User can access to search session layer for system';
        Description              : 'Provides a search session layer to search results.'
      );
      CLSInf:TCLSInfo=(
        Name                     : 'TSearchSessionCore';
        Location                 : 'coSessions.pas';
      );
      Header:TCoreObjectInfo=(
        ID                       : 0;
        ProviderID               : 0;
        Enabled                  : true;
        Anonymous                : false;
        Scale                    : 0;
        CLSInfo                  : @CLSInf;
        ACLInfo                  : @ACLInf;
      );
      XMLInf:TXMLInfo=(
        Enabled                  : false;
      );
      fieldID                    : String = 'SessionID';
      fieldParentID              : String = 'ParentID';
      fieldClassID               : String = 'ClassID';
      fieldResultID              : String = 'ResultID';
      fieldTitle                 : String = 'Title';
    Type
      List=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'List';
          NameSpace              : '/sl';
          Caption                : 'Session List';
          Prompt                 : 'User can access list of sessions';
          Description            : 'Provides access to the list of feeds';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Delete=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Delete';
          NameSpace              : '/d';
          Caption                : 'Delete Session';
          Prompt                 : 'User can delete sessions';
          Description            : 'Provides access to delete search sessions';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      end;
      Result=class
      const
        ACLInf:TACLInfo=(
          Name                   : 'Result';
          NameSpace              : '/res/rs';
          Caption                : 'Results Store';
          Prompt                 : 'User can access session layer result store';
          Description            : 'Provides access to results for search sessions';
        );
        cmd:TCoreCommand=(
          HeaderP                : @Header;
          ID                     : 0;
          Enabled                : true;
          Anonymous              : false;
          Cache                  : false;
          Compress               : true;
          Secure                 : false;
          XMLInfo                : @XMLInf;
          ACLInfo                : @ACLInf;
          Method             : nil;
          Resource           : nil;
        );
      type
        Delete=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Delete';
            NameSpace            : '/res/d';
            Caption              : 'Delete result';
            Prompt               : 'User can delete results';
            Description          : 'Provides access to delete results for search sessions';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Save=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Save';
            NameSpace            : '/res/s';
            Caption              : 'Save result';
            Prompt               : 'User can save results';
            Description          : 'Provides access to save results for search sessions';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Rank=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Rank';
            NameSpace            : '/res/rk';
            Caption              : 'Rank result';
            Prompt               : 'User can rank results';
            Description          : 'Provides access to rank results for search sessions';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Rate=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Rate';
            NameSpace            : '/res/rt';
            Caption              : 'Rate result';
            Prompt               : 'User can rate results';
            Description          : 'Provides access to rate results for search sessions';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Annotate=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Annotate';
            NameSpace            : '/res/nt';
            Caption              : 'Annotate result';
            Prompt               : 'User can annotate results';
            Description          : 'Provides access to annotate results for search sessions';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
        Ammend=class
        const
          ACLInf:TACLInfo=(
            Name                 : 'Ammend';
            NameSpace            : '/res/amen';
            Caption              : 'Amment result';
            Prompt               : 'User can ammend results';
            Description          : 'Provides access to ammend results for search sessions';
          );
          cmd:TCoreCommand=(
            HeaderP              : @Header;
            ID                   : 0;
            Enabled              : true;
            Anonymous            : false;
            Cache                : false;
            Compress             : true;
            Secure               : false;
            XMLInfo              : @XMLInf;
            ACLInfo              : @ACLInf;
            Method             : nil;
            Resource           : nil;
          );
        end;
      end;
    end;
  end;
  TSearchSessionCore=Class(TCoreObject)
  private
    ProviderCache                : Storage.SrchProviders.Items.TCache;
    ProviderInfoP                : PProviderSocketInfo;
    HTTP                         : PHTTP;
    resultID                     : QWord;
    Session                      : Storage.SrchSessions.Session.Item;
    Sessions                     : Storage.SrchSessions.Session.Items;
    Results                      : Storage.SrchResults.Store.Items;
    Refactor                     : TMemoryStream;
  private
    function  PerformSession_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;  // Deletes a session
    function  PerformInteraction_Save_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;     // Adds a save id to saveid items list...
    function  PerformInteraction_Delete_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  PerformInteraction_Rank_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  PerformInteraction_Rate_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  PerformInteraction_Annotate_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  PerformInteraction_Ammend_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  PerformSession_List_Sessions(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
    function  PerformSession_List_Results(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD; // Retrieves expanded version of saved results for sessionID;
    function  PerformInteraction_Add_Session(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
  protected
    // RSR Core Object Initialization Call happens on entry
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
    function  BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD; override;
  end;
  procedure Install(Task:Core.Database.Types.TTask);

implementation


procedure Install(Task:Core.Database.Types.TTask);
begin
  TSearchSessionCore.Install(Task);
end;

class procedure TSearchSessionCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TSearchSessionCore);
  with ns.Session do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
    COREOBJECT_VerifyID(Task,List.cmd);
    COREOBJECT_VerifyID(Task,Delete.cmd);
    COREOBJECT_VerifyID(Task,Result.cmd);
    COREOBJECT_VerifyID(Task,Result.Delete.cmd);
    COREOBJECT_VerifyID(Task,Result.Save.cmd);
    COREOBJECT_VerifyID(Task,Result.Rank.cmd);
    COREOBJECT_VerifyID(Task,Result.Rate.cmd);
    COREOBJECT_VerifyID(Task,Result.Annotate.cmd);
    COREOBJECT_VerifyID(Task,Result.Ammend.cmd);
  end;
end;

class procedure TSearchSessionCore.UnInstall;
begin
  UnRegisterClass(TSearchSessionCore);
end;

procedure TSearchSessionCore.Initialize;
var
  icmdID:QWord;
begin
  ProviderCache:=Storage.SrchProviders.Items.TCache.Create;
  With ns.Session do begin
    Storage.CoreObjects.Add(List.cmd,FCommands,Header,@PerformSession_List_Sessions);
    Storage.CoreObjects.Add(Delete.cmd,FCommands,Header,@PerformSession_Delete);
    Storage.CoreObjects.Add(Result.cmd,FCommands,Header,@PerformSession_List_Results);
    Storage.CoreObjects.Add(Result.Delete.cmd,FCommands,Header,@PerformInteraction_Delete_Result);
    Storage.CoreObjects.Add(Result.Save.cmd,FCommands,Header,@PerformInteraction_Save_Result);
    Storage.CoreObjects.Add(Result.Rank.cmd,FCommands,Header,@PerformInteraction_Rank_Result);
    Storage.CoreObjects.Add(Result.Rate.cmd,FCommands,Header,@PerformInteraction_Rate_Result);
    Storage.CoreObjects.Add(Result.Annotate.cmd,FCommands,Header,@PerformInteraction_Annotate_Result);
    Storage.CoreObjects.Add(Result.Ammend.cmd,FCommands,Header,@PerformInteraction_Ammend_Result);
  end;
end;

procedure TSearchSessionCore.Finalize;
begin
  Storage.SrchSessions.Session.Done(Session);
  Storage.SrchSessions.Session.Done(Sessions);
  Storage.SrchResults.Store.Done(Results);
  ProviderCache.Free();
end;

function  TSearchSessionCore.BeforeExecute(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
begin
  Refactor:=OwnerP^.Manager.Refactor;
  HTTP:=SR.Info.DataP;
  Handled:=true;
end;

function TSearchSessionCore.PerformSession_List_Results(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD;
var
  iLcv,iLength,iCount:Integer;
begin
  Handled:=true;
  If (HTTP<>Nil) then begin
    Result:=TTransportBase(SR.Transport).OnCoreObjectCheckCredentials(CommandP,SR,srcHeaders,respHeaders,Data);
    if (Result=CO_STATUS_OK) then begin
      Storage.SrchResults.Store.Empty(Results);
      Storage.SrchSessions.Session.Empty(Session);
      Session.ID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldID,0);
      Storage.SrchSessions.Session.DB.ListSavedIDs(FTask,Session.ID,Session.SavedIDs);
      Storage.SrchResults.Store.DB.List(FTask,Session.SavedIDs,Results);
      iCount:=Length(Results);
      Refactor.Clear;
      Refactor.WriteAnsiString(IntToStr(iCount)+#1);
      for iLcv:=0 to iCount-1 do begin
        Refactor.WriteAnsiString(IntToStr(Results[iLcv].ID)+#4);
        Refactor.WriteAnsiString(Results[iLcv].Title);
        Refactor.WriteAnsiString(#4);
        Refactor.WriteAnsiString(Results[iLcv].Link);
        Refactor.WriteAnsiString(#4);
        Refactor.WriteAnsiString(Results[iLcv].Description);
        Refactor.WriteAnsiString(#4);
        Refactor.WriteAnsiString(ProviderCache.GetName(Results[iLcv].ProviderID));
        Refactor.WriteAnsiString(#3);
      end;
      If iCount>0 then
        Refactor.Size:=Refactor.Size-1;

      //sOutput:=Core.Streams.toString(Refactor);

      Refactor.Clear;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function TSearchSessionCore.PerformInteraction_Add_Session(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Handled:=True;
  If (HTTP<>Nil) then begin
    Result:=TTransportBase(SR.Transport).OnCoreObjectCheckCredentials(CommandP,SR,srcHeaders,respHeaders,Data);
    if (Result=CO_STATUS_OK) then begin
      Storage.SrchSessions.Session.Empty(Session);
      Session.ParentID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldParentID);
      Session.ClassID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldClassID);
      Session.Caption:=Core.Arrays.KeyString.GetItemByKey(srcHeaders,ns.Session.fieldTitle);
      Session.UserID:=HTTP^.UAP^.ID;
      If (Length(Session.Caption)>0) then begin
        Storage.SrchSessions.Session.DB.Add(FTask,Session);
        {
            Datagram
            [0] ID#2Parent#2Caption
            #1  if there was more data
        }
        Refactor.Clear;
        Refactor.WriteAnsiString(Concat(IntToStr(Session.ID),#2,IntToStr(Session.ParentID),#2,Session.Caption));
        //sOutput:=Core.Streams.toString(Refactor);
        Refactor.Clear;
        Result:=CO_STATUS_OK;
        TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
      end;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function  TSearchSessionCore.PerformInteraction_Rank_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin

end;

function  TSearchSessionCore.PerformInteraction_Rate_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin

end;

function  TSearchSessionCore.PerformInteraction_Annotate_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin

end;

function  TSearchSessionCore.PerformInteraction_Ammend_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin

end;

function TSearchSessionCore.PerformInteraction_Delete_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iIndex:integer;
begin
  Handled:=true;
  If (HTTP<>Nil) then begin
    Result:=TTransportBase(SR.Transport).OnCoreObjectCheckCredentials(CommandP,SR,srcHeaders,respHeaders,Data);
    if (Result=CO_STATUS_OK) then begin
      Session.ID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldID);
      resultID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldResultID);
      If (Session.ID<>0) and (resultID<>0) then begin
        Storage.SrchSessions.Session.DB.ListSavedIDs(FTask,Session.ID,Session.SavedIDs);
        iIndex:=Core.Arrays.LargeWord.IndexOf(resultID,Session.SavedIDs);
        if iIndex<>-1 then begin
          Core.Arrays.LargeWord.Remove(iIndex,Session.SavedIDs);
          Storage.SrchSessions.Session.DB.UpdateSavedIDs(FTask,Session.ID,Session.SavedIDs);
        end;
        Result:=CO_STATUS_OK;
        TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
      end;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
  end;
end;

function TSearchSessionCore.PerformInteraction_Save_Result(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
begin
  Handled:=true;
  If (HTTP<>Nil) then begin
    Result:=TTransportBase(SR.Transport).OnCoreObjectCheckCredentials(CommandP,SR,srcHeaders,respHeaders,Data);
    if (Result=CO_STATUS_OK) then begin
      Session.ID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldID);
      resultID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldResultID);
      If (Session.ID<>0) and (resultID<>0) then begin
        Storage.SrchSessions.Session.DB.ListSavedIDs(FTask,Session.ID,Session.SavedIDs);
        Core.Arrays.LargeWord.Add(resultID,Session.SavedIDs,aoCheckForDuplicates);
        Storage.SrchSessions.Session.DB.UpdateSavedIDs(FTask,Session.ID,Session.SavedIDs);
        Result:=CO_STATUS_OK;
        TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
      end else begin
        Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
      end;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function TSearchSessionCore.PerformSession_List_Sessions(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iIndex,iLength,iCount,iLcv:Integer;
begin
  Handled:=true;
  If (HTTP<>Nil) then begin
    Result:=TTransportBase(SR.Transport).OnCoreObjectCheckCredentials(CommandP,SR,srcHeaders,respHeaders,Data);
    if (Result=CO_STATUS_OK) then begin
      Session.ClassID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldClassID);
      Session.UserID:=HTTP^.UAP^.ID;
      Storage.SrchSessions.Session.Empty(Sessions);
      if Storage.SrchSessions.Session.DB.List(FTask,Session.ClassID,Session.UserID,Sessions) then begin
        {
                Datagram
        Header    [0] TotalSessions#1
        SessionX  [1] ID#4Parent#4Caption#3
        SessionX+1[2] ID#4Parent#4Caption#3
        SessionX+2[3] ID#4Parent#4Caption#3
                      #1  if there was more data
        }
        iCount:=Length(Sessions);
        Refactor.Clear;
        Refactor.WriteAnsiString(IntToStr(iCount)+#1);
        For iLcv:=0 to iCount-1 do begin
          Refactor.WriteAnsiString(IntToStr(Sessions[iLcv]^.ID)+#4+IntToStr(Sessions[iLcv]^.ParentID)+#4);
          Refactor.WriteAnsiString(Sessions[iLcv]^.Caption);
          Refactor.WriteAnsiString(#3);
        end;
        if iCount>0 then
          Refactor.Size:=Refactor.Size-1;
        //sOutput:=Core.Streams.toString(Refactor);
        Result:=CO_STATUS_OK;
        TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
      end else begin
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
      end;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

function TSearchSessionCore.PerformSession_Delete(CommandP:PCoreCommand; Var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:boolean):WORD;
var
  iIndex:Integer;
begin
  Handled:=true;
  If (HTTP<>Nil) then begin
    Result:=TTransportBase(SR.Transport).OnCoreObjectCheckCredentials(CommandP,SR,srcHeaders,respHeaders,Data);
    if (Result=CO_STATUS_OK) then begin
      Session.ID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldID);
      Session.ClassID:=Core.Arrays.KeyString.GetItemAsQWord(srcHeaders,ns.Session.fieldClassID);
      Session.UserID:=HTTP^.UAP^.ID;
      If (Session.ID<>0) then begin
        if Storage.SrchSessions.Session.DB.Delete(FTask,Session.ClassID,Session.UserID,Session.ID) then begin
          Result:=CO_STATUS_OK;
          TTransportBase(SR.Transport).OnCoreObjectSuccess(CommandP,SR,CO_STATUS_OK);
        end else begin
          OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          Result:=CO_STATUS_ERR_CO_CMD_DBMS_FAILURE;
          TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_DBMS_FAILURE);
        end;
      end else begin
        OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        Result:=CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD;
        TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_UNDERSTOOD);
      end;
    end;
  end else begin
    OwnerP^.LastError:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    Result:=CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED;
    TTransportBase(SR.Transport).OnCoreObjectError(CommandP,SR,CO_STATUS_ERR_CO_CMD_NOT_INITIALIZED);
  end;
end;

end.

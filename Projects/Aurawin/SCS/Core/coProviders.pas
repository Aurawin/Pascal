{
 Copyright Aurawin LLC 2003-2012
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}

unit coProviders;

interface
  uses
    Classes,

    App,
    App.Consts,


    Core.Timer,

    Core.Database,
    Core.Database.Types,
    Core.Database.SQL,


    RSR,
    RSR.Core,
    Core.Arrays,
    Core.Arrays.Types,
    Core.Arrays.VarString,
    Core.Arrays.KeyString,
    Core.Arrays.Bytes,

    Core.Strings,
    Core.Keywords,

    Storage,
    Storage.Main,
    Storage.CoreObjects,

    Storage.SrchQueries,
    Storage.SrchTmpUserInteractions,
    Storage.SrchProviders,

    uGoogle,
    uProviders,
    hHttpd,

    SysUtils;

type
  ns=class
  type
    Providers=class
    const
      ACLInf:TACLInfo=(
        Name        : 'Provider';
        NameSpace   : '/core/srch/pvdr';
        Caption     : 'Search Provider Core Object';
        Prompt      : 'User can access search result provisioning';
        Description : 'Back-end result provider gateway';
      );
      CLSInf:TCLSInfo=(
        Name        : 'TProviderCore';
        Location    : 'coProviders.pas';
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
    end;
  end;

  TProviderCore=Class(TCoreObject)
  private
    saQuery                      : Core.Arrays.Types.VarString;
    saRequestCookies             : Core.Arrays.Types.VarString;
    kpRequestCookies             : Core.Arrays.Types.KeyStrings;
    sQueryString                 : String;
    ProviderInfoP                : PProviderSocketInfo;
    RSRDataP                     : PHTTP;
    RSRP                         : PRSR;
    resultID                     : Int64;
    bbResultData                 : Core.Arrays.Types.Bytes;
    ResultSet                    : Storage.SrchQueries.Results.Item;
  private
    procedure PerformInteraction_Delete;
  protected
    class procedure Install(Task:Core.Database.Types.TTask); override;
    class procedure UnInstall; override;
  protected
    procedure Initialize; override;
    procedure Finalize; override;
  public
    function ProviderP:Storage.SrchProviders.Items.PItem;
  End;

  procedure Install(Task:Core.Database.Types.TTask);

implementation

procedure Install(Task:Core.Database.Types.TTask);
begin
  TProviderCore.Install(Task);
end;

class procedure TProviderCore.Install(Task:Core.Database.Types.TTask);
begin
  RegisterClass(TProviderCore);
  with ns.Providers do begin
    Storage.CoreObjects.Add(Header,CoreObjectItems);
    COREOBJECT_VerifyID(Task,Header);
  end;
end;

class procedure TProviderCore.UnInstall;
begin
  UnRegisterClass(TProviderCore);
end;

procedure TProviderCore.Initialize;
begin

end;

procedure TProviderCore.Finalize;
begin

end;

function TProviderCore.ProviderP:Storage.SrchProviders.Items.PItem;
begin
  Result:=TProviderManager(OwnerP^.Manager).ProviderP;
end;

procedure TProviderCore.PerformInteraction_Delete;
var
  iIndex,iPageLength,iPageStart:Integer;
begin
  Storage.SrchTmpUserInteractions.Interaction.DB.Add(FTask,ProviderP^.ID,OwnerP^.DomainP^.ID,RSRDataP^.UAP^.ID,resultID,Integer(tsuiDelete),bbResultData);
  {
  sPage:=StringReplace(sPage,'{$i srs_page_output}',sOutput,[rfReplaceAll]);
  iPageStart:=System.Pos(#13#10#13#10,sPage);
  If iPageStart>0 then
    iPageLength:=Length(sPage)-iPageStart-4
  else
    iPageLength:=Length(sPage);
  sPage:=StringReplace(sPage,'{$i srs_page_length}',IntToStr(iPageLength),[rfReplaceAll]);
  OwnerP^.Manager.Send(RSRP,sPage);
  }
end;



(*
procedure TProviderCore.Execute(

      Server:TRSRServer;
      Manager:TRSRManager;
      Fat:TDSFAT;
      Accounts:TUserAccounts;
      Var SR:TRSR;
      Var Keywords:TKeywords;
      Folder:TDSFolder;
      Resource:TDSFile;
      NameSpace:String;
      out sTitle,sPage:String;
      out Status:Cardinal
);
var
  saLength:Integer;
  ProviderP:PSRCHProvider;
  ProviderMan:TProviderManager;

  procedure CheckCookies;
  var
    iLcv:Integer;
  begin
    If (RSRDataP^.UserID=0) then begin
      Empty(kpRequestCookies);
      Empty(saRequestCookies);
      Core.Arrays.KeyString.GetItemsByKey(Request.Headers,'Cookie',saRequestCookies);
      for iLcv:=0 to High(saRequestCookies) do
        Core.Arrays.KeyString.fromString(kpRequestCookies,saRequestCookies[iLcv],'=','; ');
      RSRDataP^.Cookie:=Core.Arrays.KeyString.GetItemByKey(kpRequestCookies,'EUIDC');
      If RSRDataP^.Cookie<>'' then
        uStorage.TempUser_Find(Manager.Module,RSRDataP^.UserID,RSRDataP^.Cookie);
    end;
  end;
begin
  // This script will take
  // srscripts/results.srs?list?provider?searchterm?maxresults?firstreturn?  and have us issue query to provider... And populate content and send it back...
  // srscripts/results.srs?delete?provider?resultID?
  FManager:=Manager;
  RSRP:=@SR;
  RSRDataP:=SR.Info.DataP;
  sQueryString:=''; sOutPut:='';
  Core.Arrays.VarString.fromString(saQuery,Request.Query,'?');
  saLength:=Length(saQuery);
  If (saLength>=3) then begin
    ProviderP:=uStorage.Providers_Find(saQuery[1]);
    If (ProviderP<>Nil) then begin
      ProviderMan:=TProviderManager(uStorage.Providers_Find(ProviderP));
      If ProviderMan<>Nil then begin
        If Resource<>Nil then
          sPage:=Resource.toString;
        if sPage<>'' then begin
          //  Identify Query...
          //  If Results... Return them,
          //  else Pass through...
          if (saQuery[0]='search') then begin
            sPage:=ProviderP^.LP_WEB;
            sPage:=StringReplace(sPage,'{$i search_term}',saQuery[2],[rfReplaceAll]);
            SendContentAsHTML(RSRP,sPage);
            Response.Close:=False;
          end else If (saQuery[0]='list') then begin
            If (RSRDataP<>Nil) then begin
              CheckCookies;
              If (RSRDataP^.UserID<>0) then begin
                If Length(saQuery)>=4 then begin
                  Response.Close:=False;
                           // SubmitQuery(aBindIP:Int64; aBindPort:Word; sQuery:String; EUID:int64; SenderP:PRSR; MaxResults,FirstReturnOn:Integer; Var OutputFormat:String);
                  ProviderMan.SubmitQuery(Server.Address.sin_addr.S_addr,Server.Port,saQuery[2],RSRDataP^.UserID,@SR,StrToIntDef(saQuery[3],0),StrToIntDef(saQuery[4],0),sPage);
                end else begin
                  sPage:=Format('Command %s is not recognized with these parameters.<br>File: %s',[saQuery[0],Request.uri]);
                  sTitle:='Invalid Request.';
                  SendRequestNotFound(@SR,sTitle,sPage);
                end;
              end else begin
                sPage:=Format('Command %s cannot be performed until you enable cookies.<br>File: %s',[saQuery[0],Request.uri]);
                SendRequestNotFound(@SR,'Please submit credentials.',sPage);
              end;
            end else begin
              sPage:=Format('Command %s cannot be performed until you enable cookies.<br>File: %s',[saQuery[0],Request.uri]);
              SendRequestNotFound(@SR,'Please submit credentials.',sPage);
            end;
          end else if (saQuery[0]='more') then begin
            If (RSRDataP<>Nil) then begin
              CheckCookies;
              If (RSRDataP^.UserID<>0) then begin
                If (saLength=7) then begin
                  //                           0        1           2              3             4      5        6
                  // "/_srscripts/results.srs?more?#provider#?#max_results#?#current_results#?#euid#?#quid#?#streamid#?";
                  Response.Close:=False;
                  ProviderMan.ObtainMoreResults(@SR,StrToIntDef(saQuery[6],0),StrToIntDef(saQuery[3],0),StrToIntDef(saQuery[5],0),StrToIntDef(saQuery[4],0));
                  //ProviderMan.RefreshQuery(Server.Address.sin_addr.S_addr,saQuery[4],@SR,StrToIntDef(saQuery[2],0),StrToIntDef(saQuery[1],0),sPage);
                end else begin
                  sPage:=Format('Command %s encountered an error. %s parameters received is not acceptable.',[saQuery[0],IntToStr(saLength)]);
                  sTitle:='Command not understood.  Expecting 7 Parameters.';
                  SendRequestNotFound(@SR,sTitle,sPage);
                end;
              end else begin
                sPage:=Format('Command %s cannot be performed until you enable cookies.<br>File: %s',[saQuery[0],Request.uri]);
                SendRequestNotFound(@SR,'Please submit credentials.',sPage);
              end;
            end else begin
              sPage:=Format('Command %s cannot be performed until you enable cookies.<br>File: %s',[saQuery[0],Request.uri]);
              SendRequestNotFound(@SR,'Please submit credentials.',sPage);
            end;
          end else if (saQuery[0]='delete') then begin
            // Now we take user's ID and we add the delete interaction in the DB.
            If (RSRDataP<>Nil) then begin
              CheckCookies;
              If (RSRDataP^.UserID<>0) then begin
                resultID:=StrToIntDef(saQuery[2],0);
                If resultID<>0 then begin
                  Empty(bbResultData);
                  Response.Close:=False;
                  PerformInteraction_Delete;
                end else begin
                  sPage:=Format('Command %s cannot be performed because %s is an invalid parameter.<br>File: %s',[saQuery[0],saQuery[1],Request.uri]);
                  SendRequestNotFound(@SR,'Invalid Delete Result Command',sPage);
                end;
              end else begin
                sPage:=Format('Command %s cannot be performed until you enable cookies.<br>File: %s',[saQuery[0],Request.uri]);
                SendRequestNotFound(@SR,'Please submit credentials.',sPage);
              end;
            end else begin
              sPage:=Format('Command %s cannot be performed until you enable cookies.<br>File: %s',[saQuery[0],Request.uri]);
              SendRequestNotFound(@SR,'Result Command Not Enabled',sPage);
            end;
          end else begin
            SendRequestNotFound(@SR,'Result Command Not Understood',Format('Command %s not understood.<br>File: %s<br>',[saQuery[0],Request.uri]));
          end;
        end else begin
          SendRequestNotFound(@SR,'Output File Not Found',Format('File Not Found.<br>File: %s<br>Managers: %s',[Request.uri,IntToStr(Length(ProviderP^.Managers))]));
        end;
      end else begin
        SendRequestNotFound(@SR,'Provider Manager Not Found',Format('Manager Not Found.<br>File: %s<br>Managers: %s',[Request.uri,IntToStr(Length(ProviderP^.Managers))]));
      end;
    end else begin
      SendRequestNotFound(@SR,'Provider Not Found',Format('Search Provider "%s" Not Found.<br>File: %s<br>Providers: %s',[saQuery[0],Request.uri,IntToStr(Length(uStorage.SearchProviders))]));
    end;
  end else begin
    SendRequestNotFound(@SR,'Result Request Not Understood',Format('File: %s<br>',[Request.uri]));
  end;
end;
*)

end.

unit Storage.SrchProviders;


interface

uses
  Classes,

  Core.Database,
  Core.Database.Timer,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Storage,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Pointers,
  Core.Strings,

  SysUtils;

Const

  SRCH_PVDR_FMT_DNS_FAILURE      = 'There are no IPs for provider %s server=%s';

Type
  Items=class
  type
    Item=record
      ID                         : QWord;
      PORT                       : LongInt;
      Scale                      : LongInt;
      ManagerLcv                 : LongInt;
      MaxResults                 : LongInt;
      Caption                    : Core.Strings.VarString;
      Domain                     : Core.Strings.VarString;
      qsWEB                      : Core.Strings.VarString;
      qsNEWS                     : Core.Strings.VarString;
      qsVIDEO                    : Core.Strings.VarString;
      qsIMAGE                    : Core.Strings.VarString;
      lpWEB                      : Core.Strings.VarString;
      lpNEWS                     : Core.Strings.VarString;
      lpVIDEO                    : Core.Strings.VarString;
      lpIMAGE                    : Core.Strings.VarString;
      Managers                   : Core.Arrays.Types.Pointers;
    end;
    PItem=^Item;
    List=Array of Item;
    PList=^List;
    TCache=Class
    private
      FLastProviderP               : PItem;
    public
      function getName(ID:QWord):Core.Strings.VarString;
    end;
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        Port                     : Core.Database.Types.Integer = 2;
        MaxResults               : Core.Database.Types.Integer = 3;
        Scale                    : Core.Database.Types.Integer = 4;
        Kind                     : Core.Database.Types.Integer = 5;
        Caption                  : Core.Database.Types.Integer = 6;
        Domain                   : Core.Database.Types.Integer = 7;
        qsWeb                    : Core.Database.Types.Integer = 8;
        qsNews                   : Core.Database.Types.Integer = 9;
        qsImage                  : Core.Database.Types.Integer = 10;
        qsVideo                  : Core.Database.Types.Integer = 11;
        lpWeb                    : Core.Database.Types.Integer = 12;
        lpImage                  : Core.Database.Types.Integer = 13;
        lpNews                   : Core.Database.Types.Integer = 14;
        lpVideo                  : Core.Database.Types.Integer = 15;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        Port                     : Core.Database.Types.VarString = 'ITMPRT';
        MaxResults               : Core.Database.Types.VarString = 'ITMMRS';
        Scale                    : Core.Database.Types.VarString = 'ITMSLE';
        Kind                     : Core.Database.Types.VarString = 'ITMKND';
        Caption                  : Core.Database.Types.VarString = 'ITMCAP';
        Domain                   : Core.Database.Types.VarString = 'ITMDMN';
        qsWeb                    : Core.Database.Types.VarString = 'QSWEB';
        qsNews                   : Core.Database.Types.VarString = 'QSNWS';
        qsImage                  : Core.Database.Types.VarString = 'QSIMG';
        qsVideo                  : Core.Database.Types.VarString = 'QSVDO';
        lpWeb                    : Core.Database.Types.VarString = 'LPWEB';
        lpImage                  : Core.Database.Types.VarString = 'LPIMG';
        lpNews                   : Core.Database.Types.VarString = 'LPNWS';
        lpVideo                  : Core.Database.Types.VarString = 'LPVDO';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Search';
        Name                     : 'Providers';
        Value                    : 'scs_srch_prov';
        Hint                     : 'Storage for search provider configuration';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields: array [0..15] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Port; KeyP: @Keys.Port; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.MaxResults; KeyP: @Keys.MaxResults; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Scale; KeyP: @Keys.Scale; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Caption; KeyP: @Keys.Caption; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Domain; KeyP: @Keys.Domain; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.qsWeb; KeyP: @Keys.qsWeb; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.qsNews; KeyP: @Keys.qsNews; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.qsImage; KeyP: @Keys.qsImage; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.qsVideo; KeyP: @Keys.qsVideo; DataType: dftString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.lpWeb; KeyP: @Keys.lpWeb; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*65; Flags: cfNone;  ),
        (IDP: @IDs.lpImage; KeyP: @Keys.lpImage; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*65; Flags: cfNone;  ),
        (IDP: @IDs.lpNews; KeyP: @Keys.lpNews; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*65; Flags: cfNone;  ),
        (IDP: @IDs.lpVideo; KeyP: @Keys.lpVideo; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 1024*65; Flags: cfNone;  )
      );
      class Function  Fill(Task:Core.Database.Types.TTask; Var Entries:Items.List):Boolean;
      class Function  Add(Task:Core.Database.Types.TTask; Var ID:QWord; Var Scale,Port,MaxResults:LongInt; Var Caption,Domain,QS_Web,LP_Web,QS_News,LP_News,QS_Video,LP_Video,QS_Image,LP_Image:Core.Strings.VarString):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; Var Data:Items.Item):Boolean;
      class Function  Edit(Task:Core.Database.Types.TTask; Var Data:Items.Item):Boolean;
      class Function  Synchronize(Task:Core.Database.Types.TTask; Var Entries:Items.List):Boolean;
    end;
  const
    Services:Array[0..3] of Core.Strings.VarString =('Web','Image','Video','News');

    class function  IndexOf(ID:QWord; Var Entries:Items.List): LongInt; overload;
    class function  IndexOf(const Service:Core.Strings.VarString; var Entry:Items.Item): LongInt; overload;
    class function  Find(ID:QWord; Var Entries:Items.List):Items.PItem; overload;

    class procedure Empty(Var Entry:Items.Item); overload;
    class procedure Empty(Var Entries:Items.List); overload;

    class procedure Done(Var Entries:Items.List); overload;
    class procedure Done(Var Entry:Items.Item); overload;

    class procedure Copy(Var Source,Destination:Items.Item); overload;
    class procedure Copy(Var Source,Destination:Items.List); overload;

    class Function  Find(Var Provider:Core.Strings.VarString):Items.PItem; overload;
    class Function  Find(ProviderP:Items.PItem):Pointer; overload;
    class Function  GetQueryString(const Service:Core.Strings.VarString; var Entry:Items.Item):Core.Strings.VarString;
    class procedure SetQueryString(const Service:Core.Strings.VarString; Query:Core.Strings.VarString; var Entry:Items.Item);
    class procedure SetLandingPage(const Service:Core.Strings.VarString; Page:Core.Strings.VarString; var Entry:Items.Item);
    class Function  GetLandingPage(const Service:Core.Strings.VarString; var Entry:Items.Item):Core.Strings.VarString;

  end;


var

  List : Items.List;

implementation
uses db;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;

  procedure PushProviderDeleted;
  begin
    if ItemP=Items.DB.MonitorP then begin
      with Items.DB do begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
          Result:=Core.Database.SQL.Delete(Task,@Commands);
        Finally
          Core.Database.Done(Commands);
        End;
      end;
    end;
  end;
begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.PROVIDER_DELETED : PushProviderDeleted();
  end;
end;

procedure RegisterDBM;
var
  iLcv                           : LongInt;
begin
  With Items.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
    end;
    if MonitorP = nil then begin
      New(MonitorP);
      Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyTable, @cbDBMonitorNotified);
      Core.Database.Monitor.Add(MonitorP);
    end;
  end;
end;

class Function Items.IndexOf(ID:QWord; Var Entries:Items.List): LongInt;
var
  iCt,iLcv:LongInt;
begin
  iCt:=Length(Entries);
  iLcv:=0; Result:=-1;
  While (Result=-1) and (iLcv<iCt) do begin
    If (Entries[iLcv].ID=ID) then Result:=iLcv;
    Inc(iLcv);
  end;
end;

class function  Items.IndexOf(const Service:Core.Strings.VarString; var Entry:Items.Item): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1; iLcv:=0; iCount:=Length(Services);
  While (iLcv<iCount) and (Result=-1) do begin
    if Sysutils.SameText(Services[iLcv],Service) then begin
      Result:=iLcv;
    end;
    iLcv+=1;
  end;
end;

class function  Items.Find(ID:QWord; Var Entries:Items.List):Items.PItem;
var
  iIndex:LongInt;
begin
  Result:=nil;
  iIndex:=IndexOf(ID,Entries);
  if iIndex<>-1 then
    Result:=@Entries[iIndex];
end;

class procedure Items.Copy(Var Source,Destination:Items.List);
var
  iLcv:LongInt;
begin
  Empty(Destination);
  SetLength(Destination,Length(Source));
  For iLcv:=0 to High(Source) do
    Copy(Source[iLcv],Destination[iLcv]);
end;

class procedure Items.Copy(Var Source,Destination:Items.Item);
begin
  Destination.ID:=Source.ID;
  Destination.MaxResults:=Source.MaxResults;
  Destination.ManagerLcv:=Source.ManagerLcv;
  Destination.Port:=Source.Port;
  Destination.lpWeb:=Source.lpWeb;
  Destination.lpNews:=Source.lpNews;
  Destination.lpVideo:=Source.lpVideo;
  Destination.lpImage:=Source.lpVideo;
  Destination.qsWeb:=Source.qsWeb;
  Destination.qsNews:=Source.qsNews;
  Destination.qsVideo:=Source.qsVideo;
  Destination.qsImage:=Source.qsImage;
  Destination.Caption:=Source.Caption;
  Destination.Domain:=Source.Domain;
  Core.Arrays.Pointers.Copy(Source.Managers,Destination.Managers);
end;

class procedure Items.Empty(Var Entry:Items.Item);
begin
  with Entry do begin
    ID:=0;
    MaxResults:=1000;
    ManagerLcv:=0;
    Port:=0;
    Core.Strings.Empty(lpWeb);
    Core.Strings.Empty(lpNews);
    Core.Strings.Empty(lpVideo);
    Core.Strings.Empty(lpImage);
    Core.Strings.Empty(qsWeb);
    Core.Strings.Empty(qsNews);
    Core.Strings.Empty(qsVideo);
    Core.Strings.Empty(qsImage);
    Core.Strings.Empty(Caption);
    Core.Strings.Empty(Domain);
    Core.Arrays.Pointers.Empty(Managers);
  end;
end;

class procedure Items.Empty(Var Entries:Items.List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Empty(Entries[iLcv]);
  SetLength(Entries,0);
end;

class procedure Items.Done(Var Entries:Items.List);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Entries) do
    Done(Entries[iLcv]);
  Finalize(Entries);
end;

class procedure Items.Done(Var Entry:Items.Item);
begin
  With Entry do begin
    Finalize(Caption);
    Finalize(Domain);
    Finalize(qsWeb);
    Finalize(qsNews);
    Finalize(qsVideo);
    Finalize(qsImage);
    Finalize(lpWeb);
    Finalize(lpNews);
    Finalize(lpVideo);
    Finalize(lpImage);
    Finalize(Managers);
  end;
  Finalize(Entry);
end;

procedure CB_Providers_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iIndex:LongInt;
  iID:QWord;
  iCount:LongInt;
  ListP:Items.PList;
begin
  iID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  ListP:=DataP;
  iIndex:=Items.IndexOf(iID,ListP^);
  If iIndex=-1 then begin
    iCount:=Length(ListP^);
    SetLength(ListP^,iCount+1);
    iIndex:=iCount;
    ListP^[iIndex].ID:=iID;
  end;
  ListP^[iIndex].Port:=Fields.FieldByName(Items.DB.Keys.Port).AsInteger;
  ListP^[iIndex].Scale:=Fields.FieldByName(Items.DB.Keys.Scale).AsInteger;
  ListP^[iIndex].MaxResults:=Fields.FieldByName(Items.DB.Keys.MaxResults).AsInteger;
  ListP^[iIndex].Caption:=Fields.FieldByName(Items.DB.Keys.Caption).AsString;
  ListP^[iIndex].Domain:=Fields.FieldByName(Items.DB.Keys.Domain).AsString;
  ListP^[iIndex].qsWeb:=Fields.FieldByName(Items.DB.Keys.qsWeb).AsString;
  ListP^[iIndex].qsNews:=Fields.FieldByName(Items.DB.Keys.qsNews).AsString;
  ListP^[iIndex].qsImage:=Fields.FieldByName(Items.DB.Keys.qsImage).AsString;
  ListP^[iIndex].qsVideo:=Fields.FieldByName(Items.DB.Keys.qsVideo).AsString;
  ListP^[iIndex].lpWeb:=Fields.FieldByName(Items.DB.Keys.lpWeb).AsString;
  ListP^[iIndex].lpNews:=Fields.FieldByName(Items.DB.Keys.lpNews).AsString;
  ListP^[iIndex].lpImage:=Fields.FieldByName(Items.DB.Keys.lpImage).AsString;
  ListP^[iIndex].lpVideo:=Fields.FieldByName(Items.DB.Keys.lpVideo).AsString;
end;

class Function  Items.DB.Fill(Task:Core.Database.Types.TTask; Var Entries:Items.List):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Port,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.MaxResults,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Scale,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Caption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Domain,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsWeb,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsNews,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsImage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsVideo,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpWeb,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpNews,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpImage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpVideo,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Providers_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; Var ID:QWord; Var Scale,Port,MaxResults:LongInt; Var Caption,Domain,QS_Web,LP_Web,QS_News,LP_News,QS_Video,LP_Video,QS_Image,LP_Image:Core.Strings.VarString):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  iInsertID,iReset:QWord;
begin
  Result:=False;
  Try
    iCount:=0; iInsertID:=Random(High(Integer)); iReset:=0; ID:=0;

    Core.Database.AddCommand(iCount,TableP,@Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Port,poNone,oNone,Port,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.MaxResults,poNone,oNone,MaxResults,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Scale,poNone,oNone,Scale,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Caption,poNone,oNone,Caption,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Domain,poNone,oNone,Domain,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.qsWeb,poNone,oNone,QS_Web,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.qsNews,poNone,oNone,QS_News,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.qsImage,poNone,oNone,QS_Image,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.qsVideo,poNone,oNone,QS_Video,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.lpWeb,poNone,oNone,LP_Web,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.lpNews,poNone,oNone,LP_News,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.lpImage,poNone,oNone,LP_Image,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.lpVideo,poNone,oNone,LP_Video,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands) and (ID<>0);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; ID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure CB_Providers_Read(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ProviderP:Items.PItem;
begin
  ProviderP:=DataP;
  {$i Storage.SrchProviders.ProviderP.Assignments.inc}
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; Var Data:Items.Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Data.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Port,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.MaxResults,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Scale,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Caption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Domain,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsWeb,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsNews,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsImage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsVideo,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpWeb,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpNews,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpImage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpVideo,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Providers_Read,@Data);
  Finally
    Core.Database.Done(Commands);
  end;
end;


procedure CB_Providers_Synchronize(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ListP:Items.PList;
  ProviderP:Items.PItem;
  ID:QWord;
  iIndex:LongInt;
begin
  ListP:=DataP;
  ID:=Fields.FieldByName(Items.DB.Keys.ID).AsLargeInt;
  iIndex:=Items.IndexOf(ID,ListP^);
  if iIndex=-1  then begin
    iIndex:=Length(ListP^);
    SetLength(ListP^,iIndex+1);
    ListP^[iIndex].ID:=ID;
  end;
  ProviderP:=@ListP^[iIndex];
  {$ Storage.SearchProviderDB.ProviderP.Assignments.inc}
end;

class Function  Items.DB.Synchronize(Task:Core.Database.Types.TTask; Var Entries:Items.List):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Port,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.MaxResults,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Scale,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Caption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Domain,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsWeb,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsNews,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsImage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.qsVideo,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpWeb,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpNews,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpImage,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.lpVideo,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Providers_Synchronize,@Entries);
  Finally
    Core.Database.Done(Commands);
  end;
end;


class Function  Items.DB.Edit(Task:Core.Database.Types.TTask; Var Data:Items.Item):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Data.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Port,poNone,oNone,Data.Port,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.MaxResults,poNone,oNone,Data.MaxResults,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Scale,poNone,oNone,Data.Scale,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Caption,poNone,oNone,Data.Caption,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Domain,poNone,oNone,Data.Domain,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.lpWeb,poNone,oNone,Data.lpWeb,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.lpNews,poNone,oNone,Data.lpNews,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.lpImage,poNone,oNone,Data.lpImage,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.lpVideo,poNone,oNone,Data.lpVideo,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.qsWeb,poNone,oNone,Data.qsWeb,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.qsNews,poNone,oNone,Data.qsNews,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.qsImage,poNone,oNone,Data.qsImage,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.qsVideo,poNone,oNone,Data.qsVideo,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  end;
end;

class Function Items.Find(Var Provider:Core.Strings.VarString):Items.PItem;
var
  iLcv:LongInt;
begin
  Result:=Nil; iLcv:=0;
  While (iLcv<Length(Storage.SrchProviders.List)) and (Result=Nil) do begin
    If Storage.SrchProviders.List[iLcv].Caption=Provider then
      Result:=@Storage.SrchProviders.List[iLcv];
    Inc(iLcv);
  end;
end;

class Function  Items.Find(ProviderP:Items.PItem):Pointer;
var
  iCount,iLcv:LongInt;
begin
  Result:=Nil;
  iLcv:=ProviderP^.ManagerLcv;  iCount:=Length(ProviderP^.Managers);
  If iLcv>=iCount then begin
    iLcv:=0;
    ProviderP^.ManagerLcv:=0;
  end;
  If (iLcv<iCount) then begin
    Inc(ProviderP^.ManagerLcv);
    Result:=ProviderP^.Managers[iLcv];
  end;
end;

class Function  Items.GetQueryString(const Service:Core.Strings.VarString; Var Entry:Items.Item):Core.Strings.VarString;
var
  iIndex:LongInt;
begin
  SetLength(Result,0);
  iIndex:=IndexOf(Service,Entry);
  case iIndex of
    0: Result:=Entry.qsWeb;
    1: Result:=Entry.qsImage;
    2: Result:=Entry.qsVideo;
    3: Result:=Entry.qsNews;
  end;
end;

class procedure Items.SetQueryString(const Service:Core.Strings.VarString; Query:Core.Strings.VarString; var Entry:Items.Item);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(Service,Entry);
  case iIndex of
    0 : Entry.qsWeb:=Query;
    1 : Entry.qsImage:=Query;
    2 : Entry.qsVideo:=Query;
    3 : Entry.qsNews:=Query;
  end;
end;

class procedure Items.SetLandingPage(const Service:Core.Strings.VarString; Page:Core.Strings.VarString; var Entry:Items.Item);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(Service,Entry);
  case iIndex of
    0 : Entry.lpWeb:=Page;
    1 : Entry.lpImage:=Page;
    2 : Entry.lpVideo:=Page;
    3 : Entry.lpNews:=Page;
  end;
end;

class Function  Items.GetLandingPage(const Service:Core.Strings.VarString; var Entry:Items.Item):Core.Strings.VarString;
var
  iIndex:LongInt;
begin
  SetLength(Result,0);
  iIndex:=IndexOf(Service,Entry);
  case iIndex of
    0 : Result:=Entry.lpWeb;
    1 : Result:=Entry.lpImage;
    2 : Result:=Entry.lpVideo;
    3 : Result:=Entry.lpNews;
  end;
end;

function Items.TCache.GetName(ID:QWord):Core.Strings.VarString;
var
  iIndex:LongInt;
begin
  SetLength(Result,0);
  if (FLastProviderP=nil) then begin
    iIndex:=IndexOf(ID,Storage.SrchProviders.List);
    if iIndex<>-1 then
      FLastProviderP:=@Storage.SrchProviders.List[iIndex];
  end;
  if (FLastProviderP<>nil) and (FLastProviderP^.ID<>ID) then begin
    iIndex:=IndexOf(ID,Storage.SrchProviders.List);
    if iIndex<>-1 then
      FLastProviderP:=@Storage.SrchProviders.List[iIndex];
  end;
  if (FLastProviderP<>nil) and (FLastProviderP^.ID=ID) then
    Result:=FLastProviderP^.Caption;
end;


initialization
  RegisterDBM;

end.


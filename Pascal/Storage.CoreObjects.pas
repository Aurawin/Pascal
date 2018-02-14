{
 unit Storage.CoreObjects.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}

unit Storage.CoreObjects;

interface

uses

  RSR,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,


  Core.Utils.Files,
  Core.Strings,
  Core.Keywords,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.KeyString,

  Storage.Types,
  Storage.FAT,
  Storage.UserAccounts,
  Storage.Domains,
  Storage.MatrixNodes,

  classes;

Const
    CP_NAMESPACE                 : Core.Database.Types.Byte = 0;
    CP_COMMAND                   : Core.Database.Types.Byte = 1;
    CP_OPTIONS                   : Core.Database.Types.Byte = 2;
    CO_DEFAULT_PAGE              : Core.Strings.VarString   = '{$i core_page_output}';
    FMT_INVALID_CLASS            : Core.Strings.VarString   = 'Class Mismatch Exception : %s is not of type %s';


Type
  Item=class
  Type
    DB=class
    Type
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'COID';
        InsertID                 : Core.Database.Types.VarString = 'COIID';
        Enabled                  : Core.Database.Types.VarString = 'COENAB';
        Name                     : Core.Database.Types.VarString = 'CONAME';
        NameSpace                : Core.Database.Types.VarString = 'CONASP';
        &Class                   : Core.Database.Types.VarString = 'COCLAS';
        Title                    : Core.Database.Types.VarString = 'COTITL';
        ACLPrompt                : Core.Database.Types.VarString = 'COPMPT';
        Description              : Core.Database.Types.VarString = 'CODESC';
      end;

      IDS=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        Enabled                  : Core.Database.Types.Integer = 2;
        Name                     : Core.Database.Types.Integer = 3;
        NameSpace                : Core.Database.Types.Integer = 4;
        &Class                   : Core.Database.Types.Integer = 5;
        Title                    : Core.Database.Types.Integer = 6;
        ACLPrompt                : Core.Database.Types.Integer = 7;
        Description              : Core.Database.Types.Integer = 8;
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'System/Core';
        Name                 : 'Objects';
        Value                : 'scs_cobjs';
        Hint                 : 'Storage for Core Object definitions';
        PrimaryKeyP          : @Keys.ID;
      );
      Fields: array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Enabled; KeyP: @Keys.Enabled; DataType: dftBoolean; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Name; KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.NameSpace; KeyP: @Keys.NameSpace; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.&Class; KeyP: @Keys.&Class; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Title; KeyP: @Keys.Title; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.ACLPrompt; KeyP: @Keys.ACLPrompt; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
  end;
  Command=class
  Type
    DB=class
    Type
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'COCID';
        InsertID                 : Core.Database.Types.VarString = 'COCIID';
        ObjectID                 : Core.Database.Types.VarString = 'COCOID';
        NameSpace                : Core.Database.Types.VarString = 'COCNS';
        ACLPrompt                : Core.Database.Types.VarString = 'COCPMT';
        Description              : Core.Database.Types.VarString = 'COCDES';
      end;
      IDS=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        ObjectID                 : Core.Database.Types.Integer = 2;
        NameSpace                : Core.Database.Types.Integer = 3;
        ACLPrompt                : Core.Database.Types.Integer = 4;
        Description              : Core.Database.Types.Integer = 5;
      end;
    const
      TableP   : Core.Database.Types.PTable = nil;
      MonitorP : Core.Database.Monitor.Types.PItem = nil;
      Startup  : Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'System/Core';
        Name                 : 'Commands';
        Value                : 'scs_ccmds';
        Hint                 : 'Storage for Core Command definitions';
        PrimaryKeyP          : @Keys.ID;
      );
      Fields: array [0..5] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ObjectID; KeyP: @Keys.ObjectID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.NameSpace; KeyP: @Keys.NameSpace; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ACLPrompt; KeyP: @Keys.ACLPrompt; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Description; KeyP: @Keys.Description; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );

    end;

  end;


  PCoreCommand=^TCoreCommand;
  PCoreCommands=^TCoreCommands;
  PCoreObjectInfo=^TCoreObjectInfo;
  TCoreObjectItems=Array of PCoreObjectInfo;
  PCoreListInfo=^TCoreListInfo;
  TCoreMethod=function(CommandP:PCoreCommand; var SR:TRSR; var Parameters:Core.Arrays.Types.VarString; var srcHeaders,respHeaders:Core.Arrays.Types.KeyStrings; Data:TMemoryStream; var Handled:Boolean):WORD of Object;
  PACLInfo=^TACLInfo;
  PCLSInfo=^TCLSInfo;
  PXMLInfo=^TXMLInfo;
  PCoreData=^TCoreData;
  TCoreDevice=record
    ID                           : QWord;
    devMem                       : QWord;
    srvMem                       : QWord;
  end;
  TCoreData=record
    Device                       : TCoreDevice;
  end;
  TCLSInfo=record
    Name                         : Core.Strings.VarString;
    Location                     : Core.Strings.VarString;
  end;
  TXMLInfo=record
    Enabled                      : boolean;
  end;
  TACLInfo=record
    Name                         : Core.Strings.VarString;
    NameSpace                    : Core.Strings.VarString;
    Caption                      : Core.Strings.VarString;
    Prompt                       : Core.Strings.VarString;
    Description                  : Core.Strings.VarString;
  end;
  TCoreCommand=record
    HeaderP                      : PCoreObjectInfo;
    ID                           : QWord;
    Enabled                      : boolean;
    Anonymous                    : boolean;
    Cache                        : boolean;
    Compress                     : boolean;
    Secure                       : boolean;
    XMLInfo                      : PXMLInfo;
    ACLInfo                      : PACLInfo;
    Method                       : TCoreMethod;
    Resource                     : TDSFile;
  end;
  TCoreCommands=Array of PCoreCommand;

  TCoreObjectInfo=record
    ID                           : QWord;
    ProviderID                   : QWord;
    Enabled                      : Boolean;
    Anonymous                    : Boolean;
    Scale                        : Byte;
    CLSInfo                      : PCLSInfo;
    ACLInfo                      : PACLInfo;
  end;
  TCoreListInfo=record
    Loaded                       : Boolean;
    Server                       : TRSRServer;
    Manager                      : TRSRManager;
    Accounts                     : Storage.UserAccounts.Items.TList;
    Fat                          : TDSFat;
    DomainP                      : Storage.Domains.Items.PDomain;
    NodeP                        : Storage.MatrixNodes.Node.PItem;
    RootP                        : Storage.UserAccounts.Items.PItem;
    DefaultP                     : Storage.UserAccounts.Items.PItem;
    KeywordsP                    : PKeywords;
    LastError                    : cardinal;
  end;
  TProcCoreObjectNotFound=procedure(var SR:TRSR; NameSpace:Core.Strings.VarString) of object;
  TProcCoreObject=procedure(CommandP:PCoreCommand; var RSR:TRSR) of Object;
  TProcCoreObjectError=procedure(CommandP:PCoreCommand; var SR:TRSR; Error:WORD) of Object;
  TProcCoreObjectRedirect=procedure(CommandP:PCoreCommand; var SR:TRSR; URL:Core.Strings.VarString) of Object;
  TProcCoreObjectMessage=procedure(CommandP:PCoreCommand; var SR:TRSR; sTitle,sPrompt:Core.Strings.VarString) of Object;

  TCoreObjectProperty=(coID,coInsertID,coEnabled,coName,coNameSpace,coClass,coTitle,coACLPrompt,coDescription);
  TCoreCommandProperty=(ccID,ccInsertID,ccObjectID,ccNameSpace,ccACLPrompt,ccDescription);

  TCoreParams=Array[0..2] of Core.Strings.VarString;


procedure Empty(Var Item:TCoreParams); overload;
procedure Done(Var Item:TCoreParams); overload;
procedure fromString(Var Item:Core.Strings.VarString; out Params:TCoreParams); overload;


procedure Init(Var Item:TACLInfo); overload;
procedure Empty(Var Item:TACLInfo); overload;
procedure Done(Var Item:TACLInfo); overload;

procedure Init(Var Item:TCoreListInfo); overload;
procedure Empty(Var Item:TCoreListInfo); overload;
procedure Done(Var Item:TCoreListInfo); overload;

procedure Init(Var Item:TCoreObjectInfo); overload;
procedure Copy(Var Source,Destination:TCoreObjectInfo); overload;
procedure Empty(Var Item:TCoreObjectInfo); overload;
procedure Done(Var Item:TCoreObjectInfo); overload;

procedure Init(var Item:TCoreObjectItems); overload;
procedure Empty(var Item:TCoreObjectItems); overload;
procedure Done(var Item:TCoreObjectItems); overload;
procedure Add(Var Item:TCoreObjectInfo; var List:TCoreObjectItems); overload;

procedure Init(Var Item:TCoreCommand; Header:PCoreObjectInfo); overload;
procedure Copy(Var Source,Destination:TCoreCommand); overload;
procedure Empty(Var Item:TCoreCommand); overload;
procedure Done(Var Item:TCoreCommand); overload;

procedure Init(var Item:TCoreData); overload;
procedure Empty(var Item:TCoreData); overload;
procedure Done(var Item:TCoreData); overload;

procedure Init(var Item:TCoreDevice); overload;
procedure Empty(var Item:TCoreDevice); overload;
procedure Done(var Item:TCoreDevice); overload;

{
procedure Init(Var Item:TCoreCommandInfo); overload;
procedure Copy(Var Source,Destination:TCoreCommandInfo); overload;
procedure Empty(Var Item:TCoreCommandInfo); overload;
procedure Done(Var Item:TCoreCommandInfo); overload;
}
procedure Init(var Item:TCoreCommands); overload;
procedure Empty(var Item:TCoreCommands); overload;
procedure Done(var Item:TCoreCommands); overload;
procedure Add(var Item:TCoreCommand; var List:TCoreCommands; var Header:TCoreObjectInfo; Method:TCoreMethod); overload;

function  IndexOf(ID:QWord; var List:TCoreCommands): LongInt; overload;
procedure SetMethod(Var Item:TCoreCommand; Method:TCoreMethod; var List:TCoreCommands); overload;

function  Granted(var ccItem:TCoreCommand; UserP:Storage.UserAccounts.Items.PItem):Boolean; overload;
function  Granted(cmdP:PCoreCommand; UserP:Storage.UserAccounts.Items.PItem):Boolean; overload;
function  Granted(var coItem:TCoreObjectInfo; UserP:Storage.UserAccounts.Items.PItem):Boolean; overload;

procedure Grant(var ccItem:TCoreCommand; UserP:Storage.UserAccounts.Items.PItem); overload;
procedure Grant(var coItem:TCoreObjectInfo; UserP:Storage.UserAccounts.Items.PItem); overload;
procedure Grant(var List:TCoreObjectItems; UserP:Storage.UserAccounts.Items.PItem); overload;
procedure Deny(var ccItem:TCoreCommand; UserP:Storage.UserAccounts.Items.PItem); overload;
procedure Deny(var coItem:TCoreObjectInfo; UserP:Storage.UserAccounts.Items.PItem); overload;



Function  COREOBJECT_Add(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean; overload;
Function  COREOBJECT_Add(Task:Core.Database.Types.TTask; var Command:TCoreCommand):Boolean; overload;

Function  COREOBJECT_Delete(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
Function  COREOBJECT_Set(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
Function  COREOBJECT_Get(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
Function  COREOBJECT_Exists(Task:Core.Database.Types.TTask; Name:Core.Strings.VarString):Boolean;
Function  COREOBJECT_List(Task:Core.Database.Types.TTask; Var List:Core.Arrays.Types.VarString):Boolean;
Function  COREOBJECT_ID(Task:Core.Database.Types.TTask; NameSpace:Core.Strings.VarString; out ID:QWord):Boolean; overload;
Function  COREOBJECT_ID(Task:Core.Database.Types.TTask; ObjectID:QWord; NameSpace:Core.Strings.VarString; out ID:QWord):Boolean; overload;

Function  COREOBJECT_VerifyIDs(Task:Core.Database.Types.TTask; Var List:TCoreObjectItems):Boolean; overload;
Function  COREOBJECT_VerifyID(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean; overload;
Function  COREOBJECT_VerifyID(Task:Core.Database.Types.TTask; var Item:TCoreCommand):Boolean; overload;


implementation
uses
  RSR.Core,
  db;

procedure cbDestroyCoreObjects(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Storage.CoreObjects.Item.DB do begin
    Core.Database.Done(TableP^);
    TableP:=nil;
    MonitorP:=nil;
  end;
end;

procedure cbDestroyCoreCommands(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Storage.CoreObjects.Command.DB do begin
    Core.Database.Done(TableP^);
    TableP:=nil;
    MonitorP:=nil;
  end;
end;

procedure RegisterDB;
var
  iLcv                         : LongInt;
begin
  with Item.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyCoreObjects, Core.Database.Monitor.Notify.None);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Command.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyCoreCommands, Core.Database.Monitor.Notify.None);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

Function  COREOBJECT_Add(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Item.ID:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coInsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForCriteria,Integer(coInsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForPrimaryID,Integer(coID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForResetInsertID,Integer(coInsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coNameSpace),poNone,oNone,Item.ACLInfo^.NameSpace,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coEnabled),poNone,oNone,Item.Enabled,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coName),poNone,oNone,Item.ACLInfo^.Name,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coClass),poNone,oNone,Item.CLSInfo^.Name,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coTitle),poNone,oNone,Item.ACLInfo^.Caption,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForInsert,Integer(coDescription),poNone,oNone,Item.ACLInfo^.Description,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function  COREOBJECT_Add(Task:Core.Database.Types.TTask; var Command:TCoreCommand):Boolean;
var
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; Command.ID:=0; iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForInsert,Integer(ccInsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForCriteria,Integer(ccInsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForPrimaryID,Integer(ccID),poNone,oNone,Command.ID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForResetInsertID,Integer(ccInsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForInsert,Integer(ccObjectID),poNone,oNone,Command.HeaderP^.ID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForInsert,Integer(ccNameSpace),poNone,oNone,Command.ACLInfo^.NameSpace,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForInsert,Integer(ccACLPrompt),poNone,oNone,Command.ACLInfo^.Prompt,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForInsert,Integer(ccDescription),poNone,oNone,Command.ACLInfo^.Description,Commands);
    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function  COREOBJECT_Delete(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForCriteria,Integer(coID),poNone,oEqual,Item.ID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function  COREOBJECT_Set(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForCriteria,Integer(coID),poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForValues,Integer(coEnabled),poNone,oNone,Item.Enabled,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForValues,Integer(coName),poNone,oNone,Item.ACLInfo^.Name,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForValues,Integer(coClass),poNone,oNone,Item.CLSInfo^.Name,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForValues,Integer(coTitle),poNone,oNone,Item.ACLInfo^.Caption,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForValues,Integer(coDescription),poNone,oNone,Item.ACLInfo^.Description,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function  COREOBJECT_Get(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
begin
  Result:=false;
end;

Function  COREOBJECT_Exists(Task:Core.Database.Types.TTask; Name:Core.Strings.VarString):Boolean;
begin
   Result:=false;
end;

Function  COREOBJECT_List(Task:Core.Database.Types.TTask; Var List:Core.Arrays.Types.VarString):Boolean;
begin
  Result:=false;
end;

procedure CB_COREOBJECT_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iIDP:PQWord;
begin
  iIDP:=DataP;
  iIDP^:=Fields.FieldByName(Item.DB.Keys.ID).AsLargeInt;
end;

procedure CB_COREOBJECT_COMMAND_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  iIDP:PQWord;
begin
  iIDP:=DataP;
  iIDP^:=Fields.FieldByName(Command.DB.Keys.ID).AsLargeInt;
end;


Function  COREOBJECT_ID(Task:Core.Database.Types.TTask; NameSpace:Core.Strings.VarString; out ID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForCriteria,Integer(coNameSpace),poNone,oEqual,NameSpace,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Item.DB.TableP,useForFields,Integer(coID),poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_COREOBJECT_ID,@ID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function  COREOBJECT_ID(Task:Core.Database.Types.TTask; ObjectID:QWord; NameSpace:Core.Strings.VarString; out ID:QWord):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForCriteria,Integer(ccObjectID),poNone,oEqual,ObjectID,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForCriteria,Integer(ccNameSpace),poAnd,oEqual,NameSpace,Commands);
    Core.Database.AddCommand(iCount,Storage.CoreObjects.Command.DB.TableP,useForFields,Integer(ccID),poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_COREOBJECT_COMMAND_ID,@ID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function  COREOBJECT_VerifyIDs(Task:Core.Database.Types.TTask; Var List:TCoreObjectItems):Boolean;
var
  iLcv:LongInt;
begin
  Result:=True;
  for iLcv:=0 to High(List) do
    COREOBJECT_VerifyID(Task,List[iLcv]^);
end;

Function  COREOBJECT_VerifyID(Task:Core.Database.Types.TTask; var Item:TCoreCommand):Boolean; overload;
begin
  Result:=false;
  if Item.ID=0 then begin
    if COREOBJECT_ID(Task,Item.HeaderP^.ID,Item.ACLInfo^.NameSpace,Item.ID) then begin
      if Item.ID=0 then
        Result:=COREOBJECT_Add(Task,Item);
    end;
  end;
end;

Function  COREOBJECT_VerifyID(Task:Core.Database.Types.TTask; Var Item:TCoreObjectInfo):Boolean;
begin
  Result:=false;
  If Item.ID=0 then begin
    if COREOBJECT_ID(Task,Item.ACLInfo^.NameSpace,Item.ID) then begin
      if Item.ID=0 then // Add Header to Database and Obtain new ID
        Result:=COREOBJECT_Add(Task,Item);
    end;
  end;
end;

procedure Init(Var Item:TCoreObjectInfo);
begin
  With Item do begin
    ID:=0;
    Enabled:=false;
    Anonymous:=false;
    Scale:=0;
    CLSInfo:=nil;
    ACLInfo:=nil;
  end;
end;

procedure Empty(Var Item:TCoreObjectInfo);
begin
  With Item do begin
    ID:=0;
    Enabled:=false;
    Anonymous:=false;
    Scale:=0;
    CLSInfo:=nil;
    ACLInfo:=nil;
  end;
end;

procedure Done(Var Item:TCoreObjectInfo);
begin
  With Item do begin
    CLSInfo:=nil;
    ACLInfo:=nil;
  end;
  Finalize(Item);
end;

procedure Copy(Var Source,Destination:TCoreObjectInfo);
begin
  Destination.ID:=Source.ID;
  Destination.Enabled:=Source.Enabled;
  Destination.Anonymous:=Source.Anonymous;
  Destination.Scale:=Source.Scale;

  Destination.CLSInfo:=Source.CLSInfo;
  Destination.ACLInfo:=Source.ACLInfo;
end;
{
procedure Init(Var Item:TCoreCommandInfo);
begin
  Item.Enabled:=true;
  Item.Method:=nil;
  Item.Resource:=nil;
end;

procedure Copy(Var Source,Destination:TCoreCommandInfo);
begin
  Destination.Resource:=Source.Resource;
  Destination.Method:=Source.Method;
  Destination.Enabled:=Source.Enabled;
end;

procedure Empty(Var Item:TCoreCommandInfo);
begin
  Item.Enabled:=false;
  Item.Method:=nil;
  Item.Resource:=nil;
end;

procedure Done(Var Item:TCoreCommandInfo);
begin
  Finalize(Item);
end;
}
procedure Init(var Item:TCoreObjectItems);
begin
  SetLength(Item,0);
end;

procedure Empty(var Item:TCoreObjectItems);
begin
  SetLength(Item,0);
end;

procedure Done(var Item:TCoreObjectItems);
begin
  Finalize(Item);
end;

procedure Add(Var Item:TCoreObjectInfo; var List:TCoreObjectItems);
var
  iLen:LongInt;
begin
  iLen:=System.Length(List);
  SetLength(List,iLen+1);
  List[iLen]:=@Item;
end;

procedure Init(Var Item:TCoreCommand; Header:PCoreObjectInfo);
begin
  With Item do begin
    Method:=nil;
    Resource:=nil;
    HeaderP:=Header;
    ID:=0;
    Enabled:=false;
    Anonymous:=false;
    Cache:=false;
    Compress:=true;
    Secure:=false;
    ACLInfo:=nil;
    XMLInfo:=nil;
  end;
end;

procedure Copy(Var Source,Destination:TCoreCommand);
begin
  Empty(Destination);
  Destination.HeaderP:=Source.HeaderP;
  Destination.ACLInfo:=Source.ACLInfo;
  Destination.XMLInfo:=Source.XMLInfo;
  Destination.ID:=Source.ID;
  Destination.Enabled:=Source.Enabled;
  Destination.Anonymous:=Source.Anonymous;
  Destination.Cache:=Source.Cache;
  Destination.Compress:=Source.Compress;
  Destination.Secure:=Source.Secure;
end;

procedure Empty(Var Item:TCoreCommand);
begin
  With Item do begin
    HeaderP:=nil;
    Resource:=nil;
    Method:=nil;
    ID:=0;
    Enabled:=False;
    Anonymous:=False;
    Cache:=false;
    Compress:=true;
    Secure:=false;
    ACLInfo:=nil;
    XMLInfo:=nil;
  end;
end;

procedure Done(Var Item:TCoreCommand);
begin
  Finalize(Item);
end;

procedure Init(var Item:TCoreData);
begin
  Init(Item.Device);
end;

procedure Empty(var Item:TCoreData);
begin
  Empty(Item.Device);
end;

procedure Done(var Item:TCoreData);
begin
  Done(Item.Device);
  Finalize(Item);
end;

procedure Init(var Item:TCoreDevice);
begin
  Item.ID:=0;
  Item.devMem:=1024*1024*8;
  Item.srvMem:=1024*1024*256;
end;

procedure Empty(var Item:TCoreDevice);
begin
  Item.ID:=0;
  Item.devMem:=1024*1024*8;
  Item.srvMem:=1024*1024*256;
end;

procedure Done(var Item:TCoreDevice);
begin
  Finalize(Item);
end;

procedure Add(var Item:TCoreCommand; var List:TCoreCommands; var Header:TCoreObjectInfo; Method:TCoreMethod);
var
  iIndex:LongInt;
  cmdP:PCoreCommand;
begin
  iIndex:=System.Length(List);
  SetLength(List,iIndex+1);
  new(cmdP);
  Init(cmdP^,@Header);
  List[iIndex]:=cmdP;
  Copy(Item,cmdP^);
  cmdP^.Method:=Method;
end;

procedure Init(var Item:TCoreCommands);
begin
  Empty(Item);
end;

procedure Empty(var Item:TCoreCommands);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

procedure Done(var Item:TCoreCommands);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  Finalize(Item);
end;

function  IndexOf(ID:QWord; var List:TCoreCommands): LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(List) do begin
    if List[iLcv]^.ID=ID then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

procedure SetMethod(Var Item:TCoreCommand; Method:TCoreMethod; var List:TCoreCommands);
var
  iIndex:LongInt;
begin
  iIndex:=IndexOf(Item.ID,List);
  if iIndex<>-1 then
    List[iIndex]^.Method:=Method;
end;

function  Granted(var ccItem:TCoreCommand; UserP:Storage.UserAccounts.Items.PItem):Boolean;
var
  iIndex:LongInt;
begin
  Result:=ccItem.Anonymous;
  if UserP<>nil then begin
    iIndex:=Core.Arrays.LargeWord.IndexOf(ccItem.ID,UserP^.aclCoreCommands);
    Result:=Result or (iIndex<>-1);
  end;
end;

function  Granted(cmdP:PCoreCommand; UserP:Storage.UserAccounts.Items.PItem):Boolean;
var
  iIndex:LongInt;
begin
  Result:=cmdP^.Anonymous;
  if UserP<>nil then begin
    iIndex:=Core.Arrays.LargeWord.IndexOf(cmdP^.ID,UserP^.aclCoreCommands);
    Result:=(Result or (iIndex<>-1));
  end;
end;

function  Granted(var coItem:TCoreObjectInfo; UserP:Storage.UserAccounts.Items.PItem):Boolean;
var
  iIndex:LongInt;
begin
  Result:=coItem.Anonymous;
  if UserP<>nil then begin
    iIndex:=Core.Arrays.LargeWord.IndexOf(coItem.ID,UserP^.aclCoreObjects);
    Result:=Result or (iIndex<>-1);
  end;
end;

procedure Grant(var ccItem:TCoreCommand; UserP:Storage.UserAccounts.Items.PItem);
begin
  Core.Arrays.LargeWord.Add(ccItem.ID,UserP^.aclCoreCommands,aoCheckForDuplicates);
end;

procedure Grant(var coItem:TCoreObjectInfo; UserP:Storage.UserAccounts.Items.PItem);
begin
  Core.Arrays.LargeWord.Add(coItem.ID,UserP^.aclCoreObjects,aoCheckForDuplicates);
end;

procedure Grant(var List:TCoreObjectItems; UserP:Storage.UserAccounts.Items.PItem);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(List) do
    Grant(List[iLcv]^,UserP);
end;

procedure Deny(var ccItem:TCoreCommand; UserP:Storage.UserAccounts.Items.PItem);
begin
  Core.Arrays.LargeWord.Remove(ccItem.ID,UserP^.aclCoreCommands);
end;

procedure Deny(var coItem:TCoreObjectInfo; UserP:Storage.UserAccounts.Items.PItem);
begin
  Core.Arrays.LargeWord.Remove(coItem.ID,UserP^.aclCoreObjects);
end;

procedure Init(Var Item:TACLInfo);
begin
  SetLength(Item.Name,0);
  SetLength(Item.Caption,0);
  SetLength(Item.Prompt,0);
  SetLength(Item.Description,0);
  SetLength(Item.NameSpace,0);
end;

procedure Empty(Var Item:TACLInfo);
begin
  SetLength(Item.Name,0);
  SetLength(Item.Caption,0);
  SetLength(Item.Prompt,0);
  SetLength(Item.Description,0);
  SetLength(Item.NameSpace,0);
end;

procedure Done(Var Item:TACLInfo); overload;
begin
  Finalize(Item.Name);
  Finalize(Item.Caption);
  Finalize(Item.Prompt);
  Finalize(Item.Description);
  Finalize(Item.NameSpace);
  Finalize(Item);
end;

procedure Init(Var Item:TCoreListInfo);
begin
  With Item do begin
    Loaded:=false;
    LastError:=0;
    Server:=nil;
    Manager:=nil;
    Accounts:=nil;
    Fat:=nil;
    DomainP:=nil;
    RootP:=nil;
    DefaultP:=nil;
    KeywordsP:=nil;
  end;
end;

procedure Empty(Var Item:TCoreListInfo);
begin
  With Item do begin
    Loaded:=false;
    LastError:=0;
    Server:=nil;
    Manager:=nil;
    Accounts:=nil;
    Fat:=nil;
    DomainP:=nil;
    RootP:=nil;
    DefaultP:=nil;
    KeywordsP:=nil;
  end;
end;

procedure Done(Var Item:TCoreListInfo);
begin
  Finalize(Item);
end;

procedure Empty(Var Item:TCoreParams);
begin
  SetLength(Item[CP_NAMESPACE],0);
  SetLength(Item[CP_COMMAND],0);
  SetLength(Item[CP_OPTIONS],0);
end;

procedure Done(Var Item:TCoreParams);
begin
  Finalize(Item[CP_NAMESPACE]);
  Finalize(Item[CP_COMMAND]);
  Finalize(Item[CP_OPTIONS]);
  Finalize(Item);
end;

procedure fromString(var Item:Core.Strings.VarString; out Params:TCoreParams);
var
  iCommandEnd:LongInt;
  iLength:LongInt;
begin
  //     /core/name/command?option1?option2?etc...
  Empty(Params);
  Params[CP_NAMESPACE]:=Core.Utils.Files.Extract(Item,epoRoot);
  Params[CP_COMMAND]:=Core.Utils.Files.Extract(Item,epoName);
  iCommandEnd:=Pos(Item,'?');
  if iCommandEnd>0 then begin
    iLength:=System.Length(Item);
    Params[CP_OPTIONS]:=System.Copy(Item,iCommandEnd+1,iLength-iCommandEnd-1);
  end;
end;

initialization
  RegisterDB;
end.

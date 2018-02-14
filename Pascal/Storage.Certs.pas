unit Storage.Certs;

{
 unit Storage.Certs.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
}


interface

uses
  Classes,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,
  Core.Database.Monitor.Types,
  Core.Database.Monitor.Notify,
  Core.Database.SQL,

  Core.Arrays.VarString,
  Core.Arrays.Bytes,

  Encryption.SSL,
  Encryption.Base64,
  Storage,

  SysUtils;

Const
  NO_QUOTES                      = false;

Type
  Items=class
  type
    Status=(csOK,csExpired,csMissing);
    DB=class
    type
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        Date                     : Core.Database.Types.Integer = 3;
        Key                      : Core.Database.Types.Integer = 4;
        DerKey                   : Core.Database.Types.Integer = 5;
        Request                  : Core.Database.Types.Integer = 6;
        Level                    : Core.Database.Types.Integer = 7;
        Cert1                    : Core.Database.Types.Integer = 8;
        Cert2                    : Core.Database.Types.Integer = 9;
        Cert3                    : Core.Database.Types.Integer = 10;
        Cert4                    : Core.Database.Types.Integer = 11;
        DerCert1                 : Core.Database.Types.Integer = 12;
        DerCert2                 : Core.Database.Types.Integer = 13;
        DerCert3                 : Core.Database.Types.Integer = 14;
        DerCert4                 : Core.Database.Types.Integer = 15;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        Date                     : Core.Database.Types.VarString = 'ITMDTE';
        Key                      : Core.Database.Types.VarString = 'ITMKEY';
        DerKey                   : Core.Database.Types.VarString = 'ITMDKY';
        Request                  : Core.Database.Types.VarString = 'ITMREQ';
        Level                    : Core.Database.Types.VarString = 'ITMCTL';
        Cert1                    : Core.Database.Types.VarString = 'ITMCR1';
        Cert2                    : Core.Database.Types.VarString = 'ITMCR2';
        Cert3                    : Core.Database.Types.VarString = 'ITMCR3';
        Cert4                    : Core.Database.Types.VarString = 'ITMCR4';
        DerCert1                 : Core.Database.Types.VarString = 'ITMDR1';
        DerCert2                 : Core.Database.Types.VarString = 'ITMDR2';
        DerCert3                 : Core.Database.Types.VarString = 'ITMDR3';
        DerCert4                 : Core.Database.Types.VarString = 'ITMDR4';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Security';
        Name                     : 'Certificates';
        Value                    : 'scs_sec_certs';
        Hint                     : 'Storage for domain certificates';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..15] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Date; KeyP: @Keys.Date; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Key; KeyP: @Keys.Key; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DerKey; KeyP: @Keys.DerKey; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Request; KeyP: @Keys.Request; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Level; KeyP: @Keys.Level; DataType: dftByte; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Cert1; KeyP: @Keys.Cert1; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Cert2; KeyP: @Keys.Cert2; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Cert3; KeyP: @Keys.Cert3; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Cert4; KeyP: @Keys.Cert4; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DerCert1; KeyP: @Keys.DerCert1; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DerCert2; KeyP: @Keys.DerCert2; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DerCert3; KeyP: @Keys.DerCert3; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DerCert4; KeyP: @Keys.DerCert4; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
      class Function  Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TCertData):Boolean;
      class Function  Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):Boolean;
      class Function  Write(Task:Core.Database.Types.TTask; DomainID:QWord; var Item:TCertData):Boolean;
      class Function  Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; Var Item:TCertData):Boolean;
      class Function  List(Task:Core.Database.Types.TTask; Var DomainID:QWord; Var Entries:TCertList):Boolean;
    end;

  const
    KeyStatus:Array[boolean] of Items.Status = (csMissing,csOK);
    RequestStatus:Array[boolean] of Items.Status=(csMissing,csOK);
    CertStatus:Array[boolean] of Items.Status=(csMissing,csOK);
    FMT_CERT_STATUS : Array[Items.Status] of string = (
      'Ok',
      'Expired',
      'Missing'
    );
    DIR_QUOTE:Array[boolean] of string = ('',#34);
  end;

function GetKeyFile(Folder,Domain:string; const useQuotes:boolean=false):string;
function GetKeyFileAsDer(Folder,Domain:string; const useQuotes:boolean=false):string;
function GetRequestFile(Folder,Domain:string; const useQuotes:boolean=false):string;
function GetCertFile(Folder,Domain:string; const Index:LongInt; const useQuotes:boolean=false):string;
function GetCertFileAsDer(Folder,Domain:string; const Index:LongInt; const useQuotes:boolean=false):string;

implementation
uses db;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Items.DB.MonitorP then begin
      with Items.DB do begin
        Try
          iCount:=0;
          Core.Database.AddCommand(iCount,TableP,@Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,ItemID,Commands);
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
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted();
  end;
end;

procedure cbDestroyTable(ItemP:Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure RegisterDBM;
var
  iLcv:LongInt;
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


function GetKeyFile(Folder,Domain:string; const useQuotes:boolean=false):string;
begin
  Result:=Concat(
    Items.DIR_QUOTE[useQuotes],
    Folder,
    SysUtils.PathDelim,
    Domain,
    '.key',
    Items.DIR_QUOTE[useQuotes]
  );
end;

function GetKeyFileAsDer(Folder,Domain:string; const useQuotes:boolean=false):string;
begin
  Result:=Concat(
    Items.DIR_QUOTE[useQuotes],
    Folder,
    SysUtils.PathDelim,
    Domain,
    '.dky',
    Items.DIR_QUOTE[useQuotes]
  );
end;

function GetRequestFile(Folder,Domain:string; const useQuotes:boolean=false):string;
begin
  Result:=Concat(
    Items.DIR_QUOTE[useQuotes],
    Folder,
    SysUtils.PathDelim,
    Domain,
    '.csr',
    Items.DIR_QUOTE[useQuotes]
  );
end;

function GetCertFile(Folder,Domain:string; const Index:LongInt; const useQuotes:boolean=false):string;
begin
  Result:=Concat(
    Items.DIR_QUOTE[useQuotes],
    Folder,
    SysUtils.PathDelim,
    Domain,
    '.',
    IntToStr(Index),
    '.crt',
    Items.DIR_QUOTE[useQuotes]
 );
end;

function GetCertFileAsDer(Folder,Domain:string; const Index:LongInt; const useQuotes:boolean=false):string;
begin
  Result:=Concat(
    Items.DIR_QUOTE[useQuotes],
    Folder,
    SysUtils.PathDelim,
    Domain,
    '.',
    IntToStr(Index),
    '.dct',
    Items.DIR_QUOTE[useQuotes]
  );
end;

class Function  Items.DB.Add(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TCertData):Boolean;
var
  iCount                         : LongInt;
  iReset                         : QWord;
  iInsertID                      : QWord;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Int64));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Date,poNone,oNone,Item.Date,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Key,poNone,oNone,Item.Key,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerKey,poNone,oNone,Item.DerKey,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Request,poNone,oNone,Item.Request,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Level,poNone,oNone,Item.Level,Commands);
    case Item.Level of
      1 : begin
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
      end;
      2 : begin
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert2,poNone,oNone,Item.Certs[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert2,poNone,oNone,Item.DerCerts[1],Commands);
      end;
      3 : begin
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert2,poNone,oNone,Item.Certs[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert2,poNone,oNone,Item.DerCerts[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert3,poNone,oNone,Item.Certs[2],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert3,poNone,oNone,Item.DerCerts[2],Commands);
      end;
      4 : begin
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert2,poNone,oNone,Item.Certs[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert2,poNone,oNone,Item.DerCerts[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert3,poNone,oNone,Item.Certs[2],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert3,poNone,oNone,Item.DerCerts[2],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Cert4,poNone,oNone,Item.Certs[3],Commands);
        Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DerCert4,poNone,oNone,Item.DerCerts[3],Commands);
      end;
    end;

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Delete(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class Function  Items.DB.Write(Task:Core.Database.Types.TTask; DomainID:QWord; Var Item:TCertData):Boolean;
var
  iCount,iIndex                  : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,UseForCriteria,IDs.ID,poAnd,oEqual,Item.ID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Date,poNone,oNone,Item.Date,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Key,poNone,oNone,Item.Key,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerKey,poNone,oNone,Item.DerKey,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Request,poNone,oNone,Item.Request,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Level,poNone,oNone,Item.Level,Commands);

    Case Item.Level of
      1:begin
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
      end;
      2:begin
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert2,poNone,oNone,Item.Certs[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert2,poNone,oNone,Item.DerCerts[1],Commands);
      end;
      3:begin
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert2,poNone,oNone,Item.Certs[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert2,poNone,oNone,Item.DerCerts[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert3,poNone,oNone,Item.Certs[2],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert3,poNone,oNone,Item.DerCerts[2],Commands);
      end;
      4:begin
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert1,poNone,oNone,Item.Certs[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert1,poNone,oNone,Item.DerCerts[0],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert2,poNone,oNone,Item.Certs[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert2,poNone,oNone,Item.DerCerts[1],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert3,poNone,oNone,Item.Certs[2],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert3,poNone,oNone,Item.DerCerts[2],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Cert4,poNone,oNone,Item.Certs[3],Commands);
        Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.DerCert4,poNone,oNone,Item.DerCerts[3],Commands);
      end;
    end;
    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure CB_Cert_Read(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmP:PCertData;
begin
  itmP:=DataP;
  {$i Storage.Certs.Read.inc}
end;

class Function  Items.DB.Read(Task:Core.Database.Types.TTask; DomainID,ItemID:QWord; Var Item:TCertData):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);

    {$i Storage.Certs.Fields.inc}

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Cert_Read,@Item);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_Cert_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmP:PCertData;
  ListP:PCertList;
  iCt:LongInt;
begin
  ListP:=DataP;
  iCt:=System.Length(ListP^);
  System.SetLength(ListP^,iCt+1);
  New(itmP);
  ListP^[iCt]:=itmP;
  Init(itmP^);

  {$i Storage.Certs.Read.inc}
end;


class Function  Items.DB.List(Task:Core.Database.Types.TTask; Var DomainID:QWord; Var Entries:TCertList):Boolean;
var
  iCount,iLcv:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; Empty(Entries);
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);

    {$i Storage.Certs.Fields.inc}

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Cert_List,@Entries);
  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDBM;
end.


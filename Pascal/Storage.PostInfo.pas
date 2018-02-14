unit Storage.PostInfo;


interface

uses
  Classes,

  RSR.Core,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Timer,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Strings,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Timer,

  Storage,
  Storage.Main,
  Storage.Domains,
  Storage.CoreObjects,

  Core.Streams,
  Core.XML,

  DOM,
  XMLRead,
  SysUtils;


Type
  Items = class
  Type
    DB=class
    Type
      XML=class
      type
        Stanzas=class
        const
          Info               = 'pstinf';
        type
          Fields=class
          const
            ID               = 'id';
            Created          = 'ctd';
            IP               = 'ip';
            Form             = 'frm';
            Data             = 'dta';
          end;
        end;
      end;
      IDs= class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        IP                       : Core.Database.Types.Integer = 3;
        Created                  : Core.Database.Types.Integer = 4;
        Form                     : Core.Database.Types.Integer = 5;
        Data                     : Core.Database.Types.Integer = 6;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        IP                       : Core.Database.Types.VarString = 'ITMIP';
        Created                  : Core.Database.Types.VarString = 'IDTC';
        Form                     : Core.Database.Types.VarString = 'IFRM';
        Data                     : Core.Database.Types.VarString = 'IDAT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Storage';
        Name                     : 'Post Info';
        Value                    : 'scs_pstinfo';
        Hint                     : 'Table for accepting domain level form data from the web.';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields                     : array [0..6] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.IP; KeyP: @Keys.IP; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Form; KeyP: @Keys.Form; DataType: dftSmallString; AutoCreate: True; Verified: False; Precision: 255; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
      class function  Post(Task:Core.Database.Types.TTask; DomainID,IP:QWord; Form:Core.Strings.VarString; Data:Core.Strings.VarString): Boolean;
    end;
    Email=class
    const
      Address              = 'au-eml-addr';
      NameSpace            = 'au-eml-ns';
      Subject              = 'au-eml-sbj';
      Body                 = 'au-eml-bdy';
    end;
  end;


implementation
uses DB;

procedure cbDestroyTable(ItemP: Core.Database.Monitor.Types.PItem);
begin
  With Items.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task: Core.Database.Types.TTask; TableP: Core.Database.Types.PTable; ItemID: QWord; ItemP: Core.Database.Monitor.Types.PItem; Flag: cardinal): boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure PushDomainDeleted;
  begin
    if ItemP = Items.DB.MonitorP then begin
      try
        iCount := 0;
        Core.Database.AddCommand(iCount, Items.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount, Items.DB.TableP, useForCriteria, Items.DB.IDs.DomainID, poNone, oEqual, ItemID, Commands);
        Result := Core.Database.SQL.Delete(Task, @Commands);
      finally
        Empty(Commands);
      end;
    end;
  end;
begin
  Result := False;
  case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Items.DB do begin
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


class function  Items.DB.Post(Task:Core.Database.Types.TTask; DomainID,IP:QWord; Form:Core.Strings.VarString; Data:Core.Strings.VarString): Boolean;
var
  iReset                         : QWord;
  ItemId                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  dtNow                          : Double;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    ItemID:=0;
    dtNow:=Core.Timer.dtUT;
    iInsertID:=Random(High(Integer));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.IP,poNone,oNone,IP,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Form,poNone,oNone,Form,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Data,poNone,oNone,Data,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

  Finally
    Core.Database.Done(Commands);
  End;
end;

initialization
  RegisterDB;
end.


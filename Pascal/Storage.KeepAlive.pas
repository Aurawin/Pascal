{
 unit dbmKeepAlive.pas

 Copyright Aurawin LLC 2003-2011
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
}

unit Storage.KeepAlive;



interface

uses
  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.Arrays.VarString,

  Classes,
  SysUtils;

Type
  IDS=class
  const
    ID                           : Core.Database.Types.Integer= 0;
  end;
  Keys=class
  const
    ID                           : Core.Database.Types.VarString = 'ITMID';
  end;

Var
  tblKeepAliveP                     : Core.Database.Types.PTable;
  dbmiKeepAliveP                    : Core.Database.Monitor.Types.PItem;

implementation
uses db;

Const
  dbsiKeepAlive:Core.Database.Types.TableIni=(
    AutoCreate           : True;
    AutoCommit           : True;
    Group                : 'System';
    Name                 : 'Keep Alive';
    Value                : 'scs_ka';
    Hint                 : 'Storage for cross platform presence awareness';
    PrimaryKeyP          : @Keys.ID;
  );


function cbDBMonitorNotifyied(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;
begin
  Result:=False;
end;

procedure cbDestroyKeepAlive(ItemP:Core.Database.Monitor.Types.PItem);
begin
  Done(tblKeepAliveP^);
  tblKeepAliveP:=nil;
  dbmiKeepAliveP:=nil;
end;

procedure RegisterDBM;
var
  iCount:LongInt;
begin
  if (tblKeepAliveP=nil) then begin
    New(tblKeepAliveP);
    Init(tblKeepAliveP^,dbsiKeepAlive);
    iCount:=0;
    Core.Database.AddField(iCount,True,@IDS.ID,@Keys.ID,cfNotNull or cfPrimaryKey or cfIdentity,0, dftQWord,tblKeepAliveP^.Fields);
    If dbmiKeepAliveP=nil then begin
      New(dbmiKeepAliveP);
      Init(dbmiKeepAliveP^,tblKeepAliveP^,@cbDestroyKeepAlive,@cbDBMonitorNotifyied);
      Core.Database.Monitor.Add(dbmiKeepAliveP);
    end;
  end;
end;

initialization
  RegisterDBM;
end.


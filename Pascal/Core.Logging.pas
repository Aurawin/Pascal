unit Core.Logging;

interface

uses
  Classes,

  Core.Strings,
  Core.Utils.Time,
  Core.Utils.Files,

  SysUtils;


const
  FMT_CREATE_LOG_ERROR           : Core.Strings.VarString = 'There was a problem creating file %s'^M+'Please make sure you have the propper permissions.';
  SYSTEM_LOG                     : Core.Strings.VarString = 'system';
  SYSTEM_DUMP                    : Core.Strings.VarString = 'dump.dat';

Type
  TSystemLog=Class (TFileStream)
  private
    FName                        : VarString;
    FLock                        : TRTLCriticalSection;
  public
    Constructor Create(aFileName:String); reIntroduce;
    Destructor Destroy; override;
    procedure ClearLog;
    procedure WriteLogEntry(Const Domain,Service:VarString; Entry:VarString);
    function  GetLog:String;
  end;

  procedure Start(Name:Core.Strings.FileName);
  procedure Stop();
var
 Native:TSystemLog;


implementation

procedure Start(Name:Core.Strings.FileName);
begin
  if Native=nil then
    Native:=TSystemLog.Create(Name);
end;

procedure Stop();
begin
  FreeAndNil(Native);
end;

Constructor TSystemLog.Create(aFileName:String);
begin
  FName:=aFileName;
  {$ifdef Windows}
    if Core.Utils.Files.Create(FName) then begin
      InitCriticalSection(FLock);
      Try Inherited Create(FName,fmOpenReadWrite or fmShareDenyNone); Except End;
    end else
      Raise (Exception.Create(Format(FMT_CREATE_LOG_ERROR,[FName])));
  {$else}
    {$ifdef Unix}
      if Core.Utils.Files.Create(FName) then begin
        InitCriticalSection(FLock);
        try Inherited Create(FName,fmOpenReadWrite or fmShareDenyNone); Except End;
      end else
        Raise (Exception.Create(Format(FMT_CREATE_LOG_ERROR,[FName])));
    {$else}
      Raise (Exception.Create(Format(FMT_CREATE_LOG_ERROR,[FName])));
    {$endif}
  {$endif}
end;

Destructor TSystemLog.Destroy;
begin
  Inherited Destroy;
  DoneCriticalSection(FLock);
end;

procedure TSystemLog.ClearLog;
begin
  EnterCriticalSection(FLock);
  Try
    Size:=0;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TSystemLog.WriteLogEntry(Const Domain,Service:VarString; Entry:VarString);
var
  Line:VarString;
begin
  if Handle=0 then Exit;
  Core.Strings.Replace(Entry,#13#10,#32);
  Core.Strings.Replace(Entry,#32#32,#32);
  Line:=Concat(Core.Utils.Time.toString(Now,pktInternal,pkfUTC,PrintBiasOff),#9,Domain,#9,Service,#9,Entry,#13,#10);
  EnterCriticalSection(FLock);
  Try
    Position:=Size;
    Write(Line[1],Length(Line));
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

function  TSystemLog.GetLog:String;
var
  iLen:LongInt;
begin
  EnterCriticalSection(FLock);
  Try
    iLen:=Size;
    SetLength(Result,iLen);
    if iLen>0 then begin
      Position:=0;
      ReadBuffer(Result[1],iLen);
    end;
  Finally
    LeaveCriticalSection(FLock);
  end;
end;

end.


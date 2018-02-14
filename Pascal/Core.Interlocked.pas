unit Core.Interlocked;

interface

uses
  Classes, SysUtils;

 procedure Add(var Value:QWord; Increment:Integer);
 procedure Subtract(var Value:QWord; Decrement:Integer);

implementation
var
  Lock:TRTLCriticalSection;

procedure Add(var Value:QWord; Increment:Integer);
begin
  {$ifdef cpu64}
    System.InterLockedExchangeAdd64(Value,Increment);
  {$else}
    EnterCriticalSection(Lock);
    Value+=Increment;
    LeaveCriticalSection(Lock);
  {$endif}
end;

procedure Subtract(var Value:QWord; Decrement:Integer);
begin
  {$ifdef cpu64}
    System.InterLockedExchangeAdd64(Value,-Decrement);
  {$else}
    EnterCriticalSection(Lock);
    Value-=Decrement;
    LeaveCriticalSection(Lock);
  {$endif}
end;

initialization
  InitCriticalSection(Lock);
finalization
  DoneCriticalSection(Lock);
end.


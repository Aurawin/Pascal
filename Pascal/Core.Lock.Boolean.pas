unit Core.Lock.Boolean;

{
 Unit: Core.Lock.Boolean
 Description: Boolean lock is a simple locking mechanism for GUIs.

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}


interface

uses
  Classes, SysUtils;

Type

  TItem=Class
  private
    FLockCount  : LongInt;
  private
    function GetLocked:Boolean;
    function GetUnLocked:Boolean;
  public
    procedure Lock;
    procedure Unlock;

  public
    property Locked:boolean read GetLocked;
    property Unlocked:boolean read GetUnlocked;
  end;

implementation

procedure TItem.Lock;
begin
  if FLockCount<0 then
    FLockCount:=0;
  Inc(FLockCount);
end;

procedure TItem.Unlock;
begin
  Dec(FLockCount);
end;

function TItem.GetLocked:Boolean;
begin
  Result:=FLockCount>0;
end;

function TItem.GetUnLocked:Boolean;
begin
  Result:=FLockCount<=0;
end;

end.


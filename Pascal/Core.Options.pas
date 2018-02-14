unit Core.Options;
{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Release License
 http://www.aurawin.com/aprl.html
}
interface

uses
  Classes,
  Core.Strings,
  SysUtils;

Type
  TOption=record
    sLabel       : Core.Strings.VarString;
    sName        : Core.Strings.VarString;
    sValue       : Core.Strings.VarString;
  end;
  TOptions=Array of TOption;

  procedure Empty(var Item:TOption); overload;
  procedure Empty(var Item:TOptions); overload;

  procedure Init(var Item:TOption); overload;
  procedure Init(var Item:TOptions); overload;

  procedure Done(var Item:TOption); overload;
  procedure Done(var Item:TOptions); overload;

implementation

procedure Empty(var Item:TOption);
begin
  SetLength(Item.sLabel,0);
  SetLength(Item.sName,0);
  SetLength(Item.sValue,0);
end;

procedure Empty(var Item:TOptions);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do Done(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Init(var Item:TOption);
begin
  SetLength(Item.sLabel,0);
  SetLength(Item.sName,0);
  SetLength(Item.sValue,0);
end;

procedure Init(var Item:TOptions);
begin
  Empty(Item);
end;

procedure Done(var Item:TOption);
begin
  Finalize(Item.sLabel);
  Finalize(Item.sName);
  Finalize(Item.sValue);
  Finalize(Item);
end;

procedure Done(Var Item:TOptions);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do Done(Item[iLcv]);
  Finalize(Item);
end;

end.


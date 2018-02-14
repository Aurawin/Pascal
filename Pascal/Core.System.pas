unit Core.System;

interface

uses
  Classes, SysUtils;

Type
   TCPURange=0..100;
   TMemoryInfo=record
     Total_Ram:Int64;
     Total_Swap:Int64;
     Used_Ram:Int64;
     Used_Swap:Int64;
     Free_Ram:Int64;
     Free_Swap:Int64;
     Load_Ram:Int64;
   end;
   TCPUInfo=record
     ID:Int64;
     Usage:TCPURange;
     Frequency:Double;
   end;
   TSystemProcessors=Array of TCPUInfo;
   TSystemProcessorInfo=record
     Brand:string;
     CPUs:TSystemProcessors;
   end;

   procedure Empty(Var Item:TMemoryInfo); overload;
   procedure Empty(Var Item:TCPUInfo); overload;
   procedure Empty(Var Item:TSystemProcessorInfo); overload;
   procedure Empty(Var Item:TSystemProcessors); overload;


   function GetMemoryInfo(Var Info:TMemoryInfo):boolean;
   function RefreshMemoryInfo(Var Info:TMemoryInfo):boolean;
implementation
uses
{$ifdef Windows}
   Windows;
{$else}
  {$ifdef Unix}
    {$ifdef Darwin}
      sysctl;
    {$else}
      Linux;
    {$endif}
  {$endif}
{$endif}

procedure Empty(Var Item:TMemoryInfo);
begin
  Item.Total_Ram:=0;
  Item.Total_Swap:=0;
  Item.Used_Ram:=0;
  Item.Used_Swap:=0;
  Item.Free_Ram:=0;
  Item.Free_Swap:=0;
  Item.Load_Ram:=0;
end;

procedure Empty(Var Item:TCPUInfo);
begin
  Item.ID:=0;
  Item.Usage:=0;
  Item.Frequency:=0;
end;

procedure Empty(Var Item:TSystemProcessorInfo);
begin
  SetLength(Item.Brand,0);
  Empty(Item.CPUs);
end;

procedure Empty(Var Item:TSystemProcessors);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(Item) to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;


function GetMemoryInfo(Var Info:TMemoryInfo):boolean;
{$ifdef Windows}
  var
    MS:TMemoryStatus;
{$else}
  {$ifdef Unix}
    {$ifdef Darwin}
    {$else}
      var
        SI:TSysInfo;
    {$endif}
  {$endif}
{$endif}

begin

  {$ifdef Windows}
    GlobalMemoryStatus(MS);
    Info.Total_Ram:=MS.dwTotalPhys;
    Info.Total_Swap:=MS.dwTotalVirtual;
    Info.Free_Ram:=MS.dwAvailPhys;
    Info.Free_Swap:=MS.dwAvailVirtual;
    Info.Used_Ram:=Info.Total_Ram-Info.Free_Ram;
    Info.Used_Swap:=Info.Total_Swap-Info.Free_Swap;
    Info.Load_Ram:=MS.dwMemoryLoad;
    Result:=true;
  {$else}
    {$ifdef Darwin}
      Result:=false;
    {$else}
      Linux.SysInfo(@SI);
      Info.Total_Ram:=SI.totalram;
      Info.Total_Swap:=SI.totalswap;
      Info.Free_Ram:=SI.freeram;
      Info.Free_Swap:=SI.freeswap;
      Info.Used_Ram:=Info.Total_Ram-Info.Free_Ram;
      Info.Used_Swap:=Info.Total_Swap-Info.Free_Swap;
      Info.Load_Ram:=Trunc((Info.Used_Ram / Info.Total_Ram) * 100);
      Result:=true;
    {$endif}
  {$endif}
end;

function RefreshMemoryInfo(Var Info:TMemoryInfo):boolean;
{$ifdef Windows}
  var
    MS:TMemoryStatus;
{$else}
  {$ifdef Unix}
    {$ifdef Darwin}
      var
        mib   : array[0..1] of longint;
        len   : longint;
        value : longint;
    {$else}
      var
        SI:TSysInfo;
    {$endif}
  {$endif}
{$endif}
begin
  {$ifdef Windows}
    GlobalMemoryStatus(MS);
    Info.Free_Ram:=MS.dwAvailPhys;
    Info.Free_Swap:=MS.dwAvailVirtual;
    Info.Used_Ram:=Info.Total_Ram-Info.Free_Ram;
    Info.Used_Swap:=Info.Total_Swap-Info.Free_Swap;
    Info.Load_Ram:=MS.dwMemoryLoad;
    Result:=true;
  {$else}
    {$ifdef Unix}
      {$ifdef Darwin}
        len:=SizeOf(Value);
        fpsysctl(Pchar(@MIB),2,@Value,@len,nil,0);
      {$else}
        Linux.SysInfo(@SI);
        Info.Free_Ram:=SI.freeram;
        Info.Free_Swap:=SI.freeswap;
        Info.Used_Ram:=SI.totalram-SI.freeram;
        Info.Used_Swap:=SI.totalswap-SI.freeswap;
        Info.Load_Ram:=Trunc((Info.Used_Ram / Info.Total_Ram) * 100);
        Result:=true;
      {$endif}
    {$endif}
  {$endif}
end;
end.


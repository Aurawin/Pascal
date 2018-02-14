unit Core.Utils.Sockets;

{
 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is issued under the Aurawin Public Release License
 http://www.aurawin.com/aprl.html
}


interface

uses
  Classes,SysUtils,
  Sockets,

  Core.Strings,
  Core.Arrays.Types,
  Core.Arrays.VarString,

  Core.Utils.Time

  ;

   function InAddrToStr(InAddr:System.Int64):Core.Strings.VarString;
   function InAddrFromStr(Value:Core.Strings.VarString):System.Int64; overload;
   function InAddrFromStr(Var saIP:Core.Arrays.Types.VarString):System.Int64; overload;
   function InAddrToRevStr(InAddr:System.QWord):Core.Strings.VarString;
   function GetTimeStamp(Const IncludeDateHeader:Boolean):Core.Strings.VarString;
   function SocketErrorToString(Error:System.LongInt):Core.Strings.VarString;
   function SocketAddress(IP:System.QWord; Port:System.Word):Sockets.TSockAddr;

   {$ifdef Unix}procedure SetSigNoSigPipe(var Socket:Sockets.TSocket; Const Switch:Boolean=true);{$endif}

   procedure SetTCPDelay(Socket:Sockets.TSocket; Const Delay:Boolean=true);
   function  DataSizeWaiting(Socket:TSocket):LongInt;
   procedure SetBlockingMode(var Socket:TSocket; Const Switch:Boolean=true);
   procedure SetReUseableMode(var Socket:TSocket; Const Switch:Boolean=true);
   procedure SetLinger(var Socket:Sockets.TSocket; var Value: Sockets.TLinger);
   function  IsConnected(var Socket:Sockets.TSocket):boolean;
   //procedure BuildClassC(Input:Core.Strings.VarString; out List:Core.Arrays.Types.VarString);
   function MaskClassC(Input:Core.Strings.VarString):Core.Strings.VarString;
const
  MAX_HOSTNAME_LEN          = 128;
  MAX_DOMAIN_NAME_LEN       = 128;
  MAX_SCOPE_ID_LEN          = 256;
  SOCKET_REUSE_ON           = true;
  SOCKET_REUSE_OFF          = false;

  BLOCKING_ON               = true;
  BLOCKING_OFF              = false;
  TCP_DELAY_ON              = true;
  TCP_DELAY_OFF             = false;
  {$ifdef Unix}
  SIGPIPE_ON                = false;
  SIGPIPE_OFF               = true;
  {$endif}
type
PULONG=^Cardinal;
PIPAddressString = ^TIPAddressString;
PIPMaskString    = ^TIPAddressString;
TIPAddressString = record
  _String: array[0..(4 * 4) - 1] of Char;
end;
TIPMaskString = TIPAddressString;
//
// TIPAddrString - store an IP address with its corresponding subnet mask,
// both as dotted decimal strings
//
PIPAddrString = ^TIPAddrString;
TIPAddrString = packed record
  Next: PIPAddrString;
  IpAddress: TIPAddressString;
  IpMask: TIPMaskString;
  Context: DWORD;
end;
PFixedInfo = ^TFixedInfo;
TFixedInfo = packed record
  HostName: array[0..MAX_HOSTNAME_LEN + 4 - 1] of Char;
  DomainName: array[0..MAX_DOMAIN_NAME_LEN + 4 - 1] of Char;
  CurrentDnsServer: PIPAddrString;
  DnsServerList: TIPAddrString;
  NodeType: Cardinal;
  ScopeId: array[0..MAX_SCOPE_ID_LEN + 4 - 1] of Char;
  EnableRouting,
  EnableProxy,
  EnableDns: Cardinal;
end;

implementation
uses
  {$if defined(Windows)}
    Winsock2,
  {$elseif defined(Unix)}
    baseUnix,Termio,
  {$endif}
  RSR,
  Core.Timer;

{

TWaitHandleEvent = procedure(AData: PtrInt; AFlags: dword) of object;

function  AddEventHandler(AHandle: THandle; AFlags: dword; AEventHandler: TWaitHandleEvent; AData: PtrInt): PEventHandler;
procedure RemoveEventHandler(var AHandler: PEventHandler);
procedure SetEventHandlerFlags(AHandler: PEventHandler; NewFlags: dword);

}

function SocketErrorToString(Error:LongInt):Core.Strings.VarString;
begin
  Result:=IntToStr(Error);
end;

function  SocketAddress(IP:QWord; Port:Word): Sockets.TSockAddr;
begin
  Result.sin_addr.s_addr:=Cardinal(IP);
  Result.sin_family:=AF_INET;
  Result.sin_port:=Sockets.htons(Port);
end;

function  GetTimeStamp(Const IncludeDateHeader:Boolean):Core.Strings.VarString;
Const
  HeaderStr:Array[Boolean] of Core.Strings.VarString=('','Date: ');
begin
  Result:=Concat(HeaderStr[IncludeDateHeader],Core.Utils.Time.TimeZoneTime);
end;

function InAddrFromStr(Value:Core.Strings.VarString):System.Int64;
var
  saIP:Core.Arrays.Types.VarString;
begin
  Result:=0;
  Core.Arrays.VarString.fromString(saIP,Value,'.');
  Try
    If (Length(saIP)=4) then begin
      Result:=
        (Ord(StrToQWordDef(saIP[0],0)) shl 0) +
        (Ord(StrToQWordDef(saIP[1],0)) shl 8) +
        (Ord(StrToQWordDef(saIP[2],0)) shl 16) +
        (Ord(StrToQWordDef(saIP[3],0)) shl 24);
    end;
  Finally
    Empty(saIP);
  End;
end;


function InAddrFromStr(Var saIP:Core.Arrays.Types.VarString):System.Int64;
begin
  if Length(saIP)>=4 then
      Result:=
        (Ord(StrToQWordDef(saIP[0],0)) shl 0) or
        (Ord(StrToQWordDef(saIP[1],0)) shl 8) or
        (Ord(StrToQWordDef(saIP[2],0)) shl 16) or
        (Ord(StrToQWordDef(saIP[3],0)) shl 24)
  else
    Result:=0;
end;

function InAddrToStr(InAddr:Int64):Core.Strings.VarString;
begin
  Result:=Concat(
    IntToStr(InAddr and $000000ff),'.',
    IntToStr(InAddr and $0000ff00 shr 8),'.',
    IntToStr(InAddr and $00ff0000 shr 16),'.',
    IntToStr(InAddr and $ff000000 shr 24)
  );
end;

Function  InAddrToRevStr(InAddr:QWord):Core.Strings.VarString;
begin
  Result:=Concat(
    IntToStr(InAddr and $ff000000 shr 24),'.',
    IntToStr(InAddr and $00ff0000 shr 16),'.',
    IntToStr(InAddr and $0000ff00 shr 8),'.',
    IntToStr(InAddr and $000000ff)
  );
end;

procedure SetReUseableMode(var Socket:Sockets.TSocket; Const Switch:Boolean=true);
const
  iMode:Array[Boolean] of Cardinal=(0,1);
begin
  Sockets.fpsetsockopt(Socket, SOL_SOCKET, SO_REUSEADDR, @iMode, System.SizeOf(iMode));
end;

{$ifdef Unix}
procedure SetSigNoSigPipe(var Socket:Sockets.TSocket; Const Switch:Boolean=true);
const
  iMode:Array[Boolean] of Cardinal=(0,1);
  iFlag:Cardinal={$ifdef linux}$1022{$else}SO_NOSIGPIPE{$endif};
begin
  Sockets.fpsetsockopt(Socket, SOL_SOCKET, iFlag, @iMode[Switch], System.SizeOf(iMode));
end;
{$endif}

procedure SetLinger(var Socket:Sockets.TSocket; var Value: Sockets.TLinger);
begin
  Sockets.fpSetSockOpt(Socket,SOL_SOCKET,SO_LINGER,@Value,System.SizeOf(Value));
end;

procedure SetTCPDelay(Socket:Sockets.TSocket; Const Delay:Boolean=true);
const
  iFlag:array[boolean] of longint = (1,0);
begin
  Sockets.fpSetSockOpt(Socket,IPPROTO_TCP,TCP_NODELAY,@iFlag[Delay],System.SizeOf(longint));
end;

procedure SetBlockingMode(var Socket:Sockets.TSocket; Const Switch:Boolean=true);
  {$ifdef Unix}
  var
    nbioFlag:cuInt;
    asyFlag:cuInt;
  //flags:cInt;
  {$endif}
begin
  {$if defined(Unix)}
    //flags:=FpFcntl(Socket,F_GETFL);
    if (Switch=true) then begin
      asyFlag:=0;
      nbioFlag:=0;
    end else begin
      asyFlag:=1;
      nbioFlag:=1;
    end;

    //fpcntl(Socket,F_SETFL,O_NONBLOCK,nbioFlag);
    fpioctl(Socket, FIOASYNC, @asyFlag);
    fpioctl(Socket, FIONBIO, @nbioFlag);

  {$elseif defined(Windows)}
    Winsock2.ioctlsocket(Socket, LongInt(RSR.FIONBIO), @Switch);
  {$endif}
end;

Function DataSizeWaiting(Socket:Sockets.TSocket):LongInt;
Var
   ByteCount:System.DWORD;
begin
  {$ifdef Unix}
    {$i Core.Utils.Sockets.Unix.DataSizeWaiting.inc}
  {$endif}{$ifdef Windows}
    {$i Core.Utils.Sockets.Windows.DataSizeWaiting.inc}
  {$endif}
end;

function  IsConnected(var Socket:Sockets.TSocket):boolean;
const
  Error:LongInt=0;
var
  Len:TSockLen;
begin
  Len:=SizeOf(Socket);
  Result:=Sockets.fpgetsockopt(Socket, SOL_SOCKET, SO_ERROR, @Error, @Len)=0;
end;

function MaskClassC(Input:Core.Strings.VarString):Core.Strings.VarString;
var
  iCount:LongInt;
  Master:Core.Arrays.Types.VarString;
begin
  Core.Arrays.VarString.Init(Master);
  Try
    iCount:=Core.Arrays.VarString.fromString(Master,Input,'.');
    if iCount=4 then begin
      Result:=Concat(Master[0],'.',Master[1],'.',Master[2],'.');
    end;
  finally
    Core.Arrays.VarString.Done(Master);
  end;
end;

end.


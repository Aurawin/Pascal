unit RSR.DNS;


interface
  uses
      Classes,
      Sockets,

      Core.Strings,
      Core.Arrays,
      Core.Arrays.Types,
      Core.Arrays.LargeWord,
      Core.Arrays.KeyString,
      Core.Arrays.VarString,
      Core.Arrays.Bytes,
      Core.Timer,
      Core.Utils.Sockets

      ;


  (*
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |--------------------HEADER---------------------|
  0   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                       ID                      |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     QDCOUNT                   |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     ANCOUNT                   |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     NSCOUNT                   |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                    ARCOUNT                    |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+96
      |-----------------QUESTIONS (QDCount)-----------|
  97  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |       Length          |                       -
      -                       |                       -
      -                       |                       -
      -                     QNAME                     -
      -                       |     Length            -
      -                       |                       -
      -                       |                       -
      .                       .                       .
      .                       .                       .
      .                       .                       .
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     QTYPE                     |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     QCLASS                    |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |------------Answers (ANCOUNT of RRs)-----------|
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |       Length          |                       -
      -                       |                       -
      -                       |                       -
      -                     RNAME                     -
      -                       |     Length            -
      -                       |                       -
      -                       |                       -
      .                       .                       .
      .                       .                       .
      .                       .                       .
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     RTYPE                     |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                    RCLASS                     |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                      TTL                      |
      |                                               |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                   RDLENGTH                    |
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |                     RDATA                     |
      .                       .                       .
      .                       .                       .
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |----------Authority (ARCOUNT of RRs)-----------|
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      .                 Resource Records              .
      .                       .                       .
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      |----------Additional (ARCOUNT of RRs)----------|
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
      .                       .                       .
      .                 Resource Records              .
      .                       .                       .
      +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

  *)
  Const
     DNS_PORT = 53;
     MAX_QUESTIONS=5;
     MAX_CACHE=5000;
     DNS_CACHE_TTL = 36000 * 30;  // Cache valid for 30 minutes
     NameServer : Core.Strings.VarString =  'nameserver';
  Type
     TDNSArray=Array[1..10] of Core.Strings.VarString;
     TDNSStyle=(dnsIP,dnsWait,dnsMX,dnsPtr,dnsCache);
     TDNSState=(dsNone,dsRoot,dsDestination);
     TDNSStyles=Set of TDNSStyle;
     TRRArray=Array[0..254] of System.byte;
     TResourceRec=record
       RNAME     : Core.Strings.VarString;
       RTYPE     : Word;
       RCLASS    : Word;
       TTL       : Cardinal;
       RDLength  : Word;
       RDATA     : TRRArray;
     end;
     TResourceRecs=Array of TResourceRec;
     TQQuestion=record
       Q_Name:Core.Strings.VarString;
       Q_Type:Word;
       Q_Class:Word;
     end;
     TQQuestions=Array of TQQuestion;
     TQHeader=record
       ID      : Word;
       Options : Word;
       QDCount : Word;
       Ancount : Word;
       NSCount : Word;
       ARCount : Word;
     end;
     PQCache=^TQCache;
     TQCache=record
       Input     : Core.Strings.VarString;
       Kind      : Word;
       Expires   : TDateTime;
       Answers   : Core.Arrays.Types.VarString;
     end;
     TQCaches=Array of PQCache;


     TDNSObject=Class(TObject)
     public
       Address   : TSockAddr;
       Socket    : TSocket;
       Error     : Boolean;
     private
       QD_Data   : TResourceRecs;
       AN_Data   : TResourceRecs;
       NS_Data   : TResourceRecs;
       AD_Data   : TResourceRecs;
     private
       FCacheWIX : LongInt;
       FCache    : TQCaches;
     private
       function   GetRCode():System.Byte;
       procedure  reIndexCache();
     public
       function   BufferOut(Var Buffer:Core.Arrays.Types.Bytes):LongInt; overload;
       function   StreamIn(Stream:TStream; Var iPosition:Cardinal; Const iSize:LongInt):Boolean; overload;
       function   StreamIn(Stream:TStream; Var iPosition:System.Int64; Const iSize:LongInt):Boolean; overload;
       function   StreamOut(Stream:TStream):System.Int64;
       procedure  Reset();
       procedure  Cache();
     public
       function   checkCache(sValue:Core.Strings.VarString):PQCache; overload;
       function   checkCache(Query:Core.Strings.VarString; Kind:Word):PQCache; overload;
     public
       Answers   : Core.Arrays.Types.VarString;
       Header    : TQHeader;
       Questions : TQQuestions;
     public
       property   RCode:Byte read GetRCode;
     public
       Constructor Create; reintroduce;
       Destructor  Destroy; override;
     end;

     TDNSServer=Record
       Name  : Core.Strings.VarString;
       IPs   : Core.Arrays.Types.LargeWord;
       Count : System.Byte;
     end;

     TDNSServers=Array[0..3] of TDNSServer;


      procedure StreamInResourceRecord(Var bError:Boolean; Const Index,Count:LongInt; var Position:System.Int64; Stream:TStream; out Result:TResourceRec); overload;
      procedure StreamInResourceRecord(Var bError:Boolean; Const Index,Count:LongInt; var Position:Cardinal; Stream:TStream; out Result:TResourceRec); overload;
      procedure BufferInResourceRecord(Var bError:Boolean; Const Index,Count:LongInt; Var Position:LongInt; Var Buffer:Core.Arrays.Types.Bytes; Const iSize:LongInt; out Result:TResourceRec);

      Function BufferInDNSDomain(Var Buffer:Core.Arrays.Types.Bytes; Var Position:LongInt):Core.Strings.VarString;
      Function StreamInDNSDomain(Stream:TStream):Core.Strings.VarString;
      Function DomainToDNSDomain(Const Domain:Core.Strings.VarString):Core.Strings.VarString;
      Function DNSDomainToDomain(Const DNSDomain:Core.Strings.VarString):Core.Strings.VarString;
      Function RRDomainToDomain(Const RType:LongInt; Const RR:TRRArray):Core.Strings.VarString;
      Function PeekAtID(Var ID:Word; Stream:TStream; iStart:System.Int64):Boolean;



      procedure  Empty(Var Item:TDNSServer); overload;
      procedure  Empty(Var Item:TDNSServers); overload;
      procedure  Empty(Var Item:TResourceRec); overload;
      procedure  Empty(Var Item:TResourceRecs); overload;
      procedure  Empty(Var Item:TQQuestions); overload;
      procedure  Empty(Var Item:TQQuestion); overload;
      procedure  Empty(Var Item:TQHeader); overload;


      procedure  Init(Var Item:TQCache); overload;


      procedure  Done(Var Item:TResourceRecs); overload;
      procedure  Done(Var Item:TQQuestions); overload;
      procedure  Done(Var Item:TQQuestion); overload;
      procedure  Done(Var Item:TQCaches); overload;
      procedure  Done(Var Item:TQCache); overload;

      procedure  Copy(Var Source,Destination:TResourceRecs); overload;
      procedure  Copy(Var Source,Destination:TResourceRec); overload;

      Function GetRCode(Var Header:TQHeader):System.Byte;
      Function GetZ(Var Header:TQHeader):System.Byte;
      Function GetRA(Var Header:TQHeader):Boolean;

      Function  GetRD(Var Header:TQHeader):Boolean;
      procedure SetRD(Var Header:TQHeader; Value:Boolean);

      Function GetTC(Var Header:TQHeader):Boolean;
      Function GetAA(Var Header:TQHeader):Boolean;
      Function GetOpCode(Var Header:TQHeader):System.Byte;
      Function GetQR(Var Header:TQHeader):Boolean;


      Function   GetNextDNSServer:System.QWord;

      procedure SyncSystemDNS;

  Const
      QT_NONE = 0;
      QT_IP   = 1; // DNS Query Type for IP hosts
      QT_MX   = 15; // DNS Query Type for MX hosts
      QT_PTR  = 12; // DNS Query Type for IP to Name Values

       // Query Header Masking Constants
      {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
       |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
       +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
       QHMC_RCODE  =     15;
       QHMC_Z      =    112;
       QHMC_RA     =    128;
       QHMC_RD     =    256;
       QHMC_TC     =    512;
       QHMC_AA     =   1024;
       QHMC_OPCODE =  30720;
       QHMC_QR     =  32768;

  Var
    DNS_Servers : TDNSServers;
    System_DNS  : Core.Arrays.Types.LargeWord;
    {$ifdef Windows}
      const iphlpapidll = 'iphlpapi.dll';
      function  GetNetworkParams(pFixedInfo: PFixedInfo; pOutBufLen: PULONG): DWORD; stdcall external iphlpapidll name 'GetNetworkParams';
    {$endif}
    procedure Check_DNS_Servers();

implementation

uses SysUtils,DateUtils
    {$ifdef Windows},Windows{$endif}
  ;



procedure Check_DNS_Servers();
var
  iCount:LongInt;
  iLcv:LongInt;
begin
  iCount:=0;
  for iLcv:=0 to High(DNS_Servers) do
    if DNS_Servers[iLcv].Count>0 then
       inc(iCount);
  if (iCount=0) then begin
    for iLcv:=0 to High(System_DNS) do
      Core.Arrays.LargeWord.Add(System_DNS[iLcv],DNS_Servers[0].IPs,aoCheckForDuplicates);
    DNS_Servers[0].Count:=System.Length(DNS_Servers[0].IPs);
  end;
end;

Function   GetNextDNSServer:System.QWord;
Const
  OldSrvCount:Byte=0;
  SrvIndex:Byte=0;
  IPIndex:Byte=0;
  IpCount:Byte=0;
Var
  SrvCount:LongInt;
begin
  Result:=0;
  SrvCount:=Length(DNS_Servers);
  if SrvCount<>OldSrvCount then begin
    SrvIndex:=0;
    OldSrvCount:=SrvCount;
    If (SrvCount>0) then
      IPCount:=DNS_Servers[0].Count
    else
      IPCount:=0;
  end else begin
    If (SrvIndex<SrvCount) and (DNS_Servers[SrvIndex].Count<>IPCount) then begin
      IPCount:=DNS_Servers[SrvIndex].Count;
      IPIndex:=0;
    end;
  end;
  // General Increase Of IPs
  If IPIndex>=IPCount then begin
    IPCount:=0;
    While (SrvIndex<SrvCount) and (IPCount=0) do begin
      Inc(SrvIndex);
      IPIndex:=0;
      if (SrvIndex<SrvCount) then
        IPCount:=DNS_Servers[SrvIndex].Count;
    end;
  end;
  If (SrvIndex>=SrvCount) then begin
    SrvIndex:=0;
    IPIndex:=0;
    if (SrvCount>0) then
      IPCount:=DNS_Servers[0].Count
    else
      IPCount:=0;
  end;
  If (IPCount>0) then begin
    Result:=DNS_Servers[SrvIndex].IPs[IpIndex];
    Inc(IPIndex);
  end;

end;

procedure  Empty(Var Item:TDNSServers);
begin
  SetLength(Item[0].Name,0);
  SetLength(Item[1].Name,0);
  SetLength(Item[2].Name,0);
  SetLength(Item[3].Name,0);
end;

procedure  Empty(Var Item:TDNSServer);
begin
  Core.Arrays.LargeWord.Empty(Item.IPs);
  Item.Count:=0;
  SetLength(Item.Name,0);
end;

procedure  Copy(Var Source,Destination:TResourceRecs);
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iCount:=Length(Source);
  Empty(Destination);
  SetLength(Destination,iCount);
  For iLcv:=0 to iCount-1 do
    Copy(Source[iLcv],Destination[iLcv]);
end;

procedure  Copy(Var Source,Destination:TResourceRec);
begin
  Destination.RNAME:=Source.RNAME;
  Destination.RTYPE:=Source.RTYPE;
  Destination.RCLASS:=Source.RCLASS;
  Destination.TTL:=Source.TTL;
  Destination.RDLength:=Source.RDLength;
  Destination.RDATA:=Source.RDATA;
end;

procedure  Empty(Var Item:TQQuestion);
begin
  SetLength(Item.Q_Name,0);
  Item.Q_Type:=0;
  Item.Q_Class:=0;
end;

procedure Empty(Var Item:TQHeader);
begin
  Item.ID:=0;
  Item.Options:=0;
  Item.QDCount:=0;
  Item.Ancount:=0;
  Item.NSCount:=0;
  Item.ARCount:=0;
end;

procedure  Empty(Var Item:TQQuestions);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure  Empty(Var Item:TResourceRecs);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Empty(Var Item:TResourceRec);
begin
  SetLength(Item.RNAME,0);
  Item.RTYPE:=0;
  Item.RCLASS:=0;
  Item.TTL:=0;
  Item.RDLength:=0;
  FillByte(Item.RData[0],SizeOf(Item.RData),0);
end;


procedure  Done(Var Item:TResourceRecs);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Finalize(Item[iLcv].RNAME);
  Finalize(Item,0);
end;

procedure  Done(Var Item:TQQuestion);
begin
  SetLength(Item.Q_Name,0);
  Finalize(Item);
end;

procedure  Done(Var Item:TQCaches);
var
  iLcv:LongInt;
  itmP:PQCache;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  Finalize(Item);
end;

procedure  Done(Var Item:TQCache);
begin
  Finalize(Item.Input);
  Core.Arrays.VarString.Done(Item.Answers);
  Finalize(Item);
end;

procedure  Init(Var Item:TQCache);
begin
  SetLength(Item.Input,0);
  Core.Arrays.VarString.Init(Item.Answers);
  Item.Kind:=0;
  Item.Expires:=0;
end;

procedure  Done(Var Item:TQQuestions);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure SyncSystemDNS;
var
  List:Core.Arrays.Types.LargeWord;
  {$ifdef Windows}
    pFI: PFixedInfo;
    pIPAddr: PIPAddrString;
    OutLen: Cardinal;
    iCount:LongInt;

    procedure PushAdd;
    begin
      If pFI^.DnsServerList.IpAddress._String[0] <> #0 then begin
        SetLength(List,iCount+1);
        List[iCount]:=InAddrFromStr(pFI^.DnsServerList.IpAddress._String);
        Inc(iCount);
        pIPAddr := pFI^.DnsServerList.Next;
        while Assigned(pIPAddr) do begin
          SetLength(List,iCount+1);
          List[iCount]:=InAddrFromStr(pIPAddr^.IpAddress._String);
          Inc(iCount);
          pIPAddr := pIPAddr^.Next;
        end;
      end;
    end;
  {$endif}{$ifdef Unix}
    kpFile:Core.Arrays.Types.KeyStrings;
    iLcv:LongInt;
  {$endif}
begin
  Try
    {$ifdef Windows}
      {$i RSR.DNS.Windows.List.Servers.inc}
    {$else}
      {$i RSR.DNS.Unix.List.Servers.inc}
    {$endif}
    Copy(List,System_DNS);
  Finally
    SetLength(List,0);
  end;
end;

procedure BufferInResourceRecord(Var bError:Boolean; Const Index,Count:LongInt; Var Position:LongInt; Var Buffer:Core.Arrays.Types.Bytes; Const iSize:LongInt; out Result:TResourceRec);
var
  SLcv,RDLcv:LongInt;
  bSP,c,CheckByte : byte;

  procedure FillData;
  var
    StreamPos,SaveStreamPos:LongInt;
  begin
     if (c=192) and (Result.RType<>1) and (Position<iSize) then begin
        bSP:=Buffer[Position];
        Inc(Position);
        Inc(SLcv);
        StreamPos:=bSP;
        SaveStreamPos:=Position;
        If StreamPos<iSize then begin
            Position:=StreamPos;
            Repeat
               C:=Buffer[Position];
               Inc(Position);
               Result.RDATA[RDLcv]:=C;
               Inc(RDLcv);
            Until (c=192) or (C=0) or (RDLcv>=High(Result.RDATA)) or (Position>=iSize);
            If C=192 then begin
              Dec(RDLcv);
              Result.RDATA[RDLcv]:=0;
              // Check for circular references...
              If Position<iSize then begin
                CheckByte:=Buffer[Position];
                Inc(Position);
                If CheckByte<>StreamPos then begin
                  Dec(Position);
                  FillData;
                end;
              end;
            end;
            Position:=SaveStreamPos;
        end;
     end else If RDLcv<=High(Result.RData) then begin
        Result.RDATA[RDLcv]:=C;
        Inc(RDLcv);
     end;
  end;
begin
   bError:=False;
   Empty(Result);
   Result.RName:=BufferInDNSDomain(Buffer,Position);
   If Position<iSize then begin
     System.Move(Buffer[Position],Result.RType,2);
     Inc(Position,2);
     Result.RType:=nTohs(Result.RType);
     If Position<iSize then begin
       System.Move(Buffer[Position],Result.RClass,2);
       Inc(Position,2);
       Result.RClass:=nTohs(Result.RClass);
       If Position<iSize then begin
         System.Move(Buffer[Position],Result.TTL,4);
         Inc(Position,4);
         Result.TTL:=nTohl(Result.TTL);
         If Position<iSize then begin
           System.Move(Buffer[Position],Result.RDLength,2);
           Inc(Position,2);
           Result.RDLength:=nTohs(Result.RDLength);
         end;
         If (Position<iSize) then begin
           FillByte(Result.RData[0],System.SizeOf(Result.RData),0);
           RDLcv:=Low(Result.RDATA);SLcv:=1;
           While (SLcv<=Result.RDLength) and (Position<iSize) and (RDLcv<=High(Result.RDATA)) do begin
              C:=Buffer[Position];
              Inc(Position);
              Inc(SLcv);
              FillData;
           end;
         end else
           bError:=True;
       end else
         bError:=True;
     end else
       bError:=True;
   end else
     bError:=True;
end;

procedure StreamInResourceRecord(Var bError:Boolean; Const Index,Count:LongInt; var Position:Cardinal; Stream:TStream; out Result:TResourceRec);
{$i RSR.DNS.StreamInResourceRecord.inc}

procedure StreamInResourceRecord(Var bError:Boolean; Const Index,Count:LongInt; var Position:System.Int64; Stream:TStream; out Result:TResourceRec);
{$i RSR.DNS.StreamInResourceRecord.inc}


Function RRDomainToDomain(Const RType:LongInt; Const RR:TRRArray):Core.Strings.VarString;
var
  LLcv,Len,Lcv:LongInt;

  procedure FillDomain;
  begin
     Repeat
        Len:=RR[Lcv];Inc(Lcv); LLcv:=1;
        While (Lcv<=High(RR)) and (LLcv<=Len) and (RR[Lcv]<>0) do begin
           Result:=Concat(Result,Char(RR[Lcv]));
           Inc(Lcv);
           Inc(LLcv);
        end;
        If RR[Lcv]<>0 then
           Result:=Concat(Result,'.');
     Until (RR[Lcv]=0);
  end;

begin
   Result:=''; Lcv:=Low(RR);
   Case RType of
      1: begin
         Result:=Concat(
            IntToStr(RR[Lcv]),'.',
            IntToStr(RR[Lcv+1]),'.',
            IntToStr(RR[Lcv+2]),'.',
            IntToStr(RR[Lcv+3])
         );
         end;
      5,12: FillDomain;
      15:begin
           Inc(Lcv,2); // Preference
           FillDomain;
         end;
   end;
end;

Function BufferInDNSDomain(Var Buffer:Core.Arrays.Types.Bytes; Var Position:LongInt):Core.Strings.VarString;
var
  BufferPos,SaveBufferPos:Word;
  bSP,c,i : System.byte;
  Ch : System.char;
begin
  Result:=''; c:=0;
  repeat
    If Position<Length(Buffer) then begin
      c:=Buffer[Position];
      Inc(Position);
      if (c=192) then begin
        If (Position<Length(Buffer)) then begin
          bsp:=Buffer[Position];
          Inc(Position);
          BufferPos:=bSP;
          If BufferPos>Length(Buffer) then
            Exit;
          SaveBufferPos:=Position;
          If SaveBufferPos=BufferPos then
            Exit;
          Position:=BufferPos;
          Result:=Concat(Result,BufferInDNSDomain(Buffer,Position));
          Position:=SaveBufferPos;
          c:=0;
        end;
      end else begin
        i:=1;
        While (i<=c) and (Position<Length(Buffer)) do begin
          Ch:=Char(Buffer[Position]);
          Inc(Position);
          If Ch<>#0 then
            Result:=Concat(Result,Ch);
          Inc(i);
        end;
        If Position<Length(Buffer)then begin
          c:=Buffer[Position];
          Inc(Position);
        end;
        if c<>0 then begin
          Result:=Concat(Result,'.');
          If Position<Length(Buffer) then
             Dec(Position);
        end;
      end;
    end;
  until (c=0) or (Position>=Length(Buffer));
end;

Function PeekAtID(Var ID:WORD; Stream:TStream; iStart:Int64):boolean;
begin
  ID:=0;
  Stream.Position:=iStart;
  Result:=Stream.Read(ID,2)=2;
  ID:=ntohs(ID);
  Stream.Position:=iStart;
End;


Function StreamInDNSDomain(Stream:TStream):Core.Strings.VarString;
var
  StreamPos,SaveStreamPos:Word;
  bSP,c,i : byte;
  Ch : char;
begin
  Result:='';
  repeat
    If Stream.Position<Stream.Size then begin
      Stream.ReadBuffer(c,1);
      if (c=192) then begin
        If (Stream.Position<Stream.Size) then begin
          Stream.ReadBuffer(bSP,1);
          StreamPos:=bSP;
          If StreamPos>Stream.Size then
            Exit;
          SaveStreamPos:=Stream.Position;
          If SaveStreamPos=StreamPos then
            Exit;
          Stream.Position:=StreamPos;
          Result:=Concat(Result,StreamInDNSDomain(Stream));
          Stream.Position:=SaveStreamPos;
          c:=0;
        end;
      end else begin
        i:=1;
        While (i<=c) and (Stream.Position<Stream.Size) do begin
          Stream.ReadBuffer(Ch,1);
          If Ch<>#0 then
            Result:=Concat(Result,Ch);
          Inc(i);
        end;
        If Stream.Position<Stream.Size then
          Stream.ReadBuffer(C,1);
        if c<>0 then begin
          Result:=Concat(Result,'.');
          If Stream.Position<Stream.Size then
             Stream.Position:=Stream.Position-1;
        end;
      end;
    end;
  until (c=0) or (Stream.Position>=Stream.Size);
end;


Function DomainToDNSDomain(Const Domain:Core.Strings.VarString):Core.Strings.VarString;
var
  Lcv,DLcv:LongInt;
begin
  Result:=Concat('.',Domain); DLcv:=0;
  for Lcv:=Length(Result) downto 1 do begin
    if Result[Lcv]='.' then begin
      Result[Lcv]:=Chr(DLcv);
      DLcv:=0;
    end else
      Inc(DLcv);
  end;
end;

Function DNSDomainToDomain(Const DNSDomain:Core.Strings.VarString):Core.Strings.VarString;
var
  iLen,DIndex:LongInt;
  CByte:System.Byte;
begin
   //    mail1.cybercreek.com
   //  $c0#5mail1$c0#10cybercreek$c0#3com
   //
   DIndex:=1;
   iLen:=Length(DNSDomain);
   SetLength(Result,0);
   if (iLen>0) and (Byte(DNSDomain[DIndex])=$c0)  then begin
     While (DIndex<iLen) do begin
       cByte:=Byte(DNSDomain[DIndex]);
       if (cByte<>$c0) then begin
         Result:=Concat(Result,System.Copy(DNSDomain,DIndex+1,cByte),'.');
         Inc(DIndex,cByte+1);
       end else
         Inc(DIndex);
     end;
     iLen:=System.Length(Result);
     If (iLen>0) and (Result[iLen]='.') then
       System.Delete(Result,iLen,1);
   end else
     Result:=System.Copy(DNSDomain,1,iLen)
end;
(*
Function  BufferIn(Var DNS:TDNSStruct;  Var Buffer:Core.Arrays.Bytes.Core.Arrays.Types.Bytes; Var iPosition:LongInt; Const iSize:LongInt):Boolean;
var
  sServer:String;
  ALcv,Lcv:LongInt;
  Q_Type:LongInt;
begin
   Reset(DNS);
   Result:=iSize>=System.SizeOf(TQHeader);
   If Result then begin
     System.Move(Buffer[iPosition],DNS.Header,System.SizeOf(TQHeader));
     Inc(iPosition,System.SizeOf(TQHeader));

     DNS.Header.ID:=ntohs(DNS.Header.ID);
     DNS.Header.Options:=nTohs(DNS.Header.Options);
     DNS.Header.QDCount:=nTohs(DNS.Header.QDCount);
     DNS.Header.Ancount:=nTohs(DNS.Header.Ancount);
     DNS.Header.NsCount:=nTohs(DNS.Header.NsCount);
     DNS.Header.ARCount:=nTohs(DNS.Header.ARCount);
     //    QD COUNT
     Q_Type:=QT_NONE;
     Lcv:=Low(DNS.Questions);
     While (Lcv<DNS.Header.QDCount) and (Lcv<MAX_QUESTIONS) and not DNS.Error and (iPosition<iSize) do begin
       SetLength(DNS.Questions,Lcv+1);
       DNS.Questions[Lcv].Q_Name:=BufferInDNSDomain(Buffer,iPosition);
       If iPosition+4<=iSize then begin
         System.Move(Buffer[iPosition],DNS.Questions[Lcv].Q_Type,2);
         Inc(iPosition,2);
         System.Move(Buffer[iPosition],DNS.Questions[Lcv].Q_Class,2);
         Inc(iPosition,2);
         DNS.Questions[Lcv].Q_Type:=nTohs(DNS.Questions[Lcv].Q_Type);
         DNS.Questions[Lcv].Q_Class:=nTohs(DNS.Questions[Lcv].Q_Class);
         Q_Type:=DNS.Questions[Lcv].Q_Type;
       end else begin
         DNS.Error:=True;
         Result:=False;
       end;
       Inc(Lcv);
     end;
     //    AN COUNT
     ALcv:=0;
     Lcv:=Low(DNS.AN_Data);
     While (Lcv<DNS.Header.ANCount) and (not DNS.Error) and (iPosition<iSize) do begin
        SetLength(DNS.AN_Data,Lcv+1);
        BufferInResourceRecord(DNS.Error,Lcv,DNS.Header.ANCount,iPosition,Buffer,iSize,DNS.AN_Data[Lcv]);
        If DNS.AN_Data[Lcv].RType=Q_Type then begin
           sServer:=RRDomainToDomain(DNS.AN_Data[Lcv].RType,DNS.AN_Data[Lcv].RData);
           If sServer<>'' then begin
             SetLength(DNS.Answers,ALcv+1);
             DNS.Answers[ALcv]:=sServer;
             Inc(ALcv);
           end;
        end;
        Inc(Lcv);
     end;
     //    NS COUNT
     Lcv:=Low(DNS.NS_Data);
     While (Lcv<DNS.Header.NSCount) and not DNS.Error and (iPosition<iSize) do begin
       SetLength(DNS.NS_Data,Lcv+1);
       BufferInResourceRecord(DNS.Error,Lcv,DNS.Header.NSCount,iPosition,Buffer,iSize,DNS.NS_Data[Lcv]);
       Inc(Lcv);
     end;
     //    AR COUNT
     Lcv:=Low(DNS.AD_Data);
     While (Lcv<DNS.Header.ARCount) and not DNS.Error and (iPosition<iSize) do begin
       SetLength(DNS.AD_Data,Lcv+1);
       BufferInResourceRecord(DNS.Error,Lcv,DNS.Header.ARCount,iPosition,Buffer,iSize,DNS.AD_Data[Lcv]);
       Inc(Lcv);
     end;
   end else
     Result:=False;
end;
*)
Function GetQR(Var Header:TQHeader):Boolean;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=(Header.Options or QHMC_QR)=Header.Options;
end;

Function GetOpCode(Var Header:TQHeader):Byte;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=(Header.Options and QHMC_OPCODE) shr 11;
end;

Function GetAA(Var Header:TQHeader):Boolean;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=(Header.Options or QHMC_AA)=Header.Options;
end;

Function GetTC(Var Header:TQHeader):Boolean;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=(Header.Options or QHMC_TC)=Header.Options;
end;

Procedure SetRD(Var Header:TQHeader; Value:Boolean);
Var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=0;  If Value then wValue:=QHMC_RD;
   Header.Options:=Header.Options and not QHMC_RD or wValue;
end;


Function GetRD(Var Header:TQHeader):Boolean;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=(Header.Options or QHMC_RD)=Header.Options;
end;

Function GetRA(Var Header:TQHeader):Boolean;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=(Header.Options or QHMC_RA)=Header.Options;
end;

Function GetZ(Var Header:TQHeader):Byte;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
                                 {3 Bit Val}
   Result:=Header.Options and QHMC_Z shr 4;
end;

Function GetRCode(Var Header:TQHeader):Byte;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=Header.Options and QHMC_RCODE;
end;

(*

Procedure TQueryHeader.SetQR(Const Value:Boolean);
Var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=0;  If Value then wValue:=QHMC_QR;
   HeaderData.Options:=HeaderData.Options and not QHMC_QR or wValue;
end;

Procedure TQueryHeader.SetOpCode(Const Value:Byte);
var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=(Value and not 240) shl 11;
   HeaderData.Options:=HeaderData.Options and not QHMC_OPCODE or wValue;
end;

Procedure TQueryHeader.SetAA(Const Value:Boolean);
Var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=0;  If Value then wValue:=QHMC_AA;
   HeaderData.Options:=HeaderData.Options and not QHMC_AA or wValue;
end;

Procedure TQueryHeader.SetTC(Const Value:Boolean);
Var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=0;  If Value then wValue:=QHMC_TC;
   HeaderData.Options:=HeaderData.Options and not QHMC_TC or wValue;
end;


Procedure TQueryHeader.SetRA(Const Value:Boolean);
Var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=0;  If Value then wValue:=QHMC_RA;
   HeaderData.Options:=HeaderData.Options and not QHMC_RA or wValue;
end;

Procedure TQueryHeader.SetZ(Const Value:Byte);
Var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=(Value and not 248) shl 4;
   HeaderData.Options:=HeaderData.Options and not QHMC_Z or wValue;
end;

Procedure TQueryHeader.SetRCode(Const Value:Byte);
var
  wValue:Word;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   wValue:=(Value and not 240);
   HeaderData.Options:=HeaderData.Options and not QHMC_RCODE or wValue;
end;



procedure TDNSQuery.BufferOut(Var Buffer:TByteBuffer);
Const
  TTL:LongInt=0;
var
  iRead,iLength:LongInt;
  Hdr:TQHeader;
  dWVal:Word;
begin
  iLength:=0;
  Hdr.ID:=ccWinsock.hToNs(FQH.ID);
  Hdr.Options:=hToNs(FQH.Options);
  Hdr.QDCount:=hToNs(FQH.QDCount);
  Hdr.ANCount:=hToNs(FQH.ANCount);
  Hdr.NSCount:=hToNs(FQH.NSCount);
  Hdr.ARCount:=hToNs(FQH.ARCount);

  iRead:=SizeOf(Hdr);
  SetLength(Buffer,iLength+iRead);
  CopyMemory(@Buffer[iLength],@HDr,iRead);
  Inc(iLength,iRead);

  iRead:=Length(FQQ.QNAME);
  SetLength(Buffer,iLength+iRead);
  CopyMemory(@Buffer[iLength],@FQQ.QName[1],iRead);
  Inc(iLength,iRead);

  iRead:=1;
  SetLength(Buffer,iLength+iRead);
  Buffer[iLength]:=0;
  Inc(iLength,iRead);

  iRead:=2;
  dWVal:=hToNs(FQQ.QType);
  SetLength(Buffer,iLength+iRead);
  CopyMemory(@Buffer[iLength],@dwVal,2);
  Inc(iLength,iRead);

  dWVal:=hToNs(FQQ.QClass);
  SetLength(Buffer,iLength+iRead);
  CopyMemory(@Buffer[iLength],@dwVal,2);
  Inc(iLength,iRead);

  iRead:=4;
  SetLength(Buffer,iLength+iRead);
  CopyMemory(@Buffer[iLength],@TTL,4);
  Inc(iLength,iRead);

  dWVal:=hToNs(0);
  iRead:=2;
  SetLength(Buffer,iLength+iRead);
  CopyMemory(@Buffer[iLength],@dwVal,2);
  Inc(iLength,iRead);
end;

procedure TDNSQuery.StreamOut(Stream:TStream);
Const
  TTL:LongInt=0;
var
  Hdr:TQHeader;
  EOS:Char;
  dWVal:Word;
begin
  Hdr.ID:=hToNs(FQH.ID);
  Hdr.Options:=hToNs(FQH.Options);
  Hdr.QDCount:=hToNs(FQH.QDCount);
  Hdr.ANCount:=hToNs(FQH.ANCount);
  Hdr.NSCount:=hToNs(FQH.NSCount);
  Hdr.ARCount:=hToNs(FQH.ARCount);
  Stream.WriteBuffer(Hdr,SizeOf(Hdr));
  Stream.WriteBuffer(FQQ.QName[1],Length(FQQ.QName));
  EOS:=#0; Stream.WriteBuffer(EOS,1);
  dWVal:=hToNs(FQQ.QType); Stream.WriteBuffer(dWVal,2);
  dWVal:=hToNs(FQQ.QClass); Stream.WriteBuffer(dWVal,2);
  Stream.WriteBuffer(TTL,4);
  dWVal:=hToNs(0); Stream.WriteBuffer(dWVal,2);
end;

Function  TDNSQuery.BufferIn(Var Buffer:TByteBuffer):Boolean;
var
  Stream:TMemoryStream;
begin
  Stream:=TMemoryStream.Create;
  Try
    Stream.WriteBuffer(Buffer[0],Length(Buffer));
    Stream.Seek(0,SoFromBeginning);
    Result:=StreamIn(Stream);
  Finally
    FreeAndNil(Stream);
  end;
end;

Function TDNSQuery.StreamIn(Stream:TStream):Boolean;
var
  ALcv,Lcv:LongInt;
begin
   Reset;
   Stream.Position:=0;
   For Lcv:=Low(FQH.Servers) to High(FQH.Servers) do
     FQH.Servers[Lcv]:='';

   Result:=Stream.Size>=SizeOf(FQH.HeaderData);
   If Result then begin
     Stream.ReadBuffer(FQH.HeaderData,SizeOf(FQH.HeaderData));

     FQH.HeaderData.ID:=ntohs(FQH.HeaderData.ID);
     FQH.HeaderData.Options:=nTohs(FQH.HeaderData.Options);
     FQH.HeaderData.QDCount:=nTohs(FQH.HeaderData.QDCount);
     FQH.HeaderData.Ancount:=nTohs(FQH.HeaderData.Ancount);
     FQH.HeaderData.NsCount:=nTohs(FQH.HeaderData.NsCount);
     FQH.HeaderData.ARCount:=nTohs(FQH.HeaderData.ARCount);

     FQQ.QName:=StreamInDNSDomain(Stream);
     If Stream.Position+4<=Stream.Size then begin
       Stream.ReadBuffer(FQQ.QType,2);
       Stream.ReadBuffer(FQQ.QClass,2);
       FQQ.QType:=nTohs(FQQ.QType);
       FQQ.QClass:=nTohs(FQQ.QClass);
       Lcv:=Low(FQA.QDData);
       ALcv:=Low(FQH.Servers);
       While (Lcv<=FQH.QDCount) and Not Error and (Lcv<=High(FQA.QDData)) and (Stream.Position<Stream.Size) do begin
          FQA.QDData[Lcv]:=StreamInResourceRecord(Error,Lcv,FQH.QDCount,Stream);
          If FQA.QDData[Lcv].RType=FQQ.QType then begin
             FQH.Servers[ALcv]:=RRDomainToDomain(FQA.QDData[Lcv].RType,FQA.QDData[Lcv].RData);
             If FQH.Servers[ALcv]<>'' then
               Inc(ALcv);
          end;
          Inc(Lcv);
       end;
       Lcv:=Low(FQA.ANData);
       While (Lcv<=FQH.ANCount) and not Error and (Lcv<=High(FQA.ANData)) and (ALcv<=High(FQH.Servers)) and (Stream.Position<Stream.Size) do begin
          FQA.ANData[Lcv]:=StreamInResourceRecord(Error,Lcv,FQH.ANCount,Stream);
          If FQA.ANData[Lcv].RType=FQQ.QType then begin
             FQH.Servers[ALcv]:=RRDomainToDomain(FQA.ANData[Lcv].RType,FQA.ANData[Lcv].RData);
             If FQH.Servers[ALcv]<>'' then
               Inc(ALcv);
          end;
          Inc(Lcv);
       end;
       FQH.HeaderData.Ancount:=ALcv-1;
       Lcv:=Low(FQA.NSData);
       While (Lcv<=FQH.NSCount) and not Error and (Lcv<=High(FQA.NSData)) and (Stream.Position<Stream.Size) do begin
         FQA.NSData[Lcv]:=StreamInResourceRecord(Error,Lcv,FQH.NSCount,Stream);
         Inc(Lcv);
       end;
         {
         Lcv:=Low(FQA.NSData);
         While (Lcv<=FQH.NSCount) and (Lcv<=High(FQA.NSData)) and (ALcv<=High(FQH.FServers)) and (Stream.Position<Stream.Size) do begin
            FQA.NSData[Lcv]:=StreamInResourceRecord(Stream);
            If FQA.NSData[Lcv].RType=FQQ.QType then begin
               FQH.FServers[ALcv]:=RRDomainToDomain(FQA.NSData[Lcv].RType,FQA.NSData[Lcv].RData);
               If FQH.FServers[ALcv]<>'' then
                 Inc(ALcv);
            end;
            Inc(Lcv);
         end;
         }

     end else
       Result:=False;
   end else
     Result:=False;
end;
*)

Constructor   TDNSObject.Create;
var
  iLcv:LongInt;
begin
  {$i RSR.DNS.Init.inc}
  Inherited Create;
end;

Destructor    TDNSObject.Destroy;
begin
  Done(FCache);
  Done(Answers);
  Done(Questions);
  Done(NS_Data);
  Done(QD_Data);
  Done(AN_Data);

  Inherited Destroy;
end;

procedure     TDNSObject.Reset;
var
  iLcv:LongInt;
begin
  {$i RSR.DNS.Init.inc}
end;

function   TDNSObject.checkCache(sValue:Core.Strings.VarString):PQCache;
var
  iLcv:LongInt;
  ReIndex:Boolean;
begin
  Result:=nil;  ReIndex:=false;
  for iLcv:=0 to high(FCache) do begin
    if (FCache[iLcv]<>nil) then begin
      if (Core.Timer.dtUT>=FCache[iLcv]^.Expires) then begin
        Done(FCache[iLcv]^);
        Dispose(FCache[iLcv]);
        FCache[iLcv]:=nil;
        ReIndex:=true;
      end else if (FCache[iLcv]^.Kind=Questions[0].Q_Type) and SameText(FCache[iLcv]^.Input,sValue) then begin
        Result:=FCache[iLcv];
        Core.Arrays.VarString.Copy(FCache[iLcv]^.Answers,Answers);
        Header.Ancount:=System.Length(Answers);
        break;
      end;
    end else
      ReIndex:=true;
  end;
  if ReIndex=true then
    reIndexCache();
end;

function      TDNSObject.checkCache(Query:Core.Strings.VarString; Kind:Word):PQCache;
var
  iLcv:LongInt;
  ReIndex:Boolean;
begin
  Result:=nil;  ReIndex:=false;
  for iLcv:=0 to high(FCache) do begin
    if (FCache[iLcv]<>nil) then begin
      if Core.Timer.dtUT>=FCache[iLcv]^.Expires then begin
        Done(FCache[iLcv]^);
        Dispose(FCache[iLcv]);
        FCache[iLcv]:=nil;
        ReIndex:=true;
      end else if (FCache[iLcv]^.Kind=Kind) and SameText(FCache[iLcv]^.Input,Query) then begin
        Result:=FCache[iLcv];
        Core.Arrays.VarString.Copy(FCache[iLcv]^.Answers,Answers);
        Header.Ancount:=System.Length(Answers);
        break;
      end;
    end else
      ReIndex:=true;
  end;
  if ReIndex=true then
    reIndexCache();
end;

procedure     TDNSObject.Cache();
var
  iCt:LongInt;
  cacheP:PQCache;
begin
  iCt:=System.Length(FCache);
  if (iCt>=MAX_CACHE) then begin
    cacheP:=FCache[FCacheWIX];
    Inc(FCacheWIX);
    if FCacheWIX>iCt then
       FCacheWIX:=0;
  end else begin
    new(cacheP);
    Init(cacheP^);
    SetLength(FCache,iCt+1);
    FCache[iCt]:=cacheP;
  end;
  cacheP^.Expires:=DateUtils.IncMilliSecond(Core.Timer.dtUT,DNS_CACHE_TTL);
  cacheP^.Input:=DNSDomainToDomain(Questions[0].Q_Name);
  cacheP^.Kind:=Questions[0].Q_Type;
  Core.Arrays.VarString.Copy(Answers,cacheP^.Answers);
end;

function     TDNSObject.BufferOut(Var Buffer:Core.Arrays.Types.Bytes):LongInt;
Const
  TTL:LongInt=0;
var
  iRead,iLength:LongInt;
  Hdr:TQHeader;
  dWVal:Word;
begin
  iLength:=0;
  Hdr.ID:=hToNs(Header.ID);
  Hdr.Options:=hToNs(Header.Options);
  Hdr.QDCount:=hToNs(Header.QDCount);
  Hdr.ANCount:=hToNs(Header.ANCount);
  Hdr.NSCount:=hToNs(Header.NSCount);
  Hdr.ARCount:=hToNs(Header.ARCount);

  iRead:=System.SizeOf(Hdr);
  SetLength(Buffer,iLength+iRead);
  System.Move(HDr,Buffer[iLength],iRead);
  Inc(iLength,iRead);

  iRead:=Length(Questions[0].Q_NAME);
  SetLength(Buffer,iLength+iRead);
  System.Move(Questions[0].Q_Name[1],Buffer[iLength],iRead);
  Inc(iLength,iRead);

  iRead:=1;
  SetLength(Buffer,iLength+iRead);
  Buffer[iLength]:=0;
  Inc(iLength,iRead);

  iRead:=2;
  dWVal:=hToNs(Questions[0].Q_Type);
  SetLength(Buffer,iLength+iRead);
  System.Move(dwVal,Buffer[iLength],2);
  Inc(iLength,iRead);

  dWVal:=hToNs(Questions[0].Q_Class);
  SetLength(Buffer,iLength+iRead);
  System.Move(dwVal,Buffer[iLength],2);
  Inc(iLength,iRead);

  iRead:=4;
  SetLength(Buffer,iLength+iRead);
  System.Move(TTL,Buffer[iLength],4);
  Inc(iLength,iRead);

  dWVal:=hToNs(0);
  iRead:=2;
  SetLength(Buffer,iLength+iRead);
  System.Move(dwVal,Buffer[iLength],2);
  Inc(iLength,iRead);

  Result:=System.Length(Buffer);
end;

function     TDNSObject.StreamOut(Stream:TStream):Int64;
Const
  TTL:LongInt=0;
  TERM:Byte=0;
var
  Hdr:TQHeader;
  iRead:LongInt;
begin
  Result:=Stream.Size;

  Hdr.ID:=hToNs(Header.ID);
  Hdr.Options:=hToNs(Header.Options);
  Hdr.QDCount:=hToNs(Header.QDCount);
  Hdr.ANCount:=hToNs(Header.ANCount);
  Hdr.NSCount:=hToNs(Header.NSCount);
  Hdr.ARCount:=hToNs(Header.ARCount);
  Stream.Write(HDR,System.SizeOf(Hdr));

  iRead:=Length(Questions[0].Q_NAME);
  if iRead>0 then
    Stream.Write(Questions[0].Q_NAME[1],iRead);
  Stream.Write(TERM,1);
  Stream.Write(hToNs(Questions[0].Q_Type),2);
  Stream.Write(hToNs(Questions[0].Q_Class),2);
  Stream.Write(TTL,4);
  Stream.Write(hToNs(0),2);

  Result:=Stream.Size-Result;
end;

procedure    TDNSObject.reIndexCache();
var
  iLcv,jLcv:LongInt;
  iCount:LongInt;
begin
  iLcv:=0;
  While iLcv<Length(FCache) do begin
    If FCache[iLcv]=nil then begin
      iCount:=System.Length(FCache);
      for jLcv:=iLcv to iCount-2 do
        FCache[jLcv]:=FCache[jLcv+1];
      System.SetLength(FCache,iCount-1);
    end else
      Inc(iLcv);
  end;
end;

function     TDNSObject.GetRCode():Byte;
begin
  {+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
   |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+}
   Result:=Header.Options and QHMC_RCODE;
end;

function      TDNSObject.StreamIn(Stream:TStream; Var iPosition:Cardinal; Const iSize:LongInt):Boolean; overload;
{$i RSR.DNS.StreamIn.inc}
end;

function      TDNSObject.StreamIn(Stream:TStream; Var iPosition:System.Int64; Const iSize:LongInt):Boolean; overload;
{$i RSR.DNS.StreamIn.inc}
end;

initialization
  SyncSystemDNS;
finalization
 Empty(System_DNS);
end.
end.


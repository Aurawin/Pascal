{
***************************************************
* A binary compatible SHA1 implementation         *
* written by Dave Barton (davebarton@bigfoot.com) *
***************************************************
* 160bit hash size                                *
***************************************************
}
unit Encryption.SHA;

interface
uses

  Core.Strings,
  Core.Arrays.Types,
  Core.Arrays.Bytes,

  Encryption,
  Encryption.Tools,
  Sysutils;
type
  TSHA1Digest  = Core.Arrays.Types.Bytes20;
  TSHA1Context = record
    LenW       : System.Int64;
    BitCount   : System.Int64;
    m_h        : Array [0..4] of System.Cardinal;
    m_w        : Array [0..79] of System.Cardinal;
  end;

procedure Init  (var Context: TSHA1Context);
procedure Update(Var Context: TSHA1Context; Buffer:Core.Strings.VarString); overload;
procedure Update(var Context: TSHA1Context; Buffer: System.Pointer); overload;
procedure Update(Var Context: TSHA1Context; Buffer:System.Pointer; Length:LongInt); overload;
procedure Final (var Context: TSHA1Context; var Digest: TSHA1Digest);
function  Print(const Digest: TSHA1Digest): Core.Strings.VarString;

//******************************************************************************
implementation


{$OVERFLOWCHECKS OFF}
//******************************************************************************
function F1(x, y, z: Cardinal): Cardinal;
begin
  Result:= z xor (x and (y xor z));
end;
function F2(x, y, z: Cardinal): Cardinal;
begin
  Result:= x xor y xor z;
end;
function F3(x, y, z: Cardinal): Cardinal;
begin
  Result:= (x and y) or (z and (x or y));
end;

//******************************************************************************
function RB(A: Cardinal): Cardinal;
begin
  Result:= (A shr 24) or ((A shr 8) and $FF00) or ((A shl 8) and $FF0000) or (A shl 24);
end;

//******************************************************************************
procedure Init(var Context: TSHA1Context);
var
 iLcv:LongInt;
begin
  Context.LenW:=0;
  Context.BitCount:=0;
  For iLcv:=0 to High(Context.m_w) do
    Context.M_W[iLcv]:=0;
  For iLcv:=0 to High(Context.m_h) do
    Context.m_h[iLcv]:=0;
	Context.m_h[0]:=$67452301;
	Context.m_h[1]:=$EFCDAB89;
	Context.m_h[2]:=$98BADCFE;
	Context.m_h[3]:=$10325476;
	Context.m_h[4]:=$C3D2E1F0;
end;

// -----------------------------------------------------------------
// SHA rotate left.
// -----------------------------------------------------------------
Function shaRotl(X,N:LongInt):LongInt;
begin
  Result:=(X shl N) or (X shr (32-N));
  //Result:=LRot32(x,N);
end;


procedure Sha1HashBlock(Var Context:TSHA1Context);
var
	iA,iB,iC,iD,iE,iTemp:LongInt;
  t:LongInt;
begin
    For t:=16 to 79 do
			Context.m_w[t]:= shaRotl (Context.m_w[t-3] xor Context.m_w[t-8] xor Context.m_w[t-14] xor Context.m_w[t-16], 1);

		iA:= Context.m_h[0];
 		iB:= Context.m_h[1];
		iC:= Context.m_h[2];
		iD:= Context.m_h[3];
		iE:= Context.m_h[4];

		// -----Round 1
    t:=0;
    While t<20 do begin
    	iTemp:= shaRotl(iA, 5) + (((iC xor iD) and iB) xor iD) + iE + Context.m_w[t] + $5a827999;
			iE:=iD;
			iD:=iC;
			iC:=shaRotl (iB, 30);
			iB:=iA;
			iA:=iTemp;
      inc(t);
		end;

		// -----Round 2
    t:=20;
    While t<40 do begin
			iTemp:= shaRotl (iA, 5) + (iB xor iC xor iD) + iE + Context.m_w[t] + $6ed9eba1;
			iE:=iD;
			iD:=iC;
			iC:=shaRotl (iB, 30);
			iB:=iA;
			iA:=iTemp;
      Inc(t);
		end;

		// -----Round 3
    t:=40;
    While t<60 do begin
			iTemp:= shaRotl (iA, 5) + ((iB and iC) or (iD and (iB or iC))) + iE + Context.m_w[t] + $8f1bbcdc;
			iE:=iD;
			iD:=iC;
			iC:=shaRotl (iB, 30);
			iB:=iA;
			iA:=iTemp;
      Inc(t);
    end;

		// -----Round 4
    t:=60;
    While t<80 do begin
			iTemp:= shaRotl (iA, 5) + (iB xor iC xor iD) + iE + Context.m_w[t] + $ca62c1d6;
			iE:= iD;
			iD:= iC;
			iC:= shaRotl (iB, 30);
			iB:= iA;
			iA:= iTemp;
      Inc(t);
		end;
		Inc(Context.m_h[0],iA);
		Inc(Context.m_h[1],iB);
		Inc(Context.m_h[2],iC);
		Inc(Context.m_h[3],iD);
		Inc(Context.m_h[4],iE);
end;

procedure Update(Var Context: TSHA1Context; Buffer:Core.Strings.VarString); overload;
begin
  If Length(Buffer)>0 then
    Update(Context,@Buffer[1],Length(Buffer));
end;

procedure Update(Var Context:TSHA1Context; Buffer:System.Pointer); overload;
begin
  Update(Context, Buffer, Length(Core.Arrays.Types.PBytes(Buffer)^));
end;

procedure Update(Var Context:TSHA1Context; Buffer:System.Pointer; Length:LongInt); overload;
type
  PByte= ^Byte;
begin
  While Length>0 do begin
    Context.m_w[Context.LenW div 4]:= Context.m_w[Context.LenW div 4] shl 8;
    Context.m_w[Context.LenW div 4]:= Context.m_w[Context.LenW div 4] or  (PByte(Buffer)^ and $FF);
    Inc(Context.LenW);
    If Context.LenW mod 64 = 0 then begin
      SHA1HashBlock(Context);
      Context.LenW:=0;
    end;
    Inc(Context.BitCount,8);
    Inc(PByte(Buffer));
    Dec(Length);
	end;
end;

// -----------------------------------------------------------------
// Complete the hash and get the final digest (resets internal state).
// -----------------------------------------------------------------
procedure Final (var Context: TSHA1Context; var Digest: TSHA1Digest);
var
  PadLen:Array[0..7] of System.byte;
  Pad00:Array[0..0] of System.Byte;
  Pad80:Array[0..0] of System.Byte;
  i:LongInt;
begin
  Pad00[0]:=$00;
  Pad80[0]:=$80;
  padlen[0]:=((Context.BitCount shr 56) and $FF);
  padlen[1]:=((Context.BitCount shr 48) and $FF);
  padlen[2]:=((Context.BitCount shr 40) and $FF);
  padlen[3]:=((Context.BitCount shr 32) and $FF);
  padlen[4]:=((Context.BitCount shr 24) and $FF);
  padlen[5]:=((Context.BitCount shr 16) and $FF);
  padlen[6]:=((Context.BitCount shr 8) and $FF);
  padlen[7]:=((Context.BitCount shr 0) and $FF);

  Update(Context,@Pad80,1);

  While(Context.LenW<>56) do
    Update(Context,@Pad00,1);

  Update(Context,@Padlen,8);

  For i:=0 to 19 do begin
    Digest[i]:=Context.m_H[i div 4] shr 24;
    Context.m_h[i div 4]:=Context.m_h[i div 4] shl 8;
  end;
  Init(Context);
end;

function Print(const Digest: TSHA1Digest): Core.Strings.VarString;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, 40);
  P := Pointer(Result);
  for I := 0 to 19 do
  begin
    P[0] := HexTbl[(Digest[i] shr 4) and 15];
    P[1] := HexTbl[Digest[i] and 15];
    Inc(P,2);
  end;
end;
end.

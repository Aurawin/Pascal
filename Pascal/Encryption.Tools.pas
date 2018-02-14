unit Encryption.Tools;


interface


uses

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes;


  function LRot16(X: System.word; c: System.LongInt): System.word; assembler;
  function RRot16(X: System.word; c: System.LongInt): System.word; assembler;
  function LRot32(X: System.Cardinal; c: System.LongInt): System.Cardinal; assembler;
  function RRot32(X: System.Cardinal; c: System.LongInt): System.Cardinal; assembler;
  procedure XorBlock(I1, I2, O1: Core.Arrays.Types.PBytes; Len: System.LongInt);
  procedure IncBlock(P: Core.Arrays.Types.PBytes; Len: System.LongInt);

implementation

{$ASMMODE INTEL}


function LRot16(X: System.word; c: System.LongInt): System.word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  rol ax,cl
  mov &Result,ax
end;

function RRot16(X: System.word; c: System.LongInt): System.word; assembler;
asm
  mov ecx,&c
  mov ax,&X
  ror ax,cl
  mov &Result,ax
end;

function LRot32(X: System.Cardinal; c: System.LongInt): System.Cardinal; assembler;
asm
  mov ecx, edx
  rol eax, cl
end;

function RRot32(X: System.Cardinal; c: System.LongInt): System.Cardinal; assembler;
asm
  mov ecx, edx
  ror eax, cl
end;

procedure XorBlock(I1, I2, O1: Core.Arrays.Types.PBytes; Len: System.LongInt);
var
  i:LongInt;
begin
  for i:= 0 to Len-1 do
    O1^[i]:= (I1^[i] xor I2^[i]);
end;

procedure IncBlock(P: Core.Arrays.Types.PBytes; Len: System.LongInt);
begin
  Inc(P^[Len-1]);
  if (P^[Len-1]= 0) and (Len> 1) then
    IncBlock(P,Len-1);
end;

end.

 



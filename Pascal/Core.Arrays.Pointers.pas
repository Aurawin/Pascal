unit Core.Arrays.Pointers;

interface

uses
  Core.Arrays,
  Core.Arrays.Types;

  procedure Copy(var Source,Destination:Core.Arrays.Types.Pointers); overload;
  procedure Empty(Var Item:Core.Arrays.Types.Pointers); overload;
  procedure Add(Var List:Core.Arrays.Types.Pointers; Item:System.Pointer; Const Option:AddOption=aoNone); overload;
  procedure Remove(Var List:Core.Arrays.Types.Pointers; Item:System.Pointer); overload;
  function  IndexOf(Item:System.Pointer; var List:Core.Arrays.Types.Pointers):LongInt; overload;
  procedure Done(Var List:Core.Arrays.Types.Pointers); overload;

implementation


procedure Empty(Var Item:Core.Arrays.Types.Pointers);
begin
  SetLength(Item,0);
end;

procedure Done(Var List:Core.Arrays.Types.Pointers);
begin
  Finalize(List);
end;

procedure Copy(var Source,Destination:Core.Arrays.Types.Pointers);
var
  iLen:LongInt;
  iLcv:LongInt;
begin
  iLen:=Length(Source);
  SetLength(Destination,iLen);
  for iLcv:=0 to iLen-1 do
    Destination[iLcv]:=Source[iLcv];
end;

procedure Remove(Var List:Core.Arrays.Types.Pointers; Item:System.Pointer);
var
  idx:LongInt;
  iLcv:LongInt;
  iCt:LongInt;
begin
  idx:=IndexOf(Item,List);
  if (idx>-1) then begin
    iCt:=System.length(List);
    for iLcv:=iLcv to iCt-1 do
      List[iLcv]:=List[iCt+1];
    System.SetLength(List,iCt-1);
  end;
end;

procedure Add(Var List:Core.Arrays.Types.Pointers; Item:System.Pointer; Const Option:AddOption=aoNone);
var
  iLen:LongInt;
begin
  iLen:=System.Length(List);
  SetLength(List,iLen+1);
  List[iLen]:=Item;
end;

function  IndexOf(Item:System.Pointer; var List:Core.Arrays.Types.Pointers):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(List) do begin
    if List[iLcv]=Item then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

procedure Remove(Var List:TPointerArray; Item:Pointer);
begin

end;
end.


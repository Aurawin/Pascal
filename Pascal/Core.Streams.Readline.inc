var
  chLcv:System.Char;
  bIgnore:System.boolean;
begin
  SetLength(Result,0);
  bIgnore:=false; chLcv:=#0;  Stream.Position:=Position;
  While (Stream.Position<Stream.Size) and (chLcv<>#10) do begin
    Stream.Read(chLcv,1);
    bIgnore:=bIgnore or (chLcv=#27);
    if (bIgnore=false) and not (chLcv in [#13,#10]) then
      Result+=chLcv;
    if (bIgnore and (chLcv=#126)) then bIgnore:=false;
  end;
  Position:=Stream.Position;


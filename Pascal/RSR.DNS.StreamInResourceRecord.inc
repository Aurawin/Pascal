var
  SLcv,RDLcv:LongInt;
  bSP,c,CheckByte : byte;
  iEndPos:Int64;
  procedure FillData;
  var
    StreamPos,SaveStreamPos:LongInt;
  begin
     if (c=192) and (Result.RType<>1) and (Stream.Position<Stream.Size) then begin
        Stream.ReadBuffer(bSP,1); Inc(SLcv);
        StreamPos:=bSP;
        SaveStreamPos:=Stream.Position;
        If StreamPos<Stream.Size then begin
            Stream.Position:=StreamPos;
            Repeat
               Stream.ReadBuffer(C,1);
               Result.RDATA[RDLcv]:=C;
               Inc(RDLcv);
            Until (c=192) or (C=0) or (RDLcv>=High(Result.RDATA)) or (Stream.Position>=Stream.Size);
            If C=192 then begin
              Dec(RDLcv);
              Result.RDATA[RDLcv]:=0;
              // Check for circular references...
              If Stream.Position<Stream.Size then begin
                Stream.ReadBuffer(CheckByte,1);
                If CheckByte<>StreamPos then begin
                  Stream.Position:=Stream.Position-1;
                  FillData;
                end;
              end;
            end;
            Stream.Position:=SaveStreamPos;
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
   Result.RName:=StreamInDNSDomain(Stream);
   Position:=Stream.Position;
   If Position<Stream.Size then begin
     Stream.ReadBuffer(Result.RType,2);
     Result.RType:=nTohs(Result.RType);
     Position:=Stream.Position;
     If Position<Stream.Size then begin
       Stream.ReadBuffer(Result.RClass,2);
       Result.RClass:=nTohs(Result.RClass);
       Position:=Stream.Position;
       If Position<Stream.Size then begin
         Stream.ReadBuffer(Result.TTL,4);
         Result.TTL:=nTohl(Result.TTL);
         Position:=Stream.Position;
         If Position<Stream.Size then begin
           Stream.ReadBuffer(Result.RDLength,2);
           Result.RDLength:=nTohs(Result.RDLength);
         end;
         Position:=Stream.Position;
         If Position<Stream.Size then begin
           iEndPos:=Position+Result.RDLength;
           FillByte(Result.RData[0],System.SizeOf(Result.RData),0);
           RDLcv:=Low(Result.RDATA);SLcv:=1;
           While (SLcv<=Result.RDLength) and (Position<iEndPos) and (RDLcv<=High(Result.RDATA)) and (Stream.Position<Stream.Size) do begin
              Stream.ReadBuffer(C,1);
              Inc(SLcv);
              Inc(Position);
              FillData;
           end;
           Position:=Stream.Position;
         end else
           bError:=True;
       end else
         bError:=True;
     end else
       bError:=True;
   end else
     bError:=True;
end;

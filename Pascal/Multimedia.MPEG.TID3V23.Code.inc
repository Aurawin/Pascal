procedure TID3V23TagHeader.Reset();
begin
  Inherited Reset();
end;

function TID3V23TagHeader.Frame:TID3V23TagFrame;
begin
  Result:=FReader.FID3V23;
end;

function TID3V23TagHeader.Load(Stream:TStream):boolean;
begin
  Result:=false;
  System.FillByte(hdrCRC,SizeOf(hdrCRC),0);
  System.FillByte(hdrExtended,SizeOf(hdrExtended),0);
  System.FillByte(hdrUpdate,SizeOf(hdrUpdate),0);
  System.FillByte(hdrRestrictions,SizeOf(hdrRestrictions),0);
  //FName:=FID;
end;

constructor TID3V23TagFrame.Create(aReader:TReader);
begin
  FHeader:=TID3V23TagHeader.Create(aReader);
  FPayload:=TID3V23TagPayload.Create(aReader);
  inherited Create(TFrameKind.fID3v23,aReader);
end;

function  TID3V23TagFrame.Header:TID3V23TagHeader;
begin
  Result:=TID3V23TagHeader(FReader.FID3V23.FHeader);
end;

procedure TID3V23TagFrame.Reset();
begin

end;

function TID3V23TagFrame.Load(Stream:TStream; var Handled:Boolean):boolean;
begin
  Result:=FHeader.Load(Stream);
  if Result then begin
    FPayLoad.FStreamStart:=Stream.Position;
    FPayLoad.FLength:=FHeader.FLength;
    FLength:=FHeader.FLength;
    FPayload.Load(Stream,Handled);
    if Handled=false then
      Stream.Position:=FPayLoad.FStreamStart+FPayLoad.FLength;
  end;
end;

function  TID3V23TagPayload.Header:TID3V23TagHeader;
begin
  Result:=TID3V23TagHeader(FReader.FID3V23.FHeader);
end;

procedure TID3V23TagPayload.Reset();
begin
end;

procedure TID3V23TagPayload.Load(Stream:TStream; var Handled:Boolean);
begin
  Handled:=false;
end;



















(*
function ID3.V23.TReader.getUnSynchronisation:Boolean;
begin
  Result:=(FData.Flags = (FData.Flags or 1 shl 7));
end;

function ID3.V23.TReader.getExtended:Boolean;
begin
  Result:=(FData.Flags=(FData.Flags or 1 shl 6));
end;

function ID3.V23.TReader.getExperimental:Boolean;
begin
  Result:=(FData.Flags=FData.Flags or 1 shl 5);
end;

function ID3.V23.TReader.getFooter:Boolean;
begin
  Result:=(FData.Flags=FData.Flags or 1 shl 4);
end;

function ID3.V23.TReader.getSize:Cardinal;
begin
  Result:=FData.Size[3] + FData.Size[2] shl 7 + FData.Size[1] shl 14 + FData.Size[0] shl 21;
end;

function ID3.V23.TReader.getExCRC:Cardinal;
begin
  Result:=FCRC.Data[4] + FCRC.Data[3] shl 7 + FCRC.Data[2] shl 14 + FCRC.Data[1] shl 21 + FCRC.Data[0] shl 28;
end;

function ID3.V23.TReader.getExUpdate:boolean;
begin
  Result:=( FExtended.Flags or (1 shl 6) = FExtended.Flags);
end;

function ID3.V23.TReader.getExCRCPresent:boolean;
begin
  Result:=( FExtended.Flags or (1 shl 5) = FExtended.Flags);
end;

function ID3.V23.TReader.getExRestrictions:boolean;
begin
  Result:=( FExtended.Flags or (1 shl 4) = FExtended.Flags);
end;

function ID3.V23.TReader.getSizeRestriction:TSizeRestriction;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FRestrictions.Data[0];
  Mask:=(1 shl 7) or (1 shl 6);
  Value:=(Value and Mask) shr 6;
  {                       76543210
  Restrictions           %ppqrrstt
  }
  Case Value of
   0: Result:=sr128frames1MBtag;
   1: Result:=sr64frames128kbtag;
   2: Result:=sr32frames40kbtag;
   3: Result:=sr32frames4kbtag;
  end;
end;

function ID3.V23.TReader.getTextEncodingRestriction:TTextEncodingRestriction;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FRestrictions.Data[0];
  Mask:=(1 shl 5);
  Value:=(Value and Mask) shr 5;
  Case Value of
   0: Result:=erNone;
   1: Result:=erStandards;
  end;
  {                       76543210
  Restrictions           %ppqrrstt
  }
end;

function ID3.V23.TReader.getTextSizeRestriction:TTextSizeRestriction;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FRestrictions.Data[0];
  Mask:=(1 shl 4) or (1 shl 3);
  Value:=(Value and Mask) shr 3;
  {                       76543210
  Restrictions           %ppqrrstt
  00   No restrictions
  01   No string is longer than 1024 characters.
  10   No string is longer than 128 characters.
  11   No string is longer than 30 characters.
  }
  Case Value of
   0: Result:=tsrNoLimit;
   1: Result:=tsr1024maxChars;
   2: Result:=txr128maxChars;
   3: Result:=txr30Chars;
  end;
end;

function ID3.V23.TReader.getImageRestriction:TImageRestriction;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FRestrictions.Data[0];
  Mask:=(1 shl 2);
  Value:=(Value and Mask) shr 2;
  {                       76543210
  Restrictions           %ppqrrstt
     0   No restrictions
     1   Images are encoded only with PNG [PNG] or JPEG [JFIF].
  }
  Case Value of
    0: Result:=irNone;
    1: Result:=irPNGorJPEG;
  end;
end;

function ID3.V23.TReader.getImageSizeRestriction:TImageSizeRestriction;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FRestrictions.Data[0];
  Mask:=(1 shl 1) or (1 shl 0);
  Value:=(Value and Mask);
  {                       76543210
  Restrictions           %ppqrrstt
    00  No restrictions
    01  All images are 256x256 pixels or smaller.
    10  All images are 64x64 pixels or smaller.
    11  All images are exactly 64x64 pixels, unless required otherwise.
  }
  Case Value of
   0: Result:=isrNone;
   1: Result:=isr256x256OrLess;
   2: Result:=isr64x64OrLess;
   3: Result:=isr64x64;
  end;
end;

function ID3.V23.TReader.getFrameID:string;
begin
  SetLength(Result,SizeOf(FFrameHeader.ID));
  System.Move(FFrameHeader.ID[0],Result[1],SizeOf(FFrameHeader.ID));
end;

function ID3.V23.TReader.getFrameSize:Cardinal;
begin
  Result:=FFrameHeader.Size[3] + FFrameHeader.Size[2] shl 7 + FFrameHeader.Size[1] shl 14 + FFrameHeader.Size[0] shl 21;
end;

function ID3.V23.TReader.getFrameTagAlterPreservation:TFramePreservation;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[0];
  Mask:=(1 shl 6);
  Value:=(Value and Mask) shr 6;
 {
    76543210  76543210
   %0abc0000 %0h00kmnp
   a - Tag altered preservation

   0     Frame should be preserved.
   1     Frame should be discarded.

 }
  Case Value of
    0: Result:=ftPreserveFrame;
    1: Result:=ftDiscardFrame;
  end;
end;

function ID3.V23.TReader.getFrameFileAlterPreservation:TFramePreservation;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[0];
  Mask:=(1 shl 5);
  Value:=(Value and Mask) shr 5;
 {
    76543210  76543210
   %0abc0000 %0h00kmnp
   b - File altered preservation

   0     Frame should be preserved.
   1     Frame should be discarded.

 }
  Case Value of
    0: Result:=ftPreserveFrame;
    1: Result:=ftDiscardFrame;
  end;
end;

function ID3.V23.TReader.getFrameReadOnly:Boolean;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[0];
  Mask:=(1 shl 4);
  Value:=(Value and Mask) shr 4;
 {
    76543210  76543210
   %0abc0000 %0h00kmnp
   c - Read Only

   0     Frame should be preserved.
   1     Frame should be discarded.

 }
 Result:=(Value=1)
end;

function ID3.V23.TReader.getFrameGrouped:Boolean;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[1];
  Mask:=(1 shl 6);
  Value:=(Value and Mask) shr 6;
 {

   76543210  76543210
  %0abc0000 %0h00kmnp

  h - Grouping identity

        This flag indicates whether or not this frame belongs in a group
        with other frames. If set, a group identifier byte is added to the
        frame. Every frame with the same group identifier belongs to the
        same group.

        0     Frame does not contain group information
        1     Frame contains group information
  }
  Result:=(Value=1);
end;

function ID3.V23.TReader.getFrameCompressed:Boolean;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[1];
  Mask:=(1 shl 3);
  Value:=(Value and Mask) shr 3;
 {
   76543210  76543210
  %0abc0000 %0h00kmnp

   k - Compression

      This flag indicates whether or not the frame is compressed.
      A 'Data Length Indicator' byte MUST be included in the frame.

      0     Frame is not compressed.
      1     Frame is compressed using zlib [zlib] deflate method.
            If set, this requires the 'Data Length Indicator' bit
            to be set as well.
  }
  Result:=(Value=1);
end;

function ID3.V23.TReader.getFrameEncrypted:Boolean;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[1];
  Mask:=(1 shl 2);
  Value:=(Value and Mask) shr 2;
  {
   76543210  76543210
  %0abc0000 %0h00kmnp

   m - Encryption

      This flag indicates whether or not the frame is encrypted. If set,
      one byte indicating with which method it was encrypted will be
      added to the frame. See description of the ENCR frame for more
      information about encryption method registration. Encryption
      should be done after compression. Whether or not setting this flag
      requires the presence of a 'Data Length Indicator' depends on the
      specific algorithm used.

      0     Frame is not encrypted.
      1     Frame is encrypted.
  }
  Result:=(Value=1);
end;

function ID3.V23.TReader.getFrameUnSynchronized:Boolean;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[1];
  Mask:=(1 shl 1);
  Value:=(Value and Mask) shr 1;
  {
   76543210  76543210
  %0abc0000 %0h00kmnp

   n - Unsynchronisation

      This flag indicates whether or not unsynchronisation was applied
      to this frame. See section 6 for details on unsynchronisation.
      If this flag is set all data from the end of this header to the
      end of this frame has been unsynchronised. Although desirable, the
      presence of a 'Data Length Indicator' is not made mandatory by
      unsynchronisation.

      0     Frame has not been unsynchronised.
      1     Frame has been unsyrchronised.
  }
  Result:=(Value=1);
end;

function ID3.V23.TReader.getFrameDataLengthFlag:Boolean;
var
  Value:byte;
  Mask:byte;
begin
  Value:=FFrameHeader.Flags[1];
  Mask:=(1 shl 0);
  Value:=(Value and Mask);
  {
   76543210  76543210
  %0abc0000 %0h00kmnp

   p - Data length indicator

      This flag indicates that a data length indicator has been added to
      the frame. The data length indicator is the value one would write
      as the 'Frame length' if all of the frame format flags were
      zeroed, represented as a 32 bit synchsafe integer.

      0      There is no Data Length Indicator.
      1      A data length Indicator has been added to the frame.
  }
  Result:=(Value=1);
end;
{
procedure ID3.V23.TReader.Load(var exData:TIDFlagData; Stream:TStream);
begin
  System.FillByte(exData,SizeOf(exData),0);
  Stream.ReadBuffer(exData.Size,1);
  if exData.Size>0 then
    Stream.ReadBuffer(exData.Data[0],exData.Size);
end;
}
procedure ID3.V23.TReader.Load(Stream:TStream);
var
  iSize:LongInt;
  iStart:LongInt;
  iFrameSize:Cardinal;
begin
  iStart:=Stream.Position;
  iSize:=Self.Size;
  Repeat
    Stream.ReadBuffer(FFrameHeader,SizeOf(FFrameHeader));
    iFrameSize:=frameSize;
    if (iFrameSize>0) then begin
      uByteArray.SetSize(Self.FFrameData,iFrameSize);
      Stream.ReadBuffer(FFrameData[0],iFrameSize);
    end;
  until (Stream.Position>=(iStart+iSize)) or (Stream.Position>=Stream.Size);
end;
*)


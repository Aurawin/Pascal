(*******************************************************************************
mp3parse
(c) 2008 David Pethes (imcold), dadop /at/ centrum.sk

*)
program project1;
{$mode objfpc}{$H+}


const
  READ_BUFFER = 4;
{ 2.4.2.3 Header
  bit_rate_index -> bitrate mapping
  '0000'  free format
  '0001'  32 kbit/s
  '0010'  40 kbit/s
  '0011'  48 kbit/s
  '0100'  56 kbit/s
  '0101'  64 kbit/s
  '0110'  80 kbit/s
  '0111'  96 kbit/s
  '1000' 112 kbit/s
  '1001' 128 kbit/s
  '1010' 160 kbit/s
  '1011' 192 kbit/s
  '1100' 224 kbit/s
  '1101' 256 kbit/s
  '1110' 320 kbit/s
}
mp3_bit_rate_index_tab: array[0..14] of word = (
  0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320
);

{ 2.4.2.3
  sampling_frequency
  '00' 44.1 kHz
  '01' 48 kHz
  '10' 32 kHz
  '11' reserved
}
mp3_sampling_frequency_tab: array[0..3] of longword = (
  44100, 48000, 32000, 0
);


var
  f: file;     //evil global vars
  pb: pbyte;
  nb_frames, bitrate_sum: integer;
  verbose: boolean;


(* parse_frame_header
   examine the frame header from pb buffer and seek to the next frame/syncword
   returns:
     true  - ok
     false - failed to parse/seek/unsupported stream
*)
function parse_frame_header: boolean;
var
  i, bitrate, sampling_freq, frame_size: integer;
begin
  result := false;

  if (pb[0] = $FF) and (pb[1] and $F0 = $F0) then begin
      //writeln('syncword found!');
  end else begin
      if verbose then begin
          writeln(stderr, 'syncword not found');
          write  (stderr, 'got: ');
          for i := 0 to 3 do write(stderr, hexstr(pb[i], 2):3);
          writeln(stderr);
      end;
      exit;
  end;

  i := pb[2] shr 4;
  if i in [0, 15] then begin
      writeln(stderr, 'unsupported bit_rate_index value:', i);
      exit;
  end;
  bitrate := mp3_bit_rate_index_tab[i];
  bitrate_sum += bitrate;

  i := pb[2] shr 2 and 3;
  if i = 3 then begin
      writeln(stderr, 'unsupported sampling_frequency value');
      exit;
  end;
  sampling_freq := mp3_sampling_frequency_tab[i];

  { calculate the distance between the start of two consecutive syncwords
      2.4.3 The Audio Decoding Process
      2.4.3.1 General
  }
  frame_size := 144 * bitrate * 1000 div sampling_freq;
  frame_size += (pb[2] shr 1 and 1);  //add padding if padding bit is set (1 byte)

  //seek to next frame
  i := FilePos(f) - READ_BUFFER + frame_size;
  if i <= 4 then writeln(stderr, 'reading broken');
  if FileSize(f) > i then begin
      Seek(f, i);
      nb_frames += 1;
      result := true;
  end;

  if verbose then begin
      writeln(stderr, 'frame:                   ', nb_frames:5     );
      writeln(stderr, 'bitrate (kbps):          ', bitrate:5       );
      writeln(stderr, 'sampling frequency (Hz): ', sampling_freq:5 );
      writeln(stderr, 'frame size (bytes):      ', frame_size:5    );
  end;
end;


(* parse_id3v2_tag
   skip ID3v2 data
*)
procedure parse_id3v2_tag();
var
  i: integer;
begin
  if not( (pb[0] = $49) and
          (pb[1] = $44) and
          (pb[2] = $33) ) then
  begin
      Seek(f, 0);
      exit;
  end;
  writeln('ID3v2 found!');
  i := 10 + pb[9] + pb[8] shl 7 + pb[7] shl 14 + pb[6] shl 21;
  writeln('tag size (bytes): ', i);
  Seek(f, i);
end;


(* main *)
begin
  if Paramcount < 1 then begin
      writeln('usage: mp3parse input [-v]');
      writeln('no input specified');
      halt;
  end;
  verbose := ParamStr(2) = '-v';

  pb := getmem(10 + READ_BUFFER);
  nb_frames   := 0;
  bitrate_sum := 0;

  AssignFile(f, ParamStr(1));
  Reset(f, 1);
  BlockRead(f, pb^, 10);
  parse_id3v2_tag();
  repeat
      BlockRead(f, pb^, READ_BUFFER);
  until not parse_frame_header();
  CloseFile(f);

  freemem(pb);

  //writeln('bytes left: ', FileSize(f) - FilePos(f));
  writeln('frames: ', nb_frames);
  if nb_frames > 0 then
      writeln('avg. bitrate: ', bitrate_sum div nb_frames);
end.


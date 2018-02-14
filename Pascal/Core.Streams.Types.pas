unit Core.Streams.Types;

interface
uses Classes,SysUtils;

Const
  fmCreate=Classes.fmCreate;
  fmOpenRead=Classes.fmOpenRead;
  fmOpenWrite=Classes.fmOpenWrite;
  fmOpenReadWrite=Classes.fmOpenReadWrite;
  fmShareCompat=SysUtils.fmShareCompat;
  fmShareDenyNone=SysUtils.fmShareDenyNone;
  fmShareDenyRead=SysUtils.fmShareDenyRead;
  fmShareDenyWrite=SysUtils.fmShareDenyWrite;
  fmShareExclusive=SysUtils.fmShareExclusive;
Type

  VarString=TStringStream;
  Disk=TFileStream;
  Memory=TMemoryStream;
  Base=TStream;

  Option=(soNone,soClean);
  Options=Set of Option;

  Encoding=(seNormal,seUTF16,seUTF16BE,seUTF8);

implementation



end.


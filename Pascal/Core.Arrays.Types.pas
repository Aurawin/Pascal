unit Core.Arrays.Types;

interface
uses
  MD5,
  Core.Strings;

Type



  LargeInt=Array of System.Int64;
  PLargeInt=^LargeInt;

  LargeWord=Array of System.QWord;
  PLargeWord=^LargeWord;

  Bytes=Array of System.Byte;
  PBytes=^Bytes;
  BytesManifest=Array of Bytes;

  Bytes20=Array[0..19] of System.Byte;

  MD5Digest=MD5.TMD5Digest;
  MD5Context=MD5.TMD5Context;

  VarString=Array of Core.Strings.VarString;
  StringManifest=Array of VarString;
  PVarString=^VarString;

  PStringManifest=^StringManifest;


  KeyStrings=array of PKeyString;
  PKeyStrings=^KeyStrings;

  Pointers=Array of System.Pointer;

implementation

end.


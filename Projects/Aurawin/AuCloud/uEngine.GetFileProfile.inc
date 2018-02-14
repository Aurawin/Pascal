
function GetFileProfile(var FileName:Core.Strings.VarString; var Digest:TMD5Digest):boolean; overload;
var
  Data:TFileStream;
begin
  Result:=False;
  Data:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
  Try
    Core.Streams.CheckSum(Data,Digest);
    Result:=true;
  finally
    Data.Free();
  end;
end;

function GetFileProfile(var FileName:Core.Strings.VarString; var Size:QWord; var dtModified:TDateTime):boolean; overload;
begin
  Result:=Core.Utils.Files.FileAge(FileName,dtModified);
  if Result then begin
    // adjust to UTC time
    dtModified:=DateUtils.incMillisecond(dtModified,ToUTCBias*60000);
    Size:=Core.Utils.Files.FileSize(FileName);
    Result:=True;
  end;
end;

function GetFileProfile(var FileName:Core.Strings.VarString; var dtModified:TDateTime):boolean; overload;
begin
  Result:=Core.Utils.Files.FileAge(FileName,dtModified);
  if Result then begin
    dtModified:=DateUtils.IncMillisecond(dtModified,ToUTCBias*60000);
    Result:=True;
  end;
end;


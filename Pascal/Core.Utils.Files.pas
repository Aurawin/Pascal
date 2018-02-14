unit Core.Utils.Files;

interface

uses
  Classes, SysUtils,

  Core.Strings,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Utils.Time;

Type
  TExtractFileExtensionOptions=(efeoNone,efeoIncludeDot);
  TExtractPathOptions=(epoName,epoRoot,epoAllButName,epoAllButRoot);
  PSearchRec=^TSearchRec;
Const
  INVALID_PATH_CHARS='\/:* ?"<>|''';
  FCFlags:Array[Boolean] of System.LongInt = (fmCreate,fmOpenReadWrite);


procedure  ListFiles(Var List:Core.Arrays.Types.VarString; Path,Filter:Core.Strings.VarString);
function   Search(Var List:Core.Arrays.Types.VarString; Name:Core.Strings.VarString):LongInt;
function   Extract(Path:Core.Strings.VarString; Const Options:TExtractPathOptions; Sep:Char='/'):Core.Strings.VarString; overload;
function   Extract(FileName:Core.Strings.VarString; Const Options:TExtractFileExtensionOptions):Core.Strings.VarString; overload;
function   Join(Path:Core.Strings.VarString; FileName:Core.Strings.VarString; const Sep:Char='/'):Core.Strings.VarString; overload;
function   Prepare(FileName:Core.Strings.VarString; Const Ext:Core.Strings.VarString=''):Core.Strings.VarString; overload;
function   EndWithSeparator(Path:Core.Strings.VarString):Core.Strings.VarString;
function   toString(FileName:Core.Strings.VarString):Core.Strings.VarString; overload;
procedure  fromString(FileName:Core.Strings.VarString; Data:Core.Strings.VarString );
function   Create(FileName:Core.Strings.VarString; Const OwnerID:System.int64=-1; Const GroupID:System.Int64=-1; Const Mode:LongInt=&774):System.boolean; overload;
function   Append(Path1,Path2:Core.Strings.VarString; const Delim:Core.Strings.VarString=PathDelim):Core.Strings.VarString; overload;
function   isDirectory(Attr:System.Cardinal):System.boolean;
function   Validate(var sData:Core.Strings.VarString):System.boolean;
function   FileAge(Const FileName:Core.Strings.VarString; Var dtAge:TDateTime):System.Boolean;
function   FileSize(Const FileName:Core.Strings.VarString):System.QWord;
function   Truncate(Const FileName:Core.Strings.VarString):System.Int64;
function   SetFileDateTime(Const FileName:Core.Strings.VarString; const dtUT:System.TDateTime; const Bias:LongInt):LongInt;
function   ForceDirectories(Path:Core.Strings.VarString; Const OwnerID:System.Int64=-1; Const GroupID:System.Int64=-1; Const Mode:LongInt=&774):System.boolean;
function   SetPermissions(Path:Core.Strings.VarString; Const OwnerID:System.Int64=-1; Const GroupID:System.Int64=-1; Const Mode:LongInt=&774):System.boolean;

implementation
  uses DateUtils,StrUtils
{$ifdef Unix}
  ,BaseUnix
{$endif}
  ;

function   Search(Var List:Core.Arrays.Types.VarString; Name:Core.Strings.VarString):LongInt;
var
  iLcv:LongInt;
  iLength:LongInt;
begin
  iLength:=Length(List);
  Result:=-1; iLcv:=0;
  Name:=Lowercase(Name);
  While (iLcv<iLength) and (Result=-1) do begin
    if Pos(List[iLcv],Name)=1 then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  ForceDirectories(Path:Core.Strings.VarString; Const OwnerID:System.Int64=-1; Const GroupID:System.Int64=-1; Const Mode:LongInt=&774):System.boolean;
var
  iLcv:LongInt;
  saPath:Core.Arrays.Types.VarString;
begin
  Result:=SysUtils.DirectoryExists(Path);
  if Result=false then begin
    Core.Arrays.VarString.fromString(saPath,Path,System.DirectorySeparator);
    Try
      System.SetLength(Path,0);
      for iLcv:=0 to High(saPath) do begin
        Path:=Concat(Path,saPath[iLcv],System.DirectorySeparator);
        if SysUtils.DirectoryExists(Path)=false then begin
          if SysUtils.CreateDir(Path)=false then
            break;
          {$ifdef Unix}
            if (OwnerID<>-1) and (GroupID<>-1) then
              BaseUnix.FpChown(Path,OwnerID,GroupID);
            if (Mode<>-1) then
              BaseUnix.FpChmod(Path,Mode);
          {$endif}
        end;
      end;
      Result:=SysUtils.DirectoryExists(Path);
    finally
      Core.Arrays.VarString.Done(saPath);
    end;
  end;
end;

function  SetPermissions(Path:Core.Strings.VarString; Const OwnerID:System.Int64=-1; Const GroupID:System.Int64=-1; Const Mode:LongInt=&774):System.boolean;
begin
  Result:=false;
  {$ifdef Unix}
    if (OwnerID<>-1) and (GroupID<>-1) then
      BaseUnix.FpChown(Path,OwnerID,GroupID);
    if (Mode<>-1) then
      BaseUnix.FpChmod(Path,Mode);
    Result:=True;
  {$endif}

end;

function EndWithSeparator(Path:Core.Strings.VarString):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  Result:=Path;
  iLen:=Length(Result);
  if (iLen>0) and (Result[iLen]<>System.DirectorySeparator) then begin
    Inc(iLen);
    SetLength(Result,iLen);
    Result[iLen]:=System.DirectorySeparator;
  end;
end;

function Create(FileName:Core.Strings.VarString; Const OwnerID:System.Int64=-1; Const GroupID:System.Int64=-1; Const Mode:LongInt=&774):System.boolean;
var
  sPath:Core.Strings.VarString;
  hFile:THandle;
begin
  sPath:=ExtractFilePath(FileName);
  Result:=ForceDirectories(sPath,OwnerID,GroupID,Mode);
  if Result then begin
    Result:=FileExists(FileName);
    if Result=false then begin
      hFile:=FileCreate(FileName);
      Result:={$ifdef Unix}hFile<>-1{$else}hFile<>0{$endif};
      if Result then begin
        FileClose(hFile);
        SetPermissions(FileName,OwnerID,GroupID,Mode);
      end;
    end;
  end;
end;

function Prepare(FileName:Core.Strings.VarString; Const Ext:Core.Strings.VarString=''):Core.Strings.VarString;
begin
  Result:=FileName;
  If (Length(Ext)>0) and (Length(SysUtils.ExtractFileExt(FileName))=0) then
    Result:=SysUtils.ChangeFileExt(Result,Ext);
end;

function   toString(FileName:Core.Strings.VarString):Core.Strings.VarString;
var
  fsData:TFileStream;
  iLen:Cardinal;
begin
  fsData:=TFileStream.Create(FileName,fmOpenRead);
  Try
    iLen:=fsData.Size;
    SetLength(Result,iLen);
    if iLen>0 then begin
      fsData.Seek(0,soFromBeginning);
      fsData.Read(Result[1],iLen);
    end;
    System.SetLength(Result,iLen);
  finally
    FreeAndNil(fsData);
  end;
end;

procedure  fromString(FileName:Core.Strings.VarString; Data:Core.Strings.VarString);
var
  fsData:TFileStream;
  iLen:LongInt;
begin
  fsData:=TFileStream.Create(FileName,fmCreate);
  Try
    iLen:=System.Length(Data);
    if iLen>0 then
      fsData.WriteBuffer(Data[1],iLen);
  finally
    FreeAndNil(fsData);
  end;
end;

function Join(Path:Core.Strings.VarString; FileName:Core.Strings.VarString; const Sep:char='/'):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  iLen:=System.Length(Path);
  if (iLen=0) then
    Path:=Sep
  else if (Path[iLen]<>Sep) then
    Path+=Sep;
  Result:=Concat(Path,FileName);
end;

function Extract(Path:Core.Strings.VarString; Const Options:TExtractPathOptions; Sep:Char='/'):Core.Strings.VarString;
var
  iLen:LongInt;

  function GetEnd:Core.Strings.VarString;
  var
    iLoc:LongInt;
  begin
    SetLength(Result,0);
    if iLen>0 then begin
      iLoc:=StrUtils.RPosEx(Sep,Path,iLen-1);
      if iLoc>0 then begin
        Result:=System.Copy(Path,iLoc+1,iLen-iLoc);
      end else begin
        Result:=Path;
      end;
    end;
  end;

  function GetAllButName:Core.Strings.VarString;
  var
    iLoc:LongInt;
  begin
    SetLength(Result,0);
    if iLen>0 then begin
      iLoc:=StrUtils.RPosEx(Sep,Path,iLen-1);
      if iLoc>0 then begin
        Result:=System.Copy(Path,1,iLoc-1);
      end else begin
        Result:=Path;
      end;
    end;
  end;

  function GetStart:Core.Strings.VarString;
  var
    iLoc:LongInt;
  begin
    iLoc:=System.Pos(Sep,Path);
    if iLoc>0 then begin
      Result:=System.Copy(Path,1,iLoc);
    end else begin
      Result:=Path;
    end;
  end;

  function GetAllButRoot:Core.Strings.VarString;
  var
    iLoc:LongInt;
  begin
    iLoc:=System.Pos(Sep,Path);
    if (iLoc>0) then begin
      Result:=System.Copy(Path,iLoc+1,iLen-iLoc);
    end else begin
      Result:='';
    end;
  end;

begin
  iLen:=Length(Path);
  case Options of
    epoName       : Result:=GetEnd;
    epoRoot       : Result:=GetStart;
    epoAllButName : Result:=GetAllButName;
    epoAllButRoot : Result:=GetAllButRoot;
  end;
  iLen:=Length(Result);
  if (iLen>0) and (Result[iLen]=Sep) then
    SetLength(Result,iLen-1);
end;

function Extract(FileName:Core.Strings.VarString; Const Options:TExtractFileExtensionOptions):Core.Strings.VarString;
var
  iLen:LongInt;
  iLoc:LongInt;

  function PushGetNoDot:Core.Strings.VarString;
  begin
    SetLength(Result,0);
    iLoc:=iLen;
    While (iLoc>0) and (FileName[iLoc]<>'.') do
      Dec(iLoc);
    if (iLoc>0) then begin
      Inc(iLoc);
      Result:=System.Copy(FileName,iLoc,(iLen-iLoc)+1);
    end;
  end;

  function PushGetAll:Core.Strings.VarString;
  begin
    iLoc:=iLen;
    While (iLoc>0) and (FileName[iLoc]<>'.') do
      Dec(iLoc);
    if iLoc=0 then iLoc:=1;
    Result:=System.Copy(FileName,iLoc,iLen-iLoc);
  end;

begin
  iLen:=System.Length(FileName);
  Case Options of
    efeoNone: Result:=PushGetNoDot;
    efeoIncludeDot: Result:=PushGetAll;
  end;
end;

procedure ListFiles(Var List:Core.Arrays.Types.VarString; Path,Filter:Core.Strings.VarString);
var
  Allocated:Boolean;
  Sep:Char;

  Procedure ParseFolder(FName:Core.Strings.VarString);
  Var
    Lcv,iLen:LongInt;
    SearchRec:TSearchRec;
    TempFile:Core.Strings.VarString;
  begin
     If DirectoryExists(Fname) then begin
        // Prep Folder with slash
        iLen:=Length(FName);
        If FName[iLen]=Sep then
           SetLength(FName,iLen-1);
        Allocated:=False;
        Lcv:=FindFirst(FName+Sep+Filter,faArchive or faReadOnly,SearchRec);
        While Lcv=0 do begin
           Allocated:=True;
           If (SearchRec.Name<>'.') then begin
              TempFile:=FName + SeP + SearchRec.Name;
              ParseFolder(TempFile);
           end;
           Lcv:=FindNext(SearchRec);
        end;
        If Allocated then
           FindClose(SearchRec);
     end else if FileExists(FName) then
       Core.Arrays.VarString.Add(List,FName);
  end;

begin
  Sep:=SysUtils.PathSep;
  Allocated:=False;
  Empty(List);
  ParseFolder(Path);
end;

function   isDirectory(Attr:Cardinal):boolean;
begin
  Result:=((Attr or faDirectory)=Attr);
end;

function   Validate(var sData:Core.Strings.VarString):boolean;
var
  iLcv:LongInt;
  iLen:LongInt;
begin
  Result:=true;
  iLcv:=1;
  iLen:=System.Length(sData);
  While (iLcv<=iLen) do begin
    if Pos(sData[iLcv],INVALID_PATH_CHARS)>0 then begin
      System.Delete(sData,iLcv,1);
      Dec(iLen);
      Result:=false;
    end else begin
      Inc(iLcv);
    end;
  end;
end;

function   FileAge(Const FileName:Core.Strings.VarString; Var dtAge:TDateTime):Boolean;
var
  iAge:LongInt;
begin
  Result:=false; dtAge:=0;
  iAge:=SysUtils.FileAge(FileName);
  if iAge<>-1 then begin
    dtAge:=FileDateToDateTime(iAge);
    Result:=True;
  end;
end;

function   SetFileDateTime(Const FileName:Core.Strings.VarString; const dtUT:TDateTime; const Bias:LongInt):LongInt;
var
  dtAge:TDateTime;
  iMSecs:System.Int64;
begin
  iMSecs:=Bias*60000;
  dtAge:=DateUtils.IncMilliSecond(dtUT,iMSecs);
  SysUtils.DateTimeToFileDate(dtAge);
  Result:=SysUtils.DateTimeToFileDate(dtAge);
  SysUtils.FileSetDate(FileName,Result);
end;

function   FileSize(Const FileName:Core.Strings.VarString):System.QWord;
var
  FS:TFileStream;
begin
  Result:=0;
  try
    FS:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
    Try
      Result:=FS.Size;
    finally
      FreeAndNil(FS);
    end;
  Except

  end;
end;

function   Truncate(Const FileName:Core.Strings.VarString):System.Int64;
var
  FS:TFileStream;
begin
  Result:=-1;
  try
    FS:=TFileStream.Create(FileName,fmOpenReadWrite or fmShareDenyNone);
    Try
      FS.Size:=0;
      Result:=FS.Size;
    finally
      FreeAndNil(FS);
    end;
  Except

  end;
end;

function   Append(Path1,Path2:Core.Strings.VarString; const Delim:Core.Strings.VarString=PathDelim):Core.Strings.VarString;
var
  iLen1 : LongInt;
  iLen2 : LongInt;
begin
  SetLength(Result,0);
  iLen1:=Length(Path1);
  iLen2:=Length(Path2);
  if (iLen1>0) then begin
    if (Path1[iLen1]<>Delim) and ((iLen2>0) and (Path2[1]<>Delim)) then
      Path1+=Delim;
    if iLen2>0 then begin
      Result:=Concat(Path1,Path2);
    end else begin
      Result:=Path1;
    end;
  end;
end;
end.


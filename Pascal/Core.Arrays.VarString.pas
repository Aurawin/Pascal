unit Core.Arrays.VarString;

interface

uses
  Core,
  Core.Streams,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Streams.Types;


Const
  LEADING_DELIM_ON = true;
  LEADING_DELIM_OFF = false;
  TRAILING_DELIM_ON = true;
  TRAILING_DELIM_OFF = false;

Type

  ProgressEvent=procedure(iIndex,iCount:System.Int64) of object;

  ProgressCallback=procedure(Progress,Total:System.Int64; Method:ProgressEvent);
  AddCallback=procedure(Var List:Core.Arrays.Types.VarString; Var Value:Core.Strings.VarString);
  LineItemCallback=procedure(Var Value:Core.Strings.VarString);


  procedure Init(Var List:Core.Arrays.Types.StringManifest); overload;
  procedure Init(Var List:Core.Arrays.Types.VarString); overload;

  procedure Done(Var List:Core.Arrays.Types.StringManifest); overload;
  procedure Done(Var List:Core.Arrays.Types.VarString); overload;

  procedure Empty(Var List:Core.Arrays.Types.StringManifest); overload;
  procedure Empty(Var List:Core.Arrays.Types.VarString); overload;

  function  Pos(Var Data:Core.Strings.VarString; Var Searches:Core.Arrays.Types.VarString):Int64; overload;

  function  Add(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):Int64; overload;
  function  Add(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.Small; const Defaults:AddOptions=[aoCheckForDuplicates]):Int64; overload;
  function  Add(ListP:Core.Arrays.Types.PVarString; sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):Int64; overload;
  procedure Add(Var Source,Destination:Core.Arrays.Types.VarString); overload;

  function  Remove(Var List:Core.Arrays.Types.VarString; Value:Core.Strings.VarString):System.boolean; overload;
  function  Remove(Var List:Core.Arrays.Types.VarString; Index:LongInt):System.boolean; overload;
  function  IndexOf(ListP:Core.Arrays.Types.PVarString; sItem:Core.Strings.VarString):Int64; overload;
  function  IndexOf(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString; Offset:Int64):Int64; overload;
  function  IndexOf(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString):Int64; overload;
  function  IndexOf(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.Small):Int64; overload;
  function  IndexOf(Var List:Core.Arrays.Types.VarString):Int64; overload;
  function  Find(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString):System.Boolean; overload;
  function  Search(Var List:Core.Arrays.Types.VarString; sItem:Core.Strings.VarString; Option:SearchOption=soAnywhere):Int64; overload;
  function  IsNumeric(var List:Core.Arrays.Types.VarString):System.boolean;
  procedure SetSize(Var List:Core.Arrays.Types.VarString; iSize:Int64); overload;
  function  Last(var sData:Core.Strings.VarString; const Delim:Core.Strings.VarString=#13#10):Core.Strings.VarString;

  function  fromStream(var List:Core.Arrays.Types.VarString; Stream:Core.Streams.Types.Base; const Delim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):Int64; overload;
  function  fromString(var List:Core.Arrays.Types.VarString; var sData:Core.Strings.VarString; const Delim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]; const Callback:ProgressEvent=nil):System.Int64; overload;
  function  fromString(ListP:Core.Arrays.Types.PVarString; sData:Core.Strings.VarString; const Delim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):Int64; overload;

  function  toString(var List:Core.Arrays.Types.VarString; const Delim:Core.Strings.VarString=#13#10; const Refactor:Core.Streams.Types.Base=nil; Const TrailingDelim:System.boolean=false; const LeadingDelim:System.boolean=false):Core.Strings.VarString; overload;
  function  toString(Var List:Core.Arrays.Types.VarString; iStart,iCount:Int64; const Delim:Core.Strings.VarString=#13#10; const Refactor:Core.Streams.Types.Base=nil; Const TrailingDelim:System.boolean=false; Const LeadingDelim:System.boolean=false):Core.Strings.VarString; overload;
  procedure toFile(Var List:Core.Arrays.Types.VarString; Const sFile:Core.Strings.FileName; const Option:Core.Arrays.SaveOption=Core.Arrays.SaveOption.soNone); overload;

  procedure fromFile(Var List:Core.Arrays.Types.VarString; Const sFile:Core.Strings.FileName; const Delim:Core.Strings.VarString=#13#10); overload;
  function  GenerateRandomString(Const MinLength:LongInt=8; Const MaxLength:LongInt=255):Core.Strings.VarString;
  function  Random(var List:Core.Arrays.Types.VarString):Core.Strings.VarString; overload;
  function  Parameter(Var List:Core.Arrays.Types.VarString; Index:Int64; Option:ParameterOption=poOneBased):Core.Strings.VarString; overload;
  function  Sub(var List:Core.Arrays.Types.VarString; Start,Count:integer):Core.Arrays.Types.PVarString;

  procedure Trim(var List:Core.Arrays.Types.VarString; Count:Int64); overload;
  procedure Copy(Var Source,Destination:Core.Arrays.Types.VarString); overload;
  procedure Copy(Var Source,Destination:Core.Arrays.Types.StringManifest); overload;

implementation

Uses StrUtils;

function  Sub(var List:Core.Arrays.Types.VarString; Start,Count:integer):Core.Arrays.Types.PVarString;
var
  iLcv:integer;
  iCt:integer;
  iTotal:integer;
begin
  new(Result);
  SetLength(Result^,Count);
  iLcv:=Start; iCt:=0;
  iTotal:=Length(List);
  while (iLcv<iTotal) and (iCt<Count) do begin
    Result^[iLcv]:=List[iLcv];
    iLcv+=1;
    iCt+=1;
  end;
  SetLength(Result^,iCt);
end;

procedure Trim(var List:Core.Arrays.Types.VarString; Count:Int64);
var
  iLen:Int64;
begin
  iLen:=System.Length(List);
  Dec(iLen,Count);
  if iLen<0 then iLen:=0;
  System.SetLength(List,iLen);
end;

procedure Empty(Var Item:Core.Strings.VarString);
begin
  SetLength(Item,0);
end;

procedure Empty(Var List:Core.Arrays.Types.StringManifest);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(List) to High(List) do
    Empty(List[iLcv]);
  SetLength(List,0);
end;

procedure Empty(Var List:Core.Arrays.Types.VarString);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(List) to High(List) do
    SetLength(List[iLcv],0);
  SetLength(List,0);
end;
procedure Init(Var List:Core.Arrays.Types.VarString);
begin
  System.SetLength(List,0);
end;

procedure Init(Var List:Core.Arrays.Types.StringManifest);
begin
  System.SetLength(List,0);
end;

procedure Done(Var List:Core.Arrays.Types.StringManifest);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(List) to High(List) do
    Done(List[iLcv]);
  Finalize(List);
end;

procedure Done(Var List:Core.Arrays.Types.VarString);
var
  iLcv:LongInt;
begin
  for iLcv:=Low(List) to High(List) do
    Finalize(List[iLcv]);
  Finalize(List);
end;

procedure SetSize(Var List:Core.Arrays.Types.VarString; iSize:Int64);
var
  iLen:Int64;
begin
  iLen:=Length(List);
  if (iSize<iLen) then begin
    // Going to Shrink the list
    While (iLen>iSize) do begin
      SetLength(List[iLen-1],0);
      Dec(iLen);
    end;
    SetLength(List,iSize);
  end else if (iSize>iLen) then begin
    // Going to Grow the list
    SetLength(List,iSize);
  end;
end;


procedure  Callback_TrimLine(Var Value:Core.Strings.VarString);
begin
  Core.Strings.Trim(Value);
end;

procedure  Callback_RemoveQuotesFromValue(Var Value:Core.Strings.VarString);
var
  iLen:LongInt;
begin
  iLen:=Length(Value);
  if iLen>1 then begin
    If Value[1]=#34 then begin
      System.Delete(Value,1,1);
      Dec(iLen);
    end;
  end;
  if iLen>1 then begin
    If Value[iLen]=#34 then begin
      System.Delete(Value,iLen,1);
      Dec(iLen);
    end;
  end;
  if iLen>1 then begin
    If Value[1]=#39 then begin
      System.Delete(Value,1,1);
      Dec(iLen);
    end;
  end;
  if iLen>1 then begin
    If Value[iLen]=#39 then begin
      System.Delete(Value,iLen,1);
      Dec(iLen);
    end;
  end;
end;

procedure Callback_LineVoid(Var Value:Core.Strings.VarString);
begin

end;

procedure Callback_ProgressVoid(Progress,Total:System.Int64; Method:ProgressEvent);
begin
end;

procedure Callback_Progress(Progress,Total:System.Int64; Method:ProgressEvent);
begin
  Method(Progress,Total);
end;

procedure  Callback_AddToListLowercase(Var List:Core.Arrays.Types.VarString; Var Value:Core.Strings.VarString);
begin
  Value:=Lowercase(Value);
  Add(List,Value,[]);
end;

procedure  Callback_AddToList(Var List:Core.Arrays.Types.VarString; Var Value:Core.Strings.VarString);
begin
  Add(List,Value,[]);
end;

function  fromStream(var List:Core.Arrays.Types.VarString; Stream:Core.Streams.Types.Base; const Delim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):Int64;
var
  sData:Core.Strings.VarString;
begin
  sData:=Core.Streams.toString(Stream);
  Result:=fromString(List,sData,Delim,Defaults);
end;

function  fromString(ListP:Core.Arrays.Types.PVarString; sData:Core.Strings.VarString; const Delim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]):Int64;
begin
  Result:=fromString(ListP^,sData,Delim,Defaults);
end;

function  fromString(var List:Core.Arrays.Types.VarString; var sData:Core.Strings.VarString; const Delim:Core.Strings.VarString=#13#10; const Defaults:SplitOptions=[soClearList]; const Callback:ProgressEvent=nil):System.Int64;
var
  iLen:System.int64;
  iPosition:System.int64;
  iDelimLen:System.int64;
  sItem:Core.Strings.VarString;
  ParenWraps:System.boolean;
  BraceWraps:System.boolean;
  BracketWraps:System.boolean;
  QuoteWraps:System.boolean;

  RemoveQuotes:System.boolean;
  AddAsLowercase:System.boolean;
  TrimLine:System.boolean;
  PerformProgress:System.boolean;

  ProgressProcs:Array[Boolean] of ProgressCallback;
  AddProcs:Array[Boolean] of AddCallback;
  TrimLineProcs:array[Boolean] of LineItemCallback;
  RemoveQuoteProcs:Array[Boolean] of LineItemCallback;

  function GetRemainder:Core.Strings.VarString;
  begin
    Result:=System.Copy(sData,iPosition,iLen-iPosition+1);
    iPosition:=iLen+1;
  end;

  function GetNextEntryBasic:Core.Strings.VarString;
  var
    iLoc:System.int64;
  begin
    iLoc:=Core.Strings.PosEx(Delim,sData,iPosition,iLen);
    if iLoc>0 then begin
      Result:=System.Copy(sData,iPosition,iLoc-iPosition);
      iPosition:=iLoc+iDelimLen;
    end else if (iPosition=iLen) then begin
      Result:=sData[iPosition];
      iPosition:=iLen+1;
    end else if (iPosition>0) then begin
      Result:=System.Copy(sData,iPosition,iLen-iPosition+1);
      iPosition:=iLen+1;
    end else begin
      Result:=sData;
      iPosition:=iLen+1;
    end;
  end;

  function GetNextEntryWithWrapCheck:Core.Strings.VarString;
  var
    iLoc:System.int64;
    iParenStart:System.int64;
    iBraceStart:System.int64;
    iBracketStart:System.int64;
    iQuoteStart:System.int64;
    iWrapBias:System.int64;
    iWrapLcv:System.int64;
    iEnd:System.int64;
    iAdjBias:System.int64;

    procedure PushProcessWithChecks;
    begin
      iAdjBias:=0;
      If iLoc=0 then begin
        iLoc:=iLen;
        iAdjBias:=1;
      end;
      {$i Core.Arrays.VarString.fromString.BracketCheck.inc}
      {$i Core.Arrays.VarString.fromString.ParenCheck.inc}
      {$i Core.Arrays.VarString.fromString.BraceCheck.inc}
      {$i Core.Arrays.VarString.fromString.QuoteCheck.inc}

      if (BracketWraps=true) then begin
        {$i Core.Arrays.VarString.fromString.BracketWraps.inc}
      end else if (ParenWraps=true) then begin
        {$i Core.Arrays.VarString.fromString.ParenWraps.inc}
      end else if (BraceWraps=true) then begin
        {$i Core.Arrays.VarString.fromString.BraceWraps.inc}
      end else if (QuoteWraps=true) then begin
        {$i Core.Arrays.VarString.fromString.QuoteWraps.inc}
      end else begin
        Result:=System.Copy(sData,iPosition,iLoc-iPosition+iAdjBias);
        iPosition:=iLoc+iDelimLen;
      end;
    end;

  begin
    iLoc:=StrUtils.PosEx(Delim,sData,iPosition);
    if (iPosition=iLen) then begin
      Result:=sData[iPosition];
      iPosition:=iLen+1;
    end else
      PushProcessWithChecks();
  end;

begin
  PerformProgress:=Assigned(Callback);
  ProgressProcs[False]:=@Callback_ProgressVoid;
  ProgressProcs[True]:=@Callback_Progress;
  AddProcs[False]:=@Callback_AddToList;
  AddProcs[True]:=@Callback_AddToListLowercase;
  TrimLineProcs[False]:=@Callback_LineVoid;
  TrimLineProcs[True]:=@Callback_TrimLine;
  RemoveQuoteProcs[False]:=@Callback_LineVoid;
  RemoveQuoteProcs[True]:=@Callback_RemoveQuotesFromValue;
  RemoveQuotes:=soRemoveQuotes in Defaults;
  TrimLine:=soTrimLines in Defaults;
  AddAsLowercase:=soMakeLowercase in Defaults;
  if soClearList in Defaults then
    Empty(List);
  iDelimLen:=Length(Delim);
  iLen:=Length(sData);
  iPosition:=1;

  if (soIgnoreDelimAtStart in Defaults) and (Pos(Delim,sData)=1) then
    Inc(iPosition,iDelimLen);

  if soSingleton in Defaults then begin
    sItem:=GetNextEntryBasic();
    RemoveQuoteProcs[RemoveQuotes](sItem);
    TrimLineProcs[TrimLine](sItem);
    AddProcs[AddAsLowercase](List,sItem);
    if (iPosition<=iLen) then begin
      sItem:=GetRemainder();
      RemoveQuoteProcs[RemoveQuotes](sItem);
      AddProcs[AddAsLowercase](List,sItem);
    end;
  end else if (soParenWraps in Defaults) or (soBraceWraps in Defaults) or (soQuoteWraps in Defaults) then begin
    While (iPosition<=iLen) do begin
      sItem:=GetNextEntryWithWrapCheck();
      RemoveQuoteProcs[RemoveQuotes](sItem);
      TrimLineProcs[TrimLine](sItem);
      AddProcs[AddAsLowercase](List,sItem);
    end;
  end else begin
    While (iPosition<=iLen) do begin
      sItem:=GetNextEntryBasic();
      RemoveQuoteProcs[RemoveQuotes](sItem);
      TrimLineProcs[TrimLine](sItem);
      AddProcs[AddAsLowercase](List,sItem);
      ProgressProcs[PerformProgress](iPosition,iLen,Callback);
    end;
  end;
  Result:=Length(List);
end;

function  toString(var List:Core.Arrays.Types.VarString; const Delim:Core.Strings.VarString=#13#10; const Refactor:Core.Streams.Types.Base=nil; Const TrailingDelim:System.boolean=false; const LeadingDelim:System.boolean=false):Core.Strings.VarString;
var
  ssData:Core.Streams.Types.Base;

  procedure ProcessData;
  var
    iLcv:System.int64;
    iDelimLen:System.int64;
    iLen:System.int64;
  begin
    iDelimLen:=Length(Delim);
    if (LeadingDelim=true) then
      Core.Streams.Write(Delim,iDelimLen,ssData);
    iLcv:=0;
    iLen:=Length(List);
    While (iLcv<iLen) do begin
      Core.Streams.Write(List[iLcv],ssData);
      Core.Streams.Write(Delim,iDelimLen,ssData);
      Inc(iLcv);
    end;
    iLen:=ssData.Size;
    if (iLen>0) and (TrailingDelim=false) then
      ssData.Size:=iLen-iDelimLen;
    Result:=Core.Streams.toString(ssData);
  end;

begin
  ssData:=Refactor;
  if ssData=nil then begin
    ssData:=Core.Streams.Types.Memory.Create();
    try
      ProcessData;
    finally
      ssData.Free();
    end;
  end else begin
    Refactor.Size:=0;
    ProcessData;
    Refactor.Size:=0;
  end;
end;

function  toString(Var List:Core.Arrays.Types.VarString; iStart,iCount:Int64; const Delim:Core.Strings.VarString=#13#10; Const Refactor:Core.Streams.Types.Base=nil; Const TrailingDelim:System.boolean=false; Const LeadingDelim:System.boolean=false):Core.Strings.VarString;
var
  isaLength:Int64;
  iEnd:Int64;
  ssData:Core.Streams.Types.Base;

  procedure PushDataProcess;
  var
    iLcv:LongInt;
    iDelimLen:Int64;
    iLen:Int64;
  begin
    Try
      ssData.Size:=0;
      if (LeadingDelim=true) then
        Core.Streams.Write(Delim,ssData);
      For iLcv:=iStart to iEnd do
        Core.Streams.Write(Concat(List[iLcv],Delim),ssData);
      iLen:=ssData.Size; iDelimLen:=Length(Delim);
      if (iLen>0) and (TrailingDelim=false) then
        ssData.Size:=iLen-iDelimLen;
      Result:=Core.Streams.toString(ssData);
      ssData.Size:=0;
    except
      On e:Core.Exception do iLcv:=0;
    end;
  end;

begin
  isaLength:=Length(List);
  iEnd:=iStart+iCount-1;
  if iEnd>isaLength then
    iEnd:=isaLength-1;

  ssData:=Refactor;
  if ssData=nil then begin
    ssData:=Core.Streams.Types.Memory.Create();
    try
      PushDataProcess;
    finally
      FreeAndNil(ssData);
    end;
  end else begin
    PushDataProcess;
  end;
end;

function  Last(var sData:Core.Strings.VarString; const Delim:Core.Strings.VarString=#13#10):Core.Strings.VarString;
var
  iLength:LongInt;
  iCount:LongInt;
  iLcv:LongInt;
begin
  iLength:=System.Length(sData); SetLength(Result,0);
  if iLength>0 then begin
    for iLcv:=iLength downto 1 do begin
      if StrUtils.PosEx(Delim,sData,iLcv)=iLcv then
        Break;
    end;
    if iLcv=0 then begin
      iLcv:=1;
      iCount:=iLength;
    end else begin
      iLcv+=System.Length(Delim);
      iCount:=iLength-iLcv+1;
      // www#13#13#13data (10)
      //    |<-------------4
      // www#13#10data    (9)
      //    |<-------------4
      // www/data         (8)
      //    |<------------ 4
    end;
    Result:=System.Copy(sData,iLcv,iCount);
  end;
end;

procedure toFile(Var List:Core.Arrays.Types.VarString; Const sFile:Core.Strings.FileName; const Option:Core.Arrays.SaveOption=Core.Arrays.SaveOption.soNone);
var
  FS:Core.Streams.Types.Disk;
  sData:Core.Strings.VarString;
  iLcv:LongInt;
begin
  FS:=Core.Streams.Types.Disk.Create(sFile,fmCreate or fmShareDenyNone);
  Try
    Case Option of
      Core.Arrays.SaveOption.soUTF8 : begin
        for iLcv:=0 to High(List) do begin
          sData:=Concat(List[iLcv],#13#10);
          FS.Write(sData[1],Length(sData));
        end;
      end;
      Core.Arrays.SaveOption.soNone : begin
        for iLcv:=0 to High(List) do begin
          sData:=Concat(List[iLcv],#13#10);
          FS.Write(sData[1],Length(sData));
        end;
      end;
    end;
  finally
    FreeAndNil(FS);
  end;
end;

procedure fromFile(Var List:Core.Arrays.Types.VarString; Const sFile:Core.Strings.FileName; const Delim:Core.Strings.VarString=#13#10);
var
  FS:Core.Streams.Types.Disk;
  sData:Core.Strings.VarString;
begin
  FS:=Core.Streams.Types.Disk.Create(sFile,fmOpenRead or fmShareDenyNone);
  Try
    SetLength(sData,FS.Size);
    if FS.Size>0 then begin
      FS.Read(sData[1],FS.Size);
      fromString(List,sData,Delim);
    end;
  finally
    FreeAndNil(FS);
  end;
end;

function  Add(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.Small; const Defaults:AddOptions=[aoCheckForDuplicates]):Int64;
var
  sItem2:Core.Strings.VarString;
begin
  sItem2:=sItem;
  Result:=Add(List,sItem2,Defaults);
end;


function  Add(ListP:Core.Arrays.Types.PVarString; sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):Int64;
begin
  Result:=Add(ListP^,sItem,Defaults);
end;

procedure Add(Var Source,Destination:Core.Arrays.Types.VarString);
var
  iLcv:LongInt;
  iCount:Int64;
begin
  iCount:=System.Length(Destination);
  for iLcv:=0 to high(Source) do begin
    System.SetLength(Destination,iCount+1);
    Destination[iCount]:=Source[iLcv];
    Inc(iCount);
  end;
end;

function  Add(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString; const Defaults:AddOptions=[aoCheckForDuplicates]):Int64;
var
  iIndex:Int64;
begin
  iIndex:=-1;
  if aoCheckForDuplicates in Defaults then
    iIndex:=IndexOf(List,sItem);
  if (iIndex=-1) then begin
    Result:=Length(List)+1;
    SetLength(List,Result);
    List[Result-1]:=sItem;
  end else if (aoOverwriteDuplicate in Defaults) then begin
    Result:=iIndex;
    List[iIndex]:=sItem;
  end;
end;

function  IndexOf(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.Small):Int64;
var
  iLcv:Int64;
  iLength:Int64;
  {$ifdef RSR_DEBUG}
  sValue:Core.Strings.VarString;
  {$endif}
begin
  iLength:=Length(List);
  Result:=-1; iLcv:=0;
  While (iLcv<iLength) and (Result=-1) do begin
    {$ifdef RSR_DEBUG}
      sValue:=List[iLcv];
      If SameText(sValue,sItem) then
        Result:=iLcv;
    {$else}
      If SameText(List[iLcv],sItem) then
        Result:=iLcv;
    {$endif}
    Inc(iLcv);
  end;
end;

function  IndexOf(Var List:Core.Arrays.Types.VarString):Int64; overload;
var
  iLcv:Int64;
  iLength:Int64;
begin
  iLength:=Length(List);
  Result:=-1; iLcv:=0;
  While (iLcv<iLength) and (Result=-1) do begin
    if System.Length(List[iLcv])=0 then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  Find(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString):System.Boolean;
var
  iLcv:Int64;
  iLength:Int64;
  iIndex:Int64;
begin
  iLength:=Length(List);
  iIndex:=-1; iLcv:=0;
  While (iLcv<iLength) and (iIndex=-1) do begin
    If SameText(List[iLcv],sItem) then
      iIndex:=iLcv;
    Inc(iLcv);
  end;
  Result:=(iIndex<>-1);
end;

function  Search(Var List:Core.Arrays.Types.VarString; sItem:Core.Strings.VarString; Option:SearchOption=soAnywhere):Int64;
var
  iLcv:Int64;
  iLength:Int64;
  sValue:Core.Strings.VarString;

  procedure Push_Anywhere();
  begin
    While (iLcv<iLength) and (Result=-1) do begin
      sValue:=Lowercase(List[iLcv]);
      if Pos(sItem,sValue)>0 then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;

  procedure Push_Start();
  begin
    While (iLcv<iLength) and (Result=-1) do begin
      sValue:=Lowercase(List[iLcv]);
      if Pos(sItem,sValue)=1 then
        Result:=iLcv;
      Inc(iLcv);
    end;
  end;

begin
  iLength:=Length(List);
  Result:=-1; iLcv:=0;
  sItem:=Lowercase(sItem);
  case Option of
    soAnywhere : Push_Anywhere();
    soStart    : Push_Start();
  end;

end;

function  IsNumeric(var List:Core.Arrays.Types.VarString):System.boolean;
var
  iLcv:LongInt;
begin
  Result:=(Length(List)>0);
  for iLcv:=0 to High(List) do begin
    if Core.Strings.toInt(List[iLcv],-1)=-1 then begin
      Result:=false;
      break;
    end;
  end;
end;

function  IndexOf(ListP:Core.Arrays.Types.PVarString; sItem:Core.Strings.VarString):Int64;
var
  iLcv:Int64;
  iLength:Int64;
begin
  iLength:=Length(ListP^);
  Result:=-1; iLcv:=0;
  While (iLcv<iLength) and (Result=-1) do begin
    If SameText(ListP^[iLcv],sItem) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  IndexOf(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString; Offset:Int64):Int64;
var
  iLcv:Int64;
  iLength:Int64;
begin
  iLength:=Length(List);
  Result:=-1; iLcv:=Offset;
  While (iLcv<iLength) and (Result=-1) do begin
    If SameText(List[iLcv],sItem) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  IndexOf(Var List:Core.Arrays.Types.VarString; var sItem:Core.Strings.VarString):Int64;
var
  iLcv:Int64;
  iLength:Int64;
begin
  iLength:=Length(List);
  Result:=-1; iLcv:=0;
  While (iLcv<iLength) and (Result=-1) do begin
    If SameText(List[iLcv],sItem) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function   Pos(Var Data:Core.Strings.VarString; Var Searches:Core.Arrays.Types.VarString):Int64;
var
  iCount,iLcv:Int64;
begin
  Result:=0; iLcv:=0; iCount:=Length(Searches);
  While (iLcv<iCount) and (Result=0) do begin
    Result:=System.Pos(Searches[iLcv],Data);
    Inc(iLcv);
  end;
end;

function  Parameter(Var List:Core.Arrays.Types.VarString; Index:Int64; Option:ParameterOption=poOneBased):Core.Strings.VarString;
const
  Bias:Array[ParameterOption] of Int64 = (0,1);
begin
  SetLength(Result,0);
  If (Index>0) and ((Index-Bias[Option])<Length(List)) then
    Result:=List[Index-Bias[Option]];
end;

function  Remove(Var List:Core.Arrays.Types.VarString; Value:Core.Strings.VarString):System.boolean;
var
  iIndex:Int64;
  iCount:Int64;
  iLcv:LongInt;
begin
  Result:=false;
  iIndex:=IndexOf(List,Value);
  if iIndex<>-1 then begin
    iCount:=System.Length(List);
    for iLcv:=iIndex to iCount-2 do
      List[iLcv]:=List[iLcv+1];
    System.SetLength(List,iCount-1);
    Result:=true;
  end;
end;

function  Remove(Var List:Core.Arrays.Types.VarString; Index:LongInt):boolean;
var
  iCount:LongInt;
  iLcv:LongInt;
begin
  Result:=false; iCount:=System.Length(List);
  if (Index<>-1) and (Index<iCount) then begin
    for iLcv:=Index to iCount-2 do
      List[iLcv]:=List[iLcv+1];
    System.SetLength(List,iCount-1);
  end;
end;

procedure Copy(Var Source,Destination:Core.Arrays.Types.VarString);
var
  iLcv,iCount:LongInt;
begin
  iCount:=Length(Source);
  SetLength(Destination,iCount);
  For iLcv:=0 to iCount-1 do
    Destination[iLcv]:=Source[iLcv];
end;

procedure Copy(Var Source,Destination:Core.Arrays.Types.StringManifest);
var
  iLcv,iCount:LongInt;
begin
  Empty(Destination);
  iCount:=Length(Source);
  SetLength(Destination,iCount);
  for iLcv:=0 to iCount-1 do
    Copy(Source[iLcv],Destination[iLcv]);
end;

function Random(var List:Core.Arrays.Types.VarString):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  iLen:=System.Length(List);
  if iLen>0 then begin
    Result:=List[System.Random(iLen)];
  end else
    System.SetLength(Result,0);
end;

Function   GenerateRandomString(Const MinLength:LongInt=8; Const MaxLength:LongInt=255):Core.Strings.VarString;
Const
  Numbers:Core.Strings.VarString='0123456789';
  L_Letters:Core.Strings.VarString='abcdefghijklmnopqrstuvwxyz';
  U_Letters:Core.Strings.VarString='ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  Symbols:Core.Strings.VarString='-._#*';
var
  iStop:LongInt;
  Length,iLcv:LongInt;
begin
  if MinLength<>MaxLength then begin
    Length:=System.Random(MaxLength+1);
    If (Length<MinLength) then
      Inc(Length,MinLength);
    if (Length>MaxLength) then
      Length:=MaxLength;
  end else
    Length:=MinLength;

  System.SetLength(Result,Length); iLcv:=1;
  iStop:=Length+1;
  While (iLcv<iStop) do begin
    Case System.Random(4) of
      0: Result[iLcv]:=L_Letters[System.Random(26)+1];
      1: Result[iLcv]:=U_Letters[System.Random(26)+1];
      2: Result[iLcv]:=Symbols[System.Random(5)+1];
      3: Result[iLcv]:=Numbers[System.Random(9)+1];
    End;
    Inc(iLcv);
  end;
end;

end.


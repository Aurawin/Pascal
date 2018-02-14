unit Core.Utils.Unix.Account;

interface
uses
  types,

  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  SysUtils;
const
  FMT_PROCESS_USER_HINT = '%s (%s)';
  FMT_GET_ID            = 'id %s';
  FMT_PROCESS_USER_NOT_FOUND = 'User %s not found!';
type

  TNixGroup=record
    ID       : QWord;
    Name     : Core.Strings.VarString;
  end;
  TNixGroups=Array of TNixGroup;

  TNixUser=record
    ID       : QWord;
    Name     : Core.Strings.VarString;
    Group    : TNixGroup;
    Groups   : TNixGroups;
  end;

  procedure Empty(Var Item:TNixUser); overload;
  procedure Empty(Var Item:TNixGroup); overload;
  procedure Empty(Var Item:TNixGroups); overload;

  procedure Done(Var Item:TNixUser); overload;
  procedure Done(Var Item:TNixGroup); overload;
  procedure Done(Var Item:TNixGroups); overload;

  procedure Init(Var Item:TNixUser); overload;
  procedure Init(Var Item:TNixGroup); overload;
  procedure Init(Var Item:TNixGroups); overload;

  function  Find(Username:Core.Strings.VarString; var Item:TNixUser):boolean; overload;
  function  Load(sData:Core.Strings.VarString; var Item:TNixUser):boolean; overload;
  function  Load(sData:Core.Strings.VarString; Out ID:QWord; Out Name:Core.Strings.VarString):boolean; overload;

  function  UserID(value:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;
  function  GroupID(value:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;

  function  UserMod(sUser,sID:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;
  function  GroupMod(sGroup,sID:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;

  function  UserAdd(sUser,sGroup,sHome,sID:Core.Strings.VarString; out sError:Core.Strings.VarString; const bSystem:boolean=true):QWord;
  function  GroupAdd(sGroup,sID:Core.Strings.VarString; out sError:Core.Strings.VarString; const bSystem:boolean=true):QWord;

  function  Add(var List:TNixGroups; ID:QWord; Name:Core.Strings.VarString): LongInt; overload;
implementation
uses Process;

procedure Empty(Var Item:TNixUser);
begin
  Item.ID:=0;
  SetLength(Item.Name,0);
  Empty(Item.Group);
  Empty(Item.Groups);
end;

procedure Empty(Var Item:TNixGroup);
begin
  Item.ID:=0;
  SetLength(Item.Name,0);
end;

procedure Empty(Var Item:TNixGroups);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Done(Var Item:TNixUser);
begin
  Item.ID:=0;
  Finalize(Item.Name);
  Done(Item.Group);
  Done(Item.Groups);
  Finalize(Item);
end;

procedure Done(Var Item:TNixGroup);
begin
  Item.ID:=0;
  Finalize(Item.Name);
  Finalize(Item);
end;

procedure Done(Var Item:TNixGroups);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure Init(Var Item:TNixUser);
begin
  Item.ID:=0;
  SetLength(Item.Name,0);
  Init(Item.Group);
  Init(Item.Groups);
end;

procedure Init(Var Item:TNixGroup);
begin
  Item.ID:=0;
  SetLength(Item.Name,0);
end;

procedure Init(Var Item:TNixGroups);
begin
  Empty(Item);
end;

function  Add(var List:TNixGroups; ID:QWord; Name:Core.Strings.VarString): LongInt;
begin
  Result:=System.Length(List);
  System.SetLength(List,Result+1);
  List[Result].ID:=ID;
  List[Result].Name:=Name;
end;

function  UserMod(sUser,sID:Core.Strings.VarString;out sError:Core.Strings.VarString):QWord;
var
  Process:TProcess;
  iRead:QWord;
  iDX:Int64;
begin
  Result:=0;
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Concat('usermod -u ',sID,' ',sUser);
    Process.Execute();

    iRead:=Process.Stderr.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sError,iRead);
      Process.Stderr.Read(sError[1],iRead);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
      Exit;
    end;
    Result:=UserID(sUser,sError);
  finally
    FreeAndNil(Process);
  end;
end;

function  UserID(value:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;
var
  Process:TProcess;
  iRead:QWord;
  sData:Core.Strings.VarString;
  iDX:QWord;
begin
  Result:=0; System.SetLength(sError,0);
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Concat('id -u ',value);
    Process.Execute();
    iRead:=Process.Stderr.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sError,iRead);
      Process.Stderr.Read(sError[1],iRead);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
    end;
    iRead:=Process.Output.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sData,iRead);
      Process.Output.Read(sData[1],iRead);
      iDX:=System.Pos(#10,sData);
      if iDX>0 then
        System.SetLength(sData,iDX-1);
      Result:=StrToQWordDef(sData,0);
    end;
    SetLength(sData,0);
  finally
    FreeAndNil(Process);
  end;
end;

function  GroupMod(sGroup,sID:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;
var
  Process:TProcess;
  iRead:QWord;
  iDX:QWord;
begin
  Result:=0;
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Concat('groupmod -g ',sID,' ',sGroup);
    Process.Execute();
    iRead:=Process.Stderr.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sError,iRead);
      Process.Stderr.Read(sError[1],iRead);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
      Exit;
    end;
    Result:=GroupID(sGroup,sError);
  finally
    FreeAndNil(Process);
  end;
end;


function  UserAdd(sUser,sGroup,sHome,sID:Core.Strings.VarString; out sError:Core.Strings.VarString; const bSystem:boolean=true):QWord;
var
  Process:TProcess;
  iRead:QWord;
  iDX:QWord;
Const
  FMT_SYSTEM:Array[Boolean] of Core.Strings.VarString=('','-r -M ');
  FMT_USER_ADD:Core.Strings.VarString='useradd %s-g %s -d %s -u %s %s';
begin
  Result:=0;
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Format(FMT_USER_ADD,[FMT_SYSTEM[bSystem],sGroup,sHome,sID,sGroup]);
    //'useradd -g ',sGroup,' -d ',sHome,' -u ',sID,' ',sGroup);
    Process.Execute();
    iRead:=Process.Stderr.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sError,iRead);
      Process.Stderr.Read(sError[1],iRead);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);

      Exit;
    end;
    Result:=GroupID(sGroup,sError);
  finally
    FreeAndNil(Process);
  end;
end;

function  GroupAdd(sGroup,sID:Core.Strings.VarString; out sError:Core.Strings.VarString; const bSystem:boolean=true):QWord;
var
  Process:TProcess;
  iRead:QWord;
  iDX:QWord;
Const
  FMT_SYSTEM:Array[Boolean] of Core.Strings.VarString=('','-r ');
  FMT_GROUP_ADD:Core.Strings.VarString='groupadd %s-g %s %s';
begin
  Result:=0;
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Format(FMT_GROUP_ADD,[FMT_SYSTEM[bSystem],sID,sGroup]);
    Process.Execute();
    iRead:=Process.Stderr.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sError,iRead);
      Process.Stderr.Read(sError[1],iRead);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
      Exit;
    end else
      Result:=StrToQWordDef(sID,0);
  finally
    FreeAndNil(Process);
  end;
end;

function  GroupID(value:Core.Strings.VarString; out sError:Core.Strings.VarString):QWord;
var
  Process:TProcess;
  iRead:LongInt;
  sData:Core.Strings.VarString;
  iDX:QWord;
begin
  Result:=0;
  System.SetLength(sError,0);
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Concat('id -g ',value);
    Process.Execute();

    iRead:=Process.Stderr.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sError,iRead);
      Process.Stderr.Read(sError[1],iRead);
      iDX:=System.Pos(#10,sError);
      if iDX>0 then
        System.SetLength(sError,iDX-1);
    end;

    iRead:=Process.Output.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sData,iRead);
      Process.Output.Read(sData[1],iRead);
      iDX:=System.Pos(#10,sData);
      if iDX>0 then
        System.SetLength(sData,iDX-1);
      Result:=StrToQWordDef(sData,0);
    end;
    SetLength(sData,0);
  finally
    FreeAndNil(Process);
  end;
end;

function Find(Username:Core.Strings.VarString; var Item:TNixUser):boolean;
var
  Process:TProcess;
  iRead:LongInt;
  sData:Core.Strings.VarString;
begin
  Result:=false;
  Process:=TProcess.Create(Nil);
  Try
    Process.Options:=Process.Options + [poWaitOnExit,poUsePipes,poNoConsole];
    Process.CommandLine:=Format(FMT_GET_ID,[Username]);
    Process.Execute;
    iRead:=Process.Output.NumBytesAvailable;
    if iRead>0 then begin
      SetLength(sData,iRead);
      Process.Output.Read(sData[1],iRead);
      Result:=Load(sData,Item);
    end;
  finally
    FreeAndNil(Process);
  end;
end;

function Load(sData:Core.Strings.VarString; var Item:TNixUser):boolean;
var
  kplData:Core.Arrays.Types.KeyStrings;
  saGroups:Core.Arrays.Types.VarString;
  iLcv:LongInt;
  iIndex:LongInt;
  sName:Core.Strings.VarString;
  iID:QWord;
begin
  Result:=false; Empty(Item);
  Core.Arrays.KeyString.fromString(kplData,sData,'=',' ');
  Try
    if Load(Core.Arrays.KeyString.GetItemByKey(kplData,'uid'),Item.ID,Item.Name) then begin
      if Load(Core.Arrays.KeyString.GetItemByKey(kplData,'gid'),Item.Group.ID,Item.Group.Name) then begin
        Core.Arrays.VarString.fromString(@saGroups,Core.Arrays.KeyString.GetItemByKey(kplData,'groups'),',');
        Try
          Result:=True;
          for iLcv:=0 to High(saGroups) do begin
            if Load(saGroups[iLcv],iID,sName) then
              Add(Item.Groups,iID,sName);
          end;
        finally
          Empty(saGroups);
        end;
      end;
    end;

  finally
    Empty(kplData);
  end;
end;

function Load(sData:Core.Strings.VarString; Out ID:QWord; Out Name:Core.Strings.VarString):boolean;
var
  iStart:LongInt;
  iEnd:LongInt;
  iCopyLen:LongInt;
begin
  Result:=false;
  //                   11111
  //         '12345678901234'
  // sData = '1000(Username)';

  iStart:=Pos('(',sData);
  ID:=0;
  SetLength(Name,0);
  if iStart<>0 then begin
    ID:=StrToIntDef(System.Copy(sData,1,iStart-1),0);
    iEnd:=Pos(')',sData);
    if iEnd=0 then begin
      iEnd:=System.Length(sData);
      iCopyLen:=iEnd-iStart;
    end else begin
      iCopyLen:=iEnd-iStart-1;
    end;
    Name:=System.Copy(sData,iStart+1,iCopyLen);
    Result:=true;
  end;
end;

end.




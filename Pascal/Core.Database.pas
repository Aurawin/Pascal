unit Core.Database;

interface

uses
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.LargeInt,
  Core.Arrays.LargeWord,
  Core.Strings,
  Core.Database.Commands,
  Core.Database.Types,
  Core.Database.SQL,
  db;



  function Like_Prep(sQuery:Core.Strings.VarString):Core.Strings.VarString;
  function IniGetSection(Var Table:Core.Database.Types.Table):Core.Strings.VarString;
  Function CheckTable(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable):System.Boolean;



  procedure Copy(Var Source,Destination:Core.Database.Types.TableIni); overload;

  procedure Empty(Var Item:Core.Database.Types.Table); overload;
  procedure Empty(Var Item:Core.Database.Types.Tables); overload;
  procedure Empty(Var Item:Core.Database.Types.TableIni); overload;

  procedure Empty(Var Item:Core.Database.Types.Field); overload;
  procedure Empty(Var Item:Core.Database.Types.Fields); overload;
  procedure Empty(Var Item:Core.Database.Types.Commands); overload;
  procedure Empty(Var Item:Core.Database.Types.CommandParameters); overload;

  procedure Done(Var Item:Core.Database.Types.Field); overload;
  procedure Done(Var Item:Core.Database.Types.Fields); overload;
  procedure Done(Var Item:Core.Database.Types.Commands); overload;
  procedure Done(Var Item:Core.Database.Types.CommandParameters); overload;


  procedure Done(Var Item:Core.Database.Types.Table); overload;
  procedure Done(Var Item:Core.Database.Types.TableIni); overload;
  procedure Done(Var Item:Core.Database.Types.Tables); overload;


  procedure Init(Var Item:Core.Database.Types.Table; Var Info:Core.Database.Types.TableIni); overload;
  procedure Init(Var Item:Core.Database.Types.TableIni); overload;

  Function  IndexOf(sName:Core.Strings.VarString; Var List:Core.Database.Types.Tables):Core.Database.Types.Integer; overload;
  Function  IndexOf(sName:Core.Strings.VarString; FieldsP:Core.Database.Types.PFields):Core.Database.Types.Integer; overload;
  Function  IndexOf(CommandsP:Core.Database.Types.PCommands; Scanner:Core.Database.Types.CommandUse):Core.Database.Types.Integer; overload;

  procedure SetFieldCommandValue(Source:TField; var Command:Core.Database.Types.Command; CommandsP:Core.Database.Types.PCommands); overload;

  Function  FieldName(iPropertyID:LongInt; FieldsP:Core.Database.Types.PFields):Core.Strings.VarString;
  Function  FieldDataType(iPropertyID:LongInt; FieldsP:Core.Database.Types.PFields):Core.Database.Types.FieldType; overload;
  Function  FieldDataType(Name:Core.Strings.VarString; CommandsP:Core.Database.Types.PCommands):Core.Database.Types.FieldType; overload;
  Function  FieldPropertyID(Name:Core.Strings.VarString; CommandsP:Core.Database.Types.PCommands):Core.Database.Types.Integer;
  Function  FieldTypeToSQLType(Value:Core.Database.Types.FieldType):Core.Database.Types.Integer;

  // Add Parameters to be bound later
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Bool; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Byte; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Word; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Integer; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeInt; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWord; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeIntArray; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWordArray; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Float; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Double; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.MD5Digest; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.VarString; Var List:Core.Database.Types.CommandParameters); overload;
  procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Buffer; Var List:Core.Database.Types.CommandParameters); overload;

  procedure AddField(FieldP:Core.Database.Types.PField; TableP:Core.Database.Types.PTable); overload;
  procedure AddField(Var iCount:Core.Database.Types.Integer; AutoCreate:Boolean; IDP:Core.Database.Types.PInteger; KeyP:Core.Strings.PVarString; Flags,Precision:Core.Database.Types.Integer; Kind:Core.Database.Types.FieldType; Var Fields:Core.Database.Types.Fields); overload;
  procedure AddTable(TableP:Core.Database.Types.PTable; TablesP:Core.Database.Types.PTables); overload;

  Function  AddCommand(Var Command:Core.Database.Types.Command; Var Commands:Core.Database.Types.Commands): LongInt; overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; CommandsP:Core.Database.Types.PCommands); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Bool; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Byte; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Word; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Integer; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeInt; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeWord; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeIntArray; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeWordArray; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Float; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Double; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.MD5Digest; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.VarString; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Buffer; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;
  procedure AddCommand(Var iCount:LongInt; Operation:Core.Strings.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Value:Pointer; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil); overload;

  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; CommandsP:Core.Database.Types.PCommands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeInt; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeWord; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeIntArray; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeWordArray; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Bool; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Byte; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Integer; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Word; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Float; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Double; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.MD5Digest; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.VarString; Var Commands:Core.Database.Types.Commands); overload;
  procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Buffer; Var Commands:Core.Database.Types.Commands); overload;

  procedure AddCommand(Var iCount:LongInt; Usage:Core.Database.Types.CommandUse; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands); overload;

  procedure Sort(ParametersP:PCommandParameters);

  function Prepare(var sStatement:Core.Strings.VarString; Const Encase:Boolean=False):Core.Strings.VarString;

  function fromString(Item:Core.Strings.VarString; var Mode:Core.Database.Types.Driver):System.boolean; overload;

  function toInCriteria(Var List:Core.Arrays.Types.LargeInt):Core.Strings.VarString; overload;
  function toInCriteria(Var List:Core.Arrays.Types.LargeWord):Core.Strings.VarString; overload;



implementation

uses
  SysUtils,StrUtils;


function fromString(Item:Core.Strings.VarString; var Mode:Core.Database.Types.Driver):System.boolean;
var
  iLcv:Core.Database.Types.Driver;
begin
  Result:=False;
  for iLcv:=Low(Core.Database.Types.Driver) to High(Core.Database.Types.Driver) do begin
    if SameText(DB_Modes[iLcv],Item) then begin
      Result:=true;
      Mode:=iLcv;
      Break;
    end;
  end;
end;

function Like_Prep(sQuery:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=Concat('%',sQuery,'%');
  Result:=StringReplace(Result,'*','%',[rfReplaceAll]);
  Result:=StringReplace(Result,'%%','%',[rfReplaceAll]);
end;

function IniGetSection(Var Table:Core.Database.Types.Table):Core.Strings.VarString;
begin
  Result:=Concat(Table.StartupP^.Group,'/',Table.StartupP^.Name);
end;

Function CheckTable(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable):System.Boolean;
{$i Core.Database.CheckTable.Declarations.inc}
begin
  {$i Core.Database.CheckTable.Method.inc}
end;

procedure Empty(Var Item:Core.Database.Types.Table);
begin
  Empty(Item.Fields);
  Empty(Item.Name);
  Item.StartupP:=nil;
end;

procedure Empty(Var Item:Core.Database.Types.Tables);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Empty(Var Item:Core.Database.Types.TableIni);
begin
  Item.AutoCreate:=false;
  SetLength(Item.Group,0);
  SetLength(Item.Hint,0);
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
  Item.PrimaryKeyP:=nil;
end;


function toString(var Value:Core.Database.Types.MD5Digest):Core.Database.Types.VarString;
begin
  SetLength(Result,SizeOf(Value));
  System.Move(Value[0],Result[1],SizeOf(Value));
end;

procedure Copy(Var Source,Destination:Core.Database.Types.TableIni);
begin
  Destination.AutoCreate:=Source.AutoCreate;
  Destination.Group:=Source.Group;
  Destination.Hint:=Source.Hint;
  Destination.Name:=Source.Name;
  Destination.Value:=Source.Value;
  Destination.PrimaryKeyP:=Source.PrimaryKeyP;
end;

procedure Empty(Var Item:Core.Database.Types.CommandParameters);
begin
  SetLength(Item,0);
end;

procedure Empty(Var Item:Core.Database.Types.Commands);
begin
  {
  For iLcv:=0 to High(Commands) do
    Finalize(Commands[iLcv]);
  }
  SetLength(Item,0);
end;

procedure Empty(Var Item:Core.Database.Types.Field);
begin
  Item.IDP:=nil;
  Item.KeyP:=nil;

  Item.AutoCreate:=False;
  Item.Verified:=false;
  Item.DataType:=dftUnknown;
  Item.Flags:=0;
  Item.Precision:=0;
end;

procedure Empty(Var Item:Core.Database.Types.Fields);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Done(Var Item:Core.Database.Types.CommandParameters);
begin
  Finalize(Item);
end;

procedure Done(Var Item:Core.Database.Types.Commands);
begin
  Finalize(Item);
end;
procedure Done(Var Item:Core.Database.Types.Field);
begin
  Item.IDP:=nil;
  Item.KeyP:=nil;

  Finalize(Item);
end;

procedure Done(Var Item:Core.Database.Types.Fields);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure Done(Var Item:Core.Database.Types.Table);
begin
  Done(Item.Fields);
  Finalize(Item.Name);
  Finalize(Item);
end;

procedure Done(Var Item:Core.Database.Types.Tables);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

procedure Done(Var Item:Core.Database.Types.TableIni);
begin
  Item.PrimaryKeyP:=nil;
  Finalize(Item.Group);
  Finalize(Item.Value);
  Finalize(Item.Name);
  Finalize(Item.Hint);
  Finalize(Item);
end;

procedure Init(Var Item:Core.Database.Types.Table; Var Info:Core.Database.Types.TableIni);
begin
  Empty(Item.Fields);
  Item.Name:=Info.Value;
  Item.StartupP:=@Info;
end;


procedure Init(Var Item:Core.Database.Types.TableIni);
begin
  Item.PrimaryKeyP:=nil;
  Item.AutoCreate:=false;
  SetLength(Item.Group,0);
  SetLength(Item.Hint,0);
  SetLength(Item.Name,0);
  SetLength(Item.Value,0);
end;

Function   Prepare(var sStatement:Core.Database.Types.VarString; Const Encase:Boolean=False): Core.Database.Types.VarString;
Const
 SQ=#39;
 SQ2=SQ+SQ;
begin
  //If Value='' then Value:=' ';
  If Encase then
    Result:=Concat(SQ,SysUtils.StringReplace(sStatement,SQ,SQ2,[rfReplaceAll]),SQ)
  else
    Result:=SysUtils.StringReplace(sStatement,SQ,SQ2,[rfReplaceAll]);
end;

procedure Sort(ParametersP:PCommandParameters);
var
  iCount,iLcv:Core.Database.Types.Integer;
  Parameter:Core.Database.Types.CommandParameter;
begin
  iCount:=Length(ParametersP^);iLcv:=0;
  While iLcv<iCount-1 do begin
    If ParametersP^[iLcv+1].Location<ParametersP^[iLcv].Location then begin
      Parameter:=ParametersP^[iLcv];
      ParametersP^[iLcv]:=ParametersP^[iLcv+1];
      ParametersP^[iLcv+1]:=Parameter;
      iLcv:=-1;
    end;
    Inc(iLcv);
  end;
end;

Function  FieldDataType(Name:Core.Database.Types.VarString; CommandsP:Core.Database.Types.PCommands):Core.Database.Types.FieldType;
var
  iCount,iLcv:Core.Database.Types.Integer;
begin
  Result:=Core.Database.Types.dftUnknown; iLcv:=0; iCount:=Length(CommandsP^);
  While (iLcv<iCount) and (Result=dftUnknown) do begin
    If SysUtils.SameText(CommandsP^[iLcv].Operation,Name) then
      Result:=CommandsP^[iLcv].DataType;
    Inc(iLcv);
  end;
end;

Function  FieldPropertyID(Name:Core.Database.Types.VarString; CommandsP:Core.Database.Types.PCommands):Core.Database.Types.Integer;
var
  iCount,iLcv:Core.Database.Types.Integer;
begin
  Result:=-1; iLcv:=0; iCount:=System.Length(CommandsP^);
  While (iLcv<iCount) and (Result=-1) do begin
    If SysUtils.SameText(CommandsP^[iLcv].Operation,Name) then
      Result:=CommandsP^[iLcv].PropertyID;
    Inc(iLcv);
  end;
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Bool; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftBoolean) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftBoolean');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;



procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Byte; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftByte) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftByte');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;


procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Word; Var List:Core.Database.Types.CommandParameters);
var
  Index:LongInt;
begin
  If (DataType<>dftWord) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftWord');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Integer; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftInteger) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftInteger');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;



procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeInt; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftInt64) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftInt64');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWord; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftQword) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftQword');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;


procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeIntArray; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftInt64Array) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftInt64Array');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.LargeWordArray; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftQWordArray) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftQWordArray');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Float; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftFloat) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftFloat');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Double; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftDouble) and (DataType<>dftDateTime) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftDouble');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.MD5Digest; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
begin
  If (DataType<>dftMD5Digest) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftMD5Digest');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.SizeOf(Value);
end;

procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.VarString; Var List:Core.Database.Types.CommandParameters);
var
  Index:Core.Database.Types.Integer;
  iLen:Core.Database.Types.Integer;
begin
  If not (DataType in [dftSmallString,dftString,dftMemo]) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftSmallString or dftString or dftMemo');
  iLen:=System.Length(Value);

  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=iLen;
end;


procedure AddCommandParameter(DataType:Core.Database.Types.FieldType; Var Value:Core.Database.Types.Buffer; Var List:Core.Database.Types.CommandParameters);
var
  Index:LongInt;
begin
  If  (DataType<>dftByteBuffer) then Raise Exception.Create('Invalid DataType or Datatype Mismatch: dftByteBuffer');
  Index:=Length(List);
  SetLength(List,Index+1);
  List[Index].DataType:=DataType;
  List[Index].Value:=@Value;
  List[Index].Index:=Index+1;
  List[Index].Size:=System.Length(Value);
end;

procedure AddField(Var iCount:Core.Database.Types.Integer; AutoCreate:Boolean; IDP:Core.Database.Types.PInteger; KeyP:Core.Strings.PVarString; Flags,Precision:Core.Database.Types.Integer; Kind:Core.Database.Types.FieldType; Var Fields:Core.Database.Types.Fields);
begin
  If (Kind=dftBoolean) and (Precision=0) then
    Precision:=8;

  SetLength(Fields,iCount+1);
  Fields[iCount].IDP:=IDP;
  Fields[iCount].KeyP:=KeyP;
  Fields[iCount].AutoCreate:=AutoCreate;
  Fields[iCount].Flags:=Flags;
  Fields[iCount].Precision:=Precision;
  Fields[iCount].DataType:=Kind;
  Fields[iCount].Verified:=false;
  //hDatabase.SeCore.Database.Types.FieldType(Fields[iCount],Kind);
  Inc(iCount);
end;

Function  IndexOf(sName:Core.Database.Types.VarString; Var List:Core.Database.Types.Tables):Core.Database.Types.Integer;
var
  iLen,iLcv:Core.Database.Types.Integer;
begin
  Result:=-1; iLcv:=0; iLen:=Length(List);
  While (iLcv<iLen) and (Result=-1) do begin
    If Core.Strings.SameText(sName,List[iLcv].Name) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

Function  IndexOf(sName:Core.Database.Types.VarString; FieldsP:Core.Database.Types.PFields):Core.Database.Types.Integer;
var
  iLcv:Core.Database.Types.Integer;
begin
  Result:=-1;
  for iLcv:=0 to System.High(FieldsP^) do begin
    If Core.Strings.SameText(sName,FieldsP^[iLcv].KeyP^) then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

Function  IndexOf(CommandsP:Core.Database.Types.PCommands; Scanner:Core.Database.Types.CommandUse):Core.Database.Types.Integer;
var
  iLcv:Core.Database.Types.Integer;
begin
  Result:=-1;
  for iLcv:=0 to System.High(CommandsP^) do begin
    If CommandsP^[iLcv].Useage=Scanner then begin
      Result:=iLcv;
      Break;
    end;
  end;
end;

Function  FieldName(iPropertyID:LongInt; FieldsP:Core.Database.Types.PFields):Core.Database.Types.VarString;
var
  iLcv:Core.Database.Types.Integer;
begin
  Result:=''; iLcv:=0;
  for iLcv:=0 to High(FieldsP^) do begin
    If ((FieldsP^[iLcv].IDP<>nil) and (FieldsP^[iLcv].IDP^=iPropertyID) and (FieldsP^[iLcv].KeyP<>nil) ) then begin
      Result:=FieldsP^[iLcv].KeyP^;
      Break;
    end;
  end;
end;

Function  FieldDataType(iPropertyID:LongInt; FieldsP:Core.Database.Types.PFields):Core.Database.Types.FieldType;
var
  iLcv:Core.Database.Types.Integer;
begin
  Result:=Core.Database.Types.dftUnknown;
  for iLcv:=0 to High(FieldsP^) do begin
    If ( (FieldsP^[iLcv].IDP<>nil) and (FieldsP^[iLcv].IDP^=iPropertyID) )  then begin
       Result:=FieldsP^[iLcv].DataType;
       Break;
    end;
  end;
end;

procedure AddField(FieldP:Core.Database.Types.PField; TableP:Core.Database.Types.PTable);
var
  iLen:Core.Database.Types.Integer;
begin
  If (FieldP^.DataType=dftBoolean) and (FieldP^.Precision=0) then
    FieldP^.Precision:=8;
  iLen:=Length(TableP^.Fields);
  SetLength(TableP^.Fields,iLen+1);

  TableP^.Fields[iLen].AutoCreate:=FieldP^.AutoCreate;
  TableP^.Fields[iLen].IDP:=FieldP^.IDP;
  TableP^.Fields[iLen].KeyP:=FieldP^.KeyP;
  TableP^.Fields[iLen].DataType:=FieldP^.DataType;
  TableP^.Fields[iLen].Precision:=FieldP^.Precision;
  TableP^.Fields[iLen].Verified:=false;
  TableP^.Fields[iLen].Flags:=FieldP^.Flags;

end;

procedure AddTable(TableP:Core.Database.Types.PTable; TablesP:Core.Database.Types.PTables);
var
  iIndex:LongInt;
  iLen:LongInt;
  iLcv:LongInt;
begin
  iIndex:=IndexOf(TableP^.Name,TablesP^);
  if iIndex=-1 then begin
    iLen:=Length(TablesP^);
    SetLength(TablesP^,iLen+1);
    TablesP^[iLen].Name:=TableP^.Name;
    TablesP^[iLen].StartupP:=TableP^.StartupP;
    For iLcv:=0 to Length(TableP^.Fields) do
      AddField(@TableP^.Fields[iLcv],@TablesP^[iLen].Fields);
  end else
    Exception.Create(Format(FMT_TABLE_DUPLICATED_EXCEPTION,[TableP^.Name]));
end;

Function AddCommand(Var Command:Core.Database.Types.Command; Var Commands:Core.Database.Types.Commands): LongInt;
begin
  Result:=Length(Commands);
  SetLength(Commands,Result+1);
  Commands[Result]:=Command;
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,TableP^.Name,Usage,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeInt; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeWord; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeIntArray; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.LargeWordArray; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Bool; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Byte; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Integer; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Word; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Float; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Double; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.MD5Digest; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.VarString; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;


procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; Usage:Core.Database.Types.CommandUse; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; Var Value:Core.Database.Types.Buffer; Var Commands:Core.Database.Types.Commands);
begin
  AddCommand(iCount,FieldName(PropertyID,@TableP^.Fields),Usage,FieldDataType(PropertyID,@TableP^.Fields),PropertyID,Value,PreOp,OP,@Commands);
end;

procedure AddCommand(Var iCount:LongInt; TableP:Core.Database.Types.PTable; CommandsP:Core.Database.Types.PCommands);
Const
  Value:Pointer=Nil;
var
  Operation:Core.Database.Types.SmallString;
  Usage:Core.Database.Types.CommandUse;
  DataType:Core.Database.Types.FieldType;
  PropertyID:Core.Database.Types.Integer;
  PreOp:Core.Database.Types.SQLPreOperator;
  Op:Core.Database.Types.SQLOperator;
begin
  Operation:=TableP^.Name;
  Usage:=useAsTable;
  DataType:=dftUnknown;
  PropertyID:=0;
  PreOp:=poNone;
  Op:=oNone;
  {$i Core.Database.AddCommand.inc}
  CommandsP^[iCount-1].Value:=Pointer(TableP);
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; CommandsP:Core.Database.Types.PCommands);
Const
  DataType=DATATYPE_VOID;
  PropertyID=PROPERTY_ID_VOID;
  Value:Pointer=Nil;
  PreOp=poNone;
  Op=oNone;
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands);
Const
  DataType=DATATYPE_VOID;
  PropertyID=PROPERTY_ID_VOID;
  Value:Pointer=Nil;
  PreOp=poNone;
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands);
Const
  DataType=DATATYPE_VOID;
  PropertyID=PROPERTY_ID_VOID;
  Value:Pointer=Nil;
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Usage:Core.Database.Types.CommandUse; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands);
Const
  DataType=DATATYPE_VOID;
  PropertyID=PROPERTY_ID_VOID;
  Value:Pointer=Nil;
  Operation='';
  PreOp=poNone;
begin

{$i Core.Database.AddCommand.inc}
end;


procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; PreOp:Core.Database.Types.SQLPreOperator; Op:Core.Database.Types.SQLOperator; CommandsP:Core.Database.Types.PCommands);
Const
  Value:Pointer=Nil;
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Bool; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Byte; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Word; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Integer; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeInt; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeWord; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeIntArray; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.LargeWordArray; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Float; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Double; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.MD5Digest; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;


procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.VarString; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;
procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Var Value:Core.Database.Types.Buffer; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone;  CommandsP:Core.Database.Types.PCommands=nil);
begin
{$i Core.Database.AddCommand.inc}
end;

procedure AddCommand(Var iCount:LongInt; Operation:Core.Database.Types.VarString; Usage:Core.Database.Types.CommandUse; DataType:Core.Database.Types.FieldType; PropertyID:Core.Database.Types.Integer; Value:Pointer; Const PreOp:Core.Database.Types.SQLPreOperator=poNone; Const Op:Core.Database.Types.SQLOperator=oNone; CommandsP:Core.Database.Types.PCommands=Nil);
begin
  SetLength(CommandsP^,iCount+1);
  CommandsP^[iCount].Operation:=Operation;
  CommandsP^[iCount].Useage:=Usage;
  CommandsP^[iCount].DataType:=DataType;
  CommandsP^[iCount].PropertyID:=PropertyID;
  CommandsP^[iCount].Value:=Value;
  CommandsP^[iCount].SQLPreOperator:=PreOp;
  CommandsP^[iCount].SQLOperator:=Op;
  Inc(iCount);
end;

procedure SetFieldCommandValue(Source:TField; var Command:Core.Database.Types.Command; CommandsP:Core.Database.Types.PCommands);
 {$i Core.Database.SetFieldCommandValue.inc}
begin
  Case Command.DataType of
    dftBoolean      : PushBoolean;
    dftByte         : PushByte;
    dftWord         : PushWord;
    dftInteger      : PushInteger;
    dftInt64        : PushLargeInt;
    dftQWord        : PushQWord;
    dftInt64Array   : PushInt64Array;
    dftQWordArray   : PushQWordArray;
    dftDateTime     : PushDateTime;
    dftFloat        : PushFloat;
    dftString       : PushString;
    dftDouble       : PushDouble;
    dftMemo         : PushString;
    dftByteBuffer   : PushByteArray;
  end;
end;

Function  FieldTypeToSQLType(Value:Core.Database.Types.FieldType):Core.Database.Types.Integer;
begin
  Case Value of
    dftBoolean      : Result:=SQL_BIT;
    dftByte         : Result:=SQL_SMALLINT;
    dftWord         : Result:=SQL_Integer;
    dftInteger      : Result:=SQL_Integer;
    dftInt64        : Result:=SQL_BIGINT;
    dftQword        : Result:=SQL_BIGINT;
    dftInt64Array   : Result:=SQL_LONGVARBINARY;
    dftDateTime     : Result:=SQL_DOUBLE;
    dftFloat        : Result:=SQL_REAL;
    dftDouble       : Result:=SQL_DOUBLE;
    dftString       : Result:=SQL_VARCHAR;
    dftSmallString  : Result:=SQL_VARCHAR;
    dftMemo         : Result:=SQL_TEXT;
    dftMD5Digest    : Result:=SQL_VARCHAR;
    dftByteBuffer   : Result:=SQL_VARCHAR;
  End;
end;


function toInCriteria(Var List:Core.Arrays.Types.LargeInt):Core.Database.Types.VarString;
var
  iLen:Core.Database.Types.Integer;
begin
  Core.Arrays.LargeInt.toString(List,',',Result);
  iLen:=System.Length(Result);
  if iLen>0 then
    System.SetLength(Result,iLen-1);
end;

function toInCriteria(Var List:Core.Arrays.Types.LargeWord):Core.Database.Types.VarString;
var
  iLen:Core.Database.Types.Integer;
begin
  Core.Arrays.LargeWord.toString(List,',',Result);
  iLen:=System.Length(Result);
  if iLen>0 then
    System.SetLength(Result,iLen-1);
end;

end.


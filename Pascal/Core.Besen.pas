unit Core.Besen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Besen,BESENOpcodes,BESENObjectPropertyDescriptor,BESENConstants, BESENValue,
  BESENObject, BESENErrors, BESENNativeObject,BESENASTNodes,BESENObjectArray,
  BESENObjectArrayConstructor,BESENObjectPrototype,BESENObjectArrayPrototype,BESENBaseObject,
  Core,Core.Strings,Core.Data,fpJSON,jsonparser;

Type
  TJSExceptionEvent=procedure(LineNumber:DWord; Message:Core.Strings.VarString) of Object;
  TJS=Class;

  TCore=Class(TBESENObject)
  Type
    Arrays=Class(TBESENObject)
    Type
      Byte=Class(TBESENObject)
      protected
        procedure Dump(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
        procedure Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
      end;
    protected
      procedure Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
    end;
    Data=Class(TBESENObject)
    protected
      Singleton                : Core.Data.Singleton;
      Collection               : Core.Data.Collection;
    protected
      procedure Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
    public
      constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Override;
      destructor  Destroy(); override;
    end;

  private
    Compiler                     : TJS;
  protected
    procedure JSONWrite(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
    procedure ObjectWrite(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
    procedure Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Override;
  end;


  TJS=Class(TBesen)
  private
    FCore                        : TCore;
    FCoreArrays                  : TCore.Arrays;
    FCoreArraysByte              : TCore.Arrays.Byte;
    FCoreData                    : TCore.Data;

    FOnException                 : TJSExceptionEvent;
  public
    procedure Run(sSource:Core.Strings.VarString);
  public
    constructor Create(ACompatibility:longword=0); override;
    destructor Destroy; override;
  published
    property OnException:TJSExceptionEvent read FOnException write FOnException;
  end;

implementation

constructor TJS.Create(ACompatibility:longword=0);
begin
  CodeLineInfo:=True;
  CodeTracable:=True;

  Inherited Create(ACompatibility);

  FCore:=TCore.Create(Self,TBESEN(Self).ObjectPrototype,false);
  GarbageCollector.Add(FCore);
  ObjectGlobal.OverwriteData('Core',BESENObjectValue(FCore),[]);
  FCore.RegisterNativeFunction('ObjectWrite',@FCore.ObjectWrite,1,[]);
  FCore.RegisterNativeFunction('JSONWrite',@FCore.JSONWrite,1,[]);

  FCoreArrays:=TCore.Arrays.Create(Self,FCore.PrototypeObject,false);
  GarbageCollector.Add(FCoreArrays);
  FCore.OverwriteData('Arrays',BESENObjectValue(FCoreArrays),[]);
  FCoreArrays.RegisterNativeFunction('Test',@FCore.Arrays.Test,1,[]);

  FCoreArraysByte:=TCore.Arrays.Byte.Create(Self,FCoreArrays.PrototypeObject,false);
  GarbageCollector.Add(FCoreArraysByte);
  FCoreArraysByte.DefineOwnProperty('Data',BESENDataPropertyDescriptor(BESENStringValue(''),[bopaWRITABLE,bopaCONFIGURABLE]),false);

  FCoreArrays.OverwriteData('Byte',BESENObjectValue(FCoreArraysByte),[]);
  FCoreArraysByte.RegisterNativeFunction('Test',@FCore.Arrays.Byte.Test,1,[]);
  FCoreArraysByte.RegisterNativeFunction('Dump',@FCore.Arrays.Byte.Dump,1,[]);

  FCoreData:=TCore.Data.Create(Self,FCore.PrototypeObject,false);
  GarbageCollector.Add(FCoreData);
  FCore.OverwriteData('Data',BESENObjectValue(FCoreData),[]);
  FCoreData.DefineOwnProperty('Singleton',BESENDataPropertyDescriptor(BESENObjectValue(FCoreData.Singleton),[bopaWRITABLE,bopaCONFIGURABLE]),false);
  FCoreData.DefineOwnProperty('Collection',BESENDataPropertyDescriptor(BESENObjectValue(FCoreData.Collection),[bopaWRITABLE,bopaCONFIGURABLE]),false);
end;

destructor TJS.Destroy;
begin
  Inherited Destroy();
end;

procedure TJS.Run(sSource:Core.Strings.VarString);
var
  Node    : TBESENASTNode;
begin
  //sSource:=Concat(CORE_PRECODE,sSource);
  Try
    Node:=Compile(sSource);
    Try
      Execute(Node,BESENUndefinedValue);
    finally
      Node.Free();
    end;
  Except
    On E:EBESENError do
      If Assigned(FOnException) then
        FOnException(LineNumber,Concat(E.Name,':',ToStr(E.Value),'. ',E.Message,'.'));
    On E:Exception do
      If Assigned(FOnException) then
        FOnException(LineNumber,Concat('Error unknown: ',E.Message,'.'));
  end;
end;

constructor TCore.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  Compiler:=AInstance as TJS;
  Inherited Create(AInstance,APrototype,AHasPrototypeProperty);
end;

procedure TCore.JSONWrite(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
var
  Parser : TJSONParser;
  Chunk  : TJSONData;
  OBJ    : TJSONObject;
  OBD    : TJSONData;
  K      : TJSONData;
  RC     : TJSONEnum;
  FLD    : TJSONEnum;


  OE : TJSONEnum;
  iLcv:LongInt;
  s:Core.Strings.VarString;
begin
  ResultValue.ValueType:=bvtNumber;
  ResultValue.Num:=0;


  If (Arguments^[0]^.ValueType=bvtSTRING) then begin
    Parser:=TJSONParser.Create(Arguments^[0]^.Str);
    Try
      Chunk:=Parser.Parse();
      Try
        if (Chunk is TJSONArray) then begin
          For RC in TJSONArray(Chunk) do begin
            if (RC.Value is TJSONData) then begin
              For FLD in TJSONData(RC.Value) do begin
                if (FLD.Value is TJSONString) then begin
                  s:=FLD.Value.AsString;
                end;
              end;
              ResultValue.Num:=1;

            end;
          end;
        end;
      Finally
        Chunk.Free();
      end;
    finally
      Parser.Free();
    end;

  end;
end;

procedure TCore.ObjectWrite(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
var
  obj:TObject;
  fld:TBESENObjectProperty;
  rc:TBESENObject;
  fv,rv:TBESENValue;
  List:TBESENObjectArray;
  iLcv,jLcv:LongInt;
  sName:Core.Strings.VarString;
  sValue:Core.Strings.VarString;

begin
  if (Arguments^[0]^.ValueType=bvtObject) then begin
    if (Arguments^[0]^.Obj is TBESENObjectArray) then begin
      List:=TBESENObjectArray(Arguments^[0]^.Obj);
      for iLcv:=0 to List.Len-1 do begin
        if List.GetArrayIndex(iLcv, rv,rc) then begin
          if (rv.ValueType=bvtObject) then begin
            rc:=TBESENObject(rv.Obj);
            for jLcv:=0 to rc.Properties.ItemCount-1 do begin
              if rc.Properties.Items[jLcv] is TBESENBaseObject then begin
                fld:=rc.Properties.Items[jLcv];
                sName:=fld.Key;
                if fld.Descriptor.Value.ValueType=bvtString then begin
                  sValue:=fld.Descriptor.Value.Str;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

constructor TCore.Data.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  Inherited Create(AInstance,APrototype,AHasPrototypeProperty);

  Singleton:=Core.Data.Singleton.Create(AInstance,TBESEN(AInstance).ObjectPrototype,AHasPrototypeProperty);
  TBESEN(Instance).GarbageCollector.Add(Singleton);

  Collection:=Core.Data.Collection.Create(AInstance,TBESEN(AInstance).ObjectPrototype,AHasPrototypeProperty);
  TBESEN(Instance).GarbageCollector.Add(Collection);
end;

destructor TCore.Data.Destroy();
begin
  Singleton.Free();
  Collection.Free();
  Inherited Destroy();
end;

procedure TCore.Data.Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin

end;

procedure TCore.Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  ResultValue.ValueType:=bvtSTRING;
  ResultValue.Str:='Core.Test';
end;

procedure TCore.Arrays.Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  ResultValue.ValueType:=bvtSTRING;
  ResultValue.Str:='Core.Arrays.Test';
end;

procedure TCore.Arrays.Byte.Test(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  ResultValue.ValueType:=bvtSTRING;
  ResultValue.Str:='Core.Arrays.Byte.Test';
end;

procedure TCore.Arrays.Byte.Dump(const ThisArgument:TBESENValue; Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  ResultValue.ValueType:=bvtSTRING;
  ResultValue.Str:='ok';
end;

end.


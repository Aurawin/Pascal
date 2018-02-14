unit Core.Data;

{$mode objfpc}{$H+}

interface

uses Core.Strings,BESEN,BESENObjectArray,BESENObjectArrayConstructor,BESENObjectPropertyDescriptor,BESENNativeObject,BESENNativeArrayObject,BESENValue,BESENObject,BESENObjectFunction;

Type
  Singleton=Class(TBESENNativeObject)
  protected
    FNameSpace:Core.Strings.VarString;
  protected
    procedure ConstructObject(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:LongInt); override;
  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Override;
  published
    property NameSpace:Core.Strings.VarString read FNameSpace write FNameSpace;
  end;

  Collection=Class(TBESENNativeObject)
  protected
    FNameSpace : Core.Strings.VarString;
    FItems     : TBESENObjectArray;
    FPageInfo  : TBESENObject;
  protected
    procedure ConstructObject(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:LongInt); override;
  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Override;
    destructor  Destroy(); override;
  published
    property NameSpace:Core.Strings.VarString read FNameSpace write FNameSpace;
    property Items:TBESENObjectArray read FItems write FItems;
  published
    procedure Read(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:LongInt;var AResult:TBESENValue);
  end;

implementation

constructor Singleton.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  Inherited Create(AInstance,APrototype,AHasPrototypeProperty);
end;

procedure Singleton.ConstructObject(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:LongInt);
begin
  Inherited ConstructObject(ThisArgument,Arguments,CountArguments);
  if CountArguments=1 then
    FNameSpace:=Arguments^[0]^.Str;

end;

constructor Collection.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  Inherited Create(AInstance,APrototype,AHasPrototypeProperty);

  FItems:=TBESENObjectArray.Create(AInstance,TBESEN(AInstance).ObjectArrayPrototype,false);
  OverwriteData('Items',BESENObjectValue(FItems),[bopaWRITABLE,bopaCONFIGURABLE]);
end;

procedure Collection.ConstructObject(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:LongInt);
begin
  Inherited ConstructObject(ThisArgument,Arguments,CountArguments);
  if CountArguments=1 then
    FNameSpace:=Arguments^[0]^.Str;
end;

destructor  Collection.Destroy();
begin
  FItems.Free();
  Inherited Destroy();
end;

procedure Collection.Read(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:LongInt;var AResult:TBESENValue);
begin
  //given namespace, use DMBS to retrieve item(s) in collection

end;

end.


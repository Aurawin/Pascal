unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  Besen, BESENConstants, BESENValue, BESENObject, BESENErrors, BESENNativeObject,BESENASTNodes,
  Core.Strings,Core.Besen;

type

  { TForm1 }

  TForm1 = class(TForm)
    Timer1: TTimer;
    txtSource: TMemo;
    txtOutput: TMemo;
    Splitter1: TSplitter;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
  private
    { private declarations }
    JS   : Core.Besen.TJS;
  protected
    procedure OnException(LineNumber:DWord; Message:Core.Strings.VarString);
  public
    procedure Log(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
  end;

  API=class(TBESENNativeObject)
  type
    Network=class(TBESENNativeObject)
    public
      procedure Test(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
    end;
    Database=class(TBESENNativeObject)
    public
      procedure Test(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
    end;
  end;

  TIcing=class(TBESENNativeObject)

  private
    FColor : String;
  protected
    procedure ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer); Override;
  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Overload; Override;
  published
    property Color: String read FColor write FColor;
  end;

  TBakedGoods = class(TBESENNativeObject)
  private
    fWeight: Integer;
    fName: String;
  protected
    procedure ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer); Override;
  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Overload; Override;
  published
    property Name: String read fName write FName;
    property Weight: Integer read fWeight write fWeight;
  end;

  TCake = class(TBakedGoods)
  private
    FIcing:TIcing;
  protected
    procedure ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer); Override;
  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Overload; Override;
    destructor Destroy();override;
  published
    property  Icing:TIcing read FIcing write FIcing;
    procedure MyTest(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
    procedure Eat(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
    procedure IsALie(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
    procedure Info(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
  end;

  TCookie = class(TBakedGoods)
  protected
    procedure ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer); Override;

  public
    constructor Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false); Overload; Override;
  published
    procedure Get(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
    procedure Info(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
  end;



var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure API.Network.Test(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  Form1.txtOutput.Lines.Add('API.Network.Test Called');
end;

procedure API.Database.Test(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  Form1.txtOutput.Lines.Add('API.Database.Test Called');
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  txtOutput.Lines.Clear();
  JS.Run(txtSource.Lines.Text);
end;

procedure TForm1.OnException(LineNumber:DWord; Message:Core.Strings.VarString);
begin
  txtOutput.Lines.Add(Concat(#8251,IntToStr(LineNumber),#8212,Message));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  JS:=Core.Besen.TJS.Create(COMPAT_JS);
  JS.OnException:=@OnException;
  JS.ObjectGlobal.RegisterNativeFunction('Log',@Log,1,[],false);


  JS.RegisterNativeObject('BakedGoods', TBakedGoods);
  JS.RegisterNativeObject('Cake', TCake);
  JS.RegisterNativeObject('Cookie', TCookie);
  JS.RegisterNativeObject('Icing',TIcing);

end;

constructor TBakedGoods.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  inherited Create(AInstance, APrototype, AHasPrototypeProperty);

  fName := 'Not baked yet.';
  Self.Weight := 0;
end;

procedure TForm1.Log(const ThisArgument:TBESENValue;Arguments:PPBESENValues;CountArguments:longint;var ResultValue:TBESENValue);
begin
  Form1.txtOutput.Lines.Add(
    Concat('Runtime: ',Arguments^[0]^.Str)
  );
end;

procedure TBakedGoods.ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  inherited ConstructObject(ThisArgument, Arguments, CountArguments);
end;

procedure TIcing.ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  inherited ConstructObject(ThisArgument, Arguments, CountArguments);
end;

constructor TIcing.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  inherited Create(AInstance, APrototype, AHasPrototypeProperty);
  FColor := 'White';
end;


constructor TCake.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  inherited Create(AInstance, APrototype, AHasPrototypeProperty);
  fName := 'Cake';
  Self.Weight := 1250;
end;

destructor TCake.Destroy();
begin
  Inherited Destroy();
end;

procedure TCake.ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  inherited ConstructObject(ThisArgument, Arguments, CountArguments);
end;

procedure TCake.MyTest(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  Form1.txtOutput.Lines.Add('Native: My Test');
end;

procedure TCake.Eat(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  Self.Weight := Self.Weight - 250;
  if Self.Weight < 0 then Self.Weight := 0;
end;

procedure TCake.Info(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  Form1.txtOutput.Lines.Add(Format('Native: Name: %s; Weight: %d g', [Self.Name, Self.Weight]));
end;

procedure TCake.IsALie(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  Self.Weight := 0;
  fName := '(Imaginary) Cake';
end;

constructor TCookie.Create(AInstance: TObject; APrototype: TBESENObject=nil; AHasPrototypeProperty: longbool=false);
begin
  inherited Create(AInstance, APrototype, AHasPrototypeProperty);

  fName := 'Cookie';
  Self.Weight := 200;
end;

procedure TCookie.ConstructObject(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  inherited ConstructObject(ThisArgument, Arguments, CountArguments);
end;

procedure TCookie.Get(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  Form1.txtOutput.Lines.Add('Native: You got a cookie');
end;

procedure TCookie.Info(const ThisArgument: TBESENValue; Arguments: PPBESENValues; CountArguments: integer);
begin
  Form1.txtOutput.Lines.Add(Format('Native: Name: %s; Weight: %d g', [Self.Name, Self.Weight]));
end;

end.


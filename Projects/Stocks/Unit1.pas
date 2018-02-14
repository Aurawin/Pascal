unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls;

type

  { TForm1 }
  TPriceMode=(pmPrice,pmQuantity,pmTotal);
  TValuationMode=(vmPrice,vmShares,vmValuation);

  TTicket=record
    Price : Double;
    Quantity: Double;
    Total: Double;
  end;
  TTicketModes=record
    Price : TPriceMode;
    Valuation: TValuationMode;
  end;

  TForm1 = class(TForm)
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    rgValuationGoals: TRadioGroup;
    txtPricePrice: TEdit;
    txtValuationPrice: TEdit;
    txtPriceQuantity: TEdit;
    txtValuationShares: TEdit;
    txtPriceTotal: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    PageControl1: TPageControl;
    rgPriceGoals: TRadioGroup;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    txtValuationTotal: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure rgPriceGoalsClick(Sender: TObject);
    procedure rgValuationGoalsClick(Sender: TObject);
    procedure txtPricePriceChange(Sender: TObject);
    procedure txtPriceQuantityChange(Sender: TObject);
    procedure txtPriceTotalChange(Sender: TObject);
    procedure txtValuationPriceChange(Sender: TObject);
    procedure txtValuationSharesChange(Sender: TObject);
    procedure txtValuationTotalChange(Sender: TObject);
  private
    { private declarations }
    Price:TTicket;
    Valuation:TTicket;
    Modes:TTicketModes;

    procedure UpdatePrice;
    procedure UpdateValuation;
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{ TForm1 }

procedure TForm1.txtPricePriceChange(Sender: TObject);
begin
  Price.Price:=StrToFloatDef(txtPricePrice.Text,0);
  UpdatePrice;
end;

procedure TForm1.txtPriceQuantityChange(Sender: TObject);
const
  NoBlock : boolean = true;
var
  sValue:String;
begin
  if NoBlock then begin
    NoBlock:=false;
    try
      sValue:=StringReplace(txtPriceQuantity.Text,',','',[rfReplaceAll]);
      Price.Quantity:=StrToFloatDef(sValue,0);
      txtPriceQuantity.Text:=FormatFloat('#,##0',Price.Quantity);
      UpdatePrice;
    finally
      NoBlock:=true;
    end;
  end;
end;

procedure TForm1.rgPriceGoalsClick(Sender: TObject);
begin
  Modes.Price:=TPriceMode(rgPriceGoals.ItemIndex);
  UpdatePrice;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Modes.Valuation:=vmValuation;
  Modes.Price:=pmTotal;
end;

procedure TForm1.rgValuationGoalsClick(Sender: TObject);
begin
  Modes.Valuation:=TValuationMode(rgValuationGoals.ItemIndex);
  UpdateValuation;
end;

procedure TForm1.txtPriceTotalChange(Sender: TObject);
begin
  Price.Total:=StrToFloatDef(txtPriceTotal.Text,0);
  UpdatePrice;
end;

procedure TForm1.txtValuationPriceChange(Sender: TObject);
begin
  Valuation.Price:=StrToFloatDef(txtValuationPrice.Text,0);
  UpdateValuation;
end;

procedure TForm1.txtValuationSharesChange(Sender: TObject);
const
  NoBlock : boolean = true;
var
  sValue:String;
begin
  if NoBlock then begin
    NoBlock:=false;
    try
      sValue:=txtValuationShares.Text;
      sValue:=StringReplace(txtValuationShares.Text,',','',[rfReplaceAll]);
      Valuation.Quantity:=StrtoInt64Def(sValue,0);
      txtValuationShares.Text:=FormatFloat('#,##0',Valuation.Quantity);
      UpdateValuation;
    finally
      NoBlock:=true;
    end;
  end;
end;

procedure TForm1.txtValuationTotalChange(Sender: TObject);
begin
  Valuation.Total:=StrToFloatDef(txtValuationTotal.Text,0);
  UpdateValuation;
end;

procedure TForm1.UpdateValuation;

  procedure PushPrice;
  begin
    If Valuation.Quantity<>0 then
      Valuation.Price:=Valuation.Total/Valuation.Quantity
    else
     Valuation.Price:=0;
    txtValuationPrice.Text:=FormatFloat('#,##0.0000',Valuation.Price);
  end;

  procedure PushQuantity;
  begin
    if Valuation.Price<>0 then
      Valuation.Quantity:=Valuation.Total/Valuation.Price
    else
      Valuation.Quantity:=0;
    txtValuationShares.Text:=FormatFloat('#,##0',Valuation.Quantity);
  end;

  procedure PushTotal;
  begin
    Valuation.Total:=Valuation.Price*Valuation.Quantity;
    txtValuationTotal.Text:=FormatFloat('$ #,##0',Valuation.Total);
  end;

begin
  Case Modes.Valuation of
    vmPrice     : PushPrice;
    vmShares    : PushQuantity;
    vmValuation : PushTotal;
  end;
end;

procedure TForm1.UpdatePrice;

  procedure PushPrice;
  begin
    If Price.Quantity<>0 then
      Price.Price:=Price.Total/Price.Quantity
    else
     Price.Price:=0;
    txtPricePrice.Text:=FormatFloat('$ #,##0.000',Price.Price);
  end;

  procedure PushQuantity;
  begin
    if Price.Price<>0 then
      Price.Quantity:=Trunc(Price.Total/Price.Price)
    else
      Price.Quantity:=0;
    txtPriceQuantity.Text:=FormatFloat('#,##0',Price.Quantity);
  end;

  procedure PushTotal;
  begin
    Price.Total:=Price.Price*Price.Quantity;
    txtPriceTotal.Text:=FormatFloat('$ #,##0',Price.Total);
  end;

begin
  Case Modes.Price of
    pmPrice  : PushPrice;
    pmQuantity : PushQuantity;
    pmTotal : PushTotal;
  end;
end;

initialization
  {$I Unit1.lrs}

end.


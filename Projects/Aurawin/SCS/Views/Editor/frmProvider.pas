unit frmProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ComCtrls, SynMemo, SynHighlighterHTML,SynEdit,SynEditTypes,


  Core.Keywords,
  Core.Timer,
  Core.Strings,

  Storage,
  Storage.Main,
  Storage.SrchProviders,


  SysUtils;
Const
  FMT_CAPTION='Provider %s : %s';

type

  { TProviderForm }

  TProviderForm = class(TForm)
    sbStatus: TStatusBar;
    synHTML: TSynHTMLSyn;
    txtBody: TSynMemo;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure txtBodyChange(Sender: TObject);
    procedure txtBodyStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { private declarations }
    FService      : String;
    FProviderP    : Storage.SrchProviders.Items.PItem;
    tiModified    : Core.Timer.Item;
    FLoading      : boolean;
  private
    procedure OnModified(Item:Core.Timer.PItem);
  public
    { public declarations }
    procedure Show(aService:String; aProvider:Storage.SrchProviders.Items.PItem); reIntroduce;
  end; 

var
  ProviderForm: TProviderForm;

implementation
uses DateUtils;

{ TProviderForm }

procedure TProviderForm.FormCreate(Sender: TObject);
begin
  FLoading:=True;
  Try
    FProviderP:=nil;
    tiModified.Expires:=0;
    tiModified.Event:=@OnModified;
    tiModified.Location:='frmProvider.TProviderForm.OnModified';
    tiModified.Priority:=tpIdle;
    Core.Timer.Background.RegisterEvent(tiModified,LoadNoUpdate);
  finally
    FLoading:=false;
  end;
end;

procedure TProviderForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if txtBody.Modified then begin
    tiModified.Expires:=0;
    OnModified(@tiModified);
  end;
end;

procedure TProviderForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
var
  iLcv:integer;
begin
  CloseAction:=caHide;
  FLoading:=True;
  Try
    Caption:='';
    for iLcv:=0 to sbStatus.Panels.Count-1 do
      sbStatus.Panels[iLcv].Text:='';
    txtBody.Clear;
    txtBody.Modified:=false;
    FProviderP:=nil;
    tiModified.Expires:=0;
  finally
    FLoading:=False;
  end;
end;

procedure TProviderForm.FormDestroy(Sender: TObject);
begin
  FProviderP:=nil;
  Core.Timer.Background.UnloadEvent(tiModified,UnloadNoExecute);
end;

procedure TProviderForm.txtBodyChange(Sender: TObject);
begin
  if FLoading then exit;
  tiModified.Expires:=IncSecond(Core.Timer.dtNow,TI_AUTOSAVE);
end;

procedure TProviderForm.txtBodyStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if scCaretX in Changes then
    sbStatus.Panels[0].Text:=Core.Strings.toString(txtBody.CaretX);
  if scCaretY in Changes then
    sbStatus.Panels[1].Text:=Core.Strings.toString(txtBody.CaretY);
end;

procedure TProviderForm.OnModified(Item:Core.Timer.PItem);
begin
  if (FProviderP<>nil) and (txtBody.Modified) then begin
    Storage.SrchProviders.Items.SetLandingPage(FService,txtBody.Lines.Text,FProviderP^);
    txtBody.Modified:=False;
    Storage.SrchProviders.Items.DB.Edit(Storage.Main.Task,FProviderP^);
  end;
  Item^.Expires:=0;
end;

procedure TProviderForm.Show(aService:String; aProvider:Storage.SrchProviders.Items.PItem);
begin
  FLoading:=True;
  Try
    FService:=aService;
    FProviderP:=aProvider;
    sbStatus.Panels[3].Text:=FProviderP^.Domain;
    Caption:=Format(FMT_CAPTION,[FProviderP^.Caption,FService]);
    txtBody.Lines.Text:=Storage.SrchProviders.Items.GetLandingPage(FService,FProviderP^);
    txtBody.Modified:=False;
  finally
    FLoading:=false;
  end;
  Inherited ShowModal;
end;

initialization
  {$I frmProvider.lrs}

end.


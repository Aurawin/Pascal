unit frmKeyword; 

{$mode objfpc}{$H+}

interface

uses
  Classes,
  FileUtil,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  Menus,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  SynEdit,
  SynMemo,
  SynHighlighterHTML,
  SynEditTypes,
  SynHighlighterJava,

  Core.Keywords,
  Core.Timer,
  Core.Strings,

  Storage,
  Storage.Main,

  Storage.Keywords,
  Core.Utils.ListView,

  Core.Utils.Forms,

  SysUtils;
Const
  FMT_CAPTION='{$i %s}';

type

  { TKeywordForm }

  TKeywordForm = class(TForm)
    FindDialog1: TFindDialog;
    lbStatus: TLabel;
    lbModified: TLabel;
    lbX: TLabel;
    lbY: TLabel;
    miWindow: TMenuItem;
    miSearchReplace: TMenuItem;
    miSearchSep1: TMenuItem;
    miSearchFindNext: TMenuItem;
    miSearchFind: TMenuItem;
    miSearch: TMenuItem;
    miViewJava: TMenuItem;
    miViewHTML: TMenuItem;
    miVIew: TMenuItem;
    miEditDelete1: TMenuItem;
    miHelp: TMenuItem;
    MainMenu: TMainMenu;
    miFile: TMenuItem;
    miFileExit: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    miEditSep2: TMenuItem;
    miEdit: TMenuItem;
    miEditUndo: TMenuItem;
    miEditRedo: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    miEditSep1: TMenuItem;
    miFileSep: TMenuItem;
    pnlText: TPanel;
    pnlX: TPanel;
    pnlModified: TPanel;
    pnlY: TPanel;
    pnlStatus: TPanel;
    ReplaceDialog1: TReplaceDialog;
    synHTML: TSynHTMLSyn;
    synJava: TSynJavaSyn;
    txtBody: TSynMemo;
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miEditDelete1Click(Sender: TObject);
    procedure miFileExitClick(Sender: TObject);
    procedure miEditCutClick(Sender: TObject);
    procedure miEditCopyClick(Sender: TObject);
    procedure miEditPasteClick(Sender: TObject);
    procedure miEditUndoClick(Sender: TObject);
    procedure miEditRedoClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miSearchFindClick(Sender: TObject);
    procedure miSearchFindNextClick(Sender: TObject);
    procedure miSearchReplaceClick(Sender: TObject);
    procedure miViewHTMLClick(Sender: TObject);
    procedure miViewJavaClick(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure txtBodyChange(Sender: TObject);
    procedure txtBodyStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    { private declarations }
    FKeywordP     : PKeyword;
    tiModified    : Core.Timer.Item;
    FLoading      : boolean;
    FInfoP        : PFormInfo;
  private
    procedure OnModified(Item:Core.Timer.PItem);
  public
    { public declarations }
    procedure Show(aKeyword:PKeyword); overload;
  end;

implementation
uses DateUtils,frmConsole;

{ TKeywordForm }

procedure TKeywordForm.FormCreate(Sender: TObject);
begin
  FLoading:=True;
  Try
    FKeywordP:=nil;
    tiModified.Expires:=0;
    tiModified.Event:=@OnModified;
    tiModified.Location:='frmKeyword.TKeywordForm.OnModified';
    tiModified.Priority:=tpIdle;
    Core.Timer.Background.RegisterEvent(tiModified,LoadNoUpdate);
  finally
    FLoading:=false;
  end;
end;

procedure TKeywordForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  tiModified.Expires:=Core.Timer.dtNow;
end;

procedure TKeywordForm.FormClose(Sender: TObject; var CloseAction: TCloseAction
  );
begin
  CloseAction:=caFree;
  Core.Utils.Forms.List.UnLoad(FInfoP^);
end;

procedure TKeywordForm.FindDialog1Find(Sender: TObject);
var
  Options:TSynSearchOptions;
begin
  Options:=[];
  if not (frDown in FindDialog1.Options) then
    Include(Options,ssoBackwards);
  if (frFindNext in FindDialog1.Options) then
    Include(Options,ssoFindContinue);
  if (frMatchCase in FindDialog1.Options) then
    Include(Options,ssoMatchCase);
  //if (frReplace in FindDialog1.Options) then
    //Include(Options,ssoReplace);
  if (frReplaceAll in FindDialog1.Options) then
    Include(Options,ssoReplaceAll);
  if (frWholeWord in FindDialog1.Options) then
    Include(Options,ssoWholeWord);
  if (frEntireScope in FindDialog1.Options) then
    Include(Options,ssoEntireScope);
  txtBody.SearchReplace(FindDialog1.FindText,'',Options);
end;

procedure TKeywordForm.FormDestroy(Sender: TObject);
begin
  Core.Timer.Background.UnloadEvent(tiModified,UnloadExecute);
  FKeywordP^.Editor:=nil;
  FKeywordP:=nil;
end;

procedure TKeywordForm.FormShow(Sender: TObject);
begin
  miEdit.Visible:=True;
end;

procedure TKeywordForm.miEditDelete1Click(Sender: TObject);
begin
  txtBody.ClearSelection;
end;

procedure TKeywordForm.miFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TKeywordForm.miEditCutClick(Sender: TObject);
begin
  txtBody.CutToClipboard;
end;

procedure TKeywordForm.miEditCopyClick(Sender: TObject);
begin
  txtBody.CopyToClipboard;
end;

procedure TKeywordForm.miEditPasteClick(Sender: TObject);
begin
  txtBody.PasteFromClipboard;
end;

procedure TKeywordForm.miEditUndoClick(Sender: TObject);
begin
  txtBody.Undo;
end;

procedure TKeywordForm.miEditRedoClick(Sender: TObject);
begin
  txtBody.Redo;
end;

procedure TKeywordForm.miFileSaveClick(Sender: TObject);
begin
  tiModified.Expires:=Core.Timer.dtNow;
end;

procedure TKeywordForm.miSearchFindClick(Sender: TObject);
begin
   FindDialog1.Execute;
end;

procedure TKeywordForm.miSearchFindNextClick(Sender: TObject);
begin
  FindDialog1.Options:=FindDialog1.Options+[frFindNext];
  FindDialog1.OnFind(Sender);
end;

procedure TKeywordForm.miSearchReplaceClick(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TKeywordForm.miViewHTMLClick(Sender: TObject);
begin
  txtBody.Highlighter:=synHTML;
  miViewHTML.Checked:=True;
end;

procedure TKeywordForm.miViewJavaClick(Sender: TObject);
begin
  txtBody.Highlighter:=synJava;
  miViewJava.Checked:=True;
end;

procedure TKeywordForm.ReplaceDialog1Find(Sender: TObject);
var
  Options:TSynSearchOptions;
begin
  Options:=[];
  if not (frDown in ReplaceDialog1.Options) then
    Include(Options,ssoBackwards);
  if (frFindNext in ReplaceDialog1.Options) then
    Include(Options,ssoFindContinue);
  if (frMatchCase in ReplaceDialog1.Options) then
    Include(Options,ssoMatchCase);
  if (frReplace in ReplaceDialog1.Options) then
    Include(Options,ssoReplace);
  if (frReplaceAll in ReplaceDialog1.Options) then
    Include(Options,ssoReplaceAll);
  if (frWholeWord in ReplaceDialog1.Options) then
    Include(Options,ssoWholeWord);
  if (frEntireScope in ReplaceDialog1.Options) then
    Include(Options,ssoEntireScope);
  txtBody.SearchReplace(ReplaceDialog1.FindText,ReplaceDialog1.ReplaceText,Options);
end;

procedure TKeywordForm.txtBodyChange(Sender: TObject);
begin
  if FLoading then exit;
  tiModified.Expires:=IncSecond(Core.Timer.dtNow,TI_AUTOSAVE);
end;

procedure TKeywordForm.txtBodyStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  if scCaretX in Changes then
    lbX.Caption:=Core.Strings.toString(txtBody.CaretX);
  if scCaretY in Changes then
    lbY.Caption:=Core.Strings.toString(txtBody.CaretY);
end;

procedure TKeywordForm.OnModified(Item:Core.Timer.PItem);
var
  liItem:TListItem;
begin
  if (FKeywordP<>nil) and (txtBody.Modified) then begin
    FKeywordP^.Value:=txtBody.Lines.Text;
    txtBody.Modified:=False;
    if Core.Utils.ListView.IndexOf(frmConsole.ConsoleForm.lvKeywords,FKeywordP,liItem)<>-1 then begin
      liItem.Caption:=FKeywordP^.Name;
      liItem.SubItems[0]:=FKeywordP^.Value;
    end;
    Storage.Keywords.Items.DB.Save(Storage.Main.Task,FKeywordP^);
  end;
  Item^.Expires:=0;
end;

procedure TKeywordForm.Show(aKeyword: PKeyword);
begin
  FLoading:=True;
  Try
    if (FKeywordP<>nil) and (FKeywordP<aKeyword) and (FKeywordP^.Editor<>nil) then
      FKeywordP^.Editor:=nil;

    FKeywordP:=aKeyword;
    FKeywordP^.Editor:=Self;
    Caption:=Format(FMT_CAPTION,[FKeywordP^.Name]);

    txtBody.Lines.Text:=FKeywordP^.Value;
    txtBody.Modified:=False;

    FInfoP:=Core.Utils.Forms.List.Load(Self,FKeywordP,MainMenu,miWindow);
  finally
    FLoading:=false;
  end;
  Show;
end;

initialization
  {$I frmKeyword.lrs}

end.


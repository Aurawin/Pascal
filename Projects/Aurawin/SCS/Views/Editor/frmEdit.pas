unit frmEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  LResources,
  Forms,
  Controls,
  Graphics,
  Dialogs,
  ComCtrls,
  Menus,
  ActnList,
  ExtCtrls,
  StdCtrls,
  SynMemo,
  SynEdit,
  SynEditTypes,
  SynHighlighterXML,
  SynHighlighterHTML,
  SynHighlighterJava,
  SynHighlighterPerl,
  synhighlighterunixshellscript,
  SynHighlighterCss,
  SynHighlighterSQL,
  SynHighlighterTeX,

  Storage,
  Storage.Types,
  Storage.Main,
  Storage.FAT,

  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Streams,

  uSCSEditor,

  Core.Utils.Forms,

  FileUtil,
  SysUtils;

type

  { TSCSEditorForm }

  TSCSEditorForm = class(TForm)
    actEditCut: TAction;
    actEditCopy: TAction;
    actEditPaste: TAction;
    actEditDelete: TAction;
    actFileSave: TAction;
    actFileExit: TAction;
    actEditSelecteAll: TAction;
    actSearchReplace: TAction;
    actSearchFindNext: TAction;
    actSearchFind: TAction;
    actViewXML: TAction;
    actViewText: TAction;
    actViewSQL: TAction;
    actViewShellScript: TAction;
    actViewPerl: TAction;
    actViewJava: TAction;
    actViewHTML: TAction;
    actViewCSS: TAction;
    alEditor: TActionList;
    FindDialog1: TFindDialog;
    ilBookMarks: TImageList;
    lbModified: TLabel;
    lbStatus: TLabel;
    lbX: TLabel;
    lbY: TLabel;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    miWindow: TMenuItem;
    mipuSep4: TMenuItem;
    mipuSelectAll: TMenuItem;
    mipuView: TMenuItem;
    mipuSep3: TMenuItem;
    mipuSep2: TMenuItem;
    mipuSave: TMenuItem;
    mipuDelete: TMenuItem;
    mipuSep1: TMenuItem;
    mipuPaste: TMenuItem;
    mipuCopy: TMenuItem;
    mipuCut: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    miViewText: TMenuItem;
    miViewXML: TMenuItem;
    miViewCSS: TMenuItem;
    miViewSQL: TMenuItem;
    miViewPerl: TMenuItem;
    miViewShellScript: TMenuItem;
    miSearch: TMenuItem;
    miSearchReplace: TMenuItem;
    miView: TMenuItem;
    miViewHTML: TMenuItem;
    miViewJava: TMenuItem;
    miSearchFind: TMenuItem;
    miSearchFindNext: TMenuItem;
    miSearchSep1: TMenuItem;
    miFileSave: TMenuItem;
    MenuItem2: TMenuItem;
    miFileExit: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    miSelectAll: TMenuItem;
    miSaveAs: TMenuItem;
    miImport: TMenuItem;
    miDelete: TMenuItem;
    miPaste: TMenuItem;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miEdit: TMenuItem;
    miFile: TMenuItem;
    pnlModified: TPanel;
    pnlStatus: TPanel;
    pnlText: TPanel;
    pnlX: TPanel;
    pnlY: TPanel;
    puBody: TPopupMenu;
    ReplaceDialog1: TReplaceDialog;
    synCSS: TSynCssSyn;
    synHTML: TSynHTMLSyn;
    synJava: TSynJavaSyn;
    synPerl: TSynPerlSyn;
    synShellScript: TSynUNIXShellScriptSyn;
    synSQL: TSynSQLSyn;
    SynTeX: TSynTeXSyn;
    txtBody: TSynMemo;
    synXML: TSynXMLSyn;
    procedure actEditCopyExecute(Sender: TObject);
    procedure actEditCutExecute(Sender: TObject);
    procedure actEditDeleteExecute(Sender: TObject);
    procedure actEditPasteExecute(Sender: TObject);
    procedure actEditSelecteAllExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actFileSaveExecute(Sender: TObject);
    procedure actSearchFindExecute(Sender: TObject);
    procedure actSearchFindNextExecute(Sender: TObject);
    procedure actSearchReplaceExecute(Sender: TObject);
    procedure actViewCSSExecute(Sender: TObject);
    procedure actViewHTMLExecute(Sender: TObject);
    procedure actViewJavaExecute(Sender: TObject);
    procedure actViewPerlExecute(Sender: TObject);
    procedure actViewShellScriptExecute(Sender: TObject);
    procedure actViewSQLExecute(Sender: TObject);
    procedure actViewTextExecute(Sender: TObject);
    procedure actViewXMLExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure miFileClick(Sender: TObject);
    procedure ReplaceDialog1Find(Sender: TObject);
    procedure txtBodyStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  private
    FFolder:TDSFolder;
    FFile:TDSFile;
    sData:String;
    FInfoP:PFormInfo;
    procedure Push_UpdateStatusbar;
  public
    procedure Clear;
    procedure Show(aFile:TDSFile); reIntroduce;
  end;

implementation

{ TSCSEditorForm }
const
   PI_Caret_X = 0;
   PI_Caret_Y = 1;
   PI_Modified = 2;

procedure TSCSEditorForm.txtBodyStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  Push_UpdateStatusbar;
end;

procedure TSCSEditorForm.Push_UpdateStatusbar;
begin
  lbX.Caption:=Format('%0.N',[Double(txtBody.CaretX)]);
  lbY.Caption:=Format('%0.N',[Double(txtBody.CaretY)]);
  lbModified.Caption:=FMT_TXT_MODIFIED[txtBody.Modified];
  actFileSave.Enabled:=txtBody.Modified;
end;

procedure TSCSEditorForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction:=caFree;
  Core.Utils.Forms.List.Unload(FInfoP^);
end;

procedure TSCSEditorForm.FormDestroy(Sender: TObject);
begin
  Clear;
end;

procedure TSCSEditorForm.FindDialog1Find(Sender: TObject);
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

procedure TSCSEditorForm.actViewCSSExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synCSS;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actSearchFindExecute(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TSCSEditorForm.actFileSaveExecute(Sender: TObject);
var
  FS:TFileStream;
begin
  if (FFile.ReadOnly = false) and txtBody.Modified then begin
    FS:=FFile.AcquireData();
    Try
      FS.Size:=0;
      txtBody.Lines.SaveToStream(FS);
      Core.Streams.CheckSum(FS,FFIle.Digest);
      FFile.Size:=FS.Size;
    finally
      FS.Free();
    end;
    FFile.Save(Storage.Main.Task,dsfileSaveAll);
    txtBody.Modified:=false;
    Push_UpdateStatusbar;
  end;
end;

procedure TSCSEditorForm.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TSCSEditorForm.actEditCutExecute(Sender: TObject);
begin
  txtBody.CutToClipboard;
end;

procedure TSCSEditorForm.actEditDeleteExecute(Sender: TObject);
begin
  txtBody.ClearSelection;
end;

procedure TSCSEditorForm.actEditPasteExecute(Sender: TObject);
begin
  txtBody.PasteFromClipboard;
end;

procedure TSCSEditorForm.actEditSelecteAllExecute(Sender: TObject);
begin
  txtBody.SelectAll;
end;

procedure TSCSEditorForm.actEditCopyExecute(Sender: TObject);
begin
  txtBody.CopyToClipboard;
end;

procedure TSCSEditorForm.actSearchFindNextExecute(Sender: TObject);
begin
  FindDialog1.Options:=FindDialog1.Options+[frFindNext];
  FindDialog1.OnFind(Sender);
end;

procedure TSCSEditorForm.actSearchReplaceExecute(Sender: TObject);
begin
  ReplaceDialog1.Execute;
end;

procedure TSCSEditorForm.actViewHTMLExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synHTML;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actViewJavaExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synJava;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actViewPerlExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synPerl;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actViewShellScriptExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synShellScript;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actViewSQLExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synSQL;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actViewTextExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synTeX;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.actViewXMLExecute(Sender: TObject);
begin
  txtBody.Highlighter:=synXML;
  txtBody.Highlighter.Enabled:=true;
end;

procedure TSCSEditorForm.miFileClick(Sender: TObject);
begin
  miFileSave.Enabled:=not FFile.ReadOnly;
end;

procedure TSCSEditorForm.ReplaceDialog1Find(Sender: TObject);
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

procedure TSCSEditorForm.Show(aFile:TDSFile);
var
  skKind : TSyntaxKind;
  FS     : TFileStream;
begin
  Inherited Show;
  FFolder:=aFile.Parent;
  FFile:=aFile;
  FFile.Load(Storage.Main.Task);
  Caption:=Format(FMT_EDITOR_CAPTION,[FFolder.Path,FFile.Name]);
  lbStatus.Caption:=Format(FMT_EDITOR_STATUS_PATH,[FFolder.Owner.FAT.Domain,FFolder.Path,FFile.Name,FMT_EDITOR_STATUS_READ_ONLY[FFile.ReadOnly]]);
  skKind:=uSCSEditor.getSyntax(FFile.Name);
  txtBody.Highlighter:=nil;
  FInfoP:=Core.Utils.Forms.List.Load(Self,FFile,MainMenu,miWindow);
  Case skKind of
    skText        : actViewText.Execute;
    skCSS         : actViewCSS.Execute;
    skHTML        : actViewHTML.Execute;
    skJava        : actViewJava.Execute;
    skPerl        : actViewPerl.Execute;
    skShellScript : actViewShellScript.Execute;
    skSQL         : actViewSQL.Execute;
    skXML         : actViewXML.Execute;
  end;

  txtBody.Lines.BeginUpdate;
  try
    FS:=FFile.AcquireData();
    Try
      FS.Position:=0;
      txtBody.Lines.Clear;
      txtBody.Lines.LoadFromStream(FS);
      txtBody.Modified:=false;
    finally
      FS.Free();
    end;
  finally
    txtBody.Lines.EndUpdate;
  end;
end;
procedure TSCSEditorForm.Clear;
begin
  txtBody.Clear;
  FFolder:=nil;
  FFile:=nil;
  lbX.Caption:='';
  lbY.Caption:='';
  lbModified.Caption:='';
  lbStatus.Caption:='';
  Push_UpdateStatusbar;
end;

initialization
  {$I frmEdit.lrs}

end.


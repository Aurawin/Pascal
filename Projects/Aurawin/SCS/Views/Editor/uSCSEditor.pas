unit uSCSEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Core.Arrays.KeyString;

Type
  TSyntaxKind=(skRaw,skText,skCSS,skHTML,skJava,skPerl,skShellScript,skSQL,skXML);
  TFileExt=String[5];
  TSyntaxExt=record
    Kind : TSyntaxKind;
    Ext  : TFileExt;
  end;
Const
  Extensions:Array[0..9] of TSyntaxExt =(
    (Kind: skRaw;         Ext:''),
    (Kind: skText;        Ext:'.txt'),
    (Kind: skCSS;         Ext:'.css'),
    (Kind: skHTML;        Ext:'.html'),
    (Kind: skHTML;        Ext:'.htm'),
    (Kind: skJava;        Ext:'.js'),
    (Kind: skPerl;        Ext:'.pl'),
    (Kind: skShellScript; Ext:'.sh'),
    (Kind: skSQL;         Ext:'.sql'),
    (Kind: skXML;         Ext:'.xml')
  );

  function getSyntax(FileName:String):TSyntaxKind;

implementation

function getSyntax(FileName:String):TSyntaxKind;
var
  iLcv:integer;
  sExt:String;
begin
  Result:=skRaw; sExt:=ExtractFileExt(FileName);
  for iLcv:=Low(Extensions) to High(Extensions) do begin
    If SameText(sExt,Extensions[iLcv].Ext) then begin
      Result:=Extensions[iLcv].Kind;
      Break;
    end;
  end;
end;

end.


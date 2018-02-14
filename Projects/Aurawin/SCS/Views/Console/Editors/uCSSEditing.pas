unit uCSSEditing;

interface

uses uItemScrollBox,hSRConsts,OCL,ccUtils,uStorage,LibXMLParser;

Type

  TCSSEditItem=Record
    DomainP           : PDomain;
    CSS               : TCSS;

    BGC               : TGroupItem;  // background-color
    BGI               : TGroupItem;  // background-image
    BGR               : TGroupItem;  // background-repeat
    BGA               : TGroupItem;  // background-attachment
    BGP               : TGroupItem;  // background-position
    BW                : TGroupItem;  // border-width
    BTW               : TGroupItem;  // border-top-width
    BRW               : TGroupItem;  // border-right-width
    BBW               : TGroupItem;  // border-bottom-width
    BLW               : TGroupItem;  // border-left-width
    BCOLOR            : TGroupItem;  // border-color
    BTC               : TGroupItem;  // border-top-color
    BRC               : TGroupItem;  // border-right-color
    BBC               : TGroupItem;  // border-bottom-color
    BLC               : TGroupItem;  // border-left-color
    BS                : TGroupItem;  // border-style
    BTS               : TGroupItem;  // border-top-style
    BRS               : TGroupItem;  // border-right-style
    BBS               : TGroupItem;  // border-bottom-style
    BLS               : TGroupItem;  // border-left-style
    BCP               : TGroupItem;  // border-collapse
    BOTTOM            : TGroupItem;  // bottom
    CAPS              : TGroupItem;  // caption-side
    CLEAR             : TGroupItem;  // clear 
    // CLIP              : TGroupItem;  // clip
    CLR               : TGroupItem;  // color
    CURS              : TGroupItem;  // cursor
    DIR               : TGroupItem;  // direction
    DIS               : TGroupItem;  // display
    EMPTY             : TGroupItem;  // empty
    FLOAT             : TGroupItem;  // float
    FSIZE             : TGroupItem;  // font-size
    FFAMILY           : TGroupItem;  // font-family
    FSTYLE            : TGroupItem;  // font-style
    FVARIANT          : TGroupItem;  // font-variant
    FWEIGHT           : TGroupItem;  // font-weight
    HEIGHT            : TGroupItem;  // height
    LEFT              : TGroupItem;  // left
    LETSPAC           : TGroupItem;  // letter-spacing
    LINEHGT           : TGroupItem;  // line-height
    LST               : TGroupItem;  // list-style-type
    LSP               : TGroupItem;  // list-style-position
    LSI               : TGroupItem;  // list-style-image
    MART              : TGroupItem;  // margin-top
    MARR              : TGroupItem;  // margin-right
    MARB              : TGroupItem;  // margin-bottom
    MARL              : TGroupItem;  // margin-left
    MAXH              : TGroupItem;  // max-height
    MAXW              : TGroupItem;  // max-width
    MINH              : TGroupItem;  // min-height
    MINW              : TGroupItem;  // min-width
    ORHPANS           : TGroupItem;  // orphans
    OVERFLOW          : TGroupItem;  // overflow
    PADDING           : TGroupItem;  // padding
    PADT              : TGroupItem;  // padding-top
    PADR              : TGroupItem;  // padding-right
    PADB              : TGroupItem;  // padding-bottom
    PADL              : TGroupItem;  // padding-left
    PBA               : TGroupItem;  // page-break-after
    PBB               : TGroupItem;  // page-break-before
    PBI               : TGroupItem;  // page-break-inside
    POS               : TGroupItem;  // position
    Right             : TGroupItem;  // right
    TLAY              : TGroupItem;  // table-layout
    TALI              : TGroupItem;  // text-align
    TDEC              : TGroupItem;  // text-decoration
    TIND              : TGroupItem;  // text-indent
    TTNS              : TGroupItem;  // text-transform
    TOP               : TGroupItem;  // top
    UBDI              : TGroupItem;  // unicode-bidi
    VALIGN            : TGroupItem;  // vertical-align
    VISIBILITY        : TGroupItem;  // visibility
    WhiteSpace        : TGroupItem;  // white-space
    Width             : TGroupItem;  // width
    Windows           : TGroupItem;  // windows 
    WordSpacing       : TGroupItem;  // word-spacing 
    ZIndex            : TGroupItem;  // z-index
  end;

  Function  Load_CSS(ISB:TItemScrollBox; Var Item:TCSSEditItem):Boolean;
  Function  Add_CSS(Var Item:TCSSEditItem):Boolean;
  Function  Delete_CSS(Var Item:TCSSEditItem):Boolean;
  Function  Save_CSS(ISB:TItemScrollBox; Var Item:TCSSEditItem):Boolean;
  Function  Exists_CSS(Var Item:TCSSEditItem):Boolean;

implementation
uses forms;
Function  Load_CSS(ISB:TItemScrollBox; Var Item:TCSSEditItem):Boolean;
var
  iPropLcv,iLcv:Integer;
  Prop:TProperty;
  GP:TGroup;

  procedure PushFontFamily;
  var
    iLcv:Integer;
  begin
    Prop.Style:=psComboSelect;
    Prop.ItemCount:=Screen.Fonts.Count;
    SetLength(Prop.Options,Prop.ItemCount);
    For iLcv:=0 to Screen.Fonts.Count-1 do begin
      Prop.Options[iLcv].sLabel:=Screen.Fonts[iLcv];
      Prop.Options[iLcv].sLabel:=Screen.Fonts[iLcv];
    end;
  end;

  procedure PushPositions;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=5;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='top';
      sValue:='top';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='left';
      sValue:='left';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='bottom';
      sValue:='bottom';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='right';
      sValue:='right';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='center';
      sValue:='center';
      iIndex:=-1;
    end;
  end;

  procedure PushTextDecoration;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=6;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='underline';
      sValue:='underline';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='overline';
      sValue:='overline';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='line-through';
      sValue:='line-through';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='blink';
      sValue:='blink';
      iIndex:=-1;
    end;
  end;

  procedure PushTextTransform;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=5;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='capitalize';
      sValue:='capitalize';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='uppercase';
      sValue:='uppercase';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='lowercase';
      sValue:='lowercase';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
  end;


  procedure PushPosition;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=5;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='static';
      sValue:='static';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='relative';
      sValue:='relative';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='absolute';
      sValue:='absolute';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='fixed';
      sValue:='fixed';
      iIndex:=-1;
    end;
  end;

  procedure PushFontSize;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=6;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='larger';
      sValue:='larger';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='medium';
      sValue:='medium';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='smaller';
      sValue:='smaller';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='small';
      sValue:='small';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='x-small';
      sValue:='x-small';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='xx-small';
      sValue:='xx-small';
      iIndex:=-1;
    end;    
  end;

  procedure PushCollapse;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='collapse';
      sValue:='collapse';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='separate';
      sValue:='separate';
      iIndex:=-1;
    end;
  end;

  procedure PushVisibility;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='hidden';
      sValue:='hidden';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='collapse';
      sValue:='collapse';
      iIndex:=-1;
    end;
  end;

  procedure PushCellVisibility;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='show';
      sValue:='show';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='hide';
      sValue:='hide';
      iIndex:=-1;
    end;
  end;

  procedure PushDirection;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='ltr';
      sValue:='ltr';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='rtl';
      sValue:='rtl';
      iIndex:=-1;
    end;
  end;

  procedure PushTableLayout;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='auto';
      sValue:='auto';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='fixed';
      sValue:='fixed';
      iIndex:=-1;
    end;
  end;

  procedure PushFontVariant;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='normal';
      sValue:='normal';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='small-caps';
      sValue:='small-caps';
      iIndex:=-1;
    end;
  end;

  procedure PushBorder;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='thin';
      sValue:='thin';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='medium';
      sValue:='medium';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='thick';
      sValue:='thick';
      iIndex:=-1;
    end;
  end;

  procedure PushRepeat;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=4;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='repeat';
      sValue:='repeat';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='repeat-x';
      sValue:='repeat-x';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='repeat-y';
      sValue:='repeat-y';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='no-repeat';
      sValue:='no-repeat';
      iIndex:=-1;
    end;
  end;

  procedure PushFloat;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=4;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='left';
      sValue:='left';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='right';
      sValue:='right';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
  end;

  procedure PushWhiteSpace;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=6;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='normal';
      sValue:='normal';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='pre';
      sValue:='pre';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='nowrap';
      sValue:='nowrap';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='pre-wrap';
      sValue:='pre-wrap';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='pre-line';
      sValue:='pre-line';
      iIndex:=-1;
    end;
  end;

  procedure PushPageBreak;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=6;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='auto';
      sValue:='auto';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='always';
      sValue:='always';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='avoid';
      sValue:='avoid';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='left';
      sValue:='left';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='right';
      sValue:='right';
      iIndex:=-1;
    end;
  end;

  procedure PushTextAlign;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=5;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='left';
      sValue:='left';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='right';
      sValue:='right';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='center';
      sValue:='center';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='justify';
      sValue:='justify';
      iIndex:=-1;
    end;
  end;

  procedure PushOverflow;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=5;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='visible';
      sValue:='visible';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='hidden';
      sValue:='hidden';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='scroll';
      sValue:='scroll';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='auto';
      sValue:='auto';
      iIndex:=-1;
    end;
  end;

  procedure PushClear;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=4;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='left';
      sValue:='left';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='right';
      sValue:='right';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='both';
      sValue:='both';
      iIndex:=-1;
    end;
  end;

  procedure PushUnicode;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=4;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='normal';
      sValue:='normal';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='embed';
      sValue:='embed';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='bidi-overrride';
      sValue:='bidi-overrride';
      iIndex:=-1;
    end;
  end;

  procedure PushFontStyle;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=4;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='normal';
      sValue:='normal';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='italic';
      sValue:='italic';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='oblique';
      sValue:='oblique';
      iIndex:=-1;
    end;
  end;

  procedure PushDisplay;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=16;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='inline';
      sValue:='inline';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='block';
      sValue:='block';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='inline-block';
      sValue:='inline-block';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='run-in';
      sValue:='run-in';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='list-item';
      sValue:='list-item';
      iIndex:=-1;
    end;
    With Prop.Options[6] do begin
      sLabel:='table';
      sValue:='table';
      iIndex:=-1;
    end;
    With Prop.Options[7] do begin
      sLabel:='inline-table';
      sValue:='inline-table';
      iIndex:=-1;
    end;
    With Prop.Options[8] do begin
      sLabel:='table-row-group';
      sValue:='table-row-group';
      iIndex:=-1;
    end;
    With Prop.Options[9] do begin
      sLabel:='table-header-group';
      sValue:='table-header-group';
      iIndex:=-1;
    end;
    With Prop.Options[10] do begin
      sLabel:='table-footer-group';
      sValue:='table-footer-group';
      iIndex:=-1;
    end;
    With Prop.Options[11] do begin
      sLabel:='table-row';
      sValue:='table-row';
      iIndex:=-1;
    end;
    With Prop.Options[12] do begin
      sLabel:='table-column-group';
      sValue:='table-column-group';
      iIndex:=-1;
    end;
    With Prop.Options[13] do begin
      sLabel:='table-column';
      sValue:='table-column';
      iIndex:=-1;
    end;
    With Prop.Options[14] do begin
      sLabel:='table-cell';
      sValue:='table-cell';
      iIndex:=-1;
    end;
    With Prop.Options[15] do begin
      sLabel:='table-caption';
      sValue:='table-caption';
      iIndex:=-1;
    end;
  end;

  procedure PushListStylePosition;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='inside';
      sValue:='inside';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='outside';
      sValue:='outside';
      iIndex:=-1;
    end;
  end;


  procedure PushListStyleType;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=14;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='disc';
      sValue:='disc';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='circle';
      sValue:='circle';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='square';
      sValue:='square';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='decimal';
      sValue:='decimal';
      iIndex:=-1;
    end;
    With Prop.Options[6] do begin
      sLabel:='decimal-leading-zero';
      sValue:='decimal-leading-zero';
      iIndex:=-1;
    end;
    With Prop.Options[7] do begin
      sLabel:='lower-roman';
      sValue:='lower-roman';
      iIndex:=-1;
    end;
    With Prop.Options[8] do begin
      sLabel:='upper-roman';
      sValue:='upper-roman';
      iIndex:=-1;
    end;
    With Prop.Options[9] do begin
      sLabel:='lower-greek';
      sValue:='lower-greek';
      iIndex:=-1;
    end;
    With Prop.Options[10] do begin
      sLabel:='lower-latin';
      sValue:='lower-latin';
      iIndex:=-1;
    end;
    With Prop.Options[11] do begin
      sLabel:='upper-latin';
      sValue:='upper-latin';
      iIndex:=-1;
    end;
    With Prop.Options[12] do begin
      sLabel:='armenian';
      sValue:='armenian';
      iIndex:=-1;
    end;
    With Prop.Options[13] do begin
      sLabel:='georgian';
      sValue:='georgian';
      iIndex:=-1;
    end;
  end;

  procedure PushBorderStyle;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=10;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='none';
      sValue:='none';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='dotted';
      sValue:='dotted';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='dashed';
      sValue:='dashed';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='solid';
      sValue:='solid';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='double';
      sValue:='double';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='groove';
      sValue:='groove';
      iIndex:=-1;
    end;
    With Prop.Options[6] do begin
      sLabel:='ridge';
      sValue:='ridge';
      iIndex:=-1;
    end;
    With Prop.Options[7] do begin
      sLabel:='inset';
      sValue:='inset';
      iIndex:=-1;
    end;
    With Prop.Options[8] do begin
      sLabel:='outset';
      sValue:='outset';
      iIndex:=-1;
    end;
    With Prop.Options[9] do begin
      sLabel:='hidden';
      sValue:='hidden';
      iIndex:=-1;
    end;
  end;

  procedure PushVertAlign;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=9;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='baseline';
      sValue:='baseline';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='sub';
      sValue:='sub';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='super';
      sValue:='super';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='top';
      sValue:='top';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='text-top';
      sValue:='text-top';
      iIndex:=-1;
    end;
    With Prop.Options[6] do begin
      sLabel:='middle';
      sValue:='middle';
      iIndex:=-1;
    end;
    With Prop.Options[7] do begin
      sLabel:='bottom';
      sValue:='bottom';
      iIndex:=-1;
    end;
    With Prop.Options[8] do begin
      sLabel:='text-bottom';
      sValue:='text-bottom';
      iIndex:=-1;
    end;
  end;
  
  procedure PushFontWeight;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=14;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='normal';
      sValue:='normal';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='bolder';
      sValue:='bolder';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='bold';
      sValue:='bold';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='lighter';
      sValue:='lighter';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='100';
      sValue:='100';
      iIndex:=-1;
    end;
    With Prop.Options[6] do begin
      sLabel:='200';
      sValue:='200';
      iIndex:=-1;
    end;
    With Prop.Options[7] do begin
      sLabel:='300';
      sValue:='300';
      iIndex:=-1;
    end;
    With Prop.Options[8] do begin
      sLabel:='400';
      sValue:='400';
      iIndex:=-1;
    end;
    With Prop.Options[9] do begin
      sLabel:='500';
      sValue:='500';
      iIndex:=-1;
    end;
    With Prop.Options[10] do begin
      sLabel:='600';
      sValue:='600';
      iIndex:=-1;
    end;
    With Prop.Options[11] do begin
      sLabel:='700';
      sValue:='700';
      iIndex:=-1;
    end;
    With Prop.Options[12] do begin
      sLabel:='800';
      sValue:='800';
      iIndex:=-1;
    end;
    With Prop.Options[13] do begin
      sLabel:='900';
      sValue:='900';
      iIndex:=-1;
    end;
  end;

  procedure PushCursor;
  begin
    Prop.Style:=psComboBox;
    Prop.ItemCount:=18;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='auto';
      sValue:='auto';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='crosshair';
      sValue:='crosshair';
      iIndex:=-1;
    end;
    With Prop.Options[3] do begin
      sLabel:='default';
      sValue:='default';
      iIndex:=-1;
    end;
    With Prop.Options[4] do begin
      sLabel:='help';
      sValue:='help';
      iIndex:=-1;
    end;
    With Prop.Options[5] do begin
      sLabel:='move';
      sValue:='move';
      iIndex:=-1;
    end;
    With Prop.Options[6] do begin
      sLabel:='n-resize';
      sValue:='n-resize';
      iIndex:=-1;
    end;
    With Prop.Options[7] do begin
      sLabel:='ne-resize';
      sValue:='ne-resize';
      iIndex:=-1;
    end;
    With Prop.Options[8] do begin
      sLabel:='e-resize';
      sValue:='e-resize';
      iIndex:=-1;
    end;
    With Prop.Options[9] do begin
      sLabel:='se-resize';
      sValue:='se-resize';
      iIndex:=-1;
    end;
    With Prop.Options[10] do begin
      sLabel:='s-resize';
      sValue:='s-resize';
      iIndex:=-1;
    end;
    With Prop.Options[11] do begin
      sLabel:='sw-resize';
      sValue:='sw-resize';
      iIndex:=-1;
    end;
    With Prop.Options[12] do begin
      sLabel:='w-resize';
      sValue:='w-resize';
      iIndex:=-1;
    end;
    With Prop.Options[13] do begin
      sLabel:='nw-resize';
      sValue:='nw-resize';
      iIndex:=-1;
    end;
    With Prop.Options[14] do begin
      sLabel:='text';
      sValue:='text';
      iIndex:=-1;
    end;
    With Prop.Options[15] do begin
      sLabel:='pointer';
      sValue:='pointer';
      iIndex:=-1;
    end;
    With Prop.Options[16] do begin
      sLabel:='progress';
      sValue:='progress';
      iIndex:=-1;
    end;
    With Prop.Options[17] do begin
      sLabel:='wait';
      sValue:='wait';
      iIndex:=-1;
    end;
  end;

begin
  ISB.Caption:='Loading...';
  ISB.Clear;
  Result:=uStorage.CSS_Get(Item.DomainP,Item.CSS);
  if Result then begin

    GP:=ISB.AddGroup(VarStringToString(Item.CSS.Name));
    Prop.MaskValue:=False;

    Prop.Style:=psString;
    Prop.Name:='background-color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BGC:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='background-image';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BGI:=GP.AddPropertyItem(-1,Prop);

    PushRepeat;
    Prop.Name:='background-repeat';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BGR:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psComboBox;
    Prop.ItemCount:=3;
    SetLength(Prop.Options,Prop.ItemCount);
    With Prop.Options[0] do begin
      sLabel:='scroll';
      sValue:='scroll';
      iIndex:=-1;
    end;
    With Prop.Options[1] do begin
      sLabel:='fixed';
      sValue:='fixed';
      iIndex:=-1;
    end;
    With Prop.Options[2] do begin
      sLabel:='inherit';
      sValue:='inherit';
      iIndex:=-1;
    end;
    Prop.Name:='background-attachment';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BGA:=GP.AddPropertyItem(-1,Prop);

    PushPositions;
    Prop.Name:='background-position';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BGP:=GP.AddPropertyItem(-1,Prop);

    PushBorder;
    Prop.Name:='border-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BW:=GP.AddPropertyItem(-1,Prop);

    PushBorder;
    Prop.Name:='border-top-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BTW:=GP.AddPropertyItem(-1,Prop);

    PushBorder;
    Prop.Name:='border-right-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BRW:=GP.AddPropertyItem(-1,Prop);

    PushBorder;
    Prop.Name:='border-bottom-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BBW:=GP.AddPropertyItem(-1,Prop);

    PushBorder;
    Prop.Name:='border-left-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BLW:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='border-color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BCOLOR:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='border-top-color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BTC:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='border-right-color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BRC:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='border-bottom-color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BBC:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='border-left-color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BLC:=GP.AddPropertyItem(-1,Prop);

    PushBorderStyle;
    Prop.Name:='border-style';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BS:=GP.AddPropertyItem(-1,Prop);
    PushBorderStyle;
    Prop.Name:='border-top-style';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BTS:=GP.AddPropertyItem(-1,Prop);
    PushBorderStyle;
    Prop.Name:='border-right-style';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BRS:=GP.AddPropertyItem(-1,Prop);
    PushBorderStyle;
    Prop.Name:='border-bottom-style';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BBS:=GP.AddPropertyItem(-1,Prop);
    PushBorderStyle;
    Prop.Name:='border-left-style';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BLS:=GP.AddPropertyItem(-1,Prop);

    PushCollapse;
    Prop.Name:='border-collapse';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BCP:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='bottom';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.BOTTOM:=GP.AddPropertyItem(-1,Prop);

    PushPositions;
    Prop.Name:='caption-side';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.CAPS:=GP.AddPropertyItem(-1,Prop);

    PushClear;
    Prop.Name:='clear';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.CLEAR:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='color';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.CLR:=GP.AddPropertyItem(-1,Prop);

    PushCursor;
    Prop.Name:='cursor';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.CURS:=GP.AddPropertyItem(-1,Prop);

    PushDirection;
    Prop.Name:='direction';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.DIR:=GP.AddPropertyItem(-1,Prop);

    PushDisplay;
    Prop.Name:='display';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.DIS:=GP.AddPropertyItem(-1,Prop);

    PushCellVisibility;
    Prop.Name:='empty-cells';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.EMPTY:=GP.AddPropertyItem(-1,Prop);

    PushFloat;
    Prop.Name:='float';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.FLOAT:=GP.AddPropertyItem(-1,Prop);

    PushFontFamily;
    Prop.Name:='font-family';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.FFAMILY:=GP.AddPropertyItem(-1,Prop);
    
    PushFontSize;
    Prop.Name:='font-size';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.FSIZE:=GP.AddPropertyItem(-1,Prop);

    PushFontStyle;
    Prop.Name:='font-style';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.FSTYLE:=GP.AddPropertyItem(-1,Prop);

    PushFontVariant;
    Prop.Name:='font-variant';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.FVARIANT:=GP.AddPropertyItem(-1,Prop);

    PushFontWeight;
    Prop.Name:='font-weight';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.FWEIGHT:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='height';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.HEIGHT:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='left';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.LEFT:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='letter-spacing';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.LETSPAC:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='line-height';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.LINEHGT:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='list-style-image';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.LSI:=GP.AddPropertyItem(-1,Prop);

    PushListStylePosition;
    Prop.Name:='list-style-position';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.LSP:=GP.AddPropertyItem(-1,Prop);

    PushListStyleType;
    Prop.Name:='list-style-type';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.LST:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='margin-top';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MART:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='margin-right';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MARR:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='margin-bottom';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MARB:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='margin-left';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MARL:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='max-height';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MAXH:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='max-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MAXW:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='min-height';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MINH:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='min-width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.MINW:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='orphans';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.ORHPANS:=GP.AddPropertyItem(-1,Prop);

    PushOverflow;
    Prop.Name:='overflow';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.OVERFLOW:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='padding';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PADDING:=GP.AddPropertyItem(-1,Prop);
    Prop.Style:=psString;
    Prop.Name:='padding-top';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PADT:=GP.AddPropertyItem(-1,Prop);
    Prop.Name:='padding-right';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PADR:=GP.AddPropertyItem(-1,Prop);
    Prop.Name:='padding-bottom';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PADB:=GP.AddPropertyItem(-1,Prop);
    Prop.Name:='padding-left';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PADL:=GP.AddPropertyItem(-1,Prop);
    PushPageBreak;
    Prop.Name:='page-break-before';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PBB:=GP.AddPropertyItem(-1,Prop);
    PushPageBreak;
    Prop.Name:='page-break-after';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PBA:=GP.AddPropertyItem(-1,Prop);
    PushPageBreak;
    Prop.Name:='page-break-inside';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.PBI:=GP.AddPropertyItem(-1,Prop);

    PushPosition;
    Prop.Name:='position';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.POS:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='right';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.Right:=GP.AddPropertyItem(-1,Prop);

    PushTableLayout;
    Prop.Name:='table-layout';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.TLAY:=GP.AddPropertyItem(-1,Prop);

    PushTextAlign;
    Prop.Name:='text-align';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.TALI:=GP.AddPropertyItem(-1,Prop);

    PushTextDecoration;
    Prop.Name:='text-decoration';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.TDEC:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='text-indent';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.TIND:=GP.AddPropertyItem(-1,Prop);

    PushTextTransform;
    Prop.Name:='text-transform';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.TTNS:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='top';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.TOP:=GP.AddPropertyItem(-1,Prop);

    PushUnicode;
    Prop.Name:='unicode-bidi';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.UBDI:=GP.AddPropertyItem(-1,Prop);

    PushVertAlign;
    Prop.Name:='vertical-align';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.VALIGN:=GP.AddPropertyItem(-1,Prop);

    PushVisibility;
    Prop.Name:='visibility';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.VISIBILITY:=GP.AddPropertyItem(-1,Prop);

    PushWhiteSpace;
    Prop.Name:='white-space';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.WhiteSpace:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='windows';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.Windows:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='width';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.Width:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='word-spacing';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.WordSpacing:=GP.AddPropertyItem(-1,Prop);

    Prop.Style:=psString;
    Prop.Name:='z-index';
    Prop.Value:=ccUtils.GetItemByKey(Prop.Name,Item.CSS.Properties);
    Item.ZIndex:=GP.AddPropertyItem(-1,Prop);
  end;
end;

Function  Add_CSS(Var Item:TCSSEditItem):Boolean;
begin
  SetLength(Item.CSS.Properties,0);
  Result:=uStorage.CSS_Add(Item.DomainP,Item.CSS);
end;

Function  Delete_CSS(Var Item:TCSSEditItem):Boolean;
begin
  Result:=uStorage.CSS_Delete(Item.DomainP,Item.CSS);
end;

Function  Save_CSS(ISB:TItemScrollBox; Var Item:TCSSEditItem):Boolean;
begin
  With Item do begin
    ccUtils.UpdateKeyPair(BGC.Caption,BGC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BGI.Caption,BGI.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BGR.Caption,BGR.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BGA.Caption,BGA.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BGP.Caption,BGP.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BW.Caption,BW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BTW.Caption,BTW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BRW.Caption,BRW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BBW.Caption,BBW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BLW.Caption,BLW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BCOLOR.Caption,BCOLOR.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BTC.Caption,BTC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BRC.Caption,BRC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BBC.Caption,BBC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BLC.Caption,BLC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BS.Caption,BS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BTS.Caption,BTS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BRS.Caption,BRS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BBS.Caption,BBS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BLS.Caption,BLS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BCP.Caption,BCP.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(BOTTOM.Caption,BOTTOM.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(CAPS.Caption,CAPS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(CLEAR.Caption,CLEAR.Properties.Value,CSS.Properties);
    //ccUtils.UpdateKeyPair(CLIP.Caption,CLIP.PropertyString,CSS.Properties);
    ccUtils.UpdateKeyPair(CLR.Caption,CLR.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(CURS.Caption,CURS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(DIR.Caption,DIR.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(DIS.Caption,DIS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(EMPTY.Caption,EMPTY.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(FLOAT.Caption,FLOAT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(FSIZE.Caption,FSIZE.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(FFAMILY.Caption,FFAMILY.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(FSTYLE.Caption,FSTYLE.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(FVARIANT.Caption,FVARIANT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(FWEIGHT.Caption,FWEIGHT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(HEIGHT.Caption,HEIGHT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(LEFT.Caption,LEFT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(LETSPAC.Caption,LETSPAC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(LINEHGT.Caption,LINEHGT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(LST.Caption,LST.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(LSP.Caption,LSP.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(LSI.Caption,LSI.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MART.Caption,MART.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MARR.Caption,MARR.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MARB.Caption,MARB.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MARL.Caption,MARL.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MAXH.Caption,MAXH.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MAXW.Caption,MAXW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MINH.Caption,MINH.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(MINW.Caption,MINW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(ORHPANS.Caption,ORHPANS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(OVERFLOW.Caption,OVERFLOW.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PADDING.Caption,PADDING.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PADT.Caption,PADT.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PADR.Caption,PADR.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PADB.Caption,PADB.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PADL.Caption,PADL.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PBA.Caption,PBA.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PBB.Caption,PBB.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(PBI.Caption,PBI.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(POS.Caption,POS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(Right.Caption,Right.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(TLAY.Caption,TLAY.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(TALI.Caption,TALI.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(TDEC.Caption,TDEC.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(TIND.Caption,TIND.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(TTNS.Caption,TTNS.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(TOP.Caption,TOP.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(UBDI.Caption,UBDI.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(VALIGN.Caption,VALIGN.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(VISIBILITY.Caption,VISIBILITY.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(WhiteSpace.Caption,WhiteSpace.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(Width.Caption,Width.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(Windows.Caption,Windows.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(WordSpacing.Caption,WordSpacing.Properties.Value,CSS.Properties);
    ccUtils.UpdateKeyPair(ZIndex.Caption,ZIndex.Properties.Value,CSS.Properties);
  end;
  Result:=uStorage.CSS_Set(Item.DomainP,Item.CSS);
end;

Function  Exists_CSS(Var Item:TCSSEditItem):Boolean;
begin
  Result:=uStorage.CSS_Exists(Item.DomainP,VarStringToString(Item.CSS.Name));
end;

end.


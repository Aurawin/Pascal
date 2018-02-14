unit Core.Utils.Forms;

interface

uses
  Classes, Forms,

  Core.Generics,

  SysUtils,
  Menus;

Type
  PFormInfo=^TFormInfo;
  TFormMenuItem=class(TMenuItem)
  private
    FInfoP:PFormInfo;
  end;

  TFormInfo=record
    Form   : TForm;
    Menu   : TMainMenu;
    Window : TMenuItem;
    Item   : TMenuItem;
    Data   : Pointer;
  end;


  GFormInfoList=specialize GObjectList<PFormInfo>;

  TFormInfoList=class(GFormInfoList)
  private
    procedure miFormClick(Sender: TObject);
  public
    procedure Unload(var Info:TFormInfo);
    function  Load(Item:TForm; Data:Pointer; Menu:TMainMenu; miWindow:TMenuItem):PFormInfo;
    function  Find(miWindow:TMenuItem; Data:pointer; var miForm:TFormMenuItem):boolean; overload;
    function  Show(Data:Pointer):Boolean;
    procedure Shutdown;
  end;

  Function List:TFormInfoList;

implementation


Var
  FManifest:TFormInfoList;

function List:TFormInfoList;
begin
  if FManifest=nil then FManifest:=TFormInfoList.Create;
  Result:=FManifest;
end;

procedure TFormInfoList.Shutdown;
var
  ItemP:PFormInfo;
  iLcv:LongInt;
begin
  for iLcv:=0 to Count-1 do begin
    ItemP:=Items[iLcv];
    Items[iLcv]:=nil;
    Dispose(ItemP);
  end;
  Clear;
  Halt(1);
end;

function TFormInfoList.Find(miWindow:TMenuItem; Data:pointer; var miForm:TFormMenuItem):boolean;
var
  iLcv:LongInt;
begin
  miForm:=nil;
  for iLcv:=0 to miWindow.Count-1 do begin
    if (miWindow.Items[iLcv] is TFormMenuItem) and ((TFormMenuItem(miWindow.Items[iLcv]).FInfoP=Data) or (TFormMenuItem(miWindow.Items[iLcv]).FInfoP^.Data=Data)) then begin
      miForm:=TFormMenuItem(miWindow.Items[iLcv]);
      break;
    end;
  end;
  Result:=miForm<>nil;
end;


procedure TFormInfoList.miFormClick(Sender: TObject);
var
  frmData:TForm;
  infoP:PFormInfo;
begin
  if Sender is TFormMenuItem then begin
    infoP:=TFormMenuItem(Sender).FInfoP;
    if IndexOf(infoP)<>-1 then begin
      infoP^.Form.Show;
      infoP^.Form.WindowState:=wsNormal;
      infoP^.Form.BringToFront;
    end else begin
      TFormMenuItem(Sender).Parent.Remove(TFormMenuItem(Sender));
      Sender.Free;
    end;
  end;
end;

function TFormInfoList.Show(Data:Pointer):Boolean;
var
  iLcv:LongInt;
  InfoP:PFormInfo;
begin
  Result:=False;
  for iLcv:=0 to Count-1 do begin
    InfoP:=Items[iLcv];
    if InfoP^.Data=Data then begin
      InfoP^.Form.Show;
      InfoP^.Form.WindowState:=wsNormal;
      InfoP^.Form.BringToFront;
      Result:=True;
    end;
  end;
end;

procedure TFormInfoList.Unload(var Info:TFormInfo);
var
  InfoP:PFormInfo;
  iLcv:LongInt;
  infLcvP:PFormInfo;
  miForm:TFormMenuItem;
begin
  InfoP:=@Info;
  if InfoP<>nil then begin
    if (Application.Terminated=false) then begin
      // Remove menu item from each form's window menu..
      for iLcv:=0 to Count-1 do begin
        infLcvP:=Items[iLcv];
        if (Application.Terminated=false) then begin
          if (infLcvP^.Window<>nil) and Find(infLcvP^.Window,InfoP,miForm) then begin
            miForm.Parent.Remove(miForm);
            miForm.FInfoP:=nil;
            miForm.Free;
          end;
        end;
      end;
    end;
    Delete(InfoP);
    Dispose(InfoP);
  end;
end;


function TFormInfoList.Load(Item:TForm; Data:Pointer; Menu:TMainMenu; miWindow:TMenuItem):PFormInfo;
var
  InfoP:PFormInfo;
  iLcvP:PFormInfo;
  iLcv:LongInt;
  miForm:TFormMenuItem;
begin
  Result:=nil;
  if Data=nil then raise Exception.Create('Cannot have nil Data pointer');
  New(InfoP);
  Result:=InfoP;
  InfoP^.Data:=Data;
  InfoP^.Form:=Item;
  InfoP^.Menu:=Menu;
  InfoP^.Window:=miWindow;
  if miWindow<>nil then begin
    // add item to all previous forms to menu
    for iLcv:=0 to Count-1 do begin
      iLcvP:=Items[iLcv];
      miForm:=TFormMenuItem.Create(miWindow);
      miForm.Caption:=iLcvP^.Form.Caption;
      miForm.OnClick:=@miFormClick;
      miForm.FInfoP:=iLcvP;
      miWindow.Add(miForm);
    end;
  end;
  Add(InfoP);
  // add this item to every form's window menu
  for iLcv:=0 to Count-1 do begin
    iLcvP:=Items[iLcv];
    if iLcvP^.Window<>nil then begin
      miForm:=TFormMenuItem.Create(iLcvP^.Window);
      miForm.Caption:=Item.Caption;
      miForm.OnClick:=@miFormClick;
      miForm.FInfoP:=InfoP;
      iLcvP^.Window.Add(miForm);
    end;
  end;
  Item.Show;
end;

end.


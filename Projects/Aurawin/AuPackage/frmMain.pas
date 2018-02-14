unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls,uAppSettings, types;

type

  { TPackageForm }

  TPackageForm = class(TForm)
    btnNext: TButton;
    btnCancel: TButton;
    imgLicense: TImage;
    lblCloseFile: TLabel;
    lblCanceled: TLabel;
    lblDescription: TLabel;
    lblPlease: TLabel;
    lblSource: TLabel;
    lblDest: TLabel;
    lblTitle: TLabel;
    lvFiles: TListView;
    pcPackage: TPageControl;
    pnlCopySource: TPanel;
    pnlCopyDest: TPanel;
    pnlHeader: TPanel;
    pnlImage: TPanel;
    pnlRight: TPanel;
    pnlSource: TPanel;
    pnlButtons: TPanel;
    pbIndex: TProgressBar;
    pbTotal: TProgressBar;
    pnlSource1: TPanel;
    lblDone: TLabel;
    tsCanceled: TTabSheet;
    tsPleaseClose: TTabSheet;
    tsFinished: TTabSheet;
    tsFiles: TTabSheet;
    tsWelcome: TTabSheet;
    tsCopyFiles: TTabSheet;
    txtWelcome: TMemo;
    procedure btnNextClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure pushWelcome();
    procedure pushCopy();
    procedure pushLicense();
    procedure pushFinished();
    procedure pushCanceled();
  public
    procedure SetStep(Value:integer);
    procedure Init();
  end;

var
  PackageForm: TPackageForm;

implementation
uses frmLicense,DateUtils;

{$R *.lfm}

{ TPackageForm }

procedure TPackageForm.Init();
var
  iLcv:integer;
  rcL:Package.Resources.PResources;
  rcP:Package.Resources.PResource;
  li:TListItem;
begin
  rcL:=Package.Resources.ResourcesP;
  for iLcv:=0 to High(rcL^) do begin
    rcP:=rcL^[iLcv];
    li:=lvFiles.Items.Add();
    li.Caption:=rcP^.Name;
    li.SubItems.Add(rcP^.Target);
    li.SubItems.Add(IntToStr(rcP^.Size));
  end;
end;

procedure TPackageForm.pushWelcome();
begin
  pcPackage.ActivePage:=tsWelcome;
  Logic.Step:=Logic.Welcome;
end;

procedure TPackageForm.pushCopy();
const
  MAX_BUFFER=512;
var
  Buffer:Array[0..MAX_BUFFER] of byte;
  iLcv:integer;
  lnsP:Package.Resources.Links.PLinks;
  lnP:Package.Resources.Links.PLink;
  rcsP:Package.Resources.PResources;
  rcP:Package.Resources.PResource;
  sSource:String;
  sDest:string;
  sPath:string;
  iRead:integer;
  dtExpires:TDateTime;
  FS:TFileStream;
  Opened:Boolean;
  CloseNeeded:Boolean;
  iPosition:integer;
begin
  btnNext.Enabled:=false;
  pcPackage.ActivePage:=tsCopyFiles;
  Logic.Step:=Logic.Copy;
  rcsP:=Package.Resources.ResourcesP;
  lnsP:=Package.Resources.LinksP;
  pbTotal.Position:=0;
  pbTotal.Max:=Length(rcsP^);
  pbIndex.Position:=0;
  pbIndex.Max:=100;
  for iLcv:=0 to High(rcsP^) do begin
    if (Application.Terminated=true) then
      exit;
    pbIndex.Position:=0;
    pbTotal.Position:=iLcv+1;
    rcP:=rcsP^[iLcv];
    if (System.Length(rcP^.Target)>0) then begin
      sSource:=rcP^.Name;
      sDest:=Package.Resources.prepFolder(rcP^);
      sPath:=SysUtils.ExtractFilePath(sDest);
      lblSource.Caption:=sSource;
      lblDest.Caption:=sDest;
      SysUtils.ForceDirectories(sPath);
      CloseNeeded:=false;
      Repeat
        Opened:=false;
        Try
          FS:=TFileStream.Create(sDest,fmCreate or fmShareDenyNone);
          Opened:=True;
          if (CloseNeeded=true) then begin
            CloseNeeded:=False;
            lblPlease.Caption:=Lang.Copying.ThanksForClosing;
            lblCloseFile.Caption:=sDest;
            dtExpires:=IncMillisecond(Now,1200);
            repeat
              Application.ProcessMessages();
            until ((Now>dtExpires) or (Application.Terminated=true));
            pcPackage.ActivePage:=tsCopyFiles;
            Application.ProcessMessages();
          end;
        Except
          On E:Exception do begin
            CloseNeeded:=True;
            lblPlease.Caption:=Lang.Copying.PleaseClose;
            lblCloseFile.Caption:=sDest;
            pcPackage.ActivePage:=tsPleaseClose;
            dtExpires:=IncMillisecond(Now,1200);
            repeat
              Application.ProcessMessages();
            until ((Now>dtExpires) or (Application.Terminated=true));
          end;
        end;
        If (Opened=true) then begin
          try
            rcP^.Stream.Position:=0;
            repeat
              iRead:=MAX_BUFFER;
              if ( (iRead+rcP^.Stream.Position)>rcP^.Stream.Size) then
                iRead:=rcP^.Stream.Size-rcP^.Stream.Position;
              iRead:=rcP^.Stream.Read(Buffer[0],iRead);
              if iRead>0 then
                FS.Write(Buffer[0],iRead);
              iPosition:=Trunc(rcP^.Stream.Position / rcP^.Stream.Size*100);
              if (iPosition<>pbIndex.Position) then begin
                pbIndex.Position:=iPosition;
                dtExpires:=IncMillisecond(Now,20);
                repeat
                  Application.ProcessMessages();
                until ((Now>dtExpires) or (Application.Terminated=true));
              end;
            until ((rcP^.Stream.Position>=rcP^.Stream.Size) or (Application.Terminated=true));
            rcP^.Stream.Position:=0;
            rcP^.Installed:=sDest;
          finally
            FS.Free();
          end;
          pbIndex.Position:=100;
          dtExpires:=IncMillisecond(Now,200);
          repeat
            Application.ProcessMessages();
          until ((Now>dtExpires) or (Application.Terminated=true));
        end;
      Until ((Opened=true) or (Application.Terminated=true));
    end;
  end;
  pbIndex.Position:=0;
  pbIndex.Max:=Length(lnsP^);

  for iLcv:=0 to High(lnsP^) do begin
    pbIndex.Position:=iLcv+1;
    lnP:=lnsP^[iLcv];
    rcP:=lnP^.ResourceP;

    if (rcP<>nil) then begin
      sSource:=Concat('Creating shortcut to ',rcP^.Name);
      sDest:=rcP^.Installed;

      lblSource.Caption:=sSource;
      lblDest.Caption:=sDest;
      Package.Resources.Links.CreateLink(lnP^);

      dtExpires:=IncMillisecond(Now,200);
      repeat
        Application.ProcessMessages();
      until ((Now>dtExpires) or (Application.Terminated=true));
    end;
  end;


  SetStep(Logic.Finished);
end;

procedure TPackageForm.pushLicense();
begin
  pcPackage.ActivePage:=tsWelcome;
  Logic.Step:=Logic.License;
  if LicenseForm.ShowModal=mrOK then begin
    SetStep(Logic.Copy);
  end else begin
    SetStep(Logic.Canceled);
  end;
end;

procedure TPackageForm.pushFinished();
var
  dtExpires:TDateTime;
begin
  pcPackage.ActivePage:=tsFinished;
  Logic.Step:=Logic.Finished;
  dtExpires:=IncMillisecond(Now,2500);
  repeat
      Application.ProcessMessages;
  until ((Now>dtExpires) or (Application.Terminated=true));
  Application.Terminate();
  Halt();
end;

procedure TPackageForm.pushCanceled();
var
  dtExpires:TDateTime;
begin
  btnNext.Enabled:=false;
  if Logic.Step=Logic.Finished then begin
    Application.Terminate();
    Halt();
    Exit();
  end;
  Logic.Step:=Logic.Canceled;
  pcPackage.ActivePage:=tsCanceled;
  dtExpires:=IncMillisecond(Now,2500);
  repeat
      Application.ProcessMessages;
  until ((Now>dtExpires) or (Application.Terminated=true));
  Application.Terminate();
  Halt();
end;

procedure TPackageForm.SetStep(Value:integer);
begin
  case Value of
    Logic.Welcome  : pushWelcome();
    Logic.Copy     : pushCopy();
    Logic.License  : pushLicense();
    Logic.Finished : pushFinished();
    Logic.Canceled : pushCanceled();
  end;
end;

procedure TPackageForm.FormCreate(Sender: TObject);
var
  rcsP:Package.Resources.PResources;
  rcP:Package.Resources.PResource;
begin
  rcsP:=Package.Resources.ResourcesP;
  rcP:=Package.Resources.Get(rcsP^,Package.Resources.Readme);
  if rcP<>nil then begin
    rcP^.Stream.Position:=0;
    txtWelcome.Lines.LoadFromStream(rcP^.Stream);
    rcP^.Stream.Position:=0;
  end;
  rcP:=Package.Resources.Get(rcsP^,Package.Resources.Image);
  if rcP<>nil then begin
    rcP^.Stream.Position:=0;
    imgLicense.Picture.LoadFromStream(rcP^.Stream);
    rcP^.Stream.Position:=0;
  end;
  pnlButtons.BorderSpacing.Around:=GUI.BUTTON_BOX_A;
  pnlButtons.ChildSizing.LeftRightSpacing:=GUI.BUTTON_BOX_LRS;
  pnlButtons.ChildSizing.TopBottomSpacing:=GUI.BUTTON_BOX_TBS;
  txtWelcome.BorderSpacing.Top:=GUI.TEXT_TOP;
  txtWelcome.BorderSpacing.Left:=GUI.TEXT_LEFT;
  txtWelcome.BorderSpacing.Right:=GUI.TEXT_RIGHT;
  txtWelcome.BorderSpacing.Bottom:=GUI.TEXT_BOTTOM;
  pcPackage.BorderSpacing.Around:=GUI.PAGES_A;
  pnlHeader.BorderSpacing.Around:=GUI.PANEL_A;
  lblDone.Caption:=Lang.Finished.Complete;
  lblCanceled.Caption:=Lang.Canceled.Stopped;
  lblTitle.Caption:=Lang.Welcome.Title;
  lblDescription.Caption:=Lang.Welcome.Description;
  pnlCopySource.Caption:=Lang.Copying.Source;
  pnlCopyDest.Caption:=Lang.Copying.Destination;
  Init();
  SetStep(uAppSettings.Logic.Welcome);
end;

procedure TPackageForm.btnCancelClick(Sender: TObject);
begin
  SetStep(uAppSettings.Logic.Canceled);
end;

procedure TPackageForm.btnNextClick(Sender: TObject);
begin
  SetStep(uAppSettings.Logic.Step+1);
end;

end.


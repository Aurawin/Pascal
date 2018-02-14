program RSRBuilder;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TRSRBuild }

  TRSRBuild = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TRSRBuild }
var
  Application: TRSRBuild;

  {$i RSRBuild.inc}

{$IFDEF WINDOWS}{$R RSRBuilder.rc}{$ENDIF}

begin
  Application:=TRSRBuild.Create(nil);
  Application.Run;
  Application.Free;
end.


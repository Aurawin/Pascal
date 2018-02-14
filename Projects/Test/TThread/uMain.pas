unit uMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls;

type
  TForm1=Class;
  { TForm1 }
  TMyThread=Class(TThread)
  private
    FIndex                       : LongInt;
    FOwner                       : TForm1;
  protected
    procedure Execute; override;

  public
    constructor Create(aOwner:TForm1;aIndex:Integer); reIntroduce;
    destructor  Destroy; override;
  end;

  TForm1 = class(TForm)
    sbStatus: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FSleepP                      : PRTLEvent;
    FMyList                      : Array of TMyThread;
  private
    Function AllComplete:Boolean;
  public
    { public declarations }

  end;

Const
  MAX_THREADS                    : cardinal = 250;
  MY_STACK_SIZE                  : cardinal = 1024*128;
  TIME_SLICE_YIELD_MS            : cardinal = 50;
var
  Form1: TForm1; 

implementation

{$R *.lfm}



Constructor TMyThread.Create(aOwner:TForm1; aIndex:Integer);
begin
  FOwner:=aOwner;
  FIndex:=aIndex;
  FreeOnTerminate:=True;
  Inherited Create(false,MY_STACK_SIZE);
end;

Destructor  TMyThread.Destroy;
begin
  if FIndex>-1 then begin
    if FIndex<System.Length(FOwner.FMyList) then
      FOwner.FMyList[FIndex]:=nil;
    FIndex:=-1;
  end;
  FOwner:=nil;
  Inherited Destroy;
end;

procedure TMyThread.Execute;
begin
  While Not (Terminated) and (FOwner<>nil) do begin
    Sleep(TIME_SLICE_YIELD_MS);
    {
     if (FOwner<>nil) and (FOwner.FSleepP<>nil) then
       RTLeventWaitFor(FOwner.FSleepP,TIME_SLICE_YIELD_MS);
    }
  end;
end;

Function TForm1.AllComplete:Boolean;
var
  iLcv:integer;
  bExistingFound:Boolean;
begin
  Result:=False; bExistingFound:=False;
  For iLcv:=0 to High(FMyList)do begin
    bExistingFound:=FMyList[iLcv]<>nil;
    if bExistingFound then Break;
  end;
  Result:=(bExistingFound=False);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  iLcv:integer;
begin
  FSleepP:=RTLEventCreate;
  SetLength(FMyList,MAX_THREADS);
  for iLcv:=0 to MAX_THREADS-1 do begin
    Try
      FMyList[iLcv]:=TMythread.Create(Self,iLcv);
    except
      on E:Exception do begin
        SetLength(FMyList,iLcv);
        Break;
      end;
    end;
  end;
  sbStatus.SimpleText:=Format('System started with actual %.D of %.D specified thread(s).',[Length(FMyList),MAX_THREADS]);
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  EventP:PRTLEvent;
  iLcv:integer;
begin
  For iLcv:=0 to High(FMyList) do
   if (FMyList[iLcv]<>nil) then FMyList[iLcv].Terminate;
  While not AllComplete do
    RTLeventWaitFor(FSleepP,TIME_SLICE_YIELD_MS);
  EventP:=FSleepP;
  InterlockedExchange(FSleepP,nil);
  SetLength(FMyList,0);
  RTLEventDestroy(EventP);

end;

end.


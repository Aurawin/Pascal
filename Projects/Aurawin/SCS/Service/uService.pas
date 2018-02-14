Unit uService;

{$mode objfpc}{$H+}

Interface

Uses
  {$IFDEF UNIX}
  CThreads,
  {$ENDIF}
  Interfaces, Classes, SysUtils, FileUtil, LResources, DaemonApp;

Type
  
  { TdmAurawin }

  TdmAurawin = Class(TDaemonMapper)
  Private
    { private declarations }
  Public
    { public declarations }
  End; 

Var
  dmAurawin: TdmAurawin; 

Implementation

Procedure RegisterMapper; 
Begin
  RegisterDaemonMapper(TdmAurawin);
End;

Initialization
  {$I uService.lrs}


  RegisterMapper; 
End.


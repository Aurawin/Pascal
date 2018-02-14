unit Storage;

interface

uses
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Boolean,
  Core.Arrays.Bytes,
  Core.Arrays.LargeWord,
  Core.Arrays.Pointers,

  Core.Database,
  Core.Database.Types,
  Core.Database.Monitor,

  Core.Utils.Files,
  Core.Utils.Time,

  Core.Streams,
  Core.Keywords,
  Core.Timer,
  Core.XML,

  Encryption.SHA,
  Encryption.Simple,

  RSR,
  RSR.Core,
  RSR.HTTP,

  Storage.Types,
  Storage.FAT,
  Storage.AuraDisks,

  App.Build,
  App.Consts,

  SysUtils,
  classes,
  IniFiles,
  db,
  sqldb,
  Sockets;

const
  INI_DB_SECT_MATRIX='Matrix';

  INI_DB_VALUE_CLUSTERID='ClusterID';
  INI_DB_VALUE_RESOURCEID='ResourceID';
  INI_DB_VALUE_NODEID='NodeID';
  INI_DB_VALUE_AUXILIARY_NODES='Auxiliary Nodes';
  INI_DB_VALUE_LOAD_DISKS='Load Disks';
  INI_GUI_LOAD_SERVICES   = 'GUI Load Services';
  INI_LOAD_DISKS          =  'Load Disks';

  procedure Load_UA_TYPES_Strings(SL:TStrings);

  function  WrapValue(sString,sDelimiter:String; BreakChars: TSysCharSet; iMaxLength:LongInt):String;

  procedure ReadINIFile;
  procedure SaveINIFile;

var
  ClusterID                      : Core.Database.Types.LargeWord;
  ResourceID                     : Core.Database.Types.LargeWord;
  NodeID                         : Core.Database.Types.LargeWord;

  guiServices                    : Core.Database.Types.Bool;
  AuDisks                        : Core.Database.Types.Bool;
  AuxNodes                       : Core.Database.Types.LargeWordArray;

implementation
uses
  App.IniFile,
  Storage.Main,
  DateUtils;

procedure SaveINIFile;
begin
  App.IniFile.Native.WriteString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_HOST,Storage.Main.Header.HostName);
  App.IniFile.Native.WriteString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_USER,Storage.Main.Header.Username);
  App.IniFile.Native.WriteString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_PASSWORD,Encryption.Simple.EncryptString(Storage.Main.Header.Password));
  App.IniFile.Native.WriteString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_SCHEMA,Storage.Main.Header.Schema);
  App.IniFile.Native.WriteString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_ENCODING,Storage.Main.Header.Encoding);
  App.IniFile.Native.WriteString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_MODE,DB_MODES[Storage.Main.Header.Mode]);
  App.IniFile.Native.WriteBool(INI_DB_SECT_MATRIX,INI_GUI_LOAD_SERVICES,guiServices);
  App.IniFile.Native.WriteBool(INI_DB_SECT_MATRIX,INI_DB_VALUE_LOAD_DISKS,AuDisks);
  App.IniFile.Native.WriteString(INI_DB_SECT_MATRIX,INI_DB_VALUE_AUXILIARY_NODES,Core.Arrays.LargeWord.toString(AuxNodes,','));

  Core.Database.Monitor.SetIniValues(App.IniFile.Native);
end;

procedure ReadINIFile();
begin
  guiServices:=App.IniFile.Native.ReadBool(INI_DB_SECT_MATRIX,INI_GUI_LOAD_SERVICES,false);
  auDisks:=App.IniFile.Native.ReadBool(INI_DB_SECT_MATRIX,INI_DB_VALUE_LOAD_DISKS,true);


  ClusterID:=App.IniFile.Native.ReadInteger(INI_DB_SECT_MATRIX,INI_DB_VALUE_CLUSTERID,0);
  ResourceID:=App.IniFile.Native.ReadInteger(INI_DB_SECT_MATRIX,INI_DB_VALUE_RESOURCEID,0);
  NodeID:=App.IniFile.Native.ReadInteger(INI_DB_SECT_MATRIX,INI_DB_VALUE_NODEID,0);

  Core.Arrays.LargeWord.fromString(
    App.IniFile.Native.ReadString(INI_DB_SECT_MATRIX,INI_DB_VALUE_AUXILIARY_NODES,''),
    AuxNodes,
    ','
  );
end;

procedure Load_UA_TYPES_Strings(SL:TStrings);
var
  iLcv:LongInt;
begin
  SL.Clear;
  For iLcv:=Low(UA_Types_Strings) to High(UA_Types_Strings) do
    SL.Add(UA_Types_Strings[iLcv]);
end;

Function WrapValue(sString,sDelimiter:String; BreakChars: TSysCharSet; iMaxLength:LongInt):String;
var
 iLen:LongInt;
 iCharacterCounter:LongInt;
 iLcv:LongInt;

 iLastKnownBreak:LongInt;


 Function IsBreakBefore(iEnd:LongInt):Boolean;
 var
   iLcv:LongInt;
 begin
   iLcv:=iEnd; iLastKnownBreak:=0;
   While (iLcv>0) and (iLastKnownBreak=0) do begin
     If sString[iLcv] in BreakChars then
       iLastKnownBreak:=iLcv;
     Dec(iLcv);
   end;
   Result:=iLastKnownBreak<>0;
 end;


begin
  iLen:=Length(sString); iLastKnownBreak:=0;  Result:=sString; iCharacterCounter:=0;
  iLcv:=1;
  While (iLcv<=iLen) do begin
    If sString[iLcv]=#10 then
      iCharacterCounter:=0
    else
      Inc(iCharacterCounter);
    If iCharacterCounter=iMaxLength then begin
      If IsBreakBefore(iLcv) then
        System.Insert(sDelimiter,Result,iLastKnownBreak)
      else
        System.Insert(sDelimiter,Result,iLcv);
      iCharacterCounter:=0;
    end;
    Inc(iLcv);
  end;
end;



procedure PushFileOpen(Const FName:TFilename; Var hStream:LongInt);
var
  FCResult:LongInt;
begin
  If not FileExists(FName) then begin
    FCResult:=FileCreate(FName);
    If FCResult=-1 then begin
      Raise Exception.Create(
      Concat(
        'PushFileOpen: The file "',FName,'" could not be created!'
      ));
    end else
      FileClose(FCResult);
  end;
  hStream:=FileOpen(FName,fmOpenReadWrite or fmShareDenyNone);
  If hStream=-1 then begin
    Raise Exception.Create(Concat(
      'PushFileOpen: The file "',FName,'" could not be opened!'
    ));
  end;
end;


end.

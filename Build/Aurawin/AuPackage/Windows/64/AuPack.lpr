program AuPack;

{$mode objfpc}{$H+}

uses
  Classes,uKPList,uStringArray,uZip,SysUtils;

Type
  Pack=class
  const
    AppFolder  : string='';
    TempFolder : string='';
    AuPackage  : string =
    {$if defined(Unix)}
      'AuPackage';
    {$elseif defined(Windows)}
      'AuPackage.exe';
    {$else}
      'AuPackage';
    {$endif}
    Parameters : PKPList = nil;
    Master     : TMemoryStream = nil;
  type
    class procedure Init();
    class procedure LoadParameters();
    class procedure LoadFiles();
  end;

  class procedure Pack.LoadParameters();
  var
    iLen:integer;
    iCt:integer;
    iLcv:integer;
    saEntry:TStringArray;
    ListP:PKPList;
    ZipFile:TZipper;
    MS:TMemoryStream;
    sFileName:string;
    sPackFile:string;
    FSOut:TFileStream;
    iSize:QWord;
  begin
    ListP:=Pack.Parameters;
    iCt:=System.Paramcount();
    for iLcv:=1 to iCt do begin
      uStringArray.fromString(@saEntry,System.ParamStr(iLcv),'=',[sasoClearList,sasoRemoveQuotes,sasoSingleton,sasoTrimLines]);
      iLen:=System.Length(saEntry);
      case iLen of
        1: uKPList.Add(ListP, saEntry[0],'',[]);
        2: uKPList.Add(ListP,saEntry[0],saEntry[1],[]);
      end;
    end;

    ZipFile:=TZipper.Create();
    Try
      for iLcv:=0 to High(ListP^) do begin
        if (ListP^[iLcv]^.Key='-A') then begin
          sFileName:=SysUtils.ExtractFileName(ListP^[iLcv]^.Value);
          ZipFile.Entries.AddFileEntry(ListP^[iLcv]^.Value,sFileName);
        end;
      end;
      MS:=TMemoryStream.Create();
      Try
        ZipFile.SaveToStream(MS);
        iSize:=MS.Size;
        Master.Position:=0;
        MS.Position:=0;
        sPackFile:=uKPList.GetItemAsString(ListP^,'-P');
        FSOut:=TFileStream.Create(sPackFile,fmCreate or fmShareDenyNone);
        Try
          FSOut.CopyFrom(Master,Master.Size);
          FSOut.Position:=FSOut.Size;
          FSOut.CopyFrom(MS,iSize);
          FSOut.Position:=FSOut.Size;
          FSOut.Write(iSize,SizeOf(QWord));
        finally
          FSOut.Free();
        end;
      finally
        MS.Free();
      end;
    finally
      ZipFile.Free();
    end;
  end;

  class procedure Pack.Init();
  begin
    AppFolder:=SysUtils.ExcludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
    TempFolder:=SysUtils.ExcludeTrailingPathDelimiter(SysUtils.GetTempDir(true));
    Master:=TMemoryStream.Create();
    Master.LoadFromFile(Concat(AppFolder,PathDelim,AuPackage));
    new(Parameters);
    uKPList.Init(Parameters^);
    LoadParameters();
  end;

  class procedure Pack.LoadFiles();
  begin

  end;

begin
  Pack.Init();



end.


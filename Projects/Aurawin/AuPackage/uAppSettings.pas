unit uAppSettings;

{$mode objfpc}{$H+}

interface

uses
  Classes,uZip;

Type
  Logic=class
  const
    Step     : LongInt = 0;
    Welcome  = 1;
    License  = 2;
    Copy     = 3;
    Finished = 4;
    Canceled = 5;
  end;
  Package=class
  type

    Resources = class
    Type
      TResource=record
        Name         : string;
        Target       : string;
        Installed    : string;
        Size         : Cardinal;
        Stream       : TMemoryStream;
      end;
      PResource=^TResource;
      PResources=^TResources;
      TResources=array of PResource;
      Links = class
      type
        TLink=record
          Name         : string;
          Folder       : string;
          Icon         : string;
          Target       : string;
          Description  : string;
          Installed    : string;
          IconIndex    : LongInt;
          ResourceP    : PResource;
        end;
        PLink=^TLink;
        TLinks=array of PLink;
        PLinks=^TLinks;
        class function prepFolder(var Item:TLink):string;
        class function Add(var List:TLinks; Name,Folder,Target,Description,Icon:string; IconIndex:integer):PLink;
        class function CreateLink(var Item:TLink):boolean;
      end;

      class procedure Init();
      class function prepFolder(var Item:TResource):string;

      class function Get(var List:TResources; Name:string):PResource;
      class function Add(var List:TResources; Name:string; Size:Cardinal; Stream:TMemoryStream):PResource;
    const
      LinksP      : Links.PLinks = nil;
      ResourcesP  : PResources = nil;
      LauncherP   : PResource = nil;
      ReadmeP     : PResource = nil;
      LicenseP    : PResource = nil;
      IconP       : PResource = nil;
      Extracted   : boolean = false;
      License     : string = 'License.txt';
      Readme      : string = 'Readme.txt';
      Image       : string = 'Image.png';
      Executeable : string = '';
    end;
  const
    Temp         : string = '';
    TempFileExt  : string = '.dat';
    {$if defined(Unix)}
    User         : string = '/usr/local';
    ETC          : string = '/etc';

    {$elseif defined (Windows)}
    Lib           : string = 'C:\Windows\System';
    Lib32         : string = 'C:\Windows\System32';
    ProgramFiles  : string = 'C:\Program Files';
    ProgramData   : string = 'C:\ProgramData';
    StartMenu     : string = '';
    StartMenuPrograms : string = '';
    StartMenuStartup : string = '';
    {$else}

    {$endif}
    class procedure Init();
  end;

  GUI=class
  const
    {$if defined(Unix)}
      BUTTON_BOX_LRS=3;
      BUTTON_BOX_TBS=3;
      BUTTON_BOX_A=4;
      TEXT_A=1;
      TEXT_TOP=1;
      TEXT_BOTTOM=1;
      TEXT_LEFT=1;
      TEXT_RIGHT=1;
      PANEL_A=4;
      PAGES_A=4;
    {$elseif defined(Windows)}
      BUTTON_BOX_LRS=3;
      BUTTON_BOX_TBS=3;
      BUTTON_BOX_A=4;
      TEXT_A=1;
      TEXT_TOP=1;
      TEXT_BOTTOM=1;
      TEXT_LEFT=0;
      TEXT_RIGHT=1;
      PANEL_A=4;
      PAGES_A=4;
    {$else}
      BUTTON_BOX_LRS=3;
      BUTTON_BOX_TBS=3;
      BUTTON_BOX_A=4;
      TEXT_A=1;
      TEXT_TOP=1;
      TEXT_BOTTOM=1;
      TEXT_LEFT=0;
      TEXT_RIGHT=1;
      PANEL_A=4;
      PAGES_A=4;
     {$endif}
  end;
  Manifest=class
  const
    Roster     : string = 'Manifest.xml';
    Stanza     : string = 'manifest';
    Product    : string = 'product';
    Company    : string = 'company';
    Items      : string = 'files';
    Item       : string = 'file';
    Name       : string = 'name';
    Links      : string = 'links';
    Link       : string = 'link';
    Folder     : string = 'folder';
    Target     : string = 'target';
    Image      : string = 'image';
    Icon       : string = 'icon';
    IconIndex  : string = 'icon-index';
    Executable : string = 'program';
    Description: string = 'des';
  type
    class procedure Init();
  end;

  Lang=class
  Type
    Welcome=class
    const
      Title            : string = 'Welcome to $Product';
      Description      : string= 'Installing $Product.';
    end;
    License=class
    const
      Title            : string = 'License Agreement';
      Description      : string ='Please agree and adhere to this agreement';
    end;
    Copying=class
    const
      Source           : string =' Source';
      Destination      : string = ' Destination';
      PleaseClose      : string ='Please close file...';
      ThanksForClosing : string = 'Thanks for closing...';
    end;
    Canceled=class
    const
      Stopped          : string ='Installation canceled.';
    end;

    Finished=class
    const
      Complete         : string = 'Installation complete!';
    end;
    class procedure Init(sCompany,sProduct:string);
  end;
  TExtractor=Class(TUnZipper)
  private
    procedure DoStreamCreate(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    procedure DoStreamDestroy(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
  public
    Constructor Create; reIntroduce;
  end;
implementation
uses
{$if defined(Windows)}
  Windows,Registry,WinShell,ShellAPI,shFolder,
{$endif}
  SysUtils,XMLRead,DOM, Core.XML;

class procedure Package.Resources.Init();
var
  sFileName:string;
  sTempFileName:string;
  FSData:TFileStream;
  iSize:QWord;
  FPayload  : TFileStream;
  ZPayload  : TUnZipper;
  iLcv      : LongInt;


begin
  iSize:=0;
  new(LinksP);
  new(ResourcesP);
  sFileName:=SysUtils.ExtractFileName(ParamStr(0));
  sFileName:=SysUtils.ChangeFileExt(sFileName,TempFileExt);
  sTempFileName:=Concat(Package.Temp,PathDelim,sFileName);
  FSData:=TFileStream.Create(ParamStr(0),fmOpenRead or fmShareDenyNone);
  Try
    FSData.Position:=FSData.Size-SizeOf(QWord);
    FSData.Read(iSize,SizeOf(QWord));
    FSData.Position:=FSData.Size-(iSize+SizeOf(QWord));
    FPayload:=TFileStream.Create(sTempFileName,fmCreate or fmShareDenyNone);
    try
      FPayload.CopyFrom(FSData,iSize);
    finally
      FPayload.Free();
    end;
    ZPayload:=TExtractor.Create();
    Try
      ZPayload.FileName:=sTempFileName;
      ZPayload.OutputPath:=Package.Temp;
      ZPayload.Examine();
      ZPayload.UnZipAllFiles();
      Extracted:=true;
    finally
      ZPayload.Free();
    end;
  finally
    FSData.Free();
  end;
end;

class function Package.Resources.Add(var List:TResources; Name:string; Size:Cardinal; Stream:TMemoryStream):PResource;
var
  iCt:integer;
  rcP:PResource;
begin
  new(rcP);
  rcP^.Name:=Name;
  rcP^.Size:=Size;
  rcP^.Target:='';
  rcP^.Installed:='';
  rcP^.Stream:=Stream;
  iCt:=Length(List);
  SetLengtH(List,iCt+1);
  List[iCt]:=rcP;
  Result:=rcP;
end;

class function Package.Resources.prepFolder(var Item:TResource):string;
begin
  Result:=Item.Target;
  Result:=StringReplace(Result,'$ProgramFiles',Package.ProgramFiles,[rfReplaceAll]);
  Result:=StringReplace(Result,'$Lib32',Package.Lib32,[rfReplaceAll]);
  Result:=StringReplace(Result,'$Lib',Package.Lib,[rfReplaceAll]);
  Result:=SysUtils.IncludeTrailingBackslash(Result);
  Result:=Concat(Result,Item.Name);
end;

class function Package.Resources.Get(var List:TResources; Name:string):PResource;
var
  iLcv:integer;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    if List[iLcv]^.Name=Name then begin
      Result:=List[iLcv];
      Exit;
    end;
  end;
end;

class procedure Package.Init();
var
  sPath:Array[0..255] of char;
  iLength:integer;
begin
  Temp:=ExcludeTrailingPathDelimiter(SysUtils.GetTempDir(True));

  {$if defined(Unix)}

  {$elseif defined(Windows)}
    FillChar(sPath[0],255,#0);
    Windows.GetSystemDirectory(@sPath[0],255);
    Lib32:=ExcludeTrailingPathDelimiter(StrPas(sPath));
    {$if defined(CPU64)}
      FillChar(sPath[0],255,#0);
      Windows.GetWindowsDirectory(@sPath[0],255);
      Lib:=IncludeTrailingPathDelimiter(StrPas(sPath));
      Lib:=Concat(Lib,'SysWOW64');
    {$else}
      FillChar(sPath[0],255,#0);
      Windows.GetWindowsDirectory(@sPath[0],255);
      Lib:=IncludeTrailingPathDelimiter(StrPas(sPath));
      Lib:=Concat(Lib,'System');
    {$endif}
    ProgramFiles:=ExcludeTrailingPathDelimiter(GetEnvironmentVariable('ProgramFiles'));
    ProgramData:=ExcludeTrailingPathDelimiter(SysUtils.GetAppConfigDir(true));

    FillChar(sPath[0],255,#0);
    SHGetFolderPath(0,CSIDL_STARTMENU,0,0,@sPath[0]);
    StartMenu:=StrPas(sPath);

    FillChar(sPath[0],255,#0);
    SHGetFolderPath(0,CSIDL_COMMON_PROGRAMS,0,0,@sPath[0]);
    StartMenuPrograms:=StrPas(sPath);

    FillChar(sPath[0],255,#0);
    SHGetFolderPath(0,CSIDL_COMMON_STARTUP,0,0,@sPath[0]);
    StartMenuStartup:=StrPas(sPath);
  {$else}

  {$endif}

end;

class procedure Manifest.Init();
var
  xDoc      : TXMLDocument;
  xSrc      : TXMLInputSource;
  xManifest : TDOMNode;
  xFiles    : TDOMNode;
  xFile     : TDOMNode;
  xCompany  : TDOMNode;
  xProduct  : TDOMNode;
  xDescription : TDOMNode;
  xImage    : TDOMNode;
  xIcon     : TDOMNode;
  xIconIndex : TDOMNode;
  xName     : TDOMNode;
  xTarget   : TDOMNOde;
  xProgram  : TDOMNode;
  xStartMenu : TDOMNode;
  xFolder    : TDOMNode;
  xLinks     : TDOMNode;
  xLink      : TDOMNode;
  iLcv      : LongInt;
  rcsP      : Package.Resources.PResources;
  rcP       : Package.Resources.PResource;
  lnksP     : Package.Resources.Links.PLinks;
  lnkP      : Package.Resources.Links.PLink;
begin
  if (Package.Resources.Extracted=true) then begin
    lnksP:=Package.Resources.LinksP;
    rcsP:=Package.Resources.ResourcesP;
    rcP:=Package.Resources.Get(rcsP^,Roster);
    if rcP<>nil then begin
      rcP^.Stream.Position:=0;
      XMLRead.ReadXMLFile(xDoc,rcP^.Stream);
      if (xDoc<>nil) then begin
        xManifest:=XML.getNode(xDoc,Stanza);
        if (xManifest<>nil) then begin
          xProduct:=XML.getChildNode(xManifest,Product);
          xCompany:=XML.getChildNode(xManifest,Company);
          xFiles:=XML.getChildNode(xManifest,Items);
          xLinks:=XML.getChildNode(xManifest,Links);
          xImage:=XML.getChildNode(xManifest,Image);
          xProgram:=XML.getChildNode(xManifest,Executable);
          if (xProgram<>nil) then begin
            Package.Resources.Executeable:=xProgram.TextContent;
            Package.Resources.LauncherP:=Package.Resources.Get(rcsP^,xProgram.TextContent);
          end;
          Package.Resources.LicenseP:=Package.Resources.Get(rcsP^,Package.Resources.License);
          Package.Resources.ReadmeP:=Package.Resources.Get(rcsP^,Package.Resources.Readme);

          if (xImage<>nil) then Package.Resources.Image:=xImage.TextContent;
          Lang.Init(xCompany.TextContent,xProduct.TextContent);
          if (xFiles<>nil) then begin
            for iLcv:=0 to xFiles.ChildNodes.Count-1 do begin
              xFile:=xFiles.ChildNodes[iLcv];
              xName:=XML.getChildNode(xFile,Name);
              xTarget:=XML.getChildNode(xFile,Target);
              rcP:=Package.Resources.Get(rcsP^,xName.TextContent);
              if (rcP<>nil) then begin
                rcP^.Target:=xTarget.TextContent;
              end;
            end;
          end;
          if (xLinks<>nil) then begin
            for iLcv:=0 to xLinks.ChildNodes.Count-1 do begin
              xLink:=xLinks.ChildNodes[iLcv];
              if xLink<>nil then begin
                xName:=XML.getChildNode(xLink,Name);
                if xName=nil then
                  raise Exception.Create('Missing or corrupt XML Link Tag "name"');
                xFolder:=XML.getChildNode(xLink,Folder);
                if xFolder=nil then
                  raise Exception.Create('Missing or corrupt XML Link Tag "folder"');
                xFile:=XML.getChildNode(xLink,Item);
                if xFile=nil then
                  raise Exception.Create('Missing or corrupt XML Link Tag "folder"');
                xDescription:=XML.getChildNode(xLink,Description);
                if xDescription=nil then
                  raise Exception.Create('Missing or corrupt XML Link Tag "des"');
                lnkP:=Package.Resources.Links.Add(lnksP^,xName.TextContent,xFolder.TextContent,xFile.TextContent,xDescription.TextContent,'',0);
                xIcon:=XML.getChildNode(xLink,Icon);
                if (xIcon<>nil) then begin
                  lnkP^.Icon:=xIcon.TextContent;
                  xIconIndex:=XML.getChildNode(xLink,IconIndex);
                  if (xIconIndex<>nil) then
                    lnkP^.IconIndex:=StrToIntDef(xIconIndex.TextContent,0);
                end;
              end;
            end;
          end;

        end;

      end;
    end;
  end else begin
    Lang.Init('Aurawin LLC','AuPackage');
  end;
end;

class procedure Lang.Init(sCompany,sProduct:string);
begin
  Welcome.Title:=StringReplace(Welcome.Title,'$Company',sCompany,[rfReplaceAll]);
  Welcome.Title:=StringReplace(Welcome.Title,'$Product',sProduct,[rfReplaceAll]);

  Welcome.Description:=StringReplace(Welcome.Description,'$Company',sCompany,[rfReplaceAll]);
  Welcome.Description:=StringReplace(Welcome.Description,'$Product',sProduct,[rfReplaceAll]);

  License.Title:=StringReplace(License.Title,'$Company',sCompany,[rfReplaceAll]);
  License.Title:=StringReplace(License.Title,'$Product',sProduct,[rfReplaceAll]);

  License.Description:=StringReplace(License.Description,'$Company',sCompany,[rfReplaceAll]);
  License.Description:=StringReplace(License.Description,'$Product',sProduct,[rfReplaceAll]);
end;

procedure TExtractor.DoStreamCreate(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
begin
  AStream:=TMemoryStream.Create();
end;

procedure TExtractor.DoStreamDestroy(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
var
  ResourcesP:Package.Resources.PResources;
begin
  ResourcesP:=Package.Resources.ResourcesP;
  AStream.Position:=0;
  Package.Resources.Add(ResourcesP^,AItem.ArchiveFileName,AItem.Size,TMemoryStream(AStream));
end;

Constructor TExtractor.Create;
begin
  OnCreateStream:=@DoStreamCreate;
  OnDoneStream:=@DoStreamDestroy;
  Inherited Create();
end;

class function Package.Resources.Links.Add(var List:TLinks; Name,Folder,Target,Description,Icon:string; IconIndex:integer):PLink;
var
  iCt:integer;
  lnP:PLink;
begin
  Result:=nil;

  new(lnP);
  lnP^.Name:=Name;
  lnP^.Folder:=Folder;
  lnP^.Target:=Target;
  lnP^.Icon:=Icon;
  lnP^.IconIndex:=IconIndex;
  lnP^.Description:=Description;
  lnP^.Installed:='';
  lnP^.ResourceP:=Get(ResourcesP^,Target);

  iCt:=Length(List);
  SetLength(List,iCt+1);
  List[iCt]:=lnP;

  Result:=lnP;
end;

class function Package.Resources.Links.CreateLink(var Item:TLink):boolean;
var
  sLinkFile:string;
  sWorkingFolder:string;
  rcP:Resources.PResource;
begin
  sLinkFile:=Concat(prepFolder(Item),Item.Name,'.lnk');
  sWorkingFolder:=IncludeTrailingPathDelimiter(ExtractFilePath(Item.ResourceP^.Installed));
  if (Length(Item.Icon)>0) then begin
    rcP:=Resources.Get(Resources.ResourcesP^,Item.Icon);
    if (rcP<>nil) then begin
      Item.Icon:=rcP^.Installed;
    end else begin
      Item.Icon:=#0;
      Item.IconIndex:=0;
    end;
  end else
    Item.Icon:=#0;
  Try
    WinShell.CreateShortcut(
      @sLinkFile[1],                 //pszLinkFile
      @Item.ResourceP^.Installed[1], //pszPathName,
      nil,                           //pszArgs
      @sWorkingFolder[1],            //pszWorkingDir,
      @Item.Description[1],          //pszDesc,
      @Item.Icon[1],                 //pszIconPath
      Item.IconIndex                 //nIconIndex
    );
  except
    on e:exception do begin
      // what to do?
    end;
  end;
  Item.Installed:=sLinkFile;
end;

class function Package.Resources.Links.prepFolder(var Item:TLink):string;
begin
  Result:=Item.Folder;
  Result:=StringReplace(Result,'$StartMenuPrograms',Package.StartMenuPrograms,[rfReplaceAll]);
  Result:=StringReplace(Result,'$StartMenuStartup',Package.StartMenuStartup,[rfReplaceAll]);
  Result:=StringReplace(Result,'$StartMenu',Package.StartMenu,[rfReplaceAll]);
  Result:=SysUtils.IncludeTrailingPathDelimiter(Result);
  SysUtils.ForceDirectories(Result);
end;

initialization

end.


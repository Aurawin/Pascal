{
 unit Storage.RTSP.pas

 Copyright Aurawin LLC 2003-2015
 Written by: Andrew Thomas Brunner

 This code is protected under the Aurawin Public Release License
}

unit Storage.RTSP;

interface

uses
  Classes,

  RSR,

  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.KeyString,
  Core.Arrays.VarString,
  Core.Arrays.LargeWord,
  Core.Arrays.Bytes,
  Core.Timer,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Storage.AuraDisks,

  SysUtils;

type
  Manifest = class
  type
    PItem=^TItem;
    TItem=record
      ID                       : QWord;
      FolderID                 : QWord;
      FileID                   : QWord;
      SpaceID                  : QWord;
      Size                     : QWord;
      Modified                 : Double;
      Pages                    : LongInt;
      BinCount                 : LongInt;
      Bins                     : Core.Arrays.Types.LargeWord;
    end;
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        FolderID                 : Core.Database.Types.Integer = 4;
        FileID                   : Core.Database.Types.Integer = 5;
        SpaceID                  : Core.Database.Types.Integer = 6;
        Size                     : Core.Database.Types.Integer = 7;
        Modified                 : Core.Database.Types.Integer = 8;
        Pages                    : Core.Database.Types.Integer = 9;
        BinCount                 : Core.Database.Types.Integer = 10;
        Bins                     : Core.Database.Types.Integer = 11;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        FolderID                 : Core.Database.Types.VarString = 'IFID';
        FileID                   : Core.Database.Types.VarString = 'IFLD';
        SpaceID                  : Core.Database.Types.VarString = 'ISID';
        Size                     : Core.Database.Types.VarString = 'ISZE';
        Modified                 : Core.Database.Types.VarString = 'ITMD';
        Pages                    : Core.Database.Types.VarString = 'ITGS';
        BinCount                 : Core.Database.Types.VarString = 'ITBC';
        Bins                     : Core.Database.Types.VarString = 'BINS';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Services/Streaming';
        Name                     : 'Manifest';
        Value                    : 'scs_rts_mt';
        Hint                     : 'Stream Manifest storage';
        PrimaryKeyP              : @Keys.ID;
        );
      Fields                     : array [0..11] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.FolderID; KeyP: @Keys.FolderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.FileID; KeyP: @Keys.FileID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.SpaceID; KeyP: @Keys.SpaceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Size; KeyP: @Keys.Size; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Pages; KeyP: @Keys.Pages; DataType: dftInteger; AutoCreate: false; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.BinCount; KeyP: @Keys.BinCount; DataType: dftInteger; AutoCreate: false; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Bins; KeyP: @Keys.Bins; DataType: dftQWordArray; AutoCreate: True; Verified: False; Precision: 1024*1024*4; Flags: cfNone;  )
      );
      class function Read(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID:QWord; Space:LongInt; out Item:TItem):boolean;
      class function createStream(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID:QWord; Space:LongInt; Size:QWord; out Item:TItem):boolean;
      class function deleteStream(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Space:LongInt; ItemID:QWord):boolean;
    end;
    class procedure Init(var Item:TItem);
    class procedure Done(var Item:TItem);
    class procedure Empty(var Item:TItem);
  end;
  SDP = class
  type
    DB = class
    type
      IDs = class
      const
        ManifestID               : Core.Database.Types.Integer = 0;
        DomainID                 : Core.Database.Types.Integer = 1;
        UserID                   : Core.Database.Types.Integer = 2;
        FolderID                 : Core.Database.Types.Integer = 3;
        FileID                   : Core.Database.Types.Integer = 4;
        FileModified             : Core.Database.Types.Integer = 5;
        SpaceID                  : Core.Database.Types.Integer = 6;
        Modified                 : Core.Database.Types.Integer = 7;
        Data                     : Core.Database.Types.Integer = 8;
        sessionVersion           : Core.Database.Types.Integer = 9;
        sessionOrigin            : Core.Database.Types.Integer = 10;
        sessionName              : Core.Database.Types.Integer = 11;
        sessionInformation       : Core.Database.Types.Integer = 12;
        sessionURI               : Core.Database.Types.Integer = 13;
        sessionEmail             : Core.Database.Types.Integer = 14;
        sessionPhone             : Core.Database.Types.Integer = 15;
        sessionConnection        : Core.Database.Types.Integer = 16;
        sessionBandwidth         : Core.Database.Types.Integer = 17;
        sessionEncryption        : Core.Database.Types.Integer = 18;
        sessionAttributes        : Core.Database.Types.Integer = 19;
        timeSession              : Core.Database.Types.Integer = 20;
        timeStart                : Core.Database.Types.Integer = 21;
        timeEnd                  : Core.Database.Types.Integer = 22;
        timeRepeat               : Core.Database.Types.Integer = 23;
        timeZone                 : Core.Database.Types.Integer = 24;
        mediaHeaders             : Core.Database.Types.Integer = 25;
        mediaTitle               : Core.Database.Types.Integer = 26;
        mediaConnection          : Core.Database.Types.Integer = 27;
        mediaInformation         : Core.Database.Types.Integer = 28;
        mediaEncryption          : Core.Database.Types.Integer = 29;
        mediaAttributes          : Core.Database.Types.Integer = 30;
      end;
      Keys=class
      const
        ManifestID               : Core.Database.Types.VarString = 'IMID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        FolderID                 : Core.Database.Types.VarString = 'IFID';
        FileID                   : Core.Database.Types.VarString = 'IFLD';
        FileModified             : Core.Database.Types.VarString = 'IFMD';
        SpaceID                  : Core.Database.Types.VarString = 'ISID';
        Modified                 : Core.Database.Types.VarString = 'IMOD';
        Data                     : Core.Database.Types.VarString = 'IDAT';
        sessionVersion           : Core.Database.Types.VarString = 'SNVS';
        sessionOrigin            : Core.Database.Types.VarString = 'SNOR';
        sessionName              : Core.Database.Types.VarString = 'SNME';
        sessionInformation       : Core.Database.Types.VarString = 'SNIF';
        sessionURI               : Core.Database.Types.VarString = 'SNUR';
        sessionEmail             : Core.Database.Types.VarString = 'SNEM';
        sessionPhone             : Core.Database.Types.VarString = 'SNPH';
        sessionConnection        : Core.Database.Types.VarString = 'SNCN';
        sessionBandwidth         : Core.Database.Types.VarString = 'SNBW';
        sessionEncryption        : Core.Database.Types.VarString = 'SNKY';
        sessionAttributes        : Core.Database.Types.VarString = 'SNAT';
        timeSession              : Core.Database.Types.VarString = 'TMSN';
        timeStart                : Core.Database.Types.VarString = 'TMSS';
        timeEnd                  : Core.Database.Types.VarString = 'TMED';
        timeRepeat               : Core.Database.Types.VarString = 'TMRP';
        timeZone                 : Core.Database.Types.VarString = 'TMZN';
        mediaHeaders             : Core.Database.Types.VarString = 'MDHS';
        mediaTitle               : Core.Database.Types.VarString = 'MDTL';
        mediaConnection          : Core.Database.Types.VarString = 'MDCN';
        mediaInformation         : Core.Database.Types.VarString = 'MDIF';
        mediaEncryption          : Core.Database.Types.VarString = 'MDKY';
        mediaAttributes          : Core.Database.Types.VarString = 'MDAT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Services/Streaming';
        Name                     : 'Description';
        Value                    : 'scs_rts_dsc';
        Hint                     : 'Stream Description storage';
        PrimaryKeyP              : NoKey;
        );
      Fields                     : array [0..30] of Core.Database.Types.Field = (
        (IDP: @IDs.ManifestID; KeyP: @Keys.ManifestID;  DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfIdentity; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.FolderID; KeyP: @Keys.FolderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.FileID; KeyP: @Keys.FileID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.FileModified; KeyP: @Keys.FileModified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.SpaceID; KeyP: @Keys.SpaceID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.sessionVersion; KeyP: @Keys.sessionVersion; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.sessionOrigin; KeyP: @Keys.sessionOrigin; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.sessionName; KeyP: @Keys.sessionName; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.sessionInformation; KeyP: @Keys.sessionInformation; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.sessionURI; KeyP: @Keys.sessionURI; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.sessionEmail; KeyP: @Keys.sessionEmail; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.sessionPhone; KeyP: @Keys.sessionPhone; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.sessionConnection; KeyP: @Keys.sessionConnection; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.sessionBandwidth; KeyP: @Keys.sessionBandwidth; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.sessionEncryption; KeyP: @Keys.sessionEncryption; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.sessionAttributes; KeyP: @Keys.sessionAttributes; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.timeSession; KeyP: @Keys.timeSession; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.timeStart; KeyP: @Keys.timeStart; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.timeEnd; KeyP: @Keys.timeEnd; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.timeRepeat; KeyP: @Keys.timeRepeat; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.timeZone; KeyP: @Keys.timeZone; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.mediaHeaders; KeyP: @Keys.mediaHeaders; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.mediaTitle; KeyP: @Keys.mediaTitle; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.mediaConnection; KeyP: @Keys.mediaConnection; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.mediaInformation; KeyP: @Keys.mediaInformation; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.mediaEncryption; KeyP: @Keys.mediaEncryption; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.mediaAttributes; KeyP: @Keys.mediaAttributes; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );
    end;
    PItem=^TItem;
    TItem=record
      ManifestID               : QWord;
      FolderID                 : QWord;
      FileID                   : QWord;
      FileModified             : Double;
      SpaceID                  : QWord;
      Modified                 : Double;
      Data                     : Core.Strings.VarString;
      sessionVersion           : LongInt;
      sessionOrigin            : Core.Strings.VarString;
      sessionName              : Core.Strings.VarString;
      sessionInformation       : Core.Strings.VarString;
      sessionURI               : Core.Strings.VarString;
      sessionEmail             : Core.Strings.VarString;
      sessionPhone             : Core.Strings.VarString;
      sessionConnection        : Core.Strings.VarString;
      sessionBandwidth         : Core.Strings.VarString;
      sessionEncryption        : Core.Strings.VarString;
      sessionAttributes        : Core.Arrays.Types.VarString;
      timeSession              : Core.Strings.VarString;
      timeStart                : QWord;
      timeEnd                  : QWord;
      timeRepeat               : Core.Strings.VarString;
      timeZone                 : Core.Strings.VarString;
      mediaHeaders             : Core.Arrays.Types.VarString;
      mediaTitle               : Core.Strings.VarString;
      mediaConnection          : Core.Strings.VarString;
      mediaInformation         : Core.Strings.VarString;
      mediaEncryption          : Core.Strings.VarString;
      mediaAttributes          : Core.Arrays.Types.VarString;
    end;
    class procedure Init(var Item:TItem); overload;
    class procedure Done(var Item:TItem); overload;
    class procedure Empty(var Item:TItem); overload;
    class function  toString(Var Item:TItem):Core.Strings.VarString;overload;

    class function Read(Task:Core.Database.Types.TTask; ManifestID:QWord; out Item:TItem):boolean;
    class function Create(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID,ManifestID:QWord; Space:LongInt; FileModified:double; out Item:TItem):boolean;
    class function Delete(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; ManifestID:QWord):boolean;
  end;
  ByteBin=class
  type
    DB = class
    type
      IDs = class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        ManifestID               : Core.Database.Types.Integer = 4;
        Serial                   : Core.Database.Types.Integer = 5;
        Modified                 : Core.Database.Types.Integer = 6;
        Size                     : Core.Database.Types.Integer = 7;
        Data                     : Core.Database.Types.Integer = 8;
      end;
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITID';
        InsertID                 : Core.Database.Types.VarString = 'IIID';
        DomainID                 : Core.Database.Types.VarString = 'IDID';
        UserID                   : Core.Database.Types.VarString = 'IUID';
        ManifestID               : Core.Database.Types.VarString = 'IMFD';
        Serial                   : Core.Database.Types.VarString = 'ISRL';
        Modified                 : Core.Database.Types.VarString = 'ITMD';
        Size                     : Core.Database.Types.VarString = 'ISZE';
        Data                     : Core.Database.Types.VarString = 'IDAT';
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Services/Streaming';
        Name                     : 'Byte Bins';
        Value                    : 'scs_rts_bb';
        Hint                     : 'Stream Byte Bin storage';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields                     : array [0..8] of Core.Database.Types.Field = (
        (IDP: @IDs.ID; KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;  ),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.ManifestID; KeyP: @Keys.ManifestID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Serial; KeyP: @Keys.Serial; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Size; KeyP: @Keys.Size; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Data; KeyP: @Keys.Data; DataType: dftByteBuffer; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; )
      );
    end;
  const
    maxBins                    = 5;
    binSize                    = (1024*1024*1)+(1024*512);
  type
    PItem=^TItem;
    PBin=^TBin;
    PBins=^TBins;
    TItem=record
      ID                       : QWord;
      ManifestID               : QWord;
      Serial                   : LongInt;
      Modified                 : Double;
      Size                     : QWord;
      Data                     : Core.Arrays.Types.Bytes;
    end;
    TBin=record
      fillWait                 : boolean;
      sendWait                 : boolean;
      Size                     : LongInt;
      Data                     : Array[0..binSize-1] of byte;
    end;
    TBins                      = Array[0..maxBins-1] of TBin;

    class procedure Init(var Item:TItem); overload;
    class procedure Init(var Item:TBin); overload;
    class procedure Init(var Item:TBins); overload;
    class procedure Done(var Item:TItem); overload;
    class procedure Done(var Item:TBin); overload;
    class procedure Done(var Item:TBins); overload;
    class procedure Empty(var Item:TItem); overload;
    class procedure Empty(var Item:TBin); overload;
    class procedure Empty(var Item:TBins); overload;

    class function createBin(Task:Core.Database.Types.TTask; DomainID,UserID,ManifestID:QWord; Serial:LongInt; Size:QWord; out ItemID:QWord):boolean;
    class function deleteBin(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; ItemID:QWord):boolean;
    class function readBin(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; out Item:TItem):boolean;
    class function writeBin(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; out Item:TItem):boolean;
  end;
  TManifest=Manifest.TItem;
  TBin=ByteBin.TBin;
  TBins=ByteBin.TBins;
  PManifest=^TManifest;
  PBins=^TBins;
  PBin=^TBin;
  TReader=class
  private
    FItem : ByteBin.TItem;
  public
    function  getNextBin(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var ManifestP:Manifest.PItem; var binIndex:LongInt):boolean;
  public
    constructor Create(); reIntroduce;
    destructor Destroy(); override;
  public
    property Item:ByteBin.TItem read FItem;
  end;

implementation
uses db;

procedure cbDestroyManifest(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with Manifest.DB do begin;
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyByteBin(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with ByteBin.DB do begin;
    {$i Storage.Destroy.Table.inc}
  end;
end;

function cbDBMonitorNotified(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Manifest.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,Manifest.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=SDP.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,SDP.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=ByteBin.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,ByteBin.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;
  procedure PushUserDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Manifest.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,Manifest.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=SDP.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,SDP.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=ByteBin.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,ByteBin.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;
begin
  Result:=False;
  Case Flag of
    Core.Database.Monitor.Notify.DOMAIN_DELETED : PushDomainDeleted;
    Core.Database.Monitor.Notify.USER_DELETED : PushUserDeleted;
  end;
end;

procedure RegisterDB;
var
  iLcv:LongInt;
begin
  with Manifest.DB do begin
    if TableP = nil then begin
      New(TableP);
      Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Init(MonitorP^, TableP^, @cbDestroyManifest, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with ByteBin.DB do begin
    if TableP = nil then begin
      New(TableP);
      Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then begin
        New(MonitorP);
        Init(MonitorP^, TableP^, @cbDestroyByteBin, @cbDBMonitorNotified);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

class procedure Manifest.Init(var Item:TItem);
begin
  With Item do begin
    ID:=0;
    FolderID:=0;
    FileID:=0;
    SpaceID:=0;
    Modified:=0;
    Size:=0;
    Pages:=0;
    Core.Arrays.LargeWord.Init(Bins);
  end;
end;

class procedure Manifest.Empty(var Item:TItem);
begin
  With Item do begin
    ID:=0;
    FolderID:=0;
    FileID:=0;
    SpaceID:=0;
    Modified:=0;
    Size:=0;
    Pages:=0;
    Core.Arrays.LargeWord.Empty(Bins);
  end;
end;


class procedure Manifest.Done(var Item:TItem);
begin
  Core.Arrays.LargeWord.Done(Item.Bins);
  Finalize(Item);
end;

class function Manifest.DB.createStream(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID:QWord; Space:LongInt; Size:QWord; out Item:TItem):boolean;
var
  Chunks:Double;
  Pages:Double;
  iChunks:LongInt;
  iBins:LongInt;
  iMod:LongInt;
  iLcv:LongInt;
  iLastSize:LongInt;
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Item);
  Item.Size:=Size;
  Item.SpaceID:=Space;
  Item.FolderID:=FolderID;
  Item.FileID:=FileID;
  Item.Modified:=Core.Timer.dtUT;
  iMod:=Item.Size mod ByteBin.binSize;
  Chunks:=Item.Size / ByteBin.binSize;
  if (Chunks=0) then begin
    // File size =0 ;
    Item.Pages:=0;
    iChunks:=0;
    iLastSize:=0;
  end else if (Chunks<1) then begin
    iChunks:=1;
    Item.Pages:=1;
    iLastSize:=Size;
  end else if (iMod=0) then begin
    iChunks:=trunc(Chunks);
    iLastSize:=ByteBin.binSize;
    Pages:=iChunks / ByteBin.maxBins;
    Item.Pages:=trunc(Pages);
    if iChunks mod ByteBin.maxBins<>0 then
      Item.Pages+=1;
  end else begin
    iChunks:=trunc(Chunks);
    iLastSize:=Size-(iChunks*ByteBin.binSize);
    iChunks+=1;
    Pages:=iChunks / ByteBin.maxBins;
    Item.Pages:=trunc(Pages);
    if iChunks mod ByteBin.maxBins<>0 then
      Item.Pages+=1;
  end;
  Core.Arrays.LargeWord.SetSize(Item.Bins,iChunks);
  // Create record to get ID of Manifest
  Try
    iCount:=0; iReset:=0; iInsertID:=Random(High(Integer));
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.SpaceID),poNone,oNone,Item.SpaceID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Pages),poNone,oNone,Item.Pages,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (Item.ID<>0);
  Finally
    Core.Database.Empty(Commands);
  End;
  if (Item.ID<>0) then begin
    if  (iChunks>0) then begin
      for iLcv:=0 to High(Item.Bins)-1 do
        ByteBin.createBin(Task,DomainID,UserID,Item.ID,iLcv+1,ByteBin.binSize,Item.Bins[iLcv]);
      ByteBin.createBin(Task,DomainID,UserID,Item.ID,iChunks,iLastSize,Item.Bins[iChunks-1]);
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.ID),poNone,oEqual,Item.ID,Commands);
        Core.Database.AddCommand(iCount,DB.TableP,useForUpdates,Integer(DB.IDs.Bins),poNone,oNone,Item.Bins,Commands);

        Result:=Core.Database.SQL.Update(Task,@Commands);

      finally
        Core.Database.Empty(Commands);
      end;
    end else
      Result:=true;
  end;
end;

class function Manifest.DB.deleteStream(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Space:LongInt; ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Empty(Commands);
  end;
  Try
    iCount := 0;
    Core.Database.AddCommand(iCount, ByteBin.DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, ByteBin.DB.TableP, useForCriteria, ByteBin.DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, ByteBin.DB.TableP, useForCriteria, ByteBin.DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, ByteBin.DB.TableP, useForCriteria, ByteBin.DB.IDs.ManifestID, poAnd, oEqual, ItemID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadManifest(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with Manifest do begin
    PItem(DataP)^.ID:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
    PItem(DataP)^.Size:=Fields.FieldByName(DB.Keys.Size).AsLargeInt;
    PItem(DataP)^.Modified:=Fields.FieldByName(DB.Keys.Modified).AsFloat;
    Core.Arrays.LargeWord.fromString(Fields.FieldByName(DB.Keys.Bins).AsString,PItem(DataP)^.Bins,',');
  end;
end;

class function Manifest.DB.Read(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID:QWord; Space:LongInt; out Item:Manifest.TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;

  procedure pushDomainSpace;
  begin
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
  end;

  procedure pushUserSpace;
  begin
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
  end;

begin
  Result := False;
  try
    Manifest.Empty(Item);
    Item.FolderID:=FolderID;
    Item.FileID:=FileID;
    Item.SpaceID:=Space;
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP, @Commands);
    case Space of
      Storage.AuraDisks.Kind.Domain  : pushDomainSpace();
      Storage.AuraDisks.Kind.User    : pushUserSpace();
    end;
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.FolderID, poAnd, oEqual, FolderID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.FileID, poAnd, oEqual, FileID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.SpaceID, poAnd, oEqual, Space, Commands);

    Result := Core.Database.SQL.Select(Task, @Commands,@cbReadManifest,@Item) and (Item.ID<>0);

    Item.BinCount:=System.Length(Item.Bins);

  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure SDP.Init(var Item:TItem);
begin
  With Item do begin
    ManifestID:=0;
    FolderID:=0;
    FileID:=0;
    FileModified:=0;
    SpaceID:=0;
    Modified:=0;
    SetLength(Data,0);

    sessionVersion:=0;
    SetLength(sessionOrigin,0);
    SetLength(sessionName,0);
    SetLength(sessionInformation,0);
    SetLength(sessionURI,0);
    SetLength(sessionEmail,0);
    SetLength(sessionPhone,0);
    SetLength(sessionConnection,0);
    SetLength(sessionBandwidth,0);
    SetLength(sessionEncryption,0);
    Core.Arrays.VarString.Empty(sessionAttributes);
    SetLength(timeSession,0);
    timeStart:=0;
    timeEnd:=0;
    SetLength(timeRepeat,0);
    SetLength(timeZone,0);
    Core.Arrays.VarString.Empty(mediaHeaders);
    SetLength(mediaTitle,0);
    SetLength(mediaConnection,0);
    SetLength(mediaInformation,0);
    SetLength(mediaEncryption,0);
    Core.Arrays.VarString.Empty(mediaAttributes);
  end;
end;

class procedure SDP.Done(var Item:TItem);
begin
  With Item do begin
    Finalize(Data);

    Finalize(sessionOrigin);
    Finalize(sessionName);
    Finalize(sessionInformation);
    Finalize(sessionURI);
    Finalize(sessionEmail);
    Finalize(sessionPhone);
    Finalize(sessionConnection);
    Finalize(sessionBandwidth);
    Finalize(sessionEncryption);
    Core.Arrays.VarString.Done(sessionAttributes);
    Finalize(timeSession);
    Finalize(timeRepeat);
    Finalize(timeZone);
    Core.Arrays.VarString.Done(mediaHeaders);
    Finalize(mediaTitle);
    Finalize(mediaConnection);
    Finalize(mediaInformation);
    Finalize(mediaEncryption);
    Core.Arrays.VarString.Done(mediaAttributes);
  end;
  Finalize(Item);
end;

class function SDP.toString(Var Item:TItem):Core.Strings.VarString;
begin

end;

class procedure SDP.Empty(var Item:TItem);
begin
  With Item do begin
    ManifestID:=0;
    FolderID:=0;
    FileID:=0;
    FileModified:=0;
    SpaceID:=0;
    Modified:=0;
    SetLength(Data,0);

    sessionVersion:=0;
    SetLength(sessionOrigin,0);
    SetLength(sessionName,0);
    SetLength(sessionInformation,0);
    SetLength(sessionURI,0);
    SetLength(sessionEmail,0);
    SetLength(sessionPhone,0);
    SetLength(sessionConnection,0);
    SetLength(sessionBandwidth,0);
    SetLength(sessionEncryption,0);
    Core.Arrays.VarString.Empty(sessionAttributes);
    SetLength(timeSession,0);
    timeStart:=0;
    timeEnd:=0;
    SetLength(timeRepeat,0);
    SetLength(timeZone,0);
    Core.Arrays.VarString.Empty(mediaHeaders);
    SetLength(mediaTitle,0);
    SetLength(mediaConnection,0);
    SetLength(mediaInformation,0);
    SetLength(mediaEncryption,0);
    Core.Arrays.VarString.Empty(mediaAttributes);
  end;
end;

procedure cbReadSDP(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with SDP do begin
    PItem(DataP)^.ManifestID:=Fields.FieldByName(DB.Keys.ManifestID).AsLargeInt;
    PItem(DataP)^.FolderID:=Fields.FieldByName(DB.Keys.FolderID).AsLargeInt;
    PItem(DataP)^.FileID:=Fields.FieldByName(DB.Keys.FileID).AsLargeInt;
    PItem(DataP)^.FileModified:=Fields.FieldByName(DB.Keys.FileModified).AsDateTime;
    PItem(DataP)^.SpaceID:=Fields.FieldByName(DB.Keys.SpaceID).AsLargeInt;
    PItem(DataP)^.Modified:=Fields.FieldByName(DB.Keys.Modified).AsFloat;
    Core.Arrays.Bytes.fromString(Fields.FieldByName(DB.Keys.Data).AsString,@PItem(DataP)^.Data);
  end;
end;

class function SDP.Read(Task:Core.Database.Types.TTask; ManifestID:QWord; out Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Core.Database.AddCommand(iCount, DB.TableP, @Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ManifestID, poNone, oEqual, ManifestID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ManifestID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.FolderID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.FileID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.FileModified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.SpaceID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Data,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionVersion,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionOrigin,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionName,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionInformation,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionURI,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionEmail,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionPhone,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionConnection,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionBandwidth,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionEncryption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.sessionAttributes,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.timeSession,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.timeStart,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.timeEnd,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.timeRepeat,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.timeZone,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.mediaHeaders,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.mediaTitle,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.mediaConnection,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.mediaInformation,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.mediaEncryption,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.mediaAttributes,poNone,oNone,Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadSDP, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function SDP.Create(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,FileID,ManifestID:QWord; Space:LongInt; FileModified:double; out Item:TItem):boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
  sSessionAttributes:Core.Strings.VarString;
  sMediaHeaders:Core.Strings.VarString;
  sMediaAttributes:Core.Strings.VarString;
begin
  Try
    iCount:=0;

    Item.FolderID:=FolderID;
    Item.FileID:=FileID;
    Item.FileModified:=FileModified;
    Item.Modified:=Core.Timer.dtUT;
    Item.ManifestID:=ManifestID;
    Item.SpaceID:=Space;
    Item.Data:=toString(Item);
    sSessionAttributes:=Core.Arrays.VarString.toString(Item.sessionAttributes);
    sMediaHeaders:=Core.Arrays.VarString.toString(Item.mediaHeaders);
    sMediaAttributes:=Core.Arrays.VarString.toString(Item.mediaAttributes);

    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.FolderID),poNone,oNone,FolderID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.FileID),poNone,oNone,FileID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ManifestID),poNone,oNone,ManifestID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Data),poNone,oNone,Item.Data,Commands);
    // Session Information
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionVersion),poNone,oNone,Item.sessionVersion,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionOrigin),poNone,oNone,Item.sessionOrigin,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionName),poNone,oNone,Item.sessionName,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionInformation),poNone,oNone,Item.sessionInformation,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionURI),poNone,oNone,Item.sessionURI,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionEmail),poNone,oNone,Item.sessionEmail,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionPhone),poNone,oNone,Item.sessionPhone,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionConnection),poNone,oNone,Item.sessionConnection,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionBandwidth),poNone,oNone,Item.sessionBandwidth,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionEncryption),poNone,oNone,Item.sessionEncryption,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.sessionAttributes),poNone,oNone,sSessionAttributes,Commands);
    // Session Time
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.timeSession),poNone,oNone,Item.timeSession,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.timeStart),poNone,oNone,Item.timeStart,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.timeEnd),poNone,oNone,Item.timeEnd,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.timeRepeat),poNone,oNone,Item.timeRepeat,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.timeZone),poNone,oNone,Item.timeZone,Commands);
    // Session Media
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.mediaHeaders),poNone,oNone,sMediaHeaders,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.mediaTitle),poNone,oNone,Item.mediaTitle,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.mediaConnection),poNone,oNone,Item.mediaConnection,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.mediaInformation),poNone,oNone,Item.mediaInformation,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.mediaEncryption),poNone,oNone,Item.mediaEncryption,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.mediaAttributes),poNone,oNone,sMediaAttributes,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Empty(Commands);
  End;
end;

class function SDP.Delete(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; ManifestID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ManifestID, poAnd, oEqual, ManifestID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class procedure ByteBin.Init(var Item:TItem);
begin
  With Item do begin
    ID:=0;
    ManifestID:=0;
    Serial:=0;
    Modified:=0;
    Size:=0;
    Core.Arrays.Bytes.Init(Data);
  end;
end;

class procedure ByteBin.Init(var Item:TBin);
begin
  With Item do begin
    fillWait:=true;
    sendWait:=false;
    System.FillByte(Data[0],ByteBin.binSize,0);
  end;
end;

class procedure ByteBin.Init(var Item:TBins);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Init(Item[iLcv]);
end;

class procedure ByteBin.Done(var Item:TItem);
begin
  Core.Arrays.Bytes.Done(Item.Data);
  Finalize(Item);
end;

class procedure ByteBin.Done(var Item:TBin);
begin
  Finalize(Item);
end;

class procedure ByteBin.Done(var Item:TBins);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

class procedure ByteBin.Empty(var Item:TItem);
begin
  with Item do begin
    ID:=0;
    ManifestID:=0;
    Serial:=0;
    Modified:=0;
    Size:=0;
    Core.Arrays.Bytes.Empty(Data);
  end;
end;

class procedure ByteBin.Empty(var Item:TBin);
begin
  With Item do begin
    fillWait:=true;
    sendWait:=false;
    System.FillByte(Data[0],binSize,0);
  end;
end;

class procedure ByteBin.Empty(var Item:TBins);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Empty(Item[iLcv]);
end;

class function ByteBin.createBin(Task:Core.Database.Types.TTask; DomainID,UserID,ManifestID:QWord; Serial:LongInt; Size:QWord; out ItemID:QWord):boolean;
var
  dtModified:double;
  iCount:LongInt;
  iReset,iInsertID:QWord;
  Commands:Core.Database.Types.Commands;
begin
  Try
    iCount:=0; iReset:=0; ItemID:=0; iInsertID:=Random(High(Integer)); dtModified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount,DB.TableP,@Commands);
    // Set Primary ID
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.InsertID),poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForCriteria,Integer(DB.IDs.InsertID),poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForPrimaryID,Integer(DB.IDs.ID),poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForResetInsertID,Integer(DB.IDs.InsertID),poNone,oNone,iReset,Commands);
    // Values
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.DomainID),poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.UserID),poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.ManifestID),poNone,oNone,ManifestID,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Serial),poNone,oNone,Serial,Commands);
    Core.Database.AddCommand(iCount,DB.TableP,useForInsert,Integer(DB.IDs.Modified),poNone,oNone,dtModified,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands) and (ItemID<>0);
  Finally
    Core.Database.Empty(Commands);
  End;
end;

class function ByteBin.deleteBin(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; ItemID:QWord):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Core.Database.AddCommand(iCount, DB.TableP,@Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);
    Result := Core.Database.SQL.Delete(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

procedure cbReadBin(CommandsP: Core.Database.Types.PCommands; Fields: TFields; const DataP: Pointer);
begin
  with ByteBin do begin
    PItem(DataP)^.ID:=Fields.FieldByName(DB.Keys.ID).AsLargeInt;
    PItem(DataP)^.ManifestID:=Fields.FieldByName(DB.Keys.ManifestID).AsLargeInt;
    PItem(DataP)^.Size:=Fields.FieldByName(DB.Keys.Size).AsLargeInt;
    PItem(DataP)^.Serial:=Fields.FieldByName(DB.Keys.Serial).AsInteger;
    PItem(DataP)^.Modified:=Fields.FieldByName(DB.Keys.Modified).AsFloat;
    Core.Arrays.Bytes.fromString(Fields.FieldByName(DB.Keys.Data).AsString,@PItem(DataP)^.Data);
  end;
end;

class function ByteBin.readBin(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; out Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Empty(Item);

    Core.Database.AddCommand(iCount, DB.TableP, @Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, ItemID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.ManifestID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Size,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Serial,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Modified,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForFields, DB.IDs.Data,poNone,oNone,Commands);

    Result := Core.Database.SQL.Select(Task, @Commands, @cbReadBin, @Item);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function ByteBin.writeBin(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; out Item:TItem):boolean;
var
  iCount:LongInt;
  Commands: Core.Database.Types.Commands;
begin
  Result := False;
  try
    iCount := 0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.AddCommand(iCount, DB.TableP, @Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.DomainID, poNone, oEqual, DomainID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.UserID, poAnd, oEqual, UserID, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForCriteria, DB.IDs.ID, poAnd, oEqual, Item.ID, Commands);

    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Modified, poNone, oNone, Item.Modified, Commands);
    Core.Database.AddCommand(iCount, DB.TableP, useForValues, DB.IDs.Data, poNone, oNone, Item.Data, Commands);

    Result := Core.Database.SQL.Update(Task, @Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

constructor TReader.Create();
begin
  ByteBin.Init(FItem);
  Inherited Create;
end;

destructor TReader.Destroy;
begin
  ByteBin.Done(FItem);
  Inherited Destroy;
end;

function  TReader.getNextBin(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var ManifestP:Manifest.PItem; var binIndex:LongInt):boolean;
begin
  Result:=false;
  If (ManifestP<>nil) and (binIndex<ManifestP^.BinCount) then begin
    ByteBin.Empty(FItem);
    Result:=ByteBin.readBin(Task,DomainID,UserID,ManifestP^.Bins[binIndex],FItem);
  end;
end;

initialization
  RegisterDB;
end.


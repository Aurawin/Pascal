unit RSR.AuDisks;


interface

uses
  Classes, SysUtils;


Type
  Version=class
  Type
    Peek=class
    type
      TData=record
        Version                  : Word;
        ContentLength            : QWord;
      end;
    const
      Length                     : Word = SizeOf(word)+SizeOf(QWord);
    end;
    One=class
    Type
      TAuDiskCommand=(audcNone,audcCreate,audcCopy,audcRead,audcWrite,audcDelete,audcExists);
      PAuDiskHeader=^TAuDiskHeader;
      TAuDiskHeader=record
        Version                  : Word;
        ContentLength            : QWord;
        NodeID                   : QWord;
        DomainID                 : QWord;
        UseID                    : QWord;
        FolderID                 : QWord;
        FileID                   : QWord;
        Kind                     : LongInt;
        Command                  : TAuDiskCommand;
        Code                     : DWORD;
      end;
      class procedure Initialize(var Item:TAuDiskHeader);
      class procedure Finalize(var Item:TAuDiskHeader);
      class procedure Empty(var Item:TAuDiskHeader);
    Const
      Length                     : DWORD = SizeOf(QWord)+6*SizeOf(QWord)+SizeOf(Integer)+SizeOf(TAuDiskCommand)+SizeOf(DWord);
      Stamp                      : Word  = 1;
    end;
  end;
Const
  AUDISK_NONE                    : DWORD = 0;
  AUDISK_FAIL                    : DWORD = 1;
  AUDISK_OK                      : DWORD = 2;
  AUDISK_ERROR_DISK_MISSING      : DWORD = 3;
  AUDISK_ERROR_FILE_MISSING      : DWORD = 4;
  AUDISK_ERROR_SERVER_DOWN       : DWORD = 5;

  function  Scan(Stream:TStream; Start:QWord; Out Vers:Word):Pointer;

implementation

class procedure Version.One.Initialize(var Item:TAuDiskHeader);
begin
  Item.Version:=Version.One.Stamp;
  Item.ContentLength:=0;
  Item.NodeID:=0;
  Item.DomainID:=0;
  Item.UseId:=0;
  Item.FolderID:=0;
  Item.FileID:=0;
  Item.Kind:=0;
  Item.Command:=audcNone;
  Item.Code:=AUDISK_NONE;
end;

class procedure Version.One.Finalize(var Item:TAuDiskHeader);
begin
  System.Finalize(Item);
end;

class procedure Version.One.Empty(var Item:TAuDiskHeader);
begin
  Item.Version:=Version.One.Stamp;
  Item.ContentLength:=0;
  Item.NodeID:=0;
  Item.DomainID:=0;
  Item.UseId:=0;
  Item.FolderID:=0;
  Item.FileID:=0;
  Item.Kind:=0;
  Item.Command:=audcNone;
  Item.Code:=AUDISK_NONE;
end;

function Scan(Stream:TStream; Start:QWord; Out Vers:Word):pointer;
var
  HdrV1:Version.One.PAuDiskHeader;
  Peek:Version.Peek.TData;
begin
  Result:=nil; Vers:=0;
  Stream.Position:=Start;
  if ((Stream.Position+Version.Peek.Length)<Stream.Size) then begin
    Stream.Read(Peek,Version.Peek.Length);
    Stream.Position:=Start;
    if (
      (Peek.Version=Version.One.Stamp) and
      ( (Start+Peek.ContentLength)<=Stream.Size)
    ) then begin
      New(HdrV1);
      Vers:=1;
      Version.One.Initialize(HdrV1^);
      Stream.Read(HdrV1^,Version.One.Length);
      Result:=HdrV1;
    end;
  end;
end;

end.


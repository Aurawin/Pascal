unit Encryption.Simple;

interface

uses
  SysUtils, Classes;

type
  TCrypt = (atEncryption,atDecryption);
  TEncryption = class
  private
    { Private declarations }

    FOutputString:string;
    FKeyString:string;
    FAction:TCrypt;

    procedure SetOutputString(input:string);
    procedure SetKeyString(input:string);

    Function  EncryptionEngine (Src:String; Key : String; Encrypt : Boolean):String;
  protected
    { Protected declarations }
  public
    { Public declarations }
    InputString:string;
    Validated  :Boolean;
    constructor Create; Virtual;
    Procedure Execute;
    procedure LoadFromStream(Stream:TStream);
  published
    { Published declarations }

    property Output: String read FOutputString write SetOutputString;
    property Key: String read FKeyString write SetKeyString;
    property Action: TCrypt read FAction write FAction default atEncryption;
  end;

  Function  EncryptString(Data:String):String;
  Function  DecryptString(Data:String):String;

implementation

constructor TEncryption.Create;
begin
   inherited Create;
   Action:=atEncryption;
end;

Procedure TEncryption.SetOutputString(input:string);
begin
   if input<> FOutputString then
     FOutputString:=input;
end;

Procedure TEncryption.SetKeyString(input:string);
begin
     if input <> FKeyString Then
        FKeyString:=input;
end;

procedure TEncryption.LoadFromStream(Stream:TStream);
begin
  SetLength(InputString,Stream.Size);
  Stream.ReadBuffer(InputString[1],Stream.Size);
end;

Const
  Key:String='Password';
  KeyLen=8;

Function  EncryptString(Data:String):String;
var
   KeyPos      : LongInt;
   offset      : LongInt;
   SrcPos      : LongInt;
   SrcAsc      : LongInt;
begin
  Result:='';
  If Data<>'' then begin
    Randomize;
    offset:=140;
    KeyPos:=0;
    Result:=format('%1.2x',[offset]);
    for SrcPos := 1 to Length(Data) do begin
         SrcAsc:=(Ord(Data[SrcPos]) + offset) MOD 255;
         if KeyPos < KeyLen then
            KeyPos:= KeyPos + 1
         else
            KeyPos:=1;
         SrcAsc :=SrcAsc xor  Ord(Key[KeyPos]);
         Result:=Result + format('%1.2x',[SrcAsc]);
         offset:=SrcAsc;
    end;
  end;
end;

Function  DecryptString(Data:String):String;

var
  KeyPos      : LongInt;
  offset      : LongInt;
  SrcPos      : LongInt;
  SrcAsc      : LongInt;
  TmpSrcAsc   : LongInt;
  iLength     : LongInt;
begin
  Result:=''; iLength:=Length(Data); KeyPos:=0;
  Try
    If (iLength>0) and ((iLength mod 2)=0) then begin
      offset:=StrToInt('$'+ copy(Data,1,2));
      SrcPos:=3;
      repeat
        SrcAsc:=StrToInt('$'+ copy(Data,SrcPos,2));
        if KeyPos < KeyLen Then
           KeyPos := KeyPos + 1
        else
           KeyPos := 1;
        TmpSrcAsc := SrcAsc xor Ord(Key[KeyPos]);
        if TmpSrcAsc <= offset then
           TmpSrcAsc := 255 + TmpSrcAsc - offset
        else
           TmpSrcAsc := TmpSrcAsc - offset;
        Result := Result + chr(TmpSrcAsc);
        offset:=srcAsc;
        SrcPos:=SrcPos + 2;
      until SrcPos >= Length(Data);
    end;
  Except
    On E:Exception do begin end;
  end;
end;

Function TEncryption.EncryptionEngine (Src:String; Key:String; Encrypt : Boolean):string;
var
   KeyLen      : LongInt;
   KeyPos      : LongInt;
   offset      : LongInt;
   dest        :string;
   SrcPos      : LongInt;
   SrcAsc      : LongInt;
   TmpSrcAsc   : LongInt;
begin
     KeyLen:=Length(Key);
     if KeyLen = 0 then key:='String Value';
     KeyPos:=0;
     if Encrypt then begin
          Randomize;
          offset:=140;//Random(Range);
          dest:=format('%1.2x',[offset]);
          for SrcPos := 1 to Length(Src) do begin
               SrcAsc:=(Ord(Src[SrcPos]) + offset) MOD 255;
               if KeyPos < KeyLen then
                  KeyPos:= KeyPos + 1
               else
                  KeyPos:=1;
               SrcAsc :=SrcAsc xor  Ord(Key[KeyPos]);
               dest:=dest + format('%1.2x',[SrcAsc]);
               offset:=SrcAsc;
          end;
     end else begin
        offset:=StrToInt('$'+ copy(src,1,2));
        SrcPos:=3;
        repeat
              SrcAsc:=StrToInt('$'+ copy(src,SrcPos,2));
              if KeyPos < KeyLen Then
                 KeyPos := KeyPos + 1
              else
                 KeyPos := 1;
              TmpSrcAsc := SrcAsc xor Ord(Key[KeyPos]);
              if TmpSrcAsc <= offset then
                 TmpSrcAsc := 255 + TmpSrcAsc - offset
              else
                 TmpSrcAsc := TmpSrcAsc - offset;
              dest := dest + chr(TmpSrcAsc);
              offset:=srcAsc;
              SrcPos:=SrcPos + 2;
        until SrcPos >= Length(Src);
     end;
     Result:=Dest;
end;


procedure TEncryption.Execute;
var
   EncryptionFlag:Boolean;
begin
  if length(InputString)=0 then begin
    FOutputString:='';
    Exit;
  end;
  if FAction = atEncryption then
    EncryptionFlag:=True
  else
    EncryptionFlag:=False;
  Try
    FOutputString:=EncryptionEngine(InputString,FKeyString,EncryptionFlag);
    Validated:=True;
  Except
    FOutPutString:='';
  end;
end;

end.

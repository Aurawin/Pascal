unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, uMPEG,uByteArray,Core.Streams;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Image1: TImage;
    ListView1: TListView;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { private declarations }
    FReader: TReader;
  private
    procedure OnTagFrame(ID3:TFrame; Item:TFrame; Stream:TStream; var Handled:boolean);
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FReader:=TReader.Create();
  FReader.OnTagFrame:=@OnTagFrame;
end;

procedure TForm1.OnTagFrame(ID3:TFrame; Item:TFrame; Stream:TStream; var Handled:Boolean);
var
  sValue:String;
  sName:String;
  sKind:String;
  li:TListItem;
  ms:TMemoryStream;
begin
  SetLength(sValue,0);
  sName:=Item.Header.Name;
  sKind:=uMPEG.FrameKind[Item.Kind];
  case Item.Kind of
    fEncodedBy                   : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fComments                    : sValue:=Concat( PCommentPayload(Item.Payload.Data)^.Description,' ',PCommentPayload(Item.Payload.Data)^.Text);
    fYear                        : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fISRC                        : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fRecordingDates              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fTrackNumber                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fLength                      : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fSize                        : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fEncodingParams              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fTitle                       : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fMediaType                   : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fOriginalArtist              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fOriginalfilename            : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fOriginalWriter              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fOriginalReleaseYear         : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fFileLicense                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fComposer                    : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fConductor                   : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fContentType                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fCopyrightMessage            : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fCompilation                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fLeadArtist                  : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fAccompaniment               : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPerformerRefinement         : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fModifiedBy                  : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fRadioStationOwner           : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPartOfaSet                  : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPublisher                   : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fContentGroupDescription     : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fTitleDescription            : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fSubTitleDescription         : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fTextWriter                  : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fURL                         : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fCommericalInfo              : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fCopyrightInfo               : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fOfficialFileWebpage         : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fOfficialArtistWebpage       : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fOfficialAudioSourceWebpage  : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fStationURL                  : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fPaymentURL                  : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fPublishersOfficialWebpage   : sValue:=PURLPayload(Item.Payload.Data)^.URL;
    fUserDefinedTextInformation  : sValue:=Concat(PUserURLPayload(Item.Payload.Data)^.Description,' ',PUserURLPayload(Item.Payload.Data)^.URL);
    fReleaseTime                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;

    fPodcast                     : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPodcastDescription          : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPodcastKeywords             : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPodcastFeed                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPodcastID                   : sValue:=PTextPayload(Item.Payload.Data)^.Information;

    fEncodingTime                : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fOriginalReleaseTime         : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fRecordingTime               : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fTaggingTime                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fInvolvedPeopleList          : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fMusicianCreditsList         : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fMood                        : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fProducedNotice              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fAlbumSortOrder              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPerformerSortOrder          : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fTitleSortOrder              : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fSetSubTitle                 : sValue:=PTextPayload(Item.Payload.Data)^.Information;
    fPicture                     : sValue:=Concat(
      PPicturePayload(Item.Payload.Data)^.ImageFormat,
      ' ',
      PPicturePayload(Item.Payload.Data)^.Description
    );
    fAttachedPicture             : sValue:=Concat(
      PPicturePayload(Item.Payload.Data)^.ImageFormat,
      ' ',
      PAttachedPicturePayload(Item.Payload.Data)^.Description
    );
  end;
  li:=ListView1.Items.Add;
  li.Caption:=sName;
  li.SubItems.Add(sKind);
  li.SubItems.Add(sValue);

  case Item.Kind of
    fPicture : begin
      ms:=TMemoryStream.Create();
      try
        Core.Streams.fromData(PPicturePayload(Item.Payload.Data)^.Data,ms);
        Image1.Picture.LoadFromStream(ms);
      finally
        ms.Free;
      end;
      //sValue:=; //
    end;
  end;

end;

procedure TForm1.Button1Click(Sender: TObject);
var
  fs:TFileStream;
begin
  ListView1.Items.Clear;
  FS:=TFileStream.Create(Edit1.Text,fmOpenRead);
  Try
    FReader.LoadFirst(fs);
  Finally
    FreeAndNil(FS);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
    Edit1.Text:=OpenDialog1.FileName;
  Button1Click(Sender);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FReader);
end;

end.


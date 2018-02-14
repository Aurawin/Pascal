program UserStorage;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  CThreads,BaseUnix,CWString,
  {$ENDIF}
  Classes,SysUtils,uKPList,uStringArray,uVarString,uStreams,dbmUserStorage,hHTTP,dbmContentTypes;

  procedure Test();
  const
    IF_SPAM='/home/atbrunner/Desktop/Spam/Extended Mime.txt';
    IF_MSG='/home/atbrunner/Desktop/Messages/Message (1).txt';
    IF_ATTACH='/home/atbrunner/Desktop/Attachment.txt';
    INPUT_FILE:TVarString=IF_ATTACH;
  var
    Body      : Storage.Items.IMAP.TBodyElement;
    Mail      : Storage.Items.SMTP.TRecvMessage;
    Summary   : Storage.Items.SMTP.TSummary;
    FBodyStream : TMemoryStream;

    Refactor  : TMemoryStream;
    iLcv      : integer;
    FStreamEnd: Cardinal;
    FStreamStart:Cardinal;
    iHdrCount : integer;
    FBodyCount : Cardinal;
    sHeaders  : TVarString;
    sXML      : TVarString;

    procedure OutputMime(const Section:TVarString; var Mime:Storage.Items.SMTP.TMime);
    var
      iLcv:integer;
      jLcv:integer;
    begin
      dbmUserStorage.Extract(Mime,FBodyStream,FStreamStart,FStreamEnd,FBodyCount);
      uStreams.toFile(FBodyStream,FStreamStart,FBodyCount,Concat('/home/atbrunner/Desktop/Mimes/BODY[',Section,']<',IntToStr(FBodyCount),'>.txt'));

      for iLcv:=0 to High(Mime.Mimes) do
         OutputMime(Concat(Section,'.',IntToStr(iLcv+1)),Mime.Mimes[iLcv]^);
    end;
  begin
    Refactor:=TMemoryStream.Create();
    Try
      FBodyStream:=TMemoryStream.Create();
      Try
        uStreams.fromFile(INPUT_FILE,FBodyStream);
        Storage.Items.SMTP.Init(Mail);
        Try
          Storage.Items.SMTP.Init(Summary);
          Try
            uStringArray.fromFile(Mail.Content,INPUT_FILE);
            SetLength(sHeaders,0);
            for iLcv:=0 to High(Mail.Content) do begin
              if Mail.Content[iLcv]<>'' then begin
                sHeaders:=Concat(sHeaders,Mail.Content[iLcv],#13#10);
              end else begin
                break;
              end;
            end;
            sHeaders:=UnwrapHeaders(sHeaders);
            uKPList.fromString(Mail.Headers,sHeaders,': ',#13#10,[kplsoClearList]);

            iHdrCount:=Length(Mail.Headers);
            Mail.ContentType:=getType(Mail.Headers,iHdrCount,Mail.CharSet,Mail.CharFormat,Mail.ContentName);

            Mimes(Mail,Summary,Refactor);

            OutputMime('1',Summary.Mime);

            dbmUserStorage.ParseBody(Summary,Body);

            sXML:=Storage.Items.SMTP.toXML(Summary);
            uVarString.toFile(sXML,'/home/atbrunner/Desktop/Summary.txt');
             sXML:=Storage.Items.SMTP.toXML(Summary.Mime);
            uVarString.toFile(sXML,'/home/atbrunner/Desktop/Mimes/Mimes.xml');
            sXML:=Storage.Items.IMAP.toString(Body,true);
            uVarString.toFile(sXML,'/home/atbrunner/Desktop/Body.txt');

          finally
            Storage.Items.SMTP.Done(Summary);
          end;
        Finally
          Storage.Items.SMTP.Done(Mail);
        end;
      finally
        FBodyStream.Free();
      end;
    Finally
      Refactor.Free();
    end;
  end;

begin
  DefaultSystemCodePage:=CP_UTF8;
  Test();
end.


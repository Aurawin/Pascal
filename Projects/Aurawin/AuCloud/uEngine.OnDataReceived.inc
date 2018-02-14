procedure   TAuSocketMan.OnDataReceived(RSRP:PRSR; var Handled:Boolean);
const
  iLcv:integer=1;
var
  Complete:boolean;
  iHdrCount:integer;
  ID:QWord;

  {$i uEngine.OnDataReceived.ProcessVDM.inc}
  {$i uEngine.OnDataReceived.ProcessVDMAccount.inc}
  {$i uEngine.OnDataReceived.ProcessSocial.inc}

  procedure ProcessResponse;
  begin
    EntryPoint:='uEngine.OnDataReceived.ProcessResponse';
    if SysUtils.SameText(FResponse.ContentType,RSR.HTTP.ctXML) then
      ParseXML(FResponse.Content,True);
    if SameText(FCoreObject,NS_CORE_OBJ_VDM) then begin
      ProcessVDM();
    end else if SameText(FCoreObject,NS_CORE_OBJ_SOCIAL) then begin
      ProcessSocial();
    end else if SameTExt(FCoreObject,NS_CORE_OBJ_VDM_ACCOUNT) then begin
      ProcessVDMAccount();
    end;
  end;

begin
  FRSRP:=RSRP;
  EntryPoint:='uEngine.OnDataReceived';
  if  ( RSR.Pos(RSRP^.RecvBuffer,HTTP_HEADER_SEP)>-1 ) then begin
    Complete:=FResponse.fromBuffer(RSRP^.RecvBuffer);
    if (Complete) then begin
      {$ifdef Buffer_Debug}
      Core.Logging.Native.WriteLogEntry(
        SYSTEM_LOG,
        EntryPoint,
        Core.Streams.toString(FResponse.Content)
      );
      {$endif}

      Handled:=True;
      iHdrCount:=System.Length(FResponse.Headers);
      FCoreObject:=Core.Arrays.KeyString.GetItemByKey(FResponse.Headers,fieldCoreObject,iHdrCount);
      FCoreCommand:=Core.Arrays.KeyString.GetItemByKey(FResponse.Headers,fieldCoreCommand,iHdrCount);
      FCoreCode:=Core.Arrays.KeyString.GetItemAsQword(FResponse.Headers,fieldCode,iHdrCount,CO_STATUS_FAIL);
      {$ifdef Core_Debug}
      Core.Logging.Native.WriteLogEntry(
        SYSTEM_LOG,
        'uEngine.OnDataReceived',
        Format(auLang.Table.Format.CoreError,[FCoreObject,FCoreCommand,FCoreCode])
      );
      {$endif}
      FreeAndNil(FXMLDocument);
      Try
        ProcessResponse();
      finally
        FreeAndNil(FXMLDocument);
      end;
    end;
    inc(iLcv);
  end else begin
    Handled:=True;
  end;
  if Handled then begin
    Handled:=(RSRP^.RecvBuffer.posWrite=0);
  end;
  FRSRP:=nil;
end;


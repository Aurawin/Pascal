unit uGoogle;

interface
uses
  Classes,

  Core.Strings,

  uProviders,


  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Streams,

  Storage,
  Storage.Main,
  Storage.SrchResults,

  SysUtils;

  function  IsContentValid(Stream:TStream):Boolean;
  function  ParseContent(InfoP:PProviderSocketInfo; Max:Integer; var Total:Integer; var sNextPage,sData:Core.Strings.VarString; OnResultParsed:TResultParsed; OnError:TOnProviderContent):Boolean;

implementation
uses StrUtils;

function  IsContentValid(Stream:TStream):Boolean;
begin
  Result:=(Stream.Size>100);
  If Result then
    Result:=Core.Streams.Pos(Stream,'</html>')<>-1;
end;


function  ParseContent(InfoP:PProviderSocketInfo; Max:Integer; var Total:Integer; var sNextPage,sData:Core.Strings.VarString; OnResultParsed:TResultParsed; OnError:TOnProviderContent):Boolean;
var
  iLen:Integer;
  sLink:Core.Strings.VarString;
  sCaption:Core.Strings.VarString;
  sDescription:Core.Strings.VarString;
  sResult:Core.Strings.VarString;
  sResults:Core.Strings.VarString;
  iPropertyOffset:Integer;
  iPropertyStart:Integer;
  iPropertyEnd:Integer;
  iResultLoc:Integer;
  iResultsStart:Integer;
  iResultsEnd:Integer;
  iResultStart:Integer;
  iResultEnd:Integer;
  iAlternate:Integer;
begin
  Result:=Length(sData)>0;
  If Result and (Total<Max) then begin
    SetLength(sNextPage,0);
    Try
      iPropertyStart:=System.Pos('<td nowrap class=b><a href=',sData);
      If iPropertyStart>0 then begin
        Inc(iPropertyStart,27);
        iPropertyEnd:=StrUtils.PosEx('>',sData,iPropertyStart);
        sNextPage:=System.Copy(sData,iPropertyStart,iPropertyEnd-iPropertyStart);
        sNextPage:=StringReplace(sNextPage,#34,'',[rfReplaceAll]);
      end else begin
        OnError(InfoP,sData);
      end;
      iResultsStart:=System.Pos('<h2 class=r>',sData);
      iResultsEnd:=Length(sData);
      sResults:=System.Copy(sData,iResultsStart,iResultsEnd-iResultsStart);
      iResultLoc:=1;
      Repeat
        iResultStart:=StrUtils.PosEx('<h2 class=r>',sResults,iResultLoc);
        If iResultStart>0 then begin
          iResultLoc:=iResultStart+12;
          iResultEnd:=StrUtils.PosEx('</table>',sResults,iResultLoc);
          iAlternate:=StrUtils.PosEx('<h2 class=r>',sResults,iResultLoc);
          If (iAlternate<>0) and (iAlternate<iResultEnd) then  // result inside htis thing... go back some...
            iResultEnd:=iAlternate;
          If iResultEnd>0 then begin
            iResultLoc:=iResultEnd;
            sResult:=System.Copy(sResults,iResultStart,iResultEnd-iResultStart);
            sResult:=SysUtils.StringReplace(sResult,'<br>','',[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'<b>','',[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'</b>','',[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'&#39;',#39,[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'&amp;',#39,[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'&middot;',#183,[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'&lt;',#60,[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'&gt;',#62,[rfReplaceAll]);
            sResult:=SysUtils.StringReplace(sResult,'<font size=-1>','',[]);
            sResult:=SysUtils.StringReplace(sResult,'&quot;',#39,[rfReplaceAll]);

            SetLength(sLink,0);
            SetLength(sCaption,0);
            SetLength(sDescription,0);

            iPropertyStart:=Core.Strings.Pos('<a ',sResult);
            if iPropertyStart>0 then
              iPropertyStart:=PosEx('href=',sResult,iPropertyStart);
            
            iPropertyStart:=iPropertyStart+5; // Reset the position to after "
            iPropertyEnd:=PosEx('>',sResult,iPropertyStart);
            iPropertyOffset:=iPropertyEnd+1;

            sLink:=System.Copy(sResult,iPropertyStart,iPropertyEnd-iPropertyStart);
            iLen:=Length(sLink);
            If (iLen>1) then begin
              //sLink:=ccUtils.UTF8ToStr(sLink);
              If (sLink[1]='"') then begin
                System.Delete(sLink,1,1);
                Dec(iLen);
              end;
              iLen:=Core.Strings.Pos('"',sLink);
              If iLen>0 then 
                System.SetLength(sLink,iLen-1);
            end;

            iPropertyStart:=iPropertyOffset;
            iPropertyEnd:=PosEx('</a>',sResult,iPropertyStart);
            iPropertyOffset:=iPropertyEnd+4;

            sCaption:=System.Copy(sResult,iPropertyStart,iPropertyEnd-iPropertyStart);
            //sCaption:=ccUtils.UTF8ToStr(sCaption);

            iPropertyStart:=PosEx('<td class="j">',sResult,iPropertyOffset);
            If iPropertyStart=0 then begin
              iPropertyStart:=PosEx('<td class="j hc">',sResult,iPropertyOffset);
              If iPropertyStart=0 then begin
                //Beep(0,0);
              end else begin
                iPropertyOffset:=iPropertyStart+17;
              end;
            end else begin
              iPropertyOffset:=iPropertyStart+14;
            end;
            iPropertyStart:=iPropertyOffset; // reset position to after >
            iPropertyEnd:=PosEx('<span class=a',sResult,iPropertyOffset);
            iPropertyOffset:=iPropertyEnd+4;
            sDescription:=System.Copy(sResult,iPropertyStart,iPropertyEnd-iPropertyStart);
            //sDescription:=ccUtils.UTF8ToStr(sDescription);
            Repeat
              iPropertyStart:=Core.Strings.Pos('<span class=f>',sDescription);
              If iPropertyStart>0 then begin
                iPropertyOffset:=iPropertyStart+14;
                iPropertyEnd:=PosEx(' - <a href=',sDescription,iPropertyOffset);
                If iPropertyEnd>0 then begin
                  iPropertyStart:=PosEx('</span>',sDescription,iPropertyOffset);
                  iPropertyOffset:=iPropertyStart+7;
                  iPropertyStart:=iPropertyOffset;
                  sDescription:=System.Copy(sDescription,iPropertyStart,iPropertyEnd-iPropertyStart);
                end else begin
                  iPropertyEnd:=PosEx('</span>',sDescription,iPropertyOffset);
                  If iPropertyEnd>0 then
                    sDescription:=System.Copy(sDescription,iPropertyEnd+7,(Length(sDescription)-(iPropertyEnd+7)));
                end;
              end;
            Until (iPropertyStart=0) or (Total>=Max);
            iPropertyEnd:=Length(sCaption);
            if iPropertyEnd<>0 then begin
              if iPropertyEnd>Store.Defaults.Max.Length.Title then
                SetLength(sCaption,Store.Defaults.Max.Length.Title);
              iPropertyEnd:=Length(sLink);
              if iPropertyEnd<>0 then begin
                if iPropertyEnd>Store.Defaults.Max.Length.Link then
                  SetLength(sLink,Store.Defaults.Max.Length.Link);
                iPropertyEnd:=Length(sDescription);
                if iPropertyEnd<>0 then begin
                  if iPropertyEnd>Store.Defaults.Max.Length.Description then
                    SetLength(sDescription,Store.Defaults.Max.Length.Description);
                  OnResultParsed(InfoP,sCaption,sLink,sDescription);
                end;
              end;
            end;
          end;
        end;
      until (iResultStart=0) or (Total>=Max);
    Finally
      SetLength(sLink,0);
      SetLength(sCaption,0);
      SetLength(sDescription,0);
      SetLength(sResult,0);
      SetLength(sResults,0);
      SetLength(sData,0);
    End;
  end;
end;

end.

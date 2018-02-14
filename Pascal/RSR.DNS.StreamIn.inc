var
  sServer:Core.Strings.VarString;
  ALcv,Lcv:LongInt;
  Q_Type:LongInt;
begin
   Reset();
   Result:=iSize>=System.SizeOf(TQHeader);
   Stream.Position:=iPosition;
   If Result then begin
     Stream.Read(Header,System.SizeOf(TQHeader));
     Inc(iPosition,System.SizeOf(TQHeader));

     Header.ID:=ntohs(Header.ID);
     Header.Options:=nTohs(Header.Options);
     Header.QDCount:=nTohs(Header.QDCount);
     Header.Ancount:=nTohs(Header.Ancount);
     Header.NsCount:=nTohs(Header.NsCount);
     Header.ARCount:=nTohs(Header.ARCount);
     //    QD COUNT
     Q_Type:=QT_NONE;
     Lcv:=Low(Questions);
     While (Lcv<Header.QDCount) and (Lcv<MAX_QUESTIONS) and not Error and (iPosition<iSize) do begin
       SetLength(Questions,Lcv+1);
       Questions[Lcv].Q_Name:=StreamInDNSDomain(Stream);
       iPosition:=Stream.Position;
       If (iPosition+4<=iSize) then begin
         Stream.Read(Questions[Lcv].Q_Type,2);
         Stream.Read(Questions[Lcv].Q_Class,2);
         Inc(iPosition,4);
         Questions[Lcv].Q_Type:=nTohs(Questions[Lcv].Q_Type);
         Questions[Lcv].Q_Class:=nTohs(Questions[Lcv].Q_Class);
         Q_Type:=Questions[Lcv].Q_Type;
       end else begin
         Error:=True;
         Result:=False;
       end;
       Inc(Lcv);
     end;
     //    AN COUNT
     ALcv:=0;
     Lcv:=Low(AN_Data);
     While (Lcv<Header.ANCount) and (not Error) and (iPosition<iSize) do begin
        SetLength(AN_Data,Lcv+1);
        StreamInResourceRecord(Error,Lcv,Header.ANCount,iPosition,Stream,AN_Data[Lcv]);
        If AN_Data[Lcv].RType=Q_Type then begin
           sServer:=RRDomainToDomain(AN_Data[Lcv].RType,AN_Data[Lcv].RData);
           If sServer<>'' then begin
             SetLength(Answers,ALcv+1);
             Answers[ALcv]:=sServer;
             Inc(ALcv);
           end;
        end;
        Inc(Lcv);
     end;
     //    NS COUNT
     Lcv:=Low(NS_Data);
     While (Lcv<Header.NSCount) and not Error and (iPosition<iSize) do begin
       SetLength(NS_Data,Lcv+1);
       StreamInResourceRecord(Error,Lcv,Header.NSCount,iPosition,Stream,NS_Data[Lcv]);
       Inc(Lcv);
     end;
     //    AR COUNT
     Lcv:=Low(AD_Data);
     While (Lcv<Header.ARCount) and not Error and (iPosition<iSize) do begin
       SetLength(AD_Data,Lcv+1);
       StreamInResourceRecord(Error,Lcv,Header.ARCount,iPosition,Stream,AD_Data[Lcv]);
       Inc(Lcv);
     end;
   end else
     Result:=False;


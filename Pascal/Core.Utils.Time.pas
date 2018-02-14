unit Core.Utils.Time;



interface

uses

  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.LargeInt,

  SysUtils;


Const
  TimeZoneCount = 35;
  PrintBiasOn=true;
  PrintBiasOff=false;

  Millisecond : double =1.1576048564165831e-08;
Type
  TPKType=(pktRFC822,pktInternal);
  TPKFormat=(pkfUTC,pkfGMT);
  TDateTimePrintOption=(dtpoShort,dtpoLong);
  TMonthArray=Array[1..12] of Core.Strings.VarString;
  TWeekArray=Array[1..7] of Core.Strings.VarString;
  TTimeZone=record
    Zone:Core.Strings.VarString;
    Bias:LongInt;
  end;
  TTimeZones=Array[1..TimeZoneCount] of TTimeZone;
const
  UTC_BIAS:Core.Strings.VarString='+0000';
  Kind_Label:Array[TPKFormat] of Core.Strings.VarString=('UTC','GMT');
  Day_Long:TWeekArray = ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday','Sunday');
  Day_Short:TWeekArray = ('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat','Sun');
  Month_Short:TMonthArray = ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  Month_Long:TMonthArray = ('January','February','March','April','May','June','July','August','September','October','November','December');

  ZoneBias:TTimeZones = (
       (Zone:'GMT'; Bias:0),
       (Zone:'UT';  Bias:0),
       (Zone:'EST'; Bias:-5*60),
       (Zone:'EDT'; Bias:-4*60),
       (Zone:'CST'; Bias:-6*60),
       (Zone:'CDT'; Bias:-5*60),
       (Zone:'MST'; Bias:-7*60),
       (Zone:'MDT'; Bias:-6*60),
       (Zone:'PST'; Bias:-8*60),
       (Zone:'PDT'; Bias:-7*60),
       (Zone:'Z';   Bias:0),
       (Zone:'A';   Bias:-1*60),
       (Zone:'B';   Bias:-2*60),
       (Zone:'C';   Bias:-3*60),
       (Zone:'D';   Bias:-4*60),
       (Zone:'E';   Bias:-5*60),
       (Zone:'F';   Bias:-6*60),
       (Zone:'G';   Bias:-7*60),
       (Zone:'H';   Bias:-8*60),
       (Zone:'I';   Bias:-9*60),
       (Zone:'K';   Bias:-10*60),
       (Zone:'L';   Bias:-11*60),
       (Zone:'M';   Bias:-12*60),
       (Zone:'N';   Bias:1*60),
       (Zone:'O';   Bias:2*60),
       (Zone:'P';   Bias:3*60),
       (Zone:'Q';   Bias:4*60),
       (Zone:'R';   Bias:3*60),
       (Zone:'S';   Bias:6*60),
       (Zone:'T';   Bias:3*60),
       (Zone:'U';   Bias:8*60),
       (Zone:'V';   Bias:3*60),
       (Zone:'W';   Bias:10*60),
       (Zone:'X';   Bias:3*60),
       (Zone:'Y';   Bias:12*60)
  );
   BIAS_STAMP_ON=true;
   BIAS_STAMP_OFF=false;
var
   TZIBias         : Core.Strings.VarString;
   BiasMinutes     : LongInt;
   ToUTCBias       : LongInt;
   FromUTCBias     : LongInt;
   TZI             : TTimeZone;
   TimeZoneTime    : Core.Strings.VarString;
   UTCTime         : Core.Strings.VarString;

   stLocalTime     : TSystemTime;
   stUniversal     : TSystemTime;


   function  GMTBias : LongInt;

   function  toString(Value:TDateTime; const Kind:TPKType=pktRFC822; const Format:TPKFormat=pkfUTC; const PrintBias:boolean=true):Core.Strings.VarString;
   function  DateTimeToString(Value:TDateTime):Core.Strings.VarString; overload;
   function  DateTimeToString(Value:TDateTime; Options:TDateTimePrintOption; Const IncludeBias:Boolean):Core.Strings.VarString; overload;
   function  DateToString(Value:TDateTime; Bias:LongInt):Core.Strings.VarString; overload;

   function  toGMTTime(const D: TDateTime): TDateTime; overload;
   function  toGMTTime(sDateTime: Core.Strings.VarString): TDateTime; overload;
   function  getMonth(sMonth:Core.Strings.VarString):Word;
   function  toDateTime(iHour,iMinute,iSecond,iMillisecond,iDay,iMonth,iYear:System.WORD; iBias:LongInt):TDateTime; overload;

   function  MonthfromString(sValue:Core.Strings.VarString; Var List:TMonthArray):System.Word;

   function  GMTBiasfromString(sValue:Core.Strings.VarString):LongInt;

   function  BiasFromString(sValue:Core.Strings.VarString; out Sign:LongInt; out iHour,iMinute:System.Word):System.boolean;
   Function  RFCDateTimeToDateTime(sDateTime:Core.Strings.VarString):TDateTime;
   Function  RFCDateToDateTime(sDate:Core.Strings.VarString):TDateTime;
   Function  DateTimetoRFCDateTime(dtValue:TDateTime):Core.Strings.VarString;

   procedure Empty(Var Item:TTimeZone); overload;
   procedure Done(Var Item:TTimeZone); overload;

   Function  DifferInMilliseconds(dt1,dt2:TDateTime):System.Int64;


   function  IndexOf(Value:Core.Strings.VarString; var Weeks:TMonthArray):LongInt; overload;


implementation

uses
  {$ifdef Windows}
    Windows,
  {$else}
    unix,baseunix,
  {$endif}
  Math,DateUtils;



procedure Empty(Var Item:TTimeZone);
begin
  Item.Bias:=0;
  SetLength(Item.Zone,0);
end;

procedure Done(var Item:TTimeZone);
begin
  Finalize(Item.Zone);
  Finalize(Item);
end;

function   DateTimeToRFCDateTime(dtValue: TDateTime): Core.Strings.VarString;
begin
//  DateUtils.DecodeDateTime(dtValue,wYear,wMonth,wDay,wHour,wMinute,wSecond,wMillisecond);
  Result:=Format('%s, %.2d %s %d %.2d:%.2d:%.2d',[Day_Short[DayOfWeek(dtValue)-1],DayOf(dtValue),Month_Short[MonthOf(dtValue)],YearOf(dtValue),HourOf(dtValue),MinuteOf(dtValue),SecondOf(dtValue)]);
end;

function  Different(One,Two:Double; Const Tolerance:Double=0.0000001):Boolean;
var
  Val:Double;
begin
  Val:=abs(One-Two);
  Result:=Val<Tolerance;
end;


function  MonthfromString(sValue:Core.Strings.VarString; Var List:TMonthArray):Word;
var
  iLcv:Word;
begin
  Result:=0; iLcv:=Low(List);
  While (iLcv<=High(List)) and (Result=0) do begin
    If SameText(sValue,List[iLcv]) then
      Result:=iLcv;
    Inc(iLcv);
  end;
end;

function  GMTBiasfromString(sValue:Core.Strings.VarString):LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  Result:=-1; iLcv:=Low(ZoneBias); iCount:=High(ZoneBias);
  While (iLcv<=iCount) and (Result=-1) do begin
    If SameText(ZoneBias[iLcv].Zone,sValue) then
      Result:=ZoneBias[iLcv].Bias;
    Inc(iLcv);
  end;
end;

function  BiasFromString(sValue:Core.Strings.VarString; out Sign:LongInt; out iHour,iMinute:System.Word):System.boolean;
const
  iSign:Array[System.Boolean] of LongInt=(1,-1);
var
  iLen:LongInt;
begin
  Result:=False; iHour:=0; iMinute:=0;
  iLen:=System.Length(sValue);
  if (iLen=5) then begin
    // "-0400"
    Sign:=iSign[sValue[1]='+'];
    iHour:=StrToIntDef(System.Copy(sValue,2,2),0);
    iMinute:=StrToIntDef(System.Copy(sValue,4,2),0);
    Result:=True;
  end else if (iLen=4) then begin
    // "0400"
    Sign:=1;
    iHour:=StrToIntDef(System.Copy(sValue,1,2),0);
    iMinute:=StrToIntDef(System.Copy(sValue,3,2),0);
    Result:=True;
  end;

end;

function  toDateTime(iHour,iMinute,iSecond,iMillisecond,iDay,iMonth,iYear:System.WORD; iBias:LongInt):TDateTime;
const
  Sign:Array[System.Boolean] of LongInt=(1,-1);
begin
  Result:=EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond)+( Sign[iBias<1] * EncodeTime(iBias,0,0,0)) ;
end;

function  toGMTTime(const D: TDateTime): TDateTime;
begin
  Result := D + (BiasMinutes / (24 * 60));
end;

function  toGMTTime(sDateTime: Core.Strings.VarString): TDateTime;
var
  saInput      : Core.Arrays.Types.VarString;
  iInputCount  : LongInt;

  function Push_RFC850:TDateTime;
  var
    saTime       : Core.Arrays.Types.VarString;
    saDate       : Core.Arrays.Types.VarString;
    iHour        : System.Word;
    iMinute      : System.Word;
    iSecond      : System.Word;
    iMillisecond : System.Word;
    iDay         : System.Word;
    iMonth       : System.Word;
    iYear        : System.Word;
    iSign        : LongInt;
    iBias        : LongInt;
    iBiasHour    : System.Word;
    iBiasMinute  : System.Word;
    iTimeLen     : LongInt;

    procedure EmptyLists;
    begin
      Empty(saDate);
      Empty(saTime);
    end;

  begin
    //                             +/-HHMM
    // "Monday, 01-Feb-10 12:44:77 -0400"
    // "Monday, 01-Feb-10 12:44:77 -0400"
    // "Monday, 01-Feb-10 12:44:77 GMT"
    Result:=0;
    Core.Arrays.VarString.fromString(saDate,saInput[1],'-');
    Try
      Core.Arrays.VarString.fromString(saTime,saInput[2],':');
      iTimeLen:=System.Length(saTime);
      If (System.Length(saDate)=3) and (iTimeLen>2) then begin
        iHour:=StrToIntDef(saTime[0],0);
        iMinute:=StrToIntDef(saTime[1],0);
        iSecond:=StrToIntDef(saTime[2],0);
        iMillisecond:=StrToIntDef(Core.Arrays.VarString.Parameter(saTime,3,poZeroBased),0);
        iDay:=StrToIntDef(saDate[0],0);
        iMonth:=MonthfromString(saDate[1],Month_Short);
        if iMonth=0 then
          MonthfromString(saDate[1],Month_Long);
        iYear:=StrToIntDef(saDate[2],0);
        iBias:=GMTBiasfromString(saInput[3]);
        if iBias<>-1 then
          Result:=(iBias/1440) + DateUtils.EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond)
        else begin
          BiasFromString(saInput[3],iSign,iBiasHour,iBiasMinute);
          Result:=DateUtils.EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond) + (iSign*EncodeTime(iBiasHour,iBiasMinute,0,0));
        end;
      end;
    finally
      EmptyLists;
    end;
  end;

  function Push_WC3:TDateTime;
  const
    Sign:Array[Boolean] of Integer=(1,-1);
  var
    iBiasHour     : Word;
    iBiasMinute   : Word;
    iHour         : Word;
    iMinute       : Word;
    iSecond       : Word;
    iMillisecond  : Word;
    iYear         : Word;
    iMonth        : Word;
    iDay          : Word;
    sSign         : Core.Strings.VarString;
    saDate        : Core.Arrays.Types.VarString;
    saTime        : Core.Arrays.Types.VarString;
    saBias        : Core.Arrays.Types.VarString;
    iLen          : LongInt;
    iSignLoc      : LongInt;

    procedure EmptyLists;
    begin
      Empty(saDate);
      Empty(saTime);
      Empty(saBias);
    end;

  begin
    //                       +/-[hh:mm]
    // "2010-02-01T12:44:47[.45]+01:00"
    Result:=0;
    Try
      iLen:=System.Length(saInput[0]);
      if (iLen>19) and (saInput[0][11]='t') then begin
        Core.Arrays.VarString.fromString(saInput,saInput[0],'t');
        iLen:=System.Length(saInput);
        if (iLen=2) then begin
          Core.Arrays.VarString.fromString(saDate,saInput[0],'-');
          iSignLoc:=System.Pos(saInput[1],'+');
          if iSignLoc=0 then
            iSignLoc:=System.Pos(saInput[1],'-');
          if (iSignLoc<>0) and (System.Length(saDate)=3) then begin
            sSign:=saInput[1][iSignLoc];
            Core.Arrays.VarString.fromString(saInput,saInput[1],sSign);
            iLen:=System.Length(saInput);
            if iLen=2 then begin
              Core.Arrays.VarString.fromString(saTime,saInput[0],':');
              Core.Arrays.VarString.fromString(saBias,saInput[1],':');
              if (System.Length(saBias)=2) and (System.Length(saTime)>2) then begin
                iYear:=StrToIntDef(saDate[0],0);
                iMonth:=StrToIntDef(saDate[1],0);
                iDay:=StrToIntDef(saDate[3],0);
                iHour:=StrToIntDef(saTime[0],0);
                iMinute:=StrToIntDef(saTime[1],0);
                iSecond:=StrToIntDef(saTime[2],0);
                iMillisecond:=StrToIntDef(Core.Arrays.VarString.Parameter(saTime,3,poZeroBased),0);
                iBiasHour:=StrToIntDef(saBias[0],0);
                iBiasMinute:=StrToIntDef(saBias[1],0);
                Result:=EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond)+( Sign[sSign='-'] * EncodeTime(iBiasHour,iBiasMinute,0,0));
              end;
            end;
          end;
        end;
      end;
    finally
      EmptyLists;
    end;
  end;

  function Push_UTC:TDateTime;
  var
    saTime        : Core.Arrays.Types.VarString;
    iYear         : Word;
    iMonth        : Word;
    iDay          : Word;
    iHour         : Word;
    iMinute       : Word;
    iSecond       : Word;
    iMillisecond  : Word;


    procedure EmptyLists;
    begin
      Empty(saTime);
    end;
  begin
    // "Mon Feb 1 12:44:47 2010" << Already UTC
    Result:=0;
    Try
      Core.Arrays.VarString.fromString(saTime,saInput[3],':');
      if (System.Length(saTime)>2) then begin
        iHour:=StrToIntDef(saTime[0],0);
        iMinute:=StrToIntDef(saTime[1],0);
        iSecond:=StrToIntDef(saTime[2],0);
        iMillisecond:=StrToIntDef(Parameter(saTime,3,poZeroBased),0);
        iYear:=StrToIntDef(saInput[4],0);
        iMonth:=MonthfromString(saInput[1],Month_Short);
        if iMonth=0 then
          MonthfromString(saInput[1],Month_Long);
        iDay:=StrToIntDef(saInput[2],0);
        Result:=DateUtils.EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond);
      end;
    finally
      EmptyLists;
    end;
  end;

  function Push_RFC822:TDateTime;
  var
    saTime       : Core.Arrays.Types.VarString;
    iHour        : System.Word;
    iMinute      : System.Word;
    iSecond      : System.Word;
    iMillisecond : System.Word;
    iDay         : System.Word;
    iMonth       : System.Word;
    iYear        : System.Word;
    iSign        : LongInt;
    iBias        : LongInt;
    iBiasHour    : System.Word;
    iBiasMinute  : System.Word;
    iTimeLen     : LongInt;

    procedure EmptyLists;
    begin
      Empty(saTime);
    end;

  begin
    ///                           +/-HHMM
    // "Mon, 01 Feb 2010 12:44:47 GMT"
    // "Mon, 01 Feb 2010 12:44:47 +0000"
    Result:=0;
    Core.Arrays.VarString.fromString(saTime,saInput[4],':');
    Try
      iTimeLen:=System.Length(saTime);
      If (iTimeLen>2) then begin
        iHour:=StrToIntDef(saTime[0],0);
        iMinute:=StrToIntDef(saTime[1],0);
        iSecond:=StrToIntDef(saTime[2],0);
        iMillisecond:=StrToIntDef(Core.Arrays.VarString.Parameter(saTime,3,poZeroBased),0);
        iDay:=StrToIntDef(saInput[1],0);
        iMonth:=MonthfromString(saInput[2],Month_Short);
        if iMonth=0 then
          MonthfromString(saInput[2],Month_Long);
        iYear:=StrToIntDef(saInput[3],0);
        iBias:=GMTBiasfromString(saInput[5]);
        if iBias<>-1 then
          Result:=(iBias/1440) + DateUtils.EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond)
        else begin
          BiasFromString(saInput[5],iSign,iBiasHour,iBiasMinute);
          Result:=DateUtils.EncodeDateTime(iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond) + (iSign*EncodeTime(iBiasHour,iBiasMinute,0,0));
        end;
      end;
    finally
      EmptyLists;
    end;
  end;
begin
  Result:=0;
  sDateTime:=SysUtils.Trim(sDateTime);
  Core.Arrays.VarString.fromString(saInput,sDateTime,#32,[soClearList,soMakeLowercase,soRemoveQuotes]);
  try
    iInputCount:=System.Length(saInput);
    Case iInputCount of
      1: Result:=Push_WC3;
      4: Result:=Push_RFC850;
      5: Result:=Push_UTC;
      6: Result:=Push_RFC822;
    end;
  finally
    Done(saInput);
  end;
end;

function getMonth(sMonth:Core.Strings.VarString):Word;
var
  iLcv:Word;
begin
  Result:=0;
  for iLcv:=1 to 12 do begin
    if Core.Strings.SameText(sMonth,Month_Short[iLcv]) then
      Result:=iLcv;
  end;
end;

Function DateTimeToString(Value:TDateTime):Core.Strings.VarString;
var
  iMonth:Word;
  iDayOfWeek:Word;
  iDay:Word;
  iYear:Word;
  iHour:Word;
  iMinute:Word;
  iSecond:Word;
  iMillisecond:Word;
begin
  iDayOfWeek:=SysUtils.DayOfWeek(Value);
  DateUtils.DecodeDateTime(Value,iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond);
  Result:=Concat(
    Day_Short[iDayOfWeek],', ',Format('%.2d',[iDay]),' ' ,Month_Short[iMonth],' ',Format('%.4d',[iYear]),' ',
    Format('%.2d',[iHour]),':',Format('%.2d',[iMinute]),':',Format('%.2d',[iSecond]),':',Format('%.2d',[iMillisecond]),
    ' ',TZIBias);
end;

function  toString(Value:TDateTime; const Kind:TPKType=pktRFC822; const Format:TPKFormat=pkfUTC; const PrintBias:boolean=true):Core.Strings.VarString;
var
  iMonth:Word;
  iDayOfWeek:Word;
  iDay:Word;
  iYear:Word;
  iHour:Word;
  iMinute:Word;
  iSecond:Word;
  iMillisecond:Word;

  procedure Push_RFC822;
  begin
    iDayOfWeek:=SysUtils.DayOfWeek(Value);
    DateUtils.DecodeDateTime(Value,iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond);
    Result:=Concat(
      Day_Short[iDayOfWeek],', ',
      SysUtils.Format('%.2d',[iDay]),' ' ,
      Month_Short[iMonth],' ',
      SysUtils.Format('%.4d',[iYear]),' ',
      SysUtils.Format('%.2d',[iHour]),':',
      SysUtils.Format('%.2d',[iMinute]),':',
      SysUtils.Format('%.2d',[iSecond]),':',
      SysUtils.Format('%.2d',[iMillisecond]),' ',
      Kind_Label[Format]
    );
  end;

  procedure Push_InternalDate;
  begin
    DateUtils.DecodeDateTime(Value,iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond);
    Result:=Concat(
      SysUtils.Format('%.2d',[iDay]),'-' ,
      Month_Short[iMonth],'-',
      SysUtils.Format('%.4d',[iYear]),' ',
      SysUtils.Format('%.2d',[iHour]),':',
      SysUtils.Format('%.2d',[iMinute]),':',
      SysUtils.Format('%.2d',[iSecond]),':',
      SysUtils.Format('%.3d',[iMillisecond])
    );
    if (PrintBias=true) then
      Result:=Concat(Result,' ',UTC_BIAS);
  end;

begin
  Case Kind of
    pktRFC822   : Push_RFC822();
    pktInternal : Push_InternalDate();
  end;

end;

function GMTBias : LongInt;
var
{$ifdef Windows}
  TZI : TTimeZoneInformation;
{$else}
  {$ifdef Unix}
    timeval: TTimeVal;
    Const TimeZone: PTimeZone =  nil;
  {$endif}
{$endif}

begin
  {$ifdef Windows}
  if GetTimeZoneInformation(TZI) = TIME_ZONE_ID_DAYLIGHT then
    Result := TZI.DaylightBias
  else
    Result := 0;
  Result := Result + TZI.Bias;
  {$else}
    {$ifdef Unix}
      fpGetTimeOfDay (@TimeVal, TimeZone);
      Result:=(TimeZone^.tz_minuteswest div 60);
    {$endif}
  {$endif}
end;


function   DateTimeToString(Value:TDateTime; Options:TDateTimePrintOption; Const IncludeBias:Boolean):Core.Strings.VarString;
var
  iMonth:Word;
  iDay:Word;
  iYear:Word;
  iHour:Word;
  iMinute:Word;
  iSecond:Word;
  iMillisecond:Word;
begin
  DateUtils.DecodeDateTime(Value,iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond);
  Case Options of
    dtpoShort    : Result:=Concat(
      Format('%.4d',[iYear]),'-',Format('%.2d',[iMonth]),'-',Format('%.2d',[iDay]),' ',
      Format('%.2d',[iHour]),':',Format('%.2d',[iMinute]),':',Format('%.2d',[iSecond])
    );
    dtpoLong    : Result:=Concat(
      Format('%.4d',[iYear]),'-',Format('%.2d',[iMonth]),'-',Format('%.2d',[iDay]),' ',
      Format('%.2d',[iHour]),':',Format('%.2d',[iMinute]),':',Format('%.2d',[iSecond]),':',Format('%.2d',[iMillisecond])
    );
  end;
  if IncludeBias then
    Result+=Concat(' ',TZIBias);
end;

function   DateToString(Value:TDateTime; Bias:LongInt):Core.Strings.VarString;
var
  iMonth:Word;
  iDay:Word;
  iYear:Word;
  iHour:Word;
  iMinute:Word;
  iSecond:Word;
  iMillisecond:Word;
begin
  Value:=DateUtils.IncMillisecond(Value,Bias*60000);
  DateUtils.DecodeDateTime(Value,iYear,iMonth,iDay,iHour,iMinute,iSecond,iMillisecond);
  Result:=Concat(Format('%.4d',[iYear]),'-',Format('%.2d',[iMonth]),'-',Format('%.2d',[iDay]));
end;

Function   RFCDateTimeToDateTime(sDateTime:Core.Strings.VarString):TDateTime;
var
  iLength:LongInt;
  saParameters:Core.Arrays.Types.VarString;
  saTime:Core.Arrays.Types.VarString;
  sMonth:Core.Strings.VarString;
  wYear,wMonth,wDay:Word;
  wHour,wMinute,wSecond,wMillisecond:Word;
begin
  Result:=0; iLength:=Length(sDateTime);
  If (iLength=26) and (sDateTime[11]='T') then begin
    // 2007-01-25T00:01:10-05:00
    Core.Arrays.VarString.fromString(saParameters,sDateTime,'T');
    Try
      If Length(saParameters)=2 then begin
        Core.Arrays.VarString.fromString(saTime,saParameters[0],'-');
        Try
          If Length(saTime)=3 then begin
            wYear:=StrToIntDef(saTime[0],0);
            wMonth:=StrToIntDef(saTime[1],1);
            wDay:=StrToIntDef(saTime[2],1);
            Core.Arrays.VarString.fromString(saParameters,saParameters[1],'-');
            If Length(saParameters)=2 then begin
              Core.Arrays.VarString.fromString(saTime,saParameters[0],':');
              If Length(saTime)=3 then begin
                wHour:=StrToIntDef(saTime[0],0);
                wMinute:=StrToIntDef(saTime[1],0);
                wSecond:=StrToIntDef(saTime[2],0);
                wMillisecond:=0;
                Try
                  If (wYear>0) and Core.Arrays.LargeInt.Range(wMonth,1,12) and Range(wDay,1,31) and Range(wHour,0,23) and Range(wMinute,0,59) then
                    Result:=DateUtils.EncodeDateTime(wYear,wMonth,wDay,wHour,wMinute,wSecond,wMillisecond);
                Except
                  On E:Exception do Result:=0;
                End;
              end;
            end;
          end;

        Finally
          SetLength(saTime,0);
        End;
      end;
    Finally
      SetLength(saParameters,0);
    End;
  end else begin
    Core.Arrays.VarString.fromString(saParameters,sDateTime,' ');
    Try
      Try
        If Length(saParameters)>4 then begin
          wDay:=1; wMonth:=1; wYear:=0; wHour:=0; wMinute:=0; wSecond:=0; wMillisecond:=0;

          // Define Timezone

          // Define Day
          wDay:=StrToIntDef(SysUtils.Trim(saParameters[1]),0);
          // Define Month
          sMonth:=SysUtils.Trim(saParameters[2]);
          wMonth:=getMonth(sMonth);
          wYear:=StrToIntDef(SysUtils.Trim(saParameters[3]),0);
          // Define Time
          Core.Arrays.VarString.fromString(saTime,saParameters[4],':');
          Try
            If Length(saTime)=3 then begin
              wHour:=StrToIntDef(saTime[0],0);
              wMinute:=StrToIntDef(saTime[1],0);
              wSecond:=StrToIntDef(saTime[2],0);
            End;
          Finally
            SetLength(saTime,0);
          end;
          Try
            If (wYear>0) and Range(wMonth,1,12) and Range(wDay,1,31) and Range(wHour,0,23) and Range(wMinute,0,59) then
              Result:=DateUtils.EncodeDateTime(wYear,wMonth,wDay,wHour,wMinute,wSecond,wMillisecond);
          Except
            On E:Exception do Result:=0;
          End;
        end else if Length(saParameters)=1 then begin
          Core.Arrays.VarString.fromString(saParameters,sDateTime,'-');
          if (Length(saParameters)=3) then begin
            // DAY-MONTH-YEAR
            wDay:=StrToIntDef(saParameters[0],0);
            wMonth:=getMonth(saParameters[1]);
            wYear:=StrToIntDef(saParameters[2],0);
            wHour:=0;
            wMinute:=0;
            wSecond:=0;
            wMillisecond:=0;
            If (wYear>0) and Range(wMonth,1,12) and Range(wDay,1,31) then
              Result:=DateUtils.EncodeDateTime(wYear,wMonth,wDay,wHour,wMinute,wSecond,wMillisecond);
          end;
        end;

      Except
        On E:Exception do Result:=0;
      End;
    Finally
      Finalize(saParameters,0);
    End;
  end;
end;

Function  RFCDateToDateTime(sDate:Core.Strings.VarString):TDateTime;
var
  saDate:Core.Arrays.Types.VarString;
  iYear:Word;
  iMonth:Word;
  iDay:Word;
  iElse:Word;
begin
  // 1-Feb-1994
  Result:=0;
  iYear:=1;
  iMonth:=1;
  iDay:=1;
  iElse:=0;
  Core.Arrays.VarString.fromString(saDate,sDate,'-',[soClearList]);
  if (Length(saDate)=3) then begin
    iYear:=StrToIntDef(saDate[2],1);
    iMonth:=IndexOf(saDate[1],Month_Short)+1;
    iDay:=StrToIntDef(saDate[0],1);
    Result:=DateUtils.EncodeDateTime(iYear,iMonth,iDay,iElse,iElse,iElse,iElse);
  end;
end;

function  IndexOf(Value:Core.Strings.VarString; var Weeks:TMonthArray):LongInt;
var
 iLcv:LongInt;
begin
  Result:=0;
  for iLcv:=0 to High(Weeks) do begin
    if SameText(Weeks[iLcv],Value) then begin
      Result:=iLcv;
      Exit;
    end;
  end;
end;

Function  DifferInMilliseconds(dt1,dt2:TDateTime):System.Int64;
var
  dtDiff:System.Double;
  Threshold:System.Double;
begin
  dtDiff:=dt1-dt2;
  Result:=Round(dtDiff/MilliSecond);
end;

end.


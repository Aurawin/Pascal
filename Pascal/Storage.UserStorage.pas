{
  unit Storage.UserStorage.pas

  Based on Two Tables

  UserStorage is where we virtualize files / folder storage for domains

  Users on Domains can store Files and Folders

  Storage Folder Table  : Stores Directory Structure
  Storage Item Table    : Stores Items with Ptrs back to Folders.

  DOES NOT Require Access Control Unit Because user is already authenticated.

  Copyright Aurawin LLC 2003-2014
  Written by: Andrew Thomas Brunner

  This code is protected under the Aurawin Public Release License
  http://www.aurawin.com/aprl.html
}

unit Storage.UserStorage;


interface
uses
  RSR.HTTP,

  Core.Timer,
  Core.Utils.Time,
  Core.Utils.Files,

  Core.Database,
  Core.Database.Types,
  Core.Database.SQL,
  Core.Database.Monitor,
  Core.Database.Monitor.Notify,
  Core.Database.Monitor.Types,

  Core.XML,
  Core.Strings,
  Core.Arrays,
  Core.Arrays.Types,
  Core.Arrays.Bytes,
  Core.Arrays.VarString,
  Core.Arrays.KeyString,
  Core.Arrays.Boolean,
  Core.Arrays.LargeWord,



  Encryption.Base64,

  Core.Streams,
  Core.Utils.Mail,

  Storage.UserAccounts,

  Multimedia.MPEG,

  Storage.Main,
  Storage.MatrixNodes,
  Storage.AuraDisks,
  Storage.Security,


  MD5,
  DOM,
  XMLRead,
  Classes,
  SysUtils;

Type
  Items=class
  type
    IMAP=class
    type
      XML=class
      const
        Records                : Core.Strings.VarString = 'recs';
      type
        Fields=class
        const
          Total                : Core.Strings.VarString = 'tol';
          Unread               : Core.Strings.VarString = 'urd';
          Recent               : Core.Strings.VarString = 'rnt';
          Read                 : Core.Strings.VarString = 'red';
          Deleted              : Core.Strings.VarString = 'del';
          Pages                : Core.Strings.VarString = 'pgs';
        end;
      end;
      Flags=class
      Const
        None                         = 0  shl  0;
        Seen                         = 1  shl  1;
        Answered                     = 1  shl  2;
        Flagged                      = 1  shl  3;
        Deleted                      = 1  shl  4;
        Draft                        = 1  shl  5;
        Recent                       = 1  shl  6;
        NoSelect                     = 1  shl  7;
        Pinned                       = 1  shl  8;
      end;
      TAddress=record
        Name                         : Core.Strings.VarString;
        ADL                          : Core.Strings.VarString;
        Mailbox                      : Core.Strings.VarString;
        Host                         : Core.Strings.VarString;
      end;
      TDisposition=record
        &Type                        : Byte;
        Name                         : Core.Strings.VarString;
      end;
      TRecordCount=record
        Total                        : DWord;
        Unread                       : DWord;
        Recent                       : DWord;
        Read                         : DWord;
        Deleted                      : DWord;
        Pages                        : DWord;
      end;
      PRecordCount=^TRecordCount;
      TAddresses=Array of TAddress;
      PBodyElement=^TBodyElement;
      TBodyElements=Array of PBodyElement;
      TBodyElement=record
        &Type                        : Core.Strings.VarString;
        SubType                      : Core.Strings.VarString;
        Parameters                   : Core.Arrays.Types.KeyStrings;
        ID                           : Core.Strings.VarString;
        Description                  : Core.Strings.VarString;
        Encoding                     : Core.Strings.VarString;
        Size                         : LongInt;
        Lines                        : LongInt;
        Disposition                  : TDisposition;
        SubElements                  : TBodyElements;
      end;
      TEnvelope=record
        // Fields In Streaming Order
        Date                         : Core.Strings.VarString;
        Subject                      : Core.Strings.VarString;
        From                         : TAddresses;
        Sender                       : TAddresses;
        ReplyTo                      : TAddresses;
        &To                          : TAddresses;
        CC                           : TAddresses;
        BCC                          : TAddresses;
        InReplyTo                    : Core.Strings.VarString;
        MessageId                    : Core.Strings.VarString;
      end;
      class procedure fromString(var Source:Core.Strings.VarString; var Item:TAddress); overload;
      class procedure fromString(var Source:Core.Strings.VarString; var Item:TAddresses); overload;
      class procedure fromString(var Source:Core.Arrays.Types.VarString; var Item:TAddresses); overload;

      class procedure Empty(var Item:TRecordCount); overload;
      class procedure Empty(var Item:TDisposition); overload;
      class procedure Empty(var Item:TBodyElement); overload;
      class procedure Empty(var Item:TBodyElements); overload;
      class procedure Empty(var Item:TAddress); overload;
      class procedure Empty(var Item:TAddresses); overload;
      class procedure Empty(var Item:TEnvelope); overload;
      class procedure Init(var Item:TRecordCount); overload;
      class procedure Init(var Item:TDisposition); overload;
      class procedure Init(var Item:TBodyElement); overload;
      class procedure Init(var Item:TBodyElements); overload;
      class procedure Init(var Item:TAddress); overload;
      class procedure Init(var Item:TAddresses); overload;
      class procedure Init(var Item:TEnvelope); overload;
      class procedure Done(var Item:TRecordCount); overload;
      class procedure Done(var Item:TDisposition); overload;
      class procedure Done(var Item:TBodyElement); overload;
      class procedure Done(var Item:TBodyElements); overload;
      class procedure Done(var Item:TAddresses); overload;
      class procedure Done(var Item:TAddress); overload;
      class procedure Done(var Item:TEnvelope); overload;

      class function  toString(State:LongInt):Core.Strings.VarString; overload;
      class function  toString(Stamp:Double):Core.Strings.VarString; overload;
      class function  toString(Stamp:QWord):Core.Strings.VarString; overload;
      class function  toString(var Item:TBodyElements):Core.Strings.VarString; overload;
      class function  toString(var Item:TBodyElement; const ExData:boolean=false):Core.Strings.VarString; overload;
      class function  toStringWithSubs(var Item:TBodyElement; const ExData:boolean=false):Core.Strings.VarString; overload;
      class function  toStringExtended(var Item:TBodyElement; const ExData:boolean=false):Core.Strings.VarString; overload;
      class function  toString(var Item:TAddress):Core.Strings.VarString; overload;
      class function  toString(var Item:TAddresses):Core.Strings.VarString; overload;
      class function  toString(var Item:TEnvelope; Refactor:TMemoryStream):Core.Strings.VarString; overload;

      class function  toXML(var Item:TRecordCount; Output:TMemoryStream; Header:Boolean):boolean; overload;

      class procedure Paginate(var Records:TRecordCount; ItemsPerPage:Word);

      class procedure Append(ItemP:PBodyElement; var List:TBodyElements); overload;
    end;
    SMTP=class
    const
      MaxLines                       = 2;
      MaxLineLength                  = 255;
      MaxCSSThreshold                = 30;  // % of css aggregate data
      MaxCSSDefinesThreshold         = 1;   // Minimum number of css definitions in a block
      MaxCommentThreshold            = 50;  // % of message block can be comment lines before it's determined to be spam
      MaxCSSFieldsToWordsThreshold   = 180; // % of css fields to number of words in block
      MaxMessageIdInBodyLength       = 8;   // characters threshold to trigger filter
      MaxMessageIdInBody             = 1;
      MaxAlternateMissingText        = 1.5; // % of plain text to html text
      MaxMissedRecipients            = 15;  // 15 recipients per transaction allowed 450 mailbox not present.
      RISK_1_CSSFieldsToWords        = 1;
      RISK_2_CSSFieldsToDATA         = 2;
      RISK_3_COMMENTS                = 3;
      RISK_4_MESSAGE_ID              = 4;
      RISK_5_CSS_DEF_MISMATCH        = 5;
      RISK_6_CSS_DEFINITIONS         = 6;
      RISK_7_ALT_MIME                = 7;
      RISK_8_ALT_MISSING_TEXT        = 8;
      RISK_9_MISSING_FROM            = 9;
      MSG_MESSAGE_ACCEPTED           : Core.Strings.VarString = '250 message was accepted for delivery.';
      MSG_INTERCEPTED                : Core.Strings.VarString = 'was intercepted by the Aurawin content security system.';
      MSG_RISK_1_CSSFieldsToWords    : Core.Strings.VarString = 'AU-RISK-1';
      MSG_RISK_2_CSSFieldsToDATA     : Core.Strings.VarString = 'AU-RISK-2';
      MSG_RISK_3_COMMENTS            : Core.Strings.VarString = 'AU-RISK-3';
      MSG_RISK_4_MESSAGE_ID          : Core.Strings.VarString = 'AU-RISK-4';
      MSG_RISK_5_CSS_DEF_MISMATCH    : Core.Strings.VarString = 'AU-RISK-5';
      MSG_RISK_6_CSS_DEFINITIONS     : Core.Strings.VarString = 'AU-RISK-6';
      MSG_RISK_7_ALT_MIME            : Core.Strings.VarString = 'AU-RISK-7';
      MSG_RISK_8_ALT_MISSING_TEXT    : Core.Strings.VarString = 'AU-RISK-8';
      MSG_RISK_9_MISSING_FROM        : Core.Strings.VarString = 'AU-RISK-9';

      BypassFilters            = true;
      EnforceFilters           = false;
      LineBreak                = #13#10;
      HeaderBreak              = #13#10#13#10;
      HeaderWrapEnd            = #13#10#32#32#32#32#32#32#32#32;
      EndOfMessage             = #13#10#46#13#10;
    type
      XML=class
      const
        Stanza                 : Core.Strings.VarString = 's';
        Stat                   : Core.Strings.VarString = 'stat';
        Relay                  : Core.Strings.VarString = 'rm';
      type
        Fields=class
        const
          Bound                : Core.Strings.VarString = 'bnd';
          Count                : Core.Strings.VarString = 'count';
          &To                  : Core.Strings.VarString = 'rcpt';
          Sender               : Core.Strings.VarString = 'sndr';
          CC                   : Core.Strings.VarString = 'cc';
          BCC                  : Core.Strings.VarString = 'bcc';
          ID                   : Core.Strings.VarString = 'id';
          UserID               : Core.Strings.VarString = 'uid';
          DomainID             : Core.Strings.VarString = 'did';
          FilterID             : Core.Strings.VarString = 'fltr';
          FileID               : Core.Strings.VarString = 'flid';
          FolderID             : Core.Strings.VarString = 'fid';
          InboxID              : Core.Strings.VarString = 'ibid';
          SpamID               : Core.Strings.VarString = 'spid';
          QueueID              : Core.Strings.VarString = 'qid';
          DNSIP                : Core.Strings.VarString = 'dnsip';
          iMXLcv               : Core.Strings.VarString = 'mxlcv';
          iIPLcv               : Core.Strings.VarString = 'iplcv';
          iTryMax              : Core.Strings.VarString = 'itmax';
          iTry                 : Core.Strings.VarString = 'itry';
          iTransitTry          : Core.Strings.VarString = 'ittry';
          Status               : Core.Strings.VarString = 'stts';
          State                : Core.Strings.VarString = 'stte';
          MXServer             : Core.Strings.VarString = 'smxs';
          MXServers            : Core.Strings.VarString = 'mxl';
          DNSQuery             : Core.Strings.VarString = 'dnsq';
          Response             : Core.Strings.VarString = 'srsp';
          Error                : Core.Strings.VarString = 'serr';
          SenderIP             : Core.Strings.VarString = 'sdrip';
          Helo                 : Core.Strings.VarString = 'shlo';
          Kind                 : Core.Strings.VarString = 'kind';
          From                 : Core.Strings.VarString = 'from';
          Read                 : Core.Strings.VarString = 'read';
          ReplyTo              : Core.Strings.VarString = 'rplt';
          InReplyTo            : Core.Strings.VarString = 'irpt';
          Replied              : Core.Strings.VarString = 'rep';
          RepliedAll           : Core.Strings.VarString = 'rep-all';
          Forwarded            : Core.Strings.VarString = 'fwdd';
          Boundary             : Core.Strings.VarString = 'bndy';
          Lines                : Core.Strings.VarString = 'lines';
          Flags                : Core.Strings.VarString = 'flags';
          Date                 : Core.Strings.VarString = 'date';
          Domain               : Core.Strings.VarString = 'domain';
          tzBias               : Core.Strings.VarString = 'tz-bias';
          Name                 : Core.Strings.VarString = 'name';
          Sent                 : Core.Strings.VarString = 'sent';
          Subject              : Core.Strings.VarString = 'sbj';
          Risk                 : Core.Strings.VarString = 'risk';
          Spam                 : Core.Strings.VarString = 'spam';
          BlackList            : Core.Strings.VarString = 'bl';
          WhiteList            : Core.Strings.VarString = 'wl';
          Size                 : Core.Strings.VarString = 'size';
          Rendered             : Core.Strings.VarString = 'rndrd';
          Group                : Core.Strings.VarString = 'grp';
          RemoteIP             : Core.Strings.VarString = 'rem-ip';
          RemoteDomain         : Core.Strings.VarString = 'rem-dn';
          RemoteFrom           : Core.Strings.VarString = 'rem-fm';
          User                 : Core.Strings.VarString = 'user';
          UTC                  : Core.Strings.VarString = 'utc';
          Pinned               : Core.Strings.VarString = 'pnnd';
          Mimes                : Core.Strings.VarString = 'mimes';
          Mime                 : Core.Strings.VarString = 'mime';
          MessageId            : Core.Strings.VarString = 'msgid';
          Attachments          : Core.Strings.VarString = 'atmts';
          Deliveries           : Core.Strings.VarString = 'dvs';
          Delivery             : Core.Strings.VarString = 'dvy';
          DeliveryCode         : Core.Strings.VarString = 'dc';
          DeliveryMessage      : Core.Strings.VarString = 'dm';
          DeliveryRead         : Core.Strings.VarString = 'dr';
          DeliveryDate         : Core.Strings.VarString = 'dd';
          DeliveryAddress      : Core.Strings.VarString = 'da';

          idxHeadersStart      : Core.Strings.VarString = 'idx-hrs-s';
          idxHeadersEnd        : Core.Strings.VarString = 'idx-hrs-e';
          idxContentStart      : Core.Strings.VarString = 'idx-cnt-s';
          idxContentEnd        : Core.Strings.VarString = 'idx-cnt-e';

          cntLast              : Core.Strings.VarString = 'cnt-last';
          cntAttachment        : Core.Strings.VarString = 'cnt-atch';
          cntEncoding          : Core.Strings.VarString = 'cnt-enc';
          cntType              : Core.Strings.VarString = 'cnt-type';
          cntCharFormat        : Core.Strings.VarString = 'cnt-char-fmt';
          cntCharSet           : Core.Strings.VarString = 'cnt-char-set';
          cntID                : Core.Strings.VarString = 'cnt-id';
          cntDisposition       : Core.Strings.VarString = 'cnt-disp';
          cntCreated           : Core.Strings.VarString = 'cnt-ctd';
          cntModified          : Core.Strings.VarString = 'cnt-mtd';
          cntRead              : Core.Strings.VarString = 'cnt-rad';
          cntSize              : Core.Strings.VarString = 'cnt-siz';
          cntBoundary          : Core.Strings.VarString = 'cnt-bndry';
          cntName              : Core.Strings.VarString = 'cnt-nme';
          cntDescription       : Core.Strings.VarString = 'cnt-dsc';
          cntReadableSize      : Core.Strings.VarString = 'cnt-rs';
        end;
      end;
      Defaults=class
      Const
        Read                   = true;
        Unread                 = false;
        Sent                   = true;
        Unsent                 = false;
        Inbound                = 0;
        Outbound               = 1;
        ContentText            = 0;
        ContentRich            = 1;
        ContentAlternative     = 2;
      end;
      CharSet=class
      const
        csNone                 = 0;  // Unknown

        csUTF_8                = 1; // Unicode
        csUTF_16               = 2; // Unicode
        csUTF_16BE             = 3;// Unicode
        csUTF_16LE             = 4;// Unicode

        csISO_8859_1           = 5;  // Western
        csISO_8859_2           = 6;  // Central European
        csISO_8859_3           = 7;  // South Eurpean
        csISO_8859_4           = 8;  // Baltic
        csISO_8859_5           = 9;  // Cyrillic
        csISO_8859_6           = 10;  // Arabic
        csISO_8859_7           = 11;  // Greek
        csISO_8859_8           = 12;  // Hebrew visual
        csISO_8859_8_1         = 13;  // Hebrew
        csISO_8859_9           = 14; // Turkish
        csISO_8859_10          = 15; // Nordic
        csISO_8859_11          = 16; // Thai
        csISO_8859_12          = 17; // Unknown
        csISO_8859_13          = 18; // Baltic / Polish
        csISO_8859_14          = 19; // Celtic
        csISO_8859_15          = 20; // Western with Euro sign and other rationalizations
        csISO_8859_16          = 21; // Romanian
        csISO_2022_KR          = 22; // Korean
        csISO_2022_JP          = 23; // Japanese
        csISO_2022_CN          = 24; // Chinese Simplified

        csISO_IR_111           = 25; // Cyrillic

        csWindows_874          = 26; // Thai
        csWindows_1250         = 27; // Central European
        csWindows_1251         = 28; // Cyrillic
        csWindows_1252         = 29; // Western
        csWindows_1253         = 30; // Greek
        csWindows_1254         = 31; // Turkish
        csWindows_1255         = 32; // Hebrew
        csWindows_1256         = 33; // Arabic
        csWindows_1257         = 34; // Baltic
        csWindows_1258         = 35; // Vietnamese


        csEUC_KR               = 36; // Korean
        csEUC_JP               = 37; // Japanese
        csEUC_TW               = 38; // Chinese Traditional

        csTIS_620              = 39; // Thai
        csUHC                  = 40; // Korean
        csJOHAB                = 41; // Korean
        csTCVN                 = 42; //Vietnamese;

        csVPS                  = 43;// Vietnamese
        csCP_866               = 44; // Cyrillic/Russian

        csARMSCII_8            = 45; // Armenian
        csUSASCII              = 46;
        csVISCII               = 47; // Vietnamese

        csHZ                   = 48; // Chinese Simplified
        csGBK                  = 49; // Chinese Simplified
        csBig5                 = 50; // Chinese Traditional
        csBig5_HKSCS           = 51; // Chinese Traditional

        csGB2312               = 52; // Chinese Simplified
        csGB18030              = 53; // Chinese Simplified

        csKO18_R               = 54; // Cyrillic/Russain
        csKO18_U               = 55; // Cyrillic/Ukrainian

        csIBM_850              = 56; // Western
        csIBM_852              = 57; // Central European
        csIBM_855              = 58 ; // Cyrillic
        csIBM_857              = 59; // Turkish
        csIBM_864              = 60; // Arabic
        csIBM_862              = 61; // Hebrew

        csMacCE                = 62; // Central European
        csMacRoman             = 63; // Western
        csMacRomanian          = 64; // Romanian
        csMacTurkish           = 65; // Turkish
        csMacIcelandic         = 66; // Icelandic
        csShift_JIS            = 67; // Japanese
        csMacCyrillic          = 68; // Cyrillic
        csMacCroatian          = 69; // Croatian
        csMacDevanagari        = 70; // Hindi
        csMacGurmukhi          = 71; // Gurmukhi
        csMacGujarati          = 72; // Gujarati

        Keys:Array[0..72] of Core.Strings.VarString=(
          'None',
          'UTF-8',
          'UTF-16',
          'UTF-16BE',
          'UTF-16LE',

          'ISO-8859-1',
          'ISO-8859-2',
          'ISO-8859-3',
          'ISO-8859-4',
          'ISO-8859-5',
          'ISO-8859-6',
          'ISO-8859-7',
          'ISO-8859-8',
          'ISO-8859-8_1',
          'ISO-8859-9',
          'ISO-8859-10',
          'ISO-8859-11',
          'ISO-8859-12',
          'ISO-8859-13',
          'ISO-8859-14',
          'ISO-8859-15',
          'ISO-8859-16',
          'ISO-2022-KR',
          'ISO-2022-JP',
          'ISO-2022-CN',

          'csISO_IR_111',

          'Windows-874',
          'Windows-1250',
          'Windows-1251',
          'Windows-1252',
          'Windows-1253',
          'Windows-1254',
          'Windows-1255',
          'Windows-1256',
          'Windows-1257',
          'Windows-1258',

          'EUC-KR',
          'EUC-JP',
          'EUC-TW',

          'TIS-620',
          'UHC',
          'JOHAB',
          'TCVN',

          'VPS',
          'CP-866',

          'ARMSCII-8',
          'US-ASCII',
          'VISCII',

          'HZ',
          'GBK',
          'Big5',
          'Big5_HKSCS',

          'GB2312',
          'GB18030',

          'KO18-R',
          'KO18-U',

          'IBM-850',
          'IBM-852',
          'IBM-855',
          'IBM-857',
          'IBM-864',
          'IBM-862',

          'MacCE',
          'MacRoman',
          'MacRomanian',
          'MacTurkish',
          'MacIcelandic',
          'Shift-JIS',
          'MacCyrillic',
          'MacCroatian',
          'MacDevanagari',
          'MacGurmukhi',
          'MacGujarati'
        );
      public
        class function IndexOf(Value:Core.Strings.VarString):Byte;
        //class function Parse(Value:Core.Strings.VarString):Byte;
      end;

      Encoding=class
      const
        emNone                 = 0;
        emBase64               = 1;
        em7Bit                 = 2;
        em8Bit                 = 3;
        emQuotedPrintable      = 4;
        emBinary               = 5;
        Value                  : Array[0..5] of Core.Strings.VarString=(
          'none',
          'base64',
          '7bit',
          '8bit',
          'quoted-printable',
          'binary'
        );
      Type
        class procedure Prepare(var Data:Core.Strings.VarString);
      end;
      Content=class
      const
        ctUnknown              = 0;
        ctTextPlain            = 1;
        ctTextHTML             = 2;
        ctMultiAlternative     = 3;
        ctMultiMixed           = 4;
        ctMultiRelated         = 5;
        ctMultiDigest          = 6;
        ctMultiMessage         = 7;
        ctMultiSigned          = 8;
        ctMultiEncrypted       = 9;
        ctApplication          = 10;
        ctImage                = 11;
        ctPGPSignature         = 12;
        MainType : Array[0..12] of Core.Strings.VarString=(
          'unknown',           // 0
          'text',              // 1
          'text',              // 2
          'multipart',         // 3
          'multipart',         // 4
          'multipart',         // 5
          'multipart',         // 6
          'multipart',         // 7
          'multipart',         // 8
          'multipart',         // 9
          'application',       // 10
          'image',             // 11
          'application'        // 12
        );
        SubType : Array[0..12] of Core.Strings.VarString=(
          'unknown',           // 0
          'plain',             // 1
          'html',              // 2
          'alternative',       // 3
          'mixed',             // 4
          'related',           // 5
          'digest',            // 6
          'message',           // 7
          'signed',            // 8
          'encrypted',         // 9
          'binary',            // 10
          'binary',            // 11
          'pgp-signature'      // 12
        );
      Type
        Format=Class
        const
          None                 = 0;
          Fixed                = 1;
          Flowed               = 2;
          Value : Array[0..2] of Core.Strings.VarString=(
            'none',           // 0
            'fixed',          // 1
            'flowed'          // 2
          );
        end;
        Disposition=class
        const
          None                 = 0;
          Inline               = 1;
          Attachment           = 2;
          Value : Array[0..2] of Core.Strings.VarString=(
            'none',            // 0
            'inline',          // 1
            'attachment'       // 2
          );
        end;
        Multiparts=class
        const
          Alternative          = 0;
          Mixed                = 1;
          Related              = 2;
          Digest               = 3;
          Message              = 4;
          Signed               = 5;
          Encrypted            = 6;

          Value : Array[0..6] of Core.Strings.VarString=(
            'alternative',
            'mixed',
            'related',
            'digest',
            'message',
            'signed',
            'encrypted'
          );

        end;

      public
        //class function getType(Var Headers:Core.Arrays.Types.KeyString; var Boundary:Core.Strings.VarString; iHdrCount:LongInt; out Charset:Byte):Byte; overload;
        //class function getType(Var Headers:Core.Arrays.Types.KeyString; iHdrCount:LongInt; out CharSet:Byte):Byte; overload;
      end;
      PMIME=^TMIME;
      PMIMES=^TMIMES;
      TMIMES=array of PMIME;
      TMIME=record
        idxHeadersStart        : LongInt;
        idxHeadersEnd          : LongInt;
        idxContentStart        : LongInt;
        idxContentEnd          : LongInt;
        iReadableSize          : LongInt;
        cntLast                : Boolean;
        cntEncoding            : Byte;
        cntType                : Byte;
        cntCharSet             : Byte;
        cntCharFormat          : Byte;
        cntID                  : Core.Strings.VarString;
        cntDisposition         : Byte;
        cntCreated             : Double;
        cntModified            : Double;
        cntRead                : Double;
        cntSize                : QWord;
        cntBoundary            : Core.Strings.VarString;
        cntName                : Core.Strings.VarString;
        cntDescription         : Core.Strings.VarString;
        Mimes                  : TMimes;
      end;
      TDelivery=record
        Read                   : boolean;
        Code                   : LongInt;
        Date                   : Double;
        Message                : Core.Strings.VarString;
        Address                : Core.Strings.VarString;
      end;
      PDelivery=^TDelivery;
      TDeliveries=array of PDelivery;

      PRecvMessage=^TRecvMessage;
      PStats=^TStats;
      TStats=record
        Count                         : QWord;
        Size                          : QWord;
      end;
      PRelayMail=^TRelayMail;
      TRecvMessage=record
        ErrorPushed            : Boolean;
        MTALoopNotPushed       : Boolean;
        Spam                   : Boolean;
        WhiteListed            : Boolean;
        BlackListed            : Boolean;
        ContentType            : Byte;
        Encoding               : Byte;
        CharSet                : Byte;
        CharFormat             : Byte;
        ErrorCount             : LongInt;
        State                  : LongInt;
        UAP                    : Storage.UserAccounts.Items.PItem;

        RecipientMissCount     : Word;
        FilterID               : QWord;

        NodeID                 : QWord;
        FileID                 : QWord;
        FolderID               : QWord;

        AuthMD5                : Boolean;
        Nonce                  : Core.Strings.VarString;
        BlackListDNS           : Core.Strings.VarString;
        WhiteListDNS           : Core.Strings.VarString;
        Subject                : Core.Strings.VarString;
        SenderDomain           : Core.Strings.VarString;
        MailFrom               : Core.Strings.VarString;
        MessageId              : Core.Strings.VarString;
        Exchanger              : Core.Strings.VarString;
        SenderIP               : Core.Strings.VarString;
        ContentName            : Core.Strings.VarString;
        FForward               : Core.Strings.VarString;
        From                   : Core.Strings.VarString;
        CMD                    : Core.Strings.VarString;
        Param1                 : Core.Strings.VarString;
        Param2                 : Core.Strings.VarString;
        Recipients             : Core.Arrays.Types.KeyStrings;
        RelayRecipients        : Core.Arrays.Types.VarString;
        Headers                : Core.Arrays.Types.KeyStrings;
        Content                : Core.Arrays.Types.VarString;
      end;
      PSummary=^TSummary;
      TSummary=record
        // Required Fields
        ID                     : QWord;
        Kind                   : LongInt;
        // Custom Fields
        Sender                 : Core.Strings.VarString;
        &To                    : Core.Strings.VarString;
        CC                     : Core.Strings.VarString;
        BCC                    : Core.Strings.VarString;
        From                   : Core.Strings.VarString;
        Subject                : Core.Strings.VarString;
        Domain                 : Core.Strings.VarString;
        User                   : Core.Strings.VarString;
        Group                  : Core.Strings.VarString;
        RemoteIP               : Core.Strings.VarString;
        RemoteDomain           : Core.Strings.VarString;
        RemoteFrom             : Core.Strings.VarString;
        Exchanger              : Core.Strings.VarString;
        Lines                  : Core.Strings.VarString;
        Flags                  : Core.Strings.VarString;
        ReplyTo                : Core.Strings.VarString;
        InReplyTo              : Core.Strings.VarString;
        MessageId              : Core.Strings.VarString;
        FilterID               : QWord;
        UTC                    : Double;
        Date                   : Double;
        tzBias                 : LongInt;
        Bound                  : Byte;
        cntType                : Byte;
        Risk                   : Byte;
        Read                   : Boolean;
        Replied                : Boolean;
        RepliedAll             : Boolean;
        Forwarded              : Boolean;
        Sent                   : Boolean;
        Spam                   : Boolean;
        BlackListed            : Boolean;
        WhiteListed            : Boolean;
        Rendered               : Boolean;
        Pinned                 : Boolean;
        Attachments            : Boolean;
        Mime                   : TMime;
        Deliveries             : TDeliveries;
      end;
      TRelayMail=record
        UserID                 : QWord;
        DomainID               : QWord;
        FileID                 : QWord;
        FolderID               : QWord;
        InboxID                : QWord;
        SpamID                 : QWord;
        QueueID                : QWord;
        DNSIP                  : QWord;
        iMXLcv                 : Byte;
        iIPLcv                 : Byte;
        iTryMax                : Byte;
        iTry                   : Byte;
        iTransitTry            : Byte;
        Status                 : Word;
        State                  : LongInt;
        Sent                   : Boolean;
        Date                   : TDateTime;
        DNSQuery               : Core.Strings.VarString;
        Response               : Core.Strings.VarString;
        MXServer               : Core.Strings.VarString;
        Domain                 : Core.Strings.VarString;
        Error                  : Core.Strings.VarString;
        Helo                   : Core.Strings.VarString;
        From                   : Core.Strings.VarString;
        &To                    : Core.Strings.VarString;
        Subject                : Core.Strings.VarString;
        SenderIP               : Core.Strings.VarString;
        Data                   : Core.Strings.VarString;    // Runtime must pull from Aura Disk
        Disk                   : Storage.MatrixNodes.Node.Item;   // Runtime
        MXServers              : Core.Arrays.Types.VarString;  // Runtime Available MX Servers of a Domain.
        IPS                    : Core.Arrays.Types.VarString;  // Runtime Available IPs of Particular MX Server.
      end;
      class function  fromXML(xDoc:TXMLDocument; var Item:TSummary):boolean; overload;
      class function  fromXML(var Item:TMimes; xNode:TDOMNode):boolean; overload;
      class function  fromXML(var Item:TMime; xNode:TDOMNode):boolean;  overload;
      class function  fromXML(var Item:TDeliveries; xNode:TDOMNode):boolean; overload;
      class function  fromXML(var Item:TDelivery; xNode:TDOMNode):boolean;  overload;
      class function  fromXML(xDoc:TXMLDocument; var Item:TMime):boolean; overload;
      class function  fromXML(var Item:TRelayMail; xNode:TDOMNode):boolean;  overload;
      class function  fromXML(xDoc:TXMLDocument; var Item:TRelayMail):boolean; overload;


      class function  toXML(var Item:TRelayMail; Header:boolean):Core.Strings.VarString; overload;
      class function  toXML(var Item:TSummary):Core.Strings.VarString; overload;
      class function  toXML(var Item:TSummary; Output:TMemoryStream):boolean; overload;
      class function  toXML(var Item:TMimes; Output:TMemoryStream):boolean; overload;
      class function  toXML(var Item:TMime):Core.Strings.VarString; overload;
      class function  toXML(var Item:TMimes):Core.Strings.VarString; overload;
      class function  toXML(var Item:TStats; Output:TMemoryStream):boolean; overload;
      class function  toXML(var Item:TDelivery):Core.Strings.VarString; overload;
      class function  toXML(var Item:TDeliveries):Core.Strings.VarString; overload;

      class function  getMessageID(var sItem:Core.Strings.VarString):Core.Strings.VarString;
      class procedure offsetMimes(var Summary:TSummary; Count:LongInt);
      class function  HasAttachments(var Summary:TSummary):Boolean;
      class function  getMime(var Parent:TMime; cntType:Byte):PMime;
      class function  getFlags(var Summary:Items.SMTP.TSummary; Flags:LongInt):LongInt;
      class function  Write(
        Task:Core.Database.Types.TTask;
        var DomainID,UserID:QWord;
        var Summary:TSummary;
        Flags:Core.Database.Types.Integer
      ):Boolean; overload;
      class function  Write(
        Task:Core.Database.Types.TTask;
        var DomainID,UserID:QWord;
        var Summary:TSummary;
        var Name:Core.Strings.VarString;
        Flags:Core.Database.Types.Integer
      ):Boolean; overload;
      class procedure Stamp(
        var Summary:TSummary;
        var Headers:Core.Arrays.Types.KeyStrings;
        var Data:Core.Arrays.Types.VarString;
        Refactor:TStream
      );
      class procedure Update(
        var Summary:TSummary;
        Const aFilterID:QWord;
        Const aRead,aSent,aSpam,aBlackListed,aWhiteListed:Boolean
      ); overload;
      class procedure Update(
        var Summary:TSummary;
        var Headers:Core.Arrays.Types.KeyStrings;
        aDomain,aUser,aSenderIP,aSenderDomain,aSenderFrom,aExchanger:Core.Strings.VarString;
        Const aBound,aContentType:Byte
      ); overload;
      class procedure SecurityCheck(
        var DropContent:Storage.Security.Filter.Items;
        var DropPhrases:Storage.Security.Filter.Items;
        var DropDomains:Storage.Security.Filter.Items;
        var FilterID:QWord;
        var Risk:Byte;
        var Filtered:Boolean;
        const Force:Boolean;
        var Summary:TSummary;
        var Headers:Core.Arrays.Types.KeyStrings;
        var Data:Core.Arrays.Types.VarString;
        Refactor:TStream
      );
      class function  Write(
        Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item;
        SpamID,UserID,DomainID:QWord;
        var Summary:TSummary;
        var Data:Core.Arrays.Types.VarString;
        Flags:LongInt;
        Refactor:TStream;
        IgnoreFilters : Boolean;
        var FolderID:QWord;
        out FileID:QWord
      ):Boolean;
      class function  ExtractSenderDomain(Var Value:Core.Strings.VarString):Core.Strings.VarString;
      //class function  UnwrapHeaders(Var Value:Core.Strings.VarString):Core.Strings.VarString;
      //class function  GetParameter(var Data : Core.Strings.VarString; Param:Core.Strings.VarString) : Core.Strings.VarString;
      class function  Stats(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Item:TStats):Boolean;
      class procedure Init(var Item:TMimes); overload;
      class procedure Init(var Item:TMime); overload;
      class procedure Done(Var Item:TMimes); overload;
      class procedure Done(Var Item:TMime); overload;
      class procedure Empty(var Item:TMimes); overload;
      class procedure Empty(var Item:TMime); overload;
      class procedure Copy(var Source,Destination:TMime); overload;
      class procedure Copy(var Source,Destination:TMimes); overload;
      class procedure Append(var Item:PMime; var Items:TMimes); overload;

      class procedure Append(var Item:PDelivery; var Items:TDeliveries); overload;
      class procedure Append(var Item:TDelivery; var Items:TDeliveries); overload;

      class function  Append(Task:Core.Database.Types.TTask; Parser:TDOMParser; var ItemP:PDelivery; DomainID,UserID,FileID:QWord):boolean; overload;
      class procedure Copy(var Source,Destination:TDelivery); overload;
      class procedure Copy(var Source,Destination:TDeliveries); overload;
      class procedure Init(var Item:TDeliveries); overload;
      class procedure Init(var Item:TDelivery); overload;
      class procedure Done(Var Item:TDeliveries); overload;
      class procedure Done(Var Item:TDelivery); overload;
      class procedure Empty(var Item:TDeliveries); overload;
      class procedure Empty(var Item:TDelivery); overload;

      class procedure Init(var Item:TSummary); overload;
      class procedure Empty(var Item:TSummary);overload;
      class procedure Done(var Item:TSummary);overload;
      class procedure Copy(Var Source,Destination:TSummary); overload;

      class procedure Init(Var Item:TRecvMessage); overload;
      class procedure Empty(Var Item:TRecvMessage); overload;
      class procedure Done(var Item:TRecvMessage); overload;
      class procedure Copy(Var Source,Destination:TRecvMessage); overload;

      class procedure Copy(Var Source,Destination:TRelayMail); overload;
      class procedure Init(var Item:TRelayMail); overload;
      class procedure Empty(Var Item:TRelayMail); overload;
      class procedure Done(var Item:TRelayMail); overload;
    end;
  end;
  Kind=class
  const
    // WARNING... APPEND TO THIS LIST//
    BIN                        = 0;
    SMTP                       = 1;
    XMPP                       = 2;
    Image                      = 3;
    Music                      = 4;
    Video                      = 5;
    CalcSheet                  = 6;
    Document                   = 7;
    Text                       = 8;
    Presentation               = 9;
    PlayList                   = 10;
    PDF                        = 11;
    IMAP                       = 12;
    // WARNING... APPEND TO THIS LIST//
  end;
  IDs=class
  const
    Inbox                      = 0;
  end;
  Files=class
  type
    XML=class
    type
      Stanzas=class
      const
        Items                  = 'files';
        Item                   = 'file';
      end;
    const
      ID                       = 'id';
      FolderID                 = 'fid';
      Created                  = 'ctd';
      Modified                 = 'mtd';
      Allocated                = 'atd';
      Kind                     = 'kd';
      Flags                    = 'fs';
      Name                     = 'n';
      Size                     = 'z';
      Digest                   = 'd';
      Summary                  = 's';
      Data                     = 'data';
    end;
  type
    PItem=^TItem;
    TItems=Array of PItem;
    PItems=^TItems;
    TItem=record
      ID                       : Core.Database.Types.LargeWord;
      FolderID                 : Core.Database.Types.LargeWord;
      Size                     : Core.Database.Types.LargeWord;
      Kind                     : Core.Database.Types.Integer;
      Flags                    : Core.Database.Types.Integer;
      Created                  : Core.Database.Types.DateTime;
      Modified                 : Core.Database.Types.DateTime;
      Allocated                : Core.Database.Types.DateTime;
      Digest                   : Core.Database.Types.MD5Digest;
      Name                     : Core.Database.Types.VarString;
      Summary                  : Core.Database.Types.VarString;
      Manifest                 : Pointer;       // Manifest Item (used at runtime)
      Valid                    : Boolean;       // Runtime;
    end;

    TFilesEvent=procedure(var Items:TItems) of object;
    TFileEvent=procedure(var Item:TItem) of object;
    DB=class
    Type

      Keys=class
      const
        ID                         : Core.Database.Types.VarString = 'ITMID';
        InsertID                   : Core.Database.Types.VarString = 'ITMIID';
        DomainID                   : Core.Database.Types.VarString = 'ITMDID';
        UserID                     : Core.Database.Types.VarString = 'ITMUID';
        FolderID                   : Core.Database.Types.VarString = 'ITMFID';
        Created                    : Core.Database.Types.VarString = 'ITMDTC';
        Modified                   : Core.Database.Types.VarString = 'ITMDTM';
        Allocated                  : Core.Database.Types.VarString = 'ITMALC';
        Digest                     : Core.Database.Types.VarString = 'ITMMD5';
        Kind                       : Core.Database.Types.VarString = 'ITMKND';
        Flags                      : Core.Database.Types.VarString = 'ITMFGS';
        Path                       : Core.Database.Types.VarString = 'ITMPTH';
        Name                       : Core.Database.Types.VarString = 'ITMNID';
        Summary                    : Core.Database.Types.VarString = 'ITMSMY';
        Size                       : Core.Database.Types.VarString = 'ITMSZE';
        InFolders                  : Core.Database.Types.VarString = 'ITMFID';
      end;
      IDs=class
      const
        ID                         : Core.Database.Types.Integer = 0;
        InsertID                   : Core.Database.Types.Integer = 1;
        DomainID                   : Core.Database.Types.Integer = 2;
        UserID                     : Core.Database.Types.Integer = 3;
        FolderID                   : Core.Database.Types.Integer = 4;
        Created                    : Core.Database.Types.Integer = 5;
        Modified                   : Core.Database.Types.Integer = 6;
        Allocated                  : Core.Database.Types.Integer = 7;
        Digest                     : Core.Database.Types.Integer = 8;
        Kind                       : Core.Database.Types.Integer = 9;
        Flags                      : Core.Database.Types.Integer = 10;
        Size                       : Core.Database.Types.Integer = 11;
        Name                       : Core.Database.Types.Integer = 12;
        Summary                    : Core.Database.Types.Integer = 13;
        InFolders                  : Core.Database.Types.Integer = 14;
      end;
    const
      TableP: Core.Database.Types.PTable = nil;
      MonitorP: Core.Database.Monitor.Types.PItem = nil;
      Startup: Core.Database.Types.TableIni = (
        AutoCreate           : True;
        AutoCommit           : True;
        Group                : 'Domains/Users/Storage';
        Name                 : 'Files';
        Value                : 'scs_ufls';
        Hint                 : 'Storage of files for domain users';
        PrimaryKeyP          : @Keys.ID;
        );
      Fields: array [0..14] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.FolderID; KeyP: @Keys.FolderID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Created; KeyP: @Keys.Created; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Modified; KeyP: @Keys.Modified; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Allocated; KeyP: @Keys.Allocated; DataType: dftDouble; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Digest; KeyP: @Keys.Digest; DataType: dftMD5Digest; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Kind; KeyP: @Keys.Kind; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.Flags;  KeyP: @Keys.Flags; DataType: dftInteger; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Size;  KeyP: @Keys.Size; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;),
        (IDP: @IDs.Name;  KeyP: @Keys.Name; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Summary; KeyP: @Keys.Summary; DataType: dftMemo; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.InFolders; KeyP: @Keys.InFolders; DataType: dftQWordArray; AutoCreate: False; Verified: True; Precision: 1024*1024*4; Flags: cfNone; )
      );
      class function  Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID,ItemID:QWord; Name:Core.Strings.VarString; dtCreated:Double=0; iKind:LongInt=0; iFlags:LongInt=0): Boolean; overload;
      class function  Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID,ItemID:QWord; var Name,Data:Core.Strings.VarString): Boolean; overload;

      class function  SetDigest(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Size:QWord; var Digest:TMD5Digest): Boolean;
      class function  SetCreatedStamp(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Stamp:Double): Boolean;
      class function  SetModifiedStamp(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Stamp:Double): Boolean;
      class function  SetAllocatedStamp(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Stamp:Double): Boolean;
      class function  SetFlags(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Flags:LongInt): Boolean;
      class function  GetFlags(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; out Flags:LongInt): Boolean;
      class function  SetSummary(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Summary:Core.Strings.VarString; Flags:LongInt): Boolean;
      class function  GetSummary(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Value:Core.Strings.VarString): Boolean;
      class function  Add(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID:QWord; Var Item:TItem; Stream:TStream):Boolean; overload;

      class function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID,ItemID:QWord): Boolean;
      class function  Clear(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID:QWord): Boolean;

      class function  Extension(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; var Value:Core.Strings.VarString):Boolean;
      class function  Fill(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,ItemID:QWord; Var Item:TItem; var Data:TFileStream):Boolean; overload;
      class function  Fill(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; Var Item:TItem):Boolean; overload;
      class function  Fill(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Value:Core.Strings.VarString; Var Item:TItem):Boolean; overload;
      class function  Force(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID:QWord; iKind:LongInt; dtCreated:double; Name:Core.Strings.VarString; Var Item:TItem; var Data:TFileStream):Boolean;
      class function  Exists(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; Var Item:TItem):Boolean; overload;
      class function  Exists(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Value:Core.Strings.VarString):Boolean; overload;
      class function  Count(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Records:Items.IMAP.TRecordCount):Boolean; overload;

      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Items:TItems; SortField:Byte=0):Boolean; overload;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Items:Core.Arrays.Types.LargeWord; SortField:Byte=0):Boolean; overload;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Kind:LongInt;  Var Items:Core.Arrays.Types.LargeWord; SortField:Byte=0):Boolean; overload;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TItems; SortField:Byte=0):Boolean; overload;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TItems; var Pattern:Core.Arrays.Types.VarString; SortField:Byte=0):Boolean; overload;
      class function  ListAll(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Items:TItems):Boolean; overload;

      class function  Write(Task:Core.Database.Types.TTask; Var DomainID,UserID:QWord; var Item:TItem; const UpdateStamp:Boolean):Boolean;
      class function  Refresh(Task:Core.Database.Types.TTask; Var DomainID,UserID:QWord; var Item:TItem):Boolean; overload;
      class function  Refresh(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Items:TItems; SortField:Byte=0):Boolean; overload;
      class function  Rename(Task:Core.Database.Types.TTask; Var DomainID,UserID:QWord; var Item:TItem):Boolean;
      class function  Move(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,ItemID,FolderID,NewFolderID:QWord): Boolean;
      class function  Copy(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,ItemID, NewItemID:QWord): Boolean; overload;
      class function  Copy(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,ItemID, NewFolderID,NewItemID:QWord): Boolean; overload;
      class function  Data(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID:QWord; var Item:TItem; var Stream:TFileStream):Boolean; overload;
      class function  Data(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID:QWord; var Item:TItem; var Mime:Items.SMTP.TMime; Refactor:TMemoryStream; Output:TStream):boolean; overload;
    end;
  const
    Allocate_Creating          = 1;
    Allocate_Waiting           = 2;
    Allocate_Writing           = 3;
    Allocate_Base              = 4;
    Allocate_Window            = 5; // Minutes

    class function  toXML(var Item:TItem; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean; overload;
    class function  toXML(var Item:TItem; Data:TStream; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean; overload;
    class function  toXML(var Item:TItems; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean; overload;
    class function  toXML(var Item:TItems; Data:TStream; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean; overload;

    class function  fromXML(xDoc:TXMLDocument; var Item:TItem):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TItems):boolean; overload;

    class procedure fromString(var Digest:TMD5Digest; Value:Core.Strings.VarString);
    class function  Force(var Item:Titem; Var Items:TItems):LongInt; overload;

    class function  IndexOf(ID:QWord; Var Items:TItems):LongInt; overload;
    class function  IndexOf(ItemP:PItem; Var Items:TItems):LongInt; overload;
    class function  IndexOf(FolderID:QWord; Name:Core.Strings.VarString; Var Items:TItems):LongInt; overload;

    class function  Get(ID:QWord; Var Items:TItems):PItem; overload;
    class function  Get(FolderID:QWord; Name:Core.Strings.VarString; Var Items:TItems):PItem; overload;
    class function  Get(Name:Core.Strings.VarString; Var Items:TItems):PItem; overload;

    class procedure Invalidate(var Items:TItems);

    class function  Create(Var Items:TItems):PItem; overload;


    class function  Add(Name:Core.Strings.VarString; FolderID,Size:QWord; Kind:LongInt; var Digest:TMD5Digest; Var Items:TItems):PItem; overload;
    class function  Add(var Item:TItem; var Items:TItems):LongInt; overload;

    class function  Data(var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID,ItemID:QWord; var Stream:TFileStream):Boolean; overload;

    class procedure Pack(var Items:TItems);
    class procedure Clear(var Items:TItems);
    class procedure Purge(var Items:TItems);

    class procedure Paginate(var Items,Refactor:TItems; ItemsPerPage,Page:LongInt);

    class procedure Copy(Var Source,Destination:TMD5Digest); overload;
    class procedure Copy(Var Source,Destination:TItem); overload;
    class procedure Copy(Var Source,Destination:TItems); overload;

    class function  Compare(var Val1,Val2:TMD5Digest):boolean;
    class procedure Empty(Var Item:TItem); overload;
    class procedure Empty(Var Items:TItems); overload;
    class procedure Done(Var Item:TItem); overload;
    class procedure Done(Var Items:TItems); overload;
    class procedure Init(var Item:TItem); overload;
    class procedure Init(var Item:TItems); overload;
  end;
  Folders=class
  type
    Defaults=class
    Type
       Mail=class
       const
         Inbox                   : Core.Strings.VarString = 'Inbox';
         Outbox                  : Core.Strings.VarString = 'Outbox';
         Sent                    : Core.Strings.VarString = 'Sent';
         Draft                   : Core.Strings.VarString = 'Draft';
         Drafts                  : Core.Strings.VarString = 'Drafts';
         Archive                 : Core.Strings.VarString = 'Archive';
         Trash                   : Core.Strings.VarString = 'Trash';
         Spam                    : Core.Strings.VarString = 'Spam';
         Junk                    : Core.Strings.VarString = 'Junk';
       end;
       Home=class
       const
         Documents               : Core.Strings.VarString = 'Documents';
         Mail                    : Core.Strings.VarString = 'Mail';
         Music                   : Core.Strings.VarString = 'Music';
         Pictures                : Core.Strings.VarString = 'Pictures';
         Videos                  : Core.Strings.VarString = 'Videos';
         Trash                   : Core.Strings.VarString = 'Trash';
         Devices                 : Core.Strings.VarString = 'Devices';
         Social                  : Core.Strings.VarString = 'Social';
         MyNetworks              : Core.Strings.VarString = 'My Networks';
         OtherNetworks           : Core.Strings.VarString = 'Other Networks';
       end;
    end;
    XML=class
    type
      Stanzas=class
      const
        Items                  = 'folders';
        Item                   = 'folder';
      end;
    const
      ID                       = 'id';
      Path                     = 'path';
    end;
  type
    FolderVerifiyFind=(fvkTrash,fvkInbox,fvkOutbox,fvkSentBox,fvkSpambox,fvkTrashbox,fvkArchiveBox);
    PFolder=^TFolder;
    TFolders=Array of PFolder;
    PFolders=^TFolders;
    TFolder=record
      ID                         : QWord;
      Verified                   : boolean;
      Path                       : Core.Strings.VarString;
      Files                      : Files.TItems; // runtime; don't stream
      Inspected                  : Double;
      Manifest                   : Pointer;
    end;
    TFoldersEvent=procedure(var Folders:TFolders) of object;
    TFolderEvent=procedure(var Folders:TFolder) of object;
    DB=class
    Type
      Keys=class
      const
        ID                       : Core.Database.Types.VarString = 'ITMID';
        InsertID                 : Core.Database.Types.VarString = 'ITMIID';
        DomainID                 : Core.Database.Types.VarString = 'ITMDID';
        UserID                   : Core.Database.Types.VarString = 'ITMUID';
        Path                     : Core.Database.Types.VarString = 'ITMPTH';
      end;
      IDs=class
      const
        ID                       : Core.Database.Types.Integer = 0;
        InsertID                 : Core.Database.Types.Integer = 1;
        DomainID                 : Core.Database.Types.Integer = 2;
        UserID                   : Core.Database.Types.Integer = 3;
        Path                     : Core.Database.Types.Integer = 4;
      end;
    const
      TableP                     : Core.Database.Types.PTable = nil;
      MonitorP                   : Core.Database.Monitor.Types.PItem = nil;
      Startup                    : Core.Database.Types.TableIni = (
        AutoCreate               : True;
        AutoCommit               : True;
        Group                    : 'Domains/Users/Storage';
        Name                     : 'Folders';
        Value                    : 'scs_uflds';
        Hint                     : 'Storage of folders structures for domain users';
        PrimaryKeyP              : @Keys.ID;
      );
      Fields: array [0..4] of Core.Database.Types.Field = (
        (IDP: @IDs.ID;  KeyP: @Keys.ID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNotNull or cfPrimaryKey or cfIdentity;),
        (IDP: @IDs.InsertID; KeyP: @Keys.InsertID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.DomainID; KeyP: @Keys.DomainID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  ),
        (IDP: @IDs.UserID; KeyP: @Keys.UserID; DataType: dftQWord; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone; ),
        (IDP: @IDs.Path; KeyP: @Keys.Path; DataType: dftString; AutoCreate: True; Verified: False; Precision: 0; Flags: cfNone;  )
      );

      class function  getPath(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord):Core.Strings.VarString;
      class function  Create(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var ItemID:QWord; Path:Core.Strings.VarString): Boolean;
      class function  Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,ItemID:QWord): Boolean;
      class function  ID(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Path:Core.Strings.VarString; out ItemID:QWord):boolean;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Items:TFolders): Boolean; overload;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Path:Core.Strings.VarString; var Items:TFolders): Boolean; overload;
      class function  List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Path:Core.Strings.VarString; var Items:Core.Arrays.Types.LargeWord): Boolean; overload;

      class function  Rename(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Item:TFolder): Boolean;
      class function  Rename(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Items:TFolders): Boolean; overload;

      class function  Refresh(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Path:Core.Strings.VarString; var Items:TFolders): Boolean; overload;

      class function  Force(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Path:Core.Strings.VarString; out ItemID:QWord): Boolean; overload;
      class function  Verify(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Path:Core.Strings.VarString; Kind:FolderVerifiyFind; out ItemID:QWord): Boolean; overload;
      class function  Verify(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord): Boolean; overload;

      class function  CreateDefaults(Task:Core.Database.Types.TTask; var DomainID,UserID,Trash,Inbox,SpamBox,OutBox,SentBox,ArchiveBox,TrashBox,Devices:QWord): Boolean;
    end;
  const
    FREE_FILES                 = true;
    CLEAR_FILES                = false;

    class function  toXML(var Item:TFolder; Output:TMemoryStream; Header:boolean):boolean;
    class function  toXML(var Item:TFolders; Output:TMemoryStream; Header:Boolean):boolean;
    class function  fromXML(xDoc:TXMLDocument; var Item:TFolder):boolean; overload;
    class function  fromXML(xDoc:TXMLDocument; var Items:TFolders):boolean; overload;

    class procedure Empty(Var Item:TFolder; const FreeFiles:boolean); overload;
    class procedure Invalidate(var Item:TFolders);
    class procedure Pack(var Items:TFolders);
    class procedure Purge(var Item:TFolders); overload;
    class procedure Purge(var Item:TFolders; Depth:LongInt); overload;
    class procedure Empty(Var Item:TFolders; const FreeFiles:boolean); overload;
    class procedure Init(Var Item:TFolder); overload;
    class procedure Init(Var Item:TFolders); overload;
    class procedure Done(Var Item:TFolder; const FreeFiles:boolean); overload;
    class procedure Done(Var Items:TFolders; const FreeFiles:boolean); overload;

    class function  Create(var Items:TFolders):PFolder;
    class function  getFolder(iID:QWord; var List:TFolders):PFolder;
    class function  getFolder(Path:Core.Strings.VarString; var List:TFolders):PFolder;
    class procedure ClearFiles(var Folders:TFolders);

  end;


function  Consumption(Task:Core.Database.Types.TTask; Var DomainID,UserID,Value:QWord):boolean;

Type
  TFileStorageRequest=record
    UserID                          : QWord;         // Returns Userid
    DomainID                        : QWord;         // Returns Domain ID
    InboxID                         : QWord;         // Returns Location of Inbox
    SpamID                          : QWord;         // Returns Location of Spam Folder
    Quota                           : QWord;         // Returns Quota Size
    Consumption                     : QWord;         // Returns Size Of Data In Storage
    Exists                          : Boolean;       // Returns if User is on System
    Enabled                         : Boolean;       // Returns Account.Enabled
    Forwarding                      : Boolean;       // Returns Account.Forwarding

    Node                            : Storage.MatrixNodes.Node.Item;   // User's assigned cloud storage device
    Router                          : Storage.AuraDisks.Router.TItem;

    Kind                            : LongInt;       // Pass this
    Username                        : Core.Strings.VarString;        // Pass This
    DomainName                      : Core.Strings.VarString;        // Pass This

    ForwardingAddress               : Core.Strings.VarString;        // Retruns A Forwarding Address (if applicable)
  end;
  PFileStorageRequest=^TFileStorageRequest;
  TUserSMTPRelayRequest=Record
    UserName                      : Core.Strings.VarString;
    DomainID                      : QWord;
    UserID                        : QWord;   // Returned
    LastIP                        : QWord;   // Returned
    Enabled                       : Boolean; // Returned
    Exists                        : Boolean; // Returned
  end;
  PUserSMTPRelayRequest=^TUserSMTPRelayRequest;
  TPOP3TopRequest=Record
    ID                            : QWord;
    DomainID                      : QWord;
    UserID                        : QWord;
    FolderID                      : QWord;
    Data                          : Core.Strings.VarString;
  end;
  PPOP3TopRequest=^TPOP3TopRequest;
  TPOP3RetrRequest=Record
    UserID                        : QWord;
    FileID                        : QWord;
    FolderID                      : QWord;
    DomainID                      : QWord;
    Data                          : Core.Strings.VarString;
  end;
  PPOP3RetrRequest=^TPOP3RetrRequest;
  TPOP3UIDLRequest=Record
    User_ID                       : QWord;
    Field_ID                      : QWord;
    Data                          : Core.Strings.VarString;
  end;
  PPOP3UIDLRequest=^TPOP3UIDLRequest;
  TPOP3UIDLAllItem=Record
    ID                            : QWord;
    Size                          : QWord;
    Deleted                       : Boolean; // runtime
    UID                           : Core.Strings.VarString;
  end;
  TPOP3UIDLAllItems=Array of TPOP3UIDLAllItem;
  TPOP3UIDLAllRequest=record
    UserID                        : QWord;
    Count                         : LongInt;
    Size                          : QWord;
    Items                         : TPOP3UIDLAllItems;
  end;
  PPOP3UIDLAllRequest=^TPOP3UIDLAllRequest;


  Function  FillPOP3RetrRequest(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,ItemID:QWord; Var RR:TPOP3RetrRequest):Boolean;
  Function  FillRelayRequest(Task:Core.Database.Types.TTask; Var UR:TUserSMTPRelayRequest):Boolean;
  Function  FillPOP3UIDLRequest(Task:Core.Database.Types.TTask; Var UR:TPOP3UIDLRequest):Boolean;
  Function  FillUIDLAllRequest(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var UAR:TPOP3UIDLAllRequest):Boolean;
  Function  FillPOP3TopRequest(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Var TR:TPOP3TopRequest):Boolean;
  Function  FillStorageRequest(Task:Core.Database.Types.TTask; Var SR:TFileStorageRequest):Boolean;

  Function  Execute_POP3_Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID,ItemID:QWord):Boolean;
  Function  Execute_POP3_Clear(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID:QWord):Boolean;

  procedure Copy(var Source,Destination:TUserSMTPRelayRequest); overload;
  procedure Empty(var Item:TUserSMTPRelayRequest); overload;
  procedure Empty(Var Item:TPOP3UIDLAllItem); overload;
  procedure Done(Var Item:TPOP3UIDLAllItem); overload;
  procedure Empty(Var Item:TPOP3UIDLAllItems); overload;
  procedure Done(Var Item:TPOP3UIDLAllItems); overload;
  procedure Empty(Var Item:TPOP3UIDLAllRequest); overload;
  procedure Done(Var Item:TPOP3UIDLAllRequest); overload;
  procedure Empty(Var Item:TPOP3UIDLRequest); overload;
  procedure Done(Var Item:TPOP3UIDLRequest); overload;
  procedure Empty(Var Item:TPOP3TopRequest); overload;
  procedure Done(Var Item:TPOP3TopRequest); overload;
  procedure Empty(Var Item:TFileStorageRequest); overload;
  procedure Done(Var Item:TFileStorageRequest); overload;
  procedure Copy(Var Source,Destination:TFileStorageRequest); overload;
  procedure Empty(Var Item:TPOP3RetrRequest); overload;
  procedure Done(Var Item:TPOP3RetrRequest); overload;

  function  CharsetParse(Value:Core.Strings.VarString; out Format:Byte):Byte;
  function  GenerateUID:Core.Strings.VarString;

  function getTransferEncoding(var Headers:Core.Arrays.Types.KeyStrings; iHdrCount:LongInt):Byte;
  function getType(var Headers:Core.Arrays.Types.KeyStrings; iHdrCount:LongInt; out CharSet:Byte; out CharFormat:Byte; out Name:Core.Strings.VarString):Byte;
  function getDisposition(var Headers:Core.Arrays.Types.KeyStrings; iHdrCount:LongInt; var Name:Core.Strings.VarString; out Created,Modified,Read:Double; out Size:QWord):byte;
  function getType(var Headers:Core.Arrays.Types.KeyStrings; var Boundary:Core.Strings.VarString; iHdrCount:LongInt; out CharSet:Byte; out CharFormat:Byte; out Name:Core.Strings.VarString):Byte;
  function GetParameter(Data:Core.Strings.VarString; Param : Core.Strings.VarString; Const Singleton:boolean) : Core.Strings.VarString;
  function UnwrapHeaders(Var Value:Core.Strings.VarString):Core.Strings.VarString;
  function ExtractHeaders(var Content:Core.Arrays.Types.VarString; const idxBreak:LongInt):Core.Strings.VarString;
  function EndOfMessage(var Content:Core.Arrays.Types.VarString):boolean;
  function IndexOfHeaderBreak(var Content:Core.Arrays.Types.VarString):LongInt;

  function Examine(var Mime:Items.SMTP.TMime; Source:TStream;  out idxStart,idxStop,Length:Int64):boolean;

  function Mimes(var Headers:Core.Arrays.Types.KeyStrings; var Content:Core.Arrays.Types.VarString; var Summary:Items.SMTP.TSummary; Refactor:TStream):Boolean;

  function ParseBody(var Summary:Items.SMTP.TSummary; var Body:Items.IMAP.TBodyElement):boolean;
  function ParseEnvelope(var Content:Core.Arrays.Types.VarString; var Summary:Items.SMTP.TSummary; var Envelope:Items.IMAP.TEnvelope):boolean;

implementation
uses
  Storage.Domains,
  Storage.CoreObjects,
  Storage.ContentTypes,
  App.Build,
  StrUtils,
  Math,

  db;

function cbDBMonitorNotifyied(Task:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:Cardinal):Boolean;

  procedure PushDomainDeleted;
  var
    iCount                       : LongInt;
    Commands                     : Core.Database.Types.Commands;
  begin
    if ItemP=Folders.DB.MonitorP then begin
      Result:=False;
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Folders.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Folders.DB.TableP,useForCriteria,Folders.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if ItemP=Files.DB.MonitorP then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Files.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Files.DB.TableP,useForCriteria,Files.DB.IDs.DomainID,poNone,oEqual,ItemID,Commands);
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
    if ItemP=Folders.DB.MonitorP then begin
      Result:=False;
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Folders.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Folders.DB.TableP,useForCriteria,Folders.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
        Result:=Core.Database.SQL.Delete(Task,@Commands);
      Finally
        Core.Database.Done(Commands);
      End;
    end else if (ItemP=Files.DB.MonitorP) then begin
      Try
        iCount:=0;
        Core.Database.AddCommand(iCount,Files.DB.TableP,@Commands);
        Core.Database.AddCommand(iCount,Files.DB.TableP,useForCriteria,Files.DB.IDs.UserID,poNone,oEqual,ItemID,Commands);
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
    Core.Database.Monitor.Notify.USER_DELETED   : PushUserDeleted;
  end;
end;

procedure cbDestroyFolders(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with Folders.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;

procedure cbDestroyFiles(ItemP:Core.Database.Monitor.Types.PItem);
begin
  with Files.DB do begin
    {$i Storage.Destroy.Table.inc}
  end;
end;


procedure RegisterDBM;
var
  iLcv: LongInt;
begin
  with Files.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then  begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFiles, @cbDBMonitorNotifyied);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
  with Folders.DB do begin
    if TableP = nil then begin
      New(TableP);
      Core.Database.Init(TableP^, Startup);
      for iLcv := 0 to High(Fields) do
        Core.Database.AddField(@Fields[iLcv], TableP);
      if MonitorP = nil then  begin
        New(MonitorP);
        Core.Database.Monitor.Init(MonitorP^, TableP^, @cbDestroyFolders, @cbDBMonitorNotifyied);
        Core.Database.Monitor.Add(MonitorP);
      end;
    end;
  end;
end;

procedure CB_FillRelayRequest(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  With PUserSMTPRelayRequest(DataP)^ do begin
    Exists:=True;
    UserID:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.ID).AsLargeInt;
    LastIP:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.LastIP).AsLargeInt;
    Enabled:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Enabled).AsBoolean;
  end;
end;

Function FillRelayRequest(Task:Core.Database.Types.TTask; Var UR:TUserSMTPRelayRequest):Boolean;
var
  iCount:LongInt;
  Commands:Core.Database.Types.Commands;
begin
  Result:=False; UR.Exists:=False; Empty(Commands); iCount:=0;
  Try
    With Storage.UserAccounts.Items.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,UR.DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,UR.UserName,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.LastIP,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Enabled,poNone,oNone,Commands);
      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillRelayRequest,@UR);
    end;
  Finally
    Empty(Commands);
  End;
end;

class procedure Files.Init(var Item:TItem);
begin
  Item.ID:=0;
  Item.FolderID:=0;
  Item.Size:=0;
  Item.Kind:=0;
  Item.Flags:=0;
  Item.Created:=0.0;
  Item.Modified:=0.0;
  Item.Allocated:=0.0;
  Item.Manifest:=nil;
  System.SetLength(Item.Name,0);
  System.SetLength(Item.Summary,0);
  FillChar(Item.Digest[0],SizeOf(Item.Digest),#0);
end;

class procedure Files.Init(var Item:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  System.SetLength(Item,0);
end;

class procedure Files.Clear(var Items:TItems);
begin
  SetLength(Items,0);
end;

class procedure Files.Purge(var Items:TItems);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Items) do begin
    if ( (Items[iLcv]<>nil) and (Items[iLcv]^.Valid=false)) then begin
      Done(Items[iLcv]^);
      Dispose(Items[iLcv]);
      Items[iLcv]:=nil;
    end;
  end;
  Pack(Items);
end;

class procedure Files.Pack(var Items:TItems);
var
  iLcv,jLcv:LongInt;
  iCt:LongInt;
  iStartCt:LongInt;
begin
  iStartCt:=System.Length(Items);
  iCt:=iStartCt;
  iLcv:=0;
  while (iLcv<iCt) do begin
    if Items[iLcv]=nil then begin
      for jLcv:=iLcv to iCt-2 do
        Items[jLcv]:=Items[jLcv+1];
      Dec(iCt);
    end else
      Inc(iLcv);
  end;
  if (iStartCt<>iCt) then
    System.SetLength(Items,iCt);
end;

class procedure Files.Empty(Var Item:TItem);
begin
  Item.Valid:=false;
  Item.ID:=0;
  Item.FolderID:=0;
  Item.Size:=0;
  Item.Kind:=0;
  Item.Flags:=0;
  Item.Created:=0.0;
  Item.Modified:=0.0;
  Item.Allocated:=0.0;
  Item.Manifest:=nil;
  System.FillChar(Item.Digest[0],SizeOf(Item.Digest),#0);
  System.SetLength(Item.Summary,0);
  SetLength(Item.Name,0);
end;

class procedure Files.Empty(Var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do begin
    Done(Items[iLcv]^);
    Dispose(Items[iLcv]);
  end;
  SetLength(Items,0);
end;

class procedure Files.Done(Var Item:TItem);
begin
  SetLength(Item.Name,0);
  SetLength(Item.Summary,0);


  Finalize(Item.Name);
  Finalize(Item.Digest);
  Finalize(Item.Summary);

  Finalize(Item);
end;

class procedure Files.Done(Var Items:TItems);
var
  iLcv:LongInt;
  itmP:PItem;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    if (itmP<>nil) then begin
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
  System.SetLength(Items,0);
  Finalize(Items);
end;

class procedure Files.fromString(var Digest:TMD5Digest; Value:Core.Strings.VarString);
begin
  FillByte(Digest,MD5DigestSize,0);
  if System.Length(Value)=MD5DigestSize then
    System.Move(Value[1],Digest[0],MD5DigestSize);
end;

class function  Files.IndexOf(ID:QWord; Var Items:TItems):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]^.ID=ID then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function  Files.Force(var Item:TItem; Var Items:TItems):LongInt;
var
  iDX:LongInt;
begin
  iDX:=IndexOf(@Item,Items);
  if (iDX=-1) then
    iDX:=Add(Item,Items);
  Result:=iDX;
end;

class function  Files.IndexOf(ItemP:PItem; Var Items:TItems):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]=ItemP then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function  Files.IndexOf(FolderID:QWord; Name:Core.Strings.VarString; Var Items:TItems):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Items) do begin
    if (Items[iLcv]^.FolderID=FolderID) and (Items[iLcv]^.Name=Name) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

class function  Files.Get(ID:QWord; Var Items:TItems):PItem;
var
  iLcv:LongInt;
begin
  Result:=nil;;
  for iLcv:=0 to High(Items) do begin
    if Items[iLcv]^.ID=ID then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

class function  Files.Get(Name:Core.Strings.VarString; Var Items:TItems):PItem;
var
  iLcv:LongInt;
begin
  Result:=nil;;
  for iLcv:=0 to High(Items) do begin
    if SameText(Items[iLcv]^.Name,Name) then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

class function  Files.Get(FolderID:QWord; Name:Core.Strings.VarString; Var Items:TItems):PItem;
var
  iLcv:LongInt;
begin
  Result:=nil;;
  for iLcv:=0 to High(Items) do begin
    if (Items[iLcv]^.FolderID=FolderID) and SameText(Items[iLcv]^.Name,Name) then begin
      Result:=Items[iLcv];
      break;
    end;
  end;
end;

class function  Files.Add(var Item:TItem; var Items:TItems):LongInt;
var
  iCt:LongInt;
begin
  Result:=IndexOf(@Item,Items);
  if (Result=-1) then begin
    if (Item.ID<>0) then
      Result:=IndexOf(Item.ID,Items);
    if (Result=-1) then
      Result:=IndexOf(Item.FolderID,Item.Name,Items);
  end;
  if (Result=-1)then begin
    iCt:=System.Length(Items);
    System.SetLength(Items,iCt+1);
    Items[iCt]:=@Item;
    Result:=iCt;
  end;
end;

class function  Files.Create(Var Items:TItems):PItem;
var
  iCt:LongInt;
  itmP:PItem;
begin
  new(itmP);
  Init(itmP^);
  iCt:=System.Length(Items);
  System.SetLength(Items,iCt+1);
  Items[iCt]:=itmP;
  Result:=itmP;
end;

class function Files.Add(Name:Core.Strings.VarString; FolderID,Size:QWord; Kind:LongInt; var Digest:TMD5Digest; Var Items:TItems):PItem;
begin
  Result:=Create(Items);
  Result^.FolderID:=FolderID;
  Result^.Size:=Size;
  Result^.Kind:=Kind;
  Result^.Name:=Name;
  Copy(Digest,Result^.Digest);
end;

class function  Files.DB.Add(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID:QWord; var Item:TItem; Stream:TStream):Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  auFile                         : TFileStream;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(LongInt));

    if (Item.Created=0) then
      Item.Created:=Core.Timer.dtUT;
    Item.Modified:=Core.Timer.dtUT;
    Item.Allocated:=1;

    if (Stream<>nil) then begin
      Item.Allocated:=Core.Timer.dtUT;
      Item.Size:=Stream.Size;
    end;

    Core.Streams.CheckSum(Stream,Item.Digest);

    Core.Database.AddCommand(iCount,TableP,@Commands);

    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    {$i Storage.UserStorage.File.Add.Fields.inc}

    Result:=Core.Database.SQL.Insert(Task,@Commands);

    If (Item.ID<>0) then begin
      if Storage.AuraDisks.Files.Create(Node,DomainID,UserID,FolderID,Item.ID,Storage.AuraDisks.Kinds.User,auFile) then begin
        Try
          if (Stream<>nil) then
            Core.Streams.Copy(Stream,auFile);
        finally
          auFile.Free();
        end;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure cb_Storage_Files_Fill_Name(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PString(DataP)^:=Fields.FieldByName(Files.DB.Keys.Name).AsString;
end;

procedure cb_Storage_Files_Fill(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmP:Files.PItem;
begin
  itmP:=DataP;
  {$i Storage.UserStorage.File.Fill.inc}
  itmP^.Summary:=Fields.FieldByName(Files.DB.Keys.Summary).AsString;
end;

procedure cb_Storage_Files_Fill_List(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmsP:Files.PItems;
  itmP:Files.PItem;
  iLen:LongInt;
begin
  itmsP:=DataP;
  iLen:=System.Length(itmsP^);
  System.SetLength(itmsP^,iLen+1);
  System.New(itmP);
  Files.Init(itmP^);

  itmsP^[iLen]:=itmP;

  {$i Storage.UserStorage.File.Fill.inc}
  itmP^.Summary:=Fields.FieldByName(Files.DB.Keys.Summary).AsString;
end;

procedure cb_Storage_Files_Fill_Refresh(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmsP:Files.PItems;
  itmP:Files.PItem;
  iLen:LongInt;
begin
  itmsP:=DataP;
  iLen:=System.Length(itmsP^);

  itmP:=Files.Get(Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt,itmsP^);
  if (itmP=nil) then begin
    System.SetLength(itmsP^,iLen+1);
    System.New(itmP);
    Files.Init(itmP^);
    itmsP^[iLen]:=itmP;
  end;
  {$i Storage.UserStorage.File.Fill.inc}
  itmP^.Summary:=Fields.FieldByName(Files.DB.Keys.Summary).AsString;
end;


procedure cb_Storage_Files_Fill_List_All(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  itmsP:Files.PItems;
  itmP:Files.PItem;
  iLen:LongInt;
begin
  itmsP:=DataP;
  iLen:=System.Length(itmsP^);
  System.SetLength(itmsP^,iLen+1);
  System.New(itmP);
  Files.Init(itmP^);

  itmsP^[iLen]:=itmP;

  {$i Storage.UserStorage.File.Fill.inc}
end;


procedure cb_Storage_Files_Fill_IMAP_Records(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  RecsP:Items.IMAP.PRecordCount;
  iFlags:LongInt;
begin
  RecsP:=DataP;
  iFlags:=Fields.FieldByName(Files.DB.Keys.Flags).AsLongInt;
  Inc(RecsP^.Total);
  if (iFlags or Items.IMAP.Flags.Deleted=iFlags) then begin
    Inc(RecsP^.Deleted);
  end else begin
    if (iFlags or Items.IMAP.Flags.Recent=iFlags) then
      Inc(RecsP^.Recent);
    if (iFlags or Items.IMAP.Flags.Seen=iFlags) then
      Inc(RecsP^.Read)
    else
      Inc(RecsP^.Unread);
  end;
end;

procedure cb_Storage_Files_Get_Flags(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  FlagsP:Core.Database.Types.PInteger;
begin
  FlagsP:=DataP;
  FlagsP^:=Fields.FieldByName(Files.DB.Keys.Flags).AsLongInt;
end;

procedure cb_Storage_Files_Get_Summary(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItmP:Core.Database.Types.PVarString;
begin
  ItmP:=DataP;
  ItmP^:=Fields.FieldByName(Files.DB.Keys.Summary).AsString;
end;

procedure cb_Storage_Files_Fill_List_IDs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  Core.Arrays.LargeWord.Add(Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt,Core.Database.Types.PLargeWordArray(DataP)^);
end;


class function  Files.DB.Data(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID:QWord; var Item:TItem; var Stream:TFileStream):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Item.Allocated:=0.0;
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item);
    Result:=Result and (Item.Allocated<>0);
    if (Result=true) then
      Storage.AuraDisks.Files.Acquire(Node,DomainID,UserID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.User,Stream);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Files.DB.Data(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID:QWord; var Item:TItem; var Mime:Items.SMTP.TMime; Refactor:TMemoryStream; Output:TStream):boolean;
var
  iCount                         : LongInt;
  idx                            : LongInt;
  iLen                           : LongInt;
  iStop                          : LongInt;
  saData                         : Core.Arrays.Types.VarString;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Core.Arrays.VarString.Init(saData);
  Try
    Core.Database.Empty(Commands);
    Try
      iCount:=0;

      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Item.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      {$i Storage.UserStorage.Files.Fields.inc}
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);

      Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item);

      Result:=Result and (Item.Allocated<>0);
      if (Result=true) then
        Storage.AuraDisks.Files.Read(Node,DomainID,UserID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.User,saData);

      Refactor.Size:=0;
      Output.Size:=0;
      // many need to increase by actual main body headers Could include in
      idx:=Mime.idxContentStart;
      iCount:=System.Length(saData);
      iStop:=Mime.idxContentEnd;
      while ( (idx<=iStop) and (idx<iCount)) do begin
        iLen:=System.Length(saData[idx]);
        if iLen>0 then
          Core.Streams.Append(saData[idx],iLen,Refactor);
        Refactor.Write(#13#10,2);
        Inc(idx);
      end;
      Refactor.Position:=0;
      Output.Position:=0;
      if (Mime.cntEncoding<>Items.SMTP.Encoding.emBase64) then begin
        Encryption.Base64.Encode(Refactor,Output,Refactor.Size);
      end else begin
        Core.Streams.Copy(Refactor,Output);
      end;
      Result:=true;
      Refactor.Size:=0;
    Finally
      Core.Database.Done(Commands);
    End;
  Finally
    Core.Arrays.VarString.Done(saData);
  end;
end;


class function  Files.Data(var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID,ItemID:QWord; var Stream:TFileStream):Boolean;
begin
  Result:=Storage.AuraDisks.Files.Acquire(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User,Stream);
end;


class function Files.DB.Fill(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,ItemID:QWord; Var Item:TItem; var Data:TFileStream):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item) and (Item.ID<>0);
    if (Item.ID<>0) then
      Storage.AuraDisks.Files.Acquire(Node,DomainID,UserID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.User,Data);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Files.DB.Fill(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Value:Core.Strings.VarString; Var Item:TItem):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Value,Commands);
    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item) and (Item.ID<>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class function Files.DB.Fill(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; Var Item:TItem):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Files.DB.Extension(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; var Value:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    SetLength(Value,0);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_Name,@Value);

    Value:=Core.Utils.Files.Extract(Value,efeoNone);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Files.DB.Force(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID:QWord; iKind:LongInt; dtCreated:double; Name:Core.Strings.VarString; Var Item:TItem; var Data:TFileStream):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Name,Commands);
    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item);
    if (Item.ID=0) then begin
      Item.FolderID:=FolderID;
      Item.Name:=Name;
      Item.Kind:=iKind;
      Item.Created:=dtCreated;
      Files.DB.Create(Task,Node,DomainID,UserID,FolderID,Item.ID,Name,Item.Created,Item.Kind);
    end;
    if Item.ID<>0 then begin
      Storage.AuraDisks.Files.Acquire(Node,DomainID,UserID,Item.FolderID,Item.ID,Storage.AuraDisks.Kinds.User,Data);
      Result:=true;
    end else
      Result:=False;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Exists(Task:Core.Database.Types.TTask; DomainID,UserID,ItemID:QWord; Var Item:TItem):Boolean;
var
  iCount                         : LongInt;
  fCount                         : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,fCount) and (fCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Exists(Task:Core.Database.Types.TTask; DomainID,UserID,FolderId:QWord; Var Value:Core.Strings.VarString):Boolean;
var
  iCount                         : LongInt;
  fCount                         : QWord;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poNone,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oEqual,Value,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,fCount) and (fCount>0);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Clear(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID:QWord): Boolean;
var
  Files                          : Core.Arrays.Types.LargeWord;
  iLcv                           : LongInt;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    List(Task,DomainID,UserID,FolderID,Files,IDs.ID);
    Try
      iCount:=0;
      Core.Database.Empty(Commands);

      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);

      Result:=Core.Database.SQL.Delete(Task,@Commands);
    Finally
      Core.Database.Done(Commands);
    End;
    for iLcv:=0 to High(Files) do
      Storage.AuraDisks.Files.Delete(Node,DomainID,UserID,FolderID,Files[iLcv],Storage.AuraDisks.Kinds.User);
  finally
    Core.Arrays.LargeWord.Done(Files);
  end;
end;

class function Files.DB.Count(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Records:Items.IMAP.TRecordCount):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Items.IMAP.Empty(Records);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Flags,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_IMAP_Records,@Records);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Items:TItems; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);


    Core.Database.AddCommand(iCount,TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);


    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_List,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Refresh(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Items:TItems; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);

    Invalidate(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);


    Core.Database.AddCommand(iCount,TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_Refresh,@Items);

    If Result then
      Purge(Items);

  Finally
    Core.Database.Done(Commands);
  End;
end;


class function Files.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Items:Core.Arrays.Types.LargeWord; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Arrays.LargeWord.Empty(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);


    Core.Database.AddCommand(iCount,TableP,useForOrderBy,SortField,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_List_IDS,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Kind:LongInt; Var Items:Core.Arrays.Types.LargeWord; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Arrays.LargeWord.Empty(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,Kind,Commands);


    Core.Database.AddCommand(iCount,TableP,useForOrderBy,SortField,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_List_IDS,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TItems; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InFolders,poAnd,oIn,Folders,Commands);


    Core.Database.AddCommand(iCount,TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);


    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_List,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Folders:Core.Arrays.Types.LargeWord; Var Items:TItems; var Pattern:Core.Arrays.Types.VarString; SortField:Byte=0):Boolean;
var
  iCount                         : LongInt;
  iPatternCount                  : LongInt;
  iLcv                           : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Files.Empty(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InFolders,poAnd,oIn,Folders,Commands);


    iPatternCount:=System.Length(Pattern);
    if iPatternCount>0 then begin
      if iPatternCount>1 then begin
        Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poNone,oLike,Pattern[0],Commands);
        for iLcv:=1 to iPatternCount-2 do
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poOr,oLike,Pattern[iLcv],Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poOr,oLike,Pattern[iPatternCount-1],Commands);

        Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);
      end else begin
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Name,poAnd,oLike,Pattern[0],Commands);
      end;
    end;

    Core.Database.AddCommand(iCount,TableP,useForOrderBy,SortField,poNone,oNone,Commands);

    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);


    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_List,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class function Files.DB.ListAll(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Var Items:TItems):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Items);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    {$i Storage.UserStorage.Files.Fields.inc}
    //  Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill_List_All,@Items);
  Finally
    Core.Database.Done(Commands);
  End;
end;

procedure CB_FillUIDLAllRequest(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  RequestP:PPOP3UIDLAllRequest;
begin
  RequestP:=DataP;
  SetLength(RequestP^.Items,RequestP^.Count+1);
  Try
    RequestP^.Items[RequestP^.Count].ID:=Fields.FieldByName(Files.DB.Keys.ID).AsLargeInt;
    RequestP^.Items[RequestP^.Count].Size:=Fields.FieldByName(Files.DB.Keys.Size).AsLongInt;
    RequestP^.Items[RequestP^.Count].UID:=Fields.FieldByName(Files.DB.Keys.Name).AsString;
    Inc(RequestP^.Size,RequestP^.Items[RequestP^.Count].Size);
  Finally
    Inc(RequestP^.Count);
  End;
end;

function FillUIDLAllRequest(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var UAR: TPOP3UIDLAllRequest):Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Empty(UAR); Empty(Commands); UAR.UserID:=UserID; UAR.Size:=0;
    iCount:=0; iKind:=Kind.SMTP;
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,iKind,Commands);
      Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Size,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oNone,Commands);
    end;
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillUIDLAllRequest,@UAR);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function Execute_POP3_Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID,ItemID:QWord):Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Storage.AuraDisks.Files.Delete(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User);

  Try
    iCount:=0; iKind:=Kind.SMTP; Empty(Commands);
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
    end;
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.toXML(var Item:TItems; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean;
var
  iLcv,iCount:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  iCount:=System.Length(Item);
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to iCount-1 do
    if Item[iLcv]^.Valid then
      toXML(Item[iLcv]^,Output,Refactor,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  Result:=true;
end;

class function Files.toXML(var Item:TItems; Data:TStream; Output:TMemoryStream; Refactor:TStream; Header:boolean):boolean;
var
  iLcv,iCount:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  iCount:=System.Length(Item);
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to iCount-1 do
    toXML(Item[iLcv]^,Data,Output,Refactor,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  Result:=true;
end;

class function Files.toXML(var Item:TItem; Data:TStream; Output:TMemoryStream; Refactor:TStream; Header:Boolean):boolean;
begin
  Result:=false;
  Data.Position:=0;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.FolderID,Item.FolderID),Output);
    Core.Streams.Write(Print(XML.Size,Item.Size),Output);
    Core.Streams.Write(Print(XML.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(XML.Flags,Item.Flags),Output);
    Core.Streams.Write(Print(XML.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Allocated,Item.Allocated),Output);
    Core.Streams.Write(Print(XML.Digest,Item.Digest),Output);
    Core.Streams.Write(Print(XML.Name,Item.Name),Output);
    Core.Streams.Write(Print(XML.Summary,Item.Summary,CDATA_OFF),Output);

    Core.Streams.Write('<',1,Output);
    Core.Streams.Write(XML.Data,Output);
    Core.Streams.Write('>',1,Output);

    Encryption.Base64.Encode(Data,Output,Data.Size);

    Core.Streams.Write('</',2,Output);
    Core.Streams.Write(XML.Data,Output);
    Core.Streams.Write('>',1,Output);

  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=true;
end;

class function Files.toXML(var Item:TItem; Output:TMemoryStream; Refactor:TStream; Header:Boolean):boolean;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.FolderID,Item.FolderID),Output);
    Core.Streams.Write(Print(XML.Size,Item.Size),Output);
    Core.Streams.Write(Print(XML.Kind,Item.Kind),Output);
    Core.Streams.Write(Print(XML.Flags,Item.Flags),Output);
    Core.Streams.Write(Print(XML.Created,Item.Created),Output);
    Core.Streams.Write(Print(XML.Modified,Item.Modified),Output);
    Core.Streams.Write(Print(XML.Allocated,Item.Allocated),Output);
    Core.Streams.Write(Print(XML.Digest,Item.Digest),Output);
    Core.Streams.Write(Print(XML.Name,Item.Name),Output);

    Core.Streams.Write(Print(XML.Summary,Item.Summary,CDATA_OFF),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  Result:=true;
end;

class function Files.fromXML(xDoc:TXMLDocument; var Item:TItem):boolean;
var
  xItem:TDOMNode;
  xSmry:TDOMNode;
begin
  Result:=False;
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      xSmry:=getChildNode(xItem,XML.Summary);
      Item.ID:=toQWord(xItem,XML.ID);
      Item.FolderID:=toQWord(xItem,XML.FolderID);
      Item.Size:=toQWord(xItem,XML.Size);
      Item.Kind:=toInteger(xItem,XML.Kind);
      Item.Flags:=toInteger(xItem,XML.Flags);
      Item.Created:=toDouble(xItem,XML.Created);
      Item.Modified:=toDouble(xItem,XML.Modified);
      Item.Allocated:=toDouble(xItem,XML.Allocated);
      Item.Name:=toString(xItem,XML.Name);
      Item.Summary:=getNodeText(xSmry);
      toMD5Digest(xItem,XML.Digest,Item.Digest);
      Item.Valid:=True;
      Result:=True;
    end;
  end;
end;

class procedure Files.Invalidate(var Items:TItems);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Items) do
    Items[iLcv]^.Valid:=false;
end;

class function Files.fromXML(xDoc:TXMLDocument; var Items:TItems):boolean;
var
  xItems:TDOMNode;
  xItem:TDOMNode;
  xSmry:TDOMNode;
  iCount,iLcv:LongInt;
  itmP:PItem;
  iID:QWORD;
  iFolderID:QWORD;
  sName:Core.Strings.VarString;
begin
  Result:=False;
  xItems:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Items);
  //Core.XML.DB.getNode(xDoc,XML.Stanzas.Items);
  Invalidate(Items);
  if (xItems<>nil) then begin
    iCount:=System.Length(Items);
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      with Core.XML.DB do begin
        xItem:=xItems.ChildNodes[iLcv];
        xSmry:=getChildNode(xItem,XML.Summary);

        iID:=toQWord(xItem,XML.ID);
        itmP:=Get(iID,Items);
        if (itmP=nil) then begin
          iFolderID:=toQWord(xItem,XML.FolderID);
          sName:=toString(xItem,XML.Name);
          itmP:=Get(iFolderID,sName,Items);
        end;
        if (itmP=nil) or ((itmP^.ID<>0) and (itmP^.ID<>iID)) then begin
          new(itmP);
          Init(itmP^);
          itmP^.ID:=iID;
          itmP^.FolderID:=iFolderID;
          itmP^.Name:=sName;
          SetLength(Items,iCount+1);
          Items[iCount]:=itmP;
          Inc(iCount);
        end;
        itmP^.ID:=iID;
        itmP^.FolderID:=toQWord(xItem,XML.FolderID);
        itmP^.Size:=toInteger(xItem,XML.Size);
        itmP^.Kind:=toInteger(xItem,XML.Kind);
        itmP^.Created:=toDouble(xItem,XML.Created);
        itmP^.Modified:=toDouble(xItem,XML.Modified);
        itmP^.Allocated:=toDouble(xItem,XML.Allocated);
        itmP^.Name:=toString(xItem,XML.Name);
        itmP^.Summary:=getNodeText(xSmry);
        toMD5Digest(xItem,XML.Digest,itmP^.Digest);
        Result:=True;
        itmP^.Valid:=true;
      end;

    end;
  end;
end;

class function Files.DB.Move(Task:Core.Database.Types.TTask; Var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,ItemID,FolderID,NewFolderID:QWord): Boolean;
var
  iCount   : LongInt;
  Commands : Core.Database.Types.Commands;
  FSSource : TFileStream;
  FSDest   : TFileStream;
begin
  Result:=False; iCount:=0;
  Try
    with Files do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.FolderID,poNone,oNone,NewFolderID,Commands);
    end;
    Result:=Core.Database.SQL.Update(Task,@Commands);
    if Storage.AuraDisks.Files.Exists(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User) then begin
      Storage.AuraDisks.Files.Read(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User,FSSource);
      Storage.AuraDisks.Files.Create(Node,DomainID,UserID,NewFolderID,ItemID,Storage.AuraDisks.Kinds.User,FSDest);
      Core.Streams.Copy(FSSource,FSDest);
      FSSource.Size:=0;
      FSSource.Free();
      FSDest.Free();
      Storage.AuraDisks.Files.Delete(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Files.Copy(Var Source,Destination:TMD5Digest);
begin
  System.Move(Source[0],Destination[0],SizeOf(TMD5Digest));
end;

class procedure Files.Copy(Var Source,Destination:TItem);
begin
  With Destination do begin
    ID:=Source.ID;
    FolderID:=Source.FolderID;
    Size:=Source.Size;
    Kind:=Source.Kind;
    Created:=Source.Created;
    Modified:=Source.Modified;
    Allocated:=Source.Allocated;
    Copy(Source.Digest,Digest);
    Name:=Source.Name;
    Summary:=Source.Summary;
    Manifest:=Source.Manifest;
  end;
end;

class procedure Files.Copy(Var Source,Destination:TItems);
var
  iLcv:LongInt;
  iLen:LongInt;
begin
  iLen:=System.Length(Source);
  SetLength(Destination,iLen);
  for iLcv:=0 to iLen-1 do
    Destination[iLcv]:=Source[iLcv];
end;

class function Files.Compare(var Val1,Val2:TMD5Digest):boolean;
begin
  Result:=SysUtils.CompareMem(@Val1[0],@Val2[0],SizeOf(TMD5Digest));
end;

class function Files.DB.Copy(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,ItemID, NewItemID:QWord): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Item                           : TItem;
  FSSource                       : TFileStream;
  FSDest                         : TFileStream;
begin
  iCount:=0;
  Result:=False;
  Empty(Item);
  Try
    Item.ID:=ItemID;
    if Fill(Task,Node,DomainID,UserID,ItemID,Item,FSSource) then begin
      Try
        Try
          Core.Database.AddCommand(iCount,TableP,@Commands);
          // Setup Primary ID
          Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,NewItemID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
          {$i Storage.UserStorage.File.Add.Fields.inc}
          Core.Database.SQL.Insert(Task,@Commands);
          if NewItemID<>0 then begin
            Storage.AuraDisks.Files.Copy(Node,DomainID,UserID,Item.FolderID,ItemID,Storage.AuraDisks.Kinds.User,NewItemID);
            Result:=true;
          end;
        Finally
          Core.Database.Done(Commands);
        End;
      finally
        FSSource.Free();
      end;
    end;
  finally
    Done(Item);
  end;
end;

class function Files.DB.Copy(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,ItemID, NewFolderID,NewItemID:QWord): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Item                           : TItem;
  OldFolderID                    : QWord;
  FSSource                       : TFileStream;
begin
  iCount:=0;
  Result:=False;
  Init(Item);
  Try
    Item.ID:=ItemID;
    if Fill(Task,Node,DomainID,UserID,ItemID,Item,FSSource) then begin
      OldFolderID:=Item.FolderID;
      Item.FolderID:=NewFolderID;
      Try
        Try
          Core.Database.AddCommand(iCount,TableP,@Commands);
          // Setup Primary ID
          Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,NewItemID,Commands);
          Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);
          {$i Storage.UserStorage.File.Add.Fields.inc}
          Core.Database.SQL.Insert(Task,@Commands);
          if NewItemID<>0 then begin
            Storage.AuraDisks.Files.Copy(Node,DomainID,UserID,OldFolderID,ItemID,NewFolderID,Storage.AuraDisks.Kinds.User,NewItemID);
            Result:=true;
          end;
        Finally
          Core.Database.Done(Commands);
        End;
      finally
        FSSource.Free();
      end;
    end;
  finally
    Done(Item);
  end;
end;


procedure CB_Refresh_Folders(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemsP:Folders.PFolders;
  ItemP:Folders.PFolder;
  iCount:LongInt;
begin
  ItemsP:=DataP;
  ItemP:=Folders.getFolder(Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt,ItemsP^);
  if ItemP=nil then begin
    iCount:=System.Length(ItemsP^);
    New(ItemP);
    Folders.Init(ItemP^);
    System.SetLength(ItemsP^,iCount+1);
    ItemsP^[iCount]:=ItemP;
  end;
  ItemP^.ID:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
  ItemP^.Path:=Fields.FieldByName(Folders.DB.Keys.Path).AsString;
  ItemP^.Verified:=True;
end;

procedure CB_List_Folders(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemsP:Folders.PFolders;
  ItemP:Folders.PFolder;
  iCount:LongInt;
begin
  ItemsP:=DataP;
  iCount:=System.Length(ItemsP^);
  New(ItemP);
  Folders.Init(ItemP^);
  System.SetLength(ItemsP^,iCount+1);
  Itemsp^[iCount]:=ItemP;

  ItemP^.ID:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
  ItemP^.Path:=Fields.FieldByName(Folders.DB.Keys.Path).AsString;
  ItemP^.Verified:=True;
end;

procedure CB_List_Folder(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemP:Folders.PFolder;
  iCount:LongInt;
begin
  ItemP:=DataP;
  ItemP^.ID:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
  ItemP^.Path:=Fields.FieldByName(Folders.DB.Keys.Path).AsString;
  ItemP^.Verified:=True;
end;

procedure CB_List_FolderIDs(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemsP:Core.Database.Types.PLargeWordArray;
  iCount:LongInt;
begin
  ItemsP:=DataP;
  iCount:=System.Length(ItemsP^);
  System.SetLength(ItemsP^,iCount+1);
  ItemsP^[iCount]:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
end;

procedure CB_Folder_ID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
end;

class function  Folders.getFolder(iID:QWord; var List:TFolders):PFolder;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    if List[iLcv]^.ID=iID then begin
      Result:=List[iLcv];
      break;
    end;
  end;
end;

class function  Folders.getFolder(Path:Core.Strings.VarString; var List:TFolders):PFolder;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    if SameText(List[iLcv]^.Path,Path) then begin
      Result:=List[iLcv];
      break;
    end;
  end;
end;

procedure CB_Path(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PString(DataP)^:=Fields.FieldByName(Folders.DB.Keys.Path).AsString;
end;

class procedure Folders.ClearFiles(var Folders:TFolders);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Folders) do
    Files.Empty(Folders[iLcv]^.Files);
end;

class function  Folders.DB.getPath(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord):Core.Strings.VarString;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sPath                          : Core.Strings.VarString;
begin
  SetLength(sPath,0);
  iCount:=0;
  Core.Database.AddCommand(iCount,TableP,@Commands);
  Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
  Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
  Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
  Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);

  Core.Database.SQL.Select(Task,@Commands,@CB_Path,@sPath);

  Result:=sPath;
end;

class function  Folders.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var Items:TFolders): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Empty(Items,FREE_FILES); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Path,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List_Folders,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Folders.DB.Refresh(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Path:Core.Strings.VarString; var Items:TFolders): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sSubFolders                    : Core.Strings.VarString;
begin
  Result:=False;
  Invalidate(Items);
  sSubFolders:=Concat(Path,'/%');
  iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poNone,oLike,Path,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poOr,oLike,sSubFolders,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Path,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Refresh_Folders,@Items);

    if Result then
      Purge(Items);
  finally
    Core.Database.Done(Commands);
  end;
end;


class function  Folders.DB.ID(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Path:Core.Strings.VarString; out ItemID:QWord):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  iCount:=0; ItemID:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poAnd,oEqual,Path,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_Folder_ID,@ItemID) and (ItemID<>0);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Folders.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Path:Core.Strings.VarString; var Items:TFolders): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sSubFolders                    : Core.Strings.VarString;
begin
  Result:=False;
  sSubFolders:=Concat(Path,'/%');
  Empty(Items,FREE_FILES); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poNone,oLike,Path,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poOr,oLike,sSubFolders,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Path,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Path,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List_Folders,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Folders.DB.List(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; Path:Core.Strings.VarString; var Items:Core.Arrays.Types.LargeWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sSubFolders                    : Core.Strings.VarString;
begin
  Result:=False;
  sSubFolders:=Concat(Path,'/%');
  Core.Arrays.LargeWord.Empty(Items); iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poAnd,oOpenBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poNone,oLike,Path,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Path,poOr,oLike,sSubFolders,Commands);

    Core.Database.AddCommand(iCount,TableP,useForCriteriaBracket,poNone,oCloseBracket,Commands);

    Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.Path,poNone,oNone,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_List_FolderIDs,@Items);
  finally
    Core.Database.Done(Commands);
  end;
end;


class function  Folders.DB.Rename(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Items:TFolders): Boolean;
var
  iLcv                           : LongInt;
begin
  Result:=True;
  For iLcv:=0 to High(Items) do
    Rename(Task,DomainID,UserID,Items[iLcv]^);
  Result:=True;
end;

class function  Folders.DB.Rename(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; var Item:TFolder): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Path,poNone,oNone,Item.Path,Commands);
    Result:=Core.Database.SQL.Update(Task,@Commands);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Folders.DB.Create(Task:Core.Database.Types.TTask; DomainID,UserID:QWord; var ItemID:QWord; Path:Core.Strings.VarString): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(LongInt));

    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Path,poNone,oNone,Path,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Folders.DB.Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,ItemID:QWord): Boolean;
var
  Commands                       : Core.Database.Types.Commands;
  Folders                        : Core.Arrays.Types.LargeWord;
  sPath                          : Core.Strings.VarString;
  iLcv                           : LongInt;

  procedure PushDelete(iID:QWord);
  var
    iCount                       : LongInt;
  begin
    Files.DB.Clear(Task,Node,DomainID,UserID,iID);

    iCount:=0;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,iID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);

    Storage.AuraDisks.Files.Delete(Node,DomainID,UserID,iID,Storage.AuraDisks.Kinds.User);
  end;

begin
  Result:=false;
  sPath:=getPath(Task,DomainID,UserID,ItemID);
  if (ItemID<>0) then begin
    PushDelete(ItemID);
    if Length(sPath)>0 then begin
      Try
        Result:=List(Task,DomainID,UserID,sPath,Folders);
        try
          for iLcv:=0 to High(Folders) do
            PushDelete(Folders[iLcv]);
        finally
          Core.Arrays.LargeWord.Empty(Folders);
        end;
      Finally
        Core.Database.Done(Commands);
      End;
    end;
  end;
end;

procedure CB_Read_Folder(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemP:Folders.PFolder;
  iCount:LongInt;
begin
  ItemP:=DataP;
  ItemP^.ID:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
  ItemP^.Path:=Fields.FieldByName(Folders.DB.Keys.Path).AsString;
end;

class function  Folders.DB.CreateDefaults(Task:Core.Database.Types.TTask; var DomainID,UserID,Trash,Inbox,SpamBox,OutBox,SentBox,ArchiveBox,Trashbox,Devices:QWord): Boolean;
var
  BoxID:QWord;
  sFolder:Core.Strings.VarString;
begin


  sFolder:=Folders.Defaults.Home.Devices;
  Result:=Create(Task,DomainID,UserID,Devices,sFolder);

  sFolder:=Folders.Defaults.Home.Documents;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Mail;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Music;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Pictures;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Videos;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Social;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Social+'/'+Folders.Defaults.Home.MyNetworks;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Social+'/'+Folders.Defaults.Home.OtherNetworks;
  Result:=Create(Task,DomainID,UserID,BoxID,sFolder);

  sFolder:=Folders.Defaults.Home.Trash;
  Result:=Create(Task,DomainID,UserID,Trash,sFolder);

  sFolder:=Folders.Defaults.Home.Mail+'/'+Folders.Defaults.Mail.Archive;
  Result:=Create(Task,DomainID,UserId,ArchiveBox,sFolder);

  sFolder:=Folders.Defaults.Home.Mail + '/' + Folders.Defaults.Mail.Inbox;
  Result:=Create(Task,DomainID,UserID,Inbox,sFolder);

  sFolder:=Folders.Defaults.Home.Mail + '/' + Folders.Defaults.Mail.Outbox;
  Result:=Create(Task,DomainID,UserID,OutBox,sFolder);

  sFolder:=Folders.Defaults.Home.Mail + '/' + Folders.Defaults.Mail.Sent;
  Result:=Create(Task,DomainID,UserID,SentBox,sFolder);

  sFolder:=Folders.Defaults.Home.Mail + '/' + Folders.Defaults.Mail.Spam;
  Result:=Create(Task,DomainID,UserID,Spambox,sFolder);

  sFolder:=Folders.Defaults.Home.Mail + '/' + Folders.Defaults.Mail.Trash;
  Result:=Create(Task,DomainID,UserID,Trashbox,sFolder);

end;

class function  Folders.DB.Force(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Path:Core.Strings.VarString; out ItemID:Qword): Boolean;
var
  qID                            : QWord;
begin
  Result:=false;
  ID(Task,DomainID,UserID,Path,qID);
  if (qID=0) then begin
    Result:=Folders.DB.Create(Task,DomainID,UserID,qID,Path);
  end;
  ItemID:=qID;
end;

class function  Folders.DB.Verify(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Path:Core.Strings.VarString; Kind:FolderVerifiyFind; out ItemID:Qword): Boolean;
var
  qID                            : QWord;
begin
  Result:=false;
  ID(Task,DomainID,UserID,Path,qID);
  if (qID=0) then begin
    Result:=Folders.DB.Create(Task,DomainID,UserID,qID,Path);
    case Kind of
      fvkTrash        : Storage.UserAccounts.Items.DB.SetTrash(Task,DomainID,UserID,ItemID);
      fvkInbox        : Storage.UserAccounts.Items.DB.SetInBox(Task,DomainID,UserID,ItemID);
      fvkOutbox       : Storage.UserAccounts.Items.DB.SetOutBox(Task,DomainID,UserID,ItemID);
      fvkSentBox      : Storage.UserAccounts.Items.DB.SetSentBox(Task,DomainID,UserID,ItemID);
      fvkArchiveBox   : Storage.UserAccounts.Items.DB.SetArchiveBox(Task,DomainID,UserID,ItemID);
      fvkSpambox      : Storage.UserAccounts.Items.DB.SetSpamBox(Task,DomainID,UserID,ItemID);
      fvkTrashbox     : Storage.UserAccounts.Items.DB.SetTrashbox(Task,DomainID,UserID,ItemID);
    end;
  end;
  ItemID:=qID;
end;

class function  Folders.DB.Verify(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  qFolders                       : QWord;
begin
  Result:=False;
  qFolders:=0;
  Try
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,ItemID,Commands);
    Result:=Core.Database.SQL.Count(Task,@Commands,qFolders) and (qFolders>0);
  finally
    Core.Database.Done(Commands);
  end;
end;

function  Consumption(Task:Core.Database.Types.TTask; Var DomainID,UserID,Value:QWord):boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; iCount:=0;
  Try
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Size,poNone,oNone,Commands);
    end;
    Result:=Core.Database.SQL.Sum(Task,@Commands,Value);
  finally
    Core.Database.Done(Commands);
  end;
end;

class function  Folders.toXML(var Item:TFolders; Output:TMemoryStream; Header:Boolean):boolean;
var
  iLcv:LongInt;
  sItem:Core.Strings.VarString;
begin
  Result:=False;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to high(Item) do
    toXML(Item[iLcv]^,Output,XML_HEADER_OFF);

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Items,Output);
  Core.Streams.Write('>',1,Output);

  Result:=true;
end;

class function  Folders.toXML(var Item:TFolder; Output:TMemoryStream; Header:boolean):boolean;
begin
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.ID,Item.ID),Output);
    Core.Streams.Write(Print(XML.Path,Item.Path),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanzas.Item,Output);
  Core.Streams.Write('>',1,Output);
end;

class function  Folders.fromXML(xDoc:TXMLDocument; var Item:TFolder):boolean;
var
  xItem:TDOMNode;
begin
  Result:=False;
  xItem:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Item);
  if (xItem<>nil) then begin
    with Core.XML.DB do begin
      Item.Verified:=true;
      Item.ID:=toQWord(xItem,XML.ID);
      Item.Path:=toString(xItem,XML.Path);
      Result:=True;
    end;
  end;
end;

class function  Folders.fromXML(xDoc:TXMLDocument; var Items:TFolders):boolean;
var
  xItems:TDOMNode;
  xItem:TDOMNode;
  iCount,iLcv:LongInt;
  itmP:PFolder;
  iID:QWOrd;
begin
  Result:=False;
  xItems:=Core.XML.DB.getNode(xDoc,XML.Stanzas.Items);
  Invalidate(Items);
  if (xItems<>nil) then begin
    for iLcv:=0 to xItems.ChildNodes.Count-1 do begin
      xItem:=xItems.ChildNodes[iLcv];
      iID:=Core.XML.DB.toQWord(xItem,XML.ID);
      itmP:=getFolder(iID,Items);
      if (itmP=nil) then begin
        new(itmP);
        Init(itmP^);
        iCount:=System.Length(Items);
        SetLength(Items,iCount+1);
        Items[iCount]:=itmP;
      end;
      itmP^.Verified:=true;
      itmP^.ID:=iID;
      itmP^.Path:=Core.XML.DB.toString(xItem,XML.Path);
      Result:=True;
    end;
  end;
end;

class procedure Folders.Empty(Var Item:TFolder; const FreeFiles:boolean);
begin
  Item.Verified:=false;
  Item.Inspected:=0;
  Item.ID:=0;
  Item.Manifest:=nil;
  if (FreeFiles=true) then begin
    Files.Empty(Item.Files);
  end else begin
    Files.Clear(Item.Files);
  end;
  SetLength(Item.Path,0);
end;

class procedure Folders.Empty(Var Item:TFolders; const FreeFiles:boolean);
var
  iLcv:LongInt;
  itmP:PFolder;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^,FreeFiles);
    Dispose(itmP);
  end;
  SetLength(Item,0);
end;

class procedure Folders.Invalidate(var Item:TFolders);
var
  iLcv:LongInt;
  itmP:PFolder;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    if (itmP<>nil) then
      itmP^.Verified:=false;
  end;
end;

class procedure Folders.Pack(var Items:TFolders);
var
  iLcv,jLcv:LongInt;
  iCt:LongInt;
  iStartCt:LongInt;
begin
  iStartCt:=System.Length(Items);
  iCt:=iStartCt;
  iLcv:=0;
  while (iLcv<iCt) do begin
    if Items[iLcv]=nil then begin
      for jLcv:=iLcv to iCt-2 do
        Items[jLcv]:=Items[jLcv+1];
      Dec(iCt);
    end else
      Inc(iLcv);
  end;
  if (iStartCt<>iCt) then
    System.SetLength(Items,iCt);
end;


class procedure Folders.Purge(var Item:TFolders);
var
  iLcv,jLcv:LongInt;
  fldrP:PFolder;
begin
  for iLcv:=0 to High(Item) do begin
    fldrP:=Item[iLcv];
    if (fldrP^.Verified=false) then begin
      Folders.Done(fldrP^,FREE_FILES);
      Dispose(fldrP);
      Item[iLcv]:=nil;
    end;
  end;
  Pack(Item);
end;

class procedure Folders.Purge(var Item:TFolders; Depth:LongInt);
var
  saPath:Core.Arrays.Types.VarString;
  fldrP:PFolder;
  iCount:LongInt;
  iLcvDepth:LongInt;
  iBase:LongInt;
  iLcv:LongInt;
begin
  iCount:=Length(Item);
  if iCount>0 then begin
    fldrP:=Item[0]; // first one determines level
    iBase:=Core.Arrays.VarString.fromString(saPath,fldrP^.Path,'/');
    fldrP^.Verified:=true;
    for iLcv:=1 to iCount-1 do begin
      fldrP:=Item[iLcv];
      iLcvDepth:=Core.Arrays.VarString.fromString(saPath,fldrP^.Path,'/');
      fldrP^.Verified:=((iLcvDepth-iBase)<=Depth);
    end;
  end;
  Core.Arrays.VarString.Done(saPath);
  Purge(Item);
end;

class procedure Folders.Init(Var Item:TFolder);
begin
  Item.Verified:=false;
  Item.ID:=0;
  Item.Inspected:=0;
  Item.Manifest:=nil;
  Files.Init(Item.Files);
  SetLength(Item.Path,0);
end;

class procedure Folders.Init(Var Item:TFolders);
var
  iLcv:LongInt;
  itmP:PFolder;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^,true);
    Dispose(itmP);
  end;
  SetLength(Item,0);
end;

class procedure Folders.Done(Var Item:TFolder; const FreeFiles:boolean);
begin
  if FreeFiles=true then begin
    Files.Done(Item.Files);
  end else begin
    Files.Clear(Item.Files);
    Finalize(Item.Files);
  end;
  Finalize(Item.Path);
  Finalize(Item);
end;

class procedure Folders.Done(Var Items:TFolders; const FreeFiles:boolean);
var
  iLcv:LongInt;
  itmP:PFolder;
begin
  for iLcv:=0 to High(Items) do begin
    itmP:=Items[iLcv];
    Done(itmP^,FreeFiles);
    Dispose(itmP);
  end;
  System.SetLength(Items,0);
  Finalize(Items);
end;

class function Folders.Create(var Items:TFolders):PFolder;
var
  iCt:LongInt;
  fldr:PFolder;
begin
  iCt:=System.Length(Items);
  new(fldr);
  Init(fldr^);
  SetLength(Items,iCt+1);
  Items[iCt]:=fldr;
  REsult:=fldr;
end;

class function Files.DB.Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID,ItemID:QWord; Name:Core.Strings.VarString; dtCreated:Double=0; iKind:LongInt=0; iFlags:LongInt=0): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Allocated                      : Double;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(LongInt));
    if dtCreated=0 then
      dtCreated:=Core.Timer.dtUT;
    Allocated:=Allocate_Waiting;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.FolderID,poNone,oNone,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,iKind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Flags,poNone,oNone,iFlags,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,dtCreated,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,dtCreated,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Allocated,poNone,oNone,Allocated,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Name,poNone,oNone,Name,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);

    if ItemID<>0 then
      Storage.AuraDisks.Files.Create(Node,DomainID,UserID,FolderID,Storage.AuraDisks.Kinds.User,ItemID);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Create(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID,ItemID:QWord; var Name,Data:Core.Strings.VarString): Boolean;
var
  iReset                         : QWord;
  iInsertID                      : QWord;
  iCount                         : LongInt;
  Kind                           : LongInt;
  Flags                          : LongInt;
  Size                           : QWord;
  Commands                       : Core.Database.Types.Commands;
  dtNow                          : Double;
  ctxMD5                         : TMD5Context;
  dgMD5                          : TMD5Digest;
begin
  Result:=False;
  Try
    iCount:=0; iReset:=0;
    Core.Database.Empty(Commands);
    iInsertID:=Random(High(LongInt));
    dtNow:=Core.Timer.dtUT;
    Kind:=Storage.UserStorage.Kind.Bin;
    Flags:=0;
    Size:=System.Length(Data);
    FillChar(dgMD5,SizeOf(dgMD5),#0);
    if Size>0 then begin
      MD5.MD5Init(ctxMD5);
      MD5.MD5Update(ctxMD5,Data[1],Size);
      MD5.MD5Final(ctxMD5,dgMD5);
    end;
    Core.Database.AddCommand(iCount,TableP,@Commands);
    // Setup Primary ID
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.InsertID,poNone,oNone,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.InsertID,poNone,oEqual,iInsertID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForPrimaryID,IDs.ID,poNone,oNone,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForResetInsertID,IDs.InsertID,poNone,oNone,iReset,Commands);

    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.DomainID,poNone,oNone,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.UserID,poNone,oNone,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.FolderID,poNone,oNone,FolderID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Kind,poNone,oNone,Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Flags,poNone,oNone,Flags,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Size,poNone,oNone,Size,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Created,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Modified,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Allocated,poNone,oNone,dtNow,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Digest,poNone,oNone,dgMD5,Commands);
    Core.Database.AddCommand(iCount,TableP,useForInsert,IDs.Name,poNone,oNone,Name,Commands);

    Result:=Core.Database.SQL.Insert(Task,@Commands);
    if ItemID<>0 then
      Storage.AuraDisks.Files.Create(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User,Data);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.SetDigest(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Size:QWord; var Digest:TMD5Digest): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  Stamp                          : Double;
begin
  Result:=False;
  Try
    iCount:=0;
    Stamp:=Core.Timer.dtUT;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Digest,poNone,oNone,Digest,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Size,poNone,oNone,Size,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class function Files.DB.SetCreatedStamp(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Stamp:Double): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Created,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.SetModifiedStamp(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Stamp:Double): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.SetAllocatedStamp(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Stamp:Double): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Allocated,poNone,oNone,Stamp,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.SetSummary(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Summary:Core.Strings.VarString; Flags:LongInt): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  dtModified                     : Double;
begin
  Result:=False;
  Try
    iCount:=0;
    dtModified:=Core.Timer.dtUT;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Summary,poNone,oNone,Summary,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dtModified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Flags,poNone,oNone,Flags,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.SetFlags(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; Flags:LongInt): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  dtModified                     : Double;
begin
  Result:=False;
  Try
    iCount:=0;
    dtModified:=Core.Timer.dtUT;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Flags,poNone,oNone,Flags,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dtModified,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.GetFlags(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; out Flags:LongInt): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Flags,poNone,oNone,Flags,Commands);
    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Get_Flags,@Flags);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.GetSummary(Task:Core.Database.Types.TTask; var DomainID,UserID,ItemID:QWord; var Value:Core.Strings.VarString): Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);

    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Get_Summary,@Value);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class procedure Files.Paginate(var Items,Refactor:TItems; ItemsPerPage,Page:LongInt);
var
  iPCount,iRCount,iCount:LongInt;
  iLcv:LongInt;
  iStart:LongInt;
  iEnd:LongInt;
  Pages:LongInt;

  procedure PushAnswer;
  begin
    iPCount+=1;
    iRCount+=1;
    SetLength(Refactor,iRCount);
    Items[iLcv]^.Valid:=true;
    Refactor[iRCount-1]:=Items[iLcv];
  end;

begin
  SetLength(Refactor,0);
  iRCount:=0;
  iPCount:=0;
  iCount:=Length(Items);
  Pages:=iCount div ItemsPerPage;
  if (iCount mod ItemsPerPage)>0 then
    Pages+=1;

  iStart:=iCount-(ItemsPerPage*Page);
  if iStart<0 then
    iStart:=0;

  iEnd:=iStart+ItemsPerPage-1;
  if iEnd>iCount-1 then
    iEnd:=iCount-1;

  for iLcv:=0 to iCount-1 do begin
    if  (Items[iLcv]^.Flags or Storage.UserStorage.Items.IMAP.Flags.Pinned=Items[iLcv]^.Flags) then begin
      iRCount+=1;
      SetLength(Refactor,iRCount);
      Items[iLcv]^.Valid:=true;
      Refactor[iRCount-1]:=Items[iLcv];
    end else
      Items[iLcv]^.Valid:=false;
  end;
  for iLcv:=0 to iCount-1 do begin
    if (
      (Items[iLcv]^.Valid=false) and
      ((iLcv>=iStart) and (iLcv<=iEnd))
    ) then begin
      PushAnswer();
      if (iPCount>=ItemsPerPage) then
        Break;
    end;
  end;
  if iPCount<ItemsPerPage then begin
    for iLcv:=0 to iCount-1 do begin
      if (
        (Items[iLcv]^.Valid=false) and
        (Items[iLcv]^.Flags or Storage.UserStorage.Items.IMAP.Flags.Seen<>Items[iLcv]^.Flags)
      ) then begin
        PushAnswer();
        if (iPCount>=ItemsPerPage) then
          Break;
      end;
    end;
  end;

end;

class function Files.DB.Write(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Var Item:TItem; const UpdateStamp:Boolean):Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;

    Core.Database.Empty(Commands);

    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    if (UpdateStamp=true) then begin
      Item.Modified:=Core.Timer.dtUT;
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Item.Modified,Commands);
    end;

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Allocated,poNone,oNone,Item.Allocated,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Kind,poNone,oNone,Item.Kind,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Flags,poNone,oNone,Item.Flags,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Size,poNone,oNone,Item.Size,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Summary,poNone,oNone,Item.Summary,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Digest,poNone,oNone,Item.Digest,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Refresh(Task:Core.Database.Types.TTask; Var DomainID,UserID:QWord; var Item:TItem):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Empty(Item);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Modified,poAnd,oNotEqual,Item.Modified,Commands);

    {$i Storage.UserStorage.Files.Fields.inc}
    Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Summary,poNone,oNone,Commands);


    Result:=Core.Database.SQL.Select(Task,@Commands,@cb_Storage_Files_Fill,@Item);
  Finally
    Core.Database.Done(Commands);
  End;
end;


class function Files.DB.Rename(Task:Core.Database.Types.TTask; var DomainID,UserID:QWord; Var Item:TItem):Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Item.Modified:=Core.Timer.dtUT;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Item.ID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,Item.Modified,Commands);
    Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,Item.Name,Commands);

    Result:=Core.Database.SQL.Update(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function Files.DB.Delete(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; var DomainID,UserID,FolderID,ItemID:QWord): Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    iCount:=0;
    Core.Database.Empty(Commands);
    Core.Database.AddCommand(iCount,TableP,@Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,ItemID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
    Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    Result:=Core.Database.SQL.Delete(Task,@Commands);
    if Result then
      Result:=Storage.AuraDisks.Files.Delete(Node,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User);
  Finally
    Core.Database.Done(Commands);
  End;
end;

Function Execute_POP3_Clear(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; DomainID,UserID,FolderID:QWord):Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
  TableP                         : Core.Database.Types.PTable;
  IDs                            : Core.Arrays.Types.LargeWord;
  iLcv                           : LongInt;
begin
  Result:=False;
  iKind:=Storage.UserStorage.Kind.SMTP;
  Files.DB.List(Task,DomainID,UserID,FolderID,IDs);
  Try
    for iLcv:=0 to High(IDs) do
      Storage.AuraDisks.Files.Delete(Node,DomainID,UserID,FolderID,IDs[iLcv],Storage.AuraDisks.Kinds.User);
  finally
    Core.Arrays.LargeWord.Done(IDs);
  end;
  Try
    iCount:=0;
    Empty(Commands);
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poNone,oEqual,iKind,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
    end;
    Result:=Core.Database.SQL.Delete(Task,@Commands);
  Finally
    Core.Database.Done(Commands);
  End;
end;

function FillPOP3RetrRequest(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID,ItemID:QWord; Var RR: TPOP3RetrRequest):Boolean;
begin
  Empty(RR);
  RR.FileID:=ItemID;
  RR.UserID:=UserID;
  RR.FolderID:=FolderID;
  RR.DomainID:=DomainID;
  Result:=Storage.AuraDisks.Files.Read(Task,DomainID,UserID,FolderID,ItemID,Storage.AuraDisks.Kinds.User,RR.Data);
end;

procedure CB_FillPOP3UIDLRequest(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PPOP3UIDLRequest(DataP)^.Data:=Fields.FieldByName(Files.DB.Keys.Name).AsString;
end;

function FillPOP3UIDLRequest(Task:Core.Database.Types.TTask; Var UR: TPOP3UIDLRequest):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False; Empty(UR.Data);
  Try
    iCount:=0;
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,UR.Field_ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Name,poNone,oEqual,Commands);
    end;
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillPOP3UIDLRequest,@UR);
  Finally
    Core.Database.Done(Commands);
  End;
end;


function FillPOP3TopRequest(Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item; Var TR:TPOP3TopRequest):Boolean;
begin
  SetLength(TR.Data,0);
  Result:=Storage.AuraDisks.Files.Read(Node,TR.DomainID,TR.UserID,TR.FolderID,TR.ID,Storage.AuraDisks.Kinds.User,TR.Data);
end;

procedure CB_FillStorageRequestDomainID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  With PFileStorageRequest(DataP)^ do begin
    Exists:=True;
    DomainID:=Fields.FieldByName(Storage.Domains.Items.DB.Keys.ID).AsLargeInt;
  end;
end;

procedure CB_FillStorageRequestUserByName(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  with PFileStorageRequest(DataP)^ do begin
    UserID:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.ID).AsLargeInt;
    InboxID:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Inbox).AsLargeInt;
    Enabled:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Enabled).AsBoolean;
    Forwarding:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Forwarding).AsBoolean;
    Quota:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Quota).AsLargeInt;
    Consumption:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Consumption).AsLargeInt;
    ForwardingAddress:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.ForwardAddress).AsString;
    Exists:=True;
  end;
end;

procedure CB_FillStorageRequestUserByID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  with PFileStorageRequest(DataP)^ do begin
    Enabled:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Enabled).AsBoolean;
    InboxID:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Inbox).AsLargeInt;
    Forwarding:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Forwarding).AsBoolean;
    Quota:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Quota).AsLargeInt;
    Consumption:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.Consumption).AsLargeInt;
    ForwardingAddress:=Fields.FieldByName(Storage.UserAccounts.Items.DB.Keys.ForwardAddress).AsString;
    Exists:=True;
  end;
end;

procedure CB_FillStorageRequestFolderID(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
begin
  PQWord(DataP)^:=Fields.FieldByName(Folders.DB.Keys.ID).AsLargeInt;
end;

Function  FillStorageRequest(Task:Core.Database.Types.TTask; Var SR:TFileStorageRequest):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  sCheck                         : Core.Strings.VarString;
begin
  Result:=False; SR.Exists:=False; SR.InboxID:=0;
  Try
    if SR.DomainID=0 then begin
      iCount:=0;
      Empty(Commands);
      Core.Database.AddCommand(iCount,Storage.Domains.Items.DB.TableP,@Commands);
      Core.Database.AddCommand(iCount,Storage.Domains.Items.DB.TableP,useForCriteria,Storage.Domains.Items.DB.IDs.Name,poNone,oEqual,SR.DomainName,Commands);
      Core.Database.AddCommand(iCount,Storage.Domains.Items.DB.TableP,useForFields,Storage.Domains.Items.DB.IDs.ID,poNone,oNone,Commands);
      Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillStorageRequestDomainID,@SR);
    end else begin
      Result:=True;
      SR.Exists:=True;
    end;
    If Result and SR.Exists then begin
      SR.Exists:=False;
      iCount:=0;
      Empty(Commands);
      With Storage.UserAccounts.Items.DB do begin
        Core.Database.AddCommand(iCount,TableP,@Commands);
        Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,SR.DomainID,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Quota,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Inbox,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Consumption,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Enabled,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Forwarding,poNone,oNone,Commands);
        Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ForwardAddress,poNone,oNone,Commands);
        if (SR.UserID=0) then begin
          Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserName,poAnd,oEqual,SR.UserName,Commands);
          Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillStorageRequestUserByName,@SR);
        end else begin
          Core.Database.AddCommand(iCount,TableP,useForFields,IDs.ID,poNone,oNone,Commands);
          Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,SR.UserID,Commands);
          Result:=Core.Database.SQL.Select(Task,@Commands,@CB_FillStorageRequestUserByID,@SR);
        end;
        if (SR.UserID<>0) then begin
          // We have to fill with node of disk for storage
          Storage.AuraDisks.Router.Verify(Task,SR.DomainID,SR.UserID,Storage.AuraDisks.Kinds.User,SR.Node,SR.Router);

          sCheck:=Concat(Folders.Defaults.Home.Mail,'/',Folders.Defaults.Mail.Spam);
          Folders.DB.Verify(Task,SR.DomainID,SR.UserID,sCheck,Folders.FolderVerifiyFind.fvkSpambox,SR.SpamID);
        end;
      end;
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;


procedure Empty(Var Item:TPOP3UIDLAllRequest);
begin
  Empty(Item.Items);
  Item.Count:=0;
  Item.Size:=0;
  Item.UserID:=0;
end;

procedure Done(Var Item:TPOP3UIDLAllRequest);
begin
  Done(Item.Items);
  Finalize(Item);
end;

procedure Empty(Var Item:TPOP3UIDLRequest);
begin
  Item.Field_ID:=0;
  Item.User_ID:=0;
  SetLength(Item.Data,0);
end;

procedure Done(Var Item:TPOP3UIDLRequest);
begin
  Finalize(Item.Data);
  Finalize(Item);
end;


procedure Empty(Var Item:TPOP3TopRequest);
begin
  Item.ID:=0;
  Item.FolderID:=0;
  Item.UserID:=0;
  Item.DomainID:=0;
  SetLength(Item.Data,0);
end;

procedure Done(Var Item:TPOP3TopRequest);
begin
  Finalize(Item.Data);
  Finalize(Item);
end;

procedure Copy(Var Source,Destination:TFileStorageRequest);
begin
  Destination.UserID:=Source.UserID;
  Destination.DomainID:=Source.DomainID;
  Destination.InboxID:=Source.InboxID;
  Destination.SpamID:=Source.SpamID;
  Destination.Quota:=Source.Quota;
  Destination.Consumption:=Source.Consumption;
  Destination.Exists:=Source.Exists;
  Destination.Enabled:=Source.Enabled;
  Destination.Forwarding:=Source.Forwarding;
  Destination.Kind:=Source.Kind;
  Destination.Username:=Source.Username;
  Destination.DomainName:=Source.DomainName;
  Destination.ForwardingAddress:=Source.ForwardingAddress;

  Storage.MatrixNodes.Node.Copy(Source.Node,Destination.Node);
  Storage.AuraDisks.Router.Copy(Source.Router,Destination.Router);
end;

procedure Empty(Var Item:TFileStorageRequest);
begin
  Item.UserID:=0;
  Item.InboxID:=0;
  Item.SpamID:=0;
  Item.Forwarding:=False;
  Item.DomainID:=0;
  Item.Enabled:=false;
  Item.Exists:=False;
  Item.Kind:=0;
  Item.Quota:=0;
  Item.Consumption:=0;
  SetLength(Item.UserName,0);
  SetLength(Item.DomainName,0);
  SetLength(Item.ForwardingAddress,0);

  Storage.MatrixNodes.Node.Empty(Item.Node);
  Storage.AuraDisks.Router.Empty(Item.Router);
end;

procedure Done(Var Item:TFileStorageRequest);
begin
  Finalize(Item.UserName);
  Finalize(Item.DomainName);
  Finalize(Item.ForwardingAddress);
  Storage.MatrixNodes.Node.Done(Item.Node);
  Storage.AuraDisks.Router.Done(Item.Router);
  Finalize(Item);
end;

procedure Empty(Var Item:TPOP3RetrRequest);
begin
  Item.UserID:=0;
  Item.FileID:=0;
  Item.FolderID:=0;
  Item.DomainID:=0;
  SetLength(Item.Data,0);
end;

procedure Done(Var Item:TPOP3RetrRequest);
begin
  Finalize(Item.Data);
  Finalize(Item);
end;

procedure Copy(var Source,Destination:TUserSMTPRelayRequest);
begin
  Destination.UserName:=Source.UserName;
  Destination.DomainID:=Source.DomainID;
  Destination.UserID:=Source.UserID;
  Destination.LastIP:=Source.LastIP;
  Destination.Enabled:=Source.Enabled;
  Destination.Exists:=Source.Exists;
end;

procedure Empty(Var Item:TUserSMTPRelayRequest);
begin
  with Item do begin
    SetLength(UserName,0);
    DomainID:=0;
    UserID:=0;
    LastIP:=0;
    Enabled:=false;
    Exists:=false;
  end;
end;

procedure Empty(Var Item:TPOP3UIDLAllItem);
begin
  Item.ID:=0;
  Item.Size:=0;
  Item.Deleted:=false;
  SetLength(Item.UID,0);
end;

procedure Done(Var Item:TPOP3UIDLAllItem);
begin
  Finalize(Item.UID);
  Finalize(Item);
end;

procedure Empty(Var Item:TPOP3UIDLAllItems);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;

procedure Done(Var Item:TPOP3UIDLAllItems);
var
  iLcv:LongInt;
begin
  For iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

Function GenerateByte:byte;
const
  xml:Array[0..4] of byte=(34,38,39,60,62);
  xReplace:Array[0..4] of byte=(32,33,35,36,37);
var
  idx:LongInt;
  iLcv:LongInt;
begin
  idx:=-1;
  Result:= 33 + Random(127-33);
  for iLcv:=low(xml) to high(xml) do begin
    if (xml[iLcv]=Result) then
      idx:=iLcv;
  end;
  if (idx<>-1) then
    Result:=xReplace[idx];
end;

Function GenerateUID:Core.Strings.VarString;
const
  MAX_ROTATE = 4;
  MAX_CHARS = 10;
  Punct : Array[0..MAX_CHARS-1] of Char=(#35,#36,#37,#38,#39,#40,#41,#45,#46,#95);
var
  iLcv:LongInt;
begin
  for iLcv:=low(TUID) to high(TUID) do begin
    case Random(MAX_ROTATE) of
      0: Result[iLcv] := Char(  Random(10)  + 48 ); //48-57
      1: Result[iLcv] := Char(  Random(26)  + 65 ); //65-90
      2: Result[iLcv] := Char(  Random(26)  + 97 ); //97-122
      3: Result[iLcv] := Punct[Random(MAX_CHARS)];
    end;
  end;
end;

class procedure Items.SMTP.Encoding.Prepare(var Data:Core.Strings.VarString);
var
  iLcv:LongInt;
  iLen:LongInt;
begin
  iLen:=Length(Data);
  while (iLen>0) and (Data[iLen] in [#13,#10]) do
    Dec(iLen);
  System.SetLength(Data,iLen);
end;

class function  Items.SMTP.ExtractSenderDomain(Var Value:Core.Strings.VarString):Core.Strings.VarString;
var
  iLen:LongInt;
begin
  // EHELO [122.33.32.33]
  Result:=Lowercase(Value);
  iLen:=System.Length(Value);
  if (iLen>0) then begin
    if Result[1]='[' then begin
      System.Delete(Result,1,1);
      Dec(iLen);
      if (iLen>0) and (Result[iLen]=']') then begin
        Dec(iLen);
        System.SetLength(Result,iLen);
      end;
    end;
  end;
end;

//class function  Items.SMTP.UnwrapHeaders(Var Value:Core.Strings.VarString):Core.Strings.VarString;
function  UnwrapHeaders(Var Value:Core.Strings.VarString):Core.Strings.VarString;
begin
  Result:=StringReplace(Value,#13#10#9,#32,[rfReplaceAll]);
  Result:=StringReplace(Result,#13#10#32,#32,[rfReplaceAll]);
  Result:=StringReplace(Result,#9,#32,[rfReplaceAll]);
  Result:=StringReplace(Result,#32#32,#32,[rfReplaceAll]);
  Result:=System.UTF8Encode(Result);
end;

function  ExtractHeaders(Var Content:Core.Arrays.Types.VarString; Const idxBreak:LongInt):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  SetLength(Result,0);
  for iLcv:=0 to idxBreak do
    Result:=Concat(Result,Content[iLcv],#13#10);
end;

function EndOfMessage(var Content:Core.Arrays.Types.VarString):boolean;
var
  iLen:LongInt;
begin
  iLen:=System.Length(Content);
  Result:=( (iLen>=1) and (Content[iLen-1]='.') );
end;

function IndexOfHeaderBreak(var Content:Core.Arrays.Types.VarString):LongInt;
var
  iLcv:LongInt;
begin
  Result:=-1;
  for iLcv:=0 to High(Content) do begin
    if Content[iLcv]='' then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

function  Examine(var Mime:Items.SMTP.TMime; Source:TStream;  out idxStart,idxStop,Length:Int64):boolean;
var
  iLcv:LongInt;
begin
  Result:=false;

  idxStart:=0; idxStop:=0; Length:=0;
  Source.Position:=0;
  for iLcv:=0 to Mime.idxContentStart-1 do
   idxStart:=Core.Streams.SkipLine(Source);
  idxStop:=idxStart;
  for iLcv:=Mime.idxContentStart to Mime.idxContentEnd do
    idxStop:=Core.Streams.SkipLine(Source);

  Length:=idxStop-idxStart;

  Result:=true;
end;

class function Items.SMTP.HasAttachments(var Summary:TSummary):boolean;

 function getHasMimes(ParentP:PMime):boolean;
 var
   iLcv:LongInt;
 begin
   Result:=false;
   If (
     (ParentP^.cntDisposition=Content.Disposition.Attachment) and
     (ParentP^.cntType<>Items.SMTP.Content.ctPGPSignature)
   ) then begin
     if (Length(ParentP^.cntName)>0) then
       Result:=true;
   end else begin
     for iLcv:=0 to High(ParentP^.Mimes) do begin
       if (getHasMimes(ParentP^.Mimes[iLcv])=true) then begin
         Result:=true;
         break;
       end;
     end;
   end;
 end;

begin
  Result:=getHasMimes(@Summary.Mime);
end;

class procedure Items.SMTP.offsetMimes(var Summary:TSummary; Count:LongInt);
var
  iLcv:LongInt;

  Procedure ProcessMime(ParentP:PMime);
  var
    mimeP:Items.SMTP.PMime;
    iLcv:LongInt;
  begin
    ParentP^.idxHeadersStart+=Count;
    ParentP^.idxHeadersEnd+=Count;
    ParentP^.idxContentStart+=Count;
    ParentP^.idxContentEnd+=Count;
    for iLcv:=0 to High(ParentP^.Mimes) do
      ProcessMime(ParentP^.Mimes[iLcv]);
  end;

begin
  // Main Message Adjustment
  //Mail.Mime.idxHeadersStart+=0;
  Summary.Mime.idxHeadersEnd+=Count;
  Summary.Mime.idxContentStart+=Count;
  Summary.Mime.idxContentEnd+=Count;
  For iLcv:=0 to High(Summary.Mime.Mimes) do
    ProcessMime(Summary.Mime.Mimes[iLcv]);
end;

function  Mimes(var Headers:Core.Arrays.Types.KeyStrings; var Content:Core.Arrays.Types.VarString; var Summary:Items.SMTP.TSummary; Refactor:TStream):Boolean;
var
  iBodyCount  : LongInt;
  blkHeaders  : Core.Arrays.Types.KeyStrings;
  blkContent  : Core.Arrays.Types.VarString;

  procedure GetParagraphs(var Data:Core.Strings.VarString; var Lines:Core.Arrays.Types.VarString);
  var
    iOffset   : LongInt;
    StartLoc  : LongInt;
    sEntry    : Core.Strings.VarString;

    PStart    : LongInt;
    PEnd      : LongInt;
    sCopy     : Core.Strings.VarString;
    iCopyLen  : LongInt;
    sTag      : Core.Strings.VarString;

  begin
    iOffset:=1;
    sCopy:=Lowercase(Data);
    iCopyLen:=Length(sCopy);
    repeat
      StartLoc:=Core.Strings.PosEx('<p',sCopy,iOffset,iCopyLen);
      If (StartLoc>0) then begin
        if (sCopy[StartLoc+2] in [#32,#62]) then begin
          PStart:=Core.Strings.PosEx('>',sCopy,StartLoc+1,iCopyLen)+1;
          PEnd:=Core.Strings.PosEx('</p',sCopy,PStart,iCopyLen);
          if ( (PEnd=0) or Not(sCopy[PEnd] in [#32,#62])) then
            PEnd:=Core.Strings.PosEx('<p',sCopy,PStart+1,iCopyLen);
          if (PEnd=0) then
            PEnd:=iCopyLen;
          if ((PStart>0) and (PEnd>0)) then begin
            sEntry:=System.Copy(Data,PStart,PEnd-PStart);
            Core.Arrays.VarString.Add(Lines,sEntry,[]);
            iOffset:=PEnd+3;
          end else
            iOffset:=StartLoc+2;
        end else
          iOffset:=StartLoc+2;
      end;
    until (StartLoc=0);
  end;


  procedure processHeaders(var mimeP:Items.SMTP.PMime; sParentBoundary:Core.Strings.VarString);
  var
    blkHdrCount:LongInt;
    iLcv:LongInt;
    sHeaders:Core.Strings.VarString;
  begin
    Core.Arrays.VarString.Empty(blkContent);
    Core.Arrays.KeyString.Empty(blkHeaders);
    SetLength(sHeaders,0);
    iLcv:=mimeP^.idxHeadersStart;
    While (
      (iLcv<=mimeP^.idxHeadersEnd) and
      (iLcv<iBodyCount) and
      (Length(Content[iLcv])>0) and
      (Core.Strings.Pos(sParentBoundary,Content[iLcv])=0)
    )do begin
      sHeaders:=Concat(sHeaders,Content[iLcv],#13#10);
      Inc(iLcv);
    end;
    sHeaders:=UnwrapHeaders(sHeaders); // undo wordwrap
    blkHdrCount:=Core.Arrays.KeyString.fromString(blkHeaders,sHeaders,': ',#13#10,[soClearList]);

    mimeP^.cntType:=getType(blkHeaders,mimeP^.cntBoundary,blkHdrCount,mimeP^.cntCharSet,mimeP^.cntCharFormat,mimeP^.cntName);
    mimeP^.cntID:=Core.Arrays.KeyString.GetItemByKey(blkHeaders,'Content-ID',blkHdrCount);


    mimeP^.cntDisposition:=getDisposition(blkHeaders,blkHdrCount,mimeP^.cntName,mimeP^.cntCreated,mimeP^.cntModified,mimeP^.cntRead,mimeP^.cntSize);
    mimeP^.cntDescription:=Core.Arrays.KeyString.GetItemByKey(blkHeaders,'Content-Description',blkHdrCount);
    mimeP^.cntEncoding:=getTransferEncoding(blkHeaders,blkHdrCount);
  end;

  function processMultiParts(iBodyIndex:LongInt; var Parent:Items.SMTP.TMime):LongInt;
  var
    iBndryLen:LongInt;
    iBndryLoc:LongInt;
    iBodyBndryLoc:LongInt;
    iLineLen:LongInt;
    iLcv:LongInt;
    iLen:LongInt;
    mimeP:Items.SMTP.PMime;
    ParentP:Items.SMTP.PMime;
    bLastOfBoundary:boolean;
    sBndryEnd:Core.Strings.VarString;
  begin
    Result:=iBodyIndex;
    mimeP:=nil;
    ParentP:=@Parent;
    iBndryLen:=System.Length(Parent.cntBoundary);
    // seek to new section
    while (iBodyIndex<iBodyCount) and (iBodyIndex<=Parent.idxContentEnd) and (Content[iBodyIndex]='') do
      inc(iBodyIndex);
    While (iBodyIndex<iBodyCount) do begin
      iLineLen:=System.Length(Content[iBodyIndex]);
      iBodyBndryLoc:=Core.Strings.Pos(Parent.cntBoundary,Content[iBodyIndex]);
      if (iBodyBndryLoc=3) then begin
        bLastOfBoundary:=  ( (iLineLen>(iBodyBndryLoc+iBndryLen)) and (Content[iBodyIndex][iLineLen-1]='-') and (Content[iBodyIndex][iLineLen]='-') );
        if (bLastOfBoundary=false) then begin
          new(mimeP);
          Items.SMTP.Init(mimeP^);
          Items.SMTP.Append(mimeP,ParentP^.Mimes);

          mimeP^.idxHeadersStart:=iBodyIndex+1;
          iBodyIndex:=mimeP^.idxHeadersStart;
          iBndryLoc:=0;
          while (
            (iBodyIndex<iBodyCount) and
            (Content[iBodyIndex]<>'') and
            (iBndryLoc=0)
          ) do begin
            iBndryLoc:=Core.Strings.Pos(Parent.cntBoundary,Content[iBodyIndex]);
            if (iBndryLoc=0) then begin
              inc(iBodyIndex);
              iBndryLoc:=Core.Strings.Pos(Parent.cntBoundary,Content[iBodyIndex]);
              if iBndryLoc>0 then
                Dec(iBodyIndex);
            end;
          end;
          mimeP^.idxHeadersEnd:=iBodyIndex;
          mimeP^.idxContentStart:=iBodyIndex+1;
          processHeaders(mimeP,Parent.cntBoundary);
          iLen:=System.Length(mimeP^.cntBoundary);
          if (iLen>0) then begin
            // going to process multi-parts
            sBndryEnd:=Concat('--',mimeP^.cntBoundary,'--');
            mimeP^.idxContentEnd:=Core.Arrays.VarString.IndexOf(Content,sBndryEnd,mimeP^.idxContentStart);
            if (mimeP^.idxContentEnd=-1) then
              mimeP^.idxContentEnd:=iBodyCount-1;
            iBodyIndex:=processMultiParts(iBodyIndex,mimeP^);
          end else begin
            sBndryEnd:=Concat('--',Parent.cntBoundary);
            mimeP^.idxContentEnd:=Core.Arrays.VarString.IndexOf(Content,sBndryEnd,mimeP^.idxContentStart)-1;
            if mimeP^.idxContentEnd<0 then begin
              sBndryEnd:=Concat('--',Parent.cntBoundary,'--');
              mimeP^.idxContentEnd:=Core.Arrays.VarString.IndexOf(Content,sBndryEnd,mimeP^.idxContentStart)-1;
            end;
            if (mimeP^.idxContentEnd>0) then begin
              iBodyIndex:=mimeP^.idxContentEnd+1;
            end else begin
              iBodyIndex:=iBodyCount;
              mimeP^.cntLast:=true;
              mimeP^.idxContentEnd:=iBodyCount-1;
            end;
          end;
        end else begin
          if mimeP<>nil then
            mimeP^.cntLast:=true
          else
            Parent.cntLast:=true;
          Result:=iBodyIndex;
          Exit();
        end;
      end else begin
        Inc(iBodyIndex);
      end;
    end;
    Result:=iBodyIndex;
  end;

  procedure processReadableSize(var Mime:Items.SMTP.TMime);
  var
    iLcv:LongInt;
    bOld:LongInt;
  begin
    Mime.iReadableSize:=0;
    for iLcv:=Mime.idxContentStart to Mime.idxContentEnd do
      Mime.iReadableSize+=Length(Content[iLcv])+2;

    for iLcv:=0 to High(Mime.Mimes) do
      processReadableSize(Mime.Mimes[iLcv]^);
  end;

  procedure processMain();
  var
    mimeP       : Items.SMTP.PMIME;
    iHdrCount   : LongInt;
    iBodyIndex  : LongInt;
    sBndryEnd   : Core.Strings.VarString;
  begin
    Items.SMTP.Empty(Summary.Mime);
    mimeP:=@Summary.Mime;

    While  (mimeP^.idxHeadersEnd<iBodyCount) and (Content[mimeP^.idxHeadersEnd]<>'')  do
      Inc(mimeP^.idxHeadersEnd);
    mimeP^.idxContentStart:=mimeP^.idxHeadersEnd+1;
    mimeP^.idxContentEnd:=iBodyCount-1;
    iHdrCount:=System.Length(Headers);
    iBodyIndex:=mimeP^.idxContentStart;

    mimeP^.cntType:=getType(Headers,mimeP^.cntBoundary,iHdrCount,mimeP^.cntCharSet,mimeP^.cntCharFormat,mimeP^.cntName);
    mimeP^.cntDisposition:=getDisposition(Headers,iHdrCount,mimeP^.cntName,mimeP^.cntCreated,mimeP^.cntModified,mimeP^.cntRead,mimeP^.cntSize);
    mimeP^.cntDescription:=Core.Arrays.KeyString.GetItemByKey(Headers,'Content-Description',iHdrCount);
    mimeP^.cntEncoding:=getTransferEncoding(Headers,iHdrCount);
    if (Length(mimeP^.cntBoundary)>0) then begin
      sBndryEnd:=Concat('--',mimeP^.cntBoundary,'--');
      mimeP^.idxContentEnd:=Core.Arrays.VarString.IndexOf(Content,sBndryEnd,mimeP^.idxContentStart);
      if mimeP^.idxContentEnd<0 then
        mimeP^.idxContentEnd:=iBodyCount-1;
      processMultiParts(iBodyIndex, mimeP^);
    end;
    processReadableSize(mimeP^);
  end;

  procedure processSummaryLines();
  var
    mimeP    : Items.SMTP.PMIME;
    iLcv     : LongInt;
    iLen     : LongInt;
    iLineLcv : LongInt;
    iLineCt  : LongInt;
    iLineLen : LongInt;
    sChunk   : Core.Strings.VarString;
    saLines  : Core.Arrays.Types.VarString;
  begin
    mimeP:=Items.SMTP.getMime(Summary.Mime,Items.SMTP.Content.ctTextPlain);
    if ((mimeP=nil) or (mimeP^.iReadableSize<8)) then
      mimeP:=Items.SMTP.getMime(Summary.Mime,Items.SMTP.Content.ctTextHTML);
    iLen:=0;
    SetLength(Summary.Lines,0);
    if (mimeP<>nil) then begin
      Core.Arrays.VarString.Empty(saLines);
      SetLength(sChunk,0);
      for iLcv:=mimeP^.idxContentStart to mimeP^.idxContentEnd do begin;
        if System.Length(Content[iLcv])>0 then
          sChunk:=Concat(sChunk,Content[iLcv],#13#10);
      end;
      if (mimeP^.cntEncoding=Items.SMTP.Encoding.emBase64) then begin
        sChunk:=Encryption.Base64.Decode(sChunk);
        if (Core.Strings.Search(sChunk,#13)=0) and (Core.Strings.Search(sChunk,#10)>0) then
          Core.Strings.Replace(sChunk,#13,#13#10);
      end;
      sChunk:=StringReplace(sChunk,'='#13#10,'',[rfReplaceAll]);

      if (mimeP^.cntType=Items.SMTP.Content.ctTextHTML) then
        GetParagraphs(sChunk,saLines);

      if Length(saLines)=0 then
        Core.Arrays.VarString.fromString(saLines,sChunk,#13#10);

      iLineCt:=Length(saLines);
      if (iLineCt>Items.SMTP.MaxLines) then
        Core.Arrays.VarString.SetSize(saLines,Items.SMTP.MaxLines);
      for iLcv:=0 to High(saLines) do
        if Length(saLines[iLcv])>Items.SMTP.MaxLineLength then
          SetLength(saLines[iLcv],Items.SMTP.MaxLineLength);

      Summary.Lines:=Core.Arrays.VarString.toString(saLines,'<br>');
      Summary.Lines:=Encryption.Base64.Encode(Summary.Lines);

      Core.Strings.Done(sChunk);
      Core.Arrays.VarString.Done(saLines);
    end;
  end;

begin
  Result:=true;
  iBodyCount:=System.Length(Content);
  processMain();
  processSummaryLines();
end;

class function  Items.SMTP.Write(
  Task:Core.Database.Types.TTask;
  var DomainID,UserID:QWord;
  var Summary:TSummary;
  var Name:Core.Strings.VarString;
  Flags:Core.Database.Types.Integer
  ):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  dtModified                     : Double;
  sData                          : Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0;
    dtModified:=Core.Timer.dtUT;
    sData:=UTF8Encode(toXML(Summary));
    Core.Database.Empty(Commands);
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Summary.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dtModified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Flags,poNone,oNone,Flags,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Name,poNone,oNone,Name,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Summary,poNone,oNone,sData,Commands);

      Result:=Core.Database.SQL.Update(Task,@Commands);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.SMTP.Write(
  Task:Core.Database.Types.TTask;
  var DomainID,UserID:QWord;
  var Summary:TSummary;
  Flags:LongInt):Boolean;
var
  iCount                         : LongInt;
  Commands                       : Core.Database.Types.Commands;
  dtModified                     : Double;
  sData                          : Core.Strings.VarString;
begin
  Result:=False;
  Try
    iCount:=0;
    dtModified:=Core.Timer.dtUT;
    sData:=UTF8Encode(toXML(Summary));
    Core.Database.Empty(Commands);
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poNone,oEqual,Summary.ID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poAnd,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);

      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Modified,poNone,oNone,dtModified,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Flags,poNone,oNone,Flags,Commands);
      Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Summary,poNone,oNone,sData,Commands);

      Result:=Core.Database.SQL.Update(Task,@Commands);
    end;
  Finally
    Core.Database.Done(Commands);
  End;
end;


class procedure Items.SMTP.Stamp(
  var Summary:TSummary;
  var Headers:Core.Arrays.Types.KeyStrings;
  var Data:Core.Arrays.Types.VarString;
  Refactor:TStream
);
var
  kpI:Core.Strings.KeyString;
  iLcv:LongInt;
  iCt:LongInt;

  procedure AddData(sName,sValue:Core.Strings.VarString);
  begin
    kpI.Key:=sName;
    kpI.Value:=sValue;
    Core.Arrays.KeyString.Append(kpI,Headers);
  end;

  procedure InsertData(sName,sValue:Core.Strings.VarString);
  begin
    kpI.Key:=sName;
    kpI.Value:=sValue;
    Core.Arrays.KeyString.Insert(kpI,0,Headers);
  end;

  procedure UpdateData(sName,sValue:Core.Strings.VarString);
  var
    idxKey:LongInt;
  begin
    idxKey:=Core.Arrays.KeyString.IndexOf(Headers,sName);
    if idxKey<>-1 then begin
      Headers[idxKey]^.Value:=sValue;
    end else
      AddData(sName,sValue);
  end;

  procedure PushWrite(var Keyword:Core.Strings.KeyString);
  begin
    Core.Streams.Write(Keyword.Key,Refactor);
    Core.Streams.Write(': ',Refactor);
    Core.Streams.Write(Keyword.Value,Refactor);
    Core.Streams.Write(#13#10,Refactor);
  end;

begin
  Core.Arrays.KeyString.Init(kpI);
  Try
    kpI.Streams:=True;
    UpdateData('X-Remote-IP',Summary.RemoteIP);
    UpdateData('X-Sec-Filtered',YES_NO[Summary.Spam]);
    UpdateData('X-Sec-Black',YES_NO[Summary.Blacklisted]);
    UpdateData('X-Sec-White',YES_NO[Summary.WhiteListed]);
    UpdateData('X-Sec-Risk',IntToStr(Summary.Risk));
    UpdateData('X-Sec-ID',IntToStr(Summary.FilterID));

    Refactor.Size:=0;
    iCt:=Length(Headers);
    for iLcv:=0 to iCt-1 do begin
      PushWrite(Headers[iLcv]^);
    end;
    iCt:=Length(Data);
    for iLcv:=0 to iCt-1 do begin
      if Data[iLcv]='' then
        Break;
    end;
    for iLcv:=iLcv to iCt-1 do
      Core.Streams.WriteLine(Data[iLcv],Refactor);
    Refactor.Position:=0;
    Core.Arrays.VarString.fromStream(Data,Refactor);
  Finally
    Refactor.Size:=0;
    Core.Arrays.KeyString.Done(kpI);
  end;
end;

class procedure Items.SMTP.Update(
  var Summary:TSummary;
  var Headers:Core.Arrays.Types.KeyStrings;
  aDomain,aUser,aSenderIP,aSenderDomain,aSenderFrom,aExchanger:Core.Strings.VarString;
  Const aBound,aContentType:Byte
);
var
  iHdrCount:LongInt;
begin
  iHdrCount:=System.Length(Headers);
  Summary.Sender:=aSenderFrom;
  Summary.&To:=Core.Arrays.KeyString.GetItemByKey(Headers,'To',iHdrCount);
  Summary.CC:=Core.Arrays.KeyString.GetItemByKey(Headers,'CC',iHdrCount);
  Summary.From:=Core.Arrays.KeyString.GetItemByKey(Headers,'From',iHdrCount);
  Summary.Subject:=Core.Arrays.KeyString.GetItemByKey(Headers,'Subject',iHdrCount);
  Summary.ReplyTo:=Core.Arrays.KeyString.GetItemByKey(Headers,'Reply-To',iHdrCount);
  Summary.InReplyTo:=Core.Arrays.KeyString.GetItemByKey(Headers,'In-Reply-To',iHdrCount);
  Summary.MessageId:=Core.Arrays.KeyString.GetItemByKey(Headers,'Message-Id',iHdrCount);
  Summary.Group:=Core.Arrays.KeyString.GetItemByKey(Headers,'List-Id',iHdrCount);
  if (Length(Summary.Group)=0) then
    Summary.Group:=Core.Arrays.KeyString.GetItemByKey(Headers,'X-List-Name',iHdrCount);
  Summary.Group:=Core.Utils.Mail.ExtractListName(Summary.Group);


  Summary.Exchanger:=aExchanger;
  Summary.cntType:=aContentType;
  Summary.Date:=Core.Timer.dtNow;
  Summary.UTC:=Core.Timer.dtUT;
  Summary.tzBias:=Core.Utils.Time.BiasMinutes;
  Summary.Domain:=aDomain;
  Summary.User:=aUser;
  Summary.Bound:=aBound;
  Summary.RemoteIP:=aSenderIP;
  Summary.RemoteDomain:=aSenderDomain;
  Summary.RemoteFrom:=aSenderFrom;
end;

class procedure Items.SMTP.Update(
  var Summary:TSummary;
  Const aFilterID:QWord;
  Const aRead,aSent,aSpam,aBlackListed,aWhiteListed:Boolean
);
begin
  Summary.FilterID:=aFilterID;
  Summary.Spam:=aSpam;
  Summary.Read:=aRead;
  Summary.Sent:=aSent;
  Summary.BlackListed:=aBlackListed;
  Summary.WhiteListed:=aWhiteListed;
end;

procedure CB_SMTP_Stats(CommandsP:Core.Database.Types.PCommands; Fields:TFields; Const DataP:System.Pointer);
var
  ItemP:Items.SMTP.PStats;
begin
  ItemP:=DataP;
  Inc(ItemP^.Count,1);
  Inc(ItemP^.Size,Fields.FieldByName(Files.DB.Keys.Size).AsLongInt);
end;

function  getTransferEncoding(var Headers:Core.Arrays.Types.KeyStrings; iHdrCount:LongInt):Byte;
var
  S:Core.Strings.VarString;
begin
  S:=Core.Arrays.KeyString.GetItemByKey(Headers,'Content-Transfer-Encoding',iHdrCount);
  S:=Lowercase(s);
  if Core.Strings.Pos('base64',S)>0 then
    Result:=Items.SMTP.Encoding.emBase64
  else if Core.Strings.Pos('7bit',S)>0 then
      Result:=Items.SMTP.Encoding.em7Bit
  else if Core.Strings.Pos('8bit',S)>0 then
      Result:=Items.SMTP.Encoding.em8Bit
  else if Core.Strings.Pos('quoted-printable',S)>0 then
    Result:=Items.SMTP.Encoding.emQuotedPrintable
  else If Core.Strings.Pos('binary',S)>0 then
    Result:=Items.SMTP.Encoding.emBinary
  else
    Result:=Items.SMTP.Encoding.emNone;
end;

function getType(var Headers:Core.Arrays.Types.KeyStrings; iHdrCount:LongInt; out CharSet:Byte; out CharFormat:Byte; out Name:Core.Strings.VarString):Byte;
var
  Boundary:Core.Strings.VarString;
begin
  Core.Strings.Init(Boundary);
  Result:=getType(Headers,Boundary,iHdrCount,CharSet,CharFormat,Name);
  Core.Strings.Done(Boundary);
end;

function getDisposition(var Headers:Core.Arrays.Types.KeyStrings; iHdrCount:LongInt; var Name:Core.Strings.VarString; out Created,Modified,Read:Double; out Size:QWord):byte;
var
  sInput:Core.Strings.VarString;
  sValue:Core.Strings.VarString;
  sSearch:Core.Strings.VarString;
begin
  Created:=0;
  Modified:=0;
  Read:=0;
  Size:=0;
  Result:=Items.SMTP.Content.Disposition.None;
  sInput:=Core.Arrays.KeyString.GetItemByKey(Headers,'Content-Disposition',iHdrCount);
  sSearch:=Lowercase(sInput);
  if Length(sSearch)>0 then begin
    if (Length(Name)=0) then begin
      sValue:=GetParameter(sInput,'filename',false);
      if Length(sValue)>0 then
        Name:=sValue;
    end;
    if Core.Strings.Pos('inline',sSearch)>0 then begin
      Result:=Items.SMTP.Content.Disposition.Inline;
    end else if Core.Strings.Pos('attachment',sSearch)>0 then begin
      Result:=Items.SMTP.Content.Disposition.Attachment;
    end;

    sValue:=GetParameter(sSearch,'creation-date',false);
    if Length(sValue)>0 then
      Created:=SysUtils.StrToFloatDef(sValue,0);

    sValue:=GetParameter(sSearch,'modification-date',false);
    if Length(sValue)>0 then
      Modified:=SysUtils.StrToFloatDef(sValue,0);

    sValue:=GetParameter(sSearch,'read-date',false);
    if Length(sValue)>0 then
      Read:=SysUtils.StrToFloatDef(sValue,0);
  end;
end;

function getType(var Headers:Core.Arrays.Types.KeyStrings; var Boundary:Core.Strings.VarString; iHdrCount:LongInt; out CharSet:Byte; out CharFormat:Byte; out Name:Core.Strings.VarString):Byte;
var
  sInput:Core.Strings.VarString;
begin
  CharSet:=Items.SMTP.CharSet.csNone;
  CharFormat:=Items.SMTP.Content.Format.None;

  sInput:=Core.Arrays.KeyString.GetItemByKey(Headers,'Content-Type',iHdrCount);
  Boundary:=GetParameter(sInput,'boundary',false);
  Name:=GetParameter(sInput,'name',false);

  CharSet:={Items.SMTP.CharSet.}CharsetParse(sInput,CharFormat);
  sInput:=Lowercase(sInput);
  if (Core.Strings.Pos('multipart/alternative',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiAlternative
  else if (Core.Strings.Pos('multipart/mixed',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiMixed
  else if (Core.Strings.Pos('multipart/related',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiRelated
  else if (Core.Strings.Pos('multipart/digest',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiDigest
  else if (Core.Strings.Pos('multipart/message',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiMessage
  else if (Core.Strings.Pos('multipart/signed',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiSigned
  else if (Core.Strings.Pos('multipart/encrypted',sInput)>0) then
    Result:=Items.SMTP.Content.ctMultiEncrypted
  else if ( (Core.Strings.Pos('text/plain',sInput)>0) or (Length(sInput)=0) )then begin
    Result:=Items.SMTP.Content.ctTextPlain;
  end else if ( (Core.Strings.Pos('text/html',sInput)>0) ) then begin
    Result:=Items.SMTP.Content.ctTextHTML;
  end else if ( (Core.Strings.Pos('text/',sInput)>0) ) then begin
    Result:=Items.SMTP.Content.ctTextPlain;
  end else if (Core.Strings.Pos('application/',sInput)>0) then begin
    if (Core.Strings.Pos('application/pgp-signature',sInput)>0) then
      Result:=Items.SMTP.Content.ctPGPSignature
    else
      Result:=Items.SMTP.Content.ctApplication
  end else if (Core.Strings.Pos('image/',sInput)>0) then
    Result:=Items.SMTP.Content.ctImage
  else
    Result:=Items.SMTP.Content.ctUnknown;
end;

function GetParameter(Data:Core.Strings.VarString; Param : Core.Strings.VarString; Const Singleton:boolean) : Core.Strings.VarString;
Const
  SA_Options:Array[boolean] of SplitOptions=([soClearList,soTrimLines],[soClearList,soSingleton,soTrimLines]);
Var
  saLcv,saParam:Core.Arrays.Types.VarString;
  iLen,iLcv:LongInt;
begin
   SetLength(Result,0);
  Data:=StringReplace(Data,';'#13#10#9,';',[rfReplaceAll]);
  Data:=StringReplace(Data,';'#13#10#32,';',[rfReplaceAll]);

  Data:=StringReplace(Data,#9,#32,[rfReplaceAll]);
  Data:=StringReplace(Data,';'#32,';',[rfReplaceAll]);
  Data:=StringReplace(Data,#32#32,#32,[rfReplaceAll]);
  iLen:=System.Length(Data);
  if (iLen>0) and (Data[iLen]=';') then begin
    Dec(iLen);
    SetLength(Data,iLen);
  end;
  Core.Arrays.VarString.fromString(saLcv,Data,';',SA_Options[Singleton]);
  Try
    iLcv:=0;  iLen:=System.Length(saLcv);
    While (iLcv<iLen) do begin
      // text/plain
      // Charset=us-ascii
      Core.Arrays.VarString.fromString(saParam,saLcv[iLcv],'=',[soClearList,soSingleton,soTrimLines]);
      Try
        If (Core.Arrays.VarString.IndexOf(saParam,Param)=0) then begin
          Result:=saParam[1];
          iLcv:=iLen;
        end;
      Finally
        SetLength(saParam,0);
      end;
      Inc(iLcv);
    end;
  Finally
    SetLength(saLcv,0);
  end;
  iLen:=System.Length(Result);
  if (iLen>0) then begin
    if (Result[1]='"') then begin
      System.Delete(Result,1,1);
      Dec(iLen);
    end;
    if (iLen>0) and (Result[iLen]='"') then
      System.SetLength(Result,iLen-1);
  end;
end;

class function  Items.SMTP.Stats(Task:Core.Database.Types.TTask; DomainID,UserID,FolderID:QWord; Var Item:TStats):Boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
begin
  Result:=False;
  Try
    Core.Database.Empty(Commands); Item.Size:=0; Item.Count:=0;
    iCount:=0; iKind:=Storage.UserStorage.Kind.SMTP;
    with Files.DB do begin
      Core.Database.AddCommand(iCount,TableP,@Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.FolderID,poAnd,oEqual,FolderID,Commands);
      Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.Kind,poAnd,oEqual,iKind,Commands);
      Core.Database.AddCommand(iCount,TableP,useForOrderBy,IDs.ID,poNone,oNone,Commands);
      Core.Database.AddCommand(iCount,TableP,useForFields,IDs.Size,poNone,oNone,Commands);
    end;
    Result:=Core.Database.SQL.Select(Task,@Commands,@CB_SMTP_Stats,@Item);
  Finally
    Core.Database.Done(Commands);
  End;
end;

class function  Items.SMTP.Append(Task:Core.Database.Types.TTask; Parser:TDOMParser; var ItemP:PDelivery; DomainID,UserID,FileID:QWord):boolean;
var
  iCount                         : LongInt;
  iKind                          : LongInt;
  Commands                       : Core.Database.Types.Commands;
  FSummary                       : Core.Strings.VarString;
  FSMTPSummary                   : TSummary;
  sSplice                        : Core.Strings.VarString;
  FDocument                      : TXMLDocument;
  FSource                        : TXMLInputSource;
begin
  Result:=False;
  Core.Database.SQL.StartTransaction(Task);
  Try
    if Files.DB.GetSummary(Task,DomainID,UserID,FileID,FSummary) then begin
      Core.XML.DB.Wrap(Core.XML.DB.Header(Storage.Main.Header.Encoding),Items.SMTP.XML.Stanza,FSummary);
      Try
        FSource:=TXMLInputSource.Create(FSummary);
        try
          Parser.Parse(FSource,FDocument);
          Try
            if Items.SMTP.fromXML(FDocument,FSMTPSummary) then begin
              Append(ItemP,FSMTPSummary.Deliveries);
              FSummary:=toXML(FSMTPSummary);
              Core.Database.Empty(Commands);
              Try
                iCount:=0;
                with Files.DB do begin
                  Core.Database.AddCommand(iCount,TableP,@Commands);
                  Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.DomainID,poNone,oEqual,DomainID,Commands);
                  Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.UserID,poAnd,oEqual,UserID,Commands);
                  Core.Database.AddCommand(iCount,TableP,useForCriteria,IDs.ID,poAnd,oEqual,FileID,Commands);
                  Core.Database.AddCommand(iCount,TableP,useForUpdates,IDs.Summary,poNone,oNone,FSummary,Commands);
                end;
                Result:=Core.Database.SQL.Update(Task,@Commands);
                Core.Database.SQL.CommitTransaction(Task);
                ItemP:=nil;
              Finally
                Core.Database.Done(Commands);
              End;
            end;
          finally
            Done(FSMTPSummary);
          end;
        Finally
          FreeAndNil(FSource);
        end;
      Finally
        FreeAndNil(FDocument);
      end;
      SetLength(FSummary,0);
    end;
  Finally
    Core.Database.SQL.CommitTransaction(Task);
  end;
end;

class procedure Items.SMTP.SecurityCheck(
  var DropContent:Storage.Security.Filter.Items;
  var DropPhrases:Storage.Security.Filter.Items;
  var DropDomains:Storage.Security.Filter.Items;
  var FilterID:QWord;
  var Risk:Byte;
  var Filtered:Boolean;
  const Force:Boolean;
  var Summary:TSummary;
  var Headers:Core.Arrays.Types.KeyStrings;
  var Data:Core.Arrays.Types.VarString;
  Refactor:TStream
);
var
  iDataCount : LongInt;

  sData      : Core.Strings.VarString;
  sLine      : Core.Strings.VarString;

  baHits     : TBooleanArray;

  function AreHeadersFiltered:boolean;
  var
    iDomainCount : LongInt;
    iDomainLcv   : LongInt;
    iHdrLcv      : LongInt;
    iDropLcv     : LongInt;

  begin
    Result:=false;
    iDomainCount:=System.Length(DropDomains);
    for iDomainLcv:=0 to iDomainCount-1 do begin
      if DropDomains[iDomainLcv]^.Enabled then begin
        sLine:=Concat('.',DropDomains[iDomainLcv]^.Value,'/');
        for iHdrLcv:=0 to High(Headers) do begin
          if Core.Strings.Pos(sLine,Headers[iHdrLcv]^.Value)>0 then begin
            Filtered:=True;
            Result:=True;
            FilterID:=DropDomains[iDomainLcv]^.ID;
            exit;
          end;
          sLine:=Concat('http://',DropDomains[iDomainLcv]^.Value,'/');
          if Core.Strings.Pos(sLine,Headers[iHdrLcv]^.Value)>0 then begin
            Filtered:=True;
            Result:=True;
            FilterID:=DropDomains[iDomainLcv]^.ID;
            exit;
          end;
        end;
      end;
    end;
    for iHdrLcv:=0 to High(Headers) do begin
      sLine:=Lowercase(Headers[iHdrLcv]^.Value);
      for iDropLcv:=0 to High(DropPhrases) do begin
        if ((DropPhrases[iDropLcv]^.Enabled=true) and (DropPhrases[iDropLcv]^.Stale=false)) then begin
          if Core.Strings.Pos(DropPhrases[iDropLcv]^.Value,sLine)>0 then begin
            Filtered:=True;
            Result:=True;
            FilterID:=DropPhrases[iDropLcv]^.ID;
            Exit;
          end;
        end;
      end;
    end;

  end;

  function IsDomainBlocked(var Content:Core.Strings.VarString):boolean;
  var
    iDomainCount : LongInt;
    iDomainLcv   : LongInt;
    iHdrLcv      : LongInt;
  begin
    Result:=false;
    iDomainCount:=System.Length(DropDomains);
    for iDomainLcv:=0 to iDomainCount-1 do begin
      if DropDomains[iDomainLcv]^.Enabled=true then begin
        sLine:=Concat('.',DropDomains[iDomainLcv]^.Value,'/');
        if Core.Strings.Pos(sLine,Content)>0 then begin
          Filtered:=True;
          Result:=True;
          FilterID:=DropDomains[iDomainLcv]^.ID;
          exit;
        end;
        sLine:=Concat('http://',DropDomains[iDomainLcv]^.Value,'/');
        if Core.Strings.Pos(sLine,Content)>0 then begin
          Filtered:=True;
          Result:=True;
          FilterID:=DropDomains[iDomainLcv]^.ID;
          exit;
        end;
      end;
    end;
  end;

  function IsContentProfiled(var Content:Core.Strings.VarString):boolean;
  var
    iManifestCount:LongInt;
    iManifestLcv:LongInt;
    idxContent:LongInt;
    saP:Core.Arrays.Types.PVarString;
    iHitLoc,iHitsLcv,iHitCount:LongInt;
    iHitLocStart,iHitLocEnd:LongInt;
  begin
    // Manfest of Content to Drop check
    Result:=false;
    iManifestLcv:=0;
    iManifestCount:=System.Length(DropContent);
    while (iManifestLcv<iManifestCount) and (Result=false) do begin

      saP:=@DropContent[iManifestLcv]^.Data;

      {
      if DropContent[iManifestLcv]^.ID=35662 then
        saP:=@DropContent[iManifestLcv]^.Data;
      }

      iHitCount:=Length(saP^);
      SetLength(baHits,iHitCount);
      Core.Arrays.Boolean.Fill(baHits,false);
      for iHitsLcv:=0 to iHitCount-1 do begin
        iHitLoc:=Core.Strings.Pos(saP^[iHitsLcv],Content);
        if iHitLoc>0 then begin
          idxContent:=Core.Strings.LineCount(Content,1,iHitLoc)+1;
          baHits[iHitsLcv]:=true;
        end;
      end;
      if (iHitCount>0) and Core.Arrays.Boolean.All(true,baHits) then begin
        Filtered:=True;
        Result:=True;
        FilterID:=DropContent[iManifestLcv]^.ID;
        Exit;
      end;
      Inc(iManifestLcv);
    end;
  end;

  function IsContentFiltered(var Content:Core.Strings.VarString; Const NumberOfLines:LongInt):boolean;
  var
    sLine      : Core.Strings.VarString;
    sMessageId : Core.Strings.VarString;
    iMessageIdLen : LongInt;


    dCSSRatio  : double;
    dCSSFWRatio : double;
    iCSSEndCount  : LongInt;
    iCSSStartCount  : LongInt;
    iCSSLen    : LongInt;
    iCSSFields : LongInt;
    iCSSLines  : LongInt;
    iCSSWords  : LongInt;

    iFieldLoc  : LongInt;
    iFieldLen  : LongInt;
    iLineLen   : LongInt;
    iLineCount : LongInt;
    iDropLcv   : LongInt;

    iHitLoc,iHitsLcv,iHitCount:LongInt;
    iHitLocStart,iHitLocEnd:LongInt;
    iContentLen : LongInt;
    iBodyLcv:LongInt;
  begin
    Result:=false;
    iHitLocStart:=1;
    iContentLen:=System.Length(Content);

    // Validate CSS STYLE Check for Invalid <STYLE>
    repeat
      iHitLocStart:=StrUtils.PosEx('<style',Content,iHitLocStart);
      if (iHitLocStart>0) then begin
        iHitLocStart:=StrUtils.PosEx('>',Content,iHitLocStart+1)+1;
        iHitLocEnd:=StrUtils.PosEx('</style>',Content,iHitLocStart);
        if (iHitLocEnd>0) then begin
          sLine:=System.Copy(Content,iHitLocStart,iHitLocEnd-iHitLocStart);
          // Spam is filled with jibberish.  Normal style is Name { asdfsdf: sdfs; asdfkslf:sdfsfs}

          sLine:=StringReplace(sLine,#13#10#13#10,#13#10,[rfReplaceAll]); // Reduce double lines
          iCSSLines:=Math.Max(1,Core.Strings.WordCount(sLine,#13#10));
          iCSSWords:=Core.Strings.WordCount(sLine,#32);

          sLine:=StringReplace(sLine,#13#10,'',[rfReplaceAll]);
          sLine:=StringReplace(sLine,#32,'',[rfReplaceAll]);
          (*
          sLine:=StringReplace(sLine,'{ ','{',[rfReplaceAll]);
          sLine:=StringReplace(sLine,'| ','{',[rfReplaceAll]);
          sLine:=StringReplace(sLine,'; ',';',[rfReplaceAll]);
          *)
          iLineLen:=System.Length(sLine);

          if (iLineLen>0) then begin

            //h1{color:red;bladf:asdfasfd;alsdkfasdf:asdfasf}p{color:blue;}
            iCSSStartCount:=Core.Strings.WordCount(sLine,'{');
            iCSSEndCount:=Core.Strings.WordCount(sLine,'}');
            iCSSFields:=Core.Strings.WordCount(sLine,':');

            // Check CSS Definitions Threshold
            if (iCSSStartCount<Items.SMTP.MaxCSSDefinesThreshold) then begin
              Filtered:=True;
              Result:=True;
              Risk:=RISK_6_CSS_DEFINITIONS;
              Exit;
            end;
            // Check CSS Definitions Count
            if (iCSSStartCount<>iCSSStartCount) then begin
              Filtered:=True;
              Result:=True;
              Risk:=RISK_5_CSS_DEF_MISMATCH;
              Exit;
            end;
            // Check Fields per Words
            if iCSSWords=0 then iCSSWords:=1;
            dCSSFWRatio:=(iCSSFields/iCSSWords)*100;
            if (dCSSFWRatio>Items.SMTP.MaxCSSFieldsToWordsThreshold) then begin
              Filtered:=True;
              Result:=True;
              Risk:=RISK_1_CSSFieldsToWords;
              Exit;
            end;
            // Check Fields per Lines
            dCSSRatio:=(iCSSFields/iCSSLines)*100;
            if (dCSSRatio<Items.SMTP.MaxCSSThreshold) then begin
              Filtered:=True;
              Result:=True;
              Risk:=RISK_2_CSSFieldsToDATA;
              Exit;
            end;
          end;
          iHitLocStart:=iHitLocEnd+1;
        end else
          Inc(iHitLocStart,1);
      end;
    until (Filtered=true) or (iHitLocStart=0) or (iHitLocStart>=iContentLen);
    if (Filtered=false) then begin
      iHitLocStart:=1;
      // Validate HTML COMMENTS
      repeat
        iHitLocStart:=StrUtils.PosEx('<!--',Content,iHitLocStart);
        if (iHitLocStart>0) then begin
          Inc(iHitLocStart,4);
          iHitLocEnd:=StrUtils.PosEx('-->',Content,iHitLocStart);
          if (iHitLocEnd>0) then begin
            sLine:=System.Copy(Content,iHitLocStart,iHitLocEnd-iHitLocStart);
            // Spam is filled with jibberish.  Normal style is Name { asdfsdf: sdfs; asdfkslf:sdfsfs}

            sLine:=StringReplace(sLine,#13#10#13#10,#13#10,[rfReplaceAll]); // Reduce double lines
            iLineLen:=System.Length(sLine);
            if (iLineLen>0) and (NumberOfLines>0) then begin
              iLineCount:=Core.Strings.WordCount(sLine,#13#10);
              dCSSRatio:=iLineCount/NumberOfLines*100;
              if (dCSSRatio>=Items.SMTP.MaxCommentThreshold) then begin
                Filtered:=True;
                Result:=True;
                Risk:=RISK_3_COMMENTS;
                Exit;
              end;
            end;
            iHitLocStart:=iHitLocEnd+1;
          end;
        end;
      until (Filtered=true) or (iHitLocStart=0) or (iHitLocStart>=iContentLen);
      if (Filtered=false) then begin
        // Extra long message-id strings located everywhere in message
        sMessageId:=Core.Arrays.KeyString.GetItemAsString(Headers,'Message-id');
        sMessageId:=Lowercase(Items.SMTP.getMessageID(sMessageID));
        iMessageIdLen:=Length(sMessageId);

        if ( iMessageIdLen>=Items.SMTP.MaxMessageIdInBodyLength) then begin
          iHitCount:=0;
          iHitLocStart:=1;
          repeat
            iHitLocStart:=StrUtils.PosEx(sMessageId,Content,iHitLocStart);
            if (iHitLocStart>0) then begin
              Inc(iHitCount);
              Inc(iHitLocStart,iMessageIdLen);
              if (iHitCount>=Items.SMTP.MaxMessageIdInBody) then begin
                Filtered:=True;
                Result:=True;
                Risk:=RISK_4_MESSAGE_ID;
                Exit;
              end;
            end;
          until (Filtered=true) or (iHitLocStart=0) or (iHitLocStart>=iContentLen);
        end;
        if (Filtered=false) then begin
          for iDropLcv:=0 to High(DropPhrases) do begin
            if ((DropPhrases[iDropLcv]^.Enabled=true) and (DropPhrases[iDropLcv]^.Stale=false)) then begin
              if Core.Strings.Pos(DropPhrases[iDropLcv]^.Value,Content)>0 then begin
                Filtered:=True;
                Result:=True;
                FilterID:=DropPhrases[iDropLcv]^.ID;
                Exit;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  function IsFromBlank():boolean;
  var
    idxFrom      : Int64;
  begin
    Result:=false;
    idxFrom:=Core.Arrays.KeyString.IndexOf(Headers,Items.SMTP.XML.Fields.From);
    if (idxFrom=-1) then begin
      Filtered:=True;
      Result:=True;
      Risk:=RISK_9_MISSING_FROM;
    end;
  end;

  function AreMimesFiltered():boolean;

    function IsMimeFiltered(MimeP:PMime):boolean;
    var
      iLcv       : LongInt;

      sChunk     : Core.Strings.VarString;
      iChunkLen  : LongInt;
      iChunkLcv  : LongInt;
      iLineCount : LongInt;
      trMimeP    : PMime;
      txMimeP    : PMime;
    begin
      if (
       (MimeP^.cntType=SMTP.Content.ctTextPlain) or
       (MimeP^.cntType=SMTP.Content.ctTextHTML)
      )
      then begin
        SetLength(sChunk,0);
        for iChunkLcv:=MimeP^.idxContentStart to MimeP^.idxContentEnd do
          if System.Length(Data[iChunkLcv])>0 then
            sChunk:=Concat(sChunk,Data[iChunkLcv],#13#10);
        Encoding.Prepare(sChunk);
        if (mimeP^.cntEncoding=Encoding.emBase64) then begin
          iChunkLen:=Length(sChunk);
          Dec(iChunkLen,2);
          SetLength(sChunk,iChunkLen);
          try
            sChunk:=Encryption.Base64.Decode(sChunk);
          except
          end;
          iChunkLen:=Length(sChunk);
          iLineCount:=Core.Strings.LineCount(sChunk,1,iChunkLen);
        end else begin
          iLineCount:=mimeP^.idxContentEnd-mimeP^.idxContentStart;
        end;
        sChunk:=Lowercase(sChunk);
        mimeP^.iReadableSize:=System.Length(sChunk);
        if IsDomainBlocked(sChunk)=false then begin
          if IsContentFiltered(sChunk,iLineCount)=true then begin
            Result:=True;
            Exit;
          end;
        end else begin
          Result:=True;
          Exit;
        end;
      end;
      if (MimeP^.cntType=Content.ctMultiAlternative) then begin
        txMimeP:=Items.SMTP.getMime(MimeP^,Content.ctTextPlain);
        trMimeP:=Items.SMTP.getMime(MimeP^,Content.ctTextHTML);
        if ((txMimeP<>nil) and (trMimeP<>nil)) then begin
          // AURA_RISK_7 Check
          if ( (trMimeP^.cntEncoding=Encoding.emBase64) and (txMimeP^.iReadableSize<4) and (trMimeP^.iReadableSize>4) ) then begin
            Filtered:=True;
            Result:=True;
            Risk:=RISK_7_ALT_MIME;
            Exit;
          end;
          if (
            (txMimeP^.iReadableSize=0) or
            ( (txMimeP^.iReadableSize/trMimeP^.iReadableSize)<MaxAlternateMissingText)
          ) then begin
            Filtered:=True;
            Result:=True;
            Risk:=RISK_7_ALT_MIME;
            Exit;
          end;
        end;
      end;
      for iLcv:=0 to High(MimeP^.Mimes) do begin
        Result:=IsMimeFiltered(MimeP^.Mimes[iLcv]);
        If Result=true then
          Exit;
      end;
    end;
  begin
    Result:=IsMimeFiltered(@Summary.Mime);
  end;
begin
  Risk:=0;
  Try
    if (Force=false) then begin
      if (Summary.BlackListed=true) then begin
        Filtered:=true;
        exit;
      end;
      if (Summary.WhiteListed=false) then begin
        if IsFromBlank then begin
          Filtered:=true;
          exit;
        end;
        if AreHeadersFiltered then begin
          Filtered:=true;
          exit;
        end;
        if AreMimesFiltered()=true then
          Exit;
        sData:=Core.Arrays.VarString.toString(Data,Summary.Mime.idxContentStart,Summary.Mime.idxContentEnd-Summary.Mime.idxContentStart,#13#10,Refactor,TRAILING_DELIM_ON);
        Try
          sData:=Lowercase(sData);
          if IsDomainBlocked(sData)=false then begin
            if IsContentProfiled(sData)=false then begin
              IsContentFiltered(sData,iDataCount);
            end else
              Exit;
          end else
            Exit;
        finally
          SetLength(sData,0);
        end;
      end;
    end;
  finally
    Core.Arrays.Boolean.Done(baHits);
  end;
end;

class function Items.SMTP.Write
(
  Task:Core.Database.Types.TTask; var Node:Storage.MatrixNodes.Node.Item;
  SpamID,UserID,DomainID:QWord;
  var Summary:TSummary;
  var Data:Core.Arrays.Types.VarString;
  Flags:LongInt;
  Refactor:TStream;
  IgnoreFilters : Boolean;
  var FolderID:QWord;
  out FileID:QWord
):Boolean;
var
  Item       : Files.TItem;
  sStamp     : Core.Strings.VarString;
begin
  Refactor.Size:=0;
  Result:=false;
  Files.Init(Item);
  FileID:=0;
  Item.Flags:=Flags;
  Item.Created:=Core.Timer.dtUT;
  If (
    (IgnoreFilters=false) and
    (  (Summary.WhiteListed=false) and
       ((Summary.Spam=true) or (Summary.Risk>0) )
    )
  ) then
    Item.FolderID:=SpamID
  else
    Item.FolderID:=FolderID;
  Item.Kind:=Storage.UserStorage.Kind.SMTP;
  Try
    // NOTICE: If you modify lines you must edit offsetMimes(Summary,#Lines)
    sStamp:=Concat(
       'Received: from ',Summary.RemoteDomain,' [',Summary.RemoteIP,'] by ',
       Summary.Exchanger,' (envelope-from <',Summary.RemoteFrom,'>) for <',
       Summary.User,'@',Summary.Domain,'> with ',App.Build.Title,' (',App.Build.Edition,
       ' Version ',App.Build.Version,' RSR Build ',App.Build.RSR,') AURA SMTP; ',
       Core.Utils.Time.TimeZoneTime,
       #13#10
    );
    Core.Streams.Write(sStamp,Refactor);
    Core.Streams.Write(Data,Refactor);

    offsetMimes(Summary,1);
    Summary.Attachments:=HasAttachments(Summary);

    Item.Summary:=Items.SMTP.toXML(Summary); // utf8 string should already be encoded
    Item.Summary:=System.UTF8Encode(Item.Summary);

    Result:=Files.DB.Add(Task,Node,DomainID,UserID,Item.FolderID,Item,Refactor);
    if Result then begin
      Item.Name:=IntToStr(Item.ID);
      Summary.ID:=Item.ID;
      Items.SMTP.Write(Task,DomainID,UserID,Summary,Item.Name,Flags);
    end;

    FolderID:=Item.FolderID;
    FileID:=Item.ID;
  finally
    Refactor.Size:=0;
    Files.Done(Item);
  end;
end;

class function Items.SMTP.toXML(var Item:TRelayMail; Header:boolean):Core.Strings.VarString;
begin
  SetLength(Result,0);
  if (Header) then
    Result:=Core.XML.DB.Header(Storage.Main.Header.Encoding);
  with Core.XML.DB do begin
    Result:=Concat(
      Result,
      '<',XML.Relay,'>',
      Print(XML.Fields.UserID,Item.UserID),
      Print(XML.Fields.DomainID,Item.DomainID),
      Print(XML.Fields.FileID,Item.FileID),
      Print(XML.Fields.FolderID,Item.FolderID),
      Print(XML.Fields.InboxID,Item.InboxID),
      Print(XML.Fields.SpamID,Item.SpamID),
      Print(XML.Fields.QueueID,Item.QueueID),
      Print(XML.Fields.DNSIP,Item.DNSIP),
      Print(XML.Fields.iMXLcv,Item.iMXLcv),
      Print(XML.Fields.iIPLcv,Item.iIPLcv),
      Print(XML.Fields.iTryMax,Item.iTryMax),
      Print(XML.Fields.iTry,Item.iTry),
      Print(XML.Fields.iTransitTry,Item.iTransitTry),
      Print(XML.Fields.Status,Item.Status),
      Print(XML.Fields.State,Item.State),
      Print(XML.Fields.Sent,Item.Sent),
      Print(XML.Fields.Date,Item.Date),
      Print(XML.Fields.DNSQuery,Item.DNSQuery),
      Print(XML.Fields.Response,Item.Response),
      Print(XML.Fields.MXServer,Item.MXServer),
      Print(XML.Fields.Domain,Item.Domain),
      Print(XML.Fields.Error,Item.Error),
      Print(XML.Fields.Helo,Item.Helo),
      Print(XML.Fields.From,Item.From),
      Print(XML.Fields.&To,Item.&To),
      Print(XML.Fields.Subject,Item.Subject),
      Print(XML.Fields.SenderIP,Item.SenderIP),
      Print(XML.Fields.MXServers,Item.MXServers),
      '</',XML.Relay,'>'
    );
  end;
end;

class function Items.SMTP.toXML(var Item:TSummary):Core.Strings.VarString;
begin
  SetLength(Result,0);
  with Core.XML.DB do begin
    Result:=Concat(
      Print(XML.Fields.ID,Item.ID),
      Print(XML.Fields.Risk,Item.Risk),
      Print(XML.Fields.FilterID,Item.FilterID),
      Print(XML.Fields.Kind,Storage.UserStorage.Kind.SMTP),
      Print(XML.Fields.Sender,Item.Sender),
      Print(XML.Fields.ReplyTo,Item.ReplyTo),
      Print(XML.Fields.InReplyTo,Item.InReplyTo),
      Print(XML.Fields.MessageId,Item.MessageId),
      Print(XML.Fields.&To,Item.&To),
      Print(XML.Fields.CC,Item.CC),
      Print(XML.Fields.From,Item.From),
      Print(XML.Fields.Subject,Item.Subject),
      Print(XML.Fields.Group,Item.Group),
      Print(XML.Fields.Lines,Item.Lines,CDATA_OFF),
      Print(XML.Fields.Flags,Item.Flags,CDATA_ON),
      Print(XML.Fields.Date,Item.Date),
      Print(XML.Fields.tzBias,Item.tzBias),
      Print(XML.Fields.cntType,Item.cntType),
      Print(XML.Fields.BlackList,Item.BlackListed),
      Print(XML.Fields.WhiteList,Item.WhiteListed),
      Print(XML.Fields.Attachments,Item.Attachments),
      Print(XML.Fields.Bound,Item.Bound),
      Print(XML.Fields.Sent,Item.Sent),
      Print(XML.Fields.Read,Item.Read),
      Print(XML.Fields.Replied,Item.Replied),
      Print(XML.Fields.RepliedAll,Item.RepliedAll),
      Print(XML.Fields.Forwarded,Item.Forwarded),
      Print(XML.Fields.Spam,Item.Spam),
      Print(XML.Fields.Rendered,Item.Rendered),
      Print(XML.Fields.Pinned,Item.Pinned),
      Print(XML.Fields.RemoteIP,Item.RemoteIP),
      Print(XML.Fields.RemoteDomain,Item.RemoteDomain),
      Items.SMTP.toXML(Item.Deliveries),
      Items.SMTP.toXML(Item.Mime)
    );
  end;
end;

class function  Items.SMTP.toXML(var Item:TSummary; Output:TMemoryStream):boolean;
var
  sXML:Core.Strings.VarString;
begin
  Result:=False;
  Output.Seek(0,soFromEnd);
  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stanza,Output);
  Core.Streams.Write('>',1,Output);
  sXML:=toXML(Item);
  Core.Streams.Write(sXML,Length(sXML),Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stanza,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;


class function  Items.SMTP.toXML(var Item:TStats; Output:TMemoryStream):boolean;
var
  sXML:Core.Strings.VarString;
begin
  Result:=false;
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Stat,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    sXML:=Concat(
      Print(XML.Fields.Count,Item.Count),
      Print(XML.Fields.Size,Item.Size)
    );
  end;
  Core.Streams.Write(sXML,Length(sXML),Output);
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Stat,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Items.SMTP.toXML(var Item:TMimes; Output:TMemoryStream):boolean;
var
  iLcv:LongInt;
  sXML:Core.Strings.VarString;
begin
  Result:=false;
  Output.Seek(0,soFromEnd);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Fields.Mimes,Output);
  Core.Streams.Write('>',1,Output);

  for iLcv:=0 to high(Item) do begin
    sXML:=toXML(Item[iLcv]^);
    Core.Streams.Write(sXML,Output);
  end;

  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Fields.Mimes,Output);
  Core.Streams.Write('>',1,Output);
  Result:=True;
end;

class function  Items.SMTP.toXML(var Item:TMime):Core.Strings.VarString;
begin
  SetLength(Result,0);
  with Core.XML.DB do begin
    Result:=Concat(
      '<',XML.Fields.Mime,'>',
      Print(XML.Fields.idxHeadersStart,Item.idxHeadersStart),
      Print(XML.Fields.idxHeadersEnd,Item.idxHeadersEnd),
      Print(XML.Fields.idxContentStart,Item.idxContentStart),
      Print(XML.Fields.idxContentEnd,Item.idxContentEnd),
      Print(XML.Fields.cntReadableSize,Item.iReadableSize),
      Print(XML.Fields.cntLast,Item.cntLast),
      Print(XML.Fields.cntDisposition,Item.cntDisposition),
      Print(XML.Fields.cntCreated,Item.cntCreated),
      Print(XML.Fields.cntModified,Item.cntModified),
      Print(XML.Fields.cntRead,Item.cntRead),
      Print(XML.Fields.cntSize,Item.cntSize),
      Print(XML.Fields.cntEncoding,Item.cntEncoding),
      Print(XML.Fields.cntType,Item.cntType),
      Print(XML.Fields.cntCharSet,Item.cntCharSet),
      Print(XML.Fields.cntCharFormat,Item.cntCharFormat),
      Print(XML.Fields.cntID,Item.cntID),
      Print(XML.Fields.cntBoundary,Item.cntBoundary),
      Print(XML.Fields.cntName,Item.cntName),
      Print(XML.Fields.cntDescription,Item.cntDescription),
      SMTP.toXML(Item.Mimes),
      '</',XML.Fields.Mime,'>'
    );
  end;
end;

class function  Items.SMTP.toXML(var Item:TMimes):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  Result:=Concat('<',XML.Fields.Mimes,'>');
  for iLcv:=0 to High(Item) do
    Result:=Concat(Result,toXML(Item[iLcv]^));
  Result:=Concat(Result,'</',XML.Fields.Mimes,'>');
end;

class function  Items.SMTP.toXML(var Item:TDelivery):Core.Strings.VarString;
begin
  SetLength(Result,0);
  with Core.XML.DB do begin
    Result:=Concat(
      '<',XML.Fields.Delivery,'>',
      Print(XML.Fields.DeliveryRead,Item.Read),
      Print(XML.Fields.DeliveryCode,Item.Code),
      Print(XML.Fields.DeliveryDate,Item.Date),
      Print(XML.Fields.DeliveryMessage,Item.Message),
      Print(XML.Fields.DeliveryAddress,Item.Address),
      '</',XML.Fields.Delivery,'>'
    );
  end;
end;
(*        Read                   : boolean;
          Code                   : LongInt;
          Date                   : Double;
          Message                : string;
          Address                : string;
*)
class function  Items.SMTP.toXML(var Item:TDeliveries):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  Result:=Concat('<',XML.Fields.Deliveries,'>');
  for iLcv:=0 to High(Item) do
    Result:=Concat(Result,toXML(Item[iLcv]^));
  Result:=Concat(Result,'</',XML.Fields.Deliveries,'>');
end;


class function  Items.SMTP.getMessageID(var sItem:Core.Strings.VarString):Core.Strings.VarString;
var
  iDX:LongInt;
  iStart:LongInt;
begin
  //<987742606@support.whissa.us>
  SetLength(Result,0);
  idx:=System.Pos('@',sItem);
  if (idx<>0) then begin
    if (sItem[1]='<') then begin
      iStart:=2;
      Dec(idx,2);
    end else begin
      iStart:=1;
      Dec(idx,1);
    end;
    Result:=System.Copy(sItem,iStart,idx);
  end;
end;

class function  Items.SMTP.getMime(var Parent:TMime; cntType:Byte):PMime;

  function  checkChildren(var Mimes:TMimes):PMime;
  var
    iLcv:LongInt;
  begin
    Result:=nil;
    for iLcv:=0 to High(Mimes) do begin
      if (Mimes[iLcv]^.cntType=cntType) then begin
        Result:=Mimes[iLcv];
        exit;
      end else begin
        Result:=checkChildren(Mimes[iLcv]^.Mimes);
        if Result<>nil then
          exit;
      end;
    end;
  end;
begin
  Result:=nil;
  if Parent.cntType=cntType then begin
    Result:=@Parent;
  end else
    Result:=checkChildren(Parent.Mimes);
end;

class function  Items.SMTP.getFlags(var Summary:Items.SMTP.TSummary; Flags:LongInt):LongInt;
begin
  Result:=Flags;
  if Summary.Read then begin
    Result:=Result or Items.IMAP.Flags.Seen;
  end else begin
    Result:=Result and not Items.IMAP.Flags.Seen;
  End;

  if Summary.Replied or Summary.RepliedAll or Summary.Forwarded then begin
    Result:=Result or Items.IMAP.Flags.Answered;
  End else begin
    Result:=Result and not Items.IMAP.Flags.Answered;
  End;

  if (Length(Summary.Flags)>0) then begin
    Result:=Result or Items.IMAP.Flags.Flagged;
  End else begin
    Result:=Result and not Items.IMAP.Flags.Flagged;
  End;

  if Summary.Pinned then begin
    Result:=Result or Items.IMAP.Flags.Pinned;
  end else begin
    Result:=Result and not Items.IMAP.Flags.Pinned;
  End;

end;

class function  Items.SMTP.fromXML(xDoc:TXMLDocument; var Item:TSummary):boolean;
var
  xKind:TDOMNode;
  xNode:TDOMNode;
  xMime:TDOMNode;
  xDeliveries:TDOMNode;
begin
  Result:=false;
  Empty(Item);
  with Core.XML.DB do begin
    xNode:=getNode(xDOC,XML.Stanza);
    if xNode<>nil then begin
      xMime:=getChildNode(xNode,XML.Fields.Mime);
      xDeliveries:=getChildNode(xNode,XML.Fields.Deliveries);
      with Item do begin
        ID                     := toQWord(xNode,XML.Fields.ID);
        FilterID               := toQWord(xNode,XML.Fields.FilterID);
        Kind                   := toInteger(xNode,XML.Fields.Kind);
        Sender                 := toString(xNode,XML.Fields.Sender);
        ReplyTo                := toString(xNode,XML.Fields.ReplyTo);
        InReplyTo              := toString(xNode,XML.Fields.InReplyTo);
        MessageId              := toString(xNode,XML.Fields.MessageId);
        &To                    := toString(xNode,XML.Fields.&To);
        CC                     := toString(xNode,XML.Fields.CC);
        BCC                    := toString(xNode,XML.Fields.BCC);
        From                   := toString(xNode,XML.Fields.From);
        Subject                := toString(xNode,XML.Fields.Subject);
        Domain                 := toString(xNode,XML.Fields.Domain);
        User                   := toString(xNode,XML.Fields.User);
        Group                  := toString(xNode,XML.Fields.Group);
        RemoteIP               := toString(xNode,XML.Fields.RemoteIP);
        RemoteDomain           := toString(xNode,XML.Fields.RemoteDomain);
        RemoteFrom             := toString(xNode,XML.Fields.RemoteFrom);
        Lines                  := toString(xNode,XML.Fields.Lines);
        Flags                  := toString(xNode,XML.Fields.Flags);
        Date                   := toDouble(xNode,XML.Fields.Date);
        tzBias                 := toInteger(xNode,XML.Fields.tzBias);
        cntType                := toByte(xNode,XML.Fields.cntType);
        Bound                  := toByte(xNode,XML.Fields.Bound);
        Sent                   := toBoolean(xNode,XML.Fields.Sent);
        Read                   := toBoolean(xNode,XML.Fields.Read);
        Rendered               := toBoolean(xNode,XML.Fields.Rendered);
        Pinned                 := toBoolean(xNode,XML.Fields.Pinned);
        Replied                := toBoolean(xNode,XML.Fields.Replied);
        RepliedAll             := toBoolean(xNode,XML.Fields.RepliedAll);
        Forwarded              := toBoolean(xNode,XML.Fields.Forwarded);
        Spam                   := toBoolean(xNode,XML.Fields.Spam);
        BlackListed            := toBoolean(xNode,XML.Fields.BlackList);
        WhiteListed            := toBoolean(xNode,XML.Fields.WhiteList);
        Attachments            := toBoolean(xNode,XML.Fields.Attachments);
        Risk                   := toByte(xNode,XML.Fields.Risk);
      end;
      if (xMime<>nil) then
        fromXML(Item.Mime,xMime);
      if (xDeliveries<>nil) then
        fromXML(Item.Deliveries,xDeliveries);
      Result:=true;
    end;
  end;
end;

class function  Items.SMTP.fromXML(var Item:TMimes; xNode:TDOMNode):boolean;
var
  xMime:TDOMNode;
  iLength:LongInt;
  MimeP:PMime;
  iLcv:LongInt;
begin
  Result:=true; iLength:=0;
  Empty(Item);
  with Core.XML.DB do begin
    for iLcv:=0 to xNode.ChildNodes.Count-1 do begin
      xMime:=xNode.ChildNodes[iLcv];
      if (xMime<>nil) then begin
        New(MimeP);
        Init(MimeP^);
        Inc(iLength);
        System.SetLength(Item,iLength);
        Item[iLength-1]:=MimeP;
        fromXML(MimeP^,xMime);
      end;
    end;
  end;
end;

class function  Items.SMTP.fromXML(var Item:TDeliveries; xNode:TDOMNode):boolean;
var
  xDelivery:TDOMNode;
  iLength:LongInt;
  DeliveryP:PDelivery;
  iLcv:LongInt;
begin
  Result:=true; iLength:=0;
  Empty(Item);
  with Core.XML.DB do begin
    for iLcv:=0 to xNode.ChildNodes.Count-1 do begin
      xDelivery:=xNode.ChildNodes[iLcv];
      if (xDelivery<>nil) then begin
        New(DeliveryP);
        Init(DeliveryP^);
        Inc(iLength);
        System.SetLength(Item,iLength);
        Item[iLength-1]:=DeliveryP;
        fromXML(DeliveryP^,xDelivery);
      end;
    end;
  end;
end;

class function  Items.SMTP.fromXML(var Item:TDelivery; xNode:TDOMNode):boolean;
var
  xMimes:TDOMNode;
begin
  Result:=true;
  Empty(Item);
  With Core.XML.DB do begin
    with Item do begin
      Read     := toBoolean(xNode,XML.Fields.DeliveryRead);
      Code     := toInteger(xNode,XML.Fields.DeliveryCode);
      Date     := toDouble(xNode,XML.Fields.DeliveryDate);
      Message  := toString(xNode,XML.Fields.DeliveryMessage);
      Address  := toString(xNode,XML.Fields.DeliveryAddress);
    end;
  end;
end;

class function  Items.SMTP.fromXML(var Item:TMime; xNode:TDOMNode):boolean;
var
  xMimes:TDOMNode;
begin
  Result:=true;
  Empty(Item);
  With Core.XML.DB do begin
    with Item do begin
      idxHeadersStart := toInteger(xNode,XML.Fields.idxHeadersStart);
      idxHeadersEnd   := toInteger(xNode,XML.Fields.idxHeadersEnd);
      idxContentStart := toInteger(xNode,XML.Fields.idxContentStart);
      idxContentEnd   := toInteger(xNode,XML.Fields.idxContentEnd);
      iReadableSize   := toInteger(xNode,XML.Fields.cntReadableSize);
      cntLast         := toBoolean(xNode,XML.Fields.cntLast);
      cntDisposition  := toByte(xNode,XML.Fields.cntDisposition);
      cntCreated      := toDouble(xNode,XML.Fields.cntCreated);
      cntModified     := toDouble(xNode,XML.Fields.cntModified);
      cntRead         := toDouble(xNode,XML.Fields.cntRead);
      cntSize         := toQWord(xNode,XML.Fields.cntSize);
      cntEncoding     := toByte(xNode,XML.Fields.cntEncoding);
      cntType         := toByte(xNode,XML.Fields.cntType);
      cntCharSet      := toByte(xNode,XML.Fields.cntCharSet);
      cntCharFormat   := toByte(xNode,XML.Fields.cntCharFormat);
      cntID           := toString(xNode,XML.Fields.cntID);
      cntBoundary     := toString(xNode,XML.Fields.cntBoundary);
      cntName         := toString(xNode,XML.Fields.cntName);
      cntDescription  := toString(xNode,XML.Fields.cntDescription);
      xMimes:=Core.XML.DB.getChildNode(xNode,XML.Fields.Mimes);
      if xMimes<>nil then
        fromXML(Mimes,xMimes);
    end;
  end;
end;

class function  Items.SMTP.fromXML(xDoc:TXMLDocument; var Item:TMime):boolean;
var
  xKind:TDOMNode;
  xNode:TDOMNode;
  xAttachments:TDOMNode;
  xMimes:TDOMNode;
begin
  Result:=false;
  Empty(Item);
  with Core.XML.DB do begin
    xNode:=getNode(xDOC,XML.Fields.Mime);
    if xNode<>nil then begin
      Result:=fromXML(Item,xNode);
    end;
  end;
end;

class function  Items.SMTP.fromXML(var Item:TRelayMail; xNode:TDOMNode):boolean;
begin
  Result:=true;
  With Core.XML.DB do begin
    with Item do begin
      UserID:=toQWord(xNode,XML.Fields.UserID);
      DomainID:=toQWord(xNode,XML.Fields.DomainID);
      FileID:=toQWord(xNode,XML.Fields.FileID);
      FolderID:=toQWord(xNode,XML.Fields.FolderID);
      InboxID:=toQWord(xNode,XML.Fields.InboxID);
      SpamID:=toQWord(xNode,XML.Fields.SpamID);
      QueueID:=toQWord(xNode,XML.Fields.QueueID);
      DNSIP:=toQWord(xNode,XML.Fields.DNSIP);
      iMXLcv:=toByte(xNode,XML.Fields.iMXLcv);
      iIPLcv:=toByte(xNode,XML.Fields.iIPLcv);
      iTryMax:=toByte(xNode,XML.Fields.iTryMax);
      iTry:=toByte(xNode,XML.Fields.iTry);
      iTransitTry:=toByte(xNode,XML.Fields.iTransitTry);
      Status:=toWord(xNode,XML.Fields.Status);
      State:=toInteger(xNode,XML.Fields.State);
      Sent:=toBoolean(xNode,XML.Fields.Sent);
      Date:=toDouble(xNode,XML.Fields.Date);

      DNSQuery:=toString(xNode,XML.Fields.DNSQuery);
      Response:=toString(xNode,XML.Fields.Response);
      MXServer:=toString(xNode,XML.Fields.MXServer);
      Domain:=toString(xNode,XML.Fields.Domain);
      Error:=toString(xNode,XML.Fields.Error);
      Helo:=toString(xNode,XML.Fields.Helo);
      From:=toString(xNode,XML.Fields.From);
      &To:=toString(xNode,XML.Fields.&To);
      Subject:=toString(xNode,XML.Fields.Subject);
      SenderIP:=toString(xNode,XML.Fields.SenderIP);
      Core.Arrays.VarString.fromString(@MXServers,toString(xNode,XML.Fields.MXServers));
    end;
  end;
end;

class function  Items.SMTP.fromXML(xDoc:TXMLDocument; var Item:TRelayMail):boolean;
var
  xNode:TDOMNode;
begin
  Result:=false;
  with Core.XML.DB do begin
    xNode:=getNode(xDOC,XML.Relay);
    if (xNode<>nil) then begin
      Result:=fromXML(Item,xNode);
    end else begin
      Empty(Item);
    end;
  end;
end;

class procedure Items.SMTP.Copy(Var Source,Destination:TRelayMail);
begin
  Destination.Status:=Source.Status;
  Destination.UserID:=Source.UserID;

  Destination.DomainID:=Source.DomainID;
  Destination.InboxID:=Source.InboxID;
  Destination.SpamID:=Source.SpamID;
  Destination.QueueID:=Source.QueueID;

  Destination.FileID:=Source.FileID;
  Destination.FolderID:=Source.FolderID;
  Destination.Sent:=Source.Sent;
  Destination.DNSIP:=Source.DNSIP;

  Destination.iMXLcv:=Source.iMXLcv;
  Destination.iIPLcv:=Source.iIPLcv;
  Destination.iTryMax:=Source.iTryMax;
  Destination.iTry:=Source.iTry;
  Destination.iTransitTry:=Source.iTransitTry;

  Destination.State:=Source.State;
  Destination.Date:=Source.Date;
  Destination.MXServer:=Source.MXServer;
  Destination.Error:=Source.Error;
  Destination.Domain:=Source.Domain;
  Destination.Helo:=Source.Helo;
  Destination.From:=Source.From;
  Destination.&To:=Source.&To;
  Destination.Subject:=Source.Subject;
  Destination.Data:=Source.Data;
  Destination.Response:=Source.Response;

  Destination.SenderIP:=Source.SenderIP;
  {$ifdef RSR_DEBUG}
  Destination.sTransactionLog:=Source.sTransactionLog;
  {$endif}




  Core.Arrays.VarString.Copy(Source.MXServers,Destination.MXServers);
  Core.Arrays.VarString.Copy(Source.IPs,Destination.IPS);
  Storage.MatrixNodes.Node.Copy(Source.Disk,Destination.Disk);
end;

class procedure Items.SMTP.Done(var Item:TRelayMail);
begin
  Finalize(Item.DNSQuery);
  Finalize(Item.MXServer);
  Finalize(Item.Error);
  Finalize(Item.Domain);
  Finalize(Item.Helo);
  Finalize(Item.From);
  Finalize(Item.&To);
  Finalize(Item.Subject);
  Finalize(Item.Data);
  Finalize(Item.SenderIP);
  Finalize(Item.Response);
  Core.Arrays.VarString.Done(Item.MXServers);
  Core.Arrays.VarString.Done(Item.IPS);
  Storage.MatrixNodes.Node.Done(Item.Disk);
  Finalize(Item);
end;

class procedure Items.SMTP.Init(var Item:TRelayMail);
begin
  Item.Status:=0;
  Item.UserID:=0;
  Item.DomainID:=0;
  Item.InboxID:=0;
  Item.SpamID:=0;
  Item.QueueID:=0;
  Item.FileID:=0;
  Item.FolderID:=0;
  Item.DNSIP:=0;
  Item.iMXLcv:=0;
  Item.iIPLcv:=0;
  Item.iTryMax:=0;
  Item.iTry:=0;
  Item.iTransitTry:=0;
  Item.State:=0;
  Item.Date:=0;
  Item.Sent:=false;

  SetLength(Item.DNSQuery,0);
  SetLength(Item.MXServer,0);
  SetLength(Item.Response,0);
  SetLength(Item.Error,0);
  SetLength(Item.Domain,0);

  SetLength(Item.Helo,0);
  SetLength(Item.From,0);
  SetLength(Item.&To,0);
  SetLength(Item.Subject,0);
  SetLength(Item.Data,0);
  SetLength(Item.SenderIP,0);
  {$ifdef RSR_DEBUG}
  SetLength(Item.sTransactionLog,0);
  {$endif}
  Core.Arrays.VarString.Init(Item.MXServers);
  Core.Arrays.VarString.Init(Item.IPs);
  Storage.MatrixNodes.Node.Init(Item.Disk);
end;

class procedure Items.SMTP.Empty(Var Item:TRelayMail);
begin
  Item.Status:=0;
  Item.UserID:=0;
  Item.DomainID:=0;
  Item.InboxID:=0;
  Item.SpamID:=0;
  Item.QueueID:=0;
  Item.FileID:=0;
  Item.FolderID:=0;
  Item.DNSIP:=0;
  Item.iMXLcv:=0;
  Item.iIPLcv:=0;
  Item.iTryMax:=0;
  Item.iTry:=0;
  Item.iTransitTry:=0;
  Item.State:=0;
  Item.Date:=0;
  Item.Sent:=false;

  SetLength(Item.DNSQuery,0);
  SetLength(Item.MXServer,0);
  SetLength(Item.Response,0);
  SetLength(Item.Error,0);
  SetLength(Item.Domain,0);
  SetLength(Item.Helo,0);
  SetLength(Item.From,0);
  SetLength(Item.&To,0);
  SetLength(Item.Subject,0);
  SetLength(Item.Data,0);
  SetLength(Item.SenderIP,0);
  {$ifdef RSR_DEBUG}
  SetLength(Item.sTransactionLog,0);
  {$endif}
  Core.Arrays.VarString.Empty(Item.MXServers);
  Core.Arrays.VarString.Empty(Item.IPS);
  Storage.MatrixNodes.Node.Empty(Item.Disk);
end;

class procedure Items.SMTP.Init(Var Item:TRecvMessage);
begin
  Item.AuthMD5:=false;
  Item.WhiteListed:=false;
  Item.ErrorPushed:=false;
  Item.MTALoopNotPushed:=true;
  Item.Blacklisted:=false;
  Item.RecipientMissCount:=0;
  Item.ErrorCount:=0;
  Item.State:=0;
  Item.Spam:=false;
  Item.FilterID:=0;
  Item.FolderID:=0;
  Item.FileID:=0;

  Item.UAP:=nil;

  Item.ContentType:=0;
  Item.CharSet:=0;
  Item.CharFormat:=0;

  SetLength(Item.BlackListDNS,0);
  SetLength(Item.WhiteListDNS,0);
  SetLength(Item.Nonce,0);
  SetLength(Item.MailFrom,0);
  SetLength(Item.Subject,0);
  SetLength(Item.SenderDomain,0);
  SetLength(Item.SenderIP,0);
  SetLength(Item.FForward,0);
  SetLength(Item.Exchanger,0);
  SetLength(Item.From,0);
  SetLength(Item.CMD,0);
  SetLength(Item.Param1,0);
  SetLength(Item.Param2,0);
  SetLength(Item.ContentName,0);

  Core.Arrays.KeyString.Init(Item.Recipients);
  Core.Arrays.VarString.Init(Item.RelayRecipients);
  Core.Arrays.KeyString.Init(Item.Headers);
end;

class procedure Items.SMTP.Init(var Item:TSummary);
begin
  SetLength(Item.&To,0);
  SetLength(Item.ReplyTo,0);
  SetLength(Item.InReplyTo,0);
  SetLength(Item.MessageId,0);
  SetLength(Item.Sender,0);
  SetLength(Item.CC,0);
  SetLength(Item.BCC,0);
  SetLength(Item.From,0);
  SetLength(Item.Subject,0);
  SetLength(Item.Group,0);
  SetLength(Item.Domain,0);
  SetLength(Item.User,0);
  SetLength(Item.RemoteIP,0);
  SetLength(Item.RemoteDomain,0);
  SetLength(Item.RemoteFrom,0);
  SetLength(Item.Lines,0);
  SetLength(Item.Exchanger,0);
  Init(Item.Mime);
  Item.Bound:=0;
  Item.cntType:=0;
  Item.UTC:=0;
  Item.Date:=0;
  Item.tzBias:=0;
  Item.Sent:=false;
  Item.Read:=false;
  Item.Replied:=false;
  Item.RepliedAll:=false;
  Item.Forwarded:=false;
  Item.Rendered:=false;
  Item.Pinned:=false;
  Item.Spam:=False;
  Item.Risk:=0;
  Item.FilterID:=0;
  Item.BlackListed:=False;
  Item.WhiteListed:=False;
  Item.Attachments:=false;
end;

class procedure Items.SMTP.Empty(var Item:TSummary);
begin
  SetLength(Item.Sender,0);
  SetLength(Item.&To,0);
  SetLength(Item.CC,0);
  SetLength(Item.BCC,0);
  SetLength(Item.From,0);
  SetLength(Item.Subject,0);
  SetLength(Item.Domain,0);
  SetLength(Item.User,0);
  SetLength(Item.Group,0);
  SetLength(Item.RemoteIP,0);
  SetLength(Item.RemoteDomain,0);
  SetLength(Item.RemoteFrom,0);
  SetLength(Item.Exchanger,0);
  SetLength(Item.Lines,0);
  SetLength(Item.Flags,0);
  SetLength(Item.ReplyTo,0);
  SetLength(Item.InReplyTo,0);
  SetLength(Item.MessageId,0);


  Empty(Item.Mime);
  Empty(Item.Deliveries);

  Item.cntType:=0;
  Item.Bound:=0;
  Item.UTC:=0;
  Item.Date:=0;
  Item.tzBias:=0;
  Item.Read:=false;
  Item.Sent:=false;
  Item.Replied:=false;
  Item.RepliedAll:=false;
  Item.Forwarded:=false;
  Item.Spam:=False;
  Item.Rendered:=false;
  Item.Pinned:=false;
  Item.Risk:=0;
  Item.FilterID:=0;
  Item.BlackListed:=False;
  Item.WhiteListed:=False;
  Item.Attachments:=False;
end;

class procedure Items.SMTP.Done(var Item:TSummary);
begin
  Done(Item.Mime);

  Finalize(Item.ReplyTo);
  Finalize(Item.InReplyTo);
  Finalize(Item.MessageId);
  Finalize(Item.Sender);
  Finalize(Item.&To);
  Finalize(Item.Subject);
  Finalize(Item.Group);
  Finalize(Item.CC);
  Finalize(Item.BCC);
  Finalize(Item.From);
  Finalize(Item.Subject);
  Finalize(Item.Lines);
  Finalize(Item.Domain);
  Finalize(Item.User);
  Finalize(Item.RemoteIP);
  Finalize(Item.RemoteDomain);
  Finalize(Item.RemoteFrom);
  Finalize(Item.Exchanger);
  Finalize(Item);
end;

class procedure Items.SMTP.Copy(var Source,Destination:TSummary);
begin
  Destination.ID:=Source.ID;
  Destination.Kind:=Source.Kind;
  Destination.Sender:=Source.Sender;
  Destination.&To:=Source.&To;
  Destination.CC:=Source.CC;
  Destination.BCC:=Source.BCC;
  Destination.From:=Source.From;
  Destination.Subject:=Source.Subject;
  Destination.Domain:=Source.Domain;
  Destination.User:=Source.User;
  Destination.Group:=Source.Group;
  Destination.RemoteIP:=Source.RemoteIP;
  Destination.RemoteDomain:=Source.RemoteDomain;
  Destination.RemoteFrom:=Source.RemoteFrom;
  Destination.Exchanger:=Source.Exchanger;
  Destination.Lines:=Source.Lines;
  Destination.Flags:=Source.Flags;
  Destination.ReplyTo:=Source.ReplyTo;
  Destination.InReplyTo:=Source.InReplyTo;
  Destination.MessageId:=Source.MessageId;
  Destination.FilterID:=Source.FilterID;
  Destination.UTC:=Source.UTC;
  Destination.Date:=Source.Date;
  Destination.tzBias:=Source.tzBias;
  Destination.Bound:=Source.Bound;
  Destination.cntType:=Source.cntType;
  Destination.Risk:=Source.Risk;
  Destination.Read:=Source.Read;
  Destination.Replied:=Source.Replied;
  Destination.RepliedAll:=Source.RepliedAll;
  Destination.Forwarded:=Source.Forwarded;
  Destination.Sent:=Source.Sent;
  Destination.Spam:=Source.Spam;
  Destination.BlackListed:=Source.BlackListed;
  Destination.WhiteListed:=Source.WhiteListed;
  Destination.Rendered:=Source.Rendered;
  Destination.Pinned:=Source.Pinned;
  Destination.Attachments:=Source.Attachments;

  Copy(Source.Deliveries,Destination.Deliveries);
  Copy(Source.Mime,Destination.Mime);
end;

class procedure Items.SMTP.Append(var Item:PMime; var Items:TMimes);
var
  idx:LongInt;
begin
  idx:=Length(Items);
  System.SetLength(Items,idx+1);
  Items[idx]:=Item;
end;

class procedure Items.SMTP.Append(var Item:PDelivery; var Items:TDeliveries);
var
  idx:LongInt;
begin
  idx:=Length(Items);
  System.SetLength(Items,idx+1);
  Items[idx]:=Item;
end;

class procedure Items.SMTP.Append(var Item:TDelivery; var Items:TDeliveries);
var
  idx:LongInt;
  itmP:PDelivery;
begin
  new(itmP);
  Copy(Item,itmP^);
  idx:=System.Length(Items);
  System.SetLength(Items,idx+1);
  Items[idx]:=itmP;
end;


class procedure Items.SMTP.Init(var Item:TMimes);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Items.SMTP.Init(var Item:TMime);
begin
  Item.idxHeadersStart:=0;
  Item.idxHeadersEnd:=0;
  Item.idxContentStart:=0;
  Item.idxContentEnd:=0;
  Item.iReadableSize:=0;
  Item.cntType:=0;
  Item.cntEncoding:=0;
  Item.cntCharSet:=0;
  Item.cntCharFormat:=0;
  Item.cntLast:=false;
  Item.cntDisposition:=0;
  Item.cntCreated:=0;
  Item.cntModified:=0;
  Item.cntRead:=0;
  Item.cntSize:=0;

  SetLength(Item.cntBoundary,0);

  SetLength(Item.cntID,0);
  SetLength(Item.cntName,0);
  SetLength(Item.cntDescription,0);
  SetLength(Item.Mimes,0);

end;

class procedure Items.SMTP.Done(Var Item:TMimes);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
  Finalize(Item);
end;

class procedure Items.SMTP.Done(Var Item:TMime);
begin
  Finalize(Item.cntBoundary);
  Finalize(Item.cntDisposition);
  Finalize(Item.cntEncoding);
  Finalize(Item.cntID);
  Finalize(Item.cntName);
  Finalize(Item.cntDescription);
  Done(Item.Mimes);

  Finalize(Item);
end;

class procedure Items.SMTP.Empty(var Item:TMimes);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Items.SMTP.Empty(var Item:TMime);
begin
  Item.idxHeadersStart:=0;
  Item.idxHeadersEnd:=0;
  Item.idxContentStart:=0;
  Item.idxContentEnd:=0;
  Item.iReadableSize:=0;

  Item.cntType:=0;
  Item.cntEncoding:=0;
  Item.cntCharSet:=0;
  Item.cntCharFormat:=0;
  Item.cntLast:=false;
  Item.cntDisposition:=0;
  Item.cntCreated:=0;
  Item.cntModified:=0;
  Item.cntRead:=0;
  Item.cntSize:=0;

  SetLength(Item.cntBoundary,0);
  SetLength(Item.cntID,0);
  SetLength(Item.cntName,0);
  SetLength(Item.cntDescription,0);

  Empty(Item.Mimes);
end;

class procedure Items.SMTP.Copy(var Source,Destination:TMime);
begin
  Destination.idxHeadersStart:=0;
  Destination.idxHeadersEnd:=0;
  Destination.idxContentStart:=0;
  Destination.idxContentEnd:=0;
  Destination.iReadableSize:=0;

  Destination.cntType:=0;
  Destination.cntEncoding:=0;
  Destination.cntCharSet:=0;
  Destination.cntCharFormat:=0;
  Destination.cntLast:=false;
  Destination.cntDisposition:=0;
  Destination.cntCreated:=0;
  Destination.cntModified:=0;
  Destination.cntRead:=0;
  Destination.cntSize:=0;

  Destination.cntBoundary:=Source.cntBoundary;
  Destination.cntID:=Source.cntID;
  Destination.cntName:=Source.cntName;
  Destination.cntDescription:=Source.cntDescription;

  Copy(Source.Mimes,Destination.Mimes);
end;

class procedure Items.SMTP.Copy(var Source,Destination:TMimes);
var
  iLcv:LongInt;
  mimeP:PMime;
begin
   Empty(Destination);
   for iLcv:=0 to High(Source) do begin
     new(mimeP);
     Copy(Source[iLcv]^,mimeP^);
     Append(mimeP,Destination);
   end;
end;

class procedure Items.SMTP.Init(var Item:TDeliveries);
var
  iLcv:LongInt;
  itmP:PDelivery;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    if (itmP<>nil) then begin
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
  SetLength(Item,0);
end;
(*
Read                   : boolean;
Code                   : LongInt;
Date                   : Double;
Message                : string;
Address                : string;
*)

class procedure Items.SMTP.Copy(var Source,Destination:TDelivery);
begin
  Destination.Read:=Source.Read;
  Destination.Code:=Source.Code;
  Destination.Date:=Source.Date;
  Destination.Message:=Source.Message;
  Destination.Address:=Source.Address;
end;

class procedure Items.SMTP.Copy(var Source,Destination:TDeliveries);
var
  iLcv:LongInt;
  iLen:LongInt;
  srcP:PDelivery;
  dstP:PDelivery;
begin
  Empty(Destination);
  iLen:=Length(Source);
  SetLength(Destination,iLen);
  for iLcv:=0 to iLen-1 do begin
    srcP:=Source[iLcv];
    new(dstP);
    Copy(srcP^,dstP^);
    Destination[iLcv]:=dstP;
  end;
end;

class procedure Items.SMTP.Init(var Item:TDelivery);
begin
  Item.Read:=false;
  Item.Code:=0;
  Item.Date:=0;
  SetLength(Item.Message,0);
  SetLength(Item.Address,0);
end;

class procedure Items.SMTP.Done(Var Item:TDeliveries);
var
  iLcv:LongInt;
  itmP:PDelivery;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    if (itmP<>nil) then begin
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
  Finalize(Item);
end;

class procedure Items.SMTP.Done(Var Item:TDelivery);
begin
  Finalize(Item.Message);
  Finalize(Item.Address);
  Finalize(Item);
end;

class procedure Items.SMTP.Empty(var Item:TDeliveries);
var
  iLcv:LongInt;
  itmP:PDelivery;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    if (itmP<>nil) then begin
      Done(itmP^);
      Dispose(itmP);
    end;
  end;
  SetLength(Item,0);
end;

class procedure Items.SMTP.Empty(var Item:TDelivery);
begin
  Item.Read:=false;
  Item.Code:=0;
  Item.Date:=0;
  SetLength(Item.Message,0);
  SetLength(Item.Address,0);
end;

class function Items.SMTP.CharSet.IndexOf(Value:Core.Strings.VarString):Byte;
var
  iLcv:byte;
begin
  Result:=csNone;
  for iLcv:=Low(Keys) to High(Keys) do begin
    if SameText(Keys[iLcv],Value) then begin
      Result:=iLcv;
      break;
    end;
  end;
end;

{class} function {Items.SMTP.CharSet}CharSetParse(Value:Core.Strings.VarString; out Format:Byte):Byte;
var
  idxStart:LongInt;
  idxEnd:LongInt;
  iLcv:byte;
  iLength:LongInt;
  iCpyStart:LongInt;
  iCpyLen:LongInt;
begin
  // text/plain; charset=UTF-8; format=flowed
  Value:=Lowercase(Value);
  iLength:=System.Length(Value);
  Result:=Items.SMTP.CharSet.csNone;
  Format:=Items.SMTP.Content.Format.None;
  if iLength>0 then begin
    idxStart:=Core.Strings.Pos('charset=',Value);
    if (idxStart>0) then begin
      if Core.Strings.Pos('format=flowed',Value)>0 then begin
        Format:=Items.SMTP.Content.Format.Flowed;
      end else if Core.Strings.Pos('format=fixed',Value)>0 then begin
        Format:=Items.SMTP.Content.Format.Fixed;
      end;
      iCpyStart:=idxStart+8;
      idxEnd:=StrUtils.PosEx(';',Value,idxStart);
      if (idxEnd=0) then begin
        idxEnd:=iLength;
        iCpyLen:=(idxEnd-iCpyStart)+1;
      end else
        iCpyLen:=(idxEnd-iCpyStart);
      Value:=System.Copy(Value,iCpyStart,iCpyLen);
      iCpyLen:=Length(Value);
      if iCpyLen>0 then begin
        If Value[1]='"' then
          System.Delete(Value,1,1);
        iCpyLen:=Length(Value);
        if iCpyLen>0 then begin
          If Value[iCpyLen]='"' then
            System.SetLength(Value,iCpyLen-1);
          idxStart:=System.Pos(#32,Value);
          if idxStart<>0 then
            System.SetLength(Value,idxStart);
        end;
      end;
      Result:=Items.SMTP.CharSet.IndexOf(Value);
    end;
  end;

end;

class procedure Items.SMTP.Copy(Var Source,Destination:TRecvMessage);
var
  iLcv:LongInt;
  Count:LongInt;
  srP:PFileStorageRequest;
  srP2:PFileStorageRequest;
begin
  Destination.AuthMD5:=Source.AuthMD5;
  Destination.Blacklisted:=Source.Blacklisted;
  Destination.WhiteListed:=Source.WhiteListed;
  Destination.Spam:=Source.Spam;
  Destination.RecipientMissCount:=Source.RecipientMissCount;
  Destination.FilterID:=Source.FilterID;
  Destination.ErrorPushed:=Source.ErrorPushed;
  Destination.MTALoopNotPushed:=Source.MTALoopNotPushed;
  Destination.FolderID:=Source.FolderID;
  Destination.FileID:=Source.FileID;
  Destination.ErrorCount:=Source.ErrorCount;
  Destination.State:=Source.State;

  Destination.UAP:=Source.UAP;
  Destination.ContentType:=Source.ContentType;
  Destination.CharSet:=Source.CharSet;
  Destination.CharFormat:=Source.CharFormat;

  Destination.Nonce:=Source.Nonce;
  Destination.BlackListDNS:=Source.BlackListDNS;
  Destination.WhiteListDNS:=Source.WhiteListDNS;
  Destination.Subject:=Source.Subject;
  Destination.SenderDomain:=Source.SenderDomain;
  Destination.MailFrom:=Source.MailFrom;
  Destination.MessageID:=Source.MessageID;
  Destination.Exchanger:=Source.Exchanger;
  Destination.SenderIP:=Source.SenderIP;
  Destination.ContentName:=Source.ContentName;
  Destination.FForward:=Source.FForward;
  Destination.From:=Source.From;
  Destination.CMD:=Source.CMD;
  Destination.Param1:=Source.Param1;
  Destination.Param2:=Source.Param2;
  // Empty Old List
  for iLcv:=0 to High(Destination.Recipients) do begin
    srP:=Destination.Recipients[iLcv]^.Data;
    if (srP<>nil) then begin
      Destination.Recipients[iLcv]^.Data:=nil;
      Storage.UserStorage.Done(srP^);
      Dispose(srP);
    end;
  end;
  Core.Arrays.KeyString.Copy(Source.Recipients,Destination.Recipients);
  // Reset New List
  Count:=Length(Source.Recipients); // same length
  for iLcv:=0 to Count-1 do begin
    srP2:=Source.Recipients[iLcv]^.Data;
    new(srP);
    Storage.UserStorage.Copy(srP2^,srP^);
    Destination.Recipients[iLcv]^.Data:=srP;
  end;
  Core.Arrays.VarString.Copy(Source.RelayRecipients,Destination.RelayRecipients);
  Core.Arrays.KeyString.Copy(Source.Headers,Destination.Headers);
  Core.Arrays.VarString.Copy(Source.Content,Destination.Content);
end;

class procedure Items.SMTP.Empty(Var Item:TRecvMessage);
var
  iLcv:LongInt;
  srP:PFileStorageRequest;
begin
  Item.AuthMD5:=false;
  Item.Blacklisted:=false;
  Item.WhiteListed:=false;
  Item.Spam:=false;
  Item.RecipientMissCount:=0;
  Item.FilterID:=0;
  Item.FolderID:=0;
  Item.FileID:=0;

  Item.ErrorPushed:=false;
  Item.MTALoopNotPushed:=true;

  Item.ErrorCount:=0;
  Item.State:=0;

  Item.UAP:=nil;
  Item.ContentType:=0;
  Item.CharSet:=0;
  Item.CharFormat:=0;


  SetLength(Item.Nonce,0);
  SetLength(Item.BlackListDNS,0);
  SetLength(Item.WhiteListDNS,0);
  SetLength(Item.Subject,0);
  SetLength(Item.SenderDomain,0);
  SetLength(Item.MailFrom,0);
  SetLength(Item.MessageID,0);
  SetLength(Item.Exchanger,0);
  SetLength(Item.SenderIP,0);
  SetLength(Item.ContentName,0);
  SetLength(Item.FForward,0);
  SetLength(Item.From,0);
  SetLength(Item.CMD,0);
  SetLength(Item.Param1,0);
  SetLength(Item.Param2,0);

  for iLcv:=0 to High(Item.Recipients) do begin
    srP:=Item.Recipients[iLcv]^.Data;
    if (srP<>nil) then begin
      Storage.UserStorage.Done(srP^);
      Dispose(srP);
    end;
  end;
  Core.Arrays.KeyString.Empty(Item.Recipients);
  Core.Arrays.VarString.Empty(Item.RelayRecipients);
  Core.Arrays.KeyString.Empty(Item.Headers);
  Core.Arrays.VarString.Empty(Item.Content);
end;

class procedure Items.SMTP.Done(var Item:TRecvMessage);
var
  iLcv:LongInt;
  srP:PFileStorageRequest;
begin
  Finalize(Item.Nonce);
  Finalize(Item.BlackListDNS);
  Finalize(Item.WhiteListDNS);
  Finalize(Item.Subject);
  Finalize(Item.SenderDomain);
  Finalize(Item.MailFrom);
  Finalize(Item.MessageID);
  Finalize(Item.Exchanger);
  Finalize(Item.SenderIP);
  Finalize(Item.ContentName);
  Finalize(Item.FForward);
  Finalize(Item.From);
  Finalize(Item.CMD);
  Finalize(Item.Param1);
  Finalize(Item.Param2);

  for iLcv:=0 to High(Item.Recipients) do begin
    srP:=Item.Recipients[iLcv]^.Data;
    if (srP<>nil) then begin
      Storage.UserStorage.Done(srP^);
      Item.Recipients[iLcv]^.Data:=nil;
      Dispose(srP);
    end;
  end;
  Core.Arrays.KeyString.Done(Item.Recipients);
  Core.Arrays.VarString.Done(Item.RelayRecipients);
  Core.Arrays.KeyString.Done(Item.Headers);
  Core.Arrays.VarString.Done(Item.Content);
  Finalize(Item);
end;

class procedure Items.IMAP.fromString(var Source:Core.Strings.VarString; var Item:TAddress);
var
  sAddr:Core.Strings.VarString;
begin
  sAddr:=Core.Utils.Mail.ExtractAddress(Source);
  Item.Name:=Core.Utils.Mail.ExtractName(Source);
  Item.Mailbox:=Core.Utils.Mail.ExtractUserName(sAddr);
  Item.Host:=Core.Utils.Mail.ExtractDomain(sAddr);
end;

class procedure Items.IMAP.fromString(var Source:Core.Strings.VarString; var Item:TAddresses);
begin
  Empty(Item);
  SetLength(Item,1);
  fromString(Source,Item[0]);
end;

class procedure Items.IMAP.fromString(var Source:Core.Arrays.Types.VarString; var Item:TAddresses);
var
  iLength:LongInt;
  iLcv:LongInt;
begin
  Empty(Item);
  iLength:=Length(Source);
  SetLength(Item,iLength);
  for iLcv:=0 to iLength-1 do
    fromString(Source[iLcv],Item[iLcv]);
end;

class procedure Items.IMAP.Empty(var Item:TAddresses);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;

class procedure Items.IMAP.Empty(var Item:TRecordCount);
begin
  Item.Total:=0;
  Item.Unread:=0;
  Item.Recent:=0;
  Item.Read:=0;
  Item.Deleted:=0;
  Item.Pages:=0;
end;
class procedure Items.IMAP.Empty(var Item:TDisposition);
begin
  Item.&Type:=0;
  SetLength(Item.Name,0);
end;

class procedure Items.IMAP.Empty(var Item:TBodyElement);
begin
  With Item do begin
    SetLength(&Type,0);
    SetLength(SubType,0);
    Core.Arrays.KeyString.Empty(Parameters);
    SetLength(ID,0);
    SetLength(Description,0);
    SetLength(Encoding,0);
    Size:=0;
    Lines:=0;
    Empty(Item.Disposition);
    Empty(Item.SubElements);
  end;
end;

class procedure Items.IMAP.Empty(var Item:TBodyElements);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Items.IMAP.Empty(var Item:TAddress);
begin
  with Item do begin
    SetLength(Name,0);
    SetLength(ADL,0);
    SetLength(Mailbox,0);
    SetLength(Host,0);
  end;
end;

class procedure Items.IMAP.Empty(var Item:TEnvelope);
begin
  with Item do begin
    SetLength(Date,0);
    SetLength(Subject,0);
    Empty(From);
    Empty(Sender);
    Empty(ReplyTo);
    Empty(&To);
    Empty(CC);
    Empty(BCC);
    SetLength(InReplyTo,0);
    SetLength(MessageId,0);
  end;
end;

class procedure Items.IMAP.Init(var Item:TBodyElement);
begin
  With Item do begin
    SetLength(&Type,0);
    SetLength(SubType,0);
    Core.Arrays.KeyString.Init(Parameters);
    SetLength(ID,0);
    SetLength(Description,0);
    SetLength(Encoding,0);
    Size:=0;
    Lines:=0;
    Init(Disposition);
    Init(SubElements);
  end;
end;

class procedure Items.IMAP.Init(var Item:TBodyElements);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv]);
  end;
  SetLength(Item,0);
end;

class procedure Items.IMAP.Append(ItemP:PBodyElement; var List:TBodyElements);
var
  iLen:LongInt;
begin
  iLen:=System.Length(List);
  SetLength(List,iLen+1);
  List[iLen]:=ItemP;
end;

class procedure Items.IMAP.Init(var Item:TAddresses);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  SetLength(Item,0);
end;


class procedure Items.IMAP.Init(var Item:TAddress);
begin
  with Item do begin
    SetLength(Name,0);
    SetLength(ADL,0);
    SetLength(Mailbox,0);
    SetLength(Host,0);
  end;
end;

class procedure Items.IMAP.Init(var Item:TRecordCount);
begin
  Item.Total:=0;
  Item.Unread:=0;
  Item.Recent:=0;
  Item.Read:=0;
  Item.Deleted:=0;
  Item.Pages:=0;
end;

class procedure Items.IMAP.Init(var Item:TDisposition);
begin
  Item.&Type:=0;
  SetLength(Item.Name,0);
end;

class procedure Items.IMAP.Init(var Item:TEnvelope);
begin
  with Item do begin
    SetLength(Date,0);
    SetLength(Subject,0);
    Init(From);
    Init(Sender);
    Init(ReplyTo);
    Init(&To);
    Init(CC);
    Init(BCC);
    SetLength(InReplyTo,0);
    SetLength(MessageId,0);
  end;
end;

class procedure Items.IMAP.Done(var Item:TRecordCount);
begin
  Finalize(Item);
end;

class procedure Items.IMAP.Done(var Item:TDisposition);
begin
  Finalize(Item.Name);
  Finalize(Item);
end;

class procedure Items.IMAP.Done(var Item:TBodyElement);
begin
  With Item do begin
    Finalize(Kind);
    Finalize(SubType);
    Core.Arrays.KeyString.Done(Parameters);
    Finalize(ID);
    Finalize(Description);
    Finalize(Encoding);
    Done(Disposition);
    Done(SubElements);
  end;
  Finalize(Item);
end;

class procedure Items.IMAP.Done(var Item:TBodyElements);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do begin
    Done(Item[iLcv]^);
    Dispose(Item[iLcv])
  end;
  Finalize(Item);
end;


class procedure Items.IMAP.Done(var Item:TAddresses);
var
  iLcv:LongInt;
begin
  for iLcv:=0 to High(Item) do
    Done(Item[iLcv]);
  Finalize(Item);
end;

class procedure Items.IMAP.Done(var Item:TAddress);
begin
  with Item do begin
    Finalize(Name);
    Finalize(ADL);
    Finalize(Mailbox);
    Finalize(Host);
  end;
  Finalize(Item);
end;

class procedure Items.IMAP.Done(var Item:TEnvelope);
begin
  with Item do begin
    Finalize(Date);
    Finalize(Subject);
    Done(From);
    Done(Sender);
    Done(ReplyTo);
    Done(&To);
    Done(CC);
    Done(BCC);
    Finalize(InReplyTo);
    Finalize(MessageId);
  end;
  Finalize(Item);
end;

class function  Items.IMAP.toString(var Item:TAddresses):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  SetLength(Result,0);
  If Length(Item)>0 then begin
    Result:='(';
    for iLcv:=0 to High(Item) do
      Result:=Concat(Result,'(',toString(Item[iLcv]),')');
    Result:=Concat(Result,')');
  end else begin
    Result:='NIL';
  end;
end;

class function  Items.IMAP.toString(State:LongInt):Core.Strings.VarString;
begin
  Result:='FLAGS (';
  if State or Flags.Seen=State then
    Result:=Concat(Result,'\Seen ');
  if State or Flags.Answered=State then
    Result:=Concat(Result,'\Answered ');
  if State or Flags.Flagged=State then
    Result:=Concat(Result,'\Flagged ');
  if State or Flags.Deleted=State then
    Result:=Concat(Result,'\Deleted ');
  if State or Flags.Draft=State then
    Result:=Concat(Result,'\Draft ');
  if State or Flags.Recent=State then
    Result:=Concat(Result,'\Recent ');
  if State or Flags.NoSelect=State then
    Result:=Concat(Result,'\NoSelect ');
  Core.Strings.TrimRight(Result);
  Result:=Concat(Result,')');
end;

class function  Items.IMAP.toString(Stamp:Double):Core.Strings.VarString;
begin
  Result:=Concat('INTERNALDATE "',Core.Utils.Time.toString(Stamp,pktInternal,pkfUTC),'"');
end;

class function  Items.IMAP.toString(Stamp:QWord):Core.Strings.VarString;
begin
  Result:=Concat('RFC822.SIZE ',IntToStr(Stamp));
end;

class function  Items.IMAP.toString(var Item:TBodyElements):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  SetLength(Result,0);
  for iLcv:=0 to High(Item) do
    Result:=Concat(Result,'(',toString(Item[iLcv]^),')');
end;

class function  Items.IMAP.toStringWithSubs(var Item:TBodyElement; const ExData:boolean=false):Core.Strings.VarString;
var
  iLcv:LongInt;
begin
  if Length(Item.SubElements)=0 then begin
    Result:=Concat('(',toStringExtended(Item,ExData),')');
  end else begin
    Result:='(';
    for iLcv:=0 to High(Item.SubElements) do
      Result:=Concat(Result,toStringWithSubs(Item.SubElements[iLcv]^,ExData));
    Result:=Concat(Result,' "',Item.SubType,'"');
    if Length(Item.Parameters)>0  then begin
      Result:=Concat(Result,' (');
      for iLcv:=0 to High(Item.Parameters) do
        Result:=Concat(Result,'"',Item.Parameters[iLcv]^.Key,'" "',Item.Parameters[iLcv]^.Value,'" ');
      Core.Strings.TrimRight(Result);
      Result:=Concat(Result,') NIL NIL');
    end;
    Result:=Concat(Result,')');
  end;
end;

class function  Items.IMAP.toStringExtended(var Item:TBodyElement; const ExData:boolean=false):Core.Strings.VarString;
var
  iLcv:LongInt;

  procedure AppendSizeAndLines();
  begin
    Result:=Concat(Result,IntToStr(Item.Size),' ');
    Result:=Concat(Result,IntToStr(Item.Lines));
    if (ExData=true) then begin
      Result:=Concat(Result,' NIL NIL NIL');
    end;
  end;

  procedure AppendSizeAndAttachment();
  begin
    Result:=Concat(Result,IntToStr(Item.Size),' ');
    if (ExData=true) then begin
      Result:=Concat(
        Result,
        'NIL ',
        '("',Items.SMTP.Content.Disposition.Value[Items.SMTP.Content.Disposition.Attachment],'" ',
         '("FILENAME" "',Item.Disposition.Name,'")',
        ') ',
        'NIL'
      );
    end;
  end;

begin
  Result:=Concat(
    '"',Item.&Type,'" ',
    '"',Item.SubType,'" ',
    '('
  );
  if Length(Item.Parameters)>0 then begin
    for iLcv:=0 to High(Item.Parameters) do
      Result:=Concat(Result,'"',Item.Parameters[iLcv]^.Key,'" "',Item.Parameters[iLcv]^.Value,'" ');
    Core.Strings.TrimRight(Result);
    Result:=Concat(Result,') ');
  end else begin
    Result:=Concat(Result,'NIL) ');
  end;
  If Length(Item.ID)>0 then
    Result:=Concat(Result,'"',Item.ID,'" ')
  else
    Result:=Concat(Result,'NIL ');
  If Length(Item.Description)>0 then
    Result:=Concat(Result,'"',Item.Description,'" ')
  else
    Result:=Concat(Result,'NIL ');

  If Length(Item.Encoding)>0 then begin
    Result:=Concat(Result,'"',Item.Encoding,'" ');
  end else begin
    Result:=Concat(Result,'NIL ');
  end;

  case Item.Disposition.&Type of
    Items.SMTP.Content.Disposition.None : AppendSizeAndLines();
    Items.SMTP.Content.Disposition.Inline : AppendSizeAndLines();
    Items.SMTP.Content.Disposition.Attachment : AppendSizeAndAttachment();
  end;
end;

class function  Items.IMAP.toString(var Item:TBodyElement; const ExData:boolean=false):Core.Strings.VarString;
begin
  Result:=toStringWithSubs(Item,ExData);
end;

class function  Items.IMAP.toString(var Item:TAddress):Core.Strings.VarString;
begin
  SetLength(Result,0);
  if ( Length(Item.Name)>0) then
    Result:=Concat(Result,'"',Item.Name,'" ')
  else
    Result:=Concat(Result,'NIL ');

  if ( Length(Item.ADL)>0) then
    Result:=Concat(Result,'"',Item.ADL,'" ')
  else
    Result:=Concat(Result,'NIL ');

  if ( Length(Item.Mailbox)>0) then
    Result:=Concat(Result,'"',Item.Mailbox,'" ')
  else
    Result:=Concat(Result,'NIL ');

  if ( Length(Item.Host)>0) then
    Result:=Concat(Result,'"',Item.Host,'"')
  else
    Result:=Concat(Result,'NIL');
end;

class procedure Items.IMAP.Paginate(var Records:TRecordCount; ItemsPerPage:Word);
begin
  Records.Pages:=Records.Total div ItemsPerPage;
  if (Records.Total mod ItemsPerPage)>0 then
    Records.Pages+=1;
end;

class function  Items.IMAP.toXML(var Item:TRecordCount; Output:TMemoryStream; Header:boolean):boolean;
begin
  Result:=false;
  Output.Position:=Output.Size;
  if Header then
    Core.XML.DB.Stamp(Storage.Main.Header.Encoding,Output);

  Core.Streams.Write('<',1,Output);
  Core.Streams.Write(XML.Records,Output);
  Core.Streams.Write('>',1,Output);
  with Core.XML.DB do begin
    Core.Streams.Write(Print(XML.Fields.Total,Item.Total),Output);
    Core.Streams.Write(Print(XML.Fields.Unread,Item.Unread),Output);
    Core.Streams.Write(Print(XML.Fields.Recent,Item.Recent),Output);
    Core.Streams.Write(Print(XML.Fields.Read,Item.Read),Output);
    Core.Streams.Write(Print(XML.Fields.Deleted,Item.Deleted),Output);
    Core.Streams.Write(Print(XML.Fields.Pages,Item.Pages),Output);
  end;
  Core.Streams.Write('</',2,Output);
  Core.Streams.Write(XML.Records,Output);
  Core.Streams.Write('>',1,Output);

  Result:=true;
end;

class function  Items.IMAP.toString(var Item:TEnvelope; Refactor:TMemoryStream):Core.Strings.VarString;
begin
  SetLength(Result,0);
  Refactor.Size:=0;
  // Fields In Streaming Order
  Core.Streams.Write('ENVELOPE ("',Refactor);
  Core.Streams.Write(Item.Date,Refactor);
  Core.Streams.Write('" ',Refactor);
  if Length(Item.Subject)>0 then
    Core.Streams.Write(Concat('"',Item.Subject,'" '),Refactor)
  else
    Core.Streams.Write('NIL ',Refactor);
  Core.Streams.Write(toString(Item.From),Refactor);
  Core.Streams.Write(#32,Refactor);
  Core.Streams.Write(toString(Item.Sender),Refactor);
  Core.Streams.Write(#32,Refactor);
  Core.Streams.Write(toString(Item.ReplyTo),Refactor);
  Core.Streams.Write(#32,Refactor);
  Core.Streams.Write(toString(Item.&To),Refactor);
  Core.Streams.Write(#32,Refactor);
  Core.Streams.Write(toString(Item.CC),Refactor);
  Core.Streams.Write(#32,Refactor);
  Core.Streams.Write(toString(Item.BCC),Refactor);
  Core.Streams.Write(#32,Refactor);
  if (Length(Item.InReplyTo)>0) then
    Core.Streams.Write(Concat('"',Item.InReplyTo,'" '),Refactor)
  else
    Core.Streams.Write('NIL ',Refactor);

  if (Length(Item.MessageId)>0) then
    Core.Streams.Write(Concat('"',Item.MessageId,'")'),Refactor)
  else
    Core.Streams.Write('NIL)',Refactor);

  Result:=Core.Streams.toString(Refactor);

  Refactor.Size:=0;
end;


function  ParseBody(var Summary:Items.SMTP.TSummary; var Body:Items.IMAP.TBodyElement):boolean;

  procedure ProcessAsFile(var Mime:Items.SMTP.TMime; var Element:Items.IMAP.TBodyElement);
  var
    sExt:Core.Strings.VarString;
  begin
    sExt:=Core.Utils.Files.Extract(Mime.cntName,efeoNone);
    Element.&Type:=Items.SMTP.Content.MainType[Mime.cntType];
    Element.SubType:=RSR.HTTP.ContentSubTypeFromName(Storage.ContentTypes.List,Mime.cntName);
    Core.Arrays.KeyString.Add(@Element.Parameters,'name',Mime.cntName);
    Element.Disposition.&Type:=Mime.cntDisposition;
    Element.Disposition.Name:=Mime.cntName;
  end;

  procedure ProcessAsReadable(Var Mime:Items.SMTP.TMime; var Element:Items.IMAP.TBodyElement);
  begin
    if (Mime.cntCharSet>0) then
      Core.Arrays.KeyString.Add(@Element.Parameters,'charset',Items.SMTP.Charset.Keys[Mime.cntCharSet]);
    if (Mime.cntCharFormat>0) then
      Core.Arrays.KeyString.Add(@Element.Parameters,'format',Items.SMTP.Content.Format.Value[Mime.cntCharFormat]);
  end;

  procedure SetupExtendedDetails(var Mime:Items.SMTP.TMime; var Element:Items.IMAP.TBodyElement);
  begin
    Element.&Type:=Items.SMTP.Content.MainType[Mime.cntType];
    Element.SubType:=Items.SMTP.Content.SubType[Mime.cntType];
    Case Mime.cntDisposition of
      Items.SMTP.Content.Disposition.None:ProcessAsReadable(Mime,Element);
      Items.SMTP.Content.Disposition.Inline:ProcessAsReadable(Mime,Element);
      Items.SMTP.Content.Disposition.Attachment:ProcessAsFile(Mime,Element);
    end;
    Element.ID:=Mime.cntID;
    Element.Description:=Mime.cntDescription;
    if (Mime.cntEncoding<>0) then
      Element.Encoding:=Items.SMTP.Encoding.Value[Mime.cntEncoding];
    Element.Size:=Mime.iReadableSize;
    Element.Lines:=Mime.idxContentEnd-Mime.idxContentStart;
  end;

  procedure ParseMimes(var Mimes:Items.SMTP.TMimes; var Parent:Items.IMAP.TBodyElement);
  var
    iLen:LongInt;
    iLcv:LongInt;
    mimeP:Items.SMTP.PMime;
    elemP:Items.IMAP.PBodyElement;

  begin
    iLen:=Length(Mimes);
    for iLcv:=0 to iLen-1 do begin
      mimeP:=Mimes[iLcv];
      new(elemP);
      Items.IMAP.Init(elemP^);
      Items.IMAP.Append(elemP,Parent.SubElements);
      if Length(mimeP^.cntBoundary)>0 then
        Core.Arrays.KeyString.Add(@elemP^.Parameters,'boundary',mimeP^.cntBoundary);
      if Length(mimeP^.Mimes)>0 then begin
        Case mimeP^.cntType of
          Items.SMTP.Content.ctApplication : ProcessAsFile(mimeP^,elemP^);
          Items.SMTP.Content.ctImage       : ProcessAsFile(mimeP^,elemP^);
          else begin
            elemP^.&Type:=Items.SMTP.Content.MainType[mimeP^.cntType];
            elemP^.SubType:=Items.SMTP.Content.SubType[mimeP^.cntType];
          end;
        end;
        ParseMimes(mimeP^.Mimes,elemP^);
      end else begin;
        SetupExtendedDetails(mimeP^,elemP^);
      end;
    end;
  end;

  procedure ParseRootMime(var Mime:Items.SMTP.TMime; var Element:Items.IMAP.TBodyElement);
  begin
    if Length(Mime.cntBoundary)>0 then
      Core.Arrays.KeyString.Add(@Element.Parameters,'boundary',Mime.cntBoundary);
    SetupExtendedDetails(Mime,Element);
    ParseMimes(Mime.Mimes,Element);
  end;
begin
  Result:=False;
  Items.IMAP.Empty(Body);
  ParseRootMime(Summary.Mime,Body);
end;

function  ParseEnvelope(var Content:Core.Arrays.Types.VarString; var Summary:Items.SMTP.TSummary; var Envelope:Items.IMAP.TEnvelope):boolean;
begin
  Core.Arrays.VarString.fromString(Content,Summary.ReplyTo,',',[soClearList,soTrimLines]);
  Items.IMAP.fromString(Content,Envelope.ReplyTo);

  Core.Arrays.VarString.fromString(Content,Summary.&To,',',[soClearList,soTrimLines]);
  Items.IMAP.fromString(Content,Envelope.&To);

  Core.Arrays.VarString.fromString(Content,Summary.CC,',',[soClearList,soTrimLines]);
  Items.IMAP.fromString(Content,Envelope.CC);

  Core.Arrays.VarString.fromString(Content,Summary.BCC,',',[soClearList,soTrimLines]);
  Items.IMAP.fromString(Content,Envelope.BCC);

  Envelope.Date:=Core.Utils.Time.toString(Summary.UTC,pktRFC822,pkfUTC);
  Envelope.Subject:=Summary.Subject;

  Items.IMAP.fromString(Summary.From,Envelope.From);
  Items.IMAP.fromString(Summary.Sender,Envelope.Sender);

  Envelope.InReplyTo:=Summary.InReplyTo;
  Envelope.MessageID:=Summary.MessageId;
  Result:=true;
end;


initialization
  RegisterDBM;
end.


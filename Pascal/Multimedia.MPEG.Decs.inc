  TMPEGFrame=class;
  TMPEGHeader=class(TFrameHeader)
  const
      {                                  | l |
      |aaaaaaaaaaa|bb|cc|d|eeee|ff|g|h|ii|j|k|m|n|oo|
      |-----------------|-|----|--|-|-|--|-|-|-|-|--|
      |11111111111|11|01|1|0000|  | | |  | | | | |  |
      |-----------|--|--|-|----|--|-|-|--|-|-|-|-|--|
      |33222222222|21|11|1|1111|11|9|8|76|5|4|3|2|10|
      |10987654321|09|87|6|5432|10| | |  | | | | |  |
      |-----------|--|--|-|----|--|-|-|--|-|-|-|-|--|
      |01234567891|11|11|1|1111|22|2|2|22|2|2|2|2|33|
      |          0|12|34|5|6789|01|2|3|45|6|7|8|9|01|
      ----------------------------------------------
      }

      ID_MASK            : Cardinal = 4292870144;   // a
      ID_SHIFT           : Cardinal = 21;
      VERSION_MASK       : Cardinal = 1572864;      // b
      VERSION_SHIFT      : Cardinal = 19;
      LAYER_MASK         : Cardinal = 393216;       // c
      LAYER_SHIFT        : Cardinal = 17;
      CRC_MASK           : Cardinal = 65536;        // d
      CRC_SHIFT          : Cardinal = 16;
      BITRATE_MASK       : Cardinal = 61440;        // e
      BITRATE_SHIFT      : Cardinal = 12;
      FREQUENCY_MASK     : Cardinal = 3072;         // f
      FREQUENCY_SHIFT    : Cardinal = 10;
      PADDED_MASK        : Cardinal = 512;          // g
      PADDED_SHIFT       : Cardinal = 9;
      PRIVACY_MASK       : Cardinal = 256;          // h
      PRIVACY_SHIFT      : Cardinal = 8;
      MODE_MASK          : Cardinal = 192;          // i
      MODE_SHIFT         : Cardinal = 6;
      MODE_EXT_INTENSITY : Cardinal = 32;           // j
      MODE_EXT_INT_SHIFT : Cardinal = 5;
      MODE_EXT_MS        : Cardinal = 16;           // k
      MODE_EXT_MS_SHIFT  : Cardinal = 4;
      MODE_EXT_MASK      : Cardinal = 48;           // l
      MODE_EXT_SHIFT     : Cardinal = 4;
      COPYRIGHT_MASK     : Cardinal = 8;            // m
      COPYRIGHT_SHIFT    : Cardinal = 3;
      ORIGINAL_MASK      : Cardinal = 4;            // n
      ORIGINAL_SHIFT     : Cardinal = 2;
      EMPHASIS_MASK      : Cardinal = 3;            // o
      EMPHASIS_SHIFT     : Cardinal = 0;

      BIT_RATE        : Array[0..15] of word = (0, 32, 40, 48, 56, 64, 80, 96, 112, 128, 160, 192, 224, 256, 320, 384);
     { 2.4.2.3 Header
        bit_rate_index -> bitrate mapping
        '0000'  free format
        '0001'  32 kbit/s
        '0010'  40 kbit/s
        '0011'  48 kbit/s
        '0100'  56 kbit/s
        '0101'  64 kbit/s
        '0110'  80 kbit/s
        '0111'  96 kbit/s
        '1000' 112 kbit/s
        '1001' 128 kbit/s
        '1010' 160 kbit/s
        '1011' 192 kbit/s
        '1100' 224 kbit/s
        '1101' 256 kbit/s
        '1110' 320 kbit/s
      }
      SAMPLING_FREQUENCY : Array[0..3] of WORD = (44100, 48000, 32000, 0);
      { 2.4.2.3
        sampling_frequency
        '00' 44.1 kHz
        '01' 48 kHz
        '10' 32 kHz
        '11' reserved
      }
      type
                         //    0        1          2     3
        MPEGVersion      = (MPEG25,MPEGReserved,MPEG2,MPEG1);
        MPEGLayer        = (layerReserved,layerIII,layerII,layerI);
        Layer            = (I,II,III);
        Version          = (MPEGI,MPEGII,MPEGII_V);
        TBitRateValueMap = Array[0..15] of dword;
        TBitRateMap      = Array[Layer] of Array[Version] of TBitRateValueMap;
        TSampleRateValueMap = Array[0..3] of word;
        TSampleRateMap   = Array[Version] of TSampleRateValueMap;
      Const
        SampleRates:TSampleRateMap=(
          ( {MPEGI}
            {O0} 44100,
            {01} 48000,
            {10} 32000,
            {11} 22050

          ),
          ( {MPEGII}
            {0} 22050,
            {1} 24000,
            {2} 16000,
            {3} 11025
          ),
          ( {MPEGII_V}
            {0} 11025,
            {1} 12000,
            {2} 8000,
            {3} 0
          )
        );
        BitRates:TBitRateMap=(
          {layer1}
            (
              (  {version1}
                {0}  0,
                {1}  32000,
                {2}  64000,
                {3}  96000,
                {4}  128000,
                {5}  160000,
                {6}  192000,
                {7}  224000,
                {8}  256000,
                {9}  288000,
                {10} 320000,
                {11} 352000,
                {12} 384000,
                {13} 416000,
                {14} 448000,
                {15} 0
              ),
              ( {version2}
                {0}  0,
                {1}  32000,
                {2}  48000,
                {3}  56000,
                {4}  64000,
                {5}  80000,
                {6}  96000,
                {7}  112000,
                {8}  128000,
                {9}  160000,
                {10} 176000,
                {11} 192000,
                {12} 224000,
                {13} 256000,
                {14} 320000,
                {15}   0
              ),
              ( {version2.5}
                {0}  0,
                {1}  8000,
                {2}  16000,
                {3}  24000,
                {4}  32000,
                {5}  40000,
                {6}  48000,
                {7}  56000,
                {8}  64000,
                {9}  80000,
                {10} 96000,
                {11} 112000,
                {12} 128000,
                {13} 144000,
                {14} 160000,
                {15}   0
              )
            ),
          {layer2}
            (
              ( {version 1}
                {0}  0,
                {1}  32000,
                {2}  48000,
                {3}  56000,
                {4}  64000,
                {5}  80000,
                {6}  96000,
                {7}  112000,
                {8}  128000,
                {9}  160000,
                {10} 192000,
                {11} 224000,
                {12} 256000,
                {13} 320000,
                {14} 384000,
                {15} 0
              ),
              ( {version 2}
                {0}  0,
                {1}  8000,
                {2}  16000,
                {3}  24000,
                {4}  32000,
                {5}  40000,
                {6}  48000,
                {7}  56000,
                {8}  64000,
                {9}  80000,
                {10} 96000,
                {11} 112000,
                {12} 128000,
                {13} 144000,
                {14} 160000,
                {15}   0
              ),
              ( {version 2.5}
                {0}  0,
                {1}  8000,
                {2}  16000,
                {3}  24000,
                {4}  32000,
                {5}  40000,
                {6}  48000,
                {7}  56000,
                {8}  64000,
                {9}  80000,
                {10} 96000,
                {11} 112000,
                {12} 128000,
                {13} 144000,
                {14} 160000,
                {15}   0
              )
            ),
          {layer3}
            (
              ( {version 1}
                {0}  0,
                {1}  32000,
                {2}  40000,
                {3}  48000,
                {4}  56000,
                {5}  64000,
                {6}  80000,
                {7}  96000,
                {8}  112000,
                {9}  128000,
                {10} 160000,
                {11} 192000,
                {12} 224000,
                {13} 256000,
                {14} 320000,
                {15} 0
              ),
              ( {version 2}
                {0}  0,
                {1}  8000,
                {2}  16000,
                {3}  24000,
                {4}  32000,
                {5}  40000,
                {6}  48000,
                {7}  56000,
                {8}  64000,
                {9}  80000,
                {10} 96000,
                {11} 112000,
                {12} 128000,
                {13} 144000,
                {14} 160000,
                {15}   0
              ),
              ( {version 2.5}
                {0}  0,
                {1}  8000,
                {2}  16000,
                {3}  24000,
                {4}  32000,
                {5}  40000,
                {6}  48000,
                {7}  56000,
                {8}  64000,
                {9}  80000,
                {10} 96000,
                {11} 112000,
                {12} 128000,
                {13} 144000,
                {14} 160000,
                {15}   0
              )
            )
        );
  private
    FData : cardinal;
  public
    procedure Reset(); override;
    function Load(Stream:TStream):boolean; override;
    function  Frame:TMPEGFrame;
  end;

  TMPEGPayload=class(TFramePayload)
  public
    function  Header:TMPEGHeader;
    procedure Reset(); override;
    procedure Load(Stream:TStream; var Handled:Boolean); override;
  end;

  TMPEGFrame=class(TFrame)
  public
    procedure Reset(); override;
    function  Load(Stream:TStream; var Handled:Boolean):boolean; override;
    constructor Create(aReader:TReader); reIntroduce;
  end;


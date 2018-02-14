unit Encryption.SSL;

{$MODE DELPHI}{$H+}
{$Packrecords C}

interface

uses
  DynLibs, cTypes, SysUtils,
  Sockets,
  DateUtils,

  Encryption,
  Core.Strings,
  Core.Arrays.Types,
  Core.Arrays.VarString,
  Core.Arrays.Bytes;


var
  {$IFDEF WINDOWS}
  DLLSSLName: Core.Strings.VarString = 'ssleay32.dll';
  DLLSSLName2: Core.Strings.VarString = 'libssl32.dll';
  DLLUtilName: Core.Strings.VarString = 'libeay32.dll';
  {$ELSE}
  DLLSSLName: Core.Strings.VarString = 'libssl';
  DLLUtilName: Core.Strings.VarString = 'libcrypto';
  
  { ADD NEW ONES WHEN THEY APPEAR!
    Always make .so/dylib first, then versions, in descending order!
    Add "." .before the version, first is always just "" }
  DLLVersions: array[1..16] of Core.Strings.VarString = ('', '.1.0.6', '.1.0.5', '.1.0.4', '.1.0.3',
                                        '.1.0.2', '.1.0.1','.1.0.0','.0.9.8',
                                        '.0.9.7', '.0.9.6', '.0.9.5', '.0.9.4',
                                        '.0.9.3', '.0.9.2', '.0.9.1');
  {$ENDIF}

  CIPHER_LIST_SERVER : PChar = (
    'DEFAULT'
    (*
    'DHE-RSA-AES256-SHA256:'+
    'AES256-SHA256:'+
    'AES256-GCM-SHA38:'+
    *)
  );

  CIPHER_LIST_CLIENT : PChar = 'DEFAULT';

Type
  PCertData=^TCertData;
  TCertData=Record
    ID                           : System.QWord;
    Key                          : Core.Strings.VarString;
    Request                      : Core.Strings.VarString;
    Date                         : System.Double;
    DerKey                       : Core.Arrays.Types.Bytes;
    Level                        : System.Byte;
    Certs                        : Core.Arrays.Types.VarString;
    DerCerts                     : Core.Arrays.Types.BytesManifest;
  end;
  PCertList=^TCertList;
  TCertList=Array of PCertData;
const
  // EVP.h Constants

  EVP_MAX_MD_SIZE               = 64; //* longest known is SHA512 */
  EVP_MAX_KEY_LENGTH    = 32;
  EVP_MAX_IV_LENGTH     = 16;
  EVP_MAX_BLOCK_LENGTH  = 32;

  SHA_DIGEST_LENGTH = 20;

type
  SslPtr = System.Pointer;
  PSslPtr = ^SslPtr;
  PSSL_CTX = SslPtr;
  PSSL = SslPtr;
  PSSL_METHOD = SslPtr;
  PEVP_MD	= SslPtr;
  PBIO_METHOD = SslPtr;
  PBIO = SslPtr;

  PRSA = System.Pointer;
  PDH = System.Pointer;
  PSTACK_OFX509 = System.Pointer;

  X509_STORE = System.Pointer;

  X509_NAME = record
    entries: System.Pointer;
    modified: LongInt;
    bytes: System.Pointer;
    hash: System.cardinal;
  end;
  PX509_NAME = ^X509_NAME;
  PDN = ^X509_NAME;

  ASN1_STRING = record
    length : LongInt;
    kind   : LongInt;
    data   : System.PChar;
    flags  : System.longint;
  end;
  PASN1_STRING = ^ASN1_STRING;
  PASN1_TIME = PASN1_STRING;

  X509_VAL = record
    notBefore      : PASN1_TIME;    // ^ASN1_TIME
    notAfter       : PASN1_TIME;    // ^ASN1_TIME
  end;
  PX509_VAL = ^X509_VAL;

  ASN1_ENCODING=record
    enc            : System.PChar;         // DER encoding
    len            : LongInt;       // Length of encoding
    modified       : LongInt;       // set to 1 if 'enc' is invalid
  end;
  
  X509_CINF = record
    version        : System.Pointer;       // ^ASN1_INTEGER
    serialNumber   : System.Pointer;       // ^ASN1_INTEGER
    signature      : System.Pointer;       // ^X509_ALGOR
    issuer         : System.Pointer;       // ^X509_NAME
    validity       : PX509_VAL;     // ^X509_VAL
    subject        : System.Pointer;       // ^X509_NAME
    key            : System.Pointer;       // ^X509_PUBKEY
    issuerUID      : System.Pointer;       // ^ASN1_BIT_STRING
    subjectUID     : System.Pointer;       // ^ASN1_BIT_STRING
    extensions     : System.Pointer;       // ^ (STACK_OF(X509_EXTENSION) extensions)
    enc            : ASN1_ENCODING; // ASN1_ENCODING
  end;
  PX509_CINF = ^X509_CINF;
  
  CRYPTO_EX_DATA = record
    sk: System.Pointer;
    dummy: LongInt;
  end;

  X509 = record
    cert_info      : PX509_CINF;
    sig_alg        : System.Pointer;  // ^X509_ALGOR
    signature      : System.Pointer;  // ^ASN1_BIT_STRING
    valid          : LongInt;
    references     : LongInt;
    name           : System.PChar;
    ex_data        : CRYPTO_EX_DATA;
    // These contain copies of various extension values //
    ex_pathlen     : LongInt;
    ex_pcpathlen   : LongInt;
    ex_flags       : System.cardinal;
    ex_kusage      : System.cardinal;
    ex_xkusage     : System.cardinal;
    ex_nscert      : System.cardinal;
    skid           : System.Pointer;  // ^ASN1_OCTET_STRING
    akid           : System.Pointer;  // ^AUTHORITY_KEYID
    policy_cache   : System.Pointer;
    crldp          : System.Pointer;
    altname        : System.Pointer;
    nc             : System.Pointer;
    sha1_hash      : array [0..SHA_DIGEST_LENGTH-1] of System.char;
    aux            : System.Pointer;  // ^X509_CERT_AUX
  end;
  PX509 = ^X509;
  PPX509 = ^PX509;

  DSA = record
	pad: LongInt;
	version: LongInt;
	write_params: LongInt;
	p: System.Pointer;
	q: System.Pointer;
	g: System.Pointer;
	pub_key: System.Pointer;
	priv_key: System.Pointer;
	kinv: System.Pointer;
	r: System.Pointer;
	flags: LongInt;
	method_mont_p: System.PChar;
	references: LongInt;
	ex_data: record
          sk: System.Pointer;
         dummy: LongInt;
       end;
       meth: System.Pointer;
  end;
  pDSA = ^DSA;
  EVP_PKEY_PKEY = record
    case integer of
      0: (ptr: System.PChar);
      1: (rsa: pRSA);
      2: (dsa: pDSA);
      3: (dh: pDH);
   end;
  
  EVP_PKEY = record
    ktype: LongInt;
    save_type: LongInt;
    references: LongInt;
    pkey: EVP_PKEY_PKEY;
    save_parameters: LongInt;
    attributes: PSTACK_OFX509;
  end;
  PEVP_PKEY = ^EVP_PKEY;
  PPEVP_PKEY = ^PEVP_PKEY;
  
  PPRSA = ^PRSA;
  PASN1_UTCTIME = SslPtr;
  PASN1_cInt = SslPtr;
  PPasswdCb = SslPtr;
  PFunction = procedure;
  DES_cblock = array[0..7] of System.Byte;
  PDES_cblock = ^DES_cblock;
  des_ks_struct = packed record
    ks: DES_cblock;
    weak_key: cInt;
  end;
  des_key_schedule = array[1..16] of des_ks_struct;

  MD2_CTX = record
    num: LongInt;
    data: array [0..15] of System.byte;
    cksm: array [0..15] of System.cardinal;
    state: array [0..15] of System.cardinal;
  end;
  MD4_CTX = record
    A, B, C, D: System.cardinal;
    Nl, Nh: System.cardinal;
    data: array [0..15] of System.cardinal;
    num: LongInt;
  end;
  MD5_CTX = record
    A, B, C, D: System.cardinal;
    Nl, Nh: System.cardinal;
    data: array [0..15] of System.cardinal;
    num: LongInt;
  end;
  RIPEMD160_CTX = record
    A, B, C, D, E: System.cardinal;
    Nl, Nh: System.cardinal;
    data: array [0..15] of System.cardinal;
    num: LongInt;
  end;
  SHA_CTX = record
    h0, h1, h2, h3, h4: System.cardinal;
    Nl, Nh: System.cardinal;
    data: array [0..16] of System.cardinal;
    num: LongInt;
  end;
  MDC2_CTX = record
    num: LongInt;
    data: array [0..7] of System.byte;
    h, hh: des_cblock;
    pad_type: LongInt;
  end;

  // Rand
  RAND_METHOD = record
  end;
  PRAND_METHOD = ^RAND_METHOD;

  // RSA
  PENGINE = System.Pointer;
  PBIGNUM = System.Pointer;
  PBN_GENCB = System.Pointer;
  PBN_MONT_CTX = System.Pointer;
  PBN_CTX = System.Pointer;
  PPByte = ^System.PByte;

  Trsa_pub_enc = function(flen: cint; const from_, to_: PByte; arsa: PRSA; padding: cint): cint; Trsa_pub_dec = function(flen: cint; const from_, to_: PByte; arsa: PRSA; padding: cint): cint;
  Trsa_priv_enc = function(flen: cint; const from_, to_: PByte; arsa: PRSA; padding: cint): cint;  Trsa_priv_dec = function(flen: cint; const from_, to_: PByte; arsa: PRSA; padding: cint): cint;
  Trsa_mod_exp = function(r0: PBIGNUM; const l: PBIGNUM; arsa: PRSA; ctx: PBN_CTX): cint; Tbn_mod_exp = function(r: PBIGNUM; const a, p, m: PBIGNUM; arsa: PRSA; ctx: PBN_CTX; m_ctx: PBN_MONT_CTX): cint;
  Tinit = function(arsa: PRSA): cint;
  Tfinish = function(arsa: PRSA): cint;  Trsa_sign = function(type_: cint; const m: PByte; m_length: cuint; sigret: PByte; siglen: pcuint; arsa: PRSA): cint;
  Trsa_verify = function(dtype: cint; const m: PByte; m_length: cuint; const sigbuf: PByte; siglen: cuint; arsa: PRSA): cint;
  Trsa_keygen = function(arsa: PRSA; bits: cint; e: PBIGNUM; cb: PBN_GENCB): cint;

  RSA_METHOD = record
    name: System.PChar;
    rsa_pub_enc: Trsa_pub_enc;
    rsa_pub_dec: Trsa_pub_dec;
    rsa_priv_enc: Trsa_priv_enc;
    rsa_priv_dec: Trsa_priv_dec;
    rsa_mod_exp: Trsa_mod_exp; { Can be null }
    bn_mod_exp: Tbn_mod_exp; { Can be null }
    init: Tinit; { called at new }
    finish: Tfinish; { called at free }
    flags: cint; { RSA_METHOD_FLAG_* things }
    app_data: System.PChar; { may be needed! }
  { New sign and verify functions: some libraries don't allow arbitrary data
   * to be signed/verified: this allows them to be used. Note: for this to work
   * the RSA_public_decrypt() and RSA_private_encrypt() should *NOT* be used
   * RSA_sign(), RSA_verify() should be used instead. Note: for backwards
   * compatibility this functionality is only enabled if the RSA_FLAG_SIGN_VER
   * option is set in 'flags'.
   }
    rsa_sign: Trsa_sign;
    rsa_verify: Trsa_verify;
  { If this callback is NULL, the builtin software RSA key-gen will be used. This
   * is for behavioural compatibility whilst the code gets rewired, but one day
   * it would be nice to assume there are no such things as "builtin software"
   * implementations. }
    rsa_keygen: Trsa_keygen;
  end;
  PRSA_METHOD = ^RSA_METHOD;

  // EVP

  EVP_MD_CTX = record
    digest: pEVP_MD;
    case integer of
      0: (base: array [0..3] of System.byte);
      1: (md2: MD2_CTX);
      8: (md4: MD4_CTX);
      2: (md5: MD5_CTX);
      16: (ripemd160: RIPEMD160_CTX);
      4: (sha: SHA_CTX);
      32: (mdc2: MDC2_CTX);
    end;
  PEVP_MD_CTX = ^EVP_MD_CTX;

  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;

  PASN1_TYPE = System.Pointer;

  EVP_CIPHER_INIT_FUNC = function(ctx: PEVP_CIPHER_CTX; const key, iv: PByte; enc: cint): cint; cdecl;
  EVP_CIPHER_DO_CIPHER_FUNC = function(ctx: PEVP_CIPHER_CTX; out_data: PByte; const in_data: PByte; inl: csize_t): cint; cdecl;
  EVP_CIPHER_CLEANUP_FUNC = function(ctx: PEVP_CIPHER_CTX): cint; cdecl;
  EVP_CIPHER_SET_ASN1_PARAMETERS_FUNC = function(ctx: PEVP_CIPHER_CTX; asn1_type: PASN1_TYPE): cint; cdecl;
  EVP_CIPHER_GET_ASN1_PARAMETERS_FUNC = function(ctx: PEVP_CIPHER_CTX; asn1_type: PASN1_TYPE): cint; cdecl;
  EVP_CIPHER_CTRL_FUNC = function(ctx: PEVP_CIPHER_CTX; type_, arg: cint; ptr: System.Pointer): cint; cdecl;

    EVP_CIPHER = record  // Updated with EVP.h from OpenSSL 1.0.0
    nid: cint;
    block_size: cint;
    key_len: cint;  //* Default value for variable length ciphers */
    iv_len: cint;
    flags: culong; //* Various flags */
    init: EVP_CIPHER_INIT_FUNC;	//* init key */
    do_cipher: EVP_CIPHER_DO_CIPHER_FUNC;//* encrypt/decrypt data */
    cleanup: EVP_CIPHER_CLEANUP_FUNC; //* cleanup ctx */
    ctx_size: cint;		//* how big ctx->cipher_data needs to be */
    set_asn1_parameters: EVP_CIPHER_SET_ASN1_PARAMETERS_FUNC; //* Populate a ASN1_TYPE with parameters */
    get_asn1_parameters: EVP_CIPHER_GET_ASN1_PARAMETERS_FUNC; //* Get parameters from a ASN1_TYPE */
    ctrl: EVP_CIPHER_CTRL_FUNC; //* Miscellaneous operations */
    app_data: System.Pointer;	//* Application data */
  end;
  PEVP_CIPHER = ^EVP_CIPHER;

  EVP_CIPHER_CTX = record // Updated with EVP.h from OpenSSL 1.0.0
    cipher: PEVP_CIPHER;
    engine: PENGINE;  //* functional reference if 'cipher' is ENGINE-provided */
    encrypt: cint;    //* encrypt or decrypt */
    buf_len: cint;    //* number we have left */

    oiv: array[0..EVP_MAX_IV_LENGTH-1] of System.Byte;  //* original iv */
    iv: array[0..EVP_MAX_IV_LENGTH-1] of System.Byte; //* working iv */
    buf: array[0..EVP_MAX_IV_LENGTH-1] of System.Byte; //* saved partial block */
    num: cint;        //* used by cfb/ofb mode */

    app_data: System.Pointer;   //* application stuff */
    key_len: cint;    //* May change for variable length cipher */
    flags: culong;	//* Various flags */
    cipher_data: System.Pointer; //* per EVP data */
    final_used: cint;
    block_mask: cint;
    final: array[0..EVP_MAX_BLOCK_LENGTH-1] of System.Byte; //* possible final block */
    final2: array[0..$1FFF] of System.Byte; // Extra storage space, otherwise an access violation
                                     // in the OpenSSL library will occur
  end;

  // PEM

  Ppem_password_cb = System.Pointer;

const


  SSL_SESS_CACHE_OFF	= $0;
  SSL_SESS_CACHE_CLIENT	= $1;
  SSL_SESS_CACHE_SERVER	= $2;
  SSL_SESS_CACHE_BOTH	= (SSL_SESS_CACHE_CLIENT or SSL_SESS_CACHE_SERVER);
  SSL_SESS_CACHE_NO_AUTO_CLEAR	=	$0080;
  SSL_SESS_CACHE_NO_INTERNAL_LOOKUP=	$0100;
  SSL_SESS_CACHE_NO_INTERNAL_STORE=	$0200;
  SSL_SESS_CACHE_NO_INTERNAL =  (SSL_SESS_CACHE_NO_INTERNAL_LOOKUP or SSL_SESS_CACHE_NO_INTERNAL_STORE);

  SSL_ERROR_NONE = 0;
  SSL_ERROR_SSL = 1;
  SSL_ERROR_WANT_READ = 2;
  SSL_ERROR_WANT_WRITE = 3;
  SSL_ERROR_WANT_X509_LOOKUP = 4;
  SSL_ERROR_SYSCALL = 5; //look at error stack/return value/errno
  SSL_ERROR_ZERO_RETURN = 6;
  SSL_ERROR_WANT_CONNECT = 7;
  SSL_ERROR_WANT_ACCEPT = 8;
  
     SSL_CTRL_NEED_TMP_RSA = 1;
     SSL_CTRL_SET_TMP_RSA = 2;
     SSL_CTRL_SET_TMP_DH = 3;
     SSL_CTRL_SET_TMP_ECDH = 4;
     SSL_CTRL_SET_TMP_RSA_CB = 5;
     SSL_CTRL_SET_TMP_DH_CB = 6;
     SSL_CTRL_SET_TMP_ECDH_CB = 7;
     SSL_CTRL_GET_SESSION_REUSED = 8;
     SSL_CTRL_GET_CLIENT_CERT_REQUEST = 9;
     SSL_CTRL_GET_NUM_RENEGOTIATIONS = 10;
     SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS = 11;
     SSL_CTRL_GET_TOTAL_RENEGOTIATIONS = 12;
     SSL_CTRL_GET_FLAGS = 13;
     SSL_CTRL_EXTRA_CHAIN_CERT = 14;
     SSL_CTRL_SET_MSG_CALLBACK = 15;
     SSL_CTRL_SET_MSG_CALLBACK_ARG = 16;
  { only applies to datagram connections  }
     SSL_CTRL_SET_MTU = 17;
  { Stats  }
     SSL_CTRL_SESS_NUMBER = 20;
     SSL_CTRL_SESS_CONNECT = 21;
     SSL_CTRL_SESS_CONNECT_GOOD = 22;
     SSL_CTRL_SESS_CONNECT_RENEGOTIATE = 23;
     SSL_CTRL_SESS_ACCEPT = 24;
     SSL_CTRL_SESS_ACCEPT_GOOD = 25;
     SSL_CTRL_SESS_ACCEPT_RENEGOTIATE = 26;
     SSL_CTRL_SESS_HIT = 27;
     SSL_CTRL_SESS_CB_HIT = 28;
     SSL_CTRL_SESS_MISSES = 29;
     SSL_CTRL_SESS_TIMEOUTS = 30;
     SSL_CTRL_SESS_CACHE_FULL = 31;
     SSL_CTRL_OPTIONS = 32;
     SSL_CTRL_MODE = 33;

     SSL_CTRL_GET_READ_AHEAD = 40;
     SSL_CTRL_SET_READ_AHEAD = 41;

     SSL_CTRL_SET_SESS_CACHE_SIZE = 42;
     SSL_CTRL_GET_SESS_CACHE_SIZE = 43;
     SSL_CTRL_SET_SESS_CACHE_MODE = 44;
     SSL_CTRL_GET_SESS_CACHE_MODE = 45;
     SSL_CTRL_GET_MAX_CERT_LIST = 50;
     SSL_CTRL_SET_MAX_CERT_LIST = 51;

     SSL_OP_SINGLE_DH_USE = 1048576;

{* Allow SSL_write(..., n) to return r with 0 < r < n (i.e. report success
 * when just a single record has been written): *}
  SSL_MODE_ENABLE_PARTIAL_WRITE = 1;
{* Make it possible to retry SSL_write() with changed buffer location
 * (buffer contents must stay the same!); this is not the default to avoid
 * the misconception that non-blocking SSL_write() behaves like
 * non-blocking write(): *}
  SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER = 2;
{* Never bother the application with retries if the transport
 * is blocking: *}
  SSL_MODE_AUTO_RETRY = 4;
{* Don't attempt to automatically build certificate chain *}
  SSL_MODE_NO_AUTO_CHAIN = 8;

  SSL_OP_NO_SSLv2 = $01000000;
  SSL_OP_NO_SSLv3 = $02000000;
  SSL_OP_NO_TLSv1 = $04000000;
  SSL_OP_ALL = $000FFFFF;
  SSL_VERIFY_NONE = $00;
  SSL_VERIFY_PEER = $01;

  OPENSSL_DES_DECRYPT = 0;
  OPENSSL_DES_ENCRYPT = 1;

  X509_V_OK =	0;
  X509_V_ILLEGAL = 1;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT = 2;
  X509_V_ERR_UNABLE_TO_GET_CRL = 3;
  X509_V_ERR_UNABLE_TO_DECRYPT_CERT_SIGNATURE = 4;
  X509_V_ERR_UNABLE_TO_DECRYPT_CRL_SIGNATURE = 5;
  X509_V_ERR_UNABLE_TO_DECODE_ISSUER_PUBLIC_KEY = 6;
  X509_V_ERR_CERT_SIGNATURE_FAILURE = 7;
  X509_V_ERR_CRL_SIGNATURE_FAILURE = 8;
  X509_V_ERR_CERT_NOT_YET_VALID = 9;
  X509_V_ERR_CERT_HAS_EXPIRED = 10;
  X509_V_ERR_CRL_NOT_YET_VALID = 11;
  X509_V_ERR_CRL_HAS_EXPIRED = 12;
  X509_V_ERR_ERROR_IN_CERT_NOT_BEFORE_FIELD = 13;
  X509_V_ERR_ERROR_IN_CERT_NOT_AFTER_FIELD = 14;
  X509_V_ERR_ERROR_IN_CRL_LAST_UPDATE_FIELD = 15;
  X509_V_ERR_ERROR_IN_CRL_NEXT_UPDATE_FIELD = 16;
  X509_V_ERR_OUT_OF_MEM = 17;
  X509_V_ERR_DEPTH_ZERO_SELF_SIGNED_CERT = 18;
  X509_V_ERR_SELF_SIGNED_CERT_IN_CHAIN = 19;
  X509_V_ERR_UNABLE_TO_GET_ISSUER_CERT_LOCALLY = 20;
  X509_V_ERR_UNABLE_TO_VERIFY_LEAF_SIGNATURE = 21;
  X509_V_ERR_CERT_CHAIN_TOO_LONG = 22;
  X509_V_ERR_CERT_REVOKED = 23;
  X509_V_ERR_INVALID_CA = 24;
  X509_V_ERR_PATH_LENGTH_EXCEEDED = 25;
  X509_V_ERR_INVALID_PURPOSE = 26;
  X509_V_ERR_CERT_UNTRUSTED = 27;
  X509_V_ERR_CERT_REJECTED = 28;
  //These are 'informational' when looking for issuer cert
  X509_V_ERR_SUBJECT_ISSUER_MISMATCH = 29;
  X509_V_ERR_AKID_SKID_MISMATCH = 30;
  X509_V_ERR_AKID_ISSUER_SERIAL_MISMATCH = 31;
  X509_V_ERR_KEYUSAGE_NO_CERTSIGN = 32;
  X509_V_ERR_UNABLE_TO_GET_CRL_ISSUER = 33;
  X509_V_ERR_UNHANDLED_CRITICAL_EXTENSION = 34;
  //The application is not happy
  X509_V_ERR_APPLICATION_VERIFICATION = 50;

  SSL_FILETYPE_ASN1	= 2;
  SSL_FILETYPE_PEM = 1;
  EVP_PKEY_RSA = 6;

  // RSA
  RSA_PKCS1_PADDING      = 1;
  RSA_SSLV23_PADDING     = 2;
  RSA_NO_PADDING         = 3;
  RSA_PKCS1_OAEP_PADDING = 4;

  // BIO

  BIO_NOCLOSE	        = $00;
  BIO_CLOSE 	        = $01;

  //* modifiers */
  BIO_FP_READ		= $02;
  BIO_FP_WRITE		= $04;
  BIO_FP_APPEND		= $08;
  BIO_FP_TEXT		= $10;

  BIO_C_SET_CONNECT                 = 100;
  BIO_C_DO_STATE_MACHINE            = 101;
  BIO_C_SET_NBIO	            = 102;
  BIO_C_SET_PROXY_PARAM	            = 103;
  BIO_C_SET_FD	                    = 104;
  BIO_C_GET_FD		            = 105;
  BIO_C_SET_FILE_PTR	            = 106;
  BIO_C_GET_FILE_PTR	            = 107;
  BIO_C_SET_FILENAME	            = 108;
  BIO_C_SET_SSL		            = 109;
  BIO_C_GET_SSL		            = 110;
  BIO_C_SET_MD		            = 111;
  BIO_C_GET_MD	                    = 112;
  BIO_C_GET_CIPHER_STATUS           = 113;
  BIO_C_SET_BUF_MEM 	            = 114;
  BIO_C_GET_BUF_MEM_PTR  	    = 115;
  BIO_C_GET_BUFF_NUM_LINES          = 116;
  BIO_C_SET_BUFF_SIZE	            = 117;
  BIO_C_SET_ACCEPT 	            = 118;
  BIO_C_SSL_MODE 	            = 119;
  BIO_C_GET_MD_CTX	            = 120;
  BIO_C_GET_PROXY_PARAM	            = 121;
  BIO_C_SET_BUFF_READ_DATA 	    = 122; // data to read first */
  BIO_C_GET_CONNECT	 	    = 123;
  BIO_C_GET_ACCEPT		    = 124;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES   = 125;
  BIO_C_GET_SSL_NUM_RENEGOTIATES    = 126;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT = 127;
  BIO_C_FILE_SEEK		    = 128;
  BIO_C_GET_CIPHER_CTX		    = 129;
  BIO_C_SET_BUF_MEM_EOF_RETURN	= 130;//*return end of input value*/
  BIO_C_SET_BIND_MODE		= 131;
  BIO_C_GET_BIND_MODE		= 132;
  BIO_C_FILE_TELL		= 133;
  BIO_C_GET_SOCKS		= 134;
  BIO_C_SET_SOCKS		= 135;

  BIO_C_SET_WRITE_BUF_SIZE	= 136;//* for BIO_s_bio */
  BIO_C_GET_WRITE_BUF_SIZE	= 137;
  BIO_C_MAKE_BIO_PAIR		= 138;
  BIO_C_DESTROY_BIO_PAIR	= 139;
  BIO_C_GET_WRITE_GUARANTEE	= 140;
  BIO_C_GET_READ_REQUEST	= 141;
  BIO_C_SHUTDOWN_WR		= 142;
  BIO_C_NREAD0		        = 143;
  BIO_C_NREAD			= 144;
  BIO_C_NWRITE0			= 145;
  BIO_C_NWRITE			= 146;
  BIO_C_RESET_READ_REQUEST	= 147;
  BIO_C_SET_MD_CTX		= 148;

  BIO_C_SET_PREFIX		= 149;
  BIO_C_GET_PREFIX		= 150;
  BIO_C_SET_SUFFIX		= 151;
  BIO_C_GET_SUFFIX		= 152;

  BIO_C_SET_EX_ARG		= 153;
  BIO_C_GET_EX_ARG		= 154;


  V_ASN1_UNIVERSAL	        = $00;

  V_ASN1_APPLICATION		=$40;
  V_ASN1_CONTEXT_SPECIFIC	=$80;
  V_ASN1_PRIVATE	        =$c0;

  V_ASN1_CONSTRUCTED		=$20;
  V_ASN1_PRIMITIVE_TAG		=$1f;
  V_ASN1_PRIMATIVE_TAG		=$1f;

  V_ASN1_APP_CHOOSE		=-2;	//* let the recipient choose */
  V_ASN1_OTHER			=-3;	//* used in ASN1_TYPE */
  V_ASN1_ANY			=-4;	//* used in ASN1 template code */

  V_ASN1_NEG			=$100;	//* negative flag */

  V_ASN1_UNDEF			=-1;
  V_ASN1_EOC			=0;
  V_ASN1_BOOLEAN		=1;	//**/
  V_ASN1_INTEGER		=2;
  V_ASN1_NEG_INTEGER		=(2 or V_ASN1_NEG);
  V_ASN1_BIT_STRING		=3;
  V_ASN1_OCTET_STRING		=4;
  V_ASN1_NULL			=5;
  V_ASN1_OBJECT			=6;
  V_ASN1_OBJECT_DESCRIPTOR	=7;
  V_ASN1_EXTERNAL		=8;
  V_ASN1_REAL			=9;
  V_ASN1_ENUMERATED		=10;
  V_ASN1_NEG_ENUMERATED		=(10 or V_ASN1_NEG);
  V_ASN1_UTF8STRING		=12;
  V_ASN1_SEQUENCE		=16;
  V_ASN1_SET			=17;
  V_ASN1_NUMERICSTRING		=18;	//**/
  V_ASN1_PRINTABLESTRING	=19;
  V_ASN1_T61STRING		=20;
  V_ASN1_TELETEXSTRING		=20;	//* alias */
  V_ASN1_VIDEOTEXSTRING		=21;	//**/
  V_ASN1_IA5STRING		=22;
  V_ASN1_UTCTIME		=23;
  V_ASN1_GENERALIZEDTIME	=24;	//**/
  V_ASN1_GRAPHICSTRING		=25;	//**/
  V_ASN1_ISO64STRING		=26;	//**/
  V_ASN1_VISIBLESTRING		=26;	//* alias */
  V_ASN1_GENERALSTRING		=27;	//**/
  V_ASN1_UNIVERSALSTRING	=28;	//**/
  V_ASN1_BMPSTRING		=30;

  //* For use with d2i_ASN1_type_bytes() */
  B_ASN1_NUMERICSTRING	        =$0001;
  B_ASN1_PRINTABLESTRING	=$0002;
  B_ASN1_T61STRING	        =$0004;
  B_ASN1_TELETEXSTRING	        =$0004;
  B_ASN1_VIDEOTEXSTRING	        =$0008;
  B_ASN1_IA5STRING	        =$0010;
  B_ASN1_GRAPHICSTRING	        =$0020;
  B_ASN1_ISO64STRING	        =$0040;
  B_ASN1_VISIBLESTRING	        =$0040;
  B_ASN1_GENERALSTRING	        =$0080;
  B_ASN1_UNIVERSALSTRING	=$0100;
  B_ASN1_OCTET_STRING	        =$0200;
  B_ASN1_BIT_STRING	        =$0400;
  B_ASN1_BMPSTRING	        =$0800;
  B_ASN1_UNKNOWN		=$1000;
  B_ASN1_UTF8STRING	        =$2000;
  B_ASN1_UTCTIME		=$4000;
  B_ASN1_GENERALIZEDTIME	=$8000;
  B_ASN1_SEQUENCE		=$10000;

  //* For use with ASN1_mbstring_copy() */
  MBSTRING_FLAG		        =$1000;
  MBSTRING_UTF8		        = (MBSTRING_FLAG);
  MBSTRING_ASC		        = (MBSTRING_FLAG or 1);
  MBSTRING_BMP		        = (MBSTRING_FLAG or 2);
  MBSTRING_UNIV		        = (MBSTRING_FLAG or 4);

  SMIME_OLDMIME		        =$400;
  SMIME_CRLFEOL	                =$800;
  SMIME_STREAM		        =$1000;

//DES modes
  DES_ENCRYPT = 1;
  DES_DECRYPT = 0;

type
// libssl.dll
  TERR_error_string = procedure(e: cInt; buf: PChar; len: cInt); cdecl;
  TERR_free_strings = procedure; cdecl;
  TERR_remove_state = procedure(pid: cInt); cdecl;

  TERR_clear_error=procedure(); cdecl;
  TERR_get_error=function():cuLong; cdecl;
  TERR_peek_error=function():cuLong; cdecl;
  TERR_peek_last_error=function():cuLong; cdecl;
  TERR_get_error_line=function(const sFile:System.Pchar; line:pcInt):cuLong; cdecl;
  TERR_peek_error_line=function(const sFile:System.PChar; line:pcInt):cuLong; cdecl;
  TERR_peek_last_error_line=function(const sFile:System.Pchar; line:pcInt):cuLong; cdecl;

  TERR_get_error_line_data=function(const sFile:System.Pchar; line:pcInt; const data:PChar; flags:pcInt):cuLong; cdecl;
  TERR_peek_error_line_data=function(const sFile:System.PChar; line:pcInt; const data:PChar; flags:pcInt):cuLong; cdecl;
  TERR_peek_last_error_line_data=function(const sFile:System.PChar; line:pcInt; const data:PChar; flags:pcInt):cuLong; cdecl;
  TERR_load_crypto_strings = procedure; cdecl;

  TSSL_get_error = function(s: PSSL; ret_code: cInt):cInt; cdecl;
  TSslLibraryInit = function:cInt; cdecl;
  TSslLoadErrorStrings = procedure; cdecl;
  TSSL_CTX_set_cipher_list = function(arg0: PSSL_CTX; str: PChar):cInt; cdecl;
  TSSL_set_cipher_list = function (s:PSSL; str:PChar):cInt; cdecl;
  TSSL_CTX_new = function(meth: PSSL_METHOD):PSSL_CTX; cdecl;
  TSSL_CTX_new_shared=function(method:PSSL_METHOD; name:PChar):PSSL_CTX; cdecl;
  TSSL_CTX_free = procedure(arg0: PSSL_CTX); cdecl;
  TSSL_set_fd = function(s: PSSL; fd: cInt):cInt; cdecl;
  TSSL_ctrl = function(ssl: PSSL; cmd: cInt; larg: clong; parg: System.Pointer): cLong; cdecl;
  TSSL_ctx_ctrl = function(ctx: PSSL_CTX; cmd: cInt; larg: clong; parg: System.Pointer): cLong; cdecl;
  TSSL_CTX_get_cert_store=function(ctx:PSSL_CTX):X509_STORE; cdecl;
  TSSL_CTX_add_client_CA=function(ctx:PSSL_CTX; X509:PX509):cInt; cdecl;



  TSslMethodV2 = function:PSSL_METHOD; cdecl;
  TSslMethodV3 = function:PSSL_METHOD; cdecl;
  TSslMethodTLSV1 = function:PSSL_METHOD; cdecl;
  TSslMethodV23 = function:PSSL_METHOD; cdecl;

  TSSLv3_server_method = function:PSSL_METHOD; cdecl;
  TSSLv23_server_method = function:PSSL_METHOD; cdecl;
  TSSLv23_client_method = function:PSSL_METHOD; cdecl;
  TTLSv1_server_method = function:PSSL_METHOD; cdecl;
  TTLS_server_method = function:PSSL_METHOD; cdecl;


  TTLSv1_1_server_method = function :PSSL_METHOD; cdecl;
  TTLS_method = function:PSSL_METHOD; cdecl;
  TTLSv1_method=function:PSSL_METHOD; cdecl;
  TTLSv1_1_method=function:PSSL_METHOD; cdecl;
  TTLSv1_2_method = function:PSSL_METHOD; cdecl;
  TTLSv1_2_server_method = function:PSSL_METHOD; cdecl;

  TSslCtxUsePrivateKey = function(ctx: PSSL_CTX; pkey: sslptr):cInt; cdecl;
  TSSL_CTX_use_PrivateKey_ASN1 = function(pk: cInt; ctx: PSSL_CTX; d: sslptr; len: cInt):cInt; cdecl;
  TSSL_CTX_add_extra_chain_cert = function (ctx:System.Pointer; X509:PX509):cInt; cdecl;
  TSSL_CTX_use_RSAPrivateKey_ASN1 = function(ctx: PSSL_CTX; d: sslptr; len: cInt):cInt; cdecl;
  TSslCtxUsePrivateKeyFile = function(ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; cdecl;
  TSslCtxUseRSAPrivateKeyFile = function(ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; cdecl;
  TSSL_CTX_use_Certificate = function(ctx: PSSL_CTX; x: SslPtr):cInt; cdecl;
  TSSL_CTX_use_Certificate_ASN1 = function(ctx: PSSL_CTX; len: cInt; d: SslPtr):cInt; cdecl;
  TSslCtxUseCertificateFile = function(ctx: PSSL_CTX; const _file: PChar; _type: cInt):cInt; cdecl;
  TSslCtxUseCertificateChainFile = function(ctx: PSSL_CTX; const _file: PChar):cInt; cdecl;
  TSslCtxCheckPrivateKeyFile = function(ctx: PSSL_CTX):cInt; cdecl;
  TSslCtxSetDefaultPasswdCb = procedure(ctx: PSSL_CTX; cb: SslPtr); cdecl;
  TSslCtxSetDefaultPasswdCbUserdata = procedure(ctx: PSSL_CTX; u: SslPtr); cdecl;
  TSslCtxLoadVerifyLocations = function(ctx: PSSL_CTX; const CAfile: PChar; const CApath: PChar):cInt; cdecl;

  TSSL_CTX_set_session_cache_mode = function(ctx:PSSL_CTX; mode:cInt) : cInt; cdecl;
  TSSL_CTX_get_session_cache_mode = function(ctx:PSSL_CTX): cInt; cdecl;
  TSSL_CTX_set_tmp_dh = function (ctx:PSSL_CTX; DH:PDH): cInt; cdecl;
  TSSL_new = function(ctx: PSSL_CTX):PSSL; cdecl;
  TSSL_free = procedure(ssl: PSSL); cdecl;
  TSSL_accept = function(ssl: PSSL):cInt; cdecl;
  TSSL_do_handshake = function(ssl:PSSL):cInt; cdecl;
  TSSL_connect = function(ssl: PSSL):cInt; cdecl;
  TSSL_shutdown = function(ssl: PSSL):cInt; cdecl;
  TSSL_set_accept_state= procedure(ssl:PSSL); cdecl;
  TSSL_set_connect_state= procedure(ssl:PSSL); cdecl;
  TSSL_read = function(ssl: PSSL; buf: PChar; num: cInt):cInt; cdecl;
  TSSL_peek = function(ssl: PSSL; buf: PChar; num: cInt):cInt; cdecl;
  TSSL_write = function(ssl: PSSL; const buf: PChar; num: cInt):cInt; cdecl;
  TSSL_clear = function(ssl: PSSL):cInt; cdecl;
  TSSL_pending = function(ssl: PSSL):cInt; cdecl;
  TSslGetVersion = function(ssl: PSSL):PChar; cdecl;
  TSslGetPeerCertificate = function(ssl: PSSL):PX509; cdecl;
  TSslCtxSetVerify = procedure(ctx: PSSL_CTX; mode: cInt; arg2: SslPtr); cdecl;
  TSSL_set_session_id_context=function(ssl:PSSL; const sid_ctx:PChar; sid_ctx_len:cuInt):cInt;cdecl;
  TSSL_CTX_set_session_id_context=function(ctx:PSSL_CTX; const sid_ctx:PChar; sid_ctx_len:cuInt):cInt;cdecl;
  TSSLGetCurrentCipher = function(s: PSSL):SslPtr; cdecl;
  TSSLCipherGetName = function(c: Sslptr):PChar; cdecl;
  TSSLCipherGetBits = function(c: SslPtr; alg_bits: PcInt):cInt; cdecl;
  TSSLGetVerifyResult = function(ssl: PSSL):cInt; cdecl;
  TSSL_set_bio=procedure(ssl:PSSL; rbio:PBIO; wbio:PBIO); cdecl;


// libeay.dll
  TX509_new = function: PX509; cdecl;
  TX509_free = procedure(x: PX509); cdecl;
  TX509NameOneline = function(a: PX509_NAME; buf: PChar; size: cInt):PChar; cdecl;
  TX509GetSubjectName = function(a: PX509):PX509_NAME; cdecl;
  TX509GetIssuerName = function(a: PX509):PX509_NAME; cdecl;
  TX509NameHash = function(x: PX509_NAME):cuLong; cdecl;
  TX509Digest = function(data: PX509; _type: PEVP_MD; md: PChar; len: PcInt):cInt; cdecl;
  TX509print = function(b: PBIO; a: PX509): cInt; cdecl;
  TX509SetVersion = function(x: PX509; version: cInt): cInt; cdecl;
  TX509SetPubkey = function(x: PX509; pkey: PEVP_PKEY): cInt; cdecl;
  TX509SetIssuerName = function(x: PX509; name: PX509_NAME): cInt; cdecl;
  TX509NameAddEntryByTxt = function(name: PX509_NAME; field: PChar; _type: cInt;
    bytes: PChar; len, loc, _set: cInt): cInt; cdecl;
  TX509Sign = function(x: PX509; pkey: PEVP_PKEY; const md: PEVP_MD): cInt; cdecl;
  TX509GmtimeAdj = function(s: PASN1_UTCTIME; adj: cInt): PASN1_UTCTIME; cdecl;
  TX509SetNotBefore = function(x: PX509; tm: PASN1_UTCTIME): cInt; cdecl;
  TX509SetNotAfter = function(x: PX509; tm: PASN1_UTCTIME): cInt; cdecl;
  TX509GetSerialNumber = function(x: PX509): PASN1_cInt; cdecl;
  TX509_STORE_add_cert=function(Store:X509_STORE; Cert:PX509):cInt; cdecl;
  TEvpPkeyNew = function: PEVP_PKEY; cdecl;
  TEvpPkeyFree = procedure(pk: PEVP_PKEY); cdecl;
  TEvpPkeyAssign = function(pkey: PEVP_PKEY; _type: cInt; key: Prsa): cInt; cdecl;
  TEvpGetDigestByName = function(Name: PChar): PEVP_MD; cdecl;
  TEVPcleanup = procedure; cdecl;
  TSSLeayversion = function(t: cInt): PChar; cdecl;
  TRand_screen = procedure; cdecl;
  TBIO_new = function(b: PBIO_METHOD): PBIO; cdecl;

  TBIO_free_all = procedure(b: PBIO); cdecl;
  TBIO_free = procedure(b: PBIO); cdecl;
  TBIO_new_socket= function(Socket:TSocket; close_flag:cint):PBIO; cdecl;
  TBIO_s_mem = function: PBIO_METHOD; cdecl;
  TBIO_ctrl_pending = function(b: PBIO): cInt; cdecl;

  TBIO_read = function(b: PBIO; Buf: PChar; Len: cInt): cInt; cdecl;
  TBIO_write = function(b: PBIO; Buf: PChar; Len: cInt): cInt; cdecl;

  Td2iPKCS12bio = function(b:PBIO; Pkcs12: SslPtr): SslPtr; cdecl;
  TPKCS12parse = function(p12: SslPtr; pass: PChar; var pkey, cert, ca: SslPtr): cInt; cdecl;
  TPKCS12free = procedure(p12: SslPtr); cdecl;
  TAsn1UtctimeNew = function: PASN1_UTCTIME; cdecl;
  TAsn1UtctimeFree = procedure(a: PASN1_UTCTIME); cdecl;
  Td2i_X509 = function (X509:PPX509; Buf:PPChar; len: cInt):PX509; cdecl;
  Ti2dX509bio = function(b: PBIO; x: PX509): cInt; cdecl;
  Ti2dPrivateKeyBio= function(b: PBIO; pkey: PEVP_PKEY): cInt; cdecl;

  // 3DES functions
  TDESsetoddparity = procedure(Key: des_cblock); cdecl;
  TDESsetkeychecked = function(key: des_cblock; schedule: des_key_schedule): cInt; cdecl;
  TDESsetkey = TDESsetkeychecked;
  TDESecbencrypt = procedure(Input: des_cblock; output: des_cblock; ks: des_key_schedule; enc: cInt); cdecl;


  // RAND functions
  TRAND_set_rand_method = function(const meth: PRAND_METHOD): cint; cdecl;
  TRAND_get_rand_method = function(): PRAND_METHOD; cdecl;
  TRAND_SSLeay = function(): PRAND_METHOD; cdecl;
  TRAND_cleanup = procedure(); cdecl;
  TRAND_bytes = function(buf: PByte; num: cint): cint; cdecl;
  TRAND_pseudo_bytes = function(buf: PByte; num: cint): cint; cdecl;
  TRAND_seed = procedure(const buf: System.Pointer; num: cint); cdecl;
  TRAND_add = procedure(const buf: System.Pointer; num: cint; entropy: cdouble); cdecl;
  TRAND_load_file = function(const file_name: PChar; max_bytes: clong): cint; cdecl;
  TRAND_write_file = function(const file_name: PChar): cint; cdecl;
  TRAND_file_name = function(file_name: PChar; num: csize_t): PChar; cdecl;
  TRAND_status = function(): cint; cdecl;
  TRAND_query_egd_bytes = function(const path: PChar; buf: PByte; bytes: cint): cint; cdecl;
  TRAND_egd = function(const path: PChar): cint; cdecl;
  TRAND_egd_bytes = function(const path: PChar; bytes: cint): cint; cdecl;
  TERR_load_RAND_strings = procedure(); cdecl;
  TRAND_poll = function(): cint; cdecl;

  // RSA Functions
  TRSA_new = function (): PRSA; cdecl;
  TRSA_new_method = function (method: PENGINE): PRSA; cdecl;
  TRSA_size = function (arsa: PRSA): cint; cdecl;
  TRsaGenerateKey = function(bits, e: cInt; callback: PFunction; cb_arg: SslPtr): PRSA; cdecl;
  TRSA_generate_key_ex = function (arsa: PRSA; bits: cInt; e: PBIGNUM; cb: PBN_GENCB): PRSA; cdecl;
  TRSA_check_key = function (arsa: PRSA): cint; cdecl;
  TRSA_public_encrypt = function (flen: cint; from_buf, to_buf: PByte; arsa: PRSA; padding: cint): cint; cdecl;
  TRSA_private_encrypt = function (flen: cint; from_buf, to_buf: PByte; arsa: PRSA; padding: cint): cint; cdecl;
  TRSA_public_decrypt = function (flen: cint; from_buf, to_buf: PByte; arsa: PRSA; padding: cint): cint; cdecl;
  TRSA_private_decrypt = function (flen: cint; from_buf, to_buf: PByte; arsa: PRSA; padding: cint): cint; cdecl;
  TRSA_free = procedure (arsa: PRSA); cdecl;
  TRSA_flags = function (arsa: PRSA): LongInt; cdecl;
  TRSA_set_default_method = procedure (method: PRSA_METHOD); cdecl;
  TRSA_get_default_method = function : PRSA_METHOD; cdecl;
  TRSA_get_method = function (prsa: PRSA): PRSA_METHOD; cdecl;
  TRSA_set_method = function (arsa: PRSA; method: PRSA_METHOD): PRSA_METHOD; cdecl;

  // X509 Functions

  Td2i_RSAPublicKey = function (arsa: PPRSA; pp: PPByte; len: cint): PRSA; cdecl;
  Ti2d_RSAPublicKey = function (arsa: PRSA; pp: PPByte): cint; cdecl;
  Td2i_RSAPrivateKey = function (arsa: PPRSA; pp: PPByte; len: cint): PRSA; cdecl;
  Ti2d_RSAPrivateKey = function (arsa: PRSA; pp: PPByte): cint; cdecl;


  // Crypto Functions

  TSSLeay_version = function(t: cint): PChar; cdecl;


  // EVP Functions

  TOpenSSL_add_all_algorithms = procedure(); cdecl;
  TOpenSSL_add_all_ciphers = procedure(); cdecl;
  TOpenSSL_add_all_digests = procedure(); cdecl;
  //
  TEVP_DigestInit = function(ctx: PEVP_MD_CTX; type_: PEVP_MD): cint; cdecl;
  TEVP_DigestUpdate = function(ctx: PEVP_MD_CTX; const data: System.Pointer; cnt: csize_t): cint; cdecl;
  TEVP_DigestFinal = function(ctx: PEVP_MD_CTX; md: PByte; s: pcuint): cint; cdecl;

  TEVP_SignFinal = function(ctx: pEVP_MD_CTX; sig: System.Pointer; var s: cardinal;
    key: pEVP_PKEY): LongInt; cdecl;
  TEVP_PKEY_size = function(key: pEVP_PKEY): LongInt; cdecl;
  TEVP_PKEY_free = Procedure(key: pEVP_PKEY); cdecl;
  TEVP_VerifyFinal = function(ctx: pEVP_MD_CTX; sigbuf: System.Pointer;
    siglen: cardinal; pkey: pEVP_PKEY): LongInt;  cdecl;
  //
  TEVP_get_cipherbyname = function(const name: PChar): PEVP_CIPHER; cdecl;
  TEVP_get_digestbyname = function(const name: PChar): PEVP_MD; cdecl;
  //
  TEVP_CIPHER_CTX_init = procedure(a: PEVP_CIPHER_CTX); cdecl;
  TEVP_CIPHER_CTX_cleanup = function(a: PEVP_CIPHER_CTX): cint; cdecl;
  TEVP_CIPHER_CTX_set_key_length = function(x: PEVP_CIPHER_CTX; keylen: cint): cint; cdecl;
  TEVP_CIPHER_CTX_ctrl = function(ctx: PEVP_CIPHER_CTX; type_, arg: cint; ptr: System.Pointer): cint; cdecl;
  //
  TEVP_EncryptInit = function(ctx: PEVP_CIPHER_CTX; const chipher_: PEVP_CIPHER;
           const key, iv: PByte): cint; cdecl;
  TEVP_EncryptUpdate = function(ctx: PEVP_CIPHER_CTX; out_: pcuchar;
           outlen: pcint; const in_: pcuchar; inlen: cint): cint; cdecl;
  TEVP_EncryptFinal = function(ctx: PEVP_CIPHER_CTX; out_data: PByte; outlen: pcint): cint; cdecl;
  //
  TEVP_DecryptInit = function(ctx: PEVP_CIPHER_CTX; chiphir_type: PEVP_CIPHER;
           const key, iv: PByte): cint; cdecl;
  TEVP_DecryptUpdate = function(ctx: PEVP_CIPHER_CTX; out_data: PByte;
           outl: pcint; const in_: PByte; inl: cint): cint; cdecl;
  TEVP_DecryptFinal = function(ctx: PEVP_CIPHER_CTX; outm: PByte; outlen: pcint): cint; cdecl;

  // PEM functions

  TPEM_read_bio_PrivateKey = function(bp: PBIO; X: PPEVP_PKEY;
           cb: Ppem_password_cb; u: System.Pointer): PEVP_PKEY; cdecl;

  TPEM_read_bio_PUBKEY = function(bp: pBIO; var x: pEVP_PKEY;
               cb: Ppem_password_cb; u: System.Pointer): pEVP_PKEY; cdecl;
  TPEM_write_bio_PrivateKey = function(bp: pBIO; x: pEVP_PKEY;
               const enc: pEVP_CIPHER; kstr: PChar; klen: Integer; cb: Ppem_password_cb;
               u: System.Pointer): LongInt; cdecl;
  TPEM_write_bio_PUBKEY = function(bp: pBIO; x: pEVP_PKEY): LongInt; cdecl;

  // BIO Functions

  TBIO_ctrl = function(bp: PBIO; cmd: cint; larg: clong; parg: System.Pointer): clong; cdecl;

  TBIO_s_file = function: pBIO_METHOD; cdecl;
  TBIO_new_file = function(const filename: PChar; const mode: PChar): pBIO; cdecl;
  TBIO_new_mem_buf = function(buf: System.Pointer; len:LongInt): pBIO; cdecl;

  // DH Functions
  Tget_dh512=function():PDH; cdecl;
  Tget_dh1024=function():PDH; cdecl;
var
  SSL_get_error: TSSL_get_error = nil;
  SslLibraryInit: TSslLibraryInit = nil;
  SslLoadErrorStrings: TSslLoadErrorStrings = nil;
  SSL_set_cipher_list:TSSL_set_cipher_list=nil;
  SSL_CTX_set_cipher_list: TSSL_CTX_set_cipher_list = nil;
  SSL_CTX_new : TSSL_CTX_new = nil;
  SSL_CTX_new_shared : TSSL_CTX_new_shared = nil;
  SSL_CTX_free: TSSL_CTX_free = nil;
  SSL_set_fd: TSSL_set_fd = nil;
  SSL_ctrl: TSSL_ctrl = nil;
  SSL_ctx_ctrl: TSSL_ctx_ctrl = nil;
  SSL_CTX_get_cert_store:TSSL_CTX_get_cert_store=nil;
  SSL_CTX_add_client_CA:TSSL_CTX_add_client_CA=nil;

  SslMethodV2: TSslMethodV2 = nil;
  SslMethodV3: TSslMethodV3 = nil;
  SslMethodTLSV1: TSslMethodTLSV1 = nil;
  SslMethodV23: TSslMethodV23 = nil;

  SSLv3_server_method  : TSSLv3_server_method = nil;
  SSLv23_server_method : TSSLv23_server_method = nil;
  SSLv23_client_method : TSSLv23_client_method = nil;
  TLSv1_server_method  : TTLSv1_server_method = nil;
  TLS_server_method    : TTLS_server_method = nil;

  TLS_method            : TTLS_method = nil;
  TLSv1_method          : TTLSv1_method = nil;
  TLSv1_1_method        : TTLSv1_1_method = nil;
  TLSv1_2_method        : TTLSv1_2_method = nil;
  TLSv1_2_server_method : TTLSv1_2_server_method = nil;
  TLSv1_1_server_method : TTLSv1_1_server_method = nil;

  SslCtxUsePrivateKey: TSslCtxUsePrivateKey = nil;
  SSL_CTX_use_PrivateKey_ASN1: TSSL_CTX_use_PrivateKey_ASN1 = nil;
  SSL_CTX_add_extra_chain_cert:TSSL_CTX_add_extra_chain_cert=nil;
  SSL_CTX_use_RSAPrivateKey_ASN1: TSSL_CTX_use_RSAPrivateKey_ASN1 = nil;
  SslCtxUsePrivateKeyFile: TSslCtxUsePrivateKeyFile = nil;
  SslCtxUseRSAPrivateKeyFile: TSslCtxUseRSAPrivateKeyFile = nil;
  SSL_CTX_use_Certificate: TSSL_CTX_use_Certificate = nil;
  SSL_CTX_use_Certificate_ASN1: TSSL_CTX_use_Certificate_ASN1 = nil;
  SslCtxUseCertificateFile: TSslCtxUseCertificateFile = nil;
  SslCtxUseCertificateChainFile: TSslCtxUseCertificateChainFile = nil;
  SslCtxCheckPrivateKeyFile: TSslCtxCheckPrivateKeyFile = nil;
  SslCtxSetDefaultPasswdCb: TSslCtxSetDefaultPasswdCb = nil;
  SslCtxSetDefaultPasswdCbUserdata: TSslCtxSetDefaultPasswdCbUserdata = nil;
  SslCtxLoadVerifyLocations: TSslCtxLoadVerifyLocations = nil;

  SSL_CTX_set_session_cache_mode : TSSL_CTX_set_session_cache_mode = nil;
  SSL_CTX_get_session_cache_mode : TSSL_CTX_get_session_cache_mode = nil;
  SSL_CTX_set_tmp_dh : TSSL_CTX_set_tmp_dh = nil;

  SSL_new: TSSL_new = nil;
  SSL_clear: TSSL_clear=nil;
  SSL_free: TSSL_free = nil;
  SSL_accept: TSSL_accept = nil;
  SSL_do_handshake:TSSL_do_handshake = nil;
  SSL_connect: TSSL_connect = nil;
  SSL_shutdown: TSSL_shutdown = nil;
  SSL_set_bio : TSSL_set_bio=nil;
  SSL_set_accept_state:TSSL_set_accept_state=nil;
  SSL_set_connect_state:TSSL_set_connect_state=nil;
  SSL_read: TSSL_read = nil;
  SSL_peek: TSSL_peek = nil;
  SSL_write: TSSL_write = nil;
  SSL_pending: TSSL_pending = nil;
  SslGetVersion: TSslGetVersion = nil;
  SslGetPeerCertificate: TSslGetPeerCertificate = nil;
  SslCtxSetVerify: TSslCtxSetVerify = nil;
  SSL_CTX_set_session_id_context : TSSL_CTX_set_session_id_context = nil;
  SSL_set_session_id_context : TSSL_set_session_id_context = nil;

  SSLGetCurrentCipher: TSSLGetCurrentCipher = nil;
  SSLCipherGetName: TSSLCipherGetName = nil;
  SSLCipherGetBits: TSSLCipherGetBits = nil;
  SSLGetVerifyResult: TSSLGetVerifyResult = nil;

// libeay.dll
  ERR_load_crypto_strings: TERR_load_crypto_strings = nil;
  ERR_error_string: TERR_error_string = nil;
  ERR_get_error: TERR_get_error = nil;
  ERR_free_strings: TERR_free_strings = nil;
  ERR_remove_state: TERR_remove_state = nil;
  ERR_clear_error : TERR_clear_error = nil;
  ERR_peek_error : TERR_peek_error= nil;
  ERR_peek_last_error : TERR_peek_last_error=nil;
  ERR_get_error_line : TERR_get_error_line=nil;
  ERR_peek_error_line : TERR_peek_error_line=nil;
  ERR_peek_last_error_line : TERR_peek_last_error_line=nil;

  X509_new : TX509_new = nil;
  X509_free : TX509_free = nil;
  X509NameOneline : TX509NameOneline = nil;
  X509GetSubjectName : TX509GetSubjectName = nil;
  X509GetIssuerName : TX509GetIssuerName = nil;

  X509NameHash: TX509NameHash = nil;
  X509Digest: TX509Digest = nil;
  X509print: TX509print = nil;
  X509SetVersion: TX509SetVersion = nil;
  X509SetPubkey: TX509SetPubkey = nil;
  X509SetIssuerName: TX509SetIssuerName = nil;
  X509NameAddEntryByTxt: TX509NameAddEntryByTxt = nil;
  X509Sign: TX509Sign = nil;
  X509GmtimeAdj: TX509GmtimeAdj = nil;
  X509SetNotBefore: TX509SetNotBefore = nil;
  X509SetNotAfter: TX509SetNotAfter = nil;
  X509GetSerialNumber : TX509GetSerialNumber = nil;
  X509_STORE_add_cert : TX509_STORE_add_cert=nil;


  EvpPkeyNew: TEvpPkeyNew = nil;
  EvpPkeyFree: TEvpPkeyFree = nil;
  EvpPkeyAssign: TEvpPkeyAssign = nil;
  EvpGetDigestByName: TEvpGetDigestByName = nil;
  EVPcleanup: TEVPcleanup = nil;
  SSLeayversion: TSSLeayversion = nil;


  Rand_screen: TRand_screen = nil;
  BIO_new: TBIO_new = nil;
  BIO_free_all: TBIO_free_all = nil;
  BIO_free: TBIO_free = nil;
  BIO_s_mem: TBIO_s_mem = nil;
  BIO_new_socket:TBIO_new_socket =nil;
  BIO_ctrl_pending: TBIO_ctrl_pending = nil;
  BIO_read: TBIO_read = nil;
  BIO_write: TBIO_write = nil;

  d2iPKCS12bio: Td2iPKCS12bio = nil;
  PKCS12parse: TPKCS12parse = nil;
  PKCS12free: TPKCS12free = nil;
  Asn1UtctimeNew: TAsn1UtctimeNew = nil;
  Asn1UtctimeFree: TAsn1UtctimeFree = nil;
  d2i_X509:Td2i_X509=nil;
  i2dX509bio: Ti2dX509bio = nil;
  i2dPrivateKeyBio: Ti2dPrivateKeyBio = nil;

  // 3DES functions
  DESsetoddparity: TDESsetoddparity = nil;
  DESsetkey	   : TDESsetkey = nil;
  DESsetkeychecked: TDESsetkeychecked = nil;
  DESecbencrypt: TDESecbencrypt = nil;

  // RAND functions
  RAND_set_rand_method: TRAND_set_rand_method = nil;
  RAND_get_rand_method: TRAND_get_rand_method = nil;
  RAND_SSLeay: TRAND_SSLeay = nil;
  RAND_cleanup: TRAND_cleanup = nil;
  RAND_bytes: TRAND_bytes = nil;
  RAND_pseudo_bytes: TRAND_pseudo_bytes = nil;
  RAND_seed: TRAND_seed = nil;
  RAND_add: TRAND_add = nil;
  RAND_load_file: TRAND_load_file = nil;
  RAND_write_file: TRAND_write_file = nil;
  RAND_file_name: TRAND_file_name = nil;
  RAND_status: TRAND_status = nil;
  RAND_query_egd_bytes: TRAND_query_egd_bytes = nil;
  RAND_egd: TRAND_egd = nil;
  RAND_egd_bytes: TRAND_egd_bytes = nil;
  ERR_load_RAND_strings: TERR_load_RAND_strings = nil;
  RAND_poll: TRAND_poll = nil;

  // RSA Functions
  RSA_new: TRSA_new = nil;
  RSA_new_method: TRSA_new_method = nil;
  RSA_size: TRSA_size = nil;
  RsaGenerateKey: TRsaGenerateKey = nil;
  RSA_generate_key_ex: TRSA_generate_key_ex = nil;
  RSA_check_key: TRSA_check_key = nil;
  RSA_public_encrypt: TRSA_public_encrypt = nil;
  RSA_private_encrypt: TRSA_private_encrypt = nil;
  RSA_public_decrypt: TRSA_public_decrypt = nil;
  RSA_private_decrypt: TRSA_private_decrypt = nil;
  RSA_free: TRSA_free = nil;
  RSA_flags: TRSA_flags = nil;
  RSA_set_default_method: TRSA_set_default_method = nil;
  RSA_get_default_method: TRSA_get_default_method = nil;
  RSA_get_method: TRSA_get_method = nil;
  RSA_set_method: TRSA_set_method = nil;

  // X509 Functions

  d2i_RSAPublicKey: Td2i_RSAPublicKey = nil;
  i2d_RSAPublicKey: Ti2d_RSAPublicKey = nil;
  d2i_RSAPrivateKey: Td2i_RSAPrivateKey = nil;
  i2d_RSAPrivateKey: Ti2d_RSAPrivateKey = nil;


  // Crypto Functions *** REQUIRED FOR MULTI_THREADING  ***
  CRYPTO_malloc_init                  : TCRYPTO_malloc_init = nil;
  CRYPTO_num_locks                    : TCRYPTO_num_locks = nil;
  CRYPTO_set_locking_callback         : TCRYPTO_set_locking_callback = nil;

  CRYPTO_THREADID_set_numeric         : TCRYPTO_THREADID_set_numeric = nil;
  CRYPTO_THREADID_set_pointer         : TCRYPTO_THREADID_set_pointer = nil;
  CRYPTO_THREADID_set_callback        : TCRYPTO_THREADID_set_callback = nil;

  CRYPTO_set_dynlock_create_callback  : TCRYPTO_set_dynlock_create_callback = nil;
  CRYPTO_set_dynlock_lock_callback    : TCRYPTO_set_dynlock_lock_callback = nil;
  CRYPTO_set_dynlock_destroy_callback : TCRYPTO_set_dynlock_destroy_callback = nil;

  SSLeay_version: TSSLeay_version = nil;

  // EVP Functions

  OpenSSL_add_all_algorithms: TOpenSSL_add_all_algorithms = nil;
  OpenSSL_add_all_ciphers: TOpenSSL_add_all_ciphers = nil;
  OpenSSL_add_all_digests: TOpenSSL_add_all_digests = nil;
  //
  EVP_DigestInit: TEVP_DigestInit = nil;
  EVP_DigestUpdate: TEVP_DigestUpdate = nil;
  EVP_DigestFinal: TEVP_DigestFinal = nil;

  EVP_SignFinal: TEVP_SignFinal = nil;
  EVP_PKEY_size: TEVP_PKEY_size = nil;
  EVP_PKEY_free: TEVP_PKEY_free = nil;
  EVP_VerifyFinal: TEVP_VerifyFinal = nil;
  //
  EVP_get_cipherbyname: TEVP_get_cipherbyname = nil;
  EVP_get_digestbyname: TEVP_get_digestbyname = nil;
  //
  EVP_CIPHER_CTX_init: TEVP_CIPHER_CTX_init = nil;
  EVP_CIPHER_CTX_cleanup: TEVP_CIPHER_CTX_cleanup = nil;
  EVP_CIPHER_CTX_set_key_length: TEVP_CIPHER_CTX_set_key_length = nil;
  EVP_CIPHER_CTX_ctrl: TEVP_CIPHER_CTX_ctrl = nil;
  //
  EVP_EncryptInit: TEVP_EncryptInit = nil;
  EVP_EncryptUpdate: TEVP_EncryptUpdate = nil;
  EVP_EncryptFinal: TEVP_EncryptFinal = nil;
  //
  EVP_DecryptInit: TEVP_DecryptInit = nil;
  EVP_DecryptUpdate: TEVP_DecryptUpdate = nil;
  EVP_DecryptFinal: TEVP_DecryptFinal = nil;

  // PEM
  PEM_read_bio_PrivateKey: TPEM_read_bio_PrivateKey = nil;

  PEM_read_bio_PUBKEY: TPEM_read_bio_PUBKEY = nil;
  PEM_write_bio_PrivateKey: TPEM_write_bio_PrivateKey = nil;
  PEM_write_bio_PUBKEY: TPEM_write_bio_PUBKEY = nil;

  // BIO Functions

  BIO_ctrl: TBIO_ctrl = nil;
  BIO_s_file: TBIO_s_file = nil;
  BIO_new_file: TBIO_new_file = nil;
  BIO_new_mem_buf: TBIO_new_mem_buf = nil;

  // DH Functions

  get_dh512 : Tget_dh512=nil;
  get_dh1024: Tget_dh1024=nil;

Type
  TCertDateShort=record
    Year  : Array[0..1] of Char;
    Month : Array[0..1] of Char;
    Day   : Array[0..1] of Char;
    Hour  : Array[0..1] of Char;
    Min   : Array[0..1] of Char;
    Sec   : Array[0..1] of Char;
  end;
  
var
  SSLLibHandle: TLibHandle = 0;
  SSLUtilHandle: TLibHandle = 0;
  SSLLibFile: Core.Strings.VarString = '';
  SSLUtilFile: Core.Strings.VarString = '';

procedure Empty(var Item:TCertData); overload;
procedure Init(var Item:TCertData); overload;
procedure Done(Var Item:TCertData); overload;

procedure Empty(var Item:TCertList); overload;
procedure Init(var Item:TCertList); overload;
procedure Done(Var Item:TCertList); overload;

function  getItem(ID:QWord; Var List:TCertList):PCertData;

function ToDateTime(Value: PASN1_String): TDateTime;


function IsThreadSafe:Boolean;
function IsSSLloaded: Boolean;
function Islibealoaded: Boolean;
function InitSSLInterface(AVerboseLoading: Boolean = False): Boolean;
function InitThreadLocks:Boolean;

function DestroySSLInterface: Boolean;
function InitSSLEAInterface(AVerboseLoading: Boolean = False): Boolean;
function InitLibeaInterface(AVerboseLoading: Boolean = False): Boolean;
function DestroySSLEAInterface: Boolean;
function DestroyLibeaInterface: Boolean;

function SSL_ctx_set_mode(ctx: PSSL_CTX; mode: cLong): cLong;
function SSL_ctx_get_mode(ctx: PSSL_CTX): cLong;
function SSL_set_mode(s: PSSL; mode: cLong): cLong;
function SSL_get_mode(s: PSSL): cLong;

function SSLCTXSetOptions(ctx: PSSL_CTX; op: cLong): cLong;
function SSLCTXGetOptions(ctx: PSSL_CTX): cLong;

function  toString(value:System.Pointer):Core.Strings.VarString;  overload;

var
  OpenSSL_unavailable_functions: Core.Strings.VarString;

implementation
var
  SSLloaded   : boolean = false;
  libealoaded : boolean = false;
  LocksLoaded : boolean = false;

  Locks       : TTHREAD_LOCKS;

function toString(Value:System.Pointer):Core.Strings.VarString;
begin
  Result:=Format('%0:P',[Value]);
end;

{$IFNDEF WINDOWS}
{ Try to load all library versions until you find or run out }
function LoadLibHack(const Value: Core.Strings.VarString): HModule;
var
  i: cInt;
begin
  Result := NilHandle;
  
  for i := Low(DLLVersions) to High(DLLVersions) do begin
    {$IFDEF DARWIN}
    Result := LoadLibrary(Value + DLLVersions[i] + '.dylib');
    {$ELSE}
    Result := LoadLibrary(Value + '.so' + DLLVersions[i]);
    {$ENDIF}
    
    if Result <> NilHandle then
      Break;
  end;
end;
{$ENDIF}

function LoadLib(const Value: Core.Strings.VarString): HModule;
begin
  {$IFDEF WINDOWS}
  Result := LoadLibrary(Value);
  {$ELSE}
  Result := LoadLibHack(Value);
  {$ENDIF}
end;

function GetProcAddr(module: HModule; const ProcName: Core.Strings.VarString; VerboseLoading: Boolean): SslPtr;
begin
  Result := GetProcAddress(module, PChar(ProcName));
  if VerboseLoading and (Result = nil) then
    OpenSSL_unavailable_functions := OpenSSL_unavailable_functions + ProcName + LineEnding;
end;


function cbDynLockAdd(sFile:PChar; line:cInt) : PCRYPTO_dynlock_value; cdecl;
var
  l:PRTLCriticalSection;
begin
  System.New(l);
  System.InitCriticalSection(l^);
  Result:=l;
end;

procedure cbDynLockDelete(l:PCRYPTO_dynlock_value; sFile:PChar; line:cInt); cdecl;
begin
  System.DoneCriticalsection(l^);
  System.Dispose(l);
  l:=nil;
end;

procedure cbDynLockMechanism(mode:cInt; l:PCRYPTO_dynlock_value; sFile:PChar; line:cInt); cdecl;
begin
  Mode:=Mode and not CRYPTO_READ;
  Mode:=Mode and not CRYPTO_WRITE;
  case mode of
    CRYPTO_LOCK   : EnterCriticalSection(l^);
    CRYPTO_UNLOCK : LeaveCriticalSection(l^);
  end;
end;

procedure cbLockingMechanism(mode:cint; n:cInt; sFile:Pchar; Line:cInt); cdecl;
begin
  Mode:=Mode and not CRYPTO_READ;
  Mode:=Mode and not CRYPTO_WRITE;
  case mode of
    CRYPTO_LOCK   : EnterCriticalSection(Locks[n]);
    CRYPTO_UNLOCK : LeaveCriticalSection(Locks[n]);
  end;
end;

procedure cbThreadID(id:PCRYPTO_THREADID); cdecl;
begin
  {$if defined(Darwin)}
    CRYPTO_THREADID_set_pointer(id,getThreadID);
  {$elseif defined(Unix)}
    CRYPTO_THREADID_set_numeric(id,getThreadID);
  {$else}
    CRYPTO_THREADID_set_numeric(id,getThreadID);
  {$endif}
end;

function InitThreadLocks():boolean;
var
  iLockCount:LongInt;
  iLcv:LongInt;
begin
  Result:=false;

  iLockCount:=CRYPTO_num_locks();
  SetLength(Locks,iLockCount);
  for iLcv:=0 to iLockCount-1 do
    System.InitCriticalSection(Locks[iLcv]);

  CRYPTO_set_locking_callback(@cbLockingMechanism);

  CRYPTO_THREADID_set_callback(@cbThreadID);

  CRYPTO_set_dynlock_create_callback(@cbDynLockAdd);
  CRYPTO_set_dynlock_destroy_callback(@cbDynLockDelete);
  CRYPTO_set_dynlock_lock_callback(@cbDynLockMechanism);

  if Assigned(CRYPTO_malloc_init) then
     CRYPTO_malloc_init();

  LocksLoaded:=true;
  Result:=true;
end;

// The AVerboseLoading parameter can be used to check which particular
// functions weren't loaded correctly. They will be available in the
// global variable OpenSSL_unavailable_functions
function InitSSLInterface(AVerboseLoading: Boolean = False): Boolean;
Begin
 try
   if InitSSLEAInterface(AVerboseLoading) then
	if InitLIBEAInterface(AVerboseLoading) then
	  result:=true
	else
	  result:=false
   else
    result:=false;
 except
   result:=false;
 end;
end;

function InitSSLEAInterface(AVerboseLoading: Boolean = False): Boolean;
begin
  if not IsLibEaloaded then begin
    SSLLibHandle := LoadLib(DLLSSLName);
{$IFNDEF UNIX}
    if (SSLLibHandle = 0) then
      SSLLibHandle := LoadLib(DLLSSLName2);
{$ENDIF}
    if (SSLLibHandle <> 0) then begin
      SSL_get_error := GetProcAddr(SSLLibHandle, 'SSL_get_error', AVerboseLoading);
      ERR_clear_error:=GetProcAddr(SSLLibHandle, 'ERR_clear_error', AVerboseLoading);
      SslLibraryInit := GetProcAddr(SSLLibHandle, 'SSL_library_init', AVerboseLoading);
      SslLoadErrorStrings := GetProcAddr(SSLLibHandle, 'SSL_load_error_strings', AVerboseLoading);
      SSL_CTX_set_cipher_list := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_cipher_list', AVerboseLoading);
      SSL_set_cipher_list:=GetProcAddr(SSLLibHandle,'SSL_set_cipher_list',AVerboseLoading);
      SSL_CTX_new := GetProcAddr(SSLLibHandle, 'SSL_CTX_new', AVerboseLoading);
      SSL_CTX_new_shared := GetProcAddr(SSLLibHandle, 'SSL_CTX_new_shared', AVerboseLoading);
      SSL_CTX_free := GetProcAddr(SSLLibHandle, 'SSL_CTX_free', AVerboseLoading);
      SSL_set_fd := GetProcAddr(SSLLibHandle, 'SSL_set_fd', AVerboseLoading);
      SSL_ctrl := GetProcAddr(SSLLibHandle, 'SSL_ctrl', AVerboseLoading);
      SSL_ctx_ctrl := GetProcAddr(SSLLibHandle, 'SSL_CTX_ctrl', AVerboseLoading);
      SSL_CTX_get_cert_store:=GetProcAddr(SSLLibHandle, 'SSL_CTX_get_cert_store', AVerboseLoading);
      SSL_CTX_add_client_CA:=GetProcAddr(SSLLibHandle, 'SSL_CTX_add_client_CA', AVerboseLoading);
      SslMethodV2 := GetProcAddr(SSLLibHandle, 'SSLv2_method', AVerboseLoading);
      SslMethodV3 := GetProcAddr(SSLLibHandle, 'SSLv3_method', AVerboseLoading);
      SslMethodTLSV1 := GetProcAddr(SSLLibHandle, 'TLSv1_method', AVerboseLoading);
      SslMethodV23 := GetProcAddr(SSLLibHandle, 'SSLv23_method', AVerboseLoading);
      SSLv23_server_method:= GetProcAddr(SSLLibHandle, 'SSLv23_server_method', AVerboseLoading);
      SSLv3_server_method:= GetProcAddr(SSLLibHandle, 'SSLv3_server_method', AVerboseLoading);
      SSLv23_client_method:= GetProcAddr(SSLLibHandle, 'SSLv23_client_method', AVerboseLoading);
      TLSv1_server_method:=GetProcAddr(SSLLibHandle, 'TLSv1_server_method', AVerboseLoading);
      TLS_server_method:=GetProcAddr(SSLLibHandle, 'TLS_server_method', AVerboseLoading);

      TLSv1_1_server_method:=GetProcAddr(SSLLibHandle,'TLSv1_1_server_method',AVerboseLoading);
      TLSv1_2_server_method:=GetProcAddr(SSLLibHandle,'TLSv1_2_server_method',AVerboseLoading);
      TLSv1_method:=GetProcAddr(SSLLibHandle,'TLSv1_method',AVerboseLoading);
      TLSv1_1_method:=GetProcAddr(SSLLibHandle,'TLSv1_1_method',AVerboseLoading);
      TLSv1_2_method:=GetProcAddr(SSLLibHandle,'TLSv1_2_method',AVerboseLoading);
      TLS_method:=GetProcAddr(SSLLibHandle,'TLS_method',AVerboseLoading);


      SslCtxUsePrivateKey := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_PrivateKey', AVerboseLoading);
      SSL_CTX_use_PrivateKey_ASN1 := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_PrivateKey_ASN1', AVerboseLoading);
      SSL_CTX_add_extra_chain_cert :=GetProcAddr(SSLLibHandle,'SSL_CTX_add_extra_chain_cert',AVerboseLoading);
      SSL_CTX_use_RSAPrivateKey_ASN1 := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_RSAPrivateKey_ASN1', AVerboseLoading);
      SslCtxUsePrivateKeyFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_PrivateKey_file', AVerboseLoading);
      SslCtxUseRSAPrivateKeyFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_RSAPrivateKey_file', AVerboseLoading);
      SSL_CTX_use_Certificate := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate', AVerboseLoading);
      SSL_CTX_use_Certificate_ASN1 := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_Certificate_ASN1', AVerboseLoading);
      SslCtxUseCertificateFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate_file', AVerboseLoading);
      SslCtxUseCertificateChainFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_use_certificate_chain_file', AVerboseLoading);
      SslCtxCheckPrivateKeyFile := GetProcAddr(SSLLibHandle, 'SSL_CTX_check_private_key', AVerboseLoading);
      SslCtxSetDefaultPasswdCb := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_default_passwd_cb', AVerboseLoading);
      SslCtxSetDefaultPasswdCbUserdata := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_default_passwd_cb_userdata', AVerboseLoading);
      SslCtxLoadVerifyLocations := GetProcAddr(SSLLibHandle, 'SSL_CTX_load_verify_locations', AVerboseLoading);
      SSL_CTX_set_tmp_dh :=GetProcAddr(SSLLibHandle, 'SSL_CTX_set_tmp_dh', AVerboseLoading);
      SSL_CTX_set_session_cache_mode :=GetProcAddr(SSLLibHandle, 'SSL_CTX_set_session_cache_mode', AVerboseLoading);
      SSL_CTX_get_session_cache_mode :=GetProcAddr(SSLLibHandle, 'SSL_CTX_get_session_cache_mode', AVerboseLoading);

      SSL_clear:=GetProcAddr(SSLLibHandle, 'SSL_clear', AVerboseLoading);
      SSL_new := GetProcAddr(SSLLibHandle, 'SSL_new', AVerboseLoading);
      SSL_free := GetProcAddr(SSLLibHandle, 'SSL_free', AVerboseLoading);
      SSL_accept := GetProcAddr(SSLLibHandle, 'SSL_accept', AVerboseLoading);
      SSL_do_handshake:=GetProcAddr(SSLLibHandle,'SSL_do_handshake',AVerboseLoading);

      SSL_connect := GetProcAddr(SSLLibHandle, 'SSL_connect', AVerboseLoading);
      SSL_shutdown := GetProcAddr(SSLLibHandle, 'SSL_shutdown', AVerboseLoading);
      SSL_set_bio:=GetProcAddr(SSLLibHandle,'SSL_set_bio',AVerboseLoading);
      SSL_set_accept_state := GetProcAddr(SSLLibHandle, 'SSL_set_accept_state', AVerboseLoading);
      SSL_set_connect_state := GetProcAddr(SSLLibHandle, 'SSL_set_connect_state', AVerboseLoading);
      SSL_read := GetProcAddr(SSLLibHandle, 'SSL_read', AVerboseLoading);
      SSL_peek := GetProcAddr(SSLLibHandle, 'SSL_peek', AVerboseLoading);
      SSL_write := GetProcAddr(SSLLibHandle, 'SSL_write', AVerboseLoading);
      SSL_pending := GetProcAddr(SSLLibHandle, 'SSL_pending', AVerboseLoading);
      SslGetPeerCertificate := GetProcAddr(SSLLibHandle, 'SSL_get_peer_certificate', AVerboseLoading);
      SslGetVersion := GetProcAddr(SSLLibHandle, 'SSL_get_version', AVerboseLoading);
      SslCtxSetVerify := GetProcAddr(SSLLibHandle, 'SSL_CTX_set_verify', AVerboseLoading);
      SSL_set_session_id_context:= GetProcAddr(SSLLibHandle, 'SSL_set_session_id_context', AVerboseLoading);
      SSL_CTX_set_session_id_context:= GetProcAddr(SSLLibHandle, 'SSL_CTX_set_session_id_context', AVerboseLoading);

      SslGetCurrentCipher := GetProcAddr(SSLLibHandle, 'SSL_get_current_cipher', AVerboseLoading);
      SslCipherGetName := GetProcAddr(SSLLibHandle, 'SSL_CIPHER_get_name', AVerboseLoading);
      SslCipherGetBits := GetProcAddr(SSLLibHandle, 'SSL_CIPHER_get_bits', AVerboseLoading);
      SslGetVerifyResult := GetProcAddr(SSLLibHandle, 'SSL_get_verify_result', AVerboseLoading);
      Rand_screen := GetProcAddr(SSLLibHandle, 'RAND_screen', AVerboseLoading);

      get_dh512 :=GetProcAddr(SSLLibHandle, 'get_dh512', AVerboseLoading);
      get_dh1024:=GetProcAddr(SSLLibHandle, 'get_dh1024', AVerboseLoading);

      Result := True;
      SSLloaded := True;
    end else begin
      if SSLLibHandle <> 0 then begin
        FreeLibrary(SSLLibHandle);
        SSLLibHandle := 0;
      end;
      Result := False;
    end;
  end else
    Result := true;
end;

function InitlibeaInterface(AVerboseLoading: Boolean = False): Boolean;
var
  iLcv:LongInt;
  iLockCount:LongInt;
begin
  if not Islibealoaded then begin
    SSLUtilHandle := LoadLib(DLLUtilName);
    if (SSLUtilHandle <> 0) then begin
      // ERR Functions
      ERR_load_crypto_strings := GetProcAddr(SSLUtilHandle, 'ERR_load_crypto_strings', AVerboseLoading);
      ERR_error_string := GetProcAddr(SSLUtilHandle, 'ERR_error_string', AVerboseLoading);
      ERR_get_error := GetProcAddr(SSLUtilHandle, 'ERR_get_error', AVerboseLoading);
      ERR_clear_error := GetProcAddr(SSLUtilHandle, 'ERR_clear_error', AVerboseLoading);
      ERR_free_strings := GetProcAddr(SSLUtilHandle, 'ERR_free_strings', AVerboseLoading);
      ERR_remove_state := GetProcAddr(SSLUtilHandle, 'ERR_remove_state', AVerboseLoading);
      ERR_peek_error := GetProcAddr(SSLUtilHandle, 'ERR_peek_error', AVerboseLoading);
      ERR_peek_last_error :=GetProcAddr(SSLUtilHandle, 'ERR_peek_last_error', AVerboseLoading);
      ERR_get_error_line :=GetProcAddr(SSLUtilHandle, 'ERR_get_error_line', AVerboseLoading);
      ERR_peek_error_line :=GetProcAddr(SSLUtilHandle, 'ERR_peek_error_line', AVerboseLoading);
      ERR_peek_last_error_line :=GetProcAddr(SSLUtilHandle, 'ERR_peek_last_error_line', AVerboseLoading);

      X509_new := GetProcAddr(SSLUtilHandle, 'X509_new', AVerboseLoading);
      X509_free := GetProcAddr(SSLUtilHandle, 'X509_free', AVerboseLoading);
      X509NameOneline := GetProcAddr(SSLUtilHandle, 'X509_NAME_oneline', AVerboseLoading);
      X509GetSubjectName := GetProcAddr(SSLUtilHandle, 'X509_get_subject_name', AVerboseLoading);
      X509GetIssuerName := GetProcAddr(SSLUtilHandle, 'X509_get_issuer_name', AVerboseLoading);
      X509NameHash := GetProcAddr(SSLUtilHandle, 'X509_NAME_hash', AVerboseLoading);
      X509Digest := GetProcAddr(SSLUtilHandle, 'X509_digest', AVerboseLoading);
      X509print := GetProcAddr(SSLUtilHandle, 'X509_print', AVerboseLoading);
      X509SetVersion := GetProcAddr(SSLUtilHandle, 'X509_set_version', AVerboseLoading);
      X509SetPubkey := GetProcAddr(SSLUtilHandle, 'X509_set_pubkey', AVerboseLoading);
      X509SetIssuerName := GetProcAddr(SSLUtilHandle, 'X509_set_issuer_name', AVerboseLoading);
      X509NameAddEntryByTxt := GetProcAddr(SSLUtilHandle, 'X509_NAME_add_entry_by_txt', AVerboseLoading);
      X509Sign := GetProcAddr(SSLUtilHandle, 'X509_sign', AVerboseLoading);
      X509GmtimeAdj := GetProcAddr(SSLUtilHandle, 'X509_gmtime_adj', AVerboseLoading);
      X509SetNotBefore := GetProcAddr(SSLUtilHandle, 'X509_set_notBefore', AVerboseLoading);
      X509SetNotAfter := GetProcAddr(SSLUtilHandle, 'X509_set_notAfter', AVerboseLoading);
      X509GetSerialNumber := GetProcAddr(SSLUtilHandle, 'X509_get_serialNumber', AVerboseLoading);
      X509_STORE_add_cert := GetProcAddr(SSLUtilHandle,'X509_STORE_add_cert',AVerboseLoading);
      EvpPkeyNew := GetProcAddr(SSLUtilHandle, 'EVP_PKEY_new', AVerboseLoading);
      EvpPkeyFree := GetProcAddr(SSLUtilHandle, 'EVP_PKEY_free', AVerboseLoading);
      EvpPkeyAssign := GetProcAddr(SSLUtilHandle, 'EVP_PKEY_assign', AVerboseLoading);
      EVPCleanup := GetProcAddr(SSLUtilHandle, 'EVP_cleanup', AVerboseLoading);
      EvpGetDigestByName := GetProcAddr(SSLUtilHandle, 'EVP_get_digestbyname', AVerboseLoading);
      SSLeayversion := GetProcAddr(SSLUtilHandle, 'SSLeay_version', AVerboseLoading);


      Rand_screen := GetProcAddr(SSLUtilHandle, 'RAND_screen', AVerboseLoading);
      BIO_new := GetProcAddr(SSLUtilHandle, 'BIO_new', AVerboseLoading);
      BIO_new_socket := GetProcAddr(SSLUtilHandle, 'BIO_new_socket', AVerboseLoading);
      BIO_free_all := GetProcAddr(SSLUtilHandle, 'BIO_free_all', AVerboseLoading);
      BIO_free := GetProcAddr(SSLUtilHandle, 'BIO_free', AVerboseLoading);
      BIO_s_mem := GetProcAddr(SSLUtilHandle, 'BIO_s_mem', AVerboseLoading);
      BIO_ctrl_pending := GetProcAddr(SSLUtilHandle, 'BIO_ctrl_pending', AVerboseLoading);
      BIO_read := GetProcAddr(SSLUtilHandle, 'BIO_read', AVerboseLoading);
      BIO_write := GetProcAddr(SSLUtilHandle, 'BIO_write', AVerboseLoading);

      d2iPKCS12bio := GetProcAddr(SSLUtilHandle, 'd2i_PKCS12_bio', AVerboseLoading);
      PKCS12parse := GetProcAddr(SSLUtilHandle, 'PKCS12_parse', AVerboseLoading);
      PKCS12free := GetProcAddr(SSLUtilHandle, 'PKCS12_free', AVerboseLoading);
      Asn1UtctimeNew := GetProcAddr(SSLUtilHandle, 'ASN1_UTCTIME_new', AVerboseLoading);
      Asn1UtctimeFree := GetProcAddr(SSLUtilHandle, 'ASN1_UTCTIME_free', AVerboseLoading);
      d2i_X509:=GetProcAddr(SSLUtilHandle, 'd2i_X509', AVerboseLoading);
      i2dX509bio := GetProcAddr(SSLUtilHandle, 'i2d_X509_bio', AVerboseLoading);
      i2dPrivateKeyBio := GetProcAddr(SSLUtilHandle, 'i2d_PrivateKey_bio', AVerboseLoading);

      // 3DES functions
      DESsetoddparity := GetProcAddr(SSLUtilHandle, 'des_set_odd_parity', AVerboseLoading);
      DESsetkeychecked := GetProcAddr(SSLUtilHandle, 'des_set_key_checked', AVerboseLoading);
      DESsetkey := GetProcAddr(SSLUtilHandle, 'des_set_key', AVerboseLoading);
      DESecbencrypt := GetProcAddr(SSLUtilHandle, 'des_ecb_encrypt', AVerboseLoading);
        //
      CRYPTO_malloc_init:=GetProcAddr(SSLUtilHandle, 'CRYPTO_malloc_init', AVerboseLoading);
      CRYPTO_num_locks := GetProcAddr(SSLUtilHandle, 'CRYPTO_num_locks', AVerboseLoading);
      CRYPTO_set_locking_callback := GetProcAddr(SSLUtilHandle, 'CRYPTO_set_locking_callback', AVerboseLoading);

      CRYPTO_set_dynlock_create_callback:=GetProcAddr(SSLUtilHandle, 'CRYPTO_set_dynlock_create_callback', AVerboseLoading);
      CRYPTO_set_dynlock_lock_callback:=GetProcAddr(SSLUtilHandle, 'CRYPTO_set_dynlock_lock_callback', AVerboseLoading);
      CRYPTO_set_dynlock_destroy_callback:=GetProcAddr(SSLUtilHandle, 'CRYPTO_set_dynlock_lock_callback', AVerboseLoading);

      CRYPTO_THREADID_set_numeric:=GetProcAddr(SSLUtilHandle, 'CRYPTO_THREADID_set_numeric', AVerboseLoading);
      CRYPTO_THREADID_set_pointer:=GetProcAddr(SSLUtilHandle, 'CRYPTO_THREADID_set_pointer', AVerboseLoading);
      CRYPTO_THREADID_set_callback:=GetProcAddr(SSLUtilHandle, 'CRYPTO_THREADID_set_callback', AVerboseLoading);

      // RAND functions
      RAND_set_rand_method := GetProcAddr(SSLUtilHandle, 'RAND_set_rand_method', AVerboseLoading);
      RAND_get_rand_method := GetProcAddr(SSLUtilHandle, 'RAND_get_rand_method', AVerboseLoading);
      RAND_SSLeay := GetProcAddr(SSLUtilHandle, 'RAND_SSLeay', AVerboseLoading);
      RAND_cleanup := GetProcAddr(SSLUtilHandle, 'RAND_cleanup', AVerboseLoading);
      RAND_bytes := GetProcAddr(SSLUtilHandle, 'RAND_bytes', AVerboseLoading);
      RAND_pseudo_bytes := GetProcAddr(SSLUtilHandle, 'RAND_pseudo_bytes', AVerboseLoading);
      RAND_seed := GetProcAddr(SSLUtilHandle, 'RAND_seed', AVerboseLoading);
      RAND_add := GetProcAddr(SSLUtilHandle, 'RAND_add', AVerboseLoading);
      RAND_load_file := GetProcAddr(SSLUtilHandle, 'RAND_load_file', AVerboseLoading);
      RAND_write_file := GetProcAddr(SSLUtilHandle, 'RAND_write_file', AVerboseLoading);
      RAND_file_name := GetProcAddr(SSLUtilHandle, 'RAND_file_name', AVerboseLoading);
      RAND_status := GetProcAddr(SSLUtilHandle, 'RAND_status', AVerboseLoading);
      RAND_query_egd_bytes := GetProcAddr(SSLUtilHandle, 'RAND_query_egd_bytes', AVerboseLoading); // 0.9.7+
      RAND_egd := GetProcAddr(SSLUtilHandle, 'RAND_egd', AVerboseLoading);
      RAND_egd_bytes := GetProcAddr(SSLUtilHandle, 'RAND_egd_bytes', AVerboseLoading);
      ERR_load_RAND_strings := GetProcAddr(SSLUtilHandle, 'ERR_load_RAND_strings', AVerboseLoading);
      RAND_poll := GetProcAddr(SSLUtilHandle, 'RAND_poll', AVerboseLoading);

        // RSA Functions
      RSA_new := GetProcAddr(SSLUtilHandle, 'RSA_new', AVerboseLoading);
      RSA_new_method := GetProcAddr(SSLUtilHandle, 'RSA_new_method', AVerboseLoading);
      RSA_size := GetProcAddr(SSLUtilHandle, 'RSA_size', AVerboseLoading);
      RsaGenerateKey := GetProcAddr(SSLUtilHandle, 'RSA_generate_key', AVerboseLoading);
      RSA_generate_key_ex := GetProcAddr(SSLUtilHandle, 'RSA_generate_key_ex', AVerboseLoading);
      RSA_check_key := GetProcAddr(SSLUtilHandle, 'RSA_check_key', AVerboseLoading);
      RSA_public_encrypt := GetProcAddr(SSLUtilHandle, 'RSA_public_encrypt', AVerboseLoading);
      RSA_private_encrypt := GetProcAddr(SSLUtilHandle, 'RSA_private_encrypt', AVerboseLoading);
      RSA_public_decrypt := GetProcAddr(SSLUtilHandle, 'RSA_public_decrypt', AVerboseLoading);
      RSA_private_decrypt := GetProcAddr(SSLUtilHandle, 'RSA_private_decrypt', AVerboseLoading);
      RSA_free := GetProcAddr(SSLUtilHandle, 'RSA_free', AVerboseLoading);
      RSA_flags := GetProcAddr(SSLUtilHandle, 'RSA_flags', AVerboseLoading);
      RSA_set_default_method := GetProcAddr(SSLUtilHandle, 'RSA_set_default_method', AVerboseLoading);
      RSA_get_default_method := GetProcAddr(SSLUtilHandle, 'RSA_get_default_method', AVerboseLoading);
      RSA_get_method := GetProcAddr(SSLUtilHandle, 'RSA_get_method', AVerboseLoading);
      RSA_set_method := GetProcAddr(SSLUtilHandle, 'RSA_set_method', AVerboseLoading);

      // X509 Functions
      d2i_RSAPublicKey := GetProcAddr(SSLUtilHandle, 'd2i_RSAPublicKey', AVerboseLoading);
      i2d_RSAPublicKey := GetProcAddr(SSLUtilHandle, 'i2d_RSAPublicKey', AVerboseLoading);
      d2i_RSAPrivateKey := GetProcAddr(SSLUtilHandle, 'd2i_RSAPrivateKey', AVerboseLoading);
      i2d_RSAPrivateKey := GetProcAddr(SSLUtilHandle, 'i2d_RSAPrivateKey', AVerboseLoading);

      // EVP Functions
      OpenSSL_add_all_algorithms := GetProcAddr(SSLUtilHandle, 'OPENSSL_add_all_algorithms_conf', AVerboseLoading);
      OpenSSL_add_all_ciphers := GetProcAddr(SSLUtilHandle, 'OpenSSL_add_all_ciphers', AVerboseLoading);
      OpenSSL_add_all_digests := GetProcAddr(SSLUtilHandle, 'OpenSSL_add_all_digests', AVerboseLoading);
        //
      EVP_DigestInit := GetProcAddr(SSLUtilHandle, 'EVP_DigestInit', AVerboseLoading);
      EVP_DigestUpdate := GetProcAddr(SSLUtilHandle, 'EVP_DigestUpdate', AVerboseLoading);
      EVP_DigestFinal := GetProcAddr(SSLUtilHandle, 'EVP_DigestFinal', AVerboseLoading);
      EVP_SignFinal := GetProcAddr(SSLUtilHandle, 'EVP_SignFinal', AVerboseLoading);
      EVP_PKEY_size := GetProcAddr(SSLUtilHandle,'EVP_PKEY_size', AVerboseLoading);
      EVP_PKEY_free := GetProcAddr(SSLUtilHandle,'EVP_PKEY_free', AVerboseLoading);
      EVP_VerifyFinal := GetProcAddr(SSLUtilHandle,'EVP_VerifyFinal', AverboseLoading);
      //
      EVP_get_cipherbyname := GetProcAddr(SSLUtilHandle, 'EVP_get_cipherbyname', AVerboseLoading);
      EVP_get_digestbyname := GetProcAddr(SSLUtilHandle, 'EVP_get_digestbyname', AVerboseLoading);
        //
      EVP_CIPHER_CTX_init := GetProcAddr(SSLUtilHandle, 'EVP_CIPHER_CTX_init', AVerboseLoading);
      EVP_CIPHER_CTX_cleanup := GetProcAddr(SSLUtilHandle, 'EVP_CIPHER_CTX_cleanup', AVerboseLoading);
      EVP_CIPHER_CTX_set_key_length := GetProcAddr(SSLUtilHandle, 'EVP_CIPHER_CTX_set_key_length', AVerboseLoading);
      EVP_CIPHER_CTX_ctrl := GetProcAddr(SSLUtilHandle, 'EVP_CIPHER_CTX_ctrl', AVerboseLoading);
        //
      EVP_EncryptInit := GetProcAddr(SSLUtilHandle, 'EVP_EncryptInit', AVerboseLoading);
      EVP_EncryptUpdate := GetProcAddr(SSLUtilHandle, 'EVP_EncryptUpdate', AVerboseLoading);
      EVP_EncryptFinal := GetProcAddr(SSLUtilHandle, 'EVP_EncryptFinal', AVerboseLoading);
        //
      EVP_DecryptInit := GetProcAddr(SSLUtilHandle, 'EVP_DecryptInit', AVerboseLoading);
      EVP_DecryptUpdate := GetProcAddr(SSLUtilHandle, 'EVP_DecryptUpdate', AVerboseLoading);
      EVP_DecryptFinal := GetProcAddr(SSLUtilHandle, 'EVP_DecryptFinal', AVerboseLoading);

        // PEM

      PEM_read_bio_PrivateKey := GetProcAddr(SSLUtilHandle, 'PEM_read_bio_PrivateKey', AVerboseLoading);
      PEM_read_bio_PUBKEY := GetProcAddr(SSLUtilHandle, 'PEM_read_bio_PUBKEY', AVerboseLoading);
      PEM_write_bio_PrivateKey := GetProcAddr(SSLUtilHandle, 'PEM_write_bio_PrivateKey', AVerboseLoading);
      PEM_write_bio_PUBKEY := GetProcAddr(SSLUtilHandle, 'PEM_write_bio_PUBKEY', AVerboseLoading);
				
        // BIO

      BIO_ctrl := GetProcAddr(SSLUtilHandle, 'BIO_ctrl', AVerboseLoading);
		
      BIO_s_file := GetProcAddr(SSLUtilHandle, 'BIO_s_file', AVerboseLoading);
      BIO_new_file := GetProcAddr(SSLUtilHandle, 'BIO_new_file', AVerboseLoading);
      BIO_new_mem_buf := GetProcAddr(SSLUtilHandle, 'BIO_new_mem_buf', AVerboseLoading);


       // Crypto Functions

      SSLeay_version := GetProcAddr(SSLUtilHandle, 'SSLeay_version', AVerboseLoading);

      Result := True;
      libealoaded := True;
      LocksLoaded:=false;
    end else begin
      if SSLUtilHandle <> 0 then begin
        FreeLibrary(SSLUtilHandle);
      end;
      Result := False;
    end;
  end else
    Result := true;
end;

function DestroySSLEAInterface: Boolean;
begin
  if IsSSLLoaded then begin
      EVPCleanup();
      ERR_remove_state(0);
  end;
  SSLloaded := false;
  if SSLLibHandle <> 0 then begin
    FreeLibrary(SSLLibHandle);
    SSLLibHandle := 0;
  end;
  ERR_clear_error:=nil;
  SSL_get_error := nil;
  SslLibraryInit := nil;
  SslLoadErrorStrings := nil;
  SSL_CTX_set_cipher_list := nil;
  SSL_CTX_new := nil;
  SSL_CTX_new_shared:=nil;
  SSL_CTX_free := nil;
  SSL_set_fd := nil;
  SSL_ctrl := nil;
  SSL_clear:=nil;
  SSL_ctx_ctrl := nil;
  SSL_CTX_get_cert_store:=nil;
  SSL_CTX_add_client_CA:=nil;
  SslMethodV2 := nil;
  SslMethodV3 := nil;
  SslMethodTLSV1 := nil;
  SslMethodV23 := nil;
  SSLv23_client_method:=nil;
  SSLv23_server_method:=nil;
  SSLv3_server_method:=nil;
  TLSv1_server_method:=nil;
  TLS_server_method :=nil;

  TLSv1_1_server_method:=nil;
  TLSv1_2_server_method:=nil;
  TLS_method:=nil;
  TLSv1_method:=nil;
  TLSv1_1_method:=nil;
  TLSv1_2_method:=nil;


  SslCtxUsePrivateKey := nil;
  SSL_CTX_use_PrivateKey_ASN1 := nil;
  SSL_CTX_use_RSAPrivateKey_ASN1 := nil;
  SslCtxUsePrivateKeyFile := nil;
  SSL_CTX_use_Certificate:= nil;
  SSL_CTX_use_Certificate_ASN1 := nil;
  SslCtxUseCertificateFile := nil;
  SslCtxUseCertificateChainFile := nil;
  SslCtxCheckPrivateKeyFile := nil;
  SslCtxSetDefaultPasswdCb := nil;
  SslCtxSetDefaultPasswdCbUserdata := nil;
  SslCtxLoadVerifyLocations := nil;
  SSL_CTX_set_session_cache_mode :=nil;
  SSL_CTX_get_session_cache_mode :=nil;
  SSL_CTX_set_tmp_dh:=nil;

  SSL_new := nil;
  SSL_free := nil;
  SSL_accept := nil;
  SSL_do_handshake :=nil;
  SSL_Connect := nil;
  SSL_shutdown := nil;
  SSL_read := nil;
  SSL_peek := nil;
  SSL_Write := nil;
  SSL_pending := nil;
  SslGetPeerCertificate := nil;
  SslGetVersion := nil;
  SslCtxSetVerify := nil;
  SSL_set_session_id_context:=nil;
  SSL_CTX_set_session_id_context:=nil;
  SslGetCurrentCipher := nil;
  SslCipherGetName := nil;
  SslCipherGetBits := nil;
  SslGetVerifyResult := nil;


  Result := True;
end;


function DestroylibeaInterface: Boolean;
var
  iLcv:LongInt;
begin
  if IslibeaLoaded then begin
    for iLcv:=0 to High(Locks) do
      System.DoneCriticalsection(Locks[iLcv]);
    //deinit library
    EVPCleanup;
    ERR_remove_state(0);
  end;
  libealoaded := false;
  if SSLUtilHandle <> 0 then begin
    FreeLibrary(SSLUtilHandle);
    SSLUtilHandle := 0;
  end;

  SSLeayversion := nil;
  ERR_load_crypto_strings := nil;
  X509_new := nil;
  X509_free := nil;
  X509NameOneline := nil;
  X509GetSubjectName := nil;
  X509GetIssuerName := nil;
  X509NameHash := nil;
  X509Digest := nil;
  X509print := nil;
  X509SetVersion := nil;
  X509SetPubkey := nil;
  X509SetIssuerName := nil;
  X509NameAddEntryByTxt := nil;
  X509Sign := nil;
  X509GmtimeAdj := nil;
  X509SetNotBefore := nil;
  X509SetNotAfter := nil;
  X509GetSerialNumber := nil;
  X509_STORE_add_cert:=nil;

  EvpPkeyNew := nil;
  EvpPkeyFree := nil;
  EvpPkeyAssign := nil;
  EVPCleanup := nil;
  EvpGetDigestByName := nil;
  Rand_screen := nil;

  BIO_new := nil;
  BIO_new_socket:=nil;
  BIO_free_all := nil;
  BIO_free := nil;
  BIO_s_mem := nil;
  BIO_ctrl_pending := nil;
  BIO_read := nil;
  BIO_write := nil;

  d2iPKCS12bio := nil;
  PKCS12parse := nil;
  PKCS12free := nil;
  Asn1UtctimeNew := nil;
  Asn1UtctimeFree := nil;
  i2dX509bio := nil;
  i2dPrivateKeyBio := nil;

  // 3DES functions
  DESsetoddparity := nil;
  DESsetkeychecked := nil;
  DESecbencrypt := nil;

  // CRYPTO Functions
  CRYPTO_num_locks := nil;
  CRYPTO_set_locking_callback:=nil;

  CRYPTO_THREADID_set_pointer:=nil;
  CRYPTO_THREADID_set_numeric:=nil;
  CRYPTO_THREADID_set_callback:=nil;

  CRYPTO_set_dynlock_create_callback:= nil;
  CRYPTO_set_dynlock_lock_callback:= nil;
  CRYPTO_set_dynlock_destroy_callback := nil;

  // RAND functions
  RAND_set_rand_method := nil;
  RAND_get_rand_method := nil;
  RAND_SSLeay := nil;
  RAND_cleanup := nil;
  RAND_bytes := nil;
  RAND_pseudo_bytes := nil;
  RAND_seed := nil;
  RAND_add := nil;
  RAND_load_file := nil;
  RAND_write_file := nil;
  RAND_file_name := nil;
  RAND_status := nil;
  RAND_query_egd_bytes := nil;
  RAND_egd := nil;
  RAND_egd_bytes := nil;
  ERR_load_RAND_strings := nil;
  RAND_poll := nil;

  // RSA Functions
  RSA_new := nil;
  RSA_new_method := nil;
  RSA_size := nil;
  RsaGenerateKey := nil;
  RSA_generate_key_ex := nil;
  RSA_check_key := nil;
  RSA_public_encrypt := nil;
  RSA_private_encrypt := nil;
  RSA_public_decrypt := nil;
  RSA_private_decrypt := nil;
  RSA_free := nil;
  RSA_flags := nil;
  RSA_set_default_method := nil;
  RSA_get_default_method := nil;
  RSA_get_method := nil;
  RSA_set_method := nil;

  // X509 Functions

  d2i_RSAPublicKey := nil;
  i2d_RSAPublicKey := nil;
  d2i_RSAPrivateKey := nil;
  i2d_RSAPrivateKey := nil;

  // ERR Functions
  ERR_error_string := nil;
  ERR_get_error := nil;
  ERR_clear_error := nil;
  ERR_free_strings := nil;
  ERR_remove_state := nil;

  // EVP Functions

  OpenSSL_add_all_algorithms := nil;
  OpenSSL_add_all_ciphers := nil;
  OpenSSL_add_all_digests := nil;
  //
  EVP_DigestInit := nil;
  EVP_DigestUpdate := nil;
  EVP_DigestFinal := nil;

  EVP_SignFinal := nil;
  EVP_PKEY_size := nil;
  EVP_PKEY_free := nil;
  EVP_VerifyFinal := nil;
  //
  EVP_get_cipherbyname := nil;
  EVP_get_digestbyname := nil;
  //
  EVP_CIPHER_CTX_init := nil;
  EVP_CIPHER_CTX_cleanup := nil;
  EVP_CIPHER_CTX_set_key_length := nil;
  EVP_CIPHER_CTX_ctrl := nil;
  //
  EVP_EncryptInit := nil;
  EVP_EncryptUpdate := nil;
  EVP_EncryptFinal := nil;
  //
  EVP_DecryptInit := nil;
  EVP_DecryptUpdate := nil;
  EVP_DecryptFinal := nil;

  // PEM

  PEM_read_bio_PrivateKey := nil;
  PEM_read_bio_PrivateKey := nil;
  PEM_read_bio_PUBKEY := nil;
  PEM_write_bio_PrivateKey := nil;
  PEM_write_bio_PUBKEY := nil;

  // BIO

  BIO_ctrl := nil;
  BIO_s_file := nil;
  BIO_new_file := nil;
  BIO_new_mem_buf := nil;

  // DH Functions
  get_dh1024 :=nil;
  get_dh512:=nil;

  // Crypto Functions

  SSLeay_version := nil;

  Result := True;
end;


function SSL_set_mode(s: PSSL; mode: cLong): cLong;
begin
  Result := SSL_ctrl(s, SSL_CTRL_MODE, mode, nil);
end;

function SSL_get_mode(s: PSSL): cLong;
begin
  Result := SSL_ctrl(s, SSL_CTRL_MODE, 0, nil);
end;

function SSL_ctx_set_mode(ctx: PSSL_CTX; mode: cLong): cLong;
begin
  Result := ssl_ctx_ctrl(ctx, SSL_CTRL_MODE, mode, nil);
end;

function ssl_ctx_get_mode(ctx: PSSL_CTX): cLong;
begin
  Result := SSL_ctx_ctrl(ctx, SSL_CTRL_MODE, 0, nil);
end;

function SSLCTXSetOptions(ctx: PSSL_CTX; op: cLong): cLong;
begin
  Result := SSL_ctx_ctrl(ctx, SSL_CTRL_OPTIONS, op, nil);
end;

function SSLCTXGetOptions(ctx: PSSL_CTX): cLong;
begin
  Result := SSL_ctx_ctrl(ctx, SSL_CTRL_OPTIONS, 0, nil);
end;

function DestroySSLInterface: Boolean;
begin
  result:=false;
  try
    if DestroySSLEAInterface then
      if DestroylibeaInterface then
       result := true
      else
       result := False
    else
      result:=false;
  except
    result:=false;
  end;
end;

function IsThreadSafe:Boolean;
begin
  Result:=LocksLoaded;
end;

function IsSSLloaded: Boolean;
begin
  Result := SSLLoaded;
end;

function Islibealoaded: Boolean;
begin
  Result := libeaLoaded;
end;

procedure Empty(var Item:TCertData);
begin
  with Item do begin
    ID:=0;
    Date:=0;
    SetLength(Key,0);
    SetLength(Request,0);
    Empty(Certs);
    SetLength(DerKey,0);
    Empty(DerCerts);
  end;
end;

procedure Init(var Item:TCertData);
begin
  with Item do begin
    ID:=0;
    Date:=0;
    SetLength(Key,0);
    SetLength(Request,0);
    Init(Certs);
    SetLength(DerKey,0);
    Init(DerCerts);
  end;
end;

procedure Done(Var Item:TCertData);
begin
  Finalize(Item.Key);
  Finalize(Item.Request);
  Done(Item.Certs);
  Finalize(Item.DerKey);
  Done(Item.DerCerts);
  Finalize(Item);
end;

procedure Empty(var Item:TCertList);
var
  iLcv:LongInt;
  itmP:PCertData;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  SetLength(Item,0);
end;

procedure Init(var Item:TCertList); overload;
var
  iLcv:LongInt;
  itmP:PCertData;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  SetLength(Item,0);
end;

procedure Done(Var Item:TCertList); overload;
var
  iLcv:LongInt;
  itmP:PCertData;
begin
  for iLcv:=0 to High(Item) do begin
    itmP:=Item[iLcv];
    Done(itmP^);
    Dispose(itmP);
  end;
  SetLength(Item,0);
end;

function  getItem(ID:QWord; Var List:TCertList):PCertData;
var
  iLcv:LongInt;
begin
  Result:=nil;
  for iLcv:=0 to High(List) do begin
    if List[iLcv]^.ID=ID then begin
      Result:=List[iLcv];
      break;
    end;
  end;
end;

function ToDateTime(Value: PASN1_String): TDateTime;
var
  iYear:Word;
  iMonth:Word;
  iDay:Word;
  iHour:Word;
  iMin:Word;
  iSec:Word;
  iMil:Word;
  procedure EncodeShortDate;
  var
    Stamp:TCertDateShort;
  begin
    System.Move(Value^.data[0],Stamp,12);
    iYear:=StrToIntDef(Stamp.Year,0)+2000;
    iMonth:=StrToIntDef(Stamp.Month,0);
    iDay:=StrToIntDef(Stamp.Day,0);
    iHour:=StrToIntDef(Stamp.Hour,0);
    iMin:=StrToIntDef(Stamp.Min,0);
    iSec:=StrToIntDef(Stamp.Sec,0);
  end;

begin
  iYear:=0;
  iMonth:=0;
  iDay:=0;
  iHour:=0;
  iMin:=0;
  iSec:=0;
  iMil:=0;
  // 171227235959Z'
  // 17 12 27 23 59 59 Z'
  case Value^.kind of
    V_ASN1_EOC     : EncodeShortDate();
    V_ASN1_UTCTIME : EncodeShortDate();
  end;
  Result:=DateUtils.EncodeDateTime(iYear,iMonth,iDay,iHour,iMin,iSec,iMil);
end;

finalization
  DestroySSLInterface;
end.

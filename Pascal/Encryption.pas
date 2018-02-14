unit Encryption;


interface

uses
  Classes, SysUtils, cTypes;

Type
  CRYPTO_THREADID=record
    ptr : pointer;
    val : culong;
  end;
  PCRYPTO_THREADID = ^CRYPTO_THREADID;

  CRYPTO_dynlock_value=TRTLCriticalSection;
  PCRYPTO_dynlock_value=PRTLCriticalSection;

  TTHREAD_LOCKS=Array of TRTLCriticalSection;

  TTHREAD_LOCKING_FUNCTION=procedure(mode:cInt; n:LongInt; sFile:Pchar; Line:cInt); cdecl;
  TTHREAD_ADDEDLOCK_FUNCTION=function (num : pcint; mount: cint; kind : cint; sFile:PChar; line:cInt):cint; cdecl;
  TTHREAD_DYNLOCK_CREATE_FUNCTION=function(sFile:PChar; line:cInt) : PCRYPTO_dynlock_value; cdecl;
  TTHREAD_DYNLOCK_LOCK_FUNCTION=procedure(mode:cInt; l:PCRYPTO_dynlock_value; sFile:PChar; line:cInt); cdecl;
  TTHREAD_DYNLOCK_DESTROY_FUNCTION=procedure(l:PCRYPTO_dynlock_value; sFile:PChar; line:cInt); cdecl;
  TTHREAD_FUNC=procedure(id:PCRYPTO_THREADID); cdecl;

  TCRYPTO_malloc_init = function : cInt; cdecl;
  TCRYPTO_num_locks   = function : cInt; cdecl;

  TCRYPTO_THREADID_set_numeric=procedure (id:PCRYPTO_THREADID; val:PtrUInt); cdecl;
  TCRYPTO_THREADID_set_pointer=procedure (id:PCRYPTO_THREADID; val:pointer); cdecl;
  TCRYPTO_THREADID_set_callback=function (Method:TTHREAD_FUNC):cint; cdecl;

  TCRYPTO_set_dynlock_create_callback=procedure(Method:TTHREAD_DYNLOCK_CREATE_FUNCTION); cdecl;
  TCRYPTO_set_dynlock_lock_callback=procedure(Method:TTHREAD_DYNLOCK_LOCK_FUNCTION); cdecl;
  TCRYPTO_set_dynlock_destroy_callback=procedure(Method:TTHREAD_DYNLOCK_DESTROY_FUNCTION); cdecl;

  TCRYPTO_set_locking_callback = procedure(Method: TTHREAD_LOCKING_FUNCTION); cdecl;


const

CRYPTO_LOCK_ERR	        = 1;
CRYPTO_LOCK_ERR_HASH		= 2;
CRYPTO_LOCK_X509		= 3;
CRYPTO_LOCK_X509_INFO		= 4;
CRYPTO_LOCK_X509_PKEY		= 5;
CRYPTO_LOCK_X509_CRL		= 6;
CRYPTO_LOCK_X509_REQ		= 7;
CRYPTO_LOCK_DSA		= 8;
CRYPTO_LOCK_RSA		= 9;
CRYPTO_LOCK_EVP_PKEY		= 10;
CRYPTO_LOCK_X509_STORE	= 11;
CRYPTO_LOCK_SSL_CTX		= 12;
CRYPTO_LOCK_SSL_CERT		= 13;
CRYPTO_LOCK_SSL_SESSION	= 14;
CRYPTO_LOCK_SSL_SESS_CERT	= 15;
CRYPTO_LOCK_SSL		= 16;
CRYPTO_LOCK_RAND		= 17;
CRYPTO_LOCK_MALLOC		= 18;
CRYPTO_LOCK_BIO		= 19;
CRYPTO_LOCK_GETHOSTBYNAME	= 20;
CRYPTO_LOCK_GETSERVBYNAME	= 21;
CRYPTO_LOCK_READDIR		= 22;
CRYPTO_LOCK_RSA_BLINDING	= 23;
CRYPTO_LOCK_DH		= 24;
CRYPTO_LOCK_MALLOC2		= 25;
CRYPTO_LOCK_DSO		= 26;
CRYPTO_LOCK_DYNLOCK		= 27;
CRYPTO_NUM_LOCKS		= 28;

CRYPTO_LOCK		        = $01;
CRYPTO_UNLOCK		        = $02;
CRYPTO_READ		        = $04;
CRYPTO_WRITE		        = 8;

CRYPTO_F_CRYPTO_GET_EX_NEW_INDEX	= 100;
CRYPTO_F_CRYPTO_GET_NEW_DYNLOCKID	= 103;
CRYPTO_F_CRYPTO_GET_NEW_LOCKID	= 101;
CRYPTO_F_CRYPTO_SET_EX_DATA		= 102;

// Reason codes.
CRYPTO_R_NO_DYNLOCK_CREATE_CALLBACK   = 10;

const
  HexTbl: array[0..15] of char='0123456789abcdef';     // lowercase

implementation

end.


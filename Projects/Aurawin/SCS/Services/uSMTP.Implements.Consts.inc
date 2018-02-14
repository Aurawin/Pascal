Const
  WHITELIST_NOTICE:array[boolean] of string=('your IP and content, is subject to security policies','pleased to receive your mail');
  RS_NONE       =0;
  RS_HELO       =1 shl 0;
  RS_MailFrom   =1 shl 2;
  RS_Recip      =1 shl 3;
  RS_Data       =1 shl 4;
  RS_Body       =1 shl 5;
  RS_EndSession =1 shl 6;
  RS_Lookup_MX  =1 shl 7;
  RS_Lookup_IP  =1 shl 8;
  RS_EHELO      =1 shl 9;
  RS_LOGIN_USER =1 shl 10;
  RS_LOGIN_PSWD =1 shl 11;
  RS_LOGIN_OK   =1 shl 12;
  RS_WELCOME    =1 shl 13;
  RS_CONNECT    =1 shl 14;


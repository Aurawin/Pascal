{$ifdef RSR_IMAP_ONLY}
hAuDisks,uAuDiskd,  { AuDisk Servies}
uIMAPd,             { IMAP Services}
{$else}
RSR.AuDisks,
uAuDiskd,           { AuDisk Servies}
hHTTPd, uHTTPd,     { HTTP Services}
uIMAPd,             { IMAP Services}
uPOP3d,             { POP3 Services}
uSMTPd,             { SMTP Services}
uXMPPd,             { XMPP Services}
uRTSPd,             { RTSP Services}
uProviders,         { Search Services}
{$endif}

 ByteCount:=0;
 Winsock2.ioctlsocket(Socket, FIONREAD, @ByteCount);
 Result:=ByteCount;

 ByteCount:=0;
 fpioctl(Socket, FIONREAD, @ByteCount);
 Result:=ByteCount;

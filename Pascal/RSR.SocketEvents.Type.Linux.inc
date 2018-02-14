
Type TPollQueue=cInt;
Type TSocketPollEvent=TEPoll_Event;
Type TSocketPollEvents=Array[0..QUEUE_POLL_SIZE] of TEPoll_Event;
Type TSocketEventQueue=LongInt;
Type TSocketEventCallback=procedure(Socket:Sockets.TSocket; Var Event:TEPoll_Event);

procedure SocketEventsPoll(qPoll:TPollQueue; qEvents:TSocketPollEvents; Callback:TSocketEventCallback);
procedure SocketEventsAttach(qPoll:TPollQueue; Const Socket:Sockets.TSocket; Filter:TSocketFilter);
procedure SocketEventsDetach(qPoll:TPollQueue;Const Socket:Sockets.TSocket);
procedure SocketEventsInitQueues(var qRecv,qWrite,qConn,qDiscon:TPollQueue);
procedure SocketEventsCreateQueues(var qRecv,qWrite,qConn,qDiscon:TPollQueue);



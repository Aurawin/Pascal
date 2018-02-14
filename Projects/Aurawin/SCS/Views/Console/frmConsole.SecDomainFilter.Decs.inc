procedure cbSecDomainFilterOnInitItem(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterOnEmptyItem(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterOnDoneItem(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterWorkerThreadInit(Thread:TWorkerThread);
procedure cbSecDomainFilterWorkerThreadDone(Thread:TWorkerThread);
procedure cbSecDomainFilterAddedWorkItem(Thread:TWorkerThread; var Data:Pointer);

procedure cbSecDomainFilterOnWorkItemVoid(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterOnWorkItemWhite(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterOnWorkItemBlack(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterOnWorkItemAcceptable(Thread:TWorkerThread; var Data:Pointer);
procedure cbSecDomainFilterOnWorkItemWBALists(Thread:TWorkerThread; var Data:Pointer);


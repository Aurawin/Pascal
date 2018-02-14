procedure  AuraProcess.LoadContentTypes;
begin
  Storage.ContentTypes.Items.DB.Load(Storage.Main.Task,Storage.ContentTypes.List);
  {$ifdef RSR_DEBUG}
    Core.Logging.Native.WriteLogEntry(FDomain.Name,'AuraProcess.LoadContentTypes',Concat(IntToStr(Length(ContentTypes)),' ContentTypes loaded.'));
  {$endif}
end;


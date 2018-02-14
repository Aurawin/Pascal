  procedure PushBoolean;
  begin
    Core.Database.Types.PBool(Command.Value)^:=Source.AsBoolean;
  end;

  procedure PushByte;
  begin
    Core.Arrays.Bytes.fromString(Source.AsString,Core.Database.Types.PByte(Command.Value)^);
  end;

  procedure PushWord;
  begin
    Core.Database.Types.PWord(Command.Value)^:=Source.AsInteger;
  end;

  procedure PushInteger;
  begin
    Core.Database.Types.PInteger(Command.Value)^:=Source.AsInteger;
  end;

  procedure PushLargeInt;
  begin
    Core.Database.Types.PLargeInt(Command.Value)^:=Source.AsLargeInt;
  end;

  procedure PushQWord;
  begin
    Core.Database.Types.PLargeWord(Command.Value)^:=Source.AsLargeInt;
  end;

  procedure PushInt64Array;
  begin
    Core.Arrays.LargeInt.fromString(Source.AsString,Core.Database.Types.PLargeIntArray(Command.Value)^,',');
  end;

  procedure PushQWordArray;
  begin
    Core.Arrays.LargeWord.fromString(Source.AsString,Core.Database.Types.PLargeWordArray(Command.Value)^,',');
  end;

  procedure PushDateTime;
  begin
    Core.Database.Types.PDateTime(Command.Value)^:=Source.AsDateTime;
  end;

  procedure PushFloat;
  begin
    Core.Database.Types.PFloat(Command.Value)^:=Source.AsFloat;
  end;

  procedure PushDouble;
  begin
    Core.Database.Types.PDouble(Command.Value)^:=Source.AsFloat;
  end;

  procedure PushShortString;
  begin
    Core.Database.Types.PSmallString(Command.Value)^:=Source.AsString;
  end;

  procedure PushString;
  begin
    Core.Database.Types.PVarString(Command.Value)^:=Source.AsString;
  end;

  procedure PushByteArray;
  begin
    Core.Arrays.Bytes.fromString(Source.AsString,Core.Database.Types.PBuffer(Command.Value));

  end;

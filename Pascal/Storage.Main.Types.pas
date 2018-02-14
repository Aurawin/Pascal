unit Storage.Main.Types;

interface

uses
  Core.Logging,
  Core.Database,
  Core.Database.Types,

  Encryption.Simple,

  App.IniFile,

  Classes;
Type
  Format=class
  Type
    Exception=class
    const
      Module                     : Core.Database.Types.VarString = 'Module';
      Task                       : Core.Database.Types.VarString = 'Task';
      Location                   : Core.Database.Types.VarString = 'Location';
      Message                    : Core.Database.Types.VarString = 'Message';
    end;
  Const
    Sep                          : Core.Database.Types.VarString = ': ';
    Delim                        : Core.Database.Types.VarString = '; ';
    Service                      : Core.Database.Types.VarString = 'Storage';
    Domain                       : Core.Database.Types.VarString = 'System';
  end;

  THeader=Class(Core.Database.Types.THeader)
  private
    procedure OnException(sModule,sLocation,sTable,sTask,sMessage:VarString);
  public
    Constructor Create(); reIntroduce;
  end;


implementation

procedure THeader.OnException(sModule,sLocation,sTable,sTask,sMessage:VarString);
begin
  Core.Logging.Native.WriteLogEntry(
    DomainName,
    ServiceName,
    Concat(
      Format.Exception.Module,Format.Sep,sModule,Format.Delim,
      Format.Exception.Task,Format.Sep,sTask,Format.Delim,
      Format.Exception.Location,Format.Sep,sTask,Format.Delim,
      Format.Exception.Message,Format.Sep,sMessage
    )
  );
end;

Constructor THeader.Create();
begin
  HostName:=App.IniFile.Native.ReadString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_HOST,'');
  Username:=App.IniFile.Native.ReadString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_USER,'');
  Encoding:=App.IniFile.Native.ReadString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_ENCODING,'');
  Password:=Encryption.Simple.DecryptString(App.IniFile.Native.ReadString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_PASSWORD,''));

  Schema:=App.IniFile.Native.ReadString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_SCHEMA,'');

  Core.Database.fromString(App.IniFile.Native.ReadString(INI_DB_SECT_LOGIN,INI_DB_LOGIN_MODE,DB_MODES[Postgresql]),Mode);

  Inherited Create(
    Mode,
    Username,
    Password,
    Schema,
    HostName,
    'Storage',
    Encoding,
    @OnException
  );
end;

end.

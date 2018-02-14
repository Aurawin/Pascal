unit App.Lang;


interface

uses
  Core.Strings,
  Classes,
  SysUtils;
Type
  Table=class
  Type
    Engine=class
    type
      Status=class
      type
        Sync=class
        const
          Add                    : Core.Strings.VarString = 'Create';
          Read                   : Core.Strings.VarString = 'Read';
          Write                  : Core.Strings.VarString = 'Write';
          Scheduled              : Core.Strings.VarString = 'Scheduled';
          Buffered               : Core.Strings.VarString = 'Buffered';
        end;
      const
        Running                  : Core.Strings.VarString ='Running';
        Stopped                  : Core.Strings.VarString ='Stopped';
        Paused                   : Core.Strings.VarString ='Paused';
      end;
    end;
  end;

implementation

end.


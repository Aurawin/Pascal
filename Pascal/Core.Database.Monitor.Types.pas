unit Core.Database.Monitor.Types;

interface

uses
  Core.Database.Types;

Type
  PItem=^Item;
  Event=procedure (ItemP:PItem);
  Callback=function(aTask:Core.Database.Types.TTask; TableP:Core.Database.Types.PTable; ItemID:System.QWord; ItemP:Core.Database.Monitor.Types.PItem; Flag:System.Cardinal):System.Boolean;

  Item=record
    TableP         : Core.Database.Types.PTable;
    OnDestroy      : Event;
    OnNotify       : Callback;
  end;
implementation


end.


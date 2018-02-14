unit RSR.POP3;

interface

uses
  Classes,
  SysUtils,
  Storage.UserAccounts,
  Storage.UserStorage;

Const
  POP_STACK_SIZE=1024*5;
  MAX_POP3_ERRORS = 5;
  RS_NONE=0;
  RS_AUTHORIZATION=1 shl 0;
  RS_TRANSACTION  =1 shl 1;
  RS_MAILDROP     =1 shl 2;
  RS_Reserved2    =1 shl 3;
  RS_Reserved3    =1 shl 4;
  RS_EndSession   =1 shl 5;

Type

  TPOP3=record
    UAP              : Storage.UserAccounts.Items.PItem;
    sUser            : String;
    sPass            : String;
    ErrorCount       : Byte;
    DeleteIndex      : LongInt;
    State            : Byte;
    TopRequest       : TPOP3TopRequest;
    RetrRequest      : TPOP3RetrRequest;
    UIDLRequest      : TPOP3UIDLRequest;
    UniqueIDs        : TPOP3UIDLAllRequest;
  end;
  PPOP3=^TPOP3;

  procedure Init(Var Item:TPOP3); overload;
  procedure Done(Var Item:TPOP3); overload;


implementation


procedure Init(Var Item:TPOP3);
begin
  Item.UAP:=nil;
  SetLength(Item.sUser,0);
  SetLength(Item.sPass,0);
  Item.ErrorCount:=0;
  Item.DeleteIndex:=-1;
  Item.State:=RS_NONE;

  Empty(Item.TopRequest);
  Empty(Item.RetrRequest);
  Empty(Item.UIDLRequest);
  Empty(Item.UniqueIDs);
end;

procedure Done(Var Item:TPOP3);
begin
  Finalize(Item.sUser);
  Finalize(Item.sPass);
  Done(Item.TopRequest);
  Done(Item.RetrRequest);
  Done(Item.UIDLRequest);
  Done(Item.UniqueIDs);
  Finalize(Item);
end;


end.


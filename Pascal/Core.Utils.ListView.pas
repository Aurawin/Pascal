unit Core.Utils.ListView;

interface

uses
  Classes, SysUtils, ComCtrls;

  function IndexOf(List:TListView; Data:Pointer; Var Item:TListItem): LongInt; overload;
  function getItemByCaption(List:TListView; Caption:String):TListItem;
  function getItemBySubItem(Index:LongInt; List:TListView; Value:String):TListItem;
  procedure Clear(Item:TListView); overload;

implementation

procedure Clear(Item:TListView);
begin
  Item.BeginUpdate;
  Try
    Item.Clear;
  finally
    Item.EndUpdate;
  end;
end;

function getItemBySubItem(Index:LongInt; List:TListView; Value:String):TListItem;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iLcv:=0; iCount:=List.Items.Count; Result:=nil;
  while (iLcv<iCount) and (Result=nil) do begin
    if SameText(List.Items[iLcv].SubItems[Index],Value) then begin
      Result:=List.Items[iLcv];
    end;
    iLcv+=1;
  end;
end;

function getItemByCaption(List:TListView; Caption:String):TListItem;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iLcv:=0; iCount:=List.Items.Count; Result:=nil;
  while (iLcv<iCount) and (Result=nil) do begin
    if SameText(List.Items[iLcv].Caption,Caption) then begin
      Result:=List.Items[iLcv];
    end;
    iLcv+=1;
  end;
end;

function IndexOf(List:TListView; Data:Pointer; Var Item:TListItem): LongInt;
var
  iLcv:LongInt;
  iCount:LongInt;
begin
  iLcv:=0; iCount:=List.Items.Count; Result:=-1; Item:=nil;
  while (iLcv<iCount) and (Result=-1) do begin
    if List.Items[iLcv].Data=Data then begin
      Result:=iLcv;
      Item:=List.Items[iLcv];
    end;
    Inc(iLcv);
  end;
end;

end.

